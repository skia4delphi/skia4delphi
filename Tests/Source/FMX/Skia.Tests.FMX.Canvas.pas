{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.Canvas;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkFMXCodecTests }

  [TestFixture]
  TSkFMXCanvasTests = class(TTestBase)
  private
    procedure ApplyGradientPoints(AGradient: TGradient; APoints: string);
    function CreateBitmap(const AImageFileName: string): TBitmap;
    function CreateMatrix(AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree: Single): TMatrix;
    procedure DrawChessBackground(ACanvas: TCanvas; ASquareSize: Single; AEvenSquareColor, AOddSquareColor: TAlphaColor);
    function StringToCorners(const ACornersString: string): TCorners;
  public
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestClear(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestClear2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAA8PDw8AAAAADw8PDwAAQAAPj4+PgABAAA+Pj4+EAkP8A/wD/AP8A/wD/AP8A/wEAgAAAAAAAA')]
    procedure TestClearRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,8PDw8A8PDw///PDxT09PT//8+PlPT09P//z/+f9P/0//AP8A/wD/AAD/AP8A/wD/AP8A/wD/AP8')]
    procedure TestClearRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestClipRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,//////Dw8PD/////8/f+/P/////7//78//////////////////////8A/wD/AP8A/wD/AP8A/wA')]
    procedure TestClipRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,//////z88PD////////+/P////////78///////////////////////w//D/8P/w/wj/AP8A/wA')]
    procedure TestClipRect3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAA8PDw8AAB/fHx9f39OTH98fn9/f05Mf3x+f39/XkwP8A/wD/AP8A/wD/AP8A/wEAgAAAAAAAA')]
    procedure TestClipRect4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestClipRect5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,//////Dw8PD/////8/f+/P/////7//78//////////////////////8A/wD/AP8A/wD/AP8A/wA')]
    procedure TestClipRect6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,8PDw8A8PDw///PDxT09PT//8+PlPT09P//z/+f9P/0//AP8A/wD/AAD/AP8A/wD/AP8A/wD/AP8')]
    procedure TestClipRect7(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestClipRect8(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///x8cHBwf////Hxw8fP////+fnj5+//////////////A/8D/wP/A/CD8APwA/AD8APwA/////8')]
    procedure TestClipRect9(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,false,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('2',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,true,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('3',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,false,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('4',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,true,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('5',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,false,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('6',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,true,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('7',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,false,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('8',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,true,0.98,+/vxcXDAgoL///Fxc8fOzv//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('9',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,false,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//f/7m/uL96uwqDif+Pnw+Pj22O55wH8')]
    [TestCase('10', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,true,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//f/7m/uL96uwqDif+Pnw+Pj22O55wH8')]
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//fv7m/uL96uwqDif+Pnw+Pj22O55wH8')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//fv7m/uL96uwqDif+Pnw+Pj22O55wH8')]
    [TestCase('13', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,false,0.98,///4+/sLAwP///j7+09PT///+Pv7f09P////////////4P/g/+H/5//n/+fn5+DnwGfEZ45njmA')]
    [TestCase('14', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,true,0.98,///4+/sLAwP///j7+09PT///+Pv7f09P////////////4P/g/+H/5//n/+fn5+DnwGfEZ45njmA')]
    [TestCase('15', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,true,true,0.98,///4+/sLAwP///j7+09PT///+Pv7f09P////////////4P/g/+H/5//n/+fn5+DnwGfEZ45njmA')]
    [TestCase('16', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,false,true,0.98,//nh4e2tDQ3//fHh7+9PTf/98e3v729t////////////w/4D/gP+O/77/vv++/77zvuG+4b7hvs')]
    [TestCase('17', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,true,true,0.98,//nh4e2tDQ3//fHh7+9PTf/98e3v729t////////////w/4D/gP+O/77/vv++/77zvuG+4b7hvs')]
    [TestCase('18', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('19', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('20', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('21', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('22', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('23', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('24', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('25', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('26', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('27', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('28', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('29', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('30', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('31', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('32', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,false,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('33', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,B/f39/PyAgB///f38/dODH//9/f3907vf//3/////+//D/8P/w//H/8f/5//n/+f/h8AHwAPP/8')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,B/f38/PyAgB///fz8/dODH//9/f3907vf//3/////+//D/8P/w//H/+f/5//n/+f/h8AHwAPP/8')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5//l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,AD39/fz8wAD//f39///OjP/9/f3//86P///9/f//3///+f/5f/l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,B/f39/PyAgB///f38/dODH//9/f3907vf//3/////+//D/8P/w//H/8f/5//n/+f/h8AHwAPP/8')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,B/f38/PyAgB///fz8/dODH//9/f3907vf//3/////+//D/8P/w//H/+f/5//n/+f/h8AHwAPP/8')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5//l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,AD39/fz8wAD//f39///OjP/9/f3//86P///9/f//3///+f/5f/l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,A/f39/f2BgB///f39/dOTH//9/f3907vf///9//3Tv8A8ADwAPAA8ADwAPAA8ADwAHAAMH/w//g')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,A/f38/f2BgB///fz9/dOTH//9/f3907vf///9//3Tv8A8ADwAPAA8ADwAPAA8ADwAHAAMH/w//g')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,AH39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AC4ADgAeAD8AHgA/AD8APwAeAA4B///8')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.98,AD39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AD4APwAfAD8APwAfAD8APwAeAA4B///8')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.98,+/vzcXDAgp7///Nxc8fO3v//8/l3997////z+Xf33/8RABEA8QLxA/EH8wf/50PmA+cC7wDvAAA')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,A/f39/f2BgB///f39/dOTH//9/f3907vf///9//3Tv8A8ADwAPAA8ADwAPAA8ADwAHAAMH/w//g')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,A/f38/f2BgB///fz9/dOTH//9/f3907vf///9//3Tv8A8ADwAPAA8ADwAPAA8ADwAHAAMH/w//g')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,AH39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AC4ADgAeAD8AHgA/AD8APwAeAA4B///8')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.98,AD39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AD4APwAfAD8APwAfAD8APwAeAA4B///8')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.98,+/vzcXDAgp7///Nxc8fO3v//8/l3997////z+Xf33/8RABEA8QLxA/EH8wf/50PmA+cC7wDvAAA')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestDrawBitmap(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('2',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('3',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('4',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('5',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('6',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('7',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('8',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AXGDYQNjw37D/8/9Lf5tgP+83Z37Xfsg')]
    [TestCase('9',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,false,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOYAx4DNAf+H9q94f/D/9d/z+fP8E')]
    [TestCase('10', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,true,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOYAx4DNAf+H9q94f/D/9d/z+fP8E')]
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758AB3AOYAx4DNAf+v9q94f/D/9d/z+fP8E')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758AB3AOYAx4DNAf+v9q94f/D/9d/z+fP8E')]
    [TestCase('13', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,false,0.98,AAAHBwZ2/t87MMfHB3f+33u+x9fPf//fe77n189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('14', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,true,0.98,AAAHBwZ2/t87MMfHB3f+33u+x9fPf//fe77n189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('15', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,true,true,0.98,AAAHBwZ2/t87MMfHB3f+33u+x9fPf//fe77n189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('16', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,false,true,0.98,AAYeHxsTU/N7/n7fW9ff+///f99b3/////9/31vf//8AHAH+A/4D9gMGAwYBBnsG+QZ/BP0G/wQ')]
    [TestCase('17', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,true,true,0.98,AAYeHxsTU/N7/n7fW9ff+///f99b3/////9/31vf//8AHAH+A/4D9gMGAwYBBnsG+QZ/BP0G/wQ')]
    [TestCase('18', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,false,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('19', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,true,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('20', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,false,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('21', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,true,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('22', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,false,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('23', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,true,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('24', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('25', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('26', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('27', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('28', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('29', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('30', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('31', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('32', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('33', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/xtbXx///f7/u19fv//9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,/IwMDAwMfPz//HxsT098/P/+/u7Pz37///7//////v9A8gDwAPAA8ADwAPAAcUDzoPfx9//19/4')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,/IwMDAwMfPz//HxsT098/P/+/u7Pz37///7//////v9A8gDwAPAA8ADwAPAAcUDzoPfx9//19/4')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,//+DAwODh/////PjQ4fP////9/fXn+////////////+ADoAPgA+AD4APgA+AB8APoBfX/v////8')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,//+DAwODh/////PjQ4fP////9/fXn+////////////+ADoAHgA6AD4APgA+AB8APoBfX/v////8')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWODYQNhw37D/8/9LfxviP2c/7353fgg')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWMDYQdhw37D/8/9LfxviP2c37353fgg')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,/IwMDAwMfPz//HxsT098/P/+/u7Pz37///7//////v9A8gDwAPAA8ADwAPAAcUDzoPfx9//19/4')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,/IwMDAwMfPz//HxsT098/P/+/u7Pz37///7//////v9A8gDwAPAA8ADwAPAAcUDzoPfx9//19/4')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,//+DAwODh/////PjQ4fP////9/fXn+////////////+ADoAPgA+AD4APgA+AB8APoBfX/v////8')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,//+DAwODh/////PjQ4fP////9/fXn+////////////+ADoAHgA6AD4APgA+AB8APoBfX/v////8')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWODYQNhw37D/8/9LfxviP2c/7353fgg')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWMDYQdhw37D/8/9LfxviP2c37353fgg')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,/JwMDAwMfPz//MzMTw/8/P/+zs7Pz/79//7u39/v/v1QKqBVACAAAAAAACCgVVArqFdUK6r1V/4')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,/JwMDAwMfPz//MzMTw/8/P/+zs7Pj/79//7u39+v/v1QKqBVACAAAAAAACCgVVArqFdUK6r1V/4')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,//+Dg4ODj////8PDg4PP////x9eXh8/////v19ev7/9UKqgVQAqABoAHQAqoFVQqqFVVqqvVVb8')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.98,//+Dg4ODh////8PDg4PP////x9eXh8/////v19ev7/9UKqgVQAqABoAGQAqoFVQqqFVVqqvdVb8')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.96,IHDQ0fPbHQ0zcNDR89vfzXP63N3339/9c/rc/ff/3/0AWIDYQNgQ3hD7E/8LH/sCPw0/Kj5WPqg')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,/JwMDAwMfPz//MzMTw/8/P/+zs7Pz/79//7u39/v/v1QKqBVACAAAAAAACCgVVArqFdUK6r1V/4')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,/JwMDAwMfPz//MzMTw/8/P/+zs7Pj/79//7u39+v/v1QKqBVACAAAAAAACCgVVArqFdUK6r1V/4')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,//+Dg4ODj////8PDg4PP////x9eXh8/////v19ev7/9UKqgVQAqABoAHQAqoFVQqqFVVqqvVVb8')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.98,//+Dg4ODh////8PDg4PP////x9eXh8/////v19ev7/9UKqgVQAqABoAGQAqoFVQqqFVVqqvdVb8')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.96,IHDQ0fPbHQ0zcNDR89vfzXP63N3339/9c/rc/ff/3/0AWIDYQNgQ3hD7E/8LH/sCPw0/Kj5WPqg')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    procedure TestDrawBitmapWithChessBackground(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x88HD7/////Hzw8fv////8/vX1+//////////////r/+v/6//f/kP8Y/2j/Sf//////////8')]
    procedure TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x88HD7/////Hzw8fv////8/vX1+//////////////r/+v/6//f/kP8Y/2j/Sf//////////8')]
    procedure TestDrawBitmapWithClipping2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,//z48OTMnDj//Pjw58/efP/8+fXv3////////+////+f8J/gP8Q/iH8Yfjj48PHw4/DH8Y/hH+E')]
    [TestCase('2',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,false,0.98,//z48OTMnDj//Pjw58/efP/8+fXv3////////+////+f8J/gP8Q/iH8Yfjj48PHw4/DH8Y/hH+E')]
    [TestCase('3',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,false,true,0.98,//z48OTMnDj//Pjx58/efP/8+fXv3///////////////8P/g/8T/iP8Y/Dj8ePjw8/Dn8M/xD+E')]
    [TestCase('4',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,false,false,0.98,//z48OTMnDj//Pjx58/efP/8+fXv3///////////////8P/g/8T/iP8Y/Dj8ePjw8/Dn8M/xD+E')]
    [TestCase('5',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,//z48OTMnDj//Pjw58/efP/8+fXv3////////+////+f8J/gP8Q/iH8Yfjj48PHw4/DH8Y/hH+E')]
    [TestCase('6',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,false,0.98,//z48OTMnDj//Pjw58/efP/8+fXv3////////+////+f8J/gP8Q/iH8Yfjj48PHw4/DH8Y/hH+E')]
    [TestCase('7',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,true,true,0.98,//z48OTMnDj//Pjx58/efP/8+fXv3///////////////8P/w/+T/iP8Y/jj4ePjw8/Dn8M/wD+E')]
    [TestCase('8',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,0.5,true,false,0.98,//z48OTMnDj//Pjx58/efP/8+fXv3///////////////8P/w/+T/iP8Y/jj4ePjw8/Dn8M/wD+E')]
    [TestCase('9',  '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,0.98,Pz9/f3////////9/f/////////////////////////8//3//f////3////////////////////8')]
    [TestCase('10', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,false,0.98,Pz9/f3////////9/f/////////////////////////8//3//f////3////////////////////8')]
    [TestCase('11', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,false,true,0.98,Pz9/f3////////9/f/////////////////////////8//3//f/////////////////////////8')]
    [TestCase('12', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,false,false,0.98,Pz9/f3////////9/f/////////////////////////8//3//f/////////////////////////8')]
    [TestCase('13', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,0.98,Pz9/f3////////9/f/////////////////////////8//3//f////3////////////////////8')]
    [TestCase('14', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,false,0.98,Pz9/f3////////9/f/////////////////////////8//3//f////3////////////////////8')]
    [TestCase('15', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,true,true,0.98,Pz9/f3////////9/f/////////////////////////8//3//f/////////////////////////8')]
    [TestCase('16', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,0.5,true,false,0.98,Pz9/f3////////9/f/////////////////////////8//3//f/////////////////////////8')]
    [TestCase('17', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,/saGhgcGD7///vbnR0dP///+9udvT1////////9////wH/Af4j/nPucxzmDEYMDIgJyBvuc//z8')]
    [TestCase('18', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,false,0.98,/saGhgcGD7///vbnR0dP///+9udvT1////////9////wH/Af4j/nPucxzmDEYMDIgJyBvuc//z8')]
    [TestCase('19', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,0.5,false,true,0.98,/saGhgcGDr///vbnR0dO///+9udvT17///7252/PX/8f4B/gP+A94HvGe8N/if+Q/xB+IDwAAEA')]
    [TestCase('20', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,0.5,false,false,0.98,/saGhgcGDr///vbnR0dO///+9udvT17///7252/PX/8f4B/gP+A94HvGe8N/if+Q/xB+IDwAAEA')]
    [TestCase('21', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,8+Hg8PD4HO///fDx8/9e7////f33/1//////////3//OI+Yx8jn7OPkQ/YD8wPDggPEA/wX/H/8')]
    [TestCase('22', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,1,true,false,0.98,8+Hg8PD4HO///fDx8/9e7////f33/1//////////3//OI+Yx8jn7OPkQ/YD8wPDggPEA/wX/H/8')]
    [TestCase('23', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,0.5,true,true,0.98,8+Hg8PD4HO///fDx8/9e7////f33/1/////9/ff/f/8j/hHvEe8J9wD/BH8AfwI/AQ4DADwA4AA')]
    [TestCase('24', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,-200,-50,660,343,0.5,true,false,0.98,8+Hg8PD4HO///fDx8/9e7////f33/1/////9/ff/f/8j/hHvEe8J9wD/BH8AfwI/AQ4DADwA4AA')]
    [TestCase('25', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,1,false,true,0.98,/38/Pz8fDx//f39/f19PX////////9////////////+f////D/8v////X/9P/6P/Kf9h/7P///8')]
    [TestCase('26', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,1,false,false,0.98,/38/Pz8fDx//f39/f19PX////////9////////////+f////D/8v////X/9P/6P/Kf9h/7P///8')]
    [TestCase('27', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,0.5,false,true,0.98,/38/Pz8fDx//f39/f19PX////////9/////////////gAPAAcAB4ANAAwABAAGAAoADAAOAAQAA')]
    [TestCase('28', '3d-shapes.svg,300,300,-1,2,-50,-100,-30,0,0,660,343,0,0,200,200,0.5,false,false,0.98,/38/Pz8fDx//f39/f19PX////////9/////////////gAPAAcAB4ANAAwABAAGAAoADAAOAAQAA')]
    [TestCase('29', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,0.98,//////Pj4cD/////8+fvzP/////35+/+/////////////////////////x//D/8f/r/+V/qE9sw')]
    [TestCase('30', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,false,0.98,//////Pj4cD/////8+fvzP/////35+/+/////////////////////////x//D/8f/r/+V/qE9sw')]
    [TestCase('31', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,0.5,true,true,0.98,//////Pj4cD/////8+fvzP/////35+/+//////fn7/4AAAAAAAAAAAAAAAAAAAAAAAwAHwxbHv8')]
    [TestCase('32', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,0.5,true,false,0.98,//////Pj4cD/////8+fvzP/////35+/+//////fn7/4AAAAAAAAAAAAAAAAAAAAAAAwAHwxbHv8')]
    procedure TestDrawBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,1,0.98,H744cOTMnDg/vzhw5MycOP+/v3n27t6+////////3/7/9N/q/9i/+H+8/3z69PXx6/nf+b/pf+E')]
    [TestCase('2', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,H744cOTMnDh//nhw58/ePP///nn3796+//////////7/4P/A/4D/AP4Q/DDwcODgweCD4A/gD+E')]
    [TestCase('3', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,Red,1,0.98,P39/f39///////9/f3///////39/f////////3////8//3//f////3////////////////////8')]
    [TestCase('5', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,0.3,0.98,/sbGhoQAAvf//vbnx0dO////9//Xx8/////3/9fHz/8f4B/gP+A94HvOe99/v/+//3N+YTzgAcA')]
    [TestCase('6', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('7', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0.3,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('8', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,Red,0.3,0.98,//////Pj4cD/////8+fvzP/////7///d//////v//90AAAAAAAAAAAAAAAAAAAAAAAwAHwwbHj8')]
    procedure TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,+8u5NQADgp////l1Q0fO3///+33nR+7f//////fP79/vr++nr7cPcU0HAYO0j/Sf5J3YmMOZw/8')]
    procedure TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,0,0,1,1,0.4,0.98,SHt7SP9K/wB/e3tp/0//TP/7++n/7//t//v76f/v/////5JJAkm22///tskACbbJ//+22wBJtkk')]
    [TestCase('2',  'android.svg,claRed,Tile,0,0,1,1,0.4,0.98,SHt7SP9K/wD/+3tI/8//zP//f17/3//e//9/X//f/9+2WUhbQADySbf/SssBECJJ///73wAQAkg')]
    [TestCase('3',  'android.svg,claWhite,Tile,0,0,1,1,1,0.98,SHt7SP9K/wD/+3tI/8//zP/7++n/7//t///76f/v/////7ZJAkm22///ttsCSbbb//+22wJJtsk')]
    [TestCase('4',  'android.svg,claRed,Tile,0,0,1,1,1,0.98,SHt7SP9K/wD/+/vI/8//wP///97/3//W///////f////////tsm///////+2Sb////+3/7bJv/8')]
    [TestCase('5',  'android.svg,claWhite,Tile,0.15,0.15,0.85,0.85,0.4,0.98,/4GBqYGBgf///fHpw8fP///99f339//////////////AA+ST///kk8AD5JP//+STwAPkk/////8')]
    [TestCase('6',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,0.4,0.98,/4GBqYGBgf///fHpw8fN////++vLz8///////+//7//AA8ST/7fkk8AD5AP0k+STwAPAA/////8')]
    [TestCase('7',  'android.svg,claWhite,Tile,0.15,0.15,0.85,0.85,1,0.98,/4GBqYGBgf///fHpw8fN///99f339/3//////////f/kk+ST///km+ST5JP//+yb5JPkl/////8')]
    [TestCase('8',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,1,0.98,/4GBqYGBgf///fHpw8fN////++vLz8/////////////bZ//7/////9vn///t1///22/76/////8')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,0,0,1,1,0.4,0.98,Pz8///////9/f3//////////f/////////9///////////////////////////////////////8')]
    [TestCase('10', 'android.svg,claRed,TileOriginal,0,0,1,1,0.4,0.98,Pz9//////////3//////////f/////////9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,0,0,1,1,1,0.98,Pz9//////////3//////////f/////////9///////////////////////////////////////8')]
    [TestCase('12', 'android.svg,claRed,TileOriginal,0,0,1,1,1,0.98,Pz9///////////////////////////////////////////////////////////////////////8')]
    [TestCase('13', 'android.svg,claWhite,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,nw8Pn///////f3////////9/f//////////////////D/8f/5/////////////////////////8')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,vx8fn///////f3////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('15', 'android.svg,claWhite,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,/x8fn///////f3////////9/f//////////////////D/+f/5/////////////////////////8')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,/x8fn///////f3////////////////////////////8YABgAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,0,0,1,1,0.4,0.98,/8ODAADDx/////NhQ8fP////92VH79//////f//////wD9ALgAGAAYABgAHQC/AP+R/5H/2///8')]
    [TestCase('18', 'android.svg,claRed,TileStretch,0,0,1,1,0.4,0.98,/8PDAQHD5/////NhQ8fv////8+PD1+/////7//Pf///pt4ABgBGAEYARgBWAAekH+B/4H/gf//8')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,0,0,1,1,1,0.98,/8PDAQHD5/////NhQ8fv/////2VH7//////////////wD9/rgAOAA4ADgAPQC/AP+b/5v/2///8')]
    [TestCase('20', 'android.svg,claRed,TileStretch,0,0,1,1,1,0.98,/8PDAQHD5/////NhQ8fv////8+Pj1+/////////////wD///0AvQC9AL0A/wD/AP/b/9v/2///8')]
    [TestCase('21', 'android.svg,claWhite,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,/+fDgYHD5/////Phw8fv////9/fX1//////////////4H/gf4AfgB+AH4Af4H/w//D////////8')]
    [TestCase('22', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,/+fDgYHD5/////Phw8fv////8+nr7+/////////////6X/gf4AfgJ+AH4Cf4H/w//D////////8')]
    [TestCase('23', 'android.svg,claWhite,TileStretch,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////9/PX1//////////////4P/gf4AfgB+AH4A/4H/w//T////////8')]
    [TestCase('24', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+nr7+/////////////8P/v////4P/g/+D////////////////8')]
    procedure TestFillRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,true,1,0.98,S6gK+grqCOt7uPr6S+/M7/+9//9v/+3//73///////8IgGZubu79VQiyZkzu7mRUmAD1Xv7/9k4')]
    [TestCase('2',  'android.svg,claWhite,Tile,true,0.5,0.98,SrxK+gpqCGt7vOr6S2vM7/u9//9Pf+3//73////////IgGBERu6ZVQiyRFDu7kReKAT1Xv7+dG4')]
    [TestCase('3',  'android.svg,claWhite,Tile,false,1,0.98,S6gK+grqCOt7uPr6S+/M7/+9//9v/+3//73///////8IgGZubu79VQiyZkzu7mRUmAD1Xv7/9k4')]
    [TestCase('4',  'android.svg,claWhite,Tile,false,0.5,0.98,SrxK+gpqCGt7vOr6S2vM7/u9//9Pf+3//73////////IgGBERu6ZVQiyRFDu7kReKAT1Xv7+dG4')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,Qf1fUX9A/wB7/9/Rf0P/zPv///t/6//u+//////v//8IgGZsbu7dVQiCZkRu7mZUiAD3Xv7/9k4')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,Qv1XUH9A/wB7/9fQf0P/zPv///p/6//u///////v/++IgGAERO6ZEQi6RBDE7kAWqAHxXvz/dA4')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,Qf1fUX9A/wB7/9/Rf0P/zPv///t/6//u+//////v//8IgGZsbu7dVQiCZkRu7mZUiAD3Xv7/9k4')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,Qv1XUH9A/wB7/9fQf0P/zPv///p/6//u///////v/++IgGAERO6ZEQi6RBDE7kAWqAHxXvz/dA4')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,true,1,0.98,wv1CQkJCvUL7/fLiQ0f9zvv/9vbXx//////+9/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('10', 'android.svg,claWhite,TileOriginal,true,0.5,0.98,wv1CQkJCvUL7/eLCQ0P9zvv/5tbXx//////+1/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,false,1,0.98,wv1CQkJCvUL7/fLiQ0f9zvv/9vbXx//////+9/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('12', 'android.svg,claWhite,TileOriginal,false,0.5,0.98,wv1CQkJCvUL7/eLCQ0P9zvv/5tbXx//////+1/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,true,1,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/zdf77//3////0/9K/0//7//v/+//4v9C/0DvAP4A5ABkA')]
    [TestCase('18', 'android.svg,claWhite,TileStretch,true,0.5,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/79f77//3////0//C/0//7//v/+//4v9D/8DvBP4qJFQmI')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,false,1,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/zdf77//3////0/9K/0//7//v/+//4v9C/0DvAP4A5ABkA')]
    [TestCase('20', 'android.svg,claWhite,TileStretch,false,0.5,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/79f77//3////0//C/0//7//v/+//4v9D/8DvBP4qJFQmI')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,/8PDgYHD5///88PBg8Pv///z1+Pjw+////PX4+fj//8v9KpU7/Tv9O/07/Qv9C/0CtAOwApABsA')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,/8PDgYHD5///88PBg8Pv///z19PT1+////v/19f///9/9iAE//6v9K/0//4v9H/2qlVeyqpVVmo')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,/8PDgYHD5///88PBg8Pv///z1+Pjw+////PX4+fj//8v9KpU7/Tv9O/07/Qv9C/0CtAOwApABsA')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,/8PDgYHD5///88PBg8Pv///z19PT1+////v/19f///9/9iAE//6v9K/0//4v9H/2qlVeyqpVVmo')]
    procedure TestFillRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claWhite,Tile,true,1,0.98,/8XBgcGB9////fHhw8f////99fXX1////////9fX///7v9ETwRP5u///2RPBE9mT///7u/////8')]
    [TestCase('2',  'android.svg,claWhite,Tile,true,0.5,0.98,/8GBgcGB1////fHhw8ff///99fXX19///////9fX///7u9ETwAPZk///0RPAA9kT///5u/////8')]
    [TestCase('3',  'android.svg,claWhite,Tile,false,1,0.98,/8XBgcGB9////fHhw8f////99fXX1////////9fX///7v9ETwRP5u///2RPBE9mT///7u/////8')]
    [TestCase('4',  'android.svg,claWhite,Tile,false,0.5,0.98,/8GBgcGB1////fHhw8ff///99fXX19///////9fX///7u9ETwAPZk///0RPAA9kT///5u/////8')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,/9XBgcGB/////fHhw8P/////++vr6///////////////////+z/////////7P/////////////8')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,/9XBgcGB/////fHhw8P/////++vr6////////+vr///Zk8ZLxOvZU/u711PE69ET2ZvXe/////8')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,/9XBgcGB/////fHhw8P/////++vr6///////////////////+z/////////7P/////////////8')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,/9XBgcGB/////fHhw8P/////++vr6////////+vr///Zk8ZLxOvZU/u711PE69ET2ZvXe/////8')]
    [TestCase('9',  'android.svg,claWhite,TileOriginal,true,1,0.98,Pz////////9/f////////39///////////////////////////////////////////////////8')]
    [TestCase('10', 'android.svg,claWhite,TileOriginal,true,0.5,0.98,vz//////////f/////////9///////////////////////////////////////////////////8')]
    [TestCase('11', 'android.svg,claWhite,TileOriginal,false,1,0.98,Pz////////9/f////////39///////////////////////////////////////////////////8')]
    [TestCase('12', 'android.svg,claWhite,TileOriginal,false,0.5,0.98,vz//////////f/////////9///////////////////////////////////////////////////8')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,Hz8///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,Pz9///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,Hz8///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,Pz9///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('17', 'android.svg,claWhite,TileStretch,true,1,0.98,/8PDgYHDx/////Phw8fP/////+fH79/////////////wD//vwAPAC8ALwAPwD/AP+b/9v/////8')]
    [TestCase('18', 'android.svg,claWhite,TileStretch,true,0.5,0.98,/8ODgYHDx/////Phw8fP/////+fH79/////////////wD9ALwAPAA8ADwAPQC/AP+R/5v/////8')]
    [TestCase('19', 'android.svg,claWhite,TileStretch,false,1,0.98,/8PDgYHDx/////Phw8fP/////+fH79/////////////wD//vwAPAC8ALwAPwD/AP+b/9v/////8')]
    [TestCase('20', 'android.svg,claWhite,TileStretch,false,0.5,0.98,/8ODgYHDx/////Phw8fP/////+fH79/////////////wD9ALwAPAA8ADwAPQC/AP+R/5v/////8')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,/8PDgYHD5/////Phw8fv////8+Hj1+/////////////wD///8A/wD/AP8A/wD/AP/b////////8')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,/8PDgYHDx/////Phw8fP////8+Hj1+/////z4ePX7/8P8AAAD/AP8A/wD/AP8A/4AkAAAAAAAAA')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,/8PDgYHD5/////Phw8fv////8+Hj1+/////////////wD///8A/wD/AP8A/wD/AP/b////////8')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,/8PDgYHDx/////Phw8fP////8+Hj1+/////z4ePX7/8P8AAAD/AP8A/wD/AP8A/4AkAAAAAAAAA')]
    procedure TestFillRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,JOH4/j4nBcH//fj/f2cPzf//+/9/b8/f//////9v/9/5wv/C/+zn3of8j3wGfwh/CHOwY3jH8YM')]
    [TestCase('2',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,JOH4+j4nBcH//fj7f2dPzf//+/9/b8/f//////9v/9/5wv/C/+zn3of+j3wGfwh/CHOwY3jH8IM')]
    [TestCase('3',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,1,true,0.98,Pz8/f/////////9///////////////////////////8f/x//f/////////////////////////8')]
    [TestCase('4',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,Pz8/f/////////9///////////////////////////8f/x//f/////////////////////////8')]
    [TestCase('5',  'android.svg,300,300,Tile,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,58HB8LY+Dwv//fHx939PT//98/P3f9/f///7//9//9/4w/GD88T/hP+Yz7yP+B75DP8Q/xDnYM8')]
    [TestCase('6',  'android.svg,300,300,Tile,-1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,Hz9/f3////9/f39/f////39///////////////////9//z//P/9//z//f/////////////////8')]
    [TestCase('7',  'android.svg,300,300,Tile,-1,2,250,-50,30,0,0,200,200,1,true,0.98,A+dvHx8fP3///29fX18/f/////9/f/////////9///95/3//d/9j/wP/h/8H/wf/D/8//7////8')]
    [TestCase('8',  'android.svg,300,300,Tile,1,-2,-50,200,30,0,0,200,200,0.6,true,0.98,6dsfBwdHz////39nR0fP////f2/P3///////7//f///H/4d/Bj8Afwh/EH8wf/D/8f/z//////8')]
    [TestCase('9',  'android.svg,300,300,TileOriginal,1,2,150,20,-90,-200,-50,660,343,1,true,0.98,////w4HD///////jw8f//////+/nz//////////////////////8P/AH4Af4H/4///////////8')]
    [TestCase('10', 'android.svg,300,300,TileOriginal,1,2,-50,100,-90,-200,50,660,343,0.6,true,0.98,///////ngcH//////+fPzf//////78/P//////////////////////////////w/4AfgB+AH/D8')]
    [TestCase('11', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,1,true,0.98,////nx8fHz//////X19ff/////9/f39////////////////////j/+P/g/+D/4P/h/+H/5//n/8')]
    [TestCase('12', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,0.6,true,0.98,////nx8fHz//////X19ff/////9/f39////////////////////j/+P/g/+D/4P/B/+H/4//n/8')]
    [TestCase('13', 'android.svg,300,300,TileOriginal,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('14', 'android.svg,300,300,TileOriginal,-1,2,-50,-100,60,-200,-50,660,343,0.6,true,0.98,Hz9///////9/f3////////////////////////////9///////////////////////////////8')]
    [TestCase('15', 'android.svg,300,300,TileOriginal,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,Hx////////9/f////////39/////////f/////////////////////////////////////////8')]
    [TestCase('16', 'android.svg,300,300,TileOriginal,1,-2,-50,300,10,0,0,200,200,0.6,true,0.98,//9/Pz8/P3///39/f39/f///f///////////////////////H/8f/x//H/8f/x//H/8//z//f/8')]
    [TestCase('17', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,hwMBxTANBwP/+/Hlc09PD//79+d3X08P///3/3dfTw8AA4ADwAEwAQwBAwEBgQBhABkABwADAAc')]
    [TestCase('18', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,hwMBxTAdBwP/+/Hlc19PD//79/dzX08P///3/3NfTw8AA4Aj4DEwAQwBBwEBwQBhADkADwADAAc')]
    [TestCase('19', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,1,true,0.98,P39/f3////9/f39///////////////////////////9//z//P/9//3////////////////////8')]
    [TestCase('20', 'android.svg,300,300,TileStretch,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,f39/f3////9/f39///////////////////////////9//z//P/9//3////////////////////8')]
    [TestCase('21', 'android.svg,300,300,TileStretch,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,///34sDAgID///fjw8fOzP//9+/Xz87c//////////////4e/gz8DPgI+BDwEPAg4GDAQMDAwIA')]
    [TestCase('22', 'android.svg,300,300,TileStretch,-1,2,-50,-100,-60,-200,-50,660,343,0.6,true,0.98,XseBgAAAAAB///HhQ0fOzP//8eFDR87M/////+///+/f+/////////////////////////////8')]
    [TestCase('23', 'android.svg,300,300,TileStretch,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,DwcHA4P///9/f3fjw/////9/f//3//////9///f///8AfwA/AB8Az8A/95////////////////8')]
    [TestCase('24', 'android.svg,300,300,TileStretch,1,-2,-50,500,30,0,0,200,200,0.6,true,0.98,///////78cH/////////zf/////////P///////////////////////////////f/5f/h/4n8A8')]
    procedure TestFillRectBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,1,0.98,/4GBgYH///////Hh4f//////8eHh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('2',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,1,1,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADgAGAAYABwAPAA8AD//////////8')]
    [TestCase('3',   '[claBlack;0 claWhite;1],claWhite,Linear,False,1,0.5,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADwAOAAYABwAPAA8AD//////////8')]
    [TestCase('4',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,1,0.98,/4GBgYH///////Hh4f//////8eHh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('5',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,1,1,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADgAGAAYABwAPAA8AD//////////8')]
    [TestCase('6',   '[claBlack;0 claWhite;1],claWhite,Linear,True,1,0.5,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADwAOAAYABwAPAA8AD//////////8')]
    [TestCase('7',   '[claBlack;0 claWhite;1],claWhite,Linear,False,2,1,0.98,/8CAwMDAwMD//PDhw8fMzP/88OHDx+zs///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('8',   '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,False,2,1,0.98,/8CAwMDAwMD//PDhw8fMzP/88OHDx+zs//////////////AA4ADgAOAA4ADgAOAA4ADgAOAA4AA')]
    [TestCase('9',   '[claBlack;0 claWhite;1],claWhite,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('10',  '[claBlack;0 claWhite;1],claWhite,Linear,True,0.5,1,0.98,Dw8P/////////8//////////3//////////f//////+B/4H/gf////////////////////////8')]
    [TestCase('11',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('12',  '[claBlack;0 claWhite;1],claWhite,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('13',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,1,0.98,w4GBgYH///////Hh4f//////8+Hh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('14',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,1,1,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('15',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,1,0.5,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('16',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,1,0.98,w4GBgYH///////Hh4f//////8+Hh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('17',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,1,1,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('18',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,1,0.5,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('19',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zs///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('20',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx+zs//////////////////////AA4ADgAOAA4ADgAOAA4AA')]
    [TestCase('21',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('22',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,0.5,1,0.98,Dw8P/////////8//////////3//////////f//////+B/4H/gf////////////////////////8')]
    [TestCase('23',  '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('24',  '[claBlack;0.2 claWhite;0.8],claWhite,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('25',  '[claBlack;-1 claWhite;0.8],claWhite,Linear,True,1,1,0.98,gYGBgcP//////fHhw//////98eHD/////////8P////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('26',  '[claBlack;0 claWhite;2],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+PhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('27',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P//////+PDw//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('28',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,1,1,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8X////z48PDxf+AAYABgAGAAf////////////////////8')]
    [TestCase('29',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8f////z48PDx/+AAYABgAGAAf////////////////////8')]
    [TestCase('30',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P//////+PDw//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('31',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,1,1,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8X////z48PDxf+AAYABgAGAAf////////////////////8')]
    [TestCase('32',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8f////z48PDx/+AAYABgAGAAf////////////////////8')]
    [TestCase('33',  '[claBlack;0 claWhite;1],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('34',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM////////7MzgAOAA4ADgAOAA4ADgAOAA8AD///////8')]
    [TestCase('35',  '[claBlack;0 claWhite;1],claRed,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36',  '[claBlack;0 claWhite;1],claRed,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('37',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('38',  '[claBlack;0 claWhite;1],claRed,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('39',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8H//////+PDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('40',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,1,1,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8X////z48PDxf+AAYABgAHAA/////////////////////8')]
    [TestCase('41',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8f////z48PDx/+AAYABgAGAAf////////////////////8')]
    [TestCase('42',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8H//////+PDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('43',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8X////z48PDxf+AAYABgAHAA/////////////////////8')]
    [TestCase('44',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhwcPF////8+PDw8f////z48PDx/+AAYABgAGAAf////////////////////8')]
    [TestCase('45',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('46',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM////////7MzgAOAA4ADgAOAA4ADgAOAA8AD///////8')]
    [TestCase('47',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('48',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('49',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('50',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('51',  '[claBlack;-1 claWhite;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8H//////+PDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('52',  '[claBlack;0 claWhite;2],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P//////+PDw//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('53',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('54',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('55',  '[claRed;0 claBlue;1],claWhite,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('56',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('57',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('58',  '[claRed;0 claBlue;1],claWhite,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('59',  '[claRed;0 claBlue;1],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8AAAAAAAAAAAAAAAA')]
    [TestCase('60',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8AAAAAAAAAAAAAAAA')]
    [TestCase('61',  '[claRed;0 claBlue;1],claWhite,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('62',  '[claRed;0 claBlue;1],claWhite,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+////////////////8A/wD/AP+B//////////////////////8')]
    [TestCase('63',  '[$7FFF0000;0 $7F0000FF;1],claWhite,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('64',  '[claRed;0 claBlue;1],claWhite,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8AAAAAAAA')]
    [TestCase('65',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8H//////8PDwf///////////8ADwAPAA8ADwAPAA/////8')]
    [TestCase('66',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('67',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('68',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8H//////8PDwf///////////8ADwAPAA8ADwAPAA/////8')]
    [TestCase('69',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('70',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('71',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAP////////////////////8')]
    [TestCase('72',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8f/wAAAAAAAAAAAAA')]
    [TestCase('73',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('74',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////8A/wD/AP+B//////////////////////8')]
    [TestCase('75',  '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('76',  '[claRed;0.2 claBlue;0.8],claWhite,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('77',  '[claRed;-1 claBlue;0.8],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8H//////+PDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('78',  '[claRed;0 claBlue;2],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P///////////9//n/+f/4//D/8AAAAAAAAAAAAAAAAAAA')]
    [TestCase('79',  '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8P/////////////////wAPAA4ABgAGAAYABgAGAAcAD//8')]
    [TestCase('80',  '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P////////////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('81',  '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+PhwcH////////////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('82',  '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P////z4+PDw/+AAYABgAGAAcADwAP///////////////8')]
    [TestCase('83',  '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf///fHhwcHB////8+PDw8P//////8PDw//AA8ADwAPAA8ADwAPAA8AD//////////8')]
    [TestCase('84',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,1,1,0.98,/4H//4GBgf///f//wcPF///9///Dw8f//////8PDx/+AAYABwAPAA8ADwAPAA/////////////8')]
    [TestCase('85',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,False,1,1,0.98,/4H//4GBgf///f//wcPF///9///Dw8f//////8PDx/+AAYABwAPAA8ADwAPAA/////////////8')]
    [TestCase('86',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Linear,True,2,0.5,0.98,/8CAgMDA/////PDhw8f////88OHD5//////////////wAOAA4ADgAOAA4ADgAOAA4AD///////8')]
    [TestCase('87',  '[],claWhite,Linear,True,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('88',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,1,0.98,/4GBmZmBgf////H5+cHB////////z8/////////fz//gB8ADwAPAA8ADwAPAA+AHwAPIE/////8')]
    [TestCase('89',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,1,1,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('90',  '[claBlack;0 claWhite;1],claWhite,Radial,False,1,0.5,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('91',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,1,0.98,/4GBmZmBgf////H5+cHB////////z8/////////fz//gB8ADwAPAA8ADwAPAA+AHwAPIE/////8')]
    [TestCase('92',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,1,1,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('93',  '[claBlack;0 claWhite;1],claWhite,Radial,True,1,0.5,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('94',  '[claBlack;0 claWhite;1],claWhite,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('95',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P////////////4P+A/wD+APwA+AD4APAA8ADwAfAD8Ac')]
    [TestCase('96',  '[claBlack;0 claWhite;1],claWhite,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('97',  '[claBlack;0 claWhite;1],claWhite,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('98',  '[$7F000000;0 $7FFFFFFF;1],claWhite,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('99',  '[claBlack;0 claWhite;1],claWhite,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('100', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,1,0.98,/4GZvb2Zgf////n9/dnB////////383////////fzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('101', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('102', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,1,0.5,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('103', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,1,0.98,/4GZvb2Zgf////n9/dnB////////383////////fzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('104', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('105', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,1,0.5,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('106', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('107', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8APwB/AP8A8')]
    [TestCase('108', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('109', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('110', '[$7F000000;0.2 $7FFFFFFF;0.8],claWhite,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('111', '[claBlack;0.2 claWhite;0.8],claWhite,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('112', '[claBlack;-1 claWhite;0.8],claWhite,Radial,True,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('113', '[claBlack;0 claWhite;2],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz8///////+/Pz//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('114', '[claBlack;0 claWhite;1],claRed,Radial,False,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz83//////+/Pzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('115', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,1,1,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('116', '[claBlack;0 claWhite;1],claRed,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('117', '[claBlack;0 claWhite;1],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz83//////+/Pzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('118', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('119', '[claBlack;0 claWhite;1],claRed,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('120', '[claBlack;0 claWhite;1],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('121', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM////////7MzgAOAA4ADgAOAH4B/gP+B/4H/g/+D/4P8')]
    [TestCase('122', '[claBlack;0 claWhite;1],claRed,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('123', '[claBlack;0 claWhite;1],claRed,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('124', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('125', '[claBlack;0 claWhite;1],claRed,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgB+AM')]
    [TestCase('126', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz8n//////+/Pyf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('127', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,1,1,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('128', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('129', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz8n//////+/Pyf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('130', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('131', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhwcPF/////+/Pz8//////78/Pz/+DwYfhj/GP8Y/xj/GH4YPBgAGAAcAD//8')]
    [TestCase('132', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('133', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM////////7MzgAOAA4ADgAOAD4A/gH+A/4H/gf+D/4P8')]
    [TestCase('134', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('135', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('136', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('137', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AE')]
    [TestCase('138', '[claBlack;-1 claWhite;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz8///////+/Pz//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('139', '[claBlack;0 claWhite;2],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz83//////+/Pzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('140', '[claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('141', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3///////94HnAOcA5gBmAGcA5wDngefn5//n/+AAA')]
    [TestCase('142', '[claRed;0 claBlue;1],claWhite,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3//////f94HnAOYAZgBmAGYAZwDngefD5//n/+AAA')]
    [TestCase('143', '[claRed;0 claBlue;1],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('144', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3///////94HnAOcA5gBmAGcA5wDngefn5//n/+AAA')]
    [TestCase('145', '[claRed;0 claBlue;1],claWhite,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3//////f94HnAOYAZgBmAGYAZwDngefD5//n/+AAA')]
    [TestCase('146', '[claRed;0 claBlue;1],claWhite,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP/////////////////////wP/D/8f/z//f49+D/wP+A/4D/AP8A/wA')]
    [TestCase('147', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//9v/f///////2/9////8f/x//H/4f8B/AH4AfAB8AHgAeAB4AHAA')]
    [TestCase('148', '[claRed;0 claBlue;1],claWhite,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('149', '[claRed;0 claBlue;1],claWhite,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z8/////////////////D/8P/vf////////////////////////8')]
    [TestCase('150', '[$7FFF0000;0 $7F0000FF;1],claWhite,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('151', '[claRed;0 claBlue;1],claWhite,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+/v///w4ePn7+8AAAf/B/8H/wf/B/8H/wf8B/gH8AfgB+A')]
    [TestCase('152', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////Hz9//P89573bvb29vb3bvee8/zx+PAA/////8')]
    [TestCase('153', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz99X///3//f////98PngecA5wDnAOcA54Hnw+f/5//j/8AAA')]
    [TestCase('154', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8fHz99f//////f////94HnAOcA5wDnAOcA5wDngef/5//j/8AAA')]
    [TestCase('155', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////Hz9//P89573bvb29vb3bvee8/zx+PAA/////8')]
    [TestCase('156', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz99X///3//f////98PngecA5wDnAOcA54Hnw+f/5//j/8AAA')]
    [TestCase('157', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8fHz99f//////f////94HnAOcA5wDnAOcA5wDngef/5//j/8AAA')]
    [TestCase('158', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OfP39/////////////wAPAA8A/wP/B/8P/x//H/8//z//P/8/8')]
    [TestCase('159', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OfP39/e///w58/f3/8f/x//H/8f/h/wH8AfgB+AHwAfAB8AHgA')]
    [TestCase('160', '[claRed;0.2 claBlue;0.8],claWhite,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('161', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////f///////D/6X/mf////////////////////////8')]
    [TestCase('162', '[$7FFF0000;0.2 $7F0000FF;0.8],claWhite,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('163', '[claRed;0.2 claBlue;0.8],claWhite,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+/v///w4ePn7+8AAAf/B/8H/wf/B/8H/wf/B/8H/gf8B/g')]
    [TestCase('164', '[claRed;-1 claBlue;0.8],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8fHx8fP///////Hz8//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('165', '[claRed;0 claBlue;2],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///f+////D/8P/w//D///9/73/vH4/////8')]
    [TestCase('166', '[claRed;0 $7F000000;0.5 claBlue;1],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('167', '[claRed;0 $7F000000;1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8enp8fH//////+/z/f/QC/AP4AfgB+AH4AfwD9AL3DvH4/////8')]
    [TestCase('168', '[claRed;0 $7F000000;-1 claBlue;0.5],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHh8fH//////+fz/f/QC/AP4AfgB+AH4AfwD9AL3DvH4/////8')]
    [TestCase('169', '[claRed;1 $7F000000;0.5 claBlue;0],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz83//////+/Pzf/AA8ADw8PDw8PDw8PAA8ADwAPAA/////8')]
    [TestCase('170', '[claRed;1 $7F000000;0 claBlue;0.5],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF///9+e3N6/X//////83r9f/AA8ADwAPBg8GDwAPAA8ADwAPAA/////8')]
    [TestCase('171', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF////9+vr18////////vf///iR8GDy9PH48fjy9PBg+JH8A/4H/////8')]
    [TestCase('172', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf///fHhwcPF////9+vr18////////vf///iR8GDy9PH48fjy9PBg+JH8A/4H/////8')]
    [TestCase('173', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claWhite,Radial,True,2,0.5,0.98,///AwcfMyNj///Dhx8/O3P//8OHHz87c////////////wP+A/gD8APwB+A/wGPAw8GPwR/BP8M8')]
    [TestCase('174', '[],claWhite,Radial,True,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('175', '[claRed;0.3],claWhite,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('176', '[claRed;0.3],claBlue,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8eHhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('177', '[claRed;0.3],claWhite,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('178', '[claRed;0.3],claBlue,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8eHhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('179', '[claRed;0 claBlue;1 claBlue;1],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('180', '[claRed;0 claRed;0 claBlue;1],claWhite,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('181', '[claRed;0 claBlue;1 claBlue;1],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('182', '[claRed;0 claRed;0 claBlue;1],claWhite,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('183', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,1,0.98,////gYGBgf/////hw8fP/////+PDx8//////////////////wAPAA8ADwAPAA4ABgAGAAcAD//8')]
    [TestCase('184', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,1,0.98,/+fDgYHD5/////Phw8fv////8/Hz9//////////////wD+AH4AfgB+AH4AfgB/AP+B////////8')]
    [TestCase('185', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,0.5,0.98,////gYGBgf/////hw8fP/////+PDx8//////////////////wAPAA8ADwAPAA4ABgAGAAcAD//8')]
    [TestCase('186', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,0.5,0.98,/+fDgYHD5/////Phw8fv////8/Hz9//////////////wD+AH4AfgB+AH4AfgB/AP+B////////8')]
    procedure TestFillRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('17', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8B/wB/AH8A/wD/Af8B/wP/B/8H/w//D/8')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,g4ODg4ODw/////Pjw4PD////8/Pz4+P////////v+//4P/A/8D/wP/g/+G//7+/v4A////////8')]
    [TestCase('19', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,g5uro7uDg/////vj+4OD//////f/y+P////////v///8f/h/+F/4f/x/////7+/v44////////8')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('36', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P8/Pbx/f/3+/z8/v39//f//v3///3/9B6gB1AH8A/wD/Af8D9UPqg9VH6qvVV+o')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,g4ODg4ODw/+7u+PDw4PH//v78/Pz49f///v/8//z3/9/8D/wP/A/8D/wH/Af8F/wP/AAAAAAAAA')]
    [TestCase('38', '[claRed;0 claNull;0.5 claBlue;1],claWhite,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,g5ODg6uDg/+7u+PD64OH/7+/9/f/y5f/v///9//79/9nkGfQZ9BnkGOQcRB4MHzwf/A/+AAAAAA')]
    procedure TestFillRectGradientRadial(const APoints, AModulateColor: string; AGradientScaleX, AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX, AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,/vz48ODAgAD//Pjw48fOTP//////////////////////wP+A/wD+APwA+ADwAOAAwACAAAAAAAA')]
    [TestCase('2',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ABwAPAB8APwB/AP8B/wP/B/8P/x//P/8')]
    [TestCase('3',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,fz8fDwcDAQB/f39vR8fPzH9/f29Hx8/Mf/9//0//z/8D/wH/AP8AfwA/AB8ADwAHAAMAAQAAAAA')]
    [TestCase('4',  '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,/vz48ODAgAD//Pjx48fOTP/////////+//////////7/4P/A/4D/AP4A/AD4APAA4ADAAIAAAAA')]
    [TestCase('5',  '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ADwAfAD8AfwD/Af8D/wf/D/4f/D/4f/A')]
    [TestCase('6',  '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,fz8fDwcDAQB/f3/vx8fPzH9/f+/Hx8/Mf/9//9//z/4H/wP/Af8A/wB/AD8AHwAPAAcAAwABAAA')]
    [TestCase('7',  '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///9/f3//f3/////////////g/8D/gP8A/gD8APgA8ADwAPAA/////8')]
    [TestCase('8',  '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PDyf/AA8AHwA/AH8A/wH/A/8H/w//H//////8')]
    [TestCase('9',  '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8f/y//B/8D/wH/AP8AfwA/AB8ADwAPAA/////8')]
    [TestCase('10', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///9/f3//f3////////////vw/+D/wP+A/wD+APwA+ADwAPAA/////8')]
    [TestCase('11', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PDyf/AB8APwB/AP8B/wP/B/8P3x+fP//////8')]
    [TestCase('12', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8//z//D98H/wP/Af8A/wB/AD8AHwAPAA/////8')]
    [TestCase('13', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI////////////////////////AP4A/gD8APgA+ADwAOAA4ADgAP////8')]
    [TestCase('14', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+DHyP8ABwAPAB8AHwA/AH8AfwD/Af8H//////8')]
    [TestCase('15', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+f/yf8A/wB/AH8APwAfAB8ADwAHAAcAB/////8')]
    [TestCase('16', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI//////////7///////////8/gH8AfwD+APwA/AD4APAA4ADwAP////8')]
    [TestCase('17', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+DPyP8ADwAfAD8APwB/AP4A/gH8A/gH//////8')]
    [TestCase('18', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+f/y/8B/AD+AP4AfwA/AD8AHwAPAAcAD/////8')]
    [TestCase('19', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/3//9/fvz9/////////////gf4A/AD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('20', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8AfwA/AD8AfwD/AP8B/w//P/////////8')]
    [TestCase('21', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8H/wf/B/8H/wf/A/8D/wP/A/8H/wf/B/8')]
    [TestCase('22', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHx9/f2fnz8/f33//9/fvz9/f///////////4f+A/gD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('23', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z/fPz9//f/9v/8//3/8AfwA/AD8AfwD/A/8P/z////////////8')]
    [TestCase('24', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8P/w//D/8P/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('25', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/vz9/f////7+/v39/////////////5/+B/AH8A/wD/Af8B/wP/A/8H/w//D/8')]
    [TestCase('26', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8A/wB/AH8A/wD/Af8H/z////////////8')]
    [TestCase('27', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39//////////3/8P/w//D/8P/w//B/8H/wf/B/8H/w//D/8')]
    [TestCase('28', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/vz9/f////7+/v39////////////////B/wH8A/wD/Af8B/wP/A/8H/w//D/8')]
    [TestCase('29', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8A/wB/AH8A/wH/B/8f/3////////////8')]
    [TestCase('30', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39//////////3/8f/x//H/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('31', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7/fvz9/f///////////h/8B/AD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('32', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHz///+/n78/f////7+fvz9///////+//3/8A/wB/AD8AfwD/A/8H/x//P/////////8')]
    [TestCase('33', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8H/wf/B/8H/wf/B/8H/wf/B/8H/wf/D/8')]
    [TestCase('34', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7/fv79/f///////////5/+B/gD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('35', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHz///+/n78/f////7+fvz9///////+//3/8A/wB/AD8A/wH/B/8f/z////////////8')]
    [TestCase('36', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8P/w//D/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('37', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8P/w//D/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('38', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,fx8PBw8PHx//3+/n78/f3//f7+fvz9/f/////+//3/8P/w//D/8Pfw//D/8P/w//D/8P/w//D/8')]
    [TestCase('39', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,fx8PBw8PHx//3+/n7+/f3//f7+fv79/f////////3/8D/wD/AP8AfwD/Af8B/wf/A/8H/w//j/8')]
    [TestCase('40', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7/fvz9/f/////+//3/8B/wB/AD8AfwB/AP8B/wH/A/8D/wf/D/8')]
    [TestCase('41', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,fx8PBw8PHx//3+/n78/f3//f7+fvz9/f/////+//3/8H/wf/B/8H/wf/B/8H/wf/B/8H/wf/D/8')]
    [TestCase('42', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,Pw8HBw8PHz8/DwfHz8/f/z8PB9fPz9//P/8P/8//3/8B/wH/Af8B/wH/AP8B/wH/A/8D/wf/D/8')]
    [TestCase('43', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,f08HBw8PPz9/f2fnz8///39/d/fv7///f/9///////8R/xG/Gf8Z/xn/Gf8J/wn/C/8P/w//D/8')]
    [TestCase('44', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,/vz48ODAgAD//Pjw48fOTP//////////////////////wP+A/wD+APwA+ADwAOAAwACAAAAAAAA')]
    [TestCase('45', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ABwAPAB8APwB/AP8B/wP/B/8P/x//P/8')]
    [TestCase('46', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,fz8fDwcDAQB/f39vR8fPzH9/f29Hx8/Mf/9//0//z/8D/wH/AP8AfwA/AB8ADwAHAAMAAQAAAAA')]
    [TestCase('47', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,/vz48ODAgAD//Pjx48fOTP/////////+//////////7/4P/A/4D/AP4A/AD4APAA4ADAAIAAAAA')]
    [TestCase('48', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ADwAfAD8AfwD/Af8D/wf/D/4f/D/4f/A')]
    [TestCase('49', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,fz8fDwcDAQB/f3/vx8fPzH9/f+/Hx8/Mf/9//9//z/4H/wP/Af8A/wB/AD8AHwAPAAcAAwABAAA')]
    [TestCase('50', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///3/f3//d3///f9/f/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('51', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//u+HB44HJ////4cHjwc3////hwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('52', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwc3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('53', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ/////f3//d3////9/f/93/8P/B/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('54', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//u+HB44HJ////4cHjwc3////hwf/9//8//D/8P/w//D/8P/w/+D/wP+A/wAAAAAA')]
    [TestCase('55', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwc3///fhwf/B//8/8D/4P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('56', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/8AAAAAAAP//+ODAYADI///+/////97///7////////////////////////////////gHwAAAAA')]
    [TestCase('57', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wAAAAAAA///OODAYADL//+84MBgAM///7zgwPgH//////////////////////////+j/wAAAAA')]
    [TestCase('58', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wMAAAAAAP//O+DAYADI//+/4MBgANz//7/gwOP////////////////////////////4BwAAAAA')]
    [TestCase('59', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/+AAAAAAAP//+ODAYQDM///+/////97///7/////3/8//3//f//////////////////oPwAAAAA')]
    [TestCase('60', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wAAAAAAB///OODAYADP//+84MBgAM///7zgwPwX//////////////////7//v/8//jv8AAAAAA')]
    [TestCase('61', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wcAAAAAAP//P+DAYADI//+/4MBgAMz//7/gwO/w/////P/+//7////////////////8FwAAAAA')]
    [TestCase('62', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fw8HBw8PHx9/P+fHTw/f33//59fPj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('63', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('64', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPD9/f////1/8P/9//4P/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('65', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fw8HBw8PHx9/P+fHTw/f33//99fPz9/f////1//P/9//wP/g/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('66', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHDw/f3z//x9ePj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('67', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPD9/f////1/8P/9//oP+A/4D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('68', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fv39//f7/vx//f////AP+A/4D/gP8A/gD+APwA/AD4APgA8AA')]
    [TestCase('69', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fvn9//f7/vx/+f////AP+A/4D/gP8A/gD+APwA/AD8APgA8AA')]
    [TestCase('70', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f////gP/A/8D/gP+A/gD+APwA/AD8APgA8AA')]
    [TestCase('71', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fv39//f7/vx//f////AP+A/4D/gP8A/gD+APwA/AD4APgA8AA')]
    [TestCase('72', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78dvn9//f7/vx/+f////AP+A/4D/gP8A/gD+AP4A/AD8APAA8AA')]
    [TestCase('73', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///+gP5A/kD+AP6A/wD+AP4A/AD8APgA8AA')]
    [TestCase('74', '[claRed;0 claBlue;1],claWhite,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79fvz9/f/7/v1//P/9//AP/A/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('75', '[claRed;0 claBlue;1],claWhite,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvj9/f/7/v1/+P/9//AP/A/+D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('76', '[claRed;0 claBlue;1],claWhite,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//AP/A/+D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('77', '[claRed;0 claBlue;1],claWhite,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79fvz9/f/7/v1//P/9//AP/A/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('78', '[claRed;0 claBlue;1],claWhite,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P8/Hbw/f33+/z9dvj9/f/7/v1/+P/9//AP/A/+D/gP+A/wD/AP4A/AD8APgA8AA')]
    [TestCase('79', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//gP8A/yD/QP8A/4D/AP8A/AD8APgA+AA')]
    [TestCase('80', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//gP8A/yD/QP8A/4D/AP8A/AD8APgA+AA')]
    [TestCase('81', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,fx8PBw8PHx9/H+/n7y/f338f7+fvL9/f/x/v5/8v/9//AP+A/4D/gP8A/wD/AP4A/AD8APgA+AA')]
    [TestCase('82', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,fx8PBw8PHx9/H+/n7y/f338f7+fvL9/ff//v9//v3/8B+gB9AD8AfwD/AP8A/QH6A/UH6gPVB+o')]
    [TestCase('83', '[claRed;0 claBlue;1],claGreen,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,fx8PBw8PHx9/P8/Hbw/f33+/z9fvz9/ff//v1//v/99B6gB1AD8AfwD/AP8A9UHqo9VX6qvVV+o')]
    [TestCase('84', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,fx8PBw8PHx9/H+/n7y/f338f7+fvL9/f/x/v5/8v/9//AP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('85', '[claRed;0 claBlue;1],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,Pw8HBw8PHx9/f+fHzw/f339/58fPL9/f/3//x/8v/9//wP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('86', '[claRed;0.3 claNull;0.5 claBlue;0.7],claWhite,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,fw9HBw8PPz9/P8fHTw///38/9/dvL////z/39+8v///xoOHA8cDxwPEA8IDwAPEA8ADwAvAF8AI')]
    procedure TestFillRectGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY, AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean; const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////++Pj7//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,/8+Hh8////////fnz///////9+fP///////////////w//D/8P/w//////////////////////8')]
    procedure TestFillRectSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PH///Pjw8f+/f//8+Pj5//9///////////wD/AP8A/wD/AB8AHwAfAB/wH/Af8B//8')]
    procedure TestFillRectSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PD///Pjw8f+/P//8+Pj5/78///////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PH///Pjw8f+/f//8+Pj5//9///////////wD/AP8A/wD/AB8AHwAfAB/wH/Af8B//8')]
    procedure TestFillRectSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('2',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('3',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09N/3//Z/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('4',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09N/3//Z/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('5',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('6',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('7',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09N/3//Z/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('8',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09N/3//Z/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('9',  '300,300,1,2,-50,-100,60,0,0,200,200,1,true,0.98,fz8///////9//////////3////////////////////9///////////////////////////////8')]
    [TestCase('10', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('11', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('12', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('13', '300,300,1,2,-50,-100,30,0,0,200,200,1,true,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('14', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('15', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('16', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('17', '300,300,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M//z44ftH/kzwAOAAwADAAIAAgAAAAAAAAAAAAAAAAAA')]
    [TestCase('18', '300,300,1,-2,50,500,30,-200,-50,660,343,1,false,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M//z44ftH/kzwAOAAwADAAIAAgAAAAAAAAAAAAAAAAAA')]
    [TestCase('19', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,true,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M///3/8f/T/8f/x//P/8//3//f/////////////////8')]
    [TestCase('20', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,false,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M///3/8f/T/8f/x//P/8//3//f/////////////////8')]
    [TestCase('21', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('22', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('23', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,true,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('24', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,false,0.98,Hx8/P3////9/f39/f////39/f39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('25', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5///f/9x/8P///8AAwABAAEAA4AH4AfwD/wP/x//n/////8')]
    [TestCase('26', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,false,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5///f/9x/8P///8AAwABAAEAA4AH4AfwD/wP/x//n/////8')]
    [TestCase('27', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,true,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5////3//4f/n/////P////7//P/8P/gP+AfwAfAAYAAAAAA')]
    [TestCase('28', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,false,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5////3//4f/n/////P////7//P/8P/gP+AfwAfAAYAAAAAA')]
    [TestCase('29', '300,300,1,-2,-50,500,30,0,0,200,200,1,true,0.98,/////+fhwMD/////5+fOzP/////39+7s/////////////////////////3/+H/4H/AX4APwA8AE')]
    [TestCase('30', '300,300,1,-2,-50,500,30,0,0,200,200,1,false,0.98,/////+fhwMD/////5+fOzP/////39+7s/////////////////////////3/+H/4H/AX4APwA8AE')]
    [TestCase('31', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,true,0.98,/////+fhwMD/////5+fOzP/////39+78//////f37vwAAAAAAAAAAACAAeAD8AP8B/8H/w//D/8')]
    [TestCase('32', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,false,0.98,/////+fhwMD/////5+fOzP/////39+78//////f37vwAAAAAAAAAAACAAeAD8AP8B/8H/w//D/8')]
    [TestCase('33', '80,80,1,1,0,0,0,0,0,60,60,0,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('34', '80,80,1,1,0,0,0,60,0,60,60,1,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('35', '80,80,1,1,0,0,0,0,60,60,60,1,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('33', '80,80,1,1,0,0,0,0,0,60,60,0,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('34', '80,80,1,1,0,0,0,60,0,60,60,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('35', '80,80,1,1,0,0,0,0,60,60,60,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36', '80,80,1,1,0,0,10,0,0,60,60,0,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('37', '80,80,1,1,0,0,10,60,0,60,60,1,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('38', '80,80,1,1,0,0,10,0,60,60,60,1,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('39', '80,80,1,1,0,0,10,0,0,60,60,0,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('40', '80,80,1,1,0,0,10,60,0,60,60,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('41', '80,80,1,1,0,0,10,0,60,60,60,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestFillRectSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolidWithModulateColor(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,3wDB9f/NAMH/+PH1/8/Ozf/69///7+7f///3///v7t8wQfAj9iM/AbID///x4wAAAAAwQfYj9iM')]
    [TestCase('2',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,3wDB8f/JAMH/+PHx/89Ozf/69/v/727f///3///vbt8wYfYj9jP/N/7////x4zEBAABwY/Yj9jM')]
    [TestCase('3',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3wDB9f/NQMH/+PH1/8/Mzf/69///7+7f///////v7t8PAAGAFiAAAAAA//+/pwYADwAPAKWBxgM')]
    [TestCase('4',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3wDB9f/JAMH/8OH1/8lMyf/69///727b///3///vbtsQACABFiAKAAAA//9xwwAAAAAQAPAB1iM')]
    [TestCase('5',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB5/////Hhw8fv////8/Hvz//////////////MA8ADwAPAA+iPwEPAA////D/sJ/////8')]
    [TestCase('6',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGBgf///fHhw8fN///98fHvz/3///3x8f////8MMAAABAgcCD3MF8w/2D/8PPwMIAAAAAA')]
    [TestCase('7',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('8',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98fHvzfn/////////+f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,yQEBAff/////+fHh9///////9/v3///////3//f///8QBwAHFicABwAH//////////////////8')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,AQEBAf//////+fFh///////59Xn////////1+/////8wY/Yj9jP/M/77//////////////////8')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,yQEBAff////9+fHh9//////79/v3///////3//f///8ABwAHAAcABwAH//////////////////8')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,AQEBAf/////58eHh//////359fn//////f/1//////8ABwAHAAcAB/////////////////////8')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB//////Hhw8f/////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v//////////8')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGB/////fHhw8f////98fHvz/////3x8f////8OOAAABAg8CD3MP8w/3B/gAAAAAAAAAAA')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgB+AH4AfgB/////////////8')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGB/////fHhw8X////98fHvzf/////////////gB+AH4AfgB+AH4AfgB/////////////8')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,/85AAMDS8vP//nBgw9f+////d2P73/////////v///8AAAAAAAAICHgQcBjzCPvMD44/gA+40AM')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.4,0.98,/84AAMDS8vP//vBgw9f+////93P73/////////v///84wAgAAAAYGHgQ+BzzDPusX4//hP++/64')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,/85AAMDS+vP//nBgw9f+////d3P73///////+/////8XAAOAB4AHwD/AAOAjQC/IAoABgIBBwAM')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,/84AAMDS8vP//uDAw9b+////5/P73/////////v///8AAAAAAAAACDgAEBDzAPuIA4AHgIAhwAM')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////4Y8ADwAPAA8QDygPlA8CDwAPwB/////8')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGBgf///fHhw8fN///99fHz783//////////f/448yDwAPEM9wj+xP9k9+f3zv/n/////8')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///99fHz7cn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('25',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,//ExADDw9v///XFgc/f+////+Wp79///////+//3//8fggAAAAAAAAMAPwA/AP48/z4Bvgnvgf8')]
    [TestCase('26',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,//EwADDw9v//+fBgc/f+///7+Gp79//////////3//8fgwEBAAABAAcAfwD/QP98/z73vv//f/8')]
    [TestCase('27',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//ExADDw9v//+fFgc/b+///7+2p79/////////////8f8gBwAH4AfgDnAEoAjQA8ATwAMoABwAM')]
    [TestCase('28',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//EwADDw9v//+fDAc/b+///7+Mp79///////2//3//8PgAAAAAAAAAAAHwACADwsHz4APoABwA8')]
    [TestCase('29',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,///5gYGBgf////nhw8fP/////e/Hx9//////////////4+/D48PAA8ADwAPAA8Hj4APn3/////8')]
    [TestCase('30',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGBgf///fHhw8fN///9/e3Hx83///397d////8/4D/AL8ADYAAgACAAAAfgP8Af2AAAAAA')]
    [TestCase('31',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,///5gYGBgf////nhw8fP/////e/Hx8//////////7//gB+AH4AfAA8ADwAPAA8AH4Af///////8')]
    [TestCase('32',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///9/e3Hxc3/////////7f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,//ExADDw9v///XFgc/f+////+Wp79///////+//3//8fggAAAAAAAAMAPwA/AP48/z4Bvgnvgf8')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,//EwADDw9v//+fBgc/f+///7+Gp79//////////3//8fgwEBAAABAAcAfwD/QP98/z73vv//f/8')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//ExADDw9v//+fFgc/b+///7+2p79/////////////8fkgBwAH4AfgDnAEoAjQA8ATwAMoABwAM')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//EwADDw9v//+fDAc/b+///7+Mp79/////v4y3v3//+AAIAAgACAAIAAgACAAIAAgACAAIABwAc')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,///5gYGBgf////nhw8fP/////e/Hx9//////////////4+/D48PAA8ADwAPAA8Hj4APn3/////8')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGBgf///fHhw8fN///9/e3Hx83///397d////8/4D/AL8ADYAAgACAAAAfgP8Af2AAAAAA')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,///5gYGBgf////nhw8fP/////e/Hx8//////////7//gB+AH4AfAA8ADwAPAA8AH4Af///////8')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///9/e3Hxc3/////////7f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,/85AAMDS+vP//nBgw9f+////d3P73/////////v///8AAAAAAAAICHgQcBjzCPvMD44/gA+40AM')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.4,0.98,/84AAMDS8vP//vBgw9f+////93H73/////////v///84wAgAAAAYGHgQ+BzzDPusX4//hP++/64')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,/85AAMDS+vP//vBgw9f+////93P73///////+/////8XAAOAB4AHwD/AAOAjQC/IAoABgIBBwAM')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,/84AAMDS8vP//uDAw9b+////5/P73/////////v///8AAAAAAAAACDgAEACzAPuIA4ABgIABwAc')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////4Y8ADwAPAA8QDygPlA8CDwAPwB/////8')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.4,0.98,/4GBgYGBgf///fHhw8fN///99fHz783//////////f/448yDwAPEM9wj+xP9k9+f/zv/n/////8')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///99fHz7cn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('49',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,3wDB9f/NAMH/+PH1/8/Ozf/69///7+7f///3///v7t8wQfAj9iM/AbIC///x4wAAAAAwQfYj9iM')]
    [TestCase('50',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,zQAAAN1AAMH/+ODg30dMzf/69vrfb27f///3//9vbt8wYfYj9jP/N/7////x4zEBAABwY3Yj9jc')]
    [TestCase('51',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,3wDB9f/JAMH/+PH1/8/Mzf/69///7+7f///////v7t8PAAEAFgAAAAAA//+/owYADwAPAIGBxgM')]
    [TestCase('52',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,zQAAAN1AAMH98ODA3UAAwf/69trfbmbT///3299vZtMAAAAABAAAAAAA9f9hwQAAAACAAYAB5Ac')]
    [TestCase('53',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB5/////Hhw8fv////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v/D/8L/////8')]
    [TestCase('54',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////x8f////8MMAAABAg8CD3MF8w/3D/8PPwOMAAAAAA')]
    [TestCase('55',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/P/////8')]
    [TestCase('56',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('57',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,yQEBAff/////+fHh9///////9/v3///////3//f///8QBwAHFicABwAH//////////////////8')]
    [TestCase('58',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,AQEBAf//////+eHh///////59fn////////1+/////8wY/Yj9jP/M/77//////////////////8')]
    [TestCase('59',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,yQEBAff////5+fHh9//////79/v3///////3//f///8ABwAHAAcABwAH//////////////////8')]
    [TestCase('60',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,AQEBAf/////58eHB//////359dn//////f/13/////8ABwAHAAcAB/////////////////////8')]
    [TestCase('61',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB//////Hhw8f/////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v//////////8')]
    [TestCase('62',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////x8f////8OMAAABAg8CD3MP8w/3B/gAAAAAAAAAAA')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////////////gB+AH4AfgB+AH4AfgB/////////////8')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,/85AAMDS8vP//nBgw9f+////d2P73/////////v///8AAAAAAAAACHgQcBjzCPvMD44/gIu58AM')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.4,0.98,/8QAAADAEIH//ODgQ8dejf//5/N7z1+N//////v///04wAgAAAAYGHgQ+BzzDPusX4//hP+//68')]
    [TestCase('67',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,/84AAMDS8vP//vDgw9f+////9/P73///////+/////8XAAOAB4AHwC/AAOADQA/AAoAAAIBBwAM')]
    [TestCase('68',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,/8QAAEDQMIH/9ODAQNA0gf/35/N72T2p//////v5PekAAAAAAAAAAAgAAAADAAuIAACAAYAB4Ac')]
    [TestCase('69',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////4Y8ADwAPAA8QDygPhA8CD4AfwD/////8')]
    [TestCase('70',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////9fHz78/////////////448yDwAPEM9wj+xP9k9+f/zv/l/////8')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////9fHz78/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('73',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,3wBA8f/JAMH/+PDx/8/Ozf/69vv/7+7f///3+//v7t8wQPAj9iM/AZIC///x4wAAAAAwQfYj9iM')]
    [TestCase('74',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,ywAAAMEAAMH/+PDgw0dMzf/69vrTb27d///3//Nvbt0wYfYz9jf/N/7////x5zEBAABwY/Yj9jM')]
    [TestCase('75',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,3wBB8f/JAMH/+PHx/8/Mzf/69/v/7+7f////+//v7t8NAAAABgAAAAAA//+3IwQADgCFAeCD5Ac')]
    [TestCase('76',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,yQAAAAAAAMH5+PDgwAAAwf369vrSbmbT/f/3+9JvZtMAAAAABAAAAAAA8fshwQAAAACAAcAD5Ac')]
    [TestCase('77',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB5/////Hhw8fv////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v/D/8L/////8')]
    [TestCase('78',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////x8e////8MMAAABAg8CD3MF8w/3D/8HPgOMAAAAAA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gA8ADwAfgB+AH4AfgB+AH8A//P/////8')]
    [TestCase('80',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,yQEBAff/////+fHh9///////9/v3///////3//f///8QBwAHFicABwAH//////////////////8')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,gQEBAf//////+fHh///////59fn////////1+/////8wY/Yj9jP/M/77//////////////////8')]
    [TestCase('83',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,yQEBAff////5+fHh9//////79/v3///////3//f///8ABwAHAAcABwAH//////////////////8')]
    [TestCase('84',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,gQEBAf/////5+fHh//////359fn//////f/1//////8ABwAHAAcAB/////////////////////8')]
    [TestCase('85',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB//////Hhw8f/////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v//////////8')]
    [TestCase('86',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////////////OO8ADxAv8C/3P/8//39/j//////////8')]
    [TestCase('87',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('88',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////////////gB+AH4AfgB+AH4AfgB/////////////8')]
    [TestCase('89',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,/84AAEDS8PP//nBgQ9f+////d2N73/////////v///8AAAAAAAAACHgQcBjzCPvMC46/gMu78Ac')]
    [TestCase('90',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.4,0.98,78QAAAAAAIH//PDgw0dMjf//9/P7T03N//////v//f04wAgAAAAYGHgQ+BzzDPusX4//hL+//68')]
    [TestCase('91',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,/84AAEDS8vP//vDgQ9f+////9/N73//////////f//8CAAOAB4AHwAmAAOADAAPAAACAAcBD4Ac')]
    [TestCase('92',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,78QAAAAAAMP//PDgwAAAw///9/P7SQnr//////t5OesAAAAAAAAAAAgAAAABAAKAAACAAcAD4Ac')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////4Y8ADwAPAA8QDygPhA8CD4AfwD/////8')]
    [TestCase('94',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////9fHz78/////////////448yDwAPEM9wj+xP9k9+f/zf/n/////8')]
    [TestCase('95',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH8A////////8')]
    [TestCase('96',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////9fHz7+/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('97',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,3wBA8f/JAMH/+PDx/8/Ozf/69vv/7+7f///3+//v7t8wQPAj9iM/AYIC///x4wAAAAAwQfYj9ic')]
    [TestCase('98',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,ywAAAAAAAcH/+PDgw0ZNzf/69vrTbm/d///3+/Nub90wYfYz9jf/N/7////x5zEBAABwY/Yj9jM')]
    [TestCase('99',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,3wBA8f/JAMH/+PDx/8/Mzf/69vv/7+7f///////v7t8NAAAABgAAAAAA//+3IwQADgCFAcCD5Ac')]
    [TestCase('100', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,ywAAAAAAAMH7+PDgwAAAwf/69vrSbmbR///2/9JvZtEAAAAAAAAAAAAAAAAAAAAAAADAA+AH4Ac')]
    [TestCase('101', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB5/////Hhw8fv////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v/D/8L/////8')]
    [TestCase('102', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////x8e////8MOAAABAg8CD3MF8w/3D/8HPgOMAAAAAA')]
    [TestCase('103', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH8A//P/////8')]
    [TestCase('104', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8fHvz//////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('105', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,yQEBAff/////+fHh9///////9/v3///////3//f///8QBwAHFicABwAH//////////////////8')]
    [TestCase('106', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,gQEBAf//////+fHh///////59fn////////1+/////8wY/Yz9jP/M/77//////////////////8')]
    [TestCase('107', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,yQEBAff////5+fHh9//////79/v3///////3//f///8ABwAHAAcABwAH//////////////////8')]
    [TestCase('108', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,gQEBAf/////5+fHh//////359fn//////f/1//////8ABwAHAAcAB/////////////////////8')]
    [TestCase('109', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/+eBgYGB//////Hhw8f/////8/Hvz//////////////MA8ADwAPAA+iLwEPAA//v//////////8')]
    [TestCase('110', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////////////OO8ADxAv8C/3P/8//39/j//////////8')]
    [TestCase('111', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('112', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB//////Hhw8f/////8fHvz//////////////gB+AH4AfgB+AH4AfgB/////////////8')]
    [TestCase('113', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,/84AAEDS8PP//nBgQ9f+////d2N73/////////v///8AAAAAAAAACHgQcBjzCPvMC46/gMu/8Ac')]
    [TestCase('114', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.4,0.98,78QAAAAAgcP//PDgw0fNz///9/P7T83P//////v//f84wAgAAAAYGHgQ+BzzDPusX4//hP+//68')]
    [TestCase('115', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,/84AAEDS8PP//vDgQ9f+////9/N73//////////f//8CAAOAB4AHwAmAAOADAAPAAACAAcBD4Ac')]
    [TestCase('116', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,z8AAAAAAgcP/+PDgwACBw///9/P7SYnr///39/tLiesAAAAAAAAAAAAAAAAAAAAAAADAA+AH4Ac')]
    [TestCase('117', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////4Y8ADwAPAA8QDygPhA8CD8A/wD/////8')]
    [TestCase('118', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.4,0.98,/8OBgYGBw/////Hhw8fP////9fHz7+/////////////488yDxAPEM9wj+xP9k9+f7zf/n/////8')]
    [TestCase('119', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBw/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH8A////////8')]
    [TestCase('120', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////9fHz7+/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('121', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('122', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('123', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('124', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('125', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('126', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('127', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('128', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('129', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('130', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('131', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('132', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('133', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('134', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('135', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('136', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('137', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgB+AH4AfgB/////////////8')]
    [TestCase('138', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('139', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4A//v/////8')]
    [TestCase('140', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('141', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('142', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('143', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('144', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBwf////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('145', 'horse.webp,claWhite,Tile,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('146', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('147', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('148', 'horse.webp,claWhite,Tile,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('149', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('150', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('151', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('152', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('153', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('154', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('155', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('156', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('157', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('158', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgB+AH4AfgB/////////////8')]
    [TestCase('159', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AH4AfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('160', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('161', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('162', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('163', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gA8ADwAfgB+AH4AfgB+AH4A//P/////8')]
    [TestCase('164', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('165', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4A////////8')]
    [TestCase('166', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4A//P/////8')]
    [TestCase('167', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('168', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4A////////8')]
    [TestCase('169', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('170', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgB+AH4AfgB/////////////8')]
    [TestCase('171', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA8AH4AfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('172', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('173', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('174', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4Af///////8')]
    [TestCase('175', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4A//P/////8')]
    [TestCase('176', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('177', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4A////////8')]
    [TestCase('178', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4A//P/////8')]
    [TestCase('179', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('180', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBg/////Hhw8fP////9/Pz79/////////////gA8AHwAfAB+AH4AfgB+AH4A////////8')]
    [TestCase('181', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('182', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('183', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('184', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('185', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('186', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('187', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('188', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('189', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('190', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB5/////Hhw8fv////8/Pvz//////////////gB8ADwAfgB+AH4AfgB+AH4Af/v/////8')]
    [TestCase('191', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYGB//////Hhw8f/////8/Pvz//////////////AA8ADwAPgA+AH4AfgB/////////////8')]
    [TestCase('192', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYGBgf////Hhw8fP////9/Pz79/////////////gA+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('193', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYHD5/////Hhw8fv////8/Pv7//////////////wD+AH4AfgB+AH4AfgB/AP/B//f/////8')]
    [TestCase('194', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYHD//////Hhw8f/////8/Pvz//////////////wD+AH4AfgA+AH4AfgB/////////////8')]
    [TestCase('195', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+/BgYHB4/////Hhw8fv////9/Pz7+/////////////4D+AH4AfAA+AH4AfgB/AP+B////////8')]
    [TestCase('196', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////9/Pvz+/////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('197', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD//////Phw8f/////9/Pvz//////////////8P/gf8A/gB+AH8A/4H/////////////8')]
    [TestCase('198', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,///DgYHD5/////Phw8fv////9/Pzz+/////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('199', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////fP7//////////////+f/w//B/wD/AP/D/+f/5//v////////8')]
    [TestCase('200', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////fP7//////////////+f/w//B/wD/AP/D/+f/////////////8')]
    [TestCase('201', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////fT7//////////////+f/w//B/gB/gH+B/+P/5///////////8')]
    [TestCase('202', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////PP7/////////////////5//D/4H/gf/D/+f/////////////8')]
    [TestCase('203', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////PP7/////////////////5//D/4H/gf/D/+f/////////////8')]
    [TestCase('204', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////ff7/////////////////5//D/4H/wf/D/+f/////////////8')]
    [TestCase('205', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYHB5/////Hhw8fv////8/Pv7//////////////gD8AHwAfgB+AH4AfgB/AH/Af/P/////8')]
    [TestCase('206', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eBgYHB//////Hhw8f/////8/Pvz//////////////AD8AHwAfgB+AH4AfgB/////////////8')]
    [TestCase('207', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++BgYHB4f////Hhw8fv////9/Pz7+//////////7//gD8AHwAfAB+AH4AfgB/AH+Af///////8')]
    [TestCase('208', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgcHh5f////Phw+fv////9/Pv7+//////////7//AP8AfwA/gB+AH8Af4B/wH/gf/N/////8')]
    [TestCase('209', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgYHh//////Phw+f/////9/Pv7//////////////AP8AfwA/AB+AH8Af4A/////////////8')]
    [TestCase('210', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++HgYHh8f////fhw+f/////9/Pz7//////////////gP8AfwA/AB+AD8AP4A/wH/gP/n/////8')]
    [TestCase('211', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg8Hh8f////fjw+f/////9/fP7//////////////Af8B/wD/AD/gH/Af+A/8H/wf/J/////8')]
    [TestCase('212', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg8Hh//////fjw+f/////9/fP7//////////////Af8B/wD/AD/gD/Af+A/////////////8')]
    [TestCase('213', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg8Hh8f////fjw+f///////fT7//////////////gf8B/wD/AD/gD/AP+A/8D/wP/B/////8')]
    [TestCase('214', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg+Hx8f////fj4/f/////9/Pv9//////////////A/8B/wD/AH/wD/gf+A/8H/wf/J/////8')]
    [TestCase('215', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/4+Hg8Hx//////fjw/f/////9/PP///////////////A/8B/wD/AH/wD/AP+A/////////////8')]
    [TestCase('216', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg8Hx8f////fjw/f///////fT///////////////g/8B/wD/AH/wD/gP/A/8D/wP/B/////8')]
    [TestCase('217', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////9/Pvz//////////////4H/AP4AfgB+AH4AfwD/gf/D/+f/////8')]
    [TestCase('218', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD//////Phw8f/////9/Pvz//////////////4H/AP4AfgA+AH4AfwD/////////////8')]
    [TestCase('219', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+/DgYHD5/////Phw8fv////9/Pzz+/////////////4H/AP4AfAA+AD4AfwD/gf/D////////8')]
    [TestCase('220', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////9/Pvz+/////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('221', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD//////Phw8f/////9/Pvz//////////////8P/gf8A/gB+AH8A/4H/////////////8')]
    [TestCase('222', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,///DgYHD5/////Phw8fv////9/Pzz+/////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('223', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////PP7//////////////+f/5//D/4D/gP/n/+f/////////////8')]
    [TestCase('224', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f///////PP7//////////////+f/5//D/4D/gP/n/+f/////////////8')]
    [TestCase('225', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f//////+ff7//////////////+f/5//D/wD/wP/j/+f/////////////8')]
    [TestCase('226', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8P///////fjw///////9/Pv/////////////////////n/8P/w//3////////////////8')]
    [TestCase('227', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8P///////fjw///////9/Pv/////////////////////n/8P/w//3////////////////8')]
    [TestCase('228', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9+/r5////////////////////n/8P/w//n////////////////8')]
    [TestCase('229', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgYHB5/////Phw8fv////9/Pvz//////////////gH8APwAfgB+AH4AfwB/gH/Af/N/////8')]
    [TestCase('230', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgYHB//////Phw8f/////9/Pvz//////////////AH8APwAfgB+AH4AfwB/////////////8')]
    [TestCase('231', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,/++DgYHB4f////Phw8fv////9/Pzz+//////////7//gH8APwAfAB+AD4APwA/gH/AP///////8')]
    [TestCase('232', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgcHh5f////Phw+fv////9/Pv7+//////////7//AP8AfwA/gB+AH8Af4B/wH/gf/N/////8')]
    [TestCase('233', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/+eDgYHh//////Phw+f/////9/Pv7//////////////AP8AfwA/AB+AH8Af4A/////////////8')]
    [TestCase('234', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/++HgYHh8f////fhw+f/////9/Pz7//////////////gP8AfwA/AB+AD8AP4A/wH/gP/n/////8')]
    [TestCase('235', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg+Hx8f////fj4/f/////9/Pv///////////////A/8B/wH/AH/wH/gf+A/8H/wf/J/////8')]
    [TestCase('236', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/4+Hg8Hx//////fjw/f/////9/PP///////////////A/8B/wH/AH/wD/gP+A/////////////8')]
    [TestCase('237', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/++Hg8Hx8f////fjw/f///////fb///////////////g/8B/wH/AH/wD/gP/A/8D/wP/B/////8')]
    [TestCase('238', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/4+Hg+Hx8f////fj4/f/////9/Pv9//////////////A/8D/wP/Af/4D/wP/A/8H/wf/I/////8')]
    [TestCase('239', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/4+Hg+Hx//////fj4/f/////9/Pv///////////////A/8D/wP/AP/4D/wP/A/////////////8')]
    [TestCase('240', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/++Hh+Hx8f////fn4/f/////9//j///////////////g/8D/wP/Af/4D/wP/A/8D/wP/B/////8')]
    [TestCase('241', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('242', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('243', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('244', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('245', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('246', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('247', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('248', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('249', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('250', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('251', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('252', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('253', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('254', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('255', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('256', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('257', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('258', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('259', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('260', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('261', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('262', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('263', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('264', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('265', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('266', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('267', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('268', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('269', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('270', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('271', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('272', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('273', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('274', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('275', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('276', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('277', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('278', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('279', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('280', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('281', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('282', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('283', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('284', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('285', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('286', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('287', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('288', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('289', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('290', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('291', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('292', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('293', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('294', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('295', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('296', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('297', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('298', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('299', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('300', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('301', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('302', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('303', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('304', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('305', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('306', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('307', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('308', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('309', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('310', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('311', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('312', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('313', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('314', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('315', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('316', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('317', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('318', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('319', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('320', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('321', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('322', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('323', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('324', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('325', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('326', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('327', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('328', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('329', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('330', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('331', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('332', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('333', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('334', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('335', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('336', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestFillRoundRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,3wDB9f/NQOH/ePH1/8/M7f9+9///7+7///7////v7/8fACGBFgAKAAAA////JxYADwAfgKGB1iI')]
    [TestCase('2',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,3wDB9f/JAOX/cOH1/8vM7f969///7+7////3///v/v+wYfYj9jf/I/5////x47EBUCKwVfYr9jc')]
    [TestCase('3',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3wDB9f/NQMH/ePH1/8/Mzf969///7+7f//7////v7t8fAAGAFiAKAAAA////5xcADwAfgPWBVgI')]
    [TestCase('4',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3wDB9f/JQOX/cPH1/8/M7f969///7+7///73///v7v9wYfYz9jf/N/7////x5zEBAABwY3YyNjQ')]
    [TestCase('5',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3/393////0/+E/8Bfgc6F36X9ov3BP4P/xf7qIVQAI')]
    [TestCase('6',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s8HBo4PN///30dHvy93///ff8f////9eel56XnpfGn/aX+pf+n/+PvwsNA5wAAA')]
    [TestCase('7',   'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+Ef8Bfhc6H36X9o/3Dv4P/xf+qAVQAI')]
    [TestCase('8',   'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s8HBo4HJ///38dHvyd3///f70f/9//9f+h/4X/pf+l/6X/of+H/+P/gsFApQAAA')]
    [TestCase('9',   'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,3kDE9OAAAAD/ePT048fMzP9+9v73x+79//7///////1/4Pew9jD/eP74//wAAAAAgAFAAqAFUAo')]
    [TestCase('10',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,AQEBAcP///87ceHhw/////959fnX///////1+f/////5+P54//j/+P/4//wAAAAAAABAAqAFQAI')]
    [TestCase('11',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,3kDE9OAAAAD/ePT048bMzP969v73xu79//7////+//0/4Pew9jD/eP74//wAAAAAAABAAqAFQAI')]
    [TestCase('12',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,AQEBAeP///85cfHh4/////159fn3//////3/+f//////+P/4//j/+P/4//wAAAAAAAAAAAAAAAA')]
    [TestCase('13',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/3/3/7//0//H/8Pfh87H3+f94/3BAooAVQCqAFUAo')]
    [TestCase('14',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGB////s8HBo4P////30dHv3///////9f////9+/l/6X/p/3n/+f/5//gAAgAFAAqAFUAo')]
    [TestCase('15',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//H/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('16',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGB////s8HBo4H////38dHvzf////f/8f/9//9//h/6X/p/+n/+f/5//gAAAAAAAAAAAAA')]
    [TestCase('17',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,/85AAMLS+nL//vDgw9f+/v//9/P73/////////////8fgAeAB4AHwC/gAOCjQEfICoAVgAJVQAM')]
    [TestCase('18',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,0.85,0.98,/84AAMLS+vf//uDAw9f+////5/Pz3/////////v///+owAAACAAYGHgQ+BzzCvusX46/lV+675U')]
    [TestCase('19',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,/85AAMDS+nL//nDgw9f+/v//d/P73/////////////8fgAeAB4AHwD/gAOAzQC/IC4APgAJAAAA')]
    [TestCase('20',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0,0,1,1,1,0.98,/84AAMLS+vP//uDgw9f+////5/P73/////////v///84wAgACAAYGHgY+BzzjPus/4//hH++P7w')]
    [TestCase('21',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,AH5sAGR0PAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/6H/Av8h/gW3A9kF/arz10LqBVUAo')]
    [TestCase('22',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s8HBo4PN///35fHzy93///fv8f////99+l74Xfpc+lw4f7p/+H++P/g4FA/wAAA')]
    [TestCase('23',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/wW3A9kF/aPzxwLqB1QAI')]
    [TestCase('24',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s8HBo4HJ///35fHz6c3///fv8f/9//9/+h/4X/pf+F/4X/of+F/+H/goFAvQAAA')]
    [TestCase('25',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,//ExADDw9n//+fHgc/f+///7++p79////////v////9X/gB8AH4AfgD/AH8AjwA9ARUAKoAFwAo')]
    [TestCase('26',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,//EwADDw9v//+fDAc/f+///7/Mp79///////+//3//+/gwECAAAAAAcAfwD/QP98/z6vv1//L/8')]
    [TestCase('27',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//ExADDw9n//+XHg8/b+///7++r79/////////////8f/gB8AH4AfgD/AW8AjwA9Ab0ANgAAAAI')]
    [TestCase('28',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//EwADDw9v//+fDgc/b+///7+Op79//////////3//+fgwEBAAABAAcAfwD/QP98/z73vn/+P/w')]
    [TestCase('29',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,AH54eAAYOAAzfvj4Ixv+zHN//P7nn/79c3/8/+////0/fD/8P7xn/AD8ADxgXAf+O9R0/qJFQAI')]
    [TestCase('30',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s8HBo4PN////3c33x83////f/f////9/9n/Cf/Bf8l/iX/pf+l/iP+g/2A/wAAA')]
    [TestCase('31',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,AH54OAAYOAAzfvj4Ixv+zHN//P7nn/79c3/8/v////0/fH/8P/xD/AD8ADwAXAf+P9Q1/AJYAAA')]
    [TestCase('32',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s8HBo4HN////zc3nxc3////P3f/9//9/+j/4f/hf+l/6X/of+F/4H/gr0AvQAAA')]
    [TestCase('33',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,//ExADDw9n//+fHgc/f+///7++p79////////v////9X/gB8AH4AfgD/AH8AjwA9ARUAKoAFgAo')]
    [TestCase('34',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,//EwADDw9v//+fDAc/f+///7/Mp79///////+//3//+/gwECAAAAAAcAfwD/QP98/z6vv1//L/8')]
    [TestCase('35',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//ExADDw9n//+XHg8/b+///7++r79/////////////8f/gB8AH4AfgD/AW8AjwA9Ab0ANgAAAAI')]
    [TestCase('36',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,//EwADDw9v//+fDgc/b+///7+Op79//////////3//+fgwEBAAABAAcAfwD/QP98/z73vn/+P/w')]
    [TestCase('37',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,AH54eAAYOAAzfvj4Ixv+zHN//P7nn/79c3/8/+////0/fH/8P7xn/AD8ADxAXAf+O9R0/qJFQAI')]
    [TestCase('38',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s8HBo4PN////3c33x83////f/f////9/9n/Cf/Af8h/iX/of+l/iP+g/3A/wAAA')]
    [TestCase('39',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,AH54OAAYOAAzfvj4Ixv+zHN//P7nn/79c3/8/u////0/fD/8P/xD/AD8ADwAXAf+P9Q1/AJYAAA')]
    [TestCase('40',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s8HBo4HN////zc3nxc3////P3f/9//9/+j/4f/hf+l/6X/of+F/4H/gr0AvQAAA')]
    [TestCase('41',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,/85AAMLS+nL//vDgw9f+/v//9/P73/////////////8fgAeAB4AHwC/gAOAjQEfIC4BVgAJVQAM')]
    [TestCase('42',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,0.85,0.98,/84AAMLS+vf//uDAw9f+////5/Pz3/////////v///+owAAACAAYGHgQ+BzzCvusX46/lV+675Q')]
    [TestCase('43',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,/85AAMLS+nL//nDgw9f+/v//d/P73/////////////8fgAOAB4AHwD/AAOAzQC/IC4APgAJAAAA')]
    [TestCase('44',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0,0,1,1,1,0.98,/84AAMLS+vP//uDgw9f+////5/P73/////////v///84wAgACAAYGHgQ+BzzDPus34//hH++P7w')]
    [TestCase('45',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,AH5sAGR0PAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/A/8B/wG3I9EF/ar710LqA1UAo')]
    [TestCase('46',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s8HBo4PN///35fHzy83///fv8f////98+l76Xfpc+lw6f7p/+n++H/g4FA/wAAA')]
    [TestCase('47',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/An8B/wG3I9kF/aP7x0rqJ1QAI')]
    [TestCase('48',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Round,True,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s8HBo4HN///35fHz6c3///fv8f/9//9/+h/4X/pf+l/6X/of+F/+H/goFAvQAAA')]
    [TestCase('49',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,3wDB9f/NQOH/OPH1/8/M7f9+9///7+7///7////v7/8fACGBFiAKAAAA////ZxcADwAfgCWAFiA')]
    [TestCase('50',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,3QDB4f/JAMH/cOHh/8tMzf969/v/727f//73+//vft+wYfYj9jf/J/5////x47EBUCKwVfYrdjY')]
    [TestCase('51',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,3wDB9f/NQMH/OPH1/8/Mzf969///7+7f//7////v798fACGBFiAOAAAA////5xcADwAfgLWAFiI')]
    [TestCase('52',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,3QBA4f9JAMH9cODh/0kAyf169vv/b2bb//73//9vZtswYfYz9jP/N/7////x5zEBAABwY/YjdjI')]
    [TestCase('53',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3/393////0/+H/8Bfgc6H36X9ov3DP4H/xf+qIVQAI')]
    [TestCase('54',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///30dHvy9////ff8e/73/9+fl76XnpfGn/eX+5f+n/+HvgMMA5wAAA')]
    [TestCase('55',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHN/9/N/f//tc3//93////0/+Ef8Bfhc6H36X9o/3Dv4P/xf8qAVQAI')]
    [TestCase('56',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///38dHvy9////f70e/73/9/+h/4X/pf+n/6X/of+H/+H/gMMApQAAA')]
    [TestCase('57',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,3kDE9OAAAAD/ePT048fMzP9+9v73x+79//7///////1/4Pew9jD/+P74//wAAAAAgAFAAqAFUAo')]
    [TestCase('58',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,gQEBAcP///+7ceHBw/////959dnX///////1+f/////5+P54//j/+P/4//wAAAAAAABAAqAFQAI')]
    [TestCase('59',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,3kDE9OAAAAD/ePT048bMzP969v73xu79//7////+//0/4Pe09jD/eP74//wAAAAAgAFAAqAFUAo')]
    [TestCase('60',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,gQEBAcP///+5ceHBw/////159dnX//////3/+f//////+P/4//j/+P/4//wAAAAAAAAAAAAAAAA')]
    [TestCase('61',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3//93/7//1//n/8Pfh87H3+f94/3FAqoAVQCqgVUAo')]
    [TestCase('62',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGB////88HBo4P////30dHv3///////9f////9+/l/6X/5/3n/+f/5//gAAgAFAAqAFUAo')]
    [TestCase('63',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//H/8Pfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('64',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB////88HBo4P////38dHvz/////f/9e////9//h/6X/p//n/+f/5//gAAAABAAqAFQAI')]
    [TestCase('65',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,/85AAMLS+vP//vDgw9f+////9/P73/////////////8fgA+AB4AHwD/gAOCjQE/IC4BViApUAAA')]
    [TestCase('66',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,0.85,0.98,/8wAAEDSMPP//ODAQ9N8////5/Nz233///////v/ff+owAAACAAYGHgQ+BzzCvusX46/ld+7b5Y')]
    [TestCase('67',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,/85AAMDS+vP//vDgw9f+////9/P73/////////////8/gAeAB4AHwD/gAODzQK/IC4AfgIJBAAI')]
    [TestCase('68',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0,0,1,1,1,0.98,/8wAAEDScPP//ODAQdZ0+///5/N73337//////v//fs4wAgACAAYGHgQ+BzzDPus34//hL+9f74')]
    [TestCase('69',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,AH5sAGR0fAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/6H/A/8h/wW3A9kF/arz1UqqBVUAo')]
    [TestCase('70',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///35fHzy9////fv9f/73/9/+l74Xfpe+lw4f7p/+H++H/hKEq/1QAI')]
    [TestCase('71',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/An8h/we3A9kF/aP7xUKqB1QAI')]
    [TestCase('72',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///35fHz68////fv8f/73/9/+l/4X/pf+F/4f/pf+H/+H/gIEA/wAAA')]
    [TestCase('73',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,3gDB9f/NQGT/OMH1/8/M7P9+9///7+7+//7////v7/4fACGBFiAKAAAA////ZxcADwAfgCWAFiA')]
    [TestCase('74',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,3wBA4f9IAMH/cMDh/0pEzf969vv/bmbd//73+//+dv+wcfZz9jf/N/7////x47kBUCpwdXYqPzA')]
    [TestCase('75',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,30DB9f/NQGD/eMH1/8/M7P969///7+7+//7////v7/4fAGGBFiA+AAAA////5xcADwAfgDWAFiA')]
    [TestCase('76',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,zwAAQP9AAMH/cODA/0AAyf969tr/bmbZ//r3/v/udvnwY/Z397//f//////x9zGhEDBwYjY0Fzg')]
    [TestCase('77',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7tc/7393////0/+H/8Bfgc6H36X9ov3BP4T/BP+qIVQAI')]
    [TestCase('78',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///z0dHvy/////Pf8f/7//9+fl76XnpfGn/eX+pf+n/+PvwcOA5wAAA')]
    [TestCase('79',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7tc/7/93////0//n/8Bfhc6H36X9o/3Dv4D/hP+qAVQAI')]
    [TestCase('80',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///z8dHvy/////P70f/7//9//h/4X/pf+n/6X/of+H/+L/wcGApQAAA')]
    [TestCase('81',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,3kDE9OIAAAD/eMT048fMzP9+9v73x+79//7///////1/4Pew9jD/+P74//wAAAAAgAFAAqAFUAo')]
    [TestCase('82',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,gQEBAcP///+5ccHBw/////159dnX///////1+f/////5+P54//j/+P/4//wAAAAAAABAAqAFQAI')]
    [TestCase('83',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,3kDE9OAAAAD/eMT048fMzP969v73x+79//7///////1/4Pe09jD/eP74//wAAAAAgAFAAqAFUAo')]
    [TestCase('84',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,gQEBAcP///+5ceHBw/////159dnX//////3/+f//////+P/4//j/+P/4//wAAAAAAAAAAAAAAAA')]
    [TestCase('85',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7/93/77/1//n/8Pfh97H3+f94/3FEqoAVQCqgVUAo')]
    [TestCase('86',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGB////88HBo4P////z0dHv3/////v/9f////9+/l/6X/p/3n/+f/5//gAAgAFAAqAFUAo')]
    [TestCase('87',  'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7393/77/0//n/8Pfh87H3+f94/3BAooAVQCqAFUAo')]
    [TestCase('88',  'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB////88HBo4P////z8dHvz/////P/8f////9//h/6X/p//n/+f/5//gAAAAAAAAAAAAA')]
    [TestCase('89',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,/85AAMLS+nb//sDAw9f+/v//5/P73/////////////8fgA+AB4AHwD/gAOCjQE/IC4BViCpUAAA')]
    [TestCase('90',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,0.85,0.98,/8wAAEDSsPP//MDAQ9O0////5/Nz273///////v//f+o1RhKKAQYGPgY+Bzzjvu8X4//lH++P7A')]
    [TestCase('91',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,/85AAMLS+nb//sDAw9f+/v//5/P73/////////////8/gA+AB4AHyD/gEODzQO/IC4AfgAZwAAA')]
    [TestCase('92',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0,0,1,1,1,0.98,/8QAAEDQsMP//MDAQdK0y///5/N7273r/////3v//f+4+DhYOEwYPPgc/hz7nvu+/59/nj+8H/g')]
    [TestCase('93',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,AH5sAGR0fAAzfuzAZ3f8zHP+7vJ3f/79c/7+93f///0/+D/6H/A/8h/wW3A9kF/arzVUqqBVUAo')]
    [TestCase('94',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///35fHz69////fv8f/7//99+l74Xfpc+lw4f7p/+H++L/QYGA/wAAA')]
    [TestCase('95',  'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f8zHP+7vJ3f/7dc/7u93f///0/+D/4H/An8h/we3A9kF/aL7RUKqB1QAI')]
    [TestCase('96',  'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///35fHz68////fv8f/7//9/+h/4X/pf+F/4X/of+H/+L/QYGAvQAAA')]
    [TestCase('97',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,XkDB9f/NQGR/cMH1/8/M7H9+9///7+7+f/7////v7/4fgCGBFiAuAAAA////ZxcADwAfgCWAFiA')]
    [TestCase('98',  'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,3wBA4f9IAMH/cMDh/0pEzf969vv/bmbd//73+//+dv+wdfZz97//P/7////x57mFUCowdTYoPzA')]
    [TestCase('99',  'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,XkDB9f/NQGR/cMH1/8/M7H969///7+7+f/7////v7/4fAGGBFiA+AAAA////5xcBDwAfgDWAFiA')]
    [TestCase('100', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,zwAAQP9AAMH/cODA/0AAyf969tr/bmbZ//r3+v/+f/nwc/5397//f//////x97nxcfEwdD5wF7g')]
    [TestCase('101', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7tc/7393////0//H/8Bfgc6H36X9o/3Dv4j/FP+qIVUAo')]
    [TestCase('102', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///z0dHvy/////Pf8f/7//9eel76XnpfGn/eX+5f+n/+LvQcOA5wAAA')]
    [TestCase('103', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7Nc/7/93////0//H/8Bfhc6H36X9o/3Dv4H/BP8qAVQAI')]
    [TestCase('104', 'horse.webp,claRed,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///z8dHvy/////P70f/7//9f+h/4X/pf+n/6X/of+H/+L/QcGApQAAA')]
    [TestCase('105', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,3kDE9OIAAAD/cMT048fMzP9+9v73x+79//7///////1/4Pew9jD/+P74//wAAAAAgAFAAqAFUAo')]
    [TestCase('106', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,gQEBAcP///+5ccHBw/////159dnX///////1+f/////5+P54//j/+P/4//wAAAAAAABAAqAFQAI')]
    [TestCase('107', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,3kDE9OIAAAD/cMT048fMzP969v73x+79//7///////1/4Pe09jD/eP74//wAAAAAgAFAAqAFUAo')]
    [TestCase('108', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,gQEBAcP///+5ceHBw/////159dnX//////3/+f//////+P/4//j/+P/4//wAAAAAAAAAAAAAAAA')]
    [TestCase('109', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7/93/77/1//n/8Pfh97H3+f94/3FEqoAVQCqgVUAo')]
    [TestCase('110', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGB////88HBo4P////z0dHv3/////v/9f////9+/l/6X/5/3n/+f/5//gAAgAFAAqAFUAo')]
    [TestCase('111', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7/93/77/1//n/8Pfx87H3+f94/3FAqoAVQCqgVUAo')]
    [TestCase('112', 'horse.webp,claRed,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGB////88HBo4P////z8dHvz/////P/9f////9//h/6X/p//n/+f/5//gAAAABAAqAFQAI')]
    [TestCase('113', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,fs5AAMLS+nZ//sDAw9f+/n//5/P73///f/////////8/gA/AB4AHwD/gEOCjQE/IC4RXiCpUAAA')]
    [TestCase('114', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,0.85,0.98,/8wAAEDSsfP//MDAQ9O1////5/Nz273///////v//f+49RhKKAQYPPgc/Bz7jvu9X4+/lB+4P/w')]
    [TestCase('115', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,fs5AAMLS+nZ//sDAw9f+/n//5/P73///f/////////8/gA+AB4AHyD/gEODzQO/IC4AfgAdwAAA')]
    [TestCase('116', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0,0,1,1,1,0.98,/8QAAEDQsMP/9MDAQdK0y//35/N7273r/////3v//f+8/Dh+fE44Pvge/h7/nvu+/58//B+4H/g')]
    [TestCase('117', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,AH5sAGR0PAAzfuzAZ3f8zHP+7vJ3f/79c/7+93f///0/+D/6H/Av8h/wW3A9kF/arzVEKqBVUAo')]
    [TestCase('118', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBo4PP///z5fHzy/////Pv9f/7//99+l74Xfpc+lw4f7p/+H++L/RaGq/1QAI')]
    [TestCase('119', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f8zHP+7vJ3f/7dc/7u93f///0/+D/4H/Av8h/we3A9kF/aL7RUKqB1QAI')]
    [TestCase('120', 'horse.webp,claRed,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBo4PP///z5fHz6+////Pv8f/77/9/+h/4X/pf+F/4X/of+H/+L/QYGAvQAAA')]
    [TestCase('121', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('122', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('123', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('124', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('125', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('126', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('127', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('128', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('129', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('130', 'horse.webp,claWhite,Tile,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('131', 'horse.webp,claWhite,TileOriginal,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('132', 'horse.webp,claWhite,TileStretch,0,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('133', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+EXsBfhc6H36X9oP3Bv4P/xf+qAVQAI')]
    [TestCase('134', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//H/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('135', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/wW3A9kF/aPzxQKqB9QAI')]
    [TestCase('136', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+EfsBfhc6H36X9oP3Bv4P/xf+rAVQAI')]
    [TestCase('137', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393////0//H/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('138', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPzxQLqB9QAI')]
    [TestCase('139', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHN/9/N/f//tc3//93////0/+kfsBfhc6H36X9oP3Dv4P/hf/rAVQAI')]
    [TestCase('140', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('141', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPzxwLqB9QAI')]
    [TestCase('142', 'horse.webp,claWhite,Tile,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHN/9/N/f//tc3//93////0/+Ef8Bfhc6H36X9o/3Dv4H/hf/rAVQAI')]
    [TestCase('143', 'horse.webp,claWhite,TileOriginal,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//H/8Pfh87H3+f94/3BAooAVQCqAFUAo')]
    [TestCase('144', 'horse.webp,claWhite,TileStretch,0.06,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0NAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPzxwLqB9QAI')]
    [TestCase('145', 'horse.webp,claWhite,Tile,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('146', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('147', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('148', 'horse.webp,claWhite,Tile,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('149', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('150', 'horse.webp,claWhite,TileStretch,0.15,0.15,,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('151', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('152', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('153', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('154', 'horse.webp,claWhite,Tile,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('155', 'horse.webp,claWhite,TileOriginal,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('156', 'horse.webp,claWhite,TileStretch,0.15,0.15,,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('157', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+EfsBfhc6H36X9of3Dv4P/x/+qAVQAI')]
    [TestCase('158', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//H/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('159', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/wW3A9kF/aPzxQLqB1QAI')]
    [TestCase('160', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHN/9/N/f//tc3//93////0/+Ef8Bfhc6H36X9o/3Dv4P/x/8qAVQAI')]
    [TestCase('161', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393/7//0//H/8Pfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('162', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/An8h/we3A9kF/aPzxQKqB1QAI')]
    [TestCase('163', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7tc/7/93////0//l/8Bfhc6H36X9o/3Dv4P/h/+qAVQAI')]
    [TestCase('164', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7393/77/0//n/8Pfh87H3+f94/3BAooAVQCqAFUAo')]
    [TestCase('165', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f8zHP+7vJ3f/79c/7u93f///0/+D/4H/Av8h/we3A9kF/aP7RUqqB1QAI')]
    [TestCase('166', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp+ZgAzfubie3/uzHP+9/N/f/7tc/7/93////0/+H/8Bfhc6H36X9o/3Dv4P/B/8qAVQAI')]
    [TestCase('167', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP+9/N/e+79c/7/93/77/1//n/8Pfx87H3+f94/3FAqoAVQCqgVUAo')]
    [TestCase('168', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f8zHP+7vJ3f/79c/7u93f///0/+D/4H/Av8h/we3A9kF/aP7RUqqB1QAI')]
    [TestCase('169', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('170', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('171', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/wW3A9kF/aPzxQLqB1QAI')]
    [TestCase('172', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kfsBfhc6H36X9oP3Dv4P/x/8qAVQAI')]
    [TestCase('173', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vOzHN/9/N/e+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('174', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/wW3A9kF/aPzxQKqB1QAI')]
    [TestCase('175', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHP/9/N/f/7tc///93////0/+kf8Bfhc6H36X9o/3Dv4P/h/+qAVQAI')]
    [TestCase('176', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP/9/N/e+79c//393//7/0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('177', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f8zHP/7vJ3f/79c//u93f///0/+D/4H/An8h/we3A9kF/aP7RUKqB1QAI')]
    [TestCase('178', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AD4kInp+ZgAzPuTie3/uzHO/9/N/f/7tc7//93////0/+kf8Bfhc6H36X9o/3Dv4P/B/8qAVQAI')]
    [TestCase('179', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3vMzHP/9/N/e+79c//393//7/0//n/8Pfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('180', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f8zHP/7vJ3f/79c//u93f///0/+D/4H/Av8h/we3A9kF/aP7RUqqB1QAI')]
    [TestCase('181', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('182', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('183', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('184', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('185', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('186', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('187', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('188', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('189', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('190', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4kInp+ZgAzfuTie3/uzHN/9/N/f//tc3//93////0/+kXsBfhc6H36X9oP3Bv4P/x/+qAVQAI')]
    [TestCase('191', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH4mInp6AAAzfubie3/OzHN/9/N/f+79c3/393////0//n/8Lfh87H3+f94/3BAogAFAAqAFUAo')]
    [TestCase('192', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AH5sAGR0dAAzfuzAZ3f+zHN/7vJ3f/79c3/u93f///0/+D/4H/AH8h/gW3A9kF/aPz5QKqB9QAI')]
    [TestCase('193', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADwkYnp8LAAzPOTie3/szHP89vN///ztc/7293///+0f+D/8Pfg87D38H9wf3A/4r/VX6qAFUAo')]
    [TestCase('194', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADwkYno4AAAzPOTiezvMzHP89vN/u+79c/7+93/77/1f+j/8P/w/7D38P9wf3FGqoAVQCqgVUAo')]
    [TestCase('195', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADwsIGR0OAAzPOzgZ3f8zHP87vJ3//z9c/7u93f///0f+D/8P/h/8B/wG3A9lB/Yv/1V6qAFUAo')]
    [TestCase('196', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PACB4/u5ADw/MMHj+7vM/H+61fP///79f7r99/////1H4g/wD/g/6B34H/gP0EfiocVRiqgVUAo')]
    [TestCase('197', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PACB4/uZGDw/MMHj+5vc/H+61fP/3/79f7r99//f//1X6o/xD/g/7B34H/iP0VXqqBVQCqgVVCo')]
    [TestCase('198', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PByJpeXxODw/PMnl5/P8/H++3ff3+/79f7799/f7//1X6g/wH/A//B/4H/gF8Ffqq9VQiqgVUAo')]
    [TestCase('199', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,PDzD4/vbPDw/PMPj+9v8/H++8/P/3/79f7779/////1RiqPlA/A//B3sD+CjxVOKqZVUKqgVVCo')]
    [TestCase('200', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,fr3D4/vbvX5/vcPj+9v9/n+/8/P/3///f7/79/////9RiqPlA/A//B3sD+CjxVHKqBVUKqgVVCo')]
    [TestCase('201', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/73Dw+PTvf//vcPD49P9//+/1/Pz2////7//9/f7//9TiqfFB/Af+D/4D+CjxVOKqZVVqqgVVCo')]
    [TestCase('202', 'horse.webp,claWhite,Tile,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,PBiB4/uZGDw/OMHj+5vc/H+61fP/n/79f7r99/+//v1UKqvVBeAP8A/wB+Cr1VWqqBVUKqgVVCo')]
    [TestCase('203', 'horse.webp,claWhite,TileOriginal,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,PBiB4/uZGDw/OMHj+5vc/H+61fP/n/79f7r99/+//v1UKqvVBeAP8A/wB+Cr1VWqqBVUKqgVVCo')]
    [TestCase('204', 'horse.webp,claWhite,TileStretch,0.5,0.5,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,fr3Dw+PbvX5/vcPD49v9/n+/1/fz3///f7//9/f///9UKqvVA+AP8A/gB8Cr1VWqqBVUKqgVVCo')]
    [TestCase('205', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHwkIno+JgAzfOTiez/uzHN89vN/v//tc37+93///+0/+H/8Bfhc6D3+H94f3Av8D/xH+qAVQAI')]
    [TestCase('206', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHwkIno6AAAzfOTiez/OzHN89vN/v+79c37+93////1/+n/8Pfh97D3+P94f3FGqoAVQCqgVUAo')]
    [TestCase('207', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHxsAGQ0HgAzfOzAZzfezHN87vJ3v979c37u93f///0/+D/8H/A/8h/wG3A9kB/al79F6qB9UAo')]
    [TestCase('208', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AHAgIHoeDgAzcODgex/OzHN69PB/3//9c3r89X/f//0/4H/wPfg86B38D94H3Af8gf1B+qAVUAo')]
    [TestCase('209', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,IHBgYnoaAAAzcODiex/OzHN69PJ/3+79c3r893/f//1/4n/wP/h/6B38D94P3EWqoAVQCqgVUAo')]
    [TestCase('210', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,IHhoICQUHgAzeOjgJxfezHN67PI33/79c3rs93ff//1/4j/wP/B/9B/wC3AFkEf+o/9Q+qh9UAo')]
    [TestCase('211', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AHBgInoeDgAzcODiex/OzHN68PJ/n+/9c3r493+f//1/gn/gPeA9/C38C94B3EP+of1Q+qgVUAo')]
    [TestCase('212', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,IHhgYnobAAAzeODiex/OzHN68PJ/n+79c3r493+f//1/gn/gP+A//D38D94D3EHqoAVQCqgVUAo')]
    [TestCase('213', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,NHjhIESVHiQ3eOHgZ5fe7Hd65fN3n/79d3rt93ef//1/gj/AP/B/7C/4D3gBkEP+of9R+qh9UAo')]
    [TestCase('214', 'horse.webp,claWhite,Tile,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AHAgIBoeDgAzcODgOx/OzHN69PL/n+/9c3r89/+f//1/An/APcA97D38B94D3EH+oP1Q+qgVUAo')]
    [TestCase('215', 'horse.webp,claWhite,TileOriginal,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,IHhgInoaAAAzeODiex/OzHN69PL/n+79c3r89/+f//1/An/AP8A/7D38B94D3EH6oAVQCqgVUAo')]
    [TestCase('216', 'horse.webp,claWhite,TileStretch,0.5,0.5,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,NHjhIASVHiQ3eOHgJ5fe7Hd65fV3n/79d3rt9Xef//1/Aj/AP+B//D/4B3ADkEH+of9R+qh9UAo')]
    [TestCase('217', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADwmYnp+LAAzPObie3/szHP89vN///z9c/7+93////1P+h/4Pfx97n38H9gf2Ffyp8VRyqgVUAo')]
    [TestCase('218', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADxmYnp6AAAzPObie3vMzHP89vN/++79c/7+93/77/1P+h/4P/w/7n38H9gf2EH6oAVQCqgVUAo')]
    [TestCase('219', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,ADxsIGR2PAAzPOzgZ3f8zHP87vJ3//z9c/7u93f///1P+h/4H/x/+B/4P3gdkE/6r/VRyqgVUAo')]
    [TestCase('220', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PACB4/u5ADw/MMHj+7vM/H+61fP///79f7r99/////1H4g/wD/g/6B34H/gP0EfiocVRiqgVUAo')]
    [TestCase('221', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PACB4/uZGDw/MMHj+5vc/H+61fP/3/79f7r99//f//1X6o/xD/g/7B34H/iP0VXqqBVQCqgVVCo')]
    [TestCase('222', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,PByJpeXxODw/PMnl5/P8/H++3ff3+/79f7799/f7//1X6g/wH/A//B/4H/gF8Ffqq9VQiqgVUAo')]
    [TestCase('223', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,PDzD4/vbPDw/PMPj+9v8/H++1/f/3/79f77/9////v1VqqmVQ8If+B/4Q8KrlVWqqBVUKqpVVCo')]
    [TestCase('224', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,PDzD4/vbPDw/PMPj+9v8/H++1/f/3/79f77/9////v1VqqmVQ8If+B/4Q8KrlVWqqBVUKqpVVCo')]
    [TestCase('225', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,fr3Dw8PbvX5/vcPD49v9/n+/19f33///f7//1/f///9VqqnVQ8If+B/4Q+KplVWqqZVUKqpVVCo')]
    [TestCase('226', 'horse.webp,claWhite,Tile,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,fr3Dw8vDvX5/vcPD68P9/n+/z9Pv1///f7/v1/////9UKqgVUYoDwAPAU4qplVQqqlVUKqpVVCo')]
    [TestCase('227', 'horse.webp,claWhite,TileOriginal,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,fr3Dw8vDvX5/vcPD68P9/n+/z9Pv1///f7/v1/////9UKqgVUYoDwAPAU4qplVQqqlVUKqpVVCo')]
    [TestCase('228', 'horse.webp,claWhite,TileStretch,0.75,0.75,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/73Dw8PDvf//vcPD48P9//+/z/Pzz////7/v9/fv//9UKqgVUYoDwAPAUYqoFVQqqlVUKqpVVCo')]
    [TestCase('229', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHwkInp+LgAzfOTie3/uzHN89vN////tc37+93///+0/+H/4Bfhc6D3+H94P3Bf8h/1B+qAVUAo')]
    [TestCase('230', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHxmInp6AAAzfObie3/OzHN89vN//+79c37+93////1/+n/4Pfx/7D3+H94f3EGqoAVQCqgVUAo')]
    [TestCase('231', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,1,0.98,AHxsAGR0PgAzfOzAZ3f+zHN87vJ3//79c37u93f///0/+D/4P/w/8B/wO3ANkA/aj79A6qB9UAo')]
    [TestCase('232', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,AHAgIHoeDgAzcODgex/OzHN69PB/3//9c3r89X/f//0/4H/wPfg86B38D94H3Af8gf1B+qAVUAo')]
    [TestCase('233', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,IHBgYnoaAAAzcODiex/OzHN69PJ/3+79c3r893/f//1/4n/wP/h/6B38D94P3EWqoAVQCqgVUAo')]
    [TestCase('234', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,IHhoICQUHgAzeOjgJxfezHN67PI33/79c3rs93ff//1/4j/wP/B/9B/wC3AFkEf+o/9Q+qh9UAo')]
    [TestCase('235', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,AHAgIBoeDgAzcODgOx/OzHN69PT/n+/9c3r89f+f//1/gn+APcA//DX8A94D3ED+of1R+qgVUAo')]
    [TestCase('236', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,IHBgInoKAAAzcODiew/OzHN69Pb/j+79c3r89/+P//1/gn+AP8A//DX8A94D3EH6oAVQCqgVUAo')]
    [TestCase('237', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,NHjhIASVHiQ3eOHgJ5fe7Hd65fV3n/79d3rt9Xef//1/gj/AP8B/7Cf4A3gBmEH+of9R+qh9UAo')]
    [TestCase('238', 'horse.webp,claWhite,Tile,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,AHBgIAoODgAzcODgKw/OzHN69PLvj+/9c3r89/+P//1/gn8APYA/7D/8At4B3ED+oP1Q+qgVUAo')]
    [TestCase('239', 'horse.webp,claWhite,TileOriginal,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,IHhgIkoLAAAzeODiaw/OzHN69PLvj+79c3r89/+P//1/gn8AP4A/7D/8A94B3ED6oAVQCqgVUAo')]
    [TestCase('240', 'horse.webp,claWhite,TileStretch,0.75,0.75,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,NHjhAASHHiQ3eOHAJ4fe7Hd65fF3j/79d3rt9XeP//1/Aj8AP4B//D/4AHgBmEH+of9R+qh9UAo')]
    [TestCase('241', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('242', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('243', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('244', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('245', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('246', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('247', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('248', 'horse.webp,claWhite,Tile,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('249', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('250', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('251', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('252', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('253', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('254', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('255', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('256', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('257', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('258', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('259', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('260', 'horse.webp,claWhite,Tile,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('261', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('262', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('263', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('264', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('265', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('266', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('267', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('268', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('269', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('270', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('271', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('272', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('273', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('274', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('275', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('276', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('277', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('278', 'horse.webp,claWhite,Tile,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('279', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('280', 'horse.webp,claWhite,TileOriginal,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('281', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('282', 'horse.webp,claWhite,TileStretch,15,15,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('283', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('284', 'horse.webp,claWhite,Tile,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('285', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('286', 'horse.webp,claWhite,TileOriginal,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('287', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('288', 'horse.webp,claWhite,TileStretch,15,15,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('289', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('290', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('291', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('292', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('293', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('294', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('295', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('296', 'horse.webp,claWhite,Tile,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('297', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('298', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('299', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('300', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Round,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('301', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('302', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('303', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('304', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('305', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('306', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('307', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('308', 'horse.webp,claWhite,Tile,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('309', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('310', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('311', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('312', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,Bevel,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('313', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('314', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('315', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('316', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('317', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('318', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('319', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('320', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('321', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('322', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('323', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('324', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerRound,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('325', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('326', 'horse.webp,claWhite,Tile,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('327', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('328', 'horse.webp,claWhite,TileOriginal,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('329', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('330', 'horse.webp,claWhite,TileStretch,0,0,TopRight|BottomLeft,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('331', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('332', 'horse.webp,claWhite,Tile,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('333', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('334', 'horse.webp,claWhite,TileOriginal,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('335', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,0,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    [TestCase('336', 'horse.webp,claWhite,TileStretch,0,0,AllCorners,InnerLine,False,0.5,0.5,0.5,0.5,1,0.98,PDzDw8PDPDw/PMPD48P8/H++x9f3x/79f77v1/fv//1VqqvVVmq9vb29Vmqr1VWqq9VVqqpVVao')]
    procedure TestFillRoundRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8f///fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/J/////8')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wP/B/////8')]
    [TestCase('4',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/L/////8')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('7',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/w//L/////8')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('10', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8////fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/w//D/////8')]
    [TestCase('13', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8f///fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/J/////8')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wP/B/////8')]
    [TestCase('16', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/L/////8')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8f////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('19', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8f////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('22', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8/////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/w//D/////8')]
    procedure TestFillRoundRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8f///fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/J/////8')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wP/B/////8')]
    [TestCase('4',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/L/////8')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('7',  'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/w//L/////8')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('10', 'horse.webp,claWhite,Tile,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8////fHh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8////fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/w//D/////8')]
    [TestCase('13', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx8f///fHh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/J/////8')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//Hh4fHx/////fHh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,//3x8fHx8f///fHx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wP/B/////8')]
    [TestCase('16', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8H/wP/A/8H/wf/L/////8')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8f////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('19', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8f////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/wf/D/////8')]
    [TestCase('22', 'horse.webp,claWhite,Tile,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx8/////Hh8/f/////8/P/9///////////////A/8D/wP/A/8D/wP/A/8H/w//L/////8')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,//Ph4fHx//////Hh8/f/////8/P/////////////////A/8D/wP/A/8D/wP/A/////////////8')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,///x8fHx8/////Hx8/f/////9/Pz////////////////A/8D/wP/A/8D/wP/A/8D/w//D/////8')]
    procedure TestFillRoundRectBitmapWithClipping2(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwAAAAFAAYADyAPyB/iH/w8')]
    [TestCase('2',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwDA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwAAgAHgAfgD/AP/B//H//8')]
    [TestCase('3',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Round,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v//////////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('4',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwAAAAFAAYADyAPyB/iH/w8')]
    [TestCase('5',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwDA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwAAgAHgAfgD/AP/B//H//8')]
    [TestCase('6',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v/////+////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('7',  'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwAAAAFAAYADyAPyB/iH/w8')]
    [TestCase('8',  'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwDA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwAAgAHgAfgD/AP/B//H//8')]
    [TestCase('9',  'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v/////+////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('10', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwAAAAFAAYADyAPyB/iH/y8')]
    [TestCase('11', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwDA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwAAgAHgAfgD/AP/B//H//8')]
    [TestCase('12', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,TopLeft|BottomRight,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v//////////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('13', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwABAAFAAYADyAPyB/iH/w8')]
    [TestCase('14', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwDA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwABgAHgAfgD/AP/B//H//8')]
    [TestCase('15', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Round,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v/////+////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('16', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcAAwADAAFAAYADyAPyB/iH/w8')]
    [TestCase('17', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwHA4fv//+fjwcbv////9+PR3+///////9f/7/8AfwAfAAcAAwABgAHgAfgD/AP/B//H//8')]
    [TestCase('18', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,Bevel,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwAAweH//+/nw8fP7f//7+fr9+/v/////+////8AfwB/AAcAAwABAAEEAQQDwAPwB/gH/g8')]
    [TestCase('19', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcABwADAAFAAYADwAPyB/iH/w8')]
    [TestCase('20', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwHA4fv//+fjwcbv////9+PR3+///////9P/7/8AfwAfAAcAAwADgAHgAfgD/AP/B//H//8')]
    [TestCase('21', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerRound,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwEAgeH//+/nw8fP7f//7+fr9+/v/////+////8AfwB/AAcABwADAAEEAQQDwAPwB/gH/g8')]
    [TestCase('22', 'horse.webp,claWhite,Tile,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/z8HBwGAgfv//+fnwcbP////9+fR/9///////9//3/8AfwAfAAcABwADAAFAAYADwAPyB/iH/y8')]
    [TestCase('23', 'horse.webp,claWhite,TileOriginal,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/z8HAwHA4fv//+fjwcbv////9+PR3+///////9P/7/8AfwAfAAcABwADgAHgAfgD/AP/B//H//8')]
    [TestCase('24', 'horse.webp,claWhite,TileStretch,1.5,1,0,-0.25,30,0.15,0.15,AllCorners,InnerLine,False,0.15,0.15,0.85,0.85,1,0.98,/38PBwEAgeH//+/nw8fP7f//7+fr9+/v////////7/8AfwB/AAcABwADAAEEAQQDwAPwB/gH/g8')]
    procedure TestFillRoundRectBitmapWithMatrix(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree, AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAD//P////8ODP/8/////w6M////////////////////////AAAAAAAAAAAAAAAAgAE')]
    [TestCase('2',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAD//P////8ODP/8/////w6M//////////////////////////8AAAAAAAAAAAAAgAE')]
    [TestCase('3',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAD//P////8ODP/8/////w6M////////////////////////AAAAAAAAAAAAAAAAgAE')]
    [TestCase('4',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAD//P////8ODP/8/////w6M//////////////////////////8AAAAAAAAAAAAAgAE')]
    [TestCase('5',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8fN///98f//x83//////////////+AHwAP/////wAPAA8ADwAPgB/////8')]
    [TestCase('6',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8fN///98f//583/////////////////4Af/////wAPAA8ADwAPgB/////8')]
    [TestCase('7',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fJ///98f//x83//////////////+AHwAP/////wAPAA8ADwAPgB/////8')]
    [TestCase('8',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fJ///98f//x83/////////////////4Af/////wAPAA8ADwAPgB/////8')]
    [TestCase('9',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('10',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('11',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('12',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('13',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('14',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//4cn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('15',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('16',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('17',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//H5nZ39+DP/8/nd3//78///+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD/w///8')]
    [TestCase('18',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//H5nZ39+DP/8/nd3//78///+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD/w///8')]
    [TestCase('19',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,ADx+ZmZ+PAD//H5mZ38+DP/8/nd3//7s//3///////5//v///D/8P/w//D///3/+f/6//Z/5h+E')]
    [TestCase('20',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,ADx+ZmZ+PAD//H5mZ38+DP/8/nd3//7s//3///////5//v///D/8P/w//D///3/+f/6//Z/5h+E')]
    [TestCase('21',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('22',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('23',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('24',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('25',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gQD/////AAD/+P////8ODP/4/////w/M////////////////////////AAAAAAAAAAAAAIABwAM')]
    [TestCase('26',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gQD/////AAD/+P////8ODP/4/////w/M//////////////////////////8AAAAAAAAAAIABwAM')]
    [TestCase('27',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gQD/////AAD/8P////8EDP/w/////wTM////////////////////////AAAAAAAAAAAAAIABwAM')]
    [TestCase('28',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gQD/////AAD/8P////8EDP/w/////wXM//////////////////////////8AAAAAAAAAAIABwAM')]
    [TestCase('29',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8fN///98f//x83//////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('30',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8fN///98f//583/////////////////4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('31',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fN///98f//x83//////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('32',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fN///98f//x83/////////////////4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('33',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('34',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('35',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('36',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('37',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('38',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//4cn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('39',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('40',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('41',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//P5mZ38+DP/8/nd3//78//7+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD/w/f/4')]
    [TestCase('42',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//P5mZ38+DP/8/nd3//78//7+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD/w/f/4')]
    [TestCase('43',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,ADx+ZmZ+PAD+/P5mZ38+DP78/nd3//7s//////////5//v5//D/4H/gf/D/+f3/+f/6//Z/5x+M')]
    [TestCase('44',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,ADx+ZmZ+PAD+/P5mZ38+DP78/nd3//7s//////////5//v5//D/4H/gf/D/+f3/+f/6//Z/5x+M')]
    [TestCase('45',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('46',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcPnwf+AAAAAA')]
    [TestCase('47',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/J///9/fX33/n////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('48',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/J///9/fX33/n////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('49',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gQD/////AIH/+P////8Ejf/4/////wTN////////////////////////AAAAAAAAAAAAAMADwAM')]
    [TestCase('50',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gQD/////AIH/+P////8Ejf/4/////wTN//////////////////////////8AAAAAAAAAAIABwAM')]
    [TestCase('51',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gQD/////AIH/+P////8Eif/4/////0XJ////////////////////////AAAAAAAAAAAAAMADwAM')]
    [TestCase('52',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gQD/////AIH/+P////8Eif/4/////0XJ//////////////////////////8AAAAAAAAAAIABwAM')]
    [TestCase('53',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bgf////H//8fP////8f//x+///////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('54',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bgf////H//8fP////8f//5+///////////////+AH4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('55',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fP///98f//x+///////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('56',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bgf////H//8fP////8f//x+///////////////+AH4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('57',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('58',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('59',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('60',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('61',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('62',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//4cn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('63',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('64',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('65',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//P5mZ38+DP/8/nd3//78//7+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD3w+f/4')]
    [TestCase('66',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,ADx+ZmZ+PAD//P5mZ38+DP/8/nd3//78//7+f/f//v/AA8ADwAOAAYABwAPAA8AD4AfwD35+f/4')]
    [TestCase('67',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,ADx+ZmZ+PAD4/P5mZ38+DPj8/nd3//7s//////////5//v5//D/4H/gf/D/+f3/+f/6//Z/5x+M')]
    [TestCase('68',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,ADx+ZmZ+PAD4/P5mZ38+DPj8/nd3//7s//////////5//v5//D/4H/gf/D/+f3/+f/6//Z/5x+M')]
    [TestCase('69',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcHngf+AAAAAA')]
    [TestCase('70',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcHngf+AAAAAA')]
    [TestCase('71',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('72',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('73',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gQD/////AIH/+P////8Ejf/4/////4TN////////////////////////AAAAAAAAAAAAAMADwAM')]
    [TestCase('74',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gQD/////AIH/+P////8Ejf/4/////4TN//////////////////////////8AAAAAAAAAAMADwAM')]
    [TestCase('75',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gQD/////AIH/+P////8Eif/4/////4TJ////////////////////////AAAAAAAAAAAAAMADwAM')]
    [TestCase('76',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gQD/////AIH/+P////8Eif/4/////4TJ//////////////////////////8AAAAAAAAAAMADwAM')]
    [TestCase('77',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bw/////H//8fP////8f//x+///////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('78',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bw/////H//8fP////8f//5+///////////////+AH4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('79',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bw/////H//8fP////8f//x+///////////////+AHwAP//+AHwAPAA8AD4AfwD/////8')]
    [TestCase('80',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bw/////H//8fP////8f//x+///////////////+AH4Af/////wAPAA8AD4AfwD/////8')]
    [TestCase('81',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('82',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAD/////AAB/fP////9ODH98/////04M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('83',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M////////////////////////AAAAAAAAAAAAAAAAAAA')]
    [TestCase('84',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAD/////AAB/fP////8ODH98/////w4M//////////////////////////8AAAAAAAAAAAAAAAA')]
    [TestCase('85',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('86',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf///fH//8HJ///98f//4cn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('87',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('88',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn/////////////////4Af/////wAPAA8ADwAPAA/////8')]
    [TestCase('89',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gTx+ZmZ+PIH//P5mZ38+jf/8/nd3//79//7+f/f//v/AA8ADgAGAAYABgAHAA8AD4AfwD3w+f/4')]
    [TestCase('90',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gTx+ZmZ+PIH//P5mZ38+jf/8/nd3//79//7+f/f//v/AA8ADwAOAAYABwAPAA8AD4AfwD35+f/4')]
    [TestCase('91',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,ADx+ZmZ+PAD4/P5mZ38+DPj8/nd3//7s//////////5//v5//D/4H/gf/D/+f3/+f/6//d/7x+M')]
    [TestCase('92',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gTx+ZmZ+PIH5/P5mZ38+jfn8/nd3//7t//////////9//v5//D/4H/gf/D/+f3/+f/6//d/7x+M')]
    [TestCase('93',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcHngf+AAAAAA')]
    [TestCase('94',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcHngf+AAAAAA')]
    [TestCase('95',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwgBCAEMAwwDDgcHngf+AAAAAA')]
    [TestCase('96',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwwDDAMMAwwDDgcH/gf+AAAAAA')]
    [TestCase('97',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('98',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('99',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59nJ///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/Q//AAAAAA')]
    [TestCase('100', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('101', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('102', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59nJ///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/Q//AAAAAA')]
    [TestCase('103', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('104', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('105', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59nJ///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/Q//AAAAAA')]
    [TestCase('106', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('107', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('108', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59nJ///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/Q//AAAAAA')]
    [TestCase('109', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fJ///98f//x83//////////////+AHwAP/////wAPAA8ADwAPAB/////8')]
    [TestCase('110', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('111', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/w/+AAAAAA')]
    [TestCase('112', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fN///98f//x83//////////////+AHwAP/////wAPAA8ADwAfAD/////8')]
    [TestCase('113', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('114', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/J///9/fX33/n////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('115', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bgf////H//8fP////8f//x8///////////////+AHwAP/////wAPAA8ADwAfAD/////8')]
    [TestCase('116', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('117', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('118', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bg/////H//8fP////8f//x8///////////////+AHwAP/////wAPAA8ADwAfAD/////8')]
    [TestCase('119', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('120', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwwDDAMMAwwDDgcL/g/+AAAAAA')]
    [TestCase('121', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fJ///98f//x83//////////////+AHwAP/////wAPAA8ADwAPAB/////8')]
    [TestCase('122', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('123', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl593J///9/fX33fn///399f/9//84HDAMMAwwDDAMMAwwDDgcL/w//AAAAAA')]
    [TestCase('124', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8fN///98f//x83//////////////+AHwAP/////wAPAA8ADwAfAD/////8')]
    [TestCase('125', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('126', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/J///9/fX33/n////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('127', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8GB//+Bgf///fH//8fP///98f//x8///////////////+AHwAP/////wAPAA8ADwAfAD/////8')]
    [TestCase('128', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('129', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3////////////4H/AP8A/wD/AP8A/wD/gf7/f///////8')]
    [TestCase('130', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GB//+Bg////fH//8fP///98f//x8///////////////+AHwAP//+AHwAPAA8ADwAfAD/////8')]
    [TestCase('131', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf///fH//8HJ///98f//wcn//////////////+AHwAP/////wAPAA8ADwAPAA/////8')]
    [TestCase('132', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GZpaWZgf///fnl59/N///9/fX33/3///399f////84HDAMMAwwDDAMMAwwDDgcL/g/+AAAAAA')]
    [TestCase('133', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('134', '[$00FF0000;0 claNull;0.5 $000000FF;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestFillRoundRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAA7MP////9OzLs6/////27+//7/////bv4AAAAAAAAAAP//X/r///////////////8')]
    [TestCase('2',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAA7MP////9OzLs6/////27+/////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAA7MP////9OTLs7/////2/+////////b/4AAAAAAAAAAPvfV+r///////////////8')]
    [TestCase('4',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAA7MP////9OTLs7/////2/+/////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('5',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN///37f//z93///fv//////9AAkACAAAAAD28P/w//H/+P/wf+AgQAAA')]
    [TestCase('6',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN////7f//z83////v//////9//v//AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('7',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9KUgAAAAAAAD/8P/w//H/+P/wf+AgQAAA')]
    [TestCase('8',   '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93////v//////9//r/9AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('9',   '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAB7cP////9OzHv6/////+7/////////7v8AAAAAAAAAAP//X/r///////////////8')]
    [TestCase('10',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAD/////AAA7cP////9OzDv6/////+7//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('11',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHt6/////25/////////bn8AAAAAAAAAAPvfV+r///////////////8')]
    [TestCase('12',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHv6/////+5//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('13',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN///37f//z93///fv//////9AAkACAAAAAD28P/w//H/+P/w//AgQAAA')]
    [TestCase('14',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN////7f//z83///////////9//v//AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('15',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('16',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///////////9//r/9AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('17',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/fP7mZ39+zD9///d3f3/u//////d//+/Bg4fhh+GP8Y/xh+GH4cGD4AfwD/gf//8')]
    [TestCase('18',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/fP7mZ39+zD9///d3f3/u//////9//+/iR8ADiBGAAYABiBHAA+JH4Af4H/w///8')]
    [TestCase('19',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,ADx+ZmZ+PAA/fH5mZ39+DL9/f3d3f3+u////f/d//6/Bg8PDh+GP8Y/xh+HDw8GD4AfwD/w///8')]
    [TestCase('20',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,ADx+ZmZ+PAA/fH5mZ39+DL9/f3d3f3+u////f/9//6/gB8ADwAOAAYABwAPAA+AH8A/4H/5///8')]
    [TestCase('21',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///fv99////94HnGOc85n5mfmc85xjngePnwf+A/wAAA')]
    [TestCase('22',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///f//9////94HnAOcA5wDnAOcA5wDngeP/xf+r/9QAI')]
    [TestCase('23',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnwf+A/wAAA')]
    [TestCase('24',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///f///////94HnAOcA5wDnAOcA5wDngeP/xf+r/9QAI')]
    [TestCase('25',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gQD/////AIG7MP////8Mzbs7/////y/9////////L/0AAAAAAAAAAP///////////////3/+f/4')]
    [TestCase('26',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gQD/////AAC7MP////8MTLs7/////y/8/////////////6AFAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('27',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gQD/////AIG7MP////8Mjfs7/////y/9////////L/0AAAAAAAAAAPvfX/r/////////////f/4')]
    [TestCase('28',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gYH/////AAC7sf////8MDPu7/////y/8/////////////6AFAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('29',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//scH//4PN///1zf//z93///XP////3/9IEkACAAAAAD28P/w//H/+P/w//AgQAAA')]
    [TestCase('30',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//scH//4PN///9zf//z83////v//////9//v//AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('31',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//scH//4PN///1yf//y93///XP///73/9L0gAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('32',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//scH//4PN///1yf//y93////v///7//9//r/9AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('33',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAD/////AAB7cP////9OzHv6/////+7/////////7v8AAAAAAAAAAP//X/r///////////////8')]
    [TestCase('34',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAD/////AAA7cP////9OzDv6/////+7//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('35',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHt6/////25/////////bn8AAAAAAAAAAPvfV+r///////////////8')]
    [TestCase('36',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHv6/////+5//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('37',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN///37f//z93///fv//////9AAkACAAAAAD28P/w//H/+P/w//AgQAAA')]
    [TestCase('38',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN////7f//z83///////////9//v//AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('39',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('40',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///////////9//r/9AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('41',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/PP7mZ39+zL8///d3f3/u//////d//+/Bg4fhh+GP8Y/xh+GH4cGD4AfwD/gff/4')]
    [TestCase('42',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/PP7mZ39+zL8///d3f3/u//////9//+/iR8ADiBGAAYABiBHAA+JH4Af4H/w/f/4')]
    [TestCase('43',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,ADx+ZmZ+PAA/PP5mZ39+DL8//3d3f3+u////f/d//6/Bg8PDh+GP8Y/xh+HDw8GD4AfwD/w/f/4')]
    [TestCase('44',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,ADx+ZmZ+PAA/PP5mZ39+DL8//3d3f3+u//////9//6/gB8ADwAPAA8ADwAPAA+AH8A/4H///f/4')]
    [TestCase('45',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xjngePnw//A/wAAA')]
    [TestCase('46',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('47',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('48',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('49',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gQD///9+AIG7OP///38Mjfs7/////y/9////////L/0AAAAAAAAAAP///////////////3/+P/w')]
    [TestCase('50',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gYH///9+AIG7uf///38Mjfu7/////y/9/////////////6gVAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('51',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gQD/////AIG7OP////8Ejfs7/////y/9////////L/0AAAAAAAAAAPvf/////////////3/+P/w')]
    [TestCase('52',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gYH/////AIG7uf////8Ejfu7/////y/9/////////////6gVAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('53',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bgf//88H//4PP///zzf//z/////PP//////9IEkACAAAAAD28P/w//H/+P/wf+AgQAAA')]
    [TestCase('54',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bgf//88H//4PP///7zf//z+////vv//////9//v//AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('55',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bw///88H//4PP///zyf//y/////PP///7//9L0gAAAAAAAD/8P/w//H/+P/wf+AgQAAA')]
    [TestCase('56',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bgf//88H//4PP///z6f//6/////vv///7//9//r/9AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('57',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAD/////AAB7cP////9OzHv6/////+7/////////7v8AAAAAAAAAAP//X/r///////////////8')]
    [TestCase('58',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAD/////AAA7cP////9OzDv6/////+7//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('59',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHt6/////25/////////bn8AAAAAAAAAAPvfV+r///////////////8')]
    [TestCase('60',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHv6/////+5//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('61',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN///37f//z93///fv//////9AAkACAAAAAD28P/w//H/+P/w//AgQAAA')]
    [TestCase('62',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN////7f//z83///////////9//v//AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('63',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('64',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///////////9//r/9AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('65',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/PP5mZ398TL8//3d3f3/u////f/d//+/Bg8fjh+GP8Y/xh+HH48GD4AfwD3w+f/4')]
    [TestCase('66',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,ADx+ZmZ+PAA/PP5mZ398TL8//3d3f3/u////f/9//+/iR8ADiBGAAYABiBHAA+JH4Af4H3w+f/4')]
    [TestCase('67',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,ADx+ZmZ+PAA/PP5mZ388DL8//3d3fz+u////f/d//6/Bg8PDh+GP8Y/xh+HDw8GD4AfwD/w/f/4')]
    [TestCase('68',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,ADx+ZmZ+PAA/PP5mZ388DL8//3d3fz+u//////9//6/gB8ADwAPAA8ADwAPAA+AH8A/4H///f/4')]
    [TestCase('69',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///fv99////94HnGOc85n5mfmc85xjngePnwf+A/wAAA')]
    [TestCase('70',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('71',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('72',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('73',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gQD///9+AIG7OP///38Mjfs7/////y79////////Lv0AAAAAAAAAAP///////////////z/8P/w')]
    [TestCase('74',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gYH///9+AIG7uf///38Mjfu7/////679/////////////6gVAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('75',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gQD/////AIG7OP////8Ejfs7/////y79////////Lv0AAAAAAAAAAPvf/////////////z/8P/w')]
    [TestCase('76',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gYH/////AIG7uf////8Ejfu7/////679/////////////6gVAAAAAAAAAAAAAAAAAAAAAIABQAI')]
    [TestCase('77',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bw///88H//4PP///zzf//z/////Pv//////9IEkACAAAAAD28P/w//H/+P/xf+qgVQAI')]
    [TestCase('78',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OB//+Bgf//88H//4PN///7zf//z+3///vv////7/9//v//AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('79',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bw///88H//4PP///zyf//z/////PP//////9f+gAAAAAAAD/8P/w//H/+P/wf+AgQAAA')]
    [TestCase('80',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bw///88H//4PP///zyf//y/////vv///7//9//r/9AAAAAAAAAACAAUACoAVQCqgVVCo')]
    [TestCase('81',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAD/////AAB7cP////9OzHv6/////+7/////////7v8AAAAAAAAAAP//X/r///////////////8')]
    [TestCase('82',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAD/////AAA7cP////9OzDv6/////+7//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('83',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHt6/////25/////////bn8AAAAAAAAAAPvfV+r///////////////8')]
    [TestCase('84',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAD/////AAB7cv////9OTHv6/////+5//////////////6AFAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('85',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN///37f//z93///fv//////9AAkACAAAAAD28P/w//H/+P/w//AgQAAA')]
    [TestCase('86',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GB//+Bgf//s+H//4PN////7f//z83///////////9//v//AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('87',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('88',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Linear,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///////////9//r/9AAAAAAAAAACAAUACgAFAAqgVVCo')]
    [TestCase('89',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gTx+ZmZ+PIG7PP5mZ388jbs//3d3fz+v////f/d//6/Bg8fjh+GP8Y/xh+HH48GD4AfwD3w+f/4')]
    [TestCase('90',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gTx+ZmZ+PIG7PP5mZ388jbs//3d3fz+v////f/9//6/iR8ADiBGAAYABiBHAA+JH4Af4H3w+f/4')]
    [TestCase('91',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gTx+ZmZ+PIG/PP5mZ388jb8//3d3fz+v////f/d//6/Bg8PDh+GP8Y/xh+HDw8GD4AfwD3w+f/4')]
    [TestCase('92',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gTx+ZmZ+PIG/PP5mZ388jb8//3d3fz+v//////9//6/gB8ADwAPAA8ADwAPAA+AH8A/4H3/+f/4')]
    [TestCase('93',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//seHBw4PN///17dfXz93///Xv99////94HnGOc85n5mfmc85xjngePnwf+A/wAAA')]
    [TestCase('94',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//seHBw4PN///17dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('95',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('96',  '[claRed;0 claNull;0.5 claBlue;1],claRed,Radial,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///f///////94HnAOcA5wDnAOcA5wDngeP/x//r/9QAI')]
    [TestCase('97',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('98',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('99',  '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('100', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('101', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('102', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('103', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('104', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('105', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('106', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('107', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('108', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('109', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9KUgAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('110', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('111', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw/+A/wAAA')]
    [TestCase('112', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//scH//4PN///1yf//y93///XP///73/9KUgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('113', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('114', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('115', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bg///88H//4PP///zyf//y9////PP///7//9L0gAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('116', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('117', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('118', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OB//+Bg///88H//4PP///zyf//z9////PP//////9L0gAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('119', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('120', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('121', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('122', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('123', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///37df3zd3///fv9//9//94HnGOc85n5mfmc85xrngePnw/+A/wAAA')]
    [TestCase('124', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//scH//4PN///1yf//y93///XP////3/9KUgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('125', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('126', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv9/////94HnGOc85n5mfmc85xrngePnw//A/wAAA')]
    [TestCase('127', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8GB//+Bg///8cH//4PP///1yf//y9////XP//////9KUgAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('128', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('129', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///37dfXz93///fv99////94HnGOc85n5mfmc85xrngePnw/+A/wAAA')]
    [TestCase('130', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GB//+Bg///8cH//4PP///1yf//z9////XP//////9KUgAAAAAAAD/8P/w//H/+P/w/+AgQAAA')]
    [TestCase('131', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Linear,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GB//+Bgf//s+H//4PJ///36f//y93///fv//////9IEgAAAAAAAD/8P/w//H/+P/w//AgQAAA')]
    [TestCase('132', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PN///37dfXz93///fv99////94HnGOc85n5mfmc85xrngePnw/+A/wAAA')]
    [TestCase('133', '[claRed;0 claNull;0.5 claBlue;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVmqtta21Vmqr1VWqqlVVqqpVVao')]
    [TestCase('134', '[$00FF0000;0 claNull;0.5 $000000FF;1],claWhite,Radial,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVmqtta21Vmqr1VWqqlVVqqpVVao')]
    procedure TestFillRoundRectGradientWithChessBackground(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,/+fDw8PD5/////Pjw8fv////8+Pj9+//////////////////8A/wD/AP8A/wD/gf//////////8')]
    procedure TestFillRoundRectGradientWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,gYEAAAAAAMP/+eDAQ0YMz//54MBDRgzP//vgwUNGDM8AAAAAAAAAAAAAAAAAAAAAAAAAAIABwAM')]
    [TestCase('2',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,gQAAAAAAAIH/+ODAQ0YMjf/44MBDRgyN//z//H/+/////////////////////////////j/8v/w')]
    [TestCase('3',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,gQAAAAAAAIH/8MDAAQIEif/wwMABAgSJ//PAwQECBIkAAAAAAAAAAAAAAAAAAAAAAAAAAIABwAM')]
    [TestCase('4',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,gQAAAAAAAIH54MDAAQIEifngwMABAgSJ+ePAwYEDBIkAAAAAAAAAAAAAAAAAAAAAAAAAAYAB4Ac')]
    [TestCase('5',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('6',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxc3//////+PH3f/P88/zz/PP88/zz/PP88/zwAvgB/////8')]
    [TestCase('7',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PH6f/AA8ADwAPAA8ADwAPAA8ADwAPwD/////8')]
    [TestCase('8',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('9',   'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('10',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('11',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('12',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('13',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('14',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PD2f/P88/zz/PP88/zz/PP88/z0AvAA/////8')]
    [TestCase('15',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('16',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('17',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,gYEAAAAAgYH/+WBAQ0fNjf/5YEBDR82N//ngQUNHzY0AAAAAAAAAAAAAAAAAAAAAAAAAAIABgAE')]
    [TestCase('18',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,gQAAAAAAAIH/+GBAQ0dMjf/4YEBDR0yN//5//v///////////////////////////////3/+P/w')]
    [TestCase('19',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,gQAAAAAAAIH/+GBAQ0YMjf/4YEBDRgyN//ngQUNGDI0AAAAAAAAAAAAAAAAAAAAAAAAAAIABgAE')]
    [TestCase('20',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,gQAAAAAAAIH/8GBAQwYEif/wYEBDBgSJ//HgQcMHBIkAAAAAAAAAAAAAAAAAAAAAAACAAYABgAE')]
    [TestCase('21',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('22',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/P88/zz/PP88/zz/PP88/zwAPgB/////8')]
    [TestCase('23',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('24',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn/////////+f/gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('25',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w4EAAAAAgcP/8eDAgQKFy//x4MCBAoXL//PgwYEChcsAAAAAAAAAAAAAAAAAAAAAAAAAAIABwAM')]
    [TestCase('26',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w4EAAAAAgcP/8eDAgQKFy//x4MCBAoXL//HgwoEChcsAAAAAAAAAAAAAAAAAAAAAAAAAAEACgAE')]
    [TestCase('27',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,w4EAAAAAgcP78eDAgACBw/vx4MCAAIHD+/PgwYAAgcMAAAAAAAAAAAAAAAAAAAAAAAAAAIABwAM')]
    [TestCase('28',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,w4EAAAAAgcP78eDAgACBw/vx4MCAAIHD+/fgwYABgcMAAAAAAAAAAAAAAAAAAAAAAACAAYAB4Ac')]
    [TestCase('29',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('30',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8////////PPz//AA8ADwAPAA8ADwAPAA8ADwAPwD/////8')]
    [TestCase('31',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('32',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('33',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('34',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('35',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('37',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('38',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PD2f/P88/zz/PP88/zz/PP88/z0AvAA/////8')]
    [TestCase('39',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('40',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('41',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gYEAAAAAgYH/8eBAQ0aFjf/x4EBDRoWN//HgQcNHhY0AAAAAAAAAAAAAAAAAAAAAAACAAYABgAE')]
    [TestCase('42',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,gYEAAAAAgYH/8eBAQwaFif/x4EBDBoWJ//HgQEMGhYkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAE')]
    [TestCase('43',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gYEAAAAAgYH98cAAAQKFgf3xwAABAoWB/fHAAYEDhYEAAAAAAAAAAAAAAAAAAAAAAACAAYABgAE')]
    [TestCase('44',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gYEAAAAAgYH54cAAAACBgfnhwAAAAIGB+ePAAYABgYEAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('45',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx8n//////+PH6f/AA8ADwAPAA8ADwAPAA8AD4AfgB/////8')]
    [TestCase('46',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx83//////+PH3f/AA8ADwAPAA8ADwAPAA8AD0AvgB/////8')]
    [TestCase('47',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx8n//////+PH6f/AA8ADwAPAA8ADwAPAA8AD4AfgB/////8')]
    [TestCase('48',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx8n/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('49',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD+//w48ABgcMAAAAAAAAAAAAAAAAAAAAAAACAAcAD4Ac')]
    [TestCase('50',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD+/vw5cACgcMAAAAAAAAAAAAAAAAAAAAAAABAAqAFwAM')]
    [TestCase('51',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAMHD+//w48ABwcMAAAAAAAAAAAAAAAAAAAAAAACAAcAD4Ac')]
    [TestCase('52',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAMHD+//w48ABwcMAAAAAAAAAAAAAAAAAAAAAAACAAcAD4Ac')]
    [TestCase('53',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+////////PP7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('54',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+////////PP7//QC8ADwAPAA8ADwAPAA9AL4BfwD/////8')]
    [TestCase('55',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHD5+////////Pv7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('56',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHD5+/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('57',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('58',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('59',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('60',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('61',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('62',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PD2f/P88/zz/PP88/zz/PP88/z0AvAA/////8')]
    [TestCase('63',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('64',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('65',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gYEAAAAAgYH/8eBAAQKFif/x4EABAoWJ//PgQYEDhYkAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('66',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gYEAAAAAgYH/8eBAAQKFif/x4EABAoWJ//HgQwEChYkAAAAAAAAAAAAAAAAAAAAAAAAAAMADgAE')]
    [TestCase('67',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gYEAAAAAgYH58eAAAACBgfnx4AAAAIGB+fPgAYABgYEAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('68',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gYEAAAAAgYH58eAAAACBgfnx4AAAAIGB+fPgAYADgYEAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('69',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83///////PP7f/AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('70',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83//////+PH/f/AA8ADwAPAA8ADwAPAA8AD8A/gB/////8')]
    [TestCase('71',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83///////PP7f/AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('72',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83/////////7f/wD+AH4AfgB+AH4AfgB/AP4Af///////8')]
    [TestCase('73',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD+//w48ABgcMAAAAAAAAAAAAAAAAAAAAAAACAAcAD4Ac')]
    [TestCase('74',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD//v/+9/4////////////////////////f/4f+N/7z/M')]
    [TestCase('75',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD+//w48ABgcMAAAAAAAAAAAAAAAAAAAAAAACAAcAD4Ac')]
    [TestCase('76',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,w4EAAAAAgcP7+fDgwACBw/v58ODAAIHD+//w58ADgcMAAAAAAAAAAAAAAAAAAAAAAADAA+AH4Ac')]
    [TestCase('77',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+////////PP///AA8ADwAPAA8ADwAPAA8AD8A/wD/////8')]
    [TestCase('78',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+////////PP7//QC8ADwAPAA8ADwAPAA9AL6BfwD/////8')]
    [TestCase('79',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+////////PP///AA8ADwAPAA8ADwAPAA8AD8A/wD/////8')]
    [TestCase('80',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHDx+/////////////gB+AH4AfgB+AH4AfgB+AH8A////////8')]
    [TestCase('81',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('82',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('83',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('84',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('85',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('86',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PD2f/P88/zz/PP88/zz/PP88/z0AvAA/////8')]
    [TestCase('87',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('88',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn/////////6f/gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('89',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gYEAAAAAgYH/+eBAAQKFif/54EABAoWL//vgQYEDhYsAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('90',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gYEAAAAAgYH9+eAAAQKFif/54AABAoWL//3//X/+//////////////////////////9//r/9v/0')]
    [TestCase('91',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gYEAAAAAgYH5+eAAAACBgfn54AAAAIGB+fvgAYABgYEAAAAAAAAAAAAAAAAAAAAAAACAAYABwAM')]
    [TestCase('92',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gYEAAAAAgYH5+eAAAACBgfn54AAAAIGB+fvgA4ABgYEAAAAAAAAAAAAAAAAAAAAAAACAAcADwAM')]
    [TestCase('93',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('94',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw/////Hhw8fP////8eHDx8///////+PH7//QC8ADwAPAA8ADwAPAA9AL4AfgB/////8')]
    [TestCase('95',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHD58////////Pv7//AA8ADwAPAA8ADwAPAA8AD4AfwD/////8')]
    [TestCase('96',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////8eHD58/////////////wD+AH4AfgB+AH4AfgB/AP8A////////8')]
    [TestCase('97',  'claPurple,claWhite,True,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('98',  'claPurple,claWhite,True,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('99',  'claPurple,claWhite,False,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('100', 'claPurple,claWhite,True,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('101', 'claPurple,claWhite,True,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('102', 'claPurple,claWhite,False,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('103', 'claPurple,claWhite,True,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('104', 'claPurple,claWhite,True,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('105', 'claPurple,claWhite,False,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('106', 'claPurple,claWhite,True,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('107', 'claPurple,claWhite,True,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('108', 'claPurple,claWhite,False,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('109', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PH6f/AA8ADwAPAA8ADwAPAA8ADwAPgD/////8')]
    [TestCase('110', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('111', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('112', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('113', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('114', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx8n//////+PH6f/AA8ADwAPAA8ADwAPAA8ADwAfgB/////8')]
    [TestCase('115', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('116', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('117', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83///////PP7f/AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('118', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg/////Hhw8fP////8eHDx8////////PP///AA8ADwAPAA8ADwAPAA8ADwA/gD/////8')]
    [TestCase('119', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('120', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg/////Hhw8fP////8eHDx8////////PP7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('121', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn///////PH6f/AA8ADwAPAA8ADwAPAA8ADwAPgD/////8')]
    [TestCase('122', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('123', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8XJ///98eHDxcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('124', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg////fHhw8fP///98eHDx8////////PH7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('125', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('126', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fJ///98eHDx8n//////+PH6f/AA8ADwAPAA8ADwAPAA8ADwAfgB/////8')]
    [TestCase('127', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg////fHhw8fP///98eHDx8////////PH7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('128', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('129', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8fN///98eHDx83///////PH7f/AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('130', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg////fHhw8fP///98eHDx8////////PH///AA8ADwAPAA8ADwAPAA8ADwA/gD/////8')]
    [TestCase('131', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////+PHyf/AA8ADwAPAA8ADwAPAA8ADwAPgB/////8')]
    [TestCase('132', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg////fHhw8fP///98eHDx8////////PH7//AA8ADwAPAA8ADwAPAA8ADwAfgD/////8')]
    [TestCase('133', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/8GBgYGBg////fHhw8fP///98eHDx8/////////H///wA+AD4APAA8ADwAfAB8APwB/gf/////8')]
    [TestCase('134', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/8GBgYGBg////fHhw8fP///98eHDx8/////////H///wA+AD4APAA8ADwAfAB8APwB/gf/////8')]
    [TestCase('135', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///8A/gD8APgA8AHwA/AH8A/wH/g//////8')]
    [TestCase('136', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///8A/gD8APgA8AHwA/AH8A/wH/g//////8')]
    [TestCase('137', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj////fHhw8fP///98fHDx8/////////H///+A/4D/APwA8APwD/Af8B/wP/A//////8')]
    [TestCase('138', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj////fHhw8fP///98fHDx8/////////H///+A/4D/APwA8APwD/Af8B/wP/A//////8')]
    [TestCase('139', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj////fHhx8/P///9+fXHz8/////////P////A/4D/AP4A8AfwD/Af8D/wP/A//////8')]
    [TestCase('140', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj////fHhx8/P///9+fXHz8/////////P////A/4D/AP4A8AfwD/Af8D/wP/A//////8')]
    [TestCase('141', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///wA/AD4APAA8ADwAfAD8APwD/gf/////8')]
    [TestCase('142', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///wA/AD4APAA8ADwAfAD8APwD/gf/////8')]
    [TestCase('143', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///8A/gD8APgA8AHwA/AH8A/wH/g//////8')]
    [TestCase('144', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh////fHhw8fP///98eHDx8/////////H///8A/gD8APgA8AHwA/AH8A/wH/g//////8')]
    [TestCase('145', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOPj////fHhw8/P///9+fXTz8/////////P////A/4D/gP4A8AfwH/Af8D/wP/A//////8')]
    [TestCase('146', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOPj////fHhw8/P///9+fXTz8/////////P////A/4D/gP4A8AfwH/Af8D/wP/A//////8')]
    [TestCase('147', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj////fHhx8/P///9+f3Hz8/////////P////A/8D/gP8A8A/wH/A/8D/wP/A//////8')]
    [TestCase('148', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj////fHhx8/P///9+f3Hz8/////////P////A/8D/gP8A8A/wH/A/8D/wP/A//////8')]
    [TestCase('149', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////9eHDx//////////////wD+AHwAPAA8ADwAPgB/AP+B/+f/////8')]
    [TestCase('150', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw/////Hhw8fP////9eHDx//////////////wD+AHwAPAA8ADwAPgB/AP+B/+f/////8')]
    [TestCase('151', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD9//////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('152', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD9//////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('153', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9//////////////+f/w/+B/gB+AH+B/8P/5//n////////8')]
    [TestCase('154', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9//////////////+f/w/+B/gB+AH+B/8P/5//n////////8')]
    [TestCase('155', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9/////////////////5//D/4H/gf/D/+f/////////////8')]
    [TestCase('156', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9/////////////////5//D/4H/gf/D/+f/////////////8')]
    [TestCase('157', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD5//////////////wD+AP4AfAA8AD4AfwD/AP/D/+f/////8')]
    [TestCase('158', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD5//////////////wD+AP4AfAA8AD4AfwD/AP/D/+f/////8')]
    [TestCase('159', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD9//////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('160', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+HD9//////////////8P/gf8A/gB+AH8A/4H/w//n////////8')]
    [TestCase('161', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9//////////////+f/5//D/gD/AP/D/+f/5//v////////8')]
    [TestCase('162', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz9//////////////+f/5//D/gD/AP/D/+f/5//v////////8')]
    [TestCase('163', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz5////////////////////n/8P/w//n////////////////8')]
    [TestCase('164', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn//////fjw+f/////9/Pz5////////////////////n/8P/w//n////////////////8')]
    [TestCase('165', '$000000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('166', '$FF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestFillRoundRectSolid(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,w6VCAABCpcP/veLAQ0Ptz/+/5sRHx+/v///37u/X/++r1Vfq//////////9X6qvVV+qr1VfqK9Q')]
    [TestCase('2',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,w6VCAABCpcP7teLAQ0Ptz/u35sRHx+//+/f37u/X//+r1VWqq9Vf+l/6q9VVqqvVVaqqVVWqKlQ')]
    [TestCase('3',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,w4EAAAAAgcP/+eDAQ0fNz//54MBDR83P//3//v///////////////////////////////3/+P/w')]
    [TestCase('4',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0,0,1,1,1,0.98,w4EAAAAAgcP/+eDAQ0fNz//54MBDR83P//3//v///////////////////////////////3/+P/w')]
    [TestCase('5',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('6',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///fv1e/X3/9//h/4X/of+B/4X/of+H/+H/hoFqpVQAI')]
    [TestCase('7',   'claPurple,claWhite,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/5//8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('8',   'claPurple,claRed,True,0.15,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePF3/9f+h/4X/of+B/4X/of+F/6H/ggBApQAAA')]
    [TestCase('9',   'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,QqVCAABCpUJ//eLAQ0Pvzn//5sRHx+/v///27u/X/++oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('10',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,QqVCAABCpUJ7/eLAQ0Pvznv/5sRHx+/++//27u/X//6oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('11',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('12',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('13',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('14',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///f/3efH3/9f+h/4X/of+B/4X/of+F/6X/pAArpdQAI')]
    [TestCase('15',  'claPurple,claWhite,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('16',  'claPurple,claRed,True,0,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePB3/9f+h/4X/of+B/4X/of+F/6H/gAAApQAAA')]
    [TestCase('17',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,w6VCAABCpcP//eLAQ0Pvz///5sRHx+/v///27u/X/++oFVQqoAUAAAAAoAVUKqgVVCqqVVQqqlU')]
    [TestCase('18',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,0.85,0.98,w6VCAABCpcP7/eLAQ0Pvz/v/5sRHx+//+//27u/X//+oFVQqoAUAAAAAoAVUKqgVVCqqVVQqqlU')]
    [TestCase('19',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,gYEAAAAAAIG/+WBgQ0dOjb/5YGBDR06Nv/1//v///////////////////////////////3/+P/w')]
    [TestCase('20',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0,0,1,1,1,0.98,gYEAAAAAAIG/+WBgQ0dOjb/5YGBDR06Nv/1//v///////////////////////////////3/+P/w')]
    [TestCase('21',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('22',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///f/3efH3/9f+h/4X/of+B/4X/of+F/6H/hAArpdQAI')]
    [TestCase('23',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/5//8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('24',  'claPurple,claRed,False,0.08,0.15,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePB3/9f+h/4X/of+B/4X/of+F/6H/gAAApQAAA')]
    [TestCase('25',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w4EAAAAAgcP7seDAgwOFz/u35sSHh4fv+/f376/X1++qVVQqqBVAAkACqBVUKqpVVCqqVdWralY')]
    [TestCase('26',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w4EAAAAAgcP7seDAgQOFz/u35sSFh4f/+7f276/X1/+oFVAKIAQAAAAAIARQCqgVVCqqVdQraBY')]
    [TestCase('27',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,w4EAAAAAgcP/seDAgQKFy/+x4MCBAoXL////////////////////////////////////////f/4')]
    [TestCase('28',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,w4EAAAAAgcP7seDAgACBw/ux4MCAAIHD/////f///////////////////////////////7/9f/4')]
    [TestCase('29',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBw4PP///3xcXHx9////fFxd////8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('30',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBw4PP///3xcXHx9////fv1c/X3/8//B/4X/of+B/4X/of+H/+H/hIEqpVQAI')]
    [TestCase('31',  'claPurple,claWhite,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBw4PP///3wcHDw9////fBwd/7//8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('32',  'claPurple,claRed,True,0.15,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBw4PP///3wcHDw9////fL0cvT3/9//h/4X/of+B/4X/of+H/+H/gIEApQAAA')]
    [TestCase('33',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,QqVCAABCpUJ//eLAQ0Pvzn//5sRHx+/v///27u/X/++oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('34',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,QqVCAABCpUJ7/eLAQ0Pvznv/5sRHx+/++//27u/X//6oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('35',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('37',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('38',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///f/3efH3/9f+h/4X/of+B/4X/of+F/6X/pAArpdQAI')]
    [TestCase('39',  'claPurple,claWhite,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('40',  'claPurple,claRed,True,0,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePB3/9f+h/4X/of+B/4X/of+F/6H/gAAApQAAA')]
    [TestCase('41',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w6VCAABCpcP7teLAQ0Ptz/u35sRHx+/v+/f37+/X/++r1Vfq//9//n/+//9X6qvVV+qr1dfrKlQ')]
    [TestCase('42',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,0.85,0.98,w6VCAABCpcP7teLAQ0Ptz/u35sRHx+//+/f37u/X//+r1VWqq9Veel56q9VVqqvVVaqqVVWqKlQ')]
    [TestCase('43',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gYEAAAAAgYG/ueBAQ0aNjb+54EBDRo2Nv/3//v///////////////////////////////3/+P/w')]
    [TestCase('44',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0,0,1,1,1,0.98,gYEAAAAAgYG/seBAQ0aFjb+x4EBDRoWNv/3//v///////////////////////////////3/+P/w')]
    [TestCase('45',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PJ///35cXHx93///flxd////8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('46',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PJ///35cXHx93///f/3efH3/9//h/4X/of+B/4X/of+H/+H/hgBrpdQAI')]
    [TestCase('47',  'claPurple,claWhite,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44PJ///34cHjw93///fhwf/7//8//D/8P/w//D/8P/w//D/8P/wf+AAAAAA')]
    [TestCase('48',  'claPurple,claRed,False,0.08,0.15,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44PJ///34cHjw93///fr0ePH3/9f+h/4X/of+B/4X/of+F/6H/ggBApQAAA')]
    [TestCase('49',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,w4EAAAAAgcP7ueDAwQKFz/u/5sTFhofv///37u/33/+//f///////////////7/9X/qv9VfqL/Q')]
    [TestCase('50',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,w4EAAAAAgcP7ueDAwQKFz/u/5sTFhof/+//37u/X1/+r1Vfq//9//n/+//9X6qvVV+qr1VfqKlQ')]
    [TestCase('51',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,w4EAAAAAgcP7ueDAwACBw/u54MDAAIHD//n//P/+//////////////////////////9//j/8H/g')]
    [TestCase('52',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,w4EAAAAAgcP7ueDAwACBw/u54MDAAIHD//n//P/+//////////////////////////9//j/8H/g')]
    [TestCase('53',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBg4PP///3xcXHx9////fFxc/33/8//D/8P/w//D/8P/w//D/8H/gP8AAAAAA')]
    [TestCase('54',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBg4PP///3xcXHx9////fv1d/f//9//h/4X/of+B/4X/of+H/+L/RYGqpVQAI')]
    [TestCase('55',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///3wcHDw9////fBwc/z3/8//D/8P/w//D/8P/w//D/8H/gP8AAAAAA')]
    [TestCase('56',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///3wcHDw9////fL0dvb//9//h/4X/of+B/4X/of+H/+L/QYGApQAAA')]
    [TestCase('57',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,QqVCAABCpUJ//eLAQ0Pvzn//5sRHx+/v///27u/X/++oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('58',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,QqVCAABCpUJ7/eLAQ0Pvznv/5sRHx+/++//27u/X//6oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('59',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('60',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('61',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('62',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///f/3efH3/9f+h/4X/of+B/4X/of+F/6X/pAArpdQAI')]
    [TestCase('63',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('64',  'claPurple,claRed,True,0,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePB3/9f+h/4X/of+B/4X/of+F/6H/gAAApQAAA')]
    [TestCase('65',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gYEAAAAAgYG7seBAAwONzbu35kQHh4/vu/f37q/X3++v9V/6//////////9f+q/1V+qr1VfqK9Q')]
    [TestCase('66',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,0.85,0.98,gYEAAAAAgYG7seBAAwOFzbu35kQHh4f/u/f37q/X1/+r1VWqq9Vf+l/6q9VVqqvVVaqqVVWqKlQ')]
    [TestCase('67',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gYEAAAAAgYG/ueBAAQKFib+54EABAoWJv/3//3/+//////////////////////////9//v//P/w')]
    [TestCase('68',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0,0,1,1,1,0.98,gYEAAAAAgYG9seAAAQKFib2x4AABAoWJv/3//3/+//////////////////////////9//v//P/w')]
    [TestCase('69',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PN///35cXHx93///flxd//3/8//D/8P/w//D/8P/w//D/8H/gf+AAAAAA')]
    [TestCase('70',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HBw4PJ///35cXHx93///f/3cfH//9//h/4X/of+B/4X/of+H/+P/xAArpdQAI')]
    [TestCase('71',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///34cHDw93///fhwd/73/8//D/8P/w//D/8P/w//D/8H/gf+AAAAAA')]
    [TestCase('72',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///34cHDw93///fr0cPD//9f+h/4X/of+B/4X/of+F/6P/wAAApQAAA')]
    [TestCase('73',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,w4EAAAAAgcP7ueDAQQKFz/u/5sRFhofv/////P////////////////////////////+//R/4P/w')]
    [TestCase('74',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,w4EAAAAAgcP7ueDAwQKFz/u/5sTFhof/+//37O/W1/+v9V/6//////////9f+q/1V+or1BfoC9A')]
    [TestCase('75',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,w4EAAAAAgcP7ueDAwACBw/u54MDAAIHD//n/+P/8//////////////////////////8//B/4H/g')]
    [TestCase('76',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,w4EAAAAAgcP7ueDAwACBw/u54MDAAIHD//n/+P/8//////////////////////////8//B/4H/g')]
    [TestCase('77',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBg4PP///zxcXHx/////PFxc/3//8//D/8P/w//D/8P/w//D/8H/gP8AAAAAA')]
    [TestCase('78',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///88HBg4PP///zxcXHx/////Pv1dfP//9f+h/4X/of+B/4X/of+F/6L/RQCqpVQAI')]
    [TestCase('79',  'claPurple,claWhite,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///zwcHDw/////PBwc/z//8//D/8P/w//D/8P/w//D/8H/gP8AAAAAA')]
    [TestCase('80',  'claPurple,claRed,True,0.15,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///zwcHDw/////PL0dPL//9f+h/4X/of+B/4X/of+F/6L/QQCApQAAA')]
    [TestCase('81',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,QqVCAABCpUJ//eLAQ0Pvzn//5sRHx+/v///27u/X/++oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('82',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,QqVCAABCpUJ7/eLAQ0Pvznv/5sRHx+/++//27u/X//6oFVQqIAQAAAAAIARUKqgVVCqqVVQqqBU')]
    [TestCase('83',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('84',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('85',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///flxf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('86',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/4GBgYGBgf//s+HB44HJ///35cXnxd3///f/3efH3/9f+h/4X/of+B/4X/of+F/6X/pAArpdQAI')]
    [TestCase('87',  'claPurple,claWhite,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('88',  'claPurple,claRed,True,0,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fr0ePB3/9f+h/4X/of+B/4X/of+F/6H/gAAApQAAA')]
    [TestCase('89',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gYEAAAAAgYG7ueBAAwOFjbu/5kQHh4evu//37q/X1++v9V/6//////////9f+q/1V+qr1VfqK9Q')]
    [TestCase('90',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,0.85,0.98,gYEAAAAAgYG7ueBAAQOFjbu/5kQFh4e/u//37q/X17+r1VWqq9Vf+l/6q9VVqqvVVaqqVVWqKlQ')]
    [TestCase('91',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gYEAAAAAgYG/ueAAAQKFib+54AABAoWJv/3//n/+//////////////////////////9//n/+P/w')]
    [TestCase('92',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0,0,1,1,1,0.98,gYEAAAAAgYG5ueAAAACBgbm54AAAAIGBv/3//n/+//////////////////////////9//n/+P/w')]
    [TestCase('93',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///8+HBw4PP///35cXHx9////flxd//3/8//D/8P/w//D/8P/w//D/8H/gf+AAAAAA')]
    [TestCase('94',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,/8OBgYGBw///8+HBw4PP///35cXHx9////f/3cfH//9f+h/4X/of+B/4X/of+F/6P/xAArpdQAI')]
    [TestCase('95',  'claPurple,claWhite,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///8+HBw4PP///34cHDw9////fhwd/73/8//D/8P/w//D/8P/w//D/8H/gf+AAAAAA')]
    [TestCase('96',  'claPurple,claRed,False,0.08,0.15,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///8+HBw4PL///34cHDw9////fr0cPD//9f+h/4X/of+B/4X/of+F/6P/wAAApQAAA')]
    [TestCase('97',  'claPurple,claWhite,True,0.15,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('98',  'claPurple,claWhite,True,0,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('99',  'claPurple,claWhite,False,0.08,0.15,,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('100', 'claPurple,claWhite,True,0.15,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('101', 'claPurple,claWhite,True,0,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('102', 'claPurple,claWhite,False,0.08,0.15,,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('103', 'claPurple,claWhite,True,0.15,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('104', 'claPurple,claWhite,True,0,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('105', 'claPurple,claWhite,False,0.08,0.15,,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('106', 'claPurple,claWhite,True,0.15,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('107', 'claPurple,claWhite,True,0,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('108', 'claPurple,claWhite,False,0.08,0.15,,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('109', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/5//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('110', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('111', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/5//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('112', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg///88HBw4PP///3wcHDw9////fBwd/7//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('113', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('114', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44PJ///34cHjw93///fhwf/7//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('115', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg///88HBg4PP///3wcHDw9////fBwc/z3/8//D/8P/w//D/8P/w//D/8P/g/8AAAAAA')]
    [TestCase('116', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('117', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///34cHDw93///fhwd/73/8//D/8P/w//D/8P/w//D/8P/g/+AAAAAA')]
    [TestCase('118', 'claPurple,claWhite,True,0.15,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg///88HBg4PP///zwcHDw9////PBwc/z3/8//D/8P/w//D/8P/w//D/8P/g/8AAAAAA')]
    [TestCase('119', 'claPurple,claWhite,True,0,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('120', 'claPurple,claWhite,False,0.08,0.15,TopLeft|TopRight|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBg///8+HBw4PP///34cHDw9////fhwd/73/8//D/8P/w//D/8P/w//D/8P/g/+AAAAAA')]
    [TestCase('121', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('122', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('123', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('124', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg///8cHBw4PP///1wcHDw9////XBwd////8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('125', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('126', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44PJ///34cHjw93///fhwf////8//D/8P/w//D/8P/w//D/8P/w/+AAAAAA')]
    [TestCase('127', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg///8cHBg4PP///1wcHDw9////XBwc//3/8//D/8P/w//D/8P/w//D/8P/g/8AAAAAA')]
    [TestCase('128', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('129', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HBw4PJ///34cHDw93///fhwd//3/8//D/8P/w//D/8P/w//D/8P/g/+AAAAAA')]
    [TestCase('130', 'claPurple,claWhite,True,0.15,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg///8cHBg4PP///1wcHDw9////XBwc//3/8//D/8P/w//D/8P/w//D/8P/g/8AAAAAA')]
    [TestCase('131', 'claPurple,claWhite,True,0,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwd3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('132', 'claPurple,claWhite,False,0.08,0.15,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,1,0.98,/8GBgYGBg///8+HBw4PP///34cHDw9////fhwd//3/8//D/8P/w//D/8P/w//D/8P/g/+AAAAAA')]
    [TestCase('133', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/8GBgYGBg///8cHBg4PP///11cXH19////XVxcf/3/8f/B/8P/w//D/8P/w/+D/4P/A/wAAAAAA')]
    [TestCase('134', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/8GBgYGBg///8cHBg4PP///11cXH19////XVxcf/3/8f/B/8P/w//D/8P/w/+D/4P/A/wAAAAAA')]
    [TestCase('135', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///99cXP08////31xc//z/8P/B/8P/x//D/+P/w/+D/wP+A/wACAAAA')]
    [TestCase('136', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///99cXP08////31xc//z/8P/B/8P/x//D/+P/w/+D/wP+A/wACAAAA')]
    [TestCase('137', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///99eXL18////315cv/z/8B/AP8B/xf/D/6P+A/wD+AP4A/AACAAAA')]
    [TestCase('138', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///99eXL18////315cv/z/8B/AP8B/xf/D/6P+A/wD+AP4A/AACAAAA')]
    [TestCase('139', '$BF0000FF,claWhite,True,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///9/fXDx8////399cP/z/8A/AH8A/xH/D/iP8A/gD8APwA/AACAAAA')]
    [TestCase('140', '$BF0000FF,claWhite,False,0.5,0.5,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///9/fXDx8////399cP/z/8A/AH8A/xH/D/iP8A/gD8APwA/AACAAAA')]
    [TestCase('141', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///15cXH09////Xlxcf/3/8P/B/8H/w//D/8P/g/+D/wP+A/gAAAAAA')]
    [TestCase('142', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Round,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///15cXH09////Xlxcf/3/8P/B/8H/w//D/8P/g/+D/wP+A/gAAAAAA')]
    [TestCase('143', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///99cXP08////31xc//z/8P/B/8P/x//D/+P/w/+D/wP+A/wACAAAA')]
    [TestCase('144', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,Bevel,0.15,0.15,0.85,0.85,0.85,0.98,/+HBgYGDh///8cHBg4PP///99cXP08////31xc//z/8P/B/8P/x//D/+P/w/+D/wP+A/wACAAAA')]
    [TestCase('145', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///9/fXTx8////399dP/z/8B/AH8A/xP/D/yP8A/gD+APwA/AACAAAA')]
    [TestCase('146', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerRound,0.15,0.15,0.85,0.85,0.85,0.98,//HhwYOHj///8eHBg4fP///9/fXTx8////399dP/z/8B/AH8A/xP/D/yP8A/gD+APwA/AACAAAA')]
    [TestCase('147', '$BF0000FF,claWhite,True,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj///8fHhh4/P///9/f3Hz8////39/cf/z/8A/AD8AfxD/D/CP4A/AD8APwA/AACAAAA')]
    [TestCase('148', '$BF0000FF,claWhite,False,0.75,0.75,TopLeft|BottomRight,InnerLine,0.15,0.15,0.85,0.85,0.85,0.98,//Hx4YePj///8fHhh4/P///9/f3Hz8////39/cf/z/8A/AD8AfxD/D/CP4A/AD8APwA/AACAAAA')]
    [TestCase('149', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///31dXX19////fV1dfX3/8f+B/4P/w//D/8P/wf+B/4D/ADwAIAAAA')]
    [TestCase('150', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/8OBgYGBw///88HBg4PP///31dXX19////fV1dfX3/8f+B/4P/w//D/8P/wf+B/4D/ADwAIAAAA')]
    [TestCase('151', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///389XX8/////fz1dfz//8P8B/4P/w//D/8P/wf+A/wB+ADwAAAAAA')]
    [TestCase('152', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///389XX8/////fz1dfz//8P8B/4P/w//D/8P/wf+A/wB+ADwAAAAAA')]
    [TestCase('153', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9+Pj9//////35+P3//8BgAPAB+Af+B/4B+ADwAGAgYFBAqAFUAo')]
    [TestCase('154', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9+Pj9//////35+P3//8BgAPAB+Af+B/4B+ADwAGAgYFBAqAFUAo')]
    [TestCase('155', '$BF0000FF,claWhite,True,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9/Pz9///////9/P///9BgoPBB+AP8A/wB+CDwUGCoAVQCqgVVCo')]
    [TestCase('156', '$BF0000FF,claWhite,False,0.5,0.5,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9/Pz9///////9/P///9BgoPBB+AP8A/wB+CDwUGCoAVQCqgVVCo')]
    [TestCase('157', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///349XX4/////fj1dfj//8P8B/4H/g//D/8H/gf+A/wB+ABgAAAAAA')]
    [TestCase('158', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Round,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///349XX4/////fj1dfj//8P8B/4H/g//D/8H/gf+A/wB+ABgAAAAAA')]
    [TestCase('159', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///389XX8/////fz1dfz//8P8B/4P/w//D/8P/wf+A/wB+ADwAAAAAA')]
    [TestCase('160', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,Bevel,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5///98PBg8Pv///389XX8/////fz1dfz//8P8B/4P/w//D/8P/wf+A/wB+ADwAAAAAA')]
    [TestCase('161', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9/Pz9///////9/P///9BggPAB+A/+B/4B+ADwEGCoQVRCqgVUAo')]
    [TestCase('162', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerRound,0.15,0.15,0.85,0.85,1,0.98,///nw8Pn/////+fDw+f/////9/Pz9///////9/P///9BggPAB+A/+B/4B+ADwEGCoQVRCqgVUAo')]
    [TestCase('163', '$BF0000FF,claWhite,True,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,fr3Dw8PDvX5/vcPDw8P9/n+/z/Pzz///f7/v9/fv//9UKqmVA8AH4AfgA8CplVQqqBVUKqgVVCo')]
    [TestCase('164', '$BF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,fr3Dw8PDvX5/vcPDw8P9/n+/z/Pzz///f7/v9/fv//9UKqmVA8AH4AfgA8CplVQqqBVUKqgVVCo')]
    [TestCase('165', '$000000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,1,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVmqtta21Vmqr1VWqqlVVqqpVVao')]
    [TestCase('166', '$FF0000FF,claWhite,False,0.75,0.75,AllCorners,InnerLine,0.15,0.15,0.85,0.85,0,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVmqtta21Vmqr1VWqqlVVqqpVVao')]
    procedure TestFillRoundRectSolidWithChessBackground(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,//fz8fHz9/////Px8/f/////+/n7///////7+fv///8AYADwAPAA8ADwAPAA8ABgAAAAAAAAAAA')]
    procedure TestFillRoundRectSolidWithClipping(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,//fz8fHz9/////Px8/f/////+/n7////////////////H/8P/w//D/8P/w//D/8f//////////8')]
    procedure TestFillRoundRectSolidWithClipping2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f//////+fHz//////////////4H/AP8A/wD/AP8A/wD/gf//////////8')]
    procedure TestFillRoundRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestSaveState(const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.UIConsts,
  System.Math;

{$IF CompilerVersion <= 30}
type
  TRectFHelper = record helper for TRectF
    function CenterAt(const ADesignatedArea: TRectF): TRectF;
  end;

{ TRectFHelper }

function TRectFHelper.CenterAt(const ADesignatedArea: TRectF): TRectF;
begin
  Result := Self;
  RectCenter(Result, ADesignatedArea);
end;
{$ENDIF}

{ TSkFMXCanvasTests }

procedure TSkFMXCanvasTests.ApplyGradientPoints(AGradient: TGradient; APoints: string);
var
  LGradientPoint: TGradientPoint;
  LRecords: string;
begin
  AGradient.Points.Clear;
  APoints := APoints.Substring(1, APoints.Length - 2);
  for LRecords in APoints.Split([' ']) do
  begin
    LGradientPoint := TGradientPoint(AGradient.Points.Add);
    LGradientPoint.Color := StringToAlphaColor(LRecords.Split([';'])[0]);
    LGradientPoint.Offset := StrToFloat(LRecords.Split([';'])[1], TFormatSettings.Invariant);
  end;
end;

function TSkFMXCanvasTests.CreateBitmap(const AImageFileName: string): TBitmap;
var
  LBitmap: TBitmap;
  LSvgBrush: TSkSvgBrush;
begin
  LBitmap := TBitmap.Create;
  try
    if AImageFileName.EndsWith('.svg') then
    begin
      LSvgBrush := TSkSvgBrush.Create;
      try
        LSvgBrush.Source := TFile.ReadAllText(SvgAssetsPath + AImageFileName);
        LBitmap.SetSize(Round(LSvgBrush.OriginalSize.Width), Round(LSvgBrush.OriginalSize.Height));
        LBitmap.SkiaDraw(
          procedure(const ACanvas: ISkCanvas)
          begin
            LSvgBrush.Render(ACanvas, RectF(0, 0, LBitmap.Width, LBitmap.Height), 1);
          end, False);
      finally
        LSvgBrush.Free;
      end;
    end
    else
      LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
  except
    LBitmap.Free;
    raise;
  end;
  Result := LBitmap;
end;

function TSkFMXCanvasTests.CreateMatrix(AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree: Single): TMatrix;
begin
  Result := TMatrix.CreateScaling(AScaleX, AScaleY) * TMatrix.CreateRotation(DegToRad(ARotationDegree));
  Result := Result * TMatrix.CreateTranslation(ADeltaX, ADeltaY);
end;

procedure TSkFMXCanvasTests.DrawChessBackground(ACanvas: TCanvas; ASquareSize: Single; AEvenSquareColor,
  AOddSquareColor: TAlphaColor);
var
  X, Y: Integer;
begin
  ACanvas.Clear(AEvenSquareColor);
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := AOddSquareColor;
  for X := 0 to Ceil(ACanvas.Width / ASquareSize) do
    for Y := 0 to Ceil(ACanvas.Height / ASquareSize) do
      if Odd(X + Y) then
        ACanvas.FillRect(TRectF.Create(PointF(X, Y) * ASquareSize, ASquareSize, ASquareSize), 0, 0, [], 1);
end;

function TSkFMXCanvasTests.StringToCorners(const ACornersString: string): TCorners;
var
  LString: string;
begin
  if SameText(ACornersString, 'AllCorners') then
    Exit(AllCorners);
  Result := [];
  for LString in ACornersString.Split([' ', ',', '|', ';']) do
  begin
    if SameText(LString, 'TopLeft') then
      Result := Result + [TCorner.TopLeft]
    else if SameText(LString, 'TopRight') then
      Result := Result + [TCorner.TopRight]
    else if SameText(LString, 'BottomLeft') then
      Result := Result + [TCorner.BottomLeft]
    else if SameText(LString, 'BottomRight') then
      Result := Result + [TCorner.BottomRight];
  end;
end;

procedure TSkFMXCanvasTests.TestClear(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClear2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.Clear(MakeColor(TAlphaColors.Red, 0.3));
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClearRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.ClearRect(RectF(25, 25, 75, 75), MakeColor(TAlphaColors.Red, 0.3));
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClearRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Black);
        LBitmap.Canvas.ClearRect(RectF(0, 0, 50, 50), TAlphaColors.Null);
        LBitmap.Canvas.ClearRect(RectF(50, 50, 100, 100), TAlphaColors.Null);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2)
          .CenterAt(RectF(0, 0, LBitmap.Width, LBitmap.Height)));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2)
          .CenterAt(RectF(0, 0, LBitmap.Width, LBitmap.Height)));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LClipRects := [RectF(0, 0, LBitmap.Width, LBitmap.Height),
      RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LClipRects := [RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect7(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.ExcludeClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2));
        LBitmap.Canvas.ExcludeClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect8(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2));
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width / 2, LBitmap.Height / 2, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestClipRect9(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(TAlphaColors.Blue, 0.5);
        try
          LBitmap.Canvas.ClearRect(RectF(25, 25, 90, 90), TAlphaColors.Black);
          LBitmap.Canvas.ClearRect(RectF(0, 0, 50, 50), TAlphaColors.Null);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmap(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer;
  ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := ASurfaceScale;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithChessBackground(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight,
  ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := ASurfaceScale;
    if LSurface.Canvas.BeginScene then
      try
        DrawChessBackground(LSurface.Canvas, Min(ASurfaceWidth, ASurfaceHeight) / 15 / LSurface.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LSurface.Canvas.Blending := ABlending;
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.IntersectClipRect(RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2)
          .CenterAt(RectF(0, 0, ASurfaceWidth, ASurfaceHeight)));
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, True);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithClipping2(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LClipRects: TArray<TRectF>;
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LClipRects := [RectF(0, 0, ASurfaceWidth / 2, ASurfaceHeight / 2)
      .CenterAt(RectF(0, 0, ASurfaceWidth, ASurfaceHeight))];
    if LSurface.Canvas.BeginScene(@LClipRects) then
      try
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, True);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft,
  ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed,
  ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        LSurface.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
            RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
        finally
          LImage.Free;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft,
  ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed,
  ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        LSurface.Canvas.Blending := ABlending;
        if not Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(StringToAlphaColor(AModulateColor), AModulateColorOpacity);
        try
          LSurface.Canvas.SetMatrix(
            CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
          LImage := CreateBitmap(AImageFileName);
          try
            LSurface.Canvas.DrawBitmap(LImage, RectF(ASrcLeft, ASrcTop, ASrcRight, ASrcBottom),
              RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), AOpacity, AHighSpeed);
          finally
            LImage.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
  LSurface: TBitmap;
begin
  LSurface := TBitmap.Create;
  try
    LSurface.SetSize(ASurfaceWidth, ASurfaceHeight);
    LSurface.BitmapScale := 1;
    if LSurface.Canvas.BeginScene then
      try
        if not Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := MakeColor(TAlphaColors.Black, 0.5);
        try
          LImage := CreateBitmap(AImageFileName);
          try
            LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
              RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
            LModulateCanvas.ModulateColor := TAlphaColors.White;
            LSurface.Canvas.IntersectClipRect(RectF(LSurface.Width / 2, 0, LSurface.Width, LSurface.Height));
            LSurface.Canvas.Clear(TAlphaColors.Null);
            LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
              RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
          finally
            LImage.Free;
          end;
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LSurface.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LSurface.ToSkImage, AMinSimilarity);
  finally
    LSurface.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(500, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(0, 0, LBitmap.Width, LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(400, 400);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0.15 * LBitmap.Width, 0.15 * LBitmap.Height,
          0.85 * LBitmap.Width, 0.85 * LBitmap.Height));
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LBitmap.Canvas.FillRect(RectF(0, 0, LBitmap.Width, LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth,
  ASurfaceHeight: Integer; AWrapMode: TWrapMode; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY,
  ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
        LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
        LImage := CreateBitmap(AImageFileName);
        try
          LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
        finally
          LImage.Free;
        end;
        LBitmap.Canvas.FillRect(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), 0, 0, [], AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle;
  ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := AScale;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.FillRect(RectF(0, 0, 80, 80), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

type
  TOpenTransform = class(TTransform);

procedure TSkFMXCanvasTests.TestFillRectGradientRadial(const APoints, AModulateColor: string; AGradientScaleX,
  AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX,
  AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent,
  ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single;
  AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Radial;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Scale.Point := PointF(AGradientScaleX, AGradientScaleY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.Position.Point := PointF(AGradientPosX, AGradientPosY);
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationAngle := AGradientRotationDegree;
          LBitmap.Canvas.Fill.Gradient.RadialTransform.RotationCenter.Point := PointF(AGradientRotationCenterX, AGradientRotationCenterY);
          TOpenTransform(LBitmap.Canvas.Fill.Gradient.RadialTransform).Skew.Point := PointF(AGradientSkewX, AGradientSkewY);
          LBitmap.Canvas.FillRect(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY,
  AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX,
  ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean;
  const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := ABitmapScale;
    if LBitmap.Canvas.BeginScene then
      try
        if ADrawBackgroundChess then
        begin
          DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
            TAlphaColors.Black, TAlphaColors.White);
        end;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.SetMatrix(
            CreateMatrix(ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg));
          if AApplyClip then
          begin
            LBitmap.Canvas.IntersectClipRect(RectF(0.1 * LBitmap.Width, 0.1 * LBitmap.Height,
              0.9 * LBitmap.Width, 0.9 * LBitmap.Height));
          end;
          LBitmap.Canvas.Blending := ABlending;
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LBitmap.Canvas.Fill.Gradient.StartPosition.Point := PointF(AStartX, AStartY);
          LBitmap.Canvas.Fill.Gradient.StopPosition.Point := PointF(AStopX, AStopY);
          LBitmap.Canvas.FillRect(RectF(ADestLeftPercent * LBitmap.Width, ADestTopPercent * LBitmap.Height,
            ADestRightPercent * LBitmap.Width, ADestBottomPercent * LBitmap.Height), 0, 0, [], AOpacity);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.5);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := MakeColor(TAlphaColors.Black, 0.5);
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(0, 0, 50, 50));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Black;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
          LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
          LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := False;
        LBitmap.Canvas.IntersectClipRect(RectF(20, 20, 95, 95));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 1);
        LBitmap.Canvas.Fill.Color := TAlphaColors.Green;
        LBitmap.Canvas.FillRect(RectF(50, 50, 100, 100), 0, 0, [], 0.7);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX,
  ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom,
  AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ASurfaceWidth, ASurfaceHeight);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(ADestLeft, ADestTop, ADestRight, ADestBottom), 0, 0, [], AOpacity);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithModulateColor(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.6);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := TAlphaColors.Blue;
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.7);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode;
  AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent,
  ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          if AXRadius < 1 + TEpsilon.Vector then
            AXRadius := AXRadius * LDestRect.Width;
          if AYRadius < 1 + TEpsilon.Vector then
            AYRadius := AYRadius * LDestRect.Height;
          LBitmap.Canvas.FillRect(LDestRect, AXRadius, AYRadius,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          if AXRadius < 1 + TEpsilon.Vector then
            AXRadius := AXRadius * LDestRect.Width;
          if AYRadius < 1 + TEpsilon.Vector then
            AYRadius := AYRadius * LDestRect.Height;
          LBitmap.Canvas.FillRect(LDestRect, AXRadius, AYRadius,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithClipping(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithClipping2(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LClipRects: TArray<TRectF>;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    LClipRects := [RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height)];
    if LBitmap.Canvas.BeginScene(@LClipRects) then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectBitmapWithMatrix(const AImageFileName, AModulateColor: string;
  AWrapMode: TWrapMode; AScaleX, AScaleY, ADeltaX, ADeltaY, ARotationDegree, AXRadius, AYRadius: Single;
  const ACornersString: string; ACornerType: TCornerType; ASmallSize: Boolean; ALeftPercent, ATopPercent, ARightPercent,
  ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LImage: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    if ASmallSize then
      LBitmap.SetSize(300, 300)
    else
      LBitmap.SetSize(600, 600);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.SetMatrix(
          CreateMatrix(AScaleX, AScaleY, ADeltaX * LBitmap.Width, ADeltaY * LBitmap.Height, ARotationDegree));
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Bitmap;
          LBitmap.Canvas.Fill.Bitmap.WrapMode := AWrapMode;
          LImage := CreateBitmap(AImageFileName);
          try
            LBitmap.Canvas.Fill.Bitmap.Bitmap := LImage;
          finally
            LImage.Free;
          end;
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradient(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradientWithChessBackground(const APoints, AModulateColor: string;
  AGradientStyle: TGradientStyle; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType;
  ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := False;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
          LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
          ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectGradientWithResource(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Gradient;
            LBrushObject2.Brush.Gradient.Style := TGradientStyle.Linear;
            ApplyGradientPoints(LBrushObject2.Brush.Gradient, '[claRed;0 claNull;0.5 claBlue;1]');
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 1);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolid(const AColor, AModulateColor: string; ABlending: Boolean; AXRadius,
  AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent, ATopPercent, ARightPercent,
  ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := StringToAlphaColor(AColor);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithChessBackground(const AColor, AModulateColor: string;
  ABlending: Boolean; AXRadius, AYRadius: Single; const ACornersString: string; ACornerType: TCornerType; ALeftPercent,
  ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LDestRect: TRectF;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    LBitmap.BitmapScale := 1;
    if LBitmap.Canvas.BeginScene then
      try
        DrawChessBackground(LBitmap.Canvas, Min(LBitmap.Width, LBitmap.Height) / 15 / LBitmap.BitmapScale,
          TAlphaColors.Black, TAlphaColors.White);
        LBitmap.Canvas.Blending := ABlending;
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.White then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        try
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := StringToAlphaColor(AColor);
          LDestRect := RectF(ALeftPercent * LBitmap.Width, ATopPercent * LBitmap.Height,
            ARightPercent * LBitmap.Width, ABottomPercent * LBitmap.Height);
          LBitmap.Canvas.FillRect(LDestRect, AXRadius * LDestRect.Width, AYRadius * LDestRect.Height,
            StringToCorners(ACornersString), AOpacity, ACornerType);
        finally
          if LModulateCanvas <> nil then
            LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithClipping(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 0.85);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithClipping2(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LModulateCanvas: IModulateCanvas;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
          Exit;
        LModulateCanvas.ModulateColor := TAlphaColors.Blue;
        try
          LBitmap.Canvas.IntersectClipRect(RectF(LBitmap.Width * 0.5, 0, LBitmap.Width, LBitmap.Height));
          LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
          LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 0.85);
        finally
          LModulateCanvas.ModulateColor := TAlphaColors.White;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestFillRoundRectSolidWithResource(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LBrushObject: TBrushObject;
  LBrushObject2: TBrushObject;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LBrushObject := TBrushObject.Create(nil);
        try
          LBrushObject2 := TBrushObject.Create(nil);
          try
            LBrushObject2.Brush.Kind := TBrushKind.Solid;
            LBrushObject2.Brush.Color := MakeColor(TAlphaColors.Green, 0.6);
            LBrushObject.Brush.Kind := TBrushKind.Resource;
            LBrushObject.Brush.Resource.StyleResource := LBrushObject2;
            LBitmap.Canvas.Fill.Kind := TBrushKind.Resource;
            LBitmap.Canvas.Fill.Resource.StyleResource := LBrushObject;
            LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 16, 16, AllCorners, 1);
          finally
            LBrushObject2.Free;
          end;
        finally
          LBrushObject.Free;
        end;
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXCanvasTests.TestSaveState(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LSaveState: TCanvasSaveState;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(100, 100);
    if LBitmap.Canvas.BeginScene then
      try
        LSaveState := LBitmap.Canvas.SaveState;
        LBitmap.Canvas.IntersectClipRect(RectF(25, 25, 75, 75));
        LBitmap.Canvas.RestoreState(LSaveState);
        LBitmap.Canvas.Clear(TAlphaColors.Red);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkFMXCanvasTests);
end.
