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
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x8cHD7/////Hxw8fv////8/vX1+/////////////9r/+v/6//P/kP8Y/2j/Sf//////////8')]
    procedure TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x8cHD7/////Hxw8fv////8/vX1+/////////////9r/+v/6//P/kP8Y/2j/Sf//////////8')]
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
    [TestCase('1', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,1,0.98,H744cOTMnDh/vzhw5MycOP+/vnv27t6+////////3/7/9N/q/9i/+H+8/3z69PXx6/nf+b/pf+E')]
    [TestCase('2', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,H744cOTMnDh//nhw58/ePP///nn3794+///+ef/v/j9wD+A/4H/A94Hvg88Hjw4PHB94H/Ae8B4')]
    [TestCase('3', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,Red,1,0.98,P39/f39///////9/f3///////39/f////////////////////////3////////////////////8')]
    [TestCase('5', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,0.3,0.98,/sbGhoQAAvf//vbnx0dO////9//Xx8/////3/9fHz/8f4R/hP+E5wXnPe99/v3+/f3N+YTzgAcA')]
    [TestCase('6', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('7', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0.3,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('8', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,Red,0.3,0.98,//////Pj4cD/////8+fvzP/////7///d//////v//90AAAAAAAAAAAAAAOAA8AHgAewB/Q37D/8')]
    procedure TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,+8u5NQADgp////l1Q0fO3///+33nR+7f//////fP79/vr++nr7cPcU0HAYO0j/Sf5J3YmMOZw/8')]
    procedure TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',   '[claBlack;0 claWhite;1],claNull,Linear,False,1,1,0.98,/4GBgYH///////Hh4f//////8eHh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('2',   '[$7F000000;0 $7FFFFFFF;1],claNull,Linear,False,1,1,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADgAGAAYABwAPAA8AD//////////8')]
    [TestCase('3',   '[claBlack;0 claWhite;1],claNull,Linear,False,1,0.5,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADwAOAAYABwAPAA8AD//////////8')]
    [TestCase('4',   '[claBlack;0 claWhite;1],claNull,Linear,True,1,1,0.98,/4GBgYH///////Hh4f//////8eHh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('5',   '[$7F000000;0 $7FFFFFFF;1],claNull,Linear,True,1,1,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADgAGAAYABwAPAA8AD//////////8')]
    [TestCase('6',   '[claBlack;0 claWhite;1],claNull,Linear,True,1,0.5,0.98,/4GBgYH//////fHhwf//////8eHB/////////8H////AA8ADwAOAAYABwAPAA8AD//////////8')]
    [TestCase('7',   '[claBlack;0 claWhite;1],claNull,Linear,False,2,1,0.98,/8CAwMDAwMD//PDhw8fMzP/88OHDx+zs///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('8',   '[$7F000000;0 $7FFFFFFF;1],claNull,Linear,False,2,1,0.98,/8CAwMDAwMD//PDhw8fMzP/88OHDx+zs//////////////AA4ADgAOAA4ADgAOAA4ADgAOAA4AA')]
    [TestCase('9',   '[claBlack;0 claWhite;1],claNull,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('10',  '[claBlack;0 claWhite;1],claNull,Linear,True,0.5,1,0.98,Dw8P/////////8//////////3//////////f//////+B/4H/gf////////////////////////8')]
    [TestCase('11',  '[$7F000000;0 $7FFFFFFF;1],claNull,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('12',  '[claBlack;0 claWhite;1],claNull,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('13',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,False,1,1,0.98,w4GBgYH///////Hh4f//////8+Hh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('14',  '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Linear,False,1,1,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('15',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,False,1,0.5,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('16',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,True,1,1,0.98,w4GBgYH///////Hh4f//////8+Hh/////////+H////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('17',  '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Linear,True,1,1,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('18',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,True,1,0.5,0.98,/4GBgYH//////fHhwf//////8+HB/////////8H////AA4ABgAGAAYABwAPAA/////////////8')]
    [TestCase('19',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zs///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('20',  '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx+zs//////////////////////AA4ADgAOAA4ADgAOAA4AA')]
    [TestCase('21',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('22',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,True,0.5,1,0.98,Dw8P/////////8//////////3//////////f//////+B/4H/gf////////////////////////8')]
    [TestCase('23',  '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('24',  '[claBlack;0.2 claWhite;0.8],claNull,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('25',  '[claBlack;-1 claWhite;0.8],claNull,Linear,True,1,1,0.98,gYGBgcP//////fHhw//////98eHD/////////8P////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('26',  '[claBlack;0 claWhite;2],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+PhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('27',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8000000000000000000000000000000000')]
    [TestCase('28',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('29',  '[claBlack;0 claWhite;1],claRed,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('30',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('31',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('32',  '[claBlack;0 claWhite;1],claRed,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('33',  '[claBlack;0 claWhite;1],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('34',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPHz/8f/x//H/8f/x//H/8f/x//H/8f/x//H/8')]
    [TestCase('35',  '[claBlack;0 claWhite;1],claRed,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36',  '[claBlack;0 claWhite;1],claRed,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////P//////+B/4H/gf////////////////////////8')]
    [TestCase('37',  '[$7F000000;0 $7FFFFFFF;1],claRed,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('38',  '[claBlack;0 claWhite;1],claRed,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('39',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('40',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('41',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('42',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('43',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('44',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('45',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('46',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPHz/8f/x//H/8f/x//H/8f/x//H/8f/x//H/8')]
    [TestCase('47',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('48',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////P//////+B/4H/gf////////////////////////8')]
    [TestCase('49',  '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('50',  '[claBlack;0.2 claWhite;0.8],claRed,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('51',  '[claBlack;-1 claWhite;0.8],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('52',  '[claBlack;0 claWhite;2],claRed,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('53',  '[claRed;0 claBlue;1],claNull,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('54',  '[$7FFF0000;0 $7F0000FF;1],claNull,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('55',  '[claRed;0 claBlue;1],claNull,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('56',  '[claRed;0 claBlue;1],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('57',  '[$7FFF0000;0 $7F0000FF;1],claNull,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('58',  '[claRed;0 claBlue;1],claNull,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('59',  '[claRed;0 claBlue;1],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8AAAAAAAAAAAAAAAA')]
    [TestCase('60',  '[$7FFF0000;0 $7F0000FF;1],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8AAAAAAAAAAAAAAAA')]
    [TestCase('61',  '[claRed;0 claBlue;1],claNull,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('62',  '[claRed;0 claBlue;1],claNull,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+////////////////8A/wD/AP+B//////////////////////8')]
    [TestCase('63',  '[$7FFF0000;0 $7F0000FF;1],claNull,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('64',  '[claRed;0 claBlue;1],claNull,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8AAAAAAAA')]
    [TestCase('65',  '[claRed;0.2 claBlue;0.8],claNull,Linear,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8H//////8PDwf///////////8ADwAPAA8ADwAPAA/////8')]
    [TestCase('66',  '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Linear,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('67',  '[claRed;0.2 claBlue;0.8],claNull,Linear,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('68',  '[claRed;0.2 claBlue;0.8],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8H//////8PDwf///////////8ADwAPAA8ADwAPAA/////8')]
    [TestCase('69',  '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Linear,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('70',  '[claRed;0.2 claBlue;0.8],claNull,Linear,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8+PDx8f///////////////////+AAYABgAGAAYABgAGAAYAB//8')]
    [TestCase('71',  '[claRed;0.2 claBlue;0.8],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAP////////////////////8')]
    [TestCase('72',  '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Linear,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPH3/8f/x//H/8f/x//H/8f/wAAAAAAAAAAAAA')]
    [TestCase('73',  '[claRed;0.2 claBlue;0.8],claNull,Linear,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('74',  '[claRed;0.2 claBlue;0.8],claNull,Linear,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////8A/wD/AP+B//////////////////////8')]
    [TestCase('75',  '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Linear,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('76',  '[claRed;0.2 claBlue;0.8],claNull,Linear,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('77',  '[claRed;-1 claBlue;0.8],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8H//////+PDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('78',  '[claRed;0 claBlue;2],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P///////////9//n/+f/4//D/8AAAAAAAAAAAAAAAAAAA')]
    [TestCase('79',  '[claRed;0 $7F000000;0.5 claBlue;1],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8+PDw8P/////////////////wAPAA4ABgAGAAYABgAGAAcAD//8')]
    [TestCase('80',  '[claRed;0 $7F000000;1 claBlue;0.5],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P////////////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('81',  '[claRed;0 $7F000000;-1 claBlue;0.5],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+PhwcH////////////AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('82',  '[claRed;1 $7F000000;0.5 claBlue;0],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P////z4+PDw/+AAYABgAGAAcADwAP///////////////8')]
    [TestCase('83',  '[claRed;1 $7F000000;0 claBlue;0.5],claNull,Linear,True,1,1,0.98,/4GBgYGBgf///fHhwcHB////8+PDw8P//////8PDw//AA8ADwAPAA8ADwAPAA8AD//////////8')]
    [TestCase('84',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Linear,True,1,1,0.98,/4H//4GBgf///f//wcPF///9///Dw8f//////8PDx/+AAYABwAPAA8ADwAPAA/////////////8')]
    [TestCase('85',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Linear,False,1,1,0.98,/4H//4GBgf///f//wcPF///9///Dw8f//////8PDx/+AAYABwAPAA8ADwAPAA/////////////8')]
    [TestCase('86',  '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Linear,True,2,0.5,0.98,/8CAgMDA/////PDhw8f////88OHD5//////////////wAOAA4ADgAOAA4ADgAOAA4AD///////8')]
    [TestCase('87',  '[],claNull,Linear,True,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('88',  '[claBlack;0 claWhite;1],claNull,Radial,False,1,1,0.98,/4GBmZmBgf////H5+cHB////////z8/////////fz//gB8ADwAPAA8ADwAPAA+AHwAPIE/////8')]
    [TestCase('89',  '[$7F000000;0 $7FFFFFFF;1],claNull,Radial,False,1,1,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('90',  '[claBlack;0 claWhite;1],claNull,Radial,False,1,0.5,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('91',  '[claBlack;0 claWhite;1],claNull,Radial,True,1,1,0.98,/4GBmZmBgf////H5+cHB////////z8/////////fz//gB8ADwAPAA8ADwAPAA+AHwAPIE/////8')]
    [TestCase('92',  '[$7F000000;0 $7FFFFFFF;1],claNull,Radial,True,1,1,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('93',  '[claBlack;0 claWhite;1],claNull,Radial,True,1,0.5,0.98,/4GBmZmBgf///fH52cPF///////fz8/////////f///gB8ADwAPBg8GDwAPAA+AH8A/4H/////8')]
    [TestCase('94',  '[claBlack;0 claWhite;1],claNull,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('95',  '[$7F000000;0 $7FFFFFFF;1],claNull,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P////////////4P+A/wD+APwA+AD4APAA8ADwAfAD8Ac')]
    [TestCase('96',  '[claBlack;0 claWhite;1],claNull,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('97',  '[claBlack;0 claWhite;1],claNull,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('98',  '[$7F000000;0 $7FFFFFFF;1],claNull,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('99',  '[claBlack;0 claWhite;1],claNull,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('100', '[claBlack;0.2 claWhite;0.8],claNull,Radial,False,1,1,0.98,/4GZvb2Zgf////n9/dnB////////383////////fzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('101', '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Radial,False,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('102', '[claBlack;0.2 claWhite;0.8],claNull,Radial,False,1,0.5,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('103', '[claBlack;0.2 claWhite;0.8],claNull,Radial,True,1,1,0.98,/4GZvb2Zgf////n9/dnB////////383////////fzf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('104', '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Radial,True,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('105', '[claBlack;0.2 claWhite;0.8],claNull,Radial,True,1,0.5,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwYPDw8PDwYPAA8ADwAPAA/////8')]
    [TestCase('106', '[claBlack;0.2 claWhite;0.8],claNull,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('107', '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Radial,False,2,1,0.98,/8CAgICDh4f//PDhw8fPz//88OHDx8/P///////////wAPAA8ADwAPAA8ADwAPAA8APwB/AP8A8')]
    [TestCase('108', '[claBlack;0.2 claWhite;0.8],claNull,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('109', '[claBlack;0.2 claWhite;0.8],claNull,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////7+/////////v//////+B/4H/gf////////////////////////8')]
    [TestCase('110', '[$7F000000;0.2 $7FFFFFFF;0.8],claNull,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('111', '[claBlack;0.2 claWhite;0.8],claNull,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s//////////////gA+AD4APgA+AD4APgA+AD4APgA+AA')]
    [TestCase('112', '[claBlack;-1 claWhite;0.8],claNull,Radial,True,1,1,0.98,/4GZvb2Zgf///fn9/dvF////////38/////////fz//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('113', '[claBlack;0 claWhite;2],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz8///////+/Pz//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('114', '[claBlack;0 claWhite;1],claRed,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('115', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('116', '[claBlack;0 claWhite;1],claRed,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('117', '[claBlack;0 claWhite;1],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('118', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('119', '[claBlack;0 claWhite;1],claRed,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('120', '[claBlack;0 claWhite;1],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('121', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPHz/8f/x//H/8f/x//H/8f/x//H/8f/x//H/8')]
    [TestCase('122', '[claBlack;0 claWhite;1],claRed,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('123', '[claBlack;0 claWhite;1],claRed,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////P//////+B/4H/gf////////////////////////8')]
    [TestCase('124', '[$7F000000;0 $7FFFFFFF;1],claRed,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('125', '[claBlack;0 claWhite;1],claRed,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('126', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('127', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('128', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('129', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('130', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98+PDx8X///3z4/////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('131', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8eHDx8f////x4f////8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('132', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///////////wAPAA8ADwAPAA8ADwAPAA8ADwAPAA8AA')]
    [TestCase('133', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OHDx8zM///w4cPHz/8f/x//H/8f/x//H/8f/x//H/8f/x//H/8')]
    [TestCase('134', '[claBlack;0.2 claWhite;0.8],claRed,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('135', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////P//////+B/4H/gf////////////////////////8')]
    [TestCase('136', '[$7F000000;0.2 $7FFFFFFF;0.8],claRed,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('137', '[claBlack;0.2 claWhite;0.8],claRed,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+7s///w4ePn7uwAAAf/B/8H/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('138', '[claBlack;-1 claWhite;0.8],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('139', '[claBlack;0 claWhite;2],claRed,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('140', '[claRed;0 claBlue;1],claNull,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('141', '[$7FFF0000;0 $7F0000FF;1],claNull,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3///////94HnAOcA5gBmAGcA5wDngefn5//n/+AAA')]
    [TestCase('142', '[claRed;0 claBlue;1],claNull,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3//////f94HnAOYAZgBmAGYAZwDngefD5//n/+AAA')]
    [TestCase('143', '[claRed;0 claBlue;1],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('144', '[$7FFF0000;0 $7F0000FF;1],claNull,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3///////94HnAOcA5gBmAGcA5wDngefn5//n/+AAA')]
    [TestCase('145', '[claRed;0 claBlue;1],claNull,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF///98fHz9/X///3//////f94HnAOYAZgBmAGYAZwDngefD5//n/+AAA')]
    [TestCase('146', '[claRed;0 claBlue;1],claNull,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP/////////////////////wP/D/8f/z//f49+D/wP+A/4D/AP8A/wA')]
    [TestCase('147', '[$7FFF0000;0 $7F0000FF;1],claNull,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//9v/f///////2/9////8f/x//H/4f8B/AH4AfAB8AHgAeAB4AHAA')]
    [TestCase('148', '[claRed;0 claBlue;1],claNull,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('149', '[claRed;0 claBlue;1],claNull,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z8/////////////////D/8P/vf////////////////////////8')]
    [TestCase('150', '[$7FFF0000;0 $7F0000FF;1],claNull,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('151', '[claRed;0 claBlue;1],claNull,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+/v///w4ePn7+8AAAf/B/8H/wf/B/8H/wf8B/gH8AfgB+A')]
    [TestCase('152', '[claRed;0.2 claBlue;0.8],claNull,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////Hz9//P89573bvb29vb3bvee8/zx+PAA/////8')]
    [TestCase('153', '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Radial,False,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz99X///3//f////98PngecA5wDnAOcA54Hnw+f/5//j/8AAA')]
    [TestCase('154', '[claRed;0.2 claBlue;0.8],claNull,Radial,False,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8fHz99f//////f////94HnAOcA5wDnAOcA5wDngef/5//j/8AAA')]
    [TestCase('155', '[claRed;0.2 claBlue;0.8],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////Hz9//P89573bvb29vb3bvee8/zx+PAA/////8')]
    [TestCase('156', '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Radial,True,1,1,0.98,/4GBgYGBgf///fHhw8fF///98fHz99X///3//f////98PngecA5wDnAOcA54Hnw+f/5//j/8AAA')]
    [TestCase('157', '[claRed;0.2 claBlue;0.8],claNull,Radial,True,1,0.5,0.98,/4GBgYGBgf///fHhw8fF////8fHz99f//////f////94HnAOcA5wDnAOcA5wDngef/5//j/8AAA')]
    [TestCase('158', '[claRed;0.2 claBlue;0.8],claNull,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OfP39/////////////wAPAA8A/wP/B/8P/x//H/8//z//P/8/8')]
    [TestCase('159', '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Radial,False,2,1,0.98,///AwMDAwMD///Dhw8fMzP//8OfP39/e///w58/f3/8f/x//H/8f/h/wH8AfgB+AHwAfAB8AHgA')]
    [TestCase('160', '[claRed;0.2 claBlue;0.8],claNull,Radial,False,-2,0.5,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('161', '[claRed;0.2 claBlue;0.8],claNull,Radial,True,0.5,1,0.98,Dw8PD////////8/P////////z+/////////f///////D/6X/mf////////////////////////8')]
    [TestCase('162', '[$7FFF0000;0.2 $7F0000FF;0.8],claNull,Radial,True,-1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('163', '[claRed;0.2 claBlue;0.8],claNull,Radial,True,3,0.5,0.98,///gwMDAwMD///Dhw8fOzP//8OHj5+/v///w4ePn7+8AAAf/B/8H/wf/B/8H/wf/B/8H/gf8B/g')]
    [TestCase('164', '[claRed;-1 claBlue;0.8],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8fHx8fP///////Hz8//AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('165', '[claRed;0 claBlue;2],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///f+////D/8P/w//D///9/73/vH4/////8')]
    [TestCase('166', '[claRed;0 $7F000000;0.5 claBlue;1],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('167', '[claRed;0 $7F000000;1 claBlue;0.5],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8enp8fH//////+/z/f/QC/AP4AfgB+AH4AfwD9AL3DvH4/////8')]
    [TestCase('168', '[claRed;0 $7F000000;-1 claBlue;0.5],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHh8fH//////+fz/f/QC/AP4AfgB+AH4AfwD9AL3DvH4/////8')]
    [TestCase('169', '[claRed;1 $7F000000;0.5 claBlue;0],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB/////+/vz83//////+/Pzf/AA8ADw8PDw8PDw8PAA8ADwAPAA/////8')]
    [TestCase('170', '[claRed;1 $7F000000;0 claBlue;0.5],claNull,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF///9+e3N6/X//////83r9f/AA8ADwAPBg8GDwAPAA8ADwAPAA/////8')]
    [TestCase('171', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Radial,True,1,1,0.98,/4GBgYGBgf///fHhwcPF////9+vr18////////vf///iR8GDy9PH48fjy9PBg+JH8A/4H/////8')]
    [TestCase('172', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Radial,False,1,1,0.98,/4GBgYGBgf///fHhwcPF////9+vr18////////vf///iR8GDy9PH48fjy9PBg+JH8A/4H/////8')]
    [TestCase('173', '[claRed;1 $7F000000;0 claBlue;0.5 claWhite;0.49],claNull,Radial,True,2,0.5,0.98,///AwcfMyNj///Dhx8/O3P//8OHHz87c////////////wP+A/gD8APwB+A/wGPAw8GPwR/BP8M8')]
    [TestCase('174', '[],claNull,Radial,True,1,1,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('175', '[claRed;0.3],claNull,Radial,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('176', '[claRed;0.3],claBlue,Radial,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8eHhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('177', '[claRed;0.3],claNull,Linear,True,1,1,0.98,/4GBgYGBgf////HhwcHB////8eHBwcH//////8HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('178', '[claRed;0.3],claBlue,Linear,True,1,1,0.98,/4GBgYGBgf////Hh4cHB////8eHhwcH//////+HDwf/AA8ADwAPAA8ADwAPAA8ADwAPAA/////8')]
    [TestCase('179', '[claRed;0 claBlue;1 claBlue;1],claNull,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('180', '[claRed;0 claRed;0 claBlue;1],claNull,Radial,False,1,1,0.98,/4GBgYGBgf////HhwcHB////8fHx8fH///////fz///cO/gf8A/wD/AP8A/4H9w73/vH4/////8')]
    [TestCase('181', '[claRed;0 claBlue;1 claBlue;1],claNull,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('182', '[claRed;0 claRed;0 claBlue;1],claNull,Linear,False,1,1,0.98,/4GBgYGBgf////Hh4cHB////8+Pjw8P/////////////////wAOAAYABgAGAAYABgAGAAcAD//8')]
    [TestCase('183', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,1,0.98,////gYGBgf/////hw8fP/////+PDx8//////////////////wAPAA8ADwAPAA4ABgAGAAcAD//8')]
    [TestCase('184', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,1,0.98,/+fDgYHD5/////Phw8fv////8/Hz9//////////////wD+AH4AfgB+AH4AfgB/AP+B////////8')]
    [TestCase('185', '[$00FF0000;0 $FF0000FF;1],claBlack,Linear,True,1,0.5,0.98,////gYGBgf/////hw8fP/////+PDx8//////////////////wAPAA8ADwAPAA4ABgAGAAcAD//8')]
    [TestCase('186', '[$00FF0000;0 $FF0000FF;1],claBlack,Radial,True,1,0.5,0.98,/+fDgYHD5/////Phw8fv////8/Hz9//////////////wD+AH4AfgB+AH4AfgB/AP+B////////8')]
    procedure TestFillRectGradient(const APoints, AModulateColor: string; AGradientStyle: TGradientStyle; ABlending: Boolean; const AScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('2',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('3',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('4',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39///////////////w7/Bv8G/wf/B/8P/w//P////+//n/8')]
    [TestCase('5',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('6',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('7',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('8',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/vz9/f////7+/P39/////v7//f//+PAAeAB4ADgAcABgAGAAwAHAD4APgA8AA')]
    [TestCase('9',  '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('10', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('11', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('12', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,1,0.98,fx8PDw8fHz//3+/v79/f///f7+/v39/////////////z/4D/AP8A/wH/Af8D/wf/B/8f/w//n/8')]
    [TestCase('13', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('14', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('15', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('16', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39/////v7+/f3//+AP+A/4D/AP8A/gD+APwA+AD4APAA8AA')]
    [TestCase('17', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,False,0.9,0.5,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8B/wB/AH8A/wD/Af8B/wP/B/8H/w//D/8')]
    [TestCase('18', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,g4ODg4ODw/////Pjw4PD////8/Pz4+P////////v+//4P/A/8D/wP/g/+G//7+/v4A////////8')]
    [TestCase('19', '[claRed;0 claNull;0.5 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,False,0.9,1,0.98,g5uro7uDg/////vj+4OD//////f/y+P////////v///8f/h/+F/4f/x/////7+/v44////////8')]
    [TestCase('20', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('21', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('22', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('23', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,fx8PDw8fHz9/P+/vbz/f/3+/7+9vP9//f7/v7/8////+gP+A/8D/AP4A/gD8AP4A+AD8APAA8AA')]
    [TestCase('24', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('25', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('26', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('27', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///8gP9A/kD/AP6A/wD8AP4A+AD0APgA8AA')]
    [TestCase('28', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('29', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('30', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('31', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,1,0.98,fx8PBw8fHz9/H+/n7z/f/38f7+fvP9///x/v5/8////+gP/A/8D/AP+A/wD+AP4A+AD8APAA8AA')]
    [TestCase('32', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,False,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('33', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,False,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('34', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,False,True,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('35', '[claRed;0 claBlue;1],claNull,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PBw8fHz9/P8/Hbx/f/3+/z8fv39//f7/vx//f///+gP/A/8D/AP6A/wD8AP4A+AD8AvgF8AI')]
    [TestCase('36', '[claRed;0 claBlue;1],claBlack,1,0.7,0,20,-30,0,0,0.3,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,True,0.9,0.5,0.98,fx8PDw8fHz9/P8/Pbx/f/3+/z8/v39//f//v3///3/9B6gB1AH8A/wD/Af8D9UPqg9VH6qvVV+o')]
    [TestCase('37', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,g4ODg4ODw/+7u+PDw4PH//v78/Pz49f///v/8//z3/9/8D/wP/A/8D/wH/Af8F/wP/AAAAAAAAA')]
    [TestCase('38', '[claRed;0 claNull;0.5 claBlue;1],claNull,1,1,0,0,0,0.5,0.5,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,0,False,False,True,0.9,1,0.98,g5ODg6uDg/+7u+PD64OH/7+/9/f/y5f/v///9//79/9nkGfQZ9BnkGOQcRB4MHzwf/A/+AAAAAA')]
    procedure TestFillRectGradientRadial(const APoints, AModulateColor: string; AGradientScaleX, AGradientScaleY, AGradientPosX, AGradientPosY, AGradientRotationDegree, AGradientRotationCenterX, AGradientRotationCenterY, AGradientSkewX, AGradientSkewY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending, ADrawBackgroundChess: Boolean; const ABitmapScale, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,/vz48ODAgAD//Pjw48fOTP//////////////////////wP+A/wD+APwA+ADwAOAAwACAAAAAAAA')]
    [TestCase('2',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ABwAPAB8APwB/AP8B/wP/B/8P/x//P/8')]
    [TestCase('3',  '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,fz8fDwcDAQB/f39vR8fPzH9/f29Hx8/Mf/9//0//z/8D/wH/AP8AfwA/AB8ADwAHAAMAAQAAAAA')]
    [TestCase('4',  '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,/vz48ODAgAD//Pjx48fOTP/////////+//////////7/4P/A/4D/AP4A/AD4APAA4ADAAIAAAAA')]
    [TestCase('5',  '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ADwAfAD8AfwD/Af8D/wf/D/4f/D/4f/A')]
    [TestCase('6',  '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,False,0.98,fz8fDwcDAQB/f3/vx8fPzH9/f+/Hx8/Mf/9//9//z/4H/wP/Af8A/wB/AD8AHwAPAAcAAwABAAA')]
    [TestCase('7',  '[claRed;0 claBlue;1],claNull,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///9/f3//f3/////////////g/8D/gP8A/gD8APgA8ADwAPAA/////8')]
    [TestCase('8',  '[claRed;0 claBlue;1],claNull,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PDyf/AA8AHwA/AH8A/wH/A/8H/w//H//////8')]
    [TestCase('9',  '[claRed;0 claBlue;1],claNull,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8f/y//B/8D/wH/AP8AfwA/AB8ADwAPAA/////8')]
    [TestCase('10', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///9/f3//f3////////////vw/+D/wP+A/wD+APwA+ADwAPAA/////8')]
    [TestCase('11', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8PDyf/AB8APwB/AP8B/wP/B/8P3x+fP//////8')]
    [TestCase('12', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/4GBgYGBgf///fHhw8HJ///98eHDwcn//////8//z//D98H/wP/Af8A/wB/AD8AHwAPAA/////8')]
    [TestCase('13', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI////////////////////////AP4A/gD8APgA+ADwAOAA4ADgAP////8')]
    [TestCase('14', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+DHyP8ABwAPAB8AHwA/AH8AfwD/Af8H//////8')]
    [TestCase('15', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+f/yf8A/wB/AH8APwAfAB8ADwAHAAcAB/////8')]
    [TestCase('16', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI//////////7///////////8/gH8AfwD+APwA/AD4APAA4ADwAP////8')]
    [TestCase('17', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+DPyP8ADwAfAD8APwB/AP4A/gH8A/gH//////8')]
    [TestCase('18', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,False,0.98,/wAAAAAAAP//+PDg4MDI///48ODgwMj//////+f/y/8B/AD+AP4AfwA/AD8AHwAPAAcAD/////8')]
    [TestCase('19', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/3//9/fvz9/////////////gf4A/AD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('20', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8AfwA/AD8AfwD/AP8B/w//P/////////8')]
    [TestCase('21', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8H/wf/B/8H/wf/A/8D/wP/A/8H/wf/B/8')]
    [TestCase('22', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHx9/f2fnz8/f33//9/fvz9/f///////////4f+A/gD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('23', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z/fPz9//f/9v/8//3/8AfwA/AD8AfwD/A/8P/z////////////8')]
    [TestCase('24', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,Pw8HBw8PHz9/f2fnz8/f/39/Z+fPz9//f/9v/8//3/8P/w//D/8P/wf/B/8H/wf/B/8H/wf/B/8')]
    [TestCase('25', '[claRed;0 claBlue;1],claNull,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/vz9/f////7+/v39/////////////5/+B/AH8A/wD/Af8B/wP/A/8H/w//D/8')]
    [TestCase('26', '[claRed;0 claBlue;1],claNull,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8A/wB/AH8A/wD/Af8H/z////////////8')]
    [TestCase('27', '[claRed;0 claBlue;1],claNull,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39//////////3/8P/w//D/8P/w//B/8H/wf/B/8H/w//D/8')]
    [TestCase('28', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/vz9/f////7+/v39////////////////B/wH8A/wD/Af8B/wP/A/8H/w//D/8')]
    [TestCase('29', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39///////+//3/8A/wB/AH8A/wH/B/8f/3////////////8')]
    [TestCase('30', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PDw8fHz///+/v79/f////7+/v39//////////3/8f/x//H/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('31', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7/fvz9/f///////////h/8B/AD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('32', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHz///+/n78/f////7+fvz9///////+//3/8A/wB/AD8AfwD/A/8H/x//P/////////8')]
    [TestCase('33', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8H/wf/B/8H/wf/B/8H/wf/B/8H/wf/D/8')]
    [TestCase('34', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7/fv79/f///////////5/+B/gD8AfwB/AP8B/wH/A/8D/wf/B/8')]
    [TestCase('35', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHz///+/n78/f////7+fvz9///////+//3/8A/wB/AD8A/wH/B/8f/z////////////8')]
    [TestCase('36', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8P/w//D/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('37', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,fx8PBw8PHx///+/n78/f3///7+fvz9/f/////+//3/8P/w//D/8P/w//D/8P/w//D/8P/w//D/8')]
    [TestCase('38', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,fx8PBw8PHx//3+/n78/f3//f7+fvz9/f/////+//3/8P/w//D/8Pfw//D/8P/w//D/8P/w//D/8')]
    [TestCase('39', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,False,0.98,AAAAAAAAAAB/fHBhQ0dOTP98fG1bd35s//9//1//f/8D/wD/AP8AfwD/Af8B/wf/A/8H/w//j/8')]
    [TestCase('40', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,False,0.98,AAAAAAAAAAB/fHBhQ0dOTP98eG1bd35s/3z4bft3/mz+AP+A/8D/gP+A/wD+AP4A/AD8APgA8AA')]
    [TestCase('41', '[claRed;0 claBlue;1],claNull,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,fx8PBw8PHx//3+/n78/f3//f7+fvz9/f/////+//3/8H/wf/B/8H/wf/B/8H/wf/B/8H/wf/D/8')]
    [TestCase('42', '[claRed;0 claBlue;1],claNull,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,Pw8HBw8PHz8/DwfHz8/f/z8PB9fPz9//P/8P/8//3/8B/wH/Af8B/wH/AP8B/wH/A/8D/wf/D/8')]
    [TestCase('43', '[claRed;0.3 claNull;0.5 claBlue;0.7],claNull,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,False,0.98,f08HBw8PPz9/f2fnz8///39/d/fv7///f/9///////8R/xG/Gf8Z/xn/Gf8J/wn/C/8P/w//D/8')]
    [TestCase('44', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,/vz48ODAgAD//Pjw48fOTP//////////////////////wP+A/wD+APwA+ADwAOAAwACAAAAAAAA')]
    [TestCase('45', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ABwAPAB8APwB/AP8B/wP/B/8P/x//P/8')]
    [TestCase('46', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,fz8fDwcDAQB/f39vR8fPzH9/f29Hx8/Mf/9//0//z/8D/wH/AP8AfwA/AB8ADwAHAAMAAQAAAAA')]
    [TestCase('47', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,/vz48ODAgAD//Pjx48fOTP/////////+//////////7/4P/A/4D/AP4A/AD4APAA4ADAAIAAAAA')]
    [TestCase('48', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,AAEDBw8fP39/fXNnT19/f399c2dPX39/f31zZ09ff38ADwAfAD8AfwD/Af8D/wf/D/4f/D/4f/A')]
    [TestCase('49', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0,1,1,1,1,0,0,0,False,True,1,1,True,0.98,fz8fDwcDAQB/f3/vx8fPzH9/f+/Hx8/Mf/9//9//z/4H/wP/Af8A/wB/AD8AHwAPAAcAAwABAAA')]
    [TestCase('50', '[claRed;0 claBlue;1],claNull,0,0,1,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///3/f3//d3///f9/f/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('51', '[claRed;0 claBlue;1],claNull,1,1,0,0,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//u+HB44HJ////4cHjwc3////hwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('52', '[claRed;0 claBlue;1],claNull,1,0,0,1,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwc3///fhwf/9//8//D/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('53', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ/////f3//d3////9/f/93/8P/B/8P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('54', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//u+HB44HJ////4cHjwc3////hwf/9//8//D/8P/w//D/8P/w/+D/wP+A/wAAAAAA')]
    [TestCase('55', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/4GBgYGBgf//s+HB44HJ///34cHjwc3///fhwf/B//8/8D/4P/w//D/8P/w//D/8P/w//AAAAAA')]
    [TestCase('56', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/8AAAAAAAP//+ODAYADI///+/////97///7////////////////////////////////gHwAAAAA')]
    [TestCase('57', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wAAAAAAA///OODAYADL//+84MBgAM///7zgwPgH//////////////////////////+j/wAAAAA')]
    [TestCase('58', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wMAAAAAAP//O+DAYADI//+/4MBgANz//7/gwOP////////////////////////////4BwAAAAA')]
    [TestCase('59', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/+AAAAAAAP//+ODAYQDM///+/////97///7/////3/8//3//f//////////////////oPwAAAAA')]
    [TestCase('60', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wAAAAAAB///OODAYADP//+84MBgAM///7zgwPwX//////////////////7//v/8//jv8AAAAAA')]
    [TestCase('61', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1,0,0,0,False,True,1,1,True,0.98,/wcAAAAAAP//P+DAYADI//+/4MBgAMz//7/gwO/w/////P/+//7////////////////8FwAAAAA')]
    [TestCase('62', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fw8HBw8PHx9/P+fHTw/f33//59fPj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('63', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('64', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPD9/f////1/8P/9//4P/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('65', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fw8HBw8PHx9/P+fHTw/f33//99fPz9/f////1//P/9//wP/g/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('66', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHDw/f3z//x9ePj9/f////1/+P/9//wP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('67', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0,1,1,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,Pw8HBw8PHx8/P8fHTw/f3z//x9dPD9/f////1/8P/9//oP+A/4D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('68', '[claRed;0 claBlue;1],claNull,0,0,1,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fv39//f7/vx//f////AP+A/4D/gP8A/gD+APwA/AD4APgA8AA')]
    [TestCase('69', '[claRed;0 claBlue;1],claNull,1,1,0,0,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fvn9//f7/vx/+f////AP+A/4D/gP8A/gD+APwA/AD8APgA8AA')]
    [TestCase('70', '[claRed;0 claBlue;1],claNull,1,0,0,1,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f////gP/A/8D/gP+A/gD+APwA/AD8APgA8AA')]
    [TestCase('71', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78fv39//f7/vx//f////AP+A/4D/gP8A/gD+APwA/AD4APgA8AA')]
    [TestCase('72', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8fHz9/P+/Hbx/f/3+/78dvn9//f7/vx/+f////AP+A/4D/gP8A/gD+AP4A/AD8APAA8AA')]
    [TestCase('73', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0.15,0.15,0.85,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PDw8fHz9/P+/Pbx/f/3+/789vH9//f7/vz/8f///+gP5A/kD+AP6A/wD+AP4A/AD8APgA8AA')]
    [TestCase('74', '[claRed;0 claBlue;1],claNull,0,0,1,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79fvz9/f/7/v1//P/9//AP/A/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('75', '[claRed;0 claBlue;1],claNull,1,1,0,0,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvj9/f/7/v1/+P/9//AP/A/+D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('76', '[claRed;0 claBlue;1],claNull,1,0,0,1,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//AP/A/+D/gP+A/wD/AP4A/AD8APgA+AA')]
    [TestCase('77', '[claRed;0 claBlue;1],claNull,0.15,0.15,0.85,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79fvz9/f/7/v1//P/9//AP/A/8D/gP+A/wD/AP4A/gD+APgA+AA')]
    [TestCase('78', '[claRed;0 claBlue;1],claNull,0.85,0.85,0.15,0.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P8/Hbw/f33+/z9dvj9/f/7/v1/+P/9//AP/A/+D/gP+A/wD/AP4A/AD8APgA8AA')]
    [TestCase('79', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//gP8A/yD/QP8A/4D/AP8A/AD8APgA+AA')]
    [TestCase('80', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,fx8PBw8PHx9/P+/Hbw/f33+/79dvD9/ff7/v1/8P/9//gP8A/yD/QP8A/4D/AP8A/AD8APgA+AA')]
    [TestCase('81', '[claRed;0 claBlue;1],claNull,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,fx8PBw8PHx9/H+/n7y/f338f7+fvL9/f/x/v5/8v/9//AP+A/4D/gP8A/wD/AP4A/AD8APgA+AA')]
    [TestCase('82', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,1,True,0.98,gODw+PDw4MD//PD58/fuzP/+/P3/9/79//78/f///v3/Cv+F/8D/gP+A/wD+Bf4K/BX8KvgV9Co')]
    [TestCase('83', '[claRed;0 claBlue;1],claWhite,0.85,0.15,0.15,0.85,0,0.15,1,0.85,1,1.3,0,-20,30,True,False,0.9,0.5,True,0.98,gODw+PDw4MD/+PD48/fuzP/+/Pz/9/79//78/f///v1/Kv+V/8D/gP+A/wD+FX4qvBVcKqgVVCo')]
    [TestCase('84', '[claRed;0 claBlue;1],claNull,1.15,-0.15,-0.15,1.15,0,0.15,1,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,fx8PBw8PHx9/H+/n7y/f338f7+fvL9/f/x/v5/8v/9//AP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('85', '[claRed;0 claBlue;1],claNull,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,Pw8HBw8PHx9/f+fHzw/f339/58fPL9/f/3//x/8v/9//wP/g/8D/gP+A/wD/AP4A/gD8APgA+AA')]
    [TestCase('86', '[claRed;0.3 claNull;0.5 claBlue;0.7],claNull,1.15,-0.15,-0.15,1.15,0,-0.15,1.15,0.85,1,1.3,0,-20,30,True,True,0.9,1,True,0.98,fw9HBw8PPz9/P8fHTw///38/9/dvL////z/39+8v///xoOHA8cDxwPEA8IDwAPEA8ADwAvAF8AI')]
    procedure TestFillRectGradientStartEndPoint(const APoints, AModulateColor: string; AStartX, AStartY, AStopX, AStopY, ADestLeftPercent, ADestTopPercent, ADestRightPercent, ADestBottomPercent, ACanvasScaleX, ACanvasScaleY, ACanvasOffsetX, ACanvasOffsetY, ACanvasRotationDeg: Single; AApplyClip, ABlending: Boolean; const ABitmapScale, AOpacity: Single; ADrawBackgroundChess: Boolean; AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolid(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolid2(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,/8+Hh8////////fnz///////9+fP///////////////w//D/8P/w//////////////////////8')]
    procedure TestFillRectSolid3(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PH///Pjw8f+/f//8+Pj5//////////////wD/AP8A/wD/AB8AHwAfAB/wH/Af8B//8')]
    procedure TestFillRectSolid4(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PH///Pjw8f+/f//8+Pj5//////////////wD/AP8A/wD/AB8AHwAfAB/wH/Af8B//8')]
    procedure TestFillRectSolid5(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8DA8PH///Pjw8f+/f//8+Pj5//////////////wD/AP8A/wD/AB8AHwAfAB/wH/Af8B//8')]
    procedure TestFillRectSolid6(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('2',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('3',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d29DR09N/3//b/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('4',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d29DR09N/3//b/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('5',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('6',  '300,300,1,2,-50,-100,60,-200,-50,660,343,1,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d2dDR09Nf/93/0P/T/8AfwB/AD8APwAfAA8AHwAHAAcAAwADAAE')]
    [TestCase('7',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,true,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d29DR09N/3//b/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('8',  '300,300,1,2,-50,-100,60,-200,-50,660,343,0.5,false,0.98,Hw8HBwMDAQF/f3dnQ0dPTX9/d29DR09N/3//b/9H/03/gP/A/8D/4P/g//D/8P/4//z//P/+//4')]
    [TestCase('9',  '300,300,1,2,-50,-100,60,0,0,200,200,1,true,0.98,fz8///////9//////////3////////////////////9///////////////////////////////8')]
    [TestCase('10', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('11', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('12', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('13', '300,300,1,2,-50,-100,30,0,0,200,200,1,true,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('14', '300,300,1,2,-50,-100,30,0,0,200,200,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('15', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,true,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('16', '300,300,1,2,-50,-100,30,0,0,200,200,0.5,false,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('17', '300,300,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M//z44ftH/kzwAOAAwADAAIAAgAAAAAAAAAAAAAAAAAA')]
    [TestCase('18', '300,300,1,-2,50,500,30,-200,-50,660,343,1,false,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M//z44ftH/kzwAOAAwADAAIAAgAAAAAAAAAAAAAAAAAA')]
    [TestCase('19', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,true,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M///3/8f/T/8f/x//P/8//3//f/////////////////8')]
    [TestCase('20', '300,300,1,-2,50,500,30,-200,-50,660,343,0.5,false,0.98,4MDAgIAAAAD//PDhw0dOTP/88OHDR05M///3/8f/T/8f/x//P/8//3//f/////////////////8')]
    [TestCase('21', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('22', '300,300,-1,2,-50,-100,30,-200,-50,660,343,1,false,0.98,Hx8/P3////9/f39/f////39/f39/////f/9//3////8f/x//P/8/////f/////////////////8')]
    [TestCase('23', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,true,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('24', '300,300,-1,2,-50,-100,30,-200,-50,660,343,0.5,false,0.98,Hx8/P3////9/f39/f////39//39//////3//f//////wAPAA4ADgAMAAgACAAAAAAAAAAAAAAAA')]
    [TestCase('25', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,true,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5///f/9x/8P///8AAwABAAEAA4AH4AfwD/wP/x//n/////8')]
    [TestCase('26', '300,300,-1,2,-50,-100,-60,0,0,200,200,1,false,0.98,DwMBAYHD8/9/f3Fhw8f//39/cWHD5///f/9x/8P///8AAwABAAEAA4AH4AfwD/wP/x//n/////8')]
    [TestCase('27', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,true,0.98,DwMBAQHD8/9/f3FhQ8f//39/c2PD5////3//4//n/////P////7//v/8P/gP+AfwAfAAYAAAAAA')]
    [TestCase('28', '300,300,-1,2,-50,-100,-60,0,0,200,200,0.5,false,0.98,DwMBAQHD8/9/f3FhQ8f//39/c2PD5////3//4//n/////P////7//v/8P/gP+AfwAfAAYAAAAAA')]
    [TestCase('29', '300,300,1,-2,-50,500,30,0,0,200,200,1,true,0.98,/////+fhwMD/////5+fOzP/////39+7s/////////////////////////3/+H/4H/AX4APwA8AE')]
    [TestCase('30', '300,300,1,-2,-50,500,30,0,0,200,200,1,false,0.98,/////+fhwMD/////5+fOzP/////39+7s/////////////////////////3/+H/4H/AX4APwA8AE')]
    [TestCase('31', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,true,0.98,/////+fhwMD/////5+fOzP/////39+7s//////f37uwAAAAAAAAAAACAAeAD8AP8B/8H/w//D/8')]
    [TestCase('32', '300,300,1,-2,-50,500,30,0,0,200,200,0.5,false,0.98,/////+fhwMD/////5+fOzP/////39+7s//////f37uwAAAAAAAAAAACAAeAD8AP8B/8H/w//D/8')]
    procedure TestFillRectSolidWithMatrix(ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.98,///Dw8PD//////Pjw8f/////8+Pj5//////////////wD/AP8A/wD/AP8A/wD/AP//////////8')]
    procedure TestFillRectSolidWithModulateColor(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claNull,Tile,0,0,1,1,0.4,0.98,SHt7SP9K/wB/e3tp/0//TP/7++n/7//t//v76f/v/////5JJAkm22///tskACbbJ//+22wBJtkk')]
    [TestCase('2',  'android.svg,claRed,Tile,0,0,1,1,0.4,0.98,SHt7SP9K/wD/+3tI/8//zP//f1r/3//e/////v////4AAE22/7ZJJAAASbb/tkk2AABJJP+2SbY')]
    [TestCase('3',  'android.svg,claNull,Tile,0,0,1,1,1,0.98,SHt7SP9K/wD/+3tI/8//zP/7++n/7//t///76f/v/////7ZJAkm22///ttsCSbbb//+22wJJtsk')]
    [TestCase('4',  'android.svg,claRed,Tile,0,0,1,1,1,0.98,SHt7SP9K/wD/+/vI/8//xP/7/97/3//W///////f////////tsm///////+2Sb////+3/7bJv/8')]
    [TestCase('5',  'android.svg,claNull,Tile,0.15,0.15,0.85,0.85,0.4,0.98,00000000000000000000000000000000')]
    [TestCase('6',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,0.4,0.98,/8uBgYGB2/////Hhw8ff////9fHX19/////18d/33/8JIB20P/wJtAAADbQ//A20AAAJNAAAAAA')]
    [TestCase('7',  'android.svg,claNull,Tile,0.15,0.15,0.85,0.85,1,0.98,00000000000000000000000000000000')]
    [TestCase('8',  'android.svg,claRed,Tile,0.15,0.15,0.85,0.85,1,0.98,/8uBgYGB2/////Hhw8ff////9fXX19//////////////////9s/////////2T/////////////8')]
    [TestCase('9',  'android.svg,claNull,TileOriginal,0,0,1,1,0.4,0.98,Pz8///////9/f3//////////f/////////9///////////////////////////////////////8')]
    [TestCase('10', 'android.svg,claRed,TileOriginal,0,0,1,1,0.4,0.98,Pz9//////////3//////////f/////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('11', 'android.svg,claNull,TileOriginal,0,0,1,1,1,0.98,Pz9//////////3//////////f/////////9///////////////////////////////////////8')]
    [TestCase('12', 'android.svg,claRed,TileOriginal,0,0,1,1,1,0.98,Pz9//////////3//////////f/////////////////+gAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('13', 'android.svg,claNull,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,000000000000000000000000000')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,0.4,0.98,Pz////////9/f////////3//////////f/////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('15', 'android.svg,claNull,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,000000000000000000000000000')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,0.15,0.15,0.85,0.85,1,0.98,Pz9///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('17', 'android.svg,claNull,TileStretch,0,0,1,1,0.4,0.98,/8ODAADDx/////NhQ8fP////92VH79//////f//////wD9ALgAGAAYABgAHQC/AP+R/5H/2///8')]
    [TestCase('18', 'android.svg,claRed,TileStretch,0,0,1,1,0.4,0.98,/8PDAQHD5/////NhQ8fv////8+Pjx+/////z4+fn7/8P8C/0f/5//n/+f/4v9A/wBuAG4AJAAAA')]
    [TestCase('19', 'android.svg,claNull,TileStretch,0,0,1,1,1,0.98,/8PDAQHD5/////NhQ8fv/////2VH7//////////////wD9/rgAOAA4ADgAPQC/AP+b/5v/2///8')]
    [TestCase('20', 'android.svg,claRed,TileStretch,0,0,1,1,1,0.98,/8PDAQHD5/////NhQ8fv////8+Pjx+/////////////wD///0AvQC9AL0A/wD/AP/b/9v/2///8')]
    [TestCase('21', 'android.svg,claNull,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,/+fDgYHD5/////Phw8fv////9/fX1//////////////4H/gf4AfgB+AH4Af4H/w//D////////8')]
    [TestCase('22', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,0.4,0.98,/+fDgYHD5/////Phw8fv////8+Hj5+/////z4ePn7/8H4AfgH/gf+B/4H/gH4APAA8AAAAAAAAA')]
    [TestCase('23', 'android.svg,claNull,TileStretch,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////9/PX1//////////////4P/gf4AfgB+AH4A/4H/w//T////////8')]
    [TestCase('24', 'android.svg,claRed,TileStretch,0.15,0.15,0.85,0.85,1,0.98,/+fDgYHD5/////Phw8fv////8+nj7+/////////////8P/v////4P/g/+D////////////////8')]
    procedure TestFillRectBitmap(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; const ALeftPercent, ATopPercent, ARightPercent, ABottomPercent, AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claNull,Tile,true,1,0.98,S6gK+grqCOt7uPr6S+/M7/+9//9v/+3//73///////8IgGZubu79VQiyZkzu7mRUmAD1Xv7/9k4')]
    [TestCase('2',  'android.svg,claNull,Tile,true,0.5,0.98,SrxK+gpqCGt7vOr6S2vM7/u9//9Pf+3//73////////IgGBERu6ZVQiyRFDu7kReKAT1Xv7+dG4')]
    [TestCase('3',  'android.svg,claNull,Tile,false,1,0.98,S6gK+grqCOt7uPr6S+/M7/+9//9v/+3//73///////8IgGZubu79VQiyZkzu7mRUmAD1Xv7/9k4')]
    [TestCase('4',  'android.svg,claNull,Tile,false,0.5,0.98,SrxK+gpqCGt7vOr6S2vM7/u9//9Pf+3//73////////IgGBERu6ZVQiyRFDu7kReKAT1Xv7+dG4')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,Q/1fUH9A/wB7/9/Qf0P/zPv///p/6//u///////v//8AgGbu7u7/VQiyZkzu7mZMiAD3Tv7/9m8')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,Q/1fUH9A/0B7/9/Qf0P/zPv///p/6//u/////v/v/+9IgGREZu6ZVQiyRFTu7mRciAD1Xv7/dG4')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,Q/1fUH9A/wB7/9/Qf0P/zPv///p/6//u///////v//8AgGbu7u7/VQiyZkzu7mZMiAD3Tv7/9m8')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,Q/1fUH9A/0B7/9/Qf0P/zPv///p/6//u/////v/v/+9IgGREZu6ZVQiyRFTu7mRciAD1Xv7/dG4')]
    [TestCase('9',  'android.svg,claNull,TileOriginal,true,1,0.98,wv1CQkJCvUL7/fLiQ0f9zvv/9vbXx//////+9/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('10', 'android.svg,claNull,TileOriginal,true,0.5,0.98,wv1CQkJCvUL7/eLCQ0P9zvv/5tbXx//////+1/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('11', 'android.svg,claNull,TileOriginal,false,1,0.98,wv1CQkJCvUL7/fLiQ0f9zvv/9vbXx//////+9/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('12', 'android.svg,claNull,TileOriginal,false,0.5,0.98,wv1CQkJCvUL7/eLCQ0P9zvv/5tbXx//////+1/fv//90KqgVQAIAAAAAQAKoFVQqqBVUKqpVVCo')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,Gj1a//9avVo7P9r//1v/3ru/3v//3////7/+//////90KqgVUAoAAAAAUAqoFVQqqlVUKqpVVCo')]
    [TestCase('17', 'android.svg,claNull,TileStretch,true,1,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/zdf77//3////0/9K/0//7//v/+//4v9C/0DvAP4A5ABkA')]
    [TestCase('18', 'android.svg,claNull,TileStretch,true,0.5,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/79f77//3////0//C/0//7//v/+//4v9D/8DvBP4qJFQmI')]
    [TestCase('19', 'android.svg,claNull,TileStretch,false,1,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/zdf77//3////0/9K/0//7//v/+//4v9C/0DvAP4A5ABkA')]
    [TestCase('20', 'android.svg,claNull,TileStretch,false,0.5,0.98,ADw8fn48PAA7PPz+fz/8zHu+/v9/v/79f77//3////0//C/0//7//v/+//4v9D/8DvBP4qJFQmI')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,/8PDgYHD5///88PBg8Pv///z1+Pj0+////PX4+fz7/9P8K/0//7//P/8//4v9A/wDtAHwApABsA')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,/8PDgYHD5///88PBg8Pv///z19PT1+////v319f///8/9K50//7//P/8//4v9C/0jlFO4qJFVmo')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,/8PDgYHD5///88PBg8Pv///z1+Pj0+////PX4+fz7/9P8K/0//7//P/8//4v9A/wDtAHwApABsA')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,/8PDgYHD5///88PBg8Pv///z19PT1+////v319f///8/9K50//7//P/8//4v9C/0jlFO4qJFVmo')]
    procedure TestFillRectBitmapWithChessBackground(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,claNull,Tile,true,1,0.98,/8XBgcGB9////fHhw8f////99fXX1////////9fX///7v9ETwRP5u///2RPBE9mT///7u/////8')]
    [TestCase('2',  'android.svg,claNull,Tile,true,0.5,0.98,/8GBgcGB1////fHhw8ff///99fXX19///////9fX///7u9ETwAPZk///0RPAA9kT///5u/////8')]
    [TestCase('3',  'android.svg,claNull,Tile,false,1,0.98,/8XBgcGB9////fHhw8f////99fXX1////////9fX///7v9ETwRP5u///2RPBE9mT///7u/////8')]
    [TestCase('4',  'android.svg,claNull,Tile,false,0.5,0.98,/8GBgcGB1////fHhw8ff///99fXX19///////9fX///7u9ETwAPZk///0RPAA9kT///5u/////8')]
    [TestCase('5',  'android.svg,claRed,Tile,true,1,0.98,/9XBgcGB9////fHhw8P/////++vr6//////76/////8mbD/8Ozwm7ARELuw7PC7sBkAm7AAAAAA')]
    [TestCase('6',  'android.svg,claRed,Tile,true,0.5,0.98,/8XBgcGB9////fHhw8f/////++vr7//////76+/v//8ERC7sP/wmbAAALuw//CbsAAAmbAAAAAA')]
    [TestCase('7',  'android.svg,claRed,Tile,false,1,0.98,/9XBgcGB9////fHhw8P/////++vr6//////76/////8mbD/8Ozwm7ARELuw7PC7sBkAm7AAAAAA')]
    [TestCase('8',  'android.svg,claRed,Tile,false,0.5,0.98,/8XBgcGB9////fHhw8f/////++vr7//////76+/v//8ERC7sP/wmbAAALuw//CbsAAAmbAAAAAA')]
    [TestCase('9',  'android.svg,claNull,TileOriginal,true,1,0.98,Pz////////9/f////////39///////////////////////////////////////////////////8')]
    [TestCase('10', 'android.svg,claNull,TileOriginal,true,0.5,0.98,vz//////////f/////////9///////////////////////////////////////////////////8')]
    [TestCase('11', 'android.svg,claNull,TileOriginal,false,1,0.98,Pz////////9/f////////39///////////////////////////////////////////////////8')]
    [TestCase('12', 'android.svg,claNull,TileOriginal,false,0.5,0.98,vz//////////f/////////9///////////////////////////////////////////////////8')]
    [TestCase('13', 'android.svg,claRed,TileOriginal,true,1,0.98,Hz8///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('14', 'android.svg,claRed,TileOriginal,true,0.5,0.98,Pz9///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('15', 'android.svg,claRed,TileOriginal,false,1,0.98,Hz8///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('16', 'android.svg,claRed,TileOriginal,false,0.5,0.98,Pz9///////9/f3///////3//f///////f/9///////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('17', 'android.svg,claNull,TileStretch,true,1,0.98,/8PDgYHDx/////Phw8fP/////+fH79/////////////wD//vwAPAC8ALwAPwD/AP+b/9v/////8')]
    [TestCase('18', 'android.svg,claNull,TileStretch,true,0.5,0.98,/8ODgYHDx/////Phw8fP/////+fH79/////////////wD9ALwAPAA8ADwAPQC/AP+R/5v/////8')]
    [TestCase('19', 'android.svg,claNull,TileStretch,false,1,0.98,/8PDgYHDx/////Phw8fP/////+fH79/////////////wD//vwAPAC8ALwAPwD/AP+b/9v/////8')]
    [TestCase('20', 'android.svg,claNull,TileStretch,false,0.5,0.98,/8ODgYHDx/////Phw8fP/////+fH79/////////////wD9ALwAPAA8ADwAPQC/AP+R/5v/////8')]
    [TestCase('21', 'android.svg,claRed,TileStretch,true,1,0.98,/8PDgYHD5/////Phw8fv////8+Hjx+/////////////wD///8A/wD/AP8A/wD/AP/b////////8')]
    [TestCase('22', 'android.svg,claRed,TileStretch,true,0.5,0.98,/8PDgYHD5/////Phw8fv////8+Hj1+/////z4ef37/8P8C/0P/w//D/8P/wv9A/wBuAGQAAAAAA')]
    [TestCase('23', 'android.svg,claRed,TileStretch,false,1,0.98,/8PDgYHD5/////Phw8fv////8+Hjx+/////////////wD///8A/wD/AP8A/wD/AP/b////////8')]
    [TestCase('24', 'android.svg,claRed,TileStretch,false,0.5,0.98,/8PDgYHD5/////Phw8fv////8+Hj1+/////z4ef37/8P8C/0P/w//D/8P/wv9A/wBuAGQAAAAAA')]
    procedure TestFillRectBitmapWithClipping(const AImageFileName, AModulateColor: string; AWrapMode: TWrapMode; ABlending: Boolean; const AOpacity, AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,1,true,0.98,Pi8Lw9D0fE///+vj0/d+T///+/f3///f//////////8Q7yDH4IfhDucO/wj/EP7wH3Af8zn/Of8')]
    [TestCase('2',  'android.svg,300,300,Tile,1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,Pi8Lw9D0fE///+vj0/d+T///+/f3///f//////////8Q7yDH4IfhDuMO9wj/EP5wH3Af8TnvOf8')]
    [TestCase('3',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,1,true,0.98,Pz8/f/////////9///////////////////////////8f/x//f/////////////////////////8')]
    [TestCase('4',  'android.svg,300,300,Tile,1,2,-50,-100,30,0,0,200,200,0.6,true,0.98,Pz8/f/////////9///////////////////////////8f/x//f/////////////////////////8')]
    [TestCase('5',  'android.svg,300,300,Tile,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,9Pzdj4OA4Hj//P3vw8fufP/9/f/n7+////3/////////8/n/4f/B78Gfwh/OHM4R/CH8YLzhf+M')]
    [TestCase('6',  'android.svg,300,300,Tile,-1,2,-50,-100,30,-200,-50,660,343,0.6,true,0.98,Pz8/f/////////9///////////////////////////8f/x//f/////////////////////////8')]
    [TestCase('7',  'android.svg,300,300,Tile,-1,2,250,-50,30,0,0,200,200,1,true,0.98,A+dvHx8fP3///29fX18/f/////9/f/////////9///95/3//d/9j/wP/h/8H/wf/D/8//7////8')]
    [TestCase('8',  'android.svg,300,300,Tile,1,-2,-50,200,30,0,0,200,200,0.6,true,0.98,6dsfBwdHz////39nR0fP////f2/P3///////7//f///H/4d/Bj8Afwh/EH8wf/D/8f/z//////8')]
    [TestCase('9',  'android.svg,300,300,TileOriginal,1,2,150,20,30,-200,-50,660,343,1,true,0.98,//PDx8fP//////Pnx8///////+/f3//////////////8P/w/+D/4P/h/+H/x//v///////////8')]
    [TestCase('10', 'android.svg,300,300,TileOriginal,1,2,-50,100,-60,-200,50,660,343,0.6,true,0.98,/9+Pj4/////////vz//////////f///////////////z/+P/wf/A/+D///////////////////8')]
    [TestCase('11', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,1,true,0.98,////nx8fHz//////X19ff/////9/f39////////////////////j/+P/g/+D/4P/h/+H/5//n/8')]
    [TestCase('12', 'android.svg,300,300,TileOriginal,1,2,50,100,20,0,0,200,200,0.6,true,0.98,////nx8fHz//////X19ff/////9/f39////////////////////j/+P/g/+D/4P/B/+H/4//n/8')]
    [TestCase('13', 'android.svg,300,300,TileOriginal,1,-2,50,500,30,-200,-50,660,343,1,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('14', 'android.svg,300,300,TileOriginal,-1,2,-50,-100,-60,-200,-50,660,343,0.6,true,0.98,Hx////////9/f////////39/////////f/////////////////////////////////////////8')]
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
    [TestCase('1', '0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestSaveState(const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.UIConsts,
  System.Math,
  FMX.Types;

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
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
          LModulateCanvas.ModulateColor := TAlphaColors.Null;
          LSurface.Canvas.IntersectClipRect(RectF(LSurface.Width / 2, 0, LSurface.Width, LSurface.Height));
          LSurface.Canvas.Clear(TAlphaColors.Null);
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
        LBitmap.Canvas.SetMatrix(TMatrix.CreateTranslation(10, 10));
        LBitmap.Canvas.Blending := ABlending;
        LBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
        LBitmap.Canvas.Fill.Gradient.Style := AGradientStyle;
        ApplyGradientPoints(LBitmap.Canvas.Fill.Gradient, APoints);
        LBitmap.Canvas.FillRect(RectF(0, 0, 80, 80), 0, 0, [], AOpacity);
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
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
        if StringToAlphaColor(AModulateColor) <> TAlphaColors.Null then
        begin
          if not Supports(LBitmap.Canvas, IModulateCanvas, LModulateCanvas) then
            Exit;
          LModulateCanvas.ModulateColor := StringToAlphaColor(AModulateColor);
        end;
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
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(25, 25, 75, 75), 0, 0, [], 0.6);
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
