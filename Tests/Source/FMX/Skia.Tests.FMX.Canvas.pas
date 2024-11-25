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
{$IF CompilerVersion <> 36}
  // We should disable IModulateCanvas tests for v6 on RAD Studio 12 Athens,
  // as it was not implemented on Skia based canvas.
  {$DEFINE MODULATE_CANVAS}
{$ENDIF}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
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
    function CreateBitmap(const AImageFileName: string): TBitmap;
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
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//f/7m/uL96uwqDif+P3w+Pj20O55wH8')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,/v78fDjAgID//vx9e8fOzP/+/X3/1+/N//////////3//f/7m/uL96uwqDif+P3w+Pj20O55wH8')]
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
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,B/f38/PyAgB///fz8/dODH//9/f3907vf//3/////+//D/8P/w//H/8f/5//n/+f/h8AHwAPP/8')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,B/Pz8/PyAgB///Pz8/dODH//9/f3907vf//3/////+//D/8P/w//H/+f/5//n/+f/h8AHwAPP/8')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5f/l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5//n/+f/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,B/f38/PyAgB///fz8/dODH//9/f3907vf//3/////+//D/8P/w//H/8f/5//n/+f/h8AHwAPP/8')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,B/Pz8/PyAgB///Pz8/dODH//9/f3907vf//3/////+//D/8P/w//H/+f/5//n/+f/h8AHwAPP/8')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5f/l/+X/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,AD39/f38wAD//f39///OjP/9/f3//86P///9/f//3///+f/5//n/+f/5f/l/+X/5f/l4AQABAH8')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,A/f39/f2BgB///f39/dOTH//9/f3907vf///9//3Tv8A8ABwAHAA8ADwAPAA8ABwAHAAMH/w//g')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,B/f39/f2BgB///f39/dOTH//9/f3905vf///9//3Tv8AcABwAPAA8ADwAPAAcADwAHAAMH/w//g')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,AH39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AC4ADgAPAC8ALwAvAA8ADwAOAA4B///8')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.975,AX39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AA4ALgA+AD4APgAfAD8AHwAeAA4D///8')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3987////z+Xf3z/8TABEA8QLxA/EH8wf/50PmA+cC7wDvAAA')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,A/f39/f2BgB///f39/dOTH//9/f3907vf///9//3Tv8A8ABwAHAA8ADwAPAA8ABwAHAAMH/w//g')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,B/f39/f2BgB///f39/dOTH//9/f3905vf///9//3Tv8AcABwAPAA8ADwAPAAcADwAHAAMH/w//g')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,AH39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AC4ADgAPAC8ALwAvAA8ADwAOAA4B///8')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.975,AX39/f39wQB/ff39///PTH99/f3//8/Pf339/////8+AA4ALgA+AD4APgAfAD8AHwAeAA4D///8')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3987////z+Xf3z/8TABEA8QLxA/EH8wf/50PmA+cC7wDvAAA')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestDrawBitmap(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('2',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,false,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('3',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('4',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,660,343,1,true,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('5',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('6',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,false,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('7',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,false,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('8',  '3d-shapes.svg,660,343,1,0,0,960,343,0,0,660,343,1,true,true,0.98,MHBw8fP9DQ97/HDx8/9PD/v/ev33/1+v+/96/ff/X/8AWGDYQNjw37D/8/9LfxtgP+83Z35X/sg')]
    [TestCase('9',  '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,false,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOYAx4DNAf+H9q94f/D/9d/z+fP8E')]
    [TestCase('10', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,false,true,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOYAx4DNAf+H9q94f/D/9d/z+fP8E')]
    [TestCase('11', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,false,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOaAx4DNAf+v9q94f/D/9d/z+XP8k')]
    [TestCase('12', '3d-shapes.svg,660,343,1,0,0,660,343,0,0,960,343,1,true,true,0.98,GDws7Pz/CwN7/Hzs///PC//9ff7//++f//19////758ABzAOaAx4DNAf+v9q94f/D/9d/z+XP8k')]
    [TestCase('13', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,false,0.98,AAAHBwZ2/t87MMfHB3f+33u+19fPf//fe773189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('14', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,false,true,0.98,AAAHBwZ2/t87MMfHB3f+33u+19fPf//fe773189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('15', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,343,1,true,true,0.98,AAAHBwZ2/t87MMfHB3f+33u+19fPf//fe773189//98AAAAHAB4AGAAYHBh+GP+Y/9n/+v/d//o')]
    [TestCase('16', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,false,true,0.98,AAIfHxsTc3MPDv//Wxd/+59v///bX3//3+////tf//9AnEP+Af6h1gMGQ64DJvFW+Vb/rv+u/1Y')]
    [TestCase('17', '3d-shapes.svg,660,343,2,0,0,660,343,0,0,660,343,1,true,true,0.98,AAIfHxsTc3MPDv//Wxd/+59v///bX3//3+////tf//9AnEP+Af6h1gMGQ64DJvFW+Vb/rv+u/1Y')]
    [TestCase('18', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,false,true,0.98,h2kteNLSLS2Pb/3409cv7Z9v/f/b97/v3+/////3v/9Sq1KrrVWtVUNPUqtSqq1VrVVWq1arrVU')]
    [TestCase('19', '3d-shapes.svg,660,343,2,660,343,0,0,0,0,660,343,1,true,true,0.98,h2kteNLSLS2Pb/3409cv7Z9v/f/b97/v3+/////3v/9Sq1KrrVWtVUNPUqtSqq1VrVVWq1arrVU')]
    [TestCase('20', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,false,true,0.98,h2kteNLSLS2Pb/3409cv7Z9v/f/b97/v3+/////3v/9Sq1KrrVWtVUNPUqtSqq1VrVVWq1arrVU')]
    [TestCase('21', '3d-shapes.svg,660,343,2,0,0,660,343,660,343,0,0,1,true,true,0.98,h2kteNLSLS2Pb/3409cv7Z9v/f/b97/v3+/////3v/9Sq1KrrVWtVUNPUqtSqq1VrVVWq1arrVU')]
    [TestCase('22', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,false,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('23', '3d-shapes.svg,660,343,1,660,343,0,0,660,343,0,0,1,true,true,0.98,dv67//+7/nZ//vv////+fv//+//////+///7//////6HwXg+o+EXRhdGo+F4PofBeD6HwXg+h8E')]
    [TestCase('24', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('25', '3d-shapes.svg,200,200,1,660,0,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('26', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('27', '3d-shapes.svg,200,200,1,0,0,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('28', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('29', '3d-shapes.svg,200,200,1,-100,-100,0,0,0,0,660,343,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('30', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('31', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,0,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('32', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,false,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('33', '3d-shapes.svg,200,200,1,0,0,660,343,0,0,660,-10,1,true,true,0.98,Qr1CQkJCvUJ7vcLCQ0P9znu/1tbX1///f7/+19f///9VqqvVVCq9vb29VCqr1VWqq9VVqqpVVao')]
    [TestCase('34', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,true,0.98,/AwMDAwM/Pz8fHxtT0/8/P5//3/vT/79/v///////v0k8Zl0APAk+Zl0AHAk+YF1iXfh+//z+/w')]
    [TestCase('35', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,true,0.98,/AwMDAwM/Pz8fHxtT0/8/P5//3/vT/79/v////9//v0k8Zl0APAk+Zl0AHAk+YF1gXfg+//z+/w')]
    [TestCase('36', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,true,0.98,f/sDAwODh/9//zNjw8fP/3//t3vn3+//f/////////+kj5kmgA+kn5kmgAekn4EngSen//////8')]
    [TestCase('37', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,true,0.98,f/sDAwODh/9//zNjw8fP/3//t3vn3+//f/////////+kj5kmgA+kn5kmgAekn4EmgSen//////8')]
    [TestCase('38', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,true,0.98,AXBQ8fO9H5IdfFDx8/8fnl/99fv//1+fX/31+///X/8AWEBYQdhg37D/9/9r/5tmH2Y3/35mfiA')]
    [TestCase('39', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,true,0.98,AXBQ8fO9H5IdfFDx8/8fnl/99fv//1+fX/31+///X/8AWEBYQNhg37D/8/9r/5tmH2Y3/35mfiA')]
    [TestCase('40', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,false,false,0.98,/AwMDAwM/Pz8fHxtT0/8/P5//3/vT/79/v///////v0k8Zl0APAk+Zl0AHAk+YF1iXfh+//z+/w')]
    [TestCase('41', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,1,true,false,0.98,/AwMDAwM/Pz8fHxtT0/8/P5//3/vT/79/v////9//v0k8Zl0APAk+Zl0AHAk+YF1gXfg+//z+/w')]
    [TestCase('42', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,false,false,0.98,f/sDAwODh/9//zNjw8fP/3//t3vn3+//f/////////+kj5kmgA+kn5kmgAekn4EngSen//////8')]
    [TestCase('43', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,1,true,false,0.98,f/sDAwODh/9//zNjw8fP/3//t3vn3+//f/////////+kj5kmgA+kn5kmgAekn4EmgSen//////8')]
    [TestCase('44', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,false,false,0.98,AXBQ8fO9H5IdfFDx8/8fnl/99fv//1+fX/31+///X/8AWEBYQdhg37D/9/9r/5tmH2Y3/35mfiA')]
    [TestCase('45', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,1,true,false,0.98,AXBQ8fO9H5IdfFDx8/8fnl/99fv//1+fX/31+///X/8AWEBYQNhg37D/8/9r/5tmH2Y3/35mfiA')]
    [TestCase('46', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,true,0.98,fYwcDBQMfP59nBzt9898/n+9vf/3z37/f/29///vfv8k2ZkkAJAk2ZkkACAk2ZknmSck25n72/4')]
    [TestCase('47', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,true,0.98,fYwcDBQMfP59nBzt9898/n+9vf/3z37/f/29///vfv8k2ZkkAJAk2ZkkACAk2ZknmSck25n72/w')]
    [TestCase('48', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,true,0.98,f/uDA4MDB/5//5Pj48MP/n//t/vn21//f/+3+///f/8km9skJIsk29skgSYk25kkmSQk29tv2z8')]
    [TestCase('49', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,true,0.98,f/uDA4MDB/5//5Pj48MP/n//t/vn21//f/+3+///f/8km9skJIsk29skgSYk25kkmSQk29tv2z8')]
    [TestCase('50', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,true,0.98,QbBU6/WrHZJdvFTr9+sfnl+99fv3+1+/X731+/f7X/8A2EBYAPiQHLDbE/sLH5skPyw3HX4QfkI')]
    [TestCase('51', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,false,false,0.98,fYwcDBQMfP59nBzt9898/n+9vf/3z37/f/29///vfv8k2ZkkAJAk2ZkkACAk2ZknmSck25n72/4')]
    [TestCase('52', '3d-shapes.svg,300,300,1.5,200,50,660,343,0,0,660,343,0.3,true,false,0.98,fYwcDBQMfP59nBzt9898/n+9vf/3z37/f/29///vfv8k2ZkkAJAk2ZkkACAk2ZknmSck25n72/w')]
    [TestCase('53', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,false,false,0.98,f/uDA4MDB/5//5Pj48MP/n//t/vn21//f/+3+///f/8km9skJIsk29skgSYk25kkmSQk29tv2z8')]
    [TestCase('54', '3d-shapes.svg,300,300,1.5,0,0,660,343,-200,-50,660,343,0.3,true,false,0.98,f/uDA4MDB/5//5Pj48MP/n//t/vn21//f/+3+///f/8km9skJIsk29skgSYk25kkmSQk29tv2z8')]
    [TestCase('55', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0.3,true,false,0.98,QbBU6/WrHZJdvFTr9+sfnl+99fv3+1+/X731+/f7X/8A2EBYAPiQHLDbE/sLH5skPyw3HX4QfkI')]
    [TestCase('56', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,true,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('57', '3d-shapes.svg,300,300,1.5,0,0,0,0,0,0,200,200,1,true,false,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('58', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,true,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('59', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,0,0,1,true,false,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('60', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,true,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('61', '3d-shapes.svg,300,300,1.5,0,0,660,343,200,200,200,200,1,true,false,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('62', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,false,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    [TestCase('63', '3d-shapes.svg,300,300,1.5,0,0,660,343,0,0,200,200,0,true,true,0.98,aarUK9QrVZZ9vtTr9+tdnn+/9fv3+1+/f//1+///f/8k29skJNsk29sk2yQk29sk2yQk29sk2yQ')]
    procedure TestDrawBitmapWithChessBackground(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScale, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x8cHD7/////Hxw8fv////8/vX1+/////////////9r/+v/6//P/kP8Y/2j/Sf//////////8')]
    procedure TestDrawBitmapWithClipping(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,0.98,///x8cHD7/////Hxw8fv////8/vX1+/////////////9r/+v/6//P/kP8Y/2j/Sf//////////8')]
    procedure TestDrawBitmapWithClipping2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,/f3/4cCAwPD//f/hw8fO/P/9/+/f38/9//3/////z/3wAP/h///8B/AB8HDg+PA4/AD/gP/z//8')]
    [TestCase('2', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,/f3/4cCAwPD//f/hw8fO/P/9/+/f38/9//3/////z/3wAP/h///8B/AB8HDg+PA4/AD/gP/z//8')]
    [TestCase('3', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,0.98,gID//////////P/////////9//////////3///////////////////////////////////////8')]
    [TestCase('4', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,0.98,gID//////////P/////////9//////////3///////////////////////////////////////8')]
    [TestCase('5', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,0.98,w4GAgOH//////fDg4///////8efn///////5/+f////B4cPh4AHwAf////////////////////8')]
    [TestCase('6', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,0.98,8MP9/fn5wPz///39+//O/P///f37///+/////fv/////Qf/w//D/8f/h/+P/wz/DAMP8B/////8')]
    [TestCase('7', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,0.98,///P8AABg/P////xQ0fP//////Vvx//////////////k/////cH5mf7xAj9wDwHH8f/wj/+P//8')]
    [TestCase('8', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,0.98,wMD8/////////Pz////////9/P////////38/////////P////////////////////////////8')]
    procedure TestDrawBitmapWithMatrix(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF MODULATE_CANVAS}
    [TestCase('1', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,1,0.98,/Azg4cDEwPn8/PDh4MTI+f7+/PHixvz///7/+//+/P/w5v/j///8D/gB8Pjw+Px4/wD/9//7//8')]
    [TestCase('2', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,/Azg4cDEwPn//PDhw8fM/f/+/PHDx+z9///8//PP//8//wD+AAAH+A//D/8fjx/PD/8D/wD+AAA')]
    [TestCase('3', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,300,300,1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,true,true,Red,1,0.98,AID///////9//P////////////////////////////8AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('5', '3d-shapes.svg,300,300,1,-2,50,500,30,0,0,660,343,-200,-50,660,343,1,false,true,Red,0.3,0.98,woGAgOH//////fDg4//////99ODz////////4P////9//j4/P/8f/wf+AAAAAAAAAAAAAAAAAAA')]
    [TestCase('6', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,-200,-50,660,343,1,true,true,Red,0.3,0.98,8AH4/Pj5APD/+fj9+//M/P/5/v///8z////+//////8D/gAPAA8AHwAeAB4APvA8//9//wH/AAc')]
    [TestCase('7', '3d-shapes.svg,300,300,-1,2,-50,-100,30,0,0,660,343,0,0,200,200,1,false,true,Red,0.3,0.98,//+HwAEBg/P///fhQ0fP////9+tTb8/////361Nv//8/gP+AB/7mZh/+/fDP+P/4PgA/+AD4AAA')]
    [TestCase('8', '3d-shapes.svg,300,300,1,-2,-50,500,30,0,0,660,343,0,0,200,200,1,true,true,Red,0.3,0.98,wOD8/////////Pz////////+//////////////////8ADwAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    {$ENDIF}
    procedure TestDrawBitmapWithModulateColor(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; ASurfaceScaleX, ASurfaceScaleY, ASurfaceOffsetX, ASurfaceOffsetY, ARotationDeg, ASrcLeft, ASrcTop, ASrcRight, ASrcBottom, ADestLeft, ADestTop, ADestRight, ADestBottom, AOpacity: Single; AHighSpeed, ABlending: Boolean; const AModulateColor: string; AModulateColorOpacity: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$IFDEF MODULATE_CANVAS}
    [TestCase('1', '3d-shapes.svg,200,200,0.98,+8u5NQADgp////l1Q0fO3///+33nR+7f//////fP79/vr++nr7cPcU0HAYO0j/Sf5J3YmMOZw/8')]
    {$ENDIF}
    procedure TestDrawBitmapWithModulateColor2(const AImageFileName: string; ASurfaceWidth, ASurfaceHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.UIConsts,
  System.Math,
  System.Math.Vectors;

{ TSkFMXCanvasTests }

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
        DrawChessBackground(LSurface.Canvas, Min(ASurfaceWidth, ASurfaceHeight) / 15,
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
        LSurface.Canvas.SetMatrix(TMatrix.CreateScaling(ASurfaceScaleX, ASurfaceScaleY) *
          TMatrix.CreateTranslation(ASurfaceOffsetX, ASurfaceOffsetY) *
          TMatrix.CreateRotation(ARotationDeg));
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
        if Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          LModulateCanvas.ModulateColor := MakeColor(StringToAlphaColor(AModulateColor), AModulateColorOpacity);
        LSurface.Canvas.SetMatrix(TMatrix.CreateScaling(ASurfaceScaleX, ASurfaceScaleY) *
          TMatrix.CreateTranslation(ASurfaceOffsetX, ASurfaceOffsetY) *
          TMatrix.CreateRotation(ARotationDeg));
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
        if Supports(LSurface.Canvas, IModulateCanvas, LModulateCanvas) then
          LModulateCanvas.ModulateColor := MakeColor(TAlphaColors.Black, 0.5);
        LImage := CreateBitmap(AImageFileName);
        try
          LSurface.Canvas.DrawBitmap(LImage, RectF(0, 0, LImage.Width, LImage.Height),
            RectF(0, 0, ASurfaceWidth, ASurfaceHeight), 1, False);
          if LModulateCanvas <> nil then
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

initialization
  TDUnitX.RegisterTestFixture(TSkFMXCanvasTests);
end.
