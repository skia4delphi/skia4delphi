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
unit Skia.Tests.Vcl.Svg;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSvgBrushTests }

  [TestFixture]
  TSkSvgBrushTests = class(TTestBase)
  private
    procedure TestRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AWrapMode: TSkSvgWrapMode; const AOverrideColor: TAlphaColor; const AExpectedImageHash: string);
  public
    [TestCase('android.svg.1', 'android.svg,80,80,/8PDgYHD5/////Phw8fv/////+XHz//////////f///wD9f/wAPAA8ADwAPwD/AP/b/9v/2///8')]
    [TestCase('android.svg.2', 'android.svg,160,80,5+fDw8PH5+f///Pjw8fv7////+/Pz+/v///////////8P/gf+B/4H/gf+B/4H/w//n/+f/5///8')]
    [TestCase('delphi.svg.1',  'delphi.svg,80,80,w508PHgwmcP//fz8+PDZw////v7+/N33/////////ff/2f/R9gHsAP2I+gD8Qfqh+gPoQ/EP/D8')]
    [TestCase('delphi.svg.2',  'delphi.svg,160,80,58PD29PDw8P///P708fPz//////f3+//////////////r/+P/g/2D/ZP/g/+D/wP/J/4H/w//H8')]
    [TestCase('gorilla.svg.1', 'gorilla.svg,80,80,7+fDw4GBmf////Pjw8ff////++Pz19/////////////4P/g/8B/wH+APwgPCAccD7/Pn8/////8')]
    [TestCase('gorilla.svg.2', 'gorilla.svg,160,80,5+fnx8PDw/////fnw8fP////9/f39+/////////////8f/x//D/8P/gf+B/4D/kf+5/53/////8')]
    [TestCase('lion.svg.1',    'lion.svg,80,80,/4+Hh4eHz/////fnx8fP////9/ff18//////////3/+Af8B/wL+Af4Cfgv/Ev+C/8f////////8')]
    [TestCase('lion.svg.2',    'lion.svg,160,80,z8fHx8fHz+////fnx8fP7//////f1+/v///////////wP/B/8H/wf/C/8//w//h/+H/4f/////8')]
    [TestCase('tesla.svg.1',   'tesla.svg,40,40,/4GB5+fn/////fHn5+f////98ff39/////////f////gB/w//n/+f/5//n/+f/5///////////8')]
    [TestCase('tesla.svg.2',   'tesla.svg,80,40,58PD5+fn//////Pn5+f/////8/f39//////////////4H/5//n/+f/5//n/+f/5//n////////8')]
    [TestCase('youtube.svg.1', 'youtube.svg,80,80,/4EAGBgAgf///fD42MDJ///98Pzc0On/////////6f+AAYIBg4ED4YPhg4GCAYABgAH///////8')]
    [TestCase('youtube.svg.2', 'youtube.svg,160,80,/8PDw8PDw/////Pjw8fP////8+vr5//////////////4H/EP8Y/xj/GP8Y/xD/gf+B////////8')]
    procedure TestFitRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg.1', 'android.svg,80,80,/8OBgYHDx/////Hhw8fP/////+XH79/////////////wD9frgAOAC4ALgAvQC/AP+b/5v/2///8')]
    [TestCase('android.svg.2', 'android.svg,160,80,78OBgYHDx/////Hhw8fP/////+XH79/////////////wD9frgAOQA5ADkAPQC/AP+b/5v/2///8')]
    [TestCase('delphi.svg.1',  'delphi.svg,80,80,w508PHgwmcP//fz8+PDZw////v7+/N33/////////ff/2f/R9gHsAP2I+gD8Qfqh+gPoQ/EP/D8')]
    [TestCase('delphi.svg.2',  'delphi.svg,160,80,w408PDgwkcP//fz8+PDRw////v7+/tX3/////////f//2ffR9gDsYP2I+xH8QfKh+gPoE/EP/D8')]
    [TestCase('gorilla.svg.1', 'gorilla.svg,80,80,7+fDw4GBmf////Pjw8ff////++Pz19/////////////4P/g/8B/wH+APwgPCAccD7/Pn8/////8')]
    [TestCase('gorilla.svg.2', 'gorilla.svg,160,80,5+fDh4GBmf////Pnw8ff////++fz19/////////////4P/g/8B/wH+APwgPCAccD7/Pn8/////8')]
    [TestCase('lion.svg.1',    'lion.svg,80,80,/4+Ph4ePz//////nx8/P//////ff38///////9//z//AX8B/wL+Af4Afg//B/+G/4P/7v/////8')]
    [TestCase('lion.svg.2',    'lion.svg,160,80,z4+Ph4+Pj//////nz8/P//////ff38///////9//z//Af8J/wL+E/4D/w//D/+G/4P/7//////8')]
    [TestCase('tesla.svg.1',   'tesla.svg,40,40,/4GB5+fn/////fHn5+f////98ff39/////////f////gB/w//n/+f/5//n/+f/5///////////8')]
    [TestCase('tesla.svg.2',   'tesla.svg,80,40,w4GB5+fn/////fHn5+f////98ff39/////////f////gB/w//D/+f/5//n/+f/5///////////8')]
    [TestCase('youtube.svg.1', 'youtube.svg,80,80,/4EAGBgAgf///fD42MDJ///98Pzc0On/////////6f+AAYIBg4ED4YPhg4GCAYABgAH///////8')]
    [TestCase('youtube.svg.2', 'youtube.svg,160,80,//8AGBgA//////D42MD/////8PzcwP////////////+AAYIBg4GD4YPhg4GCAYABgAH///////8')]
    procedure TestStretchRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,VtZA3kD/AN///mDfQ//O3//+6v/r/+7///76//v///+7bhIks2b//5NkEiS/fv//kiSSJP//u24')]
    [TestCase('delphi.svg',  'delphi.svg,1600,1600,//////////7//////////v/////////+//////////4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE')]
    [TestCase('gorilla.svg', 'gorilla.svg,1500,1500,3p4MBP/enpz//vzl/97e3P////X/3//f/////f/////h+MBymGKcd993//////P84/jj+OH4wHA')]
    [TestCase('lion.svg',    'lion.svg,1200,1200,nh4cnv+eHhz//vz+/97e3P///P7/3t/8/////v/+//7H/If4x/7n/v///////+/+h/jr+sf8h/g')]
    [TestCase('tesla.svg',   'tesla.svg,450,450,SElJSVtJSf9/bflpW89N///t+enbz83///39+///zf9Mk///TJFMkW2b//9EkWyb//9EkUyR//8')]
    [TestCase('youtube.svg', 'youtube.svg,450,450,AP8A/wD//wB//3D/Q///TH//cP9D//9Of////////////wAAAAD//999//977wAAAAD//999//8')]
    procedure TestTileRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,/8PDgYHD5/////Phw8fv////9+HDx/////////vf///wD9ADwAPAA8ADwAPQC/AP/T/5P/2///8')]
    [TestCase('delphi.svg',  'delphi.svg,1000,1000,w4EAAAAAgcP//fDgwMDBw///8eDAwePH///x78DD48eAAYABAAAAAAAAAACAAYABwAPAA/AP/D8')]
    [TestCase('gorilla.svg', 'gorilla.svg,1000,1000,78fHw4GAuf////fjw8f/////9+fj5/////////v////wH/Af8B/gH8ADwgGCAccBz8Hnw/////8')]
    [TestCase('lion.svg',    'lion.svg,1000,1000,jwcPD4/////////vz/////////////////////////+Bf4DvgH+An8F/4X/7//////////////8')]
    [TestCase('tesla.svg',   'tesla.svg,60,60,BweP39//////////3//////////f/////////9/////z//P/8//7//v///////////////////8')]
    [TestCase('youtube.svg', 'youtube.svg,100,100,/4EAGBgAgf///fD42MDJ////+Pj4yMn/////////yf8AAAAAAwADwAPAAwAAAAAAgAH///////8')]
    procedure TestDefaultRenderWithACustomColor(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils;

{ TSkSvgBrushTests }

procedure TSkSvgBrushTests.TestDefaultRenderWithACustomColor(
  const ASvgFileName: string; const AWidth, AHeight: Integer;
  const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Default, TAlphaColors.Blueviolet, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestFitRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Fit, TAlphaColors.Null, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AWrapMode: TSkSvgWrapMode;
  const AOverrideColor: TAlphaColor; const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LBrush: TSkSvgBrush;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LBrush := TSkSvgBrush.Create;
  try
    LBrush.OverrideColor := AOverrideColor;
    LBrush.WrapMode := AWrapMode;
    LBrush.Source := TFile.ReadAllText(SvgAssetsPath + ASvgFileName);
    LBrush.Render(LSurface.Canvas, TRectF.Create(0, 0, AWidth, AHeight), 1);
  finally
    LBrush.Free;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot);
end;

procedure TSkSvgBrushTests.TestStretchRender(const ASvgFileName: string; const AWidth, AHeight: Integer;
  const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Stretch, TAlphaColors.Null, AExpectedImageHash);
end;

procedure TSkSvgBrushTests.TestTileRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Tile, TAlphaColors.Null, AExpectedImageHash);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSvgBrushTests);
end.
