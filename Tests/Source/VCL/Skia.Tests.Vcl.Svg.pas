{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
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
    [TestCase('android.svg', 'android.svg,80,80,/8PDgYHD5/////Phw8fv/////+XHz//////////f///wD9f/wAPAA8ADwAPwD/AP/b/9v/2///8')]
    [TestCase('delphi.svg',  'delphi.svg,80,80,w508PHgwmcP//fz8+PDZw////v7+/N33/////////ff/2f/R9gHsAP2I+gD8Qfqh+gPoQ/EP/D8')]
    [TestCase('gorilla.svg', 'gorilla.svg,80,80,7+fDw4GBmf////Pjw8ff////++Pz19/////////////4P/g/8B/wH+APwgPCAccD7/Pn8/////8')]
    [TestCase('lion.svg',    'lion.svg,80,80,/4+HBweHz/////fnx8fP////9/ff18//////////3/+Af8B/wL+Af4Cfgv/Ev+C/8f////////8')]
    [TestCase('tesla.svg',   'tesla.svg,40,40,/4GB5+fn/////fHn5+f////98ff39/////////f////gB/w//n/+f/5//n/+f/5///////////8')]
    procedure TestFitRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,VtZA3kD/AN///mDfQ//O3//+6v/r/+7///76//v///+7bhIks2b//5NkEiS/fv//kiSSJP//u24')]
    [TestCase('delphi.svg',  'delphi.svg,1600,1600,//////////7//////////v/////////+//////////4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE')]
    [TestCase('gorilla.svg', 'gorilla.svg,1500,1500,3p4MBP/enpz//vzl/97e3P////X/3//f/////f/////h+MBymGKcd993//////P84/jj+OH4wHA')]
    [TestCase('lion.svg',    'lion.svg,1200,1200,nh4cnv+eHhz//vz+/97e3P///P7/3t/8/////v/+//7H/If4x/7n/v///////+/+h/jr+sf8h/g')]
    [TestCase('tesla.svg',   'tesla.svg,450,450,SElJSVtJSf9/bflpW89N///t+enbz83///39+///zf9Mk///TJFMkW2b//9EkWyb//9EkUyR//8')]
    procedure TestTileRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,/8PDgYHD5/////Phw8fv////9+HDx/////////vf///wD9ADwAPAA8ADwAPQC/AP/T/5P/2///8')]
    [TestCase('delphi.svg',  'delphi.svg,1000,1000,w4EAAAAAgcP//fDgwMDBw///8eDAwePH///x78DD48eAAYABAAAAAAAAAACAAYABwAPAA/AP/D8')]
    [TestCase('gorilla.svg', 'gorilla.svg,1000,1000,78fHw4GAuf////fjw8f/////9+fj5/////////v////wH/Af8B/gH8ADwgGCAccBz8Hnw/////8')]
    [TestCase('lion.svg',    'lion.svg,1000,1000,jwcPD4/////////vz/////////////////////////+Bf4DvgH+An8F/4X/7//////////////8')]
    [TestCase('tesla.svg',   'tesla.svg,60,60,BweP39//////////3//////////f/////////9/////z//P/8//7//v///////////////////8')]
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

procedure TSkSvgBrushTests.TestTileRender(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AExpectedImageHash: string);
begin
  TestRender(ASvgFileName, AWidth, AHeight, TSkSvgWrapMode.Tile, TAlphaColors.Null, AExpectedImageHash);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSvgBrushTests);
end.
