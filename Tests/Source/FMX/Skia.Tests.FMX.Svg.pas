{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.Svg;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  Skia,
  Skia.FMX,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSvgBrushTests }

  [TestFixture]
  TSkSvgBrushTests = class(TTestBase)
  private
    procedure TestRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AWrapMode: TSkSvgWrapMode; const AOverrideColor: TAlphaColor; const AExpectedImageHash: string);
  protected
    function AssetsPath: string; override;
  public
    [TestCase('android.svg', 'android.svg,80,80,/8PDgYHDx/////Phw8fP/////+XHz9/////////f///wD9f/wAPAA8ADwAPwD/AP/b/9v/2///8')]
    [TestCase('delphi.svg',  'delphi.svg,80,80,w508PHgwkcP//fz8+PDRw////v7+/NX3/////////ff/2f/R9gHsAP2I+gD8Qfqh+gPoQ/EP/D8')]
    [TestCase('gorilla.svg', 'gorilla.svg,80,80,7+fDw4GBmf////Pjw8ff////++Pz19/////////////4P/g/8B/wH+APwgPCAccD7/Pn8/////8')]
    [TestCase('lion.svg',    'lion.svg,80,80,/4+HBweHz/////fnx8fP////9/ff18//////////3/+Af8B/wL+Af4Cfgv/Ev+C/8f////////8')]
    [TestCase('tesla.svg',   'tesla.svg,40,40,/4GB5+fn/////fHn5+f////98ff39/////////f////gB/w//n/+f/5//n/+f/5///////////8')]
    procedure TestFitRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,RP5ETExMQP///mRNT8/O///+7m/v7+7///7+b//v//+7bhIks2b//5NkEiS/fv//kiSSJP//u24')]
    [TestCase('delphi.svg',  'delphi.svg,1600,1600,/////////vz////////+/P////////78/////////vwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAE')]
    [TestCase('gorilla.svg', 'gorilla.svg,1500,1500,nowMBP//npz//Pzl///enP/9//X////f/////f/////h+MBymGKcd993//////P84/jj+OH4wHA')]
    [TestCase('lion.svg',    'lion.svg,1200,1200,nhwcnv+eHhz//Pz+/97e3P/9/P7/3t/8/////v/+//7H/If4x/7n/v///////+/+h/jr+sf8h/g')]
    [TestCase('tesla.svg',   'tesla.svg,450,450,QVtZSVtbQf9/f3lpW19P////+enb38/////9+///z/9Mk///TJFMkW2b//9EkWyb//9EkUyR//8')]
    procedure TestTileRender(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,450,450,/8PDgYHD5/////Phw8fv////9+HDx/////////vf///wD9ADwAPAA8ADwAPQC/AP/T/5P/2///8')]
    [TestCase('delphi.svg',  'delphi.svg,1000,1000,w4EAAAAAgcP//fDgwMDBw///8eDAwePH///x78DD48eAAYABAAAAAAAAAACAAYABwAPAA/AP/D8')]
    [TestCase('gorilla.svg', 'gorilla.svg,1000,1000,58fDg4EAuP////Pjw0f+////9+fjZ/////////t////wH/Af8B/gH8ADwgGCAccBz8Hnw/////8')]
    [TestCase('lion.svg',    'lion.svg,1000,1000,Dw8PDw/f///////vz9//////////3/////////////+Bf4DvgH+An8F/4X/7//////////////8')]
    [TestCase('tesla.svg',   'tesla.svg,60,60,BwePj9/////////v3//////////f/////////9/////z//P/8//7//v///////////////////8')]
    procedure TestDefaultRenderWithACustomColor(const ASvgFileName: string; const AWidth, AHeight: Integer; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils;

{ TSkSvgBrushTests }

function TSkSvgBrushTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'Svg');
end;

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
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LBrush := TSkSvgBrush.Create;
  try
    LBrush.OverrideColor := AOverrideColor;
    LBrush.WrapMode := AWrapMode;
    LBrush.Source := TFile.ReadAllText(AssetsPath + ASvgFileName);
    LBrush.Render(LSurface.Canvas, RectF(0, 0, AWidth, AHeight), 1);
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
