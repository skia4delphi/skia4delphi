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
unit Skia.Tests.Svg;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSvgDOMTests }

  [TestFixture]
  TSkSvgDOMTests = class(TTestBase)
  protected
    function AssetsPath: string; override;
  public
    [TestCase('Editing android eyes color', 'android.svg,100,100,eyes,fill,red,78PDgYHD5/////Phw8fv////9+XHz//////////f///wD9AbwAPAA8ADwAPwD/AP/b/9v/2///8')]
    procedure TestEditSvgElement(const ASvgFileName: string; const AWidth, AHeight: Integer; const AElementId, AAttributeName, AAttributeValue, AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,0,0')]
    [TestCase('delphi.svg',  'delphi.svg,0,0')]
    [TestCase('gorilla.svg', 'gorilla.svg,0,0')]
    [TestCase('lion.svg',    'lion.svg,888,746.66669')]
    [TestCase('tesla.svg',   'tesla.svg,40,40')]
    procedure TestGetIntrinsicSize(const ASvgFileName: string; const AWidth, AHeight: Single);
    [TestCase('android.svg', 'android.svg,true,0,0,96,105')]
    [TestCase('delphi.svg',  'delphi.svg,true,0,0,10666.667,10666.667')]
    [TestCase('gorilla.svg', 'gorilla.svg,true,0,0,944.88,944.88')]
    [TestCase('lion.svg',    'lion.svg,true,0,0,888,746.66669')]
    [TestCase('tesla.svg',   'tesla.svg,false,0,0,0,0')]
    procedure TestTryGetViewBox(const ASvgFileName: string; const AExpectedResult: Boolean; const AX, AY, AWidth, AHeight: Single);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkSvgDOMTests }

function TSkSvgDOMTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'Svg');
end;

procedure TSkSvgDOMTests.TestEditSvgElement(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AElementId, AAttributeName,
  AAttributeValue, AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LSVGDOM: ISkSVGDOM;
  LNode: ISkSVGNode;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSVGDOM := TSkSVGDOM.MakeFromFile(AssetsPath + ASvgFileName);
  if Assigned(LSVGDOM) then
  begin
    LSVGDOM.Root.Width  := TSkSVGLength.Create(AWidth,  TSkSVGLengthUnit.PX);
    LSVGDOM.Root.Height := TSkSVGLength.Create(AHeight, TSkSVGLengthUnit.PX);

    LNode := LSVGDOM.FindNodeById(AElementId);
    if Assigned(LNode) then
      LNode.TrySetAttribute(AAttributeName, AAttributeValue);

    LSVGDOM.Render(LSurface.Canvas);
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, 0.997);
end;

procedure TSkSvgDOMTests.TestGetIntrinsicSize(const ASvgFileName: string;
  const AWidth, AHeight: Single);
var
  LSVGDOM: ISkSVGDOM;
  LSize: TSizeF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(AssetsPath + ASvgFileName);
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  LSize := LSVGDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0));
  Assert.AreEqual(LSize.Width, AWidth, TEpsilon.Vector, 'Different width');
  Assert.AreEqual(LSize.Height, AHeight, TEpsilon.Vector, 'Different height');
end;

procedure TSkSvgDOMTests.TestTryGetViewBox(const ASvgFileName: string;
  const AExpectedResult: Boolean; const AX, AY, AWidth, AHeight: Single);
var
  LSVGDOM: ISkSVGDOM;
  LViewBox: TRectF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(AssetsPath + ASvgFileName);
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  Assert.IsTrue(LSVGDOM.Root.TryGetViewBox(LViewBox) = AExpectedResult, 'Different result of TryGetViewBox');
  if AExpectedResult then
  begin
    Assert.AreEqual(LViewBox.Left, AX, TEpsilon.Vector, 'Different x position');
    Assert.AreEqual(LViewBox.Top, AY, TEpsilon.Vector, 'Different y position');
    Assert.AreEqual(LViewBox.Width, AWidth, TEpsilon.Vector, 'Different width');
    Assert.AreEqual(LViewBox.Height, AHeight, TEpsilon.Vector, 'Different height');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSvgDOMTests);
end.
