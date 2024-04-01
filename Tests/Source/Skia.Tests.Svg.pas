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
unit Skia.Tests.Svg;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSvgDOMTests }

  [TestFixture]
  TSkSvgDOMTests = class(TTestBase)
  public
    [TestCase('Editing android eyes color', 'android.svg,100,100,eyes,fill,red,/8PDgYHD5/////Phw8fv////9+XHz//////////f///wD9AbwAPAA8ADwAPwD/AP/b/9v/2///8')]
    procedure TestEditSvgElement(const ASvgFileName: string; const AWidth, AHeight: Integer; const AElementId, AAttributeName, AAttributeValue, AExpectedImageHash: string);
    [TestCase('android.svg', 'android.svg,0,0')]
    [TestCase('delphi.svg',  'delphi.svg,0,0')]
    [TestCase('gorilla.svg', 'gorilla.svg,0,0')]
    [TestCase('lion.svg',    'lion.svg,888,746.66669')]
    [TestCase('tesla.svg',   'tesla.svg,40,40')]
    [TestCase('youtube.svg', 'youtube.svg,0,0')]
    procedure TestGetIntrinsicSize(const ASvgFileName: string; const AWidth, AHeight: Single);
    [TestCase('android.svg', 'android.svg,true,0,0,96,105')]
    [TestCase('delphi.svg',  'delphi.svg,true,0,0,10666.667,10666.667')]
    [TestCase('gorilla.svg', 'gorilla.svg,true,0,0,944.880,944.880')]
    [TestCase('lion.svg',    'lion.svg,true,0,0,888,746.66669')]
    [TestCase('tesla.svg',   'tesla.svg,false,0,0,0,0')]
    [TestCase('youtube.svg', 'youtube.svg,true,0,0,24,24')]
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

procedure TSkSvgDOMTests.TestEditSvgElement(const ASvgFileName: string;
  const AWidth, AHeight: Integer; const AElementId, AAttributeName,
  AAttributeValue, AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LSVGDOM: ISkSVGDOM;
  LNode: ISkSVGNode;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + ASvgFileName);
  if Assigned(LSVGDOM) then
  begin
    LSVGDOM.Root.Width  := TSkSVGLength.Create(AWidth,  TSkSVGLengthUnit.Pixel);
    LSVGDOM.Root.Height := TSkSVGLength.Create(AHeight, TSkSVGLengthUnit.Pixel);

    LNode := LSVGDOM.FindNodeById(AElementId);
    if Assigned(LNode) then
      LNode.TrySetAttribute(AAttributeName, AAttributeValue);

    LSVGDOM.Render(LSurface.Canvas);
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, 0.9961);
end;

procedure TSkSvgDOMTests.TestGetIntrinsicSize(const ASvgFileName: string;
  const AWidth, AHeight: Single);
var
  LSVGDOM: ISkSVGDOM;
  LSize: TSizeF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + ASvgFileName);
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  LSize := LSVGDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0));
  Assert.AreEqual(AWidth, LSize.Width, TEpsilon.Vector, 'Different width');
  Assert.AreEqual(AHeight, LSize.Height, TEpsilon.Vector, 'Different height');
end;

procedure TSkSvgDOMTests.TestTryGetViewBox(const ASvgFileName: string;
  const AExpectedResult: Boolean; const AX, AY, AWidth, AHeight: Single);
var
  LSVGDOM: ISkSVGDOM;
  LViewBox: TRectF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + ASvgFileName);
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  Assert.IsTrue(LSVGDOM.Root.TryGetViewBox(LViewBox) = AExpectedResult, 'Different result of TryGetViewBox');
  if AExpectedResult then
  begin
    Assert.AreEqual(AX, LViewBox.Left, TEpsilon.Vector, 'Different x position');
    Assert.AreEqual(AY, LViewBox.Top, TEpsilon.Vector, 'Different y position');
    Assert.AreEqual(AWidth, LViewBox.Width, TEpsilon.Vector, 'Different width');
    Assert.AreEqual(AHeight, LViewBox.Height, TEpsilon.Vector, 'Different height');
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSvgDOMTests);
end.
