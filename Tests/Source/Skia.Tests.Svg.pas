{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2026 Skia4Delphi Project.                           }
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
    [TestCase('android.svg',      'android.svg,0,0')]
    [TestCase('chinese-text.svg', 'chinese-text.svg,900,300')]
    [TestCase('delphi.svg',       'delphi.svg,0,0')]
    [TestCase('gorilla.svg',      'gorilla.svg,0,0')]
    [TestCase('lion.svg',         'lion.svg,888,746.66669')]
    [TestCase('tesla.svg',        'tesla.svg,40,40')]
    [TestCase('youtube.svg',      'youtube.svg,0,0')]
    procedure TestGetIntrinsicSize(const ASvgFileName: string; const AWidth, AHeight: Single);
    [Test]
    procedure TestPreserveAspectRatio;
    [Test]
    procedure TestSetContainerSize;
    [Test]
    procedure TestSetViewBox;
    [TestCase('android.svg',      'android.svg,true,0,0,96,105')]
    [TestCase('chinese-text.svg', 'chinese-text.svg,true,0,0,900,300')]
    [TestCase('delphi.svg',       'delphi.svg,true,0,0,10666.667,10666.667')]
    [TestCase('gorilla.svg',      'gorilla.svg,true,0,0,944.880,944.880')]
    [TestCase('lion.svg',         'lion.svg,true,0,0,888,746.66669')]
    [TestCase('tesla.svg',        'tesla.svg,false,0,0,0,0')]
    [TestCase('youtube.svg',      'youtube.svg,true,0,0,24,24')]
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

procedure TSkSvgDOMTests.TestPreserveAspectRatio;
var
  LSVGDOM: ISkSVGDOM;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + 'android.svg');
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  Assert.IsTrue(LSVGDOM.Root.PreserveAspectRatio =
    TSkSVGPreserveAspectRatio.Create(TSkSVGAspectAlign.XMidYMid, TSkSVGAspectScale.Meet),
    'The default aspect ratio should center the drawing');

  LSVGDOM.Root.PreserveAspectRatio := TSkSVGPreserveAspectRatio.Create(TSkSVGAspectAlign.None, TSkSVGAspectScale.Slice);
  Assert.IsTrue(LSVGDOM.Root.PreserveAspectRatio.Align = TSkSVGAspectAlign.None, '(Align)');
  Assert.IsTrue(LSVGDOM.Root.PreserveAspectRatio.Scale = TSkSVGAspectScale.Slice, '(Scale)');
end;

procedure TSkSvgDOMTests.TestSetContainerSize;

  function Render(const AContainerSize: Single): TBytes;
  var
    LSurface: ISkSurface;
    LSVGDOM: ISkSVGDOM;
  begin
    SetLength(Result, 100 * 100 * 4);
    LSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(100, 100), Result, 100 * 4);
    LSurface.Canvas.Clear(TAlphaColors.Null);
    LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + 'android.svg');
    Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
    LSVGDOM.SetContainerSize(TSizeF.Create(AContainerSize, AContainerSize));
    LSVGDOM.Render(LSurface.Canvas);
  end;

begin
  // android.svg declares no size of its own, so it is drawn over the whole
  // container.
  Assert.IsFalse(CompareMem(@Render(50)[0], @Render(100)[0], 100 * 100 * 4),
    'The container size should change the rendered drawing');
end;

procedure TSkSvgDOMTests.TestSetViewBox;
var
  LSVGDOM: ISkSVGDOM;
  LViewBox: TRectF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(SvgAssetsPath + 'tesla.svg');
  Assert.IsNotNull(LSVGDOM, 'Invalid SkSVGDOM');
  Assert.IsFalse(LSVGDOM.Root.TryGetViewBox(LViewBox), 'tesla.svg should not declare a view box');

  LSVGDOM.Root.SetViewBox(RectF(0, 0, 40, 40));
  Assert.IsTrue(LSVGDOM.Root.TryGetViewBox(LViewBox), 'The view box should be set');
  Assert.AreEqual(40.0, LViewBox.Width, TEpsilon.Vector, '(Width)');
  Assert.AreEqual(40.0, LViewBox.Height, TEpsilon.Vector, '(Height)');
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
