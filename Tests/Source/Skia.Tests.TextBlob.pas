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
unit Skia.Tests.TextBlob;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkTextBlobTests }

  [TestFixture]
  TSkTextBlobTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    FPixmap: ISkPixmap;
    function AssetFont(const ASize: Single = 40): ISkFont;
    function PaintedBounds(const ATextBlob: ISkTextBlob; const AX, AY: Single): TRect;
  public
    {$IF defined(MSWINDOWS)}
    [TestCase('Basic Texts', '0.99,n4f/B4P/gID///9nw//OzP///2/n/9/9/////////////////////////////+Xf3VHgwcVR//8')]
    {$ELSEIF defined(IOS)}
    [TestCase('Basic Texts', '0.99,n4P/g4P/gIj////jw//GzP////fX/87s/////////////////////////////+LvyqjAIMKA//8')]
    {$ELSEIF defined(MACOS)}
    [TestCase('Basic Texts', '0.99,h4P/g4P/gIj////jw//GzP////fX/87t////////3////////////////////+LvyqjAIMKA//8')]
    {$ENDIF}
    procedure TestBasics(const AMinSimilarity: Double; const AExpectedHash: string);
    [TestCase('Custom Font', '0.98,AQEFAAMH3////eXhw8ff////5enn1//////l6ffX//+gAfAB///AAMRJ//8AH4Af/////9v///8')]
    procedure TestCustomFont(const AMinSimilarity: Double; const AExpectedHash: string);
    [TestCase('Right-to-Left', '0.98,f3cDAQGD//9/f3NhQ8f//3//9+Vj1///f//35WP3///QoMRg1Wj1ePxom+gDACMAAgAAAAAAAAA')]
    procedure TestRightToLeft(const AMinSimilarity: Double; const AExpectedHash: string);
    [Test]
    procedure TestGetIntercepts;
    [Test]
    procedure TestMakeFromTextHorizontallyPositioned;
    [Test]
    procedure TestMakeFromTextPositioned;
    [Test]
    procedure TestMakeFromTextTransform;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Math,
  System.UITypes,
  System.IOUtils;

{ TSkTextBlobTests }

function TSkTextBlobTests.AssetFont(const ASize: Single): ISkFont;
var
  LTypeface: ISkTypeface;
begin
  LTypeface := TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf');
  Assert.IsNotNull(LTypeface, 'Invalid ISkTypeface (nil)');
  Result := TSkFont.Create(LTypeface, ASize);
  Result.Hinting := TSkFontHinting.None;
  Result.LinearMetrics := True;
end;

function TSkTextBlobTests.PaintedBounds(const ATextBlob: ISkTextBlob;
  const AX, AY: Single): TRect;
const
  Size = 300;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
  X: Integer;
  Y: Integer;
begin
  Assert.IsNotNull(ATextBlob, 'Invalid ISkTextBlob (nil)');
  SetLength(FBuffer, Size * Size * 4);
  FPixmap := TSkPixmap.Create(TSkImageInfo.Create(Size, Size, TSkColorType.BGRA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), FBuffer, Size * 4);
  LSurface := TSkSurface.MakeRasterDirect(FPixmap);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LSurface.Canvas.DrawTextBlob(ATextBlob, AX, AY, LPaint);

  Result := TRect.Empty;
  for Y := 0 to Size - 1 do
    for X := 0 to Size - 1 do
      if FPixmap.Colors[X, Y] <> TAlphaColors.Null then
        if Result.IsEmpty then
          Result := TRect.Create(X, Y, X + 1, Y + 1)
        else
          Result := TRect.Union(Result, TRect.Create(X, Y, X + 1, Y + 1));
  Assert.IsFalse(Result.IsEmpty, 'The text blob should paint something');
end;

procedure TSkTextBlobTests.TestGetIntercepts;
var
  LFont: ISkFont;
  LIntercepts: TArray<Single>;
  LTextBlob: ISkTextBlob;
begin
  LFont := AssetFont(50);
  LTextBlob := TSkTextBlob.MakeFromText('gy', LFont);
  Assert.IsNotNull(LTextBlob, 'Invalid ISkTextBlob (nil)');

  LIntercepts := LTextBlob.GetIntercepts(4, 8);
  Assert.IsTrue(Length(LIntercepts) > 0, 'The descenders should cross the underline band');
  Assert.AreEqual<NativeInt>(0, Length(LIntercepts) mod 2, 'Intercepts should be returned in pairs');

  LIntercepts := LTextBlob.GetIntercepts(-1000, -999);
  Assert.AreEqual<NativeInt>(0, Length(LIntercepts), 'A band far above the text should not be crossed');
end;

procedure TSkTextBlobTests.TestMakeFromTextHorizontallyPositioned;
var
  LBounds: TRect;
  LFont: ISkFont;
  LShiftedBounds: TRect;
begin
  LFont := AssetFont;
  LBounds := PaintedBounds(TSkTextBlob.MakeFromTextHorizontallyPositioned('I', [0], 0, LFont), 20, 100);
  LShiftedBounds := PaintedBounds(TSkTextBlob.MakeFromTextHorizontallyPositioned('I', [50], 0, LFont), 20, 100);
  Assert.AreEqual(LBounds.Left + 50, LShiftedBounds.Left, 'The X position should shift the glyph');
  Assert.AreEqual(LBounds.Top, LShiftedBounds.Top, 'The common Y should keep the glyph on the same baseline');
end;

procedure TSkTextBlobTests.TestMakeFromTextPositioned;
var
  LBounds: TRect;
  LFont: ISkFont;
  LShiftedBounds: TRect;
begin
  LFont := AssetFont;
  LBounds := PaintedBounds(TSkTextBlob.MakeFromTextPositioned('I', [PointF(0, 0)], LFont), 20, 100);
  LShiftedBounds := PaintedBounds(TSkTextBlob.MakeFromTextPositioned('I', [PointF(50, 30)], LFont), 20, 100);
  Assert.AreEqual(LBounds.Left + 50, LShiftedBounds.Left, 'The X position should shift the glyph');
  Assert.AreEqual(LBounds.Top + 30, LShiftedBounds.Top, 'The Y position should shift the glyph');
end;

procedure TSkTextBlobTests.TestMakeFromTextTransform;
var
  LBounds: TRect;
  LFont: ISkFont;
  LScaledBounds: TRect;
begin
  LFont := AssetFont;
  LBounds := PaintedBounds(TSkTextBlob.MakeFromTextTransform('I',
    [TSkRotationScaleMatrix.CreateDegrees(1, 0, 0, 0, 0, 0)], LFont), 20, 100);
  LScaledBounds := PaintedBounds(TSkTextBlob.MakeFromTextTransform('I',
    [TSkRotationScaleMatrix.CreateDegrees(2, 0, 0, 0, 0, 0)], LFont), 20, 100);
  Assert.IsTrue(LScaledBounds.Height > LBounds.Height * 1.5, 'The scale should enlarge the glyph');
  Assert.IsTrue(LScaledBounds.Width > LBounds.Width, 'The scale should widen the glyph');
end;

procedure TSkTextBlobTests.TestBasics(const AMinSimilarity: Double;
  const AExpectedHash: string);
var
  LBlob1: ISkTextBlob;
  LBlob2: ISkTextBlob;
  LFont1: ISkFont;
  LFont2: ISkFont;
  LPaint1: ISkPaint;
  LPaint2: ISkPaint;
  LPaint3: ISkPaint;
  LTypeface: ISkTypeface;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(200, 270);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
  LFont1 := TSkFont.Create(LTypeface, 64, 1);
  LFont2 := TSkFont.Create(LTypeface, 64, 1.5);
  LFont1.Edging := TSkFontEdging.AntiAlias;
  LFont2.Edging := TSkFontEdging.AntiAlias;

  LBlob1 := TSkTextBlob.MakeFromText('Skia', LFont1);
  LBlob2 := TSkTextBlob.MakeFromText('Skia', LFont2);

  LPaint1 := TSkPaint.Create;
  LPaint1.AntiAlias := True;
  LPaint1.SetARGB($FF, $42, $85, $F4);

  LPaint2 := TSkPaint.Create(TSkPaintStyle.Stroke);
  LPaint2.AntiAlias := True;
  LPaint2.SetARGB($FF, $DB, $44, $37);
  LPaint2.StrokeWidth := 3;

  LPaint3 := TSkPaint.Create;
  LPaint3.AntiAlias := True;
  LPaint3.SetARGB($FF, $0F, $9D, $58);

  LSurface.Canvas.DrawTextBlob(LBlob1, 20, 64, LPaint1);
  LSurface.Canvas.DrawSimpleText('Skia', 20, 154, LFont1, LPaint2);
  LSurface.Canvas.DrawTextBlob(LBlob2, 20, 244, LPaint3);

  Assert.AreSimilar(AExpectedHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkTextBlobTests.TestCustomFont(const AMinSimilarity: Double;
  const AExpectedHash: string);
var
  LSurface: ISkSurface;
  LFont: ISkFont;
  LPaint: ISkPaint;
begin
  LSurface := TSkSurface.MakeRaster(260, 160);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LPaint := TSkPaint.Create;
  LPaint.Color := $8FFFFFFF;
  LSurface.Canvas.DrawRect(RectF(0, 0, 260, 160), LPaint);
  LPaint.Reset;

  LFont := TSkFont.Create(TSkTypeface.MakeFromFile(FontAssetsPath + 'nunito-extrabold.ttf'), 23);
  LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(256, 145), $FFFF5F5F, $FF5B8DFE, TSkTileMode.Clamp);

  LSurface.Canvas.DrawSimpleText('"Each dream that you', 2, 25, LFont, LPaint);
  LSurface.Canvas.DrawSimpleText('leave behind is a part', 2, 55, LFont, LPaint);
  LSurface.Canvas.DrawSimpleText('of your future that will', 2, 85, LFont, LPaint);
  LSurface.Canvas.DrawSimpleText('no longer exist."', 2, 115, LFont, LPaint);

  LFont := TSkFont.Create(TSkTypeface.MakeFromFile(FontAssetsPath + 'bonheur-royale-regular.ttf'), 28);
  LPaint.Shader := nil;
  LPaint.Color  := $FF5B8DFE;
  LSurface.Canvas.DrawSimpleText('(Steve Jobs)', 2, 150, LFont, LPaint);

  Assert.AreSimilar(AExpectedHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkTextBlobTests.TestRightToLeft(const AMinSimilarity: Double; const AExpectedHash: string);
var
  LBlob: ISkTextBlob;
  LFont: ISkFont;
  LPaint: ISkPaint;
  LShaper: ISkShaper;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(256, 100);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LFont := TSkFont.Create(TSkTypeface.MakeFromFile(FontAssetsPath + 'msuighur.ttf'), 85);
  LShaper := TSkShaper.Create;
  LBlob := LShaper.Shape('سلام دنیا!', LFont, False, MaxSingle);

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := TAlphaColors.Tomato;

  LSurface.Canvas.DrawTextBlob(LBlob, 0, 0, LPaint);

  Assert.AreSimilar(AExpectedHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkTextBlobTests);
end.
