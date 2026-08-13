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
unit Skia.Tests.ParagraphStyle;

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
  { TSkTextStyleTests }

  [TestFixture]
  TSkTextStyleTests = class(TTestBase)
  public
    [Test]
    procedure TestDecoration;
    [Test]
    procedure TestDefaultValues;
    [Test]
    procedure TestFontFeatures;
    [Test]
    procedure TestFontMetrics;
    [Test]
    procedure TestForegroundAndBackground;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestSpacingAndHeight;
    [Test]
    procedure TestTypography;
  end;

  { TSkParagraphStyleTests }

  [TestFixture]
  TSkParagraphStyleTests = class(TTestBase)
  public
    [Test]
    procedure TestDefaultValues;
    [Test]
    procedure TestDisableHinting;
    [Test]
    procedure TestEllipsisAndMaxLines;
    [Test]
    procedure TestStrutStyle;
    [Test]
    procedure TestTextAlignAndDirection;
    [Test]
    procedure TestTextHeightBehaviors;
    [Test]
    procedure TestTextStyle;
  end;

  { TSkStrutStyleTests }

  [TestFixture]
  TSkStrutStyleTests = class(TTestBase)
  public
    [Test]
    procedure TestDefaultValues;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestProperties;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types,
  System.UITypes;

{ TSkTextStyleTests }

procedure TSkTextStyleTests.TestDecoration;
var
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  Assert.IsTrue(LTextStyle.Decorations = [], 'A new text style should not be decorated');

  LTextStyle.Decorations := [TSkTextDecoration.Underline, TSkTextDecoration.LineThrough];
  Assert.IsTrue(LTextStyle.Decorations = [TSkTextDecoration.Underline, TSkTextDecoration.LineThrough], '(Decorations)');

  LTextStyle.DecorationColor := TAlphaColors.Red;
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LTextStyle.DecorationColor, '(DecorationColor)');

  LTextStyle.DecorationStyle := TSkTextDecorationStyle.Wavy;
  Assert.IsTrue(LTextStyle.DecorationStyle = TSkTextDecorationStyle.Wavy, '(DecorationStyle)');

  LTextStyle.DecorationThickness := 2.5;
  Assert.AreEqual(2.5, LTextStyle.DecorationThickness, TEpsilon.Vector, '(DecorationThickness)');
end;

procedure TSkTextStyleTests.TestDefaultValues;
var
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  Assert.IsNotNull(LTextStyle);
  Assert.AreEqual<TAlphaColor>(TAlphaColors.White, LTextStyle.Color, 'Skia defaults the text color to white');
  Assert.AreEqual(14.0, LTextStyle.FontSize, TEpsilon.FontSize, '(FontSize)');
  Assert.IsTrue(LTextStyle.Decorations = [], '(Decorations)');
  Assert.IsTrue(LTextStyle.DecorationStyle = TSkTextDecorationStyle.Solid, '(DecorationStyle)');
  Assert.IsFalse(LTextStyle.HalfLeading, '(HalfLeading)');
  Assert.AreEqual(0.0, LTextStyle.LetterSpacing, TEpsilon.Vector, '(LetterSpacing)');
  Assert.AreEqual(0.0, LTextStyle.WordSpacing, TEpsilon.Vector, '(WordSpacing)');
  // GetForeground/GetBackground always return a copy of the paint: the API has
  // no equivalent of Skia's hasForeground/hasBackground.
  Assert.IsNotNull(LTextStyle.GetForeground, '(Foreground)');
  Assert.IsNotNull(LTextStyle.GetBackground, '(Background)');
end;

procedure TSkTextStyleTests.TestFontFeatures;
var
  LTextStyle: ISkTextStyle;
begin
  // The font features are write only in the API, so the check is that the calls
  // are accepted and do not disturb the rest of the style.
  LTextStyle := TSkTextStyle.Create;
  LTextStyle.AddFontFeature('liga', 1);
  LTextStyle.AddFontFeature('kern', 0);
  LTextStyle.FontSize := 20;
  LTextStyle.ResetFontFeatures;
  Assert.AreEqual(20.0, LTextStyle.FontSize, TEpsilon.FontSize, 'Resetting the font features should keep the rest of the style');

  LTextStyle.AddShadow(TSkTextShadow.Create(TAlphaColors.Red, PointF(2, 2), 3));
  LTextStyle.ResetShadows;
  Assert.AreEqual(20.0, LTextStyle.FontSize, TEpsilon.FontSize, 'Resetting the shadows should keep the rest of the style');
end;

procedure TSkTextStyleTests.TestFontMetrics;
var
  LMetrics: TSkFontMetrics;
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.FontSize := 40;
  // A standalone text style has no resolved font: Skia only fills these metrics
  // while a paragraph is laid out.
  LMetrics := LTextStyle.FontMetrics;
  Assert.IsTrue(LMetrics.Descent >= LMetrics.Ascent, 'Descent should never be above Ascent');
  Assert.AreEqual(LMetrics.Ascent, LTextStyle.FontMetrics.Ascent, TEpsilon.Position, 'The metrics should be stable');
end;

procedure TSkTextStyleTests.TestForegroundAndBackground;
var
  LPaint: ISkPaint;
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;

  LTextStyle.SetForegroundColor(LPaint);
  Assert.IsNotNull(LTextStyle.GetForeground, 'The foreground paint should be kept');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LTextStyle.GetForeground.Color, '(Foreground color)');

  LTextStyle.SetBackgroundColor(LPaint);
  Assert.IsNotNull(LTextStyle.GetBackground, 'The background paint should be kept');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LTextStyle.GetBackground.Color, '(Background color)');

  // Clearing is accepted but is not observable through the getters, which always
  // hand back a copy of the paint.
  LTextStyle.ClearForegroundColor;
  LTextStyle.ClearBackgroundColor;
  Assert.IsNotNull(LTextStyle.GetForeground, 'The getter still returns a paint after clearing');
  Assert.IsNotNull(LTextStyle.GetBackground, 'The getter still returns a paint after clearing');
end;

procedure TSkTextStyleTests.TestIsEqual;
var
  LTextStyle1: ISkTextStyle;
  LTextStyle2: ISkTextStyle;
begin
  LTextStyle1 := TSkTextStyle.Create;
  LTextStyle2 := TSkTextStyle.Create;
  Assert.IsTrue(LTextStyle1.IsEqual(LTextStyle2), 'Two new text styles should be equal');

  LTextStyle2.FontSize := 30;
  Assert.IsFalse(LTextStyle1.IsEqual(LTextStyle2), 'Text styles with different sizes should not be equal');

  LTextStyle1.FontSize := 30;
  Assert.IsTrue(LTextStyle1.IsEqual(LTextStyle2), 'Text styles with the same settings should be equal again');
end;

procedure TSkTextStyleTests.TestSpacingAndHeight;
var
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;

  LTextStyle.LetterSpacing := 3;
  Assert.AreEqual(3.0, LTextStyle.LetterSpacing, TEpsilon.Vector, '(LetterSpacing)');
  LTextStyle.WordSpacing := 7;
  Assert.AreEqual(7.0, LTextStyle.WordSpacing, TEpsilon.Vector, '(WordSpacing)');
  LTextStyle.HeightMultiplier := 2;
  Assert.AreEqual(2.0, LTextStyle.HeightMultiplier, TEpsilon.Vector, '(HeightMultiplier)');
  LTextStyle.HalfLeading := True;
  Assert.IsTrue(LTextStyle.HalfLeading, '(HalfLeading)');
end;

procedure TSkTextStyleTests.TestTypography;
var
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;

  LTextStyle.Color := TAlphaColors.Blue;
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Blue, LTextStyle.Color, '(Color)');

  LTextStyle.FontSize := 22.5;
  Assert.AreEqual(22.5, LTextStyle.FontSize, TEpsilon.FontSize, '(FontSize)');

  LTextStyle.FontStyle := TSkFontStyle.BoldItalic;
  Assert.IsTrue(LTextStyle.FontStyle = TSkFontStyle.BoldItalic, '(FontStyle)');

  LTextStyle.FontFamilies := TArray<string>.Create('Arial', DefaultFontFamily);
  Assert.AreEqual<NativeInt>(2, Length(LTextStyle.FontFamilies), '(FontFamilies count)');
  Assert.AreEqual('Arial', LTextStyle.FontFamilies[0], '(FontFamilies first)');

  LTextStyle.Locale := 'pt-BR';
  Assert.AreEqual('pt-BR', LTextStyle.Locale, '(Locale)');
end;

{ TSkParagraphStyleTests }

procedure TSkParagraphStyleTests.TestDefaultValues;
var
  LParagraphStyle: ISkParagraphStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  Assert.IsNotNull(LParagraphStyle);
  Assert.IsTrue(LParagraphStyle.TextAlign = TSkTextAlign.Start, '(TextAlign)');
  Assert.IsTrue(LParagraphStyle.TextDirection = TSkTextDirection.LeftToRight, '(TextDirection)');
  Assert.IsTrue(LParagraphStyle.TextHeightBehaviors = [], '(TextHeightBehaviors)');
  Assert.IsEmpty(LParagraphStyle.Ellipsis, '(Ellipsis)');
  Assert.IsNotNull(LParagraphStyle.TextStyle, 'A paragraph style always has a text style');
  Assert.IsNotNull(LParagraphStyle.StrutStyle, 'A paragraph style always has a strut style');
end;

procedure TSkParagraphStyleTests.TestDisableHinting;
var
  LParagraphStyle: ISkParagraphStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.MaxLines := 3;
  LParagraphStyle.DisableHinting;
  Assert.AreEqual<NativeUInt>(3, LParagraphStyle.MaxLines, 'Disabling the hinting should keep the rest of the style');
end;

procedure TSkParagraphStyleTests.TestEllipsisAndMaxLines;
var
  LParagraphStyle: ISkParagraphStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.Ellipsis := '...';
  Assert.AreEqual('...', LParagraphStyle.Ellipsis, '(Ellipsis)');
  LParagraphStyle.MaxLines := 2;
  Assert.AreEqual<NativeUInt>(2, LParagraphStyle.MaxLines, '(MaxLines)');
  LParagraphStyle.Height := 40;
  Assert.AreEqual(40.0, LParagraphStyle.Height, TEpsilon.Position, '(Height)');
end;

procedure TSkParagraphStyleTests.TestStrutStyle;
var
  LParagraphStyle: ISkParagraphStyle;
  LStrutStyle: ISkStrutStyle;
begin
  LStrutStyle := TSkStrutStyle.Create;
  LStrutStyle.Enabled := True;
  LStrutStyle.FontSize := 33;

  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.StrutStyle := LStrutStyle;
  Assert.IsNotNull(LParagraphStyle.StrutStyle);
  Assert.IsTrue(LParagraphStyle.StrutStyle.Enabled, 'The assigned strut style should be kept');
  Assert.AreEqual(33.0, LParagraphStyle.StrutStyle.FontSize, TEpsilon.FontSize, '(FontSize)');
  Assert.IsTrue(LParagraphStyle.StrutStyle.IsEqual(LStrutStyle), 'The returned strut style should match the assigned one');
end;

procedure TSkParagraphStyleTests.TestTextAlignAndDirection;
var
  LParagraphStyle: ISkParagraphStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.TextAlign := TSkTextAlign.Center;
  Assert.IsTrue(LParagraphStyle.TextAlign = TSkTextAlign.Center, '(Center)');
  LParagraphStyle.TextAlign := TSkTextAlign.Justify;
  Assert.IsTrue(LParagraphStyle.TextAlign = TSkTextAlign.Justify, '(Justify)');

  LParagraphStyle.TextDirection := TSkTextDirection.RightToLeft;
  Assert.IsTrue(LParagraphStyle.TextDirection = TSkTextDirection.RightToLeft, '(RightToLeft)');
end;

procedure TSkParagraphStyleTests.TestTextHeightBehaviors;
var
  LParagraphStyle: ISkParagraphStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.TextHeightBehaviors := [TSkTextHeightBehavior.DisableFirstAscent];
  Assert.IsTrue(LParagraphStyle.TextHeightBehaviors = [TSkTextHeightBehavior.DisableFirstAscent], '(first ascent)');

  LParagraphStyle.TextHeightBehaviors := [TSkTextHeightBehavior.DisableFirstAscent, TSkTextHeightBehavior.DisableLastDescent];
  Assert.IsTrue(LParagraphStyle.TextHeightBehaviors = [TSkTextHeightBehavior.DisableFirstAscent, TSkTextHeightBehavior.DisableLastDescent], '(both)');
end;

procedure TSkParagraphStyleTests.TestTextStyle;
var
  LParagraphStyle: ISkParagraphStyle;
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  LTextStyle.FontSize := 44;
  LTextStyle.Color := TAlphaColors.Red;

  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.TextStyle := LTextStyle;
  Assert.IsNotNull(LParagraphStyle.TextStyle);
  Assert.AreEqual(44.0, LParagraphStyle.TextStyle.FontSize, TEpsilon.FontSize, '(FontSize)');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LParagraphStyle.TextStyle.Color, '(Color)');
  Assert.IsTrue(LParagraphStyle.TextStyle.IsEqual(LTextStyle), 'The returned text style should match the assigned one');
end;

{ TSkStrutStyleTests }

procedure TSkStrutStyleTests.TestDefaultValues;
var
  LStrutStyle: ISkStrutStyle;
begin
  LStrutStyle := TSkStrutStyle.Create;
  Assert.IsNotNull(LStrutStyle);
  Assert.IsFalse(LStrutStyle.Enabled, '(Enabled)');
  Assert.IsFalse(LStrutStyle.ForceHeight, '(ForceHeight)');
  Assert.IsFalse(LStrutStyle.HalfLeading, '(HalfLeading)');
  Assert.AreEqual<NativeInt>(0, Length(LStrutStyle.FontFamilies), '(FontFamilies)');
end;

procedure TSkStrutStyleTests.TestIsEqual;
var
  LStrutStyle1: ISkStrutStyle;
  LStrutStyle2: ISkStrutStyle;
begin
  LStrutStyle1 := TSkStrutStyle.Create;
  LStrutStyle2 := TSkStrutStyle.Create;
  Assert.IsTrue(LStrutStyle1.IsEqual(LStrutStyle2), 'Two new strut styles should be equal');

  LStrutStyle2.Enabled := True;
  Assert.IsFalse(LStrutStyle1.IsEqual(LStrutStyle2), 'Strut styles with different settings should not be equal');
end;

procedure TSkStrutStyleTests.TestProperties;
var
  LStrutStyle: ISkStrutStyle;
begin
  LStrutStyle := TSkStrutStyle.Create;

  LStrutStyle.Enabled := True;
  Assert.IsTrue(LStrutStyle.Enabled, '(Enabled)');
  LStrutStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  Assert.AreEqual<NativeInt>(1, Length(LStrutStyle.FontFamilies), '(FontFamilies count)');
  Assert.AreEqual(DefaultFontFamily, LStrutStyle.FontFamilies[0], '(FontFamilies)');
  LStrutStyle.FontSize := 18;
  Assert.AreEqual(18.0, LStrutStyle.FontSize, TEpsilon.FontSize, '(FontSize)');
  LStrutStyle.FontStyle := TSkFontStyle.Bold;
  Assert.IsTrue(LStrutStyle.FontStyle = TSkFontStyle.Bold, '(FontStyle)');
  LStrutStyle.ForceHeight := True;
  Assert.IsTrue(LStrutStyle.ForceHeight, '(ForceHeight)');
  LStrutStyle.HalfLeading := True;
  Assert.IsTrue(LStrutStyle.HalfLeading, '(HalfLeading)');
  LStrutStyle.HeightMultiplier := 1.5;
  Assert.AreEqual(1.5, LStrutStyle.HeightMultiplier, TEpsilon.Vector, '(HeightMultiplier)');
  LStrutStyle.Leading := 2.5;
  Assert.AreEqual(2.5, LStrutStyle.Leading, TEpsilon.Vector, '(Leading)');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkTextStyleTests);
  TDUnitX.RegisterTestFixture(TSkParagraphStyleTests);
  TDUnitX.RegisterTestFixture(TSkStrutStyleTests);
end.
