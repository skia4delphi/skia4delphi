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
unit Skia.Tests.Font;

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
  { TSkFontTests }

  [TestFixture]
  TSkFontTests = class(TTestBase)
  strict private
    function AssetTypeface: ISkTypeface;
    function CreateFont(const ASize: Single = 12): ISkFont;
  public
    [Test]
    procedure TestCopyConstructor;
    [Test]
    procedure TestDefaultValues;
    [Test]
    procedure TestEmboldenGrowsGlyphBounds;
    [Test]
    procedure TestEmptyTextHasNoGlyphs;
    [Test]
    procedure TestGetBounds;
    [Test]
    procedure TestGetIntercepts;
    [Test]
    procedure TestGetPath;
    [Test]
    procedure TestGetPaths;
    [Test]
    procedure TestGetPositions;
    [Test]
    procedure TestHorizontalPositionsAreCumulative;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestMakeWithSize;
    [Test]
    procedure TestMeasureTextMatchesWidthsSum;
    [Test]
    procedure TestMeasureTextWithBounds;
    [Test]
    procedure TestMetrics;
    [Test]
    procedure TestMetricsScaleWithSize;
    [TestCase('Size',   'Size')]
    [TestCase('ScaleX', 'ScaleX')]
    [TestCase('SkewX',  'SkewX')]
    procedure TestSingleProperties(const APropertyName: string);
    [Test]
    procedure TestScaleXScalesMeasure;
    [Test]
    procedure TestSizeScalesMeasure;
    [Test]
    procedure TestStateProperties;
    [Test]
    procedure TestTypeface;
    // TODO: Investigate possible issue.
    // [Test]
    // procedure TestTypefaceSetToNil;
    [Test]
    procedure TestUnicharToGlyph;
    [Test]
    procedure TestUnicharsToGlyphs;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types;

{ TSkFontTests }

function TSkFontTests.AssetTypeface: ISkTypeface;
begin
  Result := TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf');
  Assert.IsNotNull(Result, 'Invalid ISkTypeface (nil)');
end;

function TSkFontTests.CreateFont(const ASize: Single): ISkFont;
begin
  Result := TSkFont.Create(AssetTypeface, ASize);
  Assert.IsNotNull(Result, 'Invalid ISkFont (nil)');
  Result.Hinting := TSkFontHinting.None;
  Result.LinearMetrics := True;
  Result.Subpixel := True;
end;

procedure TSkFontTests.TestCopyConstructor;
var
  LCopy: ISkFont;
  LFont: ISkFont;
begin
  LFont := CreateFont(17);
  LFont.SkewX := 0.25;
  LCopy := TSkFont.Create(LFont);
  Assert.IsNotNull(LCopy);
  Assert.IsTrue(LFont.IsEqual(LCopy), 'The copy should be equal to the source');
  Assert.AreEqual(17.0, LCopy.Size, TEpsilon.FontSize);
  Assert.AreEqual(0.25, LCopy.SkewX, TEpsilon.Scale);

  LCopy.Size := 18;
  Assert.AreEqual(17.0, LFont.Size, TEpsilon.FontSize, 'Changing the copy should not change the source');
end;

procedure TSkFontTests.TestDefaultValues;
var
  LFont: ISkFont;
begin
  LFont := TSkFont.Create;
  Assert.IsNotNull(LFont);
  Assert.AreEqual(12.0, LFont.Size, TEpsilon.FontSize);
  Assert.AreEqual(1.0, LFont.ScaleX, TEpsilon.Scale);
  Assert.AreEqual(0.0, LFont.SkewX, TEpsilon.Scale);
  Assert.IsTrue(LFont.Edging = TSkFontEdging.AntiAlias, '(Edging)');
  Assert.IsTrue(LFont.Hinting = TSkFontHinting.Normal, '(Hinting)');
  Assert.IsTrue(LFont.BaselineSnap, '(BaselineSnap)');
  Assert.IsFalse(LFont.EmbeddedBitmaps, '(EmbeddedBitmaps)');
  Assert.IsFalse(LFont.Embolden, '(Embolden)');
  Assert.IsFalse(LFont.ForceAutoHinting, '(ForceAutoHinting)');
  Assert.IsFalse(LFont.LinearMetrics, '(LinearMetrics)');
  Assert.IsFalse(LFont.Subpixel, '(Subpixel)');
  Assert.IsNotNull(LFont.Typeface, 'A font created without a typeface should fall back to the default one');
  Assert.AreEqual(TSkTypeface.MakeDefault.FamilyName, LFont.Typeface.FamilyName, 'The fallback should be the default typeface');
  Assert.IsNotNull(LFont.GetTypefaceOrDefault, 'TypefaceOrDefault should never be nil');
end;

procedure TSkFontTests.TestEmboldenGrowsGlyphBounds;
var
  LBounds: TRectF;
  LEmboldenBounds: TRectF;
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LWidth: Single;
begin
  LFont := CreateFont(40);
  LGlyphs := LFont.GetGlyphs('S');
  LBounds := LFont.GetBounds(LGlyphs)[0];
  LWidth := LFont.MeasureText('S');

  LFont.Embolden := True;
  LEmboldenBounds := LFont.GetBounds(LGlyphs)[0];
  Assert.IsTrue(LEmboldenBounds.Width > LBounds.Width, 'Embolden should grow the glyph bounds horizontally');
  Assert.IsTrue(LEmboldenBounds.Height > LBounds.Height, 'Embolden should grow the glyph bounds vertically');
  Assert.AreEqual(LWidth, LFont.MeasureText('S'), TEpsilon.Position, 'Embolden should not change the advance');
end;

procedure TSkFontTests.TestEmptyTextHasNoGlyphs;
var
  LFont: ISkFont;
begin
  LFont := CreateFont;
  Assert.AreEqual<NativeInt>(0, Length(LFont.GetGlyphs('')), '(GetGlyphs)');
  Assert.AreEqual(0.0, LFont.MeasureText(''), TEpsilon.Position, '(MeasureText)');
  Assert.AreEqual<NativeInt>(0, Length(LFont.GetWidths(nil)), '(GetWidths)');
  Assert.AreEqual<NativeInt>(0, Length(LFont.GetHorizontalPositions(nil)), '(GetHorizontalPositions)');
  Assert.AreEqual<NativeInt>(0, Length(LFont.UnicharsToGlyphs(nil)), '(UnicharsToGlyphs)');
end;

procedure TSkFontTests.TestGetBounds;
var
  LBounds: TArray<TRectF>;
  LBounds2: TArray<TRectF>;
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LWidths: TArray<Single>;
  I: Integer;
begin
  LFont := CreateFont(50);
  LGlyphs := LFont.GetGlyphs('Ay');
  Assert.AreEqual<NativeInt>(2, Length(LGlyphs));
  LBounds := LFont.GetBounds(LGlyphs);
  Assert.AreEqual<NativeInt>(2, Length(LBounds));
  LFont.GetWidthsAndBounds(LGlyphs, LWidths, LBounds2);
  Assert.AreEqual<NativeInt>(2, Length(LWidths));
  Assert.AreEqual<NativeInt>(2, Length(LBounds2));
  for I := 0 to High(LGlyphs) do
  begin
    Assert.IsTrue(LBounds[I].Width > 0, 'Empty bounds width at index ' + IntToStr(I));
    Assert.IsTrue(LBounds[I].Height > 0, 'Empty bounds height at index ' + IntToStr(I));
    Assert.AreSameRect(LBounds[I], LBounds2[I], TEpsilon.Position, 'GetBounds and GetWidthsAndBounds disagree at index ' + IntToStr(I));
    Assert.AreEqual(LWidths[I], LFont.GetWidths(LGlyphs)[I], TEpsilon.Position, 'GetWidths and GetWidthsAndBounds disagree at index ' + IntToStr(I));
  end;
  Assert.IsTrue(LBounds[1].Bottom > 0, 'The descender of "y" should go below the baseline');
end;

procedure TSkFontTests.TestGetIntercepts;
var
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LIntercepts: TArray<Single>;
begin
  LFont := CreateFont(50);
  LGlyphs := LFont.GetGlyphs('gy');
  LIntercepts := LFont.GetIntercepts(LGlyphs, LFont.GetPositions(LGlyphs), 4, 8);
  Assert.IsTrue(Length(LIntercepts) > 0, 'The descenders should cross the underline band');
  Assert.AreEqual<NativeInt>(0, Length(LIntercepts) mod 2, 'Intercepts should be returned in pairs');
  Assert.IsTrue(LIntercepts[0] < LIntercepts[1], 'Each intercept pair should be ordered');

  LIntercepts := LFont.GetIntercepts(LGlyphs, LFont.GetPositions(LGlyphs), -1000, -999);
  Assert.AreEqual<NativeInt>(0, Length(LIntercepts), 'A band far above the text should not be crossed');
end;

procedure TSkFontTests.TestGetPath;
var
  LFont: ISkFont;
  LPath: ISkPath;
begin
  LFont := CreateFont(50);
  LPath := LFont.GetPath(LFont.GetGlyphs('A')[0]);
  Assert.IsNotNull(LPath, 'Invalid ISkPath (nil)');
  Assert.IsFalse(LPath.IsEmpty, 'The path of "A" should not be empty');
  Assert.IsTrue(LPath.Bounds.Width > 0, 'The path of "A" should have width');

  LPath := LFont.GetPath(LFont.GetGlyphs(' ')[0]);
  Assert.IsTrue((LPath = nil) or LPath.IsEmpty, 'The path of a space should be empty');
end;

procedure TSkFontTests.TestGetPaths;
var
  LCount: Integer;
  LFont: ISkFont;
begin
  LFont := CreateFont(50);
  LCount := 0;
  LFont.GetPaths(LFont.GetGlyphs('AB'),
    procedure (const APath: ISkPath; const AMatrix: TMatrix)
    begin
      Inc(LCount);
      Assert.IsNotNull(APath, 'Invalid ISkPath (nil)');
      Assert.IsFalse(APath.IsEmpty, 'Empty glyph path');
    end);
  Assert.AreEqual(2, LCount, 'GetPaths should be called once per drawable glyph');
end;

procedure TSkFontTests.TestGetPositions;
var
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LHorizontal: TArray<Single>;
  LPositions: TArray<TPointF>;
  I: Integer;
begin
  LFont := CreateFont(30);
  LGlyphs := LFont.GetGlyphs('Skia');
  LHorizontal := LFont.GetHorizontalPositions(LGlyphs);
  LPositions := LFont.GetPositions(LGlyphs);
  Assert.AreEqual<NativeInt>(Length(LGlyphs), Length(LPositions));
  for I := 0 to High(LGlyphs) do
  begin
    Assert.AreEqual(LHorizontal[I], LPositions[I].X, TEpsilon.Position, 'X mismatch at index ' + IntToStr(I));
    Assert.AreEqual(0.0, LPositions[I].Y, TEpsilon.Position, 'Y mismatch at index ' + IntToStr(I));
  end;

  LPositions := LFont.GetPositions(LGlyphs, PointF(10, 20));
  for I := 0 to High(LGlyphs) do
  begin
    Assert.AreEqual(LHorizontal[I] + 10, LPositions[I].X, TEpsilon.Position, 'Origin X was not applied at index ' + IntToStr(I));
    Assert.AreEqual(20.0, LPositions[I].Y, TEpsilon.Position, 'Origin Y was not applied at index ' + IntToStr(I));
  end;
end;

procedure TSkFontTests.TestHorizontalPositionsAreCumulative;
var
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LPositions: TArray<Single>;
  LWidths: TArray<Single>;
  I: Integer;
begin
  LFont := CreateFont(30);
  LGlyphs := LFont.GetGlyphs('Skia');
  LWidths := LFont.GetWidths(LGlyphs);
  LPositions := LFont.GetHorizontalPositions(LGlyphs, 5);
  Assert.AreEqual<NativeInt>(Length(LGlyphs), Length(LPositions));
  Assert.AreEqual(5.0, LPositions[0], TEpsilon.Position, 'The first position should be the origin');
  for I := 1 to High(LPositions) do
    Assert.AreEqual(LPositions[I - 1] + LWidths[I - 1], LPositions[I], TEpsilon.Position, 'Advance mismatch at index ' + IntToStr(I));
end;

procedure TSkFontTests.TestIsEqual;
var
  LFont1: ISkFont;
  LFont2: ISkFont;
  LFont3: ISkFont;
  LTypeface: ISkTypeface;
begin
  LTypeface := AssetTypeface;
  LFont1 := TSkFont.Create(LTypeface, 20);
  LFont2 := TSkFont.Create(LTypeface, 20);
  LFont3 := TSkFont.Create(AssetTypeface, 20);
  Assert.IsTrue(LFont1.IsEqual(LFont2), 'Fonts with the same settings should be equal');
  Assert.IsFalse(LFont1.IsEqual(LFont3), 'Fonts using different typeface instances should not be equal');
  LFont2.Size := 21;
  Assert.IsFalse(LFont1.IsEqual(LFont2), 'Fonts with different sizes should not be equal');
  LFont2.Size := 20;
  LFont2.Embolden := True;
  Assert.IsFalse(LFont1.IsEqual(LFont2), 'Fonts with different flags should not be equal');
end;

procedure TSkFontTests.TestMakeWithSize;
var
  LFont: ISkFont;
  LOther: ISkFont;
begin
  LFont := CreateFont(20);
  LOther := LFont.MakeWithSize(40);
  Assert.IsNotNull(LOther);
  Assert.AreEqual(40.0, LOther.Size, TEpsilon.FontSize);
  Assert.AreEqual(20.0, LFont.Size, TEpsilon.FontSize, 'The source font should not be changed');
  Assert.IsFalse(LFont.IsEqual(LOther));
  Assert.IsTrue(LOther.LinearMetrics, 'MakeWithSize should preserve the other settings');
end;

procedure TSkFontTests.TestMeasureTextMatchesWidthsSum;
const
  Text = 'Skia4Delphi';
var
  LFont: ISkFont;
  LSum: Single;
  LWidth: Single;
  LWidths: TArray<Single>;
  I: Integer;
begin
  LFont := CreateFont(30);
  LWidth := LFont.MeasureText(Text);
  LWidths := LFont.GetWidths(LFont.GetGlyphs(Text));
  LSum := 0;
  for I := 0 to High(LWidths) do
    LSum := LSum + LWidths[I];
  Assert.AreEqual(LSum, LWidth, TEpsilon.Position, 'MeasureText should be the sum of the glyph advances');
  Assert.AreEqual(LWidth, LFont.MeasureTextGlyphs(LFont.GetGlyphs(Text)), TEpsilon.Position, 'MeasureText and MeasureTextGlyphs should agree');
end;

procedure TSkFontTests.TestMeasureTextWithBounds;
var
  LBounds: TRectF;
  LFont: ISkFont;
  LGlyphBounds: TRectF;
  LWidth: Single;
begin
  LFont := CreateFont(50);
  LWidth := LFont.MeasureText('Skia', LBounds);
  Assert.AreEqual(LFont.MeasureText('Skia'), LWidth, TEpsilon.Position, 'Both MeasureText overloads should agree');
  Assert.IsTrue(LBounds.Width > 0, 'The measured bounds should not be empty');
  Assert.IsTrue(LBounds.Width <= LWidth + TEpsilon.Position, 'The tight bounds should not exceed the advance');
  Assert.AreEqual(LWidth, LFont.MeasureTextGlyphs(LFont.GetGlyphs('Skia'), LGlyphBounds), TEpsilon.Position);
  Assert.AreSameRect(LBounds, LGlyphBounds, TEpsilon.Position, 'Text and glyph bounds should agree');
end;

procedure TSkFontTests.TestMetrics;
var
  LFont: ISkFont;
  LMetrics: TSkFontMetrics;
  LSpacing: Single;
begin
  LFont := CreateFont(40);
  LSpacing := LFont.GetMetrics(LMetrics);
  Assert.IsTrue(LMetrics.Ascent < 0, 'Ascent should be negative (above the baseline)');
  Assert.IsTrue(LMetrics.Descent > 0, 'Descent should be positive (below the baseline)');
  Assert.IsTrue(LMetrics.Top <= LMetrics.Ascent, 'Top should not be below Ascent');
  Assert.IsTrue(LMetrics.Bottom >= LMetrics.Descent, 'Bottom should not be above Descent');
  Assert.IsTrue(LMetrics.XHeight > 0, 'XHeight should be positive');
  Assert.IsTrue(LMetrics.CapHeight > LMetrics.XHeight, 'CapHeight should be greater than XHeight');
  Assert.AreEqual(LMetrics.Descent - LMetrics.Ascent + LMetrics.Leading, LSpacing, TEpsilon.Position, 'Spacing should be descent - ascent + leading');
  Assert.AreEqual(LSpacing, LFont.Spacing, TEpsilon.Position, 'GetMetrics and Spacing should agree');
  Assert.IsTrue(TSkFontMetricsFlag.UnderlineThicknessIsValid in LMetrics.Flags, 'The asset font declares an underline thickness');
  Assert.IsTrue(TSkFontMetricsFlag.UnderlinePositionIsValid in LMetrics.Flags, 'The asset font declares an underline position');
end;

procedure TSkFontTests.TestMetricsScaleWithSize;
var
  LFont: ISkFont;
  LMetrics1: TSkFontMetrics;
  LMetrics2: TSkFontMetrics;
begin
  LFont := CreateFont(20);
  LFont.GetMetrics(LMetrics1);
  LFont.Size := 40;
  LFont.GetMetrics(LMetrics2);
  Assert.AreEqual(LMetrics1.Ascent * 2, LMetrics2.Ascent, TEpsilon.Position, '(Ascent)');
  Assert.AreEqual(LMetrics1.Descent * 2, LMetrics2.Descent, TEpsilon.Position, '(Descent)');
  Assert.AreEqual(LMetrics1.XHeight * 2, LMetrics2.XHeight, TEpsilon.Position, '(XHeight)');
  Assert.AreEqual(LMetrics1.CapHeight * 2, LMetrics2.CapHeight, TEpsilon.Position, '(CapHeight)');
end;

procedure TSkFontTests.TestScaleXScalesMeasure;
var
  LFont: ISkFont;
  LWidth: Single;
begin
  LFont := CreateFont(30);
  LWidth := LFont.MeasureText('Skia');
  LFont.ScaleX := 2;
  Assert.AreEqual(LWidth * 2, LFont.MeasureText('Skia'), TEpsilon.Position, 'ScaleX should scale the horizontal advance');
end;

procedure TSkFontTests.TestSingleProperties(const APropertyName: string);

  procedure CheckRoundTrip(const AFont: ISkFont; const AValue: Single);
  begin
    if APropertyName = 'Size' then
    begin
      AFont.Size := AValue;
      Assert.AreEqual(AValue, AFont.Size, TEpsilon.FontSize);
    end
    else if APropertyName = 'ScaleX' then
    begin
      AFont.ScaleX := AValue;
      Assert.AreEqual(AValue, AFont.ScaleX, TEpsilon.Scale);
    end
    else
    begin
      AFont.SkewX := AValue;
      Assert.AreEqual(AValue, AFont.SkewX, TEpsilon.Scale);
    end;
  end;

var
  LFont: ISkFont;
begin
  LFont := TSkFont.Create;
  CheckRoundTrip(LFont, 0.5);
  CheckRoundTrip(LFont, 13.75);
end;

procedure TSkFontTests.TestSizeScalesMeasure;
var
  LFont: ISkFont;
  LWidth: Single;
begin
  LFont := CreateFont(15);
  LWidth := LFont.MeasureText('Skia');
  LFont.Size := 30;
  Assert.AreEqual(LWidth * 2, LFont.MeasureText('Skia'), TEpsilon.Position, 'The advance should scale linearly with the size');
end;

procedure TSkFontTests.TestStateProperties;
var
  LFont: ISkFont;
begin
  LFont := TSkFont.Create;

  LFont.Edging := TSkFontEdging.Alias;
  Assert.IsTrue(LFont.Edging = TSkFontEdging.Alias, '(Edging Alias)');
  LFont.Edging := TSkFontEdging.SubpixelAntiAlias;
  Assert.IsTrue(LFont.Edging = TSkFontEdging.SubpixelAntiAlias, '(Edging SubpixelAntiAlias)');

  LFont.Hinting := TSkFontHinting.Full;
  Assert.IsTrue(LFont.Hinting = TSkFontHinting.Full, '(Hinting Full)');
  LFont.Hinting := TSkFontHinting.Slight;
  Assert.IsTrue(LFont.Hinting = TSkFontHinting.Slight, '(Hinting Slight)');

  LFont.BaselineSnap := False;
  Assert.IsFalse(LFont.BaselineSnap, '(BaselineSnap)');
  LFont.EmbeddedBitmaps := True;
  Assert.IsTrue(LFont.EmbeddedBitmaps, '(EmbeddedBitmaps)');
  LFont.Embolden := True;
  Assert.IsTrue(LFont.Embolden, '(Embolden)');
  LFont.ForceAutoHinting := True;
  Assert.IsTrue(LFont.ForceAutoHinting, '(ForceAutoHinting)');
  LFont.LinearMetrics := True;
  Assert.IsTrue(LFont.LinearMetrics, '(LinearMetrics)');
  LFont.Subpixel := True;
  Assert.IsTrue(LFont.Subpixel, '(Subpixel)');

  LFont.BaselineSnap := True;
  Assert.IsTrue(LFont.BaselineSnap, '(BaselineSnap back)');
  LFont.EmbeddedBitmaps := False;
  Assert.IsFalse(LFont.EmbeddedBitmaps, '(EmbeddedBitmaps back)');
  LFont.Embolden := False;
  Assert.IsFalse(LFont.Embolden, '(Embolden back)');
  LFont.ForceAutoHinting := False;
  Assert.IsFalse(LFont.ForceAutoHinting, '(ForceAutoHinting back)');
  LFont.LinearMetrics := False;
  Assert.IsFalse(LFont.LinearMetrics, '(LinearMetrics back)');
  LFont.Subpixel := False;
  Assert.IsFalse(LFont.Subpixel, '(Subpixel back)');
end;

procedure TSkFontTests.TestTypeface;
var
  LFont: ISkFont;
  LTypeface: ISkTypeface;
begin
  LTypeface := AssetTypeface;
  LFont := TSkFont.Create(nil, 20);
  Assert.AreEqual(TSkTypeface.MakeDefault.FamilyName, LFont.Typeface.FamilyName, 'A font created without a typeface should use the default one');

  LFont.Typeface := LTypeface;
  Assert.IsNotNull(LFont.Typeface, 'The assigned typeface should be kept');
  Assert.AreEqual(LTypeface.FamilyName, LFont.Typeface.FamilyName);
  Assert.AreEqual(LTypeface.FamilyName, LFont.GetTypefaceOrDefault.FamilyName);
  Assert.AreNotEqual<Word>(0, LFont.UnicharToGlyph(Ord('A')), 'The assigned typeface should be used to map glyphs');

  LFont := TSkFont.Create(LTypeface, 20);
  Assert.AreEqual(LTypeface.FamilyName, LFont.Typeface.FamilyName, 'The typeface passed to the constructor should be kept');
end;

// TODO: Investigate possible issue.
//
// TSkFont.Create(nil) falls back to the default typeface, but assigning nil to
// the property later installs Skia's empty typeface instead: the family name
// becomes blank, every glyph maps to notdef and MeasureText answers zero. On
// top of that GetTypefaceOrDefault returns that same empty typeface, so the
// fallback the name promises no longer happens.
//
// procedure TSkFontTests.TestTypefaceSetToNil;
// var
//   LFont: ISkFont;
// begin
//   LFont := TSkFont.Create(AssetTypeface, 20);
//   LFont.Typeface := nil;
//   Assert.AreEqual(TSkTypeface.MakeDefault.FamilyName, LFont.GetTypefaceOrDefault.FamilyName,
//     'TypefaceOrDefault should fall back to the default typeface');
//   Assert.AreNotEqual<Word>(0, LFont.UnicharToGlyph(Ord('A')), 'The fallback typeface should map glyphs');
// end;

procedure TSkFontTests.TestUnicharToGlyph;
var
  LFont: ISkFont;
begin
  LFont := CreateFont;
  Assert.AreEqual(LFont.GetGlyphs('A')[0], LFont.UnicharToGlyph(Ord('A')), 'GetGlyphs and UnicharToGlyph should agree');
  Assert.AreNotEqual<Word>(0, LFont.UnicharToGlyph(Ord('A')), '"A" should be mapped');
  Assert.AreEqual<Word>(0, LFont.UnicharToGlyph($10FFFF), 'An unsupported codepoint should map to the notdef glyph');
end;

procedure TSkFontTests.TestUnicharsToGlyphs;
var
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
begin
  LFont := CreateFont;
  LGlyphs := LFont.UnicharsToGlyphs([Ord('S'), Ord('k')]);
  Assert.AreEqual<NativeInt>(2, Length(LGlyphs));
  Assert.AreEqual(LFont.UnicharToGlyph(Ord('S')), LGlyphs[0]);
  Assert.AreEqual(LFont.UnicharToGlyph(Ord('k')), LGlyphs[1]);
  Assert.AreEqualArray<Word>(LFont.GetGlyphs('Sk'), LGlyphs, 'GetGlyphs and UnicharsToGlyphs should agree');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkFontTests);
end.
