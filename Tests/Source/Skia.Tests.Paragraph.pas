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
unit Skia.Tests.Paragraph;

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
  { TSkParagraphTests }

  [TestFixture]
  TSkParagraphTests = class(TTestBase)
  strict private
    function BuildParagraph(const AText: string; const AWidth: Single; const AMaxLines: NativeUInt = 0): ISkParagraph;
  public
    [Test]
    procedure TestBaselines;
    [Test]
    procedure TestDidExceedMaxLines;
    [Test]
    procedure TestGetGlyphPositionAtCoordinate;
    [Test]
    procedure TestGetRectsForRange;
    [Test]
    procedure TestGetWordBoundary;
    [Test]
    procedure TestIntrinsicWidths;
    [Test]
    procedure TestLineMetrics;
    [Test]
    procedure TestPlaceholder;
    [Test]
    procedure TestPopStyle;
    [Test]
    procedure TestToPath;
    [Test]
    procedure TestVisit;
    {$IF defined(IOS)}
    [TestCase('Simple Paragraph To Path', '0.99,GQEBAW19////+fHh7//////58fHv////////8+/////3////AAMAA/9z//9+7wABAAD///////8')]
    {$ELSEIF defined(MACOS)}
    [TestCase('Simple Paragraph To Path', '0.99,GQEBAX3/////+fHh///////5+/H/////////8/////+3////ABMAA//z//9+7wABCID///////8')]
    {$ELSEIF defined(ANDROID)}
    [TestCase('Simple Paragraph To Path', '0.99,GQEBAW39////+fHh7//////58eH/////////4/////+3////ABMAA/9z//9+5wABAAD///////8')]
    {$ELSE}
    [TestCase('Simple Paragraph To Path', '0.98,GQEBAW39////+fHh7//////58eH/////////4/////+3////ABMAA/9z//9+5wABAAD///////8')]
    {$ENDIF}
    procedure TestParagraphToPath(const AMinSimilarity: Double; const AExpectedHash: string);
    [Test]
    procedure TestTextStyleGetAndSetFontFamilies;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Math.Vectors,
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkParagraphTests }

function TSkParagraphTests.BuildParagraph(const AText: string; const AWidth: Single;
  const AMaxLines: NativeUInt): ISkParagraph;
var
  LParagraphBuilder: ISkParagraphBuilder;
  LParagraphStyle: ISkParagraphStyle;
  LTextStyle: ISkTextStyle;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  if AMaxLines > 0 then
    LParagraphStyle.MaxLines := AMaxLines;
  LParagraphBuilder := TSkParagraphBuilder.Create(LParagraphStyle, FontProvider);

  LTextStyle := TSkTextStyle.Create;
  LTextStyle.Color := TAlphaColors.Black;
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontSize(20);
  LParagraphBuilder.PushStyle(LTextStyle);
  LParagraphBuilder.AddText(AText);

  Result := LParagraphBuilder.Build;
  Assert.IsNotNull(Result, 'Invalid ISkParagraph (nil)');
  Result.Layout(AWidth);
end;

procedure TSkParagraphTests.TestBaselines;
var
  LParagraph: ISkParagraph;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  Assert.IsTrue(LParagraph.AlphabeticBaseline > 0, 'The alphabetic baseline should be below the top of the line');
  Assert.IsTrue(LParagraph.AlphabeticBaseline < LParagraph.Height, 'The alphabetic baseline should be inside the paragraph');
  Assert.IsTrue(LParagraph.IdeographicBaseline >= LParagraph.AlphabeticBaseline,
    'The ideographic baseline should not be above the alphabetic one');
end;

procedure TSkParagraphTests.TestDidExceedMaxLines;
begin
  Assert.IsFalse(BuildParagraph('Hello world', 400).DidExceedMaxLines, 'A single line paragraph should fit');
  Assert.IsTrue(BuildParagraph('Hello world, this is a long text that needs more than one line', 100, 1).DidExceedMaxLines,
    'A text longer than the line limit should exceed it');
end;

procedure TSkParagraphTests.TestGetGlyphPositionAtCoordinate;
var
  LParagraph: ISkParagraph;
  LPositionAffinity: TSkPositionAffinity;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  LPositionAffinity := LParagraph.GetGlyphPositionAtCoordinate(0, 0);
  Assert.AreEqual(0, LPositionAffinity.Position, 'The first coordinate should map to the first glyph');
  Assert.IsTrue(LPositionAffinity.Affinity = TSkAffinity.Downstream, 'The first glyph should be downstream');

  LPositionAffinity := LParagraph.GetGlyphPositionAtCoordinate(LParagraph.LongestLine, 0);
  Assert.AreEqual(Length('Hello world'), LPositionAffinity.Position, 'The end of the line should map to the last glyph');
end;

procedure TSkParagraphTests.TestGetRectsForRange;
var
  LParagraph: ISkParagraph;
  LTextBoxes: TArray<TSkTextBox>;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  LTextBoxes := LParagraph.GetRectsForRange(0, 5, TSkRectHeightStyle.Tight, TSkRectWidthStyle.Tight);
  Assert.AreEqual<NativeInt>(1, Length(LTextBoxes), 'A single line range should produce a single box');
  Assert.IsTrue(LTextBoxes[0].Direction = TSkTextDirection.LeftToRight, '(Direction)');
  Assert.AreEqual(0.0, LTextBoxes[0].Rect.Left, TEpsilon.Position, 'The range should start at the left of the paragraph');
  Assert.IsTrue(LTextBoxes[0].Rect.Width > 0, 'The box should have a width');
  Assert.IsTrue(LTextBoxes[0].Rect.Right < LParagraph.LongestLine, 'Half of the text should be narrower than the whole line');

  Assert.IsTrue(LTextBoxes[0].Rect.Height <=
    LParagraph.GetRectsForRange(0, 5, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight)[0].Rect.Height,
    'The tight height style should not be taller than the max one');

  Assert.AreEqual<NativeInt>(0, Length(LParagraph.GetRectsForRange(0, 0, TSkRectHeightStyle.Tight, TSkRectWidthStyle.Tight)),
    'An empty range should not produce boxes');
end;

procedure TSkParagraphTests.TestGetWordBoundary;
var
  LEnd: Cardinal;
  LParagraph: ISkParagraph;
  LStart: Cardinal;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  LParagraph.GetWordBoundary(2, LStart, LEnd);
  Assert.AreEqual<Cardinal>(0, LStart, 'The first word should start at the beginning of the text');
  Assert.AreEqual<Cardinal>(5, LEnd, 'The first word should end before the space');

  LParagraph.GetWordBoundary(8, LStart, LEnd);
  Assert.AreEqual<Cardinal>(6, LStart, 'The second word should start after the space');
  Assert.AreEqual<Cardinal>(11, LEnd, 'The second word should end at the end of the text');
end;

procedure TSkParagraphTests.TestIntrinsicWidths;
var
  LParagraph: ISkParagraph;
begin
  LParagraph := BuildParagraph('Hello world, this is a long text that needs more than one line', 100);
  Assert.AreEqual(100.0, LParagraph.MaxWidth, TEpsilon.Position, 'The max width should be the layout width');
  Assert.IsTrue(LParagraph.LongestLine > 0, 'The longest line should have a width');
  Assert.IsTrue(LParagraph.LongestLine <= LParagraph.MaxWidth, 'No line should be wider than the layout width');
  Assert.IsTrue(LParagraph.MinIntrinsicWidth > 0, 'The widest word should have a width');
  Assert.IsTrue(LParagraph.MinIntrinsicWidth <= LParagraph.MaxIntrinsicWidth,
    'The widest word should not be wider than the whole text');
  Assert.IsTrue(LParagraph.MaxIntrinsicWidth > LParagraph.MaxWidth,
    'The unbroken text should be wider than the layout width');
end;

procedure TSkParagraphTests.TestLineMetrics;
var
  LMetrics: TArray<TSkMetrics>;
  LParagraph: ISkParagraph;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  LMetrics := LParagraph.LineMetrics;
  Assert.AreEqual<NativeInt>(1, Length(LMetrics), 'The text should fit in a single line');
  Assert.AreEqual<NativeUInt>(0, LMetrics[0].StartIndex, '(StartIndex)');
  Assert.AreEqual<NativeUInt>(Length('Hello world'), LMetrics[0].EndIndex, '(EndIndex)');
  Assert.AreEqual<NativeUInt>(0, LMetrics[0].LineNumber, '(LineNumber)');
  Assert.IsTrue(LMetrics[0].IsHardBreak, 'The last line ends with a hard break');
  Assert.IsTrue(LMetrics[0].Height > 0, '(Height)');
  Assert.IsTrue(LMetrics[0].Width > 0, '(Width)');
  Assert.IsTrue(LMetrics[0].Ascent > 0, 'The ascent should be the distance above the baseline');
  Assert.IsTrue(LMetrics[0].Descent > 0, 'The descent should be the distance below the baseline');
  Assert.IsTrue(LMetrics[0] = LParagraph.GetLineMetrics[0], 'The line metrics should be stable');

  LParagraph := BuildParagraph('Hello world, this is a long text that needs more than one line', 100);
  Assert.IsTrue(Length(LParagraph.LineMetrics) > 1, 'The text should need more than one line');
  Assert.IsFalse(LParagraph.LineMetrics[0].IsHardBreak, 'A soft wrapped line is not a hard break');
end;

procedure TSkParagraphTests.TestPlaceholder;
var
  LParagraph: ISkParagraph;
  LParagraphBuilder: ISkParagraphBuilder;
  LParagraphStyle: ISkParagraphStyle;
  LTextBoxes: TArray<TSkTextBox>;
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontSize(20);

  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphBuilder := TSkParagraphBuilder.Create(LParagraphStyle, FontProvider);
  LParagraphBuilder.PushStyle(LTextStyle);
  LParagraphBuilder.AddText('Hello ');
  LParagraphBuilder.AddPlaceholder(TSkPlaceholderStyle.Create(40, 30, TSkPlaceholderAlignment.Baseline,
    TSkTextBaseline.Alphabetic, 0));
  LParagraphBuilder.AddText(' world');

  LParagraph := LParagraphBuilder.Build;
  LParagraph.Layout(400);

  LTextBoxes := LParagraph.GetRectsForPlaceholders;
  Assert.AreEqual<NativeInt>(1, Length(LTextBoxes), 'The paragraph should hold a single placeholder');
  Assert.AreEqual(40.0, LTextBoxes[0].Rect.Width, TEpsilon.Position, '(Width)');
  Assert.AreEqual(30.0, LTextBoxes[0].Rect.Height, TEpsilon.Position, '(Height)');
  Assert.IsTrue(LTextBoxes[0].Rect.Left > 0, 'The placeholder should be placed after the first word');
end;

procedure TSkParagraphTests.TestPopStyle;
var
  LParagraph: ISkParagraph;
  LParagraphBuilder: ISkParagraphBuilder;
  LParagraphStyle: ISkParagraphStyle;
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontSize(20);

  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphBuilder := TSkParagraphBuilder.Create(LParagraphStyle, FontProvider);
  LParagraphBuilder.PushStyle(LTextStyle);
  LParagraphBuilder.AddText('Hello');

  LTextStyle := TSkTextStyle.Create;
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontSize(40);
  LParagraphBuilder.PushStyle(LTextStyle);
  LParagraphBuilder.AddText('Hello');
  LParagraphBuilder.Pop;
  LParagraphBuilder.AddText('Hello');

  LParagraph := LParagraphBuilder.Build;
  LParagraph.Layout(1000);
  Assert.AreEqual<NativeInt>(3, Length(LParagraph.GetRectsForRange(0, 15, TSkRectHeightStyle.Tight, TSkRectWidthStyle.Tight)),
    'Pop should restore the previous style, producing three differently sized runs');
end;

procedure TSkParagraphTests.TestToPath;
var
  LParagraph: ISkParagraph;
  LPath: ISkPath;
begin
  LParagraph := BuildParagraph('Hello world', 400);
  LPath := LParagraph.ToPath;
  Assert.IsNotNull(LPath, 'Invalid ISkPath (nil)');
  Assert.IsFalse(LPath.IsEmpty, 'The text outline should not be empty');
  Assert.IsTrue(LPath.Bounds.Width > 0, 'The text outline should have a width');
  Assert.IsTrue(LPath.Bounds.Width <= LParagraph.LongestLine + 1, 'The outline should not be wider than the line');
end;

procedure TSkParagraphTests.TestVisit;
var
  LGlyphCount: Integer;
  LParagraph: ISkParagraph;
  LRunCount: Integer;
begin
  LGlyphCount := 0;
  LRunCount := 0;
  LParagraph := BuildParagraph('Hello world', 400);
  LParagraph.Visit(
    procedure (const ALineNumber: Integer; const AInfo: TSkParagraphVisitorInfo)
    begin
      Assert.AreEqual(0, ALineNumber, 'The text should fit in a single line');
      Assert.IsNotNull(AInfo.Font, 'Invalid ISkFont (nil)');
      Assert.IsTrue(AInfo.AdvanceX > 0, '(AdvanceX)');
      Assert.IsTrue(AInfo.Glyphs[0] > 0, 'The first glyph should be mapped');
      Assert.AreEqual<Cardinal>(0, AInfo.Utf8Starts[LRunCount], 'The run should start at the beginning of the text');
      Assert.AreEqual(0.0, AInfo.Positions[0].X, TEpsilon.Position, 'The first glyph should be at the left of the line');
      Inc(LGlyphCount, AInfo.Count);
      Inc(LRunCount);
    end);
  Assert.AreEqual(1, LRunCount, 'A single styled text should produce a single run');
  Assert.AreEqual(Length('Hello world'), LGlyphCount, 'Every character should be visited');
end;

procedure TSkParagraphTests.TestTextStyleGetAndSetFontFamilies;
var
  LTextStyle: ISkTextStyle;
begin
  LTextStyle := TSkTextStyle.Create;
  Assert.IsTrue(Length(LTextStyle.FontFamilies) = 1, 'in test 1');

  LTextStyle.FontFamilies := TArray<string>.Create('Arial', 'Calibri');
  Assert.IsTrue(Length(LTextStyle.FontFamilies) = 2);
  Assert.AreEqual('Arial', LTextStyle.FontFamilies[0], 'in test 2');
  Assert.AreEqual('Calibri', LTextStyle.FontFamilies[1], 'in test 3');

  LTextStyle.FontFamilies := TArray<string>.Create('Arial');
  Assert.IsTrue(Length(LTextStyle.FontFamilies) = 1, 'in test 4');
  Assert.AreEqual('Arial', LTextStyle.FontFamilies[0], 'in test 5');

  LTextStyle.FontFamilies := TArray<string>.Create('');
  Assert.IsTrue(Length(LTextStyle.FontFamilies) = 1, 'in test 6');
  Assert.AreEqual('', LTextStyle.FontFamilies[0], 'in test 7');

  LTextStyle.FontFamilies := nil;
  Assert.IsNull(LTextStyle.FontFamilies, 'in test 8');
end;

procedure TSkParagraphTests.TestParagraphToPath(const AMinSimilarity: Double; const AExpectedHash: string);
var
  LParagraph: ISkParagraph;
  LBuilder: ISkParagraphBuilder;
  LTextStyle: ISkTextStyle;
  LParagraphStyle: ISkParagraphStyle;
  LSurface: ISkSurface;
begin
  LParagraphStyle := TSkParagraphStyle.Create;
  LParagraphStyle.MaxLines := 3;
  LParagraphStyle.Ellipsis := '...';
  LBuilder := TSkParagraphBuilder.Create(LParagraphStyle, FontProvider);

  LTextStyle := TSkTextStyle.Create;
  LTextStyle.Color := TAlphaColors.Black;
  LTextStyle.SetFontSize(28);
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Upright));
  LBuilder.PushStyle(LTextStyle);
  LBuilder.AddText('English English ');

  LTextStyle := TSkTextStyle.Create;
  LTextStyle.Color := TAlphaColors.Crimson;
  LTextStyle.SetFontSize(22);
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Upright));
  LTextStyle.Decorations := [TSkTextDecoration.Underline];
  LBuilder.PushStyle(LTextStyle);
  LBuilder.AddText('Hello world!');

  LTextStyle := TSkTextStyle.Create;
  LTextStyle.Color := TAlphaColors.Blueviolet;
  LTextStyle.SetFontSize(30);
  LTextStyle.FontFamilies := TArray<string>.Create(DefaultFontFamily);
  LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.Bold, TSkFontWidth.Normal, TSkFontSlant.Upright));
  LBuilder.PushStyle(LTextStyle);
  LBuilder.AddText(' World domination is such an ugly phrase - I prefer to call it world optimisation.');

  LParagraph := LBuilder.Build;
  LParagraph.Layout(440);

  LSurface := TSkSurface.MakeRaster(440, 140);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LParagraph.Paint(LSurface.Canvas, 0, 0);
  Assert.AreSimilar(AExpectedHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkParagraphTests);
end.
