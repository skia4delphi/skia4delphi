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
  public
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
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkParagraphTests }

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
