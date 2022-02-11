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
unit Skia.Tests.Paragraph;

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
  { TSkParagraphTests }

  [TestFixture]
  TSkParagraphTests = class(TTestBase)
  public
    {$IFDEF MACOS}
    [TestCase('Simple Paragraph To Path', '0.99,AQEBAX3///9/+fFhf////3/5+3H/////////8/////+3////ABMAA//z//9+7wABCID///////8')]
    {$ELSEIF DEFINED(ANDROID)}
    [TestCase('Simple Paragraph To Path', '0.99,AAEBAX3///9/+XFhf////3/5cWH///////9/8//////3////ABMAA/9z//9+5wABAAD///////8')]
    {$ELSE}
    [TestCase('Simple Paragraph To Path', '0.99,AAEBAX3///9/+XFhf//////5ceF///////9/43////+3////ABMAA/9z//9+5wABAAD///////8')]
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
