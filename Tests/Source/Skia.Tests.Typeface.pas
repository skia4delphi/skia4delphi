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
unit Skia.Tests.Typeface;

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
  { TSkTypefaceTests }

  [TestFixture]
  TSkTypefaceTests = class(TTestBase)
  public
    [Test]
    procedure TestFontStyleHelpers;
    [Test]
    procedure TestMakeDefault;
    [Test]
    procedure TestMakeFromFile;
    [Test]
    procedure TestMakeFromFileWithAnInvalidFile;
    [Test]
    procedure TestMakeFromName;
    [Test]
    procedure TestMakeFromStream;
    [TestCase('Regular', 'segoeui.ttf,400,False')]
    [TestCase('Bold',    'segoeuib.ttf,700,True')]
    procedure TestWeight(const AFileName: string; const AWeight: Integer; const AIsBold: Boolean);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Math;

{ TSkTypefaceTests }

procedure TSkTypefaceTests.TestFontStyleHelpers;
begin
  Assert.AreEqual(Integer(TSkFontWeight.Normal), TSkFontStyle.Normal.Weight, '(Normal weight)');
  Assert.IsTrue(TSkFontStyle.Normal.Slant = TSkFontSlant.Upright, '(Normal slant)');
  Assert.AreEqual(Integer(TSkFontWeight.Bold), TSkFontStyle.Bold.Weight, '(Bold weight)');
  Assert.IsTrue(TSkFontStyle.Italic.Slant = TSkFontSlant.Italic, '(Italic slant)');
  Assert.IsTrue(TSkFontStyle.BoldItalic.Slant = TSkFontSlant.Italic, '(BoldItalic slant)');
  Assert.AreEqual(Integer(TSkFontWeight.Bold), TSkFontStyle.BoldItalic.Weight, '(BoldItalic weight)');
  Assert.IsTrue(TSkFontStyle.Normal = TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Upright));
  Assert.IsTrue(TSkFontStyle.Normal <> TSkFontStyle.Bold);
end;

procedure TSkTypefaceTests.TestMakeDefault;
var
  LTypeface: ISkTypeface;
begin
  LTypeface := TSkTypeface.MakeDefault;
  Assert.IsNotNull(LTypeface, 'The default typeface should always exist');
  Assert.IsNotEmpty(LTypeface.FamilyName, 'The default typeface should have a family name');
end;

procedure TSkTypefaceTests.TestMakeFromFile;
var
  LTypeface: ISkTypeface;
begin
  LTypeface := TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf');
  Assert.IsNotNull(LTypeface, 'Invalid ISkTypeface (nil)');
  Assert.AreEqual('Segoe UI', LTypeface.FamilyName, '(FamilyName)');
  Assert.AreEqual(Integer(TSkFontWidth.Normal), LTypeface.Width, '(Width)');
  Assert.IsTrue(LTypeface.Slant = TSkFontSlant.Upright, '(Slant)');
  Assert.IsFalse(LTypeface.IsItalic, '(IsItalic)');
  Assert.AreEqual(LTypeface.Weight, LTypeface.Style.Weight, 'Weight and Style.Weight should agree');
  Assert.AreEqual(LTypeface.Width, LTypeface.Style.Width, 'Width and Style.Width should agree');
  Assert.IsTrue(LTypeface.Slant = LTypeface.Style.Slant, 'Slant and Style.Slant should agree');
end;

procedure TSkTypefaceTests.TestMakeFromFileWithAnInvalidFile;
begin
  Assert.IsNull(TSkTypeface.MakeFromFile(FontAssetsPath + 'this-font-does-not-exist.ttf'), 'A missing file should not create a typeface');
end;

procedure TSkTypefaceTests.TestMakeFromName;
var
  LBold: ISkTypeface;
  LFamilyName: string;
  LTypeface: ISkTypeface;
begin
  // The name has to come from the platform itself: the font manager of each
  // platform decides whether an unknown family falls back to a default face or
  // to no face at all.
  LFamilyName := TSkTypeface.MakeDefault.FamilyName;
  Assert.IsNotEmpty(LFamilyName, 'The default typeface should have a family name');

  LTypeface := TSkTypeface.MakeFromName(LFamilyName, TSkFontStyle.Normal);
  Assert.IsNotNull(LTypeface, 'The default family should be resolvable by name');
  Assert.AreEqual(LFamilyName, LTypeface.FamilyName, '(FamilyName)');
  Assert.IsFalse(LTypeface.IsItalic, 'A normal request should not return an italic face');

  LBold := TSkTypeface.MakeFromName(LFamilyName, TSkFontStyle.Bold);
  Assert.IsNotNull(LBold, 'The bold face should be resolvable by name');
  Assert.IsTrue(LBold.Weight >= LTypeface.Weight, 'A bold request should not return a lighter face');
end;

procedure TSkTypefaceTests.TestMakeFromStream;
var
  LStream: TStream;
  LTypeface: ISkTypeface;
begin
  LStream := TFileStream.Create(FontAssetsPath + 'segoeui.ttf', fmOpenRead or fmShareDenyWrite);
  try
    LTypeface := TSkTypeface.MakeFromStream(LStream);
  finally
    LStream.Free;
  end;
  Assert.IsNotNull(LTypeface, 'Invalid ISkTypeface (nil)');
  Assert.AreEqual('Segoe UI', LTypeface.FamilyName);
end;

procedure TSkTypefaceTests.TestWeight(const AFileName: string;
  const AWeight: Integer; const AIsBold: Boolean);
var
  LTypeface: ISkTypeface;
begin
  LTypeface := TSkTypeface.MakeFromFile(FontAssetsPath + AFileName);
  Assert.IsNotNull(LTypeface, 'Invalid ISkTypeface (nil)');
  Assert.AreEqual(AWeight, LTypeface.Weight, '(Weight)');
  Assert.AreEqual(AIsBold, LTypeface.IsBold, '(IsBold)');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkTypefaceTests);
end.
