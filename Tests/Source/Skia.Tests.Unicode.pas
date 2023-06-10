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
unit Skia.Tests.Unicode;

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
  { TSkUnicodeTests }

  [TestFixture]
  TSkUnicodeTests = class(TTestBase)
  public
    [Test]
    procedure TestBiDiRegion;
    [Test]
    procedure TestCodePoint;
    [Test]
    procedure TestGraphemes;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.IOUtils;

{ TSkUnicodeTests }

function StringHexadecimal(const AText: string): string;
var
  LChar: Char;
begin
  Result := '';
  for LChar in AText do
  begin
    if not Result.IsEmpty then
      Result := Result + ', ';
    Result := Result + '$' + InttoHex(Ord(LChar), 4);
  end;
end;

procedure TSkUnicodeTests.TestBiDiRegion;
type
  TBiDiRegion = record
    Start: Integer;
    &End: Integer;
    Level: Byte;
  end;
const
  Text: string = 'Testing... سلام دنیا! Hello';
  Expected: array[0..1] of TBiDiRegion = (
    (Start: 11; &End: 11; Level: 0),
    (Start: 20; &End: 20; Level: 1));
var
  LUnicode: ISkUnicode;
  LIndexValue: Integer;
  LIndex: PInteger;
begin
  LIndexValue := 0;
  LIndex := @LIndexValue;
  LUnicode := TSkUnicode.Create;
  LUnicode.ForEachBidiRegion(Text, TSkDirection.LeftToRight,
    procedure (const AStart, AEnd: Integer; const ALevel: Byte)
    begin
      Assert.AreEqual(Expected[LIndex^].Start, AStart);
      Assert.AreEqual(Expected[LIndex^].&End, AEnd);
      Assert.AreEqual(Expected[LIndex^].Level, ALevel);
      Inc(LIndex^);
    end);
end;

procedure TSkUnicodeTests.TestCodePoint;
type
  TCodePoint = record
    Unichar: Integer;
    Start: Integer;
    &End: Integer;
  end;
const
  Text: string = 'Hello 🖐🏻';
  Expected: array[0..7] of TCodePoint = (
    (Unichar: $00000048; Start: 0; &End: 1),
    (Unichar: $00000065; Start: 1; &End: 2),
    (Unichar: $0000006C; Start: 2; &End: 3),
    (Unichar: $0000006C; Start: 3; &End: 4),
    (Unichar: $0000006F; Start: 4; &End: 5),
    (Unichar: $00000020; Start: 5; &End: 6),
    (Unichar: $0001F590; Start: 6; &End: 8),
    (Unichar: $0001F3FB; Start: 8; &End: 10));
var
  LUnicode: ISkUnicode;
  LIndexValue: Integer;
  LIndex: PInteger;
begin
  LIndexValue := 0;
  LIndex := @LIndexValue;
  LUnicode := TSkUnicode.Create;
  LUnicode.ForEachCodepoint(Text,
    procedure (const AUnichar, AStart, AEnd: Integer)
    begin
      Assert.AreEqual(Expected[LIndex^].Unichar, AUnichar);
      Assert.AreEqual(Expected[LIndex^].Start, AStart);
      Assert.AreEqual(Expected[LIndex^].&End, AEnd);
      Inc(LIndex^);
    end);
end;

procedure TSkUnicodeTests.TestGraphemes;
const
  Text: string = 'Hi! ✋🏻🙏🏻🙋🏻‍♂️';
  GraphemesHex: array[0..6] of string = ('$0048', '$0069', '$0021', '$0020', '$270B, $D83C, $DFFB', '$D83D, $DE4F, $D83C, $DFFB', '$D83D, $DE4B, $D83C, $DFFB, $200D, $2642, $FE0F');
var
  LUnicode: ISkUnicode;
  LGraphemeIndex: Integer;
  LGrapheme: string;
begin
  LGraphemeIndex := 0;
  LUnicode := TSkUnicode.Create;
  for LGrapheme in LUnicode.GetBreaks(Text, TSkBreakType.Graphemes) do
  begin
    Assert.AreEqual(GraphemesHex[LGraphemeIndex], StringHexadecimal(LGrapheme));
    Inc(LGraphemeIndex);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkUnicodeTests);
end.

