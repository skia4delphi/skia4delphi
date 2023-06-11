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
unit Sample.Form.Unicode;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmUnicode = class(TfrmBase)
    btnGraphemeIterator: TSpeedButton;
    lblGraphemeIteratorTitle: TSkLabel;
    lblGraphemeIteratorDescription: TSkLabel;
    btnBiDiIterator: TSpeedButton;
    lblBiDiIteratorTitle: TSkLabel;
    lblBiDiIteratorDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnBiDiIteratorClick(Sender: TObject);
    procedure btnGraphemeIteratorClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Unicode.Graphemes,
  Sample.Form.Viewer.Unicode.BiDi;

{$R *.fmx}

procedure AddBiDiRegion(var ABiDiRegionDescription: string; const AText: string;
  const AStartIndex, AEndIndex: Integer; const AIsRTL: Boolean);
const
  BiDiKind: array[Boolean] of string = ('Left-to-Right', 'Right-to-Left');
var
  LRegionText: string;
begin
  LRegionText := AText.Substring(AStartIndex, AEndIndex - AStartIndex);
  ABiDiRegionDescription := ABiDiRegionDescription + sLineBreak + Format('  "%s"  - %s', [LRegionText, BiDiKind[AIsRTL]]);
end;

procedure TfrmUnicode.btnBiDiIteratorClick(Sender: TObject);
const
  Text = 'سلام دنیا! Hello Word!';
var
  LUnicode: ISkUnicode;
  LBiDiRegionDescription: string;
  LStartIndexValue: Integer;
  LStartIndex: PInteger;
begin
  LBiDiRegionDescription := '';
  LStartIndexValue := 0;
  LStartIndex := @LStartIndexValue;

  LUnicode := TSkUnicode.Create;
  LUnicode.ForEachBidiRegion(Text, TSkDirection.LeftToRight,
    procedure (const AStart, AEnd: Integer; const ALevel: Byte)
    begin
      AddBiDiRegion(LBiDiRegionDescription, Text, LStartIndex^, AEnd, ALevel = 1);
      LStartIndex^ := AEnd;
    end);
  if LStartIndexValue < Length(Text) then
    AddBiDiRegion(LBiDiRegionDescription, Text, LStartIndexValue, Length(Text), False);

  ChildForm<TfrmUnicodeBiDiViewer>.Show('BiDi Regions Iterator',
    'Iterates over regions of text based on their direction.',
    Text, LBiDiRegionDescription);
end;

function StringHexadecimal(const AText: string): string;
var
  LChar: Char;
begin
  Result := '';
  for LChar in AText do
  begin
    if not Result.IsEmpty then
      Result := Result + ' ';
    Result := Result + '$' + IntToHex(Ord(LChar), 4);
  end;
end;

procedure TfrmUnicode.btnGraphemeIteratorClick(Sender: TObject);
const
  Text: string = 'Hi! ✋🏻🙏🏻🙋🏻‍♂️';
var
  LUnicode: ISkUnicode;
  LGrapheme: string;
  LGraphemesDescription: string;
begin
  LGraphemesDescription := '';
  LUnicode := TSkUnicode.Create;
  for LGrapheme in LUnicode.GetBreaks(Text, TSkBreakType.Graphemes) do
  begin
    LGraphemesDescription := LGraphemesDescription + sLineBreak +
      Format('  %s  - %d Char - %s', [LGrapheme, Length(LGrapheme), StringHexadecimal(LGrapheme)]);
  end;
  ChildForm<TfrmUnicodeGraphemesViewer>.Show('Graphemes Iterator', 'Grapheme is the single displayed character (like one emoji, one letter).',
    Text, LGraphemesDescription);
end;

end.
