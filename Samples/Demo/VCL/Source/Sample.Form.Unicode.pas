{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
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
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmUnicode = class(TfrmBase)
    pnlGraphemeIterator: TPanel;
    lblGraphemeIteratorDescription: TSkLabel;
    lblGraphemeIteratorTitle: TSkLabel;
    svgGraphemeIteratorArrow: TSkSvg;
    pnlGraphemeIteratorLine: TPanel;
    pnlBiDiIterator: TPanel;
    lblBiDiIteratorDescription: TSkLabel;
    lblBiDiIteratorTitle: TSkLabel;
    svgBiDiIteratorArrow: TSkSvg;
    pnlBiDiIteratorLine: TPanel;
    procedure pnlBiDiIteratorClick(Sender: TObject);
    procedure pnlGraphemeIteratorClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Unicode.Graphemes,
  Sample.Form.Viewer.Unicode.BiDi;

{$R *.dfm}

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

procedure TfrmUnicode.pnlBiDiIteratorClick(Sender: TObject);
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

procedure TfrmUnicode.pnlGraphemeIteratorClick(Sender: TObject);
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
