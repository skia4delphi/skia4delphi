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
unit Sample.Form.Viewer.Unicode.Graphemes;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TfrmUnicodeGraphemesViewer = class(TfrmBaseViewer)
    lblResult: TSkLabel;
  public
    procedure Show(const ATitle, ADescription, AText, AGraphemesDescription: string); reintroduce;
  end;

implementation

{$R *.fmx}

procedure TfrmUnicodeGraphemesViewer.Show(const ATitle, ADescription, AText,
  AGraphemesDescription: string);
begin
  lblResult.Words.ItemByName['Text'].Text      := '"' + AText + '"';
  lblResult.Words.ItemByName['Length'].Text    := Length(AText).ToString;
  lblResult.Words.ItemByName['Graphemes'].Text := AGraphemesDescription;
  BackgroundKind := TBackgroundKind.Solid;
  inherited Show(ATitle, ADescription);
end;

end.
