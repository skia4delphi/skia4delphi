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
unit Sample.Form.Base.Styled.XE7;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, FMX.Controls, FMX.Layouts, FMX.StdCtrls, FMX.Objects,
  FMX.Types, FMX.Controls.Presentation,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmStyledBase = class(TfrmBase)
    stbStyle: TStyleBook;
    procedure FormShow(Sender: TObject);
  end;

implementation

{$R *.fmx}

procedure TfrmStyledBase.FormShow(Sender: TObject);
begin
  inherited;
  stbStyle.FileName := '..\..\..\Windows11.XE7.style';
end;

end.
