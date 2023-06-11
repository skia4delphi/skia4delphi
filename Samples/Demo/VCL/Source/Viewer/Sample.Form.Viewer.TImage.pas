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
unit Sample.Form.Viewer.TImage;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TImageEditProc = reference to procedure(const AImage: TImage);

  { TfrmTImageViewer }

  TfrmTImageViewer = class(TfrmBaseViewer)
    imgImage: TImage;
  public
    procedure Show(const ATitle, ADescription: string; const AImageEditProc: TImageEditProc); reintroduce;
  end;

implementation

{$R *.dfm}

{ TfrmTImageViewer }

procedure TfrmTImageViewer.Show(const ATitle, ADescription: string;
  const AImageEditProc: TImageEditProc);
begin
  if Assigned(AImageEditProc) then
  begin
    AImageEditProc(imgImage);
    imgImage.Width := imgImage.Picture.Width;
    imgImage.Height := imgImage.Picture.Height;
  end
  else
    imgImage.SetBounds(imgImage.Left, imgImage.Top, 0, 0);
  inherited Show(ATitle, ADescription);
end;

end.
