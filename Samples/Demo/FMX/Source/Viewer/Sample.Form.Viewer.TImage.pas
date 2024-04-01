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
unit Sample.Form.Viewer.TImage;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.Classes, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

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

{$R *.fmx}

{ TfrmTImageViewer }

procedure TfrmTImageViewer.Show(const ATitle, ADescription: string;
  const AImageEditProc: TImageEditProc);
var
  LSceneScale: Single;
begin
  if Assigned(AImageEditProc) then
  begin
    AImageEditProc(imgImage);
    if Assigned(imgImage.Scene) then
      LSceneScale := imgImage.Scene.GetSceneScale
    else
      LSceneScale := 1;
    imgImage.Width := imgImage.Bitmap.Width / (imgImage.AbsoluteScale.X * LSceneScale);
    imgImage.Height := imgImage.Bitmap.Height / (imgImage.AbsoluteScale.Y * LSceneScale);
  end
  else
    imgImage.SetBounds(imgImage.Position.X, imgImage.Position.Y, 0, 0);
  inherited Show(ATitle, ADescription);
end;

end.
