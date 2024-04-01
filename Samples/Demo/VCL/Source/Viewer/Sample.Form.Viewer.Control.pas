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
unit Sample.Form.Viewer.Control;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TfrmControlViewer = class(TfrmBaseViewer)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FControl: TControl;
    FControlCreationFunc: TFunc<TControl>;
  protected
    procedure OptionsChanged; override;
  public
    procedure Show(const ATitle, ADescription: string; const AControlCreationFunc: TFunc<TControl>; ABackgroundKind: TBackgroundKind = TBackgroundKind.Chess; const AOptions: IViewerOptions = nil); reintroduce;
  end;

implementation

{$R *.dfm}

procedure TfrmControlViewer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  if Action <> TCloseAction.caNone then
    FreeAndNil(FControl);
end;

procedure TfrmControlViewer.OptionsChanged;
begin
  inherited;
  FreeAndNil(FControl);
  FControl := FControlCreationFunc();
  FControl.Parent := sbxContent;
  if FControl.Align <> TAlign.alNone then
    sbxContent.Realign;
end;

procedure TfrmControlViewer.Show(const ATitle, ADescription: string;
  const AControlCreationFunc: TFunc<TControl>; ABackgroundKind: TBackgroundKind;
  const AOptions: IViewerOptions);
begin
  BackgroundKind := ABackgroundKind;
  Options := AOptions;
  FControlCreationFunc := AControlCreationFunc;
  FControl := FControlCreationFunc();
  FControl.Parent := sbxContent;
  inherited Show(ATitle, ADescription);
end;

end.
