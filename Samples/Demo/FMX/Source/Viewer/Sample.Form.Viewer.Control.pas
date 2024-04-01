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
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

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

{$R *.fmx}

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
  AllowScrollBoundsAnimation := not FControl.HitTest;
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
  AllowScrollBoundsAnimation := not FControl.HitTest;
  inherited Show(ATitle, ADescription);
end;

end.
