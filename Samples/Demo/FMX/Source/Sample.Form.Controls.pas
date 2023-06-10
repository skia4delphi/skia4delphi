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
unit Sample.Form.Controls;

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
  Sample.Form.Base;

type
  TfrmControls = class(TfrmBase)
    btnTSkSVG: TSpeedButton;
    lblTSkSVGTitle: TSkLabel;
    lblTSkSVGDescription: TSkLabel;
    btnTSkPaintBox: TSpeedButton;
    lblTSkPaintBoxTitle: TSkLabel;
    lblTSkPaintBoxDescription: TSkLabel;
    btnTSkLabel: TSpeedButton;
    lblTSkLabelTitle: TSkLabel;
    lblTSkLabelDescription: TSkLabel;
    btnTSkAnimatedImage: TSpeedButton;
    lblTSkAnimatedImageTitle: TSkLabel;
    lblTSkAnimatedImageDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnTSkAnimatedImageClick(Sender: TObject);
    procedure btnTSkLabelClick(Sender: TObject);
    procedure btnTSkPaintBoxClick(Sender: TObject);
    procedure btnTSkSVGClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Controls.TSkAnimatedImage,
  Sample.Form.Controls.TSkLabel,
  Sample.Form.Controls.TSkPaintBox,
  Sample.Form.Controls.TSkSVG;

{$R *.fmx}

procedure TfrmControls.btnTSkAnimatedImageClick(Sender: TObject);
begin
  ChildForm<TfrmTSkAnimatedImage>.Show;
end;

procedure TfrmControls.btnTSkLabelClick(Sender: TObject);
begin
  ChildForm<TfrmTSkLabel>.Show;
end;

procedure TfrmControls.btnTSkPaintBoxClick(Sender: TObject);
begin
  ChildForm<TfrmTSkPaintBox>.Show;
end;

procedure TfrmControls.btnTSkSVGClick(Sender: TObject);
begin
  ChildForm<TfrmTSkSVG>.Show;
end;

end.
