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
unit Sample.Form.Controls;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmControls = class(TfrmBase)
    pnlTSkAnimatedImage: TPanel;
    lblTSkAnimatedImageDescription: TSkLabel;
    lblTSkAnimatedImageTitle: TSkLabel;
    svgTSkAnimatedImageArrow: TSkSvg;
    pnlTSkAnimatedImageLine: TPanel;
    pnlTSkLabel: TPanel;
    lblTSkLabelDescription: TSkLabel;
    lblTSkLabelTitle: TSkLabel;
    svgTSkLabelArrow: TSkSvg;
    pnlTSkLabelLine: TPanel;
    pnlTSkPaintBox: TPanel;
    lblTSkPaintBoxDescription: TSkLabel;
    lblTSkPaintBoxTitle: TSkLabel;
    svgTSkPaintBoxArrow: TSkSvg;
    pnlTSkPaintBoxLine: TPanel;
    pnlTSkSVG: TPanel;
    lblTSkSVGDescription: TSkLabel;
    lblTSkSVGTitle: TSkLabel;
    svgTSkSVGArrow: TSkSvg;
    pnlTSkSVGLine: TPanel;
    procedure pnlTSkAnimatedImageClick(Sender: TObject);
    procedure pnlTSkLabelClick(Sender: TObject);
    procedure pnlTSkPaintBoxClick(Sender: TObject);
    procedure pnlTSkSVGClick(Sender: TObject);
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

{$R *.dfm}

procedure TfrmControls.pnlTSkAnimatedImageClick(Sender: TObject);
begin
  ChildForm<TfrmTSkAnimatedImage>.Show;
end;

procedure TfrmControls.pnlTSkLabelClick(Sender: TObject);
begin
  ChildForm<TfrmTSkLabel>.Show;
end;

procedure TfrmControls.pnlTSkPaintBoxClick(Sender: TObject);
begin
  ChildForm<TfrmTSkPaintBox>.Show;
end;

procedure TfrmControls.pnlTSkSVGClick(Sender: TObject);
begin
  ChildForm<TfrmTSkSVG>.Show;
end;

end.
