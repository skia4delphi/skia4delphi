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
unit Sample.Form.Main;

// Style created by https://www.delphistyles.com/

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmMain = class(TfrmBase)
    pnlBasics: TPanel;
    lblBasicsDescription: TSkLabel;
    lblBasicsTitle: TSkLabel;
    svgBasicsArrow: TSkSvg;
    pnlBasicsLine: TPanel;
    pnlControls: TPanel;
    lblControlsDescription: TSkLabel;
    lblControlsTitle: TSkLabel;
    svgControlsArrow: TSkSvg;
    pnlControlsLine: TPanel;
    pnlDocuments: TPanel;
    lblDocumentsDescription: TSkLabel;
    lblDocumentsTitle: TSkLabel;
    svgDocumentsArrow: TSkSvg;
    pnlDocumentsLine: TPanel;
    pnlImage: TPanel;
    lblImageDescription: TSkLabel;
    lblImageTitle: TSkLabel;
    svgImageArrow: TSkSvg;
    pnlImageLine: TPanel;
    pnlPathsAndEffects: TPanel;
    lblPathsAndEffectsDescription: TSkLabel;
    lblPathsAndEffectsTitle: TSkLabel;
    svgPathsAndEffectsArrow: TSkSvg;
    pnlPathsAndEffectsLine: TPanel;
    pnlRuntimeEffects: TPanel;
    lblRuntimeEffectsDescription: TSkLabel;
    lblRuntimeEffectsTitle: TSkLabel;
    svgRuntimeEffectsArrow: TSkSvg;
    pnlRuntimeEffectsLine: TPanel;
    pnlText: TPanel;
    lblTextDescription: TSkLabel;
    lblTextTitle: TSkLabel;
    svgTextArrow: TSkSvg;
    pnlTextLine: TPanel;
    pnlUnicode: TPanel;
    lblUnicodeDescription: TSkLabel;
    lblUnicodeTitle: TSkLabel;
    svgUnicodeArrow: TSkSvg;
    pnlUnicodeLine: TPanel;
    pnlTransforms: TPanel;
    lblTransformsDescription: TSkLabel;
    lblTransformsTitle: TSkLabel;
    svgTransformsArrow: TSkSvg;
    pnlFilter: TPanel;
    lblFilterDescription: TSkLabel;
    lblFilterTitle: TSkLabel;
    svgFilterArrow: TSkSvg;
    pnlFilterLine: TPanel;
    pnlTransformsLine: TPanel;
    procedure FormShow(Sender: TObject);
    procedure pnlBasicsClick(Sender: TObject);
    procedure pnlControlsClick(Sender: TObject);
    procedure pnlDocumentsClick(Sender: TObject);
    procedure pnlFilterClick(Sender: TObject);
    procedure pnlImageClick(Sender: TObject);
    procedure pnlPathsAndEffectsClick(Sender: TObject);
    procedure pnlRuntimeEffectsClick(Sender: TObject);
    procedure pnlTextClick(Sender: TObject);
    procedure pnlTransformsClick(Sender: TObject);
    procedure pnlUnicodeClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  { Sample }
  Sample.Form.Basics,
  Sample.Form.Controls,
  Sample.Form.Documents,
  Sample.Form.Filter,
  Sample.Form.Image,
  Sample.Form.PathsAndEffects,
  Sample.Form.RuntimeEffects,
  Sample.Form.SplashScreen,
  Sample.Form.Text,
  Sample.Form.Transforms,
  Sample.Form.Unicode;

{$R *.dfm}

procedure TfrmMain.FormShow(Sender: TObject);
begin
  inherited;
  ChildForm<TfrmSplashScreen>.Show;
end;

procedure TfrmMain.pnlBasicsClick(Sender: TObject);
begin
  ChildForm<TfrmBasics>.Show;
end;

procedure TfrmMain.pnlControlsClick(Sender: TObject);
begin
  ChildForm<TfrmControls>.Show;
end;

procedure TfrmMain.pnlDocumentsClick(Sender: TObject);
begin
  ChildForm<TfrmDocuments>.Show;
end;

procedure TfrmMain.pnlFilterClick(Sender: TObject);
begin
  ChildForm<TfrmFilter>.Show;
end;

procedure TfrmMain.pnlImageClick(Sender: TObject);
begin
  ChildForm<TfrmImage>.Show;
end;

procedure TfrmMain.pnlPathsAndEffectsClick(Sender: TObject);
begin
  ChildForm<TfrmPathsAndEffects>.Show;
end;

procedure TfrmMain.pnlRuntimeEffectsClick(Sender: TObject);
begin
  ChildForm<TfrmRuntimeEffects>.Show;
end;

procedure TfrmMain.pnlTextClick(Sender: TObject);
begin
  ChildForm<TfrmText>.Show;
end;

procedure TfrmMain.pnlTransformsClick(Sender: TObject);
begin
  ChildForm<TfrmTransforms>.Show;
end;

procedure TfrmMain.pnlUnicodeClick(Sender: TObject);
begin
  ChildForm<TfrmUnicode>.Show;
end;

end.
