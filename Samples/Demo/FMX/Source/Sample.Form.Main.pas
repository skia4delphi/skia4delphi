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
unit Sample.Form.Main;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.UITypes, System.Classes, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Layouts, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,

  { Skia }
  Skia.FMX,

  { Sample }
  {$IF CompilerVersion < 30}
  Sample.Form.Base.Styled.XE7;
  {$ELSE}
  Sample.Form.Base.Styled.Seattle;
  {$ENDIF}

type
  { TfrmMain }

  TfrmMain = class(TfrmStyledBase)
    btnUnicode: TSpeedButton;
    lblUnicodeTitle: TSkLabel;
    lblUnicodeDescription: TSkLabel;
    btnRuntimeEffects: TSpeedButton;
    lblRuntimeEffectsTitle: TSkLabel;
    lblRuntimeEffectsDescription: TSkLabel;
    btnParticles: TSpeedButton;
    lblParticlesTitle: TSkLabel;
    lblParticlesDescription: TSkLabel;
    btnImage: TSpeedButton;
    lblImageTitle: TSkLabel;
    lblImageDescription: TSkLabel;
    btnPathsAndEffects: TSpeedButton;
    lblPathsAndEffectsDescription: TSkLabel;
    lblPathsAndEffectsTitle: TSkLabel;
    btnDocuments: TSpeedButton;
    lblDocumentsTitle: TSkLabel;
    lblDocumentsDescription: TSkLabel;
    btnText: TSpeedButton;
    lblTextTitle: TSkLabel;
    lblTextDescription: TSkLabel;
    btnBasics: TSpeedButton;
    lblBasicsTitle: TSkLabel;
    lblBasicsDescription: TSkLabel;
    btnControls: TSpeedButton;
    lblControlsTitle: TSkLabel;
    lblControlsDescription: TSkLabel;
    btnTransforms: TSpeedButton;
    lblTransformsTitle: TSkLabel;
    lblTransformsDescription: TSkLabel;
    btnFilter: TSpeedButton;
    lblFilterTitle: TSkLabel;
    lblFilterDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnBasicsClick(Sender: TObject);
    procedure btnControlsClick(Sender: TObject);
    procedure btnDocumentsClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure btnParticlesClick(Sender: TObject);
    procedure btnPathsAndEffectsClick(Sender: TObject);
    procedure btnRuntimeEffectsClick(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
    procedure btnTransformsClick(Sender: TObject);
    procedure btnUnicodeClick(Sender: TObject);
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
  Sample.Form.Particles,
  Sample.Form.RuntimeEffects,
  Sample.Form.Text,
  Sample.Form.Transforms,
  Sample.Form.Unicode;

{$R *.fmx}

procedure TfrmMain.btnBasicsClick(Sender: TObject);
begin
  ChildForm<TfrmBasics>.Show;
end;

procedure TfrmMain.btnControlsClick(Sender: TObject);
begin
  ChildForm<TfrmControls>.Show;
end;

procedure TfrmMain.btnDocumentsClick(Sender: TObject);
begin
  ChildForm<TfrmDocuments>.Show;
end;

procedure TfrmMain.btnFilterClick(Sender: TObject);
begin
  ChildForm<TfrmFilter>.Show;
end;

procedure TfrmMain.btnImageClick(Sender: TObject);
begin
  ChildForm<TfrmImage>.Show;
end;

procedure TfrmMain.btnParticlesClick(Sender: TObject);
begin
  ChildForm<TfrmParticles>.Show;
end;

procedure TfrmMain.btnPathsAndEffectsClick(Sender: TObject);
begin
  ChildForm<TfrmPathsAndEffects>.Show;
end;

procedure TfrmMain.btnRuntimeEffectsClick(Sender: TObject);
begin
  ChildForm<TfrmRuntimeEffects>.Show;
end;

procedure TfrmMain.btnTextClick(Sender: TObject);
begin
  ChildForm<TfrmText>.Show;
end;

procedure TfrmMain.btnTransformsClick(Sender: TObject);
begin
  ChildForm<TfrmTransforms>.Show;
end;

procedure TfrmMain.btnUnicodeClick(Sender: TObject);
begin
  ChildForm<TfrmUnicode>.Show;
end;

end.
