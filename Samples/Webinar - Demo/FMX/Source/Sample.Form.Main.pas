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
unit Sample.Form.Main;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects,

  { Skia }
  System.Skia, FMX.Skia;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    saiAnimatedLogo: TSkAnimatedImage;
    fanFadeOutTransition: TFloatAnimation;
    rctHeader: TRectangle;
    lblTitle: TSkLabel;
    rctBottomMenu: TRectangle;
    rctPDFCreation: TRectangle;
    lblPDFCreation: TSkLabel;
    rctQRCodeRender: TRectangle;
    lblQRCodeRender: TSkLabel;
    lytContent: TLayout;
    btnQRCodeRender: TSpeedButton;
    btnPDFCreation: TSpeedButton;
    sdwBottomMenu: TShadowEffect;
    svgFireBackground: TSkSvg;
    stbStyle: TStyleBook;
    procedure fanFadeOutTransitionFinish(Sender: TObject);
    procedure saiAnimatedLogoAnimationFinished(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPDFCreationClick(Sender: TObject);
    procedure btnQRCodeRenderClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  { Sample }
  Sample.Form.PDF.Creation,
  Sample.Form.QRCode.Render;

procedure TfrmMain.btnPDFCreationClick(Sender: TObject);
begin
  frmPDFCreation.Show;
end;

procedure TfrmMain.fanFadeOutTransitionFinish(Sender: TObject);
begin
  saiAnimatedLogo.Visible := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Fill.Color := $FF372C7C; // Purple
  saiAnimatedLogo.Visible := True;
  saiAnimatedLogo.BringToFront;
  lytContent.Visible := False;
end;

procedure TfrmMain.saiAnimatedLogoAnimationFinished(Sender: TObject);
begin
  lytContent.Visible := True;
  Fill.Color := $FFEBEEF1; // Light gray
  fanFadeOutTransition.Enabled := True;
end;

procedure TfrmMain.btnQRCodeRenderClick(Sender: TObject);
begin
  frmQRCodeRender.Show;
end;

end.
