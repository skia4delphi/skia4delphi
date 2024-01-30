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
unit Sample.Form.QRCode.Render;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,

  { Skia }
  System.Skia, FMX.Skia;

type
  { TfrmQRCodeRender }

  TfrmQRCodeRender = class(TForm)
    rctHeader: TRectangle;
    lblTitle: TSkLabel;
    btnBack: TSpeedButton;
    svgBackArrow: TSkSvg;
    svgBackground: TSkSvg;
    rctQRCodeBackground: TRectangle;
    svgQRCode: TSkSvg;
    saiQRCodeLogo: TSkAnimatedImage;
    lytQRCodeLogo: TLayout;
    procedure btnBackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmQRCodeRender: TfrmQRCodeRender;

implementation

uses
  { Sample }
  Sample.QRCode.Render;

{$R *.fmx}

procedure TfrmQRCodeRender.btnBackClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmQRCodeRender.FormShow(Sender: TObject);
const
  LogoSize = 5;
  F = False;
  T = True;

  // Squares of the QR code of the url "https://t.me/ski4delphi", indicating dark squares (T is true F is false)
  // This example only focuses on QR code rendering and not encoding. There are several examples on the internet
  // of how to encode a QR code, for example: https://github.com/foxitsoftware/DelphiZXingQRCode
  Modules: T2DBooleanArray = [
    [F, F, F, F, F, F, F, F, T, F, T, T, F, F, T, T, T, T, T, F, F, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, F, F, F, T, T, F, F, T, F, T, T, T, F, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, T, F, T, T, F, F, T, T, T, T, F, T, T, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, T, T, F, F, T, T, T, F, F, F, T, F, F, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, F, T, T, T, F, T, T, T, T, F, T, F, F, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, T, T, T, F, T, F, F, F, F, F, F, T, T, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, T, F, T, F, T, F, T, F, T, F, T, F, T, F, F, F, F, F, F, F, F],
    [F, F, F, F, F, F, F, F, T, F, F, F, F, T, F, F, T, T, T, T, T, F, F, F, F, F, F, F, F],
    [F, T, F, T, F, T, T, T, T, F, T, T, F, T, F, T, F, T, T, T, T, T, T, T, F, T, T, F, T],
    [T, T, T, F, T, F, F, T, T, T, T, T, T, F, F, T, T, T, T, T, F, T, T, F, F, F, F, F, T],
    [F, F, T, T, T, F, T, F, T, T, F, F, T, F, T, F, T, F, F, T, F, F, T, F, T, T, T, T, F],
    [T, T, T, F, T, T, F, F, F, T, T, F, F, F, F, F, F, F, T, T, T, T, T, F, F, F, T, T, F],
    [F, F, F, F, F, T, T, T, F, T, T, F, F, F, F, F, F, F, T, F, T, F, T, T, F, F, F, F, F],
    [F, T, T, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, T, F, T, F, T, F, T, F, F, F],
    [F, T, F, T, T, T, T, F, F, F, T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, T],
    [T, F, F, T, F, F, F, F, F, T, F, F, F, F, F, F, F, F, F, T, T, T, F, T, F, F, F, F, F],
    [T, F, F, T, T, T, T, T, F, F, F, F, F, F, F, F, F, F, F, T, T, T, F, T, F, F, F, F, F],
    [F, T, F, F, F, F, F, T, T, F, F, F, F, F, F, F, F, F, F, T, F, T, T, T, F, F, T, T, T],
    [T, F, T, T, F, T, T, T, T, F, F, T, T, F, F, F, F, T, F, T, T, T, F, F, T, F, T, T, T],
    [F, F, F, F, T, T, F, F, T, F, T, T, F, F, F, T, T, T, T, T, T, F, F, F, T, T, F, T, T],
    [T, F, F, F, F, F, T, T, T, F, F, F, F, T, T, F, T, F, F, T, T, T, T, T, T, F, T, F, F],
    [F, F, F, F, F, F, F, F, T, T, T, T, T, T, F, F, T, T, T, F, T, F, F, F, T, F, F, T, T],
    [F, F, F, F, F, F, F, F, T, F, F, F, T, T, T, F, T, F, T, F, T, F, T, F, T, T, T, T, F],
    [F, F, F, F, F, F, F, F, T, T, F, F, F, F, T, F, F, F, T, F, T, F, F, F, T, F, T, F, F],
    [F, F, F, F, F, F, F, F, F, F, F, T, F, T, F, T, T, F, T, F, T, T, T, T, T, T, F, F, F],
    [F, F, F, F, F, F, F, F, T, F, F, T, T, F, T, T, F, T, T, T, T, T, T, F, T, T, F, F, T],
    [F, F, F, F, F, F, F, F, F, T, F, T, F, T, F, T, T, F, T, T, T, F, F, F, T, T, F, F, T],
    [F, F, F, F, F, F, F, F, T, F, T, F, F, T, F, F, T, T, T, T, F, T, T, T, F, F, F, T, F],
    [F, F, F, F, F, F, F, F, F, F, F, T, F, T, F, T, F, T, F, T, T, T, F, F, T, T, F, T, F]];
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeGradientLinear(PointF(6, Length(Modules) - 6), PointF(Length(Modules) - 6, 6), $FF072484, $FF5A0084, TSkTileMode.Clamp);
  svgQRCode.Svg.Source := TQRCodeRender.MakeRounded(Modules, LogoSize).AsSVG(LPaint);

  lytQRCodeLogo.Visible := LogoSize > 0;
  lytQRCodeLogo.Size.Size := TSizeF.Create(Round(LogoSize * Min(svgQRCode.Width, svgQRCode.Height) / Length(Modules)), Round(LogoSize * Min(svgQRCode.Width, svgQRCode.Height) / Length(Modules)));
end;

end.
