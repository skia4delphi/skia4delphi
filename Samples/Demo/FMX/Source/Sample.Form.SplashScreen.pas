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
unit Sample.Form.SplashScreen;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.Classes, System.UITypes, FMX.Types,
  FMX.Graphics, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Ani, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmSplashScreen = class(TfrmBase)
    svgLogoBackground: TSkSvg;
    aimLogoForeground: TSkAnimatedImage;
    gplBottomText: TGridPanelLayout;
    svgHeart: TSkSvg;
    lblBottomTextLeft: TSkLabel;
    lblBottomTextRight: TSkLabel;
    fanFadeOut: TFloatAnimation;
    lytSplashContent: TLayout;
    fanFadeIn: TFloatAnimation;
    procedure aimLogoForegroundAnimationFinish(Sender: TObject);
    procedure rctContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  protected
    class function FormBackgroundColor: TAlphaColor; override;
    function HasBottomContent: Boolean; override;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TfrmSplashScreen.aimLogoForegroundAnimationFinish(Sender: TObject);
begin
  fanFadeOut.Start;
end;

class function TfrmSplashScreen.FormBackgroundColor: TAlphaColor;
begin
  Result := FormBorderColor;
end;

function TfrmSplashScreen.HasBottomContent: Boolean;
begin
  Result := True;
end;

procedure TfrmSplashScreen.rctContentPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  inherited;
  fanFadeIn.Enabled := True;
end;

end.
