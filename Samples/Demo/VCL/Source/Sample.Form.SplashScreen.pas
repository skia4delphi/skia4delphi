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
unit Sample.Form.SplashScreen;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows, Winapi.Messages, System.Classes, System.Types, Vcl.Forms,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmSplashScreen = class(TfrmBase)
    gplBottomText: TGridPanel;
    lblBottomTextLeft: TSkLabel;
    lblBottomTextRight: TSkLabel;
    svgHeart: TSkSvg;
    svgLogoBackground: TSkSvg;
    aimLogoForeground: TSkAnimatedImage;
    pnlLogo: TPanel;
    apbBackForm: TSkAnimatedPaintBox;
    procedure aimLogoForegroundAnimationFinish(Sender: TObject);
    procedure apbBackFormAnimationDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
    procedure pnlContentAlignPosition(Sender: TWinControl; Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect; AlignInfo: TAlignInfo);
  private
    FBackFormImage: ISkImage;
    FFrontFormImage: ISkImage;
  protected
    class function FormBackgroundColor: TColor; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmSplashScreen.aimLogoForegroundAnimationFinish(Sender: TObject);

  procedure PaintControl(const AControl: TWinControl; const ACanvas: TCanvas;
    const AOffset: TPoint);
  begin
    SaveDC(ACanvas.Handle);
    try
      SetWindowOrgEx(ACanvas.Handle, -(AControl.Left + AOffset.X), -(AControl.Top + AOffset.Y), nil);
      AControl.Perform(WM_PRINT, ACanvas.Handle, PRF_CHILDREN or PRF_CLIENT or PRF_ERASEBKGND);
    finally
      RestoreDC(ACanvas.Handle, -1);
    end;
  end;

  function MakeScreenshot(const AForm: TfrmBase): ISkImage;
  var
    LBitmap: TBitmap;
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(AForm.pnlContent.Width, AForm.pnlContent.Height);
      LBitmap.Canvas.Lock;
      try
        PaintControl(AForm.pnlContent, LBitmap.Canvas, Point(0, 0));
      finally
        LBitmap.Canvas.Unlock;
      end;
      Result := LBitmap.ToSkImage;
    finally
      LBitmap.Free;
    end;
  end;

begin
  FFrontFormImage := MakeScreenshot(Self);
  FBackFormImage := MakeScreenshot(Application.MainForm as TfrmBase);
  BeginUpdate;
  try
    apbBackForm.BoundsRect := Rect(0, 0, pnlContent.Width, pnlContent.Height);
    apbBackForm.Align := alClient;
    apbBackForm.Parent := pnlContent.Parent;
    pnlContent.Visible := False;
    apbBackForm.Visible := True;
  finally
    EndUpdate;
  end;
end;

procedure TfrmSplashScreen.apbBackFormAnimationDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double;
  const AOpacity: Single);
begin
  ACanvas.Save;
  try
    ACanvas.Scale(1 / TSkAnimatedPaintBox(ASender).ScaleFactor, 1 / TSkAnimatedPaintBox(ASender).ScaleFactor);
    ACanvas.DrawImage(FBackFormImage, 0, 0);
    ACanvas.SaveLayerAlpha(Round(255 * (1 - AProgress)));
    try
      ACanvas.DrawImage(FFrontFormImage, 0, 0);
    finally
      ACanvas.Restore;
    end;
  finally
    ACanvas.Restore;
  end;
end;

class function TfrmSplashScreen.FormBackgroundColor: TColor;
begin
  Result := FormBorderColor;
end;

procedure TfrmSplashScreen.pnlContentAlignPosition(Sender: TWinControl;
  Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
  var AlignRect: TRect; AlignInfo: TAlignInfo);
begin
  inherited;
  if Control = pnlLogo then
  begin
    NewLeft := AlignRect.Left + ((AlignRect.Width - Control.Width) div 2);
    NewTop := AlignRect.Top + ((AlignRect.Height - Control.Height) div 2);
  end;
end;

end.
