unit View.Lottie;

interface

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls,

  { Skia }
  Skia,
  Skia.Vcl;

type
  { TfrmLottie }

  TfrmLottie = class(TForm)
    imgBackground: TImage;
    imgBackgroundPicture: TImage;
    lblHeaderTitle: TLabel;
    pnlHeader: TPanel;
    procedure FormResize(Sender: TObject);
  private
    FLottie: TSkLottieAnimation;
  public
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmLottie: TfrmLottie;

implementation

{$R *.dfm}

{ TfrmLottie }

{$REGION ' - Drawing form background'}
procedure TfrmLottie.FormResize(Sender: TObject);

  procedure TileImage(const ASource, ATarget: TImage);
  var
    LBitmap: TBitmap;
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(ATarget.Width, ATarget.Height);
      LBitmap.PixelFormat := TPixelFormat.pf32bit;
      LBitmap.AlphaFormat := TAlphaFormat.afPremultiplied;
      LBitmap.SkiaDraw(
        procedure(const ACanvas: ISkCanvas)
        var
          LImage: ISkImage;
          LPaint: ISkPaint;
          LBitmap: TBitmap;
        begin
          LBitmap := TBitmap.Create;
          try
            LBitmap.Assign(imgBackgroundPicture.Picture.Graphic);
            LImage := LBitmap.ToSkImage;
          finally
            LBitmap.Free;
          end;
          LPaint := TSkPaint.Create;
          LPaint.Shader := LImage.MakeShader(TSkTileMode.Replicate, TSkTileMode.Replicate);
          LPaint.Style := TSkPaintStyle.Fill;
          ACanvas.DrawRect(ACanvas.GetLocalClipBounds, LPaint);
        end);
      ATarget.Picture.Bitmap := LBitmap;
    finally
      LBitmap.Free;
    end;
  end;

begin
  if ((imgBackground.Width <> Screen.Width) and (imgBackground.Width < Width * 1.5)) or
    ((imgBackground.Height <> Screen.Height) and (imgBackground.Height < Height * 1.5)) then
  begin
    imgBackground.SetBounds(imgBackground.Left, imgBackground.Top, Min(Screen.Width, Width * 3), Min(Screen.Height, Height * 3));
    TileImage(imgBackgroundPicture, imgBackground);
  end;
end;
{$ENDREGION}

procedure TfrmLottie.Show(const AFileName: string);
begin
  if not Assigned(FLottie) then
  begin
    FLottie := TSkLottieAnimation.Create(Self);
    FLottie.Align := alClient;
    FLottie.Parent := Self;
  end;
  FLottie.LoadFromFile(AFileName);
  inherited Show;
end;

end.
