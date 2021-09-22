unit View.Preview.Bitmap;

interface

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls;

type
  { TfrmBitmapPreview }

  TfrmBitmapPreview = class(TForm)
    imgBackground: TImage;
    imgBackgroundPicture: TImage;
    lblHeaderTitle: TLabel;
    pnlHeader: TPanel;
    imgBitmap: TImage;
    sbxContent: TScrollBox;
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    procedure Show(ABitmap: TBitmap); reintroduce;
  end;

var
  frmBitmapPreview: TfrmBitmapPreview;

implementation

{$R *.dfm}

{ TfrmBitmapPreview }

{$REGION ' - Drawing form background'}
procedure TfrmBitmapPreview.FormResize(Sender: TObject);

  procedure TileImage(const ASource: TImage; ATarget: TCanvas; ATargetWidth,
    ATargetHeight: Integer);
  var
    X, Y: Integer;
  begin
    for Y := 0 to Ceil(ATargetHeight / ASource.Height)-1 do
      for X := 0 to Ceil(ATargetWidth / ASource.Width)-1 do
        ATarget.Draw(X * ASource.Width, Y * ASource.Height, ASource.Picture.Graphic);
  end;

begin
  if (imgBackground.Width <> Screen.Width) or (imgBackground.Height <> Screen.Height) then
  begin
    imgBackground.SetBounds(imgBackground.Left, imgBackground.Top, Screen.Width, Screen.Height);
    imgBackground.Picture.Bitmap.SetSize(Screen.Width, Screen.Height);
    TileImage(imgBackgroundPicture, imgBackground.Picture.Bitmap.Canvas, Screen.Width, Screen.Height);
  end;
  sbxContent.Repaint;
end;
{$ENDREGION}

procedure TfrmBitmapPreview.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  sbxContent.VertScrollBar.Position := sbxContent.VertScrollBar.Position - WheelDelta;
  Handled := True;
end;

procedure TfrmBitmapPreview.Show(ABitmap: TBitmap);
begin
  inherited Show;
  imgBitmap.Width := Ceil(ABitmap.Width {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
  imgBitmap.Height := Ceil(ABitmap.Height {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
  imgBitmap.Picture.Bitmap := ABitmap;
end;

end.
