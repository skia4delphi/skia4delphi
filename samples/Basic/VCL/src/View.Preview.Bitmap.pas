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
  Vcl.StdCtrls,

  { Sample }
  Vcl.WIC.Bitmap;

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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Show(ABitmap: TWICBitmap); reintroduce;
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
  imgBackground.Picture.Bitmap.SetSize(ClientWidth, ClientHeight);
  TileImage(imgBackgroundPicture, imgBackground.Picture.Bitmap.Canvas, ClientWidth, ClientHeight);
  sbxContent.Repaint;
end;
{$ENDREGION}

procedure TfrmBitmapPreview.Show(ABitmap: TWICBitmap);
begin
  inherited Show;
  imgBitmap.Width := Ceil(ABitmap.Width {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
  imgBitmap.Height := Ceil(ABitmap.Width {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
  imgBitmap.Picture.Graphic := ABitmap.Image;
end;

end.
