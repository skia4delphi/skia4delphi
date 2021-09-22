unit View.SVG;

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
  System.IOUtils,
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
  { TfrmSVG }

  TfrmSVG = class(TForm)
    imgBackgroundPicture: TImage;
    lblHeaderTitle: TLabel;
    pnlHeader: TPanel;
    imgBackground: TImage;
    procedure FormResize(Sender: TObject);
  private
    FSvg: TSkSvg;
  public
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmSVG: TfrmSVG;

implementation

{$R *.dfm}

{ TfrmSVG }

{$REGION ' - Drawing form background'}
procedure TfrmSVG.FormResize(Sender: TObject);

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

procedure TfrmSVG.Show(const AFileName: string);
begin
  if not Assigned(FSvg) then
  begin
    FSvg := TSkSvg.Create(Self);
    FSvg.Align := alClient;
    FSvg.Parent := Self;
  end;
  FSvg.Svg.Source := TFile.ReadAllText(AFileName);
  inherited Show;
end;

end.
