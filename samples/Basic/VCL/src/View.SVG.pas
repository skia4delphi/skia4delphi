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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.StdCtrls,

  { Skia }
  Skia;

type
  { TfrmSVG }

  TfrmSVG = class(TForm)
    imgBackgroundPicture: TImage;
    lblHeaderTitle: TLabel;
    pnlHeader: TPanel;
    imgBitmap: TImage;
    imgBackground: TImage;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FDOM: ISKSVGDOM;
    procedure UpdateSVG;
  public
    { Public declarations }
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmSVG: TfrmSVG;

implementation

{$R *.dfm}

uses
  { Sample }
  Vcl.WIC.Bitmap,
  View.Main;

{ TfrmSVG }

{$REGION ' - Drawing form background'}
procedure TfrmSVG.FormResize(Sender: TObject);

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
  if Assigned(FDOM) then
    UpdateSVG;
end;
{$ENDREGION}

procedure TfrmSVG.Show(const AFileName: string);
var
  LSVGStream: ISKStream;
begin
  LSVGStream := TSKFileStream.Create(AFileName);
  FDOM := TSKSVGDOM.Make(LSVGStream);
  inherited Show;
  UpdateSVG;
end;

procedure TfrmSVG.UpdateSVG;
var
  LBitmap: TWICBitmap;
begin
  // Creating the bitmap size to real rectangle size (in pixels) and clear it
  LBitmap := TWICBitmap.Create(Round(imgBitmap.Width{$IF CompilerVersion >= 33} * imgBitmap.ScaleFactor{$ENDIF}),
    Round(imgBitmap.Height{$IF CompilerVersion >= 33} * imgBitmap.ScaleFactor{$ENDIF}));
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LSVGRect: TRectF;
        LDest: TRectF;
      begin
        ACanvas.Clear(TAlphaColors.Null);
        ACanvas.Save;
        try
          // Before render we need to adjust the ACanvas scale and translate to determine the position
          // and size of the draw. In the example below we will adjust it to fit into the rectangle's bitmap
          LSVGRect := TRectF.Create(TPointF.Create(0, 0), FDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0)));
          LDest := LSVGRect.FitInto(TRectF.Create(TPointF.Create(0, 0), LBitmap.Width, LBitmap.Height));
          if not LSVGRect.IsEmpty then
          begin
            ACanvas.Translate(LDest.Left, LDest.Top);
            ACanvas.Scale(LDest.Width / LSVGRect.Width, LDest.Height / LSVGRect.Height);
          end;

          FDOM.Render(ACanvas);
        finally
          ACanvas.Restore;
        end;
      end);
    imgBitmap.Picture.Graphic := LBitmap.Image;
  finally
    LBitmap.Free;
  end;
end;

end.
