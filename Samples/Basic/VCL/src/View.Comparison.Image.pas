unit View.Comparison.Image;

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
  { TfrmImageComparison }

  TfrmImageComparison = class(TForm)
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
    procedure Show(const AImageBytes1: TBytes; const ATitle1: string;
      const AImageBytes2: TBytes; const ATitle2: string); reintroduce;
  end;

var
  frmImageComparison: TfrmImageComparison;

implementation

{$R *.dfm}

{ TfrmImageComparison }

{$REGION ' - Drawing form background'}
procedure TfrmImageComparison.FormResize(Sender: TObject);

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

procedure TfrmImageComparison.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  sbxContent.VertScrollBar.Position := sbxContent.VertScrollBar.Position - WheelDelta;
  Handled := True;
end;

procedure DoDraw(const ACanvas: ISkCanvas; const AImage: ISkImage; const AText: string);
const
  TitleHeight = 40;
  TitleX = 10;
  TitleY = 26;
  TextSize = 26;
var
  LBackPaint: ISkPaint;
  LFrontPaint: ISkPaint;
  LFont: ISkFont;
  LTypeface: ISkTypeface;
begin
  LBackPaint := TSkPaint.Create;
  LBackPaint.Color := TAlphaColors.Black;
  LBackPaint.AlphaF := 0.5;
  LBackPaint.Style := TSkPaintStyle.Fill;

  LFrontPaint := TSkPaint.Create;
  LFrontPaint.Color := TAlphaColors.White;
  LFrontPaint.Style := TSkPaintStyle.Fill;

  ACanvas.DrawImage(AImage, 0, 0);
  ACanvas.DrawRect(TRectF.Create(0, 0, AImage.Width, TitleHeight), LBackPaint);
  LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
  LFont := TSkFont.Create(LTypeface, TextSize, 1);

  ACanvas.DrawSimpleText(AText, TitleX, TitleY, LFont, LFrontPaint);
end;

procedure TfrmImageComparison.Show(const AImageBytes1: TBytes;
  const ATitle1: string; const AImageBytes2: TBytes; const ATitle2: string);
const
  BitmapsOffset = 20;
var
  LComparisonBitmap: TBitmap;
  LImage1: ISkImage;
  LImage2: ISkImage;
begin
  LImage1 := TSkImage.MakeFromEncoded(AImageBytes1);
  LImage2 := TSkImage.MakeFromEncoded(AImageBytes2);
  LComparisonBitmap := TBitmap.Create;
  try
    LComparisonBitmap.SetSize(Max(LImage1.Width, LImage2.Width), LImage1.Height + BitmapsOffset + LImage2.Height);
    LComparisonBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      begin
        DoDraw(ACanvas, LImage1, ATitle1);
        ACanvas.Translate(0, LImage1.Height + BitmapsOffset);
        DoDraw(ACanvas, LImage2, ATitle2);
      end);

    imgBitmap.Width := Ceil(LComparisonBitmap.Width {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
    imgBitmap.Height := Ceil(LComparisonBitmap.Height {$IF CompilerVersion >= 33}/ imgBitmap.ScaleFactor{$ENDIF});
    imgBitmap.Picture.Bitmap := LComparisonBitmap;
  finally
    LComparisonBitmap.Free;
  end;
  inherited Show;
end;

end.
