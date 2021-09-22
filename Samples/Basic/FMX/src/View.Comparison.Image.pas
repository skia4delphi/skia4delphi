unit View.Comparison.Image;

interface

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types,
  FMX.Graphics,
  FMX.StdCtrls,

  { Skia }
  Skia,
  Skia.FMX;

type
  { TfrmImageComparison }

  TfrmImageComparison = class(TForm)
    rctBitmap: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    sbxContent: TScrollBox;
    rctHeader: TRectangle;
    imgArrow: TImage;
    procedure btnCloseClick(Sender: TObject);
  public
    procedure Show(const AImageBytes1: TBytes; const ATitle1: string;
      const AImageBytes2: TBytes; const ATitle2: string); reintroduce;
  end;

var
  frmImageComparison: TfrmImageComparison;

implementation

{$R *.fmx}

{ TfrmImageComparison }

procedure TfrmImageComparison.btnCloseClick(Sender: TObject);
begin
  Close;
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
  LComparisonBitmap := TBitmap.Create(Max(LImage1.Width, LImage2.Width), LImage1.Height + BitmapsOffset + LImage2.Height);
  try
    LComparisonBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      begin
        DoDraw(ACanvas, LImage1, ATitle1);
        ACanvas.Translate(0, LImage1.Height + BitmapsOffset);
        DoDraw(ACanvas, LImage2, ATitle2);
      end);

    rctBitmap.Width := LComparisonBitmap.Width / rctBitmap.Scene.GetSceneScale;
    rctBitmap.Height := LComparisonBitmap.Height / rctBitmap.Scene.GetSceneScale;
    rctBitmap.Fill.Bitmap.Bitmap := LComparisonBitmap;
  finally
    LComparisonBitmap.Free;
  end;
  inherited Show;
end;

end.
