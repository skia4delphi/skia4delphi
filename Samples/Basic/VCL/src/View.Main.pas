unit View.Main;

interface

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    gbxShapes: TGroupBox;
    sbxContent: TScrollBox;
    gplShapesButtons: TGridPanel;
    btnShapesBasic: TSpeedButton;
    btnShapesBezierCurves: TSpeedButton;
    btnShapesTranslationsAndRotations: TSpeedButton;
    gbxText: TGroupBox;
    gplTextButtons: TGridPanel;
    btnTextRendering: TSpeedButton;
    btnTextRTL: TSpeedButton;
    gbxPathsAndEffects: TGroupBox;
    gplPathsAndEffectsButtons: TGridPanel;
    btnPathsAndEffectsDiscrete: TSpeedButton;
    btnPathsAndEffectsComposed: TSpeedButton;
    btnPathsAndEffectsSum: TSpeedButton;
    btnPathsAndEffectsShaders: TSpeedButton;
    gbxSVG: TGroupBox;
    gplSVGButtons: TGridPanel;
    btnSVGGorilla: TSpeedButton;
    btnSVGDelphi: TSpeedButton;
    gbxLottie: TGroupBox;
    gplLottieButtons: TGridPanel;
    btnLottieRocket: TSpeedButton;
    btnLottieCheck: TSpeedButton;
    btnSVGToPDF: TSpeedButton;
    btnLottieTelegramSticker: TSpeedButton;
    btnTextParagraph: TSpeedButton;
    gbxImage: TGroupBox;
    gplImageButtons: TGridPanel;
    btnImageWebPvsJpeg: TSpeedButton;
    btnTextCustomFont: TSpeedButton;
    procedure btnImageWebPvsJpegClick(Sender: TObject);
    procedure btnLottieCheckClick(Sender: TObject);
    procedure btnLottieRocketClick(Sender: TObject);
    procedure btnLottieTelegramStickerClick(Sender: TObject);
    procedure btnPathsAndEffectsComposedClick(Sender: TObject);
    procedure btnPathsAndEffectsDiscreteClick(Sender: TObject);
    procedure btnPathsAndEffectsShadersClick(Sender: TObject);
    procedure btnPathsAndEffectsSumClick(Sender: TObject);
    procedure btnShapesBasicClick(Sender: TObject);
    procedure btnShapesBezierCurvesClick(Sender: TObject);
    procedure btnShapesTranslationsAndRotationsClick(Sender: TObject);
    procedure btnSVGDelphiClick(Sender: TObject);
    procedure btnSVGGorillaClick(Sender: TObject);
    procedure btnSVGToPDFClick(Sender: TObject);
    procedure btnTextCustomFontClick(Sender: TObject);
    procedure btnTextParagraphClick(Sender: TObject);
    procedure btnTextRenderingClick(Sender: TObject);
    procedure btnTextRTLClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  { Skia }
  Skia,
  Skia.Vcl,

  { Sample }
  View.Comparison.Image,
  View.Preview.Bitmap,
  View.Lottie,
  View.SVG;

function GetAssetsPath: string;
begin
  Result := TPath.GetFullPath('..\..\..\..\..\..\Assets\Samples') + PathDelim;
end;

function GetOutputPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function StarPath: ISkPath;
var
  I: Integer;
  LA: Single;
  LC: Single;
  LPathBuilder: ISkPathBuilder;
  LR: Single;
begin
  LR := 115.2;
  LC := 128.0;
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(LC + LR, LC);
  for I := 1 to 7 do
  begin
    LA := 2.6927937 * I;
    LPathBuilder.LineTo(LC + LR * Cos(LA), LC + LR * Sin(LA));
  end;
  Result := LPathBuilder.Detach;
end;

function FormatBytes(const ABytesCount: Int64): string;
const
  KBYTES = Int64(1024);
begin
  Result := Format('%s KB', [FormatFloat('0.#', ABytesCount / KBYTES)]);
end;

{ TfrmMain }

procedure TfrmMain.btnImageWebPvsJpegClick(Sender: TObject);
const
  Quality = 80;
var
  LImage: ISkImage;
  LBytesWebP: TBytes;
  LBytesJpeg: TBytes;
begin
  LImage := TSkImage.MakeFromEncoded(TFile.ReadAllBytes(GetAssetsPath + 'kung_fu_panda.png'));
  LBytesWebP := LImage.EncodeToBytes(TSkEncodedImageFormat.WEBP, Quality);
  LBytesJpeg := LImage.EncodeToBytes(TSkEncodedImageFormat.JPEG, Quality);

  frmImageComparison.Show(
    LBytesWebP, Format('WebP - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesWebP))]),
    LBytesJpeg, Format('Jpeg - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesJpeg))]));
end;

procedure TfrmMain.btnLottieCheckClick(Sender: TObject);
begin
  frmLottie.Show(GetAssetsPath + 'check.json');
end;

procedure TfrmMain.btnLottieRocketClick(Sender: TObject);
begin
  frmLottie.Show(GetAssetsPath + 'rocket.json');
end;

procedure TfrmMain.btnLottieTelegramStickerClick(Sender: TObject);
begin
  frmLottie.Show(GetAssetsPath + 'telegram_sticker.tgs');
end;

procedure TfrmMain.btnPathsAndEffectsComposedClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LPath: ISkPath;
        LIntervals: TArray<Single>;
        LDashPathEffect: ISkPathEffect;
        LDiscretePathEffect: ISkPathEffect;
      begin
        SetLength(LIntervals, 4);
        LIntervals[0] := 10;
        LIntervals[1] := 5;
        LIntervals[2] := 2;
        LIntervals[3] := 5;
        LDashPathEffect := TSkPathEffect.MakeDash(LIntervals, 0);
        LDiscretePathEffect := TSkPathEffect.MakeDiscrete(10, 4);
        LPaint := TSkPaint.Create;
        LPaint.PathEffect := TSkPathEffect.MakeCompose(LDashPathEffect, LDiscretePathEffect);
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $FF4285F4;
        LPath := StarPath;
        ACanvas.DrawPath(LPath, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnPathsAndEffectsDiscreteClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LPath: ISkPath;
      begin
        LPaint := TSkPaint.Create;
        LPaint.PathEffect := TSkPathEffect.MakeDiscrete(10, 4);
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $FF4285F4;
        LPath := StarPath;
        ACanvas.DrawPath(LPath, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnPathsAndEffectsShadersClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LPath: ISkPath;
        LColors: TArray<TAlphaColor>;
      begin
        SetLength(LColors, 2);
        LColors[0] := $ff4285F4;
        LColors[1] := $ff0F9D58;
        LPaint := TSkPaint.Create;
        LPaint.PathEffect := TSkPathEffect.MakeDiscrete(10, 4);
        LPaint.Shader := TSkShader.MakeGradientLinear(TPointF.Create(0, 0), TPointF.Create(256, 256), LColors, TSkTileMode.Clamp);
        LPaint.AntiAlias := True;
        LPath := StarPath;
        ACanvas.DrawPath(LPath, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnPathsAndEffectsSumClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LPath: ISkPath;
        LIntervals: TArray<Single>;
        LDashPathEffect1: ISkPathEffect;
        LDashPathEffect2: ISkPathEffect;
      begin
        SetLength(LIntervals, 4);
        LIntervals[0] := 10;
        LIntervals[1] := 5;
        LIntervals[2] := 2;
        LIntervals[3] := 5;
        LDashPathEffect1 := TSkPathEffect.MakeDiscrete(10, 4);
        LDashPathEffect2 := TSkPathEffect.MakeDiscrete(10, 4, 1245);
        LPaint := TSkPaint.Create;
        LPaint.PathEffect := TSkPathEffect.MakeSum(LDashPathEffect1, LDashPathEffect2);
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $FF4285F4;
        LPath := StarPath;
        ACanvas.DrawPath(LPath, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnShapesBasicClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LOval: ISkRoundRect;
        LPaint: ISkPaint;
        LRect: TRectF;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.AntiAlias := True;
        LPaint.StrokeWidth := 4;
        LPaint.Color := $FF4285F4;

        LRect := TRectF.Create(TPointF.Create(10, 10), 100, 160);
        ACanvas.DrawRect(LRect, LPaint);

        LOval := TSkRoundRect.Create;
        LOval.SetOval(LRect);
        LOval.Offset(40, 80);
        LPaint.Color := $FFDB4437;
        ACanvas.DrawRoundRect(LOval, LPaint);

        LPaint.Color := $FF0F9D58;
        ACanvas.DrawCircle(180, 50, 25, LPaint);

        LRect.Offset(80, 50);
        LPaint.Color := $FFF4B400;
        LPaint.Style := TSkPaintStyle.Stroke;
        ACanvas.DrawRoundRect(LRect, 10, 10, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnShapesBezierCurvesClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LPath: ISkPath;
        LPathBuilder: ISkPathBuilder;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := 8;
        LPaint.Color := $FF4285F4;
        LPaint.AntiAlias := True;
        LPaint.StrokeCap := TSkStrokeCap.Round;

        LPathBuilder := TSkPathBuilder.Create;
        LPathBuilder.MoveTo(10, 10);
        LPathBuilder.QuadTo(256, 64, 128, 128);
        LPathBuilder.QuadTo(10, 192, 250, 250);
        LPath := LPathBuilder.Detach;
        ACanvas.DrawPath(LPath, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnShapesTranslationsAndRotationsClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
        LRect: TRectF;
      begin
        ACanvas.Translate(128, 0);
        ACanvas.Rotate(60);
        LRect := TRectF.Create(TPointF.Create(0, 0), 200, 100);

        LPaint := TSkPaint.Create;
        LPaint.AntiAlias := True;
        LPaint.Color := $FF4285F4;
        ACanvas.DrawRect(LRect, LPaint);

        ACanvas.Rotate(20);
        LPaint.Color := $FFDB4437;
        ACanvas.DrawRect(LRect, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnSVGDelphiClick(Sender: TObject);
begin
  frmSVG.Show(GetAssetsPath + 'delphi.svg');
end;

procedure TfrmMain.btnSVGGorillaClick(Sender: TObject);
begin
  frmSVG.Show(GetAssetsPath + 'gorilla.svg');
end;

procedure TfrmMain.btnSVGToPDFClick(Sender: TObject);
var
  LCanvas: ISkCanvas;
  LDocument: ISkDocument;
  LDOM: ISkSVGDOM;
  LPDFStream: TStream;
  LSize: TSizeF;
  LSVGStream: TStream;
  LPDFFileName: string;
begin
  LSVGStream := TFileStream.Create(GetAssetsPath + 'lion.svg', fmOpenRead or fmShareDenyWrite);
  try
    LDOM := TSkSVGDOM.Make(LSVGStream);
  finally
    LSVGStream.Free;
  end;
  LSize := TSizeF.Create(600, 600);
  LDOM.SetContainerSize(LSize);

  LPDFFileName := GetOutputPath + 'output.pdf';
  LPDFStream := TFileStream.Create(LPDFFileName, fmCreate);
  try
    LDocument := TSkPDFDocument.Create(LPDFStream);
    try
      LCanvas := LDocument.BeginPage(LSize.Width, LSize.Height);
      try
        LDOM.Render(LCanvas);
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LPDFStream.Free;
  end;
  // PDF Preview
  LPDFFileName := 'file://' + LPDFFileName.Replace('\', '/', [rfReplaceAll]);
  ShellExecute(0, 'open', PChar(LPDFFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.btnTextCustomFontClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LTypeface: ISkTypeface;
        LFont: ISkFont;
        LPaint: ISkPaint;
        LColors: TArray<TAlphaColor>;
        LPositions: TArray<Single>;
        LGradient: ISkShader;
      begin
        ACanvas.Clear($FF161B22);

        SetLength(LColors, 6);
        LColors[0] := $FFADFBDA;
        LColors[1] := $FF35C3FF;
        LColors[2] := $FFFDA399;
        LColors[3] := $FF76D880;
        LColors[4] := $FFEBF38B;
        LColors[5] := $FFADFBDA;
        SetLength(LPositions, 6);
        LPositions[0] := 0;
        LPositions[1] := 0.3;
        LPositions[2] := 0.5;
        LPositions[3] := 0.7;
        LPositions[4] := 0.9;
        LPositions[5] := 1;
        LGradient := TSkShader.MakeGradientLinear(TPointF.Create(0, 0), TPointF.Create(125, 256), LColors, TSkTileMode.Clamp, LPositions);

        LTypeface := TSkTypeface.MakeFromFile(GetAssetsPath + 'Poppins-Semibold.ttf');
        LFont := TSkFont.Create(LTypeface, 23);
        LPaint := TSkPaint.Create;
        LPaint.Shader := LGradient;
        LPaint.Style := TSkPaintStyle.Fill;

        ACanvas.DrawSimpleText('"Each dream that you', 2, 25, LFont, LPaint);
        ACanvas.DrawSimpleText('leave behind is a part', 2, 55, LFont, LPaint);
        ACanvas.DrawSimpleText('of your future that will', 2, 85, LFont, LPaint);
        ACanvas.DrawSimpleText('no longer exist."', 2, 115, LFont, LPaint);

        LTypeface := TSkTypeface.MakeFromFile(GetAssetsPath + 'BonheurRoyale-Regular.ttf');
        LFont.Typeface := LTypeface;
        LFont.Size := 28;

        LPaint.Shader := nil;
        LPaint.Color  := TAlphaColors.White;
        ACanvas.DrawSimpleText('(Steve Jobs)', 2, 150, LFont, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnTextParagraphClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(440, 440);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LParagraph: ISkParagraph;
        LBuilder: ISkParagraphBuilder;
        LTextStyle: ISkTextStyle;
        LParagraphStyle: ISkParagraphStyle;
      begin
        LParagraphStyle := TSkParagraphStyle.Create;
        LParagraphStyle.TurnHintingOff;
        LParagraphStyle.MaxLines := 3;
        LParagraphStyle.Ellipsis := '...';
        LBuilder := TSkParagraphBuilder.Create(LParagraphStyle);

        LTextStyle := TSkTextStyle.Create;
        LTextStyle.Color := TAlphaColors.Black;
        LTextStyle.SetFontSize(28);
        LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.Light, TSkFontWidth.Normal, TSkFontSlant.Upright));
        LBuilder.PushStyle(LTextStyle);
        LBuilder.AddText('English English 字典 字典 😀😅😂😂');

        LTextStyle := TSkTextStyle.Create;
        LTextStyle.Color := TAlphaColors.Crimson;
        LTextStyle.SetFontSize(22);
        LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.SemiBold, TSkFontWidth.Normal, TSkFontSlant.Upright));
        LBuilder.PushStyle(LTextStyle);
        LBuilder.AddText(' !سلام دنیا');

        LTextStyle := TSkTextStyle.Create;
        LTextStyle.Color := TAlphaColors.Blueviolet;
        LTextStyle.SetFontSize(30);
        LTextStyle.SetFontStyle(TSkFontStyle.Create(TSkFontWeight.ExtraBold, TSkFontWidth.Normal, TSkFontSlant.Italic));
        LBuilder.PushStyle(LTextStyle);
        LBuilder.AddText(' World domination is such an ugly phrase - I prefer to call it world optimisation.');

        LParagraph := LBuilder.Detach;
        LParagraph.Layout(LBitmap.Width);
        LParagraph.Render(ACanvas, 0, 0);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnTextRenderingClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LBlob1: ISkTextBlob;
        LBlob2: ISkTextBlob;
        LFont1: ISkFont;
        LFont2: ISkFont;
        LPaint1: ISkPaint;
        LPaint2: ISkPaint;
        LPaint3: ISkPaint;
        LTypeface: ISkTypeface;
      begin
        LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
        LFont1 := TSkFont.Create(LTypeface, 64, 1);
        LFont2 := TSkFont.Create(LTypeface, 64, 1.5);
        LFont1.Edging := TSkFontEdging.AntiAlias;
        LFont2.Edging := TSkFontEdging.AntiAlias;

        LBlob1 := TSkTextBlob.Make('Skia', LFont1);
        LBlob2 := TSkTextBlob.Make('Skia', LFont2);


        LPaint1 := TSkPaint.Create;
        LPaint1.AntiAlias := True;
        LPaint1.SetARGB($FF, $42, $85, $F4);

        LPaint2 := TSkPaint.Create;
        LPaint2.AntiAlias := True;
        LPaint2.SetARGB($FF, $DB, $44, $37);
        LPaint2.Style := TSkPaintStyle.Stroke;
        LPaint2.StrokeWidth := 3;

        LPaint3 := TSkPaint.Create;
        LPaint3.AntiAlias := True;
        LPaint3.SetARGB($FF, $0F, $9D, $58);

        ACanvas.DrawTextBlob(LBlob1, 20, 64, LPaint1);
        ACanvas.DrawSimpleText('Skia', 20, 154, LFont1, LPaint2);
        ACanvas.DrawTextBlob(LBlob2, 20, 244, LPaint3);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnTextRTLClick(Sender: TObject);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(256, 256);
    LBitmap.SkiaDraw(
      procedure (const ACanvas: ISkCanvas)
      var
        LBlob: ISkTextBlob;
        LFont: ISkFont;
        LPaint: ISkPaint;
        LRunHandler: ISkTextBlobBuilderRunHandler;
        LShaper: ISkShaper;
        LText: string;
      begin
        LFont := TSkFont.Create(TSkTypeface.MakeDefault, 55, 1);
        LText := '!سلام دنیا';

        LRunHandler := TSkTextBlobBuilderRunHandler.Create(LText, TPointF.Create(0, 0));
        LShaper := TSkShaper.Create;
        LShaper.Shape(LText, LFont, True, MaxSingle, LRunHandler);
        LBlob := LRunHandler.Detach;

        LPaint := TSkPaint.Create;
        LPaint.AntiAlias := True;
        LPaint.Color := TAlphaColors.Tomato;

        ACanvas.DrawTextBlob(LBlob, 0, 0, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  sbxContent.VertScrollBar.Position := sbxContent.VertScrollBar.Position - WheelDelta;
  Handled := True;
end;

end.
