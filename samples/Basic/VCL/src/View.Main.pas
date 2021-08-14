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
  Vcl.Buttons,

  { Skia }
  Skia,

  { Sample }
  Vcl.WIC.Bitmap;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    gbxShapes: TGroupBox;
    sbxContent: TScrollBox;
    gplShapes: TGridPanel;
    btnShapesBasic: TSpeedButton;
    btnShapesBezierCurves: TSpeedButton;
    btnShapesTranslationsAndRotations: TSpeedButton;
    gbxText: TGroupBox;
    gplText: TGridPanel;
    btnTextRendering: TSpeedButton;
    btnTextShapingRTL: TSpeedButton;
    gbxPathsAndEffects: TGroupBox;
    gplPathsAndEffects: TGridPanel;
    btnPathsAndEffectsDiscrete: TSpeedButton;
    btnPathsAndEffectsComposed: TSpeedButton;
    btnPathsAndEffectsSum: TSpeedButton;
    btnPathsAndEffectsShaders: TSpeedButton;
    gbxSVG: TGroupBox;
    gplSVG: TGridPanel;
    btnSVGGorilla: TSpeedButton;
    btnSVGDelphi: TSpeedButton;
    gbxSkottie: TGroupBox;
    gplSkottie: TGridPanel;
    btnSkottieRocket: TSpeedButton;
    btnSkottieCheck: TSpeedButton;
    btnSVGToPDF: TSpeedButton;
    procedure btnPathsAndEffectsComposedClick(Sender: TObject);
    procedure btnPathsAndEffectsDiscreteClick(Sender: TObject);
    procedure btnPathsAndEffectsShadersClick(Sender: TObject);
    procedure btnPathsAndEffectsSumClick(Sender: TObject);
    procedure btnShapesBasicClick(Sender: TObject);
    procedure btnShapesBezierCurvesClick(Sender: TObject);
    procedure btnShapesTranslationsAndRotationsClick(Sender: TObject);
    procedure btnSkottieCheckClick(Sender: TObject);
    procedure btnSkottieRocketClick(Sender: TObject);
    procedure btnSVGDelphiClick(Sender: TObject);
    procedure btnSVGGorillaClick(Sender: TObject);
    procedure btnSVGToPDFClick(Sender: TObject);
    procedure btnTextRenderingClick(Sender: TObject);
    procedure btnTextShapingRTLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDrawProc = reference to procedure (const ACanvas: ISKCanvas);
  procedure DrawOnBitmap(const ABitmap: TWICBitmap; const AProc: TDrawProc);

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  { Sample }
  View.Preview.Bitmap,
  View.Skottie,
  View.SVG;

procedure DrawOnBitmap(const ABitmap: TWICBitmap; const AProc: TDrawProc);
var
  LData: Pointer;
  LStride: Integer;
  LSurface: ISKSurface;
begin
  if ABitmap.LockPixels(LData, LStride) then
  begin
    try
      LSurface := TSKSurface.MakeRasterDirect(TSKImageInfo.Create(ABitmap.Width, ABitmap.Height, TSKColorType.BGRA8888), LData, LStride);
      AProc(LSurface.Canvas);
    finally
      ABitmap.UnlockPixels;
    end;
  end
  else
    raise Exception.Create('Unable to lock pixels');
end;

function GetAssetsPath: string;
begin
  Result := TPath.GetFullPath('..\..\..\..\..\..\assets\samples') + PathDelim;
end;

function GetOutputPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function StarPath: ISKPath;
var
  I: Integer;
  LA: Single;
  LC: Single;
  LPathBuilder: ISKPathBuilder;
  LR: Single;
begin
  LR := 115.2;
  LC := 128.0;
  LPathBuilder := TSKPathBuilder.Create;
  LPathBuilder.MoveTo(LC + LR, LC);
  for I := 1 to 7 do
  begin
    LA := 2.6927937 * I;
    LPathBuilder.LineTo(LC + LR * Cos(LA), LC + LR * Sin(LA));
  end;
  Result := LPathBuilder.Detach;
end;

{ TfrmMain }

procedure TfrmMain.btnPathsAndEffectsComposedClick(Sender: TObject);
var
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LPath: ISKPath;
        LIntervals: TArray<Single>;
        LDashPathEffect: ISKPathEffect;
        LDiscretePathEffect: ISKPathEffect;
      begin
        SetLength(LIntervals, 4);
        LIntervals[0] := 10;
        LIntervals[1] := 5;
        LIntervals[2] := 2;
        LIntervals[3] := 5;
        LDashPathEffect := TSKPathEffect.MakeDash(LIntervals, 0);
        LDiscretePathEffect := TSKPathEffect.MakeDiscrete(10, 4);
        LPaint := TSKPaint.Create;
        LPaint.PathEffect := TSKPathEffect.MakeCompose(LDashPathEffect, LDiscretePathEffect);
        LPaint.Style := TSKPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $ff4285F4;
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
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LPath: ISKPath;
      begin
        LPaint := TSKPaint.Create;
        LPaint.PathEffect := TSKPathEffect.MakeDiscrete(10, 4);
        LPaint.Style := TSKPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $ff4285F4;
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
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LPath: ISKPath;
        LColors: TArray<TAlphaColor>;
      begin
        SetLength(LColors, 2);
        LColors[0] := $ff4285F4;
        LColors[1] := $ff0F9D58;
        LPaint := TSKPaint.Create;
        LPaint.PathEffect := TSKPathEffect.MakeDiscrete(10, 4);
        LPaint.Shader := TSKShader.MakeGradientLinear(TPointF.Create(0, 0), TPointF.Create(256, 256), LColors, TSKTileMode.Clamp);
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
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LPath: ISKPath;
        LIntervals: TArray<Single>;
        LDashPathEffect1: ISKPathEffect;
        LDashPathEffect2: ISKPathEffect;
      begin
        SetLength(LIntervals, 4);
        LIntervals[0] := 10;
        LIntervals[1] := 5;
        LIntervals[2] := 2;
        LIntervals[3] := 5;
        LDashPathEffect1 := TSKPathEffect.MakeDiscrete(10, 4);
        LDashPathEffect2 := TSKPathEffect.MakeDiscrete(10, 4, 1245);
        LPaint := TSKPaint.Create;
        LPaint.PathEffect := TSKPathEffect.MakeSum(LDashPathEffect1, LDashPathEffect2);
        LPaint.Style := TSKPaintStyle.Stroke;
        LPaint.StrokeWidth := 2;
        LPaint.AntiAlias := True;
        LPaint.Color := $ff4285F4;
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
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LOval: ISKRoundRect;
        LPaint: ISKPaint;
        LRect: TRectF;
      begin
        LPaint := TSKPaint.Create;
        LPaint.Style := TSKPaintStyle.Fill;
        LPaint.AntiAlias := True;
        LPaint.StrokeWidth := 4;
        LPaint.Color := $ff4285F4;

        LRect := TRectF.Create(TPointF.Create(10, 10), 100, 160);
        ACanvas.DrawRect(LRect, LPaint);

        LOval := TSKRoundRect.Create;
        LOval.SetOval(LRect);
        LOval.Offset(40, 80);
        LPaint.Color := $ffDB4437;
        ACanvas.DrawRoundRect(LOval, LPaint);

        LPaint.Color := $ff0F9D58;
        ACanvas.DrawCircle(180, 50, 25, LPaint);

        LRect.Offset(80, 50);
        LPaint.Color := $ffF4B400;
        LPaint.Style := TSKPaintStyle.Stroke;
        ACanvas.DrawRoundRect(LRect, 10, 10, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnShapesBezierCurvesClick(Sender: TObject);
var
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LPath: ISKPath;
        LPathBuilder: ISKPathBuilder;
      begin
        LPaint := TSKPaint.Create;
        LPaint.Style := TSKPaintStyle.Stroke;
        LPaint.StrokeWidth := 8;
        LPaint.Color := $ff4285F4;
        LPaint.AntiAlias := True;
        LPaint.StrokeCap := TSKStrokeCap.Round;

        LPathBuilder := TSKPathBuilder.Create;
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
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LPaint: ISKPaint;
        LRect: TRectF;
      begin
        ACanvas.Translate(128, 0);
        ACanvas.Rotate(60);
        LRect := TRectF.Create(TPointF.Create(0, 0), 200, 100);

        LPaint := TSKPaint.Create;
        LPaint.AntiAlias := True;
        LPaint.Color := $ff4285F4;
        ACanvas.DrawRect(LRect, LPaint);

        ACanvas.Rotate(20);
        LPaint.Color := $ffDB4437;
        ACanvas.DrawRect(LRect, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TfrmMain.btnSkottieCheckClick(Sender: TObject);
begin
  frmSkottie.Show(GetAssetsPath + 'check.json');
end;

procedure TfrmMain.btnSkottieRocketClick(Sender: TObject);
begin
  frmSkottie.Show(GetAssetsPath + 'rocket.json');
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
  LCanvas: ISKCanvas;
  LDocument: ISKDocument;
  LDOM: ISKSVGDOM;
  LPDFStream: ISKWStream;
  LSize: TSizeF;
  LSVGStream: ISKStream;
  LPDFFileName: string;
begin
  LSVGStream := TSKFileStream.Create(GetAssetsPath + 'lion.svg');
  LDOM := TSKSVGDOM.Make(LSVGStream);
  LSize := LDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0));

  LPDFFileName := GetOutputPath + 'output.pdf';
  LPDFStream := TSKFileWStream.Create(LPDFFileName);
  LDocument := TSKDocument.MakePDF(LPDFStream);
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
  // PDF Preview
  LPDFFileName := 'file://' + LPDFFileName.Replace('\', '/', [rfReplaceAll]);
  ShellExecute(0, 'open', PChar(LPDFFileName), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.btnTextRenderingClick(Sender: TObject);
var
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LBlob1: ISKTextBlob;
        LBlob2: ISKTextBlob;
        LFont1: ISKFont;
        LFont2: ISKFont;
        LPaint1: ISKPaint;
        LPaint2: ISKPaint;
        LPaint3: ISKPaint;
        LTypeface: ISKTypeface;
      begin
        LTypeface := TSKTypeface.MakeFromName('Monospace', TSKFontStyle.Normal);
        LFont1 := TSKFont.Create(LTypeface, 64, 1);
        LFont2 := TSKFont.Create(LTypeface, 64, 1.5);
        LFont1.Edging := TSKFontEdging.AntiAlias;
        LFont2.Edging := TSKFontEdging.AntiAlias;

        LBlob1 := TSKTextBlob.Make('Skia', LFont1);
        LBlob2 := TSKTextBlob.Make('Skia', LFont2);


        LPaint1 := TSKPaint.Create;
        LPaint1.AntiAlias := True;
        LPaint1.SetARGB($FF, $42, $85, $F4);

        LPaint2 := TSKPaint.Create;
        LPaint2.AntiAlias := True;
        LPaint2.SetARGB($FF, $DB, $44, $37);
        LPaint2.Style := TSKPaintStyle.Stroke;
        LPaint2.StrokeWidth := 3;

        LPaint3 := TSKPaint.Create;
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

procedure TfrmMain.btnTextShapingRTLClick(Sender: TObject);
var
  LBitmap: TWICBitmap;
begin
  LBitmap := TWICBitmap.Create(256, 256);
  try
    DrawOnBitmap(LBitmap,
      procedure (const ACanvas: ISKCanvas)
      var
        LBlob: ISKTextBlob;
        LFont: ISKFont;
        LPaint: ISKPaint;
        LRunHandler: ISKTextBlobBuilderRunHandler;
        LShaper: ISKShaper;
        LText: UTF8String;
      begin
        LFont := TSKFont.Create(TSKTypeface.MakeDefault, 55, 1);
        LText := 'سلام دنیا!';

        LRunHandler := TSKTextBlobBuilderRunHandler.Create(LText, TPointF.Create(0, 0));
        LShaper := TSKShaper.Create;
        LShaper.Shape(LText, LFont, True, MaxSingle, LRunHandler);
        LBlob := LRunHandler.Detach;

        LPaint := TSKPaint.Create;
        LPaint.AntiAlias := True;
        LPaint.Color := TAlphaColors.Tomato;

        ACanvas.DrawTextBlob(LBlob, 0, 0, LPaint);
      end);

    frmBitmapPreview.Show(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

end.
