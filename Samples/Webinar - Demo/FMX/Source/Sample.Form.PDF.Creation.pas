unit Sample.Form.PDF.Creation;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts,

  { Skia }
  Skia, Skia.FMX;

type
  { TfrmPDFCreation }

  TfrmPDFCreation = class(TForm)
    rctHeader: TRectangle;
    SkLabel1: TSkLabel;
    rctPage: TRectangle;
    btnBack: TSpeedButton;
    svgBackArrow: TSkSvg;
    ShadowEffect1: TShadowEffect;
    SkLabel2: TSkLabel;
    SkLabel3: TSkLabel;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    SkLabel4: TSkLabel;
    btnGeneratePDF: TSpeedButton;
    ShadowEffect2: TShadowEffect;
    saiSignature: TSkAnimatedImage;
    pbxSignature: TSkPaintBox;
    lblSignature: TSkLabel;
    lytSignature: TLayout;
    lytPageContent: TLayout;
    procedure btnBackClick(Sender: TObject);
    procedure btnGeneratePDFClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbxSignatureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  public
    { Public declarations }
  end;

var
  frmPDFCreation: TfrmPDFCreation;

implementation

{$R *.fmx}

uses
  { Sample }
  Sample.Form.Main,
  Sample.Form.PDF.Viewer;

{$REGION ' - Freehand draw'}
type
  IFreehandRender = interface
  end;

  TFreehandRender = class(TInterfacedObject, IFreehandRender)
  strict private
    FCurrentPath: ISkPath;
    FLastPoint: TPointF;
    FOldPaths: TArray<ISkPath>;
    FPathBuilder: ISkPathBuilder;
    FPressed: Boolean;
  public
    procedure OnDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure OnMouseDown(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; X, Y: Single);
    procedure OnMouseLeave(ASender: TObject);
    procedure OnMouseMove(ASender: TObject; AShift: TShiftState; X, Y: Single);
    procedure OnMouseUp(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; X, Y: Single);
  end;

var
  FFreehandRender: IFreehandRender;

{ TFreehandRender }

procedure TFreehandRender.OnDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  LPaint: ISkPaint;
  LPath: ISkPath;
begin
  ACanvas.Save;
  try
    ACanvas.ClipRect(ADest);
    LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
    LPaint.AntiAlias := True;
    LPaint.Color := TAlphaColors.Royalblue;
    LPaint.SetPathEffect(TSkPathEffect.MakeCorner(50));
    LPaint.StrokeCap := TSkStrokeCap.Round;
    LPaint.StrokeWidth := 4;

    for LPath in FOldPaths do
      ACanvas.DrawPath(LPath, LPaint);
    if Assigned(FPathBuilder) and not Assigned(FCurrentPath) then
      FCurrentPath := FPathBuilder.Snapshot;
    if Assigned(FCurrentPath) then
      ACanvas.DrawPath(FCurrentPath, LPaint);
  finally
    ACanvas.Restore;
  end;
end;

procedure TFreehandRender.OnMouseDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Single);
begin
  FPressed := True;
  FPathBuilder := TSkPathBuilder.Create;
  FPathBuilder.MoveTo(X, Y);
  FLastPoint := PointF(X, Y);
  FCurrentPath := nil;
end;

procedure TFreehandRender.OnMouseLeave(ASender: TObject);
begin
  if Assigned(FPathBuilder) then
  begin
    if FCurrentPath = nil then
      FCurrentPath := FPathBuilder.Snapshot;
    FOldPaths := FOldPaths + [FCurrentPath];
    FPathBuilder := nil;
    FCurrentPath := nil;
  end;
end;

procedure TFreehandRender.OnMouseMove(ASender: TObject; AShift: TShiftState; X,
  Y: Single);
const
  MinPointsDistance = 5;
begin
  if FPressed and Assigned(FPathBuilder) and (FLastPoint.Distance(PointF(X, Y)) >= MinPointsDistance) then
  begin
    FCurrentPath := nil;
    FPathBuilder.LineTo(X, Y);
    FLastPoint := PointF(X, Y);
    (ASender as TSkPaintBox).Redraw;
  end;
end;

procedure TFreehandRender.OnMouseUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Single);
begin
  OnMouseLeave(ASender);
end;
{$ENDREGION}

{ TfrmPDFCreation }

procedure TfrmPDFCreation.btnBackClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPDFCreation.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
  pbxSignature.AutoCapture := True;
end;

procedure TfrmPDFCreation.FormShow(Sender: TObject);
begin
  FFreehandRender := TFreehandRender.Create;
  pbxSignature.OnDraw := TFreehandRender(FFreehandRender).OnDraw;
  pbxSignature.OnMouseMove := TFreehandRender(FFreehandRender).OnMouseMove;
  pbxSignature.OnMouseUp := TFreehandRender(FFreehandRender).OnMouseUp;
  pbxSignature.OnMouseLeave := TFreehandRender(FFreehandRender).OnMouseLeave;
  saiSignature.Visible := True;
  pbxSignature.Redraw;
end;

procedure TfrmPDFCreation.pbxSignatureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  saiSignature.Visible := False;
  TFreehandRender(FFreehandRender).OnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TfrmPDFCreation.btnGeneratePDFClick(Sender: TObject);

  function GetOutputPath: string;
  begin
    {$IF DEFINED(MACOS) and NOT DEFINED(IOS)}
    Result := TPath.GetTempPath;
    {$ELSEIF DEFINED(IOS) or DEFINED(ANDROID)}
    Result := TPath.GetDocumentsPath;
    {$ELSE}
    Result := ExtractFilePath(ParamStr(0));
    {$ENDIF}
    if (Result <> '') and not Result.EndsWith(PathDelim) then
      Result := Result + PathDelim;
  end;

  procedure ControlToPDF(AControl: TControl; const AOutputFileName: string);
  var
    LStream: TMemoryStream;
    LDocument: ISkDocument;
    LCanvas: ISkCanvas;
  begin
    LStream := TMemoryStream.Create;
    try
      LDocument := TSkDocument.MakePDF(LStream);
      LCanvas := LDocument.BeginPage(AControl.Width, AControl.Height);
      try
        LCanvas.Clear(TAlphaColors.Null);
        AControl.PaintTo(LCanvas, AControl.LocalRect, nil);
      finally
        LDocument.EndPage;
      end;
      LDocument.Close;
      LStream.SaveToFile(AOutputFileName);
    finally
      LStream.Free;
    end;
  end;

var
  LOutputFileName: string;
begin
  LOutputFileName := GetOutputPath + 'output.pdf';
  saiSignature.Visible := False;
  ControlToPDF(lytPageContent, LOutputFileName);
  frmPDFViewer.Show(LOutputFileName);
  {$IFDEF MACOS}
  Close;
  Showmessage('PDF generated successfully!');
  {$ENDIF}
end;

end.
