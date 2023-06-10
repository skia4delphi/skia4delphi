{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Controls.TSkPaintBox;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, System.Types, System.UITypes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkPaintBox = class(TfrmBase)
    pnlFreehand: TPanel;
    lblFreehandDescription: TSkLabel;
    lblFreehandTitle: TSkLabel;
    svgFreehandArrow: TSkSvg;
    pnlFreehandLine: TPanel;
    pnlSweepGradient: TPanel;
    lblSweepGradientDescription: TSkLabel;
    lblSweepGradientTitle: TSkLabel;
    svgSweepGradientArrow: TSkSvg;
    pnlSweepGradientLine: TPanel;
    procedure pnlFreehandClick(Sender: TObject);
    procedure pnlSweepGradientClick(Sender: TObject);
  private
    procedure OnSweepGradientDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Control;

{$R *.dfm}

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
    procedure OnMouseDown(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
    procedure OnMouseLeave(ASender: TObject);
    procedure OnMouseMove(ASender: TObject; AShift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(ASender: TObject; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
  end;

var
  FFreehandRender: IFreehandRender;

{ TfrmTSkPaintBox }

procedure TfrmTSkPaintBox.OnSweepGradientDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint, [$FFFCE68D, $FFF7CAA5, $FF2EBBC1, $FFFCE68D]);
  ACanvas.DrawPaint(LPaint);
end;

procedure TfrmTSkPaintBox.pnlFreehandClick(Sender: TObject);
begin
  FFreehandRender := TFreehandRender.Create;
  ChildForm<TfrmControlViewer>.Show('Freehand / Signature', 'Touch or click in screen and move to draw',
    function (): TControl
    var
      LPaintBox: TSkPaintBox absolute Result;
    begin
      LPaintBox := TSkPaintBox.Create(nil);
      LPaintBox.Align := alClient;
      LPaintBox.OnDraw := TFreehandRender(FFreehandRender).OnDraw;
      LPaintBox.OnMouseDown := TFreehandRender(FFreehandRender).OnMouseDown;
      LPaintBox.OnMouseMove := TFreehandRender(FFreehandRender).OnMouseMove;
      LPaintBox.OnMouseUp := TFreehandRender(FFreehandRender).OnMouseUp;
      LPaintBox.OnMouseLeave := TFreehandRender(FFreehandRender).OnMouseLeave;
    end);
end;

procedure TfrmTSkPaintBox.pnlSweepGradientClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Sweep Gradient', '',
    function (): TControl
    var
      LPaintBox: TSkPaintBox absolute Result;
    begin
      LPaintBox := TSkPaintBox.Create(nil);
      LPaintBox.Align := alClient;
      LPaintBox.OnDraw := OnSweepGradientDraw;
    end);
end;

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
  AShift: TShiftState; X, Y: Integer);
begin
  FPressed := True;
  FPathBuilder := TSkPathBuilder.Create;
  FLastPoint := PointF(X, Y) / (ASender as TSkPaintBox).ScaleFactor;
  FPathBuilder.MoveTo(FLastPoint.X, FLastPoint.Y);
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
  Y: Integer);
const
  MinPointsDistance = 5;
var
  LPoint: TPointF;
begin
  LPoint := PointF(X, Y) / (ASender as TSkPaintBox).ScaleFactor;
  if FPressed and Assigned(FPathBuilder) and (FLastPoint.Distance(LPoint) >= MinPointsDistance) then
  begin
    FCurrentPath := nil;
    FPathBuilder.LineTo(LPoint.X, LPoint.Y);
    FLastPoint := LPoint;
    (ASender as TSkPaintBox).Redraw;
  end;
end;

procedure TFreehandRender.OnMouseUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer);
begin
  OnMouseLeave(ASender);
end;

end.
