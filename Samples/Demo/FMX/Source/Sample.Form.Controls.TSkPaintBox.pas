{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Controls.TSkPaintBox;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkPaintBox = class(TfrmBase)
    btnSweepGradient: TSpeedButton;
    lblSweepGradientTitle: TSkLabel;
    lblSweepGradientDescription: TSkLabel;
    btnFreehand: TSpeedButton;
    lblFreehandTitle: TSkLabel;
    lblFreehandDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnFreehandClick(Sender: TObject);
    procedure btnSweepGradientClick(Sender: TObject);
  private
    procedure OnSweepGradientDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Control;

{$R *.fmx}

type
  IFreehandRender = interface
  end;

  TFreehandRender = class(TInterfacedObject, IFreehandRender)
  strict private
    FCurrentPath: ISkPath;
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

{ TfrmTSkPaintBox }

procedure TfrmTSkPaintBox.btnFreehandClick(Sender: TObject);
begin
  FFreehandRender := TFreehandRender.Create;
  ChildForm<TfrmControlViewer>.Show('Freehand / Signature', 'Touch or click in screen and move to draw',
    function (): TControl
    var
      LPaintBox: TSkPaintBox absolute Result;
    begin
      LPaintBox := TSkPaintBox.Create(nil);
      LPaintBox.Align := TAlignLayout.Client;
      LPaintBox.HitTest := True;
      LPaintBox.AutoCapture := True;
      LPaintBox.OnDraw := TFreehandRender(FFreehandRender).OnDraw;
      LPaintBox.OnMouseDown := TFreehandRender(FFreehandRender).OnMouseDown;
      LPaintBox.OnMouseMove := TFreehandRender(FFreehandRender).OnMouseMove;
      LPaintBox.OnMouseUp := TFreehandRender(FFreehandRender).OnMouseUp;
      LPaintBox.OnMouseLeave := TFreehandRender(FFreehandRender).OnMouseLeave;
    end);
end;

procedure TfrmTSkPaintBox.btnSweepGradientClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Sweep Gradient', '',
    function (): TControl
    var
      LPaintBox: TSkPaintBox absolute Result;
    begin
      LPaintBox := TSkPaintBox.Create(nil);
      LPaintBox.Align := TAlignLayout.Client;
      LPaintBox.OnDraw := OnSweepGradientDraw;
    end);
end;

procedure TfrmTSkPaintBox.OnSweepGradientDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint, [$FFFCE68D, $FFF7CAA5, $FF2EBBC1, $FFFCE68D]);
  ACanvas.DrawPaint(LPaint);
end;

{ TFreehandRender }

procedure TFreehandRender.OnDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  LPaint: ISkPaint;
  LPath: ISkPath;
begin
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
end;

procedure TFreehandRender.OnMouseDown(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Single);
begin
  FPressed := True;
  FPathBuilder := TSkPathBuilder.Create;
  FPathBuilder.MoveTo(X, Y);
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
begin
  if FPressed and Assigned(FPathBuilder) then
  begin
    FCurrentPath := nil;
    FPathBuilder.LineTo(X, Y);
    (ASender as TSkPaintBox).Redraw;
  end;
end;

procedure TFreehandRender.OnMouseUp(ASender: TObject; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Single);
begin
  OnMouseLeave(ASender);
end;

end.
