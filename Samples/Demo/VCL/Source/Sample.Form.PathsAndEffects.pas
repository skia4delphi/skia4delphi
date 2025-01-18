{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2025 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.PathsAndEffects;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, System.Types, System.UITypes, System.Math,
  System.Math.Vectors, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmPathsAndEffects = class(TfrmBase)
    pnlDiscretePathEffect: TPanel;
    lblDiscretePathEffectDescription: TSkLabel;
    lblDiscretePathEffectTitle: TSkLabel;
    svgDiscretePathEffectArrow: TSkSvg;
    pnlDiscretePathEffectLine: TPanel;
    pnlComposedPathEffect: TPanel;
    lblComposedPathEffectDescription: TSkLabel;
    lblComposedPathEffectTitle: TSkLabel;
    svgComposedPathEffectArrow: TSkSvg;
    pnlComposedPathEffectLine: TPanel;
    pnlSumPathEffect: TPanel;
    lblSumPathEffectDescription: TSkLabel;
    lblSumPathEffectTitle: TSkLabel;
    svgSumPathEffectArrow: TSkSvg;
    pnlSumPathEffectLine: TPanel;
    pnlShaders: TPanel;
    lblShadersDescription: TSkLabel;
    lblShadersTitle: TSkLabel;
    svgShadersArrow: TSkSvg;
    pnlShadersLine: TPanel;
    pnlPathFillTypes: TPanel;
    lblPathFillTypesDescription: TSkLabel;
    lblPathFillTypesTitle: TSkLabel;
    svgPathFillTypesArrow: TSkSvg;
    pnlPathFillTypesLine: TPanel;
    pnlRoundingSharpCorners: TPanel;
    lblRoundingSharpCornersDescription: TSkLabel;
    lblRoundingSharpCornersTitle: TSkLabel;
    svgRoundingSharpCornersArrow: TSkSvg;
    pnlRoundingSharpCornersLine: TPanel;
    procedure pnlComposedPathEffectClick(Sender: TObject);
    procedure pnlDiscretePathEffectClick(Sender: TObject);
    procedure pnlPathFillTypesClick(Sender: TObject);
    procedure pnlRoundingSharpCornersClick(Sender: TObject);
    procedure pnlShadersClick(Sender: TObject);
    procedure pnlSumPathEffectClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.PaintBox;

{$R *.dfm}

function StarPath: ISkPath;
const
  C = 128.0;
  R = 115.2;
var
  I: Integer;
  A: Single;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(C + R, C);
  for I := 1 to 7 do
  begin
    A := 2.6927937 * I;
    LPathBuilder.LineTo(C + R * Cos(A), C + R * Sin(A));
  end;
  Result := LPathBuilder.Detach;
end;

function HeptagonPath(const ABounds: TRectF): ISkPath;
const
  VerticesCount = 7;
var
  LPathBuilder: ISkPathBuilder;
  LPolygon: TPolygon;
  LRadius: Single;
  LVertexAngle: Double;
  I: Integer;
begin
  LRadius := 0.45 * Min(ABounds.Width, ABounds.Height);
  SetLength(LPolygon, VerticesCount);
  LVertexAngle := -0.5 * Pi;
  for I := 0 to Length(LPolygon) - 1 do
  begin
    LPolygon[I] := PointF(LRadius * Cos(LVertexAngle), LRadius * Sin(LVertexAngle));
    LVertexAngle := LVertexAngle + (2 * Pi / VerticesCount);
  end;

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddPolygon(LPolygon, True);
  LPathBuilder.Offset(ABounds.CenterPoint.X, ABounds.CenterPoint.Y);
  Result := LPathBuilder.Detach;
end;

procedure TfrmPathsAndEffects.pnlComposedPathEffectClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Composed Path Effect', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LDashPathEffect: ISkPathEffect;
      LDiscretePathEffect: ISkPathEffect;
    begin
      LDashPathEffect := TSkPathEffect.MakeDash([10, 5, 2, 5], 0);
      LDiscretePathEffect := TSkPathEffect.MakeDiscrete(10, 4);
      LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
      LPaint.PathEffect := TSkPathEffect.MakeCompose(LDashPathEffect, LDiscretePathEffect);
      LPaint.StrokeWidth := 2;
      LPaint.AntiAlias := True;
      LPaint.Color := $FF4285F4;
      ACanvas.DrawPath(StarPath, LPaint);
    end);
end;

procedure TfrmPathsAndEffects.pnlDiscretePathEffectClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Discrete Path Effect', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
    begin
      LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
      LPaint.PathEffect := TSkPathEffect.MakeDiscrete(10, 4);
      LPaint.StrokeWidth := 2;
      LPaint.AntiAlias := True;
      LPaint.Color := $FF4285F4;
      ACanvas.DrawPath(StarPath, LPaint);
    end);
end;

procedure TfrmPathsAndEffects.pnlPathFillTypesClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Path Fill Types', 'Example with EvenOdd fill type',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPath: ISkPath;
      LPathBuilder: ISkPathBuilder;
      LRadius: Single;
      LPaint: ISkPaint;
    begin
      LRadius := Min(ADest.Width, ADest.Height) / 4;

      LPathBuilder := TSkPathBuilder.Create(TSkPathFillType.EvenOdd);
      LPathBuilder.AddCircle(ADest.CenterPoint.X - LRadius / 2, ADest.CenterPoint.Y - LRadius / 2, LRadius);
      LPathBuilder.AddCircle(ADest.CenterPoint.X - LRadius / 2, ADest.CenterPoint.Y + LRadius / 2, LRadius);
      LPathBuilder.AddCircle(ADest.CenterPoint.X + LRadius / 2, ADest.CenterPoint.Y - LRadius / 2, LRadius);
      LPathBuilder.AddCircle(ADest.CenterPoint.X + LRadius / 2, ADest.CenterPoint.Y + LRadius / 2, LRadius);
      LPath := LPathBuilder.Detach;

      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.Color := TAlphaColors.Cadetblue;
      ACanvas.DrawPath(LPath, LPaint);

      LPaint.Style := TSkPaintStyle.Stroke;
      LPaint.StrokeWidth := 8;
      LPaint.Color := TAlphaColors.Chocolate;
      ACanvas.DrawPath(LPath, LPaint);
    end);
end;

procedure TfrmPathsAndEffects.pnlRoundingSharpCornersClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Corner radius', 0, 1, 0.3);

  ChildForm<TfrmPaintBoxViewer>.Show('Rounding Sharp Corners', '',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
    begin
      LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
      LPaint.Color := TAlphaColors.Indigo;
      LPaint.StrokeWidth := 6;
      LPaint.AntiAlias := True;
      LPaint.PathEffect := TSkPathEffect.MakeCorner(LOptions['Corner radius'] * Min(ADest.Width, ADest.Height) / 4);
      ACanvas.DrawPath(HeptagonPath(ADest), LPaint);
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmPathsAndEffects.pnlShadersClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Shaders', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
    begin
      LPaint := TSkPaint.Create;
      LPaint.PathEffect := TSkPathEffect.MakeDiscrete(10, 4);
      LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(256, 256), $FF4285F4, $FF0F9D58, TSkTileMode.Clamp);
      LPaint.AntiAlias := True;
      ACanvas.DrawPath(StarPath, LPaint);
    end);
end;

procedure TfrmPathsAndEffects.pnlSumPathEffectClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Sum Path Effect', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LDashPathEffect1: ISkPathEffect;
      LDashPathEffect2: ISkPathEffect;
    begin
      LDashPathEffect1 := TSkPathEffect.MakeDiscrete(10, 4);
      LDashPathEffect2 := TSkPathEffect.MakeDiscrete(10, 4, 1245);
      LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
      LPaint.PathEffect := TSkPathEffect.MakeSum(LDashPathEffect1, LDashPathEffect2);
      LPaint.StrokeWidth := 2;
      LPaint.AntiAlias := True;
      LPaint.Color := $FF4285F4;
      ACanvas.DrawPath(StarPath, LPaint);
    end);
end;

end.
