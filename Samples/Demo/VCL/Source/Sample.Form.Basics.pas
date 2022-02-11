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
unit Sample.Form.Basics;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, System.Types, System.Math, System.Math.Vectors, Vcl.Forms,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmBasics = class(TfrmBase)
    pnlEllipsesAndRectangles: TPanel;
    lblEllipsesAndRectanglesDescription: TSkLabel;
    lblEllipsesAndRectanglesTitle: TSkLabel;
    svgEllipsesAndRectanglesArrow: TSkSvg;
    pnlEllipsesAndRectanglesLine: TPanel;
    pnlCurves: TPanel;
    lblCurvesDescription: TSkLabel;
    lblCurvesTitle: TSkLabel;
    svgCurvesArrow: TSkSvg;
    pnlCurvesLine: TPanel;
    pnlTransformations: TPanel;
    lblTransformationsDescription: TSkLabel;
    lblTransformationsTitle: TSkLabel;
    svgTransformationsArrow: TSkSvg;
    pnlTransformationsLine: TPanel;
    pnlHatching: TPanel;
    lblHatchingDescription: TSkLabel;
    lblHatchingTitle: TSkLabel;
    svgHatchingArrow: TSkSvg;
    pnlHatchingLine: TPanel;
    procedure pnlCurvesClick(Sender: TObject);
    procedure pnlEllipsesAndRectanglesClick(Sender: TObject);
    procedure pnlHatchingClick(Sender: TObject);
    procedure pnlTransformationsClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.PaintBox;

{$R *.dfm}

procedure TfrmBasics.pnlCurvesClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Curves', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LPath: ISkPath;
      LPathBuilder: ISkPathBuilder;
    begin
      LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
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
end;

procedure TfrmBasics.pnlEllipsesAndRectanglesClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Ellipses & Rectangles', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LOval: ISkRoundRect;
      LPaint: ISkPaint;
      LRect: TRectF;
    begin
      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;

      LPaint.Color := $FF4285F4;
      LRect := TRectF.Create(PointF(10, 10), 100, 160);
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
      LPaint.StrokeWidth := 4;
      ACanvas.DrawRoundRect(LRect, 10, 10, LPaint);
    end);
end;

procedure TfrmBasics.pnlHatchingClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Hatching', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    const
      LinesDegree = 45;
      LinesDistance = 8;
      LineSize = 1;
    var
      LPaint: ISkPaint;
      LLattice: TMatrix;
      LDest: TRectF;
    begin
      LLattice := TMatrix.CreateRotation(DegToRad(LinesDegree)) * TMatrix.CreateScaling(LinesDistance, LinesDistance);
      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.PathEffect := TSkPathEffect.Make2DLine(LineSize, LLattice);
      LDest := ADest;
      ACanvas.Save;
      try
        ACanvas.ClipRect(LDest);
        LDest.Inflate(LinesDistance, LinesDistance);
        ACanvas.DrawRect(LDest, LPaint);
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Solid);
end;

procedure TfrmBasics.pnlTransformationsClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Translations & Rotations', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LRect: TRectF;
    begin
      ACanvas.Translate(128, 0);
      ACanvas.Rotate(60);
      LRect := RectF(0, 0, 200, 100);

      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.Color := $FF4285F4;
      ACanvas.DrawRect(LRect, LPaint);

      ACanvas.Rotate(20);
      LPaint.Color := $FFDB4437;
      ACanvas.DrawRect(LRect, LPaint);
    end);
end;

end.
