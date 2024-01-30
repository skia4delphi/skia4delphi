{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Basics;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.Classes, System.Math,
  System.Math.Vectors, FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmBasics = class(TfrmBase)
    btnTransformations: TSpeedButton;
    lblTransformationsTitle: TSkLabel;
    lblTransformationsDescription: TSkLabel;
    btnCurves: TSpeedButton;
    lblCurvesTitle: TSkLabel;
    lblCurvesDescription: TSkLabel;
    btnEllipsesAndRectangles: TSpeedButton;
    lblEllipsesAndRectanglesTitle: TSkLabel;
    lblEllipsesAndRectanglesDescription: TSkLabel;
    btnHatching: TSpeedButton;
    lblHatchingTitle: TSkLabel;
    lblHatchingDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnCurvesClick(Sender: TObject);
    procedure btnEllipsesAndRectanglesClick(Sender: TObject);
    procedure btnHatchingClick(Sender: TObject);
    procedure btnTransformationsClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.PaintBox;

{$R *.fmx}

procedure TfrmBasics.btnCurvesClick(Sender: TObject);
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

procedure TfrmBasics.btnEllipsesAndRectanglesClick(Sender: TObject);
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

procedure TfrmBasics.btnHatchingClick(Sender: TObject);
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

procedure TfrmBasics.btnTransformationsClick(Sender: TObject);
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
