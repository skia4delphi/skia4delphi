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
unit Sample.Form.Transforms;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.Math.Vectors, System.Rtti, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmTransforms = class(TfrmBase)
    btn3DRotation: TSpeedButton;
    lbl3DRotationTitle: TSkLabel;
    lbl3DRotationDescription: TSkLabel;
    btnStrechedCorners: TSpeedButton;
    lblStrechedCornersTitle: TSkLabel;
    lblStrechedCornersDescription: TSkLabel;
    btnTaperTransform: TSpeedButton;
    lblTaperTransformTitle: TSkLabel;
    lblTaperTransformDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btn3DRotationClick(Sender: TObject);
    procedure btnStrechedCornersClick(Sender: TObject);
    procedure btnTaperTransformClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.PaintBox;

{$R *.fmx}

type
  TTaperCorner = (LeftOrTop, RightOrBottom, Both);

  TMatrixHelper = record helper for TMatrix
  strict private
    function GetValue(const AIndex: Integer): Single;
    procedure SetValue(const AIndex: Integer; const AValue: Single);
  public
    const Identity: TMatrix = (m11: 1; m12: 0; m13: 0; m21: 0; m22: 1; m23: 0; m31: 0; m32: 0; m33: 1);
    class function CreateFromMatrix3D(const AMatrix3D: TMatrix3D): TMatrix; static;
    property Persp0: Single index 2 read GetValue write SetValue;
    property Persp1: Single index 5 read GetValue write SetValue;
    property Persp2: Single index 8 read GetValue write SetValue;
    property ScaleX: Single index 0 read GetValue write SetValue;
    property ScaleY: Single index 4 read GetValue write SetValue;
    property SkewX: Single index 3 read GetValue write SetValue;
    property SkewY: Single index 1 read GetValue write SetValue;
    property TransX: Single index 6 read GetValue write SetValue;
    property TransY: Single index 7 read GetValue write SetValue;
  end;

{ TMatrixHelper }

// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a TMatrix3D to TMatrix
// -   conversion. The TMatrix3D.ToMatrix method results in some wrong values.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-36958
// -
// - ---------------------------------------------------------------------------
{$IF CompilerVersion > 35}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
class function TMatrixHelper.CreateFromMatrix3D(
  const AMatrix3D: TMatrix3D): TMatrix;
begin
  Result.M11 := AMatrix3D.M11;
  Result.M21 := AMatrix3D.M21;
  Result.M31 := AMatrix3D.M41;
  Result.M12 := AMatrix3D.M12;
  Result.M22 := AMatrix3D.M22;
  Result.M32 := AMatrix3D.M42;
  Result.M13 := AMatrix3D.M14;
  Result.M23 := AMatrix3D.M24;
  Result.M33 := AMatrix3D.M44;
end;
// - ---------------------------------------------------------------------------

function TMatrixHelper.GetValue(const AIndex: Integer): Single;
begin
  Result := M[AIndex div 3].V[AIndex mod 3];
end;

procedure TMatrixHelper.SetValue(const AIndex: Integer; const AValue: Single);
begin
  M[AIndex div 3].V[AIndex mod 3] := AValue;
end;

function TaperTransform(const ASize: TSizeF; const ATaperSide: TSide;
  const ATaperCorner: TTaperCorner; const ATaperFraction: Single): TMatrix;
begin
  Result := TMatrix.Identity;
  case ATaperSide of
    TSide.Left:
      begin
        Result.ScaleX := ATaperFraction;
        Result.ScaleY := ATaperFraction;
        Result.Persp0 := (ATaperFraction - 1) / ASize.Width;

        case ATaperCorner of
          TTaperCorner.LeftOrTop:
            begin
              Result.SkewY := ASize.Height * Result.Persp0;
              Result.TransY := ASize.Height * (1 - ATaperFraction);
            end;
          TTaperCorner.RightOrBottom: ;
          TTaperCorner.Both:
            begin
              Result.SkewY := (ASize.Height / 2) * Result.Persp0;
              Result.TransY := ASize.Height * (1 - ATaperFraction) / 2;
            end;
        end;
      end;
    TSide.Top:
      begin
        Result.ScaleX := ATaperFraction;
        Result.ScaleY := ATaperFraction;
        Result.Persp1 := (ATaperFraction - 1) / ASize.Height;

        case ATaperCorner of
          TTaperCorner.LeftOrTop:
            begin
              Result.SkewX := ASize.Width * Result.Persp1;
              Result.TransX := ASize.Width * (1 - ATaperFraction);
            end;
          TTaperCorner.RightOrBottom: ;
          TTaperCorner.Both:
            begin
              Result.SkewX := (ASize.Width / 2) * Result.Persp1;
              Result.TransX := ASize.Width * (1 - ATaperFraction) / 2;
            end;
        end;
      end;
    TSide.Right:
      begin
        Result.ScaleX := 1 / ATaperFraction;
        Result.Persp0 := (1 - ATaperFraction) / (ASize.Width * ATaperFraction);

        case ATaperCorner of
          TTaperCorner.LeftOrTop: Result.SkewY := ASize.Height * Result.Persp0;
          TTaperCorner.RightOrBottom: ;
          TTaperCorner.Both: Result.SkewY := (ASize.Height / 2) * Result.Persp0;
        end;
      end;
    TSide.Bottom:
      begin
        Result.ScaleY := 1 / ATaperFraction;
        Result.Persp1 := (1 - ATaperFraction) / (ASize.Height * ATaperFraction);

        case ATaperCorner of
          TTaperCorner.LeftOrTop: Result.SkewX := ASize.Width * Result.Persp1;
          TTaperCorner.RightOrBottom: ;
          TTaperCorner.Both: Result.SkewX := (ASize.Width / 2) * Result.Persp1;
        end;
      end;
  end;
end;

{ TfrmTransforms }

procedure TfrmTransforms.btn3DRotationClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('X-Axis Rotation', 0, 360, 0);
  LOptions.AddFloat('Y-Axis Rotation', 0, 360, 45);
  LOptions.AddFloat('Z-Axis Rotation', 0, 360, 0);
  LOptions.AddFloat('Depth', 250, 2500, 650);

  ChildForm<TfrmPaintBoxViewer>.Show('3D Rotation', '',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    const
      MarginProportion = 0.3;
    var
      LMatrix: TMatrix;
      LMatrix3D: TMatrix3D;
      LPerspectiveMatrix3D: TMatrix3D;
      LImage: ISkImage;
      LRatio: Single;
    begin
      if Sign(Cos(DegToRad(LOptions['X-Axis Rotation'])) * Cos(DegToRad(LOptions['Y-Axis Rotation']))) = NegativeValue then
        LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'deck-card-back.png')
      else
        LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'deck-card-front.png');
      RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest, LRatio);
      LRatio := LRatio * (1 + MarginProportion);

      ACanvas.Save;
      try
        LMatrix3D := TMatrix3D.CreateRotationX(DegToRad(LOptions['X-Axis Rotation']))
          * TMatrix3D.CreateRotationY(DegToRad(LOptions['Y-Axis Rotation']))
          * TMatrix3D.CreateRotationZ(DegToRad(LOptions['Z-Axis Rotation']));
        LPerspectiveMatrix3D := TMatrix3D.Identity;
        LPerspectiveMatrix3D.m34 := -1 / LOptions['Depth'];
        LMatrix := TMatrix.CreateTranslation(-ADest.CenterPoint.X, -ADest.CenterPoint.Y) * TMatrix.CreateScaling(1 / LRatio, 1 / LRatio);
        LMatrix := LMatrix * TMatrix.CreateFromMatrix3D(LMatrix3D * LPerspectiveMatrix3D);
        ACanvas.Concat(LMatrix * TMatrix.CreateTranslation(ADest.CenterPoint.X, ADest.CenterPoint.Y));

        ACanvas.DrawImage(LImage, ADest.CenterPoint.X - (LImage.Width / 2), ADest.CenterPoint.Y - (LImage.Height / 2),
          TSkSamplingOptions.Create(TSkCubicResampler.Mitchell));
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Chess, LOptions);
end;

function ComputeMatrix(const ASize: TSizeF; const ATopLeft, ATopRight,
  ABottomRight, ABottomLeft: TPointF): TMatrix;
var
  S, A, N: TMatrix;
  LPoint: TPointF;
begin
  S := TMatrix.CreateScaling(1 / ASize.Width, 1 / ASize.Height);

  // Affine transform
  A := TMatrix.Identity;
  A.ScaleX := ATopRight.X - ATopLeft.X;
  A.SkewY := ATopRight.Y - ATopLeft.Y;
  A.SkewX := ABottomLeft.X - ATopLeft.X;
  A.ScaleY := ABottomLeft.Y - ATopLeft.Y;
  A.TransX := ATopLeft.X;
  A.TransY := ATopLeft.Y;

  // Non-Affine transform
  LPoint := ABottomRight * A.Inverse;
  N := TMatrix.Identity;
  N.ScaleX := LPoint.X / (LPoint.X + LPoint.Y - 1);
  N.ScaleY := LPoint.Y / (LPoint.X + LPoint.Y - 1);
  N.Persp0 := N.ScaleX - 1;
  N.Persp1 := N.ScaleY - 1;

  // Multiply S * N * A
  Result := TMatrix.Identity;
  Result := Result * S;
  Result := Result * N;
  Result := Result * A;
end;

procedure TfrmTransforms.btnStrechedCornersClick(Sender: TObject);
const
  CornerButtonSize = 20;
  TouchExpansion = 5;
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions['IsFirstDraw'] := True;
  LOptions['TouchCornerDraggingIndex'] := -1;

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure (const APoint: TPointF; var AShouldRedraw: Boolean)
    var
      I: Integer;
    begin
      for I := Low(LOptions.Value['TouchCorners'].AsType<TCornersF>) to High(LOptions.Value['TouchCorners'].AsType<TCornersF>) do
      begin
        if LOptions.Value['TouchCorners'].AsType<TCornersF>[I].Distance(APoint) <= (CornerButtonSize / 2) + TouchExpansion then
        begin
          AShouldRedraw := True;
          LOptions['TouchCornerDraggingIndex'] := I;
          Exit;
        end;
      end;
      LOptions['TouchCornerDraggingIndex'] := -1;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseMove :=
    procedure (const APoint: TPointF; const AIsMouseDown: Boolean; var AShouldRedraw: Boolean)
    var
      LCorners: TCornersF;
    begin
      if LOptions['TouchCornerDraggingIndex'] <> -1 then
      begin
        LCorners := LOptions.Value['TouchCorners'].AsType<TCornersF>;
        LCorners[Integer(LOptions['TouchCornerDraggingIndex'])] := APoint;
        LOptions.Value['TouchCorners'] := TValue.From(LCorners);
        AShouldRedraw := True;
      end;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure (const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      AShouldRedraw := LOptions['TouchCornerDraggingIndex'] <> -1;
      LOptions['TouchCornerDraggingIndex'] := -1;
    end;

  // Draw method
  ChildForm<TfrmPaintBoxViewer>.Show('Streched Corners', 'Click and drag the corners',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    const
      CornerButtonColor: array[Boolean] of TAlphaColor = ($7F365FF4, $9FFF0000);
    var
      LCorners: TCornersF;
      LDest: TRectF;
      LImage: ISkImage;
      LPaint: ISkPaint;
      I: Integer;
    begin
      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'toy-story.jpg');
      LDest := ADest;
      LDest.Inflate(-50, -50);
      if LOptions['IsFirstDraw'] then
      begin
        LCorners := CornersF(LDest);
        // Changing the top-left corner initial value
        LCorners[0] := LCorners[0] + TPointF(LDest.Size) * 0.25;
        LOptions.Value['TouchCorners'] := TValue.From(LCorners);
        LOptions['IsFirstDraw'] := False;
      end
      else
        LCorners := LOptions.Value['TouchCorners'].AsType<TCornersF>;

      // Draw the image transformed
      ACanvas.Save;
      try
        ACanvas.Concat(ComputeMatrix(LDest.Size, LCorners[0], LCorners[1], LCorners[2], LCorners[3]));
        ACanvas.Translate(-LDest.Left, -LDest.Top);
        ACanvas.DrawImageRect(LImage, LDest, TSkSamplingOptions.Create(TSkCubicResampler.Mitchell));
      finally
        ACanvas.Restore;
      end;

      // Draw corner buttons
      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      for I := Low(LCorners) to High(LCorners) do
      begin
        LPaint.Color := CornerButtonColor[I = LOptions['TouchCornerDraggingIndex']];
        ACanvas.DrawCircle(LCorners[I], CornerButtonSize / 2, LPaint);
      end;
    end);
end;

procedure TfrmTransforms.btnTaperTransformClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Fraction', 0.01, 1, 0.3);
  LOptions.AddEnum('Side', TypeInfo(TSide), TSide.Right);
  LOptions.AddEnum('Corner', TypeInfo(TTaperCorner), TTaperCorner.Both);

  ChildForm<TfrmPaintBoxViewer>.Show('Taper Transform', '',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LDest: TRectF;
      LMatrix: TMatrix;
      LImage: ISkImage;
    begin
      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'simpsons.webp');
      LDest := ADest;
      LDest.Inflate(-50, -50);

      ACanvas.Save;
      try
        LMatrix := TMatrix.CreateTranslation(-LDest.Left, -LDest.Top);
        LMatrix := LMatrix * TaperTransform(LDest.Size, LOptions['Side'], LOptions['Corner'], LOptions['Fraction']);
        LMatrix := LMatrix * TMatrix.CreateTranslation(LDest.Left, LDest.Top);
        ACanvas.Concat(LMatrix);
        ACanvas.DrawImageRect(LImage, LDest, TSkSamplingOptions.Create(TSkCubicResampler.Mitchell));
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Chess, LOptions);
end;

end.
