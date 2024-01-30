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
unit Sample.Form.Viewer.Comparison.Image;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.Math.Vectors, FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Viewer.PaintBox;

type
  TfrmComparisonImageViewer = class(TfrmPaintBoxViewer)
  public
    procedure Show(const ATitle, ADescription, AImage1Text, AImage2Text: string; const AImageBytes1, AImageBytes2: TBytes); reintroduce;
  end;

implementation

{$R *.fmx}

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

  LFrontPaint := TSkPaint.Create;
  LFrontPaint.Color := TAlphaColors.White;

  ACanvas.DrawImage(AImage, 0, 0);
  ACanvas.DrawRect(RectF(0, 0, AImage.Width, TitleHeight), LBackPaint);
  LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
  LFont := TSkFont.Create(LTypeface, TextSize, 1);

  ACanvas.DrawSimpleText(AText, TitleX, TitleY, LFont, LFrontPaint);
end;

{ TfrmComparisonImageViewer }

procedure TfrmComparisonImageViewer.Show(const ATitle, ADescription,
  AImage1Text, AImage2Text: string; const AImageBytes1, AImageBytes2: TBytes);
const
  BitmapsOffset = 20;
var
  LImage1: ISkImage;
  LImage2: ISkImage;
  LRealScale: TPointF;
begin
  LImage1 := TSkImage.MakeFromEncoded(AImageBytes1);
  LImage2 := TSkImage.MakeFromEncoded(AImageBytes2);
  LRealScale := pbxDraw.AbsoluteScale;
  if Assigned(pbxDraw.Scene) then
    LRealScale := LRealScale * pbxDraw.Scene.GetSceneScale;
  inherited Show(ATitle, ADescription, Max(LImage1.Width, LImage2.Width) / LRealScale.X,
    (LImage1.Height + BitmapsOffset + LImage2.Height) / LRealScale.Y,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LScale: TPointF;
    begin
      LScale := ACanvas.GetLocalToDeviceAs3x3.ExtractScale;
      ACanvas.Scale(1/LScale.X, 1/LScale.Y);
      DoDraw(ACanvas, LImage1, AImage1Text);
      ACanvas.Translate(0, LImage1.Height + BitmapsOffset);
      DoDraw(ACanvas, LImage2, AImage2Text);
    end);
end;

end.
