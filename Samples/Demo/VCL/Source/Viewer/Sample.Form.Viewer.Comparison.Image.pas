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
unit Sample.Form.Viewer.Comparison.Image;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  System.Math.Vectors, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Viewer.PaintBox;

type
  TfrmComparisonImageViewer = class(TfrmPaintBoxViewer)
  public
    procedure Show(const ATitle, ADescription, AImage1Text, AImage2Text: string; const AImageBytes1, AImageBytes2: TBytes); reintroduce;
  end;

implementation

{$R *.dfm}

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
  LFrontPaint.Style := TSkPaintStyle.Fill;

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
begin
  LImage1 := TSkImage.MakeFromEncoded(AImageBytes1);
  LImage2 := TSkImage.MakeFromEncoded(AImageBytes2);
  inherited Show(ATitle, ADescription,
    Round(Max(LImage1.Width, LImage2.Width) / pbxDraw.ScaleFactor),
    Round((LImage1.Height + BitmapsOffset + LImage2.Height) / pbxDraw.ScaleFactor),
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
