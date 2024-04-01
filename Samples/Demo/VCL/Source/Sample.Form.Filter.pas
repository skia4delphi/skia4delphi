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
unit Sample.Form.Filter;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.Math,
  System.Math.Vectors, System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmFilter = class(TfrmBase)
    pnlVignette: TPanel;
    lblVignetteDescription: TSkLabel;
    lblVignetteTitle: TSkLabel;
    svgVignetteArrow: TSkSvg;
    pnlVignetteLine: TPanel;
    pnlHueSaturation: TPanel;
    lblHueSaturationDescription: TSkLabel;
    lblHueSaturationTitle: TSkLabel;
    svgHueSaturationArrow: TSkSvg;
    pnlHueSaturationLine: TPanel;
    pnlCommonColorFilter: TPanel;
    lblCommonColorFilterDescription: TSkLabel;
    lblCommonColorFilterTitle: TSkLabel;
    svgCommonColorFilterArrow: TSkSvg;
    pnlCommonColorFilterLine: TPanel;
    pnlBrightnessContrast: TPanel;
    lblBrightnessContrastDescription: TSkLabel;
    lblBrightnessContrastTitle: TSkLabel;
    svgBrightnessContrastArrow: TSkSvg;
    pnlBrightnessContrastLine: TPanel;
    pnlDropShadow: TPanel;
    lblDropShadowDescription: TSkLabel;
    lblDropShadowTitle: TSkLabel;
    svgDropShadowArrow: TSkSvg;
    pnlDropShadowLine: TPanel;
    pnlLightingEffects: TPanel;
    lblLightingEffectsDescription: TSkLabel;
    lblLightingEffectsTitle: TSkLabel;
    svgLightingEffectsArrow: TSkSvg;
    pnlLightingEffectsLine: TPanel;
    procedure pnlBrightnessContrastClick(Sender: TObject);
    procedure pnlCommonColorFilterClick(Sender: TObject);
    procedure pnlDropShadowClick(Sender: TObject);
    procedure pnlHueSaturationClick(Sender: TObject);
    procedure pnlLightingEffectsClick(Sender: TObject);
    procedure pnlVignetteClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.PaintBox;

{$R *.dfm}

const
  PhotoFileName = 'photo.webp';

procedure TfrmFilter.pnlBrightnessContrastClick(Sender: TObject);
(* Using SkColorFilter is the simplest way of this example, but using shaders is the fastest way, so here
  we've added the "Brightness & Contrast" example using both, but in the demo we are using the faster way (shaders).
  Example of brightness and contrast using SkColorFilter:

var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Brightness', -1, 1, 0, 0.01);
  LOptions.AddFloat('Contrast', -1, 1, 0, 0.01);
  LOptions['IsMouseDown'] := False;

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Brightness & Contrast', 'Press the photo to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImage: ISkImage;
      LImageDest: TRectF;
      LPaint: ISkPaint;
      LContrastFilter: ISkColorFilter;
      LBrightnessFilter: ISkColorFilter;
      LBrightnessMatrix: TSkColorMatrix;
    begin
      LBrightnessMatrix := TSkColorMatrix.Identity;
      LBrightnessMatrix.M15 := LOptions['Brightness'];
      LBrightnessMatrix.M25 := LBrightnessMatrix.M15;
      LBrightnessMatrix.M35 := LBrightnessMatrix.M15;
      LBrightnessFilter := TSkColorFilter.MakeMatrix(LBrightnessMatrix);
      LContrastFilter := TSkColorFilter.MakeHighContrast(TSkHighContrastConfig.Create(False, TSkContrastInvertStyle.NoInvert, LOptions['Contrast']));

      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + PhotoFileName);
      LImageDest := RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest);
      LPaint := TSkPaint.Create;
      if LOptions['IsMouseDown'] = False then
        LPaint.ColorFilter := TSkColorFilter.MakeCompose(LContrastFilter, LBrightnessFilter);
      ACanvas.DrawImageRect(LImage, LImageDest, TSkSamplingOptions.Medium, LPaint);
    end, TBackgroundKind.Chess, LOptions);
end; *)
var
  LOptions: IViewerOptions;
  LImage: ISkImage;
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Brightness', -1, 1, 0, 0.01);
  LOptions.AddFloat('Contrast', -1, 1, 0, 0.01);
  LOptions['IsMouseDown'] := False;

  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + PhotoFileName);
  LEffect := TSkRuntimeEffect.MakeForShader(
    'uniform shader texture;' + sLineBreak +
    'uniform float brightness;' + sLineBreak +
    'uniform float contrast;' + sLineBreak +
    'half4 main(vec2 fragCoord) {' + sLineBreak +
    '    vec4 color = texture.eval(fragCoord);' + sLineBreak +
    '    color.rgb += brightness;' + sLineBreak +
    '    if (contrast > 0.0) {' + sLineBreak +
    '        color.rgb = (color.rgb - 0.5) / (1.0 - contrast) + 0.5;' + sLineBreak +
    '    } else {' + sLineBreak +
    '        color.rgb = (color.rgb - 0.5) * (1.0 + contrast) + 0.5;' + sLineBreak +
    '    }' + sLineBreak +
    '    return color;' + sLineBreak +
    '}');
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  LEffectBuilder.SetChild('texture', LImage.MakeShader(TSkSamplingOptions.Medium));

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Brightness & Contrast', 'Press the photo to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImageDest: TRectF;
      LPaint: ISkPaint;
      LRatio: Single;
    begin
      if LOptions['IsMouseDown'] then
      begin
        LEffectBuilder.SetUniform('brightness', 0);
        LEffectBuilder.SetUniform('contrast', 0);
      end
      else
      begin
        LEffectBuilder.SetUniform('brightness', Single(LOptions['Brightness']));
        LEffectBuilder.SetUniform('contrast', Single(LOptions['Contrast']));
      end;
      LImageDest := RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest, LRatio);
      ACanvas.Save;
      try
        ACanvas.ClipRect(LImageDest);
        ACanvas.Scale(1 / LRatio, 1 / LRatio);
        ACanvas.Translate(LImageDest.Left * LRatio, LImageDest.Top * LRatio);
        LPaint := TSkPaint.Create;
        LPaint.Shader := LEffectBuilder.MakeShader;
        ACanvas.DrawPaint(LPaint);
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmFilter.pnlCommonColorFilterClick(Sender: TObject);
const
  Filters: array[0..30] of record
    Name: string;
    Matrix: TSkColorMatrix;
  end = (
    (Name: 'None'; Matrix: (Vector: ((1, 0, 0, 0, 0), (0, 1, 0, 0, 0), (0, 0, 1, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Warm'; Matrix: (Vector: ((1.06, 0, 0, 0, 0), (0, 1.01, 0, 0, 0), (0, 0, 0.93, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Cool'; Matrix: (Vector: ((0.99, 0, 0, 0, 0), (0, 0.93, 0, 0, 0), (0, 0, 1.08, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Protanomaly'; Matrix: (Vector: ((0.817, 0.183, 0, 0, 0), (0.333, 0.667, 0, 0, 0), (0, 0.125, 0.875, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Deuteranomaly'; Matrix: (Vector: ((0.8, 0.2, 0, 0, 0), (0.258, 0.742, 0, 0, 0), (0, 0.142, 0.858, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Tritanomaly'; Matrix: (Vector: ((0.967, 0.033, 0, 0, 0), (0, 0.733, 0.267, 0, 0), (0, 0.183, 0.817, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Protanopia'; Matrix: (Vector: ((0.567, 0.433, 0, 0, 0), (0.558, 0.442, 0, 0, 0), (0, 0.242, 0.758, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Deuteranopia'; Matrix: (Vector: ((0.625, 0.375, 0, 0, 0), (0.7, 0.3, 0, 0, 0), (0, 0.3, 0.7, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Tritanopia'; Matrix: (Vector: ((0.95, 0.05, 0, 0, 0), (0, 0.433, 0.567, 0, 0), (0, 0.475, 0.525, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Achromatomaly'; Matrix: (Vector: ((0.618, 0.320, 0.062, 0, 0), (0.163, 0.775, 0.062, 0, 0), (0.163, 0.320, 0.516, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Polaroid'; Matrix: (Vector: ((1.438, -0.062, -0.062, 0, 0), (-0.122, 1.378, -0.122, 0, 0), (-0.016, -0.016, 1.483, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Koda Chrome'; Matrix: (Vector: ((1.128, -0.396, -0.039, 0, 0.250), (-0.164, 1.083, -0.054, 0, 0.097), (-0.167, -0.560, 1.601, 0, 0.139), (0, 0, 0, 1, 0)))),
    (Name: 'LOMO'; Matrix: (Vector: ((1.2, 0.1, 0.1, 0, -0.287), (0, 1.2, 0.1, 0, -0.287), (0, 0.1, 1.1, 0, -0.287), (0, 0, 0, 1, 0)))),
    (Name: 'Gothic'; Matrix: (Vector: ((1.9, -0.3, -0.2, 0, -0.341), (-0.2, 1.7, -0.1, 0, -0.341), (-0.1, -0.6, 2.0, 0, -0.341), (0, 0, 0, 1.0, 0)))),
    (Name: 'Elegant'; Matrix: (Vector: ((0.6, 0.3, 0.1, 0, 0.287), (0.2, 0.7, 0.1, 0, 0.287), (0.2, 0.3, 0.4, 0, 0.287), (0, 0, 0, 1, 0)))),
    (Name: 'Wine Red'; Matrix: (Vector: ((1.2, 0, 0, 0, 0), (0, 0.9, 0, 0, 0), (0, 0, 0.8, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Qingning'; Matrix: (Vector: ((0.9, 0, 0, 0, 0), (0, 1.1, 0, 0, 0), (0, 0, 0.9, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Halo'; Matrix: (Vector: ((0.9, 0, 0, 0, 0.247), (0, 0.9, 0, 0, 0.247), (0, 0, 0.9, 0, 0.247), (0, 0, 0, 1, 0)))),
    (Name: 'Fantasy'; Matrix: (Vector: ((0.8, 0.3, 0.1, 0, 0.182), (0.1, 0.9, 0, 0, 0.182), (0.1, 0.3, 0.7, 0, 0.182), (0, 0, 0, 1, 0)))),
    (Name: 'Sepia'; Matrix: (Vector: ((0.393, 0.769, 0.189, 0, 0), (0.349, 0.686, 0.168, 0, 0), (0.272, 0.534, 0.131, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Browni'; Matrix: (Vector: ((0.599, 0.345, -0.271, 0, 0.186), (-0.038, 0.861, 0.150, 0, -0.145), (0.241, -0.074, 0.44972182064877153, 0, -0.030), (0, 0, 0, 1, 0)))),
    (Name: 'Retro'; Matrix: (Vector: ((0.2, 0.5, 0.1, 0, 0.16), (0.2, 0.5, 0.1, 0, 0.16), (0.2, 0.5, 0.1, 0, 0.16), (0, 0, 0, 1, 0)))),
    (Name: 'Vintage'; Matrix: (Vector: ((0.628, 0.320, -0.040, 0, 0.038), (0.026, 0.644, 0.033, 0, 0.029), (0.047, -0.085, 0.524, 0, 0.020), (0, 0, 0, 1, 0)))),
    (Name: 'Grayscale'; Matrix: (Vector: ((0.299, 0.587, 0.114, 0, 0), (0.299, 0.587, 0.114, 0, 0), (0.299, 0.587, 0.114, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Black and White'; Matrix: (Vector: ((1.5, 1.5, 1.5, 0, 0), (1.5, 1.5, 1.5, 0, 0), (1.5, 1.5, 1.5, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Technicolor'; Matrix: (Vector: ((1.912, -0.854, -0.091, 0, 0.046), (-0.308, 1.765, -0.106, 0, -0.275), (-0.231, -0.750, 1.847, 0, 0.121), (0, 0, 0, 1, 0)))),
    (Name: 'Sharpen'; Matrix: (Vector: ((4.8, -1.0, -0.1, 0, -1.523), (-0.5, 4.4, -0.1, 0, -1.523), (-0.5, -1.0, 5.2, 0, -1.523), (0, 0, 0, 1, 0)))),
    (Name: 'LSD'; Matrix: (Vector: ((2, -0.4, 0.5, 0, 0), (-0.5, 2, -0.4, 0, 0), (-0.4, -0.5, 3, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Night Vision'; Matrix: (Vector: ((0.1, 0.4, 0, 0, 0), (0.3, 1, 0.3, 0, 0), (0, 0.4, 0.1, 0, 0), (0, 0, 0, 1, 0)))),
    (Name: 'Luminance to Alpha'; Matrix: (Vector: ((0, 0, 0, 0, 0), (0, 0, 0, 0, 0), (0, 0, 0, 0, 0), (0.2125, 0.7154, 0.0721, 0, 0)))),
    (Name: 'Invert'; Matrix: (Vector: ((-1, 0, 0, 0, 1), (0, -1, 0, 0, 1), (0, 0, -1, 0, 1), (0, 0, 0, 1, 0))))
  );
var
  LOptions: IViewerOptions;
  LNames: TArray<string>;
  I: Integer;
begin
  SetLength(LNames, Length(Filters));
  for I := 0 to Length(Filters) - 1 do
    LNames[I] := Filters[I].Name;
  LOptions := TViewerOptions.Create;
  LOptions.AddStrings('Color filter', LNames, 0);
  LOptions['IsMouseDown'] := False;

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Common Color Filter', 'Press the photo to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImage: ISkImage;
      LImageDest: TRectF;
      LPaint: ISkPaint;
    begin
      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + PhotoFileName);
      LImageDest := RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest);
      LPaint := TSkPaint.Create;
      if LOptions['IsMouseDown'] = False then
        LPaint.ColorFilter := TSkColorFilter.MakeMatrix(Filters[Integer(LOptions['Color filter'])].Matrix);
      ACanvas.DrawImageRect(LImage, LImageDest, TSkSamplingOptions.Medium, LPaint);
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmFilter.pnlDropShadowClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('DeltaX', -25, 25, 6, 0.25);
  LOptions.AddFloat('DeltaY', -25, 25, 6, 0.25);
  LOptions.AddFloat('SigmaX', 0, 10, 4, 0.05);
  LOptions.AddFloat('SigmaY', 0, 10, 4, 0.05);
  LOptions['IsMouseDown'] := False;

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Drop Shadow', 'Press the picture to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LImage: ISkImage;
      LImageDest: TRectF;
    begin
      if LOptions['IsMouseDown'] then
        LPaint := nil
      else
      begin
        LPaint := TSkPaint.Create;
        LPaint.ImageFilter := TSkImageFilter.MakeDropShadow(Single(LOptions['DeltaX']), Single(LOptions['DeltaY']),
          Single(LOptions['SigmaX']), Single(LOptions['SigmaY']), TAlphaColors.Black);
      end;

      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'chat-bubble.png');
      LImageDest := RectF(0, 0, Max(ADest.Width - 160, 50), Max(ADest.Height - 400, 50));
      RectCenter(LImageDest, ADest);
      ACanvas.DrawImageNine(LImage, TRect.Create(39, 36, 40, 37), LImageDest, TSkFilterMode.Linear, LPaint);
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmFilter.pnlHueSaturationClick(Sender: TObject);
// The implementation used here was using shader as it is the fastest way, but it is also possible to use
// SkColorFilter.MakeMatrix passing the Hue rotation matrix or the Saturation matrix. Here's how to calculate this matrix:
// https://github.com/pixijs/pixijs/blob/f2731251b1600fa4e310cf2fb27820aa85b8074e/packages/filters/filter-color-matrix/src/ColorMatrixFilter.ts#L210
var
  LOptions: IViewerOptions;
  LImage: ISkImage;
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Hue', -1, 1, 0, 0.01);
  LOptions.AddFloat('Saturation', -1, 1, 0, 0.01);
  LOptions['IsMouseDown'] := False;

  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + PhotoFileName);
  LEffect := TSkRuntimeEffect.MakeForShader(
    'uniform shader texture;' + sLineBreak +
    'uniform float hue;' + sLineBreak +
    'uniform float saturation;' + sLineBreak +
    'vec4 main(vec2 fragCoord) {' + sLineBreak +
    '    vec4 color = texture.eval(fragCoord);' + sLineBreak +
    '    /* hue adjustment, wolfram alpha: RotationTransform[angle, {1, 1, 1}][{x, y, z}] */' + sLineBreak +
    '    float angle = hue * 3.14159265;' + sLineBreak +
    '    float s = sin(angle), c = cos(angle);' + sLineBreak +
    '    vec3 weights = (vec3(2.0 * c, -sqrt(3.0) * s - c, sqrt(3.0) * s - c) + 1.0) / 3.0;' + sLineBreak +
    '    float len = length(color.rgb);' + sLineBreak +
    '    color.rgb = vec3(' + sLineBreak +
    '        dot(color.rgb, weights.xyz),' + sLineBreak +
    '        dot(color.rgb, weights.zxy),' + sLineBreak +
    '        dot(color.rgb, weights.yzx)' + sLineBreak +
    '    );' + sLineBreak +
    '    /* saturation adjustment */' + sLineBreak +
    '    float average = (color.r + color.g + color.b) / 3.0;' + sLineBreak +
    '    if (saturation > 0.0) {' + sLineBreak +
    '        color.rgb += (average - color.rgb) * (1.0 - 1.0 / (1.001 - saturation));' + sLineBreak +
    '    } else {' + sLineBreak +
    '        color.rgb += (average - color.rgb) * (-saturation);' + sLineBreak +
    '    }' + sLineBreak +
    '    return color;' + sLineBreak +
    '}');
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  LEffectBuilder.SetChild('texture', LImage.MakeShader(TSkSamplingOptions.Medium));

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Hue & Saturation', 'Press the photo to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImageDest: TRectF;
      LPaint: ISkPaint;
      LRatio: Single;
    begin
      if LOptions['IsMouseDown'] then
      begin
        LEffectBuilder.SetUniform('hue', 0);
        LEffectBuilder.SetUniform('saturation', 0);
      end
      else
      begin
        LEffectBuilder.SetUniform('hue', Single(LOptions['Hue']));
        LEffectBuilder.SetUniform('saturation', Single(LOptions['Saturation']));
      end;
      LImageDest := RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest, LRatio);
      ACanvas.Save;
      try
        ACanvas.ClipRect(LImageDest);
        ACanvas.Scale(1 / LRatio, 1 / LRatio);
        ACanvas.Translate(LImageDest.Left * LRatio, LImageDest.Top * LRatio);
        LPaint := TSkPaint.Create;
        LPaint.Shader := LEffectBuilder.MakeShader;
        ACanvas.DrawPaint(LPaint);
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmFilter.pnlLightingEffectsClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Z', 0, 10, 1.5, 0.05);
  LOptions.AddFloat('Surface Scale', -1, 1, 0.2, 0.01);
  LOptions.AddFloat('Light Constant', 0, 1, 0.6, 0.005);
  LOptions['IsMouseDown'] := False;

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Lighting Effects', 'Press the picture to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LPaint: ISkPaint;
      LImage: ISkImage;
      LImageDest: TRectF;
    begin
      if LOptions['IsMouseDown'] then
        LPaint := nil
      else
      begin
        LPaint := TSkPaint.Create;
        LPaint.ImageFilter := TSkImageFilter.MakeDistantLitDiffuse(TPoint3D.Create(2, 3, Single(LOptions['Z'])),
          TAlphaColors.White, Single(LOptions['Surface Scale']), Single(LOptions['Light Constant']));
      end;

      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'chat-bubble.png');
      LImageDest := RectF(0, 0, Max(ADest.Width - 160, 50), Max(ADest.Height - 400, 50));
      RectCenter(LImageDest, ADest);
      ACanvas.DrawImageNine(LImage, TRect.Create(39, 36, 40, 37), LImageDest, TSkFilterMode.Linear, LPaint);
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmFilter.pnlVignetteClick(Sender: TObject);
var
  LOptions: IViewerOptions;
  LImage: ISkImage;
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddFloat('Size', 0, 1, 0.5, 0.005);
  LOptions.AddFloat('Amount', 0, 1, 0, 0.005);
  LOptions['IsMouseDown'] := False;

  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + PhotoFileName);
  LEffect := TSkRuntimeEffect.MakeForShader(
    'uniform shader texture;' + sLineBreak +
    'uniform float2 resolution;' + sLineBreak +
    'uniform float size;' + sLineBreak +
    'uniform float amount;' + sLineBreak +
    'vec4 main(vec2 fragCoord) {' + sLineBreak +
    '    vec4 color = texture.eval(fragCoord);' + sLineBreak +
    '    float dist = distance(fragCoord / resolution, vec2(0.5, 0.5));' + sLineBreak +
    '    color.rgb *= smoothstep(0.8, size * 0.799, dist * (amount + size));' + sLineBreak +
    '    return color;' + sLineBreak +
    '}');
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  LEffectBuilder.SetChild('texture', LImage.MakeShader(TSkSamplingOptions.Medium));

  // Mouse events
  ChildForm<TfrmPaintBoxViewer>.OnMouseDown :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := True;
      AShouldRedraw := True;
    end;
  ChildForm<TfrmPaintBoxViewer>.OnMouseUp :=
    procedure(const APoint: TPointF; var AShouldRedraw: Boolean)
    begin
      LOptions['IsMouseDown'] := False;
      AShouldRedraw := True;
    end;

  // Draw
  ChildForm<TfrmPaintBoxViewer>.Show('Vignette', 'Press the photo to show the original',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImageDest: TRectF;
      LPaint: ISkPaint;
      LRatio: Single;
    begin
      LEffectBuilder.SetUniform('size', Single(LOptions['Size']));
      if LOptions['IsMouseDown'] then
        LEffectBuilder.SetUniform('amount', 0)
      else
        LEffectBuilder.SetUniform('amount', Single(LOptions['Amount']));
      LImageDest := RectF(0, 0, LImage.Width, LImage.Height).FitInto(ADest, LRatio);
      ACanvas.Save;
      try
        ACanvas.ClipRect(LImageDest);
        ACanvas.Scale(1 / LRatio, 1 / LRatio);
        ACanvas.Translate(LImageDest.Left * LRatio, LImageDest.Top * LRatio);
        LEffectBuilder.SetUniform('resolution', PointF(LImageDest.Width * LRatio, LImageDest.Height * LRatio));
        LPaint := TSkPaint.Create;
        LPaint.Shader := LEffectBuilder.MakeShader;
        ACanvas.DrawPaint(LPaint);
      finally
        ACanvas.Restore;
      end;
    end, TBackgroundKind.Chess, LOptions);
end;

end.
