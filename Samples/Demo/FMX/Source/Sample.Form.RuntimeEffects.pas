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
unit Sample.Form.RuntimeEffects;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.Classes, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects, System.IOUtils,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmRuntimeEffects = class(TfrmBase)
    btnShaderWithMouse: TSpeedButton;
    lblShaderWithMouseTitle: TSkLabel;
    lblShaderWithMouseDescription: TSkLabel;
    btnShaderAnimation: TSpeedButton;
    lblShaderAnimationTitle: TSkLabel;
    lblShaderAnimationDescription: TSkLabel;
    btnWavesShaderAnimation: TSpeedButton;
    lblWavesShaderAnimationTitle: TSkLabel;
    lblWavesShaderAnimationDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnShaderAnimationClick(Sender: TObject);
    procedure btnShaderWithMouseClick(Sender: TObject);
    procedure btnWavesShaderAnimationClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.AnimatedPaintBox;

{$R *.fmx}

procedure TfrmRuntimeEffects.btnShaderAnimationClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LPaint: ISkPaint;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'rainbow-twister.sksl')));
  LPaint := TSkPaint.Create;
  LPaint.Shader := LEffect.MakeShader(True);
  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Shader Animation', 'Shader that varies with time (iTime uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    begin
      LEffect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
      LEffect.SetUniform('iTime', ASeconds);
      ACanvas.DrawPaint(LPaint);
    end);
end;

procedure TfrmRuntimeEffects.btnShaderWithMouseClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LPaint: ISkPaint;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'mouse.sksl')));
  LPaint := TSkPaint.Create;
  LPaint.Shader := LEffect.MakeShader(True);

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LEffect.SetUniform('iMouse', PointF(AX, AY));
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Shader with Mouse', 'Shader that varies with mouse position (iMouse uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    begin
      LEffect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
      ACanvas.DrawPaint(LPaint);
    end);
end;

procedure TfrmRuntimeEffects.btnWavesShaderAnimationClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LPaint: ISkPaint;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'waves.sksl')));
  LPaint := TSkPaint.Create;
  LPaint.Shader := LEffect.MakeShader(True);
  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Waves Shade Animation', 'Shader that varies with time (iTime uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    begin
      LEffect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
      LEffect.SetUniform('iTime', ASeconds);
      ACanvas.DrawPaint(LPaint);
    end);
end;

end.
