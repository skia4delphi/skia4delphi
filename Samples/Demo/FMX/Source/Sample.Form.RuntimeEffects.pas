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
unit Sample.Form.RuntimeEffects;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects, System.IOUtils,
  FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmRuntimeEffects = class(TfrmBase)
    btnShaderWithMouse: TSpeedButton;
    lblShaderWithMouseTitle: TSkLabel;
    lblShaderWithMouseDescription: TSkLabel;
    btnStarNestShaderAnimation: TSpeedButton;
    lblStarNestShaderAnimationTitle: TSkLabel;
    lblStarNestShaderAnimationDescription: TSkLabel;
    btnTimeVaryingShader: TSpeedButton;
    lblTimeVaryingShaderTitle: TSkLabel;
    lblTimeVaryingShaderDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnShaderWithMouseClick(Sender: TObject);
    procedure btnStarNestShaderAnimationClick(Sender: TObject);
    procedure btnTimeVaryingShaderClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.AnimatedPaintBox;

{$R *.fmx}

procedure TfrmRuntimeEffects.btnShaderWithMouseClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders',
    'mouse.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LEffectBuilder.SetUniform('iMouse', PointF(AX, AY));
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Shader with Mouse', 'Shader that varies with mouse position (iMouse ' +
    'uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    var
      LPaint: ISkPaint;
    begin
      LEffectBuilder.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
      LPaint := TSkPaint.Create;
      LPaint.Shader := LEffectBuilder.MakeShader;
      ACanvas.DrawPaint(LPaint);
    end);
end;

procedure TfrmRuntimeEffects.btnStarNestShaderAnimationClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders',
    'star-nest.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LEffectBuilder.SetUniform('iMouse', PointF(AX, AY));
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Star Nest Shader Animation', 'Shader that varies with time (iTime ' +
    'uniform) and mouse position (iMouse uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    var
      LPaint: ISkPaint;
    begin
      LEffectBuilder.SetUniform('iResolution', PointF(ADest.Width, ADest.Height));
      LEffectBuilder.SetUniform('iTime', ASeconds);
      LPaint := TSkPaint.Create;
      LPaint.Shader := LEffectBuilder.MakeShader;
      ACanvas.DrawPaint(LPaint);
    end);
end;

procedure TfrmRuntimeEffects.btnTimeVaryingShaderClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders',
    'time.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  LEffectBuilder.SetUniform('iColor1', TAlphaColorF.Create($FFA22A2A));
  LEffectBuilder.SetUniform('iColor2', TAlphaColorF.Create($FF187735));
  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Time-varying Shader', 'Example of a simple shader varying the color ' +
    'according to time (iTime uniform)',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF; const ASeconds: Double)
    var
      LPaint: ISkPaint;
    begin
      LEffectBuilder.SetUniform('iTime', ASeconds);
      LPaint := TSkPaint.Create;
      LPaint.Shader := LEffectBuilder.MakeShader;
      ACanvas.DrawPaint(LPaint);
    end);
end;

end.
