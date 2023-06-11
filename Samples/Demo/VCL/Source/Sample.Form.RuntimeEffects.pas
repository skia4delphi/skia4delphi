{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
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
  System.SysUtils, System.Classes, System.Types, System.IOUtils, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmRuntimeEffects = class(TfrmBase)
    pnlShaderAnimation: TPanel;
    lblShaderAnimationDescription: TSkLabel;
    lblShaderAnimationTitle: TSkLabel;
    svgShaderAnimationArrow: TSkSvg;
    pnlShaderAnimationLine: TPanel;
    pnlShaderWithMouse: TPanel;
    lblShaderWithMouseDescription: TSkLabel;
    lblShaderWithMouseTitle: TSkLabel;
    svgShaderWithMouseArrow: TSkSvg;
    pnlShaderWithMouseLine: TPanel;
    pnlWavesShaderAnimation: TPanel;
    lblWavesShaderAnimationDescription: TSkLabel;
    lblWavesShaderAnimationTitle: TSkLabel;
    svgWavesShaderAnimationArrow: TSkSvg;
    pnlWavesShaderAnimationLine: TPanel;
    procedure pnlShaderAnimationClick(Sender: TObject);
    procedure pnlShaderWithMouseClick(Sender: TObject);
    procedure pnlWavesShaderAnimationClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.AnimatedPaintBox;

{$R *.dfm}

procedure TfrmRuntimeEffects.pnlShaderAnimationClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'rainbow-twister.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Shader Animation', 'Shader that varies with time (iTime uniform)',
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

procedure TfrmRuntimeEffects.pnlShaderWithMouseClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'mouse.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);

  ChildForm<TfrmAnimatedPaintBoxViewer>.OnMouseMove :=
    procedure (const AX, AY: Single)
    begin
      LEffectBuilder.SetUniform('iMouse', PointF(AX, AY));
    end;

  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Shader with Mouse', 'Shader that varies with mouse position (iMouse uniform)',
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

procedure TfrmRuntimeEffects.pnlWavesShaderAnimationClick(Sender: TObject);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + TPath.Combine('RuntimeEffects Shaders', 'waves.sksl')));
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  ChildForm<TfrmAnimatedPaintBoxViewer>.Show('Waves Shader Animation', 'Shader that varies with time (iTime uniform)',
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

end.
