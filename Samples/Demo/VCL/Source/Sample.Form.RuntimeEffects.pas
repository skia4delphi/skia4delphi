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
  System.SysUtils, System.Classes, System.Types, System.UITypes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmRuntimeEffects = class(TfrmBase)
    pnlStarNestShaderAnimation: TPanel;
    lblStarNestShaderAnimationDescription: TSkLabel;
    lblStarNestShaderAnimationTitle: TSkLabel;
    svgStarNestShaderAnimationArrow: TSkSvg;
    pnlStarNestShaderAnimationLine: TPanel;
    pnlShaderWithMouse: TPanel;
    lblShaderWithMouseDescription: TSkLabel;
    lblShaderWithMouseTitle: TSkLabel;
    svgShaderWithMouseArrow: TSkSvg;
    pnlShaderWithMouseLine: TPanel;
    pnlTimeVaryingShader: TPanel;
    lblTimeVaryingShaderDescription: TSkLabel;
    lblTimeVaryingShaderTitle: TSkLabel;
    svgTimeVaryingShaderArrow: TSkSvg;
    pnlTimeVaryingShaderLine: TPanel;
    procedure pnlShaderWithMouseClick(Sender: TObject);
    procedure pnlStarNestShaderAnimationClick(Sender: TObject);
    procedure pnlTimeVaryingShaderClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.AnimatedPaintBox;

{$R *.dfm}

procedure TfrmRuntimeEffects.pnlShaderWithMouseClick(Sender: TObject);
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

procedure TfrmRuntimeEffects.pnlStarNestShaderAnimationClick(Sender: TObject);
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

procedure TfrmRuntimeEffects.pnlTimeVaryingShaderClick(Sender: TObject);
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
