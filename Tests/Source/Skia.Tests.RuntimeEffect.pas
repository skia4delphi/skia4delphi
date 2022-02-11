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
unit Skia.Tests.RuntimeEffect;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkRuntimeEffectTests }

  [TestFixture]
  TSkRuntimeEffectTests = class(TTestBase)
  protected
    function AssetsPath: string; override;
    procedure SetUniforms(const ARuntimeEffects: ISkRuntimeEffect; const AUniforms: string);
  public
    [TestCase('File "shader.mouse.sksl"', 'shader.mouse.sksl,150,100,iMouse=70.0 80.0;iResolution=150.0 100.0,,AAAAOH7+/v4fCAA5f//+/h8PDz9/////Hw8PP3////8AAAAAB8Af8D/4P/h//H/8f/x//H/8f/w')]
    [TestCase('File "shader.brightness-and-contrast.sksl"', 'shader.brightness-and-contrast.sksl,250,250,brightness=0.3;contrast=0.5,horse.webp,/fj4+Pj4+PP//Pj5+//+///9/f3///////39/f////9Q7vAa/z///v/////fPlYz/XODODBQxUI')]
    procedure TestShader(const ASkSLFileName: string; const AWidth, AHeight: Integer; const AUniforms, AChildImageFileName, AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkRuntimeEffectTests }

function TSkRuntimeEffectTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'RuntimeEffect');
end;

procedure TSkRuntimeEffectTests.SetUniforms(
  const ARuntimeEffects: ISkRuntimeEffect; const AUniforms: string);
var
  LUniform: string;
  LUniformName: string;
  LUniformValues: TArray<string>;
begin
  for LUniform in AUniforms.Split([';']) do
  begin
    if not LUniform.Contains('=') then
      Continue;
    LUniformName := LUniform.Split(['='])[0];
    LUniformValues := LUniform.Split(['='])[1].Split([' ']);
    if LUniform.Contains('.') then
    begin
      case Length(LUniformValues) of
        1: ARuntimeEffects.SetUniform(LUniformName, StrToFloat(LUniformValues[0], TFormatSettings.Invariant));
        2: ARuntimeEffects.SetUniform(LUniformName, TSkRuntimeEffectFloat2.Create(StrToFloat(LUniformValues[0], TFormatSettings.Invariant), StrToFloat(LUniformValues[1], TFormatSettings.Invariant)));
      else
        raise Exception.Create('Uniform value not supported');
      end;
    end
    else
    begin
      case Length(LUniformValues) of
        1: ARuntimeEffects.SetUniform(LUniformName, StrToInt(LUniformValues[0]));
        2: ARuntimeEffects.SetUniform(LUniformName, TSkRuntimeEffectInt2.Create(StrToInt(LUniformValues[0]), StrToInt(LUniformValues[1])));
      else
        raise Exception.Create('Uniform value not supported');
      end;
    end;
  end;
end;

procedure TSkRuntimeEffectTests.TestShader(const ASkSLFileName: string;
  const AWidth, AHeight: Integer; const AUniforms, AChildImageFileName, AExpectedImageHash: string);
var
  LEffect: ISkRuntimeEffect;
  LError: string;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + ASkSLFileName), LError);
  Assert.IsNotNull(LEffect, 'SkSL error: ' + LError);
  if FileExists(ImageAssetsPath + AChildImageFileName) then
    LEffect.SetChildShader(0, TSkImage.MakeFromEncodedFile(ImageAssetsPath + AChildImageFileName).MakeShader(TSkSamplingOptions.Low));
  LPaint := TSkPaint.Create;
  LPaint.Shader := LEffect.MakeShader(True);
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  SetUniforms(LEffect, AUniforms);
  LSurface.Canvas.DrawPaint(LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkRuntimeEffectTests);
end.
