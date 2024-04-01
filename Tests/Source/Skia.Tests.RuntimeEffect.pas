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
unit Skia.Tests.RuntimeEffect;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkRuntimeEffectTests }

  [TestFixture]
  TSkRuntimeEffectTests = class(TTestBase)
  protected
    procedure SetChildImages(const ARuntimeEffect: ISkRuntimeEffectBuilder; const AChildImagesFileName: string);
    procedure SetUniforms(const ARuntimeEffect: ISkRuntimeEffectBuilder; const AUniforms: string);
  public
    [TestCase('File "shader.mouse.sksl"', 'shader.mouse.sksl,150,100,iMouse=70.0 80.0;iResolution=150.0 100.0,,AAAAOH7+/v4fCAA5f//+/h8PDz9/////Hw8PP3////8AAAAAB8Af8D/4P/h//H/8f/x//H/8f/w')]
    [TestCase('File "shader.mouse.sksl"', 'shader.mouse.sksl,150,100,iMouse=70 80;iResolution=150 100,,AAAAOH7+/v4fCAA5f//+/h8PDz9/////Hw8PP3////8AAAAAB8Af8D/4P/h//H/8f/x//H/8f/w')]
    [TestCase('File "shader.brightness-and-contrast.sksl"', 'shader.brightness-and-contrast.sksl,250,250,brightness=0.3;contrast=0.5,texture=horse.webp,/fj4+Pj4+PP//Pj5+//+///9/f3///////39/f////9Q7vAa/z///v/////fPlYz/XODODBQxUI')]
    procedure TestShader(const ASkSLFileName: string; const AWidth, AHeight: Integer; const AUniforms, AChildImagesFileName, AExpectedImageHash: string);
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

procedure TSkRuntimeEffectTests.SetChildImages(
  const ARuntimeEffect: ISkRuntimeEffectBuilder;
  const AChildImagesFileName: string);
var
  LChildImage: string;
  LChildName: string;
  LChildImageFileName: string;
begin
  for LChildImage in AChildImagesFileName.Split([';'], TStringSplitOptions.ExcludeEmpty) do
  begin
    if not LChildImage.Contains('=') then
      Continue;
    LChildName := LChildImage.Split(['='])[0];
    LChildImageFileName := LChildImage.Split(['='])[1];
    if TFile.Exists(ImageAssetsPath + LChildImageFileName) then
      ARuntimeEffect.SetChild(LChildName, TSkImage.MakeFromEncodedFile(ImageAssetsPath + LChildImageFileName).MakeShader(TSkSamplingOptions.Low));
  end;
end;

procedure TSkRuntimeEffectTests.SetUniforms(
  const ARuntimeEffect: ISkRuntimeEffectBuilder; const AUniforms: string);
var
  LUniform: string;
  LUniformName: string;
  LUniformValues: TArray<string>;
begin
  for LUniform in AUniforms.Split([';'], TStringSplitOptions.ExcludeEmpty) do
  begin
    if not LUniform.Contains('=') then
      Continue;
    LUniformName := LUniform.Split(['='])[0];
    LUniformValues := LUniform.Split(['='])[1].Split([' ']);
    if LUniform.Contains('.') then
    begin
      case Length(LUniformValues) of
        1: ARuntimeEffect.SetUniform(LUniformName, StrToFloat(LUniformValues[0], TFormatSettings.Invariant));
        2: ARuntimeEffect.SetUniform(LUniformName, TSkRuntimeEffectFloat2.Create(StrToFloat(LUniformValues[0], TFormatSettings.Invariant), StrToFloat(LUniformValues[1], TFormatSettings.Invariant)));
      else
        raise Exception.Create('Uniform value not supported');
      end;
    end
    else
    begin
      case Length(LUniformValues) of
        1: ARuntimeEffect.SetUniform(LUniformName, StrToInt(LUniformValues[0]));
        2: ARuntimeEffect.SetUniform(LUniformName, TSkRuntimeEffectInt2.Create(StrToInt(LUniformValues[0]), StrToInt(LUniformValues[1])));
      else
        raise Exception.Create('Uniform value not supported');
      end;
    end;
  end;
end;

procedure TSkRuntimeEffectTests.TestShader(const ASkSLFileName: string;
  const AWidth, AHeight: Integer; const AUniforms, AChildImagesFileName,
  AExpectedImageHash: string);
var
  LEffect: ISkRuntimeEffect;
  LEffectBuilder: ISkRuntimeShaderBuilder;
  LError: string;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LEffect := TSkRuntimeEffect.MakeForShader(TFile.ReadAllText(AssetsPath + ASkSLFileName), LError);
  Assert.IsNotNull(LEffect, 'SkSL error: ' + LError);
  LEffectBuilder := TSkRuntimeShaderBuilder.Create(LEffect);
  SetUniforms(LEffectBuilder, AUniforms);
  SetChildImages(LEffectBuilder, AChildImagesFileName);
  LPaint := TSkPaint.Create;
  LPaint.Shader := LEffectBuilder.MakeShader;
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSurface.Canvas.DrawPaint(LPaint);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkRuntimeEffectTests);
end.
