{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2026 Skia4Delphi Project.                           }
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
  strict private const
    ReflectionSkSL =
      'uniform float  fIntensity;'    + sLineBreak +
      'uniform float2 fCenter;'       + sLineBreak +
      'uniform int    fSteps;'        + sLineBreak +
      'uniform shader fTexture;'      + sLineBreak +
      'half4 main(float2 p) { return fTexture.eval(p) * fIntensity * float(fSteps) * fCenter.x; }';
    BlenderSkSL =
      'half4 main(half4 src, half4 dst) { return src * dst; }';
    ColorFilterSkSL =
      'uniform float fFactor;' + sLineBreak +
      'half4 main(half4 color) { return color * fFactor; }';
  strict private
    function ReflectionEffect: ISkRuntimeEffect;
  protected
    procedure SetChildImages(const ARuntimeEffect: ISkRuntimeEffectBuilder; const AChildImagesFileName: string);
    procedure SetUniforms(const ARuntimeEffect: ISkRuntimeEffectBuilder; const AUniforms: string);
  public
    [TestCase('File "shader.mouse.sksl"', 'shader.mouse.sksl,150,100,iMouse=70.0 80.0;iResolution=150.0 100.0,,AAAAOH7+/v4fCAA5f//+/h8PDz9/////Hw8PP3////8AAAAAB8Af8D/4P/h//H/8f/x//H/8f/w')]
    [TestCase('File "shader.mouse.sksl"', 'shader.mouse.sksl,150,100,iMouse=70 80;iResolution=150 100,,AAAAOH7+/v4fCAA5f//+/h8PDz9/////Hw8PP3////8AAAAAB8Af8D/4P/h//H/8f/x//H/8f/w')]
    [TestCase('File "shader.brightness-and-contrast.sksl"', 'shader.brightness-and-contrast.sksl,250,250,brightness=0.3;contrast=0.5,texture=horse.webp,/fj4+Pj4+PP//Pj5+//+///9/f3///////39/f////9Q7vAa/z///v/////fPlYz/XODODBQxUI')]
    procedure TestShader(const ASkSLFileName: string; const AWidth, AHeight: Integer; const AUniforms, AChildImagesFileName, AExpectedImageHash: string);
    [Test]
    procedure TestChildReflection;
    [Test]
    procedure TestInvalidSkSL;
    [Test]
    procedure TestMakeBlender;
    [Test]
    procedure TestMakeColorFilter;
    [Test]
    procedure TestUniformDataSize;
    [Test]
    procedure TestUniformReflection;
    [Test]
    procedure TestUniformTypes;
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

function TSkRuntimeEffectTests.ReflectionEffect: ISkRuntimeEffect;
var
  LError: string;
begin
  Result := TSkRuntimeEffect.MakeForShader(ReflectionSkSL, LError);
  Assert.IsNotNull(Result, 'Could not build the reflection shader: ' + LError);
  Assert.IsEmpty(LError, 'A valid shader should not report an error');
end;

procedure TSkRuntimeEffectTests.TestChildReflection;
var
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := ReflectionEffect;
  Assert.AreEqual(1, LRuntimeEffect.ChildCount, '(ChildCount)');
  Assert.AreEqual('fTexture', LRuntimeEffect.ChildrenNames[0], '(ChildrenNames)');
  Assert.AreEqual(0, LRuntimeEffect.IndexOfChild('fTexture'), '(IndexOfChild)');
  Assert.AreEqual(-1, LRuntimeEffect.IndexOfChild('fUnknown'), 'An unknown child should not be found');
  Assert.IsTrue(LRuntimeEffect.ChildExists('fTexture'), '(ChildExists)');
  Assert.IsFalse(LRuntimeEffect.ChildExists('fUnknown'), '(ChildExists of an unknown child)');
  Assert.IsTrue(LRuntimeEffect.ChildType[0] = TSkRuntimeEffectChildType.Shader, '(ChildType)');
  Assert.IsTrue(LRuntimeEffect.ChildTypeByName['fTexture'] = TSkRuntimeEffectChildType.Shader, '(ChildTypeByName)');
end;

procedure TSkRuntimeEffectTests.TestInvalidSkSL;
var
  LError: string;
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := TSkRuntimeEffect.MakeForShader('this is not valid sksl', LError);
  Assert.IsNull(LRuntimeEffect, 'An invalid shader should not be created');
  Assert.IsNotEmpty(LError, 'An invalid shader should report the error');
end;

procedure TSkRuntimeEffectTests.TestMakeBlender;
var
  LBlender: ISkBlender;
  LError: string;
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := TSkRuntimeEffect.MakeForBlender(BlenderSkSL, LError);
  Assert.IsNotNull(LRuntimeEffect, 'Could not build the blender: ' + LError);
  Assert.AreEqual(0, LRuntimeEffect.UniformCount, 'The blender has no uniform');
  Assert.AreEqual(0, LRuntimeEffect.ChildCount, 'The blender has no child');

  LBlender := LRuntimeEffect.MakeBlender(nil^, nil);
  Assert.IsNotNull(LBlender, 'The runtime effect should produce a blender');
end;

procedure TSkRuntimeEffectTests.TestMakeColorFilter;
var
  LColorFilter: ISkColorFilter;
  LError: string;
  LFactor: Single;
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := TSkRuntimeEffect.MakeForColorFilter(ColorFilterSkSL, LError);
  Assert.IsNotNull(LRuntimeEffect, 'Could not build the color filter: ' + LError);
  Assert.AreEqual(1, LRuntimeEffect.UniformCount, '(UniformCount)');
  Assert.AreEqual('fFactor', LRuntimeEffect.UniformNames[0], '(UniformNames)');

  LFactor := 0.5;
  LColorFilter := LRuntimeEffect.MakeColorFilter(LFactor, nil);
  Assert.IsNotNull(LColorFilter, 'The runtime effect should produce a color filter');
end;

procedure TSkRuntimeEffectTests.TestUniformDataSize;
var
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := ReflectionEffect;
  // float + float2 + int, each element being four bytes wide.
  Assert.AreEqual<NativeUInt>(4 * 4, LRuntimeEffect.UniformDataSize, '(UniformDataSize)');
  Assert.AreEqual<NativeUInt>(0, LRuntimeEffect.UniformOffset[0], 'The first uniform starts at zero');
  Assert.AreEqual<NativeUInt>(4, LRuntimeEffect.UniformOffsetByName['fCenter'], 'The second uniform follows the first');
  Assert.AreEqual<NativeUInt>(12, LRuntimeEffect.UniformOffsetByName['fSteps'], 'The third uniform follows the float2');
end;

procedure TSkRuntimeEffectTests.TestUniformReflection;
var
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := ReflectionEffect;
  Assert.AreEqual(3, LRuntimeEffect.UniformCount, '(UniformCount)');
  Assert.AreEqual('fIntensity', LRuntimeEffect.UniformNames[0], '(UniformNames)');
  Assert.AreEqual(1, LRuntimeEffect.IndexOfUniform('fCenter'), '(IndexOfUniform)');
  Assert.AreEqual(-1, LRuntimeEffect.IndexOfUniform('fUnknown'), 'An unknown uniform should not be found');
  Assert.IsTrue(LRuntimeEffect.UniformExists('fSteps'), '(UniformExists)');
  Assert.IsFalse(LRuntimeEffect.UniformExists('fUnknown'), '(UniformExists of an unknown uniform)');
end;

procedure TSkRuntimeEffectTests.TestUniformTypes;
var
  LRuntimeEffect: ISkRuntimeEffect;
begin
  LRuntimeEffect := ReflectionEffect;
  Assert.IsTrue(LRuntimeEffect.UniformType[0] = TSkRuntimeEffectUniformType.Float, '(float)');
  Assert.IsTrue(LRuntimeEffect.UniformTypeByName['fCenter'] = TSkRuntimeEffectUniformType.Float2, '(float2)');
  Assert.IsTrue(LRuntimeEffect.UniformTypeByName['fSteps'] = TSkRuntimeEffectUniformType.Int, '(int)');

  Assert.AreEqual(1, LRuntimeEffect.UniformTypeCount[0], 'A scalar uniform has a single element');
  Assert.AreEqual(1, LRuntimeEffect.UniformTypeCountByName['fCenter'], 'A float2 is still a single element');

  Assert.IsFalse(LRuntimeEffect.IsUniformTypeOrdinal(0), 'A float uniform is not ordinal');
  Assert.IsTrue(LRuntimeEffect.IsUniformTypeOrdinalByName('fSteps'), 'An int uniform is ordinal');
end;

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
