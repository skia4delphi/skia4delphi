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
unit Skia.Tests.Shader;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkShaderTests }

  [TestFixture]
  TSkShaderTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    FPixmap: ISkPixmap;
    function CreateSurface(const ASize: Integer): ISkSurface;
    function DrawWithShader(const AShader: ISkShader; const ASize: Integer = 100): ISkPixmap;
  public
    [Test]
    procedure TestMakeBlend;
    [Test]
    procedure TestMakeColor;
    [Test]
    procedure TestMakeColorF;
    [Test]
    procedure TestMakeEmpty;
    [Test]
    procedure TestMakePicture;
    [Test]
    procedure TestMakeGradientLinear;
    [Test]
    procedure TestMakeGradientRadial;
    [Test]
    procedure TestMakeGradientSweep;
    [Test]
    procedure TestMakeGradientTwoPointConical;
    [Test]
    procedure TestMakeImage;
    [Test]
    procedure TestMakePerlinNoise;
    [Test]
    procedure TestMakeWithColorFilter;
    [Test]
    procedure TestMakeWithLocalMatrix;
    [Test]
    procedure TestTileModes;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkShaderTests }

procedure TSkShaderTests.TestMakePicture;
var
  LPaint: ISkPaint;
  LPicture: ISkPicture;
  LPictureRecorder: ISkPictureRecorder;
  LPixmap: ISkPixmap;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LPictureRecorder := TSkPictureRecorder.Create;
  LPictureRecorder.BeginRecording(20, 20).DrawRect(RectF(0, 0, 10, 10), LPaint);
  LPicture := LPictureRecorder.FinishRecording;

  LPixmap := DrawWithShader(TSkShader.MakePicture(LPicture, TSkTileMode.Repeat, TSkTileMode.Repeat));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 0, 'The picture should be painted');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[25, 25], 0, 'The picture should be repeated');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[15, 15], 0, 'The empty part of the tile should stay empty');

  LPixmap := DrawWithShader(TSkShader.MakePicture(LPicture, TMatrix.CreateTranslation(10, 0),
    TSkTileMode.Repeat, TSkTileMode.Repeat));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[15, 5], 0, 'The local matrix should move the tile');

  LPixmap := DrawWithShader(TSkShader.MakePicture(LPicture, RectF(0, 0, 40, 40), TSkTileMode.Repeat, TSkTileMode.Repeat));
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[25, 25], 0, 'A wider tile rect should push the repetition away');
end;

function TSkShaderTests.CreateSurface(const ASize: Integer): ISkSurface;
begin
  SetLength(FBuffer, ASize * ASize * 4);
  FPixmap := TSkPixmap.Create(TSkImageInfo.Create(ASize, ASize, TSkColorType.BGRA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), FBuffer, ASize * 4);
  Result := TSkSurface.MakeRasterDirect(FPixmap);
  Assert.IsNotNull(Result, 'Invalid ISkSurface (nil)');
  Result.Canvas.Clear(TAlphaColors.Null);
end;

function TSkShaderTests.DrawWithShader(const AShader: ISkShader;
  const ASize: Integer): ISkPixmap;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  Assert.IsNotNull(AShader, 'Invalid ISkShader (nil)');
  LSurface := CreateSurface(ASize);
  LPaint := TSkPaint.Create;
  LPaint.Shader := AShader;
  LSurface.Canvas.DrawPaint(LPaint);
  Result := FPixmap;
end;

procedure TSkShaderTests.TestMakeBlend;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeBlend(TSkBlendMode.Src,
    TSkShader.MakeColor(TAlphaColors.Red), TSkShader.MakeColor(TAlphaColors.Lime)));
  Assert.AreSameColor(TAlphaColors.Lime, LPixmap.Colors[50, 50], 8, 'The Src blend mode should keep the source shader');
end;

procedure TSkShaderTests.TestMakeColor;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeColor(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 0], 8, '(0, 0)');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[99, 99], 8, '(99, 99)');
end;

procedure TSkShaderTests.TestMakeColorF;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeColor(TAlphaColorF.Create(0, 0, 1, 1), TSkColorSpace.MakeSRGB));
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[50, 50], 8);
end;

procedure TSkShaderTests.TestMakeEmpty;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeEmpty);
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[50, 50], 8, 'An empty shader should draw nothing');
end;

procedure TSkShaderTests.TestMakeGradientLinear;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(100, 0),
    TAlphaColors.Red, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 50], 8, 'The gradient should start red');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[99, 50], 8, 'The gradient should end blue');
  Assert.AreSameColor(LPixmap.Colors[50, 0], LPixmap.Colors[50, 99], 8, 'A horizontal gradient should not change vertically');

  LPixmap := DrawWithShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(100, 0),
    [TAlphaColors.Red, TAlphaColors.Lime, TAlphaColors.Blue], TArray<Single>(nil)));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 50], 8, '(multi color start)');
  Assert.AreSameColor(TAlphaColors.Lime, LPixmap.Colors[50, 50], 8, '(multi color middle)');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[99, 50], 8, '(multi color end)');
end;

procedure TSkShaderTests.TestMakeGradientRadial;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeGradientRadial(PointF(50, 50), 50,
    TAlphaColors.Red, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[50, 50], 8, 'The center should be the center color');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[0, 0], 8, 'The corner is past the radius, so it is clamped to the edge color');
  Assert.AreSameColor(LPixmap.Colors[50, 20], LPixmap.Colors[20, 50], 8, 'A radial gradient should be symmetric');
end;

procedure TSkShaderTests.TestMakeGradientSweep;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeGradientSweep(PointF(50, 50),
    TAlphaColors.Red, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[99, 50], 8, 'The sweep should start at angle zero');
  Assert.AreNotEqual<TAlphaColor>(LPixmap.Colors[99, 50], LPixmap.Colors[50, 99], 'The color should change with the angle');
end;

procedure TSkShaderTests.TestMakeGradientTwoPointConical;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeGradientTwoPointConical(PointF(50, 50), 0,
    PointF(50, 50), 50, TAlphaColors.Red, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[50, 50], 8, 'The inner circle should be the first color');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[0, 0], 8, 'Past the outer circle the last color is clamped');
end;

procedure TSkShaderTests.TestMakeImage;
var
  LImage: ISkImage;
  LPaint: ISkPaint;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(10, 10);
  LSurface.Canvas.Clear(TAlphaColors.Red);
  LImage := LSurface.MakeImageSnapshot;

  LPixmap := DrawWithShader(LImage.MakeShader(TSkTileMode.&Repeat, TSkTileMode.&Repeat));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 8, 'The image should be drawn');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[95, 95], 8, 'The image should be repeated');

  LPixmap := DrawWithShader(LImage.MakeRawShader(TSkTileMode.Decal, TSkTileMode.Decal));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 8, 'The raw shader should draw the image');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[95, 95], 8, 'The decal tile mode should not repeat');

  LPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeImage(LImage, TSkSamplingOptions.High, TSkTileMode.Clamp, TSkTileMode.Clamp);
  Assert.IsNotNull(LPaint.Shader, '(TSkShader.MakeImage)');
end;

procedure TSkShaderTests.TestMakePerlinNoise;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakePerlinNoiseFractalNoise(0.05, 0.05, 4, 0));
  Assert.AreNotEqual<TAlphaColor>(LPixmap.Colors[10, 10], LPixmap.Colors[80, 80], 'Fractal noise should not be uniform');

  LPixmap := DrawWithShader(TSkShader.MakePerlinNoiseTurbulence(0.05, 0.05, 4, 0));
  Assert.AreNotEqual<TAlphaColor>(LPixmap.Colors[10, 10], LPixmap.Colors[80, 80], 'Turbulence should not be uniform');

  Assert.IsNotNull(TSkShader.MakePerlinNoiseFractalNoise(0.05, 0.05, 4, 0, TSize.Create(50, 50)), '(tiled fractal noise)');
  Assert.IsNotNull(TSkShader.MakePerlinNoiseTurbulence(0.05, 0.05, 4, 0, TSize.Create(50, 50)), '(tiled turbulence)');
end;

procedure TSkShaderTests.TestMakeWithColorFilter;
var
  LPixmap: ISkPixmap;
  LShader: ISkShader;
begin
  LShader := TSkShader.MakeColor(TAlphaColors.Red).MakeWithColorFilter(
    TSkColorFilter.MakeBlend(TAlphaColors.Blue, TSkBlendMode.Src));
  LPixmap := DrawWithShader(LShader);
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[50, 50], 8, 'The color filter should replace the shader color');
end;

procedure TSkShaderTests.TestMakeWithLocalMatrix;
var
  LPixmap: ISkPixmap;
  LShader: ISkShader;
begin
  LShader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(50, 0), TAlphaColors.Red, TAlphaColors.Blue)
    .MakeWithLocalMatrix(TMatrix.CreateScaling(2, 1));
  LPixmap := DrawWithShader(LShader);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 50], 8, 'The gradient should still start red');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[99, 50], 8, 'The local matrix should stretch the gradient to the full width');
end;

procedure TSkShaderTests.TestTileModes;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(50, 0),
    TAlphaColors.Red, TAlphaColors.Blue, TSkTileMode.&Repeat));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[50, 50], 8, 'The repeat tile mode should restart the gradient');

  LPixmap := DrawWithShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(50, 0),
    TAlphaColors.Red, TAlphaColors.Blue, TSkTileMode.Mirror));
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[50, 50], 8, 'The mirror tile mode should reverse the gradient');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[99, 50], 8, 'The mirrored gradient ends with the first color');

  LPixmap := DrawWithShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(50, 0),
    TAlphaColors.Red, TAlphaColors.Blue, TSkTileMode.Decal));
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[99, 50], 8, 'The decal tile mode should not paint outside the gradient');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkShaderTests);
end.
