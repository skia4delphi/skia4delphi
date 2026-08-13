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
unit Skia.Tests.ImageFilter;

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
  { TSkImageFilterTests }

  [TestFixture]
  TSkImageFilterTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    FPixmap: ISkPixmap;
    function CreateSurface(const ASize: Integer): ISkSurface;
    function DrawWithFilter(const AFilter: ISkImageFilter): ISkPixmap;
    function RedSquareImage: ISkImage;
  public
    [Test]
    procedure TestComputeFastBoundsOfBlur;
    [Test]
    procedure TestComputeFastBoundsOfOffset;
    [Test]
    procedure TestDropShadowKeepsTheSource;
    [Test]
    procedure TestDropShadowOnlyRemovesTheSource;
    [Test]
    procedure TestFactoriesReturnAFilter;
    [Test]
    procedure TestLightFactoriesReturnAFilter;
    [Test]
    procedure TestMakeColorFilter;
    [Test]
    procedure TestMakePicture;
    [Test]
    procedure TestMakeRuntimeShader;
    [Test]
    procedure TestMakeOffsetMovesTheDrawing;
    [Test]
    procedure TestMakeTileRepeatsTheSource;
    [Test]
    procedure TestMakeWithLocalMatrix;
    [Test]
    procedure TestMatrixTransform;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkImageFilterTests }

procedure TSkImageFilterTests.TestMakePicture;
var
  LPaint: ISkPaint;
  LPicture: ISkPicture;
  LPictureRecorder: ISkPictureRecorder;
  LPixmap: ISkPixmap;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LPictureRecorder := TSkPictureRecorder.Create;
  LPictureRecorder.BeginRecording(100, 100).DrawRect(RectF(0, 0, 40, 40), LPaint);
  LPicture := LPictureRecorder.FinishRecording;

  LPixmap := DrawWithFilter(TSkImageFilter.MakePicture(LPicture));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[20, 20], 0, 'The picture should be drawn by the filter');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[60, 60], 0, 'The filter should replace the source');

  LPixmap := DrawWithFilter(TSkImageFilter.MakePicture(LPicture, RectF(0, 0, 20, 20)));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[10, 10], 0, 'The crop rect should keep its area');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[30, 30], 0, 'The crop rect should cut the rest out');
end;

procedure TSkImageFilterTests.TestMakeRuntimeShader;
const
  SkSL =
    'uniform shader fContent;' + sLineBreak +
    'half4 main(float2 p) { return fContent.eval(p).bgra; }';
var
  LError: string;
  LPixmap: ISkPixmap;
  LRuntimeEffect: ISkRuntimeEffect;
  LRuntimeShaderBuilder: ISkRuntimeShaderBuilder;
begin
  LRuntimeEffect := TSkRuntimeEffect.MakeForShader(SkSL, LError);
  Assert.IsNotNull(LRuntimeEffect, 'Could not build the shader: ' + LError);
  LRuntimeShaderBuilder := TSkRuntimeShaderBuilder.Create(LRuntimeEffect);

  LPixmap := DrawWithFilter(TSkImageFilter.MakeRuntimeShader(LRuntimeShaderBuilder, 'fContent'));
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[20, 20], 1, 'The shader should swap the red and blue channels');

  LPixmap := DrawWithFilter(TSkImageFilter.MakeRuntimeShader(LRuntimeShaderBuilder,
    TArray<string>.Create('fContent'), TArray<ISkImageFilter>.Create(nil)));
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[20, 20], 1, 'The children overload should behave the same');
end;

function TSkImageFilterTests.CreateSurface(const ASize: Integer): ISkSurface;
begin
  SetLength(FBuffer, ASize * ASize * 4);
  FPixmap := TSkPixmap.Create(TSkImageInfo.Create(ASize, ASize, TSkColorType.BGRA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), FBuffer, ASize * 4);
  Result := TSkSurface.MakeRasterDirect(FPixmap);
  Assert.IsNotNull(Result, 'Invalid ISkSurface (nil)');
  Result.Canvas.Clear(TAlphaColors.Null);
end;

function TSkImageFilterTests.DrawWithFilter(
  const AFilter: ISkImageFilter): ISkPixmap;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface(100);
  LPaint := TSkPaint.Create;
  LPaint.ImageFilter := AFilter;
  LSurface.Canvas.DrawImage(RedSquareImage, 0, 0, LPaint);
  Result := FPixmap;
end;

function TSkImageFilterTests.RedSquareImage: ISkImage;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(100, 100);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LSurface.Canvas.DrawRect(RectF(0, 0, 50, 50), LPaint);
  Result := LSurface.MakeImageSnapshot;
end;

procedure TSkImageFilterTests.TestComputeFastBoundsOfBlur;
var
  LFilter: ISkImageFilter;
begin
  LFilter := TSkImageFilter.MakeBlur(10, 10);
  Assert.IsNotNull(LFilter);
  Assert.IsTrue(LFilter.CanComputeFastBounds, 'A blur should be able to compute fast bounds');
  Assert.AreSameRect(RectF(-30, -30, 130, 130), LFilter.ComputeFastBounds(RectF(0, 0, 100, 100)), TEpsilon.Position,
    'A blur grows the bounds by three sigmas in every direction');
end;

procedure TSkImageFilterTests.TestComputeFastBoundsOfOffset;
var
  LFilter: ISkImageFilter;
begin
  LFilter := TSkImageFilter.MakeOffset(10, 20);
  Assert.IsNotNull(LFilter);
  Assert.IsTrue(LFilter.CanComputeFastBounds, 'An offset should be able to compute fast bounds');
  Assert.AreSameRect(RectF(10, 20, 110, 120), LFilter.ComputeFastBounds(RectF(0, 0, 100, 100)), TEpsilon.Position);
end;

procedure TSkImageFilterTests.TestDropShadowKeepsTheSource;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeDropShadow(20, 20, 0, 0, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[10, 10], 2, 'The source should still be drawn');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[60, 60], 2, 'The shadow should be drawn at the offset');
end;

procedure TSkImageFilterTests.TestDropShadowOnlyRemovesTheSource;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeDropShadowOnly(20, 20, 0, 0, TAlphaColors.Blue));
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[10, 10], 2, 'The source should not be drawn');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[60, 60], 2, 'Only the shadow should be drawn');
end;

procedure TSkImageFilterTests.TestFactoriesReturnAFilter;
var
  LFilter: ISkImageFilter;
begin
  LFilter := TSkImageFilter.MakeBlur(2, 2);
  Assert.IsNotNull(TSkImageFilter.MakeAlphaThreshold(Rect(0, 0, 10, 10), 0.2, 0.7), '(MakeAlphaThreshold)');
  Assert.IsNotNull(TSkImageFilter.MakeArithmetic(1, 0, 0, 0, False, LFilter), '(MakeArithmetic)');
  Assert.IsNotNull(TSkImageFilter.MakeBlend(TSkBlendMode.SrcOver, LFilter), '(MakeBlend)');
  Assert.IsNotNull(TSkImageFilter.MakeBlur(2, 2, RectF(0, 0, 10, 10)), '(MakeBlur with crop)');
  Assert.IsNotNull(TSkImageFilter.MakeCompose(LFilter, LFilter), '(MakeCompose)');
  Assert.IsNotNull(TSkImageFilter.MakeDilate(2, 2), '(MakeDilate)');
  Assert.IsNotNull(TSkImageFilter.MakeDisplacementMap(TSkColorChannel.R, TSkColorChannel.G, 4, LFilter), '(MakeDisplacementMap)');
  Assert.IsNotNull(TSkImageFilter.MakeErode(2, 2), '(MakeErode)');
  Assert.IsNotNull(TSkImageFilter.MakeImage(RedSquareImage), '(MakeImage)');
  Assert.IsNotNull(TSkImageFilter.MakeImage(RedSquareImage, RectF(0, 0, 10, 10), RectF(0, 0, 20, 20)), '(MakeImage with rects)');
  Assert.IsNotNull(TSkImageFilter.MakeMagnifier(RectF(10, 10, 40, 40), 2), '(MakeMagnifier)');
  Assert.IsNotNull(TSkImageFilter.MakeMatrixConvolution(TSize.Create(1, 1), [1], 1, 0, TPoint.Create(0, 0), TSkTileMode.Clamp, True), '(MakeMatrixConvolution)');
  Assert.IsNotNull(TSkImageFilter.MakeMerge([LFilter, LFilter]), '(MakeMerge)');
  Assert.IsNotNull(TSkImageFilter.MakeOffset(1, 1), '(MakeOffset)');
  Assert.IsNotNull(TSkImageFilter.MakeShader(TSkShader.MakeColor(TAlphaColors.Red), False), '(MakeShader)');
  Assert.IsNotNull(TSkImageFilter.MakeTile(Rect(0, 0, 10, 10), Rect(0, 0, 20, 20)), '(MakeTile)');
end;

procedure TSkImageFilterTests.TestLightFactoriesReturnAFilter;
begin
  Assert.IsNotNull(TSkImageFilter.MakeDistantLitDiffuse(Point3D(1, 1, 1), TAlphaColors.White, 1, 1), '(MakeDistantLitDiffuse)');
  Assert.IsNotNull(TSkImageFilter.MakeDistantLitSpecular(Point3D(1, 1, 1), TAlphaColors.White, 1, 1, 1), '(MakeDistantLitSpecular)');
  Assert.IsNotNull(TSkImageFilter.MakePointLitDiffuse(Point3D(1, 1, 1), TAlphaColors.White, 1, 1), '(MakePointLitDiffuse)');
  Assert.IsNotNull(TSkImageFilter.MakePointLitSpecular(Point3D(1, 1, 1), TAlphaColors.White, 1, 1, 1), '(MakePointLitSpecular)');
  Assert.IsNotNull(TSkImageFilter.MakeSpotLitDiffuse(Point3D(0, 0, 10), Point3D(0, 0, 0), 1, 45, TAlphaColors.White, 1, 1), '(MakeSpotLitDiffuse)');
  Assert.IsNotNull(TSkImageFilter.MakeSpotLitSpecular(Point3D(0, 0, 10), Point3D(0, 0, 0), 1, 45, TAlphaColors.White, 1, 1, 1), '(MakeSpotLitSpecular)');
end;

procedure TSkImageFilterTests.TestMakeColorFilter;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeColorFilter(TSkColorFilter.MakeBlend(TAlphaColors.Lime, TSkBlendMode.SrcIn)));
  Assert.AreSameColor(TAlphaColors.Lime, LPixmap.Colors[10, 10], 2, 'The color filter should replace the color');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[80, 80], 2, 'The transparent area should stay transparent');
end;

procedure TSkImageFilterTests.TestMakeOffsetMovesTheDrawing;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeOffset(30, 30));
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[10, 10], 2, 'The original position should be empty');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[40, 40], 2, 'The drawing should be moved by the offset');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[79, 79], 2, 'The whole square should be moved');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[81, 81], 2, 'Nothing should be drawn past the moved square');
end;

procedure TSkImageFilterTests.TestMakeTileRepeatsTheSource;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeTile(Rect(0, 0, 50, 50), Rect(0, 0, 100, 100)));
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[10, 10], 2, 'The first tile should be drawn');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[75, 75], 2, 'The source tile should be repeated');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[75, 10], 2, 'The source tile should be repeated horizontally');
end;

procedure TSkImageFilterTests.TestMakeWithLocalMatrix;
var
  LFilter: ISkImageFilter;
  LPixmap: ISkPixmap;
begin
  LFilter := TSkImageFilter.MakeOffset(10, 10).MakeWithLocalMatrix(TMatrix.CreateScaling(2, 2));
  Assert.IsNotNull(LFilter);
  LPixmap := DrawWithFilter(LFilter);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[25, 25], 2, 'The local matrix should scale the offset');
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[10, 10], 2, 'The scaled offset should move the drawing further');
end;

procedure TSkImageFilterTests.TestMatrixTransform;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := DrawWithFilter(TSkImageFilter.MakeMatrixTransform(TMatrix.CreateTranslation(40, 0)));
  Assert.AreSameColor(TAlphaColors.Null, LPixmap.Colors[10, 10], 2, 'The original position should be empty');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[60, 10], 2, 'The drawing should be translated');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkImageFilterTests);
end.
