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
unit Skia.Tests.Image;

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
  { TSkImageTests }

  [TestFixture]
  TSkImageTests = class(TTestBase)
  strict private
    function CreateTestImage: ISkImage;
  public
    [Test]
    procedure TestAlphaType;
    [Test]
    procedure TestColorSpace;
    [Test]
    procedure TestColorType;
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestDecode(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,//34gICYuPn//fjgw9/8/f///vbX3/39///+///////DwNawwADAAMABwADAAMAAwADAAMACwAA')]
    procedure TestDrawImage(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestEncode(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestEncodeToStream;
    [Test]
    procedure TestImageInfo;
    [Test]
    procedure TestIsLazyGenerated;
    [Test]
    procedure TestCubicResamplerSampling;
    [Test]
    procedure TestIsAlphaOnly;
    [Test]
    procedure TestIsOpaque;
    [Test]
    procedure TestIsValid;
    [Test]
    procedure TestMakeFromEncodedStream;
    [Test]
    procedure TestMakeNonTextureImage;
    [Test]
    procedure TestMakeSubset;
    [Test]
    procedure TestMakeWithFilter;
    [Test]
    procedure TestScalePixels;
    [Test]
    procedure TestUniqueId;
    [Test]
    procedure TestIsTextureBacked;
    [TestCase('1', '0.99,JycnJycnJyd//3dnZ2fv7////+/v7/////////////8cfxx/HH8cfxx/HH8cfxx/HH8cfxx/HH8')]
    procedure TestMakeRasterCopy(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestMakeRasterImage(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,+Pjw+N0PBw///PD5309PT//++vvfb99v//7/+//v/+9ngX+A/4D/2T94AfwQ8gH8AdSAXwAnCQc')]
    procedure TestMakeShader(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestPeekPixels(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestSize;
    [TestCase('1', 'horse.webp,500,333')]
    procedure TestSizeFromFile(const AImageFileName: string; const AExpectedWidth, AExpectedHeight: Integer);
  end;

  { TSkImageInfoTests }

  [TestFixture]
  TSkImageInfoTests = class(TTestBase)
  public
    [Test]
    procedure TestByteSize;
    [Test]
    procedure TestMakeVariants;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkImageTests }

function TSkImageTests.CreateTestImage: ISkImage;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(100, 100);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Blue);
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LSurface.Canvas.DrawRect(RectF(10, 10, 50, 50), LPaint);
  Result := LSurface.MakeImageSnapshot;
  Assert.IsNotNull(Result, 'Invalid ISkImage (nil)');
end;

procedure TSkImageTests.TestEncodeToStream;
var
  LImage: ISkImage;
  LStream: TBytesStream;
begin
  LImage := CreateTestImage;
  LStream := TBytesStream.Create;
  try
    Assert.IsTrue(LImage.EncodeToStream(LStream), 'EncodeToStream should succeed');
    Assert.IsTrue(LStream.Size > 0, 'The stream should not be empty');
    Assert.AreEqualPixels(LImage.Encode, Copy(LStream.Bytes, 0, LStream.Size),
      'Encode and EncodeToStream should produce the same image');
  finally
    LStream.Free;
  end;
end;

procedure TSkImageTests.TestImageInfo;
var
  LImage: ISkImage;
  LImageInfo: TSkImageInfo;
begin
  LImage := CreateTestImage;
  LImageInfo := LImage.ImageInfo;
  Assert.AreEqual(LImage.Width, LImageInfo.Width, '(Width)');
  Assert.AreEqual(LImage.Height, LImageInfo.Height, '(Height)');
  Assert.IsTrue(LImageInfo.ColorType = LImage.ColorType, '(ColorType)');
  Assert.IsTrue(LImageInfo.AlphaType = LImage.AlphaType, '(AlphaType)');
  Assert.IsFalse(LImageInfo.IsEmpty, '(IsEmpty)');
  Assert.IsTrue(LImageInfo.IsValid, '(IsValid)');
end;

procedure TSkImageTests.TestIsLazyGenerated;
var
  LImage: ISkImage;
begin
  Assert.IsFalse(CreateTestImage.IsLazyGenerated, 'A raster snapshot is not lazily generated');
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + 'horse.webp');
  Assert.IsNotNull(LImage, 'Invalid ISkImage (nil)');
  Assert.IsTrue(LImage.IsLazyGenerated, 'An encoded image is decoded on demand');
end;

procedure TSkImageTests.TestIsValid;
begin
  Assert.IsTrue(CreateTestImage.IsValid, 'A raster image should be valid without a context');
end;

procedure TSkImageTests.TestMakeFromEncodedStream;
var
  LImage: ISkImage;
  LStream: TStream;
begin
  LStream := TFileStream.Create(ImageAssetsPath + 'horse.webp', fmOpenRead or fmShareDenyWrite);
  try
    LImage := TSkImage.MakeFromEncodedStream(LStream);
  finally
    LStream.Free;
  end;
  Assert.IsNotNull(LImage, 'Invalid ISkImage (nil)');
  Assert.AreEqual(500, LImage.Width, '(Width)');
  Assert.AreEqual(333, LImage.Height, '(Height)');
end;

procedure TSkImageTests.TestMakeNonTextureImage;
var
  LImage: ISkImage;
begin
  LImage := CreateTestImage;
  Assert.IsNotNull(LImage.MakeNonTextureImage, 'A raster image should already be a non texture image');
  Assert.IsFalse(LImage.MakeNonTextureImage.IsTextureBacked);
end;

procedure TSkImageTests.TestMakeSubset;
var
  LImage: ISkImage;
  LSubset: ISkImage;
begin
  LImage := CreateTestImage;
  LSubset := LImage.MakeSubset(Rect(10, 10, 50, 50));
  Assert.IsNotNull(LSubset, 'Invalid ISkImage (nil)');
  Assert.AreEqual(40, LSubset.Width, '(Width)');
  Assert.AreEqual(40, LSubset.Height, '(Height)');
  Assert.AreSameColor(TAlphaColors.Red, LSubset.PeekPixels.Colors[0, 0], 0, 'The subset should start at the red square');
  Assert.AreSameColor(TAlphaColors.Red, LSubset.PeekPixels.Colors[39, 39], 0, 'The subset should end at the red square');

  Assert.IsNull(LImage.MakeSubset(Rect(200, 200, 300, 300)), 'A subset outside the image should fail');
end;

procedure TSkImageTests.TestMakeWithFilter;
var
  LImage: ISkImage;
  LOffset: TPoint;
  LOutSubset: TRect;
  LResult: ISkImage;
begin
  LImage := CreateTestImage;
  LResult := LImage.MakeWithFilter(TSkImageFilter.MakeOffset(10, 10), Rect(0, 0, 100, 100),
    Rect(0, 0, 100, 100), LOutSubset, LOffset);
  Assert.IsNotNull(LResult, 'Invalid ISkImage (nil)');
  Assert.AreEqual(90, LOutSubset.Width, 'Offsetting by ten inside a 100x100 clip leaves 90 columns visible');
  Assert.AreEqual(90, LOutSubset.Height, 'Offsetting by ten inside a 100x100 clip leaves 90 rows visible');
  Assert.AreEqual(10, LOffset.X, 'The offset should report where the result must be drawn');
  Assert.AreEqual(10, LOffset.Y, 'The offset should report where the result must be drawn');
  Assert.IsTrue(LResult.Width >= LOutSubset.Width, 'The subset should fit in the result');
end;

procedure TSkImageTests.TestScalePixels;
var
  LBuffer: TBytes;
  LImage: ISkImage;
  LPixmap: ISkPixmap;
begin
  LImage := CreateTestImage;
  SetLength(LBuffer, 50 * 50 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(50, 50, TSkColorType.BGRA8888, TSkAlphaType.Premul,
    TSkColorSpace.MakeSRGB), LBuffer, 50 * 4);
  Assert.IsTrue(LImage.ScalePixels(LPixmap), 'ScalePixels should succeed');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[15, 15], 4, 'The red square should be scaled to half its size');
  Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[45, 45], 4, 'The background should be preserved');
end;

procedure TSkImageTests.TestUniqueId;
var
  LImage: ISkImage;
begin
  LImage := CreateTestImage;
  Assert.AreEqual<NativeUInt>(LImage.UniqueId, LImage.UniqueId, 'The identifier should be stable');
  Assert.AreNotEqual<NativeUInt>(LImage.UniqueId, CreateTestImage.UniqueId, 'Two images should have different identifiers');
end;

procedure TSkImageTests.TestCubicResamplerSampling;
var
  LImage: ISkImage;
  LMitchell: TBytes;
  LMitchellPixmap: ISkPixmap;
  LNearest: TBytes;
  LNearestPixmap: ISkPixmap;
begin
  Assert.IsTrue(TSkCubicResampler.Mitchell <> TSkCubicResampler.CatmullRom, 'The two cubic resamplers differ');
  Assert.AreEqual(1 / 3, TSkCubicResampler.Mitchell.B, TEpsilon.Vector, 'The Mitchell B coefficient');
  Assert.AreEqual(1 / 3, TSkCubicResampler.Mitchell.C, TEpsilon.Vector, 'The Mitchell C coefficient');

  LImage := CreateTestImage;
  SetLength(LMitchell, 250 * 250 * 4);
  SetLength(LNearest, 250 * 250 * 4);
  LMitchellPixmap := TSkPixmap.Create(TSkImageInfo.Create(250, 250), LMitchell, 250 * 4);
  LNearestPixmap := TSkPixmap.Create(TSkImageInfo.Create(250, 250), LNearest, 250 * 4);
  Assert.IsTrue(LImage.ScalePixels(LMitchellPixmap, TSkSamplingOptions.Create(TSkCubicResampler.Mitchell)),
    'Scaling with the Mitchell resampler should succeed');
  Assert.IsTrue(LImage.ScalePixels(LNearestPixmap, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None)),
    'Scaling with the nearest filter should succeed');
  Assert.IsFalse(CompareMem(@LMitchell[0], @LNearest[0], 250 * 250 * 4),
    'The cubic resampler should not give the same result as the nearest filter');
end;

procedure TSkImageTests.TestIsAlphaOnly;
var
  LBuffer: TBytes;
  LPixmap: ISkPixmap;
begin
  Assert.IsFalse(CreateTestImage.IsAlphaOnly, 'A BGRA image is not alpha only');

  SetLength(LBuffer, 4 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(4, 4, TSkColorType.Alpha8, TSkAlphaType.Premul), LBuffer, 4);
  Assert.IsTrue(TSkImage.MakeRasterCopy(LPixmap).IsAlphaOnly, 'An Alpha8 image is alpha only');
end;

procedure TSkImageTests.TestAlphaType;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Premul));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.AlphaType = TSkAlphaType.Premul);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(15, 1, SkNative32ColorType, TSkAlphaType.Opaque));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.AlphaType = TSkAlphaType.Opaque);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(1, 1, SkNative32ColorType, TSkAlphaType.Unpremul));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.AlphaType = TSkAlphaType.Unpremul);
end;

procedure TSkImageTests.TestColorSpace;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(TSkColorSpace.MakeSRGB.IsEqual(LImage.ColorSpace));
  Assert.IsFalse(TSkColorSpace.MakeSRGBLinear.IsEqual(LImage.ColorSpace));
end;

procedure TSkImageTests.TestColorType;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.ColorType = SkNative32ColorType);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(15, 1, TSkColorType.RGBA8888));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.ColorType = TSkColorType.RGBA8888);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(1, 1, TSkColorType.BGRA8888));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.ColorType = TSkColorType.BGRA8888);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(1, 1, TSkColorType.RGBAF32));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.ColorType = TSkColorType.RGBAF32);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(1, 1, TSkColorType.Gray8));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.ColorType = TSkColorType.Gray8);
end;

procedure TSkImageTests.TestDecode(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
end;

procedure TSkImageTests.TestDrawImage(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: ISkImage;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(256, 256);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  LSurface.Canvas.DrawImage(LImage, 20, 10);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkImageTests.TestEncode(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  LImage := TSkImage.MakeFromEncoded(LImage.Encode);
  Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
end;

procedure TSkImageTests.TestIsOpaque;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Premul));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsFalse(LImage.IsOpaque);
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Opaque));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsTrue(LImage.IsOpaque);
end;

procedure TSkImageTests.TestIsTextureBacked;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Premul));
  LImage := LSurface.MakeImageSnapshot;
  Assert.IsFalse(LImage.IsTextureBacked);
end;

procedure TSkImageTests.TestMakeRasterCopy(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LImageInfo: TSkImageInfo;
  LPixmap: ISkPixmap;
  LImage1: ISkImage;
  LImage2: ISkImage;
  LData: TBytes;
begin
  LSurface := TSkSurface.MakeRaster(256, 50);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LData := [$CA, $DA, $CA, $C9, $A3,
    $AC, $A8, $89, $A7, $87,
    $9B, $B5, $E5, $95, $46,
    $90, $81, $C5, $71, $33,
    $75, $55, $44, $40, $30];

  LImageInfo := TSkImageInfo.Create(5, 5, TSkColorType.Gray8, TSkAlphaType.Opaque);
  LPixmap := TSkPixmap.Create(LImageInfo, LData, Length(LData) div 5);
  LImage1 := TSkImage.MakeFromRaster(LPixmap);
  LImage2 := TSkImage.MakeRasterCopy(LPixmap);

  LData[12] := $00;

  LSurface.Canvas.Scale(10, 10);
  LSurface.Canvas.DrawImage(LImage1, 0, 0);
  LSurface.Canvas.DrawImage(LImage2, 10, 0);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkImageTests.TestMakeRasterImage(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName).MakeRasterImage;
  Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
end;

procedure TSkImageTests.TestMakeShader(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LImage: ISkImage;
  LMatrix: TMatrix;
  LPaint: ISkPaint;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  LSurface := TSkSurface.MakeRaster(256, 256);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LMatrix := TMatrix.CreateRotation(DegToRad(45));
  LPaint := TSkPaint.Create;
  LPaint.Shader := LImage.MakeShader(LMatrix, TSkTileMode.Repeat, TSkTileMode.Mirror);
  LSurface.Canvas.DrawPaint(LPaint);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkImageTests.TestPeekPixels(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: ISkImage;
  LPixmap: ISkPixmap;
  LPixels: TBytes;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  Assert.IsNull(LImage.PeekPixels);

  SetLength(LPixels, SkBytesPerPixel[LImage.ColorType] * LImage.Width * LImage.Height);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(LImage.Width, LImage.Height, LImage.ColorType, LImage.AlphaType), LPixels, SkBytesPerPixel[LImage.ColorType] * LImage.Width);
  LImage.ReadPixels(LPixmap);

  LImage := TSkImage.MakeFromRaster(LPixmap);
  Assert.AreSimilar(AExpectedImageHash, LImage.PeekPixels, AMinSimilarity);
end;

procedure TSkImageTests.TestSize;
var
  LSurface: ISkSurface;
  LImage: ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(3, 4);
  LImage := LSurface.MakeImageSnapshot;
  Assert.AreEqual(3, LImage.Width);
  Assert.AreEqual(4, LImage.Height);
end;

procedure TSkImageTests.TestSizeFromFile(const AImageFileName: string; const AExpectedWidth, AExpectedHeight: Integer);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  Assert.AreEqual(AExpectedWidth, LImage.Width);
  Assert.AreEqual(AExpectedHeight, LImage.Height);
end;

{ TSkImageInfoTests }

procedure TSkImageInfoTests.TestByteSize;
var
  LImageInfo: TSkImageInfo;
begin
  LImageInfo := TSkImageInfo.Create(10, 4, TSkColorType.BGRA8888);
  Assert.AreEqual(2, LImageInfo.ShiftPerPixel, 'Four bytes per pixel is a shift of two');
  Assert.AreEqual<NativeUInt>(40, LImageInfo.MinRowBytes, '(MinRowBytes)');
  Assert.AreEqual<NativeUInt>(160, LImageInfo.MinByteSize, '(MinByteSize)');
  Assert.AreEqual<NativeUInt>(400, LImageInfo.ByteSize(100), 'ByteSize should follow the given row stride');
  Assert.IsTrue(LImageInfo.IsValidRowBytes(40), 'The minimum row stride is valid');
  Assert.IsFalse(LImageInfo.IsValidRowBytes(20), 'A row stride below the minimum is invalid');
end;

procedure TSkImageInfoTests.TestMakeVariants;
var
  LImageInfo: TSkImageInfo;
begin
  LImageInfo := TSkImageInfo.Create(10, 4, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);

  Assert.IsTrue(LImageInfo.MakeAlphaType(TSkAlphaType.Opaque).AlphaType = TSkAlphaType.Opaque, '(MakeAlphaType)');
  Assert.IsTrue(LImageInfo.MakeColorType(TSkColorType.RGBA8888).ColorType = TSkColorType.RGBA8888, '(MakeColorType)');
  Assert.AreEqual(7, LImageInfo.MakeDimensions(7, 9).Width, '(MakeDimensions width)');
  Assert.AreEqual(9, LImageInfo.MakeDimensions(7, 9).Height, '(MakeDimensions height)');
  Assert.IsNull(LImageInfo.MakeColorSpace(nil).ColorSpace, '(MakeColorSpace)');
  Assert.IsTrue(LImageInfo.AlphaType = TSkAlphaType.Premul, 'The source should not be changed');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkImageTests);
  TDUnitX.RegisterTestFixture(TSkImageInfoTests);
end.
