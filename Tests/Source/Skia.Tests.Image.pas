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
unit Skia.Tests.Image;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkImageTests }

  [TestFixture]
  TSkImageTests = class(TTestBase)
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
    procedure TestIsOpaque;
    [Test]
    procedure TestIsTextureBacked;
    [TestCase('1', '0.99,JycnJycnJyd//3dnZ2fv7////+/v7/////////////8cfxx/HH8cfxx/HH8cfxx/HH8cfxx/HH8')]
    procedure TestMakeRasterCopy(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestMakeRasterImage(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,8vD48PA4Ph///Pjx839+X//9//37/39f//3//f///9/LAM9A/0C/Av4AvwANgEfgD/AP8AfgAqA')]
    procedure TestMakeShader(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.99,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestPeekPixels(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestSize;
    [TestCase('1', 'horse.webp,500,333')]
    procedure TestSizeFromFile(const AImageFileName: string; const AExpectedWidth, AExpectedHeight: Integer);
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,

  { Skia }
  System.Skia;

{ TSkImageTests }

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

  LMatrix := TMatrix.CreateRotation(45);
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

initialization
  TDUnitX.RegisterTestFixture(TSkImageTests);
end.
