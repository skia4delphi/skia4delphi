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
unit Skia.Tests.Pixmap;

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
  { TSkPixmapTests }

  [TestFixture]
  TSkPixmapTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    function CreatePixmap(const AWidth, AHeight: Integer; out ABuffer: TBytes): ISkPixmap;
  public
    [Test]
    procedure TestAlphas;
    [Test]
    procedure TestColorSpace;
    [Test]
    procedure TestErase;
    [Test]
    procedure TestEraseSubset;
    [Test]
    procedure TestEraseWithColorF;
    [Test]
    procedure TestExtractSubset;
    [Test]
    procedure TestGetColorF;
    [Test]
    procedure TestImageInfo;
    [Test]
    procedure TestPixelAddr;
    [Test]
    procedure TestProperties;
    [Test]
    procedure TestReadPixelsConvertingTheColorType;
    [Test]
    procedure TestScalePixels;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Types,
  System.UITypes;

{ TSkPixmapTests }

function TSkPixmapTests.CreatePixmap(const AWidth, AHeight: Integer;
  out ABuffer: TBytes): ISkPixmap;
begin
  SetLength(ABuffer, AWidth * AHeight * 4);
  Result := TSkPixmap.Create(TSkImageInfo.Create(AWidth, AHeight, TSkColorType.BGRA8888,
    TSkAlphaType.Unpremul, TSkColorSpace.MakeSRGB), ABuffer, AWidth * 4);
  Assert.IsNotNull(Result, 'Invalid ISkPixmap (nil)');
end;

procedure TSkPixmapTests.TestAlphas;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(4, 4, FBuffer);
  Assert.IsTrue(LPixmap.Erase($80FF0000));
  Assert.AreEqual($80 / $FF, LPixmap.Alphas[1, 1], 0.005);
end;

procedure TSkPixmapTests.TestColorSpace;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(2, 2, FBuffer);
  Assert.IsNotNull(LPixmap.ColorSpace, 'The color space passed to the constructor should be kept');
  Assert.IsTrue(LPixmap.ColorSpace.IsSRGB, 'The color space should be sRGB');

  LPixmap.ColorSpace := TSkColorSpace.MakeSRGBLinear;
  Assert.IsNotNull(LPixmap.ColorSpace);
  Assert.IsTrue(LPixmap.ColorSpace.GammaIsLinear, 'The new color space should be linear');

  LPixmap.ColorSpace := nil;
  Assert.IsNull(LPixmap.ColorSpace, 'The color space should be removable');
end;

procedure TSkPixmapTests.TestErase;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(4, 4, FBuffer);
  Assert.IsTrue(LPixmap.Erase(TAlphaColors.Red), 'Erase should succeed');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LPixmap.Colors[0, 0], '(0, 0)');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LPixmap.Colors[3, 3], '(3, 3)');
end;

procedure TSkPixmapTests.TestEraseSubset;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(4, 4, FBuffer);
  Assert.IsTrue(LPixmap.Erase(TAlphaColors.Blue));
  Assert.IsTrue(LPixmap.Erase(TAlphaColors.Red, RectF(0, 0, 2, 2)), 'Erase with a subset should succeed');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LPixmap.Colors[1, 1], 'The subset should be erased');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Blue, LPixmap.Colors[3, 3], 'The rest should be untouched');
end;

procedure TSkPixmapTests.TestEraseWithColorF;
var
  LColor: TAlphaColorF;
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(2, 2, FBuffer);
  Assert.IsTrue(LPixmap.Erase(TAlphaColorF.Create(1, 0, 0, 1), TSkColorSpace.MakeSRGB));
  LColor := LPixmap.ColorsF[0, 0];
  Assert.AreEqual(1.0, LColor.R, 0.005, '(R)');
  Assert.AreEqual(0.0, LColor.G, 0.005, '(G)');
  Assert.AreEqual(0.0, LColor.B, 0.005, '(B)');
  Assert.AreEqual(1.0, LColor.A, 0.005, '(A)');
end;

procedure TSkPixmapTests.TestExtractSubset;
var
  LDestBuffer: TBytes;
  LDest: ISkPixmap;
  LSource: ISkPixmap;
begin
  LSource := CreatePixmap(4, 4, FBuffer);
  Assert.IsTrue(LSource.Erase(TAlphaColors.Blue));
  Assert.IsTrue(LSource.Erase(TAlphaColors.Red, RectF(2, 2, 4, 4)));

  LDest := CreatePixmap(2, 2, LDestBuffer);
  Assert.IsTrue(LSource.ExtractSubset(LDest, Rect(2, 2, 4, 4)), 'ExtractSubset should succeed');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LDest.Colors[0, 0], 'The subset should hold the red corner');

  Assert.IsFalse(LSource.ExtractSubset(LDest, Rect(10, 10, 12, 12)), 'A subset outside the pixmap should fail');
end;

procedure TSkPixmapTests.TestGetColorF;
var
  LColor: TAlphaColorF;
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(2, 2, FBuffer);
  Assert.IsTrue(LPixmap.Erase(TAlphaColors.Lime));
  LColor := LPixmap.ColorsF[0, 0];
  Assert.AreEqual(0.0, LColor.R, 0.005, '(R)');
  Assert.AreEqual(1.0, LColor.G, 0.005, '(G)');
  Assert.AreEqual(0.0, LColor.B, 0.005, '(B)');
  Assert.AreEqual(1.0, LColor.A, 0.005, '(A)');
end;

procedure TSkPixmapTests.TestImageInfo;
var
  LImageInfo: TSkImageInfo;
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(7, 3, FBuffer);
  LImageInfo := LPixmap.ImageInfo;
  Assert.AreEqual(7, LImageInfo.Width, '(Width)');
  Assert.AreEqual(3, LImageInfo.Height, '(Height)');
  Assert.IsTrue(LImageInfo.ColorType = TSkColorType.BGRA8888, '(ColorType)');
  Assert.IsTrue(LImageInfo.AlphaType = TSkAlphaType.Unpremul, '(AlphaType)');
  Assert.IsNotNull(LImageInfo.ColorSpace, '(ColorSpace)');
  Assert.AreEqual(4, LImageInfo.BytesPerPixel, '(BytesPerPixel)');
  Assert.AreEqual<NativeUInt>(7 * 4, LImageInfo.MinRowBytes, '(MinRowBytes)');
end;

procedure TSkPixmapTests.TestPixelAddr;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(4, 4, FBuffer);
  Assert.AreEqual<NativeUInt>(NativeUInt(LPixmap.Pixels), NativeUInt(LPixmap.PixelAddr[0, 0]), 'The first pixel is the buffer start');
  Assert.AreEqual<NativeUInt>(NativeUInt(LPixmap.Pixels) + 4, NativeUInt(LPixmap.PixelAddr[1, 0]), 'The second pixel is 4 bytes ahead');
  Assert.AreEqual<NativeUInt>(NativeUInt(LPixmap.Pixels) + LPixmap.RowBytes, NativeUInt(LPixmap.PixelAddr[0, 1]), 'The second row is RowBytes ahead');
end;

procedure TSkPixmapTests.TestProperties;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := CreatePixmap(5, 9, FBuffer);
  Assert.AreEqual(5, LPixmap.Width, '(Width)');
  Assert.AreEqual(9, LPixmap.Height, '(Height)');
  Assert.IsTrue(LPixmap.ColorType = TSkColorType.BGRA8888, '(ColorType)');
  Assert.IsTrue(LPixmap.AlphaType = TSkAlphaType.Unpremul, '(AlphaType)');
  Assert.AreEqual<NativeUInt>(5 * 4, LPixmap.RowBytes, '(RowBytes)');
  Assert.AreEqual<NativeUInt>(NativeUInt(@FBuffer[0]), NativeUInt(LPixmap.Pixels), '(Pixels)');
end;

procedure TSkPixmapTests.TestReadPixelsConvertingTheColorType;
var
  LDestBuffer: TBytes;
  LDest: ISkPixmap;
  LSource: ISkPixmap;
begin
  LSource := CreatePixmap(2, 2, FBuffer);
  Assert.IsTrue(LSource.Erase(TAlphaColors.Red));

  SetLength(LDestBuffer, 2 * 2);
  LDest := TSkPixmap.Create(TSkImageInfo.Create(2, 2, TSkColorType.Alpha8, TSkAlphaType.Unpremul), LDestBuffer, 2);
  Assert.IsTrue(LSource.ReadPixels(LDest), 'ReadPixels should convert to Alpha8');
  Assert.AreEqual($FF, Integer(LDestBuffer[0]), 'An opaque source should produce an opaque alpha');
end;

procedure TSkPixmapTests.TestScalePixels;
var
  LDestBuffer: TBytes;
  LDest: ISkPixmap;
  LSource: ISkPixmap;
begin
  LSource := CreatePixmap(8, 8, FBuffer);
  Assert.IsTrue(LSource.Erase(TAlphaColors.Red));

  LDest := CreatePixmap(4, 4, LDestBuffer);
  Assert.IsTrue(LSource.ScalePixels(LDest), 'ScalePixels should succeed');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LDest.Colors[0, 0], 'A uniform image should keep its color when scaled');

  Assert.IsTrue(LSource.ScalePixels(LDest, TSkSamplingOptions.High), 'ScalePixels with sampling options should succeed');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LDest.Colors[3, 3]);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPixmapTests);
end.
