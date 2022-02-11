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
unit Skia.Tests.Codec;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  DUnitX.TestFramework,

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkCodecTestBase }

  TSkCodecTestBase = class(TTestBase)
  protected
    function AssetsPath: string; override;
  end;

  { TSkCodecTests }

  [TestFixture]
  TSkCodecTests = class(TSkCodecTestBase)
  public
    [TestCase('SkCodec BMP (girl.bmp)',               'girl.bmp,1736216929')]
    [TestCase('SkCodec DNG (park.dng)',               'park.dng,1955671176')]
    [TestCase('SkCodec GIF Animated (animated.gif)',  'animated.gif,2005275412')]
    [TestCase('SkCodec GIF Animated (developer.gif)', 'developer.gif,2166160918')]
    [TestCase('SkCodec ICO (delphi.ico)',             'delphi.ico,2952345114')]
    [TestCase('SkCodec JPEG (shrek.jpg)',             'shrek.jpg,1173490088')]
    [TestCase('SkCodec PNG (elephant.png)',           'elephant.png,529596050')]
    [TestCase('SkCodec PNG (kung-fu-panda)',          'kung-fu-panda.png,3960679947')]
    [TestCase('SkCodec PNG (emoji1.png)',             'emoji1.png,875861993')]
    [TestCase('SkCodec PNG (emoji2.png)',             'emoji2.png,3865023965')]
    [TestCase('SkCodec PNG (world-time-zone.png)',    'world-time-zone.png,2305552899')]
    [TestCase('SkCodec PNG Animated (animated.png)',  'animated.png,502650454')]
    [TestCase('SkCodec WBMP (mandrill.wbmp)',         'mandrill.wbmp,2160350774')]
    [TestCase('SkCodec WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp,78640289')]
    [TestCase('SkCodec WebP Animated (rocket.webp)',  'rocket.webp,2031700395')]
    procedure TestDecode(const AImageFileName: string; const AExpectedPixmapCRC32: Cardinal);
  end;

  { TSkImageCodecTests }

  [TestFixture]
  TSkImageCodecTests = class(TSkCodecTestBase)
  private
    procedure TestDecodeAndEncode(const AImageFileName: string; const AExpectedPixmapCRC32: Cardinal; const AFormat: TSkEncodedImageFormat; const AQuality: Byte);
  public
    [TestCase('SkImage BMP (girl.bmp)',               'girl.bmp,1736216929')]
    [TestCase('SkCodec DNG (park.dng)',               'park.dng,1955671176')]
    [TestCase('SkImage GIF Animated (animated.gif)',  'animated.gif,2005275412')]
    [TestCase('SkImage GIF Animated (developer.gif)', 'developer.gif,2166160918')]
    [TestCase('SkImage ICO (delphi.ico)',             'delphi.ico,2952345114')]
    [TestCase('SkImage JPEG (shrek.jpg)',             'shrek.jpg,1173490088')]
    [TestCase('SkImage PNG (elephant.png)',           'elephant.png,529596050')]
    [TestCase('SkImage PNG (kung-fu-panda)',          'kung-fu-panda.png,3960679947')]
    [TestCase('SkImage PNG (emoji1.png)',             'emoji1.png,875861993')]
    [TestCase('SkImage PNG (emoji2.png)',             'emoji2.png,3865023965')]
    [TestCase('SkImage PNG (world-time-zone.png)',    'world-time-zone.png,2305552899')]
    [TestCase('SkImage PNG Animated (animated.png)',  'animated.png,502650454')]
    [TestCase('SkImage WBMP (mandrill.wbmp)',         'mandrill.wbmp,2160350774')]
    [TestCase('SkImage WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp,78640289')]
    [TestCase('SkImage WebP Animated (rocket.webp)',  'rocket.webp,2031700395')]
    procedure TestDecode(const AImageFileName: string; const AExpectedPixmapCRC32: Cardinal);
    [TestCase('SkImage Decode/Encode PNG (elephant.png)',        'elephant.png,80,529596050')]
    [TestCase('SkImage Decode/Encode PNG (kung-fu-panda.png)',   'kung-fu-panda.png,0,3960679947')]
    [TestCase('SkImage Decode/Encode PNG (emoji1.png)',          'emoji1.png,100,875861993')]
    [TestCase('SkImage Decode/Encode PNG (emoji2.png)',          'emoji2.png,50,3865023965')]
    [TestCase('SkImage Decode/Encode PNG (world-time-zone.png)', 'world-time-zone.png,100,2305552899')]
    [TestCase('SkImage Decode/Encode PNG (animated.png)',        'animated.png,30,502650454')]
    procedure TestDecodeAndEncodePNG(const AImageFileName: string; const AQuality: Byte; const AExpectedPixmapCRC32: Cardinal);
    [TestCase('SkImage Decode/Encode WebP (kung-fu-panda.webp)', 'kung-fu-panda.webp,100,78640289')]
    procedure TestDecodeAndEncodeWebP(const AImageFileName: string; const AQuality: Byte; const AExpectedPixmapCRC32: Cardinal);
  end;

  { TSkAnimationCodecTests }

  [TestFixture]
  TSkAnimationCodecTests = class(TSkCodecTestBase)
  public
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif)',  'animated.gif,960')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif)', 'developer.gif,5080')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp)',  'rocket.webp,5050')]
    procedure TestDecodeDuration(const AImageFileName: string; const ADuration: Cardinal);
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 0ms)',    'animated.gif,0,2005275412')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 180ms)',  'animated.gif,180,2757121112')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 0ms)',   'developer.gif,0,2166160918')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 450ms)', 'developer.gif,450,415728538')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 0ms)',    'rocket.webp,0,684131660')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 610ms)',  'rocket.webp,610,2534339832')]
    procedure TestDecode(const AImageFileName: string; const AFrameAtMilliseconds, AExpectedPixmapCRC32: Cardinal);
  end;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.IOUtils;

{ TSkCodecTestBase }

function TSkCodecTestBase.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'Codec');
end;

{ TSkCodecTests }

procedure TSkCodecTests.TestDecode(const AImageFileName: string;
  const AExpectedPixmapCRC32: Cardinal);
var
  LCodec: ISkCodec;
  LEncodedBytes: TBytes;
begin
  LEncodedBytes := TFile.ReadAllBytes(AssetsPath + AImageFileName);
  LCodec := TSkCodec.MakeWithoutCopy(LEncodedBytes, Length(LEncodedBytes));
  Assert.AreEqualCRC32(AExpectedPixmapCRC32, LCodec);
end;

{ TSkImageCodecTests }

procedure TSkImageCodecTests.TestDecode(const AImageFileName: string;
  const AExpectedPixmapCRC32: Cardinal);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + AImageFileName);
  Assert.AreEqualCRC32(AExpectedPixmapCRC32, LImage);
end;

procedure TSkImageCodecTests.TestDecodeAndEncode(const AImageFileName: string;
  const AExpectedPixmapCRC32: Cardinal; const AFormat: TSkEncodedImageFormat;
  const AQuality: Byte);
var
  LImage: ISkImage;
  LNewImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + AImageFileName);
  LNewImage := TSkImage.MakeFromEncoded(LImage.Encode(AFormat, AQuality));
  Assert.AreEqualCRC32(AExpectedPixmapCRC32, LNewImage);
end;

procedure TSkImageCodecTests.TestDecodeAndEncodePNG(
  const AImageFileName: string; const AQuality: Byte;
  const AExpectedPixmapCRC32: Cardinal);
begin
  TestDecodeAndEncode(AImageFileName, AExpectedPixmapCRC32, TSkEncodedImageFormat.PNG, AQuality);
end;

procedure TSkImageCodecTests.TestDecodeAndEncodeWebP(
  const AImageFileName: string; const AQuality: Byte;
  const AExpectedPixmapCRC32: Cardinal);
begin
  TestDecodeAndEncode(AImageFileName, AExpectedPixmapCRC32, TSkEncodedImageFormat.WEBP, AQuality);
end;

{ TSkAnimationCodecTests }

procedure TSkAnimationCodecTests.TestDecode(const AImageFileName: string;
  const AFrameAtMilliseconds, AExpectedPixmapCRC32: Cardinal);
var
  LCodec: ISkAnimationCodecPlayer;
begin
  LCodec := TSkAnimationCodecPlayer.MakeFromFile(AssetsPath + AImageFileName);
  LCodec.Seek(AFrameAtMilliseconds);
  Assert.AreEqualCRC32(AExpectedPixmapCRC32, LCodec.Frame);
end;

procedure TSkAnimationCodecTests.TestDecodeDuration(
  const AImageFileName: string; const ADuration: Cardinal);
var
  LCodec: ISkAnimationCodecPlayer;
begin
  LCodec := TSkAnimationCodecPlayer.MakeFromFile(AssetsPath + AImageFileName);
  Assert.AreEqual(ADuration, LCodec.Duration);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkCodecTests);
  TDUnitX.RegisterTestFixture(TSkImageCodecTests);
  TDUnitX.RegisterTestFixture(TSkAnimationCodecTests);
end.
