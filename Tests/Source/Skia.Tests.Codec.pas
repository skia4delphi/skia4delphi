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
unit Skia.Tests.Codec;

interface

{$SCOPEDENUMS ON}

{$IFNDEF MSWINDOWS}
  {$DEFINE SUPPORT_DNG}
{$ENDIF}

uses
  { Delphi }
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

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
    [TestCase('BMP (girl.bmp)',               'girl.bmp')]
    {$IFDEF SUPPORT_DNG}
    [TestCase('DNG (park.dng)',               'park.dng')]
    {$ENDIF}
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    [TestCase('ICO (delphi.ico)',             'delphi.ico')]
    [TestCase('JPEG (abnt.jpg)',              'abnt.jpg')]
    [TestCase('JPEG (shrek.jpg)',             'shrek.jpg')]
    [TestCase('PNG (elephant.png)',           'elephant.png')]
    [TestCase('PNG (kung-fu-panda)',          'kung-fu-panda.png')]
    [TestCase('PNG (emoji1.png)',             'emoji1.png')]
    [TestCase('PNG (emoji2.png)',             'emoji2.png')]
    [TestCase('PNG (world-time-zone.png)',    'world-time-zone.png')]
    [TestCase('PNG Animated (animated.png)',  'animated.png')]
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeFile(const AImageFileName: string);
    [TestCase('BMP (girl.bmp)',               'girl.bmp')]
    {$IFDEF SUPPORT_DNG}
    [TestCase('DNG (park.dng)',               'park.dng')]
    {$ENDIF}
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    [TestCase('ICO (delphi.ico)',             'delphi.ico')]
    [TestCase('JPEG (abnt.jpg)',              'abnt.jpg')]
    [TestCase('JPEG (shrek.jpg)',             'shrek.jpg')]
    [TestCase('PNG (elephant.png)',           'elephant.png')]
    [TestCase('PNG (kung-fu-panda)',          'kung-fu-panda.png')]
    [TestCase('PNG (emoji1.png)',             'emoji1.png')]
    [TestCase('PNG (emoji2.png)',             'emoji2.png')]
    [TestCase('PNG (world-time-zone.png)',    'world-time-zone.png')]
    [TestCase('PNG Animated (animated.png)',  'animated.png')]
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeStream(const AImageFileName: string);
    [TestCase('BMP (girl.bmp)',               'girl.bmp')]
    {$IFDEF SUPPORT_DNG}
    [TestCase('DNG (park.dng)',               'park.dng')]
    {$ENDIF}
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    [TestCase('ICO (delphi.ico)',             'delphi.ico')]
    [TestCase('JPEG (abnt.jpg)',              'abnt.jpg')]
    [TestCase('JPEG (shrek.jpg)',             'shrek.jpg')]
    [TestCase('PNG (elephant.png)',           'elephant.png')]
    [TestCase('PNG (kung-fu-panda)',          'kung-fu-panda.png')]
    [TestCase('PNG (emoji1.png)',             'emoji1.png')]
    [TestCase('PNG (emoji2.png)',             'emoji2.png')]
    [TestCase('PNG (world-time-zone.png)',    'world-time-zone.png')]
    [TestCase('PNG Animated (animated.png)',  'animated.png')]
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeWithCopy(const AImageFileName: string);
    [TestCase('BMP (girl.bmp)',               'girl.bmp')]
    {$IFDEF SUPPORT_DNG}
    [TestCase('DNG (park.dng)',               'park.dng')]
    {$ENDIF}
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    [TestCase('ICO (delphi.ico)',             'delphi.ico')]
    [TestCase('JPEG (abnt.jpg)',              'abnt.jpg')]
    [TestCase('JPEG (shrek.jpg)',             'shrek.jpg')]
    [TestCase('PNG (elephant.png)',           'elephant.png')]
    [TestCase('PNG (kung-fu-panda)',          'kung-fu-panda.png')]
    [TestCase('PNG (emoji1.png)',             'emoji1.png')]
    [TestCase('PNG (emoji2.png)',             'emoji2.png')]
    [TestCase('PNG (world-time-zone.png)',    'world-time-zone.png')]
    [TestCase('PNG Animated (animated.png)',  'animated.png')]
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeWithoutCopy(const AImageFileName: string);
  end;

  { TSkImageCodecTests }

  [TestFixture]
  TSkImageCodecTests = class(TSkCodecTestBase)
  private
    procedure TestDecodeAndEncode(const AImageFileName: string; const AQuality: Byte);
  public
    [TestCase('BMP (girl.bmp)',               'girl.bmp')]
    {$IFDEF SUPPORT_DNG}
    [TestCase('DNG (park.dng)',               'park.dng')]
    {$ENDIF}
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    [TestCase('ICO (delphi.ico)',             'delphi.ico')]
    [TestCase('JPEG (abnt.jpg)',              'abnt.jpg')]
    [TestCase('JPEG (shrek.jpg)',             'shrek.jpg')]
    [TestCase('PNG (elephant.png)',           'elephant.png')]
    [TestCase('PNG (kung-fu-panda)',          'kung-fu-panda.png')]
    [TestCase('PNG (emoji1.png)',             'emoji1.png')]
    [TestCase('PNG (emoji2.png)',             'emoji2.png')]
    [TestCase('PNG (world-time-zone.png)',    'world-time-zone.png')]
    [TestCase('PNG Animated (animated.png)',  'animated.png')]
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecode(const AImageFileName: string);
    [TestCase('SkImage Decode/Encode PNG (elephant.png)',        'elephant.png,80')]
    [TestCase('SkImage Decode/Encode PNG (kung-fu-panda.png)',   'kung-fu-panda.png,0')]
    [TestCase('SkImage Decode/Encode PNG (emoji1.png)',          'emoji1.png,100')]
    [TestCase('SkImage Decode/Encode PNG (emoji2.png)',          'emoji2.png,50')]
    [TestCase('SkImage Decode/Encode PNG (world-time-zone.png)', 'world-time-zone.png,100')]
    [TestCase('SkImage Decode/Encode PNG (animated.png)',        'animated.png,30')]
    procedure TestDecodeAndEncodePNG(const AImageFileName: string; const AQuality: Byte);
    [TestCase('SkImage Decode/Encode WebP (kung-fu-panda.webp)',   'kung-fu-panda.webp,100')]
    [TestCase('SkImage Decode/Encode WebP Animated (rocket.webp)', 'rocket.webp,90')]
    procedure TestDecodeAndEncodeWebP(const AImageFileName: string; const AQuality: Byte);
  end;

  { TSkAnimationCodecTests }

  [TestFixture]
  TSkAnimationCodecTests = class(TSkCodecTestBase)
  public
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif)',  'animated.gif,960')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif)', 'developer.gif,5080')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp)',  'rocket.webp,5050')]
    procedure TestDecodeDuration(const AImageFileName: string; const ADuration: Cardinal);
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 0ms)',    'animated.gif,0,/+/n54gAAAD///fny0dOTP//9/fv79/e///39+/v394AAAEAA4ADgAGAAD8eP/+//P/97/nf//8')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 180ms)',  'animated.gif,180,/+/v5+wAAAD////n70dOTP////fv7+/c////9+/v79wCAAEAAwADgAOAAf8ff/5//j/+3/zf//8')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 0ms)',   'developer.gif,0,z8v7fw8PDY////t/T09Pz///+//vT0/P///////////QP+C/4D/Qf9AP4Y/zj/+P4Y////////8')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 450ms)', 'developer.gif,450,//vz8eHz8/////Px4/f/////+/n7///////////////8F/0P/B/8B/AH9w/3j/eP9w////////8')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 0ms)',    'rocket.webp,0,///xg4MP//////Hjw0//////9/PfX/////////////////+P8A/wH+A/4P/A/43///////////8')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 610ms)',  'rocket.webp,610,///hgYOP//////Hhw8//////9/vf3/////////////////+H8A/4D+A/4P/A/83///////////8')]
    procedure TestDecode(const AImageFileName: string; const AFrameAtMilliseconds: Cardinal; const AExpectedImageHash: string);
  end;

  { TCodecAssertHelper }

  TCodecAssertHelper = class helper(TAssertHelper) for Assert
  public
    class procedure HasValidCodecResult(const AImageFileName: string; const AActual: ISkCodec; const AMessage: string = ''); overload;
    class procedure HasValidCodecResult(const AImageFileName: string; const AActual: ISkImage; const AMessage: string = ''); overload;
  end;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

type
  { TImageAssetInfo }

  TImageAssetInfo = record
    Name: string;
    Width: Cardinal;
    Height: Cardinal;
    CRC32: Cardinal;
    SimilarityHash: string;
    MinSimilarity: Double;
    function CanBeLossy: Boolean;
    function Format: TSkEncodedImageFormat;
  end;

const
  FImageAssets: array[0..15] of TImageAssetInfo =
    ((Name: 'girl.bmp';            Width: 244;  Height: 280;  CRC32: 1736216929; SimilarityHash: '48PBjYmDg4H///Hty8fPzf///e/P19/d////////3/0P+P78H04/vi9eJQwA+yGkKlwgBCDgP/4'; MinSimilarity: 0.99),
     (Name: 'park.dng';            Width: 600;  Height: 338;  CRC32: 1955671176; SimilarityHash: '//9/GAAAAAj//395w8fOzP//f3/X987f/////////////7//jAAAAAAAAAAAAAAAAAAAAAAAAAA'; MinSimilarity: 0.99),
     (Name: 'animated.gif';        Width: 1400; Height: 1050; CRC32: 2005275412; SimilarityHash: '/+/n54gAAAD///fny0dOTP//9/fv79/e///39+/v394AAAEAA4ADgAGAAD8eP/+//P/97/nf//8'; MinSimilarity: 0.99),
     (Name: 'developer.gif';       Width: 800;  Height: 600;  CRC32: 2166160918; SimilarityHash: 'z8v7fw8PDY////t/T09Pz///+//vT0/P///////////QP+C/4D/Qf9AP4Y/zj/+P4Y////////8'; MinSimilarity: 0.99),
     (Name: 'delphi.ico';          Width: 256;  Height: 256;  CRC32: 2952345114; SimilarityHash: 'w714eHAYjcP//fj48NjNw////Pz63u/z//////vf//+hPcw/4B/SH8Ef8J/wH709/r//P/fv//8'; MinSimilarity: 0.99),
     (Name: 'abnt.jpg';            Width: 202;  Height: 337;  CRC32: 1659968685; SimilarityHash: '5/8PAAwIf/////9hT09//////2VPb3/////////////+/3h/eP94P3j/eeMBz0niAP8ADPv///8'; MinSimilarity: 0.94),
     (Name: 'shrek.jpg';           Width: 1440; Height: 1500; CRC32: 1173490088; SimilarityHash: '+fm5KzEWBgb//fnr89bOTv/9/e/73v9/////7/////+X48zD7KPOzyGPDhdDEzMYhhr0BnAAAAA'; MinSimilarity: 0.99),
     (Name: 'elephant.png';        Width: 753;  Height: 517;  CRC32: 529596050;  SimilarityHash: '///BgcHhw/P///Hhw+fP////8+fn58/////////////+B9gHwAfAB+AD9AP+B/wH8Af4D/4f/k8'; MinSimilarity: 0.99),
     (Name: 'kung-fu-panda.png';   Width: 640;  Height: 480;  CRC32: 3960679947; SimilarityHash: '//8vDCRuKor//2/s5+7qzv////737+/v////////7+/ADsCCaAbiGOQQaEhpHGBQ4Fron8Dl0Qc'; MinSimilarity: 0.99),
     (Name: 'emoji1.png';          Width: 160;  Height: 160;  CRC32: 875861993;  SimilarityHash: '74EBvcfDw////fH9x8fP////+//31//////////////wC+ADwAPCI8BjwAvAB+AH4A/wH/////8'; MinSimilarity: 0.99),
     (Name: 'emoji2.png';          Width: 80;   Height: 80;   CRC32: 3865023965; SimilarityHash: '/+OBgYGBw/////Hhw8fP/////+vf19/////////f///pB+EDwQPAA8ADwAPgA+AH8A/8P/////8'; MinSimilarity: 0.99),
     (Name: 'world-time-zone.png'; Width: 1280; Height: 722;  CRC32: 2305552899; SimilarityHash: 'n5GBgYHV9////fHhw9f////9+fnX1//////5+dfX//+AAYABgAGAAYABkAGQAYABgAGABYAB//8'; MinSimilarity: 0.99),
     (Name: 'animated.png';        Width: 1400; Height: 1050; CRC32: 502650454;  SimilarityHash: '/+/n54AAAAD///fnw0dOTP//9/fn79/e///39+fv394BAAEAA4ADgAGAAD8eP/6//P/97/nf//8'; MinSimilarity: 0.99),
     (Name: 'mandrill.wbmp';       Width: 512;  Height: 512;  CRC32: 2160350774; SimilarityHash: 'AAA8PL2Zm+N/fHx9/9/f7/98////39///3z////f3/8H8A/wD/AP8dfjx6vDh8fj18fij+EP5r8'; MinSimilarity: 0.99),
     (Name: 'kung-fu-panda.webp';  Width: 640;  Height: 480;  CRC32: 78640289;   SimilarityHash: '//8vDCRuKor//2/s5+7qzv////737+/v////////7+/ADsCCSAbkCGQQaEhhHGhQ4Fron8Dl0Qc'; MinSimilarity: 0.99),
     (Name: 'rocket.webp';         Width: 250;  Height: 250;  CRC32: 2031700395; SimilarityHash: '///hg4MP//////Hjw0//////9/vfX/////////////////+P8A/wH+A/4P/A/43///////////8'; MinSimilarity: 0.99));

function GetImageAssetInfo(const AImageFileName: string): TImageAssetInfo;
var
  I: Integer;
begin
  for I := 0 to Length(FImageAssets) - 1 do
    if FImageAssets[I].Name = AImageFileName then
      Exit(FImageAssets[I]);
  Assert.FailFmt('Cannot possible to find the info of the image "%s"', [AImageFileName]);
end;

{ TSkCodecTestBase }

function TSkCodecTestBase.AssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Codec');
end;

{ TSkCodecTests }

procedure TSkCodecTests.TestDecodeFile(const AImageFileName: string);
var
  LCodec: ISkCodec;
begin
  LCodec := TSkCodec.MakeFromFile(AssetsPath + AImageFileName);
end;

procedure TSkCodecTests.TestDecodeStream(const AImageFileName: string);
var
  LCodec: ISkCodec;
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(AssetsPath + AImageFileName);
    LCodec := TSkCodec.MakeFromStream(LStream);
    Assert.HasValidCodecResult(AImageFileName, LCodec);
  finally
    LStream.Free;
  end;
end;

procedure TSkCodecTests.TestDecodeWithCopy(const AImageFileName: string);
var
  LCodec: ISkCodec;
  LEncodedBytes: TBytes;
begin
  LEncodedBytes := TFile.ReadAllBytes(AssetsPath + AImageFileName);
  LCodec := TSkCodec.MakeWithCopy(LEncodedBytes, Length(LEncodedBytes));
  if LEncodedBytes <> nil then
    LEncodedBytes[0] := LEncodedBytes[0] xor $FF;
  Assert.HasValidCodecResult(AImageFileName, LCodec);
end;

procedure TSkCodecTests.TestDecodeWithoutCopy(const AImageFileName: string);
var
  LCodec: ISkCodec;
  LEncodedBytes: TBytes;
begin
  LEncodedBytes := TFile.ReadAllBytes(AssetsPath + AImageFileName);
  LCodec := TSkCodec.MakeWithoutCopy(LEncodedBytes, Length(LEncodedBytes));
  Assert.HasValidCodecResult(AImageFileName, LCodec);
end;

{ TSkImageCodecTests }

procedure TSkImageCodecTests.TestDecode(const AImageFileName: string);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + AImageFileName);
  Assert.HasValidCodecResult(AImageFileName, LImage);
end;

procedure TSkImageCodecTests.TestDecodeAndEncode(const AImageFileName: string;
  const AQuality: Byte);
var
  LImage: ISkImage;
  LImageInfo: TImageAssetInfo;
  LNewImage: ISkImage;
begin
  LImageInfo := GetImageAssetInfo(AImageFileName);
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + AImageFileName);
  LNewImage := TSkImage.MakeFromEncoded(LImage.Encode(LImageInfo.Format, AQuality));
  if LImageInfo.CanBeLossy then
    Assert.AreSimilar(LImageInfo.SimilarityHash, LNewImage, LImageInfo.MinSimilarity)
  else
    Assert.AreEqualCRC32(LImageInfo.CRC32, LNewImage);
end;

procedure TSkImageCodecTests.TestDecodeAndEncodePNG(
  const AImageFileName: string; const AQuality: Byte);
begin
  TestDecodeAndEncode(AImageFileName, AQuality);
end;

procedure TSkImageCodecTests.TestDecodeAndEncodeWebP(
  const AImageFileName: string; const AQuality: Byte);
begin
  TestDecodeAndEncode(AImageFileName, AQuality);
end;

{ TSkAnimationCodecTests }

procedure TSkAnimationCodecTests.TestDecode(const AImageFileName: string;
  const AFrameAtMilliseconds: Cardinal; const AExpectedImageHash: string);
var
  LCodec: ISkAnimationCodecPlayer;
begin
  LCodec := TSkAnimationCodecPlayer.MakeFromFile(AssetsPath + AImageFileName);
  LCodec.Seek(AFrameAtMilliseconds);
  Assert.AreSimilar(AExpectedImageHash, LCodec.Frame);
end;

procedure TSkAnimationCodecTests.TestDecodeDuration(
  const AImageFileName: string; const ADuration: Cardinal);
var
  LCodec: ISkAnimationCodecPlayer;
begin
  LCodec := TSkAnimationCodecPlayer.MakeFromFile(AssetsPath + AImageFileName);
  Assert.AreEqual(ADuration, LCodec.Duration);
end;

{ TImageAssetInfo }

function TImageAssetInfo.CanBeLossy: Boolean;
begin
  Result := not (Format in [TSkEncodedImageFormat.BMP, TSkEncodedImageFormat.PNG, TSkEncodedImageFormat.WBMP]);
end;

function TImageAssetInfo.Format: TSkEncodedImageFormat;
var
  LExtension: string;
begin
  LExtension := TPath.GetExtension(Name).ToLower;
  if LExtension = '.bmp' then
    Result := TSkEncodedImageFormat.BMP
  else if LExtension = '.gif' then
    Result := TSkEncodedImageFormat.GIF
  else if LExtension = '.ico' then
    Result := TSkEncodedImageFormat.ICO
  else if LExtension = '.wbmp' then
    Result := TSkEncodedImageFormat.WBMP
  else if LExtension = '.dng' then
    Result := TSkEncodedImageFormat.DNG
  else
    Result := ExtensionToEncodedImageFormat(LExtension);
end;

{ TCodecAssertHelper }

class procedure TCodecAssertHelper.HasValidCodecResult(const AImageFileName: string;
  const AActual: ISkImage; const AMessage: string);
var
  LImageInfo: TImageAssetInfo;
  {$IFDEF ANDROID}
  LMaxActualSide: Integer;
  LMaxOriginalSide: Integer;
  {$ENDIF}
  LSimilarityReduction: Double;
begin
  Assert.IsNotNull(AActual, AMessage);
  LImageInfo := GetImageAssetInfo(AImageFileName);
  {$IFDEF ANDROID}
  LMaxActualSide := Max(AActual.Width, AActual.Height);
  LMaxOriginalSide := Max(LImageInfo.Width, LImageInfo.Height);
  if LMaxOriginalSide > LMaxActualSide then
    LSimilarityReduction := Min(((LMaxOriginalSide - LMaxActualSide) / LMaxOriginalSide) / 5, 0.05)
  else
  {$ENDIF}
    LSimilarityReduction := 0;
  if LImageInfo.CanBeLossy or not SameValue(LSimilarityReduction, 0, TEpsilon.Position) then
    Assert.AreSimilar(LImageInfo.SimilarityHash, AActual, LImageInfo.MinSimilarity - LSimilarityReduction, AMessage)
  else
    Assert.AreEqualCRC32(LImageInfo.CRC32, AActual, AMessage);
end;

class procedure TCodecAssertHelper.HasValidCodecResult(const AImageFileName: string;
  const AActual: ISkCodec; const AMessage: string);
var
  LImageInfo: TImageAssetInfo;
  {$IFDEF ANDROID}
  LMaxActualSide: Integer;
  LMaxOriginalSide: Integer;
  {$ENDIF}
  LSimilarityReduction: Double;
begin
  Assert.IsNotNull(AActual, AMessage);
  LImageInfo := GetImageAssetInfo(AImageFileName);
  {$IFDEF ANDROID}
  LMaxActualSide := Max(AActual.Width, AActual.Height);
  LMaxOriginalSide := Max(LImageInfo.Width, LImageInfo.Height);
  if LMaxOriginalSide > LMaxActualSide then
    LSimilarityReduction := Min(((LMaxOriginalSide - LMaxActualSide) / LMaxOriginalSide) / 5, 0.05)
  else
  {$ENDIF}
    LSimilarityReduction := 0;
  if LImageInfo.CanBeLossy or not SameValue(LSimilarityReduction, 0, TEpsilon.Position) then
    Assert.AreSimilar(LImageInfo.SimilarityHash, AActual, LImageInfo.MinSimilarity - LSimilarityReduction, AMessage)
  else
    Assert.AreEqualCRC32(LImageInfo.CRC32, AActual, AMessage);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkCodecTests);
  TDUnitX.RegisterTestFixture(TSkImageCodecTests);
  TDUnitX.RegisterTestFixture(TSkAnimationCodecTests);
end.
