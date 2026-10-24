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
    [TestCase('BMP',  'girl.bmp,BMP')]
    [TestCase('GIF',  'animated.gif,GIF')]
    [TestCase('ICO',  'delphi.ico,ICO')]
    [TestCase('JPEG', 'shrek.jpg,JPEG')]
    [TestCase('PNG',  'elephant.png,PNG')]
    [TestCase('WBMP', 'mandrill.wbmp,WBMP')]
    [TestCase('WebP', 'kung-fu-panda.webp,WEBP')]
    procedure TestEncodedImageFormat(const AImageFileName: string; const AExpectedFormat: TSkEncodedImageFormat);
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
    [TestCase('GIF Animated (animated.gif)',  'animated.gif')]
    [TestCase('GIF Animated (developer.gif)', 'developer.gif')]
    procedure TestDecodeAgainWhenStreamCannotRewind(const AImageFileName: string);
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
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 0ms)',    'animated.gif,0,C3COAf48OLGAESXon3sLnU+wvBJTxL+FCd7z/sDgKsieww7dh9H884iHz3kF6jcggPxBAP+A6g')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (animated.gif - frame 180ms)',  'animated.gif,180,EZC+AQBIiCHAESLwvzsNoU8Q/RJYsB+GCNbXDkHfHbjeww/XbwE9+om3XzkH4Vuwwftd+D5A6Q')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 0ms)',   'developer.gif,0,nWPNghHqD2C/BunPkL3nZxcxhP0BqK9C/yKIHzvZVpiAvv3tR1BACwuYHjYVNMRPQP4bWJCC/w')]
    [TestCase('SkAnimationCodecPlayer GIF Animated (developer.gif - frame 450ms)', 'developer.gif,450,lqO9BBHcv19+CQmgoL/vb6vgAgX3e/+D9/jfDnvXR9wwfwDpSxABDgKcXjUQLnC///0VNMBC/Q')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 0ms)',    'rocket.webp,0,z9vRNg3Jhw+/56ivr4fiFkjfffv0g9F+4wCEAUQYF3g+P+zcn1DBBCt834Xt4BOP+AUmyM07DQ')]
    [TestCase('SkAnimationCodecPlayer WebP Animated (rocket.webp - frame 610ms)',  'rocket.webp,610,3w9ytg7Mhw/+5buHDwfhElS/fvj6u+E+5AxM8UMYFqhe//Dda7ABAzeMT0Xu2/t+eAcoxI37Cw')]
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

  { TForwardOnlyStream }

  // Descends from TStream because the newer TMemoryStream overrides more
  // overloads than the ones that exist in old RTLs.
  TForwardOnlyStream = class(TStream)
  strict private
    FData: TMemoryStream;
    FForwardOnly: Boolean;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function Read(var ABuffer; ACount: Longint): Longint; override;
    function Seek(const AOffset: Int64; AOrigin: TSeekOrigin): Int64; override;
    function Write(const ABuffer; ACount: Longint): Longint; override;
    property ForwardOnly: Boolean read FForwardOnly write FForwardOnly;
  end;

const
  FImageAssets: array[0..15] of TImageAssetInfo =
    ((Name: 'girl.bmp';            Width: 244;  Height: 280;  CRC32: 1736216929; SimilarityHash: 'z+uthfwJbJGAERyYkL0HgqfgPwsr4F7FDfQHD33qNvSPvhLwO9E/CNKb4HkM/09AgvFCpOB+7w'; MinSimilarity: 0.99),
     (Name: 'park.dng';            Width: 600;  Height: 338;  CRC32: 1955671176; SimilarityHash: 'QdAdRfk4dDB9ExdsPzz+b+dgvwc31I+E/vWbzrzkEtxvAAHzB1F7/NV7/jkG8G9/AgU7vK5A+Q'; MinSimilarity: 0.99),
     (Name: 'animated.gif';        Width: 1400; Height: 1050; CRC32: 2005275412; SimilarityHash: 'C3COAf48OLGAESXon3sLnU+wvBJTxL+FCd7z/sDgKsieww7dh9H884iHz3kF6jcggPxBAP+A6g'; MinSimilarity: 0.99),
     (Name: 'developer.gif';       Width: 800;  Height: 600;  CRC32: 2166160918; SimilarityHash: 'nWPNghHqD2C/BunPkL3nZxcxhP0BqK9C/yKIHzvZVpiAvv3tR1BACwuYHjYVNMRPQP4bWJCC/w'; MinSimilarity: 0.99),
     (Name: 'delphi.ico';          Width: 256;  Height: 256;  CRC32: 2952345114; SimilarityHash: 'IYCAAABAJNGB8Faw8AD0vnOAQeYSfKDC29QjAAMJ8W9/vfucT4EA9bJDwDwVjNP+PhiZIM6/7Q'; MinSimilarity: 0.99),
     (Name: 'abnt.jpg';            Width: 202;  Height: 337;  CRC32: 1659968685; SimilarityHash: 'ta9tAwH9p4+/D8+/737pY9/wgf/u65/C/hTcf/ncKpTgPgIAuPD8CuJjDjYLL4wvQAFApL+C/A'; MinSimilarity: 0.94),
     (Name: 'shrek.jpg';           Width: 1440; Height: 1500; CRC32: 1173490088; SimilarityHash: 'R2yewPYuOMF9HBcwPz3/q6/f/v4jdH9G9OGzrr7iAqzfwAT6ezG69dCXD3kJAGjw/wc2hB9A8g'; MinSimilarity: 0.99),
     (Name: 'elephant.png';        Width: 753;  Height: 517;  CRC32: 529596050;  SimilarityHash: 'mnNRuQ7Af9984rXHHwnc3O//v/rW36E+6jfU4ToosMtevubDKz7/9RJI7jkY06teORlLRC39Bw'; MinSimilarity: 0.99),
     (Name: 'kung-fu-panda.png';   Width: 640;  Height: 480;  CRC32: 3960679947; SimilarityHash: 'A8zdAvokWNA+GNGnj778jMvwvQk0gP9E/OhDLzvVI0DQQAcVpJC5DNAjb7gHDdQ/AgMqMI4B6w'; MinSimilarity: 0.99),
     (Name: 'emoji1.png';          Width: 160;  Height: 160;  CRC32: 875861993;  SimilarityHash: 'rxehPRjPs++B5u9vUIPm3QswQvImiOF94B8cwMEfFPSf/fDsFw+BBeqjP34PpUtfvRZ7OM379w'; MinSimilarity: 0.99),
     (Name: 'emoji2.png';          Width: 80;   Height: 80;   CRC32: 3865023965; SimilarityHash: 'rUeB+xnn99975zdwsMPru1vwf/Xai1EB6C4oYT4ZG2RfPe3jW48C/CIwH8AKvOtN+BBoOI27AA'; MinSimilarity: 0.99),
     (Name: 'world-time-zone.png'; Width: 1280; Height: 722;  CRC32: 2305552899; SimilarityHash: 'DvAuwwr+f2F//SvEjz0Fo3/AQ/ZdSFBBAeQfv7/6OUT/gAPMSwCE/KoDgDwM48f/QPwyWFD+5g'; MinSimilarity: 0.99),
     (Name: 'animated.png';        Width: 1400; Height: 1050; CRC32: 502650454;  SimilarityHash: 'C3COAf48NLGAESTon3sLnU+wvBJTxL+FCd7zDsHgKsiegw7dh8E89IiHz3kF6TcwgPxBAP+A6g'; MinSimilarity: 0.99),
     (Name: 'mandrill.wbmp';       Width: 512;  Height: 512;  CRC32: 2160350774; SimilarityHash: 'CfitAgYj/FD+CRYoYPr5fPNwQQZZ7K8CCSuojoHcOzCgwhXdI3HEBK+jQHgQFYCQxPdE3ACA6g'; MinSimilarity: 0.99),
     (Name: 'kung-fu-panda.webp';  Width: 640;  Height: 480;  CRC32: 78640289;   SimilarityHash: 'A8zdAvokWNA+GNGnj778jMvwvQk1gP9E/OhDLzvVI0DQQAcVpJC5DNAjb7gHDdQ/AgMqMI4B6w'; MinSimilarity: 0.99),
     (Name: 'rocket.webp';         Width: 250;  Height: 250;  CRC32: 2031700395; SimilarityHash: 'z9vRNg3Jhw+/56izv4fiFkjfffvzf9F+4wCEEUQYGHg+P+zbn1DBBCt834Xt4BOP+AUmyM07DQ'; MinSimilarity: 0.99));

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

procedure TSkCodecTests.TestEncodedImageFormat(const AImageFileName: string;
  const AExpectedFormat: TSkEncodedImageFormat);
var
  LCodec: ISkCodec;
begin
  LCodec := TSkCodec.MakeFromFile(AssetsPath + AImageFileName);
  Assert.IsNotNull(LCodec, 'Invalid ISkCodec (nil)');
  Assert.IsTrue(LCodec.EncodedImageFormat = AExpectedFormat, 'The codec should report the format it decoded');
end;

procedure TSkCodecTests.TestDecodeAgainWhenStreamCannotRewind(
  const AImageFileName: string);
var
  LCodec: ISkCodec;
  LStream: TForwardOnlyStream;
begin
  LStream := TForwardOnlyStream.Create(AssetsPath + AImageFileName);
  try
    LCodec := TSkCodec.MakeFromStream(LStream);
    Assert.IsNotNull(LCodec, 'Invalid ISkCodec (nil)');
    Assert.IsNotNull(LCodec.GetImage(SkNative32ColorType), 'The first decode should succeed');
    LStream.ForwardOnly := True;
    Assert.IsNull(LCodec.GetImage(SkNative32ColorType), 'The codec should fail when the stream cannot rewind, instead of decoding from the wrong position');
    LCodec := nil;
  finally
    LStream.Free;
  end;
end;

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

{ TForwardOnlyStream }

constructor TForwardOnlyStream.Create(const AFileName: string);
begin
  inherited Create;
  FData := TMemoryStream.Create;
  FData.LoadFromFile(AFileName);
end;

destructor TForwardOnlyStream.Destroy;
begin
  FData.Free;
  inherited;
end;

function TForwardOnlyStream.GetSize: Int64;
begin
  Result := FData.Size;
end;

function TForwardOnlyStream.Read(var ABuffer; ACount: Longint): Longint;
begin
  Result := FData.Read(ABuffer, ACount);
end;

function TForwardOnlyStream.Seek(const AOffset: Int64;
  AOrigin: TSeekOrigin): Int64;
begin
  if FForwardOnly and (AOrigin = TSeekOrigin.soBeginning) and (AOffset < FData.Position) then
    Exit(-1);
  Result := FData.Seek(AOffset, AOrigin);
end;

function TForwardOnlyStream.Write(const ABuffer; ACount: Longint): Longint;
begin
  Result := FData.Write(ABuffer, ACount);
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
