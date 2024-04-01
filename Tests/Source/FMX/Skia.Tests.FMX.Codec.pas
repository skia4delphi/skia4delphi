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
unit Skia.Tests.FMX.Codec;

interface

{$SCOPEDENUMS ON}

{$IFNDEF MSWINDOWS}
  {$DEFINE SUPPORT_DNG}
{$ENDIF}
{$IF CompilerVersion >= 33}
  {$DEFINE SUPPORT_WBMP}
{$ENDIF}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  FMX.Graphics,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkFMXCodecTests }

  [TestFixture]
  TSkFMXCodecTests = class(TTestBase)
  private
    procedure TestDecodeAndEncode(const AImageFileName: string; const AQuality: Byte);
  protected
    function AssetsPath: string; override;
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
    {$IFDEF SUPPORT_WBMP}
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    {$ENDIF}
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeFile(const AImageFileName: string);
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

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  {$ENDIF}

  { Tests }
  Skia.Tests.Codec;

{ TSkFMXCodecTests }

function TSkFMXCodecTests.AssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Codec');
end;

procedure TSkFMXCodecTests.TestDecodeAndEncode(const AImageFileName: string;
  const AQuality: Byte);
var
  LBitmap: TBitmap;
  LNewImage: ISkImage;
  LSaveParams: TBitmapCodecSaveParams;
  LTempFile: string;
begin
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  try
  {$ENDIF}
    LSaveParams.Quality := AQuality;
    LTempFile := TPath.GetNewTempFileName(TPath.GetExtension(AImageFileName));
    LBitmap := TBitmap.Create;
    try
      LBitmap.LoadFromFile(AssetsPath + AImageFileName);
      LBitmap.SaveToFile(LTempFile, @LSaveParams);
      LNewImage := TSkImage.MakeFromEncodedFile(LTempFile);
      Assert.HasValidCodecResult(AImageFileName, LNewImage);
    finally
      LBitmap.Free;
    end;
  {$IFDEF MSWINDOWS}
  finally
    CoUnInitialize();
  end;
  {$ENDIF}
end;

procedure TSkFMXCodecTests.TestDecodeAndEncodePNG(const AImageFileName: string;
  const AQuality: Byte);
begin
  TestDecodeAndEncode(AImageFileName, AQuality);
end;

procedure TSkFMXCodecTests.TestDecodeAndEncodeWebP(const AImageFileName: string;
  const AQuality: Byte);
begin
  TestDecodeAndEncode(AImageFileName, AQuality);
end;

procedure TSkFMXCodecTests.TestDecodeFile(const AImageFileName: string);
var
  LBitmap: TBitmap;
begin
  {$IFDEF MSWINDOWS}
  CoInitialize(nil);
  try
  {$ENDIF}
    LBitmap := TBitmap.Create;
    try
      LBitmap.LoadFromFile(AssetsPath + AImageFileName);
      Assert.HasValidCodecResult(AImageFileName, LBitmap.ToSkImage);
    finally
      LBitmap.Free;
    end;
  {$IFDEF MSWINDOWS}
  finally
    CoUnInitialize();
  end;
  {$ENDIF}
end;

initialization
  TDUnitX.RegisterTestFixture(TSkFMXCodecTests);
end.
