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
unit Skia.Tests.Vcl.Codec;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  Vcl.Graphics,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkVclCodecTests }

  [TestFixture]
  TSkVclCodecTests = class(TTestBase)
  private
    function SkGraphicClass: TGraphicClass;
    procedure TestDecodeAndEncode(const AImageFileName, AEncodeFileExtension: string);
  protected
    function AssetsPath: string; override;
  public
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeFile(const AImageFileName: string);
    [TestCase('SkImage Decode/Encode PNG (elephant.png)',        'elephant.png')]
    [TestCase('SkImage Decode/Encode PNG (kung-fu-panda.png)',   'kung-fu-panda.png')]
    [TestCase('SkImage Decode/Encode PNG (emoji1.png)',          'emoji1.png')]
    [TestCase('SkImage Decode/Encode PNG (emoji2.png)',          'emoji2.png')]
    [TestCase('SkImage Decode/Encode PNG (world-time-zone.png)', 'world-time-zone.png')]
    [TestCase('SkImage Decode/Encode PNG (animated.png)',        'animated.png')]
    procedure TestDecodeAndEncodePNG(const AImageFileName: string);
    [TestCase('SkImage Decode/Encode WebP (kung-fu-panda.webp)',   'kung-fu-panda.webp')]
    [TestCase('SkImage Decode/Encode WebP Animated (rocket.webp)', 'rocket.webp')]
    procedure TestDecodeAndEncodeWebP(const AImageFileName: string);
    [TestCase('SkImage Decode/Encode WebP (kung-fu-panda.webp)',   'kung-fu-panda.webp')]
    [TestCase('SkImage Decode/Encode WebP Animated (rocket.webp)', 'rocket.webp')]
    procedure TestDecodeWebPAndEncodePNG(const AImageFileName: string);
    [Test]
    procedure TestGraphicSetSize;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.IOUtils,

  { Tests }
  Skia.Tests.Codec;

{ TSkVclCodecTests }

function TSkVclCodecTests.AssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Codec');
end;

procedure TSkVclCodecTests.TestDecodeAndEncode(const AImageFileName, AEncodeFileExtension: string);
var
  LNewImage: ISkImage;
  LPicture: TPicture;
  LTempFile: string;
begin
  LTempFile := TPath.GetNewTempFileName(AEncodeFileExtension);
  LPicture := TPicture.Create;
  try
    LPicture.LoadFromFile(AssetsPath + AImageFileName);
    LPicture.SaveToFile(LTempFile);
    LNewImage := TSkImage.MakeFromEncodedFile(LTempFile);
    Assert.HasValidCodecResult(AImageFileName, LNewImage);
  finally
    LPicture.Free;
  end;
end;

procedure TSkVclCodecTests.TestDecodeAndEncodePNG(const AImageFileName: string);
begin
  TestDecodeAndEncode(AImageFileName, TPath.GetExtension(AImageFileName));
end;

procedure TSkVclCodecTests.TestDecodeAndEncodeWebP(const AImageFileName: string);
begin
  TestDecodeAndEncode(AImageFileName, TPath.GetExtension(AImageFileName));
end;

procedure TSkVclCodecTests.TestDecodeFile(const AImageFileName: string);
var
  LActualImage: ISkImage;
  LPicture: TPicture;
begin
  LPicture := TPicture.Create;
  try
    LPicture.LoadFromFile(AssetsPath + AImageFileName);
    Assert.IsTrue(Supports(LPicture.Graphic, ISkImage, LActualImage));
    Assert.HasValidCodecResult(AImageFileName, LActualImage);
  finally
    LPicture.Free;
  end;
end;

procedure TSkVclCodecTests.TestDecodeWebPAndEncodePNG(const AImageFileName: string);
begin
  TestDecodeAndEncode(AImageFileName, '.png');
end;

// TSkGraphic is declared in the implementation section of Vcl.Skia, so its
// class is taken from the graphic TPicture creates for a WebP file
function TSkVclCodecTests.SkGraphicClass: TGraphicClass;
var
  LPicture: TPicture;
begin
  LPicture := TPicture.Create;
  try
    LPicture.LoadFromFile(AssetsPath + 'kung-fu-panda.webp');
    Assert.AreEqual('TSkGraphic', LPicture.Graphic.ClassName, False);
    Result := TGraphicClass(LPicture.Graphic.ClassType);
  finally
    LPicture.Free;
  end;
end;

procedure TSkVclCodecTests.TestGraphicSetSize;

  procedure Check(const AImageFileName: string);
  var
    LGraphic: TGraphic;
    LHeight: Integer;
    LOriginal: ISkImage;
    LSaved: ISkImage;
    LScaled: ISkImage;
    LStream: TMemoryStream;
    LWidth: Integer;
  begin
    LGraphic := SkGraphicClass.Create;
    try
      LGraphic.LoadFromFile(AssetsPath + AImageFileName);
      Assert.IsTrue(Supports(LGraphic, ISkImage, LOriginal), AImageFileName);
      LWidth := LGraphic.Width div 2;
      LHeight := LGraphic.Height div 2;
      LGraphic.SetSize(LWidth, LHeight);
      Assert.AreEqual(LWidth, LGraphic.Width, AImageFileName + ': width');
      Assert.AreEqual(LHeight, LGraphic.Height, AImageFileName + ': height');
      Assert.IsTrue(Supports(LGraphic, ISkImage, LScaled), AImageFileName);
      Assert.AreSimilar(LOriginal, LScaled, 0.97, AImageFileName + ': the content should be scaled');
      // SaveToStream must write the resized image, not the originally loaded data
      LStream := TMemoryStream.Create;
      try
        LGraphic.SaveToStream(LStream);
        LStream.Position := 0;
        LSaved := TSkImage.MakeFromEncodedStream(LStream);
      finally
        LStream.Free;
      end;
      Assert.IsNotNull(LSaved, AImageFileName + ': the saved data should decode');
      Assert.AreEqual(LWidth, LSaved.Width, AImageFileName + ': saved width');
      Assert.AreEqual(LHeight, LSaved.Height, AImageFileName + ': saved height');
    finally
      LGraphic.Free;
    end;
  end;

begin
  Check('kung-fu-panda.webp');
  Check('rocket.webp');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkVclCodecTests);
end.
