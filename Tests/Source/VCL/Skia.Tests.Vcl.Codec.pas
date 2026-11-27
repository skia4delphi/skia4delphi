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
    procedure TestDecodeAndEncode(const AImageFileName, AEncodeFileExtension: string);
  protected
    function AssetsPath: string; override;
  public
    [TestCase('WBMP (mandrill.wbmp)',         'mandrill.wbmp')]
    [TestCase('WebP (kung-fu-panda.webp)',    'kung-fu-panda.webp')]
    [TestCase('WebP Animated (rocket.webp)',  'rocket.webp')]
    procedure TestDecodeFile(const AImageFileName: string);
    [Test]
    procedure TestGraphicEquals;
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
  end;

implementation

uses
  { Delphi }
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

procedure TSkVclCodecTests.TestGraphicEquals;
var
  LBlue: TGraphic;
  LEmpty: TGraphic;
  LEmpty2: TGraphic;
  LGraphicClass: TGraphicClass;
  LPanda: TGraphic;
  LRed: TGraphic;
  LRed2: TGraphic;

  // A new TSkGraphic (declared in the implementation section of Vcl.Skia, so
  // its class is taken from the graphic TPicture creates for a WebP file)
  function NewGraphic: TGraphic;
  var
    LPicture: TPicture;
  begin
    if LGraphicClass = nil then
    begin
      LPicture := TPicture.Create;
      try
        LPicture.LoadFromFile(AssetsPath + 'kung-fu-panda.webp');
        Assert.AreEqual('TSkGraphic', LPicture.Graphic.ClassName, False);
        LGraphicClass := TGraphicClass(LPicture.Graphic.ClassType);
      finally
        LPicture.Free;
      end;
    end;
    Result := LGraphicClass.Create;
  end;

  // An 8 x 8 PNG filled with one colour, loaded into a new TSkGraphic
  function SolidGraphic(const AColor: TAlphaColor): TGraphic;
  var
    LFileName: string;
    LSurface: ISkSurface;
  begin
    LSurface := TSkSurface.MakeRaster(8, 8);
    LSurface.Canvas.Clear(AColor);
    LFileName := TPath.GetNewTempFileName('.png');
    try
      Assert.IsTrue(LSurface.MakeImageSnapshot.EncodeToFile(LFileName));
      Result := NewGraphic;
      try
        Result.LoadFromFile(LFileName);
      except
        Result.Free;
        raise;
      end;
    finally
      TFile.Delete(LFileName);
    end;
  end;

begin
  LGraphicClass := nil;
  LEmpty := nil;
  LEmpty2 := nil;
  LPanda := nil;
  LRed := nil;
  LRed2 := nil;
  LBlue := nil;
  try
    LEmpty := NewGraphic;
    LEmpty2 := NewGraphic;
    LPanda := NewGraphic;
    LPanda.LoadFromFile(AssetsPath + 'kung-fu-panda.webp');
    LRed := SolidGraphic(TAlphaColors.Red);
    LRed2 := SolidGraphic(TAlphaColors.Red);
    LBlue := SolidGraphic(TAlphaColors.Blue);
    Assert.IsTrue(LEmpty.Equals(LEmpty2), 'Two empty graphics');
    Assert.IsFalse(LEmpty.Equals(LPanda), 'An empty and a loaded graphic');
    Assert.IsFalse(LPanda.Equals(LEmpty), 'A loaded and an empty graphic');
    Assert.IsTrue(LRed.Equals(LRed), 'A graphic compared with itself');
    Assert.IsTrue(LRed.Equals(LRed2), 'Two loads of the same image');
    Assert.IsFalse(LRed.Equals(LBlue), 'Two different images of the same size');
    Assert.IsFalse(LRed.Equals(LPanda), 'Two images of different sizes');
  finally
    LBlue.Free;
    LRed2.Free;
    LRed.Free;
    LPanda.Free;
    LEmpty2.Free;
    LEmpty.Free;
  end;
end;

procedure TSkVclCodecTests.TestDecodeWebPAndEncodePNG(const AImageFileName: string);
begin
  TestDecodeAndEncode(AImageFileName, '.png');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkVclCodecTests);
end.
