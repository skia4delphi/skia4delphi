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
unit Skia.Tests.ImageHash;

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
  { TImageHashingTests }

  [TestFixture]
  TImageHashingTests = class(TTestBase)
  strict private
    function MakeHalves(const ALeft, ARight: TAlphaColor; const AWidth, AHeight: Integer): ISkImage;
    function MakePattern(const AWidth, AHeight: Integer): ISkImage;
    function MakeSolid(const AColor: TAlphaColor; const AWidth: Integer = 32; const AHeight: Integer = 32): ISkImage;
  public
    [TestCase('37x21',   '37,21,8uNAffgEgM+7+uK/b8P5EyywvfDfmyDD6/5jYL4K0Fu/e/gcmFD38gDM/gL2G7zP/vnKtz8ADQ')]
    [TestCase('1x1',     '1,1,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('300x200', '300,200,1jMw/vzia2/AArfHH4H1D5APP/jbH6CA7u9DUL0D58fv/fYHJKB7/A9Y/0D58H+//QEJ6B6/Aw')]
    procedure TestStableHash(const AWidth, AHeight: Integer; const AExpectedHash: string);
    [TestCase('1x1',   '1,1')]
    [TestCase('1x19',  '1,19')]
    [TestCase('19x1',  '19,1')]
    [TestCase('17x9',  '17,9')]
    [TestCase('2000x3', '2000,3')]
    procedure TestImageSizes(const AWidth, AHeight: Integer);
    [Test]
    procedure TestAbsoluteColor;
    [Test]
    procedure TestSmallColorChanges;
    [Test]
    procedure TestAlpha;
    [Test]
    procedure TestHiddenTransparentColor;
    [Test]
    procedure TestScaledImage;
    [Test]
    procedure TestSpatialLayout;
    [Test]
    procedure TestLocalDefect;
    [TestCase('0.25', '0.25')]
    [TestCase('0.5',  '0.5')]
    procedure TestSubpixelOffset(const AOffset: Single);
    [Test]
    procedure TestSymmetry;
    [Test]
    procedure TestEmptyHash;
    [Test]
    procedure TestInvalidHash;
  end;

implementation

uses
  { Tests }
  Skia.Tests.Foundation.ImageHash;

{ TImageHashingTests }

function TImageHashingTests.MakeHalves(const ALeft, ARight: TAlphaColor;
  const AWidth, AHeight: Integer): ISkImage;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  LSurface.Canvas.Clear(ALeft);
  LPaint := TSkPaint.Create;
  LPaint.Color := ARight;
  LPaint.Blender := TSkBlender.MakeMode(TSkBlendMode.Src);
  LSurface.Canvas.DrawRect(TRectF.Create(AWidth div 2, 0, AWidth, AHeight), LPaint);
  Result := LSurface.MakeImageSnapshot;
end;

// Premultiplied pixels are passed as is, so the hash does not depend on any
// conversion and must be identical on every platform.
function TImageHashingTests.MakePattern(const AWidth, AHeight: Integer): ISkImage;
var
  I: Integer;
  LAlpha: Integer;
  LPixels: TBytes;
  X: Integer;
  Y: Integer;
begin
  SetLength(LPixels, AWidth * AHeight * 4);
  for Y := 0 to AHeight - 1 do
    for X := 0 to AWidth - 1 do
    begin
      I := (Y * AWidth + X) * 4;
      LAlpha := (X * 7 + Y * 13) mod 256;
      LPixels[I] := (X * 31 + Y * 3) mod (LAlpha + 1);
      LPixels[I + 1] := (X * 5 + Y * 17) mod (LAlpha + 1);
      LPixels[I + 2] := (X * 11 + Y * 29) mod (LAlpha + 1);
      LPixels[I + 3] := LAlpha;
    end;
  Result := TSkImage.MakeRasterCopy(TSkImageInfo.Create(AWidth, AHeight, TSkColorType.RGBA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), @LPixels[0], AWidth * 4);
end;

function TImageHashingTests.MakeSolid(const AColor: TAlphaColor;
  const AWidth, AHeight: Integer): ISkImage;
begin
  Result := MakeHalves(AColor, AColor, AWidth, AHeight);
end;

procedure TImageHashingTests.TestAbsoluteColor;
begin
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FFFF0000), MakeSolid($FF0000FF)) < 0.5);
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FF404040), MakeSolid($FFC0C0C0)) < 0.6);
end;

procedure TImageHashingTests.TestAlpha;
begin
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FF000000), MakeSolid($00000000)) < 0.5);
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($80000000), MakeSolid($81000000)) >= 0.99);
end;

procedure TImageHashingTests.TestEmptyHash;
var
  LHash: string;
begin
  LHash := TImageHashing.Hash(MakeSolid(TAlphaColors.White));
  Assert.AreEqual('', TImageHashing.Hash(nil));
  Assert.AreEqual(1.0, TImageHashing.Similarity('', ''), 0);
  Assert.AreEqual(0.0, TImageHashing.Similarity('', LHash), 0);
  Assert.AreEqual(0.0, TImageHashing.Similarity(LHash, ''), 0);
end;

procedure TImageHashingTests.TestHiddenTransparentColor;
var
  LPixels: array[0..3] of Byte;
  LBlueHash: string;
begin
  LPixels[0] := 0;
  LPixels[1] := 0;
  LPixels[2] := 255;
  LPixels[3] := 0;
  LBlueHash := TImageHashing.Hash(TSkImage.MakeRasterCopy(TSkImageInfo.Create(1, 1, TSkColorType.RGBA8888,
    TSkAlphaType.Unpremul, TSkColorSpace.MakeSRGB), @LPixels[0], 4));
  Assert.AreEqual(TImageHashing.Hash(MakeSolid(TAlphaColors.Null, 1, 1)), LBlueHash);
end;

procedure TImageHashingTests.TestImageSizes(const AWidth, AHeight: Integer);
var
  LHash: string;
begin
  LHash := TImageHashing.Hash(MakePattern(AWidth, AHeight));
  Assert.AreEqual(74, Length(LHash));
  Assert.AreEqual(1.0, TImageHashing.Similarity(LHash, LHash), 0);
  Assert.AreEqual(1.0, TImageHashing.Similarity(MakeSolid($FF4080C0), MakeSolid($FF4080C0, AWidth, AHeight)), 0);
end;

procedure TImageHashingTests.TestInvalidHash;
const
  LegacyHash = '/wMDAwOzt/////Pjw/f/////8/fX//////////////+GD4YPts+2z7bP///fP98//7////////8';
var
  LHash: string;
begin
  LHash := TImageHashing.Hash(MakeSolid(TAlphaColors.White));
  Assert.WillRaise(
    procedure
    begin
      TImageHashing.Similarity(LHash, LegacyHash);
    end, EArgumentException);
  Assert.WillRaise(
    procedure
    begin
      TImageHashing.Similarity(LHash + 'AAAA', LHash);
    end, EArgumentException);
end;

procedure TImageHashingTests.TestLocalDefect;
var
  LDefect: ISkSurface;
  LPaint: ISkPaint;
begin
  LDefect := TSkSurface.MakeRaster(256, 256);
  LDefect.Canvas.Clear(TAlphaColors.White);
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Black;
  LDefect.Canvas.DrawRect(TRectF.Create(0, 0, 16, 16), LPaint);
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid(TAlphaColors.White, 256, 256), LDefect.MakeImageSnapshot) < 0.97);
end;

procedure TImageHashingTests.TestScaledImage;
begin
  Assert.AreEqual(1.0, TImageHashing.Similarity(MakeHalves($FFFF0000, $FF0000FF, 32, 16),
    MakeHalves($FFFF0000, $FF0000FF, 64, 32)), 0);
end;

procedure TImageHashingTests.TestSmallColorChanges;
begin
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FF808080), MakeSolid($FF818181)) >= 0.99);
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FF808080), MakeSolid($FF7F7F7F)) >= 0.99);
  Assert.IsTrue(TImageHashing.Similarity(MakeSolid($FFFF0000), MakeSolid($FFFF0001)) >= 0.99);
end;

procedure TImageHashingTests.TestSpatialLayout;
begin
  Assert.IsTrue(TImageHashing.Similarity(MakeHalves($FFFF0000, $FF0000FF, 64, 64),
    MakeHalves($FF0000FF, $FFFF0000, 64, 64)) < 0.5);
end;

procedure TImageHashingTests.TestStableHash(const AWidth, AHeight: Integer;
  const AExpectedHash: string);
begin
  Assert.AreEqual(AExpectedHash, TImageHashing.Hash(MakePattern(AWidth, AHeight)));
end;

procedure TImageHashingTests.TestSubpixelOffset(const AOffset: Single);

  function Draw(const AOffset: Single): ISkImage;
  var
    LPaint: ISkPaint;
    LSurface: ISkSurface;
  begin
    LSurface := TSkSurface.MakeRaster(256, 256);
    LSurface.Canvas.Clear(TAlphaColors.White);
    LPaint := TSkPaint.Create;
    LPaint.Color := TAlphaColors.Black;
    LSurface.Canvas.DrawCircle(PointF(128 + AOffset, 128 + AOffset), 60, LPaint);
    Result := LSurface.MakeImageSnapshot;
  end;

begin
  Assert.IsTrue(TImageHashing.Similarity(Draw(0), Draw(AOffset)) >= 0.99);
end;

procedure TImageHashingTests.TestSymmetry;
var
  LImage1: ISkImage;
  LImage2: ISkImage;
begin
  LImage1 := MakePattern(40, 30);
  LImage2 := MakeHalves($FF102030, $FFA0B0C0, 40, 30);
  Assert.AreEqual(TImageHashing.Similarity(LImage1, LImage2), TImageHashing.Similarity(LImage2, LImage1), 0);
end;

initialization
  TDUnitX.RegisterTestFixture(TImageHashingTests);
end.
