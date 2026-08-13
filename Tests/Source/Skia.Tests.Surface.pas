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
unit Skia.Tests.Surface;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSurfaceTests }

  [TestFixture]
  TSkSurfaceTests = class(TTestBase)
  public
    [Test]
    procedure TestClear;
    [Test]
    procedure TestFlush;
    [Test]
    procedure TestDraw;
    [Test]
    procedure TestMakeImageSnapshotWithBounds;
    [Test]
    procedure TestMakeRaster;
    [Test]
    procedure TestMakeRasterDirect;
    [Test]
    procedure TestProperties;
    // TODO: Investigate possible issue.
    // [Test]
    // procedure TestPropertiesArePreserved;
    [Test]
    procedure TestReadPixels;
    [Test]
    procedure TestWritePixels;
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

{ TSkSurfaceTests }

procedure TSkSurfaceTests.TestFlush;
var
  LBuffer: TBytes;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  // On a raster surface flushing is a no-op, but it must leave the pixels alone.
  SetLength(LBuffer, 10 * 10 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(10, 10), LBuffer, 10 * 4);
  LSurface := TSkSurface.MakeRasterDirect(LPixmap);
  LSurface.Canvas.Clear(TAlphaColors.Red);
  LSurface.Flush;
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 0, 'Flush should keep the drawing');
  LSurface.FlushAndSubmit;
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 0, 'FlushAndSubmit should keep the drawing');
  LSurface.FlushAndSubmit(True);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 0, 'A synchronous FlushAndSubmit should keep the drawing');
end;

procedure TSkSurfaceTests.TestClear;
var
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(4, 5, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB));
  Assert.IsNotNull(LSurface);
  Assert.AreEqualCRC32(3214345816, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Null);
  Assert.AreEqualCRC32(3214345816, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Red);
  Assert.AreEqualCRC32(1879293126, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Mediumaquamarine);
  Assert.AreEqualCRC32(114815628, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Firebrick);
  Assert.AreEqualCRC32(1124165031, LSurface.PeekPixels);


  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(4, 5, TSkColorType.RGBA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB));
  Assert.IsNotNull(LSurface);
  Assert.AreEqualCRC32(270374506, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Null);
  Assert.AreEqualCRC32(270374506, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Red);
  Assert.AreEqualCRC32(210187423, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Mediumaquamarine);
  Assert.AreEqualCRC32(2607141960, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Firebrick);
  Assert.AreEqualCRC32(3574496588, LSurface.PeekPixels);
end;

procedure TSkSurfaceTests.TestDraw;
var
  LDest: ISkSurface;
  LSource: ISkSurface;
begin
  LSource := TSkSurface.MakeRaster(20, 20);
  LSource.Canvas.Clear(TAlphaColors.Red);

  LDest := TSkSurface.MakeRaster(100, 100);
  LDest.Canvas.Clear(TAlphaColors.Null);
  LSource.Draw(LDest.Canvas, 40, 40);
  Assert.AreSameColor(TAlphaColors.Red, LDest.PeekPixels.Colors[50, 50], 0, 'The source should be drawn at the given offset');
  Assert.AreSameColor(TAlphaColors.Null, LDest.PeekPixels.Colors[10, 10], 0, 'The rest should stay empty');
end;

procedure TSkSurfaceTests.TestMakeImageSnapshotWithBounds;
var
  LImage: ISkImage;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(100, 100);
  LSurface.Canvas.Clear(TAlphaColors.Blue);
  LImage := LSurface.MakeImageSnapshot(Rect(10, 20, 60, 90));
  Assert.IsNotNull(LImage, 'Invalid ISkImage (nil)');
  Assert.AreEqual(50, LImage.Width, '(Width)');
  Assert.AreEqual(70, LImage.Height, '(Height)');

  Assert.IsNull(LSurface.MakeImageSnapshot(Rect(200, 200, 300, 300)), 'A snapshot outside the surface should fail');
end;

procedure TSkSurfaceTests.TestMakeRasterDirect;
var
  LBuffer: TBytes;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  SetLength(LBuffer, 10 * 10 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(10, 10, TSkColorType.BGRA8888, TSkAlphaType.Premul,
    TSkColorSpace.MakeSRGB), LBuffer, 10 * 4);
  LSurface := TSkSurface.MakeRasterDirect(LPixmap);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Red);
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[5, 5], 0, 'The drawing should reach the supplied buffer');
  Assert.AreEqual($FF, Integer(LBuffer[3]), 'The buffer should hold the opaque alpha of the first pixel');
end;

procedure TSkSurfaceTests.TestProperties;
var
  LProperties: TSkSurfaceProperties;
  LSurface: ISkSurface;
begin
  LProperties := TSkSurfaceProperties.Create([], TSkPixelGeometry.RGBHorizontal);
  LSurface := TSkSurface.MakeRaster(10, 10, LProperties);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  Assert.IsTrue(LSurface.Properties = LSurface.Canvas.GetBaseProperties, 'The surface and its canvas should report the same properties');
  Assert.IsTrue(LSurface.Properties = LSurface.Canvas.GetTopProperties, 'Without a layer the top properties should match the surface ones');
end;

// TODO: Investigate possible issue.
//
// A surface never reports the properties it was created with: both the wrapper
// and a direct sk4d_surface_make_raster + sk4d_surface_get_props round trip
// answer Unknown/[] for every request, with opaque and premultiplied images
// alike, so TSkSurfaceProperties is effectively write only.
//
// procedure TSkSurfaceTests.TestPropertiesArePreserved;
// var
//   LProperties: TSkSurfaceProperties;
//   LSurface: ISkSurface;
// begin
//   LProperties := TSkSurfaceProperties.Create([TSkSurfacePropertiesFlag.UseDeviceIndependentFonts], TSkPixelGeometry.RGBHorizontal);
//   LSurface := TSkSurface.MakeRaster(10, 10, LProperties);
//   Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
//   Assert.IsTrue(LSurface.Properties = LProperties, 'The surface should keep the given properties');
// end;

procedure TSkSurfaceTests.TestReadPixels;
var
  LBuffer: TBytes;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(20, 20);
  LSurface.Canvas.Clear(TAlphaColors.Red);

  SetLength(LBuffer, 10 * 10 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(10, 10, TSkColorType.BGRA8888, TSkAlphaType.Premul,
    TSkColorSpace.MakeSRGB), LBuffer, 10 * 4);
  Assert.IsTrue(LSurface.ReadPixels(LPixmap, 5, 5), 'ReadPixels should succeed');
  Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 0], 0, 'The read pixels should hold the surface content');
end;

procedure TSkSurfaceTests.TestWritePixels;
var
  LBuffer: TBytes;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  SetLength(LBuffer, 10 * 10 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(10, 10, TSkColorType.BGRA8888, TSkAlphaType.Premul,
    TSkColorSpace.MakeSRGB), LBuffer, 10 * 4);
  Assert.IsTrue(LPixmap.Erase(TAlphaColors.Lime));

  LSurface := TSkSurface.MakeRaster(50, 50);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSurface.WritePixels(LPixmap, 20, 20);
  Assert.AreSameColor(TAlphaColors.Lime, LSurface.PeekPixels.Colors[25, 25], 0, 'The pixels should be written at the given offset');
  Assert.AreSameColor(TAlphaColors.Null, LSurface.PeekPixels.Colors[5, 5], 0, 'The rest of the surface should stay empty');
end;

procedure TSkSurfaceTests.TestMakeRaster;
var
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(5, 5, SkNative32ColorType, TSkAlphaType.Premul));
  Assert.IsNotNull(LSurface);

  LSurface := TSkSurface.MakeRaster(5, 5);
  Assert.IsNotNull(LSurface);

  LSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(4, 5, TSkColorType.RGBA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB));
  Assert.IsNotNull(LSurface);
  Assert.AreEqualCRC32(270374506, LSurface.PeekPixels);

  LSurface.Canvas.Clear(TAlphaColors.Null);
  Assert.AreEqualCRC32(270374506, LSurface.PeekPixels);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSurfaceTests);
end.
