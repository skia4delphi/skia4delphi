{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
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
    procedure TestMakeRaster;
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
