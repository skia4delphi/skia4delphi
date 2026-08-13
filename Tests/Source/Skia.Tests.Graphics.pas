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
unit Skia.Tests.Graphics;

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
  { TSkGraphicsTests }

  [TestFixture]
  TSkGraphicsTests = class(TTestBase)
  public
    [Test]
    procedure TestDumpMemoryStatistics;
    [Test]
    procedure TestFontCache;
    [Test]
    procedure TestInitAndAllowJIT;
    [Test]
    procedure TestVersion;
    [Test]
    procedure TestPurgeAllCaches;
    [Test]
    procedure TestResourceCacheLimits;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Math,
  System.Types,
  System.UITypes;

type
  { TTestTraceMemoryDump }

  TTestTraceMemoryDump = class(TSkTraceMemoryDumpBaseClass)
  strict private
    FNumericValues: Integer;
    FStringValues: Integer;
  strict protected
    procedure DumpNumericValue(const ADumpName, AValueName, AUnits: string; const AValue: UInt64); override;
    procedure DumpStringValue(const ADumpName, AValueName, AValue: string); override;
  public
    property NumericValues: Integer read FNumericValues;
    property StringValues: Integer read FStringValues;
  end;

{ TTestTraceMemoryDump }

procedure TTestTraceMemoryDump.DumpNumericValue(const ADumpName, AValueName,
  AUnits: string; const AValue: UInt64);
begin
  Inc(FNumericValues);
end;

procedure TTestTraceMemoryDump.DumpStringValue(const ADumpName, AValueName,
  AValue: string);
begin
  Inc(FStringValues);
end;

{ TSkGraphicsTests }

procedure TSkGraphicsTests.TestDumpMemoryStatistics;
var
  LFont: ISkFont;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
  LTraceMemoryDump: TTestTraceMemoryDump;
begin
  LFont := TSkFont.Create(TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf'), 30);
  LSurface := TSkSurface.MakeRaster(64, 64);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LSurface.Canvas.DrawSimpleText('Skia', 0, 40, LFont, LPaint);

  LTraceMemoryDump := TTestTraceMemoryDump.Create(True, True);
  try
    TSkGraphics.DumpMemoryStatistics(LTraceMemoryDump);
    Assert.IsTrue(LTraceMemoryDump.NumericValues > 0, 'The dump should report at least one numeric value');
  finally
    LTraceMemoryDump.Free;
  end;
end;

procedure TSkGraphicsTests.TestFontCache;
var
  LFont: ISkFont;
  LOldCountLimit: Integer;
  LOldLimit: NativeUInt;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
  LUsed: NativeUInt;
begin
  LOldLimit := TSkGraphics.FontCacheLimit;
  LOldCountLimit := TSkGraphics.FontCacheCountLimit;
  try
    TSkGraphics.FontCacheLimit := 1024 * 1024;
    Assert.AreEqual<NativeUInt>(1024 * 1024, TSkGraphics.FontCacheLimit, '(FontCacheLimit)');
    TSkGraphics.FontCacheCountLimit := 256;
    Assert.AreEqual(256, TSkGraphics.FontCacheCountLimit, '(FontCacheCountLimit)');

    LFont := TSkFont.Create(TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf'), 30);
    LSurface := TSkSurface.MakeRaster(64, 64);
    LSurface.Canvas.Clear(TAlphaColors.Null);
    LPaint := TSkPaint.Create;
    LSurface.Canvas.DrawSimpleText('Skia', 0, 40, LFont, LPaint);
    LUsed := TSkGraphics.FontCacheUsed;
    Assert.IsTrue(LUsed > 0, 'Drawing text should populate the font cache');
    Assert.IsTrue(TSkGraphics.FontCacheCountUsed > 0, 'Drawing text should populate the font cache count');

    TSkGraphics.PurgeFontCache;
    Assert.IsTrue(TSkGraphics.FontCacheUsed < LUsed, 'PurgeFontCache should release the cached strikes');
  finally
    TSkGraphics.FontCacheLimit := LOldLimit;
    TSkGraphics.FontCacheCountLimit := LOldCountLimit;
  end;
end;

procedure TSkGraphicsTests.TestVersion;
begin
  Assert.IsTrue(TSkVersion.Major > 0, 'The major version should be set');
  Assert.IsTrue(TSkVersion.Minor >= 0, 'The minor version should be set');
  Assert.IsTrue(TSkVersion.Build >= 0, 'The build version should be set');
  Assert.AreEqual(Format('%d.%d.%d', [TSkVersion.Major, TSkVersion.Minor, TSkVersion.Build]), TSkVersion.ToString,
    'ToString should join the three numbers');
end;

procedure TSkGraphicsTests.TestInitAndAllowJIT;
begin
  TSkGraphics.Init;
  TSkGraphics.AllowJIT;
  Assert.IsTrue(TSkGraphics.FontCacheLimit > 0, 'The font cache limit should have a default value');
end;

procedure TSkGraphicsTests.TestPurgeAllCaches;
begin
  TSkGraphics.PurgeResourceCache;
  TSkGraphics.PurgeAllCaches;
  Assert.AreEqual<NativeUInt>(0, TSkGraphics.ResourceCacheTotalBytesUsed, 'PurgeAllCaches should empty the resource cache');
end;

procedure TSkGraphicsTests.TestResourceCacheLimits;
var
  LOldSingleAllocationLimit: NativeUInt;
  LOldTotalLimit: NativeUInt;
begin
  LOldTotalLimit := TSkGraphics.ResourceCacheTotalByteLimit;
  LOldSingleAllocationLimit := TSkGraphics.ResourceCacheSingleAllocationByteLimit;
  try
    TSkGraphics.ResourceCacheTotalByteLimit := 8 * 1024 * 1024;
    Assert.AreEqual<NativeUInt>(8 * 1024 * 1024, TSkGraphics.ResourceCacheTotalByteLimit, '(ResourceCacheTotalByteLimit)');
    TSkGraphics.ResourceCacheSingleAllocationByteLimit := 512 * 1024;
    Assert.AreEqual<NativeUInt>(512 * 1024, TSkGraphics.ResourceCacheSingleAllocationByteLimit, '(ResourceCacheSingleAllocationByteLimit)');
    Assert.IsTrue(TSkGraphics.ResourceCacheTotalBytesUsed <= TSkGraphics.ResourceCacheTotalByteLimit, 'The used bytes should not exceed the limit');
  finally
    TSkGraphics.ResourceCacheTotalByteLimit := LOldTotalLimit;
    TSkGraphics.ResourceCacheSingleAllocationByteLimit := LOldSingleAllocationLimit;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkGraphicsTests);
end.
