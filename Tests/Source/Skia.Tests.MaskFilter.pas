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
unit Skia.Tests.MaskFilter;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkMaskFilterTests }

  [TestFixture]
  TSkMaskFilterTests = class(TTestBase)
  public
    [TestCase('', '140,140,20,0.99,/4GBgYGBgf///fHhwcHB///98eHh4eH//////////////////D/8P/w//D////////////////8')]
    procedure TestMaskFilterBlur(const ABitmapWidth, ABitmapHeight: Integer; const ASigma, AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestTable;
    [Test]
    procedure TestTableClip;
    [Test]
    procedure TestTableGamma;
  end;

implementation

uses
  { Delphi }
  System.Types;

{ TSkMaskFilterTests }

function DrawBlurredCircle(const AMaskFilter: ISkMaskFilter): TBytes;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  SetLength(Result, 40 * 40 * 4);
  LSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(40, 40), Result, 40 * 4);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Black;
  LPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 4);
  LSurface.Canvas.DrawCircle(PointF(20, 20), 12, LPaint);
  if Assigned(AMaskFilter) then
  begin
    // A second pass, so the table filter has a soft coverage to work on.
    LPaint.MaskFilter := AMaskFilter;
    LSurface.Canvas.DrawCircle(PointF(20, 20), 12, LPaint);
  end;
end;

procedure TSkMaskFilterTests.TestTable;
var
  I: Integer;
  LTable: TSkTableFilter;
begin
  for I := 0 to High(LTable) do
    LTable[I] := 255 - I;
  Assert.IsFalse(CompareMem(@DrawBlurredCircle(nil)[0], @DrawBlurredCircle(TSkMaskFilter.MakeTable(LTable))[0], 40 * 40 * 4),
    'An inverting coverage table should change the drawing');
end;

procedure TSkMaskFilterTests.TestTableClip;
begin
  Assert.IsFalse(CompareMem(@DrawBlurredCircle(nil)[0], @DrawBlurredCircle(TSkMaskFilter.MakeTableClip(64, 192))[0], 40 * 40 * 4),
    'Clipping the coverage should change the drawing');
end;

procedure TSkMaskFilterTests.TestTableGamma;
begin
  Assert.IsFalse(CompareMem(@DrawBlurredCircle(nil)[0], @DrawBlurredCircle(TSkMaskFilter.MakeTableGamma(3))[0], 40 * 40 * 4),
    'A gamma coverage table should change the drawing');
end;

procedure TSkMaskFilterTests.TestMaskFilterBlur(const ABitmapWidth,
  ABitmapHeight: Integer; const ASigma, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(ABitmapWidth, ABitmapHeight);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurSTyle.Normal, ASigma, False);
  LSurface.Canvas.DrawRect(RectF(0, 0, ABitmapWidth, ABitmapHeight), LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkMaskFilterTests);
end.
