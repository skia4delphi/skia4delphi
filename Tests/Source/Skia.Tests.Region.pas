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
unit Skia.Tests.Region;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkRegionTests }

  [TestFixture]
  TSkRegionTests = class(TTestBase)
  public
    [Test]
    procedure TestBasic;
    [TestCase('1', '0.99,/wMDAwOzt/////Pjw/f/////8/fX//////////////+GD4YPts+2z7bP///fP98//7////////8')]
    procedure TestGetBoundaryPath(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestIsComplex;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestIsEmpty;
    [Test]
    procedure TestIsRect;
    [Test]
    procedure TestQuickContains;
    [Test]
    procedure TestQuickReject;
    [TestCase('1', '0.99,JycnJyc3t7f/9+fn5/f////39/f39/////////////++/55/nn+eP54/3z/fP98/3z////////8')]
    procedure TestSetRects(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,n5+fn4/Pz8//////z8/Pz//////v39/P///////////3/+v/6//n/+f/+//5//n/+f////////8')]
    procedure TestTranslate(const AMinSimilarity: Double; const AExpectedImageHash: string);
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

{ TSkRegionTests }

procedure TSkRegionTests.TestBasic;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create(Rect(1, 2, 3, 4));
  LRegion.Op(Rect(2, 3, 4, 5), TSkRegionOp.Union);
  Assert.IsTrue(LRegion.Bounds = Rect(1, 2, 4, 5));
end;

procedure TSkRegionTests.TestGetBoundaryPath(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LPath: ISkPath;
  LPaint: ISkPaint;
  LRegion: ISkRegion;
begin
  LSurface := TSkSurface.MakeRaster(256, 100);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LPaint := TSkPaint.Create;
  LRegion := TSkRegion.Create;
  LRegion.SetRect(Rect(10, 20, 90, 60));
  LRegion.Op(Rect(30, 40, 60, 80), TSkRegionOp.Xor);
  LSurface.Canvas.DrawRegion(LRegion, LPaint);

  LPath := LRegion.GetBoundaryPath;
  LSurface.Canvas.Translate(100, 0);
  LSurface.Canvas.DrawPath(LPath, LPaint);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkRegionTests.TestIsComplex;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create;
  Assert.IsFalse(LRegion.IsComplex);
  LRegion.SetRect(Rect(1, 2, 3, 4));
  Assert.IsFalse(LRegion.IsComplex);
  LRegion.Op(Rect(2, 3, 4, 5), TSkRegionOp.Union);
  Assert.IsTrue(LRegion.IsComplex);
end;

procedure TSkRegionTests.TestIsEmpty;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create;
  Assert.IsTrue(LRegion.IsEmpty);
  LRegion.SetRect(Rect(1, 2, 3, 4));
  Assert.IsFalse(LRegion.IsEmpty);
  LRegion.SetEmpty;
  Assert.IsTrue(LRegion.IsEmpty);
  LRegion.SetRect(Rect(1, 2, 3, 4));
  LRegion.Op(Rect(2, 3, 4, 5), TSkRegionOp.Union);
  Assert.IsFalse(LRegion.IsEmpty);
end;

procedure TSkRegionTests.TestIsEqual;
var
  LRegion1: ISkRegion;
  LRegion2: ISkRegion;
begin
  LRegion1 := TSkRegion.Create;
  LRegion2 := TSkRegion.Create;
  Assert.IsTrue(LRegion1.IsEqual(LRegion2));
  LRegion1.SetRect(Rect(1, 2, 3, 4));
  Assert.IsFalse(LRegion1.IsEqual(LRegion2));
  LRegion1.SetEmpty;
  Assert.IsTrue(LRegion1.IsEqual(LRegion2));
end;

procedure TSkRegionTests.TestIsRect;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create;
  Assert.IsFalse(LRegion.IsRect);
  LRegion.SetRect(Rect(1, 2, 3, 4));
  Assert.IsTrue(LRegion.IsRect);
  LRegion.SetEmpty;
  Assert.IsFalse(LRegion.IsRect);
  LRegion.SetRect(Rect(1, 2, 3, 4));
  LRegion.Op(Rect(2, 3, 4, 5), TSkRegionOp.Union);
  Assert.IsFalse(LRegion.IsRect);
end;

procedure TSkRegionTests.TestQuickContains;
const
  TestRect: TRect = (Left: 2; Top: 2; Right: 3; Bottom: 3);
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create(Rect(1, 2, 3, 4));
  Assert.IsTrue(LRegion.QuickContains(TestRect));
  LRegion.Op(Rect(1, 4, 3, 6), TSkRegionOp.Union);
  Assert.IsTrue(LRegion.QuickContains(TestRect));
  LRegion.Op(Rect(1, 7, 3, 8), TSkRegionOp.Union);
  Assert.IsFalse(LRegion.QuickContains(TestRect));
end;

procedure TSkRegionTests.TestQuickReject;
const
  TestRect: TRect = (Left: 4; Top: 2; Right: 5; Bottom: 3);
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create(Rect(1, 2, 3, 4));
  Assert.IsTrue(LRegion.QuickReject(TestRect));
  LRegion.Op(Rect(1, 4, 3, 6), TSkRegionOp.Union);
  Assert.IsTrue(LRegion.QuickReject(TestRect));
  LRegion.Op(Rect(4, 7, 5, 8), TSkRegionOp.Union);
  Assert.IsFalse(LRegion.QuickReject(TestRect));
end;

procedure TSkRegionTests.TestSetRects(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LRects: TArray<TRect>;
  LPaint: ISkPaint;
  LRegion: ISkRegion;
  LRect: TRect;
begin
  LSurface := TSkSurface.MakeRaster(256, 70);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LPaint := TSkPaint.Create;
  LRects := [Rect(10, 10, 40, 40), Rect(20, 20, 50, 50), Rect(30, 30, 60, 60)];
  LRegion := TSkRegion.Create;
  LRegion.SetRects(LRects);
  LSurface.Canvas.DrawRegion(LRegion, LPaint);

  LRegion.SetEmpty;
  for LRect in LRects do
    LRegion.Op(LRect, TSkRegionOp.Union);
  LRegion.Translate(100, 0);
  LSurface.Canvas.DrawRegion(LRegion, LPaint);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkRegionTests.TestTranslate(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
  LRegion: ISkRegion;
  LColor: TAlphaColor;
begin
  LSurface := TSkSurface.MakeRaster(256, 90);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LPaint := TSkPaint.Create;
  LRegion := TSkRegion.Create;
  LRegion.SetRects([Rect(40, 20, 50, 30), Rect(70, 40, 80, 50), Rect(60, 10, 70, 20)]);

  for LColor in TArray<TAlphaColor>.Create(TAlphaColors.Red, TAlphaColors.Blue, TAlphaColors.Green, TAlphaColors.Magenta) do
  begin
    LPaint.Color := LColor;
    LSurface.Canvas.DrawRegion(LRegion, LPaint);
    LRegion.Translate(10, 10);
  end;

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkRegionTests);
end.
