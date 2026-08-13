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
    // TODO: Investigate possible issue.
    // [Test]
    // procedure TestCliperator;
    [Test]
    procedure TestSpanerator;
    [TestCase('1', '0.99,/wMDAwOzt/////Pjw/f/////8/fX//////////////+GD4YPts+2z7bP///fP98//7////////8')]
    procedure TestGetBoundaryPath(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestIntersects;
    [Test]
    procedure TestIsComplex;
    [Test]
    procedure TestIsEqual;
    [Test]
    procedure TestIsEmpty;
    [Test]
    procedure TestIsRect;
    [Test]
    procedure TestOp;
    [Test]
    procedure TestQuickContains;
    [Test]
    procedure TestSetPath;
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

// TODO: Investigate possible issue.
//
// ISkRegionCliperator and ISkRegionIterator both skip the first rectangle and
// answer the last one twice: their move_next entry point advances the Skia
// iterator before reporting whether it is done, while the Delphi for..in
// protocol expects the first MoveNext to only open the sequence. The number of
// rectangles is right, the rectangles are not. ISkRegionSpanerator, which is
// built on a real next(out) entry point, walks correctly.
//
// procedure TSkRegionTests.TestCliperator;
// var
//   LRect: TRect;
//   LRects: string;
//   LRegion: ISkRegion;
// begin
//   LRegion := TSkRegion.Create(Rect(0, 0, 20, 20));
//   LRegion.Op(Rect(30, 0, 50, 20), TSkRegionOp.Union);
//
//   LRects := '';
//   for LRect in LRegion.GetCliperator(Rect(0, 0, 100, 100)) do
//     LRects := LRects + Format('%d,%d,%d,%d ', [LRect.Left, LRect.Top, LRect.Right, LRect.Bottom]);
//   Assert.AreEqual('0,0,20,20 30,0,50,20 ', LRects, 'The cliperator should walk both rectangles');
//
//   LRects := '';
//   for LRect in LRegion.GetIterator do
//     LRects := LRects + Format('%d,%d,%d,%d ', [LRect.Left, LRect.Top, LRect.Right, LRect.Bottom]);
//   Assert.AreEqual('0,0,20,20 30,0,50,20 ', LRects, 'The iterator should walk both rectangles');
// end;

procedure TSkRegionTests.TestSpanerator;
var
  LPoint: TPoint;
  LRegion: ISkRegion;
  LSpans: string;
begin
  LRegion := TSkRegion.Create(Rect(0, 0, 20, 20));
  LRegion.Op(Rect(30, 0, 50, 20), TSkRegionOp.Union);

  LSpans := '';
  for LPoint in LRegion.GetSpanerator(10, 0, 100) do
    LSpans := LSpans + Format('%d..%d ', [LPoint.X, LPoint.Y]);
  Assert.AreEqual('0..20 30..50 ', LSpans, 'The spanerator should walk the spans of the scanline');

  LSpans := '';
  for LPoint in LRegion.GetSpanerator(30, 0, 100) do
    LSpans := LSpans + Format('%d..%d ', [LPoint.X, LPoint.Y]);
  Assert.AreEqual('', LSpans, 'A scanline outside of the region should have no span');
end;

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

procedure TSkRegionTests.TestIntersects;
var
  LOther: ISkRegion;
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create(Rect(0, 0, 100, 100));
  Assert.IsTrue(LRegion.Intersects(Rect(50, 50, 150, 150)), 'An overlapping rect should intersect');
  Assert.IsFalse(LRegion.Intersects(Rect(200, 200, 300, 300)), 'A distant rect should not intersect');

  LOther := TSkRegion.Create(Rect(90, 90, 200, 200));
  Assert.IsTrue(LRegion.Intersects(LOther), 'An overlapping region should intersect');
  LOther := TSkRegion.Create(Rect(200, 200, 300, 300));
  Assert.IsFalse(LRegion.Intersects(LOther), 'A distant region should not intersect');
end;

procedure TSkRegionTests.TestOp;
var
  LOther: ISkRegion;
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create(Rect(0, 0, 100, 100));
  Assert.IsTrue(LRegion.Op(Rect(50, 0, 200, 100), TSkRegionOp.Intersect), 'Intersecting with an overlapping rect should succeed');
  Assert.AreEqual<TRect>(Rect(50, 0, 100, 100), LRegion.Bounds, '(intersect)');

  LRegion := TSkRegion.Create(Rect(0, 0, 100, 100));
  LOther := TSkRegion.Create(Rect(100, 0, 200, 100));
  Assert.IsTrue(LRegion.Op(LOther, TSkRegionOp.Union), 'The union should succeed');
  Assert.AreEqual<TRect>(Rect(0, 0, 200, 100), LRegion.Bounds, '(union)');

  LRegion := TSkRegion.Create(Rect(0, 0, 100, 100));
  Assert.IsFalse(LRegion.Op(Rect(0, 0, 100, 100), TSkRegionOp.Difference), 'A difference that empties the region should return false');
  Assert.IsTrue(LRegion.IsEmpty, 'The region should be empty');
end;

procedure TSkRegionTests.TestSetPath;
var
  LClip: ISkRegion;
  LPathBuilder: ISkPathBuilder;
  LRegion: ISkRegion;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddOval(RectF(0, 0, 100, 100));

  LClip := TSkRegion.Create(Rect(0, 0, 100, 100));
  LRegion := TSkRegion.Create;
  Assert.IsTrue(LRegion.SetPath(LPathBuilder.Detach, LClip),
    'Setting a path clipped by a covering region should succeed');
  Assert.AreEqual<TRect>(Rect(0, 0, 100, 100), LRegion.Bounds, 'The region should cover the oval bounds');
  Assert.IsTrue(LRegion.IsComplex, 'An oval region is complex');
  Assert.IsTrue(LRegion.Contains(50, 50), 'The center of the oval should be inside');
  Assert.IsFalse(LRegion.Contains(1, 1), 'The corner of the bounds should be outside the oval');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  LClip := TSkRegion.Create(Rect(500, 500, 600, 600));
  LRegion := TSkRegion.Create;
  Assert.IsFalse(LRegion.SetPath(LPathBuilder.Detach, LClip),
    'A path outside the clip should produce an empty region');
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
