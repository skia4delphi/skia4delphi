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
unit Skia.Tests.PathBuilder;

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
  { TSkPathBuilderTests }

  [TestFixture]
  TSkPathBuilderTests = class(TTestBase)
  public
    [Test]
    procedure TestAddArc;
    [Test]
    procedure TestAddCircle;
    [Test]
    procedure TestAddPath;
    [Test]
    procedure TestArcToOval;
    [Test]
    procedure TestArcToRadius;
    [Test]
    procedure TestArcToSvg;
    [Test]
    procedure TestBounds;
    [Test]
    procedure TestConicTo;
    [Test]
    procedure TestConstructors;
    [Test]
    procedure TestDetachResetsTheBuilder;
    [Test]
    procedure TestFillType;
    [Test]
    procedure TestIncReserve;
    [Test]
    procedure TestOffset;
    [Test]
    procedure TestPolylineTo;
    [Test]
    procedure TestRelativeCommandsMatchAbsoluteOnes;
    [Test]
    procedure TestSnapshotKeepsTheBuilder;
    [Test]
    procedure TestToggleInverseFillType;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types;

{ TSkPathBuilderTests }

procedure TSkPathBuilderTests.TestAddArc;
var
  LPathBuilder: ISkPathBuilder;
  LPath: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddArc(RectF(0, 0, 100, 100), 0, 90);
  LPath := LPathBuilder.Detach;
  Assert.IsFalse(LPath.IsEmpty, 'The arc should not be empty');
  Assert.AreSameRect(RectF(50, 50, 100, 100), LPath.Bounds, 0.5, 'A 0..90 degrees arc covers the lower right quadrant');
end;

procedure TSkPathBuilderTests.TestAddCircle;
var
  LPathBuilder: ISkPathBuilder;
  LPathCCW: ISkPath;
  LPathCW: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddCircle(PointF(50, 60), 20);
  LPathCW := LPathBuilder.Detach;
  Assert.AreSameRect(RectF(30, 40, 70, 80), LPathCW.Bounds, TEpsilon.Position);
  Assert.IsTrue(LPathCW.IsOval, 'A circle is an oval');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddCircle(50, 60, 20, TSkPathDirection.CCW);
  LPathCCW := LPathBuilder.Detach;
  Assert.AreSameRect(LPathCW.Bounds, LPathCCW.Bounds, TEpsilon.Position, 'Both overloads should produce the same bounds');
  Assert.AreNotEqual(PathToText(LPathCW), PathToText(LPathCCW), 'The direction should change the point order');
end;

procedure TSkPathBuilderTests.TestAddPath;
var
  LPathBuilder: ISkPathBuilder;
  LOther: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 50, 50));
  LOther := LPathBuilder.Detach;

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(100, 100, 150, 150));
  LPathBuilder.AddPath(LOther);
  Assert.AreSameRect(RectF(0, 0, 150, 150), LPathBuilder.Bounds, TEpsilon.Position);
end;

procedure TSkPathBuilderTests.TestArcToOval;
var
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ArcTo(RectF(0, 0, 100, 100), 0, 90, False);
  Assert.AreSameRect(RectF(0, 0, 100, 100), LPathBuilder.Bounds, 0.5, 'Without forceMoveTo the previous point is connected to the arc');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ArcTo(RectF(0, 0, 100, 100), 0, 90, True);
  Assert.AreSameRect(RectF(0, 0, 100, 100), LPathBuilder.Bounds, 0.5);
  LPath := LPathBuilder.Snapshot;
  Assert.IsFalse(LPath.IsEmpty, 'The compatibility adapter must still produce a valid path');
  Assert.AreSameRect(RectF(0, 0, 100, 100), LPath.Bounds, 0.5);
end;

procedure TSkPathBuilderTests.TestArcToRadius;
var
  LPathBuilder: ISkPathBuilder;
  LPath: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ArcTo(PointF(100, 0), PointF(100, 100), 30);
  LPath := LPathBuilder.Detach;
  Assert.IsFalse(LPath.IsEmpty);
  Assert.IsTrue(TSkSegmentMask.Conic in LPath.SegmentMasks, 'A tangent arc is built with conics');
  Assert.AreEqual(0.0, LPath.Bounds.Left, TEpsilon.Position);
  Assert.IsTrue(LPath.Bounds.Right <= 100 + TEpsilon.Position, 'The arc should not go past the corner');
end;

procedure TSkPathBuilderTests.TestArcToSvg;
var
  LPathBuilder: ISkPathBuilder;
  LLarge: ISkPath;
  LSmall: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ArcTo(PointF(60, 60), 0, TSkPathArcSize.Small, TSkPathDirection.CW, PointF(100, 0));
  LSmall := LPathBuilder.Detach;

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ArcTo(PointF(60, 60), 0, TSkPathArcSize.Large, TSkPathDirection.CW, PointF(100, 0));
  LLarge := LPathBuilder.Detach;

  Assert.IsFalse(LSmall.IsEmpty, '(small arc)');
  Assert.IsFalse(LLarge.IsEmpty, '(large arc)');
  Assert.IsTrue(LLarge.Bounds.Height > LSmall.Bounds.Height, 'The large arc should be taller than the small one');
end;

procedure TSkPathBuilderTests.TestBounds;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Assert.IsTrue(LPathBuilder.Bounds.IsEmpty, 'An empty builder should have empty bounds');
  LPathBuilder.AddRect(RectF(10, 20, 30, 40));
  Assert.AreSameRect(RectF(10, 20, 30, 40), LPathBuilder.Bounds, TEpsilon.Position);
  LPathBuilder.LineTo(100, 100);
  Assert.AreSameRect(RectF(10, 20, 100, 100), LPathBuilder.Bounds, TEpsilon.Position);
end;

procedure TSkPathBuilderTests.TestConicTo;
var
  LElem: TSkPathIteratorElem;
  LFound: Boolean;
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.ConicTo(50, 100, 100, 0, 2);
  LPath := LPathBuilder.Detach;
  Assert.IsTrue(TSkSegmentMask.Conic in LPath.SegmentMasks, 'The path should contain a conic');

  LFound := False;
  for LElem in LPath.GetIterator(False) do
    if LElem.Verb = TSkPathVerb.Conic then
    begin
      LFound := True;
      Assert.AreEqual(2.0, LElem.ConicWeight, TEpsilon.Vector, 'The conic weight should be preserved');
    end;
  Assert.IsTrue(LFound, 'The iterator should return the conic');
end;

procedure TSkPathBuilderTests.TestConstructors;
var
  LPathBuilder: ISkPathBuilder;
  LCopy: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.Winding, 'The default fill type should be Winding');

  LPathBuilder := TSkPathBuilder.Create(TSkPathFillType.EvenOdd);
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.EvenOdd);

  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  LCopy := TSkPathBuilder.Create(LPathBuilder);
  Assert.IsTrue(LCopy.FillType = TSkPathFillType.EvenOdd, 'The copy should keep the fill type');
  Assert.AreSameRect(LPathBuilder.Bounds, LCopy.Bounds, TEpsilon.Position, 'The copy should keep the contours');
end;

procedure TSkPathBuilderTests.TestDetachResetsTheBuilder;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  Assert.IsFalse(LPathBuilder.Detach.IsEmpty, 'Detach should return the built path');
  Assert.IsTrue(LPathBuilder.Detach.IsEmpty, 'Detach should reset the builder');
end;

procedure TSkPathBuilderTests.TestFillType;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.FillType := TSkPathFillType.EvenOdd;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.EvenOdd, '(EvenOdd)');
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  Assert.IsTrue(LPathBuilder.Snapshot.FillType = TSkPathFillType.EvenOdd, 'The path should keep the fill type');

  LPathBuilder.FillType := TSkPathFillType.InverseWinding;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.InverseWinding, '(InverseWinding)');
end;

procedure TSkPathBuilderTests.TestIncReserve;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.IncReserve(10);
  LPathBuilder.IncReserve(10, 10);
  Assert.IsTrue(LPathBuilder.Snapshot.IsEmpty, 'Reserving should not add contours');
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  Assert.AreSameRect(RectF(0, 0, 10, 10), LPathBuilder.Bounds, TEpsilon.Position);
end;

procedure TSkPathBuilderTests.TestOffset;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  LPathBuilder.Offset(5, -5);
  Assert.AreSameRect(RectF(5, -5, 15, 5), LPathBuilder.Bounds, TEpsilon.Position);
end;

procedure TSkPathBuilderTests.TestPolylineTo;
var
  LPathBuilder: ISkPathBuilder;
  LPath: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.PolylineTo([PointF(10, 0), PointF(10, 10), PointF(0, 10)]);
  LPath := LPathBuilder.Detach;
  Assert.AreSameRect(RectF(0, 0, 10, 10), LPath.Bounds, TEpsilon.Position);
  Assert.IsTrue(PointF(0, 10).EqualsTo(LPath.LastPoint, TEpsilon.Position), 'The last point should be the last polyline point');
end;

procedure TSkPathBuilderTests.TestRelativeCommandsMatchAbsoluteOnes;
var
  LAbsolute: ISkPath;
  LPathBuilder: ISkPathBuilder;
  LRelative: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(10, 10);
  LPathBuilder.LineTo(30, 10);
  LPathBuilder.QuadTo(40, 20, 50, 10);
  LPathBuilder.ConicTo(60, 20, 70, 10, 2);
  LPathBuilder.CubicTo(80, 20, 90, 0, 100, 10);
  LAbsolute := LPathBuilder.Detach;

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(10, 10);
  LPathBuilder.RLineTo(20, 0);
  LPathBuilder.RQuadTo(10, 10, 20, 0);
  LPathBuilder.RConicTo(10, 10, 20, 0, 2);
  LPathBuilder.RCubicTo(10, 10, 20, -10, 30, 0);
  LRelative := LPathBuilder.Detach;

  Assert.AreEqual(PathToText(LAbsolute), PathToText(LRelative), 'The relative commands should produce the same path');
end;

procedure TSkPathBuilderTests.TestSnapshotKeepsTheBuilder;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  Assert.IsFalse(LPathBuilder.Snapshot.IsEmpty, '(first snapshot)');
  Assert.IsFalse(LPathBuilder.Snapshot.IsEmpty, 'Snapshot should not reset the builder');
  Assert.AreSameRect(RectF(0, 0, 10, 10), LPathBuilder.Bounds, TEpsilon.Position);
end;

procedure TSkPathBuilderTests.TestToggleInverseFillType;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.ToggleInverseFillType;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.InverseWinding, 'Winding should become InverseWinding');
  LPathBuilder.ToggleInverseFillType;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.Winding, 'InverseWinding should become Winding');

  LPathBuilder.FillType := TSkPathFillType.EvenOdd;
  LPathBuilder.ToggleInverseFillType;
  Assert.IsTrue(LPathBuilder.FillType = TSkPathFillType.InverseEvenOdd, 'EvenOdd should become InverseEvenOdd');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPathBuilderTests);
end.
