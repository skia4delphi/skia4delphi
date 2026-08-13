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
unit Skia.Tests.PathMeasure;

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
  { TSkPathMeasureTests }

  [TestFixture]
  TSkPathMeasureTests = class(TTestBase)
  strict private
    function CreateHorizontalLine(const ALength: Single): ISkPath;
  public
    [Test]
    procedure TestForceClosed;
    [Test]
    procedure TestIsClosed;
    [Test]
    procedure TestLengthOfLine;
    [Test]
    procedure TestLengthOfRect;
    [Test]
    procedure TestMatrix;
    [Test]
    procedure TestNextContour;
    [Test]
    procedure TestOutOfRangeDistance;
    [Test]
    procedure TestPositionAndTangent;
    [Test]
    procedure TestSegment;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types;

{ TSkPathMeasureTests }

function TSkPathMeasureTests.CreateHorizontalLine(
  const ALength: Single): ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.LineTo(ALength, 0);
  Result := LPathBuilder.Detach;
end;

procedure TSkPathMeasureTests.TestForceClosed;
var
  LPathBuilder: ISkPathBuilder;
  LPathMeasure: ISkPathMeasure;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.LineTo(30, 0);
  LPathBuilder.LineTo(30, 40);

  LPathMeasure := TSkPathMeasure.Create(LPathBuilder.Snapshot, False);
  Assert.AreEqual(70.0, LPathMeasure.Length, TEpsilon.Position, 'The open contour is 30 + 40 long');

  LPathMeasure := TSkPathMeasure.Create(LPathBuilder.Snapshot, True);
  Assert.AreEqual(120.0, LPathMeasure.Length, TEpsilon.Position, 'Forcing the contour closed adds the 50 long hypotenuse');
  Assert.IsTrue(LPathMeasure.IsClosed, 'A forced closed contour should be reported as closed');
end;

procedure TSkPathMeasureTests.TestIsClosed;
var
  LPathBuilder: ISkPathBuilder;
  LPathMeasure: ISkPathMeasure;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 100, 50));
  LPathMeasure := TSkPathMeasure.Create(LPathBuilder.Detach);
  Assert.IsTrue(LPathMeasure.IsClosed, 'A rect contour is closed');
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  Assert.IsFalse(LPathMeasure.IsClosed, 'A line contour is not closed');
end;

procedure TSkPathMeasureTests.TestLengthOfLine;
var
  LPathMeasure: ISkPathMeasure;
begin
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  Assert.IsNotNull(LPathMeasure);
  Assert.AreEqual(100.0, LPathMeasure.Length, TEpsilon.Position);
end;

procedure TSkPathMeasureTests.TestLengthOfRect;
var
  LPathBuilder: ISkPathBuilder;
  LPathMeasure: ISkPathMeasure;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 100, 50));
  LPathMeasure := TSkPathMeasure.Create(LPathBuilder.Detach);
  Assert.AreEqual(300.0, LPathMeasure.Length, TEpsilon.Position);
end;

procedure TSkPathMeasureTests.TestMatrix;
var
  LMatrix: TMatrix;
  LPathMeasure: ISkPathMeasure;
begin
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  Assert.IsTrue(LPathMeasure.GetMatrix(25, LMatrix), 'GetMatrix should succeed inside the contour');
  Assert.AreEqual(25.0, LMatrix.m31, TEpsilon.Position, 'The translation should be the position at the distance');
  Assert.AreEqual(0.0, LMatrix.m32, TEpsilon.Position);
  Assert.AreEqual(1.0, LMatrix.m11, TEpsilon.Vector, 'The tangent of a horizontal line is (1, 0)');
  Assert.AreEqual(0.0, LMatrix.m12, TEpsilon.Vector);

  Assert.IsTrue(LPathMeasure.GetMatrix(25, LMatrix, [TSkPathMeasureMatrixFlag.Position]), 'GetMatrix should accept a position only request');
  Assert.AreEqual(25.0, LMatrix.m31, TEpsilon.Position);
end;

procedure TSkPathMeasureTests.TestNextContour;
var
  LPathBuilder: ISkPathBuilder;
  LPathMeasure: ISkPathMeasure;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.LineTo(100, 0);
  LPathBuilder.MoveTo(0, 10);
  LPathBuilder.LineTo(40, 10);

  LPathMeasure := TSkPathMeasure.Create(LPathBuilder.Detach);
  Assert.AreEqual(100.0, LPathMeasure.Length, TEpsilon.Position, 'The first contour is 100 long');
  Assert.IsTrue(LPathMeasure.NextContour, 'There should be a second contour');
  Assert.AreEqual(40.0, LPathMeasure.Length, TEpsilon.Position, 'The second contour is 40 long');
  Assert.IsFalse(LPathMeasure.NextContour, 'There should be no third contour');
end;

procedure TSkPathMeasureTests.TestOutOfRangeDistance;
var
  LPathMeasure: ISkPathMeasure;
  LPosition: TPointF;
  LTangent: TPointF;
begin
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  Assert.IsTrue(LPathMeasure.GetPositionAndTangent(-10, LPosition, LTangent), 'A negative distance should be clamped to the start');
  Assert.AreEqual(0.0, LPosition.X, TEpsilon.Position);
  Assert.IsTrue(LPathMeasure.GetPositionAndTangent(1000, LPosition, LTangent), 'A distance past the end should be clamped to the end');
  Assert.AreEqual(100.0, LPosition.X, TEpsilon.Position);
end;

procedure TSkPathMeasureTests.TestPositionAndTangent;
var
  LPathMeasure: ISkPathMeasure;
  LPosition: TPointF;
  LTangent: TPointF;
begin
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  Assert.IsTrue(LPathMeasure.GetPositionAndTangent(0, LPosition, LTangent));
  Assert.AreEqual(0.0, LPosition.X, TEpsilon.Position, '(start X)');
  Assert.AreEqual(0.0, LPosition.Y, TEpsilon.Position, '(start Y)');

  Assert.IsTrue(LPathMeasure.GetPositionAndTangent(30, LPosition, LTangent));
  Assert.AreEqual(30.0, LPosition.X, TEpsilon.Position, '(middle X)');
  Assert.AreEqual(0.0, LPosition.Y, TEpsilon.Position, '(middle Y)');
  Assert.AreEqual(1.0, LTangent.X, TEpsilon.Vector, '(tangent X)');
  Assert.AreEqual(0.0, LTangent.Y, TEpsilon.Vector, '(tangent Y)');

  Assert.IsTrue(LPathMeasure.GetPositionAndTangent(100, LPosition, LTangent));
  Assert.AreEqual(100.0, LPosition.X, TEpsilon.Position, '(end X)');
end;

procedure TSkPathMeasureTests.TestSegment;
var
  LPathMeasure: ISkPathMeasure;
  LSegment: ISkPath;
  LSegmentMeasure: ISkPathMeasure;
begin
  LPathMeasure := TSkPathMeasure.Create(CreateHorizontalLine(100));
  LSegment := LPathMeasure.GetSegment(25, 75, True);
  Assert.IsNotNull(LSegment, 'Invalid ISkPath (nil)');
  Assert.IsFalse(LSegment.IsEmpty, 'The segment should not be empty');
  Assert.AreSameRect(RectF(25, 0, 75, 0), LSegment.Bounds, TEpsilon.Position);
  LSegmentMeasure := TSkPathMeasure.Create(LSegment);
  Assert.AreEqual(50.0, LSegmentMeasure.Length, TEpsilon.Position, 'The segment should be half of the contour');

  LSegment := LPathMeasure.GetSegment(75, 25, True);
  Assert.IsTrue((LSegment = nil) or LSegment.IsEmpty, 'An inverted range should produce an empty segment');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPathMeasureTests);
end.
