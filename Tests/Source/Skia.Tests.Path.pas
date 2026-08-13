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
unit Skia.Tests.Path;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkPathTests }

  [TestFixture]
  TSkPathTests = class(TTestBase)
  private
    function CreateSimplePath: ISkPath;
    function MakeLine(const AX1, AY1, AX2, AY2: Single): ISkPath;
    function MakeRect(const ARect: TRectF): ISkPath;
  public
    [TestCase('Simple Path', 'simple-path.elements.txt')]
    procedure TestBasicIterator(const AExpectedIteratorOutputFileName: string);
    [TestCase('Simple Path2', 'simple-path2.elements.txt')]
    procedure TestBasicToSVG(const AExpectedIteratorOutputFileName: string);
    [TestCase('Discord Icon Path',   'discord.svg-path.txt,' +   'discord.elements.txt')]
    [TestCase('Firefox Icon Path',   'firefox.svg-path.txt,' +   'firefox.elements.txt')]
    [TestCase('Microsoft Icon Path', 'microsoft.svg-path.txt,' + 'microsoft.elements.txt')]
    [TestCase('Telegram Icon Path',  'telegram.svg-path.txt,' +  'telegram.elements.txt')]
    [TestCase('Tesla Icon Path',     'tesla.svg-path.txt,' +     'tesla.elements.txt')]
    procedure TestIteratorFromSVGPath(const ASVGPathInputFileName, AExpectedIteratorOutputFileName: string);
    [Test]
    procedure TestContains;
    [Test]
    procedure TestConvertConicToQuads;
    [Test]
    procedure TestFillType;
    [Test]
    procedure TestInterpolate;
    [Test]
    procedure TestIsConvex;
    [Test]
    procedure TestIsEmpty;
    [Test]
    procedure TestIsFinite;
    [Test]
    procedure TestIsLastContourClosed;
    [Test]
    procedure TestIsLine;
    [Test]
    procedure TestIsOval;
    [Test]
    procedure TestIsRect;
    [Test]
    procedure TestIsRoundRect;
    [Test]
    procedure TestLastPoint;
    [Test]
    procedure TestOp;
    [Test]
    procedure TestSegmentMasks;
    [Test]
    procedure TestSerializeToStream;
    [Test]
    procedure TestTightBounds;
    [Test]
    procedure TestTransform;
    [TestCase('Simple Path Serialize', 'simple-path.serialized.bin')]
    procedure TestPathSerialize(const AExpectedSerializedOutputFileName: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Math,
  System.Math.Vectors,
  System.UITypes,
  System.IOUtils;

{ TSkPathTests }

function TSkPathTests.CreateSimplePath: ISkPath;
var
  LOval: ISkRoundRect;
  LPathBuilder: ISkPathBuilder;
  LRect: TRectF;
begin
  LRect := TRectF.Create(TPointF.Create(10, 10), 100, 160);
  LOval := TSkRoundRect.Create;
  LOval.SetOval(LRect);
  LOval.Offset(40, 80);
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(LRect);
  LPathBuilder.AddRoundRect(LOval);
  LRect.Offset(80, 50);
  LPathBuilder.AddOval(LRect);

  LPathBuilder.MoveTo(0, -10);
  LPathBuilder.QuadTo(100, 100, -10, 0);

  Result := LPathBuilder.Detach;
end;

function TSkPathTests.MakeLine(const AX1, AY1, AX2, AY2: Single): ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(AX1, AY1);
  LPathBuilder.LineTo(AX2, AY2);
  Result := LPathBuilder.Detach;
end;

function TSkPathTests.MakeRect(const ARect: TRectF): ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(ARect);
  Result := LPathBuilder.Detach;
end;

procedure TSkPathTests.TestContains;
var
  LPath: ISkPath;
begin
  LPath := MakeRect(RectF(10, 10, 50, 50));
  Assert.IsTrue(LPath.Contains(20, 20), 'A point inside the rect should be contained');
  Assert.IsFalse(LPath.Contains(5, 5), 'A point outside the rect should not be contained');
end;

procedure TSkPathTests.TestConvertConicToQuads;
var
  LPoints: TArray<TPointF>;
begin
  LPoints := TSkPath.ConvertConicToQuads(PointF(0, 0), PointF(50, 100), PointF(100, 0), 0.5, 1);
  Assert.AreEqual<NativeInt>(5, Length(LPoints), 'One power of two produces two quads, so five points');
  Assert.IsTrue(PointF(0, 0).EqualsTo(LPoints[0], TEpsilon.Position), 'The first point should be the start');
  Assert.IsTrue(PointF(100, 0).EqualsTo(LPoints[High(LPoints)], TEpsilon.Position), 'The last point should be the end');

  LPoints := TSkPath.ConvertConicToQuads(PointF(0, 0), PointF(50, 100), PointF(100, 0), 0.5, 0);
  Assert.AreEqual<NativeInt>(3, Length(LPoints), 'Zero powers of two produce a single quad');
end;

procedure TSkPathTests.TestFillType;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  Assert.IsTrue(LPathBuilder.Snapshot.FillType = TSkPathFillType.Winding, '(default)');

  LPathBuilder.FillType := TSkPathFillType.InverseEvenOdd;
  Assert.IsTrue(LPathBuilder.Snapshot.FillType = TSkPathFillType.InverseEvenOdd, '(inverse even odd)');
end;

procedure TSkPathTests.TestInterpolate;
var
  LEnding: ISkPath;
  LResult: ISkPath;
  LStarting: ISkPath;
begin
  LStarting := MakeRect(RectF(0, 0, 100, 100));
  LEnding := MakeRect(RectF(100, 100, 200, 200));
  Assert.IsTrue(LStarting.IsInterpolatable(LEnding), 'Two rects have the same verbs, so they are interpolatable');

  LResult := LStarting.Interpolate(LEnding, 0.5);
  Assert.IsNotNull(LResult, 'Invalid ISkPath (nil)');
  Assert.AreSameRect(RectF(50, 50, 150, 150), LResult.Bounds, TEpsilon.Position, 'The middle should be halfway between both rects');

  Assert.IsFalse(LStarting.IsInterpolatable(MakeLine(0, 0, 10, 10)), 'A rect and a line are not interpolatable');
  Assert.IsNull(LStarting.Interpolate(MakeLine(0, 0, 10, 10), 0.5), 'Interpolating incompatible paths should fail');
end;

procedure TSkPathTests.TestIsConvex;
var
  LPathBuilder: ISkPathBuilder;
begin
  Assert.IsTrue(MakeRect(RectF(0, 0, 10, 10)).IsConvex, 'A rect is convex');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddPolygon([PointF(0, 0), PointF(100, 0), PointF(100, 100), PointF(50, 50), PointF(0, 100)], True);
  Assert.IsFalse(LPathBuilder.Detach.IsConvex, 'An arrow shaped polygon is not convex');
end;

procedure TSkPathTests.TestIsFinite;
var
  LPathBuilder: ISkPathBuilder;
begin
  Assert.IsTrue(MakeRect(RectF(0, 0, 10, 10)).IsFinite, 'A normal path is finite');
  LPathBuilder := TSkPathBuilder.Create;
  Assert.IsTrue(LPathBuilder.Detach.IsFinite, 'An empty path is finite');
end;

procedure TSkPathTests.TestIsLastContourClosed;
var
  LPathBuilder: ISkPathBuilder;
begin
  Assert.IsTrue(MakeRect(RectF(0, 0, 10, 10)).IsLastContourClosed, 'A rect contour is closed');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 10, 10));
  LPathBuilder.MoveTo(20, 20);
  LPathBuilder.LineTo(30, 30);
  Assert.IsFalse(LPathBuilder.Snapshot.IsLastContourClosed, 'The trailing open contour should not be reported as closed');
  LPathBuilder.Close;
  Assert.IsTrue(LPathBuilder.Snapshot.IsLastContourClosed, 'Close should close the last contour');
end;

procedure TSkPathTests.TestIsLine;
var
  LPath: ISkPath;
begin
  LPath := MakeLine(10, 20, 30, 40);
  Assert.IsTrue(LPath.IsLine, 'A single segment path is a line');
  Assert.IsTrue(LPath.IsLine(PointF(10, 20), PointF(30, 40)), 'The line should match its points');
  Assert.IsFalse(MakeRect(RectF(0, 0, 10, 10)).IsLine, 'A rect is not a line');
end;

procedure TSkPathTests.TestIsOval;
var
  LPathBuilder: ISkPathBuilder;
  LRect: TRectF;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddOval(RectF(10, 20, 110, 80));
  Assert.IsTrue(LPathBuilder.Snapshot.IsOval, 'An added oval should be reported as an oval');
  Assert.IsTrue(LPathBuilder.Snapshot.IsOval(LRect), '(with out parameter)');
  Assert.AreSameRect(RectF(10, 20, 110, 80), LRect, TEpsilon.Position, 'The oval bounds should be returned');

  Assert.IsFalse(MakeRect(RectF(0, 0, 10, 10)).IsOval, 'A rect is not an oval');
end;

procedure TSkPathTests.TestIsRoundRect;
var
  LPathBuilder: ISkPathBuilder;
  LRoundRect: ISkRoundRect;
  LSource: ISkRoundRect;
begin
  LSource := TSkRoundRect.Create(RectF(0, 0, 100, 60), 10, 10);
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRoundRect(LSource);
  Assert.IsTrue(LPathBuilder.Snapshot.IsRoundRect, 'An added round rect should be reported as one');
  Assert.IsTrue(LPathBuilder.Snapshot.IsRoundRect(LRoundRect), '(with out parameter)');
  Assert.IsNotNull(LRoundRect, 'Invalid ISkRoundRect (nil)');
  Assert.AreSameRect(RectF(0, 0, 100, 60), LRoundRect.Rect, TEpsilon.Position);
  Assert.IsTrue(LRoundRect.SimpleRadii.EqualsTo(PointF(10, 10), TEpsilon.Position));

  Assert.IsFalse(MakeLine(0, 0, 10, 10).IsRoundRect, 'A line is not a round rect');
end;

procedure TSkPathTests.TestLastPoint;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(10, 20);
  LPathBuilder.LineTo(30, 40);
  Assert.IsTrue(PointF(30, 40).EqualsTo(LPathBuilder.Snapshot.LastPoint, TEpsilon.Position));
  LPathBuilder.LineTo(50, 60);
  Assert.IsTrue(PointF(50, 60).EqualsTo(LPathBuilder.Snapshot.LastPoint, TEpsilon.Position));
end;

procedure TSkPathTests.TestOp;
var
  LResult: ISkPath;
begin
  LResult := MakeRect(RectF(0, 0, 100, 100)).Op(MakeRect(RectF(50, 0, 200, 100)), TSkPathOp.Intersect);
  Assert.IsNotNull(LResult);
  Assert.AreSameRect(RectF(50, 0, 100, 100), LResult.Bounds, TEpsilon.Position, '(intersect)');

  LResult := MakeRect(RectF(0, 0, 100, 100)).Op(MakeRect(RectF(50, 0, 200, 100)), TSkPathOp.Union);
  Assert.AreSameRect(RectF(0, 0, 200, 100), LResult.Bounds, TEpsilon.Position, '(union)');

  LResult := MakeRect(RectF(0, 0, 100, 100)).Op(MakeRect(RectF(0, 0, 100, 100)), TSkPathOp.&Xor);
  Assert.IsTrue(LResult.IsEmpty, 'The symmetric difference of two equal paths should be empty');
end;

procedure TSkPathTests.TestSegmentMasks;
var
  LPathBuilder: ISkPathBuilder;
begin
  Assert.IsTrue(MakeRect(RectF(0, 0, 10, 10)).SegmentMasks = [TSkSegmentMask.Line], 'A rect only has lines');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.QuadTo(10, 10, 20, 0);
  LPathBuilder.CubicTo(30, 10, 40, -10, 50, 0);
  LPathBuilder.ConicTo(60, 10, 70, 0, 2);
  Assert.IsTrue(LPathBuilder.Snapshot.SegmentMasks = [TSkSegmentMask.Quad, TSkSegmentMask.Conic, TSkSegmentMask.Cubic],
    'The masks should report every segment kind used');
end;

procedure TSkPathTests.TestSerializeToStream;
var
  LPath: ISkPath;
  LStream: TBytesStream;
begin
  LPath := CreateSimplePath;
  LStream := TBytesStream.Create;
  try
    LPath.SerializeToStream(LStream);
    Assert.AreEqualBytes(LPath.Serialize, Copy(LStream.Bytes, 0, LStream.Size),
      'Serialize and SerializeToStream should produce the same bytes');
  finally
    LStream.Free;
  end;
end;

procedure TSkPathTests.TestTightBounds;
var
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.QuadTo(50, 100, 100, 0);
  LPath := LPathBuilder.Detach;
  Assert.AreSameRect(RectF(0, 0, 100, 100), LPath.Bounds, TEpsilon.Position, 'The control point is part of the loose bounds');
  Assert.AreSameRect(RectF(0, 0, 100, 50), LPath.TightBounds, TEpsilon.Position, 'The tight bounds should follow the curve');
end;

procedure TSkPathTests.TestTransform;
var
  LPath: ISkPath;
begin
  LPath := MakeRect(RectF(0, 0, 100, 50)).Transform(TMatrix.CreateScaling(2, 3));
  Assert.IsNotNull(LPath);
  Assert.AreSameRect(RectF(0, 0, 200, 150), LPath.Bounds, TEpsilon.Position);
end;

procedure TSkPathTests.TestBasicIterator(const AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := CreateSimplePath;
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestBasicToSVG(
  const AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(10, 20);
  LPathBuilder.LineTo(35, 45);
  LPathBuilder.LineTo(60, 55);
  LPathBuilder.CubicTo(80, 80, 5, 5, 100, 100);
  LPathBuilder.LineTo(40, 40);
  LPathBuilder.Close;
  LPath := TSkPath.Create(LPathBuilder.Detach.ToSVG);
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestIsEmpty;

  procedure Check(APath: ISkPath; const AExpected: Boolean; const AMessage: string);
  begin
    Assert.AreEqual(AExpected, APath.IsEmpty, AMessage);
  end;

var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Check(LPathBuilder.Snapshot, True, '(Empty path)');

  LPathBuilder.AddRect(RectF(10, 10, 50, 70));
  Check(LPathBuilder.Snapshot, False, '(Path with single rect)');

  LPathBuilder.Reset;
  LPathBuilder.MoveTo(10, 20);
  Check(LPathBuilder.Snapshot, False, '(Path with moveTo)');

  LPathBuilder.Reset;
  Check(LPathBuilder.Snapshot, True, '(Path after reset)');

  LPathBuilder.Reset;
  LPathBuilder.AddPolygon([PointF(0, 0), PointF(0, 80), PointF(80, 80), PointF(80, 0), PointF(40, 0), PointF(20, 0)], False);
  Check(LPathBuilder.Snapshot, False, '(Path with rect polygon)');
end;

procedure TSkPathTests.TestIsRect;

  procedure Check(APath: ISkPath; const AExpected: Boolean; const AMessage: string);
  begin
    Assert.AreEqual(AExpected, APath.IsRect, AMessage);
  end;

var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  Check(LPathBuilder.Snapshot, False, '(Empty path)');

  LPathBuilder.AddRect(RectF(10, 10, 50, 70));
  Check(LPathBuilder.Snapshot, True, '(Path with single rect)');

  LPathBuilder.MoveTo(10, 20);
  Check(LPathBuilder.Snapshot, True, '(Path with rect and moveTo)');

  LPathBuilder.LineTo(10, 30);
  Check(LPathBuilder.Snapshot, False, '(Path with rect and lineTo)');

  LPathBuilder.Reset;
  LPathBuilder.AddPolygon([PointF(0, 0), PointF(0, 80), PointF(80, 80), PointF(80, 0), PointF(40, 0), PointF(20, 0)], False);
  Check(LPathBuilder.Snapshot, True, '(Path with rect polygon)');
end;

procedure TSkPathTests.TestIteratorFromSVGPath(const ASVGPathInputFileName,
  AExpectedIteratorOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := TSkPath.Create(TFile.ReadAllText(AssetsPath + ASVGPathInputFileName));
  Assert.AreEqual(TFile.ReadAllText(AssetsPath + AExpectedIteratorOutputFileName).Trim, PathToText(LPath));
end;

procedure TSkPathTests.TestPathSerialize(
  const AExpectedSerializedOutputFileName: string);
var
  LPath: ISkPath;
begin
  LPath := CreateSimplePath;
  Assert.AreEqualBytes(TFile.ReadAllBytes(AssetsPath + AExpectedSerializedOutputFileName), LPath.Serialize);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPathTests);
end.
