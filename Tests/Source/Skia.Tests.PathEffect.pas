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
unit Skia.Tests.PathEffect;

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
  { TSkPathEffectTests }

  [TestFixture]
  TSkPathEffectTests = class(TTestBase)
  strict private
    function ApplyEffect(const AEffect: ISkPathEffect; const APath: ISkPath): ISkPath;
    function ApplyStrokedEffect(const AEffect: ISkPathEffect; const APath: ISkPath; const AStrokeWidth: Single): ISkPath;
    function ContourCount(const APath: ISkPath): Integer;
    function HorizontalLine(const ALength: Single): ISkPath;
    function PathLength(const APath: ISkPath): Single;
    function UnitSquare: ISkPath;
  public
    [Test]
    procedure TestCornerRoundsTheCorners;
    [Test]
    procedure TestDashHalvesTheLength;
    [Test]
    procedure TestDiscrete;
    [Test]
    procedure TestFactoriesReturnAnEffect;
    [Test]
    procedure TestMakeCompose;
    [Test]
    procedure TestMakeMerge;
    [Test]
    procedure TestMakeSum;
    [Test]
    procedure TestMatrixTransformsThePath;
    [Test]
    procedure TestStrokeOutlinesThePath;
    [Test]
    procedure TestTranslateMovesThePath;
    [Test]
    procedure TestTrimKeepsAFraction;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types;

{ TSkPathEffectTests }

function TSkPathEffectTests.ApplyEffect(const AEffect: ISkPathEffect;
  const APath: ISkPath): ISkPath;
var
  LPaint: ISkPaint;
begin
  Assert.IsNotNull(AEffect, 'Invalid ISkPathEffect (nil)');
  LPaint := TSkPaint.Create;
  LPaint.PathEffect := AEffect;
  Result := LPaint.GetFillPath(APath);
  Assert.IsNotNull(Result, 'Invalid ISkPath (nil)');
end;

function TSkPathEffectTests.ApplyStrokedEffect(const AEffect: ISkPathEffect;
  const APath: ISkPath; const AStrokeWidth: Single): ISkPath;
var
  LPaint: ISkPaint;
begin
  Assert.IsNotNull(AEffect, 'Invalid ISkPathEffect (nil)');
  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LPaint.StrokeWidth := AStrokeWidth;
  LPaint.StrokeCap := TSkStrokeCap.Butt;
  LPaint.PathEffect := AEffect;
  Result := LPaint.GetFillPath(APath);
  Assert.IsNotNull(Result, 'Invalid ISkPath (nil)');
end;

function TSkPathEffectTests.ContourCount(const APath: ISkPath): Integer;
var
  LPathMeasure: ISkPathMeasure;
begin
  LPathMeasure := TSkPathMeasure.Create(APath);
  Result := 0;
  repeat
    if LPathMeasure.Length > 0 then
      Inc(Result);
  until not LPathMeasure.NextContour;
end;

function TSkPathEffectTests.HorizontalLine(const ALength: Single): ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 0);
  LPathBuilder.LineTo(ALength, 0);
  Result := LPathBuilder.Detach;
end;

function TSkPathEffectTests.PathLength(const APath: ISkPath): Single;
var
  LPathMeasure: ISkPathMeasure;
begin
  LPathMeasure := TSkPathMeasure.Create(APath);
  Result := 0;
  repeat
    Result := Result + LPathMeasure.Length;
  until not LPathMeasure.NextContour;
end;

function TSkPathEffectTests.UnitSquare: ISkPath;
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 100, 100));
  Result := LPathBuilder.Detach;
end;

procedure TSkPathEffectTests.TestCornerRoundsTheCorners;
var
  LResult: ISkPath;
begin
  LResult := ApplyEffect(TSkPathEffect.MakeCorner(20), UnitSquare);
  Assert.IsFalse(LResult.IsRect, 'A rounded square is not a rect anymore');
  Assert.AreSameRect(RectF(0, 0, 100, 100), LResult.Bounds, TEpsilon.Position, 'The bounds should not change');
  Assert.IsTrue(TSkSegmentMask.Quad in LResult.SegmentMasks, 'The corners should become quadratic curves');
end;

procedure TSkPathEffectTests.TestDashHalvesTheLength;
var
  LResult: ISkPath;
begin
  LResult := ApplyStrokedEffect(TSkPathEffect.MakeDash([10, 10], 0), HorizontalLine(100), 2);
  Assert.AreEqual(5, ContourCount(LResult), 'A 10/10 dash over 100 units should produce 5 dashes');
  Assert.AreSameRect(RectF(0, -1, 90, 1), LResult.Bounds, TEpsilon.Position, 'The last dash should end at 90');

  LResult := ApplyEffect(TSkPathEffect.MakeDash([10, 10], 0), HorizontalLine(100));
  Assert.AreEqual(100.0, PathLength(LResult), TEpsilon.Position, 'A dash effect is ignored by a fill paint');
end;

procedure TSkPathEffectTests.TestDiscrete;
var
  LResult1: ISkPath;
  LResult2: ISkPath;
begin
  LResult1 := ApplyEffect(TSkPathEffect.MakeDiscrete(10, 4, 1), HorizontalLine(100));
  Assert.IsFalse(LResult1.IsEmpty, 'The discrete effect should keep the path');
  Assert.IsTrue(LResult1.Bounds.Height > 0, 'The discrete effect should displace the line');

  LResult2 := ApplyEffect(TSkPathEffect.MakeDiscrete(10, 4, 1), HorizontalLine(100));
  Assert.AreEqual(PathToText(LResult1), PathToText(LResult2), 'The same seed should produce the same path');

  LResult2 := ApplyEffect(TSkPathEffect.MakeDiscrete(10, 4, 2), HorizontalLine(100));
  Assert.AreNotEqual(PathToText(LResult1), PathToText(LResult2), 'A different seed should produce a different path');
end;

procedure TSkPathEffectTests.TestFactoriesReturnAnEffect;
begin
  Assert.IsNotNull(TSkPathEffect.Make1DPath(UnitSquare, 30, 0, TSkPathEffect1DStyle.Translate), '(Make1DPath)');
  Assert.IsNotNull(TSkPathEffect.Make2DLine(2, TMatrix.CreateScaling(10, 10)), '(Make2DLine)');
  Assert.IsNotNull(TSkPathEffect.Make2DPath(TMatrix.CreateScaling(10, 10), UnitSquare), '(Make2DPath)');
  Assert.IsNotNull(TSkPathEffect.MakeCorner(4), '(MakeCorner)');
  Assert.IsNotNull(TSkPathEffect.MakeDash([4, 4], 0), '(MakeDash)');
  Assert.IsNotNull(TSkPathEffect.MakeDiscrete(4, 4), '(MakeDiscrete)');
  Assert.IsNotNull(TSkPathEffect.MakeMatrix(TMatrix.CreateTranslation(5, 5)), '(MakeMatrix)');
  Assert.IsNotNull(TSkPathEffect.MakeStroke(4, TSkStrokeJoin.Miter, TSkStrokeCap.Butt), '(MakeStroke)');
  Assert.IsNotNull(TSkPathEffect.MakeStrokeAndFill, '(MakeStrokeAndFill)');
  Assert.IsNotNull(TSkPathEffect.MakeTranslate(5, 5), '(MakeTranslate)');
  Assert.IsNotNull(TSkPathEffect.MakeTrim(0, 0.5, TSkPathEffectTrimMode.Normal), '(MakeTrim)');
end;

procedure TSkPathEffectTests.TestMakeCompose;
var
  LEffect: ISkPathEffect;
  LResult: ISkPath;
begin
  LEffect := TSkPathEffect.MakeCompose(TSkPathEffect.MakeTranslate(10, 0), TSkPathEffect.MakeDash([10, 10], 0));
  LResult := ApplyStrokedEffect(LEffect, HorizontalLine(100), 2);
  Assert.AreEqual(5, ContourCount(LResult), 'The inner dash should still produce 5 dashes');
  Assert.AreSameRect(RectF(10, -1, 100, 1), LResult.Bounds, TEpsilon.Position, 'The outer translate should move the dashed result');
end;

procedure TSkPathEffectTests.TestMakeMerge;
var
  LEffect: ISkPathEffect;
  LResult: ISkPath;
begin
  LEffect := TSkPathEffect.MakeMerge(TSkPathEffect.MakeTranslate(0, -10),
    TSkPathEffect.MakeTranslate(0, 10), TSkPathOp.Union);
  LResult := ApplyEffect(LEffect, UnitSquare);
  Assert.AreSameRect(RectF(0, -10, 100, 110), LResult.Bounds, TEpsilon.Position, 'The union should cover both translated squares');
end;

procedure TSkPathEffectTests.TestMakeSum;
var
  LEffect: ISkPathEffect;
  LResult: ISkPath;
begin
  LEffect := TSkPathEffect.MakeSum(TSkPathEffect.MakeTranslate(0, -10), TSkPathEffect.MakeTranslate(0, 10));
  Assert.IsNotNull(LEffect, 'Invalid ISkPathEffect (nil)');
  LResult := ApplyEffect(LEffect, HorizontalLine(100));
  Assert.IsFalse(LResult.IsEmpty, 'The sum should still produce a path');
  Assert.AreEqual(100.0, PathLength(LResult), TEpsilon.Position, 'Each summed effect is applied to the original path');
end;

procedure TSkPathEffectTests.TestMatrixTransformsThePath;
var
  LResult: ISkPath;
begin
  LResult := ApplyEffect(TSkPathEffect.MakeMatrix(TMatrix.CreateScaling(2, 3)), UnitSquare);
  Assert.AreSameRect(RectF(0, 0, 200, 300), LResult.Bounds, TEpsilon.Position);
end;

procedure TSkPathEffectTests.TestStrokeOutlinesThePath;
var
  LResult: ISkPath;
begin
  LResult := ApplyEffect(TSkPathEffect.MakeStroke(10, TSkStrokeJoin.Miter, TSkStrokeCap.Butt), HorizontalLine(100));
  Assert.AreSameRect(RectF(0, -5, 100, 5), LResult.Bounds, TEpsilon.Position, 'The stroke should be centered on the line');
end;

procedure TSkPathEffectTests.TestTranslateMovesThePath;
var
  LResult: ISkPath;
begin
  LResult := ApplyEffect(TSkPathEffect.MakeTranslate(15, -25), UnitSquare);
  Assert.AreSameRect(RectF(15, -25, 115, 75), LResult.Bounds, TEpsilon.Position);
end;

procedure TSkPathEffectTests.TestTrimKeepsAFraction;
var
  LResult: ISkPath;
begin
  LResult := ApplyEffect(TSkPathEffect.MakeTrim(0.25, 0.75, TSkPathEffectTrimMode.Normal), HorizontalLine(100));
  Assert.AreSameRect(RectF(25, 0, 75, 0), LResult.Bounds, TEpsilon.Position, 'Normal trim should keep the middle half');

  LResult := ApplyEffect(TSkPathEffect.MakeTrim(0.25, 0.75, TSkPathEffectTrimMode.Inverted), HorizontalLine(100));
  Assert.AreSameRect(RectF(0, 0, 100, 0), LResult.Bounds, TEpsilon.Position, 'Inverted trim should keep both ends');
  Assert.AreEqual(50.0, PathLength(LResult), TEpsilon.Position, 'Inverted trim should keep half of the length');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPathEffectTests);
end.
