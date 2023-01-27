{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2023 Google LLC.                                    }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.RoundRect;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkRoundRectTests }

  [TestFixture]
  TSkRoundRectTests = class(TTestBase)
  public
    [Test]
    procedure TestBasic;
    [Test]
    procedure TestConstructors;
    [Test]
    procedure TestDefaultValue;
    [TestCase('1', '0.99,Jy8nPyc/B///Lzc/Jz8H//+/N783vzf///9//z////8M/2x/j////wx/bH///4//bH8Mf/////8')]
    procedure TestDraw(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestInflate;
    [Test]
    procedure TestNinePatch;
    [Test]
    procedure TestOval;
    [Test]
    procedure TestSetRect;
    [Test]
    procedure TestTransform;
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
  Skia;

{ TSkRoundRectTests }

procedure TSkRoundRectTests.TestBasic;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect.SetRect(RectF(100, 20, 130, 220));
  Assert.IsTrue(RectF(100, 20, 130, 220).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
end;

procedure TSkRoundRectTests.TestConstructors;
var
  LRoundRect: ISkRoundRect;
  LRadii: TSkRoundRectRadii;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect := TSkRoundRect.Create(LRoundRect);
  Assert.IsNotNull(LRoundRect);
  LRoundRect := TSkRoundRect.Create(RectF(100, 20, 130, 220), 6, 9);
  Assert.IsNotNull(LRoundRect);
  LRadii[TSkRoundRectCorner.UpperLeft]  := 2;
  LRadii[TSkRoundRectCorner.UpperRight] := 3;
  LRadii[TSkRoundRectCorner.LowerRight] := 4;
  LRadii[TSkRoundRectCorner.LowerLeft]  := 5;
  LRoundRect := TSkRoundRect.Create(RectF(100, 20, 130, 220), LRadii);
  Assert.IsNotNull(LRoundRect);
end;

procedure TSkRoundRectTests.TestDefaultValue;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  Assert.AreEqual(0.0, LRoundRect.Width, TEpsilon.Vector);
  Assert.AreEqual(0.0, LRoundRect.Height, TEpsilon.Vector);
  Assert.IsTrue(TRectF.Empty.EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
  Assert.IsTrue(PointF(0, 0).EqualsTo(LRoundRect.Radii[TSkRoundRectCorner.UpperLeft], TEpsilon.Vector));
  Assert.IsTrue(PointF(0, 0).EqualsTo(LRoundRect.Radii[TSkRoundRectCorner.UpperRight], TEpsilon.Vector));
  Assert.IsTrue(PointF(0, 0).EqualsTo(LRoundRect.Radii[TSkRoundRectCorner.LowerRight], TEpsilon.Vector));
  Assert.IsTrue(PointF(0, 0).EqualsTo(LRoundRect.Radii[TSkRoundRectCorner.LowerLeft], TEpsilon.Vector));
end;

procedure TSkRoundRectTests.TestDraw(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
const
  Radii: array[0..3] of TPointF = ((X: 0; Y: 20), (X: 10; Y: 10), (X: 10; Y: 20), (X: 10; Y: 40));
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
  LStyle: TSkPaintStyle;
  I: Integer;
begin
  LSurface := TSkSurface.MakeRaster(256, 256);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LPaint := TSkPaint.Create;
  LPaint.StrokeWidth := 15;
  LPaint.StrokeJoin := TSkStrokeJoin.Round;
  LPaint.AntiAlias := True;
  for LStyle in TArray<TSkPaintStyle>.Create(TSkPaintStyle.Stroke, TSkPaintStyle.Fill) do
  begin
    LPaint.Style := LStyle;
    for I := 0 to Length(Radii) - 1 do
    begin
      LSurface.Canvas.DrawRoundRect(RectF(10, 10, 60, 40), Radii[I].X, Radii[I].Y, LPaint);
      LSurface.Canvas.Translate(0, 60);
    end;
    LSurface.Canvas.Translate(80, -240);
  end;

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkRoundRectTests.TestInflate;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect.SetRect(RectF(100, 20, 130, 220), 2, 5);
  Assert.IsTrue(RectF(100, 20, 130, 220).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
  LRoundRect.Inflate(1, 1);
  Assert.IsTrue(RectF(99, 19, 131, 221).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
end;

procedure TSkRoundRectTests.TestNinePatch;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect.SetRect(RectF(100, 20, 130, 220), 2, 5);
  Assert.IsTrue(RectF(100, 20, 130, 220).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
  Assert.IsTrue(LRoundRect.SimpleRadii.EqualsTo(PointF(2, 5), TEpsilon.Vector));

  LRoundRect.SetNinePatch(RectF(20, 53, 26, 435), 1, 5, 10, 15);
  Assert.IsTrue(RectF(20, 53, 26, 435).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
  LRoundRect.SetEmpty;
end;

procedure TSkRoundRectTests.TestOval;
var
  LOval: ISkRoundRect;
begin
  LOval := TSkRoundRect.Create;
  Assert.IsNotNull(LOval);
  LOval.SetOval(RectF(100, 20, 130, 220));
end;

procedure TSkRoundRectTests.TestSetRect;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect.SetRect(RectF(100, 20, 130, 220), 2, 5);
  Assert.IsTrue(RectF(100, 20, 130, 220).EqualsTo(LRoundRect.Rect, TEpsilon.Vector));
  Assert.IsTrue(LRoundRect.SimpleRadii.EqualsTo(PointF(2, 5), TEpsilon.Vector));
end;

procedure TSkRoundRectTests.TestTransform;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Assert.IsNotNull(LRoundRect);
  LRoundRect.SetRect(RectF(100, 20, 130, 220), 2, 5);
  Assert.IsNotNull(LRoundRect.Transform(TMatrix.CreateScaling(2, 1.2)));
end;

initialization
  TDUnitX.RegisterTestFixture(TSkRoundRectTests);
end.
