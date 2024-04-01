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
    [TestCase('1', 'horse.webp,0.99,/8+BgYGR0f////Hhw9fd////9fX73/3///////vf/f/AAYABgAGAAYABgAGAAYADwAPgB/AP//8')]
    procedure TestClipImage(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestConstructors;
    [Test]
    procedure TestDefaultValue;
    [TestCase('1', '0.99,Jy8nPyc/B///Lzc/Jz8H//+/N783vzf///9//z////8M/2x/j////wx/bH///4//bH8Mf/////8')]
    procedure TestDraw(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,/4GBgYGBw/////Hh4cHD////8eHhwfP//////+HD8//AA8ADwAPAA8AD4AfgB/gP+B/+f/////8')]
    procedure TestDrawCreatedWithRadii(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,/8OBgYGBw/////Hhw8fP////8eHDx+////////vf7//gB8ADwAPAA8ADwAPAA+AH4Af4H/////8')]
    procedure TestDrawCreatedWithSameRadius(const AMinSimilarity: Double; const AExpectedImageHash: string);
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
  System.Skia;

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

procedure TSkRoundRectTests.TestClipImage(const AImageFileName: string;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
const
  BitmapSize = 160;
  RectSize = BitmapSize - 20;
  Radius: TPointF = (X: RectSize/3; Y: RectSize/3);
var
  LDestRect: TRectF;
  LImage: ISkImage;
  LRoundRect: ISkRoundRect;
  LSurface: ISkSurface;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  Assert.IsNotNull(LImage, 'Invalid ISkImage (nil)');
  LSurface := TSkSurface.MakeRaster(BitmapSize, BitmapSize);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSurface.Canvas.Save;
  try
    LDestRect := RectF(0, 0, RectSize, RectSize);
    LRoundRect := TSkRoundRect.Create(RectCenter(LDestRect, RectF(0, 0, BitmapSize, BitmapSize)), Radius.X, Radius.Y);
    Assert.IsNotNull(LRoundRect, 'Invalid ISkRoundRect (nil)');
    LSurface.Canvas.ClipRoundRect(LRoundRect, TSkClipOp.Intersect, True);
    DrawImageFitCrop(LSurface.Canvas, LRoundRect.Rect, LImage);
  finally
   LSurface.Canvas.Restore;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
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
  LRadii[TSkRoundRectCorner.UpperLeft]  := PointF(2, 2);
  LRadii[TSkRoundRectCorner.UpperRight] := PointF(3, 3);
  LRadii[TSkRoundRectCorner.LowerRight] := PointF(4, 4);
  LRadii[TSkRoundRectCorner.LowerLeft]  := PointF(5, 5);
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

procedure TSkRoundRectTests.TestDrawCreatedWithRadii(
  const AMinSimilarity: Double; const AExpectedImageHash: string);
const
  BitmapSize = 80;
  RectSize = BitmapSize - 20;
  Radii: TSkRoundRectRadii = ((X: 0; Y: 0), (X: 0; Y: 0), (X: RectSize/2; Y: RectSize/2), (X: RectSize/2; Y: RectSize/2));
var
  LDestRect: TRectF;
  LPaint: ISkPaint;
  LRoundRect: ISkRoundRect;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(BitmapSize, BitmapSize);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LDestRect := RectF(0, 0, RectSize, RectSize);
  LRoundRect := TSkRoundRect.Create(RectCenter(LDestRect, RectF(0, 0, BitmapSize, BitmapSize)), Radii);
  Assert.IsNotNull(LRoundRect, 'Invalid ISkRoundRect (nil)');
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LSurface.Canvas.DrawRoundRect(LRoundRect, LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkRoundRectTests.TestDrawCreatedWithSameRadius(
  const AMinSimilarity: Double; const AExpectedImageHash: string);
const
  BitmapSize = 80;
  RectSize = BitmapSize - 20;
  Radius: TPointF = (X: RectSize/3; Y: RectSize/3);
var
  LDestRect: TRectF;
  LPaint: ISkPaint;
  LRoundRect: ISkRoundRect;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(BitmapSize, BitmapSize);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LDestRect := RectF(0, 0, RectSize, RectSize);
  LRoundRect := TSkRoundRect.Create(RectCenter(LDestRect, RectF(0, 0, BitmapSize, BitmapSize)), Radius.X, Radius.Y);
  Assert.IsNotNull(LRoundRect, 'Invalid ISkRoundRect (nil)');
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LSurface.Canvas.DrawRoundRect(LRoundRect, LPaint);
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
