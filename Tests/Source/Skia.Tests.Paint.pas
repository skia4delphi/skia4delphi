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
unit Skia.Tests.Paint;

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
  { TSkPaintTests }

  [TestFixture]
  TSkPaintTests = class(TTestBase)
  public
    [Test]
    procedure TestAlphaKeepsColorChannels;
    [Test]
    procedure TestColorF;
    [Test]
    procedure TestColorProperties;
    [Test]
    procedure TestCopyConstructor;
    [Test]
    procedure TestDefaultValues;
    [Test]
    procedure TestEffectProperties;
    [Test]
    procedure TestGetFillPathOfFillStyleIsTheSourcePath;
    [Test]
    procedure TestGetFillPathOfStroke;
    [Test]
    procedure TestGetFillPathWithCullRect;
    [Test]
    procedure TestReset;
    [Test]
    procedure TestStrokeProperties;
    [Test]
    procedure TestStyleConstructor;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Types,
  System.UITypes;

{ TSkPaintTests }

procedure TSkPaintTests.TestAlphaKeepsColorChannels;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Red;
  LPaint.Alpha := $80;
  Assert.AreEqual<TAlphaColor>($80FF0000, LPaint.Color, 'Setting the alpha should keep the color channels');
  Assert.AreEqual($80, Integer(LPaint.Alpha));

  LPaint.AlphaF := 1;
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LPaint.Color);
  Assert.AreEqual(1.0, LPaint.AlphaF, TEpsilon.Vector);
end;

procedure TSkPaintTests.TestColorF;
var
  LColor: TAlphaColorF;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.SetColorF(TAlphaColorF.Create(0.25, 0.5, 0.75, 0.5));
  LColor := LPaint.GetColorF;
  Assert.AreEqual(0.25, LColor.R, TEpsilon.Vector, '(R)');
  Assert.AreEqual(0.5, LColor.G, TEpsilon.Vector, '(G)');
  Assert.AreEqual(0.75, LColor.B, TEpsilon.Vector, '(B)');
  Assert.AreEqual(0.5, LColor.A, TEpsilon.Vector, '(A)');
  Assert.AreEqual(0.5, LPaint.AlphaF, TEpsilon.Vector, 'AlphaF should follow ColorF');

  LPaint.Color := TAlphaColors.Lime;
  LColor := LPaint.GetColorF;
  Assert.AreEqual(0.0, LColor.R, TEpsilon.Vector, '(R of lime)');
  Assert.AreEqual(1.0, LColor.G, TEpsilon.Vector, '(G of lime)');
  Assert.AreEqual(0.0, LColor.B, TEpsilon.Vector, '(B of lime)');
  Assert.AreEqual(1.0, LColor.A, TEpsilon.Vector, '(A of lime)');
end;

procedure TSkPaintTests.TestColorProperties;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := TAlphaColors.Blue;
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Blue, LPaint.Color);

  LPaint.SetARGB($40, $10, $20, $30);
  Assert.AreEqual<TAlphaColor>($40102030, LPaint.Color);
  Assert.AreEqual($40, Integer(LPaint.Alpha));
end;

procedure TSkPaintTests.TestCopyConstructor;
var
  LCopy: ISkPaint;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := TAlphaColors.Red;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 7;
  LPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 2);

  LCopy := TSkPaint.Create(LPaint);
  Assert.IsNotNull(LCopy);
  Assert.IsTrue(LCopy.AntiAlias, '(AntiAlias)');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Red, LCopy.Color, '(Color)');
  Assert.IsTrue(LCopy.Style = TSkPaintStyle.Stroke, '(Style)');
  Assert.AreEqual(7.0, LCopy.StrokeWidth, TEpsilon.Vector, '(StrokeWidth)');
  Assert.IsNotNull(LCopy.MaskFilter, '(MaskFilter)');

  LCopy.StrokeWidth := 9;
  Assert.AreEqual(7.0, LPaint.StrokeWidth, TEpsilon.Vector, 'Changing the copy should not change the source');
end;

procedure TSkPaintTests.TestDefaultValues;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  Assert.IsNotNull(LPaint);
  Assert.IsFalse(LPaint.AntiAlias, '(AntiAlias)');
  Assert.IsFalse(LPaint.Dither, '(Dither)');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Black, LPaint.Color, '(Color)');
  Assert.AreEqual($FF, Integer(LPaint.Alpha), '(Alpha)');
  Assert.AreEqual(1.0, LPaint.AlphaF, TEpsilon.Vector, '(AlphaF)');
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.Fill, '(Style)');
  Assert.AreEqual(0.0, LPaint.StrokeWidth, TEpsilon.Vector, '(StrokeWidth)');
  Assert.AreEqual(4.0, LPaint.StrokeMiter, TEpsilon.Vector, '(StrokeMiter)');
  Assert.IsTrue(LPaint.StrokeCap = TSkStrokeCap.Butt, '(StrokeCap)');
  Assert.IsTrue(LPaint.StrokeJoin = TSkStrokeJoin.Miter, '(StrokeJoin)');
  Assert.IsNull(LPaint.Blender, '(Blender)');
  Assert.IsNull(LPaint.ColorFilter, '(ColorFilter)');
  Assert.IsNull(LPaint.ImageFilter, '(ImageFilter)');
  Assert.IsNull(LPaint.MaskFilter, '(MaskFilter)');
  Assert.IsNull(LPaint.PathEffect, '(PathEffect)');
  Assert.IsNull(LPaint.Shader, '(Shader)');
end;

procedure TSkPaintTests.TestEffectProperties;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;

  LPaint.Blender := TSkBlender.MakeMode(TSkBlendMode.Multiply);
  Assert.IsNotNull(LPaint.Blender, '(Blender)');
  LPaint.ColorFilter := TSkColorFilter.MakeBlend(TAlphaColors.Red, TSkBlendMode.SrcIn);
  Assert.IsNotNull(LPaint.ColorFilter, '(ColorFilter)');
  LPaint.ImageFilter := TSkImageFilter.MakeBlur(2, 2);
  Assert.IsNotNull(LPaint.ImageFilter, '(ImageFilter)');
  LPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurStyle.Normal, 2);
  Assert.IsNotNull(LPaint.MaskFilter, '(MaskFilter)');
  LPaint.PathEffect := TSkPathEffect.MakeCorner(4);
  Assert.IsNotNull(LPaint.PathEffect, '(PathEffect)');
  LPaint.Shader := TSkShader.MakeColor(TAlphaColors.Red);
  Assert.IsNotNull(LPaint.Shader, '(Shader)');

  LPaint.Blender := nil;
  Assert.IsNull(LPaint.Blender, '(Blender cleared)');
  LPaint.ColorFilter := nil;
  Assert.IsNull(LPaint.ColorFilter, '(ColorFilter cleared)');
  LPaint.ImageFilter := nil;
  Assert.IsNull(LPaint.ImageFilter, '(ImageFilter cleared)');
  LPaint.MaskFilter := nil;
  Assert.IsNull(LPaint.MaskFilter, '(MaskFilter cleared)');
  LPaint.PathEffect := nil;
  Assert.IsNull(LPaint.PathEffect, '(PathEffect cleared)');
  LPaint.Shader := nil;
  Assert.IsNull(LPaint.Shader, '(Shader cleared)');
end;

procedure TSkPaintTests.TestGetFillPathOfFillStyleIsTheSourcePath;
var
  LFillPath: ISkPath;
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
  LSource: ISkPath;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(10, 20, 110, 70));
  LSource := LPathBuilder.Detach;

  LPaint := TSkPaint.Create;
  LFillPath := LPaint.GetFillPath(LSource);
  Assert.IsNotNull(LFillPath, 'Invalid ISkPath (nil)');
  Assert.AreSameRect(LSource.Bounds, LFillPath.Bounds, TEpsilon.Position, 'A fill paint should not change the path');
end;

procedure TSkPaintTests.TestGetFillPathOfStroke;
var
  LFillPath: ISkPath;
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 50);
  LPathBuilder.LineTo(100, 50);

  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LPaint.StrokeWidth := 10;
  LPaint.StrokeCap := TSkStrokeCap.Butt;
  LFillPath := LPaint.GetFillPath(LPathBuilder.Detach);
  Assert.IsNotNull(LFillPath, 'Invalid ISkPath (nil)');
  Assert.AreSameRect(RectF(0, 45, 100, 55), LFillPath.Bounds, TEpsilon.Position, 'The stroke outline should be half the stroke width around the line');
end;

procedure TSkPaintTests.TestGetFillPathWithCullRect;
var
  LFillPath: ISkPath;
  LPaint: ISkPaint;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.MoveTo(0, 50);
  LPathBuilder.LineTo(1000, 50);

  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LPaint.StrokeWidth := 10;
  LFillPath := LPaint.GetFillPath(LPathBuilder.Detach, RectF(0, 0, 100, 100), 1);
  Assert.IsNotNull(LFillPath, 'Invalid ISkPath (nil)');
  Assert.IsFalse(LFillPath.IsEmpty, 'The culled fill path should not be empty');
end;

procedure TSkPaintTests.TestReset;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := TAlphaColors.Red;
  LPaint.Dither := True;
  LPaint.Style := TSkPaintStyle.StrokeAndFill;
  LPaint.StrokeCap := TSkStrokeCap.Round;
  LPaint.StrokeJoin := TSkStrokeJoin.Bevel;
  LPaint.StrokeMiter := 12;
  LPaint.StrokeWidth := 5;
  LPaint.Shader := TSkShader.MakeColor(TAlphaColors.Blue);
  LPaint.Reset;

  Assert.IsFalse(LPaint.AntiAlias, '(AntiAlias)');
  Assert.IsFalse(LPaint.Dither, '(Dither)');
  Assert.AreEqual<TAlphaColor>(TAlphaColors.Black, LPaint.Color, '(Color)');
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.Fill, '(Style)');
  Assert.IsTrue(LPaint.StrokeCap = TSkStrokeCap.Butt, '(StrokeCap)');
  Assert.IsTrue(LPaint.StrokeJoin = TSkStrokeJoin.Miter, '(StrokeJoin)');
  Assert.AreEqual(4.0, LPaint.StrokeMiter, TEpsilon.Vector, '(StrokeMiter)');
  Assert.AreEqual(0.0, LPaint.StrokeWidth, TEpsilon.Vector, '(StrokeWidth)');
  Assert.IsNull(LPaint.Shader, '(Shader)');
end;

procedure TSkPaintTests.TestStrokeProperties;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;

  LPaint.AntiAlias := True;
  Assert.IsTrue(LPaint.AntiAlias, '(AntiAlias)');
  LPaint.Dither := True;
  Assert.IsTrue(LPaint.Dither, '(Dither)');

  LPaint.Style := TSkPaintStyle.Stroke;
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.Stroke, '(Style Stroke)');
  LPaint.Style := TSkPaintStyle.StrokeAndFill;
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.StrokeAndFill, '(Style StrokeAndFill)');

  LPaint.StrokeCap := TSkStrokeCap.Round;
  Assert.IsTrue(LPaint.StrokeCap = TSkStrokeCap.Round, '(StrokeCap Round)');
  LPaint.StrokeCap := TSkStrokeCap.Square;
  Assert.IsTrue(LPaint.StrokeCap = TSkStrokeCap.Square, '(StrokeCap Square)');

  LPaint.StrokeJoin := TSkStrokeJoin.Round;
  Assert.IsTrue(LPaint.StrokeJoin = TSkStrokeJoin.Round, '(StrokeJoin Round)');
  LPaint.StrokeJoin := TSkStrokeJoin.Bevel;
  Assert.IsTrue(LPaint.StrokeJoin = TSkStrokeJoin.Bevel, '(StrokeJoin Bevel)');

  LPaint.StrokeMiter := 2.5;
  Assert.AreEqual(2.5, LPaint.StrokeMiter, TEpsilon.Vector, '(StrokeMiter)');
  LPaint.StrokeWidth := 3.25;
  Assert.AreEqual(3.25, LPaint.StrokeWidth, TEpsilon.Vector, '(StrokeWidth)');
end;

procedure TSkPaintTests.TestStyleConstructor;
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.Stroke);
  LPaint := TSkPaint.Create(TSkPaintStyle.StrokeAndFill);
  Assert.IsTrue(LPaint.Style = TSkPaintStyle.StrokeAndFill);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPaintTests);
end.
