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
unit Skia.Tests.Canvas;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkCanvasTests }

  [TestFixture]
  TSkCanvasTests = class(TTestBase)
  strict private
    FBuffer: TBytes;
    FPixmap: ISkPixmap;
    function ColorAt(const AX, AY: Integer): TAlphaColor;
    function CountPaintedPixels: Integer;
    function CreateSurface(const ASize: Integer = 100): ISkSurface;
    function FillPaint(const AColor: TAlphaColor): ISkPaint;
  public
    [Test]
    procedure TestClipPath;
    [Test]
    procedure TestClipRegion;
    [Test]
    procedure TestClipShader;
    [Test]
    procedure TestConcatAndSetMatrix;
    [Test]
    procedure TestDrawArc;
    [Test]
    procedure TestDrawAtlas;
    [Test]
    procedure TestDrawAnnotation;
    [Test]
    procedure TestDrawColor;
    [Test]
    procedure TestDrawPatch;
    [Test]
    procedure TestDrawGlyphs;
    [Test]
    procedure TestDrawImageNine;
    [Test]
    procedure TestDrawLineAndPoints;
    [Test]
    procedure TestDrawOval;
    [Test]
    procedure TestDrawRoundRectDifference;
    [Test]
    procedure TestLocalAndDeviceClipBounds;
    [Test]
    procedure TestMakeSurface;
    [Test]
    procedure TestQuickReject;
    [Test]
    procedure TestRotate;
    [Test]
    procedure TestSaveCountAndRestoreToCount;
    [Test]
    procedure TestSaveLayerAlpha;
    [Test]
    procedure TestDiscardAndLocalToDevice;
    // TODO: Investigate possible issue.
    // [Test]
    // procedure TestMatrix3D;
    [Test]
    procedure TestSurfaceProperties;
    [Test]
    procedure TestTranslateAndScale;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors;

{ TSkCanvasTests }

function TSkCanvasTests.ColorAt(const AX, AY: Integer): TAlphaColor;
begin
  Result := FPixmap.Colors[AX, AY];
end;

function TSkCanvasTests.CountPaintedPixels: Integer;
var
  X: Integer;
  Y: Integer;
begin
  Result := 0;
  for Y := 0 to FPixmap.Height - 1 do
    for X := 0 to FPixmap.Width - 1 do
      if FPixmap.Colors[X, Y] <> TAlphaColors.Null then
        Inc(Result);
end;

function TSkCanvasTests.CreateSurface(const ASize: Integer): ISkSurface;
begin
  SetLength(FBuffer, ASize * ASize * 4);
  FPixmap := TSkPixmap.Create(TSkImageInfo.Create(ASize, ASize, TSkColorType.BGRA8888,
    TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), FBuffer, ASize * 4);
  Result := TSkSurface.MakeRasterDirect(FPixmap);
  Assert.IsNotNull(Result, 'Invalid ISkSurface (nil)');
  Result.Canvas.Clear(TAlphaColors.Null);
end;

function TSkCanvasTests.FillPaint(const AColor: TAlphaColor): ISkPaint;
begin
  Result := TSkPaint.Create;
  Result.Color := AColor;
end;

procedure TSkCanvasTests.TestClipPath;
var
  LPathBuilder: ISkPathBuilder;
  LSurface: ISkSurface;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(0, 0, 50, 50));

  LSurface := CreateSurface;
  LSurface.Canvas.ClipPath(LPathBuilder.Detach);
  LSurface.Canvas.DrawPaint(FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The clipped area should be painted');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(80, 80), 0, 'The area outside the clip should stay empty');
end;

procedure TSkCanvasTests.TestClipRegion;
var
  LRegion: ISkRegion;
  LSurface: ISkSurface;
begin
  LRegion := TSkRegion.Create(Rect(0, 0, 50, 50));
  LSurface := CreateSurface;
  LSurface.Canvas.ClipRegion(LRegion);
  LSurface.Canvas.DrawPaint(FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The region should be painted');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(80, 80), 0, 'The area outside the region should stay empty');
end;

procedure TSkCanvasTests.TestClipShader;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.ClipShader(TSkShader.MakeGradientLinear(PointF(0, 0), PointF(100, 0),
    TAlphaColors.White, TAlphaColors.Null));
  LSurface.Canvas.DrawPaint(FillPaint(TAlphaColors.Red));
  Assert.IsTrue(TAlphaColorRec(ColorAt(2, 50)).A > TAlphaColorRec(ColorAt(97, 50)).A,
    'The clip shader alpha should fade the drawing from left to right');
end;

procedure TSkCanvasTests.TestConcatAndSetMatrix;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.Concat(TMatrix.CreateTranslation(50, 0));
  LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(60, 10), 0, 'Concat should translate the drawing');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(10, 10), 0, 'Nothing should be drawn at the original position');
  Assert.AreEqual(50.0, LSurface.Canvas.GetLocalToDeviceAs3x3.m31, TEpsilon.Position, 'The matrix should hold the translation');

  LSurface.Canvas.SetMatrix(TMatrix.CreateTranslation(0, 50));
  Assert.AreEqual(0.0, LSurface.Canvas.GetLocalToDeviceAs3x3.m31, TEpsilon.Position, 'SetMatrix should replace the matrix');
  LSurface.Canvas.ResetMatrix;
  Assert.AreSameMatrix(TMatrix.Identity, LSurface.Canvas.GetLocalToDeviceAs3x3, TEpsilon.Matrix, 'ResetMatrix should restore the identity');
end;

procedure TSkCanvasTests.TestDrawArc;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.DrawArc(RectF(0, 0, 100, 100), 0, 90, True, FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(60, 60), 0, 'The 0..90 degrees pie should cover the lower right quadrant');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(20, 20), 0, 'The upper left quadrant should stay empty');
end;

procedure TSkCanvasTests.TestDrawAtlas;
var
  LAtlas: ISkImage;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface(20);
  LSurface.Canvas.Clear(TAlphaColors.Red);
  LAtlas := LSurface.MakeImageSnapshot;

  LSurface := CreateSurface;
  LSurface.Canvas.DrawAtlas(LAtlas, [TSkRotationScaleMatrix.Identity],
    [RectF(0, 0, 20, 20)], TSkBlendMode.SrcOver);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The sprite should be drawn at the origin');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(50, 50), 0, 'Nothing should be drawn outside the sprite');
end;

// TODO: Investigate possible issue.
//
// The TMatrix3D overloads of Concat, SetMatrix and GetLocalToDevice hand the
// record straight to Skia, which reads and writes a SkM44 - the transpose of
// the Delphi convention. A TMatrix3D.CreateTranslation therefore lands on the
// perspective row and draws nothing, while the TMatrix overloads of the same
// methods do follow the Delphi convention.
//
// procedure TSkCanvasTests.TestMatrix3D;
// var
//   LSurface: ISkSurface;
// begin
//   LSurface := CreateSurface;
//   LSurface.Canvas.SetMatrix(TMatrix3D.CreateTranslation(Point3D(40, 40, 0)));
//   LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
//   Assert.AreSameColor(TAlphaColors.Red, ColorAt(45, 45), 0, 'The 4x4 matrix should translate the drawing');
//   Assert.AreEqual(40.0, LSurface.Canvas.GetLocalToDevice.m41, TEpsilon.Position, 'The 4x4 matrix should hold the X translation');
// end;

procedure TSkCanvasTests.TestDiscardAndLocalToDevice;
var
  LMatrix: TMatrix;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.Translate(10, 20);
  LMatrix := LSurface.Canvas.GetLocalToDeviceAs3x3;
  Assert.AreEqual(10.0, LMatrix.m31, TEpsilon.Position, 'The matrix should hold the X translation');
  Assert.AreEqual(20.0, LMatrix.m32, TEpsilon.Position, 'The matrix should hold the Y translation');

  // Discard only tells the canvas that its content may be dropped; on a raster
  // surface it must at least be harmless.
  LSurface.Canvas.Discard;
  LSurface.Canvas.ResetMatrix;
  LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The canvas should still be usable after Discard');
end;

procedure TSkCanvasTests.TestDrawAnnotation;
var
  LData: TBytes;
  LPictureRecorder: ISkPictureRecorder;
  LSurface: ISkSurface;
begin
  // Annotations are metadata for the recording backends: they must not change
  // what a raster canvas paints.
  LSurface := CreateSurface;
  LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  LSurface.Canvas.DrawAnnotation(RectF(0, 0, 20, 20), 'org.skia.test');
  LData := TEncoding.UTF8.GetBytes('https://skia4delphi.org');
  LSurface.Canvas.DrawAnnotation(RectF(0, 0, 20, 20), 'org.skia.test.value', LData[0], Length(LData));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The drawing should not be changed by the annotations');

  LPictureRecorder := TSkPictureRecorder.Create;
  LPictureRecorder.BeginRecording(100, 100).DrawAnnotation(RectF(0, 0, 20, 20), 'org.skia.test');
  Assert.IsNotNull(LPictureRecorder.FinishRecording, 'A recorded annotation should produce a picture');
end;

procedure TSkCanvasTests.TestDrawPatch;
const
  Cubics: TSkPatchCubics = ((X: 0; Y: 0), (X: 33; Y: 0), (X: 66; Y: 0), (X: 100; Y: 0),
                            (X: 100; Y: 33), (X: 100; Y: 66), (X: 100; Y: 100),
                            (X: 66; Y: 100), (X: 33; Y: 100), (X: 0; Y: 100),
                            (X: 0; Y: 66), (X: 0; Y: 33));
  Colors: TSkPatchColors = (TAlphaColors.Red, TAlphaColors.Red, TAlphaColors.Red, TAlphaColors.Red);
var
  LSurface: ISkSurface;
  LTexCoords: TSkPatchTexCoords;
begin
  LTexCoords[0] := PointF(0, 0);
  LTexCoords[1] := PointF(100, 0);
  LTexCoords[2] := PointF(100, 100);
  LTexCoords[3] := PointF(0, 100);

  LSurface := CreateSurface;
  LSurface.Canvas.DrawPatch(Cubics, Colors, LTexCoords, TSkBlendMode.Dest, FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(50, 50), 2, 'The patch should fill its area');
end;

procedure TSkCanvasTests.TestDrawColor;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.DrawColor(TAlphaColors.Red);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(50, 50), 0, 'DrawColor should fill the canvas');

  LSurface.Canvas.DrawColor(TAlphaColors.Lime, TSkBlendMode.Src);
  Assert.AreSameColor(TAlphaColors.Lime, ColorAt(50, 50), 0, 'The Src blend mode should replace the content');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawColor(TAlphaColorF.Create(0, 0, 1, 1));
  Assert.AreSameColor(TAlphaColors.Blue, ColorAt(50, 50), 2, 'The float overload should fill the canvas');
end;

procedure TSkCanvasTests.TestDrawGlyphs;
var
  LFont: ISkFont;
  LGlyphs: TArray<Word>;
  LPainted: Integer;
  LSurface: ISkSurface;
  LTypeface: ISkTypeface;
begin
  LTypeface := TSkTypeface.MakeFromFile(FontAssetsPath + 'segoeui.ttf');
  Assert.IsNotNull(LTypeface, 'Invalid ISkTypeface (nil)');
  LFont := TSkFont.Create(LTypeface, 60);
  LGlyphs := LFont.GetGlyphs('I');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawGlyphs(LGlyphs, LFont.GetPositions(LGlyphs), PointF(10, 70), LFont, FillPaint(TAlphaColors.Red));
  LPainted := CountPaintedPixels;
  Assert.IsTrue(LPainted > 0, 'DrawGlyphs should draw the glyph');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawSimpleTextGlyphs(LGlyphs, 10, 70, LFont, FillPaint(TAlphaColors.Red));
  Assert.AreEqual(LPainted, CountPaintedPixels, 'DrawGlyphs and DrawSimpleTextGlyphs should draw the same glyph');
end;

procedure TSkCanvasTests.TestDrawImageNine;
var
  LImage: ISkImage;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface(30);
  LSurface.Canvas.Clear(TAlphaColors.Red);
  LSurface.Canvas.DrawRect(RectF(10, 10, 20, 20), FillPaint(TAlphaColors.Blue));
  LImage := LSurface.MakeImageSnapshot;

  LSurface := CreateSurface;
  LSurface.Canvas.DrawImageNine(LImage, Rect(10, 10, 20, 20), RectF(0, 0, 100, 100), TSkFilterMode.Nearest);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(2, 2), 0, 'The corner should keep its size and color');
  Assert.AreSameColor(TAlphaColors.Blue, ColorAt(50, 50), 0, 'The center should be stretched');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawImageLattice(LImage, TSkLattice.Create([10, 20], [10, 20]), RectF(0, 0, 100, 100), TSkFilterMode.Nearest);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(2, 2), 0, '(lattice corner)');
  Assert.AreSameColor(TAlphaColors.Blue, ColorAt(50, 50), 0, '(lattice center)');
end;

procedure TSkCanvasTests.TestDrawLineAndPoints;
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LPaint := FillPaint(TAlphaColors.Red);
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 4;

  LSurface := CreateSurface;
  LSurface.Canvas.DrawLine(0, 50, 100, 50, LPaint);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(50, 50), 0, 'The line should be drawn');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(50, 10), 0, 'Nothing should be drawn away from the line');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawPoint(20, 20, LPaint);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(20, 20), 0, 'The point should be drawn');

  LSurface := CreateSurface;
  LSurface.Canvas.DrawPoints(TSkDrawPointsMode.Polygon, [PointF(10, 10), PointF(90, 10)], LPaint);
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(50, 10), 0, 'The polygon should connect the points');
end;

procedure TSkCanvasTests.TestDrawOval;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.DrawOval(RectF(0, 0, 100, 100), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(50, 50), 0, 'The center of the oval should be painted');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(2, 2), 0, 'The corner should stay outside the oval');
end;

procedure TSkCanvasTests.TestDrawRoundRectDifference;
var
  LInner: ISkRoundRect;
  LOuter: ISkRoundRect;
  LSurface: ISkSurface;
begin
  LOuter := TSkRoundRect.Create(RectF(0, 0, 100, 100), 0, 0);
  LInner := TSkRoundRect.Create(RectF(25, 25, 75, 75), 0, 0);
  LSurface := CreateSurface;
  LSurface.Canvas.DrawRoundRectDifference(LOuter, LInner, FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 50), 0, 'The ring should be painted');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(50, 50), 0, 'The hole should stay empty');
end;

procedure TSkCanvasTests.TestLocalAndDeviceClipBounds;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  Assert.AreEqual<TRect>(Rect(0, 0, 100, 100), LSurface.Canvas.GetDeviceClipBounds, 'The device clip should be the whole surface');
  Assert.AreSameRect(RectF(0, 0, 100, 100), LSurface.Canvas.GetLocalClipBounds, 1,
    'The local clip is conservative, so it may be one pixel larger than the surface');

  LSurface.Canvas.ClipRect(RectF(10, 20, 60, 70));
  Assert.AreEqual<TRect>(Rect(10, 20, 60, 70), LSurface.Canvas.GetDeviceClipBounds, 'The device clip should follow ClipRect');

  LSurface.Canvas.Translate(10, 10);
  Assert.AreSameRect(RectF(0, 10, 50, 60), LSurface.Canvas.GetLocalClipBounds, 1,
    'The local clip should be expressed in the current coordinate system');
end;

procedure TSkCanvasTests.TestMakeSurface;
var
  LOther: ISkSurface;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LOther := LSurface.Canvas.MakeSurface(TSkImageInfo.Create(40, 30));
  Assert.IsNotNull(LOther, 'Invalid ISkSurface (nil)');
  LOther.Canvas.Clear(TAlphaColors.Red);
  Assert.AreEqual(40, LOther.MakeImageSnapshot.Width, '(Width)');
  Assert.AreEqual(30, LOther.MakeImageSnapshot.Height, '(Height)');
end;

procedure TSkCanvasTests.TestQuickReject;
var
  LPathBuilder: ISkPathBuilder;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.ClipRect(RectF(0, 0, 50, 50));
  Assert.IsTrue(LSurface.Canvas.QuickReject(RectF(200, 200, 300, 300)), 'A rect outside the clip should be rejected');
  Assert.IsFalse(LSurface.Canvas.QuickReject(RectF(10, 10, 20, 20)), 'A rect inside the clip should not be rejected');

  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddRect(RectF(200, 200, 300, 300));
  Assert.IsTrue(LSurface.Canvas.QuickReject(LPathBuilder.Detach), 'A path outside the clip should be rejected');
end;

procedure TSkCanvasTests.TestRotate;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.Translate(50, 50);
  LSurface.Canvas.Rotate(90);
  LSurface.Canvas.DrawRect(RectF(0, 0, 40, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(40, 60), 0, 'A 90 degrees rotation maps (x, y) to (-y, x)');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(60, 60), 0, 'Nothing should be drawn on the unrotated side');

  LSurface := CreateSurface;
  LSurface.Canvas.Rotate(90, 50, 50);
  LSurface.Canvas.DrawRect(RectF(50, 50, 90, 70), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(40, 60), 0, 'The rotation should happen around the given point');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(60, 60), 0, 'The source rect position should be empty after the rotation');

  LSurface := CreateSurface;
  LSurface.Canvas.Translate(50, 50);
  LSurface.Canvas.RotateRadians(Pi / 2);
  LSurface.Canvas.DrawRect(RectF(0, 0, 40, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(40, 60), 0, 'RotateRadians should match Rotate');
end;

procedure TSkCanvasTests.TestSaveCountAndRestoreToCount;
var
  LCount: Integer;
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  Assert.AreEqual(1, LSurface.Canvas.GetSaveCount, 'A new canvas starts with a save count of one');
  LCount := LSurface.Canvas.Save;
  Assert.AreEqual(1, LCount, 'Save should return the previous count');
  Assert.AreEqual(2, LSurface.Canvas.GetSaveCount, 'Save should increase the count');
  LSurface.Canvas.Save;
  LSurface.Canvas.Save;
  Assert.AreEqual(4, LSurface.Canvas.GetSaveCount);

  LSurface.Canvas.Translate(10, 10);
  LSurface.Canvas.RestoreToCount(LCount);
  Assert.AreEqual(1, LSurface.Canvas.GetSaveCount, 'RestoreToCount should unwind every save');
  Assert.AreSameMatrix(TMatrix.Identity, LSurface.Canvas.GetLocalToDeviceAs3x3, TEpsilon.Matrix, 'The matrix should be restored');
end;

procedure TSkCanvasTests.TestSaveLayerAlpha;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.SaveLayerAlpha($80);
  try
    LSurface.Canvas.DrawPaint(FillPaint(TAlphaColors.Red));
  finally
    LSurface.Canvas.Restore;
  end;
  Assert.AreSameColor($80FF0000, ColorAt(50, 50), 2, 'The layer alpha should be applied when the layer is restored');

  LSurface := CreateSurface;
  LSurface.Canvas.SaveLayerAlpha(RectF(0, 0, 50, 50), $FF);
  try
    LSurface.Canvas.DrawPaint(FillPaint(TAlphaColors.Red));
  finally
    LSurface.Canvas.Restore;
  end;
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(10, 10), 0, 'The layer bounds should be painted');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(80, 80), 0, 'The layer should clip the drawing');
end;

procedure TSkCanvasTests.TestSurfaceProperties;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  Assert.IsTrue(LSurface.Canvas.BaseProperties = LSurface.Properties, 'The base properties should match the surface ones');
  Assert.IsTrue(LSurface.Canvas.TopProperties = LSurface.Properties, 'Without a layer the top properties should match the surface ones');
end;

procedure TSkCanvasTests.TestTranslateAndScale;
var
  LSurface: ISkSurface;
begin
  LSurface := CreateSurface;
  LSurface.Canvas.Save;
  try
    LSurface.Canvas.Translate(50, 50);
    LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  finally
    LSurface.Canvas.Restore;
  end;
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(60, 60), 0, 'Translate should move the drawing');
  Assert.AreSameColor(TAlphaColors.Null, ColorAt(10, 10), 0, 'The origin should stay empty');

  LSurface := CreateSurface;
  LSurface.Canvas.Scale(4, 4);
  LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(70, 70), 0, 'Scale should enlarge the drawing');

  LSurface := CreateSurface;
  LSurface.Canvas.Skew(1, 0);
  LSurface.Canvas.DrawRect(RectF(0, 0, 20, 20), FillPaint(TAlphaColors.Red));
  Assert.AreSameColor(TAlphaColors.Red, ColorAt(25, 15), 0, 'Skew should shear the drawing');
end;

initialization
  TDUnitX.RegisterTestFixture(TSkCanvasTests);
end.
