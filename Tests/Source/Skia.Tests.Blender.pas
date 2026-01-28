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
unit Skia.Tests.Blender;

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
  { TSkBlenderTests }

  [TestFixture]
  TSkBlenderTests = class(TTestBase)
  public
    [TestCase('Clear',      '256,Clear,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('Src',        '256,Src,0.99,58PDsRgAAIH///PxW0dMyf//8/vfT8zN///z//9P7M/gB+AH4DfB/4H/g/+D/4P/g/+D/8H///8')]
    [TestCase('Dest',       '256,Dest,0.99,/8OBgY/v//////Hhz+///////eHP////////////////////9+/hh+AH4APgB+AH4A/wD/gf/n8')]
    [TestCase('SrcOver',    '256,SrcOver,0.99,58PDsRgAAIH///Px28bMyf//8/vfzszN///z///P7M/gB+AH4DfB/4H/g/+D/4P/g/+D/8H///8')]
    [TestCase('DestOver',   '256,DestOver,0.99,58OBgTw8GIH///Hh//7cyf///eP//9/N///97////+/v9//37+fPg88BzgHEAcADgAOAB8Af//8')]
    [TestCase('SrcIn',      '256,SrcIn,0.99,/8PDgYGBw/////Phw8fP//////vPz8/////////P7//gB+AH4Afhj/HP89/7//v///////////8')]
    [TestCase('DestIn',     '256,DestIn,0.99,/8OBgYnn//////Hhy+f//////ePP9///////////////////9+/hh+AH4APgB+AH4A/wD/gf//8')]
    [TestCase('SrcOut',     '256,SrcOut,0.99,w9v//3w8GIH/////f39cyf////9/f97N//////9//s/n5+/3///////9//z//P/8//i/8d+D/4c')]
    [TestCase('DestOut',    '256,DestOut,0.99,/708PP///////Xx8/////////vz////////+/P////8ABgAHAA8AAwABgAAAAAAAAAAAAAAAAAA')]
    [TestCase('SrcATop',    '256,SrcATop,0.99,/8OBgYGBw/////Hhw8fP//////vPz8/////////P7//gB+AHwAfhj/HP8//7//v///////////8')]
    [TestCase('DestATop',   '256,DestATop,0.99,58PDgTw8GIH///Phf39cyf///+N/f1/N////7/9//+/39//37+fPg88BzgHEAcABgAOAB8Af//8')]
    [TestCase('&Xor',       '256,Xor,0.99,59u9/3w8GIH///3///7cyf///////97N/////////s/n5+/33/f//f////7//P/8//i/8d+D/wc')]
    [TestCase('Plus',       '256,Plus,0.99,58OBvTw8GIH///H9//7cyf///////9/N/////////8/jx+eH7/f//fgf+H/wP8A/g/+B/8H///8')]
    [TestCase('Modulate',   '256,Modulate,0.99,/8PDgYGBw/////Phw8fP////8+nP9//////////////8P+gX4Afhh/AP8A/4H/wf/v////////8')]
    [TestCase('Screen',     '256,Screen,0.99,58OBrTw8GIH///Ht//7cyf///////9/N/////////8/jx+eH7/f//f5/+H/wP8A/g/+B/8H///8')]
    [TestCase('Overlay',    '256,Overlay,0.99,58OBgTw8GIH///Hh//7cyf//+eP//9/N///57////8//x//H7+f/j98HzgHCAcIBgAOAD8B/+/8')]
    [TestCase('Darken',     '256,Darken,0.99,58PDgZgAgNH///Ph28bM2f//8+vf9vz9///z7////P/gB+AH4OfBh4EPgg+DH4M/g/+D/8H///8')]
    [TestCase('Lighten',    '256,Lighten,0.99,58OBgTw8GIH///Hh//7cyf////P//9/N/////////8/jx+eHz/f//f//+H/wP8A/g/+B/8H///8')]
    [TestCase('ColorDodge', '256,ColorDodge,0.99,58OBgTw8GIH///Hh//7cyf///fP//9/N///9/////+/n9//37+fPg88BzwHHAcMBgQOBB8Af//8')]
    [TestCase('ColorBurn',  '256,ColorBurn,0.99,58ODgRg8OJH///Ph2/782f//8+Xf///d///z7////9/vB/8H////j/4P+A/gB8ADgAeAD8A/+/8')]
    [TestCase('HardLight',  '256,HardLight,0.99,58ODgTgAAIH///Ph+8bMyf//8/v/zszN///z///P7M/gB+AH4H/B/4B/gH+D/4P/g/+D/8H///8')]
    [TestCase('SoftLight',  '256,SoftLight,0.99,58OBgTw8GIH///Hh//7cyf//+eP//9/N///57////+//5//n7+ffh88FzgHCAcIBgAOAB8B/+/8')]
    [TestCase('Difference', '256,Difference,0.99,58OJgSQ8GIH///nh5/7cyf////f3/9/N//////f//8/jx+eHz/f+ff7//f/w/8D/gf+B/8H///8')]
    [TestCase('Exclusion',  '256,Exclusion,0.99,58OBpTQ8GIH///Hl9/7cyf////f3/9/N//////f//8/jx+eHz/f+ff7//f/w/8D/gf+B/8H///8')]
    [TestCase('Multiply',   '256,Multiply,0.99,58PDgZgAgNH///Ph28bM2f//8+nf9vz9///z7////P/gB+AH4OfBj4EPgg+DH4M/g/+D/8H///8')]
    [TestCase('Hue',        '256,Hue,0.99,58OBgTw8AIH///Hh//7Myf///+P//8/N////7///78/gB+AH4BfBvZH/i/+D/4P/w/+H/8H///8')]
    [TestCase('Saturation', '256,Saturation,0.99,58OBgTw8GIH///Hh//7cyf///+P//9/N////7////8/j5+f37+ffh94FzAHPAcIDgAOAD8B///8')]
    [TestCase('Color',      '256,Color,0.99,58OBgTw8AIH///Hh//7Myf///+P//8/N////7///78/gB+AH4DfBPZH/i/+D/4P/w/+H/8H///8')]
    [TestCase('Luminosity', '256,Luminosity,0.99,58PDsTgYAIH///Px+97Myf//8/v/387N///z///f7+/n9+/37+fPg8cDwQPgA4AHgA+AH8F///8')]
    procedure TestModes(const ASize: Integer; const ABlendMode: TSkBlendMode; const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkBlenderTests }

procedure TSkBlenderTests.TestModes(const ASize: Integer;
  const ABlendMode: TSkBlendMode; const AMinSimilarity: Double;
  const AExpectedImageHash: string);

  procedure DrawBackground(const ACanvas: ISkCanvas);
  var
    LPaint: ISkPaint;
    LComp: ISkShader;
    LRotMatrix: TMatrix;
    LRadialShader: ISkShader;
    LSweepShader: ISkShader;
  begin
    LRadialShader := TSkShader.MakeGradientRadial(PointF(ASize / 2, ASize / 2), ASize / 2, [$FFFFFFFF, $FFFFFFFF, $00FFFFFF]);
    LRotMatrix := TMatrix.CreateTranslation(- ASize / 2, - ASize / 2) * TMatrix.CreateRotation(DegToRad(-90)) * TMatrix.CreateTranslation(ASize / 2, ASize / 2);
    LSweepShader := TSkShader.MakeGradientSweep(PointF(ASize / 2, ASize / 2), [$FFFF00FF, $FFFF0000, $FFFFFF00, $FF00FF00, $FF00FFFF, $FF0000FF, $FFFF00FF], LRotMatrix);
    LComp := TSkShader.MakeBlend(TSkBlendMode.Modulate, LRadialShader, LSweepShader);
    LPaint := TSkPaint.Create;
    LPaint.Shader := LComp;
    ACanvas.DrawPaint(LPaint);
  end;

var
  LSurface: ISkSurface;
  LLayerPaint: ISkPaint;
  LDiscPaint: ISkPaint;
  LRadius: Single;
begin
  LSurface := TSkSurface.MakeRaster(ASize, ASize);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LLayerPaint := TSkPaint.Create;
  LLayerPaint.Blender := TSkBlender.MakeMode(ABlendMode);

  DrawBackground(LSurface.Canvas);

  LRadius := ASize / 3.2;
  LSurface.Canvas.SaveLayer(LLayerPaint);
  try
    LDiscPaint := TSkPaint.Create;
    LDiscPaint.AntiAlias := True;
    LDiscPaint.Blender := TSkBlender.MakeMode(TSkBlendMode.Plus);

    LDiscPaint.Color := TAlphaColors.Green;
    LSurface.Canvas.DrawCircle(ASize / 2, LRadius, LRadius, LDiscPaint);

    LDiscPaint.setColor(TAlphaColors.Red);
    LSurface.Canvas.DrawCircle(LRadius, ASize - LRadius, LRadius, LDiscPaint);

    LDiscPaint.setColor(TAlphaColors.Blue);
    LSurface.Canvas.DrawCircle(ASize - LRadius, ASize - LRadius, LRadius, LDiscPaint);
  finally
    LSurface.Canvas.Restore;
  end;
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity, Format(' w:%d h:%d', [ASize, ASize]));
end;

initialization
  TDUnitX.RegisterTestFixture(TSkBlenderTests);
end.
