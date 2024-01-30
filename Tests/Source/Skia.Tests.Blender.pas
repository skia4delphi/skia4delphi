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
    [TestCase('Dest',       '256,Dest,0.99,/8OBg4+H//////Hjz8f/////+ePP3//////////////////H/4f/A/AD8APwB/AD+AP4B/wP//8')]
    [TestCase('SrcOver',    '256,SrcOver,0.99,58PDsRgAAIH///Px28bMyf//8/vfzszN///z///P7M/gB+AH4DPB/YH/g/+D/4P/g/+D/8H///8')]
    [TestCase('DestOver',   '256,DestOver,0.99,58ODhx48GIH///Pn3/7cyf//++ff/9/N///77////8/vx+/H/4f/Af8BzwHGAeID4AOwD8B///8')]
    [TestCase('SrcIn',      '256,SrcIn,0.99,/8PDgYGBw/////Phw8fP//////vPz8/////////P7//gB+AH4Afhj/HP89/7//v///////////8')]
    [TestCase('DestIn',     '256,DestIn,0.99,/8ODg4+H//////Pjz8f/////++PP3//////////////////H/4f/A/AD8APwB/AD+AP4B/wP//8')]
    [TestCase('SrcOut',     '256,SrcOut,0.99,w9v//3w8GIH/////f39cyf////9/f97N//////9//s/n5+/3///////9//z//P/8//i/8d+D/4c')]
    [TestCase('DestOut',    '256,DestOut,0.99,/708PH///////Xx8f//////9/Xx///////39fH////8ABgAHAA8AAwABAAAAAAAAAAAAAAAAAAA')]
    [TestCase('SrcATop',    '256,SrcATop,0.99,/8ODgYGBw/////Phw8fP//////vPz8/////////P7//gB+AH4APhjfHP8//7//v///////////8')]
    [TestCase('DestATop',   '256,DestATop,0.99,58PDhx48GIH///PnX39cyf//++dff1/N///77////8/vx+/H/4f/Af8BzwHGAeID4AOwD8B///8')]
    [TestCase('&Xor',       '256,Xor,0.99,59u//3w8GIH///////7cyf///////97N/////////s/n5+/3//f//f/8//z//P/8//i/8d+D/4c')]
    [TestCase('Plus',       '256,Plus,0.99,58OHjz48GIH///fv//7cyf///+///9/N////7////8/nB+cH/2f/jf9//n/+H/wf+9+x38H///8')]
    [TestCase('Modulate',   '256,Modulate,0.99,/8PDgYGBw/////Phw8fP////8+PP3//////////////8B/AH8AfxB/EP8A/7H/of/3////////8')]
    [TestCase('Screen',     '256,Screen,0.99,58ODjz48GIH///Pv//7cyf//+////9/N///7/////8/nB+cH73f//f9//n/+H/wf+9+x38H///8')]
    [TestCase('Overlay',    '256,Overlay,0.99,58ODhx4cGIH///Pn397cyf//++ff39/N///77//f/8/nB/8H/4f/Af8B/wH/AeMD4QexD8D///8')]
    [TestCase('Darken',     '256,Darken,0.99,58PDgQgAgNH///Phy8bM2f//8+PP3uz9///z7//f7P/gB+AH4IfBB4EPgw+Dn4P/g/+D/8H///8')]
    [TestCase('Lighten',    '256,Lighten,0.99,58ODrx48GIH///Pv3/7cyf/////f/9/N/////////8/nB+cH73f//f9//n/+H/wf+9+x38H///8')]
    [TestCase('ColorDodge', '256,ColorDodge,0.99,58ODhx48GIH///Pn3/7cyf//+/ff/9/N///7/////8/vx+/H/4f/Ac8Bz4HHgcPB4cOx58D///8')]
    [TestCase('ColorBurn',  '256,ColorBurn,0.99,58ODgQ4cGJH///Phz97c2f//8+PP39/d///z7//f/9/uB/4H/4f/h/8B/gH+A/gD+AewD9B/+/8')]
    [TestCase('HardLight',  '256,HardLight,0.99,58ODhRgAAIH///Pl28bMyf//8/ffzszN///z///P7M/gB+AH4HPB/4F/gn+D/4P/g/+D/8H///8')]
    [TestCase('SoftLight',  '256,SoftLight,0.99,58ODhx4cGIH///Pn397cyf//++ff39/N///77//f/8/nx++H/4f/Af8B7wHOAeID4AewD8B///8')]
    [TestCase('Difference', '256,Difference,0.99,58ORgQY8OIH///Hhx/78yf///f3n///N///9/+f//8/nB+cH7nf+/f7//P/8X/wf+B+xn8E///8')]
    [TestCase('Exclusion',  '256,Exclusion,0.99,58OThwY8OIH///Pnx/78yf////fX///N//////f//8/nB+cH7nf+/f7//P/8X/wf+A+xn8E///8')]
    [TestCase('Multiply',   '256,Multiply,0.99,58PDgQgAgNH///Phy8bM2f//8+PP3uz9///z7//f7P/gB+AH4IfBB4MPgw+Dn4P/g/+D/8H///8')]
    [TestCase('Hue',        '256,Hue,0.99,58ODhw4cAIH///Pnz97Myf//++fP38/N///77+/f78/gB+AH4DfB/YH/i/+D/4P/g/+H/8H///8')]
    [TestCase('Saturation', '256,Saturation,0.99,58ODhx48GIH///Pn3/7cyf//++ff/9/N///77////8/jx+PH/4f/Af4BzAHPAeMD4AewD8B///8')]
    [TestCase('Color',      '256,Color,0.99,58ODhw4cAIH///Pnz97Myf//++fP38/N///77+/f78/gB+AH4DfAfYH/i/+D/4P/g/+H/8H///8')]
    [TestCase('Luminosity', '256,Luminosity,0.99,58ODsRgYAIH///Px297Myf//+/vf387N///7///f78/vx+/H74ffAd8DyQPAA+AH8A+wP8F///8')]
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
    LRotMatrix := TMatrix.CreateTranslation(- ASize / 2, - ASize / 2) * TMatrix.CreateRotation(-90) * TMatrix.CreateTranslation(ASize / 2, ASize / 2);
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
