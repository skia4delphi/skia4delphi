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
unit Skia.Tests.ColorFilter;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkColorFilterTests }

  [TestFixture]
  TSkColorFilterTests = class(TTestBase)
  private
    procedure TestColorFilter(const AImageFileName, AExpectedImageHash: string; const AColorFilter: ISkColorFilter; const ABackgroundColor: TAlphaColor = TAlphaColors.Null);
  public
    [TestCase('1.Clear - Red', 'horse.webp,Red,0.3,Clear,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2.Src - Red', 'horse.webp,Red,0.3,Src,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3.SrcOver - Red', 'horse.webp,Red,0.3,SrcOver,/85AAMDS8vL//nBgw9f+/v//d3P73///////9/////8HAAeAB4AHwA/AAOABQAfAAAAAAABAAAA')]
    [TestCase('4.DestOver - Red', 'horse.webp,Red,0.3,DestOver,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('5.SrcIn - Red', 'horse.webp,Red,0.3,SrcIn,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('6.DestIn - Red', 'horse.webp,Red,0.3,DestIn,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAACHoQcBjzSP/ID44/gAu4wAA')]
    [TestCase('7.SrcOut - Red', 'horse.webp,Red,0.3,SrcOut,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('8.DestOut - Red', 'horse.webp,Red,0.3,DestOut,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////+/gAeAB6EHxgfhDeEEYQRRAHAAYABAQAA')]
    [TestCase('9.SrcATop - Red', 'horse.webp,Red,0.3,SrcATop,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHwA/AAOABQAfAAAAAAABAAAA')]
    [TestCase('10.DestATop - Red', 'horse.webp,Red,0.3,DestATop,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAACHoQcBjzSP/ID44/gAu4wAA')]
    [TestCase('11.Xor - Red', 'horse.webp,Red,0.3,Xor,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////+/gAeAB6EHxgfhDeEEYQRRAHAAYABAQAA')]
    [TestCase('12.Plus - Red', 'horse.webp,Red,0.3,Plus,/85AAMLS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHyH/AcODzSP/IC4w/gAt4AAA')]
    [TestCase('13.Modulate - Red', 'horse.webp,Red,0.3,Modulate,/84AAMDS8vL//nBgw9f+/v//d2Hz3//+//////v///44wAgAAAAYGHgQ+BzzDPusX4//hP++/44')]
    [TestCase('14.Screen - Red', 'horse.webp,Red,0.3,Screen,/85AAMDS8vL//nBgw9f+/v//d3P73///////8/////8HAAeAB4AHwD/AAOAjQC/IAoABgABAAAA')]
    [TestCase('15.Overlay - Red', 'horse.webp,Red,0.3,Overlay,/85AAEDS+vL//nBgQ9f+/v//d3N73/////////////8DAAOAB4AHwD/AAOAzQC/IA4AFgABAAAA')]
    [TestCase('16.Darken - Red', 'horse.webp,Red,0.3,Darken,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHwA/gAOADQA/AAAAAAABAAAA')]
    [TestCase('17.Lighten - Red', 'horse.webp,Red,0.3,Lighten,/85AAMDS8vL//nBgw9f+/v//d3P73///////8/////8HAAeAB4AHwD/AAOAjQC/IAoABgABAAAA')]
    [TestCase('18.ColorDodge - Red', 'horse.webp,Red,0.3,ColorDodge,/85AAMDS8vL//nBgw9f+/v//d3P73///////8/////8HAAeAB4AHwD/AAOAjQC/IAoABgABAAAA')]
    [TestCase('19.ColorBurn - Red', 'horse.webp,Red,0.3,ColorBurn,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHwC/AAOADQA/IAAAAAABAAAA')]
    [TestCase('20.HardLight - Red', 'horse.webp,Red,0.3,HardLight,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHwA/AAOABQAfAAAAAAABAAAA')]
    [TestCase('21.SoftLight - Red', 'horse.webp,Red,0.3,SoftLight,/85AAMDS8vL//nBgw9f+/v//d3P73///////9/////8HAAOAB4AHwD/AAOAzQC/IA4AFgABAAAA')]
    [TestCase('22.Difference - Red', 'horse.webp,Red,0.3,Difference,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8/gAeAB4AHwD/QAOADQA/IA4ADgABAAAA')]
    [TestCase('23.Exclusion - Red', 'horse.webp,Red,0.3,Exclusion,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8/gAeAB4AHwD/QAOADQA/IA4ADgABAAAA')]
    [TestCase('24.Multiply - Red', 'horse.webp,Red,0.3,Multiply,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8HAAeAB4AHwA/AAOADQA/IAAAAAABAAAA')]
    [TestCase('25.Hue - Red', 'horse.webp,Red,0.3,Hue,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8XAAeAB4AHyD/AEODzQO/IC4AfgABwAAA')]
    [TestCase('26.Saturation - Red', 'horse.webp,Red,0.3,Saturation,/85AAMDS8vL//nBgw9f+/v//d2P73///////+/////8fgAeAB4AHwD/gAeAjYG/IAsABgABAAAA')]
    [TestCase('27.Color - Red', 'horse.webp,Red,0.3,Color,/85AAMDS8vL//nBgw9f+/v//d3P73///////e/////8HAAeAB4AHwD/AAOADQC+IAgAAAABAAAA')]
    [TestCase('28.Luminosity - Red', 'horse.webp,Red,0.3,Luminosity,/85AAMDS8vL//nBgw9f+/v//d3P73///////+//f//8fgAeAB4AHwA/gAOADQAbAAAAAAABAAAA')]
    [TestCase('29.Clear - Yellow', 'horse.webp,Yellow,0.3,Clear,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('30.Src - Yellow', 'horse.webp,Yellow,0.3,Src,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('31.SrcOver - Yellow', 'horse.webp,Yellow,0.3,SrcOver,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAIGHgQcBjzCPuIC4w/gA+4wAA')]
    [TestCase('32.DestOver - Yellow', 'horse.webp,Yellow,0.3,DestOver,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('33.SrcIn - Yellow', 'horse.webp,Yellow,0.3,SrcIn,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('34.DestIn - Yellow', 'horse.webp,Yellow,0.3,DestIn,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAACHoQcBjzSP/ID44/gAu4wAA')]
    [TestCase('35.SrcOut - Yellow', 'horse.webp,Yellow,0.3,SrcOut,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('36.DestOut - Yellow', 'horse.webp,Yellow,0.3,DestOut,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////+/gAeAB6EHxgfhDeEEYQRRAHAAYABAQAA')]
    [TestCase('37.SrcATop - Yellow', 'horse.webp,Yellow,0.3,SrcATop,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAICHgQcBjzCPuIC4w/gA+wwAA')]
    [TestCase('38.DestATop - Yellow', 'horse.webp,Yellow,0.3,DestATop,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAACHoQcBjzSP/ID44/gAu4wAA')]
    [TestCase('39.Xor - Yellow', 'horse.webp,Yellow,0.3,Xor,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////+/gAeAB6EHxgfhDeEEYQRRAHAAYABAQAA')]
    [TestCase('40.Plus - Yellow', 'horse.webp,Yellow,0.3,Plus,/85AQsLS8vf//nBiw9f+////d3P73///////f/v///8IYAhACAAIGHgQ8BzzCPuMX4//jL++/64')]
    [TestCase('41.Modulate - Yellow', 'horse.webp,Yellow,0.3,Modulate,/85AAMDS8vL//nBgw9f+/v//d3P73/////93c//f//8DAAeAAoEHAAfBAeEAQQRBAEAAAABAAAA')]
    [TestCase('42.Screen - Yellow', 'horse.webp,Yellow,0.3,Screen,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oQAhAAAAImHgQ8BjzCPvsX4//iD++9Q4')]
    [TestCase('43.Overlay - Yellow', 'horse.webp,Yellow,0.3,Overlay,/85AQMLS8vP//nBgw9f+////d3P73/////////v///84YghACAAYGHgQ8BzzCPuM/4//jj+//68')]
    [TestCase('44.Darken - Yellow', 'horse.webp,Yellow,0.3,Darken,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAICHgQcBjzCPvIC4g/iA+wAAA')]
    [TestCase('45.Lighten - Yellow', 'horse.webp,Yellow,0.3,Lighten,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oQAhAAAAImHgQ8BjzCPvsX4//iD++9Q4')]
    [TestCase('46.ColorDodge - Yellow', 'horse.webp,Yellow,0.3,ColorDodge,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oQAhAAAAImHgQ8BjzCPvsX4//iD++9Q4')]
    [TestCase('47.ColorBurn - Yellow', 'horse.webp,Yellow,0.3,ColorBurn,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAICHgQcBjzCPvIC4g/iA+wQAA')]
    [TestCase('48.HardLight - Yellow', 'horse.webp,Yellow,0.3,HardLight,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAICHgQcBjzCPuIC4w/gA+wwAA')]
    [TestCase('49.SoftLight - Yellow', 'horse.webp,Yellow,0.3,SoftLight,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oQghAAAAImHgQ8BjzCPuMH4//iB+89QI')]
    [TestCase('50.Difference - Yellow', 'horse.webp,Yellow,0.3,Difference,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///84gAAABQAIyDgQGBAjADuoAAAAgAAA8AA')]
    [TestCase('51.Exclusion - Yellow', 'horse.webp,Yellow,0.3,Exclusion,/85AAMDS8vL//nBgw9f+/v//d2P73/////////v///84gAAABQAIyDgQGBAjADuoAAAAgAAA8AA')]
    [TestCase('52.Multiply - Yellow', 'horse.webp,Yellow,0.3,Multiply,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8oAAgAAAAICHgQcBjzCPvIC4g/iA+wQAA')]
    [TestCase('53.Hue - Yellow', 'horse.webp,Yellow,0.3,Hue,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////87AAuAB4AHyD/AMODzSO/IC4AfgA8wAAA')]
    [TestCase('54.Saturation - Yellow', 'horse.webp,Yellow,0.3,Saturation,/85AAMDS8vL//nBgw9f+/v//d2P73///////+/////8fgAeAB4AHwD/gAeAjYG/IAsABgABAAAA')]
    [TestCase('55.Color - Yellow', 'horse.webp,Yellow,0.3,Color,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///8IAAAAAAAICHgQcBjzCPvIC4g/gA+4wAA')]
    [TestCase('56.Luminosity - Yellow', 'horse.webp,Yellow,0.3,Luminosity,/85AAMDS8vL//nBgw9f+/v//d3P73///////+/////8fgA+AB4AHyD/gAODzQO/IC4AfgAJQAAA')]
    procedure TestBlend(const AImageFileName, AColor: string; const AColorOpacity: Single; const AMode: TSkBlendMode; const AExpectedImageHash: string);
    [TestCase('1.Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('2.Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,/85AQMLS+vP//nBgw9f+////d3P73/////////v///8IwAgAAAAIGHgQcBjzCPuMH4w/gB+89QA')]
    [TestCase('3.Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,/85AQMLS+vP//nBgw9f+////d3P73/////93+/vf//+40CAwHwF6zvgdHxT3Bvn/B5gd4MAg/EA')]
    [TestCase('4.Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,/85AAMLS8vL//nBgw9f+/v//d3P73/////////////8XgAeAB4AHwD/gAOAzQC/IA4ANgABAAAA')]
    procedure TestColorMatrix(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1.Polaroid+Low contrast', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,/85AQMLS8vP//nBgw9f+////d3P73/////////////8XgAOAB4AHyD3AAODzAK+IA4AfgABgAAA')]
    procedure TestCompose(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1.Low contrast', 'horse.webp,False,NoInvert,0.05,/85AQMLS8vP//nBgw9f+////d3P73/////////////8bggOAB4AHwDzAAOAzAC+IA4APgABAAAA')]
    [TestCase('2.Medium contrast', 'horse.webp,False,NoInvert,0.50,/84AAEDyeHD//vDgw/f+/P///eP7////////6/////8Afx1PKZ6Bu/wm8J73Hv+f/7//n/+/D/8')]
    [TestCase('3.High contrast', 'horse.webp,False,NoInvert,1.00,/+4AAEB4OBD//vDgw//+3P//+OP7//7f////6////v+gSA1MBcACuv0+9p7/nv+//////n+/f88')]
    [TestCase('4.Grayscale', 'horse.webp,True,NoInvert,0.00,/85AAMDS8vL//nBgw9f+/v//d3P73/////////v///84wAgAAAAYGHgQ+BjzCPusH47/gD++/4o')]
    [TestCase('5.Invert brightness', 'horse.webp,False,InvertBrightness,0.10,AAG//7+vz/9/f////+/P///////////////////////U//z/+H/5P+Au917+vvg3////////P/8')]
    [TestCase('6.Invert lightness', 'horse.webp,False,InvertLightness,0.10,AAG////v//9/f////+////////////////////////9Bf/1/9//j84SmwMYEEgQT+C/8P74fAP8')]
    procedure TestHighContrast(const AImageFileName: string; const AGrayscale: Boolean; const AInvertStyle: TSkContrastInvertStyle; const AContrast: Single; const AExpectedImageHash: string);
    [TestCase('1.HSLA Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('2.HSLA Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,8/A4OHz+fBj/+Hh4f/9+3P/7ff9//37d/////////90HgAeAB4AHyH6gfHj/uPu4H/h/wA/QAAA')]
    [TestCase('3.HSLA Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,/85AQsLS8vP//nBiw9f+////d3P73/////938//f//8rYAvIB8ABuHKwdPjwqOxoG/5/yB98AAA')]
    [TestCase('4.HSLA Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,/85AAMDS8vL//nBgw9f+/v//d2P73/////////////8fgAeAB4AHwD/gAeADQA/IA4ANgABAAAA')]
    procedure TestHSLAMatrix(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,/85AQMLS8vP//nBgw9f+////d3P73///////+/////8fAA+AB4AHyD/gAODzYO/IC8AfwAZwAAA')]
    procedure TestLinearToSRGBGamma(const AImageFileName: string; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,ADG//z8tDQ1/f///fy0NDf//////v99d//////+/39/HP/e////n54fvB+cM9wRT4HEAd8BBAHE')]
    procedure TestLumaColor(const AImageFileName: string; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.UIConsts,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkColorFilterTests }

procedure TSkColorFilterTests.TestBlend(const AImageFileName, AColor: string;
  const AColorOpacity: Single; const AMode: TSkBlendMode;
  const AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash,
    TSkColorFilter.MakeBlend(MakeColor(StringToAlphaColor(AColor), AColorOpacity), AMode),
    TAlphaColors.Crimson);
end;

procedure TSkColorFilterTests.TestColorFilter(const AImageFileName,
  AExpectedImageHash: string; const AColorFilter: ISkColorFilter;
  const ABackgroundColor: TAlphaColor);
var
  LImage: ISkImage;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  Assert.IsNotNull(LImage, 'Invalid ISkImage (nil)');
  LSurface := TSkSurface.MakeRaster(LImage.Width, LImage.Height);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(ABackgroundColor);
  LPaint := TSkPaint.Create;
  LPaint.ColorFilter := AColorFilter;
  LSurface.Canvas.DrawImage(LImage, 0, 0, TSkSamplingOptions.Low, LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, 0.99, Format(' w:%d h:%d', [LImage.Width, LImage.Height]));
end;

procedure TSkColorFilterTests.TestColorMatrix(const AImageFileName: string;
  const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34,
  M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash,
    TSkColorFilter.MakeMatrix(TSkColorMatrix.Create(
      M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45)));
end;

procedure TSkColorFilterTests.TestCompose(const AImageFileName: string;
  const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34,
  M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash,
    TSkColorFilter.MakeCompose(
      TSkColorFilter.MakeMatrix(TSkColorMatrix.Create(
        M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45)),
      TSkColorFilter.MakeHighContrast(TSkHighContrastConfig.Create(False, TSkContrastInvertStyle.NoInvert, 0.05))),
    TAlphaColors.White);
end;

procedure TSkColorFilterTests.TestHighContrast(const AImageFileName: string;
  const AGrayscale: Boolean; const AInvertStyle: TSkContrastInvertStyle;
  const AContrast: Single; const AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash,
    TSkColorFilter.MakeHighContrast(TSkHighContrastConfig.Create(AGrayscale, AInvertStyle, AContrast)),
    TAlphaColors.Yellow);
end;

procedure TSkColorFilterTests.TestHSLAMatrix(const AImageFileName: string;
  const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34,
  M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash,
    TSkColorFilter.MakeHSLAMatrix(TSkColorMatrix.Create(
      M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45)),
    TAlphaColors.Red);
end;

procedure TSkColorFilterTests.TestLinearToSRGBGamma(const AImageFileName,
  AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash, TSkColorFilter.MakeLinearToSRGBGamma, TAlphaColors.Black);
end;

procedure TSkColorFilterTests.TestLumaColor(const AImageFileName,
  AExpectedImageHash: string);
begin
  TestColorFilter(AImageFileName, AExpectedImageHash, TSkColorFilter.MakeLumaColor, TAlphaColors.Yellow);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkColorFilterTests);
end.
