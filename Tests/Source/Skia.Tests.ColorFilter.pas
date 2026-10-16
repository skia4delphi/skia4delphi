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
    [TestCase('1.Clear - Red', 'horse.webp,Red,0.3,Clear,39f+P+/9b7B/GGeX7/f3tf/Pe/8b7B/G2eX7/X3t//Pe/wb7h3F2+X5/X/v/vPe/wf5hnF2+3w')]
    [TestCase('2.Src - Red', 'horse.webp,Red,0.3,Src,5Peef+3+e9B/GV9/nzf5vedfu/8e9F/G19/nTX7v+dfuvwf9l/H193mT33v+tfvvQf9lfP193g')]
    [TestCase('3.SrcOver - Red', 'horse.webp,Red,0.3,SrcOver,7EuOwvkVtKBAEbS7j7v9l89gPgcqAOCE8e3Xvn/qAZSfABH9J9H695q7X7gE9uPPAgNBdO6/5g')]
    [TestCase('4.DestOver - Red', 'horse.webp,Red,0.3,DestOver,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    [TestCase('5.SrcIn - Red', 'horse.webp,Red,0.3,SrcIn,5Peef+3+e9B/GV9/nzf5vedfu/8e9F/G19/nTX7v+dfuvwf9l/H193mT33v+tfvvQf9lfP193g')]
    [TestCase('6.DestIn - Red', 'horse.webp,Red,0.3,DestIn,4o8+QfQGhBDAFIuzn7n5pWMQfQIg8H/F5Otjjn7rAkAfwAr7V4F4+YyP77kC1+sfggBSBC4/4w')]
    [TestCase('7.SrcOut - Red', 'horse.webp,Red,0.3,SrcOut,39f+P+/9b7B/GGeX7/f3tf/Pe/8b7B/G2eX7/X3t//Pe/wb7h3F2+X5/X/v/vPe/wf5hnF2+3w')]
    [TestCase('8.DestOut - Red', 'horse.webp,Red,0.3,DestOut,5y/ugvsUrIBAELzXz3v8kOfQvgYn+K+E8/Prbr/oB6x/gBD7F1F7+Z+n7zcG/d+fggI9lF7A5w')]
    [TestCase('9.SrcATop - Red', 'horse.webp,Red,0.3,SrcATop,7EuOwvkVtKBAEbS/j7v9l89g/gYq/O+E8e3Xvn/qAZSfABH9J9H695q7X7gE9uPPAgNBdO6/5g')]
    [TestCase('10.DestATop - Red', 'horse.webp,Red,0.3,DestATop,4o8+QfQGiBDAFIyzn/n5pWMQfQIg8H/F5Otjjn7rAkAfwAr7V5F4+YyP77kC1+sfggBSCD4/4w')]
    [TestCase('11.Xor - Red', 'horse.webp,Red,0.3,Xor,5y/ugvsUrIBAELzXz7v8kOfQvgYn+K+E8/Prbr/oB6yPQBD7F1F7+Z+n73cG/d+fggI9lF7A5w')]
    [TestCase('12.Plus - Red', 'horse.webp,Red,0.3,Plus,9v/tA/4sEOGAD9jXb3sCgDtR/w0//K8E+/TDfoHpCczvgRn461B9+YwzIHcIByBQhAM1CB/B4w')]
    [TestCase('13.Modulate - Red', 'horse.webp,Red,0.3,Modulate,3+ee/+72U/B/F2KLzzj3vO+/O/4S/N+F2eU3fr3t9u8uPwb/n7G19oVfT7v+wtP/QP9fbN094g')]
    [TestCase('14.Screen - Red', 'horse.webp,Red,0.3,Screen,7/MtRP8l6MAAD93jX3wAgT/Bfws49H/E/PMDzwDoDeiPARb536C9+qHv/7YIC/jfAwQ0DD+B5w')]
    [TestCase('15.Overlay - Red', 'horse.webp,Red,0.3,Overlay,7wueg/wlxOAAD8arb3z+hCMR/wor/N/E9uwLTwDq/cP/wBT4B9H79ZvTb3cGCND/QgQ2oJ5A6A')]
    [TestCase('16.Darken - Red', 'horse.webp,Red,0.3,Darken,5juOQvsNkMBAD7bHr/z7l9vAfgUeCGDE8vMj7/7o/qvfvw//L4H69qaH3zcFA7jfAQM6YN4/6g')]
    [TestCase('17.Lighten - Red', 'horse.webp,Red,0.3,Lighten,7/MtRP8l6MAAD93jX3wAgT/BPws49H/E/PMHvwDoDeiPARb536C9+qHr/7YIDPjfAwQ0DD+B5w')]
    [TestCase('18.ColorDodge - Red', 'horse.webp,Red,0.3,ColorDodge,7/MtRP8l6MAAD93jX3wAgT/Bfws49H/E/PMDzwDoDeiPARb536C9+qHv/7YIC/jfAwQ0DD+B5w')]
    [TestCase('19.ColorBurn - Red', 'horse.webp,Red,0.3,ColorBurn,5zuOQvsOkLAAD7fLn/z7ltvAfgUeCGAE8/Mj7/7o/qvfvw//L4H69qaH3zcFA7jfAQM6YN4/6g')]
    [TestCase('20.HardLight - Red', 'horse.webp,Red,0.3,HardLight,7EuOwvkVtKBAEbS/j7v9l89g/gYq/O+E8e3Xvn/qAZSfABH9J9H695q7X7gE9uPPAgNBdO6/5g')]
    [TestCase('21.SoftLight - Red', 'horse.webp,Red,0.3,SoftLight,6/vtQ/4gwOCADtDL73z+gzOB/wks/I/E+fArL4DoA9TvQBT6+3D896TDH3cHDNAPgwQ0wN5A6Q')]
    [TestCase('22.Difference - Red', 'horse.webp,Red,0.3,Difference,0tcdRAIZsKAAD9rrX/35dzOBwAcs6H8E+/FDbz/nEBgAwRH2t+D9/Lib3zYIENBPAwQzHJ+B7A')]
    [TestCase('23.Exclusion - Red', 'horse.webp,Red,0.3,Exclusion,0tcdRAIZsKAAD9rrX/35dzOBwAcs6H8E+/FDbz/nEBgAwRH2t+D9/Lib3zYIENBPAwQzHJ+B7A')]
    [TestCase('24.Multiply - Red', 'horse.webp,Red,0.3,Multiply,5zuOQvsNkMBAD7fLr/z7l9vAfgUeCGAE8/Qj7/7o/qvfvw//L4H69qaH3zcFA7jfAQM6ZN4/6g')]
    [TestCase('25.Hue - Red', 'horse.webp,Red,0.3,Hue,7OM9BAAfxOBADd3nX73+gUfxPwoq/B+E/fpH/3/mC/DPABT67xD9+aq3f/YIFtT/AgQu9D6B6g')]
    [TestCase('26.Saturation - Red', 'horse.webp,Red,0.3,Saturation,4geOgwQbyJCACucb4D3+fjtxAAgzCNDD//Fbb4Do/w/gABcB7DD99aXnH7cIFdAPgwM21H7A6Q')]
    [TestCase('27.Color - Red', 'horse.webp,Red,0.3,Color,7fv9g/0m0PBAD87HPzz/gzdhfwsw+M9E+fELb4DoCcwvARX368B8+Z/X3zYICeB/gwQz2F4B6A')]
    [TestCase('28.Luminosity - Red', 'horse.webp,Red,0.3,Luminosity,2Q8OAwAMjIAADszrX335h/PgvwQh+C+E9/ZHvz7nB/QfQA/8+0C8+bV/j7cGDcAvggI2zF5A7Q')]
    [TestCase('29.Clear - Yellow', 'horse.webp,Yellow,0.3,Clear,39f+P+/9b7B/GGeX7/f3tf/Pe/8b7B/G2eX7/X3t//Pe/wb7h3F2+X5/X/v/vPe/wf5hnF2+3w')]
    [TestCase('30.Src - Yellow', 'horse.webp,Yellow,0.3,Src,AdgeQPQU7NC/Fqd7/3gAtgcQPQU79K/F6d4/HoDtAURPwQ79a3G6948HYHsA0VOwQ/9anO794w')]
    [TestCase('31.SrcOver - Yellow', 'horse.webp,Yellow,0.3,SrcOver,CSwOwwArLKGADvu73zwFkfMgQAxHADBEA+wvj8HoCQDggRj9+0C/97Az8PcGEjSQBAM2kN//6w')]
    [TestCase('32.DestOver - Yellow', 'horse.webp,Yellow,0.3,DestOver,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    [TestCase('33.SrcIn - Yellow', 'horse.webp,Yellow,0.3,SrcIn,AdgeQPQU7NC/Fqd7/3gAtgcQPQU79K/F6d4/HoDtAURPwQ79a3G6948HYHsA0VOwQ/9anO794w')]
    [TestCase('34.DestIn - Yellow', 'horse.webp,Yellow,0.3,DestIn,4o8+QfQGhBDAFIuzn7n5pWMQfQIg8H/F5Otjjn7rAkAfwAr7V4F4+YyP77kC1+sfggBSBC4/4w')]
    [TestCase('35.SrcOut - Yellow', 'horse.webp,Yellow,0.3,SrcOut,39f+P+/9b7B/GGeX7/f3tf/Pe/8b7B/G2eX7/X3t//Pe/wb7h3F2+X5/X/v/vPe/wf5hnF2+3w')]
    [TestCase('36.DestOut - Yellow', 'horse.webp,Yellow,0.3,DestOut,5y/ugvsUrIBAELzXz3v8kOfQvgYn+K+E8/Prbr/oB6x/gBD7F1F7+Z+n7zcG/d+fggI9lF7A5w')]
    [TestCase('37.SrcATop - Yellow', 'horse.webp,Yellow,0.3,SrcATop,CSwOwwAqKKGADvu73zwFke8gQAxHADBEA+wvj8HoCQDgQRj9+0C/97Az8PcGETSQBAM2kN8/7A')]
    [TestCase('38.DestATop - Yellow', 'horse.webp,Yellow,0.3,DestATop,4o8+QfQGiBDAFIyzn/n5pWMQfQIg8H/F5Otjjn7rAkAfwAr7V5F4+YyP77kC1+sfggBSCD4/4w')]
    [TestCase('39.Xor - Yellow', 'horse.webp,Yellow,0.3,Xor,5y/ugvsUrIBAELzXz7v8kOfQvgYn+K+E8/Prbr/oB6yPQBD7F1F7+Z+n73cG/d+fggI9lF7A5w')]
    [TestCase('40.Plus - Yellow', 'horse.webp,Yellow,0.3,Plus,CujdgwYvgBHBCwDw33sHf0txARBcGHADBvrbnoLnEUSwgh8AsFDB+qOb4HYKIHjgBQMrENDA6A')]
    [TestCase('41.Modulate - Yellow', 'horse.webp,Yellow,0.3,Modulate,6tcewPEIjCAAFpSH/7n7thdg/AIh9L8F5uGL7j7u+DMvQAwAdGE49o+nz/r/1fsvwgBW+G1+4w')]
    [TestCase('42.Screen - Yellow', 'horse.webp,Yellow,0.3,Screen,AeQtBAMoJJFADfLjf3wDgDvBwAtGAOADAvYDP8HlExDQARf523A/+60XELcJFCiAxAIyoK/A6w')]
    [TestCase('43.Overlay - Yellow', 'horse.webp,Yellow,0.3,Overlay,8P+dAwQjHFEBDODn//wBhTOhAA1BGOCD/vEb/0DpABRggRkB1FC+96srALcHGAwAxAMwOH8A6g')]
    [TestCase('44.Darken - Yellow', 'horse.webp,Yellow,0.3,Darken,8isOQ/4fzPAADujH7z0Akf9wPwos/E+E//B7X0DpAPDfwBX/BzE99rDTXzcGFuAPgwQy7F5A6w')]
    [TestCase('45.Lighten - Yellow', 'horse.webp,Yellow,0.3,Lighten,AeQtBAMoIJFADfLjf3wDgDvBwAtGAODDAfYDL8HlExDQARf522A/+60XELcJFCiAxAIyoK/A6w')]
    [TestCase('46.ColorDodge - Yellow', 'horse.webp,Yellow,0.3,ColorDodge,AeQtBAMoJJFADfLjf3wDgDvBwAtGAOADAvYDP8HlExDQARf523A/+60XELcJFCiAxAIyoK/A6w')]
    [TestCase('47.ColorBurn - Yellow', 'horse.webp,Yellow,0.3,ColorBurn,8isOg/4fzPDADenH3z0AkP9wPwos/E+E//B7X0DpAPDfwBX/BzE99rDTX3cGFuAPgwQx7F5A6w')]
    [TestCase('48.HardLight - Yellow', 'horse.webp,Yellow,0.3,HardLight,CSwOwwAqKKGADvu73zwFke8gQAxHADBEA+wvj8HoCQDgQRj9+0C/97Az8PcGETSQBAM2kN8/7A')]
    [TestCase('49.SoftLight - Yellow', 'horse.webp,Yellow,0.3,SoftLight,8vPtAwIh8PAADeHfL70AgzthAAs4CACE/vMzn8DnBwQwgRb95wC++K3v/zYIFvSvwwMxKK9A6w')]
    [TestCase('50.Difference - Yellow', 'horse.webp,Yellow,0.3,Difference,2tudwwIKrEDADr3vP3z4fQvRwAMrABCE8/jrfr7kE/hfQAz22/A8/rp7j/cHBdjPAgE5II+A7w')]
    [TestCase('51.Exclusion - Yellow', 'horse.webp,Yellow,0.3,Exclusion,2tudwwIKrEDADr3vP3z4fQvRwAMrABCE8/jrfr7kE/hfQAz22/A8/rp7j/cHBdjPAgE5II+A7w')]
    [TestCase('52.Multiply - Yellow', 'horse.webp,Yellow,0.3,Multiply,8isOQ/4fzPDADenH7z0AkP9wPwos/E+E//B7X0DpAPDfwBX/BzE99rDTX3cGFuAPgwQx7F5A6w')]
    [TestCase('53.Hue - Yellow', 'horse.webp,Yellow,0.3,Hue,6+ctRAAeyPAADd7nf73+gUchAAos/A/E/flP/7/mCgDQgBT753B9+a67j/YIGNQPQwQtAC9B6w')]
    [TestCase('54.Saturation - Yellow', 'horse.webp,Yellow,0.3,Saturation,4geOgwQbyJCACucb4D3+fjtxAAgzCNDD//Fbb4Do/w/gABcB7DD99aXnH7cIFdAPgwM21H7A6Q')]
    [TestCase('55.Color - Yellow', 'horse.webp,Yellow,0.3,Color,8hcug/8h2CCBDObHv70AjReh/wowADBE/u9nn4DpAPQPwRYAAIH99a/jT3cGF+g/wwQx/G4A6w')]
    [TestCase('56.Luminosity - Yellow', 'horse.webp,Yellow,0.3,Luminosity,9+MtxAIj9EBADP0XoPwAfD+RQAo76K+DAwEYn0DkGSCAwRX531B//Kr333YLGBzAgwEvjA+B6g')]
    procedure TestBlend(const AImageFileName, AColor: string; const AColorOpacity: Single; const AMode: TSkBlendMode; const AExpectedImageHash: string);
    [TestCase('1.Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    [TestCase('2.Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,++/9AwAqFFFBDe3TvzwDgksBAA85BPCDAf8vnwHnEPy/ARr629C++Z8XAHYKHxxAhAQnYM9B6A')]
    [TestCase('3.Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,8sOtRAEmBOGADPUjsDwCc39BQA038K9DBAscHwHkFRCAQRr2v4A+/JMHIDUNIvj/QwQiUF+C5g')]
    [TestCase('4.Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,6uM9RAAdwOAADd7rb33+gEsBgAkq/A/E/fpP/3/mDADAQBT745D9+a+7bzYJGdT/AgQtBE8B6w')]
    procedure TestColorMatrix(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1.Polaroid+Low contrast', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,3Ns9BAAZwIBBDcvn7739f1/RPwokEBDE+vdn3//nAfBvgBX+24A8+K6r3/UHHKCfQgYpsH6B6g')]
    procedure TestCompose(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1.Low contrast', 'horse.webp,False,NoInvert,0.05,3NstxAAZwIABDc3v/739gF8BAAomEABE+/Zn3z/o//NvgBX/30C8962r/7UHG5yPggYqpF6B6g')]
    [TestCase('2.Medium contrast', 'horse.webp,False,NoInvert,0.50,+RM+BPYVENAAELMnr7/6iVuh/gII8F8F9Njzv4Hv38NvgBH9N1G656Y3f7cBDWj/QQg/rL0/7g')]
    [TestCase('3.High contrast', 'horse.webp,False,NoInvert,1.00,9COehPMR8F/AEbgjP8D6jC8BvgL61w8G88r/b0Lt9E8/AAXv44F4463jHvgC8CdPQQZVeA2/8g')]
    [TestCase('4.Grayscale', 'horse.webp,True,NoInvert,0.00,69N9RP8fwOCADeDrTz3+gU8RgAom8A+E/v9Tj3/kFfivwBH126D9/LGP73UKGcjfAgQpHO8B7A')]
    [TestCase('5.Invert brightness', 'horse.webp,False,InvertBrightness,0.10,30P+gAb4LzH+DxOY8Hj5eUMAQgNGvG+CADFEfj7bVIDQwQ3xn0BDDbQXUDgRDuighPY4LFEB7Q')]
    [TestCase('6.Invert lightness', 'horse.webp,False,InvertLightness,0.10,zyPegAr0O7E+Dwm8wLj4eE9gAgNQ1B9CAi88fsDdSHyQQhb53xCEBqNrwLgPGwhBxfgzNIHA6A')]
    procedure TestHighContrast(const AImageFileName: string; const AGrayscale: Boolean; const AInvertStyle: TSkContrastInvertStyle; const AContrast: Single; const AExpectedImageHash: string);
    [TestCase('1.HSLA Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    [TestCase('2.HSLA Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,uk8Ogf7u458/EZxzn73qijMQQPj7EwDF5u97f7vmAeCvvv7v+1C6+dajLrkD+V8vwAFDWP4/+A')]
    [TestCase('3.HSLA Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,+Lf9BAQmLEFBDP0rgHwEdpMRAQ9D+B9DBxAQj0HiJDTAwRv4l2DA/6IjgDQPKSygBAQezP9C6A')]
    [TestCase('4.HSLA Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,4veNgwEZsKAADdjrf/36gSNBQAcnCDAE+/lfj7/mCPSfgBL67xC9+LKfz7YIF8SPAgQu6A6B7A')]
    procedure TestHSLAMatrix(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,AMitxAQrMDFACxM8APwDdlfxQA1H4F9DCQz4HgHiJzgAghj2w+CA/6YrIHYOIEyAxAApBMBB6Q')]
    procedure TestLinearToSRGBGamma(const AImageFileName: string; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,zbeufgDjkyD/Foxjzzrwrn9vAPcsDOAE4NWLTjzpCLSfv/75L6H7/N5Tz/z80SegQvtjUE/8+Q')]
    procedure TestLumaColor(const AImageFileName: string; const AExpectedImageHash: string);
    [Test]
    procedure TestColorMatrixScale;
    [Test]
    procedure TestLighting;
    [Test]
    procedure TestOverdraw;
    [Test]
    procedure TestTable;
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

function DrawRect(const AColor: TAlphaColor; const AColorFilter: ISkColorFilter): TAlphaColor;
var
  LBuffer: TBytes;
  LPaint: ISkPaint;
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  SetLength(LBuffer, 4 * 4 * 4);
  LPixmap := TSkPixmap.Create(TSkImageInfo.Create(4, 4), LBuffer, 4 * 4);
  LSurface := TSkSurface.MakeRasterDirect(LPixmap);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.Color := AColor;
  LPaint.ColorFilter := AColorFilter;
  LSurface.Canvas.DrawPaint(LPaint);
  Result := LPixmap.Colors[1, 1];
end;

procedure TSkColorFilterTests.TestColorMatrixScale;
var
  LColorMatrix: TSkColorMatrix;
begin
  LColorMatrix := TSkColorMatrix.CreateScale(0.5, 0.5, 0.5);
  Assert.IsTrue(LColorMatrix <> TSkColorMatrix.Identity, 'A scale is not the identity');
  Assert.AreSameColor($FF808080, DrawRect(TAlphaColors.White, TSkColorFilter.MakeMatrix(LColorMatrix)), 1,
    'The scale should halve the color channels and keep the alpha');
  Assert.AreSameColor(TAlphaColors.White, DrawRect(TAlphaColors.White,
    TSkColorFilter.MakeMatrix(TSkColorMatrix.CreateScale(1, 1, 1))), 0, 'A neutral scale should not change the color');
end;

procedure TSkColorFilterTests.TestLighting;
begin
  Assert.AreSameColor($FF80A080, DrawRect($FF808080, TSkColorFilter.MakeLighting(TAlphaColors.White, $FF002000)), 1,
    'The add color should be added to every channel');
  Assert.AreSameColor($FF808080, DrawRect(TAlphaColors.White, TSkColorFilter.MakeLighting($FF808080, TAlphaColors.Black)), 1,
    'The multiply color should scale every channel');
end;

procedure TSkColorFilterTests.TestOverdraw;
const
  Colors: TSkOverdrawColor = (TAlphaColors.Red, TAlphaColors.Lime, TAlphaColors.Blue,
                              TAlphaColors.Yellow, TAlphaColors.Aqua, TAlphaColors.Fuchsia);
var
  I: Integer;
begin
  // The overdraw filter reads the source alpha as the number of times a pixel
  // was painted, and answers the matching color.
  for I := 0 to High(Colors) do
    Assert.AreSameColor(Colors[I], DrawRect((Cardinal(I) shl 24) or $00FFFFFF,
      TSkColorFilter.MakeOverdraw(Colors)), 0, Format('(alpha %d)', [I]));
end;

procedure TSkColorFilterTests.TestTable;
var
  I: Integer;
  LIdentity: TSkTableFilter;
  LInvert: TSkTableFilter;
begin
  for I := 0 to High(LInvert) do
  begin
    LInvert[I] := 255 - I;
    LIdentity[I] := I;
  end;
  Assert.AreSameColor(TAlphaColors.Null, DrawRect(TAlphaColors.Red, TSkColorFilter.MakeTable(LInvert)), 0,
    'A single table also inverts the alpha channel');
  Assert.AreSameColor(TAlphaColors.Aqua, DrawRect(TAlphaColors.Red, TSkColorFilter.MakeTable(LIdentity, LInvert, LInvert, LInvert)), 1,
    'Inverting only the color channels should turn red into aqua');
end;

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
