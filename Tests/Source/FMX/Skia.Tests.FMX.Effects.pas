{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.Effects;

interface

{$SCOPEDENUMS ON}
{.$DEFINE SKIA_TESTS_FMX_EFFECTS}

{$IFDEF SKIA_TESTS_FMX_EFFECTS}
uses
  { Delphi }
  System.Types,
  FMX.Graphics,
  FMX.Effects,
  FMX.Filter.Effects,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  TObjectSetup<T: class> = reference to procedure(AObject: T);

  { TFMXEffectsTests }

  [TestFixture]
  TFMXEffectsTests = class(TTestBase)
  private
    procedure PaintPicture(ATargetBitmap: TBitmap; const ADest: TRectF; const ASvgFileName: string; const AWrapMode: TSkSvgWrapMode; const AGrayscale: Boolean);
  protected
    procedure DoTestFilter<T: TFilterBaseFilter>(const ASvgFileName: string; APictureWidth, APictureHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string; const AFilterSetup: TObjectSetup<T>);
    procedure DoTestInBitmap<T: TEffect>(const ASvgFileName: string; APictureWidth, APictureHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string; const AEffectSetup: TObjectSetup<T>);
    procedure DoTestInControl<T: TEffect>(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string; const AEffectSetup: TObjectSetup<T>);
  public
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,f/////////9///////////////////////////////////////////////////////////////8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,34OPj4eBAf/////vx8dN/////+//313////////f/f/h//9v/3vr++Nj8D/439znygP4//////8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,//////Lg4cH/////8+fvzf/////35+/P///////////////////////+/87/jf9L/tv8l/wv/Gc')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,//jw8OCAgP///PDx48fO///+9/Pr9/7/////////////u//H/8f/B/4g+PDx8MID6P7/+f/J/nw')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,///5+Pzs/Pz///n5/+/+/P//+/n///79////////////7//j/+H/8P/0/nP8u/jb+fj97P70//M')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,//////////7//////////v/////////+//////////////////////////////////////////4')]
    procedure TestBitmapAffineTransformEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,/88HAwMVFf3//3djQ1df/f////fL31///////+//3//gf08/kZ+gf1d/D6+Et8W33ffr9+f3/7c')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,/4eFFQWNwZ////V1R8/P3///9/3H/+/f/////////9/jy+ZDx43jQeMBggPFt8AnmE/OV+f78ec')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,6ODA4OHh/////PDg4+f////++Pnv7//////9///////yIfumujP+050B/AfEBu6C93ft2/7N/78')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,8ODg8PH///7//PDx8////v/+8vv3///////+//////9mgH6AboF/G1cFaT/O97d/gvP5tf7++pA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,8PDw8Ojv9/j//PDx6+///P/+9Pf77//9//7+9//v//2/CL0QvYMZAL0SX6B+w/+x3/eXf3fNS/s')]
    procedure TestBitmapBandedSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,/+Pw+OMBg8T/+/D540fPzP//8/nz78/d/////fv/z9+ehbtBL2Xcz/gPMK9CD+cPqR32O/P7+D4')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,/8bgYODy0/H//vBh4/ff/f//9nP7///9//////v///94gJsDiACYGPgZ2CroC+KOcA+vs5WH9QY')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/85AAMDS8vL//nBgw9f+/v//d3Pz3/////////P///8ogAgAIAAAAHgA+B7zDPuMVx/8hL22//g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/84AAMLS+vL//nBgw9f+/v//d3P73/////////v///881mTTPMc7//bfb789//p6RHIAIMAA+UA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapBandedSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vx8eDAgh7///Hx48fOXv//8/n3195+//////ff////p//nv3cfcVwnUIMch+6f4p3amMu5x/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v59WDCgp7///n1Y8fO3v//+/97197e//////vf/v/+r/6nvreecVwCEIK0hvYd4hjIGIMYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/v5dSCCgp////l1Y8fO3///+31358////////fv7//vp++3v3cfdVkRUQEUlfSd4J3ImIOZg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/v7dSDCgp7///t1Y8fO3v//+31/587e///////v79/vr++vr2cPdU0CUQKUnvQf4BzKHIM4g/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+9v5dQDCgp7///l1Q8fO3v//+31X587e//////fv79/tr+2nrSctdU0BMQG0hfSd4JnInMOZg/8')]
    procedure TestBitmapBandsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,//vzcTDAgJ////Nxc8fO3///83n3597////////v///3r/+vv6efdd0RkYOXjfef5pnrncO/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,//vz8TDAgJ////Pxc8fO3///8/n3x9/////////P///3r/+nv6efd90RkYOXj/ef4pnqncO/w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/vzcSDAgJ////NxY8fO3///8/v319/////////f///+r/6nvreeddQBkIOWjfef4pnLmcG///8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vzcXDCgJ////Nxc8fO3///83n3x87///////fP7///r/+nv+efd9EBkYO+nfeb6pnbncP///8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/vzcXHBgJ////Nxc8fO3///83X3587///////f/7//tr+3vrW8NZUkBAYG3neeb5ZnbmcO7//8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,//vz8TDAgJ////Pxc8fO3///8/n3597////////////3r/+vv6efdd8RkYOXj/ef5pnqncO/w/8')]
    procedure TestBitmapBevelEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress, ASizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vhcWDAgp7///FxY8fO3v//8/lz597///////Pv//8AAP//CACP8V0XAAC/n+CM/53IgP+ZAIA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+vxYGDCgpL///FgY8fO3v//82t7787///////vv//+spe+nreeccVkBUAC/n7AM/53IAP+9AAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+5gAMDSkpL//nBgw9fe3v//d/Hz3/////////P//////wAAv/ef/QAAUZkUGLed/50AAP+Yw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AAMDS8vL//nBgw9f+/v//d2Pz3/////////v////v7++nAAEP9U0RAKEAALwd7BgAANsY1/8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapBlindTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,AADR+/v98eB/ePH7+///7H97+fv7///9f3v5+/v///1DIP////////f//////7//3/1d3x+P/58')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,AAAQEHL7+/l/fHBRc////X9/eX37////f395ffv///8AAAAAAAAGAB9k3e////f///////////8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,zEQAQsLy+v/+dGDiw/f+//53c+vr//////f36/////8HAA+ABsAGwAIgCQACAALAGABJbN3/3/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapBloodTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+9v5NQBCgpb///l1Q0fO3v///Xd7X8/f//////vf7//vr++nr/cPcU0RAYC0mfQd5JjYmMOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+9v5NQDAgp7///l1Q8fO3v///X1f19/f///////f/9/vr++nr/cPcU0AAYC0kfQd5BjYmMOYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/v5dSDAgp7///l1Y8fO3v//+/1399////////f////vr++nr+cPcU0BEQC0gfQd5BjImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1399/////////////vr++nr+ePcU8BEQG0hfQd5JjImIOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp++nr6e/cd8B8QG0hfSd5JzInMOYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vx8fHBgh7///Hx88fOXv//8/n3595////////v////p/+nv6e/c98B9QG0hfSd5JzInIOYw/8')]
    procedure TestBitmapBloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,//Pz8aGBgJ////Px48fO3///+/n3x9/////////f///3j/+P/yffJ/kD0Qf3v/K/47vBv8H///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,//PzgYGBg/////Phw8fP//////vbz//////////f////D/8P/w/+D/gP8A/gf+D/4P/g//////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.99,//fjw4GBx/////Pjw8fP///////Pz9//////////////H/8f/w/8D/gP8B/gf/B/8P////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.99,///jw8PD//////Pjw8f////////Pz///////////////v/0f/A/8D/AP4B/gf/x//P////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,///jw4PD//////Pjw8f//////+/Pz/////////////////zf/A/4D/Af8B/wP/x///////////8')]
    procedure TestBitmapBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdGDAghb///F0Y8fOXv//8/xz195+//////P////tp+2nrbctcXkRIQE1h/ef5J7YnNuYg/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+nhZODAghL/+fFk48fOXv//9/Tzz95+//////P//v/8p/yjLTM8M/gBIAE1kf6fxJ7EnJOcg/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+xgQMDSshL//HBgw9f+Xv//d3Hz3/7+//////v////9o/2jfTE9OXkAIAAtg/yPxJ7MjJGMs5w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8vL//nBgw9f+/v//d3Pz3/////////v////9t/23Pb89/B0cARAFxXzPxA6ABNAA/4w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapBlurTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNu5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7efcV0HUYe/j/ef5p3bmNu5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7e/cV0HUYe/j/ef553bmNO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3195///////ff///vp++nv7e/cV0F8Ye/j/ef553bmcO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXDAgh7///Fxc8fOXv//83l3185///////ff7///p/+nv6e/M/0DsQe/h/af5p3KncO9w/8')]
    procedure TestBitmapBoxBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/Px8XOQiwz///Hxc9fPTP//9fn73899///1+f/f330DSECQqJahkc0x8cGvhdwXJJ1rGQCrAGY')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/84AAEDS+nL//nBgQ9f+fv//d2N73/////////v///8AgAAAAAAICHgAcBjzQPvsE44/gCW8gQA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/84AAMLS+vL//nBgw9f+/v//d2P73/////////v///8AgAAAAAAICHgQcBjzAPvMG44/gA28gAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAICHgQcBjzCPvMH48/gAu8gAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/84AAMLS+vL//nBgw9f+/v//d3P73/////////v///8AAAAAAAAICHgQ8BjzCPvsX4//gB++wQo')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapBrightTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,+/vxdWDAgp7///F1Y8fO3v//8/13197///////ff///vr++nr7edcVgXUAO0j/Sf5J3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,+/vhwcDDgp////Hhw8fO3///9/Hzz87///////vv/v/sr+gHqAeIAVgBUAOwB/Sf5J37mPuZ+/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/84AAMDS+vP//nBgw9f+////d2P73///////6/////+XAAeAB4AHwC/AAOADQIfAwgDAAOBA+Ac')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/84AAMDS+v///nBgw9f+////d2P73///////6/vf///gAMAAhQAGwEgAAICTAPbA5AD4APsA//8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapCircleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,////Pgxg4PD///9/T2fu/P////9f9/79///////////v/+//r/+P/V3/Ufu/n/2f/Z3/mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,////d2CAghb///93Y8fOXv///393595+//////f////v/+//r/+P/d0P0Y+/j/Sf7J/PnNO9w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,+/vx8eGPjw////Hx48/PT///8/vz/99///////P////vp++n77fvd+0H8cf///T/5P/L/8P/w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,+/vx8eDMjw////Hx48/PT///8/n3/99///////f////vp++n77fvcekH8cP///T/5P/L/8P/g/8')]
    procedure TestBitmapColorKeyAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+9v7NAACgpb///t1Q0fO3v///3frX+/e///////f////r///v/f/9f8R8fm/nfTf7J//nd+9+/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+9v7NAACgpb///t1Q0fO3v///3frX+/f///////f7//vr+/vr/fv9e8R4eG0nfTf7J/bndu9w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v5dSDAgp7///l1Y8fO3v///f1/18/f///////f79/tr+2vrfcNcUgRAIG0mfSd5JnYmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vx8fDDgw////Hx88fPT///8/n3589/////////////r/+vv2+fIV9HE4e0h+ef4ZnImIOZg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/Hx8fHfjw///fHx89/PT///8/n3/99////////////vr++nv/e/dV9BcYG+nfWd5ZnKmMOZx/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AAAAAAAAAAB/fHBhQ0dOTH9+cunX5158///////v///vr+//v/e/9X8R8YG+nfSd7Jn/mdu5w/8')]
    procedure TestBitmapContrastEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,j4cHB////////3dn/////////3f///////////////+gfyB/0H/v//////////////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,Hx////////9/f////////39///////////////////////////////////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,Hx8f//////9/f3///////39/f/////////////////+7//////////////////////////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,+/vxcWAAgv////FxY0fO////8/l3987///////f/7//vp++nr7ePcV0HUYO/j/af57////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,58fDAwf///////NjR///////92df////////f3////97P3ifCB+sH6z///////////////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,Dw8P//////////////////////////////////////9B//////////////////////////////8')]
    procedure TestBitmapCropEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,ff3h5uChBwt//fHn4+dPz////+/z/0////////P////yQ/7+/Xz6PPJJvAzgv/2vmD+JP4X/g+8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,4eDTBwcBx87/8PPnR0fPzv/0//ffT+/f//f////P/9//jfr+oX+mt0A/yT+Il2KqovPQ+vD98Lg')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,mI+DozAT/////+Pj89f/////8/Pz///////79/v///8ed60XfROCV2kU99TzOedvSe6p//z/hcc')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,//zgQOKAw/f//PDh48fP///+8vP73+/////////////8zP/MWzQiIz5cNFwACF6RTjNDM8A/xD8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/n5dXDAgo7//fl1c8fOzv///33/587+///////v////9/+3v7efsf2BkYOfmfcf8x7qnMucw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/v59XBAgoL///n1c0fOzv//+/9/987e/////////9//5/+n/7eft98x0QOXg7ef9h/qnNuYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/PzcWFBghT///NxY0fOXP//8/t31999///////f////j//v/28/Z38jowM/h/4/5j/UOZM5g/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,8/PzcWGBBB////NxY8dOX///+/l3109////7//ff////7/9vP2e/Z+MDKwP+P+4/1jnXOYf/h/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/nxcGCCgp///fFxY8fO3/////13x97///////fH////t/+3n7PdEdGDl4v3H+If65zLncP/w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+fn5+DDAgIr//fn5c8fOzv//+f17x+7+//////vn////4//z3/Hfuf6B2IHfofsP8Q78TuHe4f8')]
    procedure TestBitmapDirectionalBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13597///////fv///vr+mnr7efcV0XUYO/j/ef4I3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+/v5dEDAgJb///l0Q8fO3v//+/1X59////////f///+Dr98vrPQJ9VgXMJeHk/ad/IHYmM+Z2/0')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/89AAEDSenr///DgQ9d+fv//9/Nz3/9+////+/v///+p46ECAEoMXAgKAQFJjXGDMBA6UgEAQsw')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d2P73///////7/////8fgAeAB4AHwD/AAOGzQG+IC4APgABgAAM')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapDissolveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHKCgr/+HDg889OTv/9d/H3z87P////8f///v8QUBBYwVjIj+r/k/9LfxtmH2Y3Zn5mfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/+5gQMDx+w7//nBgw/f/Tv//d3P7///v////+/v///8WcBBwEVgQ2Mnaz/3p+b8ZSxgfBj8kPgA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/84AYMDQ8fn//nBgw9f//f//d3Pz3//9////+/vf//0AAAcwGzAe8Bl4Udjh2aHdAfhDOAs6HxQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS8PH//nBgw9f+/f//d3Pz3//9////+/////0HAAeAB6AX4C/wAfADcGPYAtgi2ANoWxg')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d3Pz3/////////////8fgAeAB4AH4D/wAPAzUC/QA9ANQAJAAEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapDropTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,/evd//zLzo////3//8/Oz///////z+/v////////7+8ZKBlIGVx5RWuBYYkvmWyTDJEXEVKrPmY')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,BGxc3vhIamB/fHz/+09ubH98fv/7z3/tf3x+///ff+0ZeBlIuVx5RWuJYYm/HeyTDJUXFVKvPmY')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,BGxc3vhoauh/fHz/+29u7H99fv/773/tf31+////f+0ZaBnYuVx5VXmJYak/XeyTDJUXEVuvfmY')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,BHxc3vhoamB/fHz++29ubP99fv/773/t/31+////f+0Z6BlYuV55VXmJYak/HezTTJ0XEVuvfmY')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,BHxc3vhpamF/fHz++29ubf99fv/7/2/t/31+////b+0Z6BnYuVx5dXmJYas9XeyTDZkXGXuvfmY')]
    procedure TestBitmapEmbossEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcWDCgp7///FxY8fO3v//8/lz597///////vv///tp+2nr6eNcUkREQG3j/ef5J3bmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgpb/+/FgY8fO3v//8/lzz8////////v////sp+ynrSeMcVgQEAG0hfaf5J3KmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+9gQEDSkpb//3BgQ9fe3v//d3Fz39////////v////sp+ynrCeMdUgREAG0hfSf5BjamIOYw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8rL//nBgw9f+/v//d3Pz3/////////v////sr+ynrScM9UgRAREUkfQd5BjIEMMQ9/8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapFadeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestBitmapFillEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+9v7NAACgpb///t1Q0fO3v///3drX+/e//////vf79/vr++nr+cPcU0AAQCUgfQd5BzIGMMYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+9v7NAACgpb///t1Q0fO3v//+/1X597+//////fv///tp+2nrWcNcU0AAQAUgOQd5BjIGIMYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+9v7NABCgpb///t1Q0fO3v//+/1X597+///////v///vr++vr/cvcV8RMYG0nfSd7JnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+9v7NQACgpb///t1Q0fO3v//+/1X597+//////fv///vr++nr/cPcU0BAYG0kfQd5JjYmMOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+9v7NQACgpb///t1Q0fO3v//+/1X597+//////fv///vr++nr+cPcU0AAQAUgfQd5BjIGMMYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH9+culX5159////////////r///v/f/9f8R8fm/nfTf7J//nd+9+/8')]
    procedure TestBitmapFillRGBEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxcXCAgh7///Fxc8fOXv//+/l3195///////ff////p/+nvye/M/kDsQO/j/af5p3LmcO/g/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/Px8eCAgA////Hx48fOT///+/n7z89v///////P/+//g/+B/wH+AfYD5APAA4R/AH+Af4D/gP8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,//Px8eCAAQ////Hx48dPT/////v7z09v///////f/+//g/+B/wH8AfQB4AGAAwAfAH/Af8B/wP8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,///h4cGAAQ////Hhw8dPT//////3119f//////////////sD8AHwAcABgAMABwAPAB+AP/B/8H8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,///DgYEBAwP///Phw0dPT////+/PT09P///////////g/+ABwAGAAYADgAOAB4APgA+AH8Af4B8')]
    procedure TestBitmapGaussianBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+/vx8fHAgh7///Hx88fOXv//8/n3595////////v////r/+nv/e/df8T8YG8nfSf5J3bnNu9w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+/vx8fHAgh7///Hx88fOXv//8/n3595////////v////r/+nv/e/df8B8QG8lfSd5J3KnMOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/vx8XDAgh7///Hxc8fOXv//8/l3595////////v////r/+nv6e/c18B9QG0hfSd5JzInMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999////////////vp/+nv6e/cV8B8QG0hfSd5JjImIOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3995////////////vp/+nvye/cf8B8QG0heSd4JzInIOYg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vxdXDAgh7///F1c8fOXv//+/13999////////////vr/+nv2cfcV8BEQC0heQd5JjImIOYw/8')]
    procedure TestBitmapGloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,+/vzcTDAgJ////Nxc8fO3///83n359/////////v////r/+nv6efdd0BkYO3jfef5p3LmcO7w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,//PzsYGBgJ////Pxw8fO3///9/nXx//////////f///3j/cP/w/HZ+ALwAvxv/G74LPBv+H///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,///jgYGBw/////Phw8fP////9+vbz9/////////////4n/of+l/yX/kP8Z/4P/AH8T////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,///DgYHBw/////Phw8fP////9+vL39//////////////v/g/+h/8D/wf+E/4b/AP//////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,///DwYHDx/////Phw8fP////9/3P1/////////////////w/+D/8D/AP+C/4D/g///////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,///Dw8PD//////Pjw8f/////8//P1/////////////////w/+B/4D/gP+A/4H/5///////////8')]
    procedure TestBitmapGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,//v/PTBy8JD///99c3f+3P////13//79//////f////vr++n7/fvd+0X8cf8//T/5P/a/8P/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+/v7fTBygJD///t9c3fO3P//+/13/979//////f////vp++n7yfvI/0H8Qf05+T/4P/I/4H/gf8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dTBigJL///l1c2fO3v//+/13/97///////f////vp++nr7fvc/kH8Yf07/T/5P/K/8O/g/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+/vx8eGBjw////Hx48fPT///8/n3589///////fv7//vr++3r/cf9V0REYk/jfcd55j7mPuYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+/vx8fHDhh////Hx88fOX///8/n3595///////f////v/+//D/8f+R34EZgfCL8J/wj/CP8Q//8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXEDghf///F1c0fOX///8/13995///////f////v7+//r/e/8d0x8Zi/jf8d7Jz/nP+Y//8')]
    procedure TestBitmapHueAdjustEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,//PzsSGBgJ////PxY8fO3///+/l3x9/////////f///3j/ev9y/XY/ED0YfWu/C/6LvBs8H///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,///Dg4HBz/////Pjw8fP/////+vTz9/////////////4n/qf+l/rR/kH6If4v/S34f////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,///Dw4HBz/////Pjw8fP/////+vLz8//////////////v/g/+h/yT/AP8I/4D/EP+f////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,///nw8PD//////fjw8f//////+vP7///////////////v/w//D/wH/AP+A/4D/n///////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,///nw8PD//////fjw8f//////+vP7/////////////////8//D/4H/gf/B/4H/////////////8')]
    procedure TestBitmapInnerGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/4uvLAACgpb///9tQ0fO3v///+/j7+7+//////Pv7//vr++nL/cPcQ0QAYAUiPQd5BjYGMMYw/8')]
    procedure TestBitmapInvertEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,/84AAEDSmp///nBgQ9fe3///d3Fz397///9//3P//v8opwgnKCcMdUgREAG1nfef5J3bmNuZ0/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,/84AAEDS+7///nBgQ9f/////d3Nz3///////+/vf//8AAAAHADcH9QURAdkUnb8f7Bn7mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,/84AAMDS+vf//nBgw9f+////d2P73///////7/////8fgAeAB4AHwD/AAOAjQC/JAoABgAAQAf8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapLineTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxfXDgoh7///F9c+fuXv//8/1/9/5////////////vr++nqPeAcUAXUEOxj/+f7x3/mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nz8fDw8J7//fPx8/f+3v//9/fz9/7///////P////jz+OPo4/jh2OBYwGDA8MD4wXX+Nv5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8ubn5+/HwID//ufn78fOzP/+7+//38/f//7///////8+Pj4/Pn88fzx/vP+4/7gf+AHwAPAA0AA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yJiYnLw8PH7//Pj9//98fv//+v////1+///7/////f7j4OPg4+DH8Mfwx/CP8I/4z/yP+J/43/w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,x4eDozMxcXn///Pj8/f9ef///+/39/////////f////mH+Yfxh/PD88Pjw+PB5+Hn4cfhz/DP8I')]
    procedure TestBitmapMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/n96MDAogL//f3pw8fuTv///e3n//5e///97//////+9977fPtwXEBlQGVAZfjv/47PzveMv58')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+fvCwcDg8Yv///Lhw+f/j///9vfz7/+f//////v////4X/AP8AfgB/AH8AeYD8gT9mf/5//E/40')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/87AgMDT+/P//vDhw9f/+///9/P73//7////9/vf//vgAOAA4ADgAOAA8AH4AZgD7g////////8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/86AgMDQ+P7//vDgw9f+/v//9+P73//+////4/vf//7AAMAA4ADgAOAA8ADwAPhA/gD/g////v8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/86AgMDQ8Pv//uDgw9f+////5+P73///////4/vf///AAMAAwADgAOAA8AD4APxA/gD/gP////8')]
    procedure TestBitmapMagnifyTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    procedure TestBitmapMaskToAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    procedure TestBitmapMonochromeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,/+vgYGDSgpb/+/BgY9fO3v//93Fz38////////v////sp+ynrSeMcVgQEAC0hfaf5J3KmIOYw/8')]
    procedure TestBitmapNormalBlendEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7JzanMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5fHDAhh7///l9c8fOXv////13595+///////v////r//nv/efdV8RUQG8jfSd7J3anMO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/v5fHDAhp7///l9c8fO3v////13597+///////v////r/+nv/efdX8RUQG0jfSd7J3anMO9w/8')]
    procedure TestBitmapPaperSketchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,fn7///////9+fv///////35+////////fn7///////8AQFNYswrile6R/X2+vUjnHIMkXTSlQGY')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,fn7///3///9+fv///f///35+////////fn7///////8IQNAQ4M/xzX+R/fm/leDHHa83nTS1QGY')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,fv7///Hv7/9+/v//8e///37+///1////fv7///3///8OoOHCscf/1W+x8fm/nfzdTJsfnR8zQO8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,fv7///Ht7/9+/v//8e///37+///1////fv7///3////doHz2tee/vW8x8fm/nfydbJkfmRk5AO8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,fv////Hh//9+////8e///37////17///fv////3v///8w+yn/P++fW8R8bG/nf2f7JldmRu5AZ8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/v////Htz///////9+/P////////78//////////7//iJ+5nrncOfX8Ycek/nfSd/5j/mV84A+Y')]
    procedure TestBitmapPencilStrokeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,//PjgwM///////PjQ3//////8+9Xf//////////////yH/6P/B/mf+nfxf+P/5////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,nw8P////////f3//////////f/////////////////+D/5v///////////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,+/FxYIACn////XFhw0ff////e3XHR//////7/+fP///vp693bRFhg7+P55/BncO5w/////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,/w+HB7//////f/dn//////9//2/////////////////2f+D/6//B/9////////////////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,78cDA////////3Nj////////f+/////////////////A/9s/hj8f//////////////////////8')]
    procedure TestBitmapPerspectiveTransformEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxZQDQsh7///FlQ9f+Xv//8+1X9/5/////7/f////vr++nr7eWcV+HUYOzT/+f7x3bmNOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,Oztzf39/Px5/f3N/f39/Hn//9////39+//////////+/37/fP78//7//v7+//5//3//P/4e+gfw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,+/vzcXDAgh7///Nxc8fOXv//8/l3995///////f////vr++/r6ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,wPj9////f3///P3///9/f//9//////////3///////////////////////v/+/////9/+3//f/s')]
    procedure TestBitmapPinchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//+31/585////////v7//vr++nr/ePdYgRgYG/j/cf7x3rmNu7w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7efcV0DUYe/j/ef553bmMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7efcV0HUYe/j/ef553bmNu5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0TEYO/j/ed5pnbmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    procedure TestBitmapPixelateEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNO5w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13597///////fv///vr++nr7evcV0D8YO/j/ef5p3bmduZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////6/////8/gAeAB4AHwD/AAOAjYK/IA4ANgABAAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8/gAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d3P73/////////////8fgAOAB4AHwD/AAOAzQC/IC4APgAJAAAA')]
    procedure TestBitmapPixelateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/vxcXDAgp7///Fxc8fO3v//8/l359////////fv///vr++nr7efdd0RkYO/jfef5J3bmcO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/vxcXDAgp////Fxc8fO3///83n359/////////v////r/+nv6efd90BkYG/j/ef5p3LmcO/w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,+/vx8XDAgp////Hxc8fO3///+/n359/////////v////r/+nv6efd/kBkYHej/Yf6p3rmcP/w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/v58XDCgp////nxc8fO3///+/l359/////////v////r/+n37ffNfkB04P/H/Id6p3pmcP/4/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/v58aDCgo////nx48fOz///+/3/59/////////v////h/+n37ffsfgD28f6H/Id4Rnh/+H/4f8')]
    procedure TestBitmapRadialBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    procedure TestBitmapRasterEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,8XAAAP//////fHBg//////9/9XX////////1///////RA/aP4pnD//////////////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,8WAAAP//////fHBg//////9/9XX////////1///////RA/aP45nD//////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,8XAAAJ//////fHBg3/////9/9XXf///////1///////RA/Sfw5nD/9P///////////////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,8XAAAB//////fHBgX/////9/9XVf///////1///////Rg/Sfw5nD/8P/+/////////////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,8XAAAA//////fHBgT/////9/9XVf///////1///////Rg/Sfw5nD/8P/6/////+f//////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,8XAABg7w////fHBmT/f///9/9Xdf9//////1///3///Rg/Sfw5nD/8P/4//37/+H//f///////8')]
    procedure TestBitmapReflectionEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALengthProgress, AOpacity, AOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,+/vz8YDAgAb///Pxw8fODv////nr794P///////v/9/+j/+XuV/3gdEB0AH9cfNf4D/zOMtes/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,+fv3w0DAiI////fjQ8fOj///9+dz9/+/////9/v/////r/yfe596U/cjTKJpUuF34cvA/9P/w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,+/v3x0HAgIL///fnQ8fOzv////f7997u//////v//u//FfRt/N39BfStuGDoAvD4wAXB+4Dvwf8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,+///byDAgAb///9vY8fOTv///2/3999O////7///38/97/3/bY9uRv40uXmoQ/D+6DnBg9D+gz8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,///vYyMDggL///9jY0fOTv////9//95+/////////v797/0v/lf+X5avmS+RD+BH4J7Aw8H/g98')]
    procedure TestBitmapRippleEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdGDAgh7///F0Y8fOXv//8313185////////f/v/tp+ynr7etcVgVEYO2j/Wf5J3LmMO5w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgp7///FgY8fO3v//83Fz78////////P////sp+ynrCOtcW0AoAG2hfSf4I3ImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/89gAMDSkpL//3Bgw9fe3v//d/P7397///////v//v/sr/2vPSccMVgBEAE1mfSd4B3bGNuZw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8rb//nBgw9f+/v//d2H73/////////v////9r+y3nnce+RgRAMEQifyZ5BDLEMPY1/k')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapRippleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nx8GDCgpb//fHxY8fO3v//8/Fz18////////v/7//tp/1n/XU9IXwBYQO1vfcf5R3dGMM41/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,//PhQOCSghv/+/Fg49fOX///+3nz385////7//P//3/5T/9nPWcxcaABawF/H8kO1QyTnIGcn/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+/gAEDAmBL///BgQ8feXv//92Vj395//////////v/47+6H7oMusy6SaIAggLTP4IzUjIOJh58')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS8vL//nBgw9f+/v//d3P73/////////v////9r+2nPWc9dQgRARktgXcZxRnQGIMQ+9k')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapRotateCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vx8XHAiA7///Hxc8fOTv//8/l3195/////////////////v/8f/V/9E4u/j/Wd7Zn/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/nx8XHZnA///fHxc9/eT///8/l3/95/////////////////v/8//V/5EZm/jf2d7Zn/mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,+/vx8XHYmA7///Hxc9/eTv//8/l3/95/////////////////v/8//X/5MZ2/nfyd/Jn/mPu5//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,+/Hx8XOQiAj//fHxc9fOTP//8/n39959////////////7///v/+/9X+xcYW/nfyd7Jn/mdu5//8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapSaturateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5dXDAgp7///l1c8fO3v//+/13997///////f////vr++nr7ePcU0BUYG0j/Sf5J3KmMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dWDAgp7///l1Y8fO3v//+/13997////////////vr++nr6e/dV8BUQG0hfSf5J3ImMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dSDAgp7///l1Y8fO3v//+/13997////////////vr++nr+e/dV8B0QG0heSf5J3ImIO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+9v5NSDAgp7///l1Y8fO3v//+/13997////////////vr++nr+evdV8B0QG0lfSf5J3ImMO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+9v5NQBCgpb///l1Q0fO3v//+X1f99/////////////vr+//r/ev9c8R8YG8nfSd7J3bnNu5w/8')]
    procedure TestBitmapSepiaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.4,0.99,+/vxcXDAgp7///Fxc8fO3v//+3n3597///////fv////r/+nvyefcd0HkQe1j+cf4Z3JmIOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.5,0.99,+/vxsTDAgI////Hxc8fOz///+/1/597////////v///vj/6n36eeMb0DkQfVH/Ef4R3JmMH/w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.6,0.99,//vzsaGAgI////Px48fOz///+/v/x//////////P///zj/av9qfGJ+QDwAPRA/Ef4RngmeH///8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.8,0.99,//PzgYGBgd////Phw8fP3///9+vbx8/////////f///wn/CP9A/UR8QHwAfQh/Eb4APht+H///8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.9,0.99,//PjgYGBgd////Phw8fP3///9/vTz+/f///////////xH/Af9B/ER8AHwCfgB/An4CPhL/////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,1.0,0.99,//fDgYGBw/////Phw8fP////9/fTz//////////////xP/Af5B/AH/gHwAfwB+AH4E////////8')]
    procedure TestBitmapShadowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vjwcHDhp////Phw8fO3///9+P7z97///////v////sH+gfuAeABVAHUAOwD/wf/J3/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/86AAEDS85P//vBhQ9f/n///92N73/////////vf///AAIABAAEAAQABAAGAAcJBwA/gD/4P/98')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapShapeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13585+//////fv7//vp++nv7efMV0HEQe2h/cf4x3KmMeYg/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vp++nvzefMVkHEYe0h+cf4x3KmIOYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vr++nvyefMVkDUYe0h+cf4x3LmIOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vr++nP2efMVkDUYe0h+cf4x3bmIOYg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXCBgh////Fxc8fOX///8/l3595///////fv///vr++nr2efcVlBUYO1j+ef4R3bmIOYh/8')]
    procedure TestBitmapSharpenEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,8/Lq4MAEHP///vrhw0de///++/nPb/7///77//////9//n7q+2LjJn8y/j7+Ov4w/nKP/ngAAAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,5vTAwAgofhn//PDhS29+Hf/88vNbf36d//z/+/////3/6O/Yvdj86Pj4/Oj4wP3Y//gAAEAAAAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,+OiIGDD4/3L//Pj58/7/fv/8+fn7/v9///z/+f/+////4H/g5+Dn4Oeg5gDmAP/g/+AQAAAAAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,8KAg+eHw/8z//nD54/f/zP/+9/3j///u//7//f///+7+EP4A/gD+BP4A/gD+AP4A/hoB8wEiAEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0MDQ8PTc//P//PDw99//8//8+Pb33//7//z49vff//vwAPAA8ADwQPAQ8ADgAPAA8AD//5+xAwE')]
    procedure TestBitmapSlideTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxfXDggh7///F9c+fOXv//+/1//95////////////vr+6nuPeQ8UAXYAOxj//f753/mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nz8fDw8J7//fPx8/f+3v//9/fz9/7f//////v//9/jz+OPo4/jhWOBYwGDA+MD8wXf6Nf5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8ubn5+/HwID//vfn78fOzP/+/+//38/f//7///////++Pj4/Pn88f7x/vP+4/7gf+AHwAPAA0AA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yJiYnLw8PH7//Pj9//98fv//+v////1+///7/////f7j4OPg4+DH8Mfwx/CP8I/4z/yP+J/43/w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,x4eDozMxcXn///Pj8/f9ef///+/39/////////f////mH+Yfxh/PD88Pjw+PB5+Hn4cfhz/DP8I')]
    procedure TestBitmapSmoothMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,+/vxdXDAgp7/+/F1c8fO3v/78/13997//////3f///9vr++nr/efcV0XUYO/j/ef5p37mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,+3t5dXDAgp7/e3l1c8fO3v97e/13997///9//3f/3/8Hrw+/r/ef8V0XUYO/j/ef7537mNuZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/vz8dHDAgp7//Px1c8fO3v////d399//////9//3///vwO+Av4Af9V//EZu/n/+d/5n/mP+Z//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,+/v5dXDAgJD///l1c8fO3P//+/1399/9///////////vr+//r/cf8V0XEYu/h/eQ74D7gPuAwAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,+/v5dXDCggD///l1c8fOTP//+/1398/t/////////+/v/+//v/cf9V//EZu/n/+d/5kAAAAAAAA')]
    procedure TestBitmapSwipeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,9/PxcXBCg5////Fxc0fP3///+XF359//////f/f/3///a/1n/WcvB20PcZw/nPeZ5p/rn9v/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,8/Px8HBCg4////Hxc0fPz///8fl359/f/////////9/97//h73OvQa0HUY5fnLcd9h/rn+v/4f8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,9/Px8HBCw8////Hxc0fPz///8/l398//////////7//Nr/+l//fvd60DkY/Xnp8Om43xz/X/9P8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,+/vx8PBiwsv///Hx82fOz///8/n3d87////////////nr++n77Xvc/8DmYOzn78Om4zaz/XP9P8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,+/vx8PCwoIH///Hx8/fuzf//8/n//+7P/////////8/zr+en77f/sf6D9oHMz8nP247Nx+zn/v8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,+/v58PDwoIH///nx8/fuzf//+/3///7v/////////+/9p/un97f+sf6T9oP2z/zP6Y7rjvXP5O8')]
    procedure TestBitmapSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,//vxcGDCgo////FwY8fOz///83lz587f//////v/7//tr+2n7SetcckB0AWXjbee5JzqnOmf4f8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,/+Pg4GDCw8f/+/DgY8fPz///8/lz78/v//////v////Nj9wl7SXsMZgAkAbWnp6O0o/wR/L/+H8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,/+ZAQMDS8/P//nBgw9f/////dXHz3//////////////piZ0hjCDMMOwD2B+Sj6qHygPIP+U//g8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,/85AAMDS8vD//nBgw9f+/P//dXPz3/////////v////hQM1g3GGcd+wH6A/AgcqDxB/ikPEj/P8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,AP///////wB/////////TH////////9Mf/////////8AAAAA//8AAAAA//8AAP//AAAAAP//AAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,UlV+VX9Vf1T//f71f1f/3P/9//X/9//9//3/9//////d3TMz3d0zM93dMzPd3TMz3d0zM93dMzM')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,Wn5aWlpaSlr//nr7299O3v//fv/f317e//9//9/f//9VVVVVVVVVVf//VVUAAP//VVVVVVVVVVQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,BBYXBl8GHgT/fvfmX8deTP9//+9fz19N/3///1/ff29ERP//VVUiIkRE//9VVSIiRET//1VVIiI')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,AfsBaSlhIf9//2Hpa2dv////4+v77+/////j6//////VVdVVgAAAAAAAAAD/////1VXVVYAAAAA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,//2k/wD/pK3//eT/Q//urf//9/9T///////3/1P///8REVVVERFVVRERVVUREVVVERFVVRER//8')]
    procedure TestBitmapTilerEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1318////////ff7//vr++nrycPcU0FAQe2h+Yf5JjImoOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5dXDCgpb///l1c8fO3v//+/1/587+///////v7//vr++nr2ePcU0HQQO2heaf5J3ImIOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dXDAgp7///l1c8fO3v//+/1318/////////f7//vr++nr/cPcU0FEYO2h/ad5JnImMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dXDAgp7///l1c8fO3v//+/1399////////f////vr++nr+evcckH8YO2h/af5J3InMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5dXDAgp7///l1c8fO3v//+/13597///////fv///vp++nr7ePcV0VUYO2j/af5J3amMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr/cPcV0HEYO+j/ad5JnbmMOZw/8')]
    procedure TestBitmapToonsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHKCgr/+HDg889OTv/9d/H3z87P////8f///v8QUBBYwVjIj+r/k/9LfxtmH2Y3Zn5mfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,//xAwODaGhD//HDg499eXP//f/Dz39/f/////P////8xJkFnYv9j/p/fo53/BRuHPwQ+AHwkcAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+ZAYMDSGx///nBgw9dfX///d3Dz319/////8Pv/f38XOAUYHx03/T79f55/CX+B3wD/AP4I5gI')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDy+nL//nBgw/f+fv//d3fz//9///////////8BhAOGBtYU0xx5BHlAf4IZBuBf4P3g4AA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS+vL//nBgw9f+/v//d3P73/////////////8XmAPgB4AHwifCCOoDYAfqAnAEYABABEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapWaterTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13185///////ff7//vr++nr/evcV0TUYO/j/ef5p3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/33197///////ff///vr++nr/evcfkHMYO/j/ed5537mMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13595///////fv///vp++nv7e/cV0DEYO/j/ef5537nNOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13185///////ff7//vr++nr7evdV0HcYO/j/ef5p3bmduZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr7cPcV0HEYe/j/ed55nrmNuYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/vxdXCAgh7///F1c8fOXv//8/13595///////fv///vr++nr/e/cV0HUYO/j/ef5p3bmNO5w/8')]
    procedure TestBitmapWaveEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcGDCgp////FwY8fO3///8/lz587///////v/7//tr+2nraetcUkRUAG3j/ef5JzqmMufw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgp//+/FgY8fO3///83lzz87f//////v/7//sp+yn7SescdgAUAGUjbae5JzqmOmf4f8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5gQEDS0pv//nBgQ9fe3///d3Fz3/7f//////v////sp+yn7CWsdJgAUACUnbSe8JzqjOnf4f8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8vP//nBgw9f+////d3Hz3/////////v////tr+yv7CWMcQgRAREUnZQclAjije3P9f8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapWaveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nxdGDAgZ7//fF1Y8ePnv//9X1z19/+///9///f///tt+2zvzOdcfkRYAG1gfWP5o/ZjsO+w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+vgYGDCgwf///BhY8fPT///9Xlz78/f///9//v////Mo/6z7DecM9gDEAI0mXS/wLeKn5q5I7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+hgQMDQkfv/+PDgw9ff////9/Xz/9////////v////8p8SgHDE8cLgBoAJEn2AT4BvpG+u/v98')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/+9AQMDScv7//3Bgw9d+/v//dnPz33/+///////////sn+yPHG88dRgQAQwggPxfdBxBGNHY9/w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestBitmapWiggleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,58fnh4fP//////fnx8/////////////////////////+f/5/73/tf/z/+f/5//////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,x8fP//////////////////////////////////////////////////////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,+fP35+fn8/H///fn5+f//f///+/v7///////7//v////v/9//3/+f/5//3//f/9//7//v/+f/48')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,8+fP38/Px+P/////z8/P7///////38/v///////fz+8EAAQACAAIAAgACgAIAA8AAYAA4ABwAAg')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,98/P34+PhwH/////z8/PTf//////399v///////////5//n/8//z//P/4//Z/+v/4f/mv8Jvw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,8/fnx8fHw4H///fnx8fPzf/////fz8/v///////////+f/5//n/+//z//n/6f/r/+l/yb+N3w/8')]
    procedure TestBitmapWrapEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress, ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress, ARightStartProgress, ARightControl1Progress, ARightControl2Progress, ARightEndProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,+5uLzYGBgJ////vtw8fM3///++3T59z///////Pv/f/tr+9vv3OLcUEDUIO8j8QPygHImNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,+/vxdXDBggb///F1c8fOTv//8/13589P//////fv/+/vr++3r/ef8V0GUQq+BfYL5Bn4ENgJwG8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,+/j58eCAgJ///Pnx48fO3//+//f3197///////ff///vuu/Hr4efBVwAUICxgOIH7An7mduJwvw')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,+/v5cHDQhJ7///lxc9fO3v//+/l/197////////f///vr++nr+GfcF0QUAC8C/Sb4Zj7iNqSw/M')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    procedure TestControlAffineTransformEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/uBATERkJ3///Fhc1fe3f//+3Xz3979//////vf3//gr883mbcIcQ8VUYMch/Qf7B37FNKxw/c')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+8OVFRDBgp////V1U8fO3///9/1X9/////////f////jr/Mnj7Wn8RkDEYONh+CV6AnKGYu9w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,+/Px8eDAgp7///Hx48fO3v//8fn/x97////////P///7rf+nrfe88V0DUAe0B/Sf5J36mNu5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,+/HxcXDDgp7//fFxc8fOnv//8/l3597//////f/v///vr++nLoe/cV8X0Yc2j/Sd4I34mNuZw/s')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,+fLx8XDCgh7//vHxc8fOXv/+9/l3585///////fv///vr++nr5efMV0SUYW8j+Yf553bmNOZw/8')]
    procedure TestControlBandedSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,/+Pw8OCAgob/+/Dx48fOzv//8/nzz8/v/////fv/7++Oxauhr+WM8VgVEBkThS8trgncGNuVxP8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,/+5AYODyuhD//nBg4/f+XP//dvL7///9//////v////oxcymjDcIsWgTEAG4LeofxBjLQNMY8ZM')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/+5AQMDS+rL//nBgw9f+/v//d3H73/////////v///+s1SbjD7eO+QTxAZgEgL9LVFigEPAA/gM')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/85AAMDS+vL//nBgw9f+/v//d3Pz3/////////f///8X0AeQB5AHxC/oAPACAAaJAAAAgAAAIAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlBandedSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vxdWDAgh7///F1Y8fOXv//8/13195///////f/////p//nv/cf8R0nEIMch+6b4pnamMOZx/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v5dTDCgp7///l1c8fO3v//+397197e//////vf/v/+r/6nPrcecRwCEII0h/Yd4hjIGIMYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/v7NSACgh////t1Y0fOX///+313589///////fv7//vp++nLycfdRkREYEUlfSd4JnIGIMZg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/v7fSDCgp7///t9Y8fO3v//+31/787e///////v79/vr++vL2cPdQ0CEQIWnvQd4BjKGIM4g/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+9v7fSDCgp7///t9Y8fO3v//+313587+//////fv7//tr+2nLSctdQ0BMQE0hfSd4JnIGMMZg/8')]
    procedure TestControlBandsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vzcTDAgJ////Nxc8fO3///83n359/////////v///3r/+vv6efdd0RkYOXifef5pnLmcO7w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vzcTDAgJ////Nxc8fO3///8/n359/////////v///3r/+nv6efdd0RkYOXgfef4pnqmcG7w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/vzcSDAgJ////NxY8fO3///8/v319/////////f///+r/6nvreeddQBkIOWjfef4pnLmcG///8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vzcTDAgJ////Nxc8fO3///83n3x8////////fP7///r/+nv+cfddEBkYO2mfef65nbmcP///8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/vzcXHBgJ////Nxc8fO3///83X3587///////f/7//tr+3vrW8NZUkBAYG3neeb5ZnbmcO7//8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,//vzcTDAgJ////Nxc8fO3///83n359/////////////3r/+vv6efdd8RgQOXjfef5pnqmcO7w/8')]
    procedure TestControlBevelEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress, ASizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vxcWDAgp7///FxY8fO3v//8/lz59////////Pv//8AAP//CAQP8V0XAAC/n+CM/5nIgP+ZAcA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+vxYGDCgpL///FgY8fO3v//82t779////////v////sp++nr+cccVkBEAC/n7AM/5nIAP+5AAQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+5gQEDSkpL//nBgQ9fe3v//d/Fz39////////P////v/wAAv/cf/QAAEZkUGLed/5kAAP+Yw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AAMDS8vL//nBgw9f+/v//d2Pz3/////////v////v7++nAAEP9U0RAKEAALwd7BgAANsY1/8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlBlindTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,EADx8fHx8fF/ePHx8/f//X97+fv39//9f3v5+/f3//1DIO/3v/+d/Vf/Udu/n7+f3/9d3x/f358')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,AEDwcHD7+9l/fPBxc///3X9/+X37///df3/5ffv//90AAAAAAAAGAA4Aka3/v/f/////3/////8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,zkRAwtLy+v/+dGDi0/f+//53c+v7//////f36/////8HgA+ABsAGwAJoCUADAALACABJCN2e3/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlBloodTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+9v5NQBAgp7///l1Q0fO3v///Xf7X8/f//////vf79/vr++vr/cPcU0RAYE0mfQd5JjYmMOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+9v5NQDAgp7///l1Q8fO3v///f1f18/f///////f79/vr++nr/cPcU0BAYA0kfQd5BjYmMOYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/v5dSDAgp7///l1Y8fO3v//+/1399////////f////vr++nr+cPcU0BEQC0gfQd5BjImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1399/////////////vr++nr+ePcU8BEQG0hfQd5JjImIOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999////////////vr++nr6e/cX8BcQG0hfSd5JnImMO5g/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vx8XHAgh7///Hxc8fOXv//8/l3995/////////////r/+nv6e/c18B/QG0hfSd5JjImMOZg/8')]
    procedure TestControlBloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/Pz8aGBgJ////Px48fO3/////n3x8/////////f///3j/+v3++PY4AHgQffH/Mb6xHjM+P///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,//PjoYGBgf////Phw8fP/////+vbz//////////////5n/qf/g/mD+AP4A/gN/An8P/z//////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.99,//fjw4GBx/////Pjw8fP///////bz9//////////////v/wf/A/0D/AP8B/gL/D/+P////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.99,///jw8PD//////Pjw8f////////fz///////////////v/wf/A/4D/gf8D/gX/h//P////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,///jw8PD//////Pjw8f////////Pz/////////////////w//A/8D/Q/8D/wf/x///////////8')]
    procedure TestControlBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/nxcGDCghb/+fFwY8fOXv/78/hzx99///////P////8p+ynLSMMcQgRAAE3j/+d55jbmMOYg/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+hgQODSihL/+HBg49fOXv//d3Dz399///////P////s5+yjDSMM+AgQABkHiX8NxwjTCJEI95w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+xAQODSuhL//HBg49f+Xv//d3Pz3//f//////P////t882jDaMN+g2cARwFBD8MZQibAMEA/wg')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AQMDS+vL//nBgw9f+/v//d3P73/////////////8X0AfQB4AHxC/4AfACAAzAAAAAAAAA9AA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlBlurTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0HEYO/j/ed55nbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPcU0HEYe/j/ed55nbmNOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPcU0HEYe3j/ed55nLmNOZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3999///////f////vp++nr7cPcU0HAYe3j/ed55nLmMuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXCAgg7///Fxc8fOTv//+3l3989///////f/7//vp++nr7cPcR0DEQc3j/ed55nKmMOYw/8')]
    procedure TestControlBoxBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNOZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxYGDCgh7/+/FgY8fOXv//8/lzx85///////v//v/sp+ynrSeMMVgBEAG3j/ef5J3LmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+pgQODSkpb/+nBg49fe3v//d3Hz39////////v////sp+ynrAccNVgREAE1hf8d5BjLGIMYw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5AQMDSupL//nBgw9f+3v//d3Pz3/////////v////sp+wnjCcM/QgRABEVmb8dZBiTAMEA094')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS+vL//nBgw9f+/v//d3Pz3/////////////8X0AeQB4AHwC/4AfAjACyAAAAAAAAAdAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlBrightTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,+/vxdUDAgp7///F1Q8fO3v//8/13x9////////fP///vr++nr/cdcVgXEAOwj/ad5pn7mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,++vBwcHDgp////Hhw8fO3///9/Hzz8////////v////oh+AHoAcIAVgBEAOwB/Id4Bn7mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHwD/AAOCjQC/IA4CFgOBA+AA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////6/////+XAAeAB4AHwC/AAODjQIfAgICggPAA/gA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlCircleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNOZw/8')]
    procedure TestControlColorKeyAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+9v7NAACgpb///v1w8fO3v////fr3+/f//////vf7//vr+/vv/c/9V8RMYG8nfSd7JnbmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+9v7NQACgpb///v1w8fO3v////fr3+/f//////vf7//vr++vr/cvdW0RIYG0nfSd5JnbmcO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v5dSDAgp7///l1Y8fO3v///f1/18/f///////f79/tr+2vrfcNcUgRAIG0mfSd5JnYmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3595////////////vr/+vP28fYR9HE4c0h+cZ45nYmIeR/f8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+7vxcSEfnBz///FxY1/cXP//8/l3/958///////////vr++nP/c/dR1BMYE0lfWd5ZnamMMZx/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+7v/Zy8c3Jz///9nb1/c3P///29//978///////////v/+/vP/c/9R8RMcU9nfSZ/pnbmd+5x/8')]
    procedure TestControlContrastEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,y8OBBfDDgp////Fl88fO3/////33997///////f//v/gLyBnsfePcV0XUYO/j/ef5p37mNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997////3//f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997////3//f////rr++nr7efcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,4/PBwSDDgp////HhY8fO3///8+d3997///////f////rr+mHCRfcERwHUYO/j/ef5537mNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,25uZ9XDCgp7///n18cbO3v//+/31997////7//3/3/9Br++nr/efcV0XUYO/j/ef5p37mNuZw/8')]
    procedure TestControlCropEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,/fnh4ODgAgv//fHh4+dOz////+nz/0////////P////w9//2vfaPMElBEAYAh/cP6x2ZGIGZw+8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,6fnRxWGAho7/+fHlY8fOzv/9++Xz787f////5/P//t/sqej3I38G9Sh5BwM9pf1JtxzRHP288L8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,mIeD4zAz/5///+Pj8/f/n///8/Pz///f////8/v///9N5y3zDfMD5QniAfEBx4APAIsJLv9Z//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,//zgQOKAg/f//PDh48eP///+9vP73+/////+////////4P2AAeID0APIB+gM4FxADgAHAAAAAA8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/nxcXDAgo7//fFxc8fOzv///3l/587////////v7v/u5+63nrePsQ0RAYOXC/cN4wjrmPuYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/vxdXBAgoL///F1c0fOzv//8/1/98//////////7//vr++nr7efsR0TEQOXg7ef5xnrmMuYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/PxcWDAghT///FxY8fOXP//83l31899//////ff7//tr+2nrWcNZQ0DAYMnh/Yf5hnSGNMwg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,+/Px8WCAAB////HxY8dOX///+/l3x09///////fP7//tr+2vLecNdQEDA5c+D+YZ5hnaEIO5g/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/n5cGCCgp///flxY8fO3///+3n3x9/////7//fP///vp6+3n7ENkxEDEY+3HfMc45jJmMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+fn58HCAgI///fnxc8fOz///+/1/x87////////H///vp/+jj/OP8QwBAIGHg/8P4wz7CPuIwf8')]
    procedure TestControlDirectionalBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr+mnr/cfcV0XEYO/j/ed4Yn7mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+/v5dEDAgJb///l0Q8fO3v//+/1X59////////f///+Dr98vrPQJ9VgXMJeHk/ad/IHYmM+Z2/0')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/89AAEDSenr///DgQ9d+fv//9/Nz3/9+////+/v///+p46ECAEoMXAgKAQFJjXGDMBA6UgEAQsw')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d2P73///////7/////8fgAeAB4AHwD/AAOGzQG+IC4APgABgAAM')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlDissolveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHaDgr/+HDg899OTv/9d/H338/P////8f////8QUBBYQdhAjqr/8v9LfxtiH2Y3ZnxmfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/+5gQMDx2w7//nBgw/ffTv//d3H7/9/v////+/v///8WcAJwEVgS2GGY4ezh+feZSxofAjcmPiY')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/84AYMDQ8fn//nBgw9f//f//d3Pz3//9////+//f//0CAAewB7AH8At4QNjj2OHdAPpjOAs6GyI')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS8PD//nBgw9f+/P//d3Pz3//9////+/////0XAAeAB4AH4C/wAPBjUGfQAtgkyANoWxg')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d3Pz3/////////////8fgAeAB4AH4D/wAPAzUC/QA9ANQAJAAEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlDropTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+end3WBIioD//f39Y0/OzP/9//33T8/s//3//fdfz+wBGABIAFggVAAhAQgACQygDIEVgBeABAI')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,JEjc3vhISoB/fPz/+09OzH98/v/7z0/tf3z+//vfT+0NeBlImewbRLmBKYlPPyyzPbA3OHJRPgM')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,JGjc3vxISoB/fPz//09OzH99/v//z0/tf33+///fT+0ZaBtYG0w71fuJselvWzzzHbkXM1qxfuc')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,JGjc3vxIaoB/ePz+/09ujP99/v//32+t/33+////b60beBtYG867xfuJsbltH3yzHbkXMXu5fuY')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,BHhc3vhoagB/eHz++29uDP99fv/7f2+t/31+//t/f60Z6BnYOV25dfmL8attXeyXDJsXGRu5PmY')]
    procedure TestControlEmbossEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxYGDCgh7/+/FgY8fOXv//8/lzx89///////v////sp+ynrScMMVgREAG3j/ed5pnLmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+pgQODSkpb/+nBg49fe3v//d/Hz39////////v////sp+ynrCcMNVgREAEVhf8d5BjLGIMYw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5AQMDSupL//nBgw9f+3v//d3Pz3/////////v////sp+ynDCcM/QgRABEVGb8ZZBiTAMEA894')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS+vL//nBgw9f+/v//d3Pz3/////////////8X0AeQB4AHwC/4AfAjACyAAAAAAAAAdAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlFadeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,8fHx8fH5Cg7//fHx8/9OTv//8/n3/15///////f////tr+2nrfeNcQ0QAYAUifQd5B3YGMMZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+/Hx8fHBig7//fHx88fOTv//+/n3999v//////f//+/sr+ynTOcMcQgAAAAUgKQZ5AjIGIMaw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+/vx8fHAgg7///Hx88fOTv///f3/19/P///9/f/3398TUBNQ0wjzjvPv/n/rZkviG+cn5zznPAA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+/vx8XDAgh7///Hxc8fOXv///f1719/f///9/X/X398TABMAUwDzBvMH/mfoZkLiAucA5wDnAAA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+/vxcXDAgh7///Fxc8fOXv///XV718/f//////vf/9//J/8n3yX/IrMG/2b0YqTrwO+I54D3gds')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestControlFillEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+9v5NQCAgp7///l1Q8fO3v//+/1X99////////f////vr++nr+cPcQ0BAQG0kfQd5BnImMOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+9v5NQAAgp7///l1Q0fO3v//+/1X597////////v///vr++nr+cPcU0BEQE0kfQd5JjImMOYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+9v5NQAAgp7///l1Q0fO3v//+/1X597///////fv///vr++nr/cPcU0RAYE0mfQd5BjamMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+9v5NQCAgp7///l1Q8fO3v//+/1X597///////fv///vr++nr+ePcU0AAQC0gfQd5BzImMOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+9v5NQCAgp7///l1Q8fO3v//+/1X597///////fv///vr++nr+cPcUwAAACUgeQd4BzImIOYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vzdTccmBz///N1d1/eXP//8/13/959////////////////v/e//X+x8b2/nfyf/Jn7mf+5//8')]
    procedure TestControlFillRGBEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxcXCAgh7///Fxc8fOXv//+3l/189////////f7//vr++nr/cPcQkHAQc3j/cd5xn7GMMYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/Hx8eCAgA7//fHx48fOTv//+/n7x89v//////vP7+/vr++nr/MPAwwDAAckB8wd7Bj8GPqZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/Hx8eCAAA7//fHx48dOTv//+/n7509v//////vv/+//r++nr6cPAwQDAAekB4Qf5An4mPiZwv8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/Px8eCAAAf///Hx48dOT/////33909///////f/////r/+nvQcAA0AHAAeAB4Af4B34mPCZwf8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vx8eCAggb///Hx48fOTv//+/3/995v/////////+/tr+2nrCeIMUAHQAewB+Cf4J3ImMG5w/8')]
    procedure TestControlGaussianBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+/vxcXHAgg7///Fxc8fOTv//8/l3595////////v////r/+vv/e/dX8T8YG8nfSd5JnbmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+/vxcXHAgh7///Fxc8fOXv//8/l3595////////v////r/+nv/e/dX8B/YG8lfSd5JnamMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3995////////////vr/+nv+e/cV8B9QG0hfSd5JjImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999////////////vp/+nv6e/cV8B8QG0hfSd5JjImIOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999////////////vp/+nvye/cV8BcQO0heSd5JjImIOYg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vxcXDAgh7///Fxc8fOXv//+/l3999////////////vr/+nv2efcV8BEQC0heQd5JjImIOYw/8')]
    procedure TestControlGloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,+/vzcTDAgJ////Nxc8fO3///83n359/////////v////r/+nv6efdd0BkYO3jfef5p3LmcO7w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,//PzsYGBgJ////Pxw8fO3///9/nXx//////////f///3j/cP/w/HZ+ALwAvxv/G74LPBv+H///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,///jgYGBw/////Phw8fP////9+vbz9/////////////4n/of+l/yX/kP8Z/4P/AH8T////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,///DgYHBw/////Phw8fP////9+vL39//////////////v/g/+h/8D/wf+E/4b/AP//////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,///DwYHDx/////Phw8fP////9/3P1/////////////////w/+D/8D/AP+C/4D/g///////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,///Dw8PD//////Pjw8f/////8//P1/////////////////w/+B/4D/gP+A/4H/5///////////8')]
    procedure TestControlGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,//v/PTBy0JD///99c3fe3P////13/979//////f////vr++n7/fvd+0X8Yf0//T/5P/a/8P/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+/v7fTBygJD///t9c3fO3P//+/13/979//////f////vp++n7yfvI/0H8Qf05+T/4P/I/4H/gf8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dTBygJD///l1c3fO3P//+/13/979//////f////vp++nr7fvc+kH8Yf05/T/5P/K/8O/g/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+/vx8eGBjw////Hx48fPT///8/n3589///////fv7//vr++3r/cf9V0REYk/jfcd55j7mPuYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+/vx8XGDhh////Hxc8fOX///8/l3595///////f////v/+//D/8f+R34EZgfCL8J/wj/AP8Q//8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXEDghf///F1c0fOX///8/13995///////f////vr+//r/c/8R0xMZg/jf8d7Jj/mP8Y/f8')]
    procedure TestControlHueAdjustEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,//PzsSGBgJ////PxY8fO3///+/l3x9/////////f///3j/ev9y/XY/ED0YfWu/C/6LvBs8H///8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,///Dg4HBz/////Pjw8fP/////+vTz9/////////////4n/qf+l/rR/kH6If4v/S34f////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,///Dw4HBz/////Pjw8fP/////+vLz8//////////////v/g/+h/yT/AP8I/4D/EP+f////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,///nw8PD//////fjw8f//////+vP7///////////////v/w//D/wH/AP+A/4D/n///////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,///nw8PD//////fjw8f//////+vP7/////////////////8//D/4H/gf/B/4H/////////////8')]
    procedure TestControlInnerGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/4uvLAACgpb///9tQ0fO3v///+/j7+7+//////Pv7//vr++nL/cPcQ0QAYAUiPQd5BjYGMMYw/8')]
    procedure TestControlInvertEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,/84AAMDSmp///nBgw9fe3///d3Hz39////////P///8AhwAnACcM/QgZAJkXnb8d/xn/mP+Z//8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,/84AAMDS+v///nBgw9f+////d3Pz3///////+/vf//8AAAIAB4AGwAn5AfkBGQ+ZBxoHGH8+//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAODzQK/IC4AfgAIkAAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlLineTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxfXDggh7///F9c+fOXv//8/1/999////////////vr++/qHcQcUAXEAOxj/ed7xn/mPuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nx8XDg4J7//fHxc+fu3v//9/d7//////////v////j7+PPo8cjwUGBAYGHh+Px5/H7mP+Zw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8vLj5eHDgID/+vPl48fOzP/6++33987f//7///////8vLy83Lnc+cR4XMIO8D7QP7ADYANwAzwE')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yNiYnDA8Pp7//Pj98//+nv//+v/3//++///7//////7xsPOws/Cn8CUYAYjPiMeYx5yLnJu4w/4')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,w8ODsTExMRn///Px8/f9Gf//+/v7//39///7//v////2D+cHpwevAQ0HAYPfh5eHhwWbwJuBA8I')]
    procedure TestControlMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/n58MDAgAL//fnxw8fOTv////3n995f/////+f////+91//PPMwdEB9AEUAB/iP/4z/iP8I/58')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+fvrwcDA4A////vhw8fuD/////Xz7/4f//////v////4P/APoAcABVAHQAeYH8gf/CD/gP+A/40')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/87AgMDT2pP//vDhw9fe2///9/P73/77////9/vf/vvgAOAA4ADgAOAA0AFwAZwD7g//v//9//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/86AgMDS+p7//vDgw9f+3v//9+P73//+////4/vf//7AAMAAwADAAOAA8ADwANhA/gD/g////v8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/86AgMDS8pv//uDgw9f+3///5+P73//f////6/vf/9/AAMAAwADAAOAA8ADwAP5A/AD/AP/5//8')]
    procedure TestControlMagnifyTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999////////////vr/+nv+efcV8BEQG0hfQd5JjImIOYg/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999////////////vr/+nv2cfcV8BEQG0hfQd5JjImIOYg/8')]
    procedure TestControlMaskToAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999////////////vr/+nv+efcV8BEQG0hfQd5JjImIOYg/8')]
    procedure TestControlMonochromeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,/+5AQODSmpL//nBg49fe3v//d3Hz39////////v///8spywnrAcMtQgREAEVlf8d5BjKGIMYw/8')]
    procedure TestControlNormalBlendEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5fTRAhBz///l9d0fMXP////1359x8///////v///vr//nP/cfdR8REQG8lfSd7JnamMO5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5fTRAgBz///l9d0fMXP////1359x8///////v///vr//nP/cfdR8REQG8lfSd5JnamMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5fTRAhBz///l9d0fMXP////1359x8///////v///vr//nP/cfdR8REQG8lfSd7JnamMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5fTRAhBz///l9d0fMXP////1359x8///////v///vr//nP/cfdR8REQG8lfSd7JnamMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5fTAAhBz///l9c0fMXP////1359x8///////v///vr//nv/cfdR8REQG8lfSd7JnamMO5g/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/v5fTAAhBz///l9c0fMXP////1359x8///////v///vr//nv/cfdR8REQG0lfSd5JnamMO5g/8')]
    procedure TestControlPaperSketchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,fv7//3///v9+/v//f//+/37+//9///7/fv7//3///v8SOEDYsdrh3X85/02/nXy3PJM3HHqpfmY')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,fv7//z3//v9+/v//Pf/+/37+//8///7/fv7//z///v8YSMHYod733f+x/am/nfiHPZ83mXy7fmY')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,fv7/fzH/7P9+/v9/Mf/+/37+/381//7/fv7/fz///v8e+PHas/e/1X+x+YG/nfzdfJs/mX8zeu8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,fv7/fzH97P9+/v9/Mf/u/37+/391//7/fv7/f3///v/N+Hz+t/e/tV8x+YG/nfSdfJs/mXs5eu8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,fv//fTHo/P9+//99Me/+/37///017/7/fv///T/////ty+/vvP+//W8ReYG+nf2d/Jl/mV+5e78')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/v///yHhzP//////J+fO//////8/5+7//////3/37//yb+7vrv8/fX8YcYU8nfXd/5j/mVs5e+Y')]
    procedure TestControlPencilStrokeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,+/PxYQACgp7///FhQ0fO3v//82/X197///////ff///tr++3rgeGARkHSQOdj7ef5537mNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,+5uZdXDCgp7///l1c8fO3v//+/13997///////f////hr/unr7ePcV0XUYO/j/ef5p3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,+/vxcQAAgp////FxQ0fO3///+/XXx97///////fP///vry83LTEhgx8HUwGXgZOZw537mNuZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,++vBFXDCgp7///H188bO3v//+/33997///////f////+L/ani3eD8U0XUYO/j/ef5p37mNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,+8OBMXDCgp7///Fxc8fO3v//+/13997//////////v/JL/InhjcfMV0XUYO/j/ef5537mNuZw/8')]
    procedure TestControlPerspectiveTransformEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/v5ZQCAgp7///llQ8fO3v//++1Xx9//////7/fP///vr++nr/cWcV0XEQOzD/ed5Bn5mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,+/vxdXDCgp7///F1c8fOnv//9/13997+//////f///+vr++nr6ePcR0XEYO/j7ef5p3LmMO4w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNu5w/8')]
    procedure TestControlPinchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr/evcV0XUYe/j/ef5p3bmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxcXDAgp7///Fxc8fO3v//+3l/59/////////v///vr++nr/cfdQkTAYM3j/cd5xjrmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cfcV0HEYe/j/ed55nbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cfcV0HEYe/j/ed55nbmNuZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0TEYO/j/ed5pnbmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    procedure TestControlPixelateEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed55nbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPcU0TEYO/j/ed55nbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////6/////8/gAeAB4AHwD/AAOAjYK/IA4ANgABAAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8/gAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d3P73/////////////8fgAOAB4AHwD/AAOAzQC/IC4APgAJAAAA')]
    procedure TestControlPixelateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3599///////fv///vr++nr7cfcV0TEYO/j/ed55nbmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/vxcXDAgp7///Fxc8fO3v//83l359////////fv///vr++nr7ePdQ0TEYOXj/ed55nbmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,+/v5cXDAgp7///lxc8fO3v//+3l359////////fv///vr++nj/ePdQ0DEYOXj/cd45n7mMOZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/v5cXDAgo7///lxc8fOzv//+3l/59/////////v///vr/+nj/eP9QkDAYOXD/MZ45npmMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/vxcTDAgo7///Fxc8fOzv//+3l/59/////////v///u7/63jrePtQkDAYeXD/cZ45npmMOZw/8')]
    procedure TestControlRadialBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    procedure TestControlRasterEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,8XAAAP//////fHBg//////9/9XX////////1///////RA/aP4pnD//////////////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,8WAAAP//////fHBg//////9/9XX////////1///////RA/aP45nD//////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,8XAAAJ//////fHBg3/////9/9XXf///////1///////RA/Sfw5nD/9P///////////////////8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,8XAAAB//////fHBgX/////9/9XVf///////1///////Rg/Sfw5nD/8P/+/////////////////8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,8XAAAA//////fHBgT/////9/9XVf///////1///////Rg/Sfw5nD/8P/6/////+f//////////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,8XAABg7w////fHBmT/f///9/9Xdf9//////1///3///Rg/Sfw5nD/8P/4//37/+H//f///////8')]
    procedure TestControlReflectionEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALengthProgress, AOpacity, AOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,+/vxcUDAgAb///FxQ8fODv////l7595///////vv///ur++XrfeP8QkDAAOcg/YP4BjzmMsYg/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,+fvxcWDAgJ7///FxY8fOnv//9/1359/+///////v///vr+2/K5c6cR0BQQMNk+OX4YnJmNO4w+8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,+/vx9UDAgob///H1Q8fOzv//+/3X597v///////v/+/vr++nrXe9sRUhEYMogvCc4AzhmMIJw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,+/v7cWDAgIb///txY8fOzv//+/l3597v//////f/3+/t7/2/L7cPdBwwEYGoA/Sc4BnBgNG4gz8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,+/vxcWDAgob///FxY8fOzv//83l3987+//////f/7///r/2vn3cPdQwHAQuRD/SN4JzAAcE4g/8')]
    procedure TestControlRippleEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcGDAgh7///FwY8fOXv//93lz389///////v////tp+ynrScMMUgVEQM3j/ed55jLmIOYw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+pgYODSih7/+nBg49fOXv//d3Hz/89///////P/7//sp+wnrCMMMQwBABU3hfcd6xjIGIMY0/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5AQMDSupL//nBgw9f+3v//d3P73//f//////v////sp+SnDCUMuAgQABgVmbsdYBibAMEA814')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/+5AAMDS+vL//nBgw9f+/v//d2P73/////////////8X0AfAB4AHwC/4AfBiAAyAAIAAgAAAdAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlRippleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nxYGDCgpb//fFhY8fO3v//83lz18////////v/7//8p+0nrXUMMRgRABM3j/cd4xnbGMMYw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,//JAQOCyihv/+vDg4/fO3///+/nz/8/////7//P/7//oB+wnLKcAsQA5ByEvHfccwRiLGIMY//8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+9gAEDQmBL//3BgQ9feXv//d2Vj399f///////////w/+ynJCcMYQhAAAgAiDaN44iQAIMA408')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS8vL//nBgw9f+/v//d3P73/////////////8fUAeQD4APyEXgFHhNAB8AAAAAoACAeAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlRotateCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vx8XHAiA7///Hxc8fOTv//8/l3195/////////////////v/8f/V/9E4u/j/Wd7Zn/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/Hx8XGZjA///fHxc9/OT///8/l3/95/////////////////v/8f/V/5EZm/jf2d7Zn/mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,+/Hx8XHYiAz//fHxc9/OTP//8/l3/959///////////v////v/8//X/5MZ2/nfyd/pn/mPu5//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,+/vx8XfTiwz///Hxd9fPTP//8/n39999///////////v7///v/+/9X+xMYW/nfyd7Jn/mdu5//8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlSaturateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr/cPcU0BEYG0j/Sd5JnbmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dSDAgp7///l1Y8fO3v//+/1399/////////////vr++nr6cfdV8BEYG0hfSd5JnImMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dSDAgp7///l1Y8fO3v//+/1399/////////////vr++nr+cfdV8BEQG0hfSd5JnImMOZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5dSBAgp7///l1Y0fO3v//+/1399/////////////vr++nr+cvdV8BEQG0lfSd5JnImMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+9v5NQAAgpb///l1Q0fO3v//+/1b99////////v////vr+//r/ev9U8RMYG8nfSd7JnbmNuZw/8')]
    procedure TestControlSepiaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.4,0.99,+/vxcXDAgp7///Fxc8fO3v//+3n3597///////fv////r/+nvyefcd0HkQe1j+cf4Z3JmIOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.5,0.99,+/vxsTDAgI////Hxc8fOz///+/1/597////////v///vj/6n36eeMb0DkQfVH/Ef4R3JmMH/w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.6,0.99,//vzsaGAgI////Px48fOz///+/v/x//////////P///zj/av9qfGJ+QDwAPRA/Ef4RngmeH///8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.8,0.99,//PzgYGBgd////Phw8fP3///9+vbx8/////////f///wn/CP9A/UR8QHwAfQh/Eb4APht+H///8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.9,0.99,//PjgYGBgd////Phw8fP3///9/vTz+/f///////////xH/Af9B/ER8AHwCfgB/An4CPhL/////8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,1.0,0.99,//fDgYGBw/////Phw8fP////9/fTz//////////////xP/Af5B/AH/gHwAfwB+AH4E////////8')]
    procedure TestControlShadowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vhwcHDhp////Hhw8fO3///9+P7z9////////vv///sH+wfuAcABVAHEAOwD/wd/Jn/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/86AAEDS85P//vBhQ9f/n///92N73/////////vf///AAIABAAEAAQABAAGAAcJBwA/gD/4P/98')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlShapeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13599///////fv///vr++nv7cfMV0HEQe0h/cd8xnKmOWYwf8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13589///////fv7//vp++nv7cfMVkHEYe0h+cd4xnLmMOYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXCAgh7///F1c8fOXv//8/13585///////fv7//vr++nvzcfcVkFEYe0h+cd4xnKmKOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3595///////fv///vr++nP2cfMRlBEYe0h+cd55nbmKOQg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3595///////fv///vr++nL2efcRlBEYO1j+cd45nbmIOQh/8')]
    procedure TestControlSharpenEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,8/Li4OAAAP///vLh40dO///+8/nvz07///77//////9//n/6u/YDNE80FhS+EP4Q/gD/mEsAAAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8vDk4AAIP53//PThQ09/3f/+9vtT33/d//7/+/////3/6O/YvdCc+Nj4mMi4gPyY/5gAAEAAAAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,+OiIGDDw/3b//Pj58/b/fv/8+fn79v9///z/+f/2////4H/g7+DfYN8A2wDOwP/A/4AQDgACAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,8KAk+ePg/8z//nT54+f/zP/+9/3z7//u//7//f/v/+7+EP4A/gD+BP4A/gD+AP4A/hoB8wEyAGA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0MDQ8PTc//f//PDw99//9//8+Pb33/////z49vff///wgPAA8ADwQPBQ8ADgAPAA4AD//5+xAwE')]
    procedure TestControlSlideTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxdXDggh7///F1c+fOXv//+/1/999////////////vr++/qPcQcUAXAAOxj/ed75n/mPuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nx8XDg4J7//fHxc+fu3v//9/d7//////////v////j7+PPo8cjwUGBAYGHh+Px5/H7mP+Zw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8vLj5eHDgID/+vPl48fOzP/6++33987f//7///////8vLy83Lnc+cR4XMIO8D7QP7ADYANwAzwE')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yNiYnDA8Pp7//Pj98//+nv//+v/3//++///7//////7xsPOws/Cn8CUYAYjPiMeYx5yLnJu4w/4')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,w8ODsTExMRn///Px8/f9Gf//+/v7//39///7//v////2D+cHpwevAQ0HAYPfh5eHhwWbwJuBA8I')]
    procedure TestControlSmoothMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,+/vxdXDAgh7/+/F1c8fOXv/78/13999//////3f///9vr++nr/cfcV0XEYO/j/ed55n7mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,+3t5dXDCgp7/e3l1c8fO3v97e/1399////9//3f/3/8Hrw//r/cf8V0XEYu/j/ed75n7mPuZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/vz8dHDAgp7//Px1c8fO3v////d399//////9//3///vwO+Av4Af9V//EZu/n/+d/5n/mP+Z//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,+/v5dXDAgJD///l1c8fO3P//+/1399/9///////////vr+//r/cf8V0XEYu/h/eQ74D7gPuAwAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,+/v5dXDCggD///l1c8fOTP//+/1398/t/////////+/v/+//v/cf9V//EZu/n/+d/5kAAAAAAAA')]
    procedure TestControlSwipeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,8/PxcXDAgp////Fxc8fO3///+Xl3597/////f/fv3//tI+1nrXcfBx2VEYA/iLeZ55n7mPuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,8/Px8HBAgo7///Hxc0fOzv//+fl3597///////fv///tr++hr/OvYw0HEYAfjLcZ55nrmOuZwf8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,8/Px8HAAgo////Hxc0fOz///8/l3d87/////////7//Nr++lr/evdx0DEY+XnIcMmwzRCNGZwP8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,+/v59HAggo7///n1c2fOzv//+/1/d87////////////nr++nr/Wf8x0DEYuTnacMjwzbCPEJw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,+/v59XCwgID///n1c/fOzP//+/1/987v/////////+/jr+e3r/ef810TUMmEj8GMw4zJgMGBw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,+/vxcXDwgID///Fxc/fOzP//8/l//97u/////////+/tr+u3t7ef810TULG2i/QM5YzLgMeJwe8')]
    procedure TestControlSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,+/vxcGDCgo7/+/FwY8fOzv//8/lz58////////v/7//8p+ynrScMcRgBEAUXj7+c55jLmMmZwf8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,/+JgQODSgob/+nBg49fOzv//d3Hz/8/v//////v////Mp+ylrSUM+RgVEAwXnJ8MggnbAMIY9H8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,/+ZAQODS8pL//nBg49f+3v//dXHz3/////////v////tyc2hj+AP+A28AfgBCY+BDoCDgOGA/44')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,/85AQMDS+vD//nBgw9f+/P//dXPz3/////////////8XkAeQB4AHxC/4AeBjASQIAEAAYABAcAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vp++nvye/cV0XEQG/j+Sf5J3bmIOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+/n5cXBAgh7//fnxc0fO3v/9+f3358/////////v7//9r6+nnfcPMR0REYEVgaAZ5RnYGNsZg/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/vxcXDCgh7/+/Hx88fO3v//9/n359////////fv///9p+3nj6cVRV9zEYEUhfefwB3dnYMZgdc')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/vxcXDCgh7///Hxc8fOXv//+/l3x99///////fP///t5++/r+cPcU0XEYm0n+Qf5A37G8M9w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxcXDAgh7///Hxc8fOXv//8/l3995////////////vr+3nrWetc20XYQe/n/Wf5J3bmYO9w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vxdXDAgh7/+/H188fOHv/78/33995/////////////r/+nv+e/c10T8Qe0h/Sf5J/Zn9uZw/8')]
    procedure TestControlTilerEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1318////////ff7//vr++nrycPcU0FAQe2h+Yf5JjImoOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5dXDAgp7///l1c8fO3v//+/1/58/////////v7//vr++nr+cPcU0HAQO2hfYd5JnImIOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dXDAgp7///l1c8fO3v//+/1318/////////f7//vr++nr/cPcU0FEYO2h/ad5JnImMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr/cvcU0HMYO2h/ad5JnYmMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5dXDAgh7///l1c8fOXv//+/13599///////fv///vp++nr7cPcV0VEYO2j/af5JnamMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr/cPcV0HEYO+j/ad5JnbmMOZw/8')]
    procedure TestControlToonsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHaDgr/+HDg899OTv/9d/H338/P////8f////8QUBBYQdhAjqr/8v9LfxtiH2Y3ZnxmfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,//xAwOHaGhD//HDg499eXP//f/Dz39/d/////P////0hJkFnIveD/h3fQo3fAx+hP4YuhHwGcAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+5AYMDQGh7//nBgw9deXv//d3Lz315/////+vv/f38DOAEYBxwH3C79fR5/AX2I3oD+AP4g5gA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDy+nL//nBgw/f+fv//d3Pz//9///////////8DBAOEB5YH0g3ZQPHjX+bYBmgP4O1g4AA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS+vL//nBgw9f+/v//d3P73//////39/////8fmAfAB4AHwjfCCOIjQO/CCnAEYABABEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlWaterTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cvcV0TEYO3j/ed55nbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3189///////ff7//vr++nr7cPcRkXEYe3j/cd55n7mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPcV0TEYO/j/ef55nbmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vxcXCAgh7///Fxc8fOXv//8/l3999///////f////vr++nr7cPdV0HEYe/j/ed55nbmNuZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr7cPcV0HEYe/j/ed55nrmNuYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/vxdXCAgh7///F1c8fOXv//8/13599///////fv///vr++nr/cPcV0HEYO/j/ed55nbmNuZw/8')]
    procedure TestControlWaveEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcGDCgp7/+/FwY8fO3v//8/lz58////////v/7//sp+ynrScMcVgBEAE3j/ed55jLmMOZwf8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+pgQODSgpb/+nBg49fO3v//d3Hz38////////v/7//sp+ynrScMtRgREAEXn78c5BjLGMEYwf8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5AQODSmpL//nBg49fe3v//d3Hz39////////v////sp+ynjSUM/QgRABEVGZ8IJAiJAMEA9d4')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS+vL//nBgw9f+/v//d3Pz3/////////////8X0AeQB4AHwC/4AfAjACyAAAAAAAAAdAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlWaveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nxcODAgJ7//fFx48eOnv//93nzz96+//////vf/v/sp+2jrTMM8UgRAAk3ifeL54XbiMOYw/4')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+pgQODSgh7//nBh49fOXv//dXnz/89e///9//v////Mo+yjjScM9BgQABA3mX8X5RDaEKMYA5w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+xgQMDQkPv//PDgw9fe////9/Hz/97///////v////sp+ShDCEM/AgQABAFWDsR5hBrAPsQ/98')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/+9AQMDSev7//3Bgw9d+/v//dnPz33////////////8H0QeAD5APyK/oAfxiAByAAAAAgAAAQAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestControlWiggleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,+/Px5WDCgp7///HlY8fO3v//+/1/997//////////v/sL+8nrzetcVwXUYOxj/ef5p3bmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNu5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,+/vxdXDAgpL///F1c8fO3v//+/13597///////fv///vr++nr7ePcV0XUQO+j/Yf5h3bmNuZw98')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,+/v5dVDCgoL///l1U8fOzv//+/13187v//////ff7+/vr++nr7ebcVkXU4Oxj/Sf5J3bGNsZw58')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,8+v59VCCgpL///n1U8fO3v//+/1X197///////ff///tr+2nn/ePcUMXQYOYj/if6B32mNIJw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,+/Px9WDCgpz///H1Y8fO3P//+/1/5879///////v///vr++nr/efcVwXUAO2D/Yf6h36CMshw/8')]
    procedure TestControlWrapEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress, ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress, ARightStartProgress, ARightControl1Progress, ARightControl2Progress, ARightEndProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,/5GD4fPxyf///cPh8/fN////1+P//9//////////3//2//gfxR/d/93/+B/jX/m///////////8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,34OPj4cBj//////vx8fP/////+/f19/////////////gf/5/+Pfnt+B3+L/dz9gH/f////////8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,//+ZkQOD4/f///nxQ8fv//////tbz//////////////1v+fn78f5r+Vvlw/KD+zP/f//P/+///8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,8+PBgfD48/////Hh8///////8+P3///////////////8f/IH5h+Ab//x/aX3kff/+6//H/////8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,/+fh4PCD5+f///Hh88fv7///+/P/1+/v///////////+P/aP7sftH/eZ/M/lT/6//j/+P/5///8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,8+PDgfD48/////Ph8///////8+P3///////////////8f/IH5h+Ab//x/aX3kff/+6//H/////8')]
    procedure TestFilterAffineTransform(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,/88HAwMVFf3//3djQ1df/f////fL31///////+//3//gf08/kZ+gf1d/D6+Et8W33ffr9+f3/7c')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,/4eFFQWNwZ////V1R8/P3///9/3H/+/f/////////9/jy+ZDx43jQeMBggPFt8AnmE/OV+f78ec')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,6ODA4OHh/////PDg4+f////++Pnv7//////9///////yIfumujP+050B/AfEBu6C93ft2/7N/78')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,8ODg8PH///7//PDx8////v/+8vv3///////+//////9mgH6AboF/G1cFaT/O97d/gvP5tf7++pA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,8PDw8Ojv9/j//PDx6+///P/+9Pf77//9//7+9//v//2/CL0QvYMZAL0SX6B+w/+x3/eXf3fNS/s')]
    procedure TestFilterBandedSwirl(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,/+Pw+OMBg8T/+/D540fPzP//8/nz78/d/////fv/z9+ehbtBL2Xcz/gPMK9CD+cPqR32O/P7+D4')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,/8bgYODy0/H//vBh4/ff/f//9nP7///9//////v///94gJsDiACYGPgZ2CroC+KOcA+vs5WH9QY')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/85AAMDS8vL//nBgw9f+/v//d3Pz3/////////P///8ogAgAIAAAAHgA+B7zDPuMVx/8hL22//g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/84AAMLS+vL//nBgw9f+/v//d3P73/////////v///881mTTPMc7//bfb789//p6RHIAIMAA+UA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterBandedSwirlTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vx8eDAgh7///Hx48fOXv//8/n3195+//////ff////p//nv3cfcVwnUIMch+6f4p3amMu5x/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v59WDCgp7///n1Y8fO3v//+/97197e//////vf/v/+r/6nvreecVwCEIK0hvYd4hjIGIMYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/v5dSCCgp////l1Y8fO3///+31358////////fv7//vp++3v3cfdVkRUQEUlfSd4J3ImIOZg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/v7dSDCgp7///t1Y8fO3v//+31/587e///////v79/vr++vr2cPdU0CUQKUnvQf4BzKHIM4g/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+9v5dQDCgp7///l1Q8fO3v//+31X587e//////fv79/tr+2nrSctdU0BMQG0hfSd4JnInMOZg/8')]
    procedure TestFilterBands(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vhcWDAgp7///FxY8fO3v//8/lz597///////Pv//8AAP//CACP8V0XAAC/n+CM/53IgP+ZAIA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+vxYGDCgpL///FgY8fO3v//82t7787///////vv//+spe+nreeccVkBUAC/n7AM/53IAP+9AAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+5gAMDSkpL//nBgw9fe3v//d/Hz3/////////P//////wAAv/ef/QAAUZkUGLed/50AAP+Yw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AAMDS8vL//nBgw9f+/v//d2Pz3/////////v////v7++nAAEP9U0RAKEAALwd7BgAANsY1/8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterBlindTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,AADR+/v98eB/ePH7+///7H97+fv7///9f3v5+/v///1DIP////////f//////7//3/1d3x+P/58')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,AAAQEHL7+/l/fHBRc////X9/eX37////f395ffv///8AAAAAAAAGAB9k3e////f///////////8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,zEQAQsLy+v/+dGDiw/f+//53c+vr//////f36/////8HAA+ABsAGwAIgCQACAALAGABJbN3/3/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterBloodTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+9v5NQBCgpb///l1Q0fO3v///Xd7X8/f//////vf7//vr++nr/cPcU0RAYC0mfQd5JjYmMOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+9v5NQDAgp7///l1Q8fO3v///X1f19/f///////f/9/vr++nr/cPcU0AAYC0kfQd5BjYmMOYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/v5dSDAgp7///l1Y8fO3v//+/1399////////f////vr++nr+cPcU0BEQC0gfQd5BjImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1399/////////////vr++nr+ePcU8BEQG0hfQd5JjImIOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp++nr6e/cd8B8QG0hfSd5JzInMOYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vx8fHBgh7///Hx88fOXv//8/n3595////////v////p/+nv6e/c98B9QG0hfSd5JzInIOYw/8')]
    procedure TestFilterBloom(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdGDAghb///F0Y8fOXv//8/xz195+//////P////tp+2nrbctcXkRIQE1h/ef5J7YnNuYg/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+nhZODAghL/+fFk48fOXv//9/Tzz95+//////P//v/8p/yjLTM8M/gBIAE1kf6fxJ7EnJOcg/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+xgQMDSshL//HBgw9f+Xv//d3Hz3/7+//////v////9o/2jfTE9OXkAIAAtg/yPxJ7MjJGMs5w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8vL//nBgw9f+/v//d3Pz3/////////v////9t/23Pb89/B0cARAFxXzPxA6ABNAA/4w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterBlurTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNu5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7efcV0HUYe/j/ef5p3bmNu5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7e/cV0HUYe/j/ef553bmNO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3195///////ff///vp++nv7e/cV0F8Ye/j/ef553bmcO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXDAgh7///Fxc8fOXv//83l3185///////ff7///p/+nv6e/M/0DsQe/h/af5p3KncO9w/8')]
    procedure TestFilterBoxBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/Px8XOQiwz///Hxc9fPTP//9fn73899///1+f/f330DSECQqJahkc0x8cGvhdwXJJ1rGQCrAGY')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/84AAEDS+nL//nBgQ9f+fv//d2N73/////////v///8AgAAAAAAICHgAcBjzQPvsE44/gCW8gQA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/84AAMLS+vL//nBgw9f+/v//d2P73/////////v///8AgAAAAAAICHgQcBjzAPvMG44/gA28gAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73/////////v///8AAAAAAAAICHgQcBjzCPvMH48/gAu8gAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/84AAMLS+vL//nBgw9f+/v//d3P73/////////v///8AAAAAAAAICHgQ8BjzCPvsX4//gB++wQo')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterBrightTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,+/vxdWDAgp7///F1Y8fO3v//8/13197///////ff///vr++nr7edcVgXUAO0j/Sf5J3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,+/vhwcDDgp////Hhw8fO3///9/Hzz87///////vv/v/sr+gHqAeIAVgBUAOwB/Sf5J37mPuZ+/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,/84AAMDS+vP//nBgw9f+////d2P73///////6/////+XAAeAB4AHwC/AAOADQIfAwgDAAOBA+Ac')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,/84AAMDS+v///nBgw9f+////d2P73///////6/vf///gAMAAhQAGwEgAAICTAPbA5AD4APsA//8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterCircleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,////Pgxg4PD///9/T2fu/P////9f9/79///////////v/+//r/+P/V3/Ufu/n/2f/Z3/mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,////d2CAghb///93Y8fOXv///393595+//////f////v/+//r/+P/d0P0Y+/j/Sf7J/PnNO9w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,+/vx8eGPjw////Hx48/PT///8/vz/99///////P////vp++n77fvd+0H8cf///T/5P/L/8P/w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,+/vx8eDMjw////Hx48/PT///8/n3/99///////f////vp++n77fvcekH8cP///T/5P/L/8P/g/8')]
    procedure TestFilterColorKeyAlpha(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+9v7NAACgpb///t1Q0fO3v///3frX+/e///////f////r///v/f/9f8R8fm/nfTf7J//nd+9+/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+9v7NAACgpb///t1Q0fO3v///3frX+/f///////f7//vr+/vr/fv9e8R4eG0nfTf7J/bndu9w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/v5dSDAgp7///l1Y8fO3v///f1/18/f///////f79/tr+2vrfcNcUgRAIG0mfSd5JnYmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vx8fDDgw////Hx88fPT///8/n3589/////////////r/+vv2+fIV9HE4e0h+ef4ZnImIOZg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/Hx8fHfjw///fHx89/PT///8/n3/99////////////vr++nv/e/dV9BcYG+nfWd5ZnKmMOZx/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AAAAAAAAAAB/fHBhQ0dOTH9+cunX5158///////v///vr+//v/e/9X8R8YG+nfSd7Jn/mdu5w/8')]
    procedure TestFilterContrast(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,8enp7eDgoIf//fnt4+fuj///+/37/+7v////////7u/u5+7n7fLt8uzw7BAIAQxBHhH+Nf4//D8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,/v7+/Pz8/Pj//v79///+/P/+/v3////9//7+///////8/Pz8/Pz8/Pz4/Pj8+Pz5/PH88fzx/PE')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,/+HBwd3d3V3//fHh39/fXf//8e3////9///////////wA/AD8APwe/P78/vz+/P78/vz+/P7c/s')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,+/v58XFwQIL///nxc3dOzv//+/P7d//O/////////8/nz++v76fvp6+3n3FdF1EDt4e+n/ef5J0')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,8fH19eTg0EP//fX15+feT/////3///7///////////9793t3e3N7c3vyenh6GgODAwOfC/8fvx8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,7Ozs3sbAwOD//Oz/x8fO7P/9/f/f587u//3//////v69+L34vfy5/Ln8uDw4BjgCPwA+IH4B/gc')]
    procedure TestFilterCrop(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,ff3h5uChBwt//fHn4+dPz////+/z/0////////P////yQ/7+/Xz6PPJJvAzgv/2vmD+JP4X/g+8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,4eDTBwcBx87/8PPnR0fPzv/0//ffT+/f//f////P/9//jfr+oX+mt0A/yT+Il2KqovPQ+vD98Lg')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,mI+DozAT/////+Pj89f/////8/Pz///////79/v///8ed60XfROCV2kU99TzOedvSe6p//z/hcc')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,//zgQOKAw/f//PDh48fP///+8vP73+/////////////8zP/MWzQiIz5cNFwACF6RTjNDM8A/xD8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterCrumpleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/n5dXDAgo7//fl1c8fOzv///33/587+///////v////9/+3v7efsf2BkYOfmfcf8x7qnMucw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/v59XBAgoL///n1c0fOzv//+/9/987e/////////9//5/+n/7eft98x0QOXg7ef9h/qnNuYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/PzcWFBghT///NxY0fOXP//8/t31999///////f////j//v/28/Z38jowM/h/4/5j/UOZM5g/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,8/PzcWGBBB////NxY8dOX///+/l3109////7//ff////7/9vP2e/Z+MDKwP+P+4/1jnXOYf/h/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/nxcGCCgp///fFxY8fO3/////13x97///////fH////t/+3n7PdEdGDl4v3H+If65zLncP/w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+fn5+DDAgIr//fn5c8fOzv//+f17x+7+//////vn////4//z3/Hfuf6B2IHfofsP8Q78TuHe4f8')]
    procedure TestFilterDirectionalBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13597///////fv///vr+mnr7efcV0XUYO/j/ef4I3bmNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+/v5dEDAgJb///l0Q8fO3v//+/1X59////////f///+Dr98vrPQJ9VgXMJeHk/ad/IHYmM+Z2/0')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/89AAEDSenr///DgQ9d+fv//9/Nz3/9+////+/v///+p46ECAEoMXAgKAQFJjXGDMBA6UgEAQsw')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d2P73///////7/////8fgAeAB4AHwD/AAOGzQG+IC4APgABgAAM')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterDissolveTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHKCgr/+HDg889OTv/9d/H3z87P////8f///v8QUBBYwVjIj+r/k/9LfxtmH2Y3Zn5mfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/+5gQMDx+w7//nBgw/f/Tv//d3P7///v////+/v///8WcBBwEVgQ2Mnaz/3p+b8ZSxgfBj8kPgA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/84AYMDQ8fn//nBgw9f//f//d3Pz3//9////+/vf//0AAAcwGzAe8Bl4Udjh2aHdAfhDOAs6HxQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS8PH//nBgw9f+/f//d3Pz3//9////+/////0HAAeAB6AX4C/wAfADcGPYAtgi2ANoWxg')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vP//nBgw9f+////d3Pz3/////////////8fgAeAB4AH4D/wAPAzUC/QA9ANQAJAAEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterDropTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,/evd//zLzo////3//8/Oz///////z+/v////////7+8ZKBlIGVx5RWuBYYkvmWyTDJEXEVKrPmY')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,BGxc3vhIamB/fHz/+09ubH98fv/7z3/tf3x+///ff+0ZeBlIuVx5RWuJYYm/HeyTDJUXFVKvPmY')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,BGxc3vhoauh/fHz/+29u7H99fv/773/tf31+////f+0ZaBnYuVx5VXmJYak/XeyTDJUXEVuvfmY')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,BHxc3vhoamB/fHz++29ubP99fv/773/t/31+////f+0Z6BlYuV55VXmJYak/HezTTJ0XEVuvfmY')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,BHxc3vhpamF/fHz++29ubf99fv/7/2/t/31+////b+0Z6BnYuVx5dXmJYas9XeyTDZkXGXuvfmY')]
    procedure TestFilterEmboss(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcWDCgp7///FxY8fO3v//8/lz597///////vv///tp+2nr6eNcUkREQG3j/ef5J3bmMOZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgpb/+/FgY8fO3v//8/lzz8////////v////sp+ynrSeMcVgQEAG0hfaf5J3KmMOZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+9gQEDSkpb//3BgQ9fe3v//d3Fz39////////v////sp+ynrCeMdUgREAG0hfSf5BjamIOYw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8rL//nBgw9f+/v//d3Pz3/////////v////sr+ynrScM9UgRAREUkfQd5BjIEMMQ9/8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterFadeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestFilterFill(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+9v7NAACgpb///t1Q0fO3v///3drX+/e//////vf79/vr++nr+cPcU0AAQCUgfQd5BzIGMMYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+9v7NAACgpb///t1Q0fO3v//+/1X597+//////fv///tp+2nrWcNcU0AAQAUgOQd5BjIGIMYw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+9v7NABCgpb///t1Q0fO3v//+/1X597+///////v///vr++vr/cvcV8RMYG0nfSd7JnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+9v7NQACgpb///t1Q0fO3v//+/1X597+//////fv///vr++nr/cPcU0BAYG0kfQd5JjYmMOYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+9v7NQACgpb///t1Q0fO3v//+/1X597+//////fv///vr++nr+cPcU0AAQAUgfQd5BjIGMMYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH9+culX5159////////////r///v/f/9f8R8fm/nfTf7J//nd+9+/8')]
    procedure TestFilterFillRGB(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxcXCAgh7///Fxc8fOXv//+/l3195///////ff////p/+nvye/M/kDsQO/j/af5p3LmcO/g/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/Px8eCAgA////Hx48fOT///+/n7z89v///////P/+//g/+B/wH+AfYD5APAA4R/AH+Af4D/gP8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,//Px8eCAAQ////Hx48dPT/////v7z09v///////f/+//g/+B/wH8AfQB4AGAAwAfAH/Af8B/wP8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,///h4cGAAQ////Hhw8dPT//////3119f//////////////sD8AHwAcABgAMABwAPAB+AP/B/8H8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,///DgYEBAwP///Phw0dPT////+/PT09P///////////g/+ABwAGAAYADgAOAB4APgA+AH8Af4B8')]
    procedure TestFilterGaussianBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,+/vx8fHAgh7///Hx88fOXv//8/n3595////////v////r/+nv/e/df8T8YG8nfSf5J3bnNu9w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,+/vx8fHAgh7///Hx88fOXv//8/n3595////////v////r/+nv/e/df8B8QG8lfSd5J3KnMOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,+/vx8XDAgh7///Hxc8fOXv//8/l3595////////v////r/+nv6e/c18B9QG0hfSd5JzInMOYw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3999////////////vp/+nv6e/cV8B8QG0hfSd5JjImIOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,+/vxcXDAgh7///Fxc8fOXv//8/l3995////////////vp/+nvye/cf8B8QG0heSd4JzInIOYg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,+/vxdXDAgh7///F1c8fOXv//+/13999////////////vr/+nv2cfcV8BEQC0heQd5JjImIOYw/8')]
    procedure TestFilterGloom(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,//v/PTBy8JD///99c3f+3P////13//79//////f////vr++n7/fvd+0X8cf8//T/5P/a/8P/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,+/v7fTBygJD///t9c3fO3P//+/13/979//////f////vp++n7yfvI/0H8Qf05+T/4P/I/4H/gf8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dTBigJL///l1c2fO3v//+/13/97///////f////vp++nr7fvc/kH8Yf07/T/5P/K/8O/g/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+/vx8eGBjw////Hx48fPT///8/n3589///////fv7//vr++3r/cf9V0REYk/jfcd55j7mPuYw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,+/vx8fHDhh////Hx88fOX///8/n3595///////f////v/+//D/8f+R34EZgfCL8J/wj/CP8Q//8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXEDghf///F1c0fOX///8/13995///////f////v7+//r/e/8d0x8Zi/jf8d7Jz/nP+Y//8')]
    procedure TestFilterHueAdjust(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/4uvLAACgpb///9tQ0fO3v///+/j7+7+//////Pv7//vr++nL/cPcQ0QAYAUiPQd5BjYGMMYw/8')]
    procedure TestFilterInvert(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,/84AAEDSmp///nBgQ9fe3///d3Fz397///9//3P//v8opwgnKCcMdUgREAG1nfef5J3bmNuZ0/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,/84AAEDS+7///nBgQ9f/////d3Nz3///////+/vf//8AAAAHADcH9QURAdkUnb8f7Bn7mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,/84AAMDS+vf//nBgw9f+////d2P73///////7/////8fgAeAB4AHwD/AAOAjQC/JAoABgAAQAf8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterLineTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxfXDgoh7///F9c+fuXv//8/1/9/5////////////vr++nqPeAcUAXUEOxj/+f7x3/mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nz8fDw8J7//fPx8/f+3v//9/fz9/7///////P////jz+OPo4/jh2OBYwGDA8MD4wXX+Nv5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8ubn5+/HwID//ufn78fOzP/+7+//38/f//7///////8+Pj4/Pn88fzx/vP+4/7gf+AHwAPAA0AA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yJiYnLw8PH7//Pj9//98fv//+v////1+///7/////f7j4OPg4+DH8Mfwx/CP8I/4z/yP+J/43/w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,x4eDozMxcXn///Pj8/f9ef///+/39/////////f////mH+Yfxh/PD88Pjw+PB5+Hn4cfhz/DP8I')]
    procedure TestFilterMagnify(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/n96MDAogL//f3pw8fuTv///e3n//5e///97//////+9977fPtwXEBlQGVAZfjv/47PzveMv58')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+fvCwcDg8Yv///Lhw+f/j///9vfz7/+f//////v////4X/AP8AfgB/AH8AeYD8gT9mf/5//E/40')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/87AgMDT+/P//vDhw9f/+///9/P73//7////9/vf//vgAOAA4ADgAOAA8AH4AZgD7g////////8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/86AgMDQ+P7//vDgw9f+/v//9+P73//+////4/vf//7AAMAA4ADgAOAA8ADwAPhA/gD/g////v8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/86AgMDQ8Pv//uDgw9f+////5+P73///////4/vf///AAMAAwADgAOAA8AD4APxA/gD/gP////8')]
    procedure TestFilterMagnifyTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    procedure TestFilterMaskToAlpha(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995////////////vp/+nvyefcV8BEQC0heQd5JzImIOYw/8')]
    procedure TestFilterMonochrome(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,/+vgYGDSgpb/+/BgY9fO3v//93Fz38////////v////sp+ynrSeMcVgQEAC0hfaf5J3KmIOYw/8')]
    procedure TestFilterNormalBlend(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7JzanMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5fXDAwh7///l9c8fOXv////13595+///////v///vr/+nv3efdV8REQC8hfSd7J3anMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5fHDAhh7///l9c8fOXv////13595+///////v////r//nv/efdV8RUQG8jfSd7J3anMO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/v5fHDAhp7///l9c8fO3v////13597+///////v////r/+nv/efdX8RUQG0jfSd7J3anMO9w/8')]
    procedure TestFilterPaperSketch(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,fn7///////9+fv///////35+////////fn7///////8AQFNYswrile6R/X2+vUjnHIMkXTSlQGY')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,fn7///3///9+fv///f///35+////////fn7///////8IQNAQ4M/xzX+R/fm/leDHHa83nTS1QGY')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,fv7///Hv7/9+/v//8e///37+///1////fv7///3///8OoOHCscf/1W+x8fm/nfzdTJsfnR8zQO8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,fv7///Ht7/9+/v//8e///37+///1////fv7///3////doHz2tee/vW8x8fm/nfydbJkfmRk5AO8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,fv////Hh//9+////8e///37////17///fv////3v///8w+yn/P++fW8R8bG/nf2f7JldmRu5AZ8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/v////Htz///////9+/P////////78//////////7//iJ+5nrncOfX8Ycek/nfSd/5j/mV84A+Y')]
    procedure TestFilterPencilStroke(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,//vz4eGBgz////Ph48fPf///9/n7799////////v////j/2P+0//T/4H+g/yf9jn7M/N/4//n/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,/+fn5+OBwfn///fn48fP/f///+fzz+/////////////mf8Y//j/+v92f3Z/yF/ZX6m/6f//n//8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,+/vxcXCAgh////Fxc8fOX///8/l3x99///////fP///vr++nr3cvdWkDIYO/nfef553bmcO7w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,//+bkcHAh8////vxw8bPz/////3T1s/////////////H39fP1c/GR/6j/M/5ZfN18D/gP+P///8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,//Px4YGIKP////Hhw89u////8+vr33/////////////9H/7P/g/qR/b//PPs4+zBjeOf//////8')]
    procedure TestFilterPerspectiveTransform(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxZQDQsh7///FlQ9f+Xv//8+1X9/5/////7/f////vr++nr7eWcV+HUYOzT/+f7x3bmNOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,Oztzf39/Px5/f3N/f39/Hn//9////39+//////////+/37/fP78//7//v7+//5//3//P/4e+gfw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,+/vzcXDAgh7///Nxc8fOXv//8/l3995///////f////vr++/r6ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,wPj9////f3///P3///9/f//9//////////3///////////////////////v/+/////9/+3//f/s')]
    procedure TestFilterPinch(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//+31/585////////v7//vr++nr/ePdYgRgYG/j/cf7x3rmNu7w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7efcV0DUYe/j/ef553bmMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7efcV0HUYe/j/ef553bmNu5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0TEYO/j/ed5pnbmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0HEYO/j/ed5pnbmNuZw/8')]
    procedure TestFilterPixelate(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13995///////f////vr++nr7ePcV0HUYO/j/ef5J3bmNu5w/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdXDAgp7///F1c8fO3v//8/13997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNO5w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/13597///////fv///vr++nr7evcV0D8YO/j/ef5p3bmduZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////6/////8/gAeAB4AHwD/AAOAjYK/IA4ANgABAAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8/gAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d3P73/////////////8fgAOAB4AHwD/AAOAzQC/IC4APgAJAAAA')]
    procedure TestFilterPixelateTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+/vxcXDAgp7///Fxc8fO3v//8/l359////////fv///vr++nr7efdd0RkYO/jfef5J3bmcO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,+/vxcXDAgp////Fxc8fO3///83n359/////////v////r/+nv6efd90BkYG/j/ef5p3LmcO/w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,+/vx8XDAgp////Hxc8fO3///+/n359/////////v////r/+nv6efd/kBkYHej/Yf6p3rmcP/w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,+/v58XDCgp////nxc8fO3///+/l359/////////v////r/+n37ffNfkB04P/H/Id6p3pmcP/4/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/v58aDCgo////nx48fOz///+/3/59/////////v////h/+n37ffsfgD28f6H/Id4Rnh/+H/4f8')]
    procedure TestFilterRadialBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,+/vz8YDAgAb///Pxw8fODv////nr794P///////v/9/+j/+XuV/3gdEB0AH9cfNf4D/zOMtes/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,+fv3w0DAiI////fjQ8fOj///9+dz9/+/////9/v/////r/yfe596U/cjTKJpUuF34cvA/9P/w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,+/v3x0HAgIL///fnQ8fOzv////f7997u//////v//u//FfRt/N39BfStuGDoAvD4wAXB+4Dvwf8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,+///byDAgAb///9vY8fOTv///2/3999O////7///38/97/3/bY9uRv40uXmoQ/D+6DnBg9D+gz8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,///vYyMDggL///9jY0fOTv////9//95+/////////v797/0v/lf+X5avmS+RD+BH4J7Aw8H/g98')]
    procedure TestFilterRipple(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxdGDAgh7///F0Y8fOXv//8313185////////f/v/tp+ynr7etcVgVEYO2j/Wf5J3LmMO5w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgp7///FgY8fO3v//83Fz78////////P////sp+ynrCOtcW0AoAG2hfSf4I3ImMOYw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/89gAMDSkpL//3Bgw9fe3v//d/P7397///////v//v/sr/2vPSccMVgBEAE1mfSd4B3bGNuZw/8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8rb//nBgw9f+/v//d2H73/////////v////9r+y3nnce+RgRAMEQifyZ5BDLEMPY1/k')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterRippleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nx8GDCgpb//fHxY8fO3v//8/Fz18////////v/7//tp/1n/XU9IXwBYQO1vfcf5R3dGMM41/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,//PhQOCSghv/+/Fg49fOX///+3nz385////7//P//3/5T/9nPWcxcaABawF/H8kO1QyTnIGcn/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+/gAEDAmBL///BgQ8feXv//92Vj395//////////v/47+6H7oMusy6SaIAggLTP4IzUjIOJh58')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS8vL//nBgw9f+/v//d3P73/////////v////9r+2nPWc9dQgRARktgXcZxRnQGIMQ+9k')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterRotateCrumpleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vx8XHAiA7///Hxc8fOTv//8/l3195/////////////////v/8f/V/9E4u/j/Wd7Zn/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+/nx8XHZnA///fHxc9/eT///8/l3/95/////////////////v/8//V/5EZm/jf2d7Zn/mP+Z//8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,+/vx8XHYmA7///Hxc9/eTv//8/l3/95/////////////////v/8//X/5MZ2/nfyd/Jn/mPu5//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,+/Hx8XOQiAj//fHxc9fOTP//8/n39959////////////7///v/+/9X+xcYW/nfyd7Jn/mdu5//8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterSaturateTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5dXDAgp7///l1c8fO3v//+/13997///////f////vr++nr7ePcU0BUYG0j/Sf5J3KmMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dWDAgp7///l1Y8fO3v//+/13997////////////vr++nr6e/dV8BUQG0hfSf5J3ImMO5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dSDAgp7///l1Y8fO3v//+/13997////////////vr++nr+e/dV8B0QG0heSf5J3ImIO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+9v5NSDAgp7///l1Y8fO3v//+/13997////////////vr++nr+evdV8B0QG0lfSf5J3ImMO5w/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+9v5NQBCgpb///l1Q0fO3v//+X1f99/////////////vr+//r/ev9c8R8YG8nfSd7J3bnNu5w/8')]
    procedure TestFilterSepia(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/vjwcHDhp////Phw8fO3///9+P7z97///////v////sH+gfuAeABVAHUAOwD/wf/J3/mP+Z//8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/86AAEDS85P//vBhQ9f/n///92N73/////////vf///AAIABAAEAAQABAAGAAcJBwA/gD/4P/98')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterShapeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13585+//////fv7//vp++nv7efMV0HEQe2h/cf4x3KmMeYg/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vp++nvzefMVkHEYe0h+cf4x3KmIOYg/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vr++nvyefMVkDUYe0h+cf4x3LmIOYg/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/vxcXDAgh////Fxc8fOX///8/l3585///////fv7//vr++nP2efMVkDUYe0h+cf4x3bmIOYg/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxcXCBgh////Fxc8fOX///8/l3595///////fv///vr++nr2efcVlBUYO1j+ef4R3bmIOYh/8')]
    procedure TestFilterSharpen(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,8/Lq4MAEHP///vrhw0de///++/nPb/7///77//////9//n7q+2LjJn8y/j7+Ov4w/nKP/ngAAAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,5vTAwAgofhn//PDhS29+Hf/88vNbf36d//z/+/////3/6O/Yvdj86Pj4/Oj4wP3Y//gAAEAAAAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,+OiIGDD4/3L//Pj58/7/fv/8+fn7/v9///z/+f/+////4H/g5+Dn4Oeg5gDmAP/g/+AQAAAAAAA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,8KAg+eHw/8z//nD54/f/zP/+9/3j///u//7//f///+7+EP4A/gD+BP4A/gD+AP4A/hoB8wEiAEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0MDQ8PTc//P//PDw99//8//8+Pb33//7//z49vff//vwAPAA8ADwQPAQ8ADgAPAA8AD//5+xAwE')]
    procedure TestFilterSlideTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cPcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,+/vxfXDggh7///F9c+fOXv//+/1//95////////////vr+6nuPeQ8UAXYAOxj//f753/mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,+/nz8fDw8J7//fPx8/f+3v//9/fz9/7f//////v//9/jz+OPo4/jhWOBYwGDA+MD8wXf6Nf5w/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,8ubn5+/HwID//vfn78fOzP/+/+//38/f//7///////++Pj4/Pn88f7x/vP+4/7gf+AHwAPAA0AA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,yJiYnLw8PH7//Pj9//98fv//+v////1+///7/////f7j4OPg4+DH8Mfwx/CP8I/4z/yP+J/43/w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,x4eDozMxcXn///Pj8/f9ef///+/39/////////f////mH+Yfxh/PD88Pjw+PB5+Hn4cfhz/DP8I')]
    procedure TestFilterSmoothMagnify(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,+/vxdXDAgp7/+/F1c8fO3v/78/13997//////3f///9vr++nr/efcV0XUYO/j/ef5p37mNuZw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,+3t5dXDAgp7/e3l1c8fO3v97e/13997///9//3f/3/8Hrw+/r/ef8V0XUYO/j/ef7537mNuZw/8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/vz8dHDAgp7//Px1c8fO3v////d399//////9//3///vwO+Av4Af9V//EZu/n/+d/5n/mP+Z//8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,+/v5dXDAgJD///l1c8fO3P//+/1399/9///////////vr+//r/cf8V0XEYu/h/eQ74D7gPuAwAA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,+/v5dXDCggD///l1c8fOTP//+/1398/t/////////+/v/+//v/cf9V//EZu/n/+d/5kAAAAAAAA')]
    procedure TestFilterSwipeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,9/PxcXBCg5////Fxc0fP3///+XF359//////f/f/3///a/1n/WcvB20PcZw/nPeZ5p/rn9v/w/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,8/Px8HBCg4////Hxc0fPz///8fl359/f/////////9/97//h73OvQa0HUY5fnLcd9h/rn+v/4f8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,9/Px8HBCw8////Hxc0fPz///8/l398//////////7//Nr/+l//fvd60DkY/Xnp8Om43xz/X/9P8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,+/vx8PBiwsv///Hx82fOz///8/n3d87////////////nr++n77Xvc/8DmYOzn78Om4zaz/XP9P8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,+/vx8PCwoIH///Hx8/fuzf//8/n//+7P/////////8/zr+en77f/sf6D9oHMz8nP247Nx+zn/v8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,+/v58PDwoIH///nx8/fuzf//+/3///7v/////////+/9p/un97f+sf6T9oP2z/zP6Y7rjvXP5O8')]
    procedure TestFilterSwirl(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,//vxcGDCgo////FwY8fOz///83lz587f//////v/7//tr+2n7SetcckB0AWXjbee5JzqnOmf4f8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,/+Pg4GDCw8f/+/DgY8fPz///8/lz78/v//////v////Nj9wl7SXsMZgAkAbWnp6O0o/wR/L/+H8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,/+ZAQMDS8/P//nBgw9f/////dXHz3//////////////piZ0hjCDMMOwD2B+Sj6qHygPIP+U//g8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,/85AAMDS8vD//nBgw9f+/P//dXPz3/////////v////hQM1g3GGcd+wH6A/AgcqDxB/ikPEj/P8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterSwirlTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,AP///////wB/////////TH////////9Mf/////////8AAAAA//8AAAAA//8AAP//AAAAAP//AAA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,UlV+VX9Vf1T//f71f1f/3P/9//X/9//9//3/9//////d3TMz3d0zM93dMzPd3TMz3d0zM93dMzM')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,Wn5aWlpaSlr//nr7299O3v//fv/f317e//9//9/f//9VVVVVVVVVVf//VVUAAP//VVVVVVVVVVQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,BBYXBl8GHgT/fvfmX8deTP9//+9fz19N/3///1/ff29ERP//VVUiIkRE//9VVSIiRET//1VVIiI')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,AfsBaSlhIf9//2Hpa2dv////4+v77+/////j6//////VVdVVgAAAAAAAAAD/////1VXVVYAAAAA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,//2k/wD/pK3//eT/Q//urf//9/9T///////3/1P///8REVVVERFVVRERVVUREVVVERFVVRER//8')]
    procedure TestFilterTiler(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+/v5dWDAgp7///l1Y8fO3v//+/1318////////ff7//vr++nrycPcU0FAQe2h+Yf5JjImoOZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+/v5dXDCgpb///l1c8fO3v//+/1/587+///////v7//vr++nr2ePcU0HQQO2heaf5J3ImIOZw/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,+/v5dXDAgp7///l1c8fO3v//+/1318/////////f7//vr++nr/cPcU0FEYO2h/ad5JnImMOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+/v5dXDAgp7///l1c8fO3v//+/1399////////f////vr++nr+evcckH8YO2h/af5J3InMO5w/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,+/v5dXDAgp7///l1c8fO3v//+/13597///////fv///vp++nr7ePcV0VUYO2j/af5J3amMOZw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr/cPcV0HEYO+j/ad5JnbmMOZw/8')]
    procedure TestFilterToons(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,/+BAwPHKCgr/+HDg889OTv/9d/H3z87P////8f///v8QUBBYwVjIj+r/k/9LfxtmH2Y3Zn5mfgA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,//xAwODaGhD//HDg499eXP//f/Dz39/f/////P////8xJkFnYv9j/p/fo53/BRuHPwQ+AHwkcAA')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+ZAYMDSGx///nBgw9dfX///d3Dz319/////8Pv/f38XOAUYHx03/T79f55/CX+B3wD/AP4I5gI')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/84AAMDy+nL//nBgw/f+fv//d3fz//9///////////8BhAOGBtYU0xx5BHlAf4IZBuBf4P3g4AA')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/85AQMDS+vL//nBgw9f+/v//d3P73/////////////8XmAPgB4AHwifCCOoDYAfqAnAEYABABEA')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterWaterTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13185///////ff7//vr++nr/evcV0TUYO/j/ef5p3bmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,+/vxdXDAgp7///F1c8fO3v//8/33197///////ff///vr++nr/evcfkHMYO/j/ed5537mMO5w/8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,+/vxdXDAgh7///F1c8fOXv//8/13595///////fv///vp++nv7e/cV0DEYO/j/ef5537nNOZw/8')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+/vxdXDAgh7///F1c8fOXv//8/13185///////ff7//vr++nr7evdV0HcYO/j/ef5p3bmduZw/8')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,+/vxdXDAgp7///F1c8fO3v//8/1399////////f////vr++nr7cPcV0HEYe/j/ed55nrmNuYw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,+/vxdXCAgh7///F1c8fOXv//8/13595///////fv///vr++nr/e/cV0HUYO/j/ef5p3bmNO5w/8')]
    procedure TestFilterWave(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+/vxcGDCgp////FwY8fO3///8/lz587///////v/7//tr+2nraetcUkRUAG3j/ef5JzqmMufw/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,/+vhYGDCgp//+/FgY8fO3///83lzz87f//////v/7//sp+yn7SescdgAUAGUjbae5JzqmOmf4f8')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,/+5gQEDS0pv//nBgQ9fe3///d3Fz3/7f//////v////sp+yn7CWsdJgAUACUnbSe8JzqjOnf4f8')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,/85AAMDS8vP//nBgw9f+////d3Hz3/////////v////tr+yv7CWMcQgRAREUnZQclAjije3P9f8')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterWaveTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,+/vxdXDAgh7///F1c8fOXv//8/13999///////f////vr++nr7cfcV0XEYO/j/ed5pnbmNuZw/8')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+/nxdGDAgZ7//fF1Y8ePnv//9X1z19/+///9///f///tt+2zvzOdcfkRYAG1gfWP5o/ZjsO+w/8')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/+vgYGDCgwf///BhY8fPT///9Xlz78/f///9//v////Mo/6z7DecM9gDEAI0mXS/wLeKn5q5I7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,/+hgQMDQkfv/+PDgw9ff////9/Xz/9////////v////8p8SgHDE8cLgBoAJEn2AT4BvpG+u/v98')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,/+9AQMDScv7//3Bgw9d+/v//dnPz33/+///////////sn+yPHG88dRgQAQwggPxfdBxBGNHY9/w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,/84AAMDS+vL//nBgw9f+/v//d2P73///////7/////8fgAeAB4AHyD/gAOBzQK/IC4APgAJgAAA')]
    procedure TestFilterWiggleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,58fnh4fP//////fnx8/////////////////////////+f/5/73/tf/z/+f/5//////////////8')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,x8fP//////////////////////////////////////////////////////////////////////8')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,+fP35+fn8/H///fn5+f//f///+/v7///////7//v////v/9//3/+f/5//3//f/9//7//v/+f/48')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,8+fP38/Px+P/////z8/P7///////38/v///////fz+8EAAQACAAIAAgACgAIAA8AAYAA4ABwAAg')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,98/P34+PhwH/////z8/PTf//////399v///////////5//n/8//z//P/4//Z/+v/4f/mv8Jvw/8')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,8/fnx8fHw4H///fnx8fPzf/////fz8/v///////////+f/5//n/+//z//n/6f/r/+l/yb+N3w/8')]
    procedure TestFilterWrap(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress, ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress, ARightStartProgress, ARightControl1Progress, ARightControl2Progress, ARightEndProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

  TOpenSkSvg = class(TSkSvg)
    property DrawCacheKind;
  end;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  System.UITypes,
  System.IOUtils,
  FMX.Objects,
  FMX.Controls,
  FMX.Utils,
  FMX.Forms;

{ TFMXEffectsTests }

procedure TFMXEffectsTests.DoTestFilter<T>(const ASvgFileName: string;
  APictureWidth, APictureHeight, AScale: Integer; AGrayscale: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string;
  const AFilterSetup: TObjectSetup<T>);
var
  LFilter: TFilterBaseFilter;
  LInputBitmap: TBitmap;
begin
  LFilter := T.Create(nil);
  try
    if Assigned(AFilterSetup) then
      AFilterSetup(LFilter);
    LInputBitmap := TBitmap.Create;
    try
      LInputBitmap.SetSize(Round(APictureWidth), Round(APictureHeight));
      LInputBitmap.BitmapScale := 1;
      LInputBitmap.Clear(TAlphaColors.Null);
      PaintPicture(LInputBitmap, RectF(0, 0, APictureWidth, APictureHeight), ASvgFileName, TSkSvgWrapMode.Stretch, AGrayscale);
      LFilter.Input := LInputBitmap;
      Assert.AreSimilar(AExpectedImageHash, LFilter.Output.ToSkImage, AMinSimilarity);
    finally
      LInputBitmap.Free;
    end;
  finally
    LFilter.Free;
  end;
end;

procedure TFMXEffectsTests.DoTestInBitmap<T>(const ASvgFileName: string;
  APictureWidth, APictureHeight, AScale: Integer; AGrayscale: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string;
  const AEffectSetup: TObjectSetup<T>);
var
  LApplyEffectProc: TProc<TBitmap>;
  LBitmap: TBitmap;
  LBitmapRect: TRectF;
  LEffect: TEffect;
  LEffectRect: TRectF;
  LPictureRect: TRectF;
begin
  LEffect := T.Create(nil);
  try
    if Assigned(AEffectSetup) then
      AEffectSetup(LEffect);

    LPictureRect := RectF(0, 0, APictureWidth, APictureHeight);
    LEffectRect := LEffect.GetRect(LPictureRect);
    LBitmapRect := LPictureRect + LEffectRect;
    LBitmapRect.Offset(-LBitmapRect.Left, -LBitmapRect.Top);

    if LEffectRect.Left < 0 then
    begin
      LPictureRect.Offset(-LEffectRect.Left, 0);
      LEffectRect.Offset(-LEffectRect.Left, 0);
    end;
    if LEffectRect.Top < 0 then
    begin
      LPictureRect.Offset(0, -LEffectRect.Top);
      LEffectRect.Offset(0, -LEffectRect.Top);
    end;

    LApplyEffectProc :=
      procedure(ABitmap: TBitmap)
      var
        LEffectBitmap: TBitmap;
        R: TRectF;
      begin
        R := TRectF.Create(LEffect.GetOffset, APictureWidth, APictureHeight);
        ABitmap.Canvas.BeginScene;
        try
          LEffectBitmap := TBitmap.Create;
          try
            LEffectBitmap.SetSize(Round(LEffectRect.Width), Round(LEffectRect.Height));
            LEffectBitmap.BitmapScale := 1;
            if not (TEffectStyle.DisablePaintToBitmap in LEffect.EffectStyle) then
            begin
              LEffectBitmap.Clear(TAlphaColors.Null);
              PaintPicture(LEffectBitmap, R, ASvgFileName, TSkSvgWrapMode.Stretch, AGrayscale);
            end
            else
              LEffectBitmap.ClearRect(R, 0);
            LEffect.ProcessEffect(LEffectBitmap.Canvas, LEffectBitmap, 1);

            ABitmap.Canvas.DrawBitmap(LEffectBitmap, RectF(0, 0, LEffectBitmap.Width, LEffectBitmap.Height), LEffectRect, 1, True);
          finally
            LEffectBitmap.Free;
          end;
        finally
          ABitmap.Canvas.EndScene;
        end;
      end;

    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(Round(LBitmapRect.Width), Round(LBitmapRect.Height));
      LBitmap.BitmapScale := AScale;
      LBitmap.Clear(TAlphaColors.Null);
      if not (TEffectStyle.DisablePaint in LEffect.EffectStyle) then
      begin
        if not (TEffectStyle.AfterPaint in LEffect.EffectStyle) then
          LApplyEffectProc(LBitmap);
        PaintPicture(LBitmap, LPictureRect, ASvgFileName, TSkSvgWrapMode.Stretch, AGrayscale);
        if TEffectStyle.AfterPaint in LEffect.EffectStyle then
          LApplyEffectProc(LBitmap);
      end
      else
        LApplyEffectProc(LBitmap);

      Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
    finally
      LBitmap.Free;
    end;
  finally
    LEffect.Free;
  end;
end;

procedure TFMXEffectsTests.DoTestInControl<T>(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string;
  const AEffectSetup: TObjectSetup<T>);
var
  LBitmap: TBitmap;
  LControlRect: TRectF;
  LEffect: TEffect;
  LScene: IScene;
  LSceneScale: Single;
  LSvgControl: TSkSvg;
  LUpdateRect: TRectF;
begin
  LScene := Application.MainForm as IScene;
  if Assigned(LScene) then
    LSceneScale := LScene.GetSceneScale
  else
    LSceneScale := 1;
  LSvgControl := TSkSvg.Create(nil);
  try
    TOpenSkSvg(LSvgControl).DrawCacheKind := TSkDrawCacheKind.Never;
    LSvgControl.Visible := False;
    LSvgControl.Scale.Point := PointF(1/LSceneScale, 1/LSceneScale);
    LSvgControl.SetBounds(0, 0, AControlWidth, AControlHeight);
    LSvgControl.Svg.Source := TFile.ReadAllText(SvgAssetsPath + ASvgFileName);
    LSvgControl.Svg.GrayScale := AGrayscale;
    LSvgControl.Svg.WrapMode := TSkSvgWrapMode.Stretch;
    LSvgControl.SetNewScene(LScene);

    LEffect := T.Create(LSvgControl);
    if Assigned(AEffectSetup) then
      AEffectSetup(LEffect);
    LEffect.Parent := LSvgControl;

    LUpdateRect := LSvgControl.UpdateRect;
    LControlRect := LSvgControl.LocalRect;
    MultiplyRect(LControlRect, LSvgControl.Scale.X, LSvgControl.Scale.Point.Y);
    LControlRect.Offset(-LUpdateRect.Left, -LUpdateRect.Top);

    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(Round(LUpdateRect.Width * LSceneScale), Round(LUpdateRect.Height * LSceneScale));
      LBitmap.BitmapScale := LSceneScale * AScale;
      LBitmap.Canvas.BeginScene;
      try
        LBitmap.Canvas.Clear(TAlphaColors.Null);
        LSvgControl.PaintTo(LBitmap.Canvas, LControlRect);
      finally
        LBitmap.Canvas.EndScene;
      end;
      Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
    finally
      LBitmap.Free;
    end;
  finally
    LSvgControl.Free;
  end;
end;

procedure TFMXEffectsTests.PaintPicture(ATargetBitmap: TBitmap;
  const ADest: TRectF; const ASvgFileName: string;
  const AWrapMode: TSkSvgWrapMode; const AGrayscale: Boolean);
begin
  ATargetBitmap.SkiaDraw(
    procedure(const ACanvas: ISkCanvas)
    var
      LSvgBrush: TSkSvgBrush;
    begin
      LSvgBrush := TSkSvgBrush.Create;
      try
        LSvgBrush.Source := TFile.ReadAllText(SvgAssetsPath + ASvgFileName);
        LSvgBrush.GrayScale := AGrayscale;
        LSvgBrush.WrapMode := AWrapMode;
        LSvgBrush.Render(ACanvas, ADest, 1);
      finally
        LSvgBrush.Free;
      end;
    end, False);
end;

procedure TFMXEffectsTests.TestBitmapAffineTransformEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AScaleProgress, ACenterProgress, ARotationProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TAffineTransformEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TAffineTransformEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Rotation := InterpolateRotation(-180, 180, ARotationProgress);
      AEffect.Scale := InterpolateSingle(0.05, 4, AScaleProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBandedSwirlEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ABandsProgress, AStrengthProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBandedSwirlEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandedSwirlEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Bands := InterpolateSingle(0, 20, ABandsProgress);
      AEffect.Strength := InterpolateSingle(-70, 70, AStrengthProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBandedSwirlTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress,
  AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TBandedSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandedSwirlTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Strength := InterpolateSingle(0, 10, AStrengthProgress);
      AEffect.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBandsEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress,
  ABandDensityProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBandsEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandsEffect)
    begin
      AEffect.BandDensity := InterpolateSingle(0, 150, ABandDensityProgress);
      AEffect.BandIntensity := InterpolateSingle(0, 1, ABandIntensityProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBevelEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress,
  ASizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBevelEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBevelEffect)
    begin
      AEffect.Direction := InterpolateRotation(0, 360, ADirectionProgress);
      AEffect.Size := Round(InterpolateSingle(0, 9, ASizeProgress));
    end);
end;

procedure TFMXEffectsTests.TestBitmapBlindTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TBlindTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlindTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.NumberOfBlinds := InterpolateSingle(2, 15, ANumberOfBlindsProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBloodTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TBloodTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBloodTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBloomEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress,
  ABaseIntensityProgress, ABloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBloomEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBloomEffect)
    begin
      AEffect.BloomIntensity := InterpolateSingle(0, 1, ABloomIntensityProgress);
      AEffect.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AEffect.BloomSaturation := InterpolateSingle(0, 1, ABloomSaturationProgress);
      AEffect.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlurEffect)
    begin
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBlurTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TBlurTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlurTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBoxBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TBoxBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBoxBlurEffect)
    begin
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapBrightTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TBrightTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBrightTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapCircleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TCircleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCircleTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
      AEffect.CircleSize := InterpolateSingle(0, 2, ACircleSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapColorKeyAlphaEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TColorKeyAlphaEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TColorKeyAlphaEffect)
    begin
      AEffect.ColorKey := InterpolateColor($7F001FFF, TAlphaColors.Orange, AColorProgress);
      AEffect.Tolerance := InterpolateSingle(0, 1, AToleranceProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapContrastEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress,
  AContrastProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TContrastEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TContrastEffect)
    begin
      AEffect.Brightness := InterpolateSingle(-1, 1, ABrightnessProgress);
      AEffect.Contrast := InterpolateSingle(0, 2, AContrastProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapCropEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress,
  ABottomRightProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TCropEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCropEffect)
    begin
      AEffect.LeftTop := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AEffect.RightBottom := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
    end);
end;

procedure TFMXEffectsTests.TestBitmapCrumpleTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TCrumpleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCrumpleTransitionEffect)
    begin
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapDirectionalBlurEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TDirectionalBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDirectionalBlurEffect)
    begin
      AEffect.Angle := InterpolateRotation(0, 360, AAngleProgress);
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapDissolveTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TDissolveTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDissolveTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapDropTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TDropTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDropTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapEmbossEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress,
  AWidthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TEmbossEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TEmbossEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 1, AAmountProgress);
      AEffect.Width := InterpolateSingle(0, 10, AWidthProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapFadeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TFadeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFadeTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapFillEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TFillEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFillEffect)
    begin
      AEffect.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapFillRGBEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TFillRGBEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFillRGBEffect)
    begin
      AEffect.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapGaussianBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TGaussianBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGaussianBlurEffect)
    begin
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapGloomEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress,
  ABaseIntensityProgress, AGloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TGloomEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGloomEffect)
    begin
      AEffect.GloomIntensity := InterpolateSingle(0, 1, AGloomIntensityProgress);
      AEffect.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AEffect.GloomSaturation := InterpolateSingle(0, 1, AGloomSaturationProgress);
      AEffect.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapGlowEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity,
  AColorProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TGlowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGlowEffect)
    begin
      AEffect.GlowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapHueAdjustEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AHueProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<THueAdjustEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: THueAdjustEffect)
    begin
      AEffect.Hue := InterpolateSingle(-1, 1, AHueProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapInnerGlowEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ASoftnessProgress, AOpacity, AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TInnerGlowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TInnerGlowEffect)
    begin
      AEffect.GlowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapInvertEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TInvertEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestBitmapLineTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress,
  AFuzzyAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TLineTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TLineTransitionEffect)
    begin
      AEffect.Origin := PointF(InterpolateSingle(0, AControlWidth, AOriginProgress), InterpolateSingle(0, AControlHeight, 1-AOriginProgress));
      AEffect.Normal := PointF(InterpolateSingle(0, AControlWidth, ANormalProgress), InterpolateSingle(0, AControlHeight, 1-ANormalProgress));
      AEffect.OffsetProp := PointF(InterpolateSingle(0, AControlWidth, AOffsetPropProgress), InterpolateSingle(0, AControlHeight, 1-AOffsetPropProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapMagnifyEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TMagnifyEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TMagnifyEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AEffect.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapMagnifyTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TMagnifyTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TMagnifyTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapMaskToAlphaEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AGrayscale: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TMonochromeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, AGrayscale, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestBitmapMonochromeEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TMonochromeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestBitmapNormalBlendEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TNormalBlendEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TNormalBlendEffect)
    var
      LBitmap: TBitmap;
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.LoadFromFile(LImagesPath + ATargetImageFileName);
        AEffect.Target.SetSize(LBitmap.Width, LBitmap.Height);
        AEffect.Target.Canvas.BeginScene;
        try
          AEffect.Target.Canvas.Clear(TAlphaColors.Null);
          AEffect.Target.Canvas.DrawBitmap(LBitmap, LBitmap.BoundsF, LBitmap.BoundsF, 0.5, True);
        finally
          AEffect.Target.Canvas.EndScene;
        end;
      finally
        LBitmap.Free;
      end;
    end);
end;

procedure TFMXEffectsTests.TestBitmapPaperSketchEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TPaperSketchEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPaperSketchEffect)
    begin
      AEffect.BrushSize := InterpolateSingle(0.6, 10, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapPencilStrokeEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TPencilStrokeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPencilStrokeEffect)
    begin
      AEffect.BrushSize := InterpolateSingle(1, 19, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapPerspectiveTransformEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ATopLeftProgress, ATopRightProgress, ABottomRightProgress,
  ABottomLeftProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TPerspectiveTransformEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPerspectiveTransformEffect)
    begin
      AEffect.TopLeft := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AEffect.TopRight := PointF(InterpolateSingle(0, AControlWidth, 1-ATopRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopRightProgress));
      AEffect.BottomRight := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
      AEffect.BottomLeft := PointF(InterpolateSingle(0, AControlWidth, 1-ABottomLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomLeftProgress));
    end);
end;

procedure TFMXEffectsTests.TestBitmapPinchEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TPinchEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPinchEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AEffect.Strength := InterpolateSingle(0, 20, AStrengthProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapPixelateEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TPixelateEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPixelateEffect)
    begin
      AEffect.BlockCount := InterpolateSingle(1, Min(AControlWidth, AControlHeight), AProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapPixelateTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TPixelateTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPixelateTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapRadialBlurEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TRadialBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRadialBlurEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapRasterEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TRasterEffect>(ASvgFileName, AControlWidth, AControlHeight,
    AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestBitmapReflectionEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ALengthProgress, AOpacity, AOffsetProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TReflectionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TReflectionEffect)
    begin
      AEffect.Length := InterpolateSingle(0, 1, ALengthProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Offset := Round(InterpolateSingle(0, 9, AOffsetProgress));
    end);
end;

procedure TFMXEffectsTests.TestBitmapRippleEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  AAmplitudeProgress, AFrequencyProgress, APhaseProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TRippleEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRippleEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Amplitude := InterpolateSingle(0, 1, AAmplitudeProgress);
      AEffect.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
      AEffect.Phase := InterpolateSingle(-20, 20, APhaseProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapRippleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TRippleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRippleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapRotateCrumpleTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TRotateCrumpleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRotateCrumpleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSaturateTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TSaturateTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSaturateTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSepiaEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TSepiaEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSepiaEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 1, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapShadowEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity,
  AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TShadowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TShadowEffect)
    begin
      AEffect.ShadowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Distance := InterpolateSingle(0, 10, ADistanceProgress);
      AEffect.Direction := InterpolateRotation(0, 360, ADistanceProgress);
      AEffect.Softness := InterpolateSingle(0, 3, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapShapeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TShapeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TShapeTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSharpenEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TSharpenEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSharpenEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 2, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSlideTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TSlideTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSlideTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.SlideAmount := PointF(InterpolateSingle(0, AControlWidth, ASlideAmountProgress), InterpolateSingle(0, AControlHeight, 1-ASlideAmountProgress));
    end);
end;

procedure TFMXEffectsTests.TestBitmapSmoothMagnifyEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ARadiusProgress, AMagnificationProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TSmoothMagnifyEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSmoothMagnifyEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.InnerRadius := InterpolateSingle(0, 1, ARadiusProgress) - 0.02;
      AEffect.OuterRadius := InterpolateSingle(0, 1, ARadiusProgress) + 0.02;
      AEffect.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSwipeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AMousePointProgress, ACornerPointProgress, ADeepProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TSwipeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwipeTransitionEffect)
    begin
      AEffect.MousePoint := PointF(InterpolateSingle(0, AControlWidth, AMousePointProgress), InterpolateSingle(0, AControlHeight, 0.8*AMousePointProgress));
      AEffect.CornerPoint := PointF(InterpolateSingle(0, AControlWidth, ACornerPointProgress), InterpolateSingle(0, AControlHeight, 0.8*ACornerPointProgress));
      AEffect.Deep := InterpolateSingle(0, 100, ADeepProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Back.SetSize(AEffect.Target.Width, AEffect.Target.Height);
      AEffect.Back.Canvas.BeginScene;
      try
        AEffect.Back.Canvas.Clear(TAlphaColors.White);
        AEffect.Back.Canvas.DrawBitmap(AEffect.Target, AEffect.Target.BoundsF, AEffect.Target.BoundsF, 0.4, True);
      finally
        AEffect.Back.Canvas.EndScene;
      end;
    end);
end;

procedure TFMXEffectsTests.TestBitmapSwirlEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TSwirlEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.SpiralStrength := InterpolateSingle(0, 1, ASpiralProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapSwirlTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AStrengthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Strength := InterpolateSingle(0, 10, AStrengthProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapTilerEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress,
  AHorizontalTileCountProgress, AHorizontalOffsetProgress,
  AVerticalOffsetProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TTilerEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TTilerEffect)
    begin
      AEffect.VerticalTileCount := InterpolateSingle(0, 20, AVerticalTileCountProgress);
      AEffect.HorizontalTileCount := InterpolateSingle(0, 20, AHorizontalTileCountProgress);
      AEffect.HorizontalOffset := InterpolateSingle(0, 1, AHorizontalOffsetProgress);
      AEffect.VerticalOffset := InterpolateSingle(0, 1, AVerticalOffsetProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapToonsEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInBitmap<TToonEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TToonEffect)
    begin
      AEffect.Levels := InterpolateSingle(3, 15, ALevelsProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapWaterTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TWaterTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWaterTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapWaveEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATimeProgress,
  AWaveSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TWaveEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWaveEffect)
    begin
      AEffect.Time := InterpolateSingle(0, 2048, ATimeProgress);
      AEffect.WaveSize := InterpolateSingle(32, 256, AWaveSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestBitmapWaveTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapWiggleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInBitmap<TWiggleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWiggleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestBitmapWrapEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress,
  ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress,
  ARightStartProgress, ARightControl1Progress, ARightControl2Progress,
  ARightEndProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInBitmap<TWrapEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWrapEffect)
    begin
      AEffect.LeftStart := InterpolateSingle(0, 1, ALeftStartProgress);
      AEffect.LeftControl1 := InterpolateSingle(0, 1, ALeftControl1Progress);
      AEffect.LeftControl2 := InterpolateSingle(0, 1, ALeftControl2Progress);
      AEffect.LeftEnd := InterpolateSingle(0, 1, ALeftEndProgress);
      AEffect.RightStart := InterpolateSingle(0, 1, ARightStartProgress);
      AEffect.RightControl1 := InterpolateSingle(0, 1, ARightControl1Progress);
      AEffect.RightControl2 := InterpolateSingle(0, 1, ARightControl2Progress);
      AEffect.RightEnd := InterpolateSingle(0, 1, ARightEndProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlAffineTransformEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AScaleProgress, ACenterProgress, ARotationProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TAffineTransformEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TAffineTransformEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Rotation := InterpolateRotation(-180, 180, ARotationProgress);
      AEffect.Scale := InterpolateSingle(0.05, 4, AScaleProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBandedSwirlEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ABandsProgress, AStrengthProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TBandedSwirlEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandedSwirlEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Bands := InterpolateSingle(0, 20, ABandsProgress);
      AEffect.Strength := InterpolateSingle(-70, 70, AStrengthProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBandedSwirlTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress,
  AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TBandedSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandedSwirlTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Strength := InterpolateSingle(0, 10, AStrengthProgress);
      AEffect.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBandsEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress,
  ABandDensityProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TBandsEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBandsEffect)
    begin
      AEffect.BandDensity := InterpolateSingle(0, 150, ABandDensityProgress);
      AEffect.BandIntensity := InterpolateSingle(0, 1, ABandIntensityProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBevelEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress,
  ASizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TBevelEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBevelEffect)
    begin
      AEffect.Direction := InterpolateRotation(0, 360, ADirectionProgress);
      AEffect.Size := Round(InterpolateSingle(0, 9, ASizeProgress));
    end);
end;

procedure TFMXEffectsTests.TestControlBlindTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TBlindTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlindTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.NumberOfBlinds := InterpolateSingle(2, 15, ANumberOfBlindsProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlBloodTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TBloodTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBloodTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlBloomEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress,
  ABaseIntensityProgress, ABloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TBloomEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBloomEffect)
    begin
      AEffect.BloomIntensity := InterpolateSingle(0, 1, ABloomIntensityProgress);
      AEffect.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AEffect.BloomSaturation := InterpolateSingle(0, 1, ABloomSaturationProgress);
      AEffect.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlurEffect)
    begin
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBlurTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TBlurTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBlurTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlBoxBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TBoxBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBoxBlurEffect)
    begin
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlBrightTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TBrightTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TBrightTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlCircleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TCircleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCircleTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
      AEffect.CircleSize := InterpolateSingle(0, 2, ACircleSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlColorKeyAlphaEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TColorKeyAlphaEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TColorKeyAlphaEffect)
    begin
      AEffect.ColorKey := InterpolateColor($7F001FFF, TAlphaColors.Orange, AColorProgress);
      AEffect.Tolerance := InterpolateSingle(0, 1, AToleranceProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlContrastEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress,
  AContrastProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TContrastEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TContrastEffect)
    begin
      AEffect.Brightness := InterpolateSingle(-1, 1, ABrightnessProgress);
      AEffect.Contrast := InterpolateSingle(0, 2, AContrastProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlCropEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress,
  ABottomRightProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TCropEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCropEffect)
    begin
      AEffect.LeftTop := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AEffect.RightBottom := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
    end);
end;

procedure TFMXEffectsTests.TestControlCrumpleTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TCrumpleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TCrumpleTransitionEffect)
    begin
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlDirectionalBlurEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TDirectionalBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDirectionalBlurEffect)
    begin
      AEffect.Angle := InterpolateRotation(0, 360, AAngleProgress);
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlDissolveTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TDissolveTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDissolveTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlDropTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TDropTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TDropTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlEmbossEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress,
  AWidthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TEmbossEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TEmbossEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 1, AAmountProgress);
      AEffect.Width := InterpolateSingle(0, 10, AWidthProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlFadeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TFadeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFadeTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlFillEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TFillEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFillEffect)
    begin
      AEffect.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlFillRGBEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TFillRGBEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TFillRGBEffect)
    begin
      AEffect.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlGaussianBlurEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TGaussianBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGaussianBlurEffect)
    begin
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlGloomEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress,
  ABaseIntensityProgress, AGloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TGloomEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGloomEffect)
    begin
      AEffect.GloomIntensity := InterpolateSingle(0, 1, AGloomIntensityProgress);
      AEffect.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AEffect.GloomSaturation := InterpolateSingle(0, 1, AGloomSaturationProgress);
      AEffect.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlGlowEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity,
  AColorProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TGlowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TGlowEffect)
    begin
      AEffect.GlowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlHueAdjustEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AHueProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<THueAdjustEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: THueAdjustEffect)
    begin
      AEffect.Hue := InterpolateSingle(-1, 1, AHueProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlInnerGlowEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ASoftnessProgress, AOpacity, AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TInnerGlowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TInnerGlowEffect)
    begin
      AEffect.GlowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Softness := InterpolateSingle(0, 9, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlInvertEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TInvertEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestControlLineTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress,
  AFuzzyAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TLineTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TLineTransitionEffect)
    begin
      AEffect.Origin := PointF(InterpolateSingle(0, AControlWidth, AOriginProgress), InterpolateSingle(0, AControlHeight, 1-AOriginProgress));
      AEffect.Normal := PointF(InterpolateSingle(0, AControlWidth, ANormalProgress), InterpolateSingle(0, AControlHeight, 1-ANormalProgress));
      AEffect.OffsetProp := PointF(InterpolateSingle(0, AControlWidth, AOffsetPropProgress), InterpolateSingle(0, AControlHeight, 1-AOffsetPropProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlMagnifyEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TMagnifyEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TMagnifyEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AEffect.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlMagnifyTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TMagnifyTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TMagnifyTransitionEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlMaskToAlphaEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AGrayscale: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TMonochromeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, AGrayscale, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestControlMonochromeEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TMonochromeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestControlNormalBlendEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TNormalBlendEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TNormalBlendEffect)
    var
      LBitmap: TBitmap;
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.LoadFromFile(LImagesPath + ATargetImageFileName);
        AEffect.Target.SetSize(LBitmap.Width, LBitmap.Height);
        AEffect.Target.Canvas.BeginScene;
        try
          AEffect.Target.Canvas.Clear(TAlphaColors.Null);
          AEffect.Target.Canvas.DrawBitmap(LBitmap, LBitmap.BoundsF, LBitmap.BoundsF, 0.5, True);
        finally
          AEffect.Target.Canvas.EndScene;
        end;
      finally
        LBitmap.Free;
      end;
    end);
end;

procedure TFMXEffectsTests.TestControlPaperSketchEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TPaperSketchEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPaperSketchEffect)
    begin
      AEffect.BrushSize := InterpolateSingle(0.6, 10, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlPencilStrokeEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TPencilStrokeEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPencilStrokeEffect)
    begin
      AEffect.BrushSize := InterpolateSingle(1, 19, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlPerspectiveTransformEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ATopLeftProgress, ATopRightProgress, ABottomRightProgress,
  ABottomLeftProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TPerspectiveTransformEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPerspectiveTransformEffect)
    begin
      AEffect.TopLeft := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AEffect.TopRight := PointF(InterpolateSingle(0, AControlWidth, 1-ATopRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopRightProgress));
      AEffect.BottomRight := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
      AEffect.BottomLeft := PointF(InterpolateSingle(0, AControlWidth, 1-ABottomLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomLeftProgress));
    end);
end;

procedure TFMXEffectsTests.TestControlPinchEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TPinchEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPinchEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AEffect.Strength := InterpolateSingle(0, 20, AStrengthProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlPixelateEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TPixelateEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPixelateEffect)
    begin
      AEffect.BlockCount := InterpolateSingle(1, Min(AControlWidth, AControlHeight), AProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlPixelateTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TPixelateTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TPixelateTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlRadialBlurEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TRadialBlurEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRadialBlurEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlRasterEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TRasterEffect>(ASvgFileName, AControlWidth, AControlHeight,
    AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestControlReflectionEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ALengthProgress, AOpacity, AOffsetProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TReflectionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TReflectionEffect)
    begin
      AEffect.Length := InterpolateSingle(0, 1, ALengthProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Offset := Round(InterpolateSingle(0, 9, AOffsetProgress));
    end);
end;

procedure TFMXEffectsTests.TestControlRippleEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  AAmplitudeProgress, AFrequencyProgress, APhaseProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TRippleEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRippleEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.Amplitude := InterpolateSingle(0, 1, AAmplitudeProgress);
      AEffect.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
      AEffect.Phase := InterpolateSingle(-20, 20, APhaseProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlRippleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TRippleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRippleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlRotateCrumpleTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TRotateCrumpleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TRotateCrumpleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlSaturateTransitionEffect(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TSaturateTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSaturateTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlSepiaEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TSepiaEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSepiaEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 1, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlShadowEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity,
  AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TShadowEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TShadowEffect)
    begin
      AEffect.ShadowColor := InterpolateColor($7FFF0000, TAlphaColors.Black, AColorProgress);
      AEffect.Opacity := InterpolateSingle(0, 1, AOpacity);
      AEffect.Distance := InterpolateSingle(0, 10, ADistanceProgress);
      AEffect.Direction := InterpolateRotation(0, 360, ADistanceProgress);
      AEffect.Softness := InterpolateSingle(0, 3, ASoftnessProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlShapeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TShapeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TShapeTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlSharpenEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TSharpenEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSharpenEffect)
    begin
      AEffect.Amount := InterpolateSingle(0, 2, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlSlideTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TSlideTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSlideTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.SlideAmount := PointF(InterpolateSingle(0, AControlWidth, ASlideAmountProgress), InterpolateSingle(0, AControlHeight, 1-ASlideAmountProgress));
    end);
end;

procedure TFMXEffectsTests.TestControlSmoothMagnifyEffect(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ARadiusProgress, AMagnificationProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TSmoothMagnifyEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSmoothMagnifyEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.InnerRadius := InterpolateSingle(0, 1, ARadiusProgress) - 0.02;
      AEffect.OuterRadius := InterpolateSingle(0, 1, ARadiusProgress) + 0.02;
      AEffect.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlSwipeTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AMousePointProgress, ACornerPointProgress, ADeepProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TSwipeTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwipeTransitionEffect)
    begin
      AEffect.MousePoint := PointF(InterpolateSingle(0, AControlWidth, AMousePointProgress), InterpolateSingle(0, AControlHeight, 0.8*AMousePointProgress));
      AEffect.CornerPoint := PointF(InterpolateSingle(0, AControlWidth, ACornerPointProgress), InterpolateSingle(0, AControlHeight, 0.8*ACornerPointProgress));
      AEffect.Deep := InterpolateSingle(0, 100, ADeepProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Back.SetSize(AEffect.Target.Width, AEffect.Target.Height);
      AEffect.Back.Canvas.BeginScene;
      try
        AEffect.Back.Canvas.Clear(TAlphaColors.White);
        AEffect.Back.Canvas.DrawBitmap(AEffect.Target, AEffect.Target.BoundsF, AEffect.Target.BoundsF, 0.4, True);
      finally
        AEffect.Back.Canvas.EndScene;
      end;
    end);
end;

procedure TFMXEffectsTests.TestControlSwirlEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TSwirlEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlEffect)
    begin
      AEffect.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AEffect.SpiralStrength := InterpolateSingle(0, 1, ASpiralProgress);
      AEffect.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlSwirlTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AStrengthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
      AEffect.Strength := InterpolateSingle(0, 10, AStrengthProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlTilerEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress,
  AHorizontalTileCountProgress, AHorizontalOffsetProgress,
  AVerticalOffsetProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TTilerEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TTilerEffect)
    begin
      AEffect.VerticalTileCount := InterpolateSingle(0, 20, AVerticalTileCountProgress);
      AEffect.HorizontalTileCount := InterpolateSingle(0, 20, AHorizontalTileCountProgress);
      AEffect.HorizontalOffset := InterpolateSingle(0, 1, AHorizontalOffsetProgress);
      AEffect.VerticalOffset := InterpolateSingle(0, 1, AVerticalOffsetProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlToonsEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestInControl<TToonEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TToonEffect)
    begin
      AEffect.Levels := InterpolateSingle(3, 15, ALevelsProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlWaterTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TWaterTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWaterTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlWaveEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATimeProgress,
  AWaveSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TWaveEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWaveEffect)
    begin
      AEffect.Time := InterpolateSingle(0, 2048, ATimeProgress);
      AEffect.WaveSize := InterpolateSingle(32, 256, AWaveSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestControlWaveTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TSwirlTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TSwirlTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlWiggleTransitionEffect(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LImagesPath: string;
begin
  LImagesPath := ImageAssetsPath;
  DoTestInControl<TWiggleTransitionEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWiggleTransitionEffect)
    begin
      AEffect.Progress := InterpolateSingle(0, 100, AProgress);
      AEffect.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
      AEffect.Target.LoadFromFile(LImagesPath + ATargetImageFileName);
    end);
end;

procedure TFMXEffectsTests.TestControlWrapEffect(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress,
  ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress,
  ARightStartProgress, ARightControl1Progress, ARightControl2Progress,
  ARightEndProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestInControl<TWrapEffect>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AEffect: TWrapEffect)
    begin
      AEffect.LeftStart := InterpolateSingle(0, 1, ALeftStartProgress);
      AEffect.LeftControl1 := InterpolateSingle(0, 1, ALeftControl1Progress);
      AEffect.LeftControl2 := InterpolateSingle(0, 1, ALeftControl2Progress);
      AEffect.LeftEnd := InterpolateSingle(0, 1, ALeftEndProgress);
      AEffect.RightStart := InterpolateSingle(0, 1, ARightStartProgress);
      AEffect.RightControl1 := InterpolateSingle(0, 1, ARightControl1Progress);
      AEffect.RightControl2 := InterpolateSingle(0, 1, ARightControl2Progress);
      AEffect.RightEnd := InterpolateSingle(0, 1, ARightEndProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterAffineTransform(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AScaleProgress, ACenterProgress, ARotationProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterAffineTransform>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterAffineTransform)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.Rotation := InterpolateRotation(-180, 180, ARotationProgress);
      AFilter.Scale := InterpolateSingle(0.05, 4, AScaleProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterBandedSwirl(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ABandsProgress, AStrengthProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterBandedSwirl>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterBandedSwirl)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.Bands := InterpolateSingle(0, 20, ABandsProgress);
      AFilter.Strength := InterpolateSingle(-70, 70, AStrengthProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterBandedSwirlTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress,
  AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterBandedSwirlTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterBandedSwirlTransition)
      begin
        AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
        AFilter.Strength := InterpolateSingle(0, 10, AStrengthProgress);
        AFilter.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterBands(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress,
  ABandDensityProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterBands>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterBands)
    begin
      AFilter.BandDensity := InterpolateSingle(0, 150, ABandDensityProgress);
      AFilter.BandIntensity := InterpolateSingle(0, 1, ABandIntensityProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterBlindTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterBlindTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterBlindTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.NumberOfBlinds := InterpolateSingle(2, 15, ANumberOfBlindsProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterBloodTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterBloodTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterBloodTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterBloom(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress,
  ABaseIntensityProgress, ABloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterBloom>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterBloom)
    begin
      AFilter.BloomIntensity := InterpolateSingle(0, 1, ABloomIntensityProgress);
      AFilter.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AFilter.BloomSaturation := InterpolateSingle(0, 1, ABloomSaturationProgress);
      AFilter.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterBlurTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterBlurTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterBlurTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterBoxBlur(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterBoxBlur>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterBoxBlur)
    begin
      AFilter.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterBrightTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterBrightTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterBrightTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterCircleTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterCircleTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterCircleTransition)
      begin
        AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
        AFilter.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
        AFilter.CircleSize := InterpolateSingle(0, 2, ACircleSizeProgress);
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterColorKeyAlpha(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterColorKeyAlpha>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterColorKeyAlpha)
    begin
      AFilter.ColorKey := InterpolateColor($7F001FFF, TAlphaColors.Orange, AColorProgress);
      AFilter.Tolerance := InterpolateSingle(0, 1, AToleranceProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterContrast(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress,
  AContrastProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterContrast>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterContrast)
    begin
      AFilter.Brightness := InterpolateSingle(-1, 1, ABrightnessProgress);
      AFilter.Contrast := InterpolateSingle(0, 2, AContrastProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterCrop(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress,
  ABottomRightProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterCrop>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterCrop)
    begin
      AFilter.LeftTop := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AFilter.RightBottom := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
    end);
end;

procedure TFMXEffectsTests.TestFilterCrumpleTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterCrumpleTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterCrumpleTransition)
      begin
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterDirectionalBlur(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterDirectionalBlur>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterDirectionalBlur)
    begin
      AFilter.Angle := InterpolateRotation(0, 360, AAngleProgress);
      AFilter.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterDissolveTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterDissolveTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterDissolveTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterDropTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterDropTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterDropTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterEmboss(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress,
  AWidthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterEmboss>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterEmboss)
    begin
      AFilter.Amount := InterpolateSingle(0, 1, AAmountProgress);
      AFilter.Width := InterpolateSingle(0, 10, AWidthProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterFadeTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterFadeTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterFadeTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterFill(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterFill>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterFill)
    begin
      AFilter.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterFillRGB(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterFillRGB>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterFillRGB)
    begin
      AFilter.Color := InterpolateColor($7F001FFF, TAlphaColors.White, AColorProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterGaussianBlur(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterGaussianBlur>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterGaussianBlur)
    begin
      AFilter.BlurAmount := InterpolateSingle(0.01, 10, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterGloom(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress,
  ABaseIntensityProgress, AGloomSaturationProgress,
  ABaseSaturationProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterGloom>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterGloom)
    begin
      AFilter.GloomIntensity := InterpolateSingle(0, 1, AGloomIntensityProgress);
      AFilter.BaseIntensity := InterpolateSingle(0, 1, ABaseIntensityProgress);
      AFilter.GloomSaturation := InterpolateSingle(0, 1, AGloomSaturationProgress);
      AFilter.BaseSaturation := InterpolateSingle(0, 1, ABaseSaturationProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterHueAdjust(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AHueProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterHueAdjust>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterHueAdjust)
    begin
      AFilter.Hue := InterpolateSingle(-1, 1, AHueProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterInvert(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterInvert>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestFilterLineTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress,
  AFuzzyAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterLineTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterLineTransition)
      begin
        AFilter.Origin := PointF(InterpolateSingle(0, AControlWidth, AOriginProgress), InterpolateSingle(0, AControlHeight, 1-AOriginProgress));
        AFilter.Normal := PointF(InterpolateSingle(0, AControlWidth, ANormalProgress), InterpolateSingle(0, AControlHeight, 1-ANormalProgress));
        AFilter.OffsetProp := PointF(InterpolateSingle(0, AControlWidth, AOffsetPropProgress), InterpolateSingle(0, AControlHeight, 1-AOffsetPropProgress));
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.FuzzyAmount := InterpolateSingle(0, 1, AFuzzyAmountProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterMagnify(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterMagnify>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterMagnify)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AFilter.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterMagnifyTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterMagnifyTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterMagnifyTransition)
      begin
        AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterMaskToAlpha(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AGrayscale: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterMonochrome>(ASvgFileName, AControlWidth, AControlHeight, AScale, AGrayscale, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestFilterMonochrome(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterMonochrome>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash, nil);
end;

procedure TFMXEffectsTests.TestFilterNormalBlend(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LBitmap := TBitmap.Create;
    try
      LBitmap.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
      LTarget.SetSize(LBitmap.Width, LBitmap.Height);
      LTarget.Canvas.BeginScene;
      try
        LTarget.Canvas.Clear(TAlphaColors.Null);
        LTarget.Canvas.DrawBitmap(LBitmap, LBitmap.BoundsF, LBitmap.BoundsF, 0.5, True);
      finally
        LTarget.Canvas.EndScene;
      end;
    finally
      LBitmap.Free;
    end;
    DoTestFilter<TFilterNormalBlend>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterNormalBlend)
      begin
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterPaperSketch(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterPaperSketch>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterPaperSketch)
    begin
      AFilter.BrushSize := InterpolateSingle(0.6, 10, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterPencilStroke(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABrushSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterPencilStroke>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterPencilStroke)
    begin
      AFilter.BrushSize := InterpolateSingle(1, 19, ABrushSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterPerspectiveTransform(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ATopLeftProgress, ATopRightProgress, ABottomRightProgress,
  ABottomLeftProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterPerspectiveTransform>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterPerspectiveTransform)
    begin
      AFilter.TopLeft := PointF(InterpolateSingle(0, AControlWidth, ATopLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopLeftProgress));
      AFilter.TopRight := PointF(InterpolateSingle(0, AControlWidth, 1-ATopRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ATopRightProgress));
      AFilter.BottomRight := PointF(InterpolateSingle(0, AControlWidth, ABottomRightProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomRightProgress));
      AFilter.BottomLeft := PointF(InterpolateSingle(0, AControlWidth, 1-ABottomLeftProgress), InterpolateSingle(0, AControlHeight, 0.8*ABottomLeftProgress));
    end);
end;

procedure TFMXEffectsTests.TestFilterPinch(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterPinch>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterPinch)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.Radius := InterpolateSingle(0, 1, ARadiusProgress);
      AFilter.Strength := InterpolateSingle(0, 20, AStrengthProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterPixelate(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterPixelate>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterPixelate)
    begin
      AFilter.BlockCount := InterpolateSingle(1, Min(AControlWidth, AControlHeight), AProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterPixelateTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterPixelateTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterPixelateTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterRadialBlur(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterRadialBlur>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterRadialBlur)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.BlurAmount := InterpolateSingle(0.01, 10, ABlurAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterRipple(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  AAmplitudeProgress, AFrequencyProgress, APhaseProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterRipple>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterRipple)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.Amplitude := InterpolateSingle(0, 1, AAmplitudeProgress);
      AFilter.Frequency := InterpolateSingle(0, 100, AFrequencyProgress);
      AFilter.Phase := InterpolateSingle(-20, 20, APhaseProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterRippleTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterRippleTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterRippleTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterRotateCrumpleTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterRotateCrumpleTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterRotateCrumpleTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterSaturateTransition(
  const ASvgFileName, ATargetImageFileName: string; AControlWidth,
  AControlHeight, AScale: Integer; AProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterSaturateTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterSaturateTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterSepia(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterSepia>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterSepia)
    begin
      AFilter.Amount := InterpolateSingle(0, 1, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterShapeTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterShapeTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterShapeTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterSharpen(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterSharpen>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterSharpen)
    begin
      AFilter.Amount := InterpolateSingle(0, 2, AAmountProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterSlideTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterSlideTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterSlideTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
        AFilter.SlideAmount := PointF(InterpolateSingle(0, AControlWidth, ASlideAmountProgress), InterpolateSingle(0, AControlHeight, 1-ASlideAmountProgress));
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterSmoothMagnify(
  const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  ACenterProgress, ARadiusProgress, AMagnificationProgress,
  AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterSmoothMagnify>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterSmoothMagnify)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.InnerRadius := InterpolateSingle(0, 1, ARadiusProgress) - 0.02;
      AFilter.OuterRadius := InterpolateSingle(0, 1, ARadiusProgress) + 0.02;
      AFilter.Magnification := InterpolateSingle(1, 5, AMagnificationProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterSwipeTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AMousePointProgress, ACornerPointProgress, ADeepProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBack: TBitmap;
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    LBack := TBitmap.Create;
    try
      LBack.SetSize(LTarget.Width, LTarget.Height);
      LBack.Canvas.BeginScene;
      try
        LBack.Canvas.Clear(TAlphaColors.White);
        LBack.Canvas.DrawBitmap(LTarget, LTarget.BoundsF, LTarget.BoundsF, 0.4, True);
      finally
        LBack.Canvas.EndScene;
      end;
      DoTestFilter<TFilterSwipeTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
        procedure (AFilter: TFilterSwipeTransition)
        begin
          AFilter.MousePoint := PointF(InterpolateSingle(0, AControlWidth, AMousePointProgress), InterpolateSingle(0, AControlHeight, 0.8*AMousePointProgress));
          AFilter.CornerPoint := PointF(InterpolateSingle(0, AControlWidth, ACornerPointProgress), InterpolateSingle(0, AControlHeight, 0.8*ACornerPointProgress));
          AFilter.Deep := InterpolateSingle(0, 100, ADeepProgress);
          AFilter.Target := LTarget;
          AFilter.Back := LBack;
        end);
    finally
      LBack.Free;
    end;
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterSwirl(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ACenterProgress,
  ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterSwirl>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterSwirl)
    begin
      AFilter.Center := PointF(InterpolateSingle(0, AControlWidth, ACenterProgress), InterpolateSingle(0, AControlHeight, 1-ACenterProgress));
      AFilter.SpiralStrength := InterpolateSingle(0, 1, ASpiralProgress);
      AFilter.AspectRatio := InterpolateSingle(0.5, 2, AAspectRatioProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterSwirlTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, AStrengthProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterSwirlTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterSwirlTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
        AFilter.Strength := InterpolateSingle(0, 10, AStrengthProgress);
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterTiler(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress,
  AHorizontalTileCountProgress, AHorizontalOffsetProgress,
  AVerticalOffsetProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterTiler>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterTiler)
    begin
      AFilter.VerticalTileCount := InterpolateSingle(0, 20, AVerticalTileCountProgress);
      AFilter.HorizontalTileCount := InterpolateSingle(0, 20, AHorizontalTileCountProgress);
      AFilter.HorizontalOffset := InterpolateSingle(0, 1, AHorizontalOffsetProgress);
      AFilter.VerticalOffset := InterpolateSingle(0, 1, AVerticalOffsetProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterToons(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterToon>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterToon)
    begin
      AFilter.Levels := InterpolateSingle(3, 15, ALevelsProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterWaterTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterWaterTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterWaterTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterWave(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ATimeProgress,
  AWaveSizeProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterWave>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterWave)
    begin
      AFilter.Time := InterpolateSingle(0, 2048, ATimeProgress);
      AFilter.WaveSize := InterpolateSingle(32, 256, AWaveSizeProgress);
    end);
end;

procedure TFMXEffectsTests.TestFilterWaveTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterSwirlTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterSwirlTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterWiggleTransition(const ASvgFileName,
  ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer;
  AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LTarget: TBitmap;
begin
  LTarget := TBitmap.Create;
  try
    LTarget.LoadFromFile(ImageAssetsPath + ATargetImageFileName);
    DoTestFilter<TFilterWiggleTransition>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
      procedure (AFilter: TFilterWiggleTransition)
      begin
        AFilter.Progress := InterpolateSingle(0, 100, AProgress);
        AFilter.RandomSeed := InterpolateSingle(0, 1, ARandomSeedProgress);
        AFilter.Target := LTarget;
      end);
  finally
    LTarget.Free;
  end;
end;

procedure TFMXEffectsTests.TestFilterWrap(const ASvgFileName: string;
  AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress,
  ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress,
  ARightStartProgress, ARightControl1Progress, ARightControl2Progress,
  ARightEndProgress: Single; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
begin
  DoTestFilter<TFilterWrap>(ASvgFileName, AControlWidth, AControlHeight, AScale, False, AMinSimilarity, AExpectedImageHash,
    procedure (AFilter: TFilterWrap)
    begin
      AFilter.LeftStart := InterpolateSingle(0, 1, ALeftStartProgress);
      AFilter.LeftControl1 := InterpolateSingle(0, 1, ALeftControl1Progress);
      AFilter.LeftControl2 := InterpolateSingle(0, 1, ALeftControl2Progress);
      AFilter.LeftEnd := InterpolateSingle(0, 1, ALeftEndProgress);
      AFilter.RightStart := InterpolateSingle(0, 1, ARightStartProgress);
      AFilter.RightControl1 := InterpolateSingle(0, 1, ARightControl1Progress);
      AFilter.RightControl2 := InterpolateSingle(0, 1, ARightControl2Progress);
      AFilter.RightEnd := InterpolateSingle(0, 1, ARightEndProgress);
    end);
end;

initialization
  TDUnitX.RegisterTestFixture(TFMXEffectsTests);
{$ELSE}
implementation
{$ENDIF}
end.
