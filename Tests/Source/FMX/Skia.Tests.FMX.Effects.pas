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
unit Skia.Tests.FMX.Effects;

interface

{$SCOPEDENUMS ON}
{$IF CompilerVersion >= 36}
  {$DEFINE SKIA_TESTS_FMX_EFFECTS}
{$ENDIF}

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
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,2VPyevfEQz4B+J5z30X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,2YexvQ+Elz++5vr338YALjiv//Wgz2A/7vSroEIt6ac+QNzdO09D/d/zT4b73S//eA34vw1/Cw')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,1VeyOvjAV15B9aCDD8b1lZx+fvCXZ3B95+FvAT0wi3f/uvDM52596UJ4//7+RHhf/RoSDG4B5A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,2E+C+vjCV47B9Z6Tz4XqAlDAARUXoIHA9hSIsEAo0ccPPPEX5K8AABUQYAT/vKMf/+znT5DA+A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,1VeyOvjAV15B9aCDD8bvcqjOBO/SpwC+7Pzf8UAsji8fv+KfYx9A6ylYz4bptfstO/zM/w9/Cg')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBobwnp6BNvuQbca05+Fg')]
    procedure TestBitmapAffineTransformEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,4feQ/wHu++/D9OQDEATwSTTf+/vPa5A/5fJL8f0B9FePu/PWo4A++c/j7gD8DHhOeg4L4F0+AQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,3L+w/Qjvyw8B9PYPkAHwEWB/v+/h6zBB6/AzgL4N+k/A/Oz4O/9++fyX/8H+45OPfgUwiE6//w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,7zuSuwjcTx/A9fEn4EED/a+/wAD2n19A7/dLoP8fwP8Of+rc4w/A+ztYTwL70wdv+wcxRC78AQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,4ZuBe/jLt67/9syff0P5LVwPPvfP1w//8fp3QP4bxWPf/OUBfO979jy4v0Xz0nvf+gIDGB++Cg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,7jdCO/rYo26/9syD/8TzRBivPf3M259+7usPAX4gr48fPu4HhM/8+jvgXgXt8vs+PATfpz7/EA')]
    procedure TestBitmapBandedSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,v0tQgATvq9+EAaAT0EL2LND//vrhA1AA/wBkUMEA4+e/PP/Qu2CB9eJfQH/8BHC/+wMRLH4+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,5n8/AQQL9O9AA9m/fwH92UtwP/n3WxDB9gIwwAAC8V/QQAfyC6C/++ebn3oD7qNPwAcgpI6/9Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,4c/OgQEIbPAACd7jrz79w6eQwP/+E0CC+f3zP8D3+uuffwnsZ+B9+dW3D/oGBcRfwQQo/P6/8Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,6l9uw/4UfPDACtrnn378pOuw/wUWBHDD+/tnv3/sBvAvAA//xyC9+cOnv3gFFtAPggQhAM+A7w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapBandedSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,CeBQwADUw+8C++Hbj4T5F6gvBfPdY2F/5viPgTz3E6Qw//vbIxCB/CLsTwECjJevO/QcGD9/8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,DeyQgADc2y8C/O7jr4T9EqhPxfPpY2G/6/Vzkfz3G6gwP/zZF2DB/SL4v0ABk4svvPQYLB//9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,CuQgQADc3x9C/OPTz4QBGaA/RfToO+G/8edLcTz3EqjwvvvbCyCA/CEIwIAAlX/vO/USRL++8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,FsgAgQHnE+CB+wH0jwQCDMjfxff8Z2H/8wA0AT33I7yAv/7b638BABwM4D8DnZ+//PYKYE/A9Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,FsQgwQHgE0CC/AXkTwQDFriPhff5L9F/9fM/kXz2JcDAPv7i50/B/xcAUIABm4NfvPQLQI+/9w')]
    procedure TestBitmapBandsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,4u9QfgPWj29D+8kP0IT4LZAPA/DOY/E+5P27MfsH/ctgv/S33z/B/Cf0HwMAo0ffuvoyoL5+9A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,4vNgfgPWi29D+8oP4IT3MZDvgu/OVxE/5P2zQbsI+79AP/S51x9B/SnoD8P/pUf/OvszlP6+9A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,4u9QfgPXg39D+8kL8IT2NIy/AvDNRzF/5P2fkTsJ/avQ/vO7z++A/SroD8P/pDtP+/ovpD6/9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,4usgvgPXf4+D+8cD4ET2LoyfwvDMQ/E+5P6nwfsIAKiQvvS3y9+A/CX4H8MAojOP+/kn1A4/9A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,4ucwfgPYj2/D+8cL8AT4JZD/gvHPY6G+4/3DcfsHA9QA//Wz3w+B+x4AMIMBnzsPu/kn5I7+8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,4fNgfgPWk29D+8oP0AT5LZQvw+/Qa/H+4/3DATsH/dOQP/W5409B/Sf0DwMAo0+v+vo1lK7+9A')]
    procedure TestBitmapBevelEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress, ASizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,C1wgQv76gx8CAccrQEP99VtwAvnhDwEB5Q5AQX3yAqDg/v75y9A/+wswsD0HudsfvvkmBK+A9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,C5yfhPoSpE8CBNFfsAL+2tPgAf3k2+CB6QrMkD7wE0QQwAEDtJC/+/QDoPsI3SMgP/wtCD8A7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,BfweRP0JEBACCNYrcID/vNvwAAL5i/AC8QY40D7sEizg/wrqw8B++NrXPzsF3a/ff/0kBP//7A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9n/eg/4RYAABC+EDMP/+nhshwQUQTHBD+P/XT//oDhAQwA/zzxC++sCzv3gI+Ks/AQAnAF8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapBlindTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,S3iQQ/rhQ5+AGhaE73v4zh9hu/fZfwBH+QPE3r0A6o+vQQIoxG5+/DNcPn7+JewuvQabM688Fg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,9k8+Qv0GoLDACsQH4Dj4eYfQAQEZHCCC7RSkvr0T4F9wPff5p8C69QzkLkH58GfPuRYXCC4CCg')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,xA8+wwcRnCCCCsg3oDv5fN/QgAMSJEDE8f4Tz374MxwPPvvwJ1H/9MSbHz0Nh/e+/QBRFG8/6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapBloodTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,3gshP/y/b09CAKqrH8b4M/BOBO7WJ0FB49F7AXn8BGhAfPLoE3D6+iskX0H1hLue+fMN6P29AA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,6P9wf/3Fh2+C/re/z4X4MByfxO7aR7FA49uDMXr7BnAwvfXjIzB8+SVsX4H5gvtOevENbD6+/Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,8POwf/7Jm38C/cPTn4X4LkT/xO/eazGA4+SHQXv6CHgQvvjfN6D99yC0X0H9fzPvuu4M7H6++w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,9+fwf//Mr58C/M3jf0X4LWhfxfDjj7H/4+2LIbz5CXzgfvvcQ+C+9hv0X8EAe19/O+wKYK8++g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/eMQAADPv6/C+tXvT0X4KJCfxfHlq0G/4/aPwfz4C4SAf/3aSxCA9hgkQMEDe4vve+sMrN/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,ANhAgADQ068C+tv/T0X4KqjvxfLsy+G+4/yLYX34C4wQwP/aV7BA9RJcQAEGdadvPOgHFOC+9w')]
    procedure TestBitmapBloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECipd/O/QhDA//8Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,wjtxuwjhQ1+D9rlDkITzOGDPfvav60B+4xOUsX0W9YsQPu/Fr/8B/RggIIMB0zvvugRLPG5++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,x/NhuQfB686A7KfbbwXuOVDPPfit8+A94gRoQcAf3ou/Puvki8/B/Cyo3wL97ZOueA0qrO36Aw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.99,0ycyuQLE2/4/7bGbL0byTyCvPfiwv6A95fZzwf8hyTOPvuvnp/+//kF8D0Xz8a/+OAgdvO27CA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.99,1kcyuf/Dy+6/766LL4b0Y/y+ffepn3C95u5vAX8jujcPfur0m1/+/kdsPwbv6c8O+QUQ8F18DA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,2FdyOf3Csy6A8aiDH8b1ddzevfWif2C95+hrUf4kskdvvegBhN+8/U9kX4fs4+ceeQUAIN68EA')]
    procedure TestBitmapBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,EGhQwfzi71+B/uK/P4P58AMghfbnL/F/6PPz0D3vEBwAvwDza/A/+A28nz4Io9tfvvUrTD//8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,C8xfAvvzH5AAAuu//0H812fgw/v1w3AA7/Br8L7qEuwf/wT3h6C/9/ibD3wKyffPAPw3NM//8A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,ADDPwvwGTFBABejPf0D+wcdgwgAFYFDB9fATwL/qDuBvfwr4qwC/99+Hv3kK7OsfggE5CJ8A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,9Ytew/4ViHBACePb7/7+owfxQAUZHKDC+vW3D4DpDPAvgA/3w1D++MaXz/cJCOCfQgQ0+B5B7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapBlurTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvzQPwDSq58C+9PXz4T3F6wfxfDVfzH/4viXUbz4DJAwP/vWM9BA+yfsj0ECipePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AvzAPwDVq5/C+tTbv4T3F6z/hPDUeyH/4vmPUfz4DZAgP/vXM9CA+yjof0ECjJeve/QiEP8+8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfywPwDWq5/C+tTfv4T3F6zvhPDTdxH/4vqLUfz4DZAgP/vYL9CA+yjof0ECjpfPu/QjEO8+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AACB///Yq59C+tXjr0T3GKy/hPDRa/H+4vuDYTz5DowQ//rZL+AA/Cnkb0ECkZf/e/UlEN9+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/AcxPwDfn39C+dbvn4T2GLA/hPDOU7H+4v1vkTz5DoAAv/rcKwAB/SjgP8EBmZd/PPcpEK/+8g')]
    procedure TestBitmapBoxBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,1ley+vfBW15B9aCH/wX1kbQ+fvCVX3C9599/Ub0jr48v/OYWVC+6+GJYPwnr3wdfeQXWh/49GA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,22+R/PnU104B+q2bT8T2WTy/vvWzT8A+7OQP4X0XwqcPPfARpO+6+D5sbwXx6jNfOwXoo57+Dg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,3YuAvvvmUy/B/rqvn8L4I8APv/rSOxCA8OqfYL4K1ccfvvkM+H87+RuArwH39Vc//QT6u07/BQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,4qtvQP34zx+BA8bH74D67UNgv//wJ2AB9e8v4D7+598P/wIGRBC8+feT/z39AYAfvwQLzO4//Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,5sdOAv8LSPBACNPbL7/8tsfAvwQOELDC+fS/fz/y+fPv/wsAlLC8+dKnL/oCDKgPQQQc5I4A9A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapBrightTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,A/TAvwDVu48C+9XT/8T3F6wfxfDUg0H/4viXcTz5CmwA//vVP7AA/CvwTwECiI+Pu/QkBO9+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,+LcQfwjmN0CB8uP/vwTx5/t/BPXly5H/3ABkIf35DTjwPv/VP0AA/SQw8H4CiFuvu/cp5F7+7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,6Bs+RAIfzOAAB+YfEH7+gEshgAks/P9D/vpTX7/sChSAPxDxu+B9+sCrr3YKD7w/QgQrBA8B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,7WPegwMczLBAA/Qv8H7+gEshgAks/P9D/vpTL//uDPy/Pw3pp1D++ti/33cJ/5t/QQMsFG/A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapCircleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,APwQwP/Rp4/C+tXPLwX3mqiO//KzI3A98NQDogAUwV+/fe+z8+C/7flrgAbvettfPPf/25/B+w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,8guh//3Gm5+C+9LfnwX6hOyOP/S/W4A+6+a/8Tv+Bmywv/fI729A/D6Uz0H/hk//e/oxAH//8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,49+R/f+hI/8A/dOjP0T2GJxfQ+24p/H+2wUYkTsGCmhAgPXi776C/2PU7oP977eu+AL8m137Dg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,5tfhfQGgMw+B/NWfL4T3F6wPhfDUg0H/4viXofsCE4hQwPbm4y5DAGbU7wL8wZPvO/Qv2D4ABw')]
    procedure TestBitmapColorKeyAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,0R+xvvq5Uy8CApiTf8b4NMjuQ+3TB8GB4sdvgXf9AmQgu+7x94/4/TLQXoHwiHfeePcQSJ19Aw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1B+hvvq7Ux+CAZqPP4b4M9DuQ+3S/6DB4shrkbf9AWgg++7v8+94/jHcjkHxhofOePgTSL19AQ')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,7xtBf/3Ia1+C/renT8X3Hyjvw+zD//BA49mXwfn7B2zQfPLZGyA9/TcgzwH4lAfP+foluJ1+9g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,BPwAgADTq68C+9bbz8T3FLQPRfDSh0H/4vmjkTz4DpBwf/vTO0DB+inkbwEDjp+vO/QiEA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,AeRAgAHS28/C+dwLUAX4HvRvxvTyDwK+4g2I4X32DaDggALfUyDC9xWg8MAIfNsPvekMUJD+9Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestBitmapContrastEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,h+vg/v/wE7B+3wWAEED0Svx+O/TRk2DA4d7vsH0lrWuP/OQVUN+5+GJYTwnr3wdfOQXXgw5+GA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,3VPi/fqwr57A+HeHD0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,B+DxQPqmb97A+LQPIET1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXQQEV4ce/vfLiDxGB8ONvEMQGqHt/+flg2L7A9Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,pIvufBHtU+A//7rvfz/t+I9/+f2uX9FB4NLTwPwhsVcvvuERhM86AFBcH0jv5jef+AXsN559GA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,HXCxPPWY405DDobT70H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestBitmapCropEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,4N+wPQHho+9/Bd6/IIAAKRDQfPH6Q+A+9wHwIP32I8B/vwL4BwDAAiM8Tz0F87s/Pgs+eA5B9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,5kOP//8JaOA/Dc3f3/8D0ItQffvvx+9C+cx////87VMwgQbGd+BABNDzb//+7/Ov/QAdUI++9w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,uAMvwQEMAEBABt5nMD/4ty9APwch/I9C+w2w377wLPBvQQUBZCA//9efvzoI/w8QgQAuIE8B8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,BYjuwf8liJC/Bfwr0Pz6sE8wPwIm3L+C+Od3D33pE8jP/g0A2CB9/8UDkLkJDwSgAQQsPF+B6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,6o9gPgHv858C/cdPQAP5DMCfxe7Kf/H95fBb0bsAEQBBAP6wG5DC/jZoj4EGe3evfPgvJD9+9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,6z+RvQbbY19D+cYDcEP1Iahvg+3Ax9H+4vyLYT32FzTA/fnCV4AC/hv0378HieNP/vEdmA9/+A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,DeRRfwHgq37D8N/fP4XwP2SfP/G446C/3xFAIf7tHwygvPcRcDA/+yZw0H7/3K8vvvcOAI++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,HvwgAfrak5/B9O0XEAbzEIDPwvPLu3D/3BFAMbzvCwQgv/4bcIC//x4QgAH6vjs/fPlGsD5+8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,4D/Qfvv923AB/eDHD8X26OsfCPTmA8G85PdHobsMD7TwP/LIe18B/0C8f0T/mq/fOQM3xF6+7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,39OgPAYRjB+B+cmLcEP7JQAgg+vR+3B96+lvYT0H7yOBAAGtv09C/j303j8EggOvPgFFOJ898w')]
    procedure TestBitmapDirectionalBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,CdDQvwHDu/+C+dK3L0T6DrjPBPLUR2F/5vl/gfz5Cpww//rSE5BA+yLsr8ECjJuP+/QiDO++8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+4cAQ/rzi6+CANTjv0P8BZBfQvjhn7B97wAk4X36HZyQ/vnpF+C/+hIkID8Krauvfv4agJ//8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,9rOORAkSXNBBAecPcAH/oqegwgASYNDB9e0voP7g+5cQwBDtx5B8+bif37kFFGyfgf42xC4B5g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,7OcdhAAfwNAADeDvb33+gEshgAks/P9D/vlTDwDnBxSwwBX850A9+bGvb3YKE9R/wgUt9C7B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapDissolveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,9t+dQ/8llGBADe67j/z+eCvhvwcXqP8EAPdvPwDrHfS/AQ76d5D//rhfDzgKAvwPhAI15B7B5A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6vtNBAQTkFDBCub337z+e08hQAsu5P9DAP5Lj4DqDhQQAhL411C++rCTj7cIAtRfAQA4IH9B6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7PdtBAIXpCBBC+b3Xz3/gT8hgAku+O9D/vdLTwDoDgxQQRL75+B9+a6jD3cMDtxPggEuDD8B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6udNhAAcvOCADOH7r/3+gUchwAks/P9D/vlLD0DmDQTQwBL948D9+ayzr3YKFcyPQgMsCD/B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapDropTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,1jMOAgAAgFA/ENYDALz1jIMAAAAg1A+E9QAAbz3jIAAAAAj1A2E9AMBbzzgIAAAAQv1AWA8A8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1zcOwv//f1D/D9cDEHz1jYMAAAAg1P+D9QEEX33jHwAQwAf3/1A9AMFb37gHAgDwgf0/UA8A8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,2UMuAv/+d1A/D9sDMHz1kYPwv/8hzP/D9QMMb/3jHfxPwAf780B9AMJfD/kGBvzvQf4+SB/A7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,3VdOAv78a0B/DuADUDz1loPwf/8iwO9D9gYUb73kGvyPgAcA5CD9AMRnT/kFC/y/wf49OB+A7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,4WduAv37XzC/DeX/f/z0nIPg//4ktO/D9gkcb73lFvjfQAcH2AB9Acdvr3kEEvifwf88KB8A7w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,6Ht+gvv6TwA/DOr3n3z0pX/Qf/4moN+D9w8kr/3nEfRfAQcPxMB8Acp/TzoCHABQgQA6FB/A7g')]
    procedure TestBitmapEmbossEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/V/AAADh4z+C/tbbXwP5+QMQxPXmMzFA6PgjET31C3SAPwDdVyDA+g7gT/8DpqP/fPckCH+/8A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+L+fQQDwG+ABAtjf70H621MQg/r45yDB7fmvwH3xC1TQfwXke3C/+vbXHz0Fwq9/vvomBN9/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8yN/ggD/U4DBBdvnb8D7vKcggv8JmBBC8/k7gP7tCzggQArsn8A++t7P73oG37v///0pBE/A7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,74NOgwAOjDBBCd3r/z79nvcggQQaSADD+PnHTz/qChxwQA/zwxD++cXDr7gH/Md/AQErAL+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapFadeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,jku/QQK+kz5//LXPQIHj0m+QgO+kzx9/7TNQ4Lj0GyTgO+nzx1/7DBQ4Lv0GCfhO+vzx1z4DBQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,rft+QgPYLy8//9e/0H/rvp/QAPbLy8//9S/037rvJzSA/fLy83/9C/237vsJDWC/vPz8X/9C/w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,zacugwTyzw8/AvmvYH7zqcsggfzzw49A/iuY33zqMkgg//zwI5D/CuY3n7oMEsg/P/wI5L+C+Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,7FfuwwUMaOD+BBuc8Dz7lftwAQMauD/BBic8z37lPlzAgAbuT7DBCc+zX7kPFzCggfsTbHDC8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,CwSexAYmCNH+Bz2QgPsCgSexgQlCtP9BDyTgvkDgSWxgghDtf9ADCbgvEHgSG5ggRPsf9EAC7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,KrBdBQhAqLG+Cl+AELoKbFcBAhBqrK/CFyCErgLbVYAAhBrrq/AFCKGrwHYVIAChxvoqfAFC6A')]
    procedure TestBitmapFillEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,yh8hPP3FFy/B88IfUIX2PlS/QPCgCzF/4vrfUbgLBZxAPeb9Nw96BS6wrgP9wpcuOPXDn06/Bw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,1Q/x/P3HP2/B9McbUAX3OHjvwfGzS9E+4v/HcXkHBqQAPuz5Z587AyYcH4P/sdcuOfLQAz+/BA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,4APB/f7Ka8/B9c0XUEX3Mpw/A/PGi3H+4QSsgboCCKzQfvL1mx+9AB+If0ICoBcvOu/eZx+/AQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,6/egvv/Mkw/C9tMTUIX3LcRvhPTayxH+4QqYoXv+CbCQf/jxz68+/hj07wEFkFsvO+zsz/9+/g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,9udwfwDOv28C+NkPUAX4J+i/BfbtC8L94Q+Awfz5C7hQgP7tAzCA+xBgUMEHf5svfOn5N9B++w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestBitmapFillRGBEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECipd/O/QhDA//8Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AQBh//7gr48C+dvzz0T2F6QvRPHPN8E+4gBgcTz5D3zwfvrmJ7BA/Srob8EAoY9v/PcrAK9+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,6hvBvQL6m6+B8dMnUETwCqi/AfLEzyA+4gIs4f3/DvyffvjoJ5BAABfcf0ABzkO//QM4vH499g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,2yMBvQTzk1/B8NAb8APvC5DvgPLMx3C+4/0rgf4DCOCPvvbqE2DA/hPUXwAC3C9fvQY0nG49+A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,zzcRPAPoh48A8dILoIPvHmivf/XOp3C+5/4DMX8J/LtvPvXx96///hfELwEA4EOPPAcohO69/Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,1z9xfADbW09A9c7Xr8PyNkgvv/XJf5B+6fPbUL4O4asf/vP518+9/R18rwL53VMPPAYQiE6+BQ')]
    procedure TestBitmapGaussianBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,AdhQAAHR479C+d4HUEX4JPCvBvb7J9J94g100T32DLDgQAPkP4DB9wuw0EAJcM//vOYHeMA++A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,ANhAwADR17/C+dz/T0X4JtBPhvTz+1H+4gWAkf32C6BwgAHgRyAB9w+A8IAHdLuvPOgJONA++A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,/9tAgADQz79C+tr7T0X4Kaz/BfPt18F+4/2LUf33C5QQwP/cS6AA9hJYEMEFd6dv/OgJBOB++A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,/98wQADQw6+C+tjzP0X4K5C/xfHnszH/4/aTEb34C4iwP/7ZTzBA9RYwIAEEeZMf/OkKzO+++A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/+MgAADQt68C+9brL0X4K3hfhfDgj5F/5PCb0Xz5CnxQf/zWT/B/9RsEQEECfofPO+wOgP/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,/d8wwP/Ps6+C+9XnP4X4MVQvhe/ecwHA5OmfoTz6CnAAP/vUU0D/8xvoXwEBfmuPO+sKbO+++Q')]
    procedure TestBitmapGloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,7d+wPgTZo38D+8n/r0T3JaCPg/DRZxF/4/2nsXsDBbgAP/e6/0/B+yIQgML/pW8vO/kqyA5/8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,sF/xuQvaO6/C9K4vcMTyOVivvfmo70B+4hCIgf4Z+Zfw/e7Lp58C/A8UAIME2TeP+gVWCA7++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,zAcSOQSst35/5pObn4XrOVTfffuiD/H94fVjUcAj1ndfP+rTfw/D/SuczwH8+Fs+dw0naF25AA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,4xMiufzJrw4/662D30b0SzBffvqyv2A+5OKHcYAkxPPe/+vWs++AAztYz0Ly/4e+tws3hN36Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,3EcC+frH3/4+77pzD8f4VhDfvfixqwC+5d+Tcb8jtgOvfuvpo7++BT9Ir0Tt9acOOQczjE28Bg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,2GMC+fnE134/87Fvr8b6a+jOffikl3C95t1/kb4jqit//egAjF+9A0BQj8bp6uu+uQMayM08Cg')]
    procedure TestBitmapGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,BuDAf/3Rsy9C+dmrH4T2PcAfx/YJDBI+4wRMMf72BJhw/wbxV5A/9vurIEH9U6Nvu+wgxA9+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,COSw//3Rpz/C+devDwT1N8wfh/T/z8H+6Ae8sT38GeTgfgXsL8BA9RdY8ED7at+fOvEfkK++8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,COywP/7Rnz8C+tarDwT2HuAvBvPot9H+5Qe0Af35Gdgwv//hFzDB+x4U0AD/fLdv+vMjKK/+8A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+gshQALSu++C+9EHoIX5F6QPhfHXq9E+4PZnUTz2+2PQ//rWO9BA+iM44EEHjKMP/e4YlL9+9Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,8euQgALOuz9D/dAbQEb7K3BPhfLo29H+3u47Ab3y9UOAgPrdM0C/9gyMEIEIgGd/feMAHKB9Ag')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,9sNwgP/Nx7+C+9j7nwX7MmhvBfPs6wH/3u9HIX3zAUgwgPvcUwC+9vqDAMEHaTsffN3wY5B9AA')]
    procedure TestBitmapHueAdjustEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,Avjg///Sp58C+9LTz4T3F6wfxfDVf0H/4viTUbz4DJAwf/vVM8AA+yXsj4ECiZOP+/MiDP8+8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,t0MROwngM58D+bA/gAT2Q1Sf/vax75A+5AqooT0Z9LMwfvC7t4+B+xgo0EMA0j+vOgNNPM4++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,vQMSuQSroy4A6YjDr8XqRyhf/fuQB+F94fpfAT8j3qPf/ubha1/C+ydc3wL/938edwoglF05/w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,2hNSOf3Ci15/6qB/34byWBA/vvucwyA+4+J78f8mw/e+f+nhoy8AAEAoj4LyCnQe9woegK36Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,3SMSOfrRq26/761vL0f5Z/jeffmlnzC+5dqHET8lrOuevuvvo599BUZAH4TtBaieOAghiK38Bw')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,10cyOfnLvw6A87Brr4b7d+iePfekh6D959qDYf4joh8vfeoFhP87BEx4r0bq79su+gQMuI09Cw')]
    procedure TestBitmapInnerGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,0Psg//u4j09CAKPL/4b5PijPBfT/z/F/4eNHEfn5A5QQffgE9G/5+xS0j8D4bLt+uun11069CQ')]
    procedure TestBitmapInvertEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,9FcehP0LYFDBD9fLD37+q/MgQQILfBBE9vEH0D3vFyjgvwTtn4C++u/z//0EpocPvfko/C7/7g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,6e9NhAAbtPDADODvb/3/iGPhvwglGLAE//dzz/7pGPhfAAv403A9+s/rP7sGxo+//v0s+J4/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT73/mDvjPgBP952C9+a/Pr3cJ+58/wQIuCH/A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapLineTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,EnyAPg32b/BA7uMPgEf5HohvxvLPQ9G+5PtDYb/1MMifAPmo199D+0j8n//+aVOPvPZJLL+98Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A6zQvh/h039F6MnH0EPq3KsQgvbDUzJ+2xMksr/0U+hAQPk0mK/+A0hMMH8Gu0OOOPUs6N5+7g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vdtwfOgNGOB9Ab/Hv3/0ILweuv++C0+F6u3zzf0RsCtggf6ab0CDDtx7bj754Es//gfhm64AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgUHr4Sdje++FXzP/z9i7038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestBitmapMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,H4AAv/UbrGB97wtgb0X2E9Sfx/a1zxA84gLUsADsNNg+fv3RN6DB+D3wbnoMin8vgQNDvK97+Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6udgug7x1/A67P2fkAPeoD+QffPo/9B/2yiEcQMGFZCPggcCdECEARwIT7gH9pdvQApfbG4B7A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,uMuvPhbw3wC/+BdosMH3ejcRAf0klMDE7eGnn34EFCC/gPzjB9DB/0CYP7gL9Odefw0uTG2C7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,4hsfAgXkhxCA9ypAMED+gEshgAks/P9D/vpTP0AR1udvPwLZxyDB/Sh4IPsK6Z9/v/o1VJ8A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0xPfhAsPjMA/9gSw8H/+gEshgAks/P9D/vpT74IeuEOvP/2M/2/D8g7ojzgPAJC/PwtDzI6B5Q')]
    procedure TestBitmapMagnifyTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestBitmapMaskToAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestBitmapMonochromeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,9m8PQgD4N7ABBNnjLwH7y3+QAv0AvKCB8PlzIL7vC0jw/wfoixB/+urT/7sF0bM/P/wnBB+A7g')]
    procedure TestBitmapNormalBlendEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxCB9QpggMAGdq/PPOoMCBB/+g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,/uMwQADPz69C+trzH8X3MtxfBvT404F+5AeAwf31BpiwwP/bUzDB9QtwkEAHdK+/POoKHAC/+g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/uMwQADPz69C+trzH8X3MdRfxvP405F+5Ad8sT32BpSgwP/cUzAB9gt0kEAHdK+vPOoKHAC/+g')]
    procedure TestBitmapPaperSketchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,ANxAAAHQ569C+d0HUAX4IQzwRvcARHK94RNowX31DcAQQQTqN6DB+QrIwAAKb9MPveYIkLD+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,bUSARQke9JAA/HtE0IQi8iegRQ5NgGDBIe238P70R2CBQBDbd+9GCvbncL8Jue8PwPLer6BB8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,W2CgBAXzdxDCAlrYnwUh/f9vhBIeWMDCGNkb0T32JHhRfw/f72+FBPO7UAEJmae/Pezug0BA7Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,ToTQQwPbX7DCBFOw70QgAxSwRBQSXJCCFdg3kb31G2iRfw3kD/DEAvW3kEEJjKs//en4iwAA7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,ROCAAgLdd9CBBEFw74QbAxwQhhYQZNCBE+EnAb31HTCxPwvuF7AEAvq3oMEKhbc/fevvbzCA8A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR569C+d8LUEX4IwSQhvj8M4K94hJo0Xz0FLCwAAT1M4AB/RGogIEKbZv/fOb3j+D+9g')]
    procedure TestBitmapPencilStrokeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,va9hOQvgj+8/6q0X4MbmLHBfe/ez/4A/4/sTsb4Yxhuv/er1iy/5BjYEz0bsAgSPuQHtB/48Eg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,9aMS//2Pz70D8bKfP0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,90PAf/v9vwBC9+N7H4f03usvyfT65+D74RFoYQIaCgzffd/2/w8/BwCIkMPxwdPv+/ZxFE8A/Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,yJ+R/wi9n36A7tJ70APzfcQ+PfO+Q0D+5N9DQX0kr3/PPOQXRJ959mJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,yU8BvP7sOw/B6/EvoIDyPPR++/bIw2BA38HjYP0krnc/fOUUZM+5+GBYbwnr4ANfeQXWgw4+GA')]
    procedure TestBitmapPerspectiveTransformEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,B4gA/wbho89B9MQb0ET3GaC/xfDSZ/H+4vp74bv1Fmzwu/j3/7/7+zzsv4H9f2eP/PUWfP9+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A/zQfwDSq59C+9LXv8T3F6wvhfDVfzH/4viTQfz4DJAgP/vWM9BA+ybsj0ECiZePO/QhEA8/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,3duxO/zSXw4B+4tvv8LueOzuPfCSt2A+5OV3YbkI1YP/POtCAFA4/15MX4nm2f+e+gDWez4+Fw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,AvzgPwDSp48C+9PTz8T3JrBfhPbPW5E+4firUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,2WeiuvbHWz6B9KOP/8X7i9BOve2xG8B+7vCb0T0lpoevfOQRXD86+FRYX8nq4ANveQXWhw5+GA')]
    procedure TestBitmapPinchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,1CcSu/jCa05B9qOPrwX1icQuvvCaU5D96ONrQX0isYsvvOYUZD/6+FpQn0js4guvOQXZjz6+Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+s/gv/vpf48C+9EXEET2Ebj/hO7JawF/3+9zsXz1GbDgP/roR4AA+ioMsMAGjHOfPPko9E4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,BgSxP//Wn09C+9bP74T3FrAPxfDUg0H/4viXQfz4C5QQP/zUN+AA+yjsX4ECjZOve/QhAO/+8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfzQvwDTq49C+9PTz4T3F6wPxfDUgzE/4/ibYXz4DYQgv/vaN9BA+yXoX4ECiZufe/MhEA+/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AgDhfwDSo5/C+tTX78T3F6wPxfDUfzE/4/iXQbz4C5Qg//rVN9BA+ynsj0ECipOfO/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    procedure TestBitmapPixelateEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp4/C+tPT34T3F6wvxfDVgzE/4/iXUbz4DJQwP/vWM9AA+yfsj0ECiZeP+/MhEA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,AvzQfwDSq48C+9TT34T3F6wfxfDVgzH/4viXUbz4C5Awf/vVM9BA+ybsf4ECiZePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,AOjQfwHTvx+D+9Lnv4T3GazfxPDXdyG/4/uPUXz5CoAwP/rQJ/AA+ybcT0EBkZv/O/QgAP8+8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,699thAAawBBBDeP7jz3+f1MhgAkuBACE/vxP3z/mCgTQABX761D9+aq/P3YIGNAPwwMw/B7B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,6ucthAAdxODADODzf33+gEshgAks/A9E/vpT/7/mCgDAgBT652B9+a23f/YIGND/AgQu/C4B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODvf33+gEshgAks/A9E/vlT/7/mCgDAgBT752B9+a23f/YIGND/QgQu/C5B6w')]
    procedure TestBitmapPixelateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp58C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+QNRvwLUlx+D+tDj3wT3I6D/g/DRbzH/4vyfEXz7CahAP/rNJ/AA/Cb8z4EAmH9v+/Ui7B4/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,7u+gfgPZm08D+8v/r0T3JaB/w/DPYxG/4/2rwTsACNiQf/nBDxAB/SoAUAIAoGsfe/gpyB6/8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,4s/w/QLkp18D/McrYAT4IKjPA/HQY+F+5P2zwTsGBwzxf/i581+B/TEEAIP/oWePOvszoC5/8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,2LsgvQLyt/+C/MNb4MP4Ery/hPHTa6F+5fy7EbwLBCQRwPa60+/B/ToM0MP+oG8f+v06gF6/8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,18NgvAL/w+/B+8B/gMP4/tvvhfHXZ0F+5vyzobwP+Q+hv/TCr3/C/kUcMMT+o4P/OQE/cG4/9Q')]
    procedure TestBitmapRadialBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    procedure TestBitmapRasterEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,x79wv/fvb3/+8d6jHwPwLOSvAPKt7xD67biLcL0krm/v/OQXPG8592RgT8nq3gdveQXVfy6+GA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,x8dwf/fuZ3/+8d+fDwPwLeSfAPKs7xD67beHcP0krW/P/OQXPH8592NcT8nq3wdveQXVfy5+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,xruQ//fva4/+8d+nLwPwLeifAPKr7yD67beHQP0hsnsv/eUTPG/59mRg78jp4CMfOgPcj17+FQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,xregv/fwY4++8d+rL8PvLuh/wPGp7zC67beD4P0dwYNf/eYIXO+59lqQH0jq4R/PugDxo74+EQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,xrewv/fwW4++8eCrH8PvL+xvgPGn70C67bZ7cH4b1HevveYHiJ/69U7If0fs4Sc/e/8IyA4/DA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,xrPA//fwX5++8eCvL8PvL+xvgPGn70C67bZ/oP4X5XvfveYBqI/79EDYH0bv3jP/+/8i9I7/Bw')]
    procedure TestBitmapReflectionEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALengthProgress, AOpacity, AOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,9DvhvAXrry9A6tHjv0XwF6hPwfW8FwH/4/tzAf8EDUhwvvzhd7/A+jaoP77/uePO/fsD6H69+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,Gpxx/Ang8w6/7bjf3wPvNUSPvvq6p9D+3u1fQb0B6Hdg++4SlE+++Sukb8MG4zef+wIcyC49/w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,8lsxvf/pi3/A7sobgEXzLpRPf/Spj3B/4PnrQL0CB1ggffbln29/BDh0j8P4x7tfO/j7c299CA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,6Z+B/AfsD+//8c4X0EPyRkAvgvG2gyA/5u/DAD4BBoxAPvjed39AAi7AX0L7pJevffcvkF+9AA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,9EdxvAX0R0/B7tMH0ITzSjRf/fa5e0B/7PQTAbwADMgAPffktx8+BECkTwP20Pce/P8QWO59Ag')]
    procedure TestBitmapRippleEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+W8wgP7mw8+B/tPPjwP6/+O/A/fgF1GA6fUvIX3yEYyQvwHdZzAA+xPor38CrI8/PfchEH//7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,8KfvAf7nNxCCAdcHEML31VugQ/r/5+DA7QaccL3vE0AgQAXgL4D/+f2/P70Dxmu/Pvsj1H4/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,AiCPggQEdMBBBuXf/z/9usvQQf8HnJDB8fM3cD7xBTDAPwj2u2B+/NrLX/oI3QMAwP0xKG/A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7Jfugv8UaOBACd7jD7/9o+OQwAUTMCCD+fXLf7/oDCxQQBDy2yB++cbL37gGAcifQQArHK/A6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapRippleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,AcygAPvsW8/A/t2r38L03OO/B/Ptr9A96f/HAD31CCCgvfzzwwA//QHM3/7+rdNvffcn0K6/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/dufgf37V3AB/uTrT0P32vffwvT6d5AC6f1XoL7xDAhQQAYF0LBA/QlQH30Gutdf//4zBM8+7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,8Wt/AAUPQBDBBOL7f/7+0E8AQAAKfPAA9PYrEL7nGthvfgMOqAA++8+rb3oL5Kuvwf4wQJ9A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6NeOQ/4CTCCBCeTTfz/+qcNAwAIkKBAD+fSrb//oCAjAQA75xwB+98DnX3gJAPB/gQAq8E6A7Q')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapRotateCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,COxggALXy99C+tn3/wT4FdjPRfLh09F+4gKckT31DYRQwADWc/CB9htc8IAGkM+PvOwV1P/+8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,COBggALY28+C+dsD8IT3GPyPhvTwB0I+4gqEAX7zEZTQgAThg2AC9hHA0AAJhvPP/OgKVGD+8w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,CNhggALW679C+d4L4MT3GhgQx/b+P8K94RFsAb7yErQQgQbtY0BC9wf4gAAKegPgPOYFlHC+9Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,CNRgQALW768C+d8L4MT3GyBAx/cBTJL94BNgEf7xErgwgQbuXyCC9wT8cEAKdwPQ/OQErIA+9g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapSaturateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AADRv//Ro5+C+87P74T3GpwfxfDWe1H/4vWLQfz4CoQAP/vYN2AA+ybon0EBh4tvO/Qh/O4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/AfBf/7Pl58C/cS/D8X3IIAfhfDZc3G/4u93MXz5BnCQvvrbO2C/+iXkvwH/gm9PO/Qh2K4+9Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+Qux//3Ok5+C/b+3H8X3JHAPhfDaa4F/4uxvIfz5BGRQvvrdP/B++iTkz8H9gF8/O/QgxI4+9g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,9ROh//zNh58C/7WnT8X3KlQPhfDdY7F/4uVbEXz6AFDwffrhQwD++SPg74H7e0cfe/QgpE4++A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,7B9xv/rJc5+CAaKHn8X3Nhj/hPDiSwGA4dkv4Xv7+ScQvfnpTyA8+SDYLwL3cQ/PevQfXL59/A')]
    procedure TestBitmapSepiaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.4,0.99,9K8wfwHd88/C/Mf7PwT4CbyfBfHWkzG/4vWHoXv9DtBg//m8E1AB/Sbgv4EChI9fe/Yt7L7+8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.5,0.99,zp/gvAX3z5/D/L1zQIP3C7x/xO/Nm+G+4/eTwfsLCiBhP/ijz48C/SfYr0ICiFcfe/w3sL7+8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.6,0.99,u1dBugrmO8+B868z0APyLIzPf/OyS9G+4v6jAX4V/vfwvfK4m9+C+RUkMAMDuhv/OgRKOO7+9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.8,0.99,tssx+QutJ69A7Z7jn0TsMWx/vPud/5A+4gF4wX4X/KPQfO/Is58C+wDcvwAI8bfuuw1VwM388w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.9,0.99,s9MheQ2sL2/A65rnr0TrHnT/vPunD8G+4PdzMb8Y+zfQfO/ky1/C/AOob4AD7ju+uRNLOD269Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,1.0,0.99,u+eh+AqxC5//6ZPjD8XtIniv/fq1/wA/4O53EX8b79PfPe/rt+8BAB6Mv0D78xsOdxM6DM25/g')]
    procedure TestBitmapShadowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/IOAvgvtj1AA6dcXwIbmwEOQg/rxE8I/1SCUsf38B3wPPwPJb4BA/DlAED4AfHffuvkv0L798A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8Evfww0c3IBB5QPAf0T7sUORgvwp1HB/+O5b8P72K+jf/gDgZ6A/ANyfn/cL08cOPgREfA7A8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapShapeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvjQPwDTq48C+9HTz4T3FqwfxfDVgzH/4viXYfz4DJAwP/vVM9AA+yfsn4ECiZeP+/MhFP8+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,APjAPwDSp4+C+8/P74T3FqQfxfDVgzG/4vibYTz5C5QQf/vUN7CA+ibkn4ECiJN/O/QfFP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,APTAPwDSp4+C+87L/4T3FqQfBfHVgzF/4vebcXz5C5QQf/vTN6BA+iXkr4ECiJN/O/QeGP/+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,//ew///Sp4/C+8vL/4T3FqAvBfHWhzF/4vefcbz5CpgAf/vSN4AA+iTgr8ECh49/O/QdHO8+8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/vOg///Sp38C/MjDH4X3FpgvRfHWhzE/4vefcbz5C5jwfvvSN2DA+SPcvwEDh4t/e/QcJO9+8w')]
    procedure TestBitmapSharpenEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,0VPA/gAgOKB+6KbbAAbr0SOfgwDoe2++5ipIjz3oEVyQxBMMfKCADiXMH/z87/twxPQeDPC//g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,kMe+/AHIk5AA+cuzzwDxEGDv+wMWiN9ADQ/I73zh8fOvQgWzn3C+Ec6/n3z+OHCABfjqM/FCAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,tWMvewDjX6A/7OfLwH7z1kvQwgAp1J9+BDKoXzvhMKQQgAQMPE889fivP3kHI0yQRAAtuHCB6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,0TvvvgsOGA+D97uHfzzqn6sQwvwiZFDC9RNkj33v2ScvgSTLj7BDA9nPn3cO+IdQhfhNsCAB7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,17dfAf4eEKG+7GeEIID6mItQvvkbhG8E9uZbn/7pBqCgABXeT0CEB8V/H/gHDyDgwgI3AGDA5w')]
    procedure TestBitmapSlideTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,IISQPgn8U3AA7+7z38f4HohPBvLNQwF/5PxDIb/yMcwfQPqq1+/D/Ebwr//+bFOvvPdEOL998g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,ArSQ/h7q3x/F6dDboEPr4q9ggvbCR+I93BQgsr/1TPAgwPsnjJ8+BEgoYP4Gt2tuefUr3K4+7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vRehPOkRCHC+/sHPz0D0ILweuv++C0+F6u3zHX0RtUtwwf6aQ+CCEOF3bj754Etf/gfgm54AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgYHr4SuD+u+JWzP+z9y3038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestBitmapSmoothMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,BQARwP/Tm48C/NPPz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,AmSPAAS+sw/CG337zr/3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,DLTehvTWt99EFqAzf7z3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/RMxwP/Qk9+C/drz34T4xNdBwQwX/C//FR4AULz4DJAwP/vWM9AA+yfsb8ECiZ9/O/QjBA8/8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXwXz5AMhgP/3cV1DA+hxYr/sLGmyAAwMhmG8D7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXYT3uzRcxwBbbK5BB+9V3X/oB+hfgAfgr9P4C8g')]
    procedure TestBitmapSwipeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,DdRBwfq8u34D8eQPIEb8DUQvhvPAB+A+4g0wkDz6CLyg/vvap8CA+igksML+ksOwOf91gA5/9A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,9e8xP/yux54D9dADIEb9JzDfBu/Q34+95PebQH38CchwPvzV4yAC+xbIb8IAkq8AuQNqcP4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,0EfSPf+pi+4D88njf8X+QgRPhPDa7w//5ujHYIAR92cQPvraW1DD/t7vvsIGmf/POwhb3O797A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,xzeSuwSz096C+MnLL8P8PDivRfL129A+4/dbsUAZCRxgffLdey/C/s+f/sEIhJsPPQdZ9H6+7g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,ujdC+Aat+67B8sDL74H6MkCPxfDuF/F+4vhn4f4i7SdAP/X2Oy8C/f2LTr8FvV8vAQ5Q6E5/6Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,zFsSOAmWX64+5ZZLX4T4LWRPhe7WX9F+4/ePkfsg24/QvPkZvH6A+CRcznwKsiv/Qg8u5B4B7Q')]
    procedure TestBitmapSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,/ZtwwP/Wk78C/dLvrwP8E9D/A/f712AA6PsD0f38AMDgvgLQq7BB+wlsH8AIl7t/vQFCAC//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,1oOwgP7VY/9C/tPvX8L/++vvAfryB0CB8eQv0MAD9su/PwDXiwCB+shnf/0F1PP/vQdP3C5A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,yJPvQP4E8P/B/trbf4D/zy+gQP3/828B9+nfL8D9BBzgfgjxZ1B/+8Krj/kNDdCPfw8oVB5B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,4Y8eA/8NeNBAB9zzDz8CmhPxvwQdBDCD/fyDbwDwBhBQwA3ym9B++L+v3zcLE5QvwQMpyF5A7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,1TvS+vnIgz5B9adzj0X1jrR+PvKgT1D96dxjUb0jrZ+PPOgTVH8691hU70jr5yMP+gTVn849Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,7XsQfvnhf89/Bsrrb4H7I5yv/vfdAyDB8vyTsP4H5J8vfvb6b3B8/hKwT0L67Id/vf8SIL9/CA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,23fwPfzeg9/AAsjDT8H2IIw/f/ffP1AA8vF7AH4G5MNvvvgPMEA8/heAz8H585svfgQGEI9/CA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,319wfvzie0/AAMvT78D3GaAvv/jeFyCA8vQ/AL4F6MNPfvcEEKB8/Q58j0H68ZPvfQECJF//Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,27uwvfvbU58AAMLPr8H2MHQP//bUL9C/8PN/gP0L2rePPfUH/D88/BlkL8P28GdPfQL8Dx+/Bw')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,5D+g/v3li78AAcvXz0D5D6yPf/nhLzDA8vY7cD4D7N+f/vgMGJB8/g6c30D7+Kc/fgMFJJ8/BA')]
    procedure TestBitmapTilerEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+BNx//7No3/C+8PHX0X3H3zvxO/SV7F/4+2T4Xr7AIgA/vjbGyA/+yq8/4D+jVe/evUitN5/9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,ABCB//7Pk1/C+8m3r0T2HIjPhO7NO+E/5PCngTv6CngQfvnVS8C/+y3ArwH/iGtfOvYjwL4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AQjBv//Ro49C+83L74T2GZT/hO/QU4F/4/OX0Xv5CYSw/vnUO2AA+yzUj4EAjW8P+/Ug4C7/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/gOh//7Pn29C+86/r8T3GJz/BPDSc5G/4/KT4Tv6DYig/vrYP9C/+yjcf0EAhn//OvQk1O7+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AADB///Ro49C+9DP3wT3GpwfRfDVa1F/4/WPAfz4CpDgvvrWM3BA+yjgr0EBin8/u/Qg+A7/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AQDR///So49C+9HPz0T3F6QPRfDSc1H/4vaXIfz4CYzwfvrUM6AA+yfkj0EBiotPu/Qi9P5+8g')]
    procedure TestBitmapToonsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+B+eQwAR7MC/DA7ob/32mQtRQQEqkL/D/gVcPz/kHeyvQA7sz7A/AdCTL/kIFwjAwgVCDE9A5g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,7D+eAwQjnLCACvcrQHz+gldRgAQY4C9E/wB4T37gHpjvPxH79yA+/c+DT3gIEQzgwgUx+K4A6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6vcNhAEVyIDBCtgDED3+kDdBwAgpzA8E//1r/3/lDrjvgBH0t1C++7jD3/YIH8zvggMu/C6B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6v/9gwAdyPAADeMHYD3+hjMhQAgl8O/D/vhX3//mD/zvwBL854A9+q2nv/YHFdDvwgErLP8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapWaterTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AuiwPwDWo6/C+tDrj8T2GaQ/BfDUb+F+4/qPcbz4DZDgvvvXN9BA/Crsf4ECiqO/+/IiJO/+8w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,BPSAfwDVw39C+9Xbr0T2F6wvxfDSgyG/4fiXQbz5CYgwv/zUE0AB+yXgH0EDjIe/e/UmBA+/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,APiAv//Qs5/C+dTXjwT3E6zvRPDVawG/4vuTcTz4DZQgv/zVK/AA/CfkP0EBi4vfu/MhDP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,Bwixv//Xqz+C+trHz0T4F7D/hPDUcyF/4/mTgfz6CYQwP/zVJyCB/Cbgb8EBjIvv+/MlBA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,BPTAP//Uo4+C+tDfr8T3GKw/BfHTfxE/4/iHUXz5DIQgv/vWJ7AA+ynon0ECjY+/O/QjCP8+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AvSAfwDTs4/C+tPfb8T3FqwfxfDVdyH/4vqPYfz4DIAwP/vVM9AA+ybgr0ECiJNvO/QiEP8+8g')]
    procedure TestBitmapWaveEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/nuQAADcu3/C/dTnj0P6BegvhPbwB2EA6PkbIX34BqBQ/wDRgwCB+xCo3/8GnKdf/f0yBH//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+O9vAQDo408CAdbvH4L87i8Ag/sHnFCB7fqXYD73A5BQPwfbu6DA+vJ/v/0It8PffgI9AK8/7g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8lM/AgD3G/CBBNjzr4D9zn/wgQAWTCCC8/obUH/zA2CgPwzn18B/+tZ3XzsJ189fAAQ/+P4/7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7KcuQwAJaHCBCNzzHz/+qN8AQQUgGBBD+fiv37/tBiggQBDy36D++b6T7zgJ+dO/QQQ4/I5A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapWaveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,3T8g//4BhI+B/cGzH8L+8xdwRfzS4yB+6+jbUHzyBnggfwHZbxDA/A2knz0OpKtvQAFOCG+/7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8bN/AfsB9H+BAtTjnwIB9jMwgvzzh2B/6vtXAH/wBvB/PwboWwC/9PcjMDwF+HdAAfxDWE+/7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7mdPwP8dWEDABN+/D0ABw7fQQf4JYLDA9vMvwP/wGSCQ/gf9kwC/+NP3X7sK78c/gAY15O5/9A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9su+Qv4YpFAACOuvH3/+vIMQPwUm4D+D++fPnz/rBdS/vw3169D+98fH7/gGAsAvgv42IG+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestBitmapWiggleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,w+/h+gfJJ09B89wroMP3ZpSOfPKoX7D/4tQT0T8hxNsefu/r22//AFGAj0ns2BvveAXqc479Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,30/S+vfNT56A96yDb4X1lKj+PfCWX5C95957UX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,1FtCOvfBbw4B9Z2Lf8X4eOD+vOyMQ6C+5eXvQL0lrnevfOQNXD/79l1IzsLv2D+/exD9683+Fw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,3UtiugDgf65/8MRLL4f0g6wuvu2UayC+5NVnkX8jvU8OvPDKl39++HcQzwTvzLcPvQzbV36+Fg')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,3Ldxvw3X135A7vDDLwjzeKCuPuydcwB/49AjcXz8C1RwfPn3E4/7Cn84nkbo4WO/PArf9z0+Ew')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,DiwS+vvmqx6+9PHb38T0d6yOPe+bB7B/4NiXEH0S1msgQQG4c59DC2jEfoPpxLeufQ0j/H08Bw')]
    procedure TestBitmapWrapEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress, ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress, ARightStartProgress, ARightControl1Progress, ARightControl2Progress, ARightEndProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,2VPyevfEQz4B+J5z30X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,2YexvQ+Elz++5vr338YALjiv//Wgz2A/7vSroEIt6ac+QNzdO09D/d/zT4b73S//eA34vw1/Cw')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,1VeyOvjAV15B9aCDD8b1lZx+fvCXZ3B95+FvAT0wi3f/uvDM52596UJ4//7+RHhf/RoSDG4B5A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,2E+C+vjCV47B9Z6Tz4XqAlDAARUXoIHA9hSIsEAo0ccPPPEX5K8AABUQYAT/vKMf/+znT5DA+A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,1VeyOvjAV15B9aCDD8bvcqjOBO/SpwC+7Pzf8UAsji8fv+KfYx9A6ylYz4bptfstO/zM/w9/Cg')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBobwnp6BNvuQbca04+Fg')]
    procedure TestControlAffineTransformEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,4feQ/wHu++/D9OQDEATwSTTf+/vPa5A/5fJL8f0B9FePu/PWo4A++c/j7gD8DHhOeg4L4F0+AQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,3L+w/Qjvyw8B9PYPkAHwEWB/v+/h6zBB6/AzgL4N+k/A/Oz4O/9++fyX/8H+45OPfgUwiE6//w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,7zuSuwjcTx/A9fEn4EED/a+/wAD2n19A7/dLoP8fwP8Of+rc4w/A+ztYTwL70wdv+wcxRC78AQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,4ZuBe/jLt67/9syff0P5LVwPPvfP1w//8fp3QP4bxWPf/OUBfO979jy4v0Xz0nvf+gIDGB++Cg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,7jdCO/rYo26/9syD/8TzRBivPf3M259+7usPAX4gr48fPu4HhM/8+jvgXgXt8vs+PATfpz7/EA')]
    procedure TestControlBandedSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,v0tQgATvq9+EAaAT0EL2LND//vrhA1AA/wBkUMEA4+e/PP/Qu2CB9eJfQH/8BHC/+wMRLH4+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,5n8/AQQL9O9AA9m/fwH92UtwP/n3WxDB9gIwwAAC8V/QQAfyC6C/++ebn3oD7qNPwAcgpI6/9Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,4c/OgQEIbPAACd7jrz79w6eQwP/+E0CC+f3zP8D3+uuffwnsZ+B9+dW3D/oGBcRfwQQo/P6/8Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,6l9uw/4UfPDACtrnn378pOuw/wUWBHDD+/tnv3/sBvAvAA//xyC9+cOnv3gFFtAPggQhAM+A7w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlBandedSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,Avjw///Sq49C+9DX/wT3GaA/BfHXg1G/4vePQTz4C5QgP/vXL5BA+yXgb4ECiIdf+/MeFB+/8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,/QPBf//Po28C/M3THwX4GpQvhfHaj0E/4vWP8Xv5CpTw/vnYMxCA+iLcT4EBhntfO/MaBN9+9g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,+AOx//7Mn3+C/MjDL8X3IJQfhfDZj4G/4/KPwXv4BpSQfvneJyB/+iLoX0EAhF8fe/MXDH/+9A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,9/egf//Lo3+C/MvPPwX4Hqh/BfLisyE/4vt/4Tv5DJgAf/rgJ2D/+iLgL0ECg3dfu/IULB//9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,8vOQf//Jn6/C/MXLT8X4IpQfxfHhk2G/4vOPcfv4DIyQ/vnnI8C++iLQT4EAg1dPO/EVBK+++A')]
    procedure TestControlBandsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,4u9QfgPWj29D+8kP0IT4LZAPA/DOY/E+5P27MfsH/ctgv/S33z/B/Cf0HwMAo0ffuvoyoL5+9A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,4vNgfgPWi29D+8oP4IT3MZDvgu/OVxE/5P2zQbsI+79AP/S51x9B/SnoD8P/pUf/OvszlP6+9A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,4u9QfgPXg39D+8kL8IT2NIy/AvDNRzF/5P2fkTsJ/avQ/vO7z++A/SroD8P/pDtP+/ovpD6/9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,4usgvgPXf4+D+8cD4ET2LoyfwvDMQ/E+5P6nwfsIAKiQvvS3y9+A/CX4H8MAojOP+/kn1A4/9A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,4ucwfgPYj2/D+8cL8AT4JZD/gvHPY6G+4/3DcfsHA9QA//Wz3w+B+x4AMIMBnzsPu/kn5I7+8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,4fNgfgPWk29D+8oP0AT5LZQvw+/Qa/H+4/3DATsH/dOQP/W5409B/Sf0DwMAo0+v+vo1lK7+9A')]
    procedure TestControlBevelEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ADirectionProgress, ASizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,C1wgQv76gx8CAccrQEP99VtwAvnhDwEB5Q5AQX3yAqDg/v75y9A/+wswsD0HudsfvvkmBK+A9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,C5yfhPoSpE8CBNFfsAL+2tPgAf3k2+CB6QrMkD7wE0QQwAEDtJC/+/QDoPsI3SMgP/wtCD8A7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,BfweRP0JEBACCNYrcID/vNvwAAL5i/AC8QY40D7sEizg/wrqw8B++NrXPzsF3a/ff/0kBP//7A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9n/eg/4RYAABC+EDMP/+nhshwQUQTHBD+P/XT//oDhAQwA/zzxC++sCzv3gI+Ks/AQAnAF8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlBlindTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,S3iQQ/rhQ5+AGhaE73v4zh9hu/fZfwBH+QPE3r0A6o+vQQIoxG5+/DNcPn7+JewuvQabM688Fg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,9k8+Qv0GoLDACsQH4Dj4eYfQAQEZHCCC7RSkvr0T4F9wPff5p8C69QzkLkH58GfPuRYXCC4CCg')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,xA8+wwcRnCCCCsg3oDv5fN/QgAMSJEDE8f4Tz374MxwPPvvwJ1H/9MSbHz0Nh/e+/QBRFG8/6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlBloodTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,3gshP/y/b09CAKqrH8b4M/BOBO7WJ0FB49F7AXn8BGhAfPLoE3D6+iskX0H1hLue+fMN6P29AA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,6P9wf/3Fh2+C/re/z4X4MByfxO7aR7FA49uDMXr7BnAwvfXjIzB8+SVsX4H5gvtOevENbD6+/Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,8POwf/7Jm38C/cPTn4X4LkT/xO/eazGA4+SHQXv6CHgQvvjfN6D99yC0X0H9fzPvuu4M7H6++w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,9+fwf//Mr58C/M3jf0X4LWhfxfDjj7H/4+2LIbz5CXzgfvvcQ+C+9hv0X8EAe19/O+wKYK8++g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/eMQAADPv6/C+tXvT0X4KJCfxfHlq0G/4/aPwfz4C4SAf/3aSxCA9hgkQMEDe4vve+sMrN/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,ANhAgADQ068C+tv/T0X4KqjvxfLsy+G+4/yLYX34C4wQwP/aV7BA9RJcQAEGdadvPOgHFOC+9w')]
    procedure TestControlBloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECipd/O/QhDA//8Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,wjtxuwjhQ1+D9rlDkITzOGDPfvav60B+4xOUsX0W9YsQPu/Fr/8B/RggIIMB0zvvugRLPG5++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,x/NhuQfB686A7KfbbwXuOVDPPfit8+A94gRoQcAf3ou/Puvki8/B/Cyo3wL97ZOueA0qrO36Aw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.99,0ycyuQLE2/4/7bGbL0byTyCvPfiwv6A95fZzwf8hyTOPvuvnp/+//kF8D0Xz8a/+OAgdvO27CA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.99,1kcyuf/Dy+6/766LL4b0Y/y+ffepn3C95u5vAX8jujcPfur0m1/+/kdsPwbv6c8O+QUQ8F18DA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,2FdyOf3Csy6A8aiDH8b1ddzevfWif2C95+hrUf4kskdvvegBhN+8/U9kX4fs4+ceeQUAIN68EA')]
    procedure TestControlBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,EGhQwfzi71+B/uK/P4P58AMghfbnL/F/6PPz0D3vEBwAvwDza/A/+A28nz4Io9tfvvUrTD//8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,C8xfAvvzH5AAAuu//0H812fgw/v1w3AA7/Br8L7qEuwf/wT3h6C/9/ibD3wKyffPAPw3NM//8A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,ADDPwvwGTFBABejPf0D+wcdgwgAFYFDB9fATwL/qDuBvfwr4qwC/99+Hv3kK7OsfggE5CJ8A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,9Ytew/4ViHBACePb7/7+owfxQAUZHKDC+vW3D4DpDPAvgA/3w1D++MaXz/cJCOCfQgQ0+B5B7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlBlurTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvzQPwDSq58C+9PXz4T3F6wfxfDVfzH/4viXUbz4DJAwP/vWM9BA+yfsj0ECipePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AvzAPwDVq5/C+tTbv4T3F6z/hPDUeyH/4vmPUfz4DZAgP/vXM9CA+yjof0ECjJeve/QiEP8+8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfywPwDWq5/C+tTfv4T3F6zvhPDTdxH/4vqLUfz4DZAgP/vYL9CA+yjof0ECjpfPu/QjEO8+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AACB///Yq59C+tXjr0T3GKy/hPDRa/H+4vuDYTz5DowQ//rZL+AA/Cnkb0ECkZf/e/UlEN9+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/AcxPwDfn39C+dbvn4T2GLA/hPDOU7H+4v1vkTz5DoAAv/rcKwAB/SjgP8EBmZd/PPcpEK/+8g')]
    procedure TestControlBoxBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,1ley+vfBW15B9aCH/wX1kbQ+fvCVX3C9599/Ub0jr48v/OYWVC+6+GJYPwnr3wdfeQXWh/49GA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,22+R/PnU104B+q2bT8T2WTy/vvWzT8A+7OQP4X0XwqcPPfARpO+6+D5sbwXx6jNfOwXoo57+Dg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,3YuAvvvmUy/B/rqvn8L4I8APv/rSOxCA8OqfYL4K1ccfvvkM+H87+RuArwH39Vc//QT6u07/BQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,4qtvQP34zx+BA8bH74D67UNgv//wJ2AB9e8v4D7+598P/wIGRBC8+feT/z39AYAfvwQLzO4//Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,5sdOAv8LSPBACNPbL7/8tsfAvwQOELDC+fS/fz/y+fPv/wsAlLC8+dKnL/oCDKgPQQQc5I4A9A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlBrightTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,A/TAvwDVu48C+9XT/8T3F6wfxfDUg0H/4viXcTz5CmwA//vVP7AA/CvwTwECiI+Pu/QkBO9+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,+LcQfwjmN0CB8uP/vwTx5/t/BPXly5H/3ABkIf35DTjwPv/VP0AA/SQw8H4CiFuvu/cp5F7+7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,6Bs+RAIfzOAAB+YfEH7+gEshgAks/P9D/vpTX7/sChSAPxDxu+B9+sCrr3YKD7w/QgQrBA8B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,7WPegwMczLBAA/Qv8H7+gEshgAks/P9D/vpTL//uDPy/Pw3pp1D++ti/33cJ/5t/QQMsFG/A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlCircleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,APwQwP/Rp4/C+tXPLwX3mqiO//KzI3A98NQDogAUwV+/fe+z8+C/7flrgAbvettfPPf/25/B+w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,8guh//3Gm5+C+9LfnwX6hOyOP/S/W4A+6+a/8Tv+Bmywv/fI729A/D6Uz0H/hk//e/oxAH//8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,49+R/f+hI/8A/dOjP0T2GJxfQ+24p/H+2wUYkTsGCmhAgPXi776C/2PU7oP977eu+AL8m137Dg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,5tfhfQGgMw+B/NWfL4T3F6wPhfDUg0H/4viXofsCE4hQwPbm4y5DAGbU7wL8wZPvO/Qv2D4ABw')]
    procedure TestControlColorKeyAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,0R+xvvq5Uy8CApiTf8b4NMjuQ+3TB8GB4sdvgXf9AmQgu+7x94/4/TLQXoHwiHfeePcQSJ19Aw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1B+hvvq7Ux+CAZqPP4b4M9DuQ+3S/6DB4shrkbf9AWgg++7v8+94/jHcjkHxhofOePgTSL19AQ')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,7xtBf/3Ia1+C/renT8X3Hyjvw+zD//BA49mXwfn7B2zQfPLZGyA9/TcgzwH4lAfP+foluJ1+9g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,BPwAgADTq68C+9bbz8T3FLQPRfDSh0H/4vmjkTz4DpBwf/vTO0DB+inkbwEDjp+vO/QiEA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,AeRAgAHS28/C+dwLUAX4HvRvxvTyDwK+4g2I4X32DaDggALfUyDC9xWg8MAIfNsPvekMUJD+9Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestControlContrastEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,h+vg/v/wE7B+3wWAEED0Svx+O/TRk2DA4d7vsH0lrWuP/OQVUN+5+GJYTwnr3wdfOQXXgw5+GA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,3VPi/fqwr57A+HeHD0X1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,B+DxQPqmb97A+LQPIET1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXQQEV4ce/vfLiDxGB8ONvEMQGqHt/+flg2L7A9Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,pIvufBHtU+A//7rvfz/t+I9/+f2uX9FB4NLTwPwhsVcvvuERhM86AFBcH0jv5jef+AXsN559GA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,HXCxPPWY405DDobT70H1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestControlCropEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,4N+wPQHho+9/Bd6/IIAAKRDQfPH6Q+A+9wHwIP32I8B/vwL4BwDAAiM8Tz0F87s/Pgs+eA5B9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,5kOP//8JaOA/Dc3f3/8D0ItQffvvx+9C+cx////87VMwgQbGd+BABNDzb//+7/Ov/QAdUI++9w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,uAMvwQEMAEBABt5nMD/4ty9APwch/I9C+w2w377wLPBvQQUBZCA//9efvzoI/w8QgQAuIE8B8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,BYjuwf8liJC/Bfwr0Pz6sE8wPwIm3L+C+Od3D33pE8jP/g0A2CB9/8UDkLkJDwSgAQQsPF+B6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,6o9gPgHv858C/cdPQAP5DMCfxe7Kf/H95fBb0bsAEQBBAP6wG5DC/jZoj4EGe3evfPgvJD9+9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,6z+RvQbbY19D+cYDcEP1Iahvg+3Ax9H+4vyLYT32FzTA/fnCV4AC/hv0378HieNP/vEdmA9/+A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,DeRRfwHgq37D8N/fP4XwP2SfP/G446C/3xFAIf7tHwygvPcRcDA/+yZw0H7/3K8vvvcOAI++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,HvwgAfrak5/B9O0XEAbzEIDPwvPLu3D/3BFAMbzvCwQgv/4bcIC//x4QgAH6vjs/fPlGsD5+8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,4D/Qfvv923AB/eDHD8X26OsfCPTmA8G85PdHobsMD7TwP/LIe18B/0C8f0T/mq/fOQM3xF6+7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,39OgPAYRjB+B+cmLcEP7JQAgg+vR+3B96+lvYT0H7yOBAAGtv09C/j303j8EggOvPgFFOJ898w')]
    procedure TestControlDirectionalBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,CdDQvwHDu/+C+dK3L0T6DrjPBPLUR2F/5vl/gfz5Cpww//rSE5BA+yLsr8ECjJuP+/QiDO++8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+4cAQ/rzi6+CANTjv0P8BZBfQvjhn7B97wAk4X36HZyQ/vnpF+C/+hIkID8Krauvfv4agJ//8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,9rOORAkSXNBBAecPcAH/oqegwgASYNDB9e0voP7g+5cQwBDtx5B8+bif37kFFGyfgf42xC4B5g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,7OcdhAAfwNAADeDvb33+gEshgAks/P9D/vlTDwDnBxSwwBX850A9+bGvb3YKE9R/wgUt9C7B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlDissolveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,9t+dQ/8llGBADe67j/z+eCvhvwcXqP8EAPdvPwDrHfS/AQ76d5D//rhfDzgKAvwPhAI15B7B5A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6vtNBAQTkFDBCub337z+e08hQAsu5P9DAP5Lj4DqDhQQAhL411C++rCTj7cIAtRfAQA4IH9B6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7PdtBAIXpCBBC+b3Xz3/gT8hgAku+O9D/vdLTwDoDgxQQRL75+B9+a6jD3cMDtxPggEuDD8B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6udNhAAcvOCADOH7r/3+gUchwAks/P9D/vlLD0DmDQTQwBL948D9+ayzr3YKFcyPQgMsCD/B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlDropTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,1jMOAgAAgFA/ENYDALz1jIMAAAAg1A+E9QAAbz3jIAAAAAj1A2E9AMBbzzgIAAAAQv1AWA8A8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1zcOwv//f1D/D9cDEHz1jYMAAAAg1P+D9QEEX33jHwAQwAf3/1A9AMFb37gHAgDwgf0/UA8A8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,2UMuAv/+d1A/D9sDMHz1kYPwv/8hzP/D9QMMb/3jHfxPwAf780B9AMJfD/kGBvzvQf4+SB/A7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,3VdOAv78a0B/DuADUDz1loPwf/8iwO9D9gYUb73kGvyPgAcA5CD9AMRnT/kFC/y/wf49OB+A7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,4WduAv37XzC/DeX/f/z0nIPg//4ktO/D9gkcb73lFvjfQAcH2AB9Acdvr3kEEvifwf88KB8A7w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,6Ht+gvv6TwA/DOr3n3z0pX/Qf/4moN+D9w8kr/3nEfRfAQcPxMB8Acp/TzoCHABQgQA6FB/A7g')]
    procedure TestControlEmbossEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/V/AAADh4z+C/tbbXwP5+QMQxPXmMzFA6PgjET31C3SAPwDdVyDA+g7gT/8DpqP/fPckCH+/8A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+L+fQQDwG+ABAtjf70H621MQg/r45yDB7fmvwH3xC1TQfwXke3C/+vbXHz0Fwq9/vvomBN9/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8yN/ggD/U4DBBdvnb8D7vKcggv8JmBBC8/k7gP7tCzggQArsn8A++t7P73oG37v///0pBE/A7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,74NOgwAOjDBBCd3r/z79nvcggQQaSADD+PnHTz/qChxwQA/zwxD++cXDr7gH/Md/AQErAL+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlFadeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,jku/QQK+kz5//LXPQIHj0m+QgO+kzx9/7TNQ4Lj0GyTgO+nzx1/7DBQ4Lv0GCfhO+vzx1z4DBQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,rft+QgPYLy8//9e/0H/rvp/QAPbLy8//9S/037rvJzSA/fLy83/9C/237vsJDWC/vPz8X/9C/w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,zacugwTyzw8/AvmvYH7zqcsggfzzw49A/iuY33zqMkgg//zwI5D/CuY3n7oMEsg/P/wI5L+C+Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,7FfuwwUMaOD+BBuc8Dz7lftwAQMauD/BBic8z37lPlzAgAbuT7DBCc+zX7kPFzCggfsTbHDC8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,CwSexAYmCNH+Bz2QgPsCgSexgQlCtP9BDyTgvkDgSWxgghDtf9ADCbgvEHgSG5ggRPsf9EAC7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,KrBdBQhAqLG+Cl+AELoKbFcBAhBqrK/CFyCErgLbVYAAhBrrq/AFCKGrwHYVIAChxvoqfAFC6A')]
    procedure TestControlFillEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,0XOR/PvA+46B97PX7wX3UhAPwe+u60C/5OmzkbkO7VOg/OgAcC86/z30bsT1vL/e+PjbZ56+Cw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,21dB/fzDK7/B97vfz0X3SUAvQvG+L+H+4/GncboJ82eAfe77l687/jNQr8P5rPeuefXj156+Bw')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,5DcR/v3HW/9B+MTrr4X3P3Rvw/LPd4F+4/qXUbsE+n9wPvT3vy/9/Ciw7wL+nS+PuvHsR6/+Aw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,7hvB/v7Khz+C+M33j8X3NaiPRPTfuyH+4gOIMbz/AJBQv/ny57+++x4QMAICjWtv++31u68+AA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,9/uQPwDNt3/C+Nb/fwX4K9jPxfXwA8I94gt4AX36BqgwAP/uDzBA+hNwcEEGfqNPPOr+K6A+/A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestControlFillRGBEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECipd/O/QhDA//8Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AQBh//7gr48C+dvzz0T2F6QvRPHPN8E+4gBgcTz5D3zwfvrmJ7BA/Srob8EAoY9v/PcrAK9+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,6hvBvQL6m6+B8dMnUETwCqi/AfLEzyA+4gIs4f3/DvyffvjoJ5BAABfcf0ABzkO//QM4vH499g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,2yMBvQTzk1/B8NAb8APvC5DvgPLMx3C+4/0rgf4DCOCPvvbqE2DA/hPUXwAC3C9fvQY0nG49+A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,zzcRPAPoh48A8dILoIPvHmivf/XOp3C+5/4DMX8J/LtvPvXx96///hfELwEA4EOPPAcohO69/Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,1z9xfADbW09A9c7Xr8PyNkgvv/XJf5B+6fPbUL4O4asf/vP518+9/R18rwL53VMPPAYQiE6+BQ')]
    procedure TestControlGaussianBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,AdhQAAHR479C+d4HUEX4JPCvBvb7J9J94g100T32DLDgQAPkP4DB9wuw0EAJcM//vOYHeMA++A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,ANhAwADR17/C+dz/T0X4JtBPhvTz+1H+4gWAkf32C6BwgAHgRyAB9w+A8IAHdLuvPOgJONA++A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,/9tAgADQz79C+tr7T0X4Kaz/BfPt18F+4/2LUf33C5QQwP/cS6AA9hJYEMEFd6dv/OgJBOB++A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,/98wQADQw6+C+tjzP0X4K5C/xfHnszH/4/aTEb34C4iwP/7ZTzBA9RYwIAEEeZMf/OkKzO+++A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/+MgAADQt68C+9brL0X4K3hfhfDgj5F/5PCb0Xz5CnxQf/zWT/B/9RsEQEECfofPO+wOgP/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,/d8wwP/Ps6+C+9XnP4X4MVQvhe/ecwHA5OmfoTz6CnAAP/vUU0D/8xvoXwEBfmuPO+sKbO+++Q')]
    procedure TestControlGloomEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,7d+wPgTZo38D+8n/r0T3JaCPg/DRZxF/4/2nsXsDBbgAP/e6/0/B+yIQgML/pW8vO/kqyA5/8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,sF/xuQvaO6/C9K4vcMTyOVivvfmo70B+4hCIgf4Z+Zfw/e7Lp58C/A8UAIME2TeP+gVWCA7++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,zAcSOQSst35/5pObn4XrOVTfffuiD/H94fVjUcAj1ndfP+rTfw/D/SuczwH8+Fs+dw0naF25AA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,4xMiufzJrw4/662D30b0SzBffvqyv2A+5OKHcYAkxPPe/+vWs++AAztYv0Ly/4e+tws3hN36Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,3EcC+frH3/4+77pzD8f4VhDfvfixqwC+5d+Tcb8jtgOvfuvpo7++BT9Ir0Tt9acOOQczjE28Bg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,2GMC+fnE134/87Fvr8b6a+jOffikl3C95t1/kb4jqit//egAjF+9A0BQj8bp6uu+uQMayM08Cg')]
    procedure TestControlGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,BuDAf/3Rsy9C+dmrH4T2PcAfx/YJDBI+4wRMMf72BJhw/wbxV5A/9vurIEH9U6Nvu+wgxA9+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,COSw//3Rpz/C+devDwT1N8wfh/T/z8H+6Ae8sT38GeTgfgXsL8BA9RdY8ED7at+fOvEfkK++8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,COywP/7Rnz8C+tarDwT2HuAvBvPot9H+5Qe0Af35Gdgwv//hFzDB+x4U0AD/fLdv+vMjKK/+8A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+gshQALSu++C+9EHoIX5F6QPhfHXq9E+4PZnUTz2+2PQ//rWO9BA+iM44EEHjKMP/e4YlL9+9Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,8euQgALOuz9D/dAbQEb7K3BPhfLo29H+3u47Ab3y9UOAgPrdM0C/9gyMEIEIgGd/feMAHKB9Ag')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,9sNwgP/Nx7+C+9j7nwX7MmhvBfPs6wH/3u9HIX3zAUgwgPvcUwC+9vqDAMEHaTsffN3wY5B9AA')]
    procedure TestControlHueAdjustEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,Avjg///Sp58C+9LTz4T3F6wfxfDVf0H/4viTUbz4DJAwf/vVM8AA+yXsj4ECiZOP+/MiDP8+8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,t0MROwngM58D+bA/gAT2Q1Sf/vax75A+5AqooT0Z9LMwfvC7t4+B+xgo0EMA0j+vOgNNPM4++Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,vQMSuQSroy4A6YjDr8XqRyhf/fuQB+F94fpfAT8j3qPf/ubha1/C+ydc3wL/938edwoglF05/w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,2hNSOf3Ci15/6qB/34byWBA/vvucwyA+4+J78f8mw/e+f+nhoy8AAEAoj4LyCnQe9woegK36Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,3SMSOfrRq26/761vL0f5Z/jeffmlnzC+5dqHET8lrOuevuvvo599BUZAH4TtBaieOAghiK38Bw')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,10cyOfnLvw6A87Brr4b7d+iePfekh6D959qDYf4joh8vfeoFhP87BEx4r0bq79su+gQMuI09Cw')]
    procedure TestControlInnerGlowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,0Psg//u4j09CAKPL/4b5PijPBfT/z/F/4eNHEfn5A5QQffgE9G/5+xS0j8D4bLt+uun11069CQ')]
    procedure TestControlInvertEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,9FcehP0LYFDBD9fLD37+q/MgQQILfBBE9vEH0D3vFyjgvwTtn4C++u/z//0EpocPvfko/C7/7g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,6e9NhAAbtPDADODvb/3/iGPhvwglGLAE//dzz/7pGPhfAAv403A9+s/rP7sGxo+//v0s+J4/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT73/mDvjPgBP952C9+a/Pr3cJ+58/wQIuCH/A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlLineTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,EnyAPg32b/BA7uMPgEf5HohvxvLPQ9G+5PtDYb/1MMifAPmo199D+0j8n//+aVOPvPZJLL+98Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A6zQvh/h039F6MnH0EPq3KsQgvbDUzJ+2xMksr/0U+hAQPk0mK/+A0hMMH8Gu0OOOPUs6N5+7g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vdtwfOgNGOB9Ab/Hv3/0ILweuv++C0+F6u3zzf0RsCtggf6ab0CDDtx7bj754Es//gfhm64AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgUHr4Sdje++FXzP/z9i7038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestControlMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,H4AAv/UbrGB97wtgb0X2E9Sfx/a1zxA84gLUsADsNNg+fv3RN6DB+D3wbnoMin8vgQNDvK97+Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6udgug7x1/A67P2fkAPeoD+QffPo/9B/2yiEcQMGFZCPggcCdECEARwIT7gH9pdvQApfbG4B7A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,uMuvPhbw3wC/+BdosMH3ejcRAf0klMDE7eGnn34EFCC/gPzjB9DB/0CYP7gL9Odefw0uTG2C7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,4hsfAgXkhxCA9ypAMED+gEshgAks/P9D/vpTP0AR1udvPwLZxyDB/Sh4IPsK6Z9/v/o1VJ8A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0xPfhAsPjMA/9gSw8H/+gEshgAks/P9D/vpT74IeuEOvP/2M/2/D8g7ojzgPAJC/PwtDzI6B5Q')]
    procedure TestControlMagnifyTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestControlMaskToAlphaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestControlMonochromeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,9m8PQgD4N7ABBNnjLwH7y3+QAv0AvKCB8PlzIL7vC0jw/wfoixB/+urT/7sF0bM/P/wnBB+A7g')]
    procedure TestControlNormalBlendEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxCB9QpggMAGdq/PPOoMCBB/+g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,/uMwQADPz69C+trzH8X3MtxfBvT404F+5AeAwf31BpiwwP/bUzDB9QtwkEAHdK+/POoKHAC/+g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/uMwQADPz69C+trzH8X3MdRfxvP405F+5Ad8sT32BpSgwP/cUzAB9gt0kEAHdK+vPOoKHAC/+g')]
    procedure TestControlPaperSketchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,ANxAAAHQ569C+d0HUAX4IQzwRvcARHK94RNowX31DcAQQQTqN6DB+QrIwAAKb9MPveYIkLD+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,/98wAAHQ569C+d0HQIX3IQgQB/cAQJL94RFooX31DrzwAATsM3AB+gu0AMEIcr/PPOcHjJA+9w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,ANhQAAHR469C+d4LQAX3IgwAB/cAOLJ94g9ogX31EMzwAATvH1DB+guoQAEJc8dv/OYKkLC+9g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,ANhQAAHR469C+d4LQIX3Igzwhvf/O6I94hBscf31DcTQAATuH2DB+gqYcMEIc9NvvOUMjMB+9g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,ANhQAAHR569C+d8LQIX3Iwjgxvf9M7I94hBwQb31EMCAQATwI6DB+guUoIEKc8uv/OYEbAA/9g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR569C+d8LUEX4IwSQhvj8M4K94hJo0Xz0FLCwAAT1M4AB/RGogIEKbZv/fOb3j+D+9g')]
    procedure TestControlPencilStrokeEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,va9hOQvgj+8/6q0X4MbmLHBfe/ez/4A/4/sTsb4Yxhuv/er1iy/5BjYEz0bsAgSPuQHtB/48Eg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,9aMS//2Pz70D8bKfP0L1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,90PAf/v9vwBC9+N7H4f03usvyfT65+D74RFoYQIaCgzffd/2/w8/BwCIkMPxwdPv+/ZxFE8A/Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,yJ+R/wi9n36A7tJ70APzfcQ+PfO+Q0D+5N9DQX0kr3/PPOQXRJ959mJUX8nq4ANfeQXVgw4+GA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,yU8BvP7sOw/B6/EvoIDyPPR++/bIw2BA38HjYP0krnc/fOUUZM+5+GBYbwnr4ANfeQXWgw4+GA')]
    procedure TestControlPerspectiveTransformEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,B4gA/wbho89B9MQb0ET3GaC/xfDSZ/H+4vp74bv1Fmzwu/j3/7/7+zzsv4H9f2eP/PUWfP9+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A/zQfwDSq59C+9LXv8T3F6wvhfDVfzH/4viTQfz4DJAgP/vWM9BA+ybsj0ECiZePO/QhEA8/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,3duxO/zSXw4B+4tvv8LueOzuPfCSt2A+5OV3YbkI1YP/POtCAFA4/15MX4nm2f+e+gDWez4+Fw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,AvzgPwDSp48C+9PTz8T3JrBfhPbPW5E+4firUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,2WeiuvbHWz6B9KOP/8X7i9BOve2xG8B+7vCb0T0lpoevfOQRXD86+FRYX8nq4ANveQXWhw5+GA')]
    procedure TestControlPinchEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,1CcSu/jCa05B9qOPrwX1icQuvvCaU5D96ONrQX0isYsvvOYUZD/6+FpQn0js4guvOQXZjz6+Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+s/gv/vpf48C+9EXEET2Ebj/hO7JawF/3+9zsXz1GbDgP/roR4AA+ioMsMAGjHOfPPko9E4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,BgSxP//Wn09C+9bP74T3FrAPxfDUg0H/4viXQfz4C5QQP/zUN+AA+yjsX4ECjZOve/QhAO/+8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfzQvwDTq49C+9PTz4T3F6wPxfDUgzE/4/ibYXz4DYQgv/vaN9BA+yXoX4ECiZufe/MhEA+/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AgDhfwDSo5/C+tTX78T3F6wPxfDUfzE/4/iXQbz4C5Qg//rVN9BA+ynsj0ECipOfO/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    procedure TestControlPixelateEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp4/C+tPT34T3F6wvxfDVgzE/4/iXUbz4DJQwP/vWM9AA+yfsj0ECiZeP+/MhEA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,AvzQfwDSq48C+9TT34T3F6wfxfDVgzH/4viXUbz4C5Awf/vVM9BA+ybsf4ECiZePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,AOjQfwHTvx+D+9Lnv4T3GazfxPDXdyG/4/uPUXz5CoAwP/rQJ/AA+ybcT0EBkZv/O/QgAP8+8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,699thAAawBBBDeP7jz3+f1MhgAkuBACE/vxP3z/mCgTQABX761D9+aq/P3YIGNAPwwMw/B7B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,6ucthAAdxODADODzf33+gEshgAks/A9E/vpT/7/mCgDAgBT652B9+a23f/YIGND/AgQu/C4B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODvf33+gEshgAks/A9E/vlT/7/mCgDAgBT752B9+a23f/YIGND/QgQu/C5B6w')]
    procedure TestControlPixelateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp58C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+QNRvwLUlx+D+tDj3wT3I6D/g/DRbzH/4vyfEXz7CahAP/rNJ/AA/Cb8z4EAmH9v+/Ui7B4/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,7u+gfgPZm08D+8v/r0T3JaB/w/DPYxG/4/2rwTsACNiQf/nBDxAB/SoAUAIAoGsfe/gpyB6/8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,4s/w/QLkp18D/McrYAT4IKjPA/HQY+F+5P2zwTsGBwzxf/i581+B/TEEAIP/oWePOvszoC5/8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,2LsgvQLyt/+C/MNb4MP4Ery/hPHTa6F+5fy7EbwLBCQRwPa60+/B/ToM0MP+oG8f+v06gF6/8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,18NgvAL/w+/B+8B/gMP4/tvvhfHXZ0F+5vyzobwP+Q+hv/TCr3/C/kUcMMT+o4P/OQE/cG4/9Q')]
    procedure TestControlRadialBlurEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    procedure TestControlRasterEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.99,x79wv/fvb3/+8d6jHwPwLOSvAPKt7xD67biLcL0krm/v/OQXPG8592RgT8nq3gdveQXVfy6+GA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.99,x8dwf/fuZ3/+8d+fDwPwLeSfAPKs7xD67beHcP0krW/P/OQXPH8592NcT8nq3wdveQXVfy5+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.99,xruQ//fva4/+8d+nLwPwLeifAPKr7yD67beHQP0hsnsv/eUTPG/59mRg78jp4CMfOgPcj17+FQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.99,xregv/fwY4++8d+rL8PvLuh/wPGp7zC67beD4P0dwYNf/eYIXO+59lqQH0jq4R/PugDxo74+EQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.99,xrewv/fwW4++8eCrH8PvL+xvgPGn70C67bZ7cH4b1HevveYHiJ/69U7If0fs4Sc/e/8IyA4/DA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,0.99,xrPA//fwX5++8eCvL8PvL+xvgPGn70C67bZ/oP4X5XvfveYBqI/79EDYH0bv3jP/+/8i9I7/Bw')]
    procedure TestControlReflectionEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALengthProgress, AOpacity, AOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,9DvhvAXrry9A6tHjv0XwF6hPwfW8FwH/4/tzAf8EDUhwvvzhd7/A+jaoP77/uePO/fsD6H69+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,Gpxx/Ang8w6/7bjf3wPvNUSPvvq6p9D+3u1fQb0B6Hdg++4SlE+++Sukb8MG4zef+wIcyC49/w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,8lsxvf/pi3/A7sobgEXzLpRPf/Spj3B/4PnrQL0CB1ggffbln29/BDh0j8P4x7tfO/j7c299CA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,6Z+B/AfsD+//8c4X0EPyRkAvgvG2gyA/5u/DAD4BBoxAPvjed39AAi7AX0L7pJevffcvkF+9AA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,9EdxvAX0R0/B7tMH0ITzSjRf/fa5e0B/7PQTAbwADMgAPffktx8+BECkTwP20Pce/P8QWO59Ag')]
    procedure TestControlRippleEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+W8wgP7mw8+B/tPPjwP6/+O/A/fgF1GA6fUvIX3yEYyQvwHdZzAA+xPor38CrI8/PfchEH//7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,8KfvAf7nNxCCAdcHEML31VugQ/r/5+DA7QaccL3vE0AgQAXgL4D/+f2/P70Dxmu/Pvsj1H4/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,AiCPggQEdMBBBuXf/z/9usvQQf8HnJDB8fM3cD7xBTDAPwj2u2B+/NrLX/oI3QMAwP0xKG/A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7Jfugv8UaOBACd7jD7/9o+OQwAUTMCCD+fXLf7/oDCxQQBDy2yB++cbL37gGAcifQQArHK/A6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlRippleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,AcygAPvsW8/A/t2r38L03OO/B/Ptr9A96f/HAD31CCCgvfzzwwA//QHM3/7+rdNvffcn0K6/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/dufgf37V3AB/uTrT0P32vffwvT6d5AC6f1XoL7xDAhQQAYF0LBA/QlQH30Gutdf//4zBM8+7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,8Wt/AAUPQBDBBOL7f/7+0E8AQAAKfPAA9PYrEL7nGthvfgMOqAA++8+rb3oL5Kuvwf4wQJ9A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6NeOQ/4CTCCBCeTTfz/+qcNAwAIkKBAD+fSrb//oCAjAQA75xwB+98DnX3gJAPB/gQAq8E6A7Q')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlRotateCrumpleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,COxggALXy99C+tn3/wT4FdjPRfLh09F+4gKckT31DYRQwADWc/CB9htc8IAGkM+PvOwV1P/+8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,COBggALY28+C+dsD8IT3GPyPhvTwB0I+4gqEAX7zEZTQgAThg2AC9hHA0AAJhvPP/OgKVGD+8w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,CNhggALW679C+d4L4MT3GhgQx/b+P8K94RFsAb7yErQQgQbtY0BC9wf4gAAKegPgPOYFlHC+9Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,CNRgQALW768C+d8L4MT3GyBAx/cBTJL94BNgEf7xErgwgQbuXyCC9wT8cEAKdwPQ/OQErIA+9g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlSaturateTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AADRv//Ro5+C+87P74T3GpwfxfDWe1H/4vWLQfz4CoQAP/vYN2AA+ybon0EBh4tvO/Qh/O4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/AfBf/7Pl58C/cS/D8X3IIAfhfDZc3G/4u93MXz5BnCQvvrbO2C/+iXkvwH/gm9PO/Qh2K4+9Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+Qux//3Ok5+C/b+3H8X3JHAPhfDaa4F/4uxvIfz5BGRQvvrdP/B++iTkz8H9gF8/O/QgxI4+9g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,9ROh//zNh58C/7WnT8X3KlQPhfDdY7F/4uVbEXz6AFDwffrhQwD++SPg74H7e0cfe/QgpE4++A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,7B9xv/rJc5+CAaKHn8X3Nhj/hPDiSwGA4dkv4Xv7+ScQvfnpTyA8+SDYLwL3cQ/PevQfXL59/A')]
    procedure TestControlSepiaEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.3,0.4,0.99,9K8wfwHd88/C/Mf7PwT4CbyfBfHWkzG/4vWHoXv9DtBg//m8E1AB/Sbgv4EChI9fe/Yt7L7+8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.5,0.5,0.99,zp/gvAX3z5/D/L1zQIP3C7x/xO/Nm+G+4/eTwfsLCiBhP/ijz48C/SfYr0ICiFcfe/w3sL7+8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.6,0.6,0.99,u1dBugrmO8+B868z0APyLIzPf/OyS9G+4v6jAX4V/vfwvfK4m9+C+RUkMAMDuhv/OgRKOO7+9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.8,0.8,0.99,tssx+QutJ69A7Z7jn0TsMWx/vPud/5A+4gF4wX4X/KPQfO/Is58C+wDcvwAI8bfuuw1VwM388w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,1.0,0.9,0.99,s9MheQ2sL2/A65rnr0TrHnT/vPunD8G+4PdzMb8Y+zfQfO/ky1/C/AOob4AD7ju+uRNLOD269Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,1.0,1.0,0.99,u+eh+AqxC5//6ZPjD8XtIniv/fq1/wA/4O53EX8b79PfPe/rt+8BAB6Mv0D78xsOdxM6DM25/g')]
    procedure TestControlShadowEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ASoftnessProgress, AOpacity, AColorProgress, ADistanceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/IOAvgvtj1AA6dcXwIbmwEOQg/rxE8I/1SCUsf38B3wPPwPJb4BA/DlAED4AfHffuvkv0L798A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8Evfww0c3IBB5QPAf0T7sUORgvwp1HB/+O5b8P72K+jf/gDgZ6A/ANyfn/cL08cOPgREfA7A8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlShapeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvjQPwDSp48C+9HT34T3F6gfxfDVgzH/4viXYfz4DJAwP/vVM8AA+ybon4ECiZePO/QhEP8+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,APjAPwDSp4+C+8/P74T3FqQfxfDVgzG/4vibYTz5C5QQf/vUN7CA+ibkn4ECiJN/O/QfFP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,APTAPwDSp4+C+87L/4T3FqQfBfHVgzF/4vebcXz5C5QQf/vTN6BA+iXkr4ECiJN/O/QeGP/+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,//ew///Sp4/C+8vL/4T3FqAvBfHWhzF/4vefcbz5CpgAf/vSN4AA+iTgr8ECh49/O/QdHO8+8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/vOg///Sp38C/MjDH4X3FpgvRfHWhzE/4vefcbz5C5jwfvvSN2DA+SPcvwEDh4t/e/QcJO9+8w')]
    procedure TestControlSharpenEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,0VPA/gAgOKB+6KbbAAbr0SOfgwDoe2++5ipIjz3oEVyQxBMMfKCADiXMH/z87/twxPQeDPC//g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,kMe+/AHIk5AA+cuzzwDxEGDv+wMWiN9ADQ/I73zh8fOvQgWzn3C+Ec6/n3z+OHCABfjqM/FCAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,tWMvewDjX6A/7OfLwH7z1kvQwgAp1J9+BDKoXzvhMKQQgAQMPE889fivP3kHI0yQRAAtuHCB6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,0TvvvgsOGA+D97uHfzzqn6sQwvwiZFDC9RNkj33v2ScvgSTLj7BDA9nPn3cO+IdQhfhNsCAB7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,17dfAf4eEKG+7GeEIID6mItQvvkbhG8E9uZbn/7pBqCgABXeT0CEB8V/H/gHDyDgwgI3AGDA5w')]
    procedure TestControlSlideTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,IISQPgn8U3AA7+7z38f4HohPBvLNQwF/5PxDIb/yMcwfQPqq1+/D/Ebwr//+bFOvvPdEOL998g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,ArSQ/h7q3x/F6dDboEPr4q9ggvbCR+I93BQgsr/1TPAgwPsnjJ8+BEgoYP4Gt2tuefUr3K4+7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vRehPOkRCHC+/sHPz0D0ILweuv++C0+F6u3zHX0RtUtwwf6aQ+CCEOF3bj754Etf/gfgm54AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgYHr4SuD+u+JWzP+z9y3038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestControlSmoothMagnifyEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,BQARwP/Tm48C/NPPz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,AmSPAAS+sw/CG337zr/3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,DLTehvTWt99EFqAzf7z3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/RMxwP/Qk9+C/drz34T4xNdBwQwX/C//FR4AULz4DJAwP/vWM9AA+yfsb8ECiZ9/O/QjBA8/8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXwXz5AMhgP/3cV1DA+hxYr/sLGmyAAwMhmG8D7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXYT3uzRcxwBbbK5BB+9V3X/oB+hfgAfgr9P4C8g')]
    procedure TestControlSwipeTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,DdRBwfq8u34D8eQPIEb8DUQvhvPAB+A+4g0wkDz6CLyg/vvap8CA+igksML+ksOwOf91gA5/9A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,9e8xP/yux54D9dADIEb9JzDfBu/Q34+95PebQH38CchwPvzV4yAC+xbIb8IAkq8AuQNqcP4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,0EfSPf+pi+4D88njf8X+QgRPhPDa7w//5ujHYIAR92cQPvraW1DD/t7vvsIGmf/POwhb3O797A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,xzeSuwSz096C+MnLL8P8PDivRfL129A+4/dbsUAZCRxgffLdey/C/s+f/sEIhJsPPQdZ9H6+7g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,ujdC+Aat+67B8sDL74H6MkCPxfDuF/F+4vhn4f4i7SdAP/X2Oy8C/f2LTr8FvV8vAQ5Q6E5/6Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,zFsSOAmWX64+5ZZLX4T4LWRPhe7WX9F+4/ePkfsg24/QvPkZvH6A+CRcznwKsiv/Qg8u5B4B7Q')]
    procedure TestControlSwirlEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,/ZtwwP/Wk78C/dLvrwP8E9D/A/f712AA6PsD0f38AMDgvgLQq7BB+wlsH8AIl7t/vQFCAC//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,1oOwgP7VY/9C/tPvX8L/++vvAfryB0CB8eQv0MAD9su/PwDXiwCB+shnf/0F1PP/vQdP3C5A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,yJPvQP4E8P/B/trbf4D/zy+gQP3/828B9+nfL8D9BBzgfgjxZ1B/+8Krj/kNDdCPfw8oVB5B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,4Y8eA/8NeNBAB9zzDz8CmhPxvwQdBDCD/fyDbwDwBhBQwA3ym9B++L+v3zcLE5QvwQMpyF5A7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlSwirlTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,1TvS+vnIgz5B9adzj0X1jrR+PvKgT1D96dxjUb0jrZ+PPOgTVH8691hU70jr5yMP+gTVn849Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,7XsQfvnhf89/Bsrrb4H7I5yv/vfdAyDB8vyTsP4H5J8vfvb6b3B8/hKwT0L67Id/vf8SIL9/CA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,23fwPfzeg9/AAsjDT8H2IIw/f/ffP1AA8vF7AH4G5MNvvvgPMEA8/heAz8H585svfgQGEI9/CA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,319wfvzie0/AAMvT78D3GaAvv/jeFyCA8vQ/AL4F6MNPfvcEEKB8/Q58j0H68ZPvfQECJF//Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,27uwvfvbU58AAMLPr8H2MHQP//bUL9C/8PN/gP0L2rePPfUH/D88/BlkL8P28GdPfQL8Dx+/Bw')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,5D+g/v3li78AAcvXz0D5D6yPf/nhLzDA8vY7cD4D7N+f/vgMGJB8/g6c30D7+Kc/fgMFJJ8/BA')]
    procedure TestControlTilerEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+BNx//7No3/C+8PHX0X3H3zvxO/SV7F/4+2T4Xr7AIgA/vjbGyA/+yq8/4D+jVe/evUitN5/9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,ABCB//7Pk1/C+8m3r0T2HIjPhO7NO+E/5PCngTv6CngQfvnVS8C/+y3ArwH/iGtfOvYjwL4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AQjBv//Ro49C+83L74T2GZT/hO/QU4F/4/OX0Xv5CYSw/vnUO2AA+yzUj4EAjW8P+/Ug4C7/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/gOh//7Pn29C+86/r8T3GJz/BPDSc5G/4/KT4Tv6DYig/vrYP9C/+yjcf0EAhn//OvQk1O7+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AADB///Ro49C+9DP3wT3GpwfRfDVa1F/4/WPAfz4CpDgvvrWM3BA+yjgr0EBin8/u/Qg+A7/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AQDR///So49C+9HPz0T3F6QPRfDSc1H/4vaXIfz4CYzwfvrUM6AA+yfkj0EBiotPu/Qi9P5+8g')]
    procedure TestControlToonsEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+B+eQwAR7MC/DA7ob/32mQtRQQEqkL/D/gVcPz/kHeyvQA7sz7A/AdCTL/kIFwjAwgVCDE9A5g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,7D+eAwQjnLCACvcrQHz+gldRgAQY4C9E/wB4T37gHpjvPxH79yA+/c+DT3gIEQzgwgUx+K4A6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6vcNhAEVyIDBCtgDED3+kDdBwAgpzA8E//1r/3/lDrjvgBH0t1C++7jD3/YIH8zvggMu/C6B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6v/9gwAdyPAADeMHYD3+hjMhQAgl8O/D/vhX3//mD/zvwBL854A9+q2nv/YHFdDvwgErLP8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlWaterTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AuiwPwDWo6/C+tDrj8T2GaQ/BfDUb+F+4/qPcbz4DZDgvvvXN9BA/Crsf4ECiqO/+/IiJO/+8w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,BPSAfwDVw39C+9Xbr0T2F6wvxfDSgyG/4fiXQbz5CYgwv/zUE0AB+yXgH0EDjIe/e/UmBA+/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,APiAv//Qs5/C+dTXjwT3E6zvRPDVawG/4vuTcTz4DZQgv/zVK/AA/CfkP0EBi4vfu/MhDP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,Bwixv//Xqz+C+trHz0T4F7D/hPDUcyF/4/mTgfz6CYQwP/zVJyCB/Cbgb8EBjIvv+/MlBA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,BPTAP//Uo4+C+tDfr8T3GKw/BfHTfxE/4/iHUXz5DIQgv/vWJ7AA+ynon0ECjY+/O/QjCP8+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AvSAfwDTs4/C+tPfb8T3FqwfxfDVdyH/4vqPYfz4DIAwP/vVM9AA+ybgr0ECiJNvO/QiEP8+8g')]
    procedure TestControlWaveEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/nuQAADcu3/C/dTnj0P6BegvhPbwB2EA6PkbIX34BqBQ/wDRgwCB+xCo3/8GnKdf/f0yBH//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+O9vAQDo408CAdbvH4L87i8Ag/sHnFCB7fqXYD73A5BQPwfbu6DA+vJ/v/0It8PffgI9AK8/7g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8lM/AgD3G/CBBNjzr4D9zn/wgQAWTCCC8/obUH/zA2CgPwzn18B/+tZ3XzsJ189fAAQ/+P4/7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7KcuQwAJaHCBCNzzHz/+qN8AQQUgGBBD+fiv37/tBiggQBDy36D++b6T7zgJ+dO/QQQ4/I5A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlWaveTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,3T8g//4BhI+B/cGzH8L+8xdwRfzS4yB+6+jbUHzyBnggfwHZbxDA/A2knz0OpKtvQAFOCG+/7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8bN/AfsB9H+BAtTjnwIB9jMwgvzzh2B/6vtXAH/wBvB/PwboWwC/9PcjMDwF+HdAAfxDWE+/7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7mdPwP8dWEDABN+/D0ABw7fQQf4JYLDA9vMvwP/wGSCQ/gf9kwC/+NP3X7sK78c/gAY15O5/9A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9su+Qv4YpFAACOuvH3/+vIMQPwUm4D+D++fPnz/rBdS/vw3169D+98fH7/gGAsAvgv42IG+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestControlWiggleTransitionEffect(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,w+/h+gfJJ09B89wroMP3ZpSOfPKoX7D/4tQT0T8hxNsefu/r22//AFGAj0ns2BvveAXqc479Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,30/S+vfNT56A96yDb4X1lKj+PfCWX5C95957UX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,1FtCOvfBbw4B9Z2Lf8X4eOD+vOyMQ6C+5eXvQL0lrnevfOQNXD/79l1IzsLv2D+/exD9683+Fw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,3UtiugDgf65/8MRLL4f0g6wuvu2UayC+5NVnkX8jvU8OvPDKl39++HcQzwTvzLcPvQzbV36+Fg')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,3Ldxvw3X135A7vDDLwjzeKCuPuydcwB/49AjcXz8C1RwfPn3E4/7Cn84nkbo4WO/PArf9z0+Ew')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,DiwS+vvmqx6+9PHb38T0d6yOPe+bB7B/4NiXEH0S1msgQQG4c59DC2jEfoPpxLeufQ0j/H08Bw')]
    procedure TestControlWrapEffect(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALeftStartProgress, ALeftControl1Progress, ALeftControl2Progress, ALeftEndProgress, ARightStartProgress, ARightControl1Progress, ARightControl2Progress, ARightEndProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.7,0.3,0.99,6ncC/AmfB59B58I/AAT6BOQ/gfrDG3E+4xu0MEAe1Q9QfeHld57/6x8wn0ABA/w9uBYdfO3+9Q')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.3,0.8,0.99,zSfRvRKWh9++5e4DgEXyHcQOAOywM8EA3wxI0AMu7QePAeLt0w9CBOfTbwb54he/dwcDcE39Cg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.6,0.99,vCvSuA/mqw8B9Nabr8TlKDDPfvDG0+A/5Rj4IEAW5puPfvPLdxA/+x2Ar4H+399+uRsiOD06/g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.0,0.2,0.99,s4tBOgP1L2/8+N73j0PhzVvPge4d+OFB0vxTMEAi53+vPewLXH8/+EKcb4MBqctfPQNAHG6/9w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.3,0.5,0.4,0.99,ohvi+A3BH+8A8rbf7wHg9gfvPv7o69B+5RD0kYAa89POvejMB38B9hP8L0H32+8evQgsfG1+BQ')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.0,0.2,0.99,so8xegP0L2+8+N33n0Phz1uvwe4d8PGB0vxbMEAi53+//esMZI8/+EOgf4MBqMM/PQNAEH4/+A')]
    procedure TestFilterAffineTransform(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AScaleProgress, ACenterProgress, ARotationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,4feQ/wHu++/D9OQDEATwSTTf+/vPa5A/5fJL8f0B9FePu/PWo4A++c/j7gD8DHhOeg4L4F0+AQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,3L+w/Qjvyw8B9PYPkAHwEWB/v+/h6zBB6/AzgL4N+k/A/Oz4O/9++fyX/8H+45OPfgUwiE6//w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,7zuSuwjcTx/A9fEn4EED/a+/wAD2n19A7/dLoP8fwP8Of+rc4w/A+ztYTwL70wdv+wcxRC78AQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,4ZuBe/jLt67/9syff0P5LVwPPvfP1w//8fp3QP4bxWPf/OUBfO979jy4v0Xz0nvf+gIDGB++Cg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.30,0.7,0.99,7jdCO/rYo26/9syD/8TzRBivPf3M259+7usPAX4gr48fPu4HhM/8+jvgXgXt8vs+PATfpz7/EA')]
    procedure TestFilterBandedSwirl(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ABandsProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,v0tQgATvq9+EAaAT0EL2LND//vrhA1AA/wBkUMEA4+e/PP/Qu2CB9eJfQH/8BHC/+wMRLH4+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,5n8/AQQL9O9AA9m/fwH92UtwP/n3WxDB9gIwwAAC8V/QQAfyC6C/++ebn3oD7qNPwAcgpI6/9Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,4c/OgQEIbPAACd7jrz79w6eQwP/+E0CC+f3zP8D3+uuffwnsZ+B9+dW3D/oGBcRfwQQo/P6/8Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,6l9uw/4UfPDACtrnn378pOuw/wUWBHDD+/tnv3/sBvAvAA//xyC9+cOnv3gFFtAPggQhAM+A7w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterBandedSwirlTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AStrengthProgress, AFrequencyProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,CeBQwADUw+8C++Hbj4T5F6gvBfPdY2F/5viPgTz3E6Qw//vbIxCB/CLsTwECjJevO/QcGD9/8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,DeyQgADc2y8C/O7jr4T9EqhPxfPpY2G/6/Vzkfz3G6gwP/zZF2DB/SL4v0ABk4svvPQYLB//9A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,CuQgQADc3x9C/OPTz4QBGaA/RfToO+G/8edLcTz3EqjwvvvbCyCA/CEIwIAAlX/vO/USRL++8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,FsgAgQHnE+CB+wH0jwQCDMjfxff8Z2H/8wA0AT33I7yAv/7b638BABwM4D8DnZ+//PYKYE/A9Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,FsQgwQHgE0CC/AXkTwQDFriPhff5L9F/9fM/kXz2JcDAPv7i50/B/xcAUIABm4NfvPQLQI+/9w')]
    procedure TestFilterBands(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABandIntensityProgress, ABandDensityProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,C1wgQv76gx8CAccrQEP99VtwAvnhDwEB5Q5AQX3yAqDg/v75y9A/+wswsD0HudsfvvkmBK+A9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,C5yfhPoSpE8CBNFfsAL+2tPgAf3k2+CB6QrMkD7wE0QQwAEDtJC/+/QDoPsI3SMgP/wtCD8A7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,BfweRP0JEBACCNYrcID/vNvwAAL5i/AC8QY40D7sEizg/wrqw8B++NrXPzsF3a/ff/0kBP//7A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9n/eg/4RYAABC+EDMP/+nhshwQUQTHBD+P/XT//oDhAQwA/zzxC++sCzv3gI+Ks/AQAnAF8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterBlindTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ANumberOfBlindsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,S3iQQ/rhQ5+AGhaE73v4zh9hu/fZfwBH+QPE3r0A6o+vQQIoxG5+/DNcPn7+JewuvQabM688Fg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,9k8+Qv0GoLDACsQH4Dj4eYfQAQEZHCCC7RSkvr0T4F9wPff5p8C69QzkLkH58GfPuRYXCC4CCg')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,xA8+wwcRnCCCCsg3oDv5fN/QgAMSJEDE8f4Tz374MxwPPvvwJ1H/9MSbHz0Nh/e+/QBRFG8/6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterBloodTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,3gshP/y/b09CAKqrH8b4M/BOBO7WJ0FB49F7AXn8BGhAfPLoE3D6+iskX0H1hLue+fMN6P29AA')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,6P9wf/3Fh2+C/re/z4X4MByfxO7aR7FA49uDMXr7BnAwvfXjIzB8+SVsX4H5gvtOevENbD6+/Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,8POwf/7Jm38C/cPTn4X4LkT/xO/eazGA4+SHQXv6CHgQvvjfN6D99yC0X0H9fzPvuu4M7H6++w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,9+fwf//Mr58C/M3jf0X4LWhfxfDjj7H/4+2LIbz5CXzgfvvcQ+C+9hv0X8EAe19/O+wKYK8++g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/eMQAADPv6/C+tXvT0X4KJCfxfHlq0G/4/aPwfz4C4SAf/3aSxCA9hgkQMEDe4vve+sMrN/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,ANhAgADQ068C+tv/T0X4KqjvxfLsy+G+4/yLYX34C4wQwP/aV7BA9RJcQAEGdadvPOgHFOC+9w')]
    procedure TestFilterBloom(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABloomIntensityProgress, ABaseIntensityProgress, ABloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,EGhQwfzi71+B/uK/P4P58AMghfbnL/F/6PPz0D3vEBwAvwDza/A/+A28nz4Io9tfvvUrTD//8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,C8xfAvvzH5AAAuu//0H812fgw/v1w3AA7/Br8L7qEuwf/wT3h6C/9/ibD3wKyffPAPw3NM//8A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,ADDPwvwGTFBABejPf0D+wcdgwgAFYFDB9fATwL/qDuBvfwr4qwC/99+Hv3kK7OsfggE5CJ8A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,9Ytew/4ViHBACePb7/7+owfxQAUZHKDC+vW3D4DpDPAvgA/3w1D++MaXz/cJCOCfQgQ0+B5B7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterBlurTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvzQPwDSq58C+9PXz4T3F6wfxfDVfzH/4viXUbz4DJAwP/vWM9BA+yfsj0ECipePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AvzAPwDVq5/C+tTbv4T3F6z/hPDUeyH/4vmPUfz4DZAgP/vXM9CA+yjof0ECjJeve/QiEP8+8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfywPwDWq5/C+tTfv4T3F6zvhPDTdxH/4vqLUfz4DZAgP/vYL9CA+yjof0ECjpfPu/QjEO8+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AACB///Yq59C+tXjr0T3GKy/hPDRa/H+4vuDYTz5DowQ//rZL+AA/Cnkb0ECkZf/e/UlEN9+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/AcxPwDfn39C+dbvn4T2GLA/hPDOU7H+4v1vkTz5DoAAv/rcKwAB/SjgP8EBmZd/PPcpEK/+8g')]
    procedure TestFilterBoxBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,1ley+vfBW15B9aCH/wX1kbQ+fvCVX3C9599/Ub0jr48v/OYWVC+6+GJYPwnr3wdfeQXWh/49GA')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,22+R/PnU104B+q2bT8T2WTy/vvWzT8A+7OQP4X0XwqcPPfARpO+6+D5sbwXx6jNfOwXoo57+Dg')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,3YuAvvvmUy/B/rqvn8L4I8APv/rSOxCA8OqfYL4K1ccfvvkM+H87+RuArwH39Vc//QT6u07/BQ')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,4qtvQP34zx+BA8bH74D67UNgv//wJ2AB9e8v4D7+598P/wIGRBC8+feT/z39AYAfvwQLzO4//Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,5sdOAv8LSPBACNPbL7/8tsfAvwQOELDC+fS/fz/y+fPv/wsAlLC8+dKnL/oCDKgPQQQc5I4A9A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterBrightTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.99,A/TAvwDVu48C+9XT/8T3F6wfxfDUg0H/4viXcTz5CmwA//vVP7AA/CvwTwECiI+Pu/QkBO9+8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.99,+LcQfwjmN0CB8uP/vwTx5/t/BPXly5H/3ABkIf35DTjwPv/VP0AA/SQw8H4CiFuvu/cp5F7+7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.99,6Bs+RAIfzOAAB+YfEH7+gEshgAks/P9D/vpTX7/sChSAPxDxu+B9+sCrr3YKD7w/QgQrBA8B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.99,7WPegwMczLBAA/Qv8H7+gEshgAks/P9D/vpTL//uDPy/Pw3pp1D++ti/33cJ/5t/QQMsFG/A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterCircleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress, AFuzzyAmountProgress, ACircleSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.2,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.3,0.99,APwQwP/Rp4/C+tXPLwX3mqiO//KzI3A98NQDogAUwV+/fe+z8+C/7flrgAbvettfPPf/25/B+w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.4,0.99,8guh//3Gm5+C+9LfnwX6hOyOP/S/W4A+6+a/8Tv+Bmywv/fI729A/D6Uz0H/hk//e/oxAH//8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.5,0.99,49+R/f+hI/8A/dOjP0T2GJxfQ+24p/H+2wUYkTsGCmhAgPXi776C/2PU7oP977eu+AL8m137Dg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.6,0.99,5tfhfQGgMw+B/NWfL4T3F6wPhfDUg0H/4viXofsCE4hQwPbm4y5DAGbU7wL8wZPvO/Qv2D4ABw')]
    procedure TestFilterColorKeyAlpha(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress, AToleranceProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,0R+xvvq5Uy8CApiTf8b4NMjuQ+3TB8GB4sdvgXf9AmQgu+7x94/4/TLQXoHwiHfeePcQSJ19Aw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1B+hvvq7Ux+CAZqPP4b4M9DuQ+3S/6DB4shrkbf9AWgg++7v8+94/jHcjkHxhofOePgTSL19AQ')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,7xtBf/3Ia1+C/renT8X3Hyjvw+zD//BA49mXwfn7B2zQfPLZGyA9/TcgzwH4lAfP+foluJ1+9g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,BPwAgADTq68C+9bbz8T3FLQPRfDSh0H/4vmjkTz4DpBwf/vTO0DB+inkbwEDjp+vO/QiEA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,AeRAgAHS28/C+dwLUAX4HvRvxvTyDwK+4g2I4X32DaDggALfUyDC9xWg8MAIfNsPvekMUJD+9Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestFilterContrast(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrightnessProgress, AContrastProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.9,0.99,jQ+RPxG5B1+E6dsnj8Tt+dvPR+HMCwGA1dnTcDruC/TffQEZfFC+Ft9vYD8EgI/v//lI6F//2A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.6,0.99,FyCTd/D1J9+3+V0Mr4D4ivxtvfa+q15/Bq8XAEExdQcvP/OBg58F8QkYYMXoQSgPAPQS+HCCBA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.5,0.99,66sS9w3Fd7/82/xLH0T0+a/AQv72w8E6+jmQUMQX5MuAwPYHhI5AE19w4Yf1GCTA/PG0k9DBIw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,1.0,0.99,3weyPAjC9x7D99e/T0PuIeAfQumoA6J+2QVQofwDyB9xfvgQZNA89rCPAAAOqFegvf01IEC99g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.9,0.99,PYiv/QH9Vz/8B6SD77/z7CvwxezKy7B+4eHnj8ICN7yvwgBPaDCD/zJMsMD/c+8PwPZPyN8/5w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.8,0.99,/+/AffhFrJ+7ASjgD4EEs5BeffvpAx78EvSf4H4E+bvuvwmoq6BBBqdXX/z8IKSfwQQO4G9E6w')]
    procedure TestFilterCrop(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ABottomRightProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,4N+wPQHho+9/Bd6/IIAAKRDQfPH6Q+A+9wHwIP32I8B/vwL4BwDAAiM8Tz0F87s/Pgs+eA5B9A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,5kOP//8JaOA/Dc3f3/8D0ItQffvvx+9C+cx////87VMwgQbGd+BABNDzb//+7/Ov/QAdUI++9w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,uAMvwQEMAEBABt5nMD/4ty9APwch/I9C+w2w377wLPBvQQUBZCA//9efvzoI/w8QgQAuIE8B8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,BYjuwf8liJC/Bfwr0Pz6sE8wPwIm3L+C+Od3D33pE8jP/g0A2CB9/8UDkLkJDwSgAQQsPF+B6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterCrumpleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,6o9gPgHv858C/cdPQAP5DMCfxe7Kf/H95fBb0bsAEQBBAP6wG5DC/jZoj4EGe3evfPgvJD9+9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,6z+RvQbbY19D+cYDcEP1Iahvg+3Ax9H+4vyLYT32FzTA/fnCV4AC/hv0378HieNP/vEdmA9/+A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,DeRRfwHgq37D8N/fP4XwP2SfP/G446C/3xFAIf7tHwygvPcRcDA/+yZw0H7/3K8vvvcOAI++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,HvwgAfrak5/B9O0XEAbzEIDPwvPLu3D/3BFAMbzvCwQgv/4bcIC//x4QgAH6vjs/fPlGsD5+8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,4D/Qfvv923AB/eDHD8X26OsfCPTmA8G85PdHobsMD7TwP/LIe18B/0C8f0T/mq/fOQM3xF6+7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,39OgPAYRjB+B+cmLcEP7JQAgg+vR+3B96+lvYT0H7yOBAAGtv09C/j303j8EggOvPgFFOJ898w')]
    procedure TestFilterDirectionalBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAngleProgress, ABlurAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,CdDQvwHDu/+C+dK3L0T6DrjPBPLUR2F/5vl/gfz5Cpww//rSE5BA+yLsr8ECjJuP+/QiDO++8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,+4cAQ/rzi6+CANTjv0P8BZBfQvjhn7B97wAk4X36HZyQ/vnpF+C/+hIkID8Krauvfv4agJ//8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,9rOORAkSXNBBAecPcAH/oqegwgASYNDB9e0voP7g+5cQwBDtx5B8+bif37kFFGyfgf42xC4B5g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,7OcdhAAfwNAADeDvb33+gEshgAks/P9D/vlTDwDnBxSwwBX850A9+bGvb3YKE9R/wgUt9C7B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterDissolveTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,9t+dQ/8llGBADe67j/z+eCvhvwcXqP8EAPdvPwDrHfS/AQ76d5D//rhfDzgKAvwPhAI15B7B5A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6vtNBAQTkFDBCub337z+e08hQAsu5P9DAP5Lj4DqDhQQAhL411C++rCTj7cIAtRfAQA4IH9B6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7PdtBAIXpCBBC+b3Xz3/gT8hgAku+O9D/vdLTwDoDgxQQRL75+B9+a6jD3cMDtxPggEuDD8B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6udNhAAcvOCADOH7r/3+gUchwAks/P9D/vlLD0DmDQTQwBL948D9+ayzr3YKFcyPQgMsCD/B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterDropTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,1jMOAgAAgFA/ENYDALz1jIMAAAAg1A+E9QAAbz3jIAAAAAj1A2E9AMBbzzgIAAAAQv1AWA8A8A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,1zcOwv//f1D/D9cDEHz1jYMAAAAg1P+D9QEEX33jHwAQwAf3/1A9AMFb37gHAgDwgf0/UA8A8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,2UMuAv/+d1A/D9sDMHz1kYPwv/8hzP/D9QMMb/3jHfxPwAf780B9AMJfD/kGBvzvQf4+SB/A7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,3VdOAv78a0B/DuADUDz1loPwf/8iwO9D9gYUb73kGvyPgAcA5CD9AMRnT/kFC/y/wf49OB+A7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,4WduAv37XzC/DeX/f/z0nIPg//4ktO/D9gkcb73lFvjfQAcH2AB9Acdvr3kEEvifwf88KB8A7w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,6Ht+gvv6TwA/DOr3n3z0pX/Qf/4moN+D9w8kr/3nEfRfAQcPxMB8Acp/TzoCHABQgQA6FB/A7g')]
    procedure TestFilterEmboss(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress, AWidthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/V/AAADh4z+C/tbbXwP5+QMQxPXmMzFA6PgjET31C3SAPwDdVyDA+g7gT/8DpqP/fPckCH+/8A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+L+fQQDwG+ABAtjf70H621MQg/r45yDB7fmvwH3xC1TQfwXke3C/+vbXHz0Fwq9/vvomBN9/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8yN/ggD/U4DBBdvnb8D7vKcggv8JmBBC8/k7gP7tCzggQArsn8A++t7P73oG37v///0pBE/A7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,74NOgwAOjDBBCd3r/z79nvcggQQaSADD+PnHTz/qChxwQA/zwxD++cXDr7gH/Md/AQErAL+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterFadeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,jku/QQK+kz5//LXPQIHj0m+QgO+kzx9/7TNQ4Lj0GyTgO+nzx1/7DBQ4Lv0GCfhO+vzx1z4DBQ')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,rft+QgPYLy8//9e/0H/rvp/QAPbLy8//9S/037rvJzSA/fLy83/9C/237vsJDWC/vPz8X/9C/w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,zacugwTyzw8/AvmvYH7zqcsggfzzw49A/iuY33zqMkgg//zwI5D/CuY3n7oMEsg/P/wI5L+C+Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,7FfuwwUMaOD+BBuc8Dz7lftwAQMauD/BBic8z37lPlzAgAbuT7DBCc+zX7kPFzCggfsTbHDC8w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,CwSexAYmCNH+Bz2QgPsCgSexgQlCtP9BDyTgvkDgSWxgghDtf9ADCbgvEHgSG5ggRPsf9EAC7g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,KrBdBQhAqLG+Cl+AELoKbFcBAhBqrK/CFyCErgLbVYAAhBrrq/AFCKGrwHYVIAChxvoqfAFC6A')]
    procedure TestFilterFill(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,yh8hPP3FFy/B88IfUIX2PlS/QPCgCzF/4vrfUbgLBZxAPeb9Nw96BS6wrgP9wpcuOPXDn06/Bw')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,1Q/x/P3HP2/B9McbUAX3OHjvwfGzS9E+4v/HcXkHBqQAPuz5Z587AyYcH4P/sdcuOfLQAz+/BA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,4APB/f7Ka8/B9c0XUEX3Mpw/A/PGi3H+4QSsgboCCKzQfvL1mx+9AB+If0ICoBcvOu/eZx+/AQ')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,6/egvv/Mkw/C9tMTUIX3LcRvhPTayxH+4QqYoXv+CbCQf/jxz68+/hj07wEFkFsvO+zsz/9+/g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,9udwfwDOv28C+NkPUAX4J+i/BfbtC8L94Q+Awfz5C7hQgP7tAzCA+xBgUMEHf5svfOn5N9B++w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR578C+d4LUEX4IQzwRvcBSGK94RRs4T31DcAgwQTpM8BB+QnQsIAKbtsvfeYHmLA++A')]
    procedure TestFilterFillRGB(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AColorProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECipd/O/QhDA//8Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AQBh//7gr48C+dvzz0T2F6QvRPHPN8E+4gBgcTz5D3zwfvrmJ7BA/Srob8EAoY9v/PcrAK9+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,6hvBvQL6m6+B8dMnUETwCqi/AfLEzyA+4gIs4f3/DvyffvjoJ5BAABfcf0ABzkO//QM4vH499g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,2yMBvQTzk1/B8NAb8APvC5DvgPLMx3C+4/0rgf4DCOCPvvbqE2DA/hPUXwAC3C9fvQY0nG49+A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,zzcRPAPoh48A8dILoIPvHmivf/XOp3C+5/4DMX8J/LtvPvXx96///hfELwEA4EOPPAcohO69/Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,1z9xfADbW09A9c7Xr8PyNkgvv/XJf5B+6fPbUL4O4asf/vP518+9/R18rwL53VMPPAYQiE6+BQ')]
    procedure TestFilterGaussianBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,AdhQAAHR479C+d4HUEX4JPCvBvb7J9J94g100T32DLDgQAPkP4DB9wuw0EAJcM//vOYHeMA++A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,ANhAwADR17/C+dz/T0X4JtBPhvTz+1H+4gWAkf32C6BwgAHgRyAB9w+A8IAHdLuvPOgJONA++A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,/9tAgADQz79C+tr7T0X4Kaz/BfPt18F+4/2LUf33C5QQwP/cS6AA9hJYEMEFd6dv/OgJBOB++A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,/98wQADQw6+C+tjzP0X4K5C/xfHnszH/4/aTEb34C4iwP/7ZTzBA9RYwIAEEeZMf/OkKzO+++A')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,/+MgAADQt68C+9brL0X4K3hfhfDgj5F/5PCb0Xz5CnxQf/zWT/B/9RsEQEECfofPO+wOgP/+9w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,/d8wwP/Ps6+C+9XnP4X4MVQvhe/ecwHA5OmfoTz6CnAAP/vUU0D/8xvoXwEBfmuPO+sKbO+++Q')]
    procedure TestFilterGloom(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGloomIntensityProgress, ABaseIntensityProgress, AGloomSaturationProgress, ABaseSaturationProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,BuDAf/3Rsy9C+dmrH4T2PcAfx/YJDBI+4wRMMf72BJhw/wbxV5A/9vurIEH9U6Nvu+wgxA9+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.99,COSw//3Rpz/C+devDwT1N8wfh/T/z8H+6Ae8sT38GeTgfgXsL8BA9RdY8ED7at+fOvEfkK++8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.99,COywP/7Rnz8C+tarDwT2HuAvBvPot9H+5Qe0Af35Gdgwv//hFzDB+x4U0AD/fLdv+vMjKK/+8A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.99,+gshQALSu++C+9EHoIX5F6QPhfHXq9E+4PZnUTz2+2PQ//rWO9BA+iM44EEHjKMP/e4YlL9+9Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.99,8euQgALOuz9D/dAbQEb7K3BPhfLo29H+3u47Ab3y9UOAgPrdM0C/9gyMEIEIgGd/feMAHKB9Ag')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,9sNwgP/Nx7+C+9j7nwX7MmhvBfPs6wH/3u9HIX3zAUgwgPvcUwC+9vqDAMEHaTsffN3wY5B9AA')]
    procedure TestFilterHueAdjust(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AHueProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,0Psg//u4j09CAKPL/4b5PijPBfT/z/F/4eNHEfn5A5QQffgE9G/5+xS0j8D4bLt+uun11069CQ')]
    procedure TestFilterInvert(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.3,0.1,0.7,0.99,9FcehP0LYFDBD9fLD37+q/MgQQILfBBE9vEH0D3vFyjgvwTtn4C++u/z//0EpocPvfko/C7/7g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.2,0.6,0.99,6e9NhAAbtPDADODvb/3/iGPhvwglGLAE//dzz/7pGPhfAAv403A9+s/rP7sGxo+//v0s+J4/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.3,0.3,0.5,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT73/mDvjPgBP952C9+a/Pr3cJ+58/wQIuCH/A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.6,0.4,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.4,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.6,0.3,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterLineTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AOriginProgress, ANormalProgress, AOffsetPropProgress, AFuzzyAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,EnyAPg32b/BA7uMPgEf5HohvxvLPQ9G+5PtDYb/1MMifAPmo199D+0j8n//+aVOPvPZJLL+98Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A6zQvh/h039F6MnH0EPq3KsQgvbDUzJ+2xMksr/0U+hAQPk0mK/+A0hMMH8Gu0OOOPUs6N5+7g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vdtwfOgNGOB9Ab/Hv3/0ILweuv++C0+F6u3zzf0RsCtggf6ab0CDDtx7bj754Es//gfhm64AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgUHr4Sdje++FXzP/z9i7038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestFilterMagnify(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,H4AAv/UbrGB97wtgb0X2E9Sfx/a1zxA84gLUsADsNNg+fv3RN6DB+D3wbnoMin8vgQNDvK97+Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,6udgug7x1/A67P2fkAPeoD+QffPo/9B/2yiEcQMGFZCPggcCdECEARwIT7gH9pdvQApfbG4B7A')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,uMuvPhbw3wC/+BdosMH3ejcRAf0klMDE7eGnn34EFCC/gPzjB9DB/0CYP7gL9Odefw0uTG2C7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,4hsfAgXkhxCA9ypAMED+gEshgAks/P9D/vpTP0AR1udvPwLZxyDB/Sh4IPsK6Z9/v/o1VJ8A6g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,0xPfhAsPjMA/9gSw8H/+gEshgAks/P9D/vpT74IeuEOvP/2M/2/D8g7ojzgPAJC/PwtDzI6B5Q')]
    procedure TestFilterMagnifyTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,false,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,true,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestFilterMaskToAlpha(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AGrayscale: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.99,/N8gwP/Pu69C+9brT0X4LniPRfHmn4E/5PGP0Tz5C4RQ//zaS5B/9RYMMEECenfPu+oImN/++Q')]
    procedure TestFilterMonochrome(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.99,9m8PQgD4N7ABBNnjLwH7y3+QAv0AvKCB8PlzIL7vC0jw/wfoixB/+urT/7sF0bM/P/wnBB+A7g')]
    procedure TestFilterNormalBlend(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMkf31A5SgQP/aUxCB9QpggMAGdq/PPOoMCBB/+g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/eMwQADPx6+C+tnzL8X3McwvhvP1w7F+5ASMof31A5SgQP/aVxBB9QlggMAGdq/PPOoMCBB/+g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,/uMwQADPz69C+trzH8X3MtxfBvT404F+5AeAwf31BpiwwP/bUzDB9QtwkEAHdK+/POoKHAC/+g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/uMwQADPz69C+trzH8X3MdRfxvP405F+5Ad8sT32BpSgwP/cUzAB9gt0kEAHdK+vPOoKHAC/+g')]
    procedure TestFilterPaperSketch(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,ANxAAAHQ569C+d0HUAX4IQzwRvcARHK94RNowX31DcAQQQTqN6DB+QrIwAAKb9MPveYIkLD+9w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,bUSARQke9JAA/HtE0IQi8iegRQ5NgGDBIe238P70R2CBQBDbd+9GCvbncL8Jue8PwPLer6BB8A')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,W2CgBAXzdxDCAlrYnwUh/f9vhBIeWMDCGNkb0T32JHhRfw/f72+FBPO7UAEJmae/Pezug0BA7Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,ToTQQwPbX7DCBFOw70QgAxSwRBQSXJCCFdg3kb31G2iRfw3kD/DEAvW3kEEJjKs//en4iwAA7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,ROCAAgLdd9CBBEFw74QbAxwQhhYQZNCBE+EnAb31HTCxPwvuF7AEAvq3oMEKhbc/fevvbzCA8A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AdhQAAHR569C+d8LUEX4IwSQhvj8M4K94hJo0Xz0FLCwAAT1M4AB/RGogIEKbZv/fOb3j+D+9g')]
    procedure TestFilterPencilStroke(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABrushSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.80,0.9,0.99,4VMiuf+4W35/7KdfT0bnJTh/gPiZLzF83RvoUf4MOySffO8nxD8EAgxwr8IF74tOeBv/I434/w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.3,0.2,0.70,0.6,0.99,kGeBufrZMw/B8tH7v0T4Jii/+vTRz7D/4tRLwf8W+f8/PPTug1+B/x74nr8E8qMtvhQsBD2+/g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.1,0.2,0.65,0.5,0.99,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.0,0.0,1.00,1.0,0.99,EQhhAP3St0/C+eDDf4X3F6zvhPHcS0G/4f2HQbz5BXTwfvvuH8C/+iIA0AH/nI9vu/Un5P5+7w')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.2,0.4,0.75,0.9,0.99,nefxeRDrl3/F76ofMIPvIGSPv/vB+9D84hrUMf0jt7Nve+jOv+7DBf47oD8F0jffuwA/1I08AQ')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.4,0.2,0.90,0.8,0.99,xh+y+AWwL89/6py3r8XkAggPgfi+U6G84jNUkEAaF3Aev+D3PyDB9TxEscUNtCcfOhI3NK/7DA')]
    procedure TestFilterPerspectiveTransform(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATopLeftProgress, ATopRightProgress, ABottomRightProgress, ABottomLeftProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,B4gA/wbho89B9MQb0ET3GaC/xfDSZ/H+4vp74bv1Fmzwu/j3/7/7+zzsv4H9f2eP/PUWfP9+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,A/zQfwDSq59C+9LXv8T3F6wvhfDVfzH/4viTQfz4DJAgP/vWM9BA+ybsj0ECiZePO/QhEA8/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,3duxO/zSXw4B+4tvv8LueOzuPfCSt2A+5OV3YbkI1YP/POtCAFA4/15MX4nm2f+e+gDWez4+Fw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,AvzgPwDSp48C+9PTz8T3JrBfhPbPW5E+4firUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,2WeiuvbHWz6B9KOP/8X7i9BOve2xG8B+7vCb0T0lpoevfOQRXD86+FRYX8nq4ANveQXWhw5+GA')]
    procedure TestFilterPinch(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AStrengthProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,1CcSu/jCa05B9qOPrwX1icQuvvCaU5D96ONrQX0isYsvvOYUZD/6+FpQn0js4guvOQXZjz6+Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,+s/gv/vpf48C+9EXEET2Ebj/hO7JawF/3+9zsXz1GbDgP/roR4AA+ioMsMAGjHOfPPko9E4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,BgSxP//Wn09C+9bP74T3FrAPxfDUg0H/4viXQfz4C5QQP/zUN+AA+yjsX4ECjZOve/QhAO/+8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,AfzQvwDTq49C+9PTz4T3F6wPxfDUgzE/4/ibYXz4DYQgv/vaN9BA+yXoX4ECiZufe/MhEA+/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AgDhfwDSo5/C+tTX78T3F6wPxfDUfzE/4/iXQbz4C5Qg//rVN9BA+ynsj0ECipOfO/QhDA8/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    procedure TestFilterPixelate(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp4/C+tPT34T3F6wvxfDVgzE/4/iXUbz4DJQwP/vWM9AA+yfsj0ECiZeP+/MhEA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,AvzQfwDSq48C+9TT34T3F6wfxfDVgzH/4viXUbz4C5Awf/vVM9BA+ybsf4ECiZePO/QiDA//8Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,AOjQfwHTvx+D+9Lnv4T3GazfxPDXdyG/4/uPUXz5CoAwP/rQJ/AA+ybcT0EBkZv/O/QgAP8+8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,699thAAawBBBDeP7jz3+f1MhgAkuBACE/vxP3z/mCgTQABX761D9+aq/P3YIGNAPwwMw/B7B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,6ucthAAdxODADODzf33+gEshgAks/A9E/vpT/7/mCgDAgBT652B9+a23f/YIGND/AgQu/C4B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODvf33+gEshgAks/A9E/vlT/7/mCgDAgBT752B9+a23f/YIGND/QgQu/C5B6w')]
    procedure TestFilterPixelateTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.3,0.99,AvzgPwDSp58C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.2,0.4,0.99,+QNRvwLUlx+D+tDj3wT3I6D/g/DRbzH/4vyfEXz7CahAP/rNJ/AA/Cb8z4EAmH9v+/Ui7B4/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.4,0.5,0.99,7u+gfgPZm08D+8v/r0T3JaB/w/DPYxG/4/2rwTsACNiQf/nBDxAB/SoAUAIAoGsfe/gpyB6/8Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.99,4s/w/QLkp18D/McrYAT4IKjPA/HQY+F+5P2zwTsGBwzxf/i581+B/TEEAIP/oWePOvszoC5/8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.8,0.7,0.99,2LsgvQLyt/+C/MNb4MP4Ery/hPHTa6F+5fy7EbwLBCQRwPa60+/B/ToM0MP+oG8f+v06gF6/8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,18NgvAL/w+/B+8B/gMP4/tvvhfHXZ0F+5vyzobwP+Q+hv/TCr3/C/kUcMMT+o4P/OQE/cG4/9Q')]
    procedure TestFilterRadialBlur(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ABlurAmountProgress, ACenterProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.99,9DvhvAXrry9A6tHjv0XwF6hPwfW8FwH/4/tzAf8EDUhwvvzhd7/A+jaoP77/uePO/fsD6H69+g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,0.4,0.99,Gpxx/Ang8w6/7bjf3wPvNUSPvvq6p9D+3u1fQb0B6Hdg++4SlE+++Sukb8MG4zef+wIcyC49/w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.6,0.45,0.5,0.99,8lsxvf/pi3/A7sobgEXzLpRPf/Spj3B/4PnrQL0CB1ggffbln29/BDh0j8P4x7tfO/j7c299CA')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.5,0.55,0.5,0.99,6Z+B/AfsD+//8c4X0EPyRkAvgvG2gyA/5u/DAD4BBoxAPvjed39AAi7AX0L7pJevffcvkF+9AA')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.8,0.7,0.50,0.7,0.99,9EdxvAX0R0/B7tMH0ITzSjRf/fa5e0B/7PQTAbwADMgAPffktx8+BECkTwP20Pce/P8QWO59Ag')]
    procedure TestFilterRipple(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, AAmplitudeProgress, AFrequencyProgress, APhaseProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,+W8wgP7mw8+B/tPPjwP6/+O/A/fgF1GA6fUvIX3yEYyQvwHdZzAA+xPor38CrI8/PfchEH//7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,8KfvAf7nNxCCAdcHEML31VugQ/r/5+DA7QaccL3vE0AgQAXgL4D/+f2/P70Dxmu/Pvsj1H4/7w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,AiCPggQEdMBBBuXf/z/9usvQQf8HnJDB8fM3cD7xBTDAPwj2u2B+/NrLX/oI3QMAwP0xKG/A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7Jfugv8UaOBACd7jD7/9o+OQwAUTMCCD+fXLf7/oDCxQQBDy2yB++cbL37gGAcifQQArHK/A6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterRippleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,AcygAPvsW8/A/t2r38L03OO/B/Ptr9A96f/HAD31CCCgvfzzwwA//QHM3/7+rdNvffcn0K6/7Q')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,/dufgf37V3AB/uTrT0P32vffwvT6d5AC6f1XoL7xDAhQQAYF0LBA/QlQH30Gutdf//4zBM8+7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,8Wt/AAUPQBDBBOL7f/7+0E8AQAAKfPAA9PYrEL7nGthvfgMOqAA++8+rb3oL5Kuvwf4wQJ9A7g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6NeOQ/4CTCCBCeTTfz/+qcNAwAIkKBAD+fSrb//oCAjAQA75xwB+98DnX3gJAPB/gQAq8E6A7Q')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterRotateCrumpleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,COxggALXy99C+tn3/wT4FdjPRfLh09F+4gKckT31DYRQwADWc/CB9htc8IAGkM+PvOwV1P/+8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,COBggALY28+C+dsD8IT3GPyPhvTwB0I+4gqEAX7zEZTQgAThg2AC9hHA0AAJhvPP/OgKVGD+8w')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,CNhggALW679C+d4L4MT3GhgQx/b+P8K94RFsAb7yErQQgQbtY0BC9wf4gAAKegPgPOYFlHC+9Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,CNRgQALW768C+d8L4MT3GyBAx/cBTJL94BNgEf7xErgwgQbuXyCC9wT8cEAKdwPQ/OQErIA+9g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterSaturateTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AADRv//Ro5+C+87P74T3GpwfxfDWe1H/4vWLQfz4CoQAP/vYN2AA+ybon0EBh4tvO/Qh/O4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,/AfBf/7Pl58C/cS/D8X3IIAfhfDZc3G/4u93MXz5BnCQvvrbO2C/+iXkvwH/gm9PO/Qh2K4+9Q')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,+Qux//3Ok5+C/b+3H8X3JHAPhfDaa4F/4uxvIfz5BGRQvvrdP/B++iTkz8H9gF8/O/QgxI4+9g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,9ROh//zNh58C/7WnT8X3KlQPhfDdY7F/4uVbEXz6AFDwffrhQwD++SPg74H7e0cfe/QgpE4++A')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,7B9xv/rJc5+CAaKHn8X3Nhj/hPDiSwGA4dkv4Xv7+ScQvfnpTyA8+SDYLwL3cQ/PevQfXL59/A')]
    procedure TestFilterSepia(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,/IOAvgvtj1AA6dcXwIbmwEOQg/rxE8I/1SCUsf38B3wPPwPJb4BA/DlAED4AfHffuvkv0L798A')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8Evfww0c3IBB5QPAf0T7sUORgvwp1HB/+O5b8P72K+jf/gDgZ6A/ANyfn/cL08cOPgREfA7A8Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterShapeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,AvjQPwDTq48C+9HTz4T3FqwfxfDVgzH/4viXYfz4DJAwP/vVM9AA+yfsn4ECiZeP+/MhFP8+8g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,APjAPwDSp4+C+8/P74T3FqQfxfDVgzG/4vibYTz5C5QQf/vUN7CA+ibkn4ECiJN/O/QfFP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,APTAPwDSp4+C+87L/4T3FqQfBfHVgzF/4vebcXz5C5QQf/vTN6BA+iXkr4ECiJN/O/QeGP/+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,//ew///Sp4/C+8vL/4T3FqAvBfHWhzF/4vefcbz5CpgAf/vSN4AA+iTgr8ECh49/O/QdHO8+8w')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,/vOg///Sp38C/MjDH4X3FpgvRfHWhzE/4vefcbz5C5jwfvvSN2DA+SPcvwEDh4t/e/QcJO9+8w')]
    procedure TestFilterSharpen(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,0VPA/gAgOKB+6KbbAAbr0SOfgwDoe2++5ipIjz3oEVyQxBMMfKCADiXMH/z87/twxPQeDPC//g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,kMe+/AHIk5AA+cuzzwDxEGDv+wMWiN9ADQ/I73zh8fOvQgWzn3C+Ec6/n3z+OHCABfjqM/FCAA')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,tWMvewDjX6A/7OfLwH7z1kvQwgAp1J9+BDKoXzvhMKQQgAQMPE889fivP3kHI0yQRAAtuHCB6w')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,0TvvvgsOGA+D97uHfzzqn6sQwvwiZFDC9RNkj33v2ScvgSTLj7BDA9nPn3cO+IdQhfhNsCAB7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,17dfAf4eEKG+7GeEIID6mItQvvkbhG8E9uZbn/7pBqCgABXeT0CEB8V/H/gHDyDgwgI3AGDA5w')]
    procedure TestFilterSlideTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ASlideAmountProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.40,0.5,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.50,0.6,0.99,IISQPgn8U3AA7+7z38f4HohPBvLNQwF/5PxDIb/yMcwfQPqq1+/D/Ebwr//+bFOvvPdEOL998g')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.60,0.4,0.99,ArSQ/h7q3x/F6dDboEPr4q9ggvbCR+I93BQgsr/1TPAgwPsnjJ8+BEgoYP4Gt2tuefUr3K4+7w')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.55,0.5,0.99,vRehPOkRCHC+/sHPz0D0ILweuv++C0+F6u3zHX0RtUtwwf6aQ+CCEOF3bj754Etf/gfgm54AFQ')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.75,0.5,0.99,sTvAQAu1098C8di3kYXwM8QPRu8DIH+7/TJgYHr4SuD+u+JWzP+z9y3038rt56ffveGbuzFACg')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.50,0.7,0.99,hTcQgPS+2+9A9QVscYDsxfvf+Puh14BF0uTTf/0ABeh+/t89SEC39fUjDrgOU8yOOxw8iDwAAA')]
    procedure TestFilterSmoothMagnify(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ARadiusProgress, AMagnificationProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.1,0.99,BQARwP/Tm48C/NPPz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.3,0.99,AmSPAAS+sw/CG337zr/3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.0,0.99,DLTehvTWt99EFqAzf7z3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.2,0.99,/RMxwP/Qk9+C/drz34T4xNdBwQwX/C//FR4AULz4DJAwP/vWM9AA+yfsb8ECiZ9/O/QjBA8/8g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.4,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXwXz5AMhgP/3cV1DA+hxYr/sLGmyAAwMhmG8D7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXYT3uzRcxwBbbK5BB+9V3X/oB+hfgAfgr9P4C8g')]
    procedure TestFilterSwipeTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AMousePointProgress, ACornerPointProgress, ADeepProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.40,0.5,0.99,DdRBwfq8u34D8eQPIEb8DUQvhvPAB+A+4g0wkDz6CLyg/vvap8CA+igksML+ksOwOf91gA5/9A')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.50,0.6,0.99,9e8xP/yux54D9dADIEb9JzDfBu/Q34+95PebQH38CchwPvzV4yAC+xbIb8IAkq8AuQNqcP4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.60,0.4,0.99,0EfSPf+pi+4D88njf8X+QgRPhPDa7w//5ujHYIAR92cQPvraW1DD/t7vvsIGmf/POwhb3O797A')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.45,0.5,0.99,xzeSuwSz096C+MnLL8P8PDivRfL129A+4/dbsUAZCRxgffLdey/C/s+f/sEIhJsPPQdZ9H6+7g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.55,0.5,0.99,ujdC+Aat+67B8sDL74H6MkCPxfDuF/F+4vhn4f4i7SdAP/X2Oy8C/f2LTr8FvV8vAQ5Q6E5/6Q')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,0.50,0.7,0.99,zFsSOAmWX64+5ZZLX4T4LWRPhe7WX9F+4/ePkfsg24/QvPkZvH6A+CRcznwKsiv/Qg8u5B4B7Q')]
    procedure TestFilterSwirl(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ACenterProgress, ASpiralProgress, AAspectRatioProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.1,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.2,0.99,/ZtwwP/Wk78C/dLvrwP8E9D/A/f712AA6PsD0f38AMDgvgLQq7BB+wlsH8AIl7t/vQFCAC//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.4,0.99,1oOwgP7VY/9C/tPvX8L/++vvAfryB0CB8eQv0MAD9su/PwDXiwCB+shnf/0F1PP/vQdP3C5A6g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.5,0.99,yJPvQP4E8P/B/trbf4D/zy+gQP3/828B9+nfL8D9BBzgfgjxZ1B/+8Krj/kNDdCPfw8oVB5B7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.6,0.99,4Y8eA/8NeNBAB9zzDz8CmhPxvwQdBDCD/fyDbwDwBhBQwA3ym9B++L+v3zcLE5QvwQMpyF5A7g')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterSwirlTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, AStrengthProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.00,0.05,0.99,1TvS+vnIgz5B9adzj0X1jrR+PvKgT1D96dxjUb0jrZ+PPOgTVH8691hU70jr5yMP+gTVn849Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.2,0.10,0.15,0.99,7XsQfvnhf89/Bsrrb4H7I5yv/vfdAyDB8vyTsP4H5J8vfvb6b3B8/hKwT0L67Id/vf8SIL9/CA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.4,0.15,0.00,0.99,23fwPfzeg9/AAsjDT8H2IIw/f/ffP1AA8vF7AH4G5MNvvvgPMEA8/heAz8H585svfgQGEI9/CA')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.6,0.05,0.10,0.99,319wfvzie0/AAMvT78D3GaAvv/jeFyCA8vQ/AL4F6MNPfvcEEKB8/Q58j0H68ZPvfQECJF//Aw')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.8,0.20,0.20,0.99,27uwvfvbU58AAMLPr8H2MHQP//bUL9C/8PN/gP0L2rePPfUH/D88/BlkL8P28GdPfQL8Dx+/Bw')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.8,1.0,0.10,0.00,0.99,5D+g/v3li78AAcvXz0D5D6yPf/nhLzDA8vY7cD4D7N+f/vgMGJB8/g6c30D7+Kc/fgMFJJ8/BA')]
    procedure TestFilterTiler(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; AVerticalTileCountProgress, AHorizontalTileCountProgress, AHorizontalOffsetProgress, AVerticalOffsetProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.99,+BNx//7No3/C+8PHX0X3H3zvxO/SV7F/4+2T4Xr7AIgA/vjbGyA/+yq8/4D+jVe/evUitN5/9Q')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.99,ABCB//7Pk1/C+8m3r0T2HIjPhO7NO+E/5PCngTv6CngQfvnVS8C/+y3ArwH/iGtfOvYjwL4+8w')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.99,AQjBv//Ro49C+83L74T2GZT/hO/QU4F/4/OX0Xv5CYSw/vnUO2AA+yzUj4EAjW8P+/Ug4C7/8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.4,0.99,/gOh//7Pn29C+86/r8T3GJz/BPDSc5G/4/KT4Tv6DYig/vrYP9C/+yjcf0EAhn//OvQk1O7+8g')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.6,0.99,AADB///Ro49C+9DP3wT3GpwfRfDVa1F/4/WPAfz4CpDgvvrWM3BA+yjgr0EBin8/u/Qg+A7/8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.99,AQDR///So49C+9HPz0T3F6QPRfDSc1H/4vaXIfz4CYzwfvrUM6AA+yfkj0EBiotPu/Qi9P5+8g')]
    procedure TestFilterToons(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ALevelsProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AxTeQwEhoPD/CwXcf3z7dA+B/wMIhH/EAAOUH7/pGhBAAQzdx7CA/8tfP3kNFlggQwdIKH9B4g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,+B+eQwAR7MC/DA7ob/32mQtRQQEqkL/D/gVcPz/kHeyvQA7sz7A/AdCTL/kIFwjAwgVCDE9A5g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,7D+eAwQjnLCACvcrQHz+gldRgAQY4C9E/wB4T37gHpjvPxH79yA+/c+DT3gIEQzgwgUx+K4A6Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,6vcNhAEVyIDBCtgDED3+kDdBwAgpzA8E//1r/3/lDrjvgBH0t1C++7jD3/YIH8zvggMu/C6B6g')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,6v/9gwAdyPAADeMHYD3+hjMhQAgl8O/D/vhX3//mD/zvwBL854A9+q2nv/YHFdDvwgErLP8A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterWaterTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.0,0.5,0.99,AuiwPwDWo6/C+tDrj8T2GaQ/BfDUb+F+4/qPcbz4DZDgvvvXN9BA/Crsf4ECiqO/+/IiJO/+8w')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.1,0.4,0.99,BPSAfwDVw39C+9Xbr0T2F6wvxfDSgyG/4fiXQbz5CYgwv/zUE0AB+yXgH0EDjIe/e/UmBA+/8Q')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.3,0.5,0.99,APiAv//Qs5/C+dTXjwT3E6zvRPDVawG/4vuTcTz4DZQgv/zVK/AA/CfkP0EBi4vfu/MhDP++8g')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.5,0.6,0.99,Bwixv//Xqz+C+trHz0T4F7D/hPDUcyF/4/mTgfz6CYQwP/zVJyCB/Cbgb8EBjIvv+/MlBA9/8Q')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.7,0.7,0.99,BPTAP//Uo4+C+tDfr8T3GKw/BfHTfxE/4/iHUXz5DIQgv/vWJ7AA+ynon0ECjY+/O/QjCP8+8g')]
    [TestCase('6', '3d-shapes.svg,200,200,1,1.0,0.8,0.99,AvSAfwDTs4/C+tPfb8T3FqwfxfDVdyH/4vqPYfz4DIAwP/vVM9AA+ybgr0ECiJNvO/QiEP8+8g')]
    procedure TestFilterWave(const ASvgFileName: string; AControlWidth, AControlHeight, AScale: Integer; ATimeProgress, AWaveSizeProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.99,/nuQAADcu3/C/dTnj0P6BegvhPbwB2EA6PkbIX34BqBQ/wDRgwCB+xCo3/8GnKdf/f0yBH//7g')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.99,+O9vAQDo408CAdbvH4L87i8Ag/sHnFCB7fqXYD73A5BQPwfbu6DA+vJ/v/0It8PffgI9AK8/7g')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.99,8lM/AgD3G/CBBNjzr4D9zn/wgQAWTCCC8/obUH/zA2CgPwzn18B/+tZ3XzsJ189fAAQ/+P4/7Q')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.99,7KcuQwAJaHCBCNzzHz/+qN8AQQUgGBBD+fiv37/tBiggQBDy36D++b6T7zgJ+dO/QQQ4/I5A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterWaveTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,horse.webp,200,200,1,0.0,0.3,0.99,AvzgPwDSp48C+9PTz4T3F6wfxfDVgzH/4viXUbz4DJAwP/vWM9AA+yfsj0ECiZd/O/QhDA8/8g')]
    [TestCase('2', '3d-shapes.svg,horse.webp,200,200,1,0.2,0.4,0.99,3T8g//4BhI+B/cGzH8L+8xdwRfzS4yB+6+jbUHzyBnggfwHZbxDA/A2knz0OpKtvQAFOCG+/7w')]
    [TestCase('3', '3d-shapes.svg,horse.webp,200,200,1,0.4,0.5,0.99,8bN/AfsB9H+BAtTjnwIB9jMwgvzzh2B/6vtXAH/wBvB/PwboWwC/9PcjMDwF+HdAAfxDWE+/7Q')]
    [TestCase('4', '3d-shapes.svg,horse.webp,200,200,1,0.6,0.6,0.99,7mdPwP8dWEDABN+/D0ABw7fQQf4JYLDA9vMvwP/wGSCQ/gf9kwC/+NP3X7sK78c/gAY15O5/9A')]
    [TestCase('5', '3d-shapes.svg,horse.webp,200,200,1,0.8,0.7,0.99,9su+Qv4YpFAACOuvH3/+vIMQPwUm4D+D++fPnz/rBdS/vw3169D+98fH7/gGAsAvgv42IG+A7A')]
    [TestCase('6', '3d-shapes.svg,horse.webp,200,200,1,1.0,0.8,0.99,6uMtxAAdxODADODzf33+gEshgAks/P9D/vpT/7/mCgDAgBT752B9+a27f/YIGND/QgQt/C5B6w')]
    procedure TestFilterWiggleTransition(const ASvgFileName, ATargetImageFileName: string; AControlWidth, AControlHeight, AScale: Integer; AProgress, ARandomSeedProgress: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,200,200,1,0.3,0.0,0.3,0.40,0.5,1.0,0.3,0.1,0.99,w+/h+gfJJ09B89wroMP3ZpSOfPKoX7D/4tQT0T8hxNsefu/r22//AFGAj0ns2BvveAXqc479Fg')]
    [TestCase('2', '3d-shapes.svg,200,200,1,0.4,0.3,0.7,0.50,0.6,0.3,0.4,0.2,0.99,30/S+vfNT56A96yDb4X1lKj+PfCWX5C95957UX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    [TestCase('3', '3d-shapes.svg,200,200,1,0.5,0.5,0.4,0.60,1.0,0.4,0.3,1.0,0.99,1FtCOvfBbw4B9Z2Lf8X4eOD+vOyMQ6C+5eXvQL0lrnevfOQNXD/79l1IzsLv2D+/exD9683+Fw')]
    [TestCase('4', '3d-shapes.svg,200,200,1,0.6,0.2,0.1,0.55,0.9,0.2,0.2,0.9,0.99,3UtiugDgf65/8MRLL4f0g6wuvu2UayC+5NVnkX8jvU8OvPDKl39++HcQzwTvzLcPvQzbV36+Fg')]
    [TestCase('5', '3d-shapes.svg,200,200,1,0.0,0.2,0.2,0.00,1.0,0.2,0.2,1.0,0.99,3Ldxvw3X135A7vDDLwjzeKCuPuydcwB/49AjcXz8C1RwfPn3E4/7Cn84nkbo4WO/PArf9z0+Ew')]
    [TestCase('6', '3d-shapes.svg,200,200,1,0.0,0.4,0.4,0.00,1.0,0.4,0.4,1.0,0.99,DiwS+vvmqx6+9PHb38T0d6yOPe+bB7B/4NiXEH0S1msgQQG4c59DC2jEfoPpxLeufQ0j/H08Bw')]
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
  LFilter: T;
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
  LEffect: T;
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
  LEffect: T;
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
  //TDUnitX.RegisterTestFixture(TFMXEffectsTests);
{$ELSE}
implementation
{$ENDIF}
end.
