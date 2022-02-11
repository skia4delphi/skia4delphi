{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.ColorFilter;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkColorFilterTests }

  [TestFixture]
  TSkColorFilterTests = class(TTestBase)
  protected
    function AssetsPath: string; override;
  public
    {$IFDEF ANDROID32}
    [TestCase('Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,//4wMDg8GAD//nBwe39eTP//d3N7f3/v////93//f+8HAAeBAoEHCX/Bf/H88f/9B/8fwADAAAA')]
    [TestCase('Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,//4AAGD8eDj//nBgY/9+fP//d2N7/3////934/////8+wAgAAAAdGHJQ/nz8/Px8X9//yP++/44')]
    [TestCase('Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,//4AAAB8eDj//nBgQ39+fP//d2N7f3////////t/f/8AAAAAAAAACHgA9Bj7iPusH48/gA+89AA')]
    [TestCase('Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,//4wMDh8GAD//nBwe39eTP//d3N7f3/v////93//f+8HAAeBAoFHCX/BX/H88f/5B/4fwADAAAA')]
    {$ELSE}
    [TestCase('Normal', 'horse.webp,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,/89AQEDw8PD//3BgQ/f+/P//d3N7//////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('Sepia', 'horse.webp,0.393,0.769,0.189,0,0,0.349,0.686,0.168,0,0,0.272,0.534,0.131,0,0,0,0,0,1,0,/89AQMLy8vv//3Bgw/f+////d3P7//////////v///8IwAgAAAAIGHgQcBjzCPuMH4w/gB+89QA')]
    [TestCase('Polaroid', 'horse.webp,1.438,-0.062,-0.062,0,0,-0.122,1.378,-0.122,0,0,-0.016,-0.016,1.483,0,0,0,0,0,1,0,/89AQMDy8vv//3Bgw/f+////d3P7//////93+/v///+40CAwHwF6zvgdHxT3Bvn/B5gd4MAg/EA')]
    [TestCase('Protanomaly', 'horse.webp,0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0,0,0,0,1,0,/89AQMDy8PD//3Bgw/f+/P//d3P7//////////////8XgAeAB4AHwD/gAOAzQC/IA4ANgABAAAA')]
    {$ENDIF}
    procedure TestColorMatrix(const AImageFileName: string; const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkColorFilterTests }

function TSkColorFilterTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'ColorFilter');
end;

procedure TSkColorFilterTests.TestColorMatrix(const AImageFileName: string;
  const M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34,
  M35, M41, M42, M43, M44, M45: Single; const AExpectedImageHash: string);
var
  LImage: ISkImage;
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LImage := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
  LSurface := TSkSurface.MakeRaster(LImage.Width, LImage.Height);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.ColorFilter := TSkColorFilter.MakeMatrix(
    TSkColorMatrix.Create(M11, M12, M13, M14, M15, M21, M22, M23, M24, M25, M31, M32, M33, M34, M35, M41, M42, M43, M44, M45));
  LSurface.Canvas.DrawImage(LImage, 0, 0, TSkSamplingOptions.Low, LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, 0.99, Format(' w:%d h:%d', [LImage.Width, LImage.Height]));
end;

initialization
  TDUnitX.RegisterTestFixture(TSkColorFilterTests);
end.
