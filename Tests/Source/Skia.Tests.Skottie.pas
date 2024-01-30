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
unit Skia.Tests.Skottie;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkSkottieTests }

  [TestFixture]
  TSkSkottieTests = class(TTestBase)
  protected
    procedure Render(const ASkottie: ISkottieAnimation; const ACanvas: ISkCanvas; const ADest: TRectF);
  public
    [TestCase('check.json', 'check.json,5.1.1,1.67999994754791,25,150,150')]
    [TestCase('rocket.json', 'rocket.json,5.7.6,5,30,600,600')]
    procedure TestLoadLottie(const ALottieFileName, AVersion: string; const ADuration, AFPS, AWidth, AHeight: Double);
    [TestCase('check.json', 'check.json,300,250,0.6,58GZva29w////fn97//P////+/3v/9/////////////n5+/zz/Pb+9373nvv9+fn88/8P/////8')]
    [TestCase('rocket.json', 'rocket.json,300,250,0.6,///Bw4OH//////Hjw8f/////9/vf3/////////////////8H8A/wD+A/4P/A/93///////////8')]
    procedure TestRenderLottie(const ALottieFileName: string; const AWidth, AHeight: Integer; const AFrameTime: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkSkottieTests }

procedure TSkSkottieTests.Render(const ASkottie: ISkottieAnimation;
  const ACanvas: ISkCanvas; const ADest: TRectF);
var
  LLottieRect: TRectF;
begin
  if ADest.IsEmpty then
    Exit;
  LLottieRect := TRectF.Create(PointF(0, 0), ASkottie.Size).FitInto(ADest);
  if LLottieRect.IsEmpty then
    Exit;
  if SameValue(ADest.Width / LLottieRect.Width, ADest.Height / LLottieRect.Height, TEpsilon.Matrix) then
    ASkottie.Render(ACanvas, ADest)
  else
  begin
    ACanvas.Save;
    try
      ACanvas.Scale(ADest.Width / LLottieRect.Width, ADest.Height / LLottieRect.Height);
      ACanvas.Translate((LLottieRect.Width - ADest.Width) / 2, (LLottieRect.Height - ADest.Height) / 2);
      ASkottie.Render(ACanvas, ADest);
    finally
      ACanvas.Restore;
    end;
  end;
end;

procedure TSkSkottieTests.TestLoadLottie(const ALottieFileName,
  AVersion: string; const ADuration, AFPS, AWidth, AHeight: Double);
var
  LSkottie: ISkottieAnimation;
begin
  LSkottie := TSkottieAnimation.MakeFromFile(AssetsPath + ALottieFileName);
  Assert.IsNotNull(LSkottie, 'Invalid ISkottieAnimation (nil)');
  Assert.AreEqual(AVersion, LSkottie.Version);
  Assert.AreSameValue(ADuration, LSkottie.Duration, TEpsilon.Matrix, 'in duration');
  Assert.AreSameValue(AFPS, LSkottie.FPS, TEpsilon.Matrix, 'in fps');
  Assert.AreSameValue(AWidth, LSkottie.Size.Width, TEpsilon.Matrix, 'in size.width');
  Assert.AreSameValue(AHeight, LSkottie.Size.Height, TEpsilon.Matrix, 'in size.height');
end;

procedure TSkSkottieTests.TestRenderLottie(const ALottieFileName: string;
  const AWidth, AHeight: Integer; const AFrameTime: Double;
  const AExpectedImageHash: string);
var
  LSkottie: ISkottieAnimation;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LSkottie := TSkottieAnimation.MakeFromFile(AssetsPath + ALottieFileName);
  Assert.IsNotNull(LSkottie, 'Invalid ISkottieAnimation (nil)');
  LSkottie.SeekFrameTime(AFrameTime);
  Render(LSkottie, LSurface.Canvas, RectF(0, 0, AWidth, AHeight));
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkSkottieTests);
end.
