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
unit Skia.Tests.MaskFilter;

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
  { TSkMaskFilterTests }

  [TestFixture]
  TSkMaskFilterTests = class(TTestBase)
  public
    [TestCase('', '140,140,20,0.99,/4GBgYGBgf///fHhwcHB///98eHh4eH//////////////////D/8P/w//D////////////////8')]
    procedure TestMaskFilterBlur(const ABitmapWidth, ABitmapHeight: Integer; const ASigma, AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types;

{ TSkMaskFilterTests }

procedure TSkMaskFilterTests.TestMaskFilterBlur(const ABitmapWidth,
  ABitmapHeight: Integer; const ASigma, AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LPaint: ISkPaint;
  LSurface: ISkSurface;
begin
  LSurface := TSkSurface.MakeRaster(ABitmapWidth, ABitmapHeight);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.MaskFilter := TSkMaskFilter.MakeBlur(TSkBlurSTyle.Normal, ASigma, False);
  LSurface.Canvas.DrawRect(RectF(0, 0, ABitmapWidth, ABitmapHeight), LPaint);
  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkMaskFilterTests);
end.
