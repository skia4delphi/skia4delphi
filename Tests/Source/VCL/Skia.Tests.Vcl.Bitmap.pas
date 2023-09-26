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
unit Skia.Tests.Vcl.Bitmap;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkBitmapTests }

  [TestFixture]
  TSkBitmapTests = class(TTestBase)
  public
    [TestCase('1', '100,100,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestBasicSkiaDrawAndToSkImage(const AWidth, AHeight: Integer; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  Vcl.Graphics;

{ TSkBitmapTests }

procedure TSkBitmapTests.TestBasicSkiaDrawAndToSkImage(const AWidth, AHeight: Integer;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImageResult: ISkImage;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(AWidth, AHeight);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
        ACanvas.Clear(TAlphaColors.Red);
      end);
    LImageResult := LBitmap.ToSkImage;
  finally
    LBitmap.Free;
  end;
  Assert.AreSimilar(AExpectedImageHash, LImageResult);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkBitmapTests);
end.
