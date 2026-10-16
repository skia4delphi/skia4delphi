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
unit Skia.Tests.FMX.TImage;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TImageTests }

  [TestFixture]
  TImageTests = class(TTestBase)
  public
    {$IF CompilerVersion >= 31}
    [TestCase('1.IconDoc',     'icons.png,46,46,8,8,30,30,1,0,0,0,-180,0.99,uSsSex62s6//4hEIwEPjyGtAQPftM+I/40rg4EAbH6z/fez/X88F/A2kz78R0yuONxRSaK07+g')]
    [TestCase('2.IconArchive', 'icons.png,46,46,8,8,30,30,1,0,-30,0,-150,0.99,zy/Sehy9LxCA5AyUL8Pi4jfAwfXvZ4K/4jjQsAEgJ2CfPu7/19/F+BaoT4EN2W+ONxZM4G27+g')]
    [TestCase('3.IconImage',   'icons.png,46,46,8,8,30,30,1,0,-60,0,-120,0.99,tgsCOx67x6//4w0YEEThxHvwv/fzRyIA4kvYEEIbIZivPu7zh8/G+QqkHwEPywNeeBNXgA17+A')]
    [TestCase('4.IconAudio',   'icons.png,46,46,8,8,30,30,1,0,-90,0,-90,0.99,sRfy+Ri6w+9+5+oPQMTkvz9QfvzqH8LA1y14AAIdG6Av/esGPB8E/AK4vz8OxQ+edxFc2Ew68w')]
    [TestCase('5.IconVideo',   'icons.png,46,46,8,8,30,30,1,0,-120,0,-60,0.99,xRPCeRq1C1DA5+Wzz8Pl2/Of//zyYzJA1BpIgEMaNUy/vu0C7F/F+Az4/wAO02c+txdjVG358g')]
    [TestCase('6.IconBook',    'icons.png,46,46,8,8,30,30,1,0,-150,0,-30,0.99,2xuy+h3GT4DB5+eHP0Pf2/9vwf8d1DI+2DZEsMUbQ/xOgfL6R5DG8xA8EIIP1OsuOBll1E158w')]
    [TestCase('7.IconFile',    'icons.png,46,46,8,8,30,30,1,0,-180,0,0,0.99,uEcS+x6tp28/4hXwr0Pky2tQgPbqM9J/4kjgcAEcHbBfPuz1c28G+gqYn4ANyRMuuBJfmP06+w')]
    procedure BitmapMarginsTest(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; AControlLeft, AControlTop, AControlWidth, AControlHeight, AScale, ALeft, ATop, ARight, ABottom: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    {$ENDIF}
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.UITypes,
  FMX.Graphics,
  FMX.Objects;

{ TImageTests }

{$IF CompilerVersion >= 31}
procedure TImageTests.BitmapMarginsTest(const AImageFileName: string;
  ABitmapWidth, ABitmapHeight: Integer; AControlLeft, AControlTop,
  AControlWidth, AControlHeight, AScale, ALeft, ATop, ARight, ABottom: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: TImage;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.BitmapScale := AScale;
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);

      LImage := TImage.Create(nil);
      try
        LImage.BeginUpdate;
        try
          LImage.SetBounds(AControlLeft, AControlTop, AControlWidth, AControlHeight);
          LImage.Bitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
          LImage.BitmapMargins.Rect := RectF(ALeft, ATop, ARight, ABottom);
          LImage.ClipChildren := True;
          LImage.MarginWrapMode := TImageWrapMode.Tile;
          LImage.WrapMode := TImageWrapMode.Place;
        finally
          LImage.EndUpdate;
        end;
        LImage.PaintTo(LBitmap.Canvas, LImage.BoundsRect);
      finally
        LImage.Free;
      end;
    finally
      LBitmap.Canvas.EndScene;
    end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TImageTests);
end.
