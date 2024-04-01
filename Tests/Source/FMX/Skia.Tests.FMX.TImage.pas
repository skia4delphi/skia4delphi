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
    [TestCase('1.IconDoc',     'icons.png,46,46,8,8,30,30,1,0,0,0,-180,0.99,/4GBgYGBgf///fHhw8fN/////+/f3+//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('2.IconArchive', 'icons.png,46,46,8,8,30,30,1,0,-30,0,-150,0.99,/8OBgYGBw/////Hhw8fP////++vr18//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('3.IconImage',   'icons.png,46,46,8,8,30,30,1,0,-60,0,-120,0.99,/4GBgZGZgf///fHh09/N////9/fX3+//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('4.IconAudio',   'icons.png,46,46,8,8,30,30,1,0,-90,0,-90,0.99,/4GBiYGBgf///fHpw8fN////9+/X18//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('5.IconVideo',   'icons.png,46,46,8,8,30,30,1,0,-120,0,-60,0.99,/4GBkZGBgf///fHx08fN////8//f9+//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('6.IconBook',    'icons.png,46,46,8,8,30,30,1,0,-150,0,-30,0.99,/4GBvZmBgf///fH928fN////9//f5+//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
    [TestCase('7.IconFile',    'icons.png,46,46,8,8,30,30,1,0,-180,0,0,0.99,/4GBkZmBgf///fHx28fN////+//fz+//////////7//gB+AH4AfgB+AH4AfgB+AH4Af///////8')]
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
