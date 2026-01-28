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
unit Skia.Tests.Vcl.Bitmap;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  DUnitX.TestFramework,
  Vcl.Graphics,

  { Skia }
  System.Skia,
  Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkVclBitmapTests }

  [TestFixture]
  TSkVclBitmapTests = class(TTestBase)
  private
    function CreateImage(const AImageFileName: string; APreferableWidth, APreferableHeight: Integer): ISkImage;
    procedure DrawChessBackground(ABitmap: TBitmap; ASquareSize: Single; AEvenSquareColor, AOddSquareColor: TAlphaColor);
    procedure LoadImageFromFile(ABitmap: TBitmap; const AImageFileName: string);
  public
    [TestCase('1', '50,50,0.98,ZtuZZmaZ22Zn25lnZ5vf/nf73Xd33///f/vff3/////jx+PH//8eeB54///jx+PH48ceeB54Hng')]
    [TestCase('2', '50,40,0.98,aWmWaWmWaWlra5Zpa/7v/W9v3m1v/u/9f+/+73/+7/3nOec5Occ4xjjGOcfnOec55zk4xjjGOMY')]
    procedure TestBitmapToSkImage(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '50,50,0.98,ZtuZZmaZ22Zn25lnZ5vf/nf73Xd33///f/vff3/////jx+PH//8eeB54///jx+PH48ceeB54Hng')]
    procedure TestBitmapToSkImage2(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestCreateBitmapFromInvalidSkImage;
    [TestCase('1', '3d-shapes.svg,300,300,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    procedure TestCreateBitmapFromSkImage(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,0.98,DwsPD/H//59/f39v8///339/f2/3////f/9///f///8A7wD/APcA9V//UZu/j/+f/53/mP+Z//8')]
    procedure TestCreateBitmapFromSkImage2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestEmptyBitmapToSkImage;
    [TestCase('1', '3d-shapes.svg,300,300,1,false,false,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWODYQNpw37D/8/9LfxviP2c/7353fgg')]
    [TestCase('2', '3d-shapes.svg,300,300,1.5,false,false,0.98,ABg8JGTk/P0zOPzkZ+f8/XO8/vd39///c7z+93f3//8AAwADAAcABmAGcA74HHAO+A/QP/hven8')]
    [TestCase('3', '3d-shapes.svg,300,300,1,true,false,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWODYQNpw37D/8/9LfxviP2c/7353fgg')]
    [TestCase('4', '3d-shapes.svg,300,300,1,true,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('5', '3d-shapes.svg,300,300,1,false,true,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    [TestCase('6', '3d-shapes.svg,300,300,1.5,true,true,0.98,//7+/nw8PDD//v7/f39+fP/+/v9/f/98/////////374Hvnd+9373fvdm9uL24vbq9CoEogYjfg')]
    procedure TestSkiaDraw(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; ASkipBeginScene, AStartClean: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,0.98,DwsPD/H//59/f39v8///339/f2/3////f/9///f///8A7wD/APcA9V//UZu/j/+f/53/mP+Z//8')]
    [TestCase('2', '3d-shapes.svg,300,300,1.5,0.98,AwMDAwMD//9/f3NjQ0f//39/c2NDR///f39zb0NP//8ADwAPAA8ADwAPAA8ADwAPr/+vH4t9j/g')]
    [TestCase('3', '3d-shapes.svg,200,300,1,0.98,Dw8PD/P//59/f39v8///339/f2/3////f/9///f///8A7wD/APcA9V//UZu/n/+f/53/mP+5+/8')]
    procedure TestSkiaDraw2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,Dw8OBsLy+/9/f35nw/f//39/f2f7////////7///////gP+A/4D/wD/AAOADQA/AAgAAAABAAAA')]
    procedure TestSkiaDraw3(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,false,0.98,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    [TestCase('2', 'horse.webp,true,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestSkiaDraw4(const AImageFileName: string; AStartClean: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,AAAAAAAAAAB/fHBhQ0dOTH98cGFDR05Mf3xwYUNHTkwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')]
    procedure TestSkiaDraw5(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,300,300,0.98,/85AAMDS8vL//nBgw9f+/v//d3P73/////////////8fgAeAB4AHwD/AAOAzQC/IC4APgABAAAA')]
    procedure TestSkiaDraw6(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '80,80,1,0.98,8PDw8AAAAAD//PDxQ0dOTP/8+PlDR05M//z/+f9H/0z/AP8A/wD/AACAAAAAAAAAAAAAAAAAAAA')]
    [TestCase('2', '100,60,1.5,0.98,/Pz8/Pz8AAD//Pz9//9OTP/+/v///05M//7///////z/8P/w//D/8P/w//D/8P/wAAgAAAAAAAA')]
    procedure TestSkiaDraw7(ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,Dw4ODn7++vJ/fn5vf//+/n9/f29/////////7///////8P/w//D/8B/wD/AP8A/4AoAAAAAAAAA')]
    procedure TestSkiaDraw8(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestSkiaDrawEmptyBitmap;
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.Math,
  System.IOUtils;

{ TSkVclBitmapTests }

function TSkVclBitmapTests.CreateImage(const AImageFileName: string; APreferableWidth,
  APreferableHeight: Integer): ISkImage;
var
  LSurface: ISkSurface;
  LSvgBrush: TSkSvgBrush;
begin
  if AImageFileName.EndsWith('.svg') then
  begin
    LSvgBrush := TSkSvgBrush.Create;
    try
      LSvgBrush.Source := TFile.ReadAllText(SvgAssetsPath + AImageFileName);
      LSvgBrush.WrapMode := TSkSvgWrapMode.Stretch;
      LSurface := TSkSurface.MakeRaster(APreferableWidth, APreferableHeight);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      LSvgBrush.Render(LSurface.Canvas, RectF(0, 0, APreferableWidth, APreferableHeight), 1);
      Result := LSurface.MakeImageSnapshot;
    finally
      LSvgBrush.Free;
    end;
  end
  else
    Result := TSkImage.MakeFromEncodedFile(ImageAssetsPath + AImageFileName);
end;

procedure TSkVclBitmapTests.DrawChessBackground(ABitmap: TBitmap; ASquareSize: Single; AEvenSquareColor,
  AOddSquareColor: TAlphaColor);
begin
  ABitmap.SkiaDraw(
    procedure(const ACanvas: ISkCanvas)
    var
      LPaint: ISkPaint;
      X, Y: Integer;
    begin
      ACanvas.Clear(AEvenSquareColor);
      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.Color := AOddSquareColor;
      for X := 0 to Ceil(ABitmap.Width / ASquareSize) do
        for Y := 0 to Ceil(ABitmap.Height / ASquareSize) do
          if Odd(X + Y) then
            ACanvas.DrawRect(
              RectF(X * ASquareSize, Y * ASquareSize, (X + 1) * ASquareSize, (Y + 1) * ASquareSize), LPaint);
    end);
end;

procedure TSkVclBitmapTests.LoadImageFromFile(ABitmap: TBitmap; const AImageFileName: string);
var
  LPicture: TPicture;
begin
  LPicture := TPicture.Create;
  try
    LPicture.LoadFromFile(ImageAssetsPath + AImageFileName);
    ABitmap.Assign(LPicture.Graphic);
  finally
    LPicture.Free;
  end;
end;

procedure TSkVclBitmapTests.TestBitmapToSkImage(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: ISkImage;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    DrawChessBackground(LBitmap, Min(ABitmapWidth, ABitmapHeight) / 5, TAlphaColors.Black, TAlphaColors.White);
    LImage := LBitmap.ToSkImage;
  finally
    LBitmap.Free;
  end;
  Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
end;

procedure TSkVclBitmapTests.TestBitmapToSkImage2(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
  LImage: ISkImage;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    DrawChessBackground(LBitmap, Min(ABitmapWidth, ABitmapHeight) / 5, TAlphaColors.Black, TAlphaColors.White);
    LImage := LBitmap.ToSkImage;
    LBitmap.Canvas.FloodFill(0, 0, clBlack, fsSurface);
    Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestCreateBitmapFromInvalidSkImage;
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.CreateFromSkImage(nil);
  try
    Assert.IsTrue(LBitmap.Empty);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestCreateBitmapFromSkImage(const AImageFileName: string; ABitmapWidth,
  ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.CreateFromSkImage(CreateImage(AImageFileName, ABitmapWidth, ABitmapHeight));
  try
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestCreateBitmapFromSkImage2(const AImageFileName: string; ABitmapWidth,
  ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.CreateFromSkImage(CreateImage(AImageFileName, ABitmapWidth, ABitmapHeight));
  try
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Color := TAlphaColors.Red;
        ACanvas.DrawRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), LPaint);
      end, False);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestEmptyBitmapToSkImage;
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    Assert.IsNull(LBitmap.ToSkImage);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
  ABitmapScale: Single; ASkipBeginScene, AStartClean: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    DrawChessBackground(LBitmap, Min(ABitmapWidth, ABitmapHeight) / 15, TAlphaColors.Black, TAlphaColors.White);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
        ACanvas.Scale(ABitmapScale, ABitmapScale);
        ACanvas.DrawImageRect(CreateImage(AImageFileName, LBitmap.Width, LBitmap.Height),
          RectF(0, 0, LBitmap.Width, LBitmap.Height), TSkSamplingOptions.High);
      end, AStartClean);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
  ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
        ACanvas.Scale(ABitmapScale, ABitmapScale);
        ACanvas.DrawImageRect(CreateImage(AImageFileName, LBitmap.Width, LBitmap.Height),
          RectF(0, 0, LBitmap.Width, LBitmap.Height), TSkSamplingOptions.High);
      end);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
      begin
        ACanvas.Scale(ABitmapScale, ABitmapScale);
        LPaint := TSkPaint.Create;
        LPaint.Color := TAlphaColors.Red;
        ACanvas.DrawRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), LPaint);
      end, False);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw3(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LoadImageFromFile(LBitmap, AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Color := TAlphaColors.Red;
        ACanvas.DrawRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), LPaint);
      end, False);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw4(const AImageFileName: string; AStartClean: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LoadImageFromFile(LBitmap, AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end, AStartClean);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw5(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LoadImageFromFile(LBitmap, AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw6(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end);
    LoadImageFromFile(LBitmap, AImageFileName);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw7(ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end);
    // If we want to use Vcl's TCanvas after a Skia draw, we should remove the alpha channel
    LBitmap.PixelFormat := pf24bit;
    LBitmap.Canvas.Brush.Style := bsSolid;
    LBitmap.Canvas.Brush.Color := clRed;
    LBitmap.Canvas.FillRect(RectF(0, 0, ABitmapScale * LBitmap.Width / 2, ABitmapScale * LBitmap.Height / 2).Round);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDraw8(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LoadImageFromFile(LBitmap, AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
        ACanvas.ClipRect(RectF(LBitmap.Width * 0.25, LBitmap.Height * 0.25, LBitmap.Width * 0.75, LBitmap.Height * 0.75));
        try
          ACanvas.Clear(TAlphaColors.Null);
        finally
          ACanvas.Restore;
        end;
      end, False);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Color := TAlphaColors.Red;
        ACanvas.DrawRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), LPaint);
      end, False);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestSkiaDrawEmptyBitmap;
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    Assert.WillNotRaise(
      procedure
      begin
        LBitmap.SkiaDraw(
          procedure(const ACanvas: ISkCanvas)
          var
            LPaint: ISkPaint;
          begin
            LPaint := TSkPaint.Create;
            LPaint.Color := TAlphaColors.Red;
            ACanvas.DrawRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), LPaint);
          end, False);
      end);
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkVclBitmapTests);
end.
