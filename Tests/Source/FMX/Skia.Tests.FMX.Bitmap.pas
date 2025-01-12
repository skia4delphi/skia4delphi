{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2025 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.Bitmap;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.UITypes,
  FMX.Graphics,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,
  FMX.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkFMXBitmapTests }

  [TestFixture]
  TSkFMXBitmapTests = class(TTestBase)
  private
    function CreateImage(const AImageFileName: string; APreferableWidth, APreferableHeight: Integer): ISkImage;
    procedure DrawChessBackground(ABitmap: TBitmap; ASquareSize: Single; AEvenSquareColor, AOddSquareColor: TAlphaColor);
  public
    [TestCase('1', '50,50,0.98,ZtuZZmaZ22Zn25lnZ5vf/nf73Xd33///f/vff3/////jx+PH//8eeB54///jx+PH48ceeB54Hng')]
    [TestCase('2', '50,40,0.98,aWmWaWmWaWlra5Zpa/7v/W9v3m1v/u/9f+/+73/+7/3nOec5Occ4xjjGOcfnOec55zk4xjjGOMY')]
    procedure TestBitmapToSkImage(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '50,50,0.98,ZtuZZmaZ22Zn25lnZ5vf/nf73Xd33///f/vff3/////jx+PH//8eeB54///jx+PH48ceeB54Hng')]
    procedure TestBitmapToSkImage2(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,0.98,+/vxcXDAgp7///Fxc8fO3v//8/l3997///////f////vr++nr7ePcV0HUYO/j/ef5p3bmNuZw/8')]
    procedure TestCreateBitmapFromSkImage(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,0.98,DwsPD/H//59/f39v8///339/f2/3////f/9///f///8A7wD/APcA9V//UZu/j/+f/53/mP+Z//8')]
    procedure TestCreateBitmapFromSkImage2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,false,false,0.98,AHBQ0fO/HwUzcNDR87/fzXd62d33v9/9d3rZ/ff/3/0AWODYQNpw37D/8/9LfxviP2c/7353fgg')]
    [TestCase('2', '3d-shapes.svg,300,300,1.5,false,false,0.98,ARg8LPTs/P0dHDzt9+/8/V+9vf/3//79X729//f//v8AAwADAAcADhAGeA54HPgO2A/wP/Bv+nc')]
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
    [TestCase('1', '80,80,1,0.98,Dw8PD/////9/f39v/////39/f2//////f/9///////8A/wD/AP8A//////////////////////8')]
    [TestCase('2', '100,60,1.5,0.98,AwMDAwMD//9/f3NjQ0f//39/c2NDR///f39zb0NP//8ADwAPAA8ADwAPAA8ADwAP//////////8')]
    procedure TestSkiaDraw7(ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,Dw4ODn7++vJ/fn5vf//+/n9/f29/////////7///////8P/w//D/8B/wD/AP8A/4AoAAAAAAAAA')]
    procedure TestSkiaDraw8(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkFMXBitmapTests }

function TSkFMXBitmapTests.CreateImage(const AImageFileName: string; APreferableWidth,
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

procedure TSkFMXBitmapTests.DrawChessBackground(ABitmap: TBitmap; ASquareSize: Single; AEvenSquareColor,
  AOddSquareColor: TAlphaColor);
var
  X, Y: Integer;
begin
  if ABitmap.Canvas.BeginScene then
  try
    ABitmap.Canvas.Clear(AEvenSquareColor);
    ABitmap.Canvas.Fill.Kind := TBrushKind.Solid;
    ABitmap.Canvas.Fill.Color := AOddSquareColor;
    for X := 0 to Ceil(ABitmap.Canvas.Width / ASquareSize) do
      for Y := 0 to Ceil(ABitmap.Canvas.Height / ASquareSize) do
        if Odd(X + Y) then
          ABitmap.Canvas.FillRect(TRectF.Create(PointF(X, Y) * ASquareSize, ASquareSize, ASquareSize), 0, 0, [], 1);
  finally
    ABitmap.Canvas.EndScene;
  end;
end;

procedure TSkFMXBitmapTests.TestBitmapToSkImage(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double;
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

procedure TSkFMXBitmapTests.TestBitmapToSkImage2(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double;
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
    LBitmap.Clear(TAlphaColors.Null);
    Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestCreateBitmapFromSkImage(const AImageFileName: string; ABitmapWidth,
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

procedure TSkFMXBitmapTests.TestCreateBitmapFromSkImage2(const AImageFileName: string; ABitmapWidth,
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

procedure TSkFMXBitmapTests.TestSkiaDraw(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
  ABitmapScale: Single; ASkipBeginScene, AStartClean: Boolean; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.BitmapScale := ABitmapScale;
    DrawChessBackground(LBitmap, Min(ABitmapWidth, ABitmapHeight) / 15, TAlphaColors.Black, TAlphaColors.White);
    if ASkipBeginScene or LBitmap.Canvas.BeginScene then
      try
        LBitmap.SkiaDraw(
          procedure(const ACanvas: ISkCanvas)
          begin
            ACanvas.DrawImageRect(CreateImage(AImageFileName, LBitmap.Width, LBitmap.Height),
              RectF(0, 0, LBitmap.Width, LBitmap.Height), TSkSamplingOptions.High);
          end, AStartClean);
      finally
        if not ASkipBeginScene then
          LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestSkiaDraw2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
  ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.BitmapScale := ABitmapScale;
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.SkiaDraw(
        procedure(const ACanvas: ISkCanvas)
        begin
          ACanvas.DrawImageRect(CreateImage(AImageFileName, LBitmap.Width, LBitmap.Height),
            RectF(0, 0, LBitmap.Width, LBitmap.Height), TSkSamplingOptions.High);
        end);
    finally
      LBitmap.Canvas.EndScene;
    end;
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

procedure TSkFMXBitmapTests.TestSkiaDraw3(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
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

procedure TSkFMXBitmapTests.TestSkiaDraw4(const AImageFileName: string; AStartClean: Boolean;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end, AStartClean);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestSkiaDraw5(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestSkiaDraw6(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer;
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
    LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestSkiaDraw7(ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single;
  const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.BitmapScale := ABitmapScale;
    LBitmap.SetSize(ABitmapWidth, ABitmapHeight);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      begin
      end);
    if LBitmap.Canvas.BeginScene then
      try
        LBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
        LBitmap.Canvas.Fill.Color := TAlphaColors.Red;
        LBitmap.Canvas.FillRect(RectF(0, 0, LBitmap.Width / 2, LBitmap.Height / 2), 0, 0, [], 1);
      finally
        LBitmap.Canvas.EndScene;
      end;
    Assert.AreSimilar(AExpectedImageHash, LBitmap.ToSkImage, AMinSimilarity);
  finally
    LBitmap.Free;
  end;
end;

procedure TSkFMXBitmapTests.TestSkiaDraw8(const AImageFileName: string; const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromFile(ImageAssetsPath + AImageFileName);
    LBitmap.ClearRect(RectF(LBitmap.Width * 0.25, LBitmap.Height * 0.25, LBitmap.Width * 0.75, LBitmap.Height * 0.75),
      TAlphaColors.Null);
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

initialization
  TDUnitX.RegisterTestFixture(TSkFMXBitmapTests);
end.
