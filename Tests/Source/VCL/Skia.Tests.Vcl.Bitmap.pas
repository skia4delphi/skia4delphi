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
  Winapi.Windows,
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
    [TestCase('1', '50,50,0.98,6XvtB/Tqq7ACCvF/wbz9WfcwAPpFuMAD4RP8fX7iCbSPvwYE+DA59axvb7X39uvfBwZyfJ5+5w')]
    [TestCase('2', '50,40,0.98,8p/9xQDk3zBDCLGj8LntoddP/gYC/I7FAvWzX3/m+befQA8GGBE4+ZXvzvcQBACw//g0OOCA/A')]
    procedure TestBitmapToSkImage(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '50,50,0.98,6XvtB/Tqq7ACCvF/wbz9WfcwAPpFuMAD4RP8fX7iCbSPvwYE+DA59axvb7X39uvfBwZyfJ5+5w')]
    procedure TestBitmapToSkImage2(ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestCreateBitmapFromInvalidSkImage;
    [TestCase('1', '3d-shapes.svg,300,300,0.98,A/zgPwDSq48C+9PTv8T3F6wfxfDVg0H/4viXUTz4DJAwf/vWN+AA+ybwj0ECiptvu/MhEA//8Q')]
    procedure TestCreateBitmapFromSkImage(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,0.98,AlRvfurxX1CAHUMvHzf4DZSfRPDfe4GA3OxX8bz7Cmhgf/zSD6AA/SHwj0ECiptvu/MiDP++8Q')]
    procedure TestCreateBitmapFromSkImage2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestEmptyBitmapToSkImage;
    [TestCase('1', '3d-shapes.svg,300,300,1,false,false,0.98,5RNuAQMNwFD+C+0DcPr1b6sgvwAM7I/D9A0UX/7hIASwAAnoA7EA/8lXb3gMEWzgwgNYcF9B5Q')]
    [TestCase('2', '3d-shapes.svg,300,300,1.5,false,false,0.98,6BMegQAHXCA+EOUD8HryjbvA/wE/PIBD+TEgX77oNVyAQQ77s8D+BK5rnjgSCBzQA/oSJKBC7Q')]
    [TestCase('3', '3d-shapes.svg,300,300,1,true,false,0.98,5RNuAQMNwFD+C+0DcPr1b6sgvwAM7I/D9A0UX/7hIASwAAnoA7EA/8lXb3gMEWzgwgNYcF9B5Q')]
    [TestCase('4', '3d-shapes.svg,300,300,1,true,true,0.98,A/jgfwDSq48C+9PTv8T3F6wfxfDVg0H/4viXUTz4DJAwv/vWN+AA+ybwj0ECi5t/u/MhEA//8Q')]
    [TestCase('5', '3d-shapes.svg,300,300,1,false,true,0.98,A/jgfwDSq48C+9PTv8T3F6wfxfDVg0H/4viXUTz4DJAwv/vWN+AA+ybwj0ECi5t/u/MhEA//8Q')]
    [TestCase('6', '3d-shapes.svg,300,300,1.5,true,true,0.98,/aMyugDXJ0494/r7LgXjbsgfuv7B1y89AOijEQAFEjRw/goPYL86DSu4rsP8D1hPw/y5468DFA')]
    procedure TestSkiaDraw(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; ASkipBeginScene, AStartClean: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '3d-shapes.svg,300,300,1,0.98,AlRvfurxX1CAHUMvHzf4DZSfRPDfe4GA3OxX8bz7C2hgf/zSD6AA/SHwj0ECiptvu/MiDP++8Q')]
    [TestCase('2', '3d-shapes.svg,300,300,1.5,0.98,8DuvfukDlBDAG01D73bv2bMf/AS4q2CEyFnDjj/2HEQPPwoERFH6CbCHTjz6EyQOgAAS7O5/+Q')]
    [TestCase('3', '3d-shapes.svg,200,300,1,0.98,AVRvfurxX1CAHUMvHzf4DpCPRPDfe4GA3O1X8Tz8Cmhgf/zRD6AA/SLwn0ECipt/u/MiDP++8Q')]
    procedure TestSkiaDraw2(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,+huPPuoAgEBAHU9DP7f+fkvhfwk04C9E/PU7bwDoDPjPABT019C9+qu3f/YIGND/AgQu/B4B6w')]
    procedure TestSkiaDraw3(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,false,0.98,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    [TestCase('2', 'horse.webp,true,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestSkiaDraw4(const AImageFileName: string; AStartClean: Boolean; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,1VeyOvjAV15B9aCDD0b1lawOPvCVV1A96OCDUX0lq4MPfOUVVA86+GBUX8nq4ANfeQXVgw4+GA')]
    procedure TestSkiaDraw5(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,300,300,0.98,6uMtxAAdxODADODzf33+gEshgAks/P9D/vlT/7/mCgDAgBT752B9+a27f/YIGND/AgQt/C5B6w')]
    procedure TestSkiaDraw6(const AImageFileName: string; ABitmapWidth, ABitmapHeight: Integer; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '80,80,1,0.98,8TNfPukDSDAAHUw7z7bgraPffe7e96/F0tlnzzjs8od/PPP6TzH1+eMDzrr64ANfPQBVLA0++A')]
    [TestCase('2', '100,60,1.5,0.98,8DuvfukDlBDAG01D7zbrzZt/+/YaTGBGzb4j7j7uDIw+/v4OzJH49ed33vv6zasO/gNhWG289g')]
    procedure TestSkiaDraw7(ABitmapWidth, ABitmapHeight: Integer; ABitmapScale: Single; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'horse.webp,0.98,/EsfP+r7Q+D/HVMPrzcR+w8wQvz+d67CB9Urb7vo3/swvAJJrF919sNvr4D766/wBfrIt3BDBQ')]
    procedure TestSkiaDraw8(const AImageFileName: string; const AMinSimilarity: Double; const AExpectedImageHash: string);
    [Test]
    procedure TestSkiaDrawEmptyBitmap;
    [Test]
    procedure TestTopDownBitmapSkiaDraw;
    [Test]
    procedure TestTopDownBitmapToSkImage;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.Math,
  System.IOUtils;

const
  // Opaque red, blue and green as a 32-bit BGRA DIB stores them
  DibRed = Cardinal($FFFF0000);
  DibBlue = Cardinal($FF0000FF);
  DibGreen = Cardinal($FF00FF00);
  TopDownWidth = 16;
  TopDownHeight = 8;

// A 32-bit top-down DIB (biHeight < 0), top half red and bottom half blue,
// loaded from a top-down BMP stream (LoadFromStream keeps the header's sign).
function MakeTopDownBitmap: TBitmap;
var
  LFileHeader: TBitmapFileHeader;
  LInfoHeader: TBitmapInfoHeader;
  LPixel: Cardinal;
  LStream: TMemoryStream;
  X: Integer;
  Y: Integer;
begin
  FillChar(LInfoHeader, SizeOf(LInfoHeader), 0);
  LInfoHeader.biSize := SizeOf(LInfoHeader);
  LInfoHeader.biWidth := TopDownWidth;
  LInfoHeader.biHeight := -TopDownHeight;
  LInfoHeader.biPlanes := 1;
  LInfoHeader.biBitCount := 32;
  LInfoHeader.biCompression := BI_RGB;
  FillChar(LFileHeader, SizeOf(LFileHeader), 0);
  LFileHeader.bfType := $4D42;
  LFileHeader.bfOffBits := SizeOf(LFileHeader) + SizeOf(LInfoHeader);
  LFileHeader.bfSize := LFileHeader.bfOffBits + Cardinal(TopDownWidth * TopDownHeight * 4);
  LStream := TMemoryStream.Create;
  try
    LStream.WriteBuffer(LFileHeader, SizeOf(LFileHeader));
    LStream.WriteBuffer(LInfoHeader, SizeOf(LInfoHeader));
    for Y := 0 to TopDownHeight - 1 do
      for X := 0 to TopDownWidth - 1 do
      begin
        if Y < TopDownHeight div 2 then
          LPixel := DibRed
        else
          LPixel := DibBlue;
        LStream.WriteBuffer(LPixel, SizeOf(LPixel));
      end;
    LStream.Position := 0;
    Result := TBitmap.Create;
    try
      Result.LoadFromStream(LStream);
    except
      Result.Free;
      raise;
    end;
  finally
    LStream.Free;
  end;
end;

// Fails the test if the bitmap is not a top-down 32-bit DIB, so it cannot
// pass on a bottom-up copy
procedure AssertTopDown(const ABitmap: TBitmap);
begin
  Assert.IsTrue(ABitmap.PixelFormat = TPixelFormat.pf32bit, 'The bitmap should be 32-bit');
  Assert.IsTrue(NativeInt(ABitmap.ScanLine[1]) > NativeInt(ABitmap.ScanLine[0]), 'The bitmap should be a top-down DIB');
end;

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

procedure TSkVclBitmapTests.TestTopDownBitmapSkiaDraw;
var
  LBitmap: TBitmap;
begin
  LBitmap := MakeTopDownBitmap;
  try
    // Already premultiplied (every alpha is 255), so SkiaDraw keeps this DIB
    LBitmap.AlphaFormat := TAlphaFormat.afPremultiplied;
    AssertTopDown(LBitmap);
    LBitmap.SkiaDraw(
      procedure(const ACanvas: ISkCanvas)
      var
        LPaint: ISkPaint;
      begin
        LPaint := TSkPaint.Create;
        LPaint.Color := TAlphaColors.Lime;
        ACanvas.DrawRect(RectF(0, 0, TopDownWidth, 1), LPaint);
      end, False);
    AssertTopDown(LBitmap);
    Assert.AreEqual(IntToHex(DibGreen, 8), IntToHex(PCardinal(LBitmap.ScanLine[0])^, 8), 'The top row should have been drawn green');
    Assert.AreEqual(IntToHex(DibRed, 8), IntToHex(PCardinal(LBitmap.ScanLine[1])^, 8), 'The second row should still be red');
    Assert.AreEqual(IntToHex(DibBlue, 8), IntToHex(PCardinal(LBitmap.ScanLine[TopDownHeight - 1])^, 8), 'The bottom row should still be blue');
  finally
    LBitmap.Free;
  end;
end;

procedure TSkVclBitmapTests.TestTopDownBitmapToSkImage;
var
  LBitmap: TBitmap;
  LImage: ISkImage;
  LPixmap: ISkPixmap;
begin
  LBitmap := MakeTopDownBitmap;
  try
    AssertTopDown(LBitmap);
    LImage := LBitmap.ToSkImage;
    Assert.IsNotNull(LImage);
    LPixmap := LImage.PeekPixels;
    Assert.IsNotNull(LPixmap);
    Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[0, 0], 0, 'The top-left pixel should be red');
    Assert.AreSameColor(TAlphaColors.Red, LPixmap.Colors[TopDownWidth - 1, TopDownHeight div 2 - 1], 0, 'The last red row');
    Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[0, TopDownHeight div 2], 0, 'The first blue row');
    Assert.AreSameColor(TAlphaColors.Blue, LPixmap.Colors[TopDownWidth - 1, TopDownHeight - 1], 0, 'The bottom-right pixel should be blue');
  finally
    LBitmap.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkVclBitmapTests);
end.
