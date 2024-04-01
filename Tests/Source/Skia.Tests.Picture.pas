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
unit Skia.Tests.Picture;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkPictureTests }

  [TestFixture]
  TSkPictureTests = class(TTestBase)
  public
    [TestCase('1', '0.99,AXl5eXkBA/8BeXl7fw8P/0V9fX9/Tw//RX99f39PD/8ADwAPAA8ADwAPAA8ADwAP//////////8')]
    procedure TestBasicSquare(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,AXl9fX05A/8BeX1/fz8P/0V/fX9/f2//RX99f39/f/8P/w//D/8P/w//D/8P/w////////////8')]
    procedure TestCullRect(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,Pz/Pz/Pz//9/f+/P8/f//3//7+/7/////////////////////P/4////////z//P//////////8')]
    procedure TestDraw(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', '0.99,AXl5eXkBA/8BeXl7fw8P/0V9fX9/Tw//RX99f39PD/8ADwAPAA8ADwAPAA8ADwAP//////////8')]
    procedure TestImageFromPicture(const AMinSimilarity: Double; const AExpectedImageHash: string);
    [TestCase('1', 'simple-picture.skp')]
    procedure TestSerialize(const AExpectedSerializedOutputFileName: string);
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils;

{ TSkPictureTests }

procedure TSkPictureTests.TestBasicSquare(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
  LRecorder: ISkPictureRecorder;
  LPictureCanvas: ISkCanvas;
  LPicture: ISkPicture;
  LPictureCopy: ISkPicture;
begin
  LSurface := TSkSurface.MakeRaster(256, 256);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LRecorder := TSkPictureRecorder.Create;
  LPictureCanvas := LRecorder.BeginRecording(256, 256);
  LPaint := TSkPaint.Create;
  LPictureCanvas.DrawRect(RectF(0, 0, 200, 200), LPaint);
  LPaint.Color := TAlphaColors.White;
  LPictureCanvas.DrawRect(RectF(20, 20, 180, 180), LPaint);
  LPicture := LRecorder.FinishRecording;
  LPictureCopy := TSkPicture.MakeFromBytes(LPicture.Serialize);
  LPictureCopy.Playback(LSurface.Canvas);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkPictureTests.TestCullRect(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
  LRecorder: ISkPictureRecorder;
  LPictureCanvas: ISkCanvas;
  LPicture: ISkPicture;
begin
  LSurface := TSkSurface.MakeRaster(256, 256);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LRecorder := TSkPictureRecorder.Create;
  LPictureCanvas := LRecorder.BeginRecording(RectF(64, 64, 192, 192));
  LPaint := TSkPaint.Create;
  LPictureCanvas.DrawRect(RectF(0, 0, 200, 200), LPaint);
  LPaint.Color := TAlphaColors.White;
  LPictureCanvas.DrawRect(RectF(20, 20, 180, 180), LPaint);
  LPicture := LRecorder.FinishRecording;
  LPicture.Playback(LSurface.Canvas);
  LPaint.Blender := TSkBlender.MakeMode(TSkBlendMode.Modulate);
  LPaint.Color := $40404040;
  LSurface.Canvas.DrawRect(LPicture.CullRect, LPaint);

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkPictureTests.TestDraw(const AMinSimilarity: Double;
  const AExpectedImageHash: string);
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
  LRecorder: ISkPictureRecorder;
  LPictureCanvas: ISkCanvas;
  LPicture: ISkPicture;
  LColor: TAlphaColor;
  LMatrix: TMatrix;
  LAlpha: Byte;
begin
  LSurface := TSkSurface.MakeRaster(256, 256);
  Assert.IsNotNull(LSurface, 'Invalid ISkSurface (nil)');
  LSurface.Canvas.Clear(TAlphaColors.Null);

  LRecorder := TSkPictureRecorder.Create;
  LPictureCanvas := LRecorder.BeginRecording(50, 50);
  LPaint := TSkPaint.Create;

  for LColor in TArray<TAlphaColor>.Create(TAlphaColors.Red, TAlphaColors.Blue, $ff007f00) do
  begin
    LPaint.Color := LColor;
    LPictureCanvas.DrawRect(RectF(10, 10, 30, 40), LPaint);
    LPictureCanvas.Translate(10, 10);
    LPictureCanvas.Scale(1.2, 1.4);
  end;
  LPicture := LRecorder.FinishRecording;
  LMatrix := TMatrix.Identity;
  for LAlpha in [70, 140, 210] do
  begin
    LPaint.Alpha := LAlpha;
    LSurface.Canvas.DrawPicture(LPicture, LMatrix, LPaint);
    LMatrix := LMatrix * LMatrix.CreateTranslation(70, 70);
  end;

  Assert.AreSimilar(AExpectedImageHash, LSurface.MakeImageSnapshot, AMinSimilarity);
end;

procedure TSkPictureTests.TestImageFromPicture(const AMinSimilarity: Double; const AExpectedImageHash: string);
var
  LImage: ISkImage;
  LPaint: ISkPaint;
  LRecorder: ISkPictureRecorder;
  LPictureCanvas: ISkCanvas;
  LPicture: ISkPicture;
begin
  LRecorder := TSkPictureRecorder.Create;
  LPictureCanvas := LRecorder.BeginRecording(256, 256);
  LPaint := TSkPaint.Create;
  LPictureCanvas.DrawRect(RectF(0, 0, 200, 200), LPaint);
  LPaint.Color := TAlphaColors.White;
  LPictureCanvas.DrawRect(RectF(20, 20, 180, 180), LPaint);
  LPicture := LRecorder.FinishRecording;

  LImage := TSkImage.MakeFromPicture(LPicture, TSize.Create(256, 256), TSkColorSpace.MakeSRGB);
  Assert.AreSimilar(AExpectedImageHash, LImage, AMinSimilarity);
end;

procedure TSkPictureTests.TestSerialize(
  const AExpectedSerializedOutputFileName: string);
var
  LPaint: ISkPaint;
  LRecorder: ISkPictureRecorder;
  LPictureCanvas: ISkCanvas;
  LPicture: ISkPicture;
  LStream: TBytesStream;
begin
  LRecorder := TSkPictureRecorder.Create;
  LPictureCanvas := LRecorder.BeginRecording(256, 256);
  LPaint := TSkPaint.Create;
  LPictureCanvas.DrawRect(RectF(0, 0, 200, 200), LPaint);
  LPaint.Color := TAlphaColors.White;
  LPictureCanvas.DrawRect(RectF(20, 20, 180, 180), LPaint);
  LPicture := LRecorder.FinishRecording;
  LStream := TBytesStream.Create;
  try
    LPicture.SerializeToStream(LStream);
    Assert.IsTrue(LStream.Size > 0);
    Assert.AreEqualBytes(LPicture.Serialize, Copy(LStream.Bytes, 0, LStream.Size));
  finally
    LStream.Free;
  end;

  Assert.AreEqualBytes(TFile.ReadAllBytes(AssetsPath + AExpectedSerializedOutputFileName), LPicture.Serialize);
end;

initialization
  TDUnitX.RegisterTestFixture(TSkPictureTests);
end.
