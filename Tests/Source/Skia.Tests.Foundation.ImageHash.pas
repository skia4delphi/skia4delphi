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
unit Skia.Tests.Foundation.ImageHash;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Generics.Collections,

  { Skia }
  System.Skia;

type
  { TImageHashing }

  TImageHashing = class abstract
  public
    type
      THashAlgorithm = class
      public
        class function BackgroundColor: TAlphaColor; virtual;
        class function GetDownscaleSize: TSize; virtual; abstract;
        class function GetHash(const AImagePixels: ISkPixmap): TBytes; virtual; abstract;
        class function GetHashSize: Integer; virtual; abstract;
        class function HighSpeedScale: Boolean; virtual;
        class function NeedGrayScale: Boolean; virtual; abstract;
      end;
      THashAlgorithmClass = class of THashAlgorithm;
  strict private
    class var
      FAlgorithms: TList<TPair<THashAlgorithmClass, Double>>;
    class constructor Create;
    class destructor Destroy;
    class procedure AppendBytes(var ABytes: TBytes; const ANewBytes: TBytes); static;
    class function DecodeHashBase64(const AHashBase64: string): TBytes;
    class function EncodeHashBase64(const AHash: TBytes): string;
    class function GetDownscaleImage(const AImage: ISkImage; const AAlgorithm: THashAlgorithmClass): ISkImage; static;
    class function GetGrayScaleImage(const AImage: ISkImage): ISkImage; static;
    class function HashBytes(const AImage: ISkImage; const AHighSpeed: Boolean): TBytes; static;
    class function ProgressiveDownsampling(const AImage: ISkImage; const ANewWidth, ANewHeight: Integer; const ABackgroundColor: TAlphaColor; const AHighSpeed: Boolean): ISkImage; static;
    class function SimilarityHashBytes(const AHash1, AHash2: TBytes): Double; overload; static;
  public
    class function Hash(const AImage: ISkImage; const AHighSpeed: Boolean = False): string; static;
    class procedure RegisterAlgorithm(const AAlgorithm: THashAlgorithmClass; const AWeight: Double = 1); static;
    class function Similarity(const AHash1, AHash2: string): Double; overload; static;
    class function Similarity(const AExpectedHash: string; const AImage: ISkImage; const AHighSpeed: Boolean = False): Double; overload; static;
    class function Similarity(const AImage1, AImage2: ISkImage; const AHighSpeed: Boolean = False): Double; overload; static;
  end;

implementation

uses
  { Delphi }
  System.NetEncoding,
  System.UIConsts,
  System.Math,
  System.Math.Vectors;

type
  // https://www.pyimagesearch.com/2017/11/27/image-hashing-opencv-python/
  TDifferenceHash = class(TImageHashing.THashAlgorithm)
  strict private
    const
      DownscaleWidth = 9;
      DownscaleHeight = 8;
      HashBits = (DownscaleWidth - 1) * DownscaleHeight;
      HashBytes = (HashBits + 7) div 8;
  public
    class function BackgroundColor: TAlphaColor; override;
    class function GetDownscaleSize: TSize; override;
    class function GetHash(const AImagePixels: ISkPixmap): TBytes; override;
    class function GetHashSize: Integer; override;
    class function HighSpeedScale: Boolean; override;
    class function NeedGrayScale: Boolean; override;
  end;

  // http://www.hackerfactor.com/blog/index.php?/archives/432-Looks-Like-It.html
  TAverageHash = class(TImageHashing.THashAlgorithm)
  strict private
    const
      DownscaleWidth = 8;
      DownscaleHeight = 8;
      HashBits = DownscaleWidth * DownscaleHeight;
      HashBytes = (HashBits + 7) div 8;
  public
    class function GetDownscaleSize: TSize; override;
    class function GetHash(const AImagePixels: ISkPixmap): TBytes; override;
    class function GetHashSize: Integer; override;
    class function NeedGrayScale: Boolean; override;
  end;

  // http://www.hackerfactor.com/blog/index.php?/archives/432-Looks-Like-It.html
  TPerceptiveHash = class(TImageHashing.THashAlgorithm)
  strict private
    const
      DownscaleWidth = 32;
      DownscaleHeight = 32;
      DCTImportantWidth = 8;
      DCTImportantHeight = 8;
      HashBits = DCTImportantWidth * DCTImportantHeight;
      HashBytes = (HashBits + 7) div 8;
    type
      THashMatrix = packed array[0..DownscaleWidth - 1, 0..DownscaleHeight - 1] of Double;
    class var
      FDCTKernel: THashMatrix;
      FDCTKernelTransposed: THashMatrix;
    class constructor Create;
    class function GenerateDCTKernel: THashMatrix; static;
    class function GetDCTMatrix(const AImagePixels: ISkPixmap): THashMatrix; static;
    class function MultiplyMatrix(const M1, M2: THashMatrix): THashMatrix; static;
    class function TransposeMatrix(const AMatrix: THashMatrix): THashMatrix; static;
  public
    class function GetDownscaleSize: TSize; override;
    class function GetHash(const AImagePixels: ISkPixmap): TBytes; override;
    class function GetHashSize: Integer; override;
    class function NeedGrayScale: Boolean; override;
  end;

  // Custom hue hash created by us, based on TAverageHash but with Hue instead Luminosity
  THSLHash = class(TImageHashing.THashAlgorithm)
  strict private
    const
      DownscaleWidth = 16;
      DownscaleHeight = 16;
      HashBits = DownscaleWidth * DownscaleHeight;
      HashBytes = (HashBits + 7) div 8;
    type
      THSL = record
        Hue: Double;
        Saturation: Double;
        Lightness: Double;
      end;
    class function CompareColorHSL(const AColor: TAlphaColor; const AAvarage: THSL): TValueRelationship; static;
    class procedure ComputeHSLAvarage(const AColor: TAlphaColor; var AAvarage: THSL); static;
  public
    class function BackgroundColor: TAlphaColor; override;
    class function GetDownscaleSize: TSize; override;
    class function GetHash(const AImagePixels: ISkPixmap): TBytes; override;
    class function GetHashSize: Integer; override;
    class function HighSpeedScale: Boolean; override;
    class function NeedGrayScale: Boolean; override;
  end;

const
  BitsPerByte = 8;
  MaxBitIndexInByte = 7;

{ TDifferenceHash }

class function TDifferenceHash.BackgroundColor: TAlphaColor;
begin
  Result := TAlphaColors.Fuchsia;
end;

class function TDifferenceHash.GetDownscaleSize: TSize;
begin
  Result := TSize.Create(DownscaleWidth, DownscaleHeight);
end;

class function TDifferenceHash.GetHash(
  const AImagePixels: ISkPixmap): TBytes;
var
  X: Integer;
  Y: Integer;
  LByteIndex: Integer;
  LBitIndexInByte: ShortInt;
begin
  Assert(AImagePixels <> nil);
  Assert(AImagePixels.Width = DownscaleWidth);
  Assert(AImagePixels.Height = DownscaleHeight);
  Assert(Odd(DownscaleWidth));

  SetLength(Result, HashBytes);
  for Y := 0 to DownscaleHeight - 1 do
  begin
    for X := 1 to DownscaleWidth - 1 do
    begin
      if TAlphaColorRec(AImagePixels.Colors[X - 1, Y]).R > TAlphaColorRec(AImagePixels.Colors[X, Y]).R then
      begin
        LByteIndex := ((Y * (DownscaleWidth - 1)) + (X - 1)) div BitsPerByte;
        Assert((LByteIndex >= 0) and (LByteIndex < HashBytes));
        LBitIndexInByte := MaxBitIndexInByte - ((Y * (DownscaleWidth - 1) + (X - 1)) mod BitsPerByte);
        Assert((LBitIndexInByte >= 0) and (LBitIndexInByte < BitsPerByte));
        Result[LByteIndex] := Result[LByteIndex] or (1 shl LBitIndexInByte);
      end;
    end;
  end;
end;

class function TDifferenceHash.GetHashSize: Integer;
begin
  Result := HashBytes;
end;

class function TDifferenceHash.HighSpeedScale: Boolean;
begin
  Result := False;
end;

class function TDifferenceHash.NeedGrayScale: Boolean;
begin
  Result := True;
end;

{ TAverageHash }

class function TAverageHash.GetDownscaleSize: TSize;
begin
  Result := TSize.Create(DownscaleWidth, DownscaleHeight);
end;

class function TAverageHash.GetHash(const AImagePixels: ISkPixmap): TBytes;
var
  X: Integer;
  Y: Integer;
  LByteIndex: Integer;
  LBitIndexInByte: ShortInt;
  LAverage: Integer;
begin
  Assert(AImagePixels <> nil);
  Assert(AImagePixels.Width = DownscaleWidth);
  Assert(AImagePixels.Height = DownscaleHeight);

  SetLength(Result, HashBytes);
  LAverage := 0;
  for Y := 0 to DownscaleHeight - 1 do
    for X := 0 to DownscaleWidth - 1 do
      Inc(LAverage, TAlphaColorRec(AImagePixels.Colors[X, Y]).R);
  LAverage := LAverage div (DownscaleWidth * DownscaleHeight);

  for Y := 0 to DownscaleHeight - 1 do
  begin
    for X := 0 to DownscaleWidth - 1 do
    begin
      if TAlphaColorRec(AImagePixels.Colors[X, Y]).R > LAverage then
      begin
        LByteIndex := (Y * DownscaleWidth + X) div BitsPerByte;
        Assert((LByteIndex >= 0) and (LByteIndex < HashBytes));
        LBitIndexInByte := MaxBitIndexInByte - ((Y * DownscaleWidth + X) mod BitsPerByte);
        Assert((LBitIndexInByte >= 0) and (LBitIndexInByte < BitsPerByte));
        Result[LByteIndex] := Result[LByteIndex] or (1 shl LBitIndexInByte);
      end;
    end;
  end;
end;

class function TAverageHash.GetHashSize: Integer;
begin
  Result := HashBytes;
end;

class function TAverageHash.NeedGrayScale: Boolean;
begin
  Result := True;
end;

{ TPerceptiveHash }

// DCT algorithm based on https://www.codeproject.com/Articles/43782/DCT-Implementation-in-C-DCT-of-Image
class constructor TPerceptiveHash.Create;
begin
  FDCTKernel := GenerateDCTKernel;
  FDCTKernelTransposed := TransposeMatrix(FDCTKernel);
end;

class function TPerceptiveHash.GenerateDCTKernel: THashMatrix;
const
  Denominator = 2 * DownscaleHeight;
var
  X: Integer;
  Y: Integer;
  LAlpha: Double;
begin
  for Y := 0 to DownscaleHeight - 1 do
    Result[0, Y] := Sqrt(1 / DownscaleHeight);
  LAlpha := Sqrt(2 / DownscaleHeight);
  for Y := 0 to DownscaleHeight - 1 do
    for X := 1 to DownscaleWidth - 1 do
      Result[X, Y] := LAlpha * Cos(((2 * Y + 1) * X * Pi) / Denominator);
end;

class function TPerceptiveHash.GetDCTMatrix(
  const AImagePixels: ISkPixmap): THashMatrix;
var
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to DownscaleHeight - 1 do
    for X := 0 to DownscaleWidth - 1 do
      Result[X, Y] := TAlphaColorRec(AImagePixels.Colors[X, Y]).R;
  Result := MultiplyMatrix(MultiplyMatrix(FDCTKernel, Result), FDCTKernelTransposed);
end;

class function TPerceptiveHash.GetDownscaleSize: TSize;
begin
  Result := TSize.Create(DownscaleWidth, DownscaleHeight);
end;

class function TPerceptiveHash.GetHash(const AImagePixels: ISkPixmap): TBytes;
var
  X: Integer;
  Y: Integer;
  LByteIndex: Integer;
  LBitIndexInByte: ShortInt;
  LAverage: Double;
  LDCTMatrix: THashMatrix;
begin
  Assert(AImagePixels <> nil);
  Assert(AImagePixels.Width = DownscaleWidth);
  Assert(AImagePixels.Height = DownscaleHeight);

  SetLength(Result, HashBytes);
  LDCTMatrix := GetDCTMatrix(AImagePixels);

  LAverage := 0;
  for Y := 0 to DCTImportantHeight - 1 do
  begin
    for X := 0 to DCTImportantWidth - 1 do
    begin
      if (Y = 0) and (X = 0) then
        Continue;
      LAverage := LAverage + LDCTMatrix[X, Y];
    end;
  end;
  LAverage := LAverage / ((DCTImportantWidth * DCTImportantHeight) - 1);

  for Y := 0 to DCTImportantHeight - 1 do
  begin
    for X := 0 to DCTImportantWidth - 1 do
    begin
      if LDCTMatrix[X, Y] > LAverage then
      begin
        LByteIndex := (Y * DCTImportantWidth + X) div BitsPerByte;
        Assert((LByteIndex >= 0) and (LByteIndex < HashBytes));
        LBitIndexInByte := MaxBitIndexInByte - ((Y * DCTImportantWidth + X) mod BitsPerByte);
        Assert((LBitIndexInByte >= 0) and (LBitIndexInByte < BitsPerByte));
        Result[LByteIndex] := Result[LByteIndex] or (1 shl LBitIndexInByte);
      end;
    end;
  end;
end;

class function TPerceptiveHash.GetHashSize: Integer;
begin
  Result := HashBytes;
end;

class function TPerceptiveHash.MultiplyMatrix(const M1,
  M2: THashMatrix): THashMatrix;
var
  X: Integer;
  Y: Integer;
begin
  Assert(DownscaleWidth = DownscaleHeight);
  FillChar(Result, SizeOf(Result), #0);
  for Y := 0 to DownscaleHeight - 1 do
    for X := 0 to DownscaleWidth - 1 do
      Result[X, Y] := Result[X, Y] + M1[X, Y] * M2[Y, X];
end;

class function TPerceptiveHash.NeedGrayScale: Boolean;
begin
  Result := True;
end;

class function TPerceptiveHash.TransposeMatrix(
  const AMatrix: THashMatrix): THashMatrix;
var
  X: Integer;
  Y: Integer;
begin
  Assert(DownscaleWidth = DownscaleHeight);
  FillChar(Result, SizeOf(Result), #0);
  for Y := 0 to DownscaleHeight - 1 do
    for X := 0 to DownscaleWidth - 1 do
      Result[Y, X] := AMatrix[X, Y];
end;

{ THSLHash }

class function THSLHash.BackgroundColor: TAlphaColor;
begin
  Result := TAlphaColors.Fuchsia;
end;

class function THSLHash.CompareColorHSL(const AColor: TAlphaColor;
  const AAvarage: THSL): TValueRelationship;
const
  HueWeight = 0.4125;
  SaturationWeight = 0.3375;
  LightnessWeight = 0.25;
var
  H, S, L: Single;
  LAvarageValue: Double;
  LColorValue: Double;
begin
  LAvarageValue := AAvarage.Hue * HueWeight + AAvarage.Saturation * SaturationWeight + AAvarage.Lightness * LightnessWeight;
  RGBToHSL(AColor, H, S, L);
  LColorValue := H * HueWeight + S * SaturationWeight + L * LightnessWeight;
  Result := CompareValue(LColorValue, LAvarageValue, TEpsilon.Vector);
end;

class procedure THSLHash.ComputeHSLAvarage(const AColor: TAlphaColor;
  var AAvarage: THSL);
var
  H, S, L: Single;
begin
  RGBToHSL(AColor, H, S, L);
  AAvarage.Hue := AAvarage.Hue + H / (DownscaleWidth * DownscaleHeight);
  AAvarage.Saturation := AAvarage.Saturation + S / (DownscaleWidth * DownscaleHeight);
  AAvarage.Lightness := AAvarage.Lightness + L / (DownscaleWidth * DownscaleHeight);
end;

class function THSLHash.GetDownscaleSize: TSize;
begin
  Result := TSize.Create(DownscaleWidth, DownscaleHeight);
end;

class function THSLHash.GetHash(const AImagePixels: ISkPixmap): TBytes;
var
  X: Integer;
  Y: Integer;
  LByteIndex: Integer;
  LBitIndexInByte: ShortInt;
  LAvarage: THSL;
begin
  Assert(AImagePixels <> nil);
  Assert(AImagePixels.Width = DownscaleWidth);
  Assert(AImagePixels.Height = DownscaleHeight);

  SetLength(Result, HashBytes);
  FillChar(LAvarage, SizeOf(LAvarage), #0);
  for Y := 0 to DownscaleHeight - 1 do
    for X := 0 to DownscaleWidth - 1 do
      ComputeHSLAvarage(AImagePixels.Colors[X, Y], LAvarage);

  for Y := 0 to DownscaleHeight - 1 do
  begin
    for X := 0 to DownscaleWidth - 1 do
    begin
      if CompareColorHSL(AImagePixels.Colors[X, Y], LAvarage) = GreaterThanValue then
      begin
        LByteIndex := (Y * DownscaleWidth + X) div BitsPerByte;
        Assert((LByteIndex >= 0) and (LByteIndex < HashBytes));
        LBitIndexInByte := MaxBitIndexInByte - ((Y * DownscaleWidth + X) mod BitsPerByte);
        Assert((LBitIndexInByte >= 0) and (LBitIndexInByte < BitsPerByte));
        Result[LByteIndex] := Result[LByteIndex] or (1 shl LBitIndexInByte);
      end;
    end;
  end;
end;

class function THSLHash.GetHashSize: Integer;
begin
  Result := HashBytes;
end;

class function THSLHash.HighSpeedScale: Boolean;
begin
  Result := False;
end;

class function THSLHash.NeedGrayScale: Boolean;
begin
  Result := False;
end;

{ TImageHashing.THashAlgorithm }

class function TImageHashing.THashAlgorithm.BackgroundColor: TAlphaColor;
begin
  Result := TAlphaColors.White;
end;

class function TImageHashing.THashAlgorithm.HighSpeedScale: Boolean;
begin
  Result := True;
end;

{ TImageHashing }

class procedure TImageHashing.AppendBytes(var ABytes: TBytes;
  const ANewBytes: TBytes);
var
  LOldLength: Integer;
begin
  if ANewBytes <> nil then
  begin
    LOldLength := Length(ABytes);
    SetLength(ABytes, LOldLength + Length(ANewBytes));
    Move(ANewBytes[0], ABytes[LOldLength], Length(ANewBytes));
  end;
end;

class constructor TImageHashing.Create;
begin
  FAlgorithms := TList<TPair<THashAlgorithmClass, Double>>.Create;
  RegisterAlgorithm(TAverageHash);
  RegisterAlgorithm(TPerceptiveHash);
  RegisterAlgorithm(TDifferenceHash);
  RegisterAlgorithm(THSLHash);
end;

class function TImageHashing.DecodeHashBase64(
  const AHashBase64: string): TBytes;
begin
  if AHashBase64 = '' then
    Exit(nil);
  Result := TNetEncoding.Base64.DecodeStringToBytes(AHashBase64 + '=');
end;

class destructor TImageHashing.Destroy;
begin
  FAlgorithms.Free;
end;

class function TImageHashing.EncodeHashBase64(const AHash: TBytes): string;
begin
  if AHash = nil then
    Exit('');
  Result := TNetEncoding.Base64.EncodeBytesToString(AHash, Length(AHash));
  Result := Result.Substring(0, Length(Result)-1);
end;

class function TImageHashing.GetDownscaleImage(const AImage: ISkImage;
  const AAlgorithm: THashAlgorithmClass): ISkImage;
begin
  Result := ProgressiveDownsampling(AImage, AAlgorithm.GetDownscaleSize.Width,
    AAlgorithm.GetDownscaleSize.Height, AAlgorithm.BackgroundColor, AAlgorithm.HighSpeedScale);
end;

class function TImageHashing.GetGrayScaleImage(
  const AImage: ISkImage): ISkImage;
var
  LSurface: ISkSurface;
  LPaint: ISkPaint;
begin
  LSurface := TSkSurface.MakeRaster(AImage.Width, AImage.Height);
  LSurface.Canvas.Clear(TAlphaColors.Null);
  LPaint := TSkPaint.Create;
  LPaint.ColorFilter := TSkColorFilter.MakeMatrix(TSkColorMatrix.CreateSaturation(0));
  LSurface.Canvas.DrawImage(AImage, 0, 0, TSkSamplingOptions.Low, LPaint);
  Result := LSurface.MakeImageSnapshot;
end;

class function TImageHashing.Hash(const AImage: ISkImage;
  const AHighSpeed: Boolean): string;
begin
  Result := EncodeHashBase64(HashBytes(AImage, AHighSpeed));
end;

class function TImageHashing.HashBytes(const AImage: ISkImage;
  const AHighSpeed: Boolean): TBytes;
const
  BytesPerPixelsBGRA8888 = 4;
  FatsAlgorithmsCount = 2;
var
  I: Integer;
  LImage: ISkImage;
  LImageData: TBytes;
  LPixmap: ISkPixmap;
begin
  if (AImage = nil) or (AImage.Width <= 0) or (AImage.Height <= 0) then
    Exit(nil);
  Assert(FAlgorithms.Count > 0);
  Result := nil;
  for I := 0 to FAlgorithms.Count - 1 do
  begin
    if (I <> 0) and AHighSpeed and (not FAlgorithms[I].Key.HighSpeedScale) then
      Break;
    LImage := GetDownscaleImage(AImage, FAlgorithms[I].Key);
    if FAlgorithms[I].Key.NeedGrayScale then
      LImage := GetGrayScaleImage(LImage);
    SetLength(LImageData, BytesPerPixelsBGRA8888 * LImage.Width * LImage.Height);
    LPixmap := TSkPixmap.Create(TSkImageInfo.Create(LImage.Width, LImage.Height, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), LImageData, BytesPerPixelsBGRA8888 * LImage.Width);
    if not LImage.ReadPixels(LPixmap) then
      raise Exception.Create('Cannot possible to get image pixels');
    AppendBytes(Result, FAlgorithms[I].Key.GetHash(LPixmap));
    if (I = 0) and AHighSpeed and (not FAlgorithms[I].Key.HighSpeedScale) then
      Break;
  end;
end;

class function TImageHashing.ProgressiveDownsampling(const AImage: ISkImage;
  const ANewWidth, ANewHeight: Integer;
  const ABackgroundColor: TAlphaColor;
  const AHighSpeed: Boolean): ISkImage;

  procedure Resize(var AImage: ISkImage; ANewWidth, ANewHeight: Integer);
  var
    LSurface: ISkSurface;
  begin
    if (ANewWidth <> AImage.Width) or (ANewHeight <> AImage.Height) then
    begin
      LSurface := TSkSurface.MakeRaster(ANewWidth, ANewHeight);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      LSurface.Canvas.DrawImageRect(AImage, TRectF.Create(0, 0, ANewWidth, ANewHeight),
        TSkSamplingOptions.Create(TSkCubicResampler.CatmullRom));
      AImage := LSurface.MakeImageSnapshot;
    end;
  end;

  function NearScalableSize(const ASize, ANewSize: Integer): Integer;
  begin
    Result := (1 shl Floor(Log2(ASize / ANewSize))) * ANewSize;
  end;

var
  LSurface: ISkSurface;
begin
  if (ANewWidth <= 0) or (ANewHeight <= 0) then
    Exit(nil);
  if AHighSpeed then
  begin
    if (ABackgroundColor <> TAlphaColors.Null) or (AImage.Width <= ANewWidth) or (AImage.Height <= ANewHeight) then
    begin
      LSurface := TSkSurface.MakeRaster(Max(AImage.Width, ANewWidth), Max(AImage.Height, ANewWidth));
      LSurface.Canvas.Clear(ABackgroundColor);
      LSurface.Canvas.DrawImage(AImage, 0, 0);
      Result := LSurface.MakeImageSnapshot;
    end
    else
      Result := AImage;

    if (Result.Width <> ANewWidth) or (Result.Height <> ANewHeight) then
    begin
      LSurface := TSkSurface.MakeRaster(ANewWidth, ANewHeight);
      LSurface.Canvas.Clear(TAlphaColors.Null);
      LSurface.Canvas.DrawImageRect(Result, TRectF.Create(0, 0, ANewWidth, ANewHeight),
        TSkSamplingOptions.Create(TSkFilterMode.Linear, TSkMipmapMode.Linear));
      Result := LSurface.MakeImageSnapshot;
    end;
  end
  else
  begin
    if (ABackgroundColor <> TAlphaColors.Null) or (AImage.Width <= ANewWidth) or (AImage.Height <= ANewHeight) then
    begin
      LSurface := TSkSurface.MakeRaster(Max(AImage.Width, ANewWidth), Max(AImage.Height, ANewWidth));
      LSurface.Canvas.Clear(ABackgroundColor);
      LSurface.Canvas.DrawImage(AImage, 0, 0);
      Result := LSurface.MakeImageSnapshot;
    end
    else
      Result := AImage;

    Resize(Result, NearScalableSize(Result.Width, ANewWidth), NearScalableSize(Result.Height, ANewHeight));
    while (Result.Width > ANewWidth) or (Result.Height > ANewHeight) do
    begin
      Resize(Result, IfThen(Result.Width <= ANewWidth, Result.Width, Ceil(Result.Width / 2)),
        IfThen(Result.Height <= ANewHeight, Result.Height, Ceil(Result.Height / 2)));
    end;
  end;
end;

class procedure TImageHashing.RegisterAlgorithm(
  const AAlgorithm: THashAlgorithmClass; const AWeight: Double);
begin
  FAlgorithms.Add(TPair<THashAlgorithmClass, Double>.Create(AAlgorithm, AWeight));
end;

class function TImageHashing.Similarity(const AExpectedHash: string;
  const AImage: ISkImage; const AHighSpeed: Boolean): Double;
begin
  Result := Similarity(AExpectedHash, Hash(AImage, AHighSpeed));
end;

class function TImageHashing.Similarity(const AImage1,
  AImage2: ISkImage; const AHighSpeed: Boolean): Double;
begin
  Result := Similarity(Hash(AImage1, AHighSpeed), Hash(AImage2, AHighSpeed));
end;

class function TImageHashing.Similarity(const AHash1, AHash2: string): Double;
begin
  Result := SimilarityHashBytes(DecodeHashBase64(AHash1), DecodeHashBase64(AHash2));
end;

class function TImageHashing.SimilarityHashBytes(const AHash1, AHash2: TBytes): Double;
type
  TAlgorithmResult = record
    Similarity: Double;
    Weight: Double;
  end;

  function CalcSimilarity(const AResults: TArray<TAlgorithmResult>): Double;
  var
    I: Integer;
    LTotalWeight: Double;
  begin
    Assert(AResults <> nil);
    LTotalWeight := 0;
    Result := 0;
    for I := 0 to Length(AResults) - 1 do
    begin
      Result := Result + (AResults[I].Similarity * AResults[I].Weight);
      LTotalWeight := LTotalWeight + AResults[I].Weight;
    end;
    Result := Result / LTotalWeight;
  end;

var
  I: Integer;
  J: Integer;
  K: Integer;
  LBitMask: Byte;
  LEquals: Integer;
  LMinSize: Integer;
  LEndAlgorithmIndex: Integer;
  LByteIndex: Integer;
  LResults: TArray<TAlgorithmResult>;
begin
  if (AHash1 = nil) and (AHash2 = nil) then
    Exit(1);
  LMinSize := Min(Length(AHash1), Length(AHash2));
  if (LMinSize = 0) and ((AHash1 <> nil) or (AHash2 <> nil)) then
    Exit(0);
  Assert(FAlgorithms.Count > 0);

  LResults := [];
  LByteIndex := 0;
  for I := 0 to FAlgorithms.Count - 1 do
  begin
    LEquals := 0;
    LEndAlgorithmIndex := Min(LMinSize, LByteIndex + FAlgorithms[I].Key.GetHashSize);
    if LByteIndex >= LEndAlgorithmIndex then
      Break;
    for J := LByteIndex to LEndAlgorithmIndex - 1 do
    begin
      for K := 0 to MaxBitIndexInByte do
      begin
        LBitMask := 1 shl (MaxBitIndexInByte - K);
        if (AHash1[J] and LBitMask) = (AHash2[J] and LBitMask) then
          Inc(LEquals);
      end;
    end;
    SetLength(LResults, Length(LResults) + 1);
    LResults[High(LResults)].Similarity := LEquals / ((LEndAlgorithmIndex - LByteIndex) * BitsPerByte);
    LResults[High(LResults)].Weight := FAlgorithms[I].Value;
    LByteIndex := LEndAlgorithmIndex;
  end;
  Result := CalcSimilarity(LResults);
end;

end.
