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
unit Skia.Tests.Foundation.ImageHash;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,

  { Skia }
  System.Skia;

type
  { TImageHashing }

  // The hash is a sketch of a smoothed 24x24 YCbCrA thumbnail: each image
  // quadrant is reduced to a few random +-1 projections, whose differences
  // estimate the RMS color error of that quadrant. The similarity is
  // 1 - (worst quadrant RMS error, in 8-bit levels) / 255.
  TImageHashing = class abstract
  strict private
    const
      GridSize = 24;
      RegionSize = GridSize div 2;
      RegionCount = 4;
      ChannelCount = 4;
      RegionLength = ChannelCount * RegionSize * RegionSize;
      ProjectionsPerRegion = 11;
      ProjectionCount = RegionCount * ProjectionsPerRegion;
      ProjectionBits = 10;
      ProjectionMax = 1 shl (ProjectionBits - 1) - 1;
      ProjectionStep = 2;
      HashSize = ProjectionCount * ProjectionBits div 8;
      NoiseAllowance = 1;
      SignSeed = 2463534242;
    type
      TFeatures = array[0..ChannelCount - 1, 0..GridSize - 1, 0..GridSize - 1] of Int64;
      TProjections = array[0..ProjectionCount - 1] of Integer;
    class function DecodeHash(const AHash: string; out AProjections: TProjections): Boolean; static;
    class function EncodeHash(const AProjections: TProjections): string; static;
    class procedure GetFeatures(const AImage: ISkImage; out AFeatures: TFeatures); static;
    class procedure GetProjections(const AImage: ISkImage; out AProjections: TProjections); static;
  public
    class function Hash(const AImage: ISkImage): string; static;
    class function Similarity(const AHash1, AHash2: string): Double; overload; static;
    class function Similarity(const AExpectedHash: string; const AImage: ISkImage): Double; overload; static;
    class function Similarity(const AImage1, AImage2: ISkImage): Double; overload; static;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.NetEncoding;

{ TImageHashing }

class function TImageHashing.DecodeHash(const AHash: string;
  out AProjections: TProjections): Boolean;
var
  I: Integer;
  LBit: Integer;
  LBytes: TBytes;
  LPadding: string;
  LValue: Integer;
begin
  if AHash = '' then
    Exit(False);
  LPadding := StringOfChar('=', (4 - Length(AHash) mod 4) mod 4);
  LBytes := TNetEncoding.Base64.DecodeStringToBytes(AHash + LPadding);
  if Length(LBytes) <> HashSize then
    raise EArgumentException.CreateFmt('Invalid image hash "%s"', [AHash]);
  for I := 0 to ProjectionCount - 1 do
  begin
    LValue := 0;
    for LBit := 0 to ProjectionBits - 1 do
      if LBytes[(I * ProjectionBits + LBit) div 8] and (1 shl ((I * ProjectionBits + LBit) mod 8)) <> 0 then
        LValue := LValue or (1 shl LBit);
    if LValue > ProjectionMax then
      Dec(LValue, 1 shl ProjectionBits);
    AProjections[I] := LValue;
  end;
  Result := True;
end;

class function TImageHashing.EncodeHash(const AProjections: TProjections): string;
var
  I: Integer;
  LBit: Integer;
  LBytes: TBytes;
begin
  SetLength(LBytes, HashSize);
  FillChar(LBytes[0], HashSize, 0);
  for I := 0 to ProjectionCount - 1 do
    for LBit := 0 to ProjectionBits - 1 do
      if AProjections[I] and (1 shl LBit) <> 0 then
        LBytes[(I * ProjectionBits + LBit) div 8] := LBytes[(I * ProjectionBits + LBit) div 8] or
          (1 shl ((I * ProjectionBits + LBit) mod 8));
  Result := TNetEncoding.Base64.EncodeBytesToString(LBytes, HashSize);
  Result := Result.Replace(#13, '').Replace(#10, '').Replace('=', '');
end;

class procedure TImageHashing.GetFeatures(const AImage: ISkImage;
  out AFeatures: TFeatures);
const
  BytesPerPixel = 4;
  HalfLevel = 128 * 16 * 256;
var
  C: Integer;
  K: Integer;
  X: Integer;
  Y: Integer;
  LArea: Int64;
  LCells: array[0..GridSize - 1, 0..GridSize - 1, 0..ChannelCount - 1] of Int64;
  LColumnFirstCell: TArray<Integer>;
  LColumnSpan: Integer;
  LColumnWeights: TArray<Integer>;
  LHeight: Integer;
  LPixel: PByte;
  LPixels: TBytes;
  LRowCells: array[0..GridSize - 1, 0..ChannelCount - 1] of Integer;
  LSmoothed: array[0..GridSize - 1, 0..GridSize - 1] of Int64;
  LSourceX: Integer;
  LSourceY: Integer;
  LWeight: Integer;
  LWidth: Integer;
  R, G, B, A: Int64;

  // Overlap, in 1/GridSize pixel units, of a source pixel with a grid cell.
  function Overlap(const ASource, ACell, ASize: Integer): Integer;
  begin
    Result := Min((ACell + 1) * ASize, (ASource + 1) * GridSize) - Max(ACell * ASize, ASource * GridSize);
  end;

  function SmoothedAt(const AX, AY: Integer): Int64;
  begin
    Result := LSmoothed[EnsureRange(AY, 0, GridSize - 1), EnsureRange(AX, 0, GridSize - 1)];
  end;

begin
  LWidth := AImage.Width;
  LHeight := AImage.Height;
  SetLength(LPixels, LWidth * LHeight * BytesPerPixel);
  if not AImage.ReadPixels(TSkImageInfo.Create(LWidth, LHeight, TSkColorType.RGBA8888, TSkAlphaType.Premul,
    TSkColorSpace.MakeSRGB), @LPixels[0], LWidth * BytesPerPixel) then
    raise Exception.Create('Cannot read the image pixels');

  // Exact area average of the premultiplied pixels, in 1/16 of a level.
  LColumnSpan := GridSize div LWidth + 2;
  SetLength(LColumnFirstCell, LWidth);
  SetLength(LColumnWeights, LWidth * LColumnSpan);
  for LSourceX := 0 to LWidth - 1 do
  begin
    LColumnFirstCell[LSourceX] := LSourceX * GridSize div LWidth;
    for X := LColumnFirstCell[LSourceX] to ((LSourceX + 1) * GridSize - 1) div LWidth do
      LColumnWeights[LSourceX * LColumnSpan + X - LColumnFirstCell[LSourceX]] := Overlap(LSourceX, X, LWidth);
  end;
  FillChar(LCells, SizeOf(LCells), 0);
  for LSourceY := 0 to LHeight - 1 do
  begin
    FillChar(LRowCells, SizeOf(LRowCells), 0);
    LPixel := @LPixels[LSourceY * LWidth * BytesPerPixel];
    for LSourceX := 0 to LWidth - 1 do
    begin
      for K := 0 to LColumnSpan - 1 do
      begin
        LWeight := LColumnWeights[LSourceX * LColumnSpan + K];
        if LWeight = 0 then
          Break;
        X := LColumnFirstCell[LSourceX] + K;
        Inc(LRowCells[X, 0], LPixel[0] * LWeight);
        Inc(LRowCells[X, 1], LPixel[1] * LWeight);
        Inc(LRowCells[X, 2], LPixel[2] * LWeight);
        Inc(LRowCells[X, 3], LPixel[3] * LWeight);
      end;
      Inc(LPixel, BytesPerPixel);
    end;
    for Y := LSourceY * GridSize div LHeight to Min(GridSize - 1, ((LSourceY + 1) * GridSize - 1) div LHeight) do
    begin
      LWeight := Overlap(LSourceY, Y, LHeight);
      for X := 0 to GridSize - 1 do
        for C := 0 to ChannelCount - 1 do
          Inc(LCells[Y, X, C], Int64(LRowCells[X, C]) * LWeight);
    end;
  end;
  LArea := Int64(LWidth) * LHeight;
  for Y := 0 to GridSize - 1 do
    for X := 0 to GridSize - 1 do
      for C := 0 to ChannelCount - 1 do
        LCells[Y, X, C] := (LCells[Y, X, C] * 16 + LArea div 2) div LArea;

  for C := 0 to ChannelCount - 1 do
  begin
    for Y := 0 to GridSize - 1 do
      for X := 0 to GridSize - 1 do
      begin
        R := LCells[Y, X, 0];
        G := LCells[Y, X, 1];
        B := LCells[Y, X, 2];
        A := LCells[Y, X, 3];
        case C of
          0: LSmoothed[Y, X] := 77 * R + 150 * G + 29 * B - HalfLevel;
          1: LSmoothed[Y, X] := 256 * A - HalfLevel;
          2: LSmoothed[Y, X] := -43 * R - 85 * G + 128 * B;
        else
          LSmoothed[Y, X] := 128 * R - 107 * G - 21 * B;
        end;
      end;
    // Separable [1 2 1] smoothing tolerates sub-pixel and anti-aliasing changes.
    for Y := 0 to GridSize - 1 do
      for X := 0 to GridSize - 1 do
        AFeatures[C, Y, X] := SmoothedAt(X, Y - 1) + 2 * SmoothedAt(X, Y) + SmoothedAt(X, Y + 1);
    for Y := 0 to GridSize - 1 do
      for X := 0 to GridSize - 1 do
        LSmoothed[Y, X] := AFeatures[C, Y, X];
    for Y := 0 to GridSize - 1 do
      for X := 0 to GridSize - 1 do
        AFeatures[C, Y, X] := SmoothedAt(X - 1, Y) + 2 * SmoothedAt(X, Y) + SmoothedAt(X + 1, Y);
  end;
end;

class procedure TImageHashing.GetProjections(const AImage: ISkImage;
  out AProjections: TProjections);
const
  // Features are scaled by 16 (cells) * 256 (color matrix) * 16 (smoothing).
  // Dividing by RegionSize, the square root of the cells of a region, makes
  // the expected squared difference of a projection the mean squared error.
  Divisor = Int64(16 * 256 * 16) * RegionSize * ProjectionStep;
var
  C: Integer;
  I: Integer;
  K: Integer;
  X: Integer;
  Y: Integer;
  LFeatures: TFeatures;
  LRegion: Integer;
  LSeed: Cardinal;
  LSum: Int64;
  LValue: Int64;
  LValues: array[0..RegionLength - 1] of Int64;
begin
  GetFeatures(AImage, LFeatures);
  for LRegion := 0 to RegionCount - 1 do
  begin
    I := 0;
    for C := 0 to ChannelCount - 1 do
      for Y := 0 to RegionSize - 1 do
        for X := 0 to RegionSize - 1 do
        begin
          LValues[I] := LFeatures[C, (LRegion div 2) * RegionSize + Y, (LRegion mod 2) * RegionSize + X];
          Inc(I);
        end;
    LSeed := SignSeed;
    for K := 0 to ProjectionsPerRegion - 1 do
    begin
      LSum := 0;
      for I := 0 to RegionLength - 1 do
      begin
        LSeed := LSeed xor (LSeed shl 13);
        LSeed := LSeed xor (LSeed shr 17);
        LSeed := LSeed xor (LSeed shl 5);
        if LSeed and $80000000 <> 0 then
          Dec(LSum, LValues[I])
        else
          Inc(LSum, LValues[I]);
      end;
      LValue := (Abs(LSum) * 2 + Divisor) div (2 * Divisor);
      if LSum < 0 then
        LValue := -LValue;
      AProjections[LRegion * ProjectionsPerRegion + K] := EnsureRange(LValue, -ProjectionMax - 1, ProjectionMax);
    end;
  end;
end;

class function TImageHashing.Hash(const AImage: ISkImage): string;
var
  LProjections: TProjections;
begin
  if (AImage = nil) or (AImage.Width <= 0) or (AImage.Height <= 0) then
    Exit('');
  GetProjections(AImage, LProjections);
  Result := EncodeHash(LProjections);
end;

class function TImageHashing.Similarity(const AExpectedHash: string;
  const AImage: ISkImage): Double;
begin
  Result := Similarity(AExpectedHash, Hash(AImage));
end;

class function TImageHashing.Similarity(const AImage1, AImage2: ISkImage): Double;
begin
  Result := Similarity(Hash(AImage1), Hash(AImage2));
end;

class function TImageHashing.Similarity(const AHash1, AHash2: string): Double;
var
  I: Integer;
  K: Integer;
  LError: Double;
  LHasHash1: Boolean;
  LHasHash2: Boolean;
  LMaxError: Double;
  LProjections1: TProjections;
  LProjections2: TProjections;
begin
  LHasHash1 := DecodeHash(AHash1, LProjections1);
  LHasHash2 := DecodeHash(AHash2, LProjections2);
  if not (LHasHash1 and LHasHash2) then
  begin
    if LHasHash1 = LHasHash2 then
      Exit(1);
    Exit(0);
  end;
  LMaxError := 0;
  for I := 0 to RegionCount - 1 do
  begin
    LError := 0;
    for K := I * ProjectionsPerRegion to (I + 1) * ProjectionsPerRegion - 1 do
      LError := LError + Sqr(ProjectionStep * (LProjections1[K] - LProjections2[K]));
    LMaxError := Max(LMaxError, LError / ProjectionsPerRegion);
  end;
  Result := 1 - EnsureRange(Sqrt(LMaxError) - NoiseAllowance, 0, 255) / 255;
end;

end.
