{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2022 Google LLC.                                    }
{ Copyright (c) 2021-2022 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.Foundation;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  DUnitX.TestFramework,
  DUnitX.Assert,

  { Skia }
  Skia;

type
  { TTestBase }

  TTestBase = class
  strict protected
    const
      DefaultFontFamily = 'Segoe UI';
    function AssetsPath: string; overload; virtual;
    function AssetsPath(const ASubPath: string): string; overload;
    function CombinePaths(const APath, ASubPath: string): string;
    function FontAssetsPath: string;
    function FontProvider: ISkTypefaceFontProvider;
    function ImageAssetsPath: string;
    procedure RegisterFontFiles(const AFontProvider: ISkTypefaceFontProvider);
    function RootAssetsPath: string;
  public
    [Setup]
    procedure Setup; virtual;
    [TearDown]
    procedure TearDown; virtual;
  end;

  { TAssertHelper }

  TAssertHelper = class helper for Assert
  strict private const
    DefaultMinImageSimilarity = 0.99;
    SArrayValuesEqual = 'Array values are equal. ';
    SArrayValuesNotEqual = 'Array values are not equal. ';
    SBytesHashNotEqual = 'Bytes hashes are not equal. Expected %u but got %u. %s';
    SBytesValuesEqual = 'Bytes values are equal. ';
    SBytesValuesNotEqual = 'Bytes values are not equal. ';
    SImagesPixelsNotEqual = 'Are equal pixels. ';
    SStreamHashNotEqual = 'Stream hashes are not equal. Expected %u but got %u. %s';
    SStringHashNotEqual = 'String hashes are not equal. Expected %u but got %u. %s';
    SImagesNotSimilar = 'Images are not similar. Expected at least %g but got %g (hash: %s). %s';
  strict private
    class function AreSameArray<T>(const ALeft, ARight: TArray<T>): Boolean; static; inline;
    class function AreSamePixels(const AExpectedEncodedImage, AActualEncodedImage: TBytes): Boolean; static;
  strict protected
    class function GetPixmapBytes(const APixmap: ISkPixmap): TBytes; static;
  public
    class procedure AreEqualArray<T>(const AExpected, AActual: TArray<T>; const AMessage: string = '');
    class procedure AreEqualBytes(const AExpected, AActual: TBytes; const AMessage: string = '');
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: TBytes; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: TStream; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: string; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: ISkImage; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: ISkCodec; const AMessage: string = ''); overload;
    class procedure AreEqualPixels(const AExpectedEncodedImage, AActualEncodedImage: TBytes; const AMessage: string = '');
    class procedure AreNotEqualArray<T>(const AExpected, AActual: TArray<T>; const AMessage: string = '');
    class procedure AreNotEqualBytes(const AExpected, AActual: TBytes; const AMessage: string = '');
    class procedure AreSameValue(const AExpected, AActual: Double; const AEpsilon: Double = 0; const AMessage: string = ''); overload;
    class procedure AreSameValue(const AExpected, AActual: Single; const AEpsilon: Single = 0; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpectedHash: string; const AActual: ISkImage; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpected, AActual: ISkImage; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
  end;

  {$IF CompilerVersion < 29}
  { TSkFormatSettingsHelper }

  TSkFormatSettingsHelper = record helper for TFormatSettings
  strict private
    class var
      FInvariant: TFormatSettings;
    class constructor Create;
  public
    class property Invariant: TFormatSettings read FInvariant;
  end;
  {$ENDIF}

  {$IF CompilerVersion < 32}
  { TSkStreamHelper }

  TSkStreamHelper = class helper for TStream
  public
    function WriteData<T>(const ABuffer: T; ACount: NativeInt): NativeInt; overload;
  end;
  {$ENDIF}

  {$IF CompilerVersion < 33}
  { TSkEpsilonHelper }

  TSkEpsilonHelper = record helper for TEpsilon
  const
    Scale = 1E-4;
    FontSize = 1E-2;
    Position = 1E-3;
  end;
  {$ENDIF}

function BytesToHex(const ABytes: TBytes): string;
function HexToBytes(AString: string): TBytes;
function PathToText(const APath: ISkPath): string;

implementation

uses
  { Delphi }
  System.IOUtils,
  System.ZLib,
  System.Types,
  System.UITypes,
  DUnitX.ResStrs,

  { Tests }
  Skia.Tests.Foundation.ImageHash;

type
  { THashCRC32 }

  THashCRC32 = class abstract
  public
    class function FromBuffer(const AData: Pointer; const ASize: Integer): Cardinal; static;
    class function FromBytes(const ABytes: TBytes): Cardinal; static;
    class function FromStream(const AStream: TStream): Cardinal; static;
    class function FromString(const AString: string): Cardinal; static;
  end;

function BytesToHex(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  if ABytes <> nil then
  begin
    for I := 0 to Length(ABytes) - 1 do
      Result := Result + IntToHex(ABytes[I], 2);
    Result := Result.ToLower;
  end;
end;

function HexToBytes(AString: string): TBytes;
var
  I: Integer;
  LByte: Integer;
begin
  if AString.StartsWith('$') or AString.StartsWith('x') then
    AString := AString.Substring(1);
  if AString.StartsWith('0x') then
    AString := AString.Substring(2);
  if (Length(AString) mod 2) <> 0 then
    Exit(nil);
  SetLength(Result, Length(AString) div 2);
  for I := 0 to Length(Result) - 1 do
  begin
    if TryStrToInt('$' + AString.Chars[I * 2] + AString.Chars[(I * 2) + 1], LByte) then
      Result[I] := Byte(LByte)
    else
      Exit(nil);
  end;
end;

// Just simple conversion of path elements to text, to help compare paths
function PathToText(const APath: ISkPath): string;

  function PointsToStr(const APoints: TSkPathPoints; const ACount: Integer): string; inline;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to ACount - 1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      Result := Result + '([' + I.ToString + ']: ' + FormatFloat('0.####', APoints[I].X, TFormatSettings.Invariant) + 'x' + FormatFloat('0.####', APoints[I].Y, TFormatSettings.Invariant) + ')';
    end;
  end;

const
  LPointCount: array[TSkPathVerb] of Integer = (   1   ,    2   ,    3   ,    3   ,    4  ,     1   );
  LVerbNames : array[TSkPathVerb] of string  = ('Move' , 'Line' , 'Quad' , 'Conic', 'Cubic', 'Close');
var
  LElem: TSkPathIteratorElem;
  LLine: string;
  LStrings: TStrings;
begin
  LStrings := TStringList.Create;
  try
    LStrings.LineBreak := #13#10;
    for LElem in APath.GetIterator(False) do
    begin
      LLine := LVerbNames[LElem.Verb] + ': ' + PointsToStr(LElem.Points, LPointCount[LElem.Verb]);
      if LElem.Verb = TSkPathVerb.Conic then
        LLine := LLine + ', (Conic Weight: ' + FormatFloat('0.####', LElem.ConicWeight, TFormatSettings.Invariant) + ')';
      LStrings.Add(LLine);
    end;
    Result := LStrings.Text.Trim;
  finally
    LStrings.Free;
  end;
end;

{ TTestBase }

function TTestBase.AssetsPath(const ASubPath: string): string;
begin
  Result := CombinePaths(AssetsPath, ASubPath);
end;

function TTestBase.AssetsPath: string;
begin
  Result := RootAssetsPath;
end;

function TTestBase.CombinePaths(const APath, ASubPath: string): string;
begin
  Result := TPath.Combine(APath, ASubPath);
  if not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

function TTestBase.FontAssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Fonts');
end;

function TTestBase.FontProvider: ISkTypefaceFontProvider;
begin
  Result := TSkTypefaceFontProvider.Create;
  RegisterFontFiles(Result);
end;

function TTestBase.ImageAssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Images');
end;

procedure TTestBase.RegisterFontFiles(
  const AFontProvider: ISkTypefaceFontProvider);

  procedure DoRegisterFiles(const AFontProvider: ISkTypefaceFontProvider; const AFiles: TStringDynArray);
  var
    I: Integer;
  begin
    for I := 0 to Length(AFiles) - 1 do
      AFontProvider.RegisterTypeface(TSkTypeface.MakeFromFile(AFiles[I]));
  end;

begin
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.ttf'));
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.otf'));
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.woff'));
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.woff2'));
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.pfa'));
  DoRegisterFiles(AFontProvider, TDirectory.GetFiles(FontAssetsPath, '*.pfb'));
end;

function TTestBase.RootAssetsPath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.GetFullPath('..\..\..\Assets\');
  {$ELSEIF defined(iOS) or defined(ANDROID)}
  Result := TPath.GetDocumentsPath;
  {$ELSEIF defined(MACOS)}
  Result := TPath.GetFullPath('../Resources/');
  {$ELSE}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

procedure TTestBase.Setup;
begin
  {$IFDEF SET_EXCEPTION_MASK}
  SetExceptionMask(exAllArithmeticExceptions);
  {$ENDIF}
end;

procedure TTestBase.TearDown;
begin
end;

{ TAssertHelper }

class procedure TAssertHelper.AreEqualArray<T>(const AExpected,
  AActual: TArray<T>; const AMessage: string);
begin
  DoAssert;
  if not AreSameArray<T>(AExpected, AActual) then
    Fail(SArrayValuesNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreEqualBytes(const AExpected, AActual: TBytes;
  const AMessage: string);
begin
  DoAssert;
  if not AreSameArray<Byte>(AExpected, AActual) then
    Fail(SBytesValuesNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: TBytes; const AMessage: string);
var
  LActualHash: Cardinal;
begin
  DoAssert;
  LActualHash := THashCRC32.FromBytes(AActual);
  if LActualHash <> AExpected then
    FailFmt(SBytesHashNotEqual, [AExpected, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: TStream; const AMessage: string);
var
  LActualHash: Cardinal;
begin
  DoAssert;
  AActual.Position := 0;
  LActualHash := THashCRC32.FromStream(AActual);
  if LActualHash <> AExpected then
    FailFmt(SStreamHashNotEqual, [AExpected, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual, AMessage: string);
var
  LActualHash: Cardinal;
begin
  DoAssert;
  LActualHash := THashCRC32.FromString(AActual);
  if LActualHash <> AExpected then
    FailFmt(SStringHashNotEqual, [AExpected, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: ISkImage; const AMessage: string);
var
  LActualPixmapBytes: TBytes;
  LPixmap: ISkPixmap;
  LData: TBytes;
begin
  LActualPixmapBytes := nil;
  SetLength(LData, 4 * AActual.Width * AActual.Height);
  if Length(LData) > 0 then
  begin
    LPixmap := TSkPixmap.Create(TSkImageInfo.Create(AActual.Width, AActual.Height, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), LData, 4 * AActual.Width);
    Assert.IsTrue(AActual.ReadPixels(LPixmap), 'Can''t read the SkImage pixels');
    LActualPixmapBytes := GetPixmapBytes(LPixmap);
  end;
  AreEqualCRC32(AExpected, LActualPixmapBytes, AMessage);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: ISkCodec; const AMessage: string);
var
  LActualPixmapBytes: TBytes;
  LPixmap: ISkPixmap;
  LData: TBytes;
begin
  LActualPixmapBytes := nil;
  SetLength(LData, 4 * AActual.Width * AActual.Height);
  if Length(LData) > 0 then
  begin
    LPixmap := TSkPixmap.Create(TSkImageInfo.Create(AActual.Width, AActual.Height, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB), LData, 4 * AActual.Width);
    Assert.IsTrue(AActual.GetPixels(LPixmap.Pixels, LPixmap.RowBytes, LPixmap.ColorType, LPixmap.AlphaType, LPixmap.ColorSpace), 'Can''t get the SkCodec pixels');
    LActualPixmapBytes := GetPixmapBytes(LPixmap);
  end;
  AreEqualCRC32(AExpected, LActualPixmapBytes, AMessage);
end;

class procedure TAssertHelper.AreEqualPixels(const AExpectedEncodedImage,
  AActualEncodedImage: TBytes; const AMessage: string);
begin
  DoAssert;
  if not AreSamePixels(AExpectedEncodedImage, AActualEncodedImage) then
    Fail(SImagesPixelsNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreNotEqualArray<T>(const AExpected,
  AActual: TArray<T>; const AMessage: string);
begin
  DoAssert;
  if AreSameArray<T>(AExpected, AActual) then
    Fail(SArrayValuesEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreNotEqualBytes(const AExpected, AActual: TBytes;
  const AMessage: string);
begin
  DoAssert;
  if AreSameArray<Byte>(AExpected, AActual) then
    Fail(SBytesValuesEqual + AMessage, ReturnAddress);
end;

class function TAssertHelper.AreSameArray<T>(const ALeft,
  ARight: TArray<T>): Boolean;
begin
  Result := (ALeft = ARight) or
    ((Length(ALeft) = Length(ARight)) and
    ((Length(ALeft) = 0) or CompareMem(PByte(@ALeft[0]), PByte(@ARight[0]), Length(ALeft))));
end;

class function TAssertHelper.AreSamePixels(const AExpectedEncodedImage,
  AActualEncodedImage: TBytes): Boolean;
var
  LExpectedCodec: ISkCodec;
  LExpectedPixels: TBytes;
  LActualCodec: ISkCodec;
  LActualPixels: TBytes;
begin
  LExpectedCodec := TSkCodec.MakeWithoutCopy(AExpectedEncodedImage, Length(AExpectedEncodedImage));
  LActualCodec := TSkCodec.MakeWithoutCopy(AActualEncodedImage, Length(AActualEncodedImage));
  if (LExpectedCodec = nil) and (LActualCodec = nil) then
    Exit(True);
  if (LExpectedCodec = nil) or (LActualCodec = nil) then
    Exit(False);
  if LExpectedCodec.Dimensions <> LActualCodec.Dimensions then
    Exit(False);
  if (LExpectedCodec.Width <= 0) or (LExpectedCodec.Height <= 0) then
    Exit(True);

  SetLength(LExpectedPixels, 4 * LExpectedCodec.Width * LExpectedCodec.Height);
  SetLength(LActualPixels, 4 * LActualCodec.Width * LActualCodec.Height);

  if not LExpectedCodec.GetPixels(LExpectedPixels, 4 * LExpectedCodec.Width, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB) or
    not LActualCodec.GetPixels(LActualPixels, 4 * LActualCodec.Width, TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB) then
  begin
    Exit(False);
  end;
  Result := AreSameArray<Byte>(LExpectedPixels, LActualPixels);
end;

class procedure TAssertHelper.AreSameValue(const AExpected, AActual,
  AEpsilon: Double; const AMessage: string);
begin
  DoAssert;
  if not System.Math.SameValue(AExpected, AActual, AEpsilon) then
    FailFmt(SUnexpectedErrorExt, [AExpected, AActual, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSameValue(const AExpected, AActual,
  AEpsilon: Single; const AMessage: string);
begin
  DoAssert;
  if not System.Math.SameValue(AExpected, AActual, AEpsilon) then
    FailFmt(SUnexpectedErrorExt, [AExpected, AActual, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSimilar(const AExpectedHash: string;
  const AActual: ISkImage; AMinSimilarity: Double; const AMessage: string);
var
  LActualHash: string;
  LSimilarity: Double;
begin
  DoAssert;
  LActualHash := TImageHashing.Hash(AActual);
  LSimilarity := TImageHashing.Similarity(AExpectedHash, LActualHash);
  if CompareValue(LSimilarity, AMinSimilarity, TEpsilon.Vector) = LessThanValue then
    FailFmt(SImagesNotSimilar, [AMinSimilarity, LSimilarity, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSimilar(const AExpected, AActual: ISkImage;
  AMinSimilarity: Double; const AMessage: string);
var
  LActualHash: string;
  LSimilarity: Double;
begin
  DoAssert;
  LActualHash := TImageHashing.Hash(AActual);
  LSimilarity := TImageHashing.Similarity(LActualHash, AExpected);
  if CompareValue(LSimilarity, AMinSimilarity, TEpsilon.Vector) = LessThanValue then
    FailFmt(SImagesNotSimilar, [AMinSimilarity, LSimilarity, LActualHash, AMessage], ReturnAddress);
end;

class function TAssertHelper.GetPixmapBytes(const APixmap: ISkPixmap): TBytes;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create;
  try
    LStream.WriteData(APixmap.Width, SizeOf(APixmap.Width));
    LStream.WriteData(APixmap.Height, SizeOf(APixmap.Height));
    LStream.WriteData(APixmap.ColorType, SizeOf(APixmap.ColorType));
    LStream.WriteData(APixmap.AlphaType, SizeOf(APixmap.AlphaType));
    if (APixmap.Width > 0) and (APixmap.Height > 0) then
      LStream.WriteData(APixmap.Pixels, 4 * APixmap.Width * APixmap.Height);
    Result := Copy(LStream.Bytes, 0, LStream.Size);
  finally
    LStream.Free;
  end;
end;

{$IF CompilerVersion < 29}
{ TSkFormatSettingsHelper }

class constructor TSkFormatSettingsHelper.Create;
begin
  FInvariant := TFormatSettings.Create('en-US');
end;
{$ENDIF}

{$IF CompilerVersion < 32}
{ TSkStreamHelper }

function TSkStreamHelper.WriteData<T>(const ABuffer: T;
  ACount: NativeInt): NativeInt;
begin
  Assert.IsTrue(ACount = SizeOf(ABuffer));
  Result := Write(ABuffer, ACount);
end;
{$ENDIF}

{ THashCRC32 }

class function THashCRC32.FromBuffer(const AData: Pointer;
  const ASize: Integer): Cardinal;
begin
  Result := crc32(0, nil, 0);
  if ASize > 0 then
    Result := crc32(Result, AData, ASize);
end;

class function THashCRC32.FromBytes(const ABytes: TBytes): Cardinal;
begin
  Result := FromBuffer(ABytes, Length(ABytes));
end;

class function THashCRC32.FromStream(const AStream: TStream): Cardinal;
var
  LBytes: TBytes;
  LOldPosition: Integer;
begin
  LOldPosition := AStream.Position;
  try
    SetLength(LBytes, AStream.Size - AStream.Position);
    if Length(LBytes) > 0 then
      AStream.ReadBuffer(LBytes, Length(LBytes));
    Result := FromBytes(LBytes);
  finally
    AStream.Position := LOldPosition;
  end;
end;

class function THashCRC32.FromString(const AString: string): Cardinal;
begin
  Result := FromBytes(TEncoding.UTF8.GetBytes(AString));
end;

end.
