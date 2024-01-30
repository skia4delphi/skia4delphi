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
unit Skia.Tests.Foundation;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,
  System.IOUtils,
  System.Types,
  DUnitX.TestFramework,
  DUnitX.Assert,

  { Skia }
  System.Skia;

type
  TOnImageCheckingProc = reference to procedure(const AImage: ISkImage);

  { TTestBase }

  TTestBase = class
  strict private
    FAssetsPathCreated: Boolean;
  strict protected
    const
      DefaultFontFamily = 'Segoe UI';
    function AssetsPath: string; overload; virtual;
    function AssetsPath(const ASubPath: string): string; overload;
    function CombinePaths(const APath, ASubPath: string): string;
    function FontAssetsPath: string;
    function FontProvider: ISkTypefaceFontProvider;
    function ImageAssetsPath: string;
    function SvgAssetsPath: string;
    procedure RegisterFontFiles(const AFontProvider: ISkTypefaceFontProvider);
  public
    [Setup]
    procedure Setup; virtual;
    [TearDown]
    procedure TearDown; virtual;
    class function RootAssetsPath: string; static;
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
    SImagesPixelsNotEqual = 'Are not equal pixels. ';
    SPixelsNotEqual = 'Are not equal pixels. ';
    SStreamHashNotEqual = 'Stream hashes are not equal. Expected %u but got %u. %s';
    SStringHashNotEqual = 'String hashes are not equal. Expected %u but got %u. %s';
    SImagesNotSimilar = 'Images are not similar. Expected at least %g but got %g (hash: %s). %s';
  strict private class var
    FOnImageChecking: TOnImageCheckingProc;
  private
    class function AreSameArray<T>(const ALeft, ARight: TArray<T>): Boolean; static; inline;
    class function AreSamePixels(const AExpectedEncodedImage, AActualEncodedImage: TBytes): Boolean; static;
    class procedure DoImageChecking(const AImage: ISkImage); overload; static;
    class procedure DoImageChecking(const AImageCodec: ISkCodec); overload; static;
    class procedure DoImageChecking(const AImagePixmap: ISkPixmap); overload; static;
  protected
    class function GetPixmapBytes(const APixmap: ISkPixmap): TBytes; static;
  public
    class procedure AreEqualArray<T>(const AExpected, AActual: TArray<T>; const AMessage: string = '');
    class procedure AreEqualBytes(const AExpected, AActual: TBytes; const AMessage: string = '');
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: TBytes; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: TStream; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: string; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: ISkImage; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: ISkCodec; const AMessage: string = ''); overload;
    class procedure AreEqualCRC32(const AExpected: Cardinal; const AActual: ISkPixmap; const AMessage: string = ''); overload;
    class procedure AreEqualPixels(const AExpectedEncodedImage, AActualEncodedImage: TBytes; const AMessage: string = ''); overload;
    class procedure AreEqualPixels(const AExpected, AActual: ISkPixmap; const AMessage: string = ''); overload;
    class procedure AreNotEqualArray<T>(const AExpected, AActual: TArray<T>; const AMessage: string = '');
    class procedure AreNotEqualBytes(const AExpected, AActual: TBytes; const AMessage: string = '');
    class procedure AreSameRect(const AExpected, AActual: TRectF; const AEpsilon: Single = 0; const AMessage: string = '');
    class procedure AreSameValue(const AExpected, AActual: Double; const AEpsilon: Double = 0; const AMessage: string = ''); overload;
    class procedure AreSameValue(const AExpected, AActual: Single; const AEpsilon: Single = 0; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpectedHash: string; const AActual: ISkImage; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpectedHash: string; const AActual: ISkPixmap; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpectedHash: string; const AActual: ISkCodec; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
    class procedure AreSimilar(const AExpected, AActual: ISkImage; AMinSimilarity: Double = DefaultMinImageSimilarity; const AMessage: string = ''); overload;
    class property OnImageChecking: TOnImageCheckingProc read FOnImageChecking write FOnImageChecking;
  end;

  { TSkPathHelper }

  TSkPathHelper = record helper for TPath
  public
    class function GetNewTempPath: string; static;
    class function GetNewTempFileName(AExtension: string = ''): string; static;
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

  {$IF CompilerVersion < 29}
  { TSkMatrixHelper }

  TSkMatrixHelper = record helper for TMatrix
    function EqualsTo(const AMatrix: TMatrix; const Epsilon: Single = TEpsilon.Matrix): Boolean;
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
procedure DrawImageFitCrop(const ACanvas: ISkCanvas; const ADest: TRectF; const AImage: ISkImage; const APaint: ISkPaint = nil);
function HexToBytes(AString: string): TBytes;
/// <summary>Simple conversion of path elements to text, to help compare paths.</summary>
function PathToText(const APath: ISkPath): string;
function RectToString(const R: TRectF): string;
function StringToRect(const S: string): TRectF;

implementation

uses
  { Delphi }
  System.ZLib,
  System.UITypes,
  {$IF CompilerVersion >= 30}
  DUnitX.ResStrs,
  {$ENDIF}

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

{$IF CompilerVersion < 30}
resourcestring
  SUnexpectedErrorExt = 'Expected %g but got %g %s';
  SUnexpectedErrorStr = 'Expected %s but got %s %s';
{$ENDIF}

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

procedure DrawImageFitCrop(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AImage: ISkImage; const APaint: ISkPaint);
var
  LRect: TRectF;
  LRatio: Double;
begin
  LRect := TRectF.Create(0, 0, AImage.Width, AImage.Height);
  if (ADest.Width > 0) and (ADest.Height > 0) then
  begin
    if (LRect.Width / ADest.Width) < (LRect.Height / ADest.Height) then
      LRatio := LRect.Width / ADest.Width
    else
      LRatio := LRect.Height / ADest.Height;

    ACanvas.Save;
    try
      if not SameValue(LRatio, 0, TEpsilon.Position) then
      begin
        ACanvas.ClipRect(ADest, TSkClipOp.Intersect, True);
        LRect := TRectF.Create(0, 0, Round(LRect.Width / LRatio), Round(LRect.Height / LRatio));
        RectCenter(LRect, ADest);
        ACanvas.Translate(LRect.Left, LRect.Top);
        ACanvas.Scale(LRect.Width / AImage.Width, LRect.Height / AImage.Height);
      end;
      ACanvas.DrawImage(AImage, 0, 0, TSkSamplingOptions.High, APaint);
    finally
      ACanvas.Restore;
    end;
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
  PointCount: array[TSkPathVerb] of Integer = (   1   ,    2   ,    3   ,    3   ,    4  ,     1   );
  VerbNames : array[TSkPathVerb] of string  = ('Move' , 'Line' , 'Quad' , 'Conic', 'Cubic', 'Close');
var
  LElem: TSkPathIteratorElem;
  LElementsReflection: TArray<string>;
  LText: string;
begin
  LElementsReflection := [];
  for LElem in APath.GetIterator(False) do
  begin
    LText := VerbNames[LElem.Verb] + ': ' + PointsToStr(LElem.Points, PointCount[LElem.Verb]);
    if LElem.Verb = TSkPathVerb.Conic then
      LText := LText + ', (Conic Weight: ' + FormatFloat('0.####', LElem.ConicWeight, TFormatSettings.Invariant) + ')';
    LElementsReflection := LElementsReflection + [LText];
  end;
  Result := string.Join(' | ', LElementsReflection);
end;

function RectToString(const R: TRectF): string;
begin
  Result := '(' + FloatToStr(R.Left, TFormatSettings.Invariant) + ',' + FloatToStr(R.Top, TFormatSettings.Invariant) + ',' +
    FloatToStr(R.Right, TFormatSettings.Invariant) + ',' + FloatToStr(R.Bottom, TFormatSettings.Invariant) + ')';
end;

function StringToRect(const S: string): TRectF;
var
  LValues: TArray<string>;
begin
  LValues := S.Split(['(', ')', ','], TStringSplitOptions.ExcludeEmpty);
  if (Length(LValues) < 4) or
    not TryStrToFloat(LValues[0], Result.Left, TFormatSettings.Invariant) or
    not TryStrToFloat(LValues[1], Result.Top, TFormatSettings.Invariant) or
    not TryStrToFloat(LValues[2], Result.Right, TFormatSettings.Invariant) or
    not TryStrToFloat(LValues[3], Result.Bottom, TFormatSettings.Invariant) then
  begin
    Result := TRectF.Empty;
  end;
end;

{ TTestBase }

function TTestBase.AssetsPath(const ASubPath: string): string;
begin
  Result := CombinePaths(AssetsPath, ASubPath);
end;

function TTestBase.AssetsPath: string;
var
  LClassSubPath: string;
begin
  Result := RootAssetsPath;
  LClassSubPath := ClassName;
  if LClassSubPath.StartsWith('TSk', False) then
    LClassSubPath := LClassSubPath.Substring(Length('TSk'));
  if LClassSubPath.EndsWith('Tests', False) then
    LClassSubPath := LClassSubPath.Substring(0, LClassSubPath.Length - Length('Tests'));
  if LClassSubPath <> '' then
    Result := CombinePaths(Result, LClassSubPath);
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

class function TTestBase.RootAssetsPath: string;
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
var
  LAssetsPath: string;
begin
  {$IFDEF SET_EXCEPTION_MASK}
  SetExceptionMask(exAllArithmeticExceptions);
  {$ENDIF}
  LAssetsPath := AssetsPath;
  if (not LAssetsPath.IsEmpty) and not TDirectory.Exists(LAssetsPath) then
  begin
    TDirectory.CreateDirectory(LAssetsPath);
    FAssetsPathCreated := True;
  end;
end;

function TTestBase.SvgAssetsPath: string;
begin
  Result := CombinePaths(RootAssetsPath, 'Svg');
end;

procedure TTestBase.TearDown;
begin
  if FAssetsPathCreated and TDirectory.Exists(AssetsPath) and (TDirectory.GetFiles(AssetsPath, '*', TSearchOption.soAllDirectories) = nil) then
    TDirectory.Delete(AssetsPath, True);
end;

{ TAssertHelper }

class procedure TAssertHelper.AreEqualArray<T>(const AExpected,
  AActual: TArray<T>; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not AreSameArray<T>(AExpected, AActual) then
    Fail(SArrayValuesNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreEqualBytes(const AExpected, AActual: TBytes;
  const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not AreSameArray<Byte>(AExpected, AActual) then
    Fail(SBytesValuesNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: TBytes; const AMessage: string);
var
  LActualHash: Cardinal;
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  LActualHash := THashCRC32.FromBytes(AActual);
  if LActualHash <> AExpected then
    FailFmt(SBytesHashNotEqual, [AExpected, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: TStream; const AMessage: string);
var
  LActualHash: Cardinal;
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
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
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
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
  Assert.IsNotNull(AActual, 'Invalid SkImage (nil)');
  DoImageChecking(AActual);
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
  Assert.IsNotNull(AActual, 'Invalid SkCodec (nil)');
  DoImageChecking(AActual);
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

class procedure TAssertHelper.AreEqualCRC32(const AExpected: Cardinal;
  const AActual: ISkPixmap; const AMessage: string);
begin
  Assert.IsNotNull(AActual, 'Invalid SkPixmap (nil)');
  DoImageChecking(AActual);
  AreEqualCRC32(AExpected, GetPixmapBytes(AActual), AMessage);
end;

class procedure TAssertHelper.AreEqualPixels(const AExpected,
  AActual: ISkPixmap; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not AreSameArray<Byte>(GetPixmapBytes(AExpected), GetPixmapBytes(AActual)) then
    Fail(SPixelsNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreEqualPixels(const AExpectedEncodedImage,
  AActualEncodedImage: TBytes; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not AreSamePixels(AExpectedEncodedImage, AActualEncodedImage) then
    Fail(SImagesPixelsNotEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreNotEqualArray<T>(const AExpected,
  AActual: TArray<T>; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if AreSameArray<T>(AExpected, AActual) then
    Fail(SArrayValuesEqual + AMessage, ReturnAddress);
end;

class procedure TAssertHelper.AreNotEqualBytes(const AExpected, AActual: TBytes;
  const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
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

class procedure TAssertHelper.AreSameRect(const AExpected, AActual: TRectF;
  const AEpsilon: Single; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not System.Math.SameValue(AExpected.Left, AActual.Left, AEpsilon) or
    not System.Math.SameValue(AExpected.Top, AActual.Top, AEpsilon) or
    not System.Math.SameValue(AExpected.Right, AActual.Right, AEpsilon) or
    not System.Math.SameValue(AExpected.Bottom, AActual.Bottom, AEpsilon) then
  begin
    FailFmt(SUnexpectedErrorStr, [RectToString(AExpected), RectToString(AActual), AMessage], ReturnAddress);
  end;
end;

class procedure TAssertHelper.AreSameValue(const AExpected, AActual,
  AEpsilon: Double; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not System.Math.SameValue(AExpected, AActual, AEpsilon) then
    FailFmt(SUnexpectedErrorExt, [AExpected, AActual, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSameValue(const AExpected, AActual,
  AEpsilon: Single; const AMessage: string);
begin
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  if not System.Math.SameValue(AExpected, AActual, AEpsilon) then
    FailFmt(SUnexpectedErrorExt, [AExpected, AActual, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSimilar(const AExpectedHash: string;
  const AActual: ISkPixmap; AMinSimilarity: Double; const AMessage: string);
var
  LActualHash: string;
  LActualImage: ISkImage;
  LSimilarity: Double;
begin
  Assert.IsNotNull(AActual, 'Invalid SkPixmap (nil)');
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  LActualImage := TSkImage.MakeRasterCopy(AActual);
  DoImageChecking(LActualImage);
  LActualHash := TImageHashing.Hash(LActualImage);
  LSimilarity := TImageHashing.Similarity(AExpectedHash, LActualHash);
  if CompareValue(LSimilarity, AMinSimilarity, TEpsilon.Vector) = LessThanValue then
    FailFmt(SImagesNotSimilar, [AMinSimilarity, LSimilarity, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSimilar(const AExpectedHash: string;
  const AActual: ISkCodec; AMinSimilarity: Double; const AMessage: string);
var
  LActualHash: string;
  LActualImage: ISkImage;
  LSimilarity: Double;
begin
  Assert.IsNotNull(AActual, 'Invalid SkCodec (nil)');
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  LActualImage := AActual.GetImage(TSkColorType.BGRA8888, TSkAlphaType.Premul, TSkColorSpace.MakeSRGB);
  DoImageChecking(LActualImage);
  LActualHash := TImageHashing.Hash(LActualImage);
  LSimilarity := TImageHashing.Similarity(AExpectedHash, LActualHash);
  if CompareValue(LSimilarity, AMinSimilarity, TEpsilon.Vector) = LessThanValue then
    FailFmt(SImagesNotSimilar, [AMinSimilarity, LSimilarity, LActualHash, AMessage], ReturnAddress);
end;

class procedure TAssertHelper.AreSimilar(const AExpectedHash: string;
  const AActual: ISkImage; AMinSimilarity: Double; const AMessage: string);
var
  LActualHash: string;
  LSimilarity: Double;
begin
  Assert.IsNotNull(AActual, 'Invalid SkImage (nil)');
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  DoImageChecking(AActual);
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
  {$IF CompilerVersion >= 32}
  DoAssert;
  {$ENDIF}
  DoImageChecking(AActual);
  if AExpected <> AActual then
  begin
    Assert.IsNotNull(AExpected, 'Invalid SkImage (nil)');
    Assert.IsNotNull(AActual, 'Invalid SkImage (nil)');
    LActualHash := TImageHashing.Hash(AActual);
    LSimilarity := TImageHashing.Similarity(LActualHash, AExpected);
    if CompareValue(LSimilarity, AMinSimilarity, TEpsilon.Vector) = LessThanValue then
      FailFmt(SImagesNotSimilar, [AMinSimilarity, LSimilarity, LActualHash, AMessage], ReturnAddress);
  end;
end;

class procedure TAssertHelper.DoImageChecking(const AImage: ISkImage);
begin
  if Assigned(AImage) and Assigned(FOnImageChecking) then
    FOnImageChecking(AImage);
end;

class procedure TAssertHelper.DoImageChecking(const AImageCodec: ISkCodec);
begin
  if Assigned(AImageCodec) and Assigned(FOnImageChecking) then
    DoImageChecking(AImageCodec.GetImage(TSkColorType.BGRA8888));
end;

class procedure TAssertHelper.DoImageChecking(const AImagePixmap: ISkPixmap);
begin
  if Assigned(AImagePixmap) and Assigned(FOnImageChecking) then
    DoImageChecking(TSkImage.MakeFromRaster(AImagePixmap));
end;

class function TAssertHelper.GetPixmapBytes(const APixmap: ISkPixmap): TBytes;
var
  LStream: TBytesStream;
  LIndex: NativeUInt;
  LRow: Integer;
begin
  LStream := TBytesStream.Create;
  try
    LStream.WriteData(Cardinal(APixmap.Width));
    LStream.WriteData(Cardinal(APixmap.Height));
    LStream.WriteData(Cardinal(APixmap.ColorType));
    LStream.WriteData(Cardinal(APixmap.AlphaType));
    if (APixmap.Width > 0) and (APixmap.Height > 0) then
    begin
      if SkBytesPerPixel[APixmap.ColorType] * APixmap.Width * APixmap.Height = NativeInt(APixmap.RowBytes) * APixmap.Height then
        LStream.WriteData(APixmap.Pixels, SkBytesPerPixel[APixmap.ColorType] * APixmap.Width * APixmap.Height)
      else
      begin
        LIndex := 0;
        for LRow := 0 to APixmap.Height - 1 do
        begin
          LStream.WriteData(NativeUInt(APixmap.Pixels) + LIndex, SkBytesPerPixel[APixmap.ColorType] * APixmap.Width);
          Inc(LIndex, APixmap.RowBytes);
        end;
      end;
    end;
    Result := Copy(LStream.Bytes, 0, LStream.Size);
  finally
    LStream.Free;
  end;
end;

{ TSkPathHelper }

class function TSkPathHelper.GetNewTempFileName(AExtension: string): string;
begin
  if (AExtension = '') or (AExtension = '.') then
    AExtension := '.tmp'
  else if not AExtension.StartsWith('.') then
    AExtension := '.' + AExtension;
  repeat
    Result := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName(False) + AExtension);
  until not TFile.Exists(Result);
end;

class function TSkPathHelper.GetNewTempPath: string;
begin
  Result := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName(False) + TPath.DirectorySeparatorChar);
  TDirectory.CreateDirectory(Result);
end;

{$IF CompilerVersion < 29}
{ TSkFormatSettingsHelper }

class constructor TSkFormatSettingsHelper.Create;
begin
  FInvariant := TFormatSettings.Create('en-US');
end;
{$ENDIF}

{$IF CompilerVersion < 29}
{ TSkMatrixHelper }

function TSkMatrixHelper.EqualsTo(const AMatrix: TMatrix; const Epsilon: Single = TEpsilon.Matrix): Boolean;
begin
  Result := SameValue(Self.m11, AMatrix.m11, Epsilon) and SameValue(Self.m12, AMatrix.m12, Epsilon) and
    SameValue(Self.m13, AMatrix.m13, Epsilon) and SameValue(Self.m21, AMatrix.m21, Epsilon) and
    SameValue(Self.m22, AMatrix.m22, Epsilon) and SameValue(Self.m23, AMatrix.m23, Epsilon) and
    SameValue(Self.m31, AMatrix.m31, Epsilon) and SameValue(Self.m32, AMatrix.m32, Epsilon) and
    SameValue(Self.m33, AMatrix.m33, Epsilon);
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
