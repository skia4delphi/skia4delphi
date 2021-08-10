{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia;

interface

{$SCOPEDENUMS ON}

{$INCLUDE sk4d.inc}

uses
  {Delphi}
  System.Classes,
  System.Math.Vectors,
  System.Types,
  System.UITypes,
  {Skia}
  sk4d;

type
  TSKDebugMessageProc = reference to procedure (const AMessage: string);

  { ISkiaObject }

  ISkiaObject = interface
    ['{8A0AF1A7-F425-4E6E-A97E-FA32C37DC0F0}']
  end;

  { TSkiaObject }

  TSkiaObject = class abstract(TInterfacedObject, ISkiaObject)
  strict private class var
    FDebugMessageProc: TSKDebugMessageProc;
  strict private
    FHandle: THandle;
    class constructor Create;
    class destructor Destroy;
    class procedure debug_msg_proc(const msg: MarshaledAString); cdecl; static;
  strict protected
    constructor CreateNative(const AHandle: THandle);
    procedure Dispose; virtual; abstract;
    property Handle: THandle read FHandle;
  public
    procedure BeforeDestruction; override;
    class function GetHandle(const AObject: ISkiaObject): THandle; static; inline;
    class property DebugMessageProc: TSKDebugMessageProc read FDebugMessageProc write FDebugMessageProc;
  end;

  { ISKObject }

  ISKObject = interface(ISkiaObject)
    ['{79EF72CC-2693-4588-829F-B507DA6D0F83}']
  end;

  { TSKObject }

  TSKObject = class abstract(TSkiaObject, ISKObject)
  strict private
    FOwnsHandle: Boolean;
  strict protected
    procedure Dispose; override; final;
    class procedure DoDestroy(const AHandle: THandle); virtual; abstract;
  public
    constructor CreateNative(const AHandle: THandle; const AOwnsHandle: Boolean = True);
  end;

  { ISKReferenceCounted }

  ISKReferenceCounted = interface(ISkiaObject)
    ['{590A0172-EB2C-4762-BAA4-BDE1594A66C6}']
  end;

  { TSKReferenceCounted }

  TSKReferenceCounted = class abstract(TSkiaObject, ISKReferenceCounted)
  strict protected
    procedure Dispose; override; final;
    class procedure DoRef(const AHandle: THandle); virtual;
    class procedure DoUnref(const AHandle: THandle); virtual;
  public
    constructor CreateNative(const AHandle: THandle; const AAlreadyReferenced: Boolean = True);
  end;

  { ISKString }

  ISKString = interface(ISKObject)
    ['{88437646-E5F0-4C53-912F-917F29928730}']
    procedure Append(const ASrc: ISKString);
    function GetText: string;
    function IsEqual(const AString: ISKString): Boolean;
    procedure SetText(const AValue: string);
    property Text: string read GetText write SetText;
  end;

  { TSKString }

  TSKString = class(TSKObject, ISKString)
  strict protected
    procedure Append(const ASrc: ISKString);
    function GetText: string;
    function IsEqual(const AString: ISKString): Boolean;
    procedure SetText(const AValue: string);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const AString: ISKString); overload;
  end;

  { ISKStream }

  ISKStream = interface(ISKObject)
    ['{ACBA8267-D2C8-413C-A1A2-06E2954DBFDB}']
    function Duplicate: ISKStream;
    function Fork: ISKStream;
    function GetLength: NativeUInt;
    function GetMemoryBase: Pointer;
    function GetPosition: NativeUInt;
    function HasLength: Boolean;
    function HasPosition: Boolean;
    function IsAtEnd: Boolean;
    function Move(const AOffset: LongInt): Boolean;
    function Peek(out ABuffer; const ASize: NativeUInt): NativeUInt;
    function Read(out ABuffer; const ASize: NativeUInt): NativeUInt;
    function Rewind: Boolean;
    function Seek(const APosition: NativeUInt): Boolean;
    property Length: NativeUInt read GetLength;
    property MemoryBase: Pointer read GetMemoryBase;
    property Position: NativeUInt read GetPosition;
  end;

  { TSKStream }

  TSKStream = class(TSKObject, ISKStream)
  strict protected
    function Duplicate: ISKStream;
    function Fork: ISKStream;
    function GetLength: NativeUInt;
    function GetMemoryBase: Pointer;
    function GetPosition: NativeUInt;
    function HasLength: Boolean;
    function HasPosition: Boolean;
    function IsAtEnd: Boolean;
    function Move(const AOffset: LongInt): Boolean;
    function Peek(out ABuffer; const ASize: NativeUInt): NativeUInt;
    function Read(out ABuffer; const ASize: NativeUInt): NativeUInt;
    function Rewind: Boolean;
    function Seek(const APosition: NativeUInt): Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { ISKStreamAsset }

  ISKStreamAsset = interface(ISKStream)
    ['{3EAAE979-C079-4A93-A01F-3A1FF517738E}']
  end;

  { TSKStreamAsset }

  TSKStreamAsset = class(TSKStream, ISKStreamAsset);

  { TSKStreamAdapter }

  TSKStreamAdapter = class(TSKStreamAsset)
  strict private
    class constructor Create;
    class function get_length_proc(context: Pointer): size_t; cdecl; static;
    class function get_position_proc(context: Pointer): size_t; cdecl; static;
    class function read_proc(context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl; static;
    class procedure release_proc(context: Pointer); cdecl; static;
    class function seek_proc(context: Pointer; position: size_t): bool; cdecl; static;
  public
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = False);
  end;

  { ISKFileStream }

  ISKFileStream = interface(ISKStreamAsset)
    ['{5E915FA2-7C67-461F-859F-14B96BAFB9AF}']
    function IsValid: Boolean;
  end;

  { TSKFileStream }

  TSKFileStream = class(TSKStreamAsset, ISKFileStream)
  strict protected
    function IsValid: Boolean;
  public
    constructor Create(const AFileName: string);
  end;

  { ISKMemoryStream }

  ISKMemoryStream = interface(ISKStreamAsset)
    ['{8EE4CE53-57F4-46A3-A40E-43BCB72DA401}']
  end;

  ISKData = interface;

  { TSKMemoryStream }

  TSKMemoryStream = class(TSKStreamAsset, ISKMemoryStream)
  public
    constructor Create(const AData: ISKData); overload;
    constructor Create(const AData: Pointer; const ASize: NativeUInt; const ACopy: Boolean = False); overload;
  end;

  { ISKWStream }

  ISKWStream = interface(ISKObject)
    ['{FCF77D49-1CAD-4C4F-988F-5780B7E73CEC}']
    procedure Flush;
    function GetBytesWritten: NativeUInt;
    function Write(const ABuffer; const ASize: NativeUInt): Boolean;
    function WriteStream(const AStream: ISKStream; const ASize: NativeUInt): Boolean;
    property BytesWritten: NativeUInt read GetBytesWritten;
  end;

  { TSKWStream }

  TSKWStream = class(TSKObject, ISKWStream)
  strict protected
    procedure Flush;
    function GetBytesWritten: NativeUInt;
    function Write(const ABuffer; const ASize: NativeUInt): Boolean;
    function WriteStream(const AStream: ISKStream; const ASize: NativeUInt): Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSKWStreamAdapter }

  TSKWStreamAdapter = class(TSKWStream)
  strict private
    class constructor Create;
    class procedure release_proc(context: Pointer); cdecl; static;
    class function write_proc(context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl; static;
  public
    constructor Create(const AStream: TStream; const AOwnsStream: Boolean = False);
  end;

  { ISKDynamicMemoryWStream }

  ISKDynamicMemoryWStream = interface(ISKWStream)
    ['{9B757AA6-F559-4AB4-9E29-52F9CB888BDC}']
    procedure CopyTo(const ADest: Pointer);
    function DetachAsData: ISKData;
    function DetachAsStream: ISKStreamAsset;
  end;

  { TSKDynamicMemoryWStream }

  TSKDynamicMemoryWStream = class(TSKWStream, ISKDynamicMemoryWStream)
  strict protected
    procedure CopyTo(const ADest: Pointer);
    function DetachAsData: ISKData;
    function DetachAsStream: ISKStreamAsset;
  public
    constructor Create;
  end;

  { ISKFileWStream }

  ISKFileWStream = interface(ISKWStream)
    ['{948A1BF6-BD77-4EA6-B205-E1A99A9017F1}']
    function IsValid: Boolean;
  end;

  { TSKFileWStream }

  TSKFileWStream = class(TSKWStream, ISKFileWStream)
  strict protected
    function IsValid: Boolean;
  public
    constructor Create(const AFileName: string);
  end;

  TSKDataReleaseProc = reference to procedure (const AData: Pointer);

  { ISKData }

  ISKData = interface(ISKReferenceCounted)
    ['{8EAFD037-E378-4B42-A3AF-0B5E9826CBD3}']
    function GetData: Pointer;
    function GetSize: NativeUInt;
    property Data: Pointer read GetData;
    property Size: NativeUInt read GetSize;
  end;

  { TSKData }

  TSKData = class(TSKReferenceCounted, ISKData)
  strict private
    class procedure release_proc(const data: Pointer; context: Pointer); cdecl; static;
  strict protected
    function GetData: Pointer;
    function GetSize: NativeUInt;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AData: Pointer; const ASize: NativeUInt; const AReleaseProc: TSKDataReleaseProc = nil): ISKData; static;
    class function MakeEmpty: ISKData; static;
    class function MakeFromFile(const AFileName: string): ISKData; static;
    class function MakeFromStream(const AStream: ISKStream; const ASize: NativeUInt): ISKData; static;
    class function MakeUninitialized(const ASize: NativeUInt): ISKData; static;
    class function MakeWithCopy(const AData: Pointer; const ASize: NativeUInt): ISKData; static;
  end;

  { TSKColorSpaceMatrix33 }

  TSKColorSpaceMatrix33 = record
    class function AdobeRGBGamut: TSKColorSpaceMatrix33; static;
    class function DisplayP3Gamut: TSKColorSpaceMatrix33; static;
    class function Rec2020Gamut: TSKColorSpaceMatrix33; static;
    class function SRGBGamut: TSKColorSpaceMatrix33; static;
    class function XYZGamut: TSKColorSpaceMatrix33; static;
    case Integer of
      0: (Values: array[0..2] of array[0..2] of Single);
      1: (M11, M12, M13, M21, M22, M23, M31, M32, M33: Single);
  end;

  { TSKColorSpaceMatrix34 }

  TSKColorSpaceMatrix34 = record
    case Integer of
      0: (Values: array[0..2] of array[0..3] of Single);
      1: (M11, M12, M13, M14, M21, M22, M23, M24, M31, M32, M33, M34: Single);
  end;

  { TSKColorSpacePrimaries }

  TSKColorSpacePrimaries = record
    function GetToXYZD50(out ADest: TSKColorSpaceMatrix33): Boolean;
    case Integer of
      0: (Values: array[0..7] of Single);
      1: (RX, RY, GX, GY, BX, BY, WX, WY: Single);
  end;

  { TSKColorSpaceTransferFunction }

  TSKColorSpaceTransferFunction = record
    class function HLG: TSKColorSpaceTransferFunction; static;
    class function Linear: TSKColorSpaceTransferFunction; static;
    class function PQ: TSKColorSpaceTransferFunction; static;
    class function Rec2020: TSKColorSpaceTransferFunction; static;
    class function SRGB: TSKColorSpaceTransferFunction; static;
    class function TwoDotTwo: TSKColorSpaceTransferFunction; static;
    case Integer of
      0: (Values: array[0..6] of Single);
      1: (G, A, B, C, D, E, F: Single);
  end;

  { TSKColorSpaceCurve }

  TSKColorSpaceCurve = record
    case Integer of
      0: (AliasOfTableEntries: Cardinal; Parametric: TSKColorSpaceTransferFunction);
      1: (TableEntries: Cardinal; Table8: PByte; Table16: PByte);
  end;

  { TSKColorSpaceA2B }

  TSKColorSpaceA2B = record
    InputChannels: Cardinal;
    InputCurves: array[0..3] of TSKColorSpaceCurve;
    GridPoints: array[0..3] of Byte;
    Grid8: PByte;
    Grid16: PByte;
    MatrixChannels: Cardinal;
    MatrixCurves: array[0..2] of TSKColorSpaceCurve;
    Matrix: TSKColorSpaceMatrix34;
    OutputChannels: Cardinal;
    OutputCurves: array[0..2] of TSKColorSpaceCurve;
  end;

  { TSKColorSpaceB2A }

  TSKColorSpaceB2A = record
    InputChannels: Cardinal;
    InputCurves: array[0..2] of TSKColorSpaceCurve;
    MatrixChannels: Cardinal;
    Matrix: TSKColorSpaceMatrix34;
    MatrixCurves: array[0..2] of TSKColorSpaceCurve;
    OutputChannels: Cardinal;
    GridPoints: array[0..3] of Byte;
    Grid8: PByte;
    Grid16: PByte;
    OutputCurves: array[0..3] of TSKColorSpaceCurve;
  end;

  { TSKColorSpaceICCProfile }

  TSKColorSpaceICCProfile = record
    Buffer: PByte;
    Size: Cardinal;
    DataColorSpace: Cardinal;
    Pcs: Cardinal;
    TagCount: Cardinal;
    HasTrc: Boolean;
    Trc: array[0..2] of TSKColorSpaceCurve;
    HasToXYZD50: Boolean;
    ToXYZD50: TSKColorSpaceMatrix33;
    HasA2B: Boolean;
    A2B: TSKColorSpaceA2B;
    HasB2A: Boolean;
    B2A: TSKColorSpaceB2A;
  end;

  { ISKColorSpace }

  ISKColorSpace = interface(ISKReferenceCounted)
    ['{8D2F2435-57C9-4E3D-BC0B-979C436EF563}']
    function GetGammaCloseToSRGB: Boolean;
    function GetGammaIsLinear: Boolean;
    function GetGamut(const ADest: ISKColorSpace): TSKColorSpaceMatrix33;
    function GetInverseTransferFunction: TSKColorSpaceTransferFunction;
    function GetToXYZD50: TSKColorSpaceMatrix33;
    function GetTransferFunction: TSKColorSpaceTransferFunction;
    function IsEqual(const AColorSpace: ISKColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSKColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeColorSpin: ISKColorSpace;
    function MakeLinearGamma: ISKColorSpace;
    function MakeSRGBGamma: ISKColorSpace;
    function ToProfile: TSKColorSpaceICCProfile;
    property GammaCloseToSRGB: Boolean read GetGammaCloseToSRGB;
    property GammaIsLinear: Boolean read GetGammaIsLinear;
    property InverseTransferFunction: TSKColorSpaceTransferFunction read GetInverseTransferFunction;
    property ToXYZD50: TSKColorSpaceMatrix33 read GetToXYZD50;
    property TransferFunction: TSKColorSpaceTransferFunction read GetTransferFunction;
  end;

  { TSKColorSpace }

  TSKColorSpace = class(TSKReferenceCounted, ISKColorSpace)
  strict protected
    function GetGammaCloseToSRGB: Boolean;
    function GetGammaIsLinear: Boolean;
    function GetGamut(const ADest: ISKColorSpace): TSKColorSpaceMatrix33;
    function GetInverseTransferFunction: TSKColorSpaceTransferFunction;
    function GetToXYZD50: TSKColorSpaceMatrix33;
    function GetTransferFunction: TSKColorSpaceTransferFunction;
    function IsEqual(const AColorSpace: ISKColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSKColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeColorSpin: ISKColorSpace;
    function MakeLinearGamma: ISKColorSpace;
    function MakeSRGBGamma: ISKColorSpace;
    function ToProfile: TSKColorSpaceICCProfile;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AProfile: TSKColorSpaceICCProfile): ISKColorSpace; static;
    class function MakeRGB(const ATransferFunction: TSKColorSpaceTransferFunction; const AToXyzD50: TMatrix): ISKColorSpace; static;
    class function MakeSRGB: ISKColorSpace; static;
    class function MakeSRGBLinear: ISKColorSpace; static;
  end;

  TSKAlphaType = (Unknown, Opaque, Premul, Unpremul);

  TSKColorType = (Unknown, Alpha8, RGB565, ARGB4444, RGBA8888, RGB888X,
    BGRA8888, RGBA1010102, BGRA1010102, RGB101010X, BGR101010X, Gray8, RGBAF16,
    RGBAF16Clamped, RGBAF32, RG88, AlphaF16, RgF16, Alpha16, RG1616,
    RGBA16161616);

  { TSKImageInfo }

  TSKImageInfo = record
    Width: Integer;
    Height: Integer;
    ColorType: TSKColorType;
    AlphaType: TSKAlphaType;
    ColorSpace: ISKColorSpace;
    constructor Create(const AWidth, AHeight: Integer; const AColorType: TSKColorType = {$IFDEF BIGENDIAN}TSKColorType.RGBA8888{$ELSE}TSKColorType.BGRA8888{$ENDIF}; const AAlphaType: TSKAlphaType = TSKAlphaType.Premul; const AColorSpace: ISKColorSpace = nil);
    function ByteSize(const ARowBytes: NativeUInt): NativeUInt; inline;
    function BytesPerPixel: Integer; inline;
    function IsEmpty: Boolean;
    function IsOpaque: Boolean;
    function IsValid: Boolean;
    function IsValidRowBytes(const ARowBytes: NativeUInt): Boolean;
    function MakeAlphaType(const AAlphaType: TSKAlphaType): TSKImageInfo;
    function MakeColorSpace(const AColorSpace: ISKColorSpace): TSKImageInfo;
    function MakeColorType(const AColorType: TSKColorType): TSKImageInfo;
    function MakeDimensions(const AWidth, AHeight: Integer): TSKImageInfo;
    function MinByteSize: NativeUInt; inline;
    function MinRowBytes: NativeUInt; inline;
    function ShiftPerPixel: Integer; inline;
    class operator Equal(const AImageInfo1, AImageInfo2: TSKImageInfo): Boolean;
    class operator Explicit(const AImageInfo: sk_imageinfo_t): TSKImageInfo;
    class operator Explicit(const AImageInfo: TSKImageInfo): sk_imageinfo_t;
    class operator NotEqual(const AImageInfo1, AImageInfo2: TSKImageInfo): Boolean;
  end;

  { TSKCubicResampler }

  TSKCubicResampler = record
    B: Single;
    C: Single;
    class function CatmullRom: TSKCubicResampler; static; inline;
    class function Mitchell: TSKCubicResampler; static; inline;
    class operator Equal(const ACubicResampler1, ACubicResampler2: TSKCubicResampler): Boolean;
    class operator NotEqual(const ACubicResampler1, ACubicResampler2: TSKCubicResampler): Boolean;
  end;

  TSKFilterMode = (Nearest, Linear);

  TSKMipmapMode = (None, Nearest, Linear);

  { TSKSamplingOptions }

  TSKSamplingOptions = record
    constructor Create(const ACubic: TSKCubicResampler); overload;
    constructor Create(const AFilter: TSKFilterMode; const AMipmap: TSKMipmapMode); overload;
    class function Low: TSKSamplingOptions; static; inline;
    class function Medium: TSKSamplingOptions; static; inline;
    class function High: TSKSamplingOptions; static; inline;
    class operator Equal(const ASamplingOptions1, ASamplingOptions2: TSKSamplingOptions): Boolean;
    class operator Explicit(const ASamplingOptions: TSKSamplingOptions): sk_samplingoptions_t;
    class operator NotEqual(const ASamplingOptions1, ASamplingOptions2: TSKSamplingOptions): Boolean;
    case UseCubic: Boolean of
      True : (Cubic: TSKCubicResampler);
      False: (Filter: TSKFilterMode; Mipmap: TSKMipmapMode);
  end;

  { ISKPixmap }

  ISKPixmap = interface(ISKObject)
    ['{F1B971BE-6D20-4EA7-9EE5-21771E1F86DF}']
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const AColorSpace: ISKColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; const AColorSpace: ISKColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISKPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSKAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorSpace: ISKColorSpace;
    function GetColorType: TSKColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetPixelAddr(const AX, AY: Integer): Pointer;
    function GetPixels: Pointer;
    function GetRowBytes: NativeUInt;
    function GetWidth: Integer;
    function ReadPixels(const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ScalePixels(const ADest: ISKPixmap; const ASampling: TSKSamplingOptions): Boolean;
    procedure SetColorSpace(const AValue: ISKColorSpace);
    property Alphas[const AX, AY: Integer]: Single read GetAlpha;
    property AlphaType: TSKAlphaType read GetAlphaType;
    property Colors[const AX, AY: Integer]: TAlphaColor read GetColor;
    property ColorSpace: ISKColorSpace read GetColorSpace write SetColorSpace;
    property ColorType: TSKColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSKImageInfo read GetImageInfo;
    property PixelAddr[const AX, AY: Integer]: Pointer read GetPixelAddr;
    property Pixels: Pointer read GetPixels;
    property RowBytes: NativeUInt read GetRowBytes;
    property Width: Integer read GetWidth;
  end;

  { TSKPixmap }

  TSKPixmap = class(TSKObject, ISKPixmap)
  protected
    constructor Create; overload;
  strict protected
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const AColorSpace: ISKColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; const AColorSpace: ISKColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISKPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSKAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorSpace: ISKColorSpace;
    function GetColorType: TSKColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetPixelAddr(const AX, AY: Integer): Pointer;
    function GetPixels: Pointer;
    function GetRowBytes: NativeUInt;
    function GetWidth: Integer;
    function ReadPixels(const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ScalePixels(const ADest: ISKPixmap; const ASampling: TSKSamplingOptions): Boolean;
    procedure SetColorSpace(const AValue: ISKColorSpace);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AImageInfo: TSKImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt); overload;
  end;

  TSKRoundRectCorner = (UpperLeft, UpperRight, LowerRight, LowerLeft);

  TSKRoundRectRadii = array[TSKRoundRectCorner] of Single;

  TSKRoundRectType = (Empty, Rect, Oval, Simple, Nine, Complex);

  { ISKRoundRect }

  ISKRoundRect = interface(ISKObject)
    ['{574DC656-4D76-4576-B7E2-E7A27D73B01F}']
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADX, ADY: Single);
    function GetDeflate(const ADX, ADY: Single): ISKRoundRect;
    function GetHeight: Single;
    function GetInflate(const ADX, ADY: Single): ISKRoundRect;
    function GetOffset(const ADX, ADY: Single): ISKRoundRect;
    function GetRadii(const ACorner: TSKRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetRoundRectType: TSKRoundRectType;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADX, ADY: Single);
    function IsEqual(const ARoundRect: ISKRoundRect): Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADX, ADY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSKRoundRectRadii); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    function Transform(const AMatrix: TMatrix): ISKRoundRect;
    property Height: Single read GetHeight;
    property Radii[const ACorner: TSKRoundRectCorner]: TPointF read GetRadii;
    property Rect: TRectF read GetRect;
    property RoundRectType: TSKRoundRectType read GetRoundRectType;
    property SimpleRadii: TPointF read GetSimpleRadii;
    property Width: Single read GetWidth;
  end;

  { TSKRoundRect }

  TSKRoundRect = class(TSKObject, ISKRoundRect)
  strict protected
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADX, ADY: Single);
    function GetDeflate(const ADX, ADY: Single): ISKRoundRect;
    function GetHeight: Single;
    function GetInflate(const ADX, ADY: Single): ISKRoundRect;
    function GetOffset(const ADX, ADY: Single): ISKRoundRect;
    function GetRadii(const ACorner: TSKRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetRoundRectType: TSKRoundRectType;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADX, ADY: Single);
    function IsEqual(const ARoundRect: ISKRoundRect): Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADX, ADY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSKRoundRectRadii); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    function Transform(const AMatrix: TMatrix): ISKRoundRect;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARoundRect: ISKRoundRect); overload;
    constructor Create(const ARect: TRectF; const ARadii: TSKRoundRectRadii); overload;
    constructor Create(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
  end;

  ISKPath = interface;

  TSKPathPoints = array[0..3] of TPointF;

  TSKPathVerb = (Move, Line, Quad, Conic, Cubic, Close);

  { ISKPathIterator }

  ISKPathIterator = interface(ISKObject)
    ['{E9155397-BA59-4720-BB32-2AAE42B8AE23}']
    function GetConicWeight: Single;
    function GetPoints: TSKPathPoints;
    function GetVerb: TSKPathVerb;
    function Next: Boolean;
    property ConicWeight: Single read GetConicWeight;
    property Points: TSKPathPoints read GetPoints;
    property Verb: TSKPathVerb read GetVerb;
  end;

  { TSKPathIterator }

  TSKPathIterator = class(TSKObject, ISKPathIterator)
  strict private
    FPoints: TSKPathPoints;
    FVerb: TSKPathVerb;
  strict protected
    function GetConicWeight: Single;
    function GetPoints: TSKPathPoints;
    function GetVerb: TSKPathVerb;
    function Next: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const APath: ISKPath; const AForceClose: Boolean);
  end;

  TSKPathFillType = (Winding, EvenOdd, InverseWinding, InverseEvenOdd);

  TSKPathOp = (Difference, Intersect, Union, Eor, ReverseDifference);

  TSKSegmentMask  = (Line, Quad, Conic, Cubic);
  TSKSegmentMasks = set of TSKSegmentMask;

  { ISKPath }

  ISKPath = interface(ISKObject)
    ['{3657A42C-09A2-48AC-A351-4D65DB4D242B}']
    function Contains(const AX, AY: Single): Boolean;
    function GetBounds: TRectF;
    function GetFillType: TSKPathFillType;
    function GetIterator(const AForceClose: Boolean): ISKPathIterator;
    function GetLastPoint: TPointF;
    function GetSegmentMasks: TSKSegmentMasks;
    function GetTightBounds: TRectF;
    function Interpolate(const AEnding: ISKPath; const AWeight: Single): ISKPath;
    function IsConvex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const APath: ISKPath): Boolean;
    function IsFinite: Boolean;
    function IsInterpolatable(const AOther: ISKPath): Boolean;
    function IsLastContourClosed: Boolean;
    function IsLine: Boolean; overload;
    function IsLine(out APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISKRoundRect): Boolean; overload;
    function Offset(const ADX, ADY: Single): ISKPath;
    function Op(const APath: ISKPath; const AOp: TSKPathOp): ISKPath;
    function ToSVG: ISKString;
    function Transform(const AMatrix: TMatrix): ISKPath;
    property Bounds: TRectF read GetBounds;
    property FillType: TSKPathFillType read GetFillType;
    property LastPoint: TPointF read GetLastPoint;
    property SegmentMasks: TSKSegmentMasks read GetSegmentMasks;
    property TightBounds: TRectF read GetTightBounds;
  end;

  { TSKPath }

  TSKPath = class(TSKObject, ISKPath)
  protected
    constructor Create; overload;
  strict protected
    function Contains(const AX, AY: Single): Boolean;
    function GetBounds: TRectF;
    function GetFillType: TSKPathFillType;
    function GetIterator(const AForceClose: Boolean): ISKPathIterator;
    function GetLastPoint: TPointF;
    function GetSegmentMasks: TSKSegmentMasks;
    function GetTightBounds: TRectF;
    function Interpolate(const AEnding: ISKPath; const AWeight: Single): ISKPath;
    function IsConvex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const APath: ISKPath): Boolean;
    function IsFinite: Boolean;
    function IsInterpolatable(const APath: ISKPath): Boolean;
    function IsLastContourClosed: Boolean;
    function IsLine: Boolean; overload;
    function IsLine(out APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISKRoundRect): Boolean; overload;
    function Offset(const ADX, ADY: Single): ISKPath;
    function Op(const APath: ISKPath; const AOp: TSKPathOp): ISKPath;
    function ToSVG: ISKString;
    function Transform(const AMatrix: TMatrix): ISKPath;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const APath: ISKPath); overload;
    constructor Create(const ASVG: string); overload;
  end;

  { ISKOpBuilder }

  ISKOpBuilder = interface(ISKObject)
    ['{12828A55-1038-4307-B9E6-48C5A580489D}']
    procedure Add(const APath: ISKPath; const AOp: TSKPathOp);
    function Detach: ISKPath;
  end;

  { TSKOpBuilder }

  TSKOpBuilder = class(TSKObject, ISKOpBuilder)
  strict protected
    procedure Add(const APath: ISKPath; const AOp: TSKPathOp);
    function Detach: ISKPath;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSKPathMeasureMatrixFlag  = (Position, Tangent);
  TSKPathMeasureMatrixFlags = set of TSKPathMeasureMatrixFlag;

  { ISKPathMeasure }

  ISKPathMeasure = interface(ISKObject)
    ['{9AB42911-F5C4-4953-A1C5-3313205D8C84}']
    function GetLength: Single;
    function GetMatrix(const ADistance: Single; out AMatrix: TMatrix; const AMatrixFlags: TSKPathMeasureMatrixFlags = [TSKPathMeasureMatrixFlag.Position, TSKPathMeasureMatrixFlag.Tangent]): Boolean;
    function GetPositionAndTangent(const ADistance: Single; out APosition, ATangent: TPointF): Boolean;
    function GetSegment(const AStart, AStop: Single; const AStartWithMoveTo: Boolean): ISKPath;
    function IsClosed: Boolean;
    function NextContour: Boolean;
    property Length: Single read GetLength;
  end;

  { TSKPathMeasure }

  TSKPathMeasure = class(TSKObject, ISKPathMeasure)
  strict protected
    function GetLength: Single;
    function GetMatrix(const ADistance: Single; out AMatrix: TMatrix; const AMatrixFlags: TSKPathMeasureMatrixFlags = [TSKPathMeasureMatrixFlag.Position, TSKPathMeasureMatrixFlag.Tangent]): Boolean;
    function GetPositionAndTangent(const ADistance: Single; out APosition, ATangent: TPointF): Boolean;
    function GetSegment(const AStart, AStop: Single; const AStartWithMoveTo: Boolean): ISKPath;
    function IsClosed: Boolean;
    function NextContour: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const APath: ISKPath; const AForceClosed: Boolean = False; const AResScale: Single = 1);
  end;

  TSKPathEffect1DStyle = (Translate, Rotate, Morph);

  TSKPathEffectTrimMode = (Normal, Inverted);

  { ISKPathEffect }

  ISKPathEffect = interface(ISKReferenceCounted)
    ['{0B5D608D-2B26-45CE-8C1E-BB5CA1293E5F}']
  end;

  { TSKPathEffect }

  TSKPathEffect = class(TSKReferenceCounted, ISKPathEffect)
  public
    class function Make1DPath(const APath: ISKPath; const AAdvance, APhase: Single; const AStyle: TSKPathEffect1DStyle): ISKPathEffect; static;
    class function Make2DLine(const AWidth: Single; const AMatrix: TMatrix): ISKPathEffect; static;
    class function Make2DPath(const AMatrix: TMatrix; const APath: ISKPath): ISKPathEffect; static;
    class function MakeCompose(const AOuter, AInner: ISKPathEffect): ISKPathEffect; static;
    class function MakeCorner(const ARadius: Single): ISKPathEffect; static;
    class function MakeDash(const AIntervals: TArray<Single>; const APhase: Single): ISKPathEffect; static;
    class function MakeDiscrete(const ASegLength, ADeviation: Single; const ASeedAssist: Cardinal = 0): ISKPathEffect; static;
    class function MakeSum(const AEffect1, AEffect2: ISKPathEffect): ISKPathEffect; static;
    class function MakeTrim(const AStart, AStop: Single; const AMode: TSKPathEffectTrimMode): ISKPathEffect; static;
  end;

  TSKPathArcSize = (Small, Large);

  TSKPathDirection = (CW, CCW);

  { ISKPathBuilder }

  ISKPathBuilder = interface(ISKObject)
    ['{1B8AC10A-F2B2-4A93-A149-7F7854473F7B}']
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPath(const APath: ISKPath);
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISKRoundRect; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISKRoundRect; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSKPathArcSize; const ASweep: TSKPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISKPath;
    function GetBounds: TRectF;
    function GetFillType: TSKPathFillType;
    procedure IncReserve(const AExtraPointCount, AExtraVerbCount: Integer);  overload;
    procedure IncReserve(const AExtraPointCount: Integer); overload;
    procedure LineTo(const APoint: TPointF); overload;
    procedure LineTo(const AX, AY: Single); overload;
    procedure MoveTo(const APoint: TPointF); overload;
    procedure MoveTo(const AX, AY: Single); overload;
    procedure Offset(const ADX, ADY: Single);
    procedure PolylineTo(const APoints: TArray<TPointF>);
    procedure QuadTo(const APoint1, APoint2: TPointF); overload;
    procedure QuadTo(const AX1, AY1, AX2, AY2: Single); overload;
    procedure RConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure RConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure RCubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure RCubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    procedure Reset;
    procedure RLineTo(const APoint: TPointF); overload;
    procedure RLineTo(const AX, AY: Single); overload;
    procedure RQuadTo(const APoint1, APoint2: TPointF); overload;
    procedure RQuadTo(const AX1, AY1, AX2, AY2: Single); overload;
    procedure SetFillType(const AValue: TSKPathFillType);
    function Snapshot: ISKPath;
    procedure ToggleInverseFillType;
    property Bounds: TRectF read GetBounds;
    property FillType: TSKPathFillType read GetFillType write SetFillType;
  end;

  { TSKPathBuilder }

  TSKPathBuilder = class(TSKObject, ISKPathBuilder)
  strict protected
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPath(const APath: ISKPath);
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISKRoundRect; ADirection: TSKPathDirection = TSKPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISKRoundRect; ADirection: TSKPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSKPathArcSize; const ASweep: TSKPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISKPath;
    function GetBounds: TRectF;
    function GetFillType: TSKPathFillType;
    procedure IncReserve(const AExtraPointCount, AExtraVerbCount: Integer); overload;
    procedure IncReserve(const AExtraPointCount: Integer); overload;
    procedure LineTo(const APoint: TPointF); overload;
    procedure LineTo(const AX, AY: Single); overload;
    procedure MoveTo(const APoint: TPointF); overload;
    procedure MoveTo(const AX, AY: Single); overload;
    procedure Offset(const ADX, ADY: Single);
    procedure PolylineTo(const APoints: TArray<TPointF>);
    procedure QuadTo(const APoint1, APoint2: TPointF); overload;
    procedure QuadTo(const AX1, AY1, AX2, AY2: Single); overload;
    procedure RConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure RConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure RCubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure RCubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    procedure Reset;
    procedure RLineTo(const APoint: TPointF); overload;
    procedure RLineTo(const AX, AY: Single); overload;
    procedure RQuadTo(const APoint1, APoint2: TPointF); overload;
    procedure RQuadTo(const AX1, AY1, AX2, AY2: Single); overload;
    procedure SetFillType(const AValue: TSKPathFillType);
    function Snapshot: ISKPath;
    procedure ToggleInverseFillType;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APathBuilder: ISKPathBuilder); overload;
    constructor Create(const AFillType: TSKPathFillType); overload;
  end;

  ISKRegion = interface;

  { ISKRegionCliperator }

  ISKRegionCliperator = interface(ISKObject)
    ['{084158F8-8DFE-488C-A927-265420C99F17}']
    function GetRect: TRect;
    function Next: Boolean;
    property Rect: TRect read GetRect;
  end;

  { TSKRegionCliperator }

  TSKRegionCliperator = class(TSKObject, ISKRegionCliperator)
  strict protected
    function GetRect: TRect;
    function Next: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const ARegion: ISKRegion; const AClip: TRect);
  end;

  { ISKRegionIterator }

  ISKRegionIterator = interface(ISKObject)
    ['{57DE4BCE-9868-4E23-A45E-8148BE1F943A}']
    function GetRect: TRect;
    function Next: Boolean;
    function Rewind: Boolean;
    property Rect: TRect read GetRect;
  end;

  { TSKRegionIterator }

  TSKRegionIterator = class(TSKObject, ISKRegionIterator)
  strict protected
    function GetRect: TRect;
    function Next: Boolean;
    function Rewind: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const ARegion: ISKRegion);
  end;

  { ISKRegionSpanerator }

  ISKRegionSpanerator = interface(ISKObject)
    ['{ECCD38EF-1436-4E64-A98F-3F96B847EF0E}']
    function GetLeft: Integer;
    function GetRight: Integer;
    function Next: Boolean;
    property Left: Integer read GetLeft;
    property Right: Integer read GetRight;
  end;

  { TSKRegionSpanerator }

  TSKRegionSpanerator = class(TSKObject, ISKRegionSpanerator)
  strict private
    FLeft: Integer;
    FRight: Integer;
  strict protected
    function GetLeft: Integer;
    function GetRight: Integer;
    function Next: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const ARegion: ISKRegion; const AY, ALeft, ARight: Integer);
  end;

  TSKRegionOp = (Difference, Intersect, Union, Eor, ReverseDifference, Replace);

  { ISKRegion }

  ISKRegion = interface(ISKObject)
    ['{67F92B11-BF86-4975-A9D3-1174477C1018}']
    function Contains(const ARegion: ISKRegion): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const AX, AY: Integer): Boolean; overload;
    function GetBoundaryPath: ISKPath;
    function GetBounds: TRectF;
    function GetCliperator(const AClip: TRect): ISKRegionCliperator;
    function GetIterator: ISKRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISKRegionSpanerator;
    function GetTranslate(const AX, AY: Integer): ISKRegion;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISKRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISKRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARegion: ISKRegion; const AOp: TSKRegionOp): Boolean; overload;
    function Op(const ARect: TRect; const AOp: TSKRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARegion: ISKRegion): Boolean; overload;
    function QuickReject(const ARect: TRect): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISKPath; const AClip: ISKRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const AX, AY: Integer);
    property Bounds: TRectF read GetBounds;
  end;

  { TSKRegion }

  TSKRegion = class(TSKObject, ISKRegion)
  strict protected
    function Contains(const ARegion: ISKRegion): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const AX, AY: Integer): Boolean; overload;
    function GetBoundaryPath: ISKPath;
    function GetBounds: TRectF;
    function GetCliperator(const AClip: TRect): ISKRegionCliperator;
    function GetIterator: ISKRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISKRegionSpanerator;
    function GetTranslate(const AX, AY: Integer): ISKRegion;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISKRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISKRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARegion: ISKRegion; const AOp: TSKRegionOp): Boolean; overload;
    function Op(const ARect: TRect; const AOp: TSKRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARegion: ISKRegion): Boolean; overload;
    function QuickReject(const ARect: TRect): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISKPath; const AClip: ISKRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const AX, AY: Integer);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARegion: ISKRegion); overload;
    constructor Create(const ARect: TRect); overload;
  end;

  { ISKTraceMemoryDump }

  ISKTraceMemoryDump = interface(ISKObject)
    ['{48706501-F5E3-4967-94A3-9DEE7731FB64}']
  end;

  { TSKTraceMemoryDump }

  TSKTraceMemoryDump = class abstract(TSKObject, ISKTraceMemoryDump);

  { TSKTraceMemoryDumpBaseClass }

  TSKTraceMemoryDumpBaseClass = class abstract(TSKTraceMemoryDump)
  strict private
    class constructor Create;
    class procedure dump_numeric_value_proc(context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl; static;
    class procedure dump_string_value_proc(context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl; static;
  strict protected
    procedure DoDumpNumericValue(const ADumpName, AValueName, AUnits: string; const AValue: UInt64); virtual; abstract;
    procedure DoDumpStringValue(const ADumpName, AValueName, AValue: string); virtual; abstract;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const ADetailedDump, ADumpWrappedObjects: Boolean);
  end;

  GRGLenum = Cardinal;
  GRGLuint = Cardinal;

  { TGRGLFramebufferInfo }

  TGRGLFramebufferInfo = record
    FBOID: GRGLuint;
    Format: GRGLenum;
    constructor Create(const AFBOID: GRGLuint; const AFormat: GRGLenum);
  end;

  { TGROpenGLTextureInfo }

  TGRGLTextureInfo = record
    Target: GRGLenum;
    ID: GRGLuint;
    Format: GRGLenum;
    constructor Create(const ATarget: GRGLenum; const AID: GRGLuint; const AFormat: GRGLenum);
  end;

  TGRGLGetProc = reference to function (const AName: string): Pointer;

  { IGRGLInterface }

  IGRGLInterface = interface(ISKReferenceCounted)
    ['{4A06112D-77CC-4691-B02D-B44487A2DA5A}']
    function HasExtension(const AName: string): Boolean;
    function Validate: Boolean;
  end;

  { TGRGLInterface }

  TGRGLInterface = class(TSKReferenceCounted, IGRGLInterface)
  strict private
    class function get_proc(context: Pointer; const name: MarshaledAString): Pointer; cdecl; static;
  strict protected
    function HasExtension(const AName: string): Boolean;
    function Validate: Boolean;
  public
    class function MakeAssembled(const AProc: TGRGLGetProc): IGRGLInterface; static;
    class function MakeAssembledGL(const AProc: TGRGLGetProc): IGRGLInterface; static;
    class function MakeAssembledGLES(const AProc: TGRGLGetProc): IGRGLInterface; static;
    class function MakeAssembledWebGL(const AProc: TGRGLGetProc): IGRGLInterface; static;
  end;

  TGRMTLHandle = Pointer;

  { TGRMTLBackendContext }

  TGRMTLBackendContext = record
    Device: TGRMTLHandle;
    Queue: TGRMTLHandle;
    constructor Create(const ADevice, AQueue: TGRMTLHandle);
  end;

  { TGRMTLTextureInfo }

  TGRMTLTextureInfo = record
    Texture: TGRMTLHandle;
    constructor Create(const ATexture: TGRMTLHandle);
  end;

  TGRBackendAPI = (OpenGL, Metal = 2);

  { IGRDirectContext }

  IGRDirectContext = interface(ISKReferenceCounted)
    ['{D653DA23-5529-4A45-B2A2-446C8CA519F3}']
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISKTraceMemoryDump);
    procedure Flush;
    procedure FreeGPUResources;
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSKColorType): Integer;
    function GetResourceCacheLimit: NativeUInt;
    procedure GetResourceCacheUsage(out AMaxResources: Integer; out AMaxResourcesBytes: NativeUInt);
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure PurgeUnlockedResources(const ScratchResourcesOnly: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext; overload;
    procedure ResetContext(const AState: Cardinal); overload;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
    property ResourceCacheLimit: NativeUInt read GetResourceCacheLimit write SetResourceCacheLimit;
  end;

  { TGRDirectContext }

  TGRDirectContext = class(TSKReferenceCounted, IGRDirectContext)
  strict protected
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISKTraceMemoryDump);
    procedure Flush;
    procedure FreeGPUResources;
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSKColorType): Integer;
    function GetResourceCacheLimit: NativeUInt;
    procedure GetResourceCacheUsage(out AMaxResources: Integer; out AMaxResourcesBytes: NativeUInt);
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure PurgeUnlockedResources(const AScratchResourcesOnly: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext; overload;
    procedure ResetContext(const AState: Cardinal); overload;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
  public
    class function MakeGL(const AInterface: IGRGLInterface = nil): IGRDirectContext; static;
    class function MakeMetal(const ABackendContext: TGRMTLBackendContext): IGRDirectContext; static;
  end;

  { IGRBackendRenderTarget }

  IGRBackendRenderTarget = interface(ISKObject)
    ['{DDD32021-9311-466D-9422-B081E3D6AF1C}']
    function GetBackendAPI: TGRBackendAPI;
    function GetGLFramebufferInfo(out AFramebufferInfo: TGRGLFramebufferInfo): Boolean;
    function GetHeight: Integer;
    function GetSampleCount: Integer;
    function GetStencilBits: Integer;
    function GetWidth: Integer;
    function IsValid: Boolean;
    property BackendAPI: TGRBackendAPI read GetBackendAPI;
    property Height: Integer read GetHeight;
    property SampleCount: Integer read GetSampleCount;
    property StencilBits: Integer read GetStencilBits;
    property Width: Integer read GetWidth;
  end;

  { TGRBackendRenderTarget }

  TGRBackendRenderTarget = class(TSKObject, IGRBackendRenderTarget)
  strict protected
    function GetBackendAPI: TGRBackendAPI;
    function GetGLFramebufferInfo(out AFramebufferInfo: TGRGLFramebufferInfo): Boolean;
    function GetHeight: Integer;
    function GetSampleCount: Integer;
    function GetStencilBits: Integer;
    function GetWidth: Integer;
    function IsValid: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor CreateGL(const AWidth, AHeight, ASampleCount, AStencilBits: Integer; const AFramebufferInfo: TGRGLFramebufferInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const ATextureInfo: TGRMTLTextureInfo);
  end;

  { IGRBackendTexture }

  IGRBackendTexture = interface(ISKObject)
    ['{BD83BB9C-6D1A-4458-8A91-EB35B73A0DA0}']
    function GetBackendAPI: TGRBackendAPI;
    function GetGLTextureInfo(out ATextureInfo: TGRGLTextureInfo): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasMipmaps: Boolean;
    function IsValid: Boolean;
    property BackendAPI: TGRBackendAPI read GetBackendAPI;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TGRBackendTexture }

  TGRBackendTexture = class(TSKObject, IGRBackendTexture)
  strict protected
    function GetBackendAPI: TGRBackendAPI;
    function GetGLTextureInfo(out ATextureInfo: TGRGLTextureInfo): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasMipmaps: Boolean;
    function IsValid: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor CreateGL(const AWidth, AHeight: Integer; const AMipmapped: Boolean; const ATextureInfo: TGRGLTextureInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const AMipmapped: Boolean; const ATextureInfo: TGRMTLTextureInfo);
  end;

  TSKHighContrastConfigInvertStyle = (NoInvert, InvertBrightness, InvertLightness);

  { TSKHighContrastConfig }

  TSKHighContrastConfig = record
    Grayscale: Boolean;
    InvertStyle: TSKHighContrastConfigInvertStyle;
    Contrast: Single;
    constructor Create(const AGrayscale: Boolean; const AInvertStyle: TSKHighContrastConfigInvertStyle; const AContrast: Single);
    class operator Explicit(const AHighContrastConfig: TSKHighContrastConfig): sk_highcontrastconfig_t;
  end;

  TSkBlendMode = (Clear,  Src, Dest, SrcOver, DestOver, SrcIn, DestIn, SrcOut,
    DestOut, SrcATop, DestATop, Eor, Plus, Modulate, Screen, Overlay, Darken,
    Lighten, ColorDodge, ColorBurn, HardLight, SoftLight, Difference, Exclusion,
    Multiply, Hue, Saturation, Color, Luminosity);

  TSKColorMatrix = array[0..19] of Single;

  TSKOverdrawColor = array[0..5] of TAlphaColor;

  TSKTableFilter = array[0..255] of Byte;

  { ISKColorFilter }

  ISKColorFilter = interface(ISKReferenceCounted)
    ['{AF15420E-1F26-4881-B4AF-638B231F7680}']
  end;

  { TSKColorFilter }

  TSKColorFilter = class(TSKReferenceCounted, ISKColorFilter)
  public
    class function MakeBlend(const AColor: TAlphaColor; const AMode: TSkBlendMode): ISKColorFilter; static;
    class function MakeCompose(const AOuter, AInner: ISKColorFilter): ISKColorFilter; static;
    class function MakeHighContrast(const AConfig: TSKHighContrastConfig): ISKColorFilter; static;
    class function MakeHSLAMatrix(const AMatrix: TSKColorMatrix): ISKColorFilter; static;
    class function MakeLerp(const AWeight: Single; const ADest, ASrc: ISKColorFilter): ISKColorFilter; static;
    class function MakeLighting(const AMultiply, AAdd: TAlphaColor): ISKColorFilter; static;
    class function MakeLinearToSRGBGamma: ISKColorFilter;
    class function MakeLumaColor: ISKColorFilter; static;
    class function MakeMatrix(const AMatrix: TSKColorMatrix): ISKColorFilter; static;
    class function MakeOverdraw(const AColors: TSKOverdrawColor): ISKColorFilter; static;
    class function MakeSRGBToLinearGamma: ISKColorFilter;
    class function MakeTable(const ATable: TSKTableFilter): ISKColorFilter; overload; static;
    class function MakeTable(const ATableA, ATableR, ATableG, ATableB: TSKTableFilter): ISKColorFilter; overload; static;
  end;

  TSKTileMode = (Clamp, Replicate, Mirror, Decal);

  { ISKShader }

  ISKShader = interface(ISKReferenceCounted)
    ['{6DC8870F-457B-408A-9D6F-4FB95FF657A0}']
    function MakeWithColorFilter(const AFilter: ISKColorFilter): ISKShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISKShader;
  end;

  { TSKShader }

  TSKShader = class(TSKReferenceCounted, ISKShader)
  strict protected
    function MakeWithColorFilter(const AFilter: ISKColorFilter): ISKShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISKShader;
  public
    class function MakeBlend(const AMode: TSkBlendMode; const ADest, ASrc: ISKShader): ISKShader; static;
    class function MakeColor(const AColor: TAlphaColor): ISKShader; overload; static;
    class function MakeColor(const AColor: TAlphaColorF; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeEmpty: ISKShader; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSKTileMode): ISKShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSKTileMode; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const ATileMode: TSKTileMode; const APositions: TArray<Single> = []): ISKShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const ATileMode: TSKTileMode; const APositions: TArray<Single> = []; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ATileMode: TSKTileMode): ISKShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ATileMode: TSKTileMode; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const ATileMode: TSKTileMode; const APositions: TArray<Single> = []): ISKShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const ATileMode: TSKTileMode; const APositions: TArray<Single> = []; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor): ISKShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = []): ISKShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = []; const AColorSpace: ISKColorSpace = nil): ISKShader; overload; static;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISKShader; overload; static;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISKShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISKShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISKShader; overload; static;
  end;

  TSKBlurStyle = (Normal, Solid, Outer, Inner);

  { ISKMaskFilter }

  ISKMaskFilter = interface(ISKReferenceCounted)
    ['{1F42E44F-D04B-486D-A6D2-EF19CEDE5B90}']
  end;

  { TSKMaskFilter }

  TSKMaskFilter = class(TSKReferenceCounted, ISKMaskFilter)
  public
    class function MakeBlur(const AStyle: TSKBlurStyle; const ASigma: Single; const ARespectCTM: Boolean = True): ISKMaskFilter; static;
    class function MakeShader(const AShader: ISKShader): ISKMaskFilter; static;
    class function MakeTable(const ATable: TSKTableFilter): ISKMaskFilter; static;
    class function MakeTableClip(const AMin, AMax: Byte): ISKMaskFilter; static;
    class function MakeTableGamma(const AGamma: Single): ISKMaskFilter; static;
  end;

  { ISKPicture }

  ISKPicture = interface(ISKReferenceCounted)
    ['{0208DD4B-CCE5-4789-8F52-8AA96B2E642F}']
    function GetCullRect: TRectF;
    function GetUniqueID: Cardinal;
    function MakeShader(const ATileModeX, ATileModeY: TSKTileMode; const AFilterMode: TSKFilterMode): ISKShader;
    function SaveToData: ISKData;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AWStream: ISKWStream);
    property CullRect: TRectF read GetCullRect;
    property UniqueID: Cardinal read GetUniqueID;
  end;

  { TSKPicture }

  TSKPicture = class(TSKReferenceCounted, ISKPicture)
  strict protected
    function GetCullRect: TRectF;
    function GetUniqueID: Cardinal;
    function MakeShader(const ATileModeX, ATileModeY: TSKTileMode; const AFilterMode: TSKFilterMode): ISKShader;
    function SaveToData: ISKData;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AWStream: ISKWStream);
  public
    class function MakeFromData(const AData: ISKData): ISKPicture; static;
    class function MakeFromFile(const AFileName: string): ISKPicture; static;
    class function MakeFromStream(const AStream: ISKStream): ISKPicture; static;
  end;

  ISKCanvas = interface;

  { ISKPictureRecorder }

  ISKPictureRecorder = interface(ISKObject)
    ['{0A676065-C294-4A3D-A65A-5F9116304EE4}']
    function BeginRecording(const ABounds: TRectF): ISKCanvas; overload;
    function BeginRecording(const AWidth, AHeight: Single): ISKCanvas; overload;
    function FinishRecording: ISKPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISKPicture; overload;
  end;

  { TSKPictureRecorder }

  TSKPictureRecorder = class(TSKObject, ISKPictureRecorder)
  strict protected
    function BeginRecording(const ABounds: TRectF): ISKCanvas; overload;
    function BeginRecording(const AWidth, AHeight: Single): ISKCanvas; overload;
    function FinishRecording: ISKPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISKPicture; overload;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  ISKImageFilter = interface;

  ISKPaint = interface;

  TGRSurfaceOrigin = (TopLeft, BottomLeft);

  TSKEncodedImageFormat = (JPEG = 3, PNG, WEBP = 6);

  TSKImageCachingHint = (Allow, Disallow);

  TSKImageRasterReleaseProc = reference to procedure (const APixels: Pointer);

  TSKImageTextureReleaseProc = reference to procedure;

  { ISKImage }

  ISKImage = interface(ISKReferenceCounted)
    ['{1C20B527-EBBD-466B-B3E3-68220D5CACED}']
    function EncodeToData: ISKData; overload;
    function EncodeToData(const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): ISKData; overload;
    function EncodeToFile(const AFileName: string): Boolean; overload;
    function EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): Boolean; overload;
    function EncodeToStream(const AWStream: ISKWStream): Boolean; overload;
    function EncodeToStream(const AWStream: ISKWStream; const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): Boolean; overload;
    function GetAlphaType: TSKAlphaType;
    function GetColorSpace: ISKColorSpace;
    function GetColorType: TSKColorType;
    function GetEncodedData: ISKData;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetUniqueID: Cardinal;
    function GetWidth: Integer;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(const ADirectContext: IGRDirectContext): Boolean;
    function MakeColorSpace(const AColorSpace: ISKColorSpace; const AContext: IGRDirectContext = nil): ISKImage;
    function MakeNonTextureImage: ISKImage;
    function MakeRasterImage: ISKImage;
    function MakeShader(const ASampling: TSKSamplingOptions; const ATileModeX: TSKTileMode = TSKTileMode.Clamp; ATileModeY: TSKTileMode = TSKTileMode.Clamp): ISKShader;
    function MakeSubset(const ASubset: TRect; const ADirectContext: IGRDirectContext = nil): ISKImage;
    function MakeTextureImage(const AContext: IGRDirectContext; const AMipmapped: Boolean = False): ISKImage;
    function MakeWithFilter(const AContext: IGRDirectContext; const AFilter: ISKImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint): ISKImage;
    function ReadPixels(const AContext: IGRDirectContext; const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean; overload;
    function ReadPixels(const AContext: IGRDirectContext; const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISKPixmap; const ASampling: TSKSamplingOptions; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean;
    property AlphaType: TSKAlphaType read GetAlphaType;
    property ColorSpace: ISKColorSpace read GetColorSpace;
    property ColorType: TSKColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSKImageInfo read GetImageInfo;
    property UniqueID: Cardinal read GetUniqueID;
    property Width: Integer read GetWidth;
  end;

  { TSKImage }

  TSKImage = class(TSKReferenceCounted, ISKImage)
  strict private
    class procedure raster_release_proc(const pixels: Pointer; context: Pointer); cdecl; static;
    class procedure texture_release_proc(context: Pointer); cdecl; static;
  strict protected
    function EncodeToData: ISKData; overload;
    function EncodeToData(const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): ISKData; overload;
    function EncodeToFile(const AFileName: string): Boolean; overload;
    function EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): Boolean; overload;
    function EncodeToStream(const AWStream: ISKWStream): Boolean; overload;
    function EncodeToStream(const AWStream: ISKWStream; const AEncodedImageFormat: TSKEncodedImageFormat; const AQuality: Integer = 100): Boolean; overload;
    function GetAlphaType: TSKAlphaType;
    function GetColorSpace: ISKColorSpace;
    function GetColorType: TSKColorType;
    function GetEncodedData: ISKData;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetUniqueID: Cardinal;
    function GetWidth: Integer;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(const AContext: IGRDirectContext): Boolean;
    function MakeColorSpace(const AColorSpace: ISKColorSpace; const AContext: IGRDirectContext = nil): ISKImage;
    function MakeNonTextureImage: ISKImage;
    function MakeRasterImage: ISKImage;
    function MakeShader(const ASampling: TSKSamplingOptions; const ATileModeX: TSKTileMode = TSKTileMode.Clamp; ATileModeY: TSKTileMode = TSKTileMode.Clamp): ISKShader;
    function MakeSubset(const ASubset: TRect; const AContext: IGRDirectContext = nil): ISKImage;
    function MakeTextureImage(const AContext: IGRDirectContext; const AMipmapped: Boolean = False): ISKImage;
    function MakeWithFilter(const AContext: IGRDirectContext; const AFilter: ISKImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint): ISKImage;
    function ReadPixels(const AContext: IGRDirectContext; const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean; overload;
    function ReadPixels(const AContext: IGRDirectContext; const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISKPixmap; const ASampling: TSKSamplingOptions; const ACachingHint: TSKImageCachingHint = TSKImageCachingHint.Allow): Boolean;
  public
    class function MakeFromAdoptedTexture(const AContext: IGRDirectContext; const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin; AColorType: TSKColorType; const AAlphaType: TSKAlphaType = TSKAlphaType.Premul; const AColorSpace: ISKColorSpace = nil): ISKImage; static;
    class function MakeFromEncodedData(const AEncoded: ISKData): ISKImage; static;
    class function MakeFromEncodedStream(const AStream: ISKStream): ISKImage; static;
    class function MakeFromEncodedFile(const AFileName: string): ISKImage; static;
    class function MakeFromPicture(const APicture: ISKPicture; const ADimensions: TSize): ISKImage; overload; static;
    class function MakeFromPicture(const APicture: ISKPicture; const ADimensions: TSize; const AMatrix: TMatrix; const APaint: ISKPaint = nil): ISKImage; overload; static;
    class function MakeFromRaster(const AImageInfo: TSKImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSKImageRasterReleaseProc = nil): ISKImage; overload; static;
    class function MakeFromRaster(const APixmap: ISKPixmap; const ARasterReleaseProc: TSKImageRasterReleaseProc = nil): ISKImage; overload; static;
    class function MakeFromTexture(const AContext: IGRDirectContext; const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin; AColorType: TSKColorType; const AAlphaType: TSKAlphaType; const AColorSpace: ISKColorSpace; const ATextureReleaseProc: TSKImageTextureReleaseProc = nil): ISKImage; static;
    class function MakeRasterCopy(const APixmap: ISKPixmap): ISKImage; static;
  end;

  TSKColorChannel = (R, G, B, A);

  { ISKImageFilter }

  ISKImageFilter = interface(ISKReferenceCounted)
    ['{B233455A-C2BB-444B-ACBE-6B09B8D848F3}']
  end;

  { TSKImageFilter }

  TSKImageFilter = class(TSKReferenceCounted, ISKImageFilter)
  public
    class function MakeAlphaThreshold(const ARegion: ISKRegion; const AInnerMin, AOuterMax: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeAlphaThreshold(const ARegion: ISKRegion; const AInnerMin, AOuterMax: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISKImageFilter; AForeground: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISKImageFilter; const ACropRect: TRectF; AForeground: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeBlend(const AMode: TSKBlendMode; const ABackground: ISKImageFilter; const AForeground: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeBlend(const AMode: TSKBlendMode; const ABackground: ISKImageFilter; const ACropRect: TRectF; const AForeground: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; const AInput: ISKImageFilter = nil; const ATileMode: TSKTileMode = TSKTileMode.Decal): ISKImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil; const ATileMode: TSKTileMode = TSKTileMode.Decal): ISKImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISKColorFilter; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISKColorFilter; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeCompose(const AOuter, AInner: ISKImageFilter): ISKImageFilter; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSKColorChannel; const AScale: Single; const AColor: ISKImageFilter; const ADisplacement: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSKColorChannel; const AScale: Single; const AColor: ISKImageFilter; const ACropRect: TRectF; const ADisplacement: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDistantLightDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDistantLightDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDistantLightSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDistantLightSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDropShadow(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDropShadow(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeImage(const AImage: ISKImage): ISKImageFilter; overload; static;
    class function MakeImage(const AImage: ISKImage; const ASampling: TSKSamplingOptions): ISKImageFilter; overload; static;
    class function MakeImage(const AImage: ISKImage; const ASrc, ADest: TRectF; const ASampling: TSKSamplingOptions): ISKImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSKTileMode; const AConvolveAlpha: Boolean; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSKTileMode; const AConvolveAlpha: Boolean; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeMatrixTransform(const AMatrix: TMatrix; const ASampling: TSKSamplingOptions; const AInput: ISKImageFilter = nil): ISKImageFilter; static;
    class function MakeMerge(const AFilter1, AFilter2: ISKImageFilter): ISKImageFilter; overload; static;
    class function MakeMerge(const AFilter1, AFilter2: ISKImageFilter; const ACropRect: TRectF): ISKImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISKImageFilter>): ISKImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISKImageFilter>; const ACropRect: TRectF): ISKImageFilter; overload; static;
    class function MakeOffset(const ADX, ADY: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeOffset(const ADX, ADY: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakePicture(const APicture: ISKPicture): ISKImageFilter; overload; static;
    class function MakePicture(const APicture: ISKPicture; const ATargetRect: TRectF): ISKImageFilter; overload; static;
    class function MakePointLightDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakePointLightDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakePointLightSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakePointLightSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeShader(const AShader: ISKShader; const ADither: Boolean = False): ISKImageFilter; overload; static;
    class function MakeShader(const AShader: ISKShader; const ACropRect: TRectF; const ADither: Boolean = False): ISKImageFilter; overload; static;
    class function MakeSpotLightDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeSpotLightDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeSpotLightSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeSpotLightSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISKImageFilter = nil): ISKImageFilter; overload; static;
    class function MakeTile(const ASrc, ADest: TRect; const AInput: ISKImageFilter = nil): ISKImageFilter; static;
  end;

  TSKPixelGeometry = (Unknown, RGBHorizontal, BGRHorizontal, RGBVertical, BGRVertical);

  TSKSurfacePropsFlag  = (AUseDeviceIndependentFonts);
  TSKSurfacePropsFlags = set of TSKSurfacePropsFlag;

  { ISKSurfaceProps }

  ISKSurfaceProps = interface(ISKObject)
    ['{1C39B509-4180-420D-A9F7-E410E816FDA5}']
    function GetFlags: TSKSurfacePropsFlags;
    function GetPixelGeometry: TSKPixelGeometry;
    function IsEqual(const AProps: ISKSurfaceProps): Boolean;
    property Flags: TSKSurfacePropsFlags read GetFlags;
    property PixelGeometry: TSKPixelGeometry read GetPixelGeometry;
  end;

  { TSKSurfaceProps }

  TSKSurfaceProps = class(TSKObject, ISKSurfaceProps)
  strict protected
    function GetFlags: TSKSurfacePropsFlags;
    function GetPixelGeometry: TSKPixelGeometry;
    function IsEqual(const AProps: ISKSurfaceProps): Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AFlags: TSKSurfacePropsFlags = []; const APixelGeometry: TSKPixelGeometry = TSKPixelGeometry.Unknown);
  end;

  TSKSurfaceRasterReleaseProc = reference to procedure (const APixels: Pointer);

  { ISKSurface }

  ISKSurface = interface(ISKReferenceCounted)
    ['{4BBFA7B0-50AA-4F69-AD08-8FBF21CA6E80}']
    function GetCanvas: ISKCanvas;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetProps: ISKSurfaceProps;
    function GetWidth: Integer;
    function MakeImageSnapshot(const ABounds: TRect): ISKImage; overload;
    function MakeImageSnapshot: ISKImage; overload;
    function PeekPixels: ISKPixmap;
    function ReadPixels(const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISKCanvas; const AX, AY: Single; const APaint: ISKPaint = nil); overload;
    procedure Draw(const ACanvas: ISKCanvas; const AX, AY: Single; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil); overload;
    procedure Flush;
    procedure WritePixels(const ASrc: ISKPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSKImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    property Canvas: ISKCanvas read GetCanvas;
    property Height: Integer read GetHeight;
    property ImageInfo: TSKImageInfo read GetImageInfo;
    property Props: ISKSurfaceProps read GetProps;
    property Width: Integer read GetWidth;
  end;

  { TSKSurface }

  TSKSurface = class(TSKReferenceCounted, ISKSurface)
  strict private
    class procedure raster_release_proc(pixels: Pointer; context: Pointer); cdecl; static;
  strict protected
    function GetCanvas: ISKCanvas;
    function GetHeight: Integer;
    function GetImageInfo: TSKImageInfo;
    function GetProps: ISKSurfaceProps;
    function GetWidth: Integer;
    function MakeImageSnapshot(const ABounds: TRect): ISKImage; overload;
    function MakeImageSnapshot: ISKImage; overload;
    function PeekPixels: ISKPixmap;
    function ReadPixels(const ADest: ISKPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISKCanvas; const AX, AY: Single; const APaint: ISKPaint = nil); overload;
    procedure Draw(const ACanvas: ISKCanvas; const AX, AY: Single; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil); overload;
    procedure Flush;
    procedure WritePixels(const ASrc: ISKPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSKImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
  public
    class function MakeFromRenderTarget(const AContext: IGRDirectContext; const ARenderTarget: IGRBackendRenderTarget; const AOrigin: TGRSurfaceOrigin; const AColorType: TSKColorType; const AColorSpace: ISKColorSpace; const AProps: ISKSurfaceProps): ISKSurface; static;
    class function MakeFromTexture(const AContext: IGRDirectContext; const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSKColorType; const AColorSpace: ISKColorSpace; const AProps: ISKSurfaceProps): ISKSurface; static;
    class function MakeNull: ISKSurface; static;
    class function MakeRaster(const AImageInfo: TSKImageInfo; const AProps: ISKSurfaceProps = nil): ISKSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSKImageInfo; const ARowBytes: NativeUInt; const AProps: ISKSurfaceProps = nil): ISKSurface; overload; static;
    class function MakeRaster(const AWidth, AHeight: Integer): ISKSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSKImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSKSurfaceRasterReleaseProc = nil; const AProps: ISKSurfaceProps = nil): ISKSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISKPixmap; const ARasterReleaseProc: TSKSurfaceRasterReleaseProc = nil; const AProps: ISKSurfaceProps = nil): ISKSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGRDirectContext; const ABudgeted: Boolean; const AImageInfo: TSKImageInfo; const ASampleCount: Integer = 0; const AOrigin: TGRSurfaceOrigin = TGRSurfaceOrigin.BottomLeft; const AProps: ISKSurfaceProps = nil; const AShouldCreateWithMips: Boolean = False): ISKSurface; static;
  end;

  TSKVertexMode = (Triangles, TriangleStrip, TriangleFan);

  { ISKVertices }

  ISKVertices = interface(ISKReferenceCounted)
    ['{576AEA37-5A7C-4CAC-B69C-D033AD7D1FA6}']
    function GetUniqueID: Cardinal;
    property UniqueID: Cardinal read GetUniqueID;
  end;

  { TSKVertices }

  TSKVertices = class(TSKReferenceCounted, ISKVertices)
  strict protected
    function GetUniqueID: Cardinal;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function MakeCopy(const AVertexMode: TSKVertexMode; const APositions, ATextures: TArray<TPointF>; const AColors: TArray<TAlphaColor>; const AIndices: TArray<Word> = []): ISKVertices; static;
  end;

  TSKFontMetricsFlag  = (UnderlineThicknessIsValid, UnderlinePositionIsValid, StrikeoutThicknessIsValid, StrikeoutPositionIsValid, BoundsInvalid);
  TSKFontMetricsFlags = set of TSKFontMetricsFlag;

  { TSKFontMetrics }

  TSKFontMetrics = record
    Flags: TSKFontMetricsFlags;
    Top: Single;
    Ascent: Single;
    Descent: Single;
    Bottom: Single;
    Leading: Single;
    AvgCharWidth: Single;
    MaxCharWidth: Single;
    XMin: Single;
    XMax: Single;
    XHeight: Single;
    CapHeight: Single;
    UnderlineThickness: Single;
    UnderlinePosition: Single;
    StrikeoutThickness: Single;
    StrikeoutPosition: Single;
    class operator Equal(const AFontMetrics1, AFontMetrics2: TSKFontMetrics): Boolean;
    class operator Explicit(const AFontMetrics: sk_fontmetrics_t): TSKFontMetrics;
    class operator NotEqual(const AFontMetrics1, AFontMetrics2: TSKFontMetrics): Boolean;
  end;

  TSKFontSlant = (Upright, Italic, Oblique);

  TSKFontWeight = (Invisible, ThinWeight, ExtraLight, Light, Normal, Medium, SemiBold, Bold, ExtraBold, Black, ExtraBlack);

  TSKFontWidth = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Normal, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded);

  { ISKFontStyle }

  ISKFontStyle = interface(ISKObject)
    ['{43EB9458-289B-48DB-B8D0-DA1F430B07D8}']
    function GetSlant: TSKFontSlant;
    function GetWeight: Integer;
    function GetWidth: Integer;
    property Slant: TSKFontSlant read GetSlant;
    property Weight: Integer read GetWeight;
    property Width: Integer read GetWidth;
  end;

  { TSKFontStyle }

  TSKFontStyle = class(TSKObject, ISKFontStyle)
  protected const
    FontWeight : array[TSKFontWeight] of Integer = (0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000);
    FontWidth  : array[TSKFontWidth]  of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9);
  strict protected
    function GetSlant: TSKFontSlant;
    function GetWeight: Integer;
    function GetWidth: Integer;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AWeight, AWidth: Integer; const ASlant: TSKFontSlant); overload;
    constructor Create(const AWeight: TSKFontWeight; const AWidth: TSKFontWidth; const ASlant: TSKFontSlant); overload;
    class function Normal: ISKFontStyle; static; inline;
    class function Bold: ISKFontStyle; static; inline;
    class function Italic: ISKFontStyle; static; inline;
    class function BoldItalic: ISKFontStyle; static; inline;
  end;

  { ISKTypeface }

  ISKTypeface = interface(ISKReferenceCounted)
    ['{7F036CD6-BD6B-4AC7-9219-0DF4E356B0BF}']
    function GetFamilyName: ISKString;
    function GetSlant: TSKFontSlant;
    function GetStyle: ISKFontStyle;
    function GetUniqueID: Cardinal;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
    property FamilyName: ISKString read GetFamilyName;
    property Slant: TSKFontSlant read GetSlant;
    property Style: ISKFontStyle read GetStyle;
    property UniqueID: Cardinal read GetUniqueID;
    property Weight: Integer read GetWeight;
    property Width: Integer read GetWidth;
  end;

  { TSKTypeface }

  TSKTypeface = class(TSKReferenceCounted, ISKTypeface)
  strict protected
    function GetFamilyName: ISKString;
    function GetSlant: TSKFontSlant;
    function GetStyle: ISKFontStyle;
    function GetUniqueID: Cardinal;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
  public
    class function MakeDefault: ISKTypeface; static;
    class function MakeFromData(const AData: ISKData; const ATTcIndex: Integer = 0): ISKTypeface; static;
    class function MakeFromStream(const AStream: ISKStream; const ATTcIndex: Integer = 0): ISKTypeface; static;
    class function MakeFromFile(const AFileName: string; const ATTcIndex: Integer = 0): ISKTypeface; static;
    class function MakeFromName(const AStyle: ISKFontStyle): ISKTypeface; overload; static;
    class function MakeFromName(const AFamilyName: string; const AStyle: ISKFontStyle): ISKTypeface; overload; static;
  end;

  TSKFontEdging = (Alias, AntiAlias, SubpixelAntiAlias);

  TSKFontGlyphPathProc = reference to procedure (const APath: ISKPath; const AMatrix: TMatrix);

  TSKFontHinting = (None, Slight, Normal, Full);

  { ISKFont }

  ISKFont = interface(ISKObject)
    ['{E94E4044-598D-4007-9146-9F0AE30CF470}']
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): TArray<TRectF>;
    function GetEdging: TSKFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>; overload;
    function GetGlyphs(const AText: UCS4String): TArray<Word>; overload;
    function GetGlyphs(const AText: UTF8String): TArray<Word>; overload;
    function GetHinting: TSKFontHinting;
    function GetIntercepts(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const ATop, ABottom: Single; const APaint: ISKPaint = nil): TArray<Single>;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSKFontMetrics): Single;
    function GetOffsets(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetPath(const AGlyph: Word): ISKPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSKFontGlyphPathProc);
    function GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>; overload;
    function GetPositions(const AGlyphs: TArray<Word>; const AOrigin: TPointF): TArray<TPointF>; overload;
    function GetScaleX: Single;
    function GetSize: Single;
    function GetSkewX: Single;
    function GetSpacing: Single;
    function GetSubpixel: Boolean;
    function GetTypeface: ISKTypeface;
    function GetTypefaceOrDefault: ISKTypeface;
    function GetWidths(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): TArray<Single>;
    procedure GetWidthsAndBounds(const AGlyphs: TArray<Word>; out AWidths: TArray<Single>; out ABounds: TArray<TRectF>; const APaint: ISKPaint = nil);
    function IsEqual(const AFont: ISKFont): Boolean;
    function MakeWithSize(const ASize: Single): ISKFont;
    function MeasureText(const AText: string; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: string; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UCS4String; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UCS4String; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UTF8String; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UTF8String; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    procedure SetBaselineSnap(const AValue: Boolean);
    procedure SetEdging(const AValue: TSKFontEdging);
    procedure SetEmbeddedBitmaps(const AValue: Boolean);
    procedure SetEmbolden(const AValue: Boolean);
    procedure SetForceAutoHinting(const AValue: Boolean);
    procedure SetHinting(const AValue: TSKFontHinting);
    procedure SetLinearMetrics(const AValue: Boolean);
    procedure SetScaleX(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetSkewX(const AValue: Single);
    procedure SetSubpixel(const AValue: Boolean);
    procedure SetTypeface(const AValue: ISKTypeface);
    function UnicharToGlyph(const AUnichar: Integer): Word;
    function UnicharsToGlyphs(const AUnichars: TArray<Integer>): TArray<Word>;
    property BaselineSnap: Boolean read GetBaselineSnap write SetBaselineSnap;
    property Edging: TSKFontEdging read GetEdging write SetEdging;
    property EmbeddedBitmaps: Boolean read GetEmbeddedBitmaps write SetEmbeddedBitmaps;
    property Embolden: Boolean read GetEmbolden write SetEmbolden;
    property ForceAutoHinting: Boolean read GetForceAutoHinting write SetForceAutoHinting;
    property Hinting: TSKFontHinting read GetHinting write SetHinting;
    property LinearMetrics: Boolean read GetLinearMetrics write SetLinearMetrics;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property Size: Single read GetSize write SetSize;
    property SkewX: Single read GetSkewX write SetSkewX;
    property Spacing: Single read GetSpacing;
    property Subpixel: Boolean read GetSubpixel write SetSubpixel;
    property Typeface: ISKTypeface read GetTypeface write SetTypeface;
  end;

  { TSKFont }

  TSKFont = class(TSKObject, ISKFont)
  strict private
    class procedure getpaths_proc(const path: sk_path_t; const [Ref] matrix: sk_matrix_t; context: Pointer); cdecl; static;
  strict protected
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): TArray<TRectF>;
    function GetEdging: TSKFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>; overload;
    function GetGlyphs(const AText: UCS4String): TArray<Word>; overload;
    function GetGlyphs(const AText: UTF8String): TArray<Word>; overload;
    function GetHinting: TSKFontHinting;
    function GetIntercepts(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const ATop, ABottom: Single; const APaint: ISKPaint = nil): TArray<Single>;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSKFontMetrics): Single;
    function GetOffsets(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetPath(const AGlyph: Word): ISKPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSKFontGlyphPathProc);
    function GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>; overload;
    function GetPositions(const AGlyphs: TArray<Word>; const AOrigin: TPointF): TArray<TPointF>; overload;
    function GetScaleX: Single;
    function GetSize: Single;
    function GetSkewX: Single;
    function GetSpacing: Single;
    function GetSubpixel: Boolean;
    function GetTypeface: ISKTypeface;
    function GetTypefaceOrDefault: ISKTypeface;
    function GetWidths(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): TArray<Single>;
    procedure GetWidthsAndBounds(const AGlyphs: TArray<Word>; out AWidths: TArray<Single>; out ABounds: TArray<TRectF>; const APaint: ISKPaint = nil);
    function IsEqual(const AFont: ISKFont): Boolean;
    function MakeWithSize(const ASize: Single): ISKFont;
    function MeasureText(const AText: string; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: string; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UCS4String; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UCS4String; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UTF8String; const APaint: ISKPaint = nil): Single; overload;
    function MeasureText(const AText: UTF8String; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; const APaint: ISKPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; out ABounds: TRectF; const APaint: ISKPaint = nil): Single; overload;
    procedure SetBaselineSnap(const AValue: Boolean);
    procedure SetEdging(const AValue: TSKFontEdging);
    procedure SetEmbeddedBitmaps(const AValue: Boolean);
    procedure SetEmbolden(const AValue: Boolean);
    procedure SetForceAutoHinting(const AValue: Boolean);
    procedure SetHinting(const AValue: TSKFontHinting);
    procedure SetLinearMetrics(const AValue: Boolean);
    procedure SetScaleX(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetSkewX(const AValue: Single);
    procedure SetSubpixel(const AValue: Boolean);
    procedure SetTypeface(const AValue: ISKTypeface);
    function UnicharToGlyph(const AUnichar: Integer): Word;
    function UnicharsToGlyphs(const AUnichars: TArray<Integer>): TArray<Word>;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AFont: ISKFont); overload;
    constructor Create(const ATypeface: ISKTypeface = nil; const ASize: Single = 12; const AScaleX: Single = 1; const ASkewX: Single = 0); overload;
  end;

  { TSKRotationScaleMatrix }

  TSKRotationScaleMatrix = record
    SCos: Single;
    SSin: Single;
    TX: Single;
    TY: Single;
    constructor Create(const AScale, ARadians, ATX, ATY, AAnchorX, AAnchorY: Single);
    constructor CreateRotation(const ARadians, AAnchorX, AAnchorY: Single);
    constructor CreateScale(const AScale: Single);
    constructor CreateTranslation(const AX, AY: Single);
    class function Identity: TSKRotationScaleMatrix; static; inline;
  end;

  { ISKTextBlob }

  ISKTextBlob = interface(ISKReferenceCounted)
    ['{AB2465A6-1886-4C72-AFC8-E91AB36D73A7}']
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISKPaint = nil): TArray<Single>;
  end;

  { TSKTextBlob }

  TSKTextBlob = class(TSKReferenceCounted, ISKTextBlob)
  strict protected
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISKPaint = nil): TArray<Single>;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AText: string; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function Make(const AText: string; const AFont: ISKFont; const AOrigin: TPointF): ISKTextBlob; overload; static;
    class function Make(const AText: UCS4String; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function Make(const AText: UCS4String; const AFont: ISKFont; const AOrigin: TPointF): ISKTextBlob; overload; static;
    class function Make(const AText: UTF8String; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function Make(const AText: UTF8String; const AFont: ISKFont; const AOrigin: TPointF): ISKTextBlob; overload; static;
    class function MakeGlyphs(const AGlyphs: TArray<Word>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeGlyphs(const AGlyphs: TArray<Word>; const AFont: ISKFont; const AOrigin: TPointF): ISKTextBlob; overload; static;
    class function MakeHorizontal(const AText: string; const APositions: TArray<Single>; const AY: Single; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeHorizontal(const AText: UCS4String; const APositions: TArray<Single>; const AY: Single; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeHorizontal(const AText: UTF8String; const APositions: TArray<Single>; const AY: Single; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeHorizontalGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<Single>; const AY: Single; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakePositioned(const AText: string; const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakePositioned(const AText: UCS4String; const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakePositioned(const AText: UTF8String; const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakePositionedGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeRotationScale(const AText: string; const APositions: TArray<TSKRotationScaleMatrix>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeRotationScale(const AText: UCS4String; const APositions: TArray<TSKRotationScaleMatrix>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeRotationScale(const AText: UTF8String; const APositions: TArray<TSKRotationScaleMatrix>; const AFont: ISKFont): ISKTextBlob; overload; static;
    class function MakeRotationScaleGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TSKRotationScaleMatrix>; const AFont: ISKFont): ISKTextBlob; overload; static;
  end;

  TSKArray<T> = array[0..0] of T;

  { TSKRunBuffer }

  TSKRunBuffer = record
    Glyphs: ^TSKArray<Word>;
    class operator Explicit(const ARunHandler: sk_runbuffer_t): TSKRunBuffer;
  end;

  { TSKHorizontalRunBuffer }

  TSKHorizontalRunBuffer = record
    Glyphs: ^TSKArray<Word>;
    Positions: ^TSKArray<Single>;
    class operator Explicit(const ARunHandler: sk_runbuffer_t): TSKHorizontalRunBuffer;
  end;

  { TSKPositionedRunBuffer }

  TSKPositionedRunBuffer = record
    Glyphs: ^TSKArray<Word>;
    Positions: ^TSKArray<TPointF>;
    class operator Explicit(const ARunHandler: sk_runbuffer_t): TSKPositionedRunBuffer;
  end;

  { TSKRotationScaleRunBuffer }

  TSKRotationScaleRunBuffer = record
    Glyphs: ^TSKArray<Word>;
    Positions: ^TSKArray<TSKRotationScaleMatrix>;
    class operator Explicit(const ARunHandler: sk_runbuffer_t): TSKRotationScaleRunBuffer;
  end;

  { ISKTextBlobBuilder }

  ISKTextBlobBuilder = interface(ISKObject)
    ['{7C310A25-B21F-4AC3-A6F3-2980BC59838B}']
    function AllocateHorizontalRun(const AFont: ISKFont; const ACount: Integer; const AY: Single): TSKHorizontalRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISKFont; const ACount: Integer; const AY: Single; const ABounds: TRectF): TSKHorizontalRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISKFont; const ACount: Integer): TSKPositionedRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISKFont; const ACount: Integer; const ABounds: TRectF): TSKPositionedRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISKFont; const ACount: Integer): TSKRotationScaleRunBuffer;
    function AllocateRun(const AFont: ISKFont; const ACount: Integer; const AX, AY: Single): TSKRunBuffer; overload;
    function AllocateRun(const AFont: ISKFont; const ACount: Integer; const AX, AY: Single; const ABounds: TRectF): TSKRunBuffer; overload;
    function Detach: ISKTextBlob;
  end;

  { TSKTextBlobBuilder }

  TSKTextBlobBuilder = class(TSKObject, ISKTextBlobBuilder)
  strict protected
    function AllocateHorizontalRun(const AFont: ISKFont; const ACount: Integer; const AY: Single): TSKHorizontalRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISKFont; const ACount: Integer; const AY: Single; const ABounds: TRectF): TSKHorizontalRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISKFont; const ACount: Integer): TSKPositionedRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISKFont; const ACount: Integer; const ABounds: TRectF): TSKPositionedRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISKFont; const ACount: Integer): TSKRotationScaleRunBuffer;
    function AllocateRun(const AFont: ISKFont; const ACount: Integer; const AX, AY: Single): TSKRunBuffer; overload;
    function AllocateRun(const AFont: ISKFont; const ACount: Integer; const AX, AY: Single; const ABounds: TRectF): TSKRunBuffer; overload;
    function Detach: ISKTextBlob;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { TSKShaperRunHandlerBuffer }

  TSKShaperRunHandlerBuffer = record
    Glyphs: ^TSKArray<Word>;
    Positions: ^TSKArray<TPointF>;
    Offsets: ^TSKArray<TPointF>;
    Clusters: ^TSKArray<Cardinal>;
    Point: TPointF;
  end;

  { TSKShaperRunHandlerRange }

  TSKShaperRunHandlerRange = record
    Start: NativeUInt;
    Size: NativeUInt;
  end;

  { TSKShaperRunHandlerInfo }

  TSKShaperRunHandlerInfo = record
    Font: ISKFont;
    BidiLevel: Byte;
    Advance: TPointF;
    GlyphCount: NativeUInt;
    UTF8Range: TSKShaperRunHandlerRange;
    class operator Explicit(const [Ref] ARunHandlerInfo: sk_shaperrunhandlerinfo_t): TSKShaperRunHandlerInfo;
  end;

  { ISKShaperRunHandler }

  ISKShaperRunHandler = interface(ISKObject)
    ['{D79E53E6-0E5A-48C3-8945-C038C2355121}']
  end;

  { TSKShaperRunHandler }

  TSKShaperRunHandler = class abstract(TSKObject, ISKShaperRunHandler)
  strict protected
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSKShaperRunHandlerBaseClass }

  TSKShaperRunHandlerBaseClass = class abstract(TSKShaperRunHandler)
  strict protected
    procedure DoBeginLine; virtual; abstract;
    procedure DoCommitLine; virtual; abstract;
    procedure DoCommitRunBuffer(const AInfo: TSKShaperRunHandlerInfo); virtual; abstract;
    procedure DoCommitRunInfo; virtual; abstract;
    function DoRunBuffer(const AInfo: TSKShaperRunHandlerInfo): TSKShaperRunHandlerBuffer; virtual; abstract;
    procedure DoRunInfo(const AInfo: TSKShaperRunHandlerInfo); virtual; abstract;
    class procedure begin_line_proc(context: Pointer); cdecl; static;
    class constructor Create;
    class procedure commit_line_proc(context: Pointer); cdecl; static;
    class procedure commit_run_buffer_proc(context: Pointer; const [Ref] info: sk_shaperrunhandlerinfo_t); cdecl; static;
    class procedure commit_run_info_proc(context: Pointer); cdecl; static;
    class procedure run_buffer_proc(context: Pointer; const [Ref] info: sk_shaperrunhandlerinfo_t; out result: sk_shaperrunhandlerbuffer_t); cdecl; static;
    class procedure run_info_proc(context: Pointer; const [Ref] info: sk_shaperrunhandlerinfo_t); cdecl; static;
  public
    constructor Create;
  end;

  { ISKTextBlobBuilderRunHandler }

  ISKTextBlobBuilderRunHandler = interface(ISKShaperRunHandler)
    ['{811E161D-F607-4144-A46A-1A1ED1A172B2}']
    function Detach: ISKTextBlob;
    function GetEndPoint: TPointF;
    property EndPoint: TPointF read GetEndPoint;
  end;

  { TSKTextBlobBuilderRunHandler }

  TSKTextBlobBuilderRunHandler = class(TSKShaperRunHandler, ISKTextBlobBuilderRunHandler)
  strict protected
    function Detach: ISKTextBlob;
    function GetEndPoint: TPointF;
  public
    constructor Create(const AText: UTF8String; const AOffset: TPointF);
  end;

  { ISKShaperRunIterator }

  ISKShaperRunIterator = interface(ISKObject)
    function Consume: Boolean;
    function GetEndOfCurrentRun: NativeUInt;
    property EndOfCurrentRun: NativeUInt read GetEndOfCurrentRun;
  end;

  { TSKShaperRunIterator }

  TSKShaperRunIterator = class abstract(TSKObject, ISKShaperRunIterator)
  strict protected
    function Consume: Boolean;
    function GetEndOfCurrentRun: NativeUInt;
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { ISKShaperBiDiRunIterator }

  ISKShaperBiDiRunIterator = interface(ISKShaperRunIterator)
    ['{FF422A04-79E0-49FA-95FD-C76C10AFD095}']
    function GetCurrentLevel: Byte;
    property CurrentLevel: Byte read GetCurrentLevel;
  end;

  { TSKShaperBiDiRunIterator }

  TSKShaperBiDiRunIterator = class(TSKShaperRunIterator, ISKShaperBiDiRunIterator)
  strict protected
    function GetCurrentLevel: Byte;
  end;

  { ISKShaperFontRunIterator }

  ISKShaperFontRunIterator = interface(ISKShaperRunIterator)
    ['{C7712D0D-929E-4162-98C4-6A7179994228}']
    function GetCurrentFont: ISKFont;
    property CurrentFont: ISKFont read GetCurrentFont;
  end;

  { TSKShaperFontRunIterator }

  TSKShaperFontRunIterator = class(TSKShaperRunIterator, ISKShaperFontRunIterator)
  strict protected
    function GetCurrentFont: ISKFont;
  end;

  { ISKShaperLanguageRunIterator }

  ISKShaperLanguageRunIterator = interface(ISKShaperRunIterator)
    ['{1FA347CC-2B6F-4A7F-82CE-95484A50BAA1}']
    function GetCurrentLanguage: string;
    property CurrentLanguage: string read GetCurrentLanguage;

  end;

  { TSKShaperLanguageRunIterator }

  TSKShaperLanguageRunIterator = class(TSKShaperRunIterator, ISKShaperLanguageRunIterator)
  strict protected
    function GetCurrentLanguage: string;
  end;

  TSKFourByteTag = type Cardinal;

  { TSKFourByteTagHelper }

  TSKFourByteTagHelper = record helper for TSKFourByteTag
    class function Create(const AA, AB, AC, AD: UTF8Char): TSKFourByteTag; static; inline;
  end;

  { ISKShaperScriptRunIterator }

  ISKShaperScriptRunIterator = interface(ISKShaperRunIterator)
    ['{F16F3673-E202-47A1-A06B-AEB6C8C4F66F}']
    function GetCurrentScript: TSKFourByteTag;
    property CurrentScript: TSKFourByteTag read GetCurrentScript;
  end;

  { TSKShaperScriptRunIterator }

  TSKShaperScriptRunIterator = class(TSKShaperRunIterator, ISKShaperScriptRunIterator)
  strict protected
    function GetCurrentScript: TSKFourByteTag;
  end;

  { TSKShaperFeature }

  TSKShaperFeature = record
    Tag: TSKFourByteTag;
    Value: Cardinal;
    Start: NativeUInt;
    Stop: NativeUInt;
    constructor Create(const ATag: TSKFourByteTag; const AValue: Cardinal; const AStart, AStop: NativeUInt);
  end;

  { ISKShaper }

  ISKShaper = interface(ISKObject)
    ['{00C65408-448D-402B-947A-86F2EFC10324}']
    procedure Shape(const AText: UTF8String; const AFont: ISKShaperFontRunIterator; const ABiDi: ISKShaperBiDiRunIterator; const AScript: ISKShaperScriptRunIterator; const ALanguage: ISKShaperLanguageRunIterator; const AWidth: Single; const AHandler: ISKShaperRunHandler; const AFeatures: TArray<TSKShaperFeature> = []); overload;
    procedure Shape(const AText: UTF8String; const ASrcFont: ISKFont; const ALeftToRight: Boolean; const AWidth: Single; const AHandler: ISKShaperRunHandler); overload;
  end;

  { TSKShaper }

  TSKShaper = class(TSKObject, ISKShaper)
  strict protected
    procedure Shape(const AText: UTF8String; const AFont: ISKShaperFontRunIterator; const ABiDi: ISKShaperBiDiRunIterator; const AScript: ISKShaperScriptRunIterator; const ALanguage: ISKShaperLanguageRunIterator; const AWidth: Single; const AHandler: ISKShaperRunHandler; const AFeatures: TArray<TSKShaperFeature> = []); overload;
    procedure Shape(const AText: UTF8String; const ASrcFont: ISKFont; const ALeftToRight: Boolean; const AWidth: Single; const AHandler: ISKShaperRunHandler); overload;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
    class function MakeBiDiRunIterator(const AText: UTF8String; const ABiDiLevel: Byte): ISKShaperBiDiRunIterator; static;
    class function MakeFontRunIterator(const AText: UTF8String; const AFont: ISKFont; const ARequestName: string = ''; const ARequestStyle: ISKFontStyle = nil; const ALanguage: ISKShaperLanguageRunIterator = nil): ISKShaperFontRunIterator; static;
    class function MakeScriptRunIterator(const AText: UTF8String; const AScript: TSKFourByteTag): ISKShaperScriptRunIterator; static;
    class function MakeStandardLanguageRunIterator(const AText: UTF8String): ISKShaperLanguageRunIterator; static;
    class procedure PurgeCaches; static;
  end;

  TSKPaintStyle = (Fill, Stroke, StrokeAndFill);

  TSKStrokeCap = (Butt, Round, Square);

  TSKStrokeJoin = (Miter, Round, Bevel);

  { ISKPaint }

  ISKPaint = interface(ISKObject)
    ['{2E5F2D19-D285-4924-A52A-37AD6730FE5B}']
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlendMode: TSKBlendMode;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISKColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISKPath): ISKPath; overload;
    function GetFillPath(const APath: ISKPath; const ACullRect: TRectF; const AResScale: Single = 1): ISKPath; overload;
    function GetImageFilter: ISKImageFilter;
    function GetMaskFilter: ISKMaskFilter;
    function GetPathEffect: ISKPathEffect;
    function GetShader: ISKShader;
    function GetStrokeCap: TSKStrokeCap;
    function GetStrokeJoin: TSKStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSKPaintStyle;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlendMode(const AValue: TSKBlendMode);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF); overload;
    procedure SetColorF(const AValue: TAlphaColorF; const AColorSpace: ISKColorSpace); overload;
    procedure SetColorFilter(const AValue: ISKColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetImageFilter(const AValue: ISKImageFilter);
    procedure SetMaskFilter(const AValue: ISKMaskFilter);
    procedure SetPathEffect(const AValue: ISKPathEffect);
    procedure SetShader(const AValue: ISKShader);
    procedure SetStrokeCap(const AValue: TSKStrokeCap);
    procedure SetStrokeJoin(const AValue: TSKStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSKPaintStyle);
    property Alpha: Byte read GetAlpha write SetAlpha;
    property AlphaF: Single read GetAlphaF write SetAlphaF;
    property AntiAlias: Boolean read GetAntiAlias write SetAntiAlias;
    property BlendMode: TSKBlendMode read GetBlendMode write SetBlendMode;
    property Color: TAlphaColor read GetColor write SetColor;
    property ColorF: TAlphaColorF read GetColorF write SetColorF;
    property ColorFilter: ISKColorFilter read GetColorFilter write SetColorFilter;
    property Dither: Boolean read GetDither write SetDither;
    property ImageFilter: ISKImageFilter read GetImageFilter write SetImageFilter;
    property MaskFilter: ISKMaskFilter read GetMaskFilter write SetMaskFilter;
    property PathEffect: ISKPathEffect read GetPathEffect write SetPathEffect;
    property Shader: ISKShader read GetShader write SetShader;
    property StrokeCap: TSKStrokeCap read GetStrokeCap write SetStrokeCap;
    property StrokeJoin: TSKStrokeJoin read GetStrokeJoin write SetStrokeJoin;
    property StrokeMiter: Single read GetStrokeMiter write SetStrokeMiter;
    property StrokeWidth: Single read GetStrokeWidth write SetStrokeWidth;
    property Style: TSKPaintStyle read GetStyle write SetStyle;
  end;

  { TSKPaint }

  TSKPaint = class(TSKObject, ISKPaint)
  strict protected
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlendMode: TSKBlendMode;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISKColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISKPath): ISKPath; overload;
    function GetFillPath(const APath: ISKPath; const ACullRect: TRectF; const AResScale: Single = 1): ISKPath; overload;
    function GetImageFilter: ISKImageFilter;
    function GetMaskFilter: ISKMaskFilter;
    function GetPathEffect: ISKPathEffect;
    function GetShader: ISKShader;
    function GetStrokeCap: TSKStrokeCap;
    function GetStrokeJoin: TSKStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSKPaintStyle;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlendMode(const AValue: TSKBlendMode);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF); overload;
    procedure SetColorF(const AValue: TAlphaColorF; const AColorSpace: ISKColorSpace); overload;
    procedure SetColorFilter(const AValue: ISKColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetImageFilter(const AValue: ISKImageFilter);
    procedure SetMaskFilter(const AValue: ISKMaskFilter);
    procedure SetPathEffect(const AValue: ISKPathEffect);
    procedure SetShader(const AValue: ISKShader);
    procedure SetStrokeCap(const AValue: TSKStrokeCap);
    procedure SetStrokeJoin(const AValue: TSKStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSKPaintStyle);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APaint: ISKPaint); overload;
  end;

  TSKLatticeRectType = (Default, Transparent, FixedColor);

  { TSKLattice }

  TSKLattice = record
    XDivs: TArray<Integer>;
    YDivs: TArray<Integer>;
    RectTypes: TArray<TSKLatticeRectType>;
    Bounds: TArray<TRect>;
    Colors: TArray<TAlphaColor>;
    class operator Explicit(const ALattice: TSKLattice): sk_lattice_t;
  end;

  TSKClipOp = (Difference, Intersect);

  TSKDrawPointsMode = (Points, Lines, Polygon);

  TSKPatchColors = array[0..3] of TAlphaColor;

  TSKPatchCubics = array[0..11] of TPointF;

  TSKPatchTexCoords = array[0..3] of TPointF;

  TSKSrcRectConstraint = (Close, Fast);

  { ISKCanvas }

  ISKCanvas = interface(ISKObject)
    ['{7ED84E94-1490-4E8E-8649-E926B6EC8C57}']
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISKPath; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISKRegion; const AOp: TSKClipOp = TSKClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISKRoundRect; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISKShader; const AOp: TSKClipOp = TSKClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue: ISKData);
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISKPaint);
    procedure DrawAtlas(const AAtlas: ISKImage; const ATansforms: TArray<TSKRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode; const ASampling: TSKSamplingOptions; const AColors: TArray<TAlphaColor> = []; const APaint: ISKPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISKImage; const ATansforms: TArray<TSKRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode; const ASampling: TSKSamplingOptions; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = []; const APaint: ISKPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISKPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISKPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSKBlendMode = TSKBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSKBlendMode = TSKBlendMode.SrcOver); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AOrigin: TPointF; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TSKRotationScaleMatrix>; const AOrigin: TPointF; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawImage(const AImage: ISKImage; const AX, AY: Single; const APaint: ISKPaint = nil); overload;
    procedure DrawImage(const AImage: ISKImage; const AX, AY: Single; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil); overload;
    procedure DrawImageLattice(const AImage: ISKImage; const ALattice: TSKLattice; const ADest: TRectF; const AFilterMode: TSKFilterMode = TSKFilterMode.Nearest; const APaint: ISKPaint = nil);
    procedure DrawImageNine(const AImage: ISKImage; const ACenter: TRect; const ADest: TRectF; const AFilterMode: TSKFilterMode; const APaint: ISKPaint = nil);
    procedure DrawImageRect(const AImage: ISKImage; const ADest: TRectF; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil; const AConstraint: TSKSrcRectConstraint = TSKSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISKImage; const ASrc, ADest: TRectF; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil; const AConstraint: TSKSrcRectConstraint = TSKSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISKPaint);  overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISKPaint);  overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISKPaint);
    procedure DrawPaint(const APaint: ISKPaint);
    procedure DrawPatch(const ACubics: TSKPatchCubics; const AColors: TSKPatchColors; const ATexCoords: TSKPatchTexCoords; const APaint: ISKPaint; const ABlendMode: TSKBlendMode = TSKBlendMode.Modulate);
    procedure DrawPath(const APath: ISKPath; const APaint: ISKPaint);
    procedure DrawPicture(const APicture: ISKPicture); overload;
    procedure DrawPicture(const APicture: ISKPicture; const AMatrix: TMatrix; const APaint: ISKPaint); overload;
    procedure DrawPoint(const APoint: TPointF; const APaint: ISKPaint); overload;
    procedure DrawPoint(const AX, AY: Single; const APaint: ISKPaint); overload;
    procedure DrawPoints(const AMode: TSKDrawPointsMode; const APoints: TArray<TPointF>; const APaint: ISKPaint);
    procedure DrawRect(const ARect: TRectF; const APaint: ISKPaint);
    procedure DrawRegion(const ARegion: ISKRegion; const APaint: ISKPaint);
    procedure DrawRoundRect(const ARoundRect: ISKRoundRect; const APaint: ISKPaint); overload;
    procedure DrawRoundRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single; const APaint: ISKPaint); overload;
    procedure DrawRoundRectDifference(const AOuter, AInner: ISKRoundRect; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: string; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: UCS4String; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: UTF8String; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawTextBlob(const ATextBlob: ISKTextBlob; const AX, AY: Single; const APaint: ISKPaint); overload;
    procedure DrawVertices(const AVertices: ISKVertices; const APaint: ISKPaint; const ABlendMode: TSKBlendMode = TSKBlendMode.Modulate);
    function FindMarkedCTM(const AName: string; out AMatrix: TMatrix3D): Boolean;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAsMatrix: TMatrix;
    function GetSaveCount: Integer;
    function IsClipEmpty: Boolean;
    function IsClipRect: Boolean;
    procedure MarkCTM(const AName: string);
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISKPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure Save;
    procedure SaveLayer(const APaint: ISKPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISKPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const SX, SY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const AKX, AKY: Single);
    procedure Translate(const DX, DY: Single);
    property DeviceClipBounds: TRect read GetDeviceClipBounds;
    property LocalClipBounds: TRectF read GetLocalClipBounds;
    property LocalToDevice: TMatrix3D read GetLocalToDevice;
    property SaveCount: Integer read GetSaveCount;
  end;

  { TSKCanvas }

  TSKCanvas = class(TSKObject, ISKCanvas)
  strict protected
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISKPath; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISKRegion; const AOp: TSKClipOp = TSKClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISKRoundRect; const AOp: TSKClipOp = TSKClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISKShader; const AOp: TSKClipOp = TSKClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue: ISKData);
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISKPaint);
    procedure DrawAtlas(const AAtlas: ISKImage; const ATansforms: TArray<TSKRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode; const ASampling: TSKSamplingOptions; const AColors: TArray<TAlphaColor> = []; const APaint: ISKPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISKImage; const ATansforms: TArray<TSKRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode; const ASampling: TSKSamplingOptions; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = []; const APaint: ISKPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISKPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISKPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSKBlendMode = TSKBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSKBlendMode = TSKBlendMode.SrcOver); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AOrigin: TPointF; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TSKRotationScaleMatrix>; const AOrigin: TPointF; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawImage(const AImage: ISKImage; const AX, AY: Single; const APaint: ISKPaint = nil); overload;
    procedure DrawImage(const AImage: ISKImage; const AX, AY: Single; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil); overload;
    procedure DrawImageLattice(const AImage: ISKImage; const ALattice: TSKLattice; const ADest: TRectF; const AFilterMode: TSKFilterMode = TSKFilterMode.Nearest; const APaint: ISKPaint = nil);
    procedure DrawImageNine(const AImage: ISKImage; const ACenter: TRect; const ADest: TRectF; const AFilterMode: TSKFilterMode; const APaint: ISKPaint = nil);
    procedure DrawImageRect(const AImage: ISKImage; const ADest: TRectF; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil; const AConstraint: TSKSrcRectConstraint = TSKSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISKImage; const ASrc, ADest: TRectF; const ASampling: TSKSamplingOptions; const APaint: ISKPaint = nil; const AConstraint: TSKSrcRectConstraint = TSKSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISKPaint); overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISKPaint); overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISKPaint);
    procedure DrawPaint(const APaint: ISKPaint);
    procedure DrawPatch(const ACubics: TSKPatchCubics; const AColors: TSKPatchColors; const ATexCoords: TSKPatchTexCoords; const APaint: ISKPaint; const ABlendMode: TSKBlendMode = TSKBlendMode.Modulate);
    procedure DrawPath(const APath: ISKPath; const APaint: ISKPaint);
    procedure DrawPicture(const APicture: ISKPicture); overload;
    procedure DrawPicture(const APicture: ISKPicture; const AMatrix: TMatrix; const APaint: ISKPaint); overload;
    procedure DrawPoint(const APoint: TPointF; const APaint: ISKPaint); overload;
    procedure DrawPoint(const AX, AY: Single; const APaint: ISKPaint); overload;
    procedure DrawPoints(const AMode: TSKDrawPointsMode; const APoints: TArray<TPointF>; const APaint: ISKPaint);
    procedure DrawRect(const ARect: TRectF; const APaint: ISKPaint);
    procedure DrawRegion(const ARegion: ISKRegion; const APaint: ISKPaint);
    procedure DrawRoundRect(const ARoundRect: ISKRoundRect; const APaint: ISKPaint); overload;
    procedure DrawRoundRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single; const APaint: ISKPaint); overload;
    procedure DrawRoundRectDifference(const AOuter, AInner: ISKRoundRect; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: string; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: UCS4String; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleText(const AText: UTF8String; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX, AY: Single; const AFont: ISKFont; const APaint: ISKPaint); overload;
    procedure DrawTextBlob(const ATextBlob: ISKTextBlob; const AX, AY: Single; const APaint: ISKPaint); overload;
    procedure DrawVertices(const AVertices: ISKVertices; const APaint: ISKPaint; const ABlendMode: TSKBlendMode = TSKBlendMode.Modulate);
    function FindMarkedCTM(const AName: string; out AMatrix: TMatrix3D): Boolean;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAsMatrix: TMatrix;
    function GetSaveCount: Integer;
    function IsClipEmpty: Boolean;
    function IsClipRect: Boolean;
    procedure MarkCTM(const AName: string);
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISKPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure Save;
    procedure SaveLayer(const APaint: ISKPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISKPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const SX, SY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const AKX, AKY: Single);
    procedure Translate(const DX, DY: Single);
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSKDateTime }

  TSKDateTime = record
    TimeZoneMinutes: SmallInt;
    Year: Word;
    Month: Byte;
    DayOfWeek: Byte;
    Day: Byte;
    Hour: Byte;
    Minute: Byte;
    Second: Byte;
    constructor Create(const ATimeZoneMinutes: SmallInt; const AYear: Word; const AMonth, ADayOfWeek, ADay, AHour, AMinute, ASecond: Byte);
    function ToISO8601: ISKString;
    class operator Implicit(const ADateTime: TDateTime): TSKDateTime;
    class operator Implicit(const ADateTime: TSKDateTime): TDateTime;
  end;

  { TSKPDFMetadata }

  TSKPDFMetadata = record
    Title: ISKString;
    Author: ISKString;
    Subject: ISKString;
    Keywords: ISKString;
    Creator: ISKString;
    Producer: ISKString;
    Creation: TSKDateTime;
    Modified: TSKDateTime;
    RasterDPI: Single;
    PDFA: Boolean;
    EncodingQuality: Integer;
    constructor Create(const ATitle, AAuthor, ASubject, AKeywords, ACreator: ISKString);
    class operator Explicit(const APDFMetadata: TSKPDFMetadata): sk_pdfmetadata_t;
  end;

  { ISKDocument }

  ISKDocument = interface(ISKReferenceCounted)
    ['{E936B2C0-95A0-4577-BC1D-251768B049C6}']
    function BeginPage(const AWidth, AHeight: Single): ISKCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISKCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
  end;

  { TSKDocument }

  TSKDocument = class(TSKReferenceCounted, ISKDocument)
  strict protected
    function BeginPage(const AWidth, AHeight: Single): ISKCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISKCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
  public
    class function MakePDF(const AWStream: ISKWStream): ISKDocument; overload;
    class function MakePDF(const AWStream: ISKWStream; const AMetadata: TSKPDFMetadata): ISKDocument; overload;
  end;

  { TSKSVGCanvas }

  TSKSVGCanvas = record
  public
    class function Make(const ABounds: TRectF; const AWStream: ISKWStream): ISKCanvas; static;
  end;

  { TSKGraphics }

  TSKGraphics = record
    class procedure AllowJIT; static;
    class procedure DumpMemoryStatistics(const ATraceMemoryDump: ISKTraceMemoryDump); static;
    class function GetFontCacheCountLimit: Integer; static;
    class function GetFontCacheCountUsed: Integer; static;
    class function GetFontCacheLimit: NativeUInt; static;
    class function GetFontCacheUsed: NativeUInt; static;
    class function GetResourceCacheSingleAllocationByteLimit: NativeUInt; static;
    class function GetResourceCacheTotalByteLimit: NativeUInt; static;
    class function GetResourceCacheTotalBytesUsed: NativeUInt; static;
    class procedure Init; static;
    class procedure PurgeAllCaches; static;
    class procedure PurgeFontCache; static;
    class procedure PurgeResourceCache; static;
    class procedure SetFontCacheCountLimit(const AValue: Integer); static;
    class procedure SetFontCacheLimit(const AValue: NativeUInt); static;
    class procedure SetResourceCacheSingleAllocationByteLimit(const AValue: NativeUInt); static;
    class procedure SetResourceCacheTotalByteLimit(const AValue: NativeUInt); static;
    class property FontCacheCountLimit: Integer read GetFontCacheCountLimit write SetFontCacheCountLimit;
    class property FontCacheCountUsed: Integer read GetFontCacheCountUsed;
    class property FontCacheLimit: NativeUInt read GetFontCacheLimit write SetFontCacheLimit;
    class property FontCacheUsed: NativeUInt read GetFontCacheUsed;
    class property ResourceCacheSingleAllocationByteLimit: NativeUInt read GetResourceCacheSingleAllocationByteLimit write SetResourceCacheSingleAllocationByteLimit;
    class property ResourceCacheTotalByteLimit: NativeUInt read GetResourceCacheTotalByteLimit write SetResourceCacheTotalByteLimit;
    class property ResourceCacheTotalBytesUsed: NativeUInt read GetResourceCacheTotalBytesUsed;
  end;

  { ISKCodec }

  ISKCodec = interface(ISKObject)
    ['{6E31698A-318E-4C64-A65A-A69B28F11829}']
    function GetInfo: TSKImageInfo;
    function GetPixels(const ADest: ISKPixmap): Boolean; overload;
    function GetPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    property Info: TSKImageInfo read GetInfo;
  end;

  { TSKCodec }

  TSKCodec = class(TSKObject, ISKCodec)
  strict protected
    function GetInfo: TSKImageInfo;
    function GetPixels(const ADest: ISKPixmap): Boolean; overload;
    function GetPixels(const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    class function MakeFromData(const AData: ISKData): ISKCodec; overload; static;
    class function MakeFromFile(const AFileName: string): ISKCodec; overload; static;
    class function MakeFromStream(const AStream: ISKStream): ISKCodec; overload; static;
  end;

  { ISKSVGSVG }

  ISKSVGSVG = interface(ISKReferenceCounted)
    function GetIntrinsicSize(const AViewPort: TSizeF; const ADPI: Single = 90): TSizeF;
  end;

  { TSKSVGSVG }

  TSKSVGSVG = class(TSKReferenceCounted, ISKSVGSVG)
    function GetIntrinsicSize(const AViewPort: TSizeF; const ADPI: Single = 90): TSizeF;
  end;

  { ISKSVGDOM }

  ISKSVGDOM = interface(ISKReferenceCounted)
    ['{9509D579-27CC-400D-A289-888E7061B7D0}']
    function GetRoot: ISKSVGSVG;
    procedure Render(const ACanvas: ISKCanvas);
    procedure SetContainerSize(const AValue: TSizeF);
    property Root: ISKSVGSVG read GetRoot;
  end;

  { TSKSVGDOM }

  TSKSVGDOM = class(TSKReferenceCounted, ISKSVGDOM)
  strict private
    FRoot: ISKSVGSVG;
  strict protected
    function GetRoot: ISKSVGSVG;
    procedure Render(const ACanvas: ISKCanvas);
    procedure SetContainerSize(const AValue: TSizeF);
  public
    class function Make(const AStream: ISKStream): ISKSVGDOM; static;
  end;

  TSKSkottieAnimationRenderFlag  = (SkipTopLevelIsolation, DisableTopLevelClipping);
  TSKSkottieAnimationRenderFlags = set of TSKSkottieAnimationRenderFlag;

  { ISKSkottieAnimation }

  ISKSkottieAnimation = interface(ISKReferenceCounted)
    ['{EE7A640C-5D3B-4EEF-AEBF-3A125ACD5B3A}']
    function GetDuration: Double;
    function GetFPS: Double;
    function GetInPoint: Double;
    function GetOutPoint: Double;
    function GetSize: TSizeF;
    function GetVersion: ISKString;
    procedure Render(const ACanvas: ISKCanvas; const ARenderFlags: TSKSkottieAnimationRenderFlags = []); overload;
    procedure Render(const ACanvas: ISKCanvas; const ADest: TRectF; const ARenderFlags: TSKSkottieAnimationRenderFlags = []); overload;
    procedure SeekFrame(const ATick: Double);
    procedure SeekFrameTime(const ATick: Double);
    property Duration: Double read GetDuration;
    property FPS: Double read GetFPS;
    property InPoint: Double read GetInPoint;
    property OutPoint: Double read GetOutPoint;
    property Size: TSizeF read GetSize;
    property Version: ISKString read GetVersion;

  end;

  { TSKSkottieAnimation }

  TSKSkottieAnimation = class(TSKReferenceCounted, ISKSkottieAnimation)
  strict protected
    function GetDuration: Double;
    function GetFPS: Double;
    function GetInPoint: Double;
    function GetOutPoint: Double;
    function GetSize: TSizeF;
    function GetVersion: ISKString;
    procedure Render(const ACanvas: ISKCanvas; const ARenderFlags: TSKSkottieAnimationRenderFlags = []); overload;
    procedure Render(const ACanvas: ISKCanvas; const ADest: TRectF; const ARenderFlags: TSKSkottieAnimationRenderFlags = []); overload;
    procedure SeekFrame(const ATick: Double);
    procedure SeekFrameTime(const ATick: Double);
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AData: string): ISKSkottieAnimation; static;
    class function MakeFromFile(const AFileName: string): ISKSkottieAnimation; static;
    class function MakeFromStream(const AStream: ISKStream): ISKSkottieAnimation; static;
  end;


implementation

uses
  {Delphi}
  System.DateUtils,
  System.Math,
  System.SysUtils,
  System.TimeSpan;

{ TSkiaObject }

procedure TSkiaObject.BeforeDestruction;
begin
  inherited;
  Dispose;
end;

class constructor TSkiaObject.Create;
begin
  {$IFDEF SK_DYNAMIC_LOADING}
  if not SkInitialize then
    raise Exception.Create('Unable to initialize Skia.');
  {$ENDIF}
  sk4d_set_debug_msg_proc(debug_msg_proc);
end;

constructor TSkiaObject.CreateNative(const AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

class procedure TSkiaObject.debug_msg_proc(const msg: MarshaledAString);
begin
  if Assigned(FDebugMessageProc) then
    FDebugMessageProc(string(Msg));
end;

class destructor TSkiaObject.Destroy;
begin
  {$IFDEF SK_DYNAMIC_LOADING}
  SkFinalize;
  {$ENDIF}
end;

class function TSkiaObject.GetHandle(const AObject: ISkiaObject): THandle;
begin
  if not Assigned(AObject) then
    Exit(0);
  Result := (AObject as TSkiaObject).FHandle;
end;

{ TSKObject }

constructor TSKObject.CreateNative(const AHandle: THandle;
  const AOwnsHandle: Boolean);
begin
  inherited CreateNative(AHandle);
  FOwnsHandle := AOwnsHandle;
end;

procedure TSKObject.Dispose;
begin
  if FOwnsHandle then
    DoDestroy(Handle);
end;

{ TSKReferenceCounted }

constructor TSKReferenceCounted.CreateNative(const AHandle: THandle;
  const AAlreadyReferenced: Boolean);
begin
  inherited CreateNative(AHandle);
  if not AAlreadyReferenced then
    DoRef(AHandle);
end;

procedure TSKReferenceCounted.Dispose;
begin
  DoUnref(Handle);
end;

class procedure TSKReferenceCounted.DoRef(const AHandle: THandle);
begin
  sk4d_refcnt_ref(AHandle);
end;

class procedure TSKReferenceCounted.DoUnref(const AHandle: THandle);
begin
  sk4d_refcnt_unref(AHandle);
end;

{ TSKString }

procedure TSKString.Append(const ASrc: ISKString);
begin
  Assert(Assigned(ASrc));
  sk4d_string_append(Handle, GetHandle(ASrc));
end;

constructor TSKString.Create;
begin
  CreateNative(sk4d_string_create);
end;

constructor TSKString.Create(const AString: ISKString);
begin
  Assert(Assigned(AString));
  CreateNative(sk4d_string_create2(GetHandle(AString)));
end;

class procedure TSKString.DoDestroy(const AHandle: THandle);
begin
  sk4d_string_destroy(AHandle);
end;

function TSKString.GetText: string;
begin
  Result := string(sk4d_string_get_text(Handle));
end;

function TSKString.IsEqual(const AString: ISKString): Boolean;
begin
  Assert(Assigned(AString));
  Result := sk4d_string_is_equal(Handle, GetHandle(AString));
end;

procedure TSKString.SetText(const AValue: string);
begin
  sk4d_string_set_text(Handle, MarshaledAString(UTF8String(AValue)));
end;

{ TSKStream }

class procedure TSKStream.DoDestroy(const AHandle: THandle);
begin
  sk4d_stream_destroy(AHandle);
end;

function TSKStream.Duplicate: ISKStream;
var
  LHandle: sk_stream_t;
begin
  LHandle := sk4d_stream_duplicate(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKStream.CreateNative(LHandle);
end;

function TSKStream.Fork: ISKStream;
var
  LHandle: sk_stream_t;
begin
  LHandle := sk4d_stream_fork(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKStream.CreateNative(LHandle);
end;

function TSKStream.GetLength: NativeUInt;
begin
  Result := sk4d_stream_get_length(Handle);
end;

function TSKStream.GetMemoryBase: Pointer;
begin
  Result := sk4d_stream_get_memory_base(Handle);
end;

function TSKStream.GetPosition: NativeUInt;
begin
  Result := sk4d_stream_get_position(Handle);
end;

function TSKStream.HasLength: Boolean;
begin
  Result := sk4d_stream_has_length(Handle);
end;

function TSKStream.HasPosition: Boolean;
begin
  Result := sk4d_stream_has_position(Handle);
end;

function TSKStream.IsAtEnd: Boolean;
begin
  Result := sk4d_stream_is_at_end(Handle);
end;

function TSKStream.Move(const AOffset: LongInt): Boolean;
begin
  Result := sk4d_stream_move(Handle, AOffset);
end;

function TSKStream.Peek(out ABuffer; const ASize: NativeUInt): NativeUInt;
begin
  Result := sk4d_stream_peek(Handle, @ABuffer, ASize);
end;

function TSKStream.Read(out ABuffer; const ASize: NativeUInt): NativeUInt;
begin
  Result := sk4d_stream_read(Handle, @ABuffer, ASize);
end;

function TSKStream.Rewind: Boolean;
begin
  Result := sk4d_stream_rewind(Handle);
end;

function TSKStream.Seek(const APosition: NativeUInt): Boolean;
begin
  Result := sk4d_stream_seek(Handle, APosition);
end;

{ TSKStreamAdapter }

class constructor TSKStreamAdapter.Create;
var
  LProcs: sk_managedstream_procs_t;
begin
  LProcs.get_length   := get_length_proc;
  LProcs.get_position := get_position_proc;
  LProcs.read         := read_proc;
  LProcs.release      := release_proc;
  LProcs.seek         := seek_proc;
  sk4d_managedstream_set_procs(LProcs);
end;

constructor TSKStreamAdapter.Create(const AStream: TStream;
  const AOwnsStream: Boolean);
begin
  CreateNative(sk4d_managedstream_create(AStream, AOwnsStream));
end;

class function TSKStreamAdapter.get_length_proc(context: Pointer): size_t;
begin
  Result := TStream(context).Size;
end;

class function TSKStreamAdapter.get_position_proc(context: Pointer): size_t;
begin
  Result := TStream(context).Position;
end;

class function TSKStreamAdapter.read_proc(context, buffer: Pointer;
  size: size_t): size_t;
begin
  Result := TStream(context).Read(buffer^, size);
end;

class procedure TSKStreamAdapter.release_proc(context: Pointer);
begin
  TStream(context).Free;
end;

class function TSKStreamAdapter.seek_proc(context: Pointer;
  position: size_t): bool;
begin
  TStream(context).Position := position;
  Result := True;
end;

{ TSKFileStream }

constructor TSKFileStream.Create(const AFileName: string);
begin
  CreateNative(sk4d_filestream_create(MarshaledAString(UTF8String(AFileName))));
end;

function TSKFileStream.IsValid: Boolean;
begin
  Result := sk4d_filestream_is_valid(Handle);
end;

{ TSKMemoryStream }

constructor TSKMemoryStream.Create(const AData: Pointer;
  const ASize: NativeUInt; const ACopy: Boolean);
var
  LData: ISKData;
begin
  if not ACopy then
    LData := TSKData.Make(AData, ASize)
  else
    LData := TSKData.MakeWithCopy(AData, ASize);
  Create(LData);
end;

constructor TSKMemoryStream.Create(const AData: ISKData);
begin
  CreateNative(sk4d_memorystream_create(GetHandle(AData)));
end;

{ TSKWStream }

class procedure TSKWStream.DoDestroy(const AHandle: THandle);
begin
  sk4d_wstream_destroy(AHandle);
end;

procedure TSKWStream.Flush;
begin
  sk4d_wstream_flush(Handle);
end;

function TSKWStream.GetBytesWritten: NativeUInt;
begin
  Result := sk4d_wstream_get_bytes_written(Handle);
end;

function TSKWStream.Write(const ABuffer; const ASize: NativeUInt): Boolean;
begin
  Result := sk4d_wstream_write(Handle, @ABuffer, ASize);
end;

function TSKWStream.WriteStream(const AStream: ISKStream;
  const ASize: NativeUInt): Boolean;
begin
  Result := sk4d_wstream_write_stream(Handle, GetHandle(AStream), ASize);
end;

{ TSKWStreamAdapter }

class constructor TSKWStreamAdapter.Create;
var
  LProcs: sk_managedwstream_procs_t;
begin
  LProcs.release := release_proc;
  LProcs.write   := write_proc;
  sk4d_managedwstream_set_procs(LProcs);
end;

constructor TSKWStreamAdapter.Create(const AStream: TStream;
  const AOwnsStream: Boolean);
begin
  CreateNative(sk4d_managedwstream_create(AStream, AOwnsStream));
end;

class procedure TSKWStreamAdapter.release_proc(context: Pointer);
begin
  TStream(context).Free;
end;

class function TSKWStreamAdapter.write_proc(context: Pointer;
  const buffer: Pointer; size: size_t): bool;
begin
  TStream(context).Write(buffer^, size);
  Result := True;
end;

{ TSKDynamicMemoryWStream }

procedure TSKDynamicMemoryWStream.CopyTo(const ADest: Pointer);
begin
  sk4d_dynamicmemorywstream_copy_to(Handle, ADest);
end;

constructor TSKDynamicMemoryWStream.Create;
begin
  CreateNative(sk4d_dynamicmemorywstream_create);
end;

function TSKDynamicMemoryWStream.DetachAsData: ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_dynamicmemorywstream_detach_as_data(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

function TSKDynamicMemoryWStream.DetachAsStream: ISKStreamAsset;
var
  LHandle: sk_streamasset_t;
begin
  LHandle := sk4d_dynamicmemorywstream_detach_as_stream(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKStreamAsset.CreateNative(LHandle);
end;

{ TSKFileWStream }

constructor TSKFileWStream.Create(const AFileName: string);
begin
  CreateNative(sk4d_filewstream_create(MarshaledAString(UTF8String(AFileName))));
end;

function TSKFileWStream.IsValid: Boolean;
begin
  Result := sk4d_filewstream_is_valid(Handle);
end;

{ TSKData }

class procedure TSKData.DoRef(const AHandle: THandle);
begin
  sk4d_data_ref(AHandle);
end;

class procedure TSKData.DoUnref(const AHandle: THandle);
begin
  sk4d_data_unref(AHandle);
end;

function TSKData.GetData: Pointer;
begin
  Result := sk4d_data_get_data(Handle);
end;

function TSKData.GetSize: NativeUInt;
begin
  Result := sk4d_data_get_size(Handle);
end;

class function TSKData.Make(const AData: Pointer; const ASize: NativeUInt;
  const AReleaseProc: TSKDataReleaseProc): ISKData;
var
  LContext: Pointer;
  LHandle: sk_data_t;
begin
  LContext := PPointer(@AReleaseProc)^;
  LHandle  := sk4d_data_make(AData, ASize, release_proc, LContext);
  if LHandle = 0 then
    Exit(nil);
  if LContext <> nil then
    IInterface(LContext)._AddRef;
  Result := TSKData.CreateNative(LHandle);
end;

class function TSKData.MakeEmpty: ISKData;
begin
  Result := Make(nil, 0);
end;

class function TSKData.MakeFromFile(const AFileName: string): ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_data_make_from_file(MarshaledAString(UTF8String(AFileName)));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

class function TSKData.MakeFromStream(const AStream: ISKStream;
  const ASize: NativeUInt): ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_data_make_from_stream(GetHandle(AStream), ASize);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

class function TSKData.MakeUninitialized(const ASize: NativeUInt): ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_data_make_uninitialized(ASize);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

class function TSKData.MakeWithCopy(const AData: Pointer;
  const ASize: NativeUInt): ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_data_make_with_copy(AData, ASize);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

class procedure TSKData.release_proc(const data: Pointer; context: Pointer);
begin
  if context <> nil then
  begin
    TSKDataReleaseProc(context)(data);
    IInterface(context)._Release;
  end;
end;

{ TSKColorSpaceMatrix33 }

class function TSKColorSpaceMatrix33.AdobeRGBGamut: TSKColorSpaceMatrix33;
begin
  sk4d_colorspacematrix33_adobe_rgb_gamut(sk_colorspacematrix33_t(Result));
end;

class function TSKColorSpaceMatrix33.DisplayP3Gamut: TSKColorSpaceMatrix33;
begin
  sk4d_colorspacematrix33_display_p3(sk_colorspacematrix33_t(Result));
end;

class function TSKColorSpaceMatrix33.Rec2020Gamut: TSKColorSpaceMatrix33;
begin
  sk4d_colorspacematrix33_rec_2020(sk_colorspacematrix33_t(Result));
end;

class function TSKColorSpaceMatrix33.SRGBGamut: TSKColorSpaceMatrix33;
begin
  sk4d_colorspacematrix33_srgb_gamut(sk_colorspacematrix33_t(Result));
end;

class function TSKColorSpaceMatrix33.XYZGamut: TSKColorSpaceMatrix33;
begin
  sk4d_colorspacematrix33_xyz(sk_colorspacematrix33_t(Result));
end;

{ TSKColorSpacePrimaries }

function TSKColorSpacePrimaries.GetToXYZD50(
  out ADest: TSKColorSpaceMatrix33): Boolean;
begin
  Result := sk4d_colorspaceprimaries_get_to_xyz_d50(sk_colorspaceprimaries_t(Self), sk_colorspacematrix33_t(ADest));
end;

{ TSKColorSpaceTransferFunction }

class function TSKColorSpaceTransferFunction.HLG: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_hlg(sk_colorspacetransferfn_t(Result));
end;

class function TSKColorSpaceTransferFunction.Linear: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_linear(sk_colorspacetransferfn_t(Result));
end;

class function TSKColorSpaceTransferFunction.PQ: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_pq(sk_colorspacetransferfn_t(Result));
end;

class function TSKColorSpaceTransferFunction.Rec2020: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_rec2020(sk_colorspacetransferfn_t(Result));
end;

class function TSKColorSpaceTransferFunction.SRGB: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_srgb(sk_colorspacetransferfn_t(Result));
end;

class function TSKColorSpaceTransferFunction.TwoDotTwo: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspacetransferfn_two_dot_two(sk_colorspacetransferfn_t(Result));
end;

{ TSKColorSpace }

class procedure TSKColorSpace.DoRef(const AHandle: THandle);
begin
  sk4d_colorspace_ref(AHandle);
end;

class procedure TSKColorSpace.DoUnref(const AHandle: THandle);
begin
  sk4d_colorspace_unref(AHandle);
end;

function TSKColorSpace.GetGammaCloseToSRGB: Boolean;
begin
  Result := sk4d_colorspace_get_gamma_close_to_srgb(Handle);
end;

function TSKColorSpace.GetGammaIsLinear: Boolean;
begin
  Result := sk4d_colorspace_get_gamma_is_linear(Handle);
end;

function TSKColorSpace.GetGamut(
  const ADest: ISKColorSpace): TSKColorSpaceMatrix33;
begin
  sk4d_colorspace_get_gamut(Handle, GetHandle(ADest), sk_colorspacematrix33_t(Result));
end;

function TSKColorSpace.GetInverseTransferFunction: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspace_get_inverse_transfer_fn(Handle, sk_colorspacetransferfn_t(Result));
end;

function TSKColorSpace.GetToXYZD50: TSKColorSpaceMatrix33;
begin
  sk4d_colorspace_get_to_xyz_d50(Handle, sk_colorspacematrix33_t(Result));
end;

function TSKColorSpace.GetTransferFunction: TSKColorSpaceTransferFunction;
begin
  sk4d_colorspace_get_transfer_fn(Handle, sk_colorspacetransferfn_t(Result));
end;

function TSKColorSpace.IsEqual(const AColorSpace: ISKColorSpace): Boolean;
begin
  Result := sk4d_colorspace_is_equal(Handle, GetHandle(AColorSpace));
end;

function TSKColorSpace.IsNumericalTransferFunction(
  out ATransferFunction: TSKColorSpaceTransferFunction): Boolean;
begin
  Result := sk4d_colorspace_is_numerical_transfer_fn(Handle, sk_colorspacetransferfn_t(ATransferFunction));
end;

function TSKColorSpace.IsSRGB: Boolean;
begin
  Result := sk4d_colorspace_is_srgb(Handle);
end;

class function TSKColorSpace.Make(
  const AProfile: TSKColorSpaceICCProfile): ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make(sk_colorspaceiccprofile_t(AProfile));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKColorSpace.MakeColorSpin: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_color_spin(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKColorSpace.MakeLinearGamma: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_linear_gamma(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

class function TSKColorSpace.MakeRGB(
  const ATransferFunction: TSKColorSpaceTransferFunction;
  const AToXyzD50: TMatrix): ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_rgb(sk_colorspacetransferfn_t(ATransferFunction), sk_colorspacematrix33_t(AToXyzD50));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

class function TSKColorSpace.MakeSRGB: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_srgb;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKColorSpace.MakeSRGBGamma: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_srgb_gamma(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

class function TSKColorSpace.MakeSRGBLinear: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_colorspace_make_srgb_linear;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKColorSpace.ToProfile: TSKColorSpaceICCProfile;
begin
  sk4d_colorspace_to_profile(Handle, sk_colorspaceiccprofile_t(Result));
end;

{ TSKImageInfo }

function TSKImageInfo.ByteSize(const ARowBytes: NativeUInt): NativeUInt;
begin
  Result := ARowBytes * NativeUInt(Height);
end;

function TSKImageInfo.BytesPerPixel: Integer;
begin
  case ColorType of
    TSKColorType.Alpha8         : Result :=  1;
    TSKColorType.RGB565         : Result :=  2;
    TSKColorType.ARGB4444       : Result :=  2;
    TSKColorType.RGBA8888       : Result :=  4;
    TSKColorType.RGB888X        : Result :=  4;
    TSKColorType.BGRA8888       : Result :=  4;
    TSKColorType.RGBA1010102    : Result :=  4;
    TSKColorType.BGRA1010102    : Result :=  4;
    TSKColorType.RGB101010X     : Result :=  4;
    TSKColorType.BGR101010X     : Result :=  4;
    TSKColorType.Gray8          : Result :=  1;
    TSKColorType.RGBAF16        : Result :=  8;
    TSKColorType.RGBAF16Clamped : Result :=  8;
    TSKColorType.RGBAF32        : Result := 16;
    TSKColorType.RG88           : Result :=  2;
    TSKColorType.AlphaF16       : Result :=  2;
    TSKColorType.RgF16          : Result :=  4;
    TSKColorType.Alpha16        : Result :=  2;
    TSKColorType.RG1616         : Result :=  4;
    TSKColorType.RGBA16161616   : Result :=  8;
  else
    Result := 0;
  end;
end;

constructor TSKImageInfo.Create(const AWidth, AHeight: Integer;
  const AColorType: TSKColorType; const AAlphaType: TSKAlphaType;
  const AColorSpace: ISKColorSpace);
begin
  Width      := AWidth;
  Height     := AHeight;
  ColorType  := AColorType;
  AlphaType  := AAlphaType;
  ColorSpace := AColorSpace;
end;

class operator TSKImageInfo.Equal(const AImageInfo1,
  AImageInfo2: TSKImageInfo): Boolean;
begin
  Result := (AImageInfo1.Width     = AImageInfo2.Width    ) and
            (AImageInfo1.Height    = AImageInfo2.Height   ) and
            (AImageInfo1.ColorType = AImageInfo2.ColorType) and
            (AImageInfo1.AlphaType = AImageInfo2.AlphaType) and
            (Assigned(AImageInfo1.ColorSpace) = Assigned(AImageInfo1.ColorSpace)) and ((not Assigned(AImageInfo1.ColorSpace)) or (AImageInfo1.ColorSpace.IsEqual(AImageInfo2.ColorSpace)));
end;

class operator TSKImageInfo.Explicit(
  const AImageInfo: TSKImageInfo): sk_imageinfo_t;
begin
  Result.width      := AImageInfo.Width;
  Result.height     := AImageInfo.Height;
  Result.colortype  := sk_colortype_t(AImageInfo.ColorType);
  Result.alphatype  := sk_alphatype_t(AImageInfo.AlphaType);
  Result.colorspace := TSkiaObject.GetHandle(AImageInfo.ColorSpace);
end;

class operator TSKImageInfo.Explicit(
  const AImageInfo: sk_imageinfo_t): TSKImageInfo;
begin
  Result.Width     := AImageInfo.width;
  Result.Height    := AImageInfo.height;
  Result.ColorType := TSKColorType(AImageInfo.colortype);
  Result.AlphaType := TSKAlphaType(AImageInfo.alphatype);
  if AImageInfo.colorspace = 0 then
    Result.ColorSpace := nil
  else
    Result.ColorSpace := TSKColorSpace.CreateNative(AImageInfo.colorspace);
end;

function TSKImageInfo.IsEmpty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TSKImageInfo.IsOpaque: Boolean;
begin
  Result := AlphaType = TSKAlphaType.Opaque;
end;

function TSKImageInfo.IsValid: Boolean;
const
  MaxDimension = MaxInt shr 2;
begin
  if (IsEmpty) or (Width > MaxDimension) or (Height > MaxDimension) or (ColorType = TSKColorType.Unknown) or (AlphaType = TSKAlphaType.Unknown) then
    Exit(False);
  case ColorType of
    TSKColorType.Alpha8,
    TSKColorType.AlphaF16,
    TSKColorType.Alpha16:
      begin
        if (AlphaType <> TSKAlphaType.Opaque) and (AlphaType <> TSKAlphaType.Premul) then
          Exit(False);
      end;
    TSKColorType.RGB565,
    TSKColorType.RGB888X,
    TSKColorType.RGB101010X,
    TSKColorType.BGR101010X,
    TSKColorType.Gray8,
    TSKColorType.RG88,
    TSKColorType.RgF16,
    TSKColorType.RG1616:
      begin
        if AlphaType <> TSKAlphaType.Opaque then
          Exit(False);
      end;
  end;
  Result := True;
end;

function TSKImageInfo.IsValidRowBytes(const ARowBytes: NativeUInt): Boolean;
begin
  if ARowBytes < MinRowBytes then
    Exit(False);
  Result := ARowBytes = (ARowBytes shr ShiftPerPixel shl ShiftPerPixel);
end;

function TSKImageInfo.MakeAlphaType(
  const AAlphaType: TSKAlphaType): TSKImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AAlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSKImageInfo.MakeColorSpace(
  const AColorSpace: ISKColorSpace): TSKImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := AColorSpace;
end;

function TSKImageInfo.MakeColorType(
  const AColorType: TSKColorType): TSKImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := AColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSKImageInfo.MakeDimensions(const AWidth,
  AHeight: Integer): TSKImageInfo;
begin
  Result.Width      := AWidth;
  Result.Height     := AHeight;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSKImageInfo.MinByteSize: NativeUInt;
begin
  Result := ByteSize(MinRowBytes);
end;

function TSKImageInfo.MinRowBytes: NativeUInt;
begin
  Result := Width * BytesPerPixel;
end;

class operator TSKImageInfo.NotEqual(const AImageInfo1,
  AImageInfo2: TSKImageInfo): Boolean;
begin
  Result := not (AImageInfo1 = AImageInfo2);
end;

function TSKImageInfo.ShiftPerPixel: Integer;
begin
  case ColorType of
    TSKColorType.Alpha8         : Result := 0;
    TSKColorType.RGB565         : Result := 1;
    TSKColorType.ARGB4444       : Result := 1;
    TSKColorType.RGBA8888       : Result := 2;
    TSKColorType.RGB888X        : Result := 2;
    TSKColorType.BGRA8888       : Result := 2;
    TSKColorType.RGBA1010102    : Result := 2;
    TSKColorType.BGRA1010102    : Result := 2;
    TSKColorType.RGB101010X     : Result := 2;
    TSKColorType.BGR101010X     : Result := 2;
    TSKColorType.Gray8          : Result := 0;
    TSKColorType.RGBAF16        : Result := 3;
    TSKColorType.RGBAF16Clamped : Result := 3;
    TSKColorType.RGBAF32        : Result := 4;
    TSKColorType.RG88           : Result := 1;
    TSKColorType.AlphaF16       : Result := 1;
    TSKColorType.RGF16          : Result := 2;
    TSKColorType.Alpha16        : Result := 1;
    TSKColorType.RG1616         : Result := 2;
    TSKColorType.RGBA16161616   : Result := 3;
  else
    Result := 0;
  end;
end;

{ TSKCubicResampler }

class function TSKCubicResampler.CatmullRom: TSKCubicResampler;
begin
  Result.B := 0;
  Result.C := 1/2;
end;

class operator TSKCubicResampler.Equal(const ACubicResampler1,
  ACubicResampler2: TSKCubicResampler): Boolean;
begin
  Result := (ACubicResampler1.B = ACubicResampler2.B) and
            (ACubicResampler1.C = ACubicResampler2.C);
end;

class function TSKCubicResampler.Mitchell: TSKCubicResampler;
begin
  Result.B := 1/3;
  Result.C := 1/3;
end;

class operator TSKCubicResampler.NotEqual(const ACubicResampler1,
  ACubicResampler2: TSKCubicResampler): Boolean;
begin
  Result := not (ACubicResampler1 = ACubicResampler2);
end;

{ TSKSamplingOptions }

constructor TSKSamplingOptions.Create(const ACubic: TSKCubicResampler);
begin
  UseCubic := True;
  Cubic    := ACubic;
end;

constructor TSKSamplingOptions.Create(const AFilter: TSKFilterMode;
  const AMipmap: TSKMipmapMode);
begin
  UseCubic := False;
  Filter   := AFilter;
  Mipmap   := AMipmap;
end;

class operator TSKSamplingOptions.Equal(const ASamplingOptions1,
  ASamplingOptions2: TSKSamplingOptions): Boolean;
begin
  if ASamplingOptions1.UseCubic <> ASamplingOptions2.UseCubic then
    Exit(False);
  if ASamplingOptions1.UseCubic then
    Result := ASamplingOptions1.Cubic = ASamplingOptions2.Cubic
  else
    Result := (ASamplingOptions1.Filter = ASamplingOptions2.Filter) and (ASamplingOptions1.Mipmap = ASamplingOptions2.Mipmap);
end;

class operator TSKSamplingOptions.Explicit(
  const ASamplingOptions: TSKSamplingOptions): sk_samplingoptions_t;
begin
  case ASamplingOptions.UseCubic of
    True:
      begin
        Result.use_cubic := True;
        Result.cubic     := sk_cubicresampler_t(ASamplingOptions.Cubic);
      end;
    False:
      begin
        Result.use_cubic := False;
        Result.filter := sk_filtermode_t(ASamplingOptions.Filter);
        Result.mipmap := sk_mipmapmode_t(ASamplingOptions.Mipmap);
      end;
  end;
end;

class function TSKSamplingOptions.High: TSKSamplingOptions;
begin
  Result := TSKSamplingOptions.Create(TSKCubicResampler.Mitchell);
end;

class function TSKSamplingOptions.Low: TSKSamplingOptions;
begin
  Result := TSKSamplingOptions.Create(TSKFilterMode.Linear, TSKMipmapMode.None);
end;

class function TSKSamplingOptions.Medium: TSKSamplingOptions;
begin
  TSKSamplingOptions.Create(TSKFilterMode.Linear, TSKMipmapMode.Nearest);
end;

class operator TSKSamplingOptions.NotEqual(const ASamplingOptions1,
  ASamplingOptions2: TSKSamplingOptions): Boolean;
begin
  Result := not (ASamplingOptions1 = ASamplingOptions2);
end;

{ TSKPixmap }

constructor TSKPixmap.Create(const AImageInfo: TSKImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt);
begin
  CreateNative(sk4d_pixmap_create2(sk_imageinfo_t(AImageInfo), APixels, ARowBytes));
end;

constructor TSKPixmap.Create;
begin
  CreateNative(sk4d_pixmap_create);
end;

class procedure TSKPixmap.DoDestroy(const AHandle: THandle);
begin
  sk4d_pixmap_destroy(AHandle);
end;

function TSKPixmap.Erase(const AColor: TAlphaColor): Boolean;
begin
  Result := sk4d_pixmap_erase(Handle, AColor, nil);
end;

function TSKPixmap.Erase(const AColor: TAlphaColorF; const ASubset: TRectF;
  const AColorSpace: ISKColorSpace): Boolean;
begin
  Result := sk4d_pixmap_erase2(Handle, sk_color4f_t(AColor), GetHandle(AColorSpace), @sk_irect_t(ASubset));
end;

function TSKPixmap.Erase(const AColor: TAlphaColorF;
  const AColorSpace: ISKColorSpace): Boolean;
begin
  Result := sk4d_pixmap_erase2(Handle, sk_color4f_t(AColor), GetHandle(AColorSpace), nil);
end;

function TSKPixmap.Erase(const AColor: TAlphaColor;
  const ASubset: TRectF): Boolean;
begin
  Result := sk4d_pixmap_erase(Handle, AColor, @sk_irect_t(ASubset));
end;

function TSKPixmap.ExtractSubset(const ADest: ISKPixmap;
  const AArea: TRect): Boolean;
begin
  Result := sk4d_pixmap_extract_subset(GetHandle(ADest), Handle, sk_irect_t(AArea));
end;

function TSKPixmap.GetAlpha(const AX, AY: Integer): Single;
begin
  Result := sk4d_pixmap_get_alpha(Handle, AX, AY);
end;

function TSKPixmap.GetAlphaType: TSKAlphaType;
begin
  Result := TSKAlphaType(sk4d_pixmap_get_alpha_type(Handle));
end;

function TSKPixmap.GetColor(const AX, AY: Integer): TAlphaColor;
begin
  Result := sk4d_pixmap_get_color(Handle, AX, AY);
end;

function TSKPixmap.GetColorSpace: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_pixmap_get_color_space(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKPixmap.GetColorType: TSKColorType;
begin
  Result := TSKColorType(sk4d_pixmap_get_color_type(Handle));
end;

function TSKPixmap.GetHeight: Integer;
begin
  Result := sk4d_pixmap_get_height(Handle);
end;

function TSKPixmap.GetImageInfo: TSKImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_pixmap_get_image_info(Handle, LResult);
  Result := TSKImageInfo(LResult);
end;

function TSKPixmap.GetPixelAddr(const AX, AY: Integer): Pointer;
begin
  Result := sk4d_pixmap_get_pixel_addr(Handle, AX, AY);
end;

function TSKPixmap.GetPixels: Pointer;
begin
  Result := sk4d_pixmap_get_pixels(Handle);
end;

function TSKPixmap.GetRowBytes: NativeUInt;
begin
  Result := sk4d_pixmap_get_row_bytes(Handle);
end;

function TSKPixmap.GetWidth: Integer;
begin
  Result := sk4d_pixmap_get_width(Handle);
end;

function TSKPixmap.ReadPixels(const ADest: ISKPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_pixmap_read_pixels(Handle, GetHandle(ADest), ASrcX, ASrcY);
end;

function TSKPixmap.ReadPixels(const ADestImageInfo: TSKImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer): Boolean;
var
  LPixmap: ISKPixmap;
begin
  LPixmap := TSKPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY);
end;

function TSKPixmap.ScalePixels(const ADest: ISKPixmap;
  const ASampling: TSKSamplingOptions): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_pixmap_scale_pixels(Handle, GetHandle(ADest), sk_samplingoptions_t(ASampling));
end;

procedure TSKPixmap.SetColorSpace(const AValue: ISKColorSpace);
begin
  sk4d_pixmap_set_colorspace(Handle, GetHandle(AValue));
end;

{ TSKRoundRect }

function TSKRoundRect.Contains(const ARect: TRect): Boolean;
begin
  Result := sk4d_rrect_contains(Handle, sk_rect_t(ARect));
end;

constructor TSKRoundRect.Create(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  Create;
  SetRect(ARect, ARadiusX, ARadiusY);
end;

constructor TSKRoundRect.Create(const ARect: TRectF;
  const ARadii: TSKRoundRectRadii);
begin
  Create;
  SetRect(ARect, ARadii);
end;

constructor TSKRoundRect.Create(const ARoundRect: ISKRoundRect);
begin
  Assert(Assigned(ARoundRect));
  CreateNative(sk4d_rrect_create2(GetHandle(ARoundRect)));
end;

constructor TSKRoundRect.Create;
begin
  CreateNative(sk4d_rrect_create);
end;

procedure TSKRoundRect.Deflate(const ADX, ADY: Single);
begin
  sk4d_rrect_deflate(Handle, ADX, ADY);
end;

class procedure TSKRoundRect.DoDestroy(const AHandle: THandle);
begin
  sk4d_rrect_destroy(AHandle);
end;

function TSKRoundRect.GetDeflate(const ADX, ADY: Single): ISKRoundRect;
var
  LRoundRect: TSKRoundRect;
begin
  LRoundRect := TSKRoundRect.Create;
  sk4d_rrect_deflate2(Handle, ADX, ADY, LRoundRect.Handle);
  Result := LRoundRect;
end;

function TSKRoundRect.GetHeight: Single;
begin
  Result := sk4d_rrect_get_height(Handle);
end;

function TSKRoundRect.GetInflate(const ADX, ADY: Single): ISKRoundRect;
var
  LRoundRect: TSKRoundRect;
begin
  LRoundRect := TSKRoundRect.Create;
  sk4d_rrect_inflate2(Handle, ADX, ADY, LRoundRect.Handle);
  Result := LRoundRect;
end;

function TSKRoundRect.GetOffset(const ADX, ADY: Single): ISKRoundRect;
begin
  Result := TSKRoundRect.CreateNative(sk4d_rrect_make_offset(Handle, ADX, ADY));
end;

function TSKRoundRect.GetRadii(const ACorner: TSKRoundRectCorner): TPointF;
begin
  sk4d_rrect_get_radii(Handle, sk_rrectcorner_t(ACorner), sk_vector_t(Result));
end;

function TSKRoundRect.GetRect: TRectF;
begin
  sk4d_rrect_get_rect(Handle, sk_rect_t(Result));
end;

function TSKRoundRect.GetRoundRectType: TSKRoundRectType;
begin
  Result := TSKRoundRectType(sk4d_rrect_get_type(Handle));
end;

function TSKRoundRect.GetSimpleRadii: TPointF;
begin
  sk4d_rrect_get_simple_radii(Handle, sk_vector_t(Result));
end;

function TSKRoundRect.GetWidth: Single;
begin
  Result := sk4d_rrect_get_width(Handle);
end;

procedure TSKRoundRect.Inflate(const ADX, ADY: Single);
begin
  sk4d_rrect_inflate(Handle, ADX, ADY);
end;

function TSKRoundRect.IsEqual(const ARoundRect: ISKRoundRect): Boolean;
begin
  Assert(Assigned(ARoundRect));
  Result := sk4d_rrect_is_equal(Handle, GetHandle(ARoundRect));
end;

function TSKRoundRect.IsValid: Boolean;
begin
  Result := sk4d_rrect_is_valid(Handle);
end;

procedure TSKRoundRect.Offset(const ADX, ADY: Single);
begin
  sk4d_rrect_offset(Handle, ADX, ADY);
end;

procedure TSKRoundRect.SetEmpty;
begin
  sk4d_rrect_set_empty(Handle);
end;

procedure TSKRoundRect.SetNinePatch(const ARect: TRectF; const ARadiusLeft,
  ARadiusTop, ARadiusRight, ARadiusBottom: Single);
begin
  sk4d_rrect_set_nine_patch(Handle, sk_rect_t(ARect), ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom);
end;

procedure TSKRoundRect.SetOval(const ARect: TRectF);
begin
  sk4d_rrect_set_oval(Handle, sk_rect_t(ARect));
end;

procedure TSKRoundRect.SetRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  sk4d_rrect_set_rect3(Handle, sk_rect_t(ARect), ARadiusX, ARadiusY);
end;

procedure TSKRoundRect.SetRect(const ARect: TRectF;
  const ARadii: TSKRoundRectRadii);
begin
  sk4d_rrect_set_rect2(Handle, sk_rect_t(ARect), @ARadii);
end;

procedure TSKRoundRect.SetRect(const ARect: TRectF);
begin
  sk4d_rrect_set_rect(Handle, sk_rect_t(ARect));
end;

function TSKRoundRect.Transform(const AMatrix: TMatrix): ISKRoundRect;
begin
  Result := TSKRoundRect.Create;
  if not sk4d_rrect_transform(Handle, sk_matrix_t(AMatrix), GetHandle(Result)) then
    Result := nil;
end;

{ TSKPathIterator }

constructor TSKPathIterator.Create(const APath: ISKPath;
  const AForceClose: Boolean);
begin
  Assert(Assigned(APath));
  CreateNative(sk4d_pathiterator_create(GetHandle(APath), AForceClose));
end;

class procedure TSKPathIterator.DoDestroy(const AHandle: THandle);
begin
  sk4d_pathiterator_destroy(AHandle);
end;

function TSKPathIterator.GetConicWeight: Single;
begin
  Result := sk4d_pathiterator_get_conic_weight(Handle);
end;

function TSKPathIterator.GetPoints: TSKPathPoints;
begin
  Result := FPoints;
end;

function TSKPathIterator.GetVerb: TSKPathVerb;
begin
  Result := FVerb;
end;

function TSKPathIterator.Next: Boolean;
var
  LVerb: sk_pathverb_t;
begin
  Result := sk4d_pathiterator_next(Handle, @sk_point_t(FPoints[0]), LVerb);
  FVerb  := TSKPathVerb(LVerb);
end;

{ TSKPath }

function TSKPath.Contains(const AX, AY: Single): Boolean;
begin
  Result := sk4d_path_contains(Handle, AX, AY);
end;

constructor TSKPath.Create;
begin
  CreateNative(sk4d_path_create);
end;

constructor TSKPath.Create(const APath: ISKPath);
begin
  Assert(Assigned(APath));
  CreateNative(sk4d_path_create2(GetHandle(APath)));
end;

constructor TSKPath.Create(const ASVG: string);
begin
  CreateNative(sk4d_path_create3(MarshaledAString(UTF8String(ASVG))));
end;

class procedure TSKPath.DoDestroy(const AHandle: THandle);
begin
  sk4d_path_destroy(AHandle);
end;

function TSKPath.GetBounds: TRectF;
begin
  sk4d_path_get_bounds(Handle, sk_rect_t(Result));
end;

function TSKPath.GetFillType: TSKPathFillType;
begin
  Result := TSKPathFillType(sk4d_path_get_fill_type(Handle));
end;

function TSKPath.GetIterator(const AForceClose: Boolean): ISKPathIterator;
begin
  Result := TSKPathIterator.Create(Self, AForceClose);
end;

function TSKPath.GetLastPoint: TPointF;
begin
  if not sk4d_path_get_last_point(Handle, sk_point_t(Result)) then
    Result := TPointF.Zero;
end;

function TSKPath.GetSegmentMasks: TSKSegmentMasks;
begin
  Result := TSKSegmentMasks(Byte(sk4d_path_get_segment_masks(Handle)));
end;

function TSKPath.GetTightBounds: TRectF;
begin
  sk4d_path_get_tight_bounds(Handle, sk_rect_t(Result));
end;

function TSKPath.Interpolate(const AEnding: ISKPath;
  const AWeight: Single): ISKPath;
begin
  Assert(Assigned(AEnding));
  Result := TSKPath.Create;
  if not sk4d_path_interpolate(Handle, GetHandle(AEnding), AWeight, GetHandle(Result)) then
    Result := nil;
end;

function TSKPath.IsConvex: Boolean;
begin
  Result := sk4d_path_is_convex(Handle);
end;

function TSKPath.IsEmpty: Boolean;
begin
  Result := sk4d_path_is_empty(Handle);
end;

function TSKPath.IsEqual(const APath: ISKPath): Boolean;
begin
  Assert(Assigned(APath));
  Result := sk4d_path_is_equal(Handle, GetHandle(APath));
end;

function TSKPath.IsFinite: Boolean;
begin
  Result := sk4d_path_is_finite(Handle);
end;

function TSKPath.IsInterpolatable(const APath: ISKPath): Boolean;
begin
  Assert(Assigned(APath));
  Result := sk4d_path_is_interpolatable(Handle, GetHandle(APath));
end;

function TSKPath.IsLastContourClosed: Boolean;
begin
  Result := sk4d_path_is_last_contour_closed(Handle);
end;

function TSKPath.IsLine(out APoint1, APoint2: TPointF): Boolean;
var
  LLines: array[0..1] of TPointF;
begin
  LLines[0] := APoint1;
  LLines[1] := APoint2;
  Result := sk4d_path_is_line(Handle, @sk_point_t(LLines[0]));
end;

function TSKPath.IsLine: Boolean;
begin
  Result := sk4d_path_is_line(Handle, nil);
end;

function TSKPath.IsOval: Boolean;
begin
  Result := sk4d_path_is_oval(Handle, nil);
end;

function TSKPath.IsOval(out ARect: TRectF): Boolean;
begin
  Result := sk4d_path_is_oval(Handle, @sk_rect_t(ARect));
end;

function TSKPath.IsRect: Boolean;
begin
  Result := sk4d_path_is_rect(Handle, nil);
end;

function TSKPath.IsRect(out ARect: TRectF): Boolean;
begin
  Result := sk4d_path_is_rect(Handle, @sk_rect_t(ARect));
end;

function TSKPath.IsRoundRect(out ARoundRect: ISKRoundRect): Boolean;
var
  LRoundRect: ISKRoundRect;
begin
  LRoundRect := TSKRoundRect.Create;
  Result     := sk4d_path_is_rrect(Handle, GetHandle(LRoundRect));
  if Result then
    ARoundRect := LRoundRect;
end;

function TSKPath.IsRoundRect: Boolean;
begin
  Result := sk4d_path_is_rrect(Handle, 0);
end;

function TSKPath.Offset(const ADX, ADY: Single): ISKPath;
var
  LPath: TSKPath;
begin
  LPath := TSKPath.Create;
  sk4d_path_offset(Handle, ADX, ADY, LPath.Handle);
  Result := LPath;
end;

function TSKPath.Op(const APath: ISKPath; const AOp: TSKPathOp): ISKPath;
begin
  Assert(Assigned(APath));
  Result := TSKPath.Create;
  if not sk4d_path_op(Handle, GetHandle(APath), sk_pathop_t(AOp), GetHandle(Result)) then
    Result := nil;
end;

function TSKPath.ToSVG: ISKString;
begin
  Result := TSKString.Create;
  sk4d_path_to_svg(Handle, GetHandle(Result));
end;

function TSKPath.Transform(const AMatrix: TMatrix): ISKPath;
begin
  Result := TSKPath.Create;
  sk4d_path_transform(Handle, sk_matrix_t(AMatrix), GetHandle(Result));
end;

{ TSKOpBuilder }

procedure TSKOpBuilder.Add(const APath: ISKPath; const AOp: TSKPathOp);
begin
  Assert(Assigned(APath));
  sk4d_opbuilder_add(Handle, GetHandle(APath), sk_pathop_t(AOp));
end;

constructor TSKOpBuilder.Create;
begin
  CreateNative(sk4d_opbuilder_create);
end;

function TSKOpBuilder.Detach: ISKPath;
begin
  Result := TSKPath.Create;
  if not sk4d_opbuilder_detach(Handle, GetHandle(Result)) then
    Result := nil;
end;

class procedure TSKOpBuilder.DoDestroy(const AHandle: THandle);
begin
  sk4d_opbuilder_destroy(AHandle);
end;

{ TSKPathMeasure }

constructor TSKPathMeasure.Create(const APath: ISKPath;
  const AForceClosed: Boolean; const AResScale: Single);
begin
  Assert(Assigned(APath));
  CreateNative(sk4d_pathmeasure_create(GetHandle(APath), AForceClosed, AResScale));
end;

class procedure TSKPathMeasure.DoDestroy(const AHandle: THandle);
begin
  sk4d_pathmeasure_destroy(AHandle);
end;

function TSKPathMeasure.GetLength: Single;
begin
  Result := sk4d_pathmeasure_get_length(Handle);
end;

function TSKPathMeasure.GetMatrix(const ADistance: Single; out AMatrix: TMatrix;
  const AMatrixFlags: TSKPathMeasureMatrixFlags): Boolean;
begin
  Result := sk4d_pathmeasure_get_matrix(Handle, ADistance, sk_matrix_t(AMatrix), Byte(AMatrixFlags));
end;

function TSKPathMeasure.GetPositionAndTangent(const ADistance: Single;
  out APosition, ATangent: TPointF): Boolean;
begin
  Result := sk4d_pathmeasure_get_position_and_tangent(Handle, ADistance, sk_point_t(APosition), sk_vector_t(ATangent));
end;

function TSKPathMeasure.GetSegment(const AStart, AStop: Single;
  const AStartWithMoveTo: Boolean): ISKPath;
begin
  Result := TSKPath.Create;
  if not sk4d_pathmeasure_get_segment(Handle, AStart, AStop, GetHandle(Result), AStartWithMoveTo) then
    Result := nil;
end;

function TSKPathMeasure.IsClosed: Boolean;
begin
  Result := sk4d_pathmeasure_is_closed(Handle);
end;

function TSKPathMeasure.NextContour: Boolean;
begin
  Result := sk4d_pathmeasure_next_contour(Handle);
end;

{ TSKPathEffect }

class function TSKPathEffect.Make1DPath(const APath: ISKPath; const AAdvance,
  APhase: Single; const AStyle: TSKPathEffect1DStyle): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  Assert(Assigned(APath));
  LHandle := sk4d_patheffect_make_1dpath(GetHandle(APath), AAdvance, APhase, sk_patheffect1dstyle_t(AStyle));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.Make2DLine(const AWidth: Single;
  const AMatrix: TMatrix): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_2dline(AWidth, sk_matrix_t(AMatrix));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.Make2DPath(const AMatrix: TMatrix;
  const APath: ISKPath): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  Assert(Assigned(APath));
  LHandle := sk4d_patheffect_make_2dpath(sk_matrix_t(AMatrix), GetHandle(APath));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeCompose(const AOuter,
  AInner: ISKPathEffect): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_compose(GetHandle(AOuter), GetHandle(AInner));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeCorner(const ARadius: Single): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_corner(ARadius);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeDash(const AIntervals: TArray<Single>;
  const APhase: Single): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_dash(@AIntervals[0], Length(AIntervals), APhase);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeDiscrete(const ASegLength, ADeviation: Single;
  const ASeedAssist: Cardinal): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_discrete(ASegLength, ADeviation, ASeedAssist);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeSum(const AEffect1,
  AEffect2: ISKPathEffect): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_sum(GetHandle(AEffect1), GetHandle(AEffect2));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

class function TSKPathEffect.MakeTrim(const AStart, AStop: Single;
  const AMode: TSKPathEffectTrimMode): ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_patheffect_make_trim(AStart, AStop, sk_patheffecttrimmode_t(AMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

{ TSKPathBuilder }

procedure TSKPathBuilder.AddArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single);
begin
  sk4d_pathbuilder_add_arc(Handle, sk_rect_t(AOval), AStartAngle, ASweepAngle);
end;

procedure TSKPathBuilder.AddCircle(const ACenterX, ACenterY, ARadius: Single;
  ADirection: TSKPathDirection);
begin
  sk4d_pathbuilder_add_circle(Handle,ACenterX, ACenterY, ARadius, sk_pathdirection_t(ADirection));
end;

procedure TSKPathBuilder.AddCircle(const ACenter: TPointF; ARadius: Single;
  ADirection: TSKPathDirection);
begin
  AddCircle(ACenter.X, ACenter.Y, ARadius, ADirection);
end;

procedure TSKPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSKPathDirection);
begin
  AddOval(ARect, ADirection, 1);
end;

procedure TSKPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSKPathDirection; AStartIndex: Cardinal);
begin
  sk4d_pathbuilder_add_oval(Handle, sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSKPathBuilder.AddPath(const APath: ISKPath);
begin
  Assert(Assigned(APath));
  sk4d_pathbuilder_add_path(Handle, GetHandle(APath));
end;

procedure TSKPathBuilder.AddPolygon(const APolygon: TPolygon;
  const IsClosed: Boolean);
begin
  sk4d_pathbuilder_add_polygon(Handle, @sk_point_t(APolygon[0]), Length(APolygon), IsClosed);
end;

procedure TSKPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSKPathDirection; AStartIndex: Cardinal);
begin
  sk4d_pathbuilder_add_rect(Handle, sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSKPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSKPathDirection);
begin
  AddRect(ARect, ADirection, 0);
end;

procedure TSKPathBuilder.AddRoundRect(const ARoundRect: ISKRoundRect;
  ADirection: TSKPathDirection);
begin
  AddRoundRect(ARoundRect, ADirection, IfThen(ADirection = TSKPathDirection.CW, 6, 7));
end;

procedure TSKPathBuilder.AddRoundRect(const ARoundRect: ISKRoundRect;
  ADirection: TSKPathDirection; AStartIndex: Cardinal);
begin
  Assert(Assigned(ARoundRect));
  sk4d_pathbuilder_add_rrect(Handle, GetHandle(ARoundRect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSKPathBuilder.ArcTo(const APoint1, APoint2: TPointF;
  const ARadius: Single);
begin
  sk4d_pathbuilder_arc_to3(Handle, sk_point_t(APoint1), sk_point_t(APoint2), ARadius);
end;

procedure TSKPathBuilder.ArcTo(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AForceMoveTo: Boolean);
begin
  sk4d_pathbuilder_arc_to2(Handle, sk_rect_t(AOval), AStartAngle, ASweepAngle, AForceMoveTo);
end;

procedure TSKPathBuilder.ArcTo(const ARadius: TPointF;
  const XAxisRotate: Single; const ALargeArc: TSKPathArcSize;
  const ASweep: TSKPathDirection; const AXY: TPointF);
begin
  sk4d_pathbuilder_arc_to(Handle, sk_point_t(ARadius), XAxisRotate, sk_patharcsize_t(ALargeArc), sk_pathdirection_t(ASweep), sk_point_t(AXY));
end;

procedure TSKPathBuilder.Close;
begin
  sk4d_pathbuilder_close(Handle);
end;

procedure TSKPathBuilder.ConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  sk4d_pathbuilder_conic_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2), AWeight);
end;

procedure TSKPathBuilder.ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  ConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

constructor TSKPathBuilder.Create;
begin
  CreateNative(sk4d_pathbuilder_create);
end;

constructor TSKPathBuilder.Create(const AFillType: TSKPathFillType);
begin
  Create;
  SetFillType(AFillType);
end;

constructor TSKPathBuilder.Create(const APathBuilder: ISKPathBuilder);
begin
  Assert(Assigned(APathBuilder));
  CreateNative(sk4d_pathbuilder_create2(GetHandle(APathBuilder)));
end;

procedure TSKPathBuilder.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  CubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

procedure TSKPathBuilder.CubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  sk4d_pathbuilder_cubic_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2), sk_point_t(APoint3));
end;

function TSKPathBuilder.Detach: ISKPath;
begin
  Result := TSKPath.CreateNative(sk4d_pathbuilder_detach(Handle));
end;

class procedure TSKPathBuilder.DoDestroy(const AHandle: THandle);
begin
  sk4d_pathbuilder_destroy(AHandle);
end;

function TSKPathBuilder.GetBounds: TRectF;
begin
  sk4d_pathbuilder_get_bounds(Handle, sk_rect_t(Result));
end;

function TSKPathBuilder.GetFillType: TSKPathFillType;
begin
  Result := TSKPathFillType(sk4d_pathbuilder_get_fill_type(Handle));
end;

procedure TSKPathBuilder.IncReserve(const AExtraPointCount,
  AExtraVerbCount: Integer);
begin
  sk4d_pathbuilder_inc_reserve(Handle, AExtraPointCount, AExtraVerbCount);
end;

procedure TSKPathBuilder.IncReserve(const AExtraPointCount: Integer);
begin
  IncReserve(AExtraPointCount, AExtraPointCount);
end;

procedure TSKPathBuilder.LineTo(const AX, AY: Single);
begin
  LineTo(TPointF.Create(AX, AY));
end;

procedure TSKPathBuilder.LineTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_line_to(Handle, sk_point_t(APoint));
end;

procedure TSKPathBuilder.MoveTo(const AX, AY: Single);
begin
  MoveTo(TPointF.Create(AX, AY));
end;

procedure TSKPathBuilder.MoveTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_move_to(Handle, sk_point_t(APoint));
end;

procedure TSKPathBuilder.Offset(const ADX, ADY: Single);
begin
  sk4d_pathbuilder_offset(Handle, ADX, ADY);
end;

procedure TSKPathBuilder.PolylineTo(const APoints: TArray<TPointF>);
begin
  if Length(APoints) > 0 then
    sk4d_pathbuilder_polyline_to(Handle, @sk_point_t(APoints[0]), Length(APoints));
end;

procedure TSKPathBuilder.QuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  QuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSKPathBuilder.QuadTo(const APoint1, APoint2: TPointF);
begin
  sk4d_pathbuilder_quad_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2));
end;

procedure TSKPathBuilder.RConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  sk4d_pathbuilder_r_conic_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2), AWeight);
end;

procedure TSKPathBuilder.RConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  RConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

procedure TSKPathBuilder.RCubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  RCubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

procedure TSKPathBuilder.RCubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  sk4d_pathbuilder_r_cubic_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2), sk_point_t(APoint3));
end;

procedure TSKPathBuilder.Reset;
begin
  sk4d_pathbuilder_reset(Handle);
end;

procedure TSKPathBuilder.RLineTo(const AX, AY: Single);
begin
  RLineTo(TPointF.Create(AX, AY));
end;

procedure TSKPathBuilder.RLineTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_r_line_to(Handle, sk_point_t(APoint));
end;

procedure TSKPathBuilder.RQuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  RQuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSKPathBuilder.RQuadTo(const APoint1, APoint2: TPointF);
begin
  sk4d_pathbuilder_r_quad_to(Handle, sk_point_t(APoint1), sk_point_t(APoint2));
end;

procedure TSKPathBuilder.SetFillType(const AValue: TSKPathFillType);
begin
  sk4d_pathbuilder_set_filltype(Handle, sk_pathfilltype_t(AValue));
end;

function TSKPathBuilder.Snapshot: ISKPath;
begin
  Result := TSKPath.CreateNative(sk4d_pathbuilder_snapshot(Handle));
end;

procedure TSKPathBuilder.ToggleInverseFillType;
begin
  sk4d_pathbuilder_toggle_inverse_filltype(Handle);
end;

{ TSKRegionCliperator }

constructor TSKRegionCliperator.Create(const ARegion: ISKRegion;
  const AClip: TRect);
begin
  Assert(Assigned(ARegion));
  CreateNative(sk4d_regioncliperator_create(GetHandle(ARegion), sk_irect_t(AClip)));
end;

class procedure TSKRegionCliperator.DoDestroy(const AHandle: THandle);
begin
  sk4d_regioncliperator_destroy(AHandle);
end;

function TSKRegionCliperator.GetRect: TRect;
begin
  sk4d_regioncliperator_get_rect(Handle, sk_irect_t(Result));
end;

function TSKRegionCliperator.Next: Boolean;
begin
  Result := sk4d_regioncliperator_next(Handle);
end;

{ TSKRegionIterator }

constructor TSKRegionIterator.Create(const ARegion: ISKRegion);
begin
  Assert(Assigned(ARegion));
  CreateNative(sk4d_regioniterator_create(GetHandle(ARegion)));
end;

class procedure TSKRegionIterator.DoDestroy(const AHandle: THandle);
begin
  sk4d_regioniterator_destroy(AHandle);
end;

function TSKRegionIterator.GetRect: TRect;
begin
  sk4d_regioniterator_get_rect(Handle, sk_irect_t(Result));
end;

function TSKRegionIterator.Next: Boolean;
begin
  Result := sk4d_regioniterator_next(Handle);
end;

function TSKRegionIterator.Rewind: Boolean;
begin
  Result := sk4d_regioniterator_rewind(Handle);
end;

{ TSKRegionSpanerator }

constructor TSKRegionSpanerator.Create(const ARegion: ISKRegion; const AY,
  ALeft, ARight: Integer);
begin
  Assert(Assigned(ARegion));
  CreateNative(sk4d_regionspanerator_create(GetHandle(ARegion), AY, ALeft, ARight));
end;

class procedure TSKRegionSpanerator.DoDestroy(const AHandle: THandle);
begin
  sk4d_regionspanerator_destroy(AHandle);
end;

function TSKRegionSpanerator.GetLeft: Integer;
begin
  Result := FLeft;
end;

function TSKRegionSpanerator.GetRight: Integer;
begin
  Result := FRight;
end;

function TSKRegionSpanerator.Next: Boolean;
begin
  Result := sk4d_regionspanerator_next(Handle, FLeft, FRight);
end;

{ TSKRegion }

function TSKRegion.Contains(const ARegion: ISKRegion): Boolean;
begin
  Assert(Assigned(ARegion));
  Result := sk4d_region_contains(Handle, GetHandle(ARegion));
end;

function TSKRegion.Contains(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_contains2(Handle, sk_irect_t(ARect));
end;

function TSKRegion.Contains(const AX, AY: Integer): Boolean;
begin
  Result := sk4d_region_contains3(Handle, AX, AY);
end;

constructor TSKRegion.Create(const ARect: TRect);
begin
  Create;
  SetRect(ARect);
end;

constructor TSKRegion.Create(const ARegion: ISKRegion);
begin
  Assert(Assigned(ARegion));
  CreateNative(sk4d_region_create2(GetHandle(ARegion)));
end;

constructor TSKRegion.Create;
begin
  CreateNative(sk4d_region_create);
end;

class procedure TSKRegion.DoDestroy(const AHandle: THandle);
begin
  sk4d_region_destroy(AHandle);
end;

function TSKRegion.GetBoundaryPath: ISKPath;
begin
  Result := TSKPath.Create;
  if not sk4d_region_get_boundary_path(Handle, GetHandle(Result)) then
    Result := nil;
end;

function TSKRegion.GetBounds: TRectF;
begin
  sk4d_region_get_bounds(Handle, sk_irect_t(Result));
end;

function TSKRegion.GetCliperator(const AClip: TRect): ISKRegionCliperator;
begin
  Result := TSKRegionCliperator.Create(Self, AClip);
end;

function TSKRegion.GetIterator: ISKRegionIterator;
begin
  Result := TSKRegionIterator.Create(Self);
end;

function TSKRegion.GetSpanerator(const AY, ALeft,
  ARight: Integer): ISKRegionSpanerator;
begin
  Result := TSKRegionSpanerator.Create(Self, AY, ALeft, ARight);
end;

function TSKRegion.GetTranslate(const AX, AY: Integer): ISKRegion;
begin
  Result := TSKRegion.Create;
  sk4d_region_translate2(Handle, AX, AY, GetHandle(Result));
end;

function TSKRegion.Intersects(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_intersects2(Handle, sk_irect_t(ARect));
end;

function TSKRegion.Intersects(const ARegion: ISKRegion): Boolean;
begin
  Assert(Assigned(ARegion));
  Result := sk4d_region_intersects(Handle, GetHandle(ARegion));
end;

function TSKRegion.IsComplex: Boolean;
begin
  Result := sk4d_region_is_complex(Handle);
end;

function TSKRegion.IsEmpty: Boolean;
begin
  Result := sk4d_region_is_empty(Handle);
end;

function TSKRegion.IsEqual(const ARegion: ISKRegion): Boolean;
begin
  Assert(Assigned(ARegion));
  Result := sk4d_region_is_equal(Handle, GetHandle(ARegion));
end;

function TSKRegion.IsRect: Boolean;
begin
  Result := sk4d_region_is_rect(Handle);
end;

function TSKRegion.Op(const ARegion: ISKRegion;
  const AOp: TSKRegionOp): Boolean;
begin
  Assert(Assigned(ARegion));
  Result := sk4d_region_op(Handle, GetHandle(ARegion), sk_regionop_t(AOp));
end;

function TSKRegion.Op(const ARect: TRect; const AOp: TSKRegionOp): Boolean;
begin
  Result := sk4d_region_op2(Handle, sk_irect_t(ARect), sk_regionop_t(AOp));
end;

function TSKRegion.QuickContains(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_quick_contains(Handle, sk_irect_t(ARect));
end;

function TSKRegion.QuickReject(const ARegion: ISKRegion): Boolean;
begin
  Assert(Assigned(ARegion));
  Result := sk4d_region_quick_reject(Handle, GetHandle(ARegion));
end;

function TSKRegion.QuickReject(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_quick_reject2(Handle, sk_irect_t(ARect));
end;

procedure TSKRegion.SetEmpty;
begin
  sk4d_region_set_empty(Handle);
end;

function TSKRegion.SetPath(const APath: ISKPath;
  const AClip: ISKRegion): Boolean;
begin
  Assert((Assigned(APath)) and (Assigned(AClip)));
  Result := sk4d_region_set_path(Handle, GetHandle(APath), GetHandle(AClip));
end;

function TSKRegion.SetRect(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_set_rect(Handle, sk_irect_t(ARect));
end;

function TSKRegion.SetRects(const ARects: TArray<TRect>): Boolean;
begin
  Result := sk4d_region_set_rects(Handle, @sk_irect_t(ARects[0]), Length(ARects));
end;

procedure TSKRegion.Translate(const AX, AY: Integer);
begin
  sk4d_region_translate(Handle, AX, AY);
end;

{ TSKTraceMemoryDumpBaseClass }

constructor TSKTraceMemoryDumpBaseClass.Create(const ADetailedDump,
  ADumpWrappedObjects: Boolean);
begin
  CreateNative(sk4d_tracememorydumpbaseclass_create(ADetailedDump, ADumpWrappedObjects, Self));
end;

class constructor TSKTraceMemoryDumpBaseClass.Create;
var
  LProcs: sk_tracememorydumpbaseclass_procs_t;
begin
  LProcs.dump_numeric_value := dump_numeric_value_proc;
  LProcs.dump_string_value  := dump_string_value_proc;
  sk4d_tracememorydumpbaseclass_set_procs(LProcs);
end;

class procedure TSKTraceMemoryDumpBaseClass.DoDestroy(const AHandle: THandle);
begin
  sk4d_tracememorydumpbaseclass_destroy(AHandle);
end;

class procedure TSKTraceMemoryDumpBaseClass.dump_numeric_value_proc(
  context: Pointer; const dump_name, value_name, units: MarshaledAString;
  value: uint64_t);
begin
  TSKTraceMemoryDumpBaseClass(context).DoDumpNumericValue(string(dump_name), string(value_name), string(units), value);
end;

class procedure TSKTraceMemoryDumpBaseClass.dump_string_value_proc(
  context: Pointer; const dump_name, value_name, value: MarshaledAString);
begin
  TSKTraceMemoryDumpBaseClass(context).DoDumpStringValue(string(dump_name), string(value_name), string(value));
end;

{ TGRGLFramebufferInfo }

constructor TGRGLFramebufferInfo.Create(const AFBOID: GRGLuint;
  const AFormat: GRGLenum);
begin
  FBOID  := AFBOID;
  Format := AFormat;
end;

{ TGRGLTextureInfo }

constructor TGRGLTextureInfo.Create(const ATarget: GRGLenum;
  const AID: GRGLuint; const AFormat: GRGLenum);
begin
  Target := ATarget;
  ID     := AID;
  Format := AFormat;
end;

{ TGRGLInterface }

class function TGRGLInterface.get_proc(context: Pointer;
  const name: MarshaledAString): Pointer;
begin
  if context = nil then
    Exit(nil);
  Result := TGRGLGetProc(context)(string(name));
end;

function TGRGLInterface.HasExtension(const AName: string): Boolean;
begin
  Result := gr4d_gl_interface_has_extension(Handle, MarshaledAString(UTF8String(AName)));
end;

class function TGRGLInterface.MakeAssembled(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  LHandle := gr4d_gl_interface_make_assembled(PPointer(@AProc)^, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledGL(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  LHandle := gr4d_gl_interface_make_assembled_gl(PPointer(@AProc)^, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledGLES(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  LHandle :=  gr4d_gl_interface_make_assembled_gles(PPointer(@AProc)^, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledWebGL(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  LHandle :=  gr4d_gl_interface_make_assembled_webgl(PPointer(@AProc)^, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

function TGRGLInterface.Validate: Boolean;
begin
  Result := gr4d_gl_interface_validate(Handle);
end;

{ TGRMTLBackendContext }

constructor TGRMTLBackendContext.Create(const ADevice, AQueue: TGRMTLHandle);
begin
  Device := ADevice;
  Queue  := AQueue;
end;

{ TGRMTLTextureInfo }

constructor TGRMTLTextureInfo.Create(const ATexture: TGRMTLHandle);
begin
  Texture := ATexture;
end;

{ TGRDirectContext }

procedure TGRDirectContext.AbandonContext;
begin
  gr4d_directcontext_abandon_context(Handle);
end;

procedure TGRDirectContext.DumpMemoryStatistics(
  const ATraceMemoryDump: ISKTraceMemoryDump);
begin
  gr4d_directcontext_dump_memory_statistics(Handle, GetHandle(ATraceMemoryDump));
end;

procedure TGRDirectContext.Flush;
begin
  gr4d_directcontext_flush(Handle);
end;

procedure TGRDirectContext.FreeGPUResources;
begin
  gr4d_directcontext_free_gpu_resources(Handle);
end;

function TGRDirectContext.GetMaxSurfaceSampleCountForColorType(
  const AColorType: TSKColorType): Integer;
begin
  Result := gr4d_directcontext_get_max_surface_sample_count_for_color_type(Handle, sk_colortype_t(AColorType));
end;

function TGRDirectContext.GetResourceCacheLimit: NativeUInt;
begin
  Result := gr4d_directcontext_get_resource_cache_limit(Handle);
end;

procedure TGRDirectContext.GetResourceCacheUsage(out AMaxResources: Integer;
  out AMaxResourcesBytes: NativeUInt);
begin
  gr4d_directcontext_get_resource_cache_usage(Handle, AMaxResources, AMaxResourcesBytes);
end;

class function TGRDirectContext.MakeGL(
  const AInterface: IGRGLInterface): IGRDirectContext;
var
  LHandle: gr_directcontext_t;
begin
  LHandle := gr4d_directcontext_make_gl(GetHandle(AInterface));
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;

class function TGRDirectContext.MakeMetal(
  const ABackendContext: TGRMTLBackendContext): IGRDirectContext;
var
  LBackendContext: gr_mtl_backendcontext_t;
  LHandle: gr_directcontext_t;
begin
  LBackendContext := gr_mtl_backendcontext_t(ABackendContext);
  LHandle         := gr4d_directcontext_make_metal(LBackendContext);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;

procedure TGRDirectContext.PerformDeferredCleanup(const AMilliseconds: Int64);
begin
  gr4d_directcontext_perform_deferred_cleanup(Handle, AMilliseconds);
end;

procedure TGRDirectContext.PurgeUnlockedResources(
  const AScratchResourcesOnly: Boolean);
begin
  gr4d_directcontext_purge_unlocked_resources(Handle, AScratchResourcesOnly);
end;

procedure TGRDirectContext.PurgeUnlockedResources(
  const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean);
begin
  gr4d_directcontext_purge_unlocked_resources2(Handle, ABytesToPurge, APreferScratchResources);
end;

procedure TGRDirectContext.ReleaseResourcesAndAbandonContext;
begin
  gr4d_directcontext_release_resources_and_abandon_context(Handle);
end;

procedure TGRDirectContext.ResetContext(const AState: Cardinal);
begin
  gr4d_directcontext_reset_context(Handle, AState);
end;

procedure TGRDirectContext.ResetContext;
begin
  ResetContext($FFFFFFFF);
end;

procedure TGRDirectContext.SetResourceCacheLimit(const AValue: NativeUInt);
begin
  gr4d_directcontext_set_resource_cache_limit(Handle, AValue);
end;

{ TGRBackendRenderTarget }

constructor TGRBackendRenderTarget.CreateGL(const AWidth, AHeight, ASampleCount,
  AStencilBits: Integer; const AFramebufferInfo: TGRGLFramebufferInfo);
begin
  CreateNative(gr4d_backendrendertarget_create_gl(AWidth, AHeight, ASampleCount, AStencilBits, gr_gl_framebufferinfo_t(AFramebufferInfo)))
end;

constructor TGRBackendRenderTarget.CreateMetal(const AWidth, AHeight: Integer;
  const ATextureInfo: TGRMTLTextureInfo);
begin
  CreateNative(gr4d_backendrendertarget_create_mtl(AWidth, AHeight, gr_mtl_textureinfo_t(ATextureInfo)));
end;

class procedure TGRBackendRenderTarget.DoDestroy(const AHandle: THandle);
begin
  gr4d_backendrendertarget_destroy(AHandle);
end;

function TGRBackendRenderTarget.GetBackendAPI: TGRBackendAPI;
begin
  Result := TGRBackendAPI(gr4d_backendrendertarget_get_backend_api(Handle));
end;

function TGRBackendRenderTarget.GetGLFramebufferInfo(
  out AFramebufferInfo: TGRGLFramebufferInfo): Boolean;
begin
  Result := gr4d_backendrendertarget_get_gl_framebuffer_info(Handle, gr_gl_framebufferinfo_t(AFramebufferInfo));
end;

function TGRBackendRenderTarget.GetHeight: Integer;
begin
  Result := gr4d_backendrendertarget_get_height(Handle);
end;

function TGRBackendRenderTarget.GetSampleCount: Integer;
begin
  Result := gr4d_backendrendertarget_get_sample_count(Handle);
end;

function TGRBackendRenderTarget.GetStencilBits: Integer;
begin
  Result := gr4d_backendrendertarget_get_stencil_bits(Handle);
end;

function TGRBackendRenderTarget.GetWidth: Integer;
begin
  Result := gr4d_backendrendertarget_get_width(Handle);
end;

function TGRBackendRenderTarget.IsValid: Boolean;
begin
  Result := gr4d_backendrendertarget_is_valid(Handle);
end;

{ TGRBackendTexture }

constructor TGRBackendTexture.CreateGL(const AWidth, AHeight: Integer;
  const AMipmapped: Boolean; const ATextureInfo: TGRGLTextureInfo);
begin
  CreateNative(gr4d_backendtexture_create_gl(AWidth, AHeight, AMipmapped, gr_gl_textureinfo_t(ATextureInfo)));
end;

constructor TGRBackendTexture.CreateMetal(const AWidth, AHeight: Integer;
  const AMipmapped: Boolean; const ATextureInfo: TGRMTLTextureInfo);
begin
  CreateNative(gr4d_backendtexture_create_mtl(AWidth, AHeight, AMipmapped, gr_mtl_textureinfo_t(ATextureInfo)));
end;

class procedure TGRBackendTexture.DoDestroy(const AHandle: THandle);
begin
  gr4d_backendtexture_destroy(AHandle);
end;

function TGRBackendTexture.GetBackendAPI: TGRBackendAPI;
begin
  Result := TGRBackendAPI(gr4d_backendtexture_get_backend_api(Handle));
end;

function TGRBackendTexture.GetGLTextureInfo(
  out ATextureInfo: TGRGLTextureInfo): Boolean;
begin
  Result := gr4d_backendtexture_get_gl_texture_info(Handle, gr_gl_textureinfo_t(ATextureInfo));
end;

function TGRBackendTexture.GetHeight: Integer;
begin
  Result := gr4d_backendtexture_get_height(Handle);
end;

function TGRBackendTexture.GetWidth: Integer;
begin
  Result := gr4d_backendtexture_get_width(Handle);
end;

function TGRBackendTexture.HasMipmaps: Boolean;
begin
  Result := gr4d_backendtexture_has_mipmaps(Handle);
end;

function TGRBackendTexture.IsValid: Boolean;
begin
  Result := gr4d_backendtexture_is_valid(Handle);
end;

{ TSKHighContrastConfig }

constructor TSKHighContrastConfig.Create(const AGrayscale: Boolean;
  const AInvertStyle: TSKHighContrastConfigInvertStyle;
  const AContrast: Single);
begin
  Grayscale   := AGrayscale;
  InvertStyle := AInvertStyle;
  Contrast    := AContrast;
end;

class operator TSKHighContrastConfig.Explicit(
  const AHighContrastConfig: TSKHighContrastConfig): sk_highcontrastconfig_t;
begin
  Result.grayscale    := AHighContrastConfig.Grayscale;
  Result.invert_style := sk_highcontrastconfiginvertstyle_t(AHighContrastConfig.InvertStyle);
  Result.contrast     := AHighContrastConfig.Contrast;
end;

{ TSKColorFilter }

class function TSKColorFilter.MakeBlend(const AColor: TAlphaColor;
  const AMode: TSkBlendMode): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_blend(AColor, sk_blendmode_t(AMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeCompose(const AOuter,
  AInner: ISKColorFilter): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_compose(GetHandle(AOuter), GetHandle(AInner));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeHighContrast(
  const AConfig: TSKHighContrastConfig): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_high_contrast(sk_highcontrastconfig_t(AConfig));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeHSLAMatrix(
  const AMatrix: TSKColorMatrix): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_hsla_matrix(@AMatrix);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeLerp(const AWeight: Single; const ADest,
  ASrc: ISKColorFilter): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_lerp(AWeight, GetHandle(ADest), GetHandle(ASrc));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeLighting(const AMultiply,
  AAdd: TAlphaColor): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_lighting(AMultiply, AAdd);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeLinearToSRGBGamma: ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_linear_to_srgb_gamma;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeLumaColor: ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_luma_color;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeMatrix(
  const AMatrix: TSKColorMatrix): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_matrix(@AMatrix);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeOverdraw(
  const AColors: TSKOverdrawColor): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_overdraw(@AColors);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeSRGBToLinearGamma: ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_srgb_to_linear_gamma;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

class function TSKColorFilter.MakeTable(
  const ATable: TSKTableFilter): ISKColorFilter;
begin
  Result := MakeTable(ATable, ATable, ATable, ATable);
end;

class function TSKColorFilter.MakeTable(const ATableA, ATableR, ATableG,
  ATableB: TSKTableFilter): ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_colorfilter_make_table(@ATableA, @ATableR, @ATableG, @ATableB);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

{ TSKShader }

class function TSKShader.MakeBlend(const AMode: TSkBlendMode; const ADest,
  ASrc: ISKShader): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_blend(sk_blendmode_t(AMode), GetHandle(ADest), GetHandle(ASrc));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeColor(const AColor: TAlphaColor): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_color(AColor);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeColor(const AColor: TAlphaColorF;
  const AColorSpace: ISKColorSpace): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_color2(sk_color4f_t(AColor), GetHandle(AColorSpace));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeEmpty: ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_empty;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const ATileMode: TSKTileMode;
  const APositions: TArray<Single>;
  const AColorSpace: ISKColorSpace): ISKShader;
var
  LHandle: sk_shader_t;
  LPoints: array[0..1] of TPointF;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LPoints[0] := AStart;
  LPoints[1] := AEnd;
  LHandle := sk4d_shader_make_gradient_linear2(@sk_point_t(LPoints[0]), @sk_color4f_t(AColors[0]), GetHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColor>; const ATileMode: TSKTileMode;
  const APositions: TArray<Single>): ISKShader;
var
  LHandle: sk_shader_t;
  LPoints: array[0..1] of TPointF;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LPoints[0] := AStart;
  LPoints[1] := AEnd;
  LHandle := sk4d_shader_make_gradient_linear(@sk_point_t(LPoints[0]), @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColor; const ATileMode: TSKTileMode): ISKShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], ATileMode);
end;

class function TSKShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ATileMode: TSKTileMode;
  const AColorSpace: ISKColorSpace): ISKShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], ATileMode, [], AColorSpace);
end;

class function TSKShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor;
  const ATileMode: TSKTileMode): ISKShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], ATileMode);
end;

class function TSKShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const ATileMode: TSKTileMode; const APositions: TArray<Single>;
  const AColorSpace: ISKColorSpace): ISKShader;
var
  LHandle: sk_shader_t;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LHandle := sk4d_shader_make_gradient_radial2(sk_point_t(ACenter), ARadius, @sk_color4f_t(AColors[0]), GetHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColor>;
  const ATileMode: TSKTileMode; const APositions: TArray<Single>): ISKShader;
var
  LHandle: sk_shader_t;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LHandle := sk4d_shader_make_gradient_radial(sk_point_t(ACenter), ARadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF;
  const ATileMode: TSKTileMode; const AColorSpace: ISKColorSpace): ISKShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], ATileMode, [], AColorSpace);
end;

class function TSKShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColorF;
  const AColorSpace: ISKColorSpace): ISKShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2], [], AColorSpace);
end;

class function TSKShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColor): ISKShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2]);
end;

class function TSKShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColor>;
  const APositions: TArray<Single>): ISKShader;
var
  LHandle: sk_shader_t;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LHandle := sk4d_shader_make_gradient_sweep(sk_point_t(ACenter), @AColors[0], @APositions[0], Length(AColors));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const AColorSpace: ISKColorSpace): ISKShader;
var
  LHandle: sk_shader_t;
begin
  Assert((not Assigned(APositions)) or (Length(AColors) = Length(APositions)));
  LHandle := sk4d_shader_make_gradient_sweep2(sk_point_t(ACenter), @sk_color4f_t(AColors[0]), GetHandle(AColorSpace), @APositions[0], Length(AColors));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

class function TSKShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

function TSKShader.MakeWithColorFilter(
  const AFilter: ISKColorFilter): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_with_color_filter(Handle, GetHandle(AFilter));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

function TSKShader.MakeWithLocalMatrix(const AMatrix: TMatrix): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_shader_make_with_local_matrix(Handle, sk_matrix_t(AMatrix));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

{ TSKMaskFilter }

class function TSKMaskFilter.MakeBlur(const AStyle: TSKBlurStyle;
  const ASigma: Single; const ARespectCTM: Boolean): ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_maskfilter_make_blur(sk_blurstyle_t(AStyle), ASigma, ARespectCTM);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

class function TSKMaskFilter.MakeShader(
  const AShader: ISKShader): ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_maskfilter_make_shader(GetHandle(AShader));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

class function TSKMaskFilter.MakeTable(
  const ATable: TSKTableFilter): ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_maskfilter_make_table(@ATable);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

class function TSKMaskFilter.MakeTableClip(const AMin,
  AMax: Byte): ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_maskfilter_make_table_clip(AMin, AMax);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

class function TSKMaskFilter.MakeTableGamma(
  const AGamma: Single): ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_maskfilter_make_table_gamma(AGamma);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

{ TSKPicture }

function TSKPicture.GetCullRect: TRectF;
begin
  sk4d_picture_get_cull_rect(Handle, sk_rect_t(Result));
end;

function TSKPicture.GetUniqueID: Cardinal;
begin
  Result := sk4d_picture_get_unique_id(Handle);
end;

class function TSKPicture.MakeFromData(const AData: ISKData): ISKPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := sk4d_picture_make_from_data(GetHandle(AData));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPicture.CreateNative(LHandle);
end;

class function TSKPicture.MakeFromFile(const AFileName: string): ISKPicture;
var
  LFileStream: ISKFileStream;
begin
  LFileStream := TSKFileStream.Create(AFileName);
  if not LFileStream.IsValid then
    Exit(nil);
  Result := MakeFromStream(LFileStream);
end;

class function TSKPicture.MakeFromStream(const AStream: ISKStream): ISKPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := sk4d_picture_make_from_stream(GetHandle(AStream));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPicture.CreateNative(LHandle);
end;

function TSKPicture.MakeShader(const ATileModeX, ATileModeY: TSKTileMode;
  const AFilterMode: TSKFilterMode): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_picture_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

function TSKPicture.SaveToData: ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_picture_save_to_data(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

procedure TSKPicture.SaveToFile(const AFileName: string);
var
  LFileWStream: ISKFileWStream;
begin
  LFileWStream := TSKFileWStream.Create(AFileName);
  if LFileWStream.IsValid then
    SaveToStream(LFileWStream);
end;

procedure TSKPicture.SaveToStream(const AWStream: ISKWStream);
begin
  sk4d_picture_save_to_stream(Handle, GetHandle(AWStream));
end;

{ TSKPictureRecorder }

function TSKPictureRecorder.BeginRecording(const ABounds: TRectF): ISKCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := sk4d_picturerecorder_begin_recording(Handle, sk_rect_t(ABounds));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCanvas.CreateNative(LHandle, False);
end;

function TSKPictureRecorder.BeginRecording(const AWidth,
  AHeight: Single): ISKCanvas;
begin
  Result := BeginRecording(TRectF.Create(0, 0, AWidth, AHeight));
end;

constructor TSKPictureRecorder.Create;
begin
  CreateNative(sk4d_picturerecorder_create);
end;

class procedure TSKPictureRecorder.DoDestroy(const AHandle: THandle);
begin
  sk4d_picturerecorder_destroy(AHandle);
end;

function TSKPictureRecorder.FinishRecording(
  const ACullRect: TRectF): ISKPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := sk4d_picturerecorder_finish_recording2(Handle, sk_rect_t(ACullRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPicture.CreateNative(LHandle);
end;

function TSKPictureRecorder.FinishRecording: ISKPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := sk4d_picturerecorder_finish_recording(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPicture.CreateNative(LHandle);
end;

{ TSKImage }

function TSKImage.EncodeToData: ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_image_encode_to_data(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

function TSKImage.EncodeToData(const AEncodedImageFormat: TSKEncodedImageFormat;
  const AQuality: Integer): ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_image_encode_to_data2(Handle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

function TSKImage.EncodeToFile(const AFileName: string): Boolean;
var
  LFileWStream: ISKFileWStream;
begin
  LFileWStream := TSKFileWStream.Create(AFileName);
  Result       := LFileWStream.IsValid;
  if Result then
    Result := EncodeToStream(LFileWStream);
end;

function TSKImage.EncodeToFile(const AFileName: string;
  const AEncodedImageFormat: TSKEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LFileWStream: ISKFileWStream;
begin
  LFileWStream := TSKFileWStream.Create(AFileName);
  Result       := LFileWStream.IsValid;
  if Result then
    Result := EncodeToStream(LFileWStream, AEncodedImageFormat, AQuality);
end;

function TSKImage.EncodeToStream(const AWStream: ISKWStream): Boolean;
var
  LData: ISKData;
begin
  LData  := EncodeToData;
  Result := Assigned(LData);
  if Result then
    AWStream.Write(LData.Data^, LData.Size);
end;

function TSKImage.EncodeToStream(const AWStream: ISKWStream;
  const AEncodedImageFormat: TSKEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LData: ISKData;
begin
  LData  := EncodeToData(AEncodedImageFormat, AQuality);
  Result := Assigned(LData);
  if Result then
    AWStream.Write(LData.Data^, LData.Size);
end;

function TSKImage.GetAlphaType: TSKAlphaType;
begin
  Result := TSKAlphaType(sk4d_image_get_alpha_type(Handle));
end;

function TSKImage.GetColorSpace: ISKColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := sk4d_image_get_color_space(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorSpace.CreateNative(LHandle);
end;

function TSKImage.GetColorType: TSKColorType;
begin
  Result := TSKColorType(sk4d_image_get_color_type(Handle));
end;

function TSKImage.GetEncodedData: ISKData;
var
  LHandle: sk_data_t;
begin
  LHandle := sk4d_image_get_encoded_data(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKData.CreateNative(LHandle);
end;

function TSKImage.GetHeight: Integer;
begin
  Result := sk4d_image_get_height(Handle);
end;

function TSKImage.GetImageInfo: TSKImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_image_get_image_info(Handle, LResult);
  Result := TSKImageInfo(LResult);
end;

function TSKImage.GetUniqueID: Cardinal;
begin
  Result := sk4d_image_get_unique_id(Handle);
end;

function TSKImage.GetWidth: Integer;
begin
  Result := sk4d_image_get_width(Handle);
end;

function TSKImage.IsAlphaOnly: Boolean;
begin
  case GetColorType of
    TSKColorType.Alpha8,
    TSKColorType.AlphaF16,
    TSKColorType.Alpha16: Result := True;
  else
    Result := False;
  end;
end;

function TSKImage.IsLazyGenerated: Boolean;
begin
  Result := sk4d_image_is_lazy_generated(Handle);
end;

function TSKImage.IsOpaque: Boolean;
begin
  Result := GetAlphaType = TSKAlphaType.Opaque;
end;

function TSKImage.IsTextureBacked: Boolean;
begin
  Result := sk4d_image_is_texture_backed(Handle);
end;

function TSKImage.IsValid(const AContext: IGRDirectContext): Boolean;
begin
  Result := sk4d_image_is_valid(Handle, GetHandle(AContext));
end;

function TSKImage.MakeColorSpace(const AColorSpace: ISKColorSpace;
  const AContext: IGRDirectContext): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_colorspace(Handle, GetHandle(AColorSpace), GetHandle(AContext));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromAdoptedTexture(const AContext: IGRDirectContext;
  const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin;
  AColorType: TSKColorType; const AAlphaType: TSKAlphaType;
  const AColorSpace: ISKColorSpace): ISKImage;
var
  LHandle: sk_image_t;
begin
  Assert(Assigned(ATexture));
  LHandle := sk4d_image_make_from_adopted_texture(GetHandle(AContext), GetHandle(ATexture), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), GetHandle(AColorSpace));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromEncodedData(const AEncoded: ISKData): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_from_encoded_data(GetHandle(AEncoded));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromEncodedFile(const AFileName: string): ISKImage;
var
  LFileStream: ISKFileStream;
begin
  LFileStream := TSKFileStream.Create(AFileName);
  Result      := MakeFromEncodedStream(LFileStream);
end;

class function TSKImage.MakeFromEncodedStream(
  const AStream: ISKStream): ISKImage;
var
  LData: ISKData;
begin
  LData  := TSKData.MakeFromStream(AStream, AStream.Length - AStream.Position);
  Result := MakeFromEncodedData(LData);
end;

class function TSKImage.MakeFromPicture(const APicture: ISKPicture;
  const ADimensions: TSize; const AMatrix: TMatrix;
  const APaint: ISKPaint): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_from_picture2(GetHandle(APicture), sk_isize_t(ADimensions), @sk_matrix_t(AMatrix), GetHandle(APaint));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromPicture(const APicture: ISKPicture;
  const ADimensions: TSize): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_from_picture(GetHandle(APicture), sk_isize_t(ADimensions));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromRaster(const AImageInfo: TSKImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ARasterReleaseProc: TSKImageRasterReleaseProc): ISKImage;
var
  LPixmap: ISKPixmap;
begin
  LPixmap := TSKPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeFromRaster(LPixmap, ARasterReleaseProc);
end;

class function TSKImage.MakeFromRaster(const APixmap: ISKPixmap;
  const ARasterReleaseProc: TSKImageRasterReleaseProc): ISKImage;
var
  LContext: Pointer;
  LHandle: sk_image_t;
begin
  Assert(Assigned(APixmap));
  LContext := PPointer(@ARasterReleaseProc)^;
  LHandle  := sk4d_image_make_from_raster(GetHandle(APixmap), raster_release_proc, LContext);
  if LHandle = 0 then
    Exit(nil);
  if LContext <> nil then
    IInterface(LContext)._AddRef;
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeFromTexture(const AContext: IGRDirectContext;
  const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin;
  AColorType: TSKColorType; const AAlphaType: TSKAlphaType;
  const AColorSpace: ISKColorSpace;
  const ATextureReleaseProc: TSKImageTextureReleaseProc): ISKImage;
var
  LContext: Pointer;
  LHandle: sk_image_t;
begin
  Assert(Assigned(ATexture));
  LContext := PPointer(@ATextureReleaseProc)^;
  LHandle  := sk4d_image_make_from_texture(GetHandle(AContext), GetHandle(ATexture), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), GetHandle(AColorSpace), texture_release_proc, LContext);
  if LHandle = 0 then
    Exit(nil);
  if LContext <> nil then
    IInterface(LContext)._AddRef;
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKImage.MakeNonTextureImage: ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_non_texture_image(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKImage.MakeRasterCopy(const APixmap: ISKPixmap): ISKImage;
var
  LHandle: sk_image_t;
begin
  Assert(Assigned(APixmap));
  LHandle := sk4d_image_make_raster_copy(GetHandle(APixmap));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKImage.MakeRasterImage: ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_raster_image(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKImage.MakeShader(const ASampling: TSKSamplingOptions;
  const ATileModeX: TSKTileMode; ATileModeY: TSKTileMode): ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_image_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_samplingoptions_t(ASampling));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle);
end;

function TSKImage.MakeSubset(const ASubset: TRect;
  const AContext: IGRDirectContext): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_subset(Handle, sk_irect_t(ASubset), GetHandle(AContext));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKImage.MakeTextureImage(const AContext: IGRDirectContext;
  const AMipmapped: Boolean): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_texture_image(Handle, GetHandle(AContext), AMipmapped);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKImage.MakeWithFilter(const AContext: IGRDirectContext;
  const AFilter: ISKImageFilter; const ASubset, AClipBounds: TRect;
  out AOutSubset: TRect; out AOffset: TPoint): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_image_make_with_filter(Handle, GetHandle(AContext), GetHandle(AFilter), sk_irect_t(ASubset), sk_irect_t(AClipBounds), sk_irect_t(AOutSubset), sk_ipoint_t(AOffset));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class procedure TSKImage.raster_release_proc(const pixels: Pointer;
  context: Pointer);
begin
  if context <> nil then
  begin
    TSKImageRasterReleaseProc(context)(pixels);
    IInterface(context)._Release;
  end;
end;

function TSKImage.ReadPixels(const AContext: IGRDirectContext;
  const ADestImageInfo: TSKImageInfo; const ADestPixels: Pointer;
  const ADestRowBytes: NativeUInt; const ASrcX, ASrcY: Integer;
  const ACachingHint: TSKImageCachingHint): Boolean;
var
  LPixmap: ISKPixmap;
begin
  LPixmap := TSKPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(AContext, LPixmap, ASrcX, ASrcY);
end;

function TSKImage.ReadPixels(const AContext: IGRDirectContext;
  const ADest: ISKPixmap; const ASrcX, ASrcY: Integer;
  const ACachingHint: TSKImageCachingHint): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_image_read_pixels(Handle, GetHandle(AContext), GetHandle(ADest), ASrcX, ASrcY, sk_imagecachinghint_t(ACachingHint));
end;

function TSKImage.ScalePixels(const ADest: ISKPixmap;
  const ASampling: TSKSamplingOptions;
  const ACachingHint: TSKImageCachingHint): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_image_scale_pixels(Handle, GetHandle(ADest), sk_samplingoptions_t(ASampling), sk_imagecachinghint_t(ACachingHint));
end;

class procedure TSKImage.texture_release_proc(context: Pointer);
begin
  if context <> nil then
  begin
    TSKImageTextureReleaseProc(context)();
    IInterface(context)._Release;
  end;
end;

{ TSKImageFilter }

class function TSKImageFilter.MakeAlphaThreshold(const ARegion: ISKRegion;
  const AInnerMin, AOuterMax: Single; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  Assert(Assigned(ARegion));
  LHandle := sk4d_imagefilter_make_alpha_threshold(GetHandle(ARegion),  AInnerMin, AOuterMax, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeAlphaThreshold(const ARegion: ISKRegion;
  const AInnerMin, AOuterMax: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  Assert(Assigned(ARegion));
  LHandle := sk4d_imagefilter_make_alpha_threshold(GetHandle(ARegion),  AInnerMin, AOuterMax, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISKImageFilter;
  const ACropRect: TRectF; AForeground: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, GetHandle(ABackground), GetHandle(AForeground), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISKImageFilter;
  AForeground: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, GetHandle(ABackground), GetHandle(AForeground), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeBlend(const AMode: TSKBlendMode;
  const ABackground, AForeground: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), GetHandle(ABackground), GetHandle(AForeground), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeBlend(const AMode: TSKBlendMode;
  const ABackground: ISKImageFilter; const ACropRect: TRectF;
  const AForeground: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), GetHandle(ABackground), GetHandle(AForeground), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const AInput: ISKImageFilter; const ATileMode: TSKTileMode): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter;
  const ATileMode: TSKTileMode): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeColorFilter(
  const AColorFilter: ISKColorFilter;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_colorfilter(GetHandle(AColorFilter), GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeColorFilter(
  const AColorFilter: ISKColorFilter; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_colorfilter(GetHandle(AColorFilter), GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeCompose(const AOuter,
  AInner: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_compose(GetHandle(AOuter), GetHandle(AInner));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSKColorChannel; const AScale: Single;
  const AColor: ISKImageFilter; const ACropRect: TRectF;
  const ADisplacement: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, GetHandle(ADisplacement), GetHandle(AColor), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSKColorChannel; const AScale: Single; const AColor,
  ADisplacement: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, GetHandle(ADisplacement), GetHandle(AColor), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDistantLightDiffuse(
  const ADirection: TPoint3D; const ALightColor: TAlphaColor;
  const ASurfaceScale, AKd: Single; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_distant_light_diffuse(sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDistantLightDiffuse(
  const ADirection: TPoint3D; const ALightColor: TAlphaColor;
  const ASurfaceScale, AKd: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_distant_light_diffuse(sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDistantLightSpecular(
  const ADirection: TPoint3D; const ALightColor: TAlphaColor;
  const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_distant_light_specular(sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDistantLightSpecular(
  const ADirection: TPoint3D; const ALightColor: TAlphaColor;
  const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_distant_light_specular(sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDropShadow(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_drop_shadow(ADX, ADY, ASigmaX, ASigmaY, AColor, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDropShadow(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_drop_shadow(ADX, ADY, ASigmaX, ASigmaY, AColor, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDropShadowOnly(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_drop_shadow_only( ADX, ADY, ASigmaX, ASigmaY, AColor, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeDropShadowOnly(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_drop_shadow_only(ADX, ADY, ASigmaX, ASigmaY, AColor, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeImage(const AImage: ISKImage; const ASrc,
  ADest: TRectF; const ASampling: TSKSamplingOptions): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_image(GetHandle(AImage), sk_rect_t(ASrc), sk_rect_t(ADest), sk_samplingoptions_t(ASampling));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeImage(const AImage: ISKImage): ISKImageFilter;
begin
  Result := MakeImage(AImage, TSKSamplingOptions.Create(TSKCubicResampler.Mitchell));
end;

class function TSKImageFilter.MakeImage(const AImage: ISKImage;
  const ASampling: TSKSamplingOptions): ISKImageFilter;
var
  LRect: TRectF;
begin
  if Assigned(AImage) then
    LRect := TRectF.Create(0, 0, AImage.Width, AImage.Height)
  else
    LRect := TRectF.Empty;
  Result := MakeImage(AImage, LRect, LRect, ASampling);
end;

class function TSKImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_magnifier(sk_rect_t(ASrc), AInset, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_magnifier(sk_rect_t(ASrc), AInset, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSKTileMode;
  const AConvolveAlpha: Boolean; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  Assert(Length(AKernel) = (AKernelSize.Width * AKernelSize.Height));
  LHandle := sk4d_imagefilter_make_matrix_convolution(sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSKTileMode;
  const AConvolveAlpha: Boolean; const ACropRect: TRectF;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  Assert(Length(AKernel) = (AKernelSize.Width * AKernelSize.Height));
  LHandle := sk4d_imagefilter_make_matrix_convolution(sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMatrixTransform(const AMatrix: TMatrix;
  const ASampling: TSKSamplingOptions;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_matrix_transform(sk_matrix_t(AMatrix), sk_samplingoptions_t(ASampling), GetHandle(AInput));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMerge(
  const AFilters: TArray<ISKImageFilter>): ISKImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
  LHandle: sk_imagefilter_t;
begin
  SetLength(LFilters, Length(AFilters));
  for I := Low(AFilters) to High(AFilters) do
    LFilters[I] := GetHandle(AFilters[I]);
  LHandle := sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISKImageFilter): ISKImageFilter;
begin
  Result := MakeMerge([AFilter1, AFilter2]);
end;

class function TSKImageFilter.MakeMerge(const AFilters: TArray<ISKImageFilter>;
  const ACropRect: TRectF): ISKImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
  LHandle: sk_imagefilter_t;
begin
  SetLength(LFilters, Length(AFilters));
  for I := Low(AFilters) to High(AFilters) do
    LFilters[I] := GetHandle(AFilters[I]);
  LHandle := sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISKImageFilter; const ACropRect: TRectF): ISKImageFilter;
begin
  Result := MakeMerge([AFilter1, AFilter2], ACropRect);
end;

class function TSKImageFilter.MakeOffset(const ADX, ADY: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_offset(ADX, ADY, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeOffset(const ADX, ADY: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_offset(ADX, ADY, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePicture(const APicture: ISKPicture;
  const ATargetRect: TRectF): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_picture(GetHandle(APicture), @sk_rect_t(ATargetRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePicture(
  const APicture: ISKPicture): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_picture(GetHandle(APicture), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePointLightDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_point_light_diffuse(sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePointLightDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_point_light_diffuse(sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePointLightSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_point_light_specular(sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakePointLightSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_point_light_specular(sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeShader(const AShader: ISKShader;
  const ACropRect: TRectF; const ADither: Boolean): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_shader(GetHandle(AShader), ADither, @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeShader(const AShader: ISKShader;
  const ADither: Boolean): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_shader(GetHandle(AShader), ADither, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeSpotLightDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_spot_light_diffuse(sk_point3_t(ALocation), sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeSpotLightDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_spot_light_diffuse(sk_point3_t(ALocation), sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeSpotLightSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_spot_light_specular(sk_point3_t(ALocation), sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeSpotLightSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_spot_light_specular(sk_point3_t(ALocation), sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, GetHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

class function TSKImageFilter.MakeTile(const ASrc, ADest: TRect;
  const AInput: ISKImageFilter): ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_imagefilter_make_tile(sk_rect_t(ASrc), sk_rect_t(ADest), GetHandle(AInput));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

{ TSKSurfaceProps }

constructor TSKSurfaceProps.Create(const AFlags: TSKSurfacePropsFlags;
  const APixelGeometry: TSKPixelGeometry);
begin
  CreateNative(sk4d_sk_surfaceprops_create(Byte(AFlags), sk_pixelgeometry_t(APixelGeometry)));
end;

class procedure TSKSurfaceProps.DoDestroy(const AHandle: THandle);
begin
  sk4d_surfaceprops_destroy(AHandle);
end;

function TSKSurfaceProps.GetFlags: TSKSurfacePropsFlags;
begin
  Result := TSKSurfacePropsFlags(Byte(sk4d_surfaceprops_get_flags(Handle)));
end;

function TSKSurfaceProps.GetPixelGeometry: TSKPixelGeometry;
begin
  Result := TSKPixelGeometry(sk4d_surfaceprops_get_pixel_geometry(Handle));
end;

function TSKSurfaceProps.IsEqual(const AProps: ISKSurfaceProps): Boolean;
begin
  Assert(Assigned(AProps));
  Result := sk4d_surfaceprops_is_equal(Handle, GetHandle(AProps));
end;

{ TSKSurface }

procedure TSKSurface.Draw(const ACanvas: ISKCanvas; const AX, AY: Single;
  const APaint: ISKPaint);
begin
  Draw(ACanvas, AX, AY, TSKSamplingOptions.Create(TSKCubicResampler.Mitchell), APaint);
end;

procedure TSKSurface.Draw(const ACanvas: ISKCanvas; const AX, AY: Single;
  const ASampling: TSKSamplingOptions; const APaint: ISKPaint);
begin
  sk4d_surface_draw(Handle, GetHandle(ACanvas), AX, AY, sk_samplingoptions_t(ASampling), GetHandle(APaint));
end;

procedure TSKSurface.Flush;
begin
  sk4d_surface_flush(Handle);
end;

function TSKSurface.GetCanvas: ISKCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := sk4d_surface_get_canvas(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCanvas.CreateNative(LHandle, False);
end;

function TSKSurface.GetHeight: Integer;
begin
  Result := sk4d_surface_get_height(Handle);
end;

function TSKSurface.GetImageInfo: TSKImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_surface_get_image_info(Handle, LResult);
  Result := TSKImageInfo(LResult);
end;

function TSKSurface.GetProps: ISKSurfaceProps;
begin
  Result := TSKSurfaceProps.CreateNative(sk4d_surface_get_props(Handle), False);
end;

function TSKSurface.GetWidth: Integer;
begin
  Result := sk4d_surface_get_width(Handle);
end;

class function TSKSurface.MakeFromRenderTarget(const AContext: IGRDirectContext;
  const ARenderTarget: IGRBackendRenderTarget; const AOrigin: TGRSurfaceOrigin;
  const AColorType: TSKColorType; const AColorSpace: ISKColorSpace;
  const AProps: ISKSurfaceProps): ISKSurface;
var
  LHandle: sk_surface_t;
begin
  Assert(Assigned(ARenderTarget));
  LHandle := sk4d_surface_make_from_rendertarget(GetHandle(AContext), GetHandle(ARenderTarget), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), GetHandle(AColorSpace), GetHandle(AProps));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSurface.CreateNative(LHandle);
end;

class function TSKSurface.MakeFromTexture(const AContext: IGRDirectContext;
  const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSKColorType;
  const AColorSpace: ISKColorSpace; const AProps: ISKSurfaceProps): ISKSurface;
var
  LHandle: sk_surface_t;
begin
  Assert(Assigned(ATexture));
  LHandle := sk4d_surface_make_from_texture(GetHandle(AContext), GetHandle(ATexture), gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), GetHandle(AColorSpace), GetHandle(AProps));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSurface.CreateNative(LHandle);
end;

function TSKSurface.MakeImageSnapshot(const ABounds: TRect): ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_surface_make_image_snapshot2(Handle, sk_irect_t(ABounds));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

function TSKSurface.MakeImageSnapshot: ISKImage;
var
  LHandle: sk_image_t;
begin
  LHandle := sk4d_surface_make_image_snapshot(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImage.CreateNative(LHandle);
end;

class function TSKSurface.MakeNull: ISKSurface;
var
  LHandle: sk_surface_t;
begin
  LHandle := sk4d_surface_make_null;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSurface.CreateNative(LHandle);
end;

class function TSKSurface.MakeRaster(const AImageInfo: TSKImageInfo;
  const ARowBytes: NativeUInt; const AProps: ISKSurfaceProps): ISKSurface;
var
  LHandle: sk_surface_t;
begin
  LHandle := sk4d_surface_make_raster(sk_imageinfo_t(AImageInfo), ARowBytes, GetHandle(AProps));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSurface.CreateNative(LHandle);
end;

class function TSKSurface.MakeRaster(const AImageInfo: TSKImageInfo;
  const AProps: ISKSurfaceProps): ISKSurface;
begin
  Result := MakeRaster(AImageInfo, AImageInfo.MinRowBytes, AProps) as ISKSurface;
end;

class function TSKSurface.MakeRaster(const AWidth,
  AHeight: Integer): ISKSurface;
begin
  Result := MakeRaster(TSKImageInfo.Create(AWidth, AHeight));
end;

class function TSKSurface.MakeRasterDirect(const APixmap: ISKPixmap;
  const ARasterReleaseProc: TSKSurfaceRasterReleaseProc;
  const AProps: ISKSurfaceProps): ISKSurface;
begin
  Result := MakeRasterDirect(APixmap.ImageInfo, APixmap.Pixels, APixmap.RowBytes, ARasterReleaseProc, AProps);
end;

class function TSKSurface.MakeRasterDirect(const AImageInfo: TSKImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ARasterReleaseProc: TSKSurfaceRasterReleaseProc;
  const AProps: ISKSurfaceProps): ISKSurface;
var
  LContext: Pointer;
  LHandle: sk_surface_t;
begin
  LContext := PPointer(@ARasterReleaseProc)^;
  LHandle  := sk4d_surface_make_raster_direct(sk_imageinfo_t(AImageInfo), APixels, ARowBytes, raster_release_proc, LContext, GetHandle(AProps));
  if LHandle = 0 then
    Exit(nil);
  if LContext <> nil then
    IInterface(LContext)._AddRef;
  Result := TSKSurface.CreateNative(LHandle);
end;

class function TSKSurface.MakeRenderTarget(const AContext: IGRDirectContext;
  const ABudgeted: Boolean; const AImageInfo: TSKImageInfo;
  const ASampleCount: Integer; const AOrigin: TGRSurfaceOrigin;
  const AProps: ISKSurfaceProps;
  const AShouldCreateWithMips: Boolean): ISKSurface;
var
  LHandle: sk_surface_t;
begin
  LHandle := sk4d_surface_make_render_target(GetHandle(AContext), ABudgeted, sk_imageinfo_t(AImageInfo), ASampleCount, gr_surfaceorigin_t(AOrigin), GetHandle(AProps), AShouldCreateWithMips);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSurface.CreateNative(LHandle);
end;

function TSKSurface.PeekPixels: ISKPixmap;
begin
  Result := TSKPixmap.Create;
  if not sk4d_surface_peek_pixels(Handle, GetHandle(Result)) then
    Result := nil;
end;

class procedure TSKSurface.raster_release_proc(pixels, context: Pointer);
begin
  if context <> nil then
  begin
    TSKSurfaceRasterReleaseProc(context)(pixels);
    IInterface(context)._Release;
  end;
end;

function TSKSurface.ReadPixels(const ADestImageInfo: TSKImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer): Boolean;
var
  LPixmap: TSKPixmap;
begin
  LPixmap := TSKPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY);
end;

function TSKSurface.ReadPixels(const ADest: ISKPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_surface_read_pixels(Handle, GetHandle(ADest), ASrcX, ASrcY);
end;

procedure TSKSurface.WritePixels(const ASrcImageInfo: TSKImageInfo;
  const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX,
  ADestY: Integer);
var
  LPixmap: TSKPixmap;
begin
  LPixmap := TSKPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  WritePixels(LPixmap, ADestX, ADestY);
end;

procedure TSKSurface.WritePixels(const ASrc: ISKPixmap; const ADestX,
  ADestY: Integer);
begin
  Assert(Assigned(ASrc));
  sk4d_surface_write_pixels(Handle, GetHandle(ASrc), ADestX, ADestY);
end;

{ TSKVertices }

class procedure TSKVertices.DoRef(const AHandle: THandle);
begin
  sk4d_vertices_ref(AHandle);
end;

class procedure TSKVertices.DoUnref(const AHandle: THandle);
begin
  sk4d_vertices_unref(AHandle);
end;

function TSKVertices.GetUniqueID: Cardinal;
begin
  Result := sk4d_vertices_get_unique_id(Handle);
end;

class function TSKVertices.MakeCopy(const AVertexMode: TSKVertexMode;
  const APositions, ATextures: TArray<TPointF>;
  const AColors: TArray<TAlphaColor>;
  const AIndices: TArray<Word>): ISKVertices;
var
  LHandle: sk_vertices_t;
begin
  Assert(((not Assigned(ATextures)) or (Length(APositions) = Length(ATextures))) and ((not Assigned(AColors)) or (Length(APositions) = Length(AColors))));
  LHandle := sk4d_vertices_make_copy(sk_vertexmode_t(AVerTexMode), Length(APositions), @sk_point_t(APositions[0]), @sk_point_t(ATextures[0]), @AColors[0], Length(AIndices), @AIndices[0]);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKVertices.CreateNative(LHandle)
end;

{ TSKFontMetrics }

class operator TSKFontMetrics.Equal(const AFontMetrics1,
  AFontMetrics2: TSKFontMetrics): Boolean;
begin
  Result := (AFontMetrics1.Flags              = AFontMetrics2.Flags             ) and
            (AFontMetrics1.Top                = AFontMetrics2.Top               ) and
            (AFontMetrics1.Ascent             = AFontMetrics2.Ascent            ) and
            (AFontMetrics1.Descent            = AFontMetrics2.Descent           ) and
            (AFontMetrics1.Bottom             = AFontMetrics2.Bottom            ) and
            (AFontMetrics1.Leading            = AFontMetrics2.Leading           ) and
            (AFontMetrics1.AvgCharWidth       = AFontMetrics2.AvgCharWidth      ) and
            (AFontMetrics1.MaxCharWidth       = AFontMetrics2.MaxCharWidth      ) and
            (AFontMetrics1.XMin               = AFontMetrics2.XMin              ) and
            (AFontMetrics1.XMax               = AFontMetrics2.XMax              ) and
            (AFontMetrics1.XMax               = AFontMetrics2.XMax              ) and
            (AFontMetrics1.XHeight            = AFontMetrics2.XHeight           ) and
            (AFontMetrics1.UnderlineThickness = AFontMetrics2.UnderlineThickness) and
            (AFontMetrics1.UnderlinePosition  = AFontMetrics2.UnderlinePosition ) and
            (AFontMetrics1.StrikeoutThickness = AFontMetrics2.StrikeoutThickness) and
            (AFontMetrics1.StrikeoutPosition  = AFontMetrics2.StrikeoutPosition );
end;

class operator TSKFontMetrics.Explicit(
  const AFontMetrics: sk_fontmetrics_t): TSKFontMetrics;
begin
  Result.Flags              := TSKFontMetricsFlags(Byte(AFontMetrics.flags));
  Result.Top                := AFontMetrics.top;
  Result.Ascent             := AFontMetrics.descent;
  Result.Descent            := AFontMetrics.bottom;
  Result.Bottom             := AFontMetrics.leading;
  Result.Leading            := AFontMetrics.average_character_width;
  Result.AvgCharWidth       := AFontMetrics.max_character_width;
  Result.MaxCharWidth       := AFontMetrics.max_character_width;
  Result.XMin               := AFontMetrics.x_min;
  Result.XMax               := AFontMetrics.x_max;
  Result.XHeight            := AFontMetrics.x_height;
  Result.CapHeight          := AFontMetrics.cap_height;
  Result.UnderlineThickness := AFontMetrics.underline_thickness;
  Result.UnderlinePosition  := AFontMetrics.underline_position;
  Result.StrikeoutThickness := AFontMetrics.strikeout_thickness;
  Result.StrikeoutPosition  := AFontMetrics.strikeout_position;
end;

class operator TSKFontMetrics.NotEqual(const AFontMetrics1,
  AFontMetrics2: TSKFontMetrics): Boolean;
begin
  Result := not (AFontMetrics1 = AFontMetrics2);
end;

{ TSKFontStyle }

class function TSKFontStyle.Bold: ISKFontStyle;
begin
  Result := TSKFontStyle.Create(TSKFontWeight.Bold, TSKFontWidth.Normal, TSKFontSlant.Upright);
end;

class function TSKFontStyle.BoldItalic: ISKFontStyle;
begin
  Result := TSKFontStyle.Create(TSKFontWeight.Bold, TSKFontWidth.Normal, TSKFontSlant.Italic);
end;

constructor TSKFontStyle.Create(const AWeight, AWidth: Integer;
  const ASlant: TSKFontSlant);
begin
  CreateNative(sk4d_fontstyle_create(AWeight, AWidth, sk_fontslant_t(ASlant)));
end;

constructor TSKFontStyle.Create(const AWeight: TSKFontWeight;
  const AWidth: TSKFontWidth; const ASlant: TSKFontSlant);
begin
  Create(FontWeight[AWeight], FontWidth[AWidth], ASlant);
end;

class procedure TSKFontStyle.DoDestroy(const AHandle: THandle);
begin
  sk4d_fontstyle_destroy(AHandle);
end;

function TSKFontStyle.GetSlant: TSKFontSlant;
begin
  Result := TSKFontSlant(sk4d_fontstyle_get_slant(Handle));
end;

function TSKFontStyle.GetWeight: Integer;
begin
  Result := sk4d_fontstyle_get_weight(Handle);
end;

function TSKFontStyle.GetWidth: Integer;
begin
  Result := sk4d_fontstyle_get_width(Handle);
end;

class function TSKFontStyle.Italic: ISKFontStyle;
begin
  Result := TSKFontStyle.Create(TSKFontWeight.Normal, TSKFontWidth.Normal, TSKFontSlant.Italic);
end;

class function TSKFontStyle.Normal: ISKFontStyle;
begin
  Result := TSKFontStyle.Create(TSKFontWeight.Normal, TSKFontWidth.Normal, TSKFontSlant.Upright);
end;

{ TSKTypeface }

function TSKTypeface.GetFamilyName: ISKString;
begin
  Result := TSKString.Create;
  sk4d_typeface_get_family_name(Handle, GetHandle(Result));
end;

function TSKTypeface.GetSlant: TSKFontSlant;
begin
  Result := TSKFontSlant(sk4d_typeface_get_slant(Handle));
end;

function TSKTypeface.GetStyle: ISKFontStyle;
begin
  Result := TSKFontStyle.CreateNative(sk4d_typeface_get_style(Handle));
end;

function TSKTypeface.GetUniqueID: Cardinal;
begin
  Result := sk4d_typeface_get_unique_id(Handle);
end;

function TSKTypeface.GetWeight: Integer;
begin
  Result := sk4d_typeface_get_weight(Handle);
end;

function TSKTypeface.GetWidth: Integer;
begin
  Result := sk4d_typeface_get_width(Handle);
end;

function TSKTypeface.IsBold: Boolean;
begin
  Result := GetWeight >= TSKFontStyle.FontWeight[TSKFontWeight.SemiBold];
end;

function TSKTypeface.IsItalic: Boolean;
begin
  Result := GetSlant <> TSKFontSlant.Upright;
end;

class function TSKTypeface.MakeDefault: ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := sk4d_typeface_make_default;
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

class function TSKTypeface.MakeFromData(const AData: ISKData;
  const ATTcIndex: Integer): ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := sk4d_typeface_make_from_data(GetHandle(AData), ATTcIndex);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

class function TSKTypeface.MakeFromFile(const AFileName: string;
  const ATTcIndex: Integer): ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := sk4d_typeface_make_from_file(MarshaledAString(UTF8String(AFileName)), ATTcIndex);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

class function TSKTypeface.MakeFromName(
  const AStyle: ISKFontStyle): ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  Assert(Assigned(AStyle));
  LHandle := sk4d_typeface_make_from_name(0, GetHandle(AStyle));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

class function TSKTypeface.MakeFromName(const AFamilyName: string;
  const AStyle: ISKFontStyle): ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  Assert(Assigned(AStyle));
  LHandle := sk4d_typeface_make_from_name(MarshaledAString(UTF8String(AFamilyName)), GetHandle(AStyle));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

class function TSKTypeface.MakeFromStream(const AStream: ISKStream;
  const ATTcIndex: Integer): ISKTypeface;
var
  LData: ISKData;
begin
  LData  := TSKData.MakeFromStream(AStream, AStream.Length - AStream.Position);
  Result := MakeFromData(LData, ATTcIndex);
end;

{ TSKFont }

constructor TSKFont.Create(const ATypeface: ISKTypeface; const ASize, AScaleX,
  ASkewX: Single);
begin
  CreateNative(sk4d_font_create(GetHandle(ATypeface), ASize, AScaleX, ASkewX));
end;

constructor TSKFont.Create(const AFont: ISKFont);
begin
  Assert(Assigned(AFont));
  CreateNative(sk4d_font_create2(GetHandle(AFont)));
end;

class procedure TSKFont.DoDestroy(const AHandle: THandle);
begin
  sk4d_font_destroy(AHandle);
end;

function TSKFont.GetBaselineSnap: Boolean;
begin
  Result := sk4d_font_get_baseline_snap(Handle);
end;

function TSKFont.GetBounds(const AGlyphs: TArray<Word>;
  const APaint: ISKPaint): TArray<TRectF>;
begin
  SetLength(Result, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), nil, @sk_rect_t(Result[0]), GetHandle(APaint));
end;

function TSKFont.GetEdging: TSKFontEdging;
begin
  Result := TSKFontEdging(sk4d_font_get_edging(Handle));
end;

function TSKFont.GetEmbeddedBitmaps: Boolean;
begin
  Result := sk4d_font_get_embedded_bitmaps(Handle);
end;

function TSKFont.GetEmbolden: Boolean;
begin
  Result := sk4d_font_get_embolden(Handle);
end;

function TSKFont.GetForceAutoHinting: Boolean;
begin
  Result := sk4d_font_get_force_auto_hinting(Handle);
end;

function TSKFont.GetGlyphs(const AText: UCS4String): TArray<Word>;
var
  LCount: Integer;
begin
  LCount := sk4d_font_get_glyphs_count(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UCS4Char), UTF32_SK_TEXTENCODING);
  SetLength(Result, LCount);
  sk4d_font_get_glyphs(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UCS4Char), UTF32_SK_TEXTENCODING, @Result[0], LCount);
end;

function TSKFont.GetGlyphs(const AText: UTF8String): TArray<Word>;
var
  LCount: Integer;
begin
  LCount := sk4d_font_get_glyphs_count(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), UTF8_SK_TEXTENCODING);
  SetLength(Result, LCount);
  sk4d_font_get_glyphs(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), UTF8_SK_TEXTENCODING, @Result[0], LCount);
end;

function TSKFont.GetGlyphs(const AText: string): TArray<Word>;
var
  LCount: Integer;
begin
  LCount := sk4d_font_get_glyphs_count(Handle, @AText[Low(AText)], Length(AText) * SizeOf(Char), UTF16_SK_TEXTENCODING);
  SetLength(Result, LCount);
  sk4d_font_get_glyphs(Handle, @AText[Low(AText)], Length(AText) * SizeOf(Char), UTF16_SK_TEXTENCODING, @Result[0], LCount);
end;

function TSKFont.GetHinting: TSKFontHinting;
begin
  Result := TSKFontHinting(sk4d_font_get_hinting(Handle));
end;

function TSKFont.GetIntercepts(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const ATop, ABottom: Single;
  const APaint: ISKPaint): TArray<Single>;
var
  LCount: Integer;
begin
  Assert(Length(AGlyphs) = Length(APositions));
  LCount := sk4d_font_get_intercepts(Handle, @AGlyphs[0], Length(AGlyphs), @sk_point_t(APositions[0]), ATop, ABottom, nil, GetHandle(APaint));
  SetLength(Result, LCount);
  sk4d_font_get_intercepts(Handle, @AGlyphs[0], Length(AGlyphs), @sk_point_t(APositions[0]), ATop, ABottom, @Result[0], GetHandle(APaint));
end;

function TSKFont.GetLinearMetrics: Boolean;
begin
  Result := sk4d_font_get_linear_metrics(Handle);
end;

function TSKFont.GetMetrics(out AMetrics: TSKFontMetrics): Single;
var
  LMetrics: sk_fontmetrics_t;
begin
  Result   := sk4d_font_get_metrics(Handle, @LMetrics);
  AMetrics := TSKFontMetrics(LMetrics);
end;

function TSKFont.GetOffsets(const AGlyphs: TArray<Word>;
  const AOrigin: Single): TArray<Single>;
begin
  SetLength(Result, Length(AGlyphs));
  sk4d_font_get_offsets(Handle, @AGlyphs[0], Length(AGlyphs), @Result[0], AOrigin);
end;

function TSKFont.GetPath(const AGlyph: Word): ISKPath;
begin
  Result := TSKPath.Create;
  if not sk4d_font_get_path(Handle, AGlyph, GetHandle(Result)) then
    Result := nil;
end;

procedure TSKFont.GetPaths(const AGlyphs: TArray<Word>;
  const AProc: TSKFontGlyphPathProc);
begin
  if Assigned(AProc) then
    sk4d_font_get_paths(Handle, @AGlyphs[0], Length(AGlyphs), getpaths_proc, @AProc);
end;

class procedure TSKFont.getpaths_proc(const path: sk_path_t;
  const [Ref] matrix: sk_matrix_t; context: Pointer);
var
  LPath: ISKPath;
begin
  if path = 0 then
    LPath := nil
  else
    LPath := TSKPath.CreateNative(path, False);
  TSKFontGlyphPathProc(context^)(LPath, TMatrix(matrix));
end;

function TSKFont.GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>;
begin
  Result := GetPositions(AGlyphs, TPointF.Zero);
end;

function TSKFont.GetPositions(const AGlyphs: TArray<Word>;
  const AOrigin: TPointF): TArray<TPointF>;
begin
  SetLength(Result, Length(AGlyphs));
  sk4d_font_get_positions(Handle, @AGlyphs[0], Length(AGlyphs), @sk_point_t(Result[0]), sk_point_t(AOrigin));
end;

function TSKFont.GetScaleX: Single;
begin
  Result := sk4d_font_get_scale_x(Handle);
end;

function TSKFont.GetSize: Single;
begin
  Result := sk4d_font_get_size(Handle);
end;

function TSKFont.GetSkewX: Single;
begin
  Result := sk4d_font_get_skew_x(Handle);
end;

function TSKFont.GetSpacing: Single;
begin
  Result := sk4d_font_get_metrics(Handle, nil);
end;

function TSKFont.GetSubpixel: Boolean;
begin
  Result := sk4d_font_get_subpixel(Handle);
end;

function TSKFont.GetTypeface: ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := sk4d_font_get_typeface(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

function TSKFont.GetTypefaceOrDefault: ISKTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := sk4d_font_get_typeface_or_default(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTypeFace.CreateNative(LHandle);
end;

function TSKFont.GetWidths(const AGlyphs: TArray<Word>;
  const APaint: ISKPaint): TArray<Single>;
begin
  SetLength(Result, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), @Result[0], nil, GetHandle(APaint));
end;

procedure TSKFont.GetWidthsAndBounds(const AGlyphs: TArray<Word>;
  out AWidths: TArray<Single>; out ABounds: TArray<TRectF>;
  const APaint: ISKPaint);
begin
  SetLength(AWidths, Length(AGlyphs));
  SetLength(ABounds, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), @AWidths[0], @sk_rect_t(ABounds[0]), GetHandle(APaint));
end;

function TSKFont.IsEqual(const AFont: ISKFont): Boolean;
begin
  Assert(Assigned(AFont));
  Result := sk4d_font_is_equal(Handle, GetHandle(AFont));
end;

function TSKFont.MakeWithSize(const ASize: Single): ISKFont;
begin
  Result := TSKFont.Create(Self);
  Result.Size := ASize;
end;

function TSKFont.MeasureText(const AText: string; out ABounds: TRectF;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(Char), UTF16_SK_TEXTENCODING, @sk_rect_t(ABounds), GetHandle(APaint));
end;

function TSKFont.MeasureText(const AText: string;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(Char), UTF16_SK_TEXTENCODING, nil, GetHandle(APaint));
end;

function TSKFont.MeasureText(const AText: UTF8String; out ABounds: TRectF;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), UTF8_SK_TEXTENCODING, @sk_rect_t(ABounds), GetHandle(APaint));
end;

function TSKFont.MeasureText(const AText: UTF8String;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), UTF8_SK_TEXTENCODING, nil, GetHandle(APaint));
end;

function TSKFont.MeasureText(const AText: UCS4String; out ABounds: TRectF;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UCS4Char), UTF32_SK_TEXTENCODING, @sk_rect_t(ABounds), GetHandle(APaint));
end;

function TSKFont.MeasureText(const AText: UCS4String;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UCS4Char), UTF32_SK_TEXTENCODING, nil, GetHandle(APaint));
end;

function TSKFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  out ABounds: TRectF; const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, @sk_rect_t(ABounds), GetHandle(APaint));
end;

function TSKFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  const APaint: ISKPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, nil, GetHandle(APaint));
end;

procedure TSKFont.SetBaselineSnap(const AValue: Boolean);
begin
  sk4d_font_set_baseline_snap(Handle, AValue);
end;

procedure TSKFont.SetEdging(const AValue: TSKFontEdging);
begin
  sk4d_font_set_edging(Handle, sk_fontedging_t(AValue));
end;

procedure TSKFont.SetEmbeddedBitmaps(const AValue: Boolean);
begin
  sk4d_font_set_embedded_bitmaps(Handle, AValue);
end;

procedure TSKFont.SetEmbolden(const AValue: Boolean);
begin
  sk4d_font_set_embolden(Handle, AValue);
end;

procedure TSKFont.SetForceAutoHinting(const AValue: Boolean);
begin
  sk4d_font_set_force_auto_hinting(Handle, AValue);
end;

procedure TSKFont.SetHinting(const AValue: TSKFontHinting);
begin
  sk4d_font_set_hinting(Handle, sk_fonthinting_t(AValue));
end;

procedure TSKFont.SetLinearMetrics(const AValue: Boolean);
begin
  sk4d_font_set_linear_metrics(Handle, AValue);
end;

procedure TSKFont.SetScaleX(const AValue: Single);
begin
  sk4d_font_set_scale_x(Handle, AValue);
end;

procedure TSKFont.SetSize(const AValue: Single);
begin
  sk4d_font_set_size(Handle, AValue);
end;

procedure TSKFont.SetSkewX(const AValue: Single);
begin
  sk4d_font_set_skew_x(Handle, AValue);
end;

procedure TSKFont.SetSubpixel(const AValue: Boolean);
begin
  sk4d_font_set_subpixel(Handle, AValue);
end;

procedure TSKFont.SetTypeface(const AValue: ISKTypeface);
begin
  sk4d_font_set_typeface(Handle, GetHandle(AValue));
end;

function TSKFont.UnicharsToGlyphs(
  const AUnichars: TArray<Integer>): TArray<Word>;
begin
  SetLength(Result, Length(AUnichars));
  sk4d_font_unichars_to_glyphs(Handle, @AUnichars[0], Length(AUnichars), @Result[0]);
end;

function TSKFont.UnicharToGlyph(const AUnichar: Integer): Word;
begin
  Result := sk4d_font_unichar_to_glyph(Handle, AUnichar);
end;

{ TSKRotationScaleMatrix }

constructor TSKRotationScaleMatrix.Create(const AScale, ARadians, ATX, ATY,
  AAnchorX, AAnchorY: Single);
begin
  SCos := Cos(ARadians) * AScale;
  SSin := Sin(ARadians) * AScale;
  TX   := ATX + - SCos * AAnchorX + SSin * AAnchorY;
  TY   := ATY + - SSin * AAnchorX - SCos * AAnchorY;
end;

constructor TSKRotationScaleMatrix.CreateRotation(const ARadians, AAnchorX,
  AAnchorY: Single);
begin
  Create(1, ARadians, 0, 0, AAnchorX, AAnchorY);
end;

constructor TSKRotationScaleMatrix.CreateScale(const AScale: Single);
begin
  SCos := AScale;
  SSin := 0;
  TX   := 0;
  TY   := 0;
end;

constructor TSKRotationScaleMatrix.CreateTranslation(const AX, AY: Single);
begin
  SCos := 1;
  SSin := 0;
  TX   := AX;
  TY   := AY;
end;

class function TSKRotationScaleMatrix.Identity: TSKRotationScaleMatrix;
begin
  Result.SCos := 1;
  Result.SSin := 0;
  Result.TX   := 0;
  Result.TY   := 0;
end;

{ TSKTextBlob }

class procedure TSKTextBlob.DoRef(const AHandle: THandle);
begin
  sk4d_textblob_ref(AHandle);
end;

class procedure TSKTextBlob.DoUnref(const AHandle: THandle);
begin
  sk4d_textblob_unref(AHandle);
end;

function TSKTextBlob.GetIntercepts(const AUpperBounds, ALowerBounds: Single;
  const APaint: ISKPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
  LCount: Integer;
begin
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  LCount := sk4d_textblob_get_intercepts(Handle, @LBounds[0], nil, GetHandle(APaint));
  SetLength(Result, LCount);
  sk4d_textblob_get_intercepts(Handle, @LBounds[0], @Result[0], GetHandle(APaint));
end;

class function TSKTextBlob.Make(const AText: UCS4String; const AFont: ISKFont;
  const AOrigin: TPointF): ISKTextBlob;
begin
  Result := Make(AText, AFont, TPointF.Zero);
end;

class function TSKTextBlob.Make(const AText: UTF8String;
  const AFont: ISKFont): ISKTextBlob;
begin
  Result := Make(AText, AFont, TPointF.Zero);
end;

class function TSKTextBlob.Make(const AText: UTF8String; const AFont: ISKFont;
  const AOrigin: TPointF): ISKTextBlob;
var
  LGlyphs: TArray<Word>;
begin
  Assert(Assigned(AFont));
  LGlyphs := AFont.GetGlyphs(AText);
  Result  := MakePositionedGlyphs(LGlyphs, AFont.GetPositions(LGlyphs, AOrigin), AFont);
end;

class function TSKTextBlob.Make(const AText: string; const AFont: ISKFont;
  const AOrigin: TPointF): ISKTextBlob;
var
  LGlyphs: TArray<Word>;
begin
  Assert(Assigned(AFont));
  LGlyphs := AFont.GetGlyphs(AText);
  Result  := MakePositionedGlyphs(LGlyphs, AFont.GetPositions(LGlyphs, AOrigin), AFont);
end;

class function TSKTextBlob.Make(const AText: string;
  const AFont: ISKFont): ISKTextBlob;
begin
  Result := Make(AText, AFont, TPointF.Zero);
end;

class function TSKTextBlob.Make(const AText: UCS4String;
  const AFont: ISKFont): ISKTextBlob;
begin
  Result := Make(AText, AFont, TPointF.Zero);
end;

class function TSKTextBlob.MakeGlyphs(const AGlyphs: TArray<Word>;
  const AFont: ISKFont): ISKTextBlob;
begin
  Result := MakeGlyphs(AGlyphs, AFont, TPointF.Zero);
end;

class function TSKTextBlob.MakeGlyphs(const AGlyphs: TArray<Word>;
  const AFont: ISKFont; const AOrigin: TPointF): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakePositionedGlyphs(AGlyphs, AFont.GetPositions(AGlyphs, AOrigin), AFont);
end;

class function TSKTextBlob.MakeHorizontal(const AText: UCS4String;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeHorizontalGlyphs(AFont.GetGlyphs(AText), APositions, AY, AFont);
end;

class function TSKTextBlob.MakeHorizontal(const AText: string;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeHorizontalGlyphs(AFont.GetGlyphs(AText), APositions, AY, AFont);
end;

class function TSKTextBlob.MakeHorizontal(const AText: UTF8String;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeHorizontalGlyphs(AFont.GetGlyphs(AText), APositions, AY, AFont);
end;

class function TSKTextBlob.MakeHorizontalGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISKFont): ISKTextBlob;
var
  LBuffer: TSKHorizontalRunBuffer;
  LBuilder: ISKTextBlobBuilder;
begin
  Assert(Length(AGlyphs) = Length(APositions));
  LBuilder := TSKTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocateHorizontalRun(AFont, Length(AGlyphs), AY);
  Move(AGlyphs[0], LBuffer.Glyphs[0], SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.Positions[0], SizeOf(Single) * Length(APositions));
  Result := LBuilder.Detach;
end;

class function TSKTextBlob.MakePositioned(const AText: UTF8String;
  const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakePositionedGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakePositioned(const AText: UCS4String;
  const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakePositionedGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakePositioned(const AText: string;
  const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakePositionedGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakePositionedGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AFont: ISKFont): ISKTextBlob;
var
  LBuffer: TSKPositionedRunBuffer;
  LBuilder: ISKTextBlobBuilder;
begin
  Assert(Length(AGlyphs) = Length(APositions));
  LBuilder := TSKTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocatePositionedRun(AFont, Length(AGlyphs));
  Move(AGlyphs[0], LBuffer.Glyphs[0], SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.Positions[0], SizeOf(TPointF) * Length(APositions));
  Result := LBuilder.Detach;
end;

class function TSKTextBlob.MakeRotationScale(const AText: UTF8String;
  const APositions: TArray<TSKRotationScaleMatrix>;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeRotationScaleGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakeRotationScale(const AText: UCS4String;
  const APositions: TArray<TSKRotationScaleMatrix>;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeRotationScaleGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakeRotationScale(const AText: string;
  const APositions: TArray<TSKRotationScaleMatrix>;
  const AFont: ISKFont): ISKTextBlob;
begin
  Assert(Assigned(AFont));
  Result := MakeRotationScaleGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSKTextBlob.MakeRotationScaleGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TSKRotationScaleMatrix>;
  const AFont: ISKFont): ISKTextBlob;
var
  LBuffer: TSKRotationScaleRunBuffer;
  LBuilder: ISKTextBlobBuilder;
begin
  Assert(Length(AGlyphs) = Length(APositions));
  LBuilder := TSKTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocateRotationScaleRun(AFont, Length(AGlyphs));
  Move(AGlyphs[0], LBuffer.Glyphs[0], SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.Positions[0], SizeOf(TSKRotationScaleMatrix) * Length(APositions));
  Result := LBuilder.Detach;
end;

{ TSKRunBuffer }

class operator TSKRunBuffer.Explicit(
  const ARunHandler: sk_runbuffer_t): TSKRunBuffer;
begin
  Result.Glyphs := Pointer(ARunHandler.glyphs);
end;

{ TSKHorizontalRunBuffer }

class operator TSKHorizontalRunBuffer.Explicit(
  const ARunHandler: sk_runbuffer_t): TSKHorizontalRunBuffer;
begin
  Result.Glyphs    := Pointer(ARunHandler.glyphs);
  Result.Positions := Pointer(ARunHandler.positions);
end;

{ TSKPositionedRunBuffer }

class operator TSKPositionedRunBuffer.Explicit(
  const ARunHandler: sk_runbuffer_t): TSKPositionedRunBuffer;
begin
  Result.Glyphs    := Pointer(ARunHandler.glyphs);
  Result.Positions := Pointer(ARunHandler.positions);
end;

{ TSKRotationScaleRunBuffer }

class operator TSKRotationScaleRunBuffer.Explicit(
  const ARunHandler: sk_runbuffer_t): TSKRotationScaleRunBuffer;
begin
  Result.Glyphs    := Pointer(ARunHandler.glyphs);
  Result.Positions := Pointer(ARunHandler.positions);
end;

{ TSKTextBlobBuilder }

function TSKTextBlobBuilder.AllocateHorizontalRun(const AFont: ISKFont;
  const ACount: Integer; const AY: Single): TSKHorizontalRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_horizontal_run(Handle, GetHandle(AFont), ACount, AY, nil, LRunBuffer);
  Result := TSKHorizontalRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocateHorizontalRun(const AFont: ISKFont;
  const ACount: Integer; const AY: Single;
  const ABounds: TRectF): TSKHorizontalRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_horizontal_run(Handle, GetHandle(AFont), ACount, AY, @sk_rect_t(ABounds), LRunBuffer);
  Result := TSKHorizontalRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocatePositionedRun(const AFont: ISKFont;
  const ACount: Integer): TSKPositionedRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_positioned_run(Handle, GetHandle(AFont), ACount, nil, LRunBuffer);
  Result := TSKPositionedRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocatePositionedRun(const AFont: ISKFont;
  const ACount: Integer; const ABounds: TRectF): TSKPositionedRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_positioned_run(Handle, GetHandle(AFont), ACount, @sk_rect_t(ABounds), LRunBuffer);
  Result := TSKPositionedRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocateRotationScaleRun(const AFont: ISKFont;
  const ACount: Integer): TSKRotationScaleRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_rotation_scale_run(Handle, GetHandle(AFont), ACount, LRunBuffer);
  Result := TSKRotationScaleRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocateRun(const AFont: ISKFont;
  const ACount: Integer; const AX, AY: Single): TSKRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_run(Handle, GetHandle(AFont), ACount, AX, AY, nil, LRunBuffer);
  Result := TSKRunBuffer(LRunBuffer);
end;

function TSKTextBlobBuilder.AllocateRun(const AFont: ISKFont;
  const ACount: Integer; const AX, AY: Single;
  const ABounds: TRectF): TSKRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  Assert(Assigned(AFont));
  sk4d_textblobbuilder_alloc_run(Handle, GetHandle(AFont), ACount, AX, AY, @sk_rect_t(ABounds), LRunBuffer);
  Result := TSKRunBuffer(LRunBuffer);
end;

constructor TSKTextBlobBuilder.Create;
begin
  CreateNative(sk4d_textblobbuilder_create);
end;

function TSKTextBlobBuilder.Detach: ISKTextBlob;
var
  LHandle: sk_textblob_t;
begin
  LHandle := sk4d_textblobbuilder_detach(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTextBlob.CreateNative(LHandle);
end;

class procedure TSKTextBlobBuilder.DoDestroy(const AHandle: THandle);
begin
  sk4d_textblobbuilder_destroy(AHandle);
end;

{ TSKShaperRunHandlerInfo }

class operator TSKShaperRunHandlerInfo.Explicit(
  const [Ref] ARunHandlerInfo: sk_shaperrunhandlerinfo_t): TSKShaperRunHandlerInfo;
begin
  Result.Font       := TSKFont.CreateNative(ARunHandlerInfo.font, False);
  Result.BidiLevel  := ARunHandlerInfo.bidi_level;
  Result.Advance    := TPointF(ARunHandlerInfo.advance);
  Result.GlyphCount := ARunHandlerInfo.glyph_count;
  Result.UTF8Range  := TSKShaperRunHandlerRange(ARunHandlerInfo.utf8_range);
end;

{ TSKShaperRunHandler }

class procedure TSKShaperRunHandler.DoDestroy(const AHandle: THandle);
begin
  sk4d_shaperrunhandler_destroy(AHandle);
end;

{ TSKShaperRunHandlerBaseClass }

class procedure TSKShaperRunHandlerBaseClass.begin_line_proc(context: Pointer);
begin
  TSKShaperRunHandlerBaseClass(context).DoBeginLine;
end;

class procedure TSKShaperRunHandlerBaseClass.commit_line_proc(context: Pointer);
begin
  TSKShaperRunHandlerBaseClass(context).DoCommitLine;
end;

class procedure TSKShaperRunHandlerBaseClass.commit_run_buffer_proc(
  context: Pointer; const [Ref] info: sk_shaperrunhandlerinfo_t);
begin
  TSKShaperRunHandlerBaseClass(context).DoCommitRunBuffer(TSKShaperRunHandlerInfo(info));
end;

class procedure TSKShaperRunHandlerBaseClass.commit_run_info_proc(
  context: Pointer);
begin
  TSKShaperRunHandlerBaseClass(context).DoCommitRunInfo;
end;

constructor TSKShaperRunHandlerBaseClass.Create;
begin
  CreateNative(sk4d_shaperrunhandlerbaseclass_create(Self));
end;

class constructor TSKShaperRunHandlerBaseClass.Create;
var
  LProcs: sk_shaperrunhandlerbaseclass_procs_t;
begin
  LProcs.begin_line        := begin_line_proc;
  LProcs.commit_line       := commit_line_proc;
  LProcs.commit_run_buffer := commit_run_buffer_proc;
  LProcs.commit_run_info   := commit_run_info_proc;
  LProcs.run_buffer        := run_buffer_proc;
  LProcs.run_info          := run_info_proc;
  sk4d_shaperrunhandlerbaseclass_set_procs(LProcs);
end;

class procedure TSKShaperRunHandlerBaseClass.run_buffer_proc(context: Pointer;
  const [Ref] info: sk_shaperrunhandlerinfo_t;
  out result: sk_shaperrunhandlerbuffer_t);
begin
  result := sk_shaperrunhandlerbuffer_t(TSKShaperRunHandlerBaseClass(context).DoRunBuffer(TSKShaperRunHandlerInfo(info)));
end;

class procedure TSKShaperRunHandlerBaseClass.run_info_proc(context: Pointer;
  const [Ref] info: sk_shaperrunhandlerinfo_t);
begin
  TSKShaperRunHandlerBaseClass(context).DoRunInfo(TSKShaperRunHandlerInfo(info));
end;

{ TSKTextBlobBuilderRunHandler }

constructor TSKTextBlobBuilderRunHandler.Create(const AText: UTF8String;
  const AOffset: TPointF);
begin
  CreateNative(sk4d_textblobbuilderrunhandler_create(@AText[Low(AText)], sk_point_t(AOffset)));
end;

function TSKTextBlobBuilderRunHandler.Detach: ISKTextBlob;
var
  LHandle: sk_textblob_t;
begin
  LHandle := sk4d_textblobbuilderrunhandler_detach(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKTextBlob.CreateNative(LHandle);
end;

function TSKTextBlobBuilderRunHandler.GetEndPoint: TPointF;
begin
  sk4d_textblobbuilderrunhandler_get_end_point(Handle, sk_point_t(Result));
end;

{ TSKShaperRunIterator }

function TSKShaperRunIterator.Consume: Boolean;
begin
  Result := sk4d_shaperruniterator_consume(Handle);
end;

class procedure TSKShaperRunIterator.DoDestroy(const AHandle: THandle);
begin
  sk4d_shaperruniterator_destroy(AHandle);
end;

function TSKShaperRunIterator.GetEndOfCurrentRun: NativeUInt;
begin
  Result := sk4d_shaperruniterator_get_end_of_current_run(Handle);
end;

{ TSKShaperBiDiRunIterator }

function TSKShaperBiDiRunIterator.GetCurrentLevel: Byte;
begin
  Result := sk_shaperbidiruniterator_get_current_level(Handle);
end;

{ TSKShaperFontRunIterator }

function TSKShaperFontRunIterator.GetCurrentFont: ISKFont;
begin
  Result := TSKFont.CreateNative(sk_shaperfontruniterator_get_current_font(Handle), False);
end;

{ TSKShaperLanguageRunIterator }

function TSKShaperLanguageRunIterator.GetCurrentLanguage: string;
begin
  Result := string(sk_shaperlanguageruniterator_get_current_language(Handle));
end;

{ TSKFourByteTagHelper }

class function TSKFourByteTagHelper.Create(const AA, AB, AC,
  AD: UTF8Char): TSKFourByteTag;
begin
  Result := ((Cardinal(AA) shl 24) or (Cardinal(AB) shl 16) or (Cardinal(AC) shl 8) or Cardinal(AD));
end;

{ TSKShaperScriptRunIterator }

function TSKShaperScriptRunIterator.GetCurrentScript: TSKFourByteTag;
begin
  Result := sk_shaperscriptruniterator_get_current_script(Handle);
end;

{ TSKShaperFeature }

constructor TSKShaperFeature.Create(const ATag: TSKFourByteTag;
  const AValue: Cardinal; const AStart, AStop: NativeUInt);
begin
  Tag   := ATag;
  Value := AValue;
  Start := AStart;
  Stop  := AStop;
end;

{ TSKShaper }

constructor TSKShaper.Create;
begin
  CreateNative(sk4d_shaper_create);
end;

class procedure TSKShaper.DoDestroy(const AHandle: THandle);
begin
  sk4d_shaper_destroy(AHandle);
end;

class function TSKShaper.MakeBiDiRunIterator(const AText: UTF8String;
  const ABiDiLevel: Byte): ISKShaperBiDiRunIterator;
var
  LHandle: sk_shaperbidiruniterator_t;
begin
  LHandle := sk4d_shaper_make_bi_di_run_iterator(@AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), ABiDiLevel);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShaperBiDiRunIterator.CreateNative(LHandle);
end;

class function TSKShaper.MakeFontRunIterator(const AText: UTF8String;
  const AFont: ISKFont; const ARequestName: string;
  const ARequestStyle: ISKFontStyle;
  const ALanguage: ISKShaperLanguageRunIterator): ISKShaperFontRunIterator;
var
  LHandle: sk_shaperfontruniterator_t;
  LRequestStyle: ISKFontStyle;
begin
  Assert(Assigned(AFont));
  if Assigned(ARequestStyle) then
    LRequestStyle := ARequestStyle
  else
    LRequestStyle := AFont.GetTypefaceOrDefault.Style;
  LHandle := sk4d_shaper_make_font_run_iterator(@AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), GetHandle(AFont), MarshaledAString(UTF8String(ARequestName)), GetHandle(LRequestStyle), GetHandle(ALanguage));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShaperFontRunIterator.CreateNative(LHandle);
end;

class function TSKShaper.MakeScriptRunIterator(const AText: UTF8String;
  const AScript: TSKFourByteTag): ISKShaperScriptRunIterator;
var
  LHandle: sk_shaperscriptruniterator_t;
begin
  LHandle := sk4d_shaper_make_script_run_iterator(@AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), AScript);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShaperScriptRunIterator.CreateNative(LHandle);
end;

class function TSKShaper.MakeStandardLanguageRunIterator(
  const AText: UTF8String): ISKShaperLanguageRunIterator;
var
  LHandle: sk_shaperlanguageruniterator_t;
begin
  LHandle := sk4d_shaper_make_std_language_run_iterator(@AText[Low(AText)], Length(AText) * SizeOf(UTF8Char));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShaperLanguageRunIterator.CreateNative(LHandle);
end;

class procedure TSKShaper.PurgeCaches;
begin
  sk4d_shaper_purge_caches;
end;

procedure TSKShaper.Shape(const AText: UTF8String; const ASrcFont: ISKFont;
  const ALeftToRight: Boolean; const AWidth: Single;
  const AHandler: ISKShaperRunHandler);
begin
  Assert(Assigned(ASrcFont));
  sk4d_shaper_shape(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), GetHandle(ASrcFont), ALeftToRight, AWidth, GetHandle(AHandler));
end;

procedure TSKShaper.Shape(const AText: UTF8String;
  const AFont: ISKShaperFontRunIterator; const ABiDi: ISKShaperBiDiRunIterator;
  const AScript: ISKShaperScriptRunIterator;
  const ALanguage: ISKShaperLanguageRunIterator; const AWidth: Single;
  const AHandler: ISKShaperRunHandler;
  const AFeatures: TArray<TSKShaperFeature>);
begin
  Assert((Assigned(AFont)) and (Assigned(ABiDi)) and (Assigned(AScript)) and (Assigned(ALanguage)));
  sk4d_shaper_shape2(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), GetHandle(AFont), GetHandle(ABiDi), GetHandle(AScript), GetHandle(ALanguage), @sk_shaperfeature_t(AFeatures[0]), Length(AFeatures), AWidth, GetHandle(AHandler));
end;

{ TSKLattice }

class operator TSKLattice.Explicit(const ALattice: TSKLattice): sk_lattice_t;
begin
  Result.x_divs      := @ALattice.XDivs[0];
  Result.y_divs      := @ALattice.YDivs[0];
  Result.rect_types  := @ALattice.RectTypes[0];
  Result.x_count     := Length(ALattice.XDivs);
  Result.y_count     := Length(ALattice.YDivs);
  Result.bounds      := @sk_irect_t(ALattice.Bounds[0]);
  Result.colors      := @ALattice.Colors[0];
end;

{ TSKPaint }

constructor TSKPaint.Create(const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  CreateNative(sk4d_paint_create2(GetHandle(APaint)));
end;

constructor TSKPaint.Create;
begin
  CreateNative(sk4d_paint_create);
end;

class procedure TSKPaint.DoDestroy(const AHandle: THandle);
begin
  sk4d_paint_destroy(AHandle);
end;

function TSKPaint.GetAlpha: Byte;
begin
  Result := sk4d_paint_get_alpha(Handle);
end;

function TSKPaint.GetAlphaF: Single;
begin
  Result := sk4d_paint_get_alphaf(Handle);
end;

function TSKPaint.GetAntiAlias: Boolean;
begin
  Result := sk4d_paint_get_anti_alias(Handle);
end;

function TSKPaint.GetBlendMode: TSKBlendMode;
begin
  Result := TSKBlendMode(sk4d_paint_get_blend_mode(Handle));
end;

function TSKPaint.GetColor: TAlphaColor;
begin
  Result := sk4d_paint_get_color(Handle);
end;

function TSKPaint.GetColorF: TAlphaColorF;
begin
  sk4d_paint_get_colorf(Handle, sk_color4f_t(Result));
end;

function TSKPaint.GetColorFilter: ISKColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := sk4d_paint_get_color_filter(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKColorFilter.CreateNative(LHandle);
end;

function TSKPaint.GetDither: Boolean;
begin
  Result := sk4d_paint_get_dither(Handle);
end;

function TSKPaint.GetFillPath(const APath: ISKPath; const ACullRect: TRectF;
  const AResScale: Single): ISKPath;
begin
  Assert(Assigned(APath));
  Result := TSKPath.Create;
  if not sk4d_paint_get_fill_path(Handle, GetHandle(APath), @sk_rect_t(ACullRect), AResScale, GetHandle(Result)) then
    Result := nil;
end;

function TSKPaint.GetFillPath(const APath: ISKPath): ISKPath;
begin
  Assert(Assigned(APath));
  Result := TSKPath.Create;
  if not sk4d_paint_get_fill_path(Handle, GetHandle(APath), nil, 1, GetHandle(Result)) then
    Result := nil;
end;

function TSKPaint.GetImageFilter: ISKImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := sk4d_paint_get_image_filter(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKImageFilter.CreateNative(LHandle);
end;

function TSKPaint.GetMaskFilter: ISKMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := sk4d_paint_get_mask_filter(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKMaskFilter.CreateNative(LHandle);
end;

function TSKPaint.GetPathEffect: ISKPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := sk4d_paint_get_path_effect(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKPathEffect.CreateNative(LHandle);
end;

function TSKPaint.GetShader: ISKShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := sk4d_paint_get_shader(Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKShader.CreateNative(LHandle)
end;

function TSKPaint.GetStrokeCap: TSKStrokeCap;
begin
  Result := TSKStrokeCap(sk4d_paint_get_stroke_cap(Handle));
end;

function TSKPaint.GetStrokeJoin: TSKStrokeJoin;
begin
  Result := TSKStrokeJoin(sk4d_paint_get_stroke_join(Handle));
end;

function TSKPaint.GetStrokeMiter: Single;
begin
  Result := sk4d_paint_get_stroke_miter(Handle);
end;

function TSKPaint.GetStrokeWidth: Single;
begin
  Result := sk4d_paint_get_stroke_width(Handle);
end;

function TSKPaint.GetStyle: TSKPaintStyle;
begin
  Result := TSKPaintStyle(sk4d_paint_get_style(Handle))
end;

procedure TSKPaint.SetAlpha(const AValue: Byte);
begin
  sk4d_paint_set_alpha(Handle, AValue);
end;

procedure TSKPaint.SetAlphaF(const AValue: Single);
begin
  sk4d_paint_set_alphaf(Handle, AValue);
end;

procedure TSKPaint.SetAntiAlias(const AValue: Boolean);
begin
  sk4d_paint_set_antialias(Handle, AValue);
end;

procedure TSKPaint.SetARGB(const A, R, G, B: Byte);
begin
  sk4d_paint_set_argb(Handle, A, R, G, B);
end;

procedure TSKPaint.SetBlendMode(const AValue: TSKBlendMode);
begin
  sk4d_paint_set_blend_mode(Handle, sk_blendmode_t(AValue));
end;

procedure TSKPaint.SetColor(const AValue: TAlphaColor);
begin
  sk4d_paint_set_color(Handle, AValue);
end;

procedure TSKPaint.SetColorF(const AValue: TAlphaColorF;
  const AColorSpace: ISKColorSpace);
begin
  sk4d_paint_set_colorf(Handle, sk_color4f_t(AValue), GetHandle(AColorSpace));
end;

procedure TSKPaint.SetColorF(const AValue: TAlphaColorF);
begin
  SetColorF(AValue, nil);
end;

procedure TSKPaint.SetColorFilter(const AValue: ISKColorFilter);
begin
  sk4d_paint_set_color_filter(Handle, GetHandle(AValue));
end;

procedure TSKPaint.SetDither(const AValue: Boolean);
begin
  sk4d_paint_set_dither(Handle, AValue);
end;

procedure TSKPaint.SetImageFilter(const AValue: ISKImageFilter);
begin
  sk4d_paint_set_image_filter(Handle, GetHandle(AValue));
end;

procedure TSKPaint.SetMaskFilter(const AValue: ISKMaskFilter);
begin
  sk4d_paint_set_mask_filter(Handle, GetHandle(AValue));
end;

procedure TSKPaint.SetPathEffect(const AValue: ISKPathEffect);
begin
  sk4d_paint_set_path_effect(Handle, GetHandle(AValue));
end;

procedure TSKPaint.SetShader(const AValue: ISKShader);
begin
  sk4d_paint_set_shader(Handle, GetHandle(AValue));
end;

procedure TSKPaint.SetStrokeCap(const AValue: TSKStrokeCap);
begin
  sk4d_paint_set_stroke_cap(Handle, sk_strokecap_t(AValue));
end;

procedure TSKPaint.SetStrokeJoin(const AValue: TSKStrokeJoin);
begin
  sk4d_paint_set_stroke_join(Handle, sk_strokejoin_t(AValue));
end;

procedure TSKPaint.SetStrokeMiter(const AValue: Single);
begin
  sk4d_paint_set_stroke_miter(Handle, AValue);
end;

procedure TSKPaint.SetStrokeWidth(const AValue: Single);
begin
  sk4d_paint_set_stroke_width(Handle, AValue);
end;

procedure TSKPaint.SetStyle(const AValue: TSKPaintStyle);
begin
  sk4d_paint_set_style(Handle, sk_paintstyle_t(AValue));
end;

{ TSKCanvas }

procedure TSKCanvas.Clear(const AColor: TAlphaColorF);
begin
  sk4d_canvas_clear2(Handle, sk_color4f_t(AColor));
end;

procedure TSKCanvas.Clear(const AColor: TAlphaColor);
begin
  sk4d_canvas_clear(Handle, AColor);
end;

procedure TSKCanvas.ClipPath(const APath: ISKPath; const AOp: TSKClipOp;
  const DoAntiAlias: Boolean);
begin
  Assert(Assigned(APath));
  sk4d_canvas_clip_path(Handle, GetHandle(APath), sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSKCanvas.ClipRect(const ARect: TRectF; const AOp: TSKClipOp;
  const DoAntiAlias: Boolean);
begin
  sk4d_canvas_clip_rect(Handle, sk_rect_t(ARect), sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSKCanvas.ClipRegion(const ARegion: ISKRegion; const AOp: TSKClipOp);
begin
  Assert(Assigned(ARegion));
  sk4d_canvas_clip_region(Handle, GetHandle(ARegion), sk_clipop_t(AOp));
end;

procedure TSKCanvas.ClipRoundRect(const ARoundRect: ISKRoundRect;
  const AOp: TSKClipOp; const DoAntiAlias: Boolean);
begin
  Assert(Assigned(ARoundRect));
  sk4d_canvas_clip_rrect(Handle, GetHandle(ARoundRect), sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSKCanvas.ClipShader(const AShader: ISKShader; const AOp: TSKClipOp);
begin
  Assert(Assigned(AShader));
  sk4d_canvas_clip_shader(Handle, GetHandle(AShader), sk_clipop_t(AOp));
end;

procedure TSKCanvas.Concat(const AMatrix: TMatrix3D);
begin
  sk4d_canvas_concat(Handle, sk_matrix44_t(AMatrix));
end;

procedure TSKCanvas.Concat(const AMatrix: TMatrix);
begin
  sk4d_canvas_concat2(Handle, sk_matrix_t(AMatrix));
end;

procedure TSKCanvas.Discard;
begin
  sk4d_canvas_discard(Handle);
end;

class procedure TSKCanvas.DoDestroy(const AHandle: THandle);
begin
  sk4d_canvas_destroy(AHandle);
end;

procedure TSKCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string;
  const AValue: ISKData);
begin
  sk4d_canvas_draw_annotation(Handle, sk_rect_t(ARect), MarshaledAString(UTF8String(AKey)), GetHandle(AValue));
end;

procedure TSKCanvas.DrawArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_arc(Handle, sk_rect_t(AOval), AStartAngle, ASweepAngle, AUseCenter, GetHandle(APaint));
end;

procedure TSKCanvas.DrawAtlas(const AAtlas: ISKImage;
  const ATansforms: TArray<TSKRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode;
  const ASampling: TSKSamplingOptions; const ACullRect: TRectF;
  const AColors: TArray<TAlphaColor>; const APaint: ISKPaint);
begin
  Assert((Length(ATansforms) = Length(ASprites)) and ((not Assigned(AColors)) or (Length(AColors) = Length(ASprites))));
  sk4d_canvas_draw_atlas(Handle, GetHandle(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), sk_samplingoptions_t(ASampling), @sk_rect_t(ACullRect), GetHandle(APaint));
end;

procedure TSKCanvas.DrawAtlas(const AAtlas: ISKImage;
  const ATansforms: TArray<TSKRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSKBlendMode;
  const ASampling: TSKSamplingOptions; const AColors: TArray<TAlphaColor>;
  const APaint: ISKPaint);
begin
  Assert((Length(ATansforms) = Length(ASprites)) and ((not Assigned(AColors)) or (Length(AColors) = Length(ASprites))));
  sk4d_canvas_draw_atlas(Handle, GetHandle(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), sk_samplingoptions_t(ASampling), nil, GetHandle(APaint));
end;

procedure TSKCanvas.DrawCircle(const ACenterX, ACenterY, ARadius: Single;
  const APaint: ISKPaint);
begin
  DrawCircle(TPointF.Create(ACenterX, ACenterY), ARadius, APaint);
end;

procedure TSKCanvas.DrawCircle(const ACenter: TPointF; ARadius: Single;
  const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_circle(Handle, sk_point_t(ACenter), ARadius, GetHandle(APaint));
end;

procedure TSKCanvas.DrawColor(const AColor: TAlphaColor;
  const ABlendMode: TSKBlendMode);
begin
  sk4d_canvas_draw_color(Handle, AColor, sk_blendmode_t(ABlendMode));
end;

procedure TSKCanvas.DrawColor(const AColor: TAlphaColorF;
  const ABlendMode: TSKBlendMode);
begin
  sk4d_canvas_draw_color2(Handle, sk_color4f_t(AColor), sk_blendmode_t(ABlendMode));
end;

procedure TSKCanvas.DrawGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TSKRotationScaleMatrix>; const AOrigin: TPointF;
  const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert(Assigned(AFont) and Assigned(APaint) and (Length(AGlyphs) = Length(APositions)));
  sk4d_canvas_draw_glyphs2(Handle, Length(AGlyphs), @AGlyphs[0], @sk_rotationscalematrix_t(APositions[0]), sk_point_t(AOrigin), GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AOrigin: TPointF;
  const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert(Assigned(AFont) and Assigned(APaint) and (Length(AGlyphs) = Length(APositions)));
  sk4d_canvas_draw_glyphs(Handle, Length(AGlyphs), @AGlyphs[0], @sk_point_t(APositions[0]), sk_point_t(AOrigin), GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawImage(const AImage: ISKImage; const AX, AY: Single;
  const ASampling: TSKSamplingOptions; const APaint: ISKPaint);
begin
  Assert(Assigned(AImage));
  sk4d_canvas_draw_image(Handle, GetHandle(AImage), AX, AY, sk_samplingoptions_t(ASampling), GetHandle(APaint));
end;

procedure TSKCanvas.DrawImage(const AImage: ISKImage; const AX, AY: Single;
  const APaint: ISKPaint);
begin
  DrawImage(AImage, AX, AY, TSKSamplingOptions.Create(TSKFilterMode.Nearest, TSKMipmapMode.None), APaint);
end;

procedure TSKCanvas.DrawImageLattice(const AImage: ISKImage;
  const ALattice: TSKLattice; const ADest: TRectF;
  const AFilterMode: TSKFilterMode; const APaint: ISKPaint);
begin
  Assert(Assigned(AImage));
  sk4d_canvas_draw_image_lattice(Handle, GetHandle(AImage), sk_lattice_t(ALattice), sk_rect_t(ADest), sk_filtermode_t(AFilterMode), GetHandle(APaint));
end;

procedure TSKCanvas.DrawImageNine(const AImage: ISKImage; const ACenter: TRect;
  const ADest: TRectF; const AFilterMode: TSKFilterMode;
  const APaint: ISKPaint);
begin
  Assert(Assigned(AImage));
  sk4d_canvas_draw_image_nine(Handle, GetHandle(AImage), sk_irect_t(ACenter), sk_rect_t(ADest), sk_filtermode_t(AFilterMode), GetHandle(APaint));
end;

procedure TSKCanvas.DrawImageRect(const AImage: ISKImage; const ADest: TRectF;
  const ASampling: TSKSamplingOptions; const APaint: ISKPaint;
  const AConstraint: TSKSrcRectConstraint);
begin
  DrawImageRect(AImage, TRectF.Create(0, 0, AImage.Width, AImage.Height), ADest, ASampling, APaint, AConstraint);
end;

procedure TSKCanvas.DrawImageRect(const AImage: ISKImage; const ASrc,
  ADest: TRectF; const ASampling: TSKSamplingOptions; const APaint: ISKPaint;
  const AConstraint: TSKSrcRectConstraint);
begin
  Assert(Assigned(AImage));
  sk4d_canvas_draw_image_rect(Handle, GetHandle(AImage), sk_rect_t(ASrc), sk_rect_t(ADest), sk_samplingoptions_t(ASampling), GetHandle(APaint), sk_srcrectconstraint_t(AConstraint));
end;

procedure TSKCanvas.DrawLine(const AX1, AY1, AX2, AY2: Single;
  const APaint: ISKPaint);
begin
  DrawLine(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), APaint);
end;

procedure TSKCanvas.DrawLine(const APoint1, APoint2: TPointF;
  const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_line(Handle, sk_point_t(APoint1), sk_point_t(APoint2), GetHandle(APaint));
end;

procedure TSKCanvas.DrawOval(const AOval: TRectF; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_oval(Handle, sk_rect_t(AOval), GetHandle(APaint));
end;

procedure TSKCanvas.DrawPaint(const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_paint(Handle, GetHandle(APaint));
end;

procedure TSKCanvas.DrawPatch(const ACubics: TSKPatchCubics;
  const AColors: TSKPatchColors; const ATexCoords: TSKPatchTexCoords;
  const APaint: ISKPaint; const ABlendMode: TSKBlendMode);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_patch(Handle, @sk_point_t(ACubics[0]), @AColors[0], @sk_point_t(ATexCoords[0]), sk_blendmode_t(ABlendMode), GetHandle(APaint));
end;

procedure TSKCanvas.DrawPath(const APath: ISKPath; const APaint: ISKPaint);
begin
  Assert((Assigned(APath)) and (Assigned(APaint)));
  sk4d_canvas_draw_path(Handle, GetHandle(APath), GetHandle(APaint));
end;

procedure TSKCanvas.DrawPicture(const APicture: ISKPicture;
  const AMatrix: TMatrix; const APaint: ISKPaint);
begin
  sk4d_canvas_draw_picture2(Handle, GetHandle(APicture), @sk_matrix_t(AMatrix), GetHandle(APaint));
end;

procedure TSKCanvas.DrawPicture(const APicture: ISKPicture);
begin
  sk4d_canvas_draw_picture(Handle, GetHandle(APicture));
end;

procedure TSKCanvas.DrawPoint(const AX, AY: Single; const APaint: ISKPaint);
begin
  DrawPoint(TPointF.Create(AX, AY), APaint);
end;

procedure TSKCanvas.DrawPoint(const APoint: TPointF; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_point(Handle, sk_point_t(APoint), GetHandle(APaint));
end;

procedure TSKCanvas.DrawPoints(const AMode: TSKDrawPointsMode;
  const APoints: TArray<TPointF>; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_points(Handle, sk_drawpointsmode_t(AMode), Length(APoints), @sk_point_t(APoints[0]), GetHandle(APaint));
end;

procedure TSKCanvas.DrawRect(const ARect: TRectF; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_rect(Handle, sk_rect_t(ARect), GetHandle(APaint));
end;

procedure TSKCanvas.DrawRegion(const ARegion: ISKRegion;
  const APaint: ISKPaint);
begin
  sk4d_canvas_draw_region(Handle, GetHandle(ARegion), GetHandle(APaint));
end;

procedure TSKCanvas.DrawRoundRect(const ARoundRect: ISKRoundRect;
  const APaint: ISKPaint);
begin
  Assert((Assigned(ARoundRect)) and (Assigned(APaint)));
  sk4d_canvas_draw_rrect(Handle, GetHandle(ARoundRect), GetHandle(APaint));
end;

procedure TSKCanvas.DrawRoundRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_rrect2(Handle, sk_rect_t(ARect), ARadiusX, ARadiusY, GetHandle(APaint));
end;

procedure TSKCanvas.DrawRoundRectDifference(const AOuter, AInner: ISKRoundRect;
  const APaint: ISKPaint);
begin
  Assert((Assigned(AOuter)) and (Assigned(AInner)) and (Assigned(APaint)));
  sk4d_canvas_draw_rrect_difference(Handle, GetHandle(AOuter), GetHandle(AInner), GetHandle(APaint));
end;

procedure TSKCanvas.DrawSimpleText(const AText: string; const AX, AY: Single;
  const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert((Assigned(AFont)) and (Assigned(APaint)));
  sk4d_canvas_draw_simple_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(Char), UTF16_SK_TEXTENCODING, AX, AY, GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawSimpleText(const AText: UCS4String; const AX,
  AY: Single; const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert((Assigned(AFont)) and (Assigned(APaint)));
  sk4d_canvas_draw_simple_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UCS4Char), UTF32_SK_TEXTENCODING, AX, AY, GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawSimpleText(const AText: UTF8String; const AX,
  AY: Single; const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert((Assigned(AFont)) and (Assigned(APaint)));
  sk4d_canvas_draw_simple_text(Handle, @AText[Low(AText)], Length(AText) * SizeOf(UTF8Char), UTF8_SK_TEXTENCODING, AX, AY, GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX,
  AY: Single; const AFont: ISKFont; const APaint: ISKPaint);
begin
  Assert((Assigned(AFont)) and (Assigned(APaint)));
  sk4d_canvas_draw_simple_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, AX, AY, GetHandle(AFont), GetHandle(APaint));
end;

procedure TSKCanvas.DrawTextBlob(const ATextBlob: ISKTextBlob; const AX,
  AY: Single; const APaint: ISKPaint);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_text_blob(Handle, GetHandle(ATextBlob), AX, AY, GetHandle(APaint));
end;

procedure TSKCanvas.DrawVertices(const AVertices: ISKVertices;
  const APaint: ISKPaint; const ABlendMode: TSKBlendMode);
begin
  Assert(Assigned(APaint));
  sk4d_canvas_draw_vertices(Handle, GetHandle(AVertices), sk_blendmode_t(ABlendMode), GetHandle(APaint));
end;

function TSKCanvas.FindMarkedCTM(const AName: string;
  out AMatrix: TMatrix3D): Boolean;
begin
  Result := sk4d_canvas_find_marked_ctm(Handle, MarshaledAString(UTF8String(AName)), sk_matrix44_t(AMatrix));
end;

function TSKCanvas.GetDeviceClipBounds: TRect;
begin
  sk4d_canvas_get_device_clip_bounds(Handle, sk_irect_t(Result));
end;

function TSKCanvas.GetLocalClipBounds: TRectF;
begin
  sk4d_canvas_get_local_clip_bounds(Handle, sk_rect_t(Result));
end;

function TSKCanvas.GetLocalToDevice: TMatrix3D;
begin
  sk4d_canvas_get_local_to_device(Handle, sk_matrix44_t(Result));
end;

function TSKCanvas.GetLocalToDeviceAsMatrix: TMatrix;
begin
  sk4d_canvas_get_local_to_device_as_matrix(Handle, sk_matrix_t(Result));
end;

function TSKCanvas.GetSaveCount: Integer;
begin
  Result := sk4d_canvas_get_save_count(Handle);
end;

function TSKCanvas.IsClipEmpty: Boolean;
begin
  Result := sk4d_canvas_is_clip_empty(Handle);
end;

function TSKCanvas.IsClipRect: Boolean;
begin
  Result := sk4d_canvas_is_clip_rect(Handle);
end;

procedure TSKCanvas.MarkCTM(const AName: string);
begin
  sk4d_canvas_mark_ctm(Handle, MarshaledAString(UTF8String(AName)));
end;

function TSKCanvas.QuickReject(const ARect: TRectF): Boolean;
begin
  Result := sk4d_canvas_quick_reject(Handle, sk_rect_t(ARect));
end;

function TSKCanvas.QuickReject(const APath: ISKPath): Boolean;
begin
  Assert(Assigned(APath));
  Result := sk4d_canvas_quick_reject2(Handle, GetHandle(APath));
end;

procedure TSKCanvas.ResetMatrix;
begin
  sk4d_canvas_reset_matrix(Handle);
end;

procedure TSKCanvas.Restore;
begin
  sk4d_canvas_restore(Handle);
end;

procedure TSKCanvas.RestoreToCount(const ASaveCount: Integer);
begin
  sk4d_canvas_restore_to_count(Handle, ASaveCount);
end;

procedure TSKCanvas.Rotate(const ADegrees: Single);
begin
  sk4d_canvas_rotate(Handle, ADegrees);
end;

procedure TSKCanvas.Rotate(const ADegrees, APX, APY: Single);
begin
  sk4d_canvas_rotate2(Handle, ADegrees, APX, APY);
end;

procedure TSKCanvas.Save;
begin
  sk4d_canvas_save(Handle);
end;

procedure TSKCanvas.SaveLayer(const APaint: ISKPaint);
begin
  sk4d_canvas_save_layer(Handle, nil, GetHandle(APaint));
end;

procedure TSKCanvas.SaveLayer(const ABounds: TRectF; const APaint: ISKPaint);
begin
  sk4d_canvas_save_layer(Handle, @sk_rect_t(ABounds), GetHandle(APaint));
end;

procedure TSKCanvas.SaveLayerAlpha(const AAlpha: Byte);
begin
  sk4d_canvas_save_layer_alpha(Handle, nil, AAlpha);
end;

procedure TSKCanvas.SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte);
begin
  sk4d_canvas_save_layer_alpha(Handle, @sk_rect_t(ABounds), AAlpha);
end;

procedure TSKCanvas.Scale(const SX, SY: Single);
begin
  sk4d_canvas_scale(Handle, SX, SY);
end;

procedure TSKCanvas.SetMatrix(const AMatrix: TMatrix);
begin
  sk4d_canvas_set_matrix2(Handle, sk_matrix_t(AMatrix));
end;

procedure TSKCanvas.SetMatrix(const AMatrix: TMatrix3D);
begin
  sk4d_canvas_set_matrix(Handle, sk_matrix44_t(AMatrix));
end;

procedure TSKCanvas.Skew(const AKX, AKY: Single);
begin
  sk4d_canvas_skew(Handle, AKX, AKY);
end;

procedure TSKCanvas.Translate(const DX, DY: Single);
begin
  sk4d_canvas_translate(Handle, DX, DY);
end;

{ TSKDateTime }

constructor TSKDateTime.Create(const ATimeZoneMinutes: SmallInt;
  const AYear: Word; const AMonth, ADayOfWeek, ADay, AHour, AMinute,
  ASecond: Byte);
begin
  TimeZoneMinutes := ATimeZoneMinutes;
  Year            := AYear;
  Month           := AMonth;
  DayOfWeek       := ADayOfWeek;
  Day             := ADay;
  Hour            := AHour;
  Minute          := AMinute;
  Second          := ASecond;
end;

class operator TSKDateTime.Implicit(const ADateTime: TDateTime): TSKDateTime;
var
  LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond: Word;
begin
  DecodeDateTime(ADateTime, Result.Year, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond);
  Result.Month           := LMonth;
  Result.Day             := LDay;
  Result.Hour            := LHour;
  Result.Minute          := LMinute;
  Result.Second          := LSecond;
  Result.TimeZoneMinutes := Round(TTimeZone.Local.UtcOffset.TotalMinutes);
end;

class operator TSKDateTime.Implicit(const ADateTime: TSKDateTime): TDateTime;
begin
  Result := EncodeDateTime(ADateTime.Year, ADateTime.Month, ADateTime.Day, ADateTime.Hour, ADateTime.Minute, ADateTime.Second, 0) + TTimeSpan.Create(0, ADateTime.TimeZoneMinutes, 0);
end;

function TSKDateTime.ToISO8601: ISKString;
begin
  Result := TSKString.Create;
  sk4d_datetime_to_iso8601(@Self, TSkiaObject.GetHandle(Result));
end;

{ TSKPDFMetadata }

constructor TSKPDFMetadata.Create(const ATitle, AAuthor, ASubject, AKeywords,
  ACreator: ISKString);
begin
  Title    := ATitle;
  Author   := AAuthor;
  Subject  := ASubject;
  Keywords := AKeywords;
  Creator  := ACreator;
end;

class operator TSKPDFMetadata.Explicit(
  const APDFMetadata: TSKPDFMetadata): sk_pdfmetadata_t;
begin
  Result.title            := TSkiaObject.GetHandle(APDFMetadata.Title);
  Result.author           := TSkiaObject.GetHandle(APDFMetadata.Author);
  Result.subject          := TSkiaObject.GetHandle(APDFMetadata.Subject);
  Result.keywords         := TSkiaObject.GetHandle(APDFMetadata.Keywords);
  Result.creator          := TSkiaObject.GetHandle(APDFMetadata.Creator);
  Result.producer         := TSkiaObject.GetHandle(APDFMetadata.Producer);
  Result.creation         := sk_datetime_t(APDFMetadata.Creation);
  Result.modified         := sk_datetime_t(APDFMetadata.Modified);
  Result.raster_dpi       := APDFMetadata.RasterDPI;
  Result.pdfa             := APDFMetadata.PDFA;
  Result.encoding_quality := APDFMetadata.EncodingQuality;
end;

{ TSKDocument }

function TSKDocument.BeginPage(const AWidth, AHeight: Single): ISKCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := sk4d_document_begin_page(Handle, AWidth, AHeight, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCanvas.CreateNative(LHandle, False);
end;

function TSKDocument.BeginPage(const AWidth, AHeight: Single;
  const AContent: TRectF): ISKCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := sk4d_document_begin_page(Handle, AWidth, AHeight, @sk_rect_t(AContent));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCanvas.CreateNative(LHandle, False);
end;

procedure TSKDocument.Close;
begin
  sk4d_document_close(Handle);
end;

procedure TSKDocument.EndPage;
begin
  sk4d_document_end_page(Handle);
end;

class function TSKDocument.MakePDF(const AWStream: ISKWStream;
  const AMetadata: TSKPDFMetadata): ISKDocument;
var
  LHandle: sk_document_t;
begin
  LHandle := sk4d_document_make_pdf2(GetHandle(AWStream), sk_pdfmetadata_t(AMetadata));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKDocument.CreateNative(LHandle);
end;

class function TSKDocument.MakePDF(const AWStream: ISKWStream): ISKDocument;
var
  LHandle: sk_document_t;
begin
  LHandle := sk4d_document_make_pdf(GetHandle(AWStream));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKDocument.CreateNative(LHandle);
end;

procedure TSKDocument.Terminate;
begin
  sk4d_document_terminate(Handle);
end;

{ TSKSVGCanvas }

class function TSKSVGCanvas.Make(const ABounds: TRectF;
  const AWStream: ISKWStream): ISKCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := sk4d_svgcanvas_make(sk_rect_t(ABounds), TSkiaObject.GetHandle(AWStream));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCanvas.CreateNative(LHandle);
end;

{ TSKGraphics }

class procedure TSKGraphics.AllowJIT;
begin
  sk4d_graphics_allow_jit;
end;

class procedure TSKGraphics.DumpMemoryStatistics(
  const ATraceMemoryDump: ISKTraceMemoryDump);
begin
  sk4d_graphics_dump_memory_statistics(TSkiaObject.GetHandle(ATraceMemoryDump));
end;

class function TSKGraphics.GetFontCacheCountLimit: Integer;
begin
  Result := sk4d_graphics_get_font_cache_count_limit;
end;

class function TSKGraphics.GetFontCacheCountUsed: Integer;
begin
  Result := sk4d_graphics_get_font_cache_count_used;
end;

class function TSKGraphics.GetFontCacheLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_font_cache_limit;
end;

class function TSKGraphics.GetFontCacheUsed: NativeUInt;
begin
  Result := sk4d_graphics_get_font_cache_used;
end;

class function TSKGraphics.GetResourceCacheSingleAllocationByteLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_single_allocation_byte_limit;
end;

class function TSKGraphics.GetResourceCacheTotalByteLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_total_byte_limit;
end;

class function TSKGraphics.GetResourceCacheTotalBytesUsed: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_total_bytes_used;
end;

class procedure TSKGraphics.Init;
begin
  sk4d_graphics_init;
end;

class procedure TSKGraphics.PurgeAllCaches;
begin
  sk4d_graphics_purge_all_caches;
end;

class procedure TSKGraphics.PurgeFontCache;
begin
  sk4d_graphics_purge_font_cache;
end;

class procedure TSKGraphics.PurgeResourceCache;
begin
  sk4d_graphics_purge_resource_cache;
end;

class procedure TSKGraphics.SetFontCacheCountLimit(const AValue: Integer);
begin
  sk4d_graphics_set_font_cache_count_limit(AValue);
end;

class procedure TSKGraphics.SetFontCacheLimit(const AValue: NativeUInt);
begin
  sk4d_graphics_set_font_cache_limit(AValue);
end;

class procedure TSKGraphics.SetResourceCacheSingleAllocationByteLimit(
  const AValue: NativeUInt);
begin
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit(AValue);
end;

class procedure TSKGraphics.SetResourceCacheTotalByteLimit(
  const AValue: NativeUInt);
begin
  sk4d_graphics_set_resource_cache_total_byte_limit(AValue);
end;

{ TSKCodec }

class procedure TSKCodec.DoDestroy(const AHandle: THandle);
begin
  sk4d_codec_destroy(AHandle);
end;

function TSKCodec.GetInfo: TSKImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_codec_get_info(Handle, LResult);
  Result := TSKImageInfo(LResult);
end;

function TSKCodec.GetPixels(const ADest: ISKPixmap): Boolean;
begin
  Assert(Assigned(ADest));
  Result := sk4d_codec_get_pixels(Handle, GetHandle(ADest));
end;

function TSKCodec.GetPixels(const ADestImageInfo: TSKImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean;
var
  LPixmap: ISKPixmap;
begin
  LPixmap := TSKPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := GetPixels(LPixmap);
end;

class function TSKCodec.MakeFromData(const AData: ISKData): ISKCodec;
var
  LHandle: sk_codec_t;
begin
  LHandle := sk4d_codec_make_from_data(GetHandle(AData));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKCodec.CreateNative(LHandle);
end;

class function TSKCodec.MakeFromFile(const AFileName: string): ISKCodec;
var
  LFileStream: ISKFileStream;
begin
  LFileStream := TSKFileStream.Create(AFileName);
  Result      := MakeFromStream(LFileStream);
end;

class function TSKCodec.MakeFromStream(const AStream: ISKStream): ISKCodec;
var
  LData: ISKData;
begin
  LData  := TSKData.MakeFromStream(AStream, AStream.Length - AStream.Position);
  Result := MakeFromData(LData);
end;

{ TSKSVGSVG }

function TSKSVGSVG.GetIntrinsicSize(const AViewPort: TSizeF;
  const ADPI: Single): TSizeF;
begin
  sk_svgsvg_get_intrinsic_size(Handle, sk_size_t(AViewPort), ADPI, sk_size_t(Result));
end;

{ TSKSVGDOM }

function TSKSVGDOM.GetRoot: ISKSVGSVG;
var
  LHandle: sk_svgsvg_t;
begin
  if not Assigned(FRoot) then
  begin
    LHandle := sk4d_svgdom_get_root(Handle);
    if LHandle <> 0 then
      FRoot := TSKSVGSVG.CreateNative(LHandle, False);
  end;
  Result := FRoot;
end;

class function TSKSVGDOM.Make(const AStream: ISKStream): ISKSVGDOM;
var
  LHandle: sk_svgdom_t;
begin
  Assert(Assigned(AStream));
  LHandle := sk4d_svgdom_make(GetHandle(AStream));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSVGDOM.CreateNative(LHandle);
end;

procedure TSKSVGDOM.Render(const ACanvas: ISKCanvas);
begin
  sk4d_svgdom_render(Handle, GetHandle(ACanvas));
end;

procedure TSKSVGDOM.SetContainerSize(const AValue: TSizeF);
begin
  sk4d_svgdom_set_container_size(Handle, sk_size_t(AValue));
end;

{ TSKSkottieAnimation }

class procedure TSKSkottieAnimation.DoRef(const AHandle: THandle);
begin
  sk4d_skottieanimation_ref(AHandle);
end;

class procedure TSKSkottieAnimation.DoUnref(const AHandle: THandle);
begin
  sk4d_skottieanimation_unref(AHandle);
end;

function TSKSkottieAnimation.GetDuration: Double;
begin
  Result := sk4d_skottieanimation_get_duration(Handle);
end;

function TSKSkottieAnimation.GetFPS: Double;
begin
  Result := sk4d_skottieanimation_get_fps(Handle);
end;

function TSKSkottieAnimation.GetInPoint: Double;
begin
  Result := sk4d_skottieanimation_get_in_point(Handle);
end;

function TSKSkottieAnimation.GetOutPoint: Double;
begin
  Result := sk4d_skottieanimation_get_out_point(Handle);
end;

function TSKSkottieAnimation.GetSize: TSizeF;
begin
  sk4d_skottieanimation_get_size(Handle, sk_size_t(Result));
end;

function TSKSkottieAnimation.GetVersion: ISKString;
begin
  Result := TSKString.CreateNative(sk4d_skottieanimation_get_version(Handle), False);
end;

class function TSKSkottieAnimation.Make(
  const AData: string): ISKSkottieAnimation;
var
  LHandle: sk_skottieanimation_t;
begin
  LHandle := sk4d_skottieanimation_make(MarshaledAString(UTF8String(AData)), Length(AData));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSkottieAnimation.CreateNative(LHandle);
end;

class function TSKSkottieAnimation.MakeFromFile(
  const AFileName: string): ISKSkottieAnimation;
var
  LHandle: sk_skottieanimation_t;
begin
  LHandle := sk4d_skottieanimation_make_from_file(MarshaledAString(UTF8String(AFileName)));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSkottieAnimation.CreateNative(LHandle);
end;

class function TSKSkottieAnimation.MakeFromStream(
  const AStream: ISKStream): ISKSkottieAnimation;
var
  LHandle: sk_skottieanimation_t;
begin
  LHandle := sk4d_skottieanimation_make_from_stream(GetHandle(AStream));
  if LHandle = 0 then
    Exit(nil);
  Result := TSKSkottieAnimation.CreateNative(LHandle);
end;

procedure TSKSkottieAnimation.Render(const ACanvas: ISKCanvas;
  const ADest: TRectF; const ARenderFlags: TSKSkottieAnimationRenderFlags);
begin
  sk4d_skottieanimation_render(Handle, GetHandle(ACanvas), @sk_rect_t(ADest), Byte(ARenderFlags));
end;

procedure TSKSkottieAnimation.Render(const ACanvas: ISKCanvas;
  const ARenderFlags: TSKSkottieAnimationRenderFlags);
begin
  sk4d_skottieanimation_render(Handle, GetHandle(ACanvas), nil, Byte(ARenderFlags));
end;

procedure TSKSkottieAnimation.SeekFrame(const ATick: Double);
begin
  sk4d_skottieanimation_seek_frame(Handle, ATick);
end;

procedure TSKSkottieAnimation.SeekFrameTime(const ATick: Double);
begin
  sk4d_skottieanimation_seek_frame_time(Handle, ATick);
end;

end.
