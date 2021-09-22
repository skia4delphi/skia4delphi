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

{$ALIGN ON}
{$MINENUMSIZE 4}
{$SCOPEDENUMS ON}

{$IF defined(MACOS) and (CompilerVersion >= 34)}
  {$DEFINE SK_METAL}
{$ENDIF}

uses
  { Delphi }

{$IFDEF SK_METAL}
  Macapi.Helpers,
  Macapi.Metal,
  Macapi.MetalKit,
  Macapi.ObjectiveC,
  {$IF defined(IOS)}
  iOSapi.Foundation,
  {$ELSE}
  Macapi.Foundation,
  {$ENDIF}
{$ENDIF}
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.Math.Vectors,
  System.SysUtils,
  System.Types,
  System.UITypes,

  { Skia }

  Skia.Api;

type
  SkException             = class(Exception);
  EGRDirectContext        = class(SkException);
  ESkCanvas               = class(SkException);
  ESkColorFilter          = class(SkException);
  ESkColorSpace           = class(SkException);
  ESkColorSpaceICCProfile = class(SkException);
  ESkEnumerable           = class(SkException);
  ESkFont                 = class(SkException);
  ESkImage                = class(SkException);
  ESkImageFilter          = class(SkException);
  ESkObject               = class(SkException);
  ESkOpBuilder            = class(SkException);
  ESkottieAnimation       = class(SkException);
  ESkPaint                = class(SkException);
  ESkParagraphBuilder     = class(SkException);
  ESkParagraphStyle       = class(SkException);
  ESkPath                 = class(SkException);
  ESkPathBuilder          = class(SkException);
  ESkPathEffect           = class(SkException);
  ESkPathMeasure          = class(SkException);
  ESkPixmap               = class(SkException);
  ESkRegion               = class(SkException);
  ESkRoundRect            = class(SkException);
  ESkRunBuffer            = class(SkException);
  ESkRuntimeEffect        = class(SkException);
  ESkShader               = class(SkException);
  ESkShaper               = class(SkException);
  ESkSkParagraph          = class(SkException);
  ESkStrutStyle           = class(SkException);
  ESkSurface              = class(SkException);
  ESkSurfaceProperties    = class(SkException);
  ESkSVGDOM               = class(SkException);
  ESkTextBlob             = class(SkException);
  ESkTextBlobBuilder      = class(SkException);
  ESkTextStyle            = class(SkException);
  ESkTypeface             = class(SkException);
  ESkVertices             = class(SkException);

  { TSkVersion }

  TSkVersion = record
  strict private
    class function GetMajorVersion: Integer; static;
    class function GetMinorVersion: Integer; static;
    class function GetMilestone: Integer; static;
  public
    class property MajorVersion: Integer read GetMajorVersion;
    class property MinorVersion: Integer read GetMinorVersion;
    class property Milestone: Integer read GetMilestone;
  end;

  TSkDebugMessageProc = reference to procedure (const AMessage: string);

  { TSkDebug }

  TSkDebug = class sealed
  strict private class var
    FDebugMessageProc: TSkDebugMessageProc;
  strict private
    class constructor Create;
    class destructor Destroy;
    class procedure debug_msg_proc(const msg: MarshaledAString); cdecl; static;
  public
    class property DebugMessageProc: TSkDebugMessageProc read FDebugMessageProc write FDebugMessageProc;
  end;

  { ISkNativeObject }

  ISkNativeObject = interface
    ['{94760A93-00FC-4D1A-B610-185E5A969EA3}']
    function GetHandle: THandle;
    property Handle: THandle read GetHandle;
  end;

  { TSkNativeObject }

  TSkNativeObject = class abstract(TInterfacedObject, ISkNativeObject)
  strict private
    FHandle: THandle;
  protected
    class function SafeHandle(const AObject: ISkNativeObject): THandle; static; inline;
  strict protected
    constructor CreateNative(const AHandle: THandle);
    procedure Dispose; virtual; abstract;
    function GetHandle: THandle; inline;
  public
    procedure BeforeDestruction; override;
  end;

  { ISkObject }

  ISkObject = interface(ISkNativeObject)
    ['{79EF72CC-2693-4588-829F-B507DA6D0F83}']
    function GetOwnsHandle: Boolean;
    property OwnsHandle: Boolean read GetOwnsHandle;
  end;

  { TSkObject }

  TSkObject = class abstract(TSkNativeObject, ISkObject)
  strict private
    FOwnsHandle: Boolean;
  protected
    constructor CreateNative(const AHandle: THandle; const AOwnsHandle: Boolean = True);
  strict protected
    procedure Dispose; override; final;
    function GetOwnsHandle: Boolean;
    class procedure DoDestroy(const AHandle: THandle); virtual;
  end;

  { ISkReferenceCounted }

  ISkReferenceCounted = interface(ISkNativeObject)
    ['{590A0172-EB2C-4762-BAA4-BDE1594A66C6}']
    procedure DecrementReference;
    procedure IncrementReference;
    function IsUnique: Boolean;
  end;

  { TSkReferenceCounted }

  TSkReferenceCounted = class abstract(TSkNativeObject, ISkReferenceCounted)
  protected
    constructor CreateNative(const AHandle: THandle; const AAlreadyReferenced: Boolean = True);
  strict protected
    procedure DecrementReference;
    procedure Dispose; override; final;
    procedure IncrementReference;
    function IsUnique: Boolean;
    class function DoIsUnique(const AHandle: THandle): Boolean; virtual;
    class procedure DoRef(const AHandle: THandle); virtual;
    class procedure DoUnref(const AHandle: THandle); virtual;
  end;

  TSkEnumerable<T> = class;

  { TSkEnumerator<T> }

  TSkEnumerator<T> = class(TEnumerator<T>)
  strict private
    FEnumerable: TSkEnumerable<T>;
  strict protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AEnumerable: TSkEnumerable<T>);
  end;

  { ISkEnumerable<T> }

  ISkEnumerable<T> = interface(ISkObject)
    ['{A0D2696B-E080-4F15-8B60-45245B0D0D7B}']
    function GetEnumerator: TSkEnumerator<T>;
  end;

  { TSkEnumerable<T> }

  TSkEnumerable<T> = class abstract(TSkObject, ISkEnumerable<T>)
  strict private
    FRun: Boolean;
  protected
    function GetCurrent: T; virtual; abstract;
    function GetEnumerator: TSkEnumerator<T>;
    function MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual;
  end;

  { TSkColorSpaceXYZ }

  TSkColorSpaceXYZ = record
    procedure Concat(const AXYZ: TSkColorSpaceXYZ);
    function Invert(out AXYZ: TSkColorSpaceXYZ): Boolean;
    class function AdobeRGB: TSkColorSpaceXYZ; static;
    class function DisplayP3: TSkColorSpaceXYZ; static;
    class function Rec2020: TSkColorSpaceXYZ; static;
    class function SRGB: TSkColorSpaceXYZ; static;
    class function XYZ: TSkColorSpaceXYZ; static;
    case Integer of
      0: (Values: array[0..2] of array[0..2] of Single);
      1: (M11, M12, M13, M21, M22, M23, M31, M32, M33: Single);
  end;

  { TSkColorSpacePrimaries }

  TSkColorSpacePrimaries = record
    function ToXYZ(out AXYZ: TSkColorSpaceXYZ): Boolean;
    case Integer of
      0: (Values: array[0..7] of Single);
      1: (RX, RY, GX, GY, BX, BY, WX, WY: Single);
  end;

  { TSkColorSpaceTransferFunction }

  TSkColorSpaceTransferFunction = record
    function Invert(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function Transform(const AX: SIngle): Single;
    class function HLG: TSkColorSpaceTransferFunction; static;
    class function Linear: TSkColorSpaceTransferFunction; static;
    class function PQ: TSkColorSpaceTransferFunction; static;
    class function Rec2020: TSkColorSpaceTransferFunction; static;
    class function SRGB: TSkColorSpaceTransferFunction; static;
    class function TwoDotTwo: TSkColorSpaceTransferFunction; static;
    case Integer of
      0: (Values: array[0..6] of Single);
      1: (G, A, B, C, D, E, F: Single);
  end;

  { ISkColorSpaceICCProfile }

  ISkColorSpaceICCProfile = interface(ISkObject)
    ['{ABB58E4D-9A61-4D6A-A15C-081BCFF18A72}']
    function GetBufferSize: Cardinal;
    function ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
    property BufferSize: Cardinal read GetBufferSize;
  end;

  { TSkColorSpaceICCProfile }

  TSkColorSpaceICCProfile = class(TSkObject, ISkColorSpaceICCProfile)
  protected
    function GetBuffer: Pointer;
  strict protected
    function GetBufferSize: Cardinal;
    procedure ReadBuffer(out ADest; const ACount: Cardinal);
    function ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    class function Make(const AInput: TBytes): ISkColorSpaceICCProfile; overload; static;
    class function Make(const AInput; const ASize: NativeUInt): ISkColorSpaceICCProfile; overload; static;
  end;

  { ISkColorSpace }

  ISkColorSpace = interface(ISkReferenceCounted)
    ['{8D2F2435-57C9-4E3D-BC0B-979C436EF563}']
    function GammaCloseToSRGB: Boolean;
    function GammaIsLinear: Boolean;
    function IsEqual(const AColorSpace: ISkColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeLinearGamma: ISkColorSpace;
    function MakeSRGBGamma: ISkColorSpace;
    function ToProfile: ISkColorSpaceICCProfile;
    function ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
  end;

  { TSkColorSpace }

  TSkColorSpace = class(TSkReferenceCounted, ISkColorSpace)
  strict protected
    function GammaCloseToSRGB: Boolean;
    function GammaIsLinear: Boolean;
    function IsEqual(const AColorSpace: ISkColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeLinearGamma: ISkColorSpace;
    function MakeSRGBGamma: ISkColorSpace;
    function ToProfile: ISkColorSpaceICCProfile;
    function ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
    class function DoIsUnique(const AHandle: THandle): Boolean; override;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AProfile: ISkColorSpaceICCProfile): ISkColorSpace; static;
    class function MakeRGB(const ATransferFunction: TSkColorSpaceTransferFunction; const AToXyzD50: TSkColorSpaceXYZ): ISkColorSpace; static;
    class function MakeSRGB: ISkColorSpace; static;
    class function MakeSRGBLinear: ISkColorSpace; static;
  end;

  TSkAlphaType = (Unknown, Opaque, Premul, Unpremul);

  TSkColorType = (Unknown, Alpha8, RGB565, ARGB4444, RGBA8888, RGB888X,
    BGRA8888, RGBA1010102, BGRA1010102, RGB101010X, BGR101010X, Gray8, RGBAF16,
    RGBAF16Clamped, RGBAF32, RG88, AlphaF16, RgF16, Alpha16, RG1616,
    RGBA16161616);

  { TSkImageInfo }

  TSkImageInfo = record
    Width: Integer;
    Height: Integer;
    ColorType: TSkColorType;
    AlphaType: TSkAlphaType;
    ColorSpace: ISkColorSpace;
    constructor Create(const AWidth, AHeight: Integer; const AColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF}; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil);
    function ByteSize(const ARowBytes: NativeUInt): NativeUInt; inline;
    function BytesPerPixel: Integer; inline;
    function IsEmpty: Boolean;
    function IsOpaque: Boolean;
    function IsValid: Boolean;
    function IsValidRowBytes(const ARowBytes: NativeUInt): Boolean;
    function MakeAlphaType(const AAlphaType: TSkAlphaType): TSkImageInfo;
    function MakeColorSpace(const AColorSpace: ISkColorSpace): TSkImageInfo;
    function MakeColorType(const AColorType: TSkColorType): TSkImageInfo;
    function MakeDimensions(const AWidth, AHeight: Integer): TSkImageInfo;
    function MinByteSize: NativeUInt; inline;
    function MinRowBytes: NativeUInt; inline;
    function ShiftPerPixel: Integer; inline;
    class operator Equal(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
    class operator NotEqual(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
  end;

  TSkFilterQuality = (None, Low, Medium, High);

  { ISkPixmap }

  ISkPixmap = interface(ISkObject)
    ['{F1B971BE-6D20-4EA7-9EE5-21771E1F86DF}']
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; const AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISkPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSkAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetPixelAddr(const AX, AY: Integer): Pointer;
    function GetPixels: Pointer;
    function GetRowBytes: NativeUInt;
    function GetWidth: Integer;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const AFilterQuality: TSkFilterQuality): Boolean;
    procedure SetColorSpace(const AValue: ISkColorSpace);
    property Alphas[const AX, AY: Integer]: Single read GetAlpha;
    property AlphaType: TSkAlphaType read GetAlphaType;
    property Colors[const AX, AY: Integer]: TAlphaColor read GetColor;
    property ColorSpace: ISkColorSpace read GetColorSpace write SetColorSpace;
    property ColorType: TSkColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSkImageInfo read GetImageInfo;
    property PixelAddr[const AX, AY: Integer]: Pointer read GetPixelAddr;
    property Pixels: Pointer read GetPixels;
    property RowBytes: NativeUInt read GetRowBytes;
    property Width: Integer read GetWidth;
  end;

  { TSkPixmap }

  TSkPixmap = class(TSkObject, ISkPixmap)
  protected
    constructor Create; overload;
  strict protected
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; const AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISkPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSkAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetPixelAddr(const AX, AY: Integer): Pointer;
    function GetPixels: Pointer;
    function GetRowBytes: NativeUInt;
    function GetWidth: Integer;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const AFilterQuality: TSkFilterQuality): Boolean;
    procedure SetColorSpace(const AValue: ISkColorSpace);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt); overload;
  end;

  TSkRoundRectCorner = (UpperLeft, UpperRight, LowerRight, LowerLeft);

  TSkRoundRectRadii = array[TSkRoundRectCorner] of Single;

  TSkRoundRectType = (Empty, Rect, Oval, Simple, Nine, Complex);

  { ISkRoundRect }

  ISkRoundRect = interface(ISkObject)
    ['{574DC656-4D76-4576-B7E2-E7A27D73B01F}']
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADX, ADY: Single);
    function GetDeflate(const ADX, ADY: Single): ISkRoundRect;
    function GetHeight: Single;
    function GetInflate(const ADX, ADY: Single): ISkRoundRect;
    function GetOffset(const ADX, ADY: Single): ISkRoundRect;
    function GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetRoundRectType: TSkRoundRectType;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADX, ADY: Single);
    function IsEqual(const ARoundRect: ISkRoundRect): Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADX, ADY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    function Transform(const AMatrix: TMatrix): ISkRoundRect;
    property Height: Single read GetHeight;
    property Radii[const ACorner: TSkRoundRectCorner]: TPointF read GetRadii;
    property Rect: TRectF read GetRect;
    property RoundRectType: TSkRoundRectType read GetRoundRectType;
    property SimpleRadii: TPointF read GetSimpleRadii;
    property Width: Single read GetWidth;
  end;

  { TSkRoundRect }

  TSkRoundRect = class(TSkObject, ISkRoundRect)
  strict protected
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADX, ADY: Single);
    function GetDeflate(const ADX, ADY: Single): ISkRoundRect;
    function GetHeight: Single;
    function GetInflate(const ADX, ADY: Single): ISkRoundRect;
    function GetOffset(const ADX, ADY: Single): ISkRoundRect;
    function GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetRoundRectType: TSkRoundRectType;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADX, ADY: Single);
    function IsEqual(const ARoundRect: ISkRoundRect): Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADX, ADY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    function Transform(const AMatrix: TMatrix): ISkRoundRect;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARoundRect: ISkRoundRect); overload;
    constructor Create(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    constructor Create(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
  end;

  ISkPath = interface;

  TSkPathPoints = array[0..3] of TPointF;

  TSkPathVerb = (Move, Line, Quad, Conic, Cubic, Close);

  { TSkPathIteratorElem }

  TSkPathIteratorElem = record
  strict private type
    TMove = record
      Point: TPointF;
    end;
    TLine = record
      LastPoint: TPointF;
      Point: TPointF;
    end;
    TQuad = record
      LastPoint: TPointF;
      Point1: TPointF;
      Point2: TPointF;
    end;
    TConic = record
      LastPoint: TPointF;
      Point1: TPointF;
      Point2: TPointF;
      Weight: Single;
    end;
    TCubic = record
      LastPoint: TPointF;
      Point1: TPointF;
      Point2: TPointF;
      Point3: TPointF;
    end;
  public
    case Verb: TSkPathVerb of
      TSkPathVerb.Move: (Move: TMove);
      TSkPathVerb.Line: (Line: TLine);
      TSkPathVerb.Quad: (Quad: TQuad);
      TSkPathVerb.Conic: (Conic: TConic);
      TSkPathVerb.Cubic: (Cubic: TCubic);
      TSkPathVerb.Close: ();
  end;

  { ISkPathIterator }

  ISkPathIterator = interface(ISkEnumerable<TSkPathIteratorElem>)
    ['{E9155397-BA59-4720-BB32-2AAE42B8AE23}']
  end;

  TSkPathFillType = (Winding, EvenOdd, InverseWinding, InverseEvenOdd);

  TSkPathOp = (Difference, Intersect, Union, Eor, ReverseDifference);

  TSkSegmentMask  = (Line, Quad, Conic, Cubic);
  TSkSegmentMasks = set of TSkSegmentMask;

  { ISkPath }

  ISkPath = interface(ISkObject)
    ['{3657A42C-09A2-48AC-A351-4D65DB4D242B}']
    function Contains(const AX, AY: Single): Boolean;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
    function GetIterator(const AForceClose: Boolean): ISkPathIterator;
    function GetLastPoint: TPointF;
    function GetSegmentMasks: TSkSegmentMasks;
    function GetTightBounds: TRectF;
    function Interpolate(const AEnding: ISkPath; const AWeight: Single): ISkPath;
    function IsConvex: Boolean;
    function IsEmpty: Boolean;
    function IsFinite: Boolean;
    function IsInterpolatable(const AOther: ISkPath): Boolean;
    function IsLastContourClosed: Boolean;
    function IsLine: Boolean; overload;
    function IsLine(out APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISkRoundRect): Boolean; overload;
    function Offset(const ADX, ADY: Single): ISkPath;
    function Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
    function ToSVG: string;
    function Transform(const AMatrix: TMatrix): ISkPath;
    property Bounds: TRectF read GetBounds;
    property FillType: TSkPathFillType read GetFillType;
    property LastPoint: TPointF read GetLastPoint;
    property SegmentMasks: TSkSegmentMasks read GetSegmentMasks;
    property TightBounds: TRectF read GetTightBounds;
  end;

  { TSkPath }

  TSkPath = class(TSkObject, ISkPath)
  strict private type
    TPathIterator = class(TSkEnumerable<TSkPathIteratorElem>, ISkPathIterator)
    strict private
      FCurrent: TSkPathIteratorElem;
    strict protected
      function GetCurrent: TSkPathIteratorElem; override;
      function MoveNext: Boolean; override;
      class procedure DoDestroy(const AHandle: THandle); override;
    public
      constructor Create(const APath: ISkPath; const AForceClose: Boolean);
    end;

  protected
    constructor Create; overload;
  strict protected
    function Contains(const AX, AY: Single): Boolean;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
    function GetIterator(const AForceClose: Boolean): ISkPathIterator;
    function GetLastPoint: TPointF;
    function GetSegmentMasks: TSkSegmentMasks;
    function GetTightBounds: TRectF;
    function Interpolate(const AEnding: ISkPath; const AWeight: Single): ISkPath;
    function IsConvex: Boolean;
    function IsEmpty: Boolean;
    function IsFinite: Boolean;
    function IsInterpolatable(const APath: ISkPath): Boolean;
    function IsLastContourClosed: Boolean;
    function IsLine: Boolean; overload;
    function IsLine(out APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISkRoundRect): Boolean; overload;
    function Offset(const ADX, ADY: Single): ISkPath;
    function Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
    function ToSVG: string;
    function Transform(const AMatrix: TMatrix): ISkPath;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const ASVG: string); overload;
  end;

  { ISkOpBuilder }

  ISkOpBuilder = interface(ISkObject)
    ['{12828A55-1038-4307-B9E6-48C5A580489D}']
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
  end;

  { TSkOpBuilder }

  TSkOpBuilder = class(TSkObject, ISkOpBuilder)
  strict protected
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkPathMeasureMatrixFlag  = (Position, Tangent);
  TSkPathMeasureMatrixFlags = set of TSkPathMeasureMatrixFlag;

  { ISkPathMeasure }

  ISkPathMeasure = interface(ISkObject)
    ['{9AB42911-F5C4-4953-A1C5-3313205D8C84}']
    function GetLength: Single;
    function GetMatrix(const ADistance: Single; out AMatrix: TMatrix; const AMatrixFlags: TSkPathMeasureMatrixFlags = [TSkPathMeasureMatrixFlag.Position, TSkPathMeasureMatrixFlag.Tangent]): Boolean;
    function GetPositionAndTangent(const ADistance: Single; out APosition, ATangent: TPointF): Boolean;
    function GetSegment(const AStart, AStop: Single; const AStartWithMoveTo: Boolean): ISkPath;
    function IsClosed: Boolean;
    function NextContour: Boolean;
    property Length: Single read GetLength;
  end;

  { TSkPathMeasure }

  TSkPathMeasure = class(TSkObject, ISkPathMeasure)
  strict protected
    function GetLength: Single;
    function GetMatrix(const ADistance: Single; out AMatrix: TMatrix; const AMatrixFlags: TSkPathMeasureMatrixFlags = [TSkPathMeasureMatrixFlag.Position, TSkPathMeasureMatrixFlag.Tangent]): Boolean;
    function GetPositionAndTangent(const ADistance: Single; out APosition, ATangent: TPointF): Boolean;
    function GetSegment(const AStart, AStop: Single; const AStartWithMoveTo: Boolean): ISkPath;
    function IsClosed: Boolean;
    function NextContour: Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const APath: ISkPath; const AForceClosed: Boolean = False; const AResScale: Single = 1);
  end;

  TSkPathEffect1DStyle = (Translate, Rotate, Morph);

  TSkPathEffectTrimMode = (Normal, Inverted);

  { ISkPathEffect }

  ISkPathEffect = interface(ISkReferenceCounted)
    ['{0B5D608D-2B26-45CE-8C1E-BB5CA1293E5F}']
  end;

  { TSkPathEffect }

  TSkPathEffect = class(TSkReferenceCounted, ISkPathEffect)
  public
    class function Make1DPath(const APath: ISkPath; const AAdvance, APhase: Single; const AStyle: TSkPathEffect1DStyle): ISkPathEffect; static;
    class function Make2DLine(const AWidth: Single; const AMatrix: TMatrix): ISkPathEffect; static;
    class function Make2DPath(const AMatrix: TMatrix; const APath: ISkPath): ISkPathEffect; static;
    class function MakeCompose(const AOuter, AInner: ISkPathEffect): ISkPathEffect; static;
    class function MakeCorner(const ARadius: Single): ISkPathEffect; static;
    class function MakeDash(const AIntervals: TArray<Single>; const APhase: Single): ISkPathEffect; static;
    class function MakeDiscrete(const ASegLength, ADeviation: Single; const ASeedAssist: Cardinal = 0): ISkPathEffect; static;
    class function MakeSum(const AEffect1, AEffect2: ISkPathEffect): ISkPathEffect; static;
    class function MakeTrim(const AStart, AStop: Single; const AMode: TSkPathEffectTrimMode): ISkPathEffect; static;
  end;

  TSkPathArcSize = (Small, Large);

  TSkPathDirection = (CW, CCW);

  { ISkPathBuilder }

  ISkPathBuilder = interface(ISkObject)
    ['{1B8AC10A-F2B2-4A93-A149-7F7854473F7B}']
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSkPathArcSize; const ASweep: TSkPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISkPath;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
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
    procedure SetFillType(const AValue: TSkPathFillType);
    function Snapshot: ISkPath;
    procedure ToggleInverseFillType;
    property Bounds: TRectF read GetBounds;
    property FillType: TSkPathFillType read GetFillType write SetFillType;
  end;

  { TSkPathBuilder }

  TSkPathBuilder = class(TSkObject, ISkPathBuilder)
  strict protected
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSkPathArcSize; const ASweep: TSkPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISkPath;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
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
    procedure SetFillType(const AValue: TSkPathFillType);
    function Snapshot: ISkPath;
    procedure ToggleInverseFillType;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APathBuilder: ISkPathBuilder); overload;
    constructor Create(const AFillType: TSkPathFillType); overload;
  end;

  ISkRegion = interface;

  { ISkRegionCliperator }

  ISkRegionCliperator = interface(ISkEnumerable<TRect>)
    ['{409210A5-A81C-4A91-8A89-95DB52B70CFA}']
  end;

  { ISkRegionIterator }

  ISkRegionIterator = interface(ISkEnumerable<TRect>)
    ['{C3E20032-BA47-4F26-8B5C-C4676E150955}']
  end;

  { ISkRegionSpanerator }

  ISkRegionSpanerator = interface(ISkEnumerable<TPoint>)
    ['{50E750B5-867C-43A8-AE36-93FBA5C588ED}']
  end;

  TSkRegionOp = (Difference, Intersect, Union, Eor, ReverseDifference, Replace);

  { ISkRegion }

  ISkRegion = interface(ISkObject)
    ['{67F92B11-BF86-4975-A9D3-1174477C1018}']
    function Contains(const ARegion: ISkRegion): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const AX, AY: Integer): Boolean; overload;
    function GetBoundaryPath: ISkPath;
    function GetBounds: TRectF;
    function GetCliperator(const AClip: TRect): ISkRegionCliperator;
    function GetIterator: ISkRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISkRegionSpanerator;
    function GetTranslate(const AX, AY: Integer): ISkRegion;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISkRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISkRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARegion: ISkRegion; const AOp: TSkRegionOp): Boolean; overload;
    function Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARegion: ISkRegion): Boolean; overload;
    function QuickReject(const ARect: TRect): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISkPath; const AClip: ISkRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const AX, AY: Integer);
    property Bounds: TRectF read GetBounds;
  end;

  { TSkRegion }

  TSkRegion = class(TSkObject, ISkRegion)
  strict private type
    TRegionCliperator = class(TSkEnumerable<TRect>, ISkRegionCliperator)
    strict protected
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      class procedure DoDestroy(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion; const AClip: TRect);
    end;

    TRegionIterator = class(TSkEnumerable<TRect>, ISkRegionIterator)
    strict protected
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
      class procedure DoDestroy(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion);
    end;

    TRegionSpanerator = class(TSkEnumerable<TPoint>, ISkRegionSpanerator)
    strict private
      FCurrent: TPoint;
    strict protected
      function GetCurrent: TPoint; override;
      function MoveNext: Boolean; override;
      class procedure DoDestroy(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion; const AY, ALeft, ARight: Integer);
    end;

  strict protected
    function Contains(const ARegion: ISkRegion): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const AX, AY: Integer): Boolean; overload;
    function GetBoundaryPath: ISkPath;
    function GetBounds: TRectF;
    function GetCliperator(const AClip: TRect): ISkRegionCliperator;
    function GetIterator: ISkRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISkRegionSpanerator;
    function GetTranslate(const AX, AY: Integer): ISkRegion;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISkRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISkRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARegion: ISkRegion; const AOp: TSkRegionOp): Boolean; overload;
    function Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARegion: ISkRegion): Boolean; overload;
    function QuickReject(const ARect: TRect): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISkPath; const AClip: ISkRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const AX, AY: Integer);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARegion: ISkRegion); overload;
    constructor Create(const ARect: TRect); overload;
  end;

  { ISkTraceMemoryDump }

  ISkTraceMemoryDump = interface(ISkObject)
    ['{48706501-F5E3-4967-94A3-9DEE7731FB64}']
  end;

  { TSkTraceMemoryDump }

  TSkTraceMemoryDump = class abstract(TSkObject, ISkTraceMemoryDump);

  { TSkTraceMemoryDumpBaseClass }

  TSkTraceMemoryDumpBaseClass = class abstract(TSkTraceMemoryDump)
  strict private
    class constructor Create;
    class procedure dump_numeric_value_proc(context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl; static;
    class procedure dump_string_value_proc(context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl; static;
  strict protected
    procedure DumpNumericValue(const ADumpName, AValueName, AUnits: string; const AValue: UInt64); virtual; abstract;
    procedure DumpStringValue(const ADumpName, AValueName, AValue: string); virtual; abstract;
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

  IGRGLInterface = interface(ISkReferenceCounted)
    ['{4A06112D-77CC-4691-B02D-B44487A2DA5A}']
    function HasExtension(const AName: string): Boolean;
    function Validate: Boolean;
  end;

  { TGRGLInterface }

  TGRGLInterface = class(TSkReferenceCounted, IGRGLInterface)
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
    class function MakeNative: IGRGLInterface; static;
  end;

  {$IFDEF SK_METAL}

  { TGRMTLBackendContext }

  TGRMTLBackendContext = record
    Device: MTLDevice;
    Queue: MTLCommandQueue;
    constructor Create(const ADevice: MTLDevice; const AQueue: MTLCommandQueue);
  end;

  { TGRMTLTextureInfo }

  TGRMTLTextureInfo = record
    Texture: MTLTexture;
    constructor Create(const ATexture: MTLTexture);
  end;

  {$ENDIF}

  { IGRRecordingContext }

  IGRRecordingContext = interface(ISkReferenceCounted)
    ['{57A66D81-8C5E-48C7-B6A4-CA4E5099E494}']
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSkColorType): Integer;
    function IsAbandoned: Boolean;
  end;

  { TGRRecordingContext }

  TGRRecordingContext = class(TSkReferenceCounted, IGRRecordingContext)
  strict protected
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSkColorType): Integer;
    function IsAbandoned: Boolean;
  end;

  { TGRContextOptions }

  TGRContextOptions = record
    BufferMapThreshold: Integer;
    DoManualMipmapping: Boolean;
    AllowPathMaskCaching: Boolean;
    GlyphCacheTextureMaximumBytes: NativeUInt;
    AvoidStencilBuffers: Boolean;
    RuntimeProgramCacheSize: Integer;
  end;

  { IGRDirectContext }

  IGRDirectContext = interface(IGRRecordingContext)
    ['{D653DA23-5529-4A45-B2A2-446C8CA519F3}']
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure FreeGPUResources;
    function GetResourceCacheLimit: NativeUInt;
    procedure GetResourceCacheUsage(out AMaxResources: Integer; out AMaxResourcesBytes: NativeUInt);
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure PurgeUnlockedResources(const ScratchResourcesOnly: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext; overload;
    procedure ResetContext(const AState: Cardinal); overload;
    procedure ResetGLTextureBindings;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
    function Submit(const ASyncCPU: Boolean = False): Boolean;
    property ResourceCacheLimit: NativeUInt read GetResourceCacheLimit write SetResourceCacheLimit;
  end;

  { TGRDirectContext }

  TGRDirectContext = class(TGRRecordingContext, IGRDirectContext)
  strict protected
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure FreeGPUResources;
    function GetResourceCacheLimit: NativeUInt;
    procedure GetResourceCacheUsage(out AMaxResources: Integer; out AMaxResourcesBytes: NativeUInt);
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure PurgeUnlockedResources(const AScratchResourcesOnly: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext; overload;
    procedure ResetContext(const AState: Cardinal); overload;
    procedure ResetGLTextureBindings;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
    function Submit(const ASyncCPU: Boolean = False): Boolean;
  public
    class function MakeGL(const AInterface: IGRGLInterface = nil): IGRDirectContext; overload; static;
    class function MakeGL(const AOptions: TGRContextOptions; const AInterface: IGRGLInterface = nil): IGRDirectContext; overload; static;
    {$IFDEF SK_METAL}
    class function MakeMetal(const ABackendContext: TGRMTLBackendContext): IGRDirectContext; overload; static;
    class function MakeMetal(const ABackendContext: TGRMTLBackendContext; const AOptions: TGRContextOptions): IGRDirectContext; overload; static;
    {$ENDIF}
  end;

  TGRBackendAPI = (OpenGL{$IFDEF SK_METAL}, Metal = 2{$ENDIF});

  { IGRBackendRenderTarget }

  IGRBackendRenderTarget = interface(ISkObject)
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

  TGRBackendRenderTarget = class(TSkObject, IGRBackendRenderTarget)
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
    {$IFDEF SK_METAL}
    constructor CreateMetal(const AWidth, AHeight: Integer; const ATextureInfo: TGRMTLTextureInfo);
    {$ENDIF}
  end;

  { IGRBackendTexture }

  IGRBackendTexture = interface(ISkObject)
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

  TGRBackendTexture = class(TSkObject, IGRBackendTexture)
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
    {$IFDEF SK_METAL}
    constructor CreateMetal(const AWidth, AHeight: Integer; const AMipmapped: Boolean; const ATextureInfo: TGRMTLTextureInfo);
    {$ENDIF}
  end;

  TSkHighContrastConfigInvertStyle = (NoInvert, InvertBrightness, InvertLightness);

  { TSkHighContrastConfig }

  TSkHighContrastConfig = record
    Grayscale: Boolean;
    InvertStyle: TSkHighContrastConfigInvertStyle;
    Contrast: Single;
    constructor Create(const AGrayscale: Boolean; const AInvertStyle: TSkHighContrastConfigInvertStyle; const AContrast: Single);
  end;

  TSkBlendMode = (Clear,  Src, Dest, SrcOver, DestOver, SrcIn, DestIn, SrcOut,
    DestOut, SrcATop, DestATop, Eor, Plus, Modulate, Screen, Overlay, Darken,
    Lighten, ColorDodge, ColorBurn, HardLight, SoftLight, Difference, Exclusion,
    Multiply, Hue, Saturation, Color, Luminosity);

  TSkColorMatrix = array[0..19] of Single;

  TSkOverdrawColor = array[0..5] of TAlphaColor;

  TSkTableFilter = array[0..255] of Byte;

  { ISkColorFilter }

  ISkColorFilter = interface(ISkReferenceCounted)
    ['{AF15420E-1F26-4881-B4AF-638B231F7680}']
  end;

  { TSkColorFilter }

  TSkColorFilter = class(TSkReferenceCounted, ISkColorFilter)
  public
    class function MakeBlend(const AColor: TAlphaColor; const AMode: TSkBlendMode): ISkColorFilter; static;
    class function MakeCompose(const AOuter, AInner: ISkColorFilter): ISkColorFilter; static;
    class function MakeHighContrast(const AConfig: TSkHighContrastConfig): ISkColorFilter; static;
    class function MakeHSLAMatrix(const AMatrix: TSkColorMatrix): ISkColorFilter; static;
    class function MakeLerp(const AWeight: Single; const ADest, ASrc: ISkColorFilter): ISkColorFilter; static;
    class function MakeLighting(const AMultiply, AAdd: TAlphaColor): ISkColorFilter; static;
    class function MakeLinearToSRGBGamma: ISkColorFilter;
    class function MakeLumaColor: ISkColorFilter; static;
    class function MakeMatrix(const AMatrix: TSkColorMatrix): ISkColorFilter; static;
    class function MakeOverdraw(const AColors: TSkOverdrawColor): ISkColorFilter; static;
    class function MakeSRGBToLinearGamma: ISkColorFilter;
    class function MakeTable(const ATable: TSkTableFilter): ISkColorFilter; overload; static;
    class function MakeTable(const ATableA, ATableR, ATableG, ATableB: TSkTableFilter): ISkColorFilter; overload; static;
  end;

  TSkTileMode = (Clamp, Replicate, Mirror, Decal);

  { ISkShader }

  ISkShader = interface(ISkReferenceCounted)
    ['{6DC8870F-457B-408A-9D6F-4FB95FF657A0}']
    function MakeWithColorFilter(const AFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
  end;

  { TSkShader }

  TSkShader = class(TSkReferenceCounted, ISkShader)
  strict protected
    function MakeWithColorFilter(const AFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
  public
    class function MakeBlend(const AMode: TSkBlendMode; const ADest, ASrc: ISkShader): ISkShader; static;
    class function MakeColor(const AColor: TAlphaColor): ISkShader; overload; static;
    class function MakeColor(const AColor: TAlphaColorF; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeEmpty: ISkShader; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const ATileMode: TSkTileMode; const APositions: TArray<Single> = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const ATileMode: TSkTileMode; const APositions: TArray<Single> = nil; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ATileMode: TSkTileMode): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ATileMode: TSkTileMode; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const ATileMode: TSkTileMode; const APositions: TArray<Single> = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const ATileMode: TSkTileMode; const APositions: TArray<Single> = nil; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISkShader; overload; static;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISkShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISkShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISkShader; overload; static;
  end;

  TSkBlurStyle = (Normal, Solid, Outer, Inner);

  { ISkMaskFilter }

  ISkMaskFilter = interface(ISkReferenceCounted)
    ['{1F42E44F-D04B-486D-A6D2-EF19CEDE5B90}']
  end;

  { TSkMaskFilter }

  TSkMaskFilter = class(TSkReferenceCounted, ISkMaskFilter)
  public
    class function MakeBlur(const AStyle: TSkBlurStyle; const ASigma: Single; const ARespectCTM: Boolean = True): ISkMaskFilter; static;
    class function MakeTable(const ATable: TSkTableFilter): ISkMaskFilter; static;
    class function MakeTableClip(const AMin, AMax: Byte): ISkMaskFilter; static;
    class function MakeTableGamma(const AGamma: Single): ISkMaskFilter; static;
  end;

  { ISkRuntimeEffectUniform }

  ISkRuntimeEffectUniform = interface(ISkObject)
    ['{1110484A-8BC3-46CB-B3A2-9247A0F87E8A}']
    function GetByteSize: NativeUInt;
    function GetName: string;
    function GetOffset: NativeUInt;
    property ByteSize: NativeUInt read GetByteSize;
    property Name: string read GetName;
    property Offset: NativeUInt read GetOffset;
  end;

  { TSkRuntimeEffectUniform }

  TSkRuntimeEffectUniform = class(TSkObject, ISkRuntimeEffectUniform)
  strict protected
    function GetByteSize: NativeUInt;
    function GetName: string;
    function GetOffset: NativeUInt;
  end;

  { ISkRuntimeEffect }

  ISkRuntimeEffect = interface(ISkReferenceCounted)
    ['{BD1F0A33-BCA2-4227-AB3A-87B9406A2EC1}']
    function FindUniform(const AName: string): ISkRuntimeEffectUniform;
    function GetChildren(const AIndex: NativeUInt): string;
    function GetChildrenCount: NativeUInt;
    function GetUniform(const AIndex: NativeUInt): ISkRuntimeEffectUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformSize: NativeUInt;
    function MakeColorFilter(const Uniform: TBytes = nil; const AChildren: TArray<ISkColorFilter> = nil): ISkColorFilter;
    function MakeShader(const AIsOpaque: Boolean; const Uniform: TBytes = nil; const AChildren: TArray<ISkShader> = nil): ISkShader;
    property Children[const AIndex: NativeUInt]: string read GetChildren;
    property ChildrenCount: NativeUInt read GetChildrenCount;
    property Uniform[const AIndex: NativeUInt]: ISkRuntimeEffectUniform read GetUniform;
    property UniformCount: NativeUInt read GetUniformCount;
    property UniformSize: NativeUInt read GetUniformSize;
  end;

  { TSkRuntimeEffect }

  TSkRuntimeEffect = class(TSkReferenceCounted, ISkRuntimeEffect)
  strict protected
    function FindUniform(const AName: string): ISkRuntimeEffectUniform;
    function GetChildren(const AIndex: NativeUInt): string;
    function GetChildrenCount: NativeUInt;
    function GetUniform(const AIndex: NativeUInt): ISkRuntimeEffectUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformSize: NativeUInt;
    function MakeColorFilter(const AUniform: TBytes = nil; const AChildren: TArray<ISkColorFilter> = nil): ISkColorFilter;
    function MakeShader(const AIsOpaque: Boolean; const AUniform: TBytes = nil; const AChildren: TArray<ISkShader> = nil): ISkShader;
  public
    class function Make(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function Make(const ASkSL: string; out AError: string): ISkRuntimeEffect; overload; static;
  end;

  { ISkPicture }

  ISkPicture = interface(ISkReferenceCounted)
    ['{0208DD4B-CCE5-4789-8F52-8AA96B2E642F}']
    function GetCullRect: TRectF;
    function GetUniqueID: Cardinal;
    function MakeShader(const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
    function SerializeToBytes: TBytes;
    procedure SerializeToStream(const AStream: TStream);
    property CullRect: TRectF read GetCullRect;
    property UniqueID: Cardinal read GetUniqueID;
  end;

  { TSkPicture }

  TSkPicture = class(TSkReferenceCounted, ISkPicture)
  strict protected
    function GetCullRect: TRectF;
    function GetUniqueID: Cardinal;
    function MakeShader(const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
    function SerializeToBytes: TBytes;
    procedure SerializeToStream(const AStream: TStream);
  public
    class function MakeFromBytes(const ABytes: TBytes): ISkPicture; static;
    class function MakeFromStream(const AStream: TStream): ISkPicture; static;
  end;

  ISkCanvas = interface;

  { ISkPictureRecorder }

  ISkPictureRecorder = interface(ISkObject)
    ['{0A676065-C294-4A3D-A65A-5F9116304EE4}']
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
  end;

  { TSkPictureRecorder }

  TSkPictureRecorder = class(TSkObject, ISkPictureRecorder)
  strict protected
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  ISkImageFilter = interface;

  ISkPaint = interface;

  TGRSurfaceOrigin = (TopLeft, BottomLeft);

  TSkEncodedImageFormat = (JPEG = 3, PNG, WEBP = 6);

  TSkImageCachingHint = (Allow, Disallow);

  { ISkImage }

  ISkImage = interface(ISkReferenceCounted)
    ['{1C20B527-EBBD-466B-B3E3-68220D5CACED}']
    function EncodeToBytes(const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 100): TBytes; overload;
    procedure EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 100); overload;
    function GetAlphaType: TSkAlphaType;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetUniqueID: Cardinal;
    function GetWidth: Integer;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(const AContext: IGRRecordingContext): Boolean;
    function MakeNonTextureImage: ISkImage;
    function MakeRasterImage: ISkImage;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader;
    function MakeSubset(const ASubset: TRect; const AContext: IGRDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGRDirectContext; const AMipmapped: Boolean = False): ISkImage;
    function MakeWithFilter(const AContext: IGRRecordingContext; const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint): ISkImage;
    function ReadPixels(const AContext: IGRDirectContext; const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ReadPixels(const AContext: IGRDirectContext; const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const AFilterQuality: TSkFilterQuality; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean;
    property AlphaType: TSkAlphaType read GetAlphaType;
    property ColorSpace: ISkColorSpace read GetColorSpace;
    property ColorType: TSkColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSkImageInfo read GetImageInfo;
    property UniqueID: Cardinal read GetUniqueID;
    property Width: Integer read GetWidth;
  end;

  { TSkImage }

  TSkImage = class(TSkReferenceCounted, ISkImage)
  strict protected
    function EncodeToBytes(const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 100): TBytes; overload;
    procedure EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 100); overload;
    function GetAlphaType: TSkAlphaType;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetUniqueID: Cardinal;
    function GetWidth: Integer;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(const AContext: IGRRecordingContext): Boolean;
    function MakeNonTextureImage: ISkImage;
    function MakeRasterImage: ISkImage;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader;
    function MakeSubset(const ASubset: TRect; const AContext: IGRDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGRDirectContext; const AMipmapped: Boolean = False): ISkImage;
    function MakeWithFilter(const AContext: IGRRecordingContext; const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint): ISkImage;
    function ReadPixels(const AContext: IGRDirectContext; const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ReadPixels(const AContext: IGRDirectContext; const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const AFilterQuality: TSkFilterQuality; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean;
  public
    class function MakeFromEncoded(const ABytes: TBytes): ISkImage; overload; static;
    class function MakeFromEncoded(const AStream: TStream): ISkImage; overload; static;
    class function MakeRaster(const APixmap: ISkPixmap): ISkImage; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage; overload; static;
  end;

  TSkColorChannel = (R, G, B, A);

  { ISkImageFilter }

  ISkImageFilter = interface(ISkReferenceCounted)
    ['{B233455A-C2BB-444B-ACBE-6B09B8D848F3}']
  end;

  { TSkImageFilter }

  TSkImageFilter = class(TSkReferenceCounted, ISkImageFilter)
  public
    class function MakeAlphaThreshold(const ARegion: TRect; const AInnerMin, AOuterMax: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeAlphaThreshold(const ARegion: ISkRegion; const AInnerMin, AOuterMax: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter; const ACropRect: TRectF; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlend(const AMode: TSkBlendMode; const ABackground: ISkImageFilter; const AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlend(const AMode: TSkBlendMode; const ABackground: ISkImageFilter; const ACropRect: TRectF; const AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; const AInput: ISkImageFilter = nil; const ATileMode: TSkTileMode = TSkTileMode.Decal): ISkImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil; const ATileMode: TSkTileMode = TSkTileMode.Decal): ISkImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISkColorFilter; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISkColorFilter; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeCompose(const AOuter, AInner: ISkImageFilter): ISkImageFilter; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSkColorChannel; const AScale: Single; const ADisplacement: ISkImageFilter; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSkColorChannel; const AScale: Single; const ADisplacement: ISkImageFilter; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadow(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadow(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADX, ADY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage; const AFilterQuality: TSkFilterQuality = TSkFilterQuality.High): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage; const ASrc, ADest: TRectF; const AFilterQuality: TSkFilterQuality = TSkFilterQuality.High): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixTransform(const AMatrix: TMatrix; const AFilterQuality: TSkFilterQuality; const AInput: ISkImageFilter = nil): ISkImageFilter; static;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISkImageFilter>): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISkImageFilter>; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeOffset(const ADX, ADY: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeOffset(const ADX, ADY: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePicture(const APicture: ISkPicture): ISkImageFilter; overload; static;
    class function MakePicture(const APicture: ISkPicture; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakePointLitDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeTile(const ASrc, ADest: TRect; const AInput: ISkImageFilter = nil): ISkImageFilter; static;
  end;

  TSkPixelGeometry = (Unknown, RGBHorizontal, BGRHorizontal, RGBVertical, BGRVertical);

  TSkSurfacePropertiesFlag  = (AUseDeviceIndependentFonts);
  TSkSurfacePropertiesFlags = set of TSkSurfacePropertiesFlag;

  { ISkSurfaceProperties }

  ISkSurfaceProperties = interface(ISkObject)
    ['{1C39B509-4180-420D-A9F7-E410E816FDA5}']
    function GetFlags: TSkSurfacePropertiesFlags;
    function GetPixelGeometry: TSkPixelGeometry;
    function IsEqual(const AProperties: ISkSurfaceProperties): Boolean;
    property Flags: TSkSurfacePropertiesFlags read GetFlags;
    property PixelGeometry: TSkPixelGeometry read GetPixelGeometry;
  end;

  { TSkSurfaceProperties }

  TSkSurfaceProperties = class(TSkObject, ISkSurfaceProperties)
  strict protected
    function GetFlags: TSkSurfacePropertiesFlags;
    function GetPixelGeometry: TSkPixelGeometry;
    function IsEqual(const AProperties: ISkSurfaceProperties): Boolean;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AFlags: TSkSurfacePropertiesFlags = []; const APixelGeometry: TSkPixelGeometry = TSkPixelGeometry.Unknown);
  end;

  TSkSurfaceRasterReleaseProc = reference to procedure (const APixels: Pointer);

  { ISkSurface }

  ISkSurface = interface(ISkReferenceCounted)
    ['{4BBFA7B0-50AA-4F69-AD08-8FBF21CA6E80}']
    function GetCanvas: ISkCanvas;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetProperties: ISkSurfaceProperties;
    function GetRecordingContext: IGRRecordingContext;
    function GetWidth: Integer;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function MakeImageSnapshot: ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure WritePixels(const ASrc: ISkPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    property Canvas: ISkCanvas read GetCanvas;
    property Height: Integer read GetHeight;
    property ImageInfo: TSkImageInfo read GetImageInfo;
    property Properties: ISkSurfaceProperties read GetProperties;
    property RecordingContext: IGRRecordingContext read GetRecordingContext;
    property Width: Integer read GetWidth;
  end;

  { TSkSurface }

  TSkSurface = class(TSkReferenceCounted, ISkSurface)
  {$IFDEF SK_METAL}
  strict private type
    MTLDrawableClass = interface(NSObjectClass)
      ['{920C8ECC-7635-4BA2-9EB8-CF349089218D}']
    end;

    TMTLDrawable = class(TOCGenericImport<MTLDrawableClass, MTLDrawable>);
  {$ENDIF}
  strict private
    class procedure raster_release_proc(pixels: Pointer; context: Pointer); cdecl; static;
  strict protected
    function GetCanvas: ISkCanvas;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetProperties: ISkSurfaceProperties;
    function GetRecordingContext: IGRRecordingContext;
    function GetWidth: Integer;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function MakeImageSnapshot: ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure WritePixels(const ASrc: ISkPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
  public
    {$IFDEF SK_METAL}
    class function MakeFromCAMetalLayer(const AContext: IGRRecordingContext; const ALayer: CAMetalLayer; const AOrigin: TGRSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AColorSpace: ISkColorSpace; const AProperties: ISkSurfaceProperties; out ADrawable: MTLDrawable): ISkSurface; static;
    class function MakeFromMTKView(const AContext: IGRRecordingContext; const AView: MTKView; const AOrigin: TGRSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AColorSpace: ISkColorSpace; const AProperties: ISkSurfaceProperties): ISkSurface; static;
    {$ENDIF}
    class function MakeFromRenderTarget(const AContext: IGRRecordingContext; const ARenderTarget: IGRBackendRenderTarget; const AOrigin: TGRSurfaceOrigin; const AColorType: TSkColorType; const AColorSpace: ISkColorSpace; const AProperties: ISkSurfaceProperties): ISkSurface; static;
    class function MakeFromTexture(const AContext: IGRRecordingContext; const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AColorSpace: ISkColorSpace; const AProperties: ISkSurfaceProperties): ISkSurface; static;
    class function MakeNull(const AWidth, AHeight: Integer): ISkSurface; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const AProperties: ISkSurfaceProperties = nil): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const ARowBytes: NativeUInt; const AProperties: ISkSurfaceProperties = nil): ISkSurface; overload; static;
    class function MakeRaster(const AWidth, AHeight: Integer): ISkSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISkPixmap; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil; const AProperties: ISkSurfaceProperties = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil; const AProperties: ISkSurfaceProperties = nil): ISkSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGRDirectContext; const ABudgeted: Boolean; const AImageInfo: TSkImageInfo; const ASampleCount: Integer = 0; const AOrigin: TGRSurfaceOrigin = TGRSurfaceOrigin.BottomLeft; const AProperties: ISkSurfaceProperties = nil; const AShouldCreateWithMips: Boolean = False): ISkSurface; static;
  end;

  TSkVertexMode = (Triangles, TriangleStrip, TriangleFan);

  { ISkVertices }

  ISkVertices = interface(ISkReferenceCounted)
    ['{576AEA37-5A7C-4CAC-B69C-D033AD7D1FA6}']
    function GetUniqueID: Cardinal;
    property UniqueID: Cardinal read GetUniqueID;
  end;

  { TSkVertices }

  TSkVertices = class(TSkReferenceCounted, ISkVertices)
  strict protected
    function GetUniqueID: Cardinal;
    class function DoIsUnique(const AHandle: THandle): Boolean; override;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function MakeCopy(const AVertexMode: TSkVertexMode; const APositions, ATextures: TArray<TPointF>; const AColors: TArray<TAlphaColor>; const AIndices: TArray<Word> = nil): ISkVertices; static;
  end;

  TSkFontMetricsFlag  = (UnderlineThicknessIsValid, UnderlinePositionIsValid, StrikeoutThicknessIsValid, StrikeoutPositionIsValid, BoundsInvalid);
  TSkFontMetricsFlags = set of TSkFontMetricsFlag;

  { TSkFontMetrics }

  TSkFontMetrics = record
    Flags: TSkFontMetricsFlags;
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
    class operator Equal(const AFontMetrics1, AFontMetrics2: TSkFontMetrics): Boolean;
    class operator NotEqual(const AFontMetrics1, AFontMetrics2: TSkFontMetrics): Boolean;
  end;

  TSkFontSlant = (Upright, Italic, Oblique);

  TSkFontWeight = (Invisible, Thin, ExtraLight, Light, Normal, Medium, SemiBold, Bold, ExtraBold, Black, ExtraBlack);

  TSkFontWidth = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Normal, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded);

  { TSkFontStyle }

  TSkFontStyle = record
  private const
    FontWeight : array[TSkFontWeight] of Integer = (0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000);
    FontWidth  : array[TSkFontWidth]  of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9);
  public
    Weight: Integer;
    Width: Integer;
    Slant: TSkFontSlant;
    constructor Create(const AWeight, AWidth: Integer; const ASlant: TSkFontSlant); overload;
    constructor Create(const AWeight: TSkFontWeight; const AWidth: TSkFontWidth; const ASlant: TSkFontSlant); overload;
    class function Normal: TSkFontStyle; static; inline;
    class function Bold: TSkFontStyle; static; inline;
    class function Italic: TSkFontStyle; static; inline;
    class function BoldItalic: TSkFontStyle; static; inline;
  end;

  { ISkTypeface }

  ISkTypeface = interface(ISkReferenceCounted)
    ['{7F036CD6-BD6B-4AC7-9219-0DF4E356B0BF}']
    function GetFamilyName: string;
    function GetSlant: TSkFontSlant;
    function GetStyle: TSkFontStyle;
    function GetUniqueID: Cardinal;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
    property FamilyName: string read GetFamilyName;
    property Slant: TSkFontSlant read GetSlant;
    property Style: TSkFontStyle read GetStyle;
    property UniqueID: Cardinal read GetUniqueID;
    property Weight: Integer read GetWeight;
    property Width: Integer read GetWidth;
  end;

  { TSkTypeface }

  TSkTypeface = class(TSkReferenceCounted, ISkTypeface)
  strict protected
    function GetFamilyName: string;
    function GetSlant: TSkFontSlant;
    function GetStyle: TSkFontStyle;
    function GetUniqueID: Cardinal;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
  public
    class function MakeDefault: ISkTypeface; static;
    class function MakeFromBytes(const ABytes: TBytes; const ATTcIndex: Integer = 0): ISkTypeface; static;
    class function MakeFromFile(const AFileName: string; const ATTcIndex: Integer = 0): ISkTypeface; static;
    class function MakeFromName(const AFamilyName: string; const AStyle: TSkFontStyle): ISkTypeface; static;
    class function MakeFromStream(const AStream: TStream; const ATTcIndex: Integer = 0): ISkTypeface; static;
  end;

  TSkFontEdging = (Alias, AntiAlias, SubpixelAntiAlias);

  TSkFontGlyphPathProc = reference to procedure (const APath: ISkPath; const AMatrix: TMatrix);

  TSkFontHinting = (None, Slight, Normal, Full);

  { ISkFont }

  ISkFont = interface(ISkObject)
    ['{E94E4044-598D-4007-9146-9F0AE30CF470}']
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<TRectF>;
    function GetEdging: TSkFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>;
    function GetHinting: TSkFontHinting;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSkFontMetrics): Single;
    function GetOffsets(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetPath(const AGlyph: Word): ISkPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSkFontGlyphPathProc);
    function GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>; overload;
    function GetPositions(const AGlyphs: TArray<Word>; const AOrigin: TPointF): TArray<TPointF>; overload;
    function GetScaleX: Single;
    function GetSize: Single;
    function GetSkewX: Single;
    function GetSpacing: Single;
    function GetSubpixel: Boolean;
    function GetTypeface: ISkTypeface;
    function GetTypefaceOrDefault: ISkTypeface;
    function GetWidths(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<Single>;
    procedure GetWidthsAndBounds(const AGlyphs: TArray<Word>; out AWidths: TArray<Single>; out ABounds: TArray<TRectF>; const APaint: ISkPaint = nil);
    function IsEqual(const AFont: ISkFont): Boolean;
    function MakeWithSize(const ASize: Single): ISkFont;
    function MeasureText(const AText: string; const APaint: ISkPaint = nil): Single; overload;
    function MeasureText(const AText: string; out ABounds: TRectF; const APaint: ISkPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; out ABounds: TRectF; const APaint: ISkPaint = nil): Single; overload;
    procedure SetBaselineSnap(const AValue: Boolean);
    procedure SetEdging(const AValue: TSkFontEdging);
    procedure SetEmbeddedBitmaps(const AValue: Boolean);
    procedure SetEmbolden(const AValue: Boolean);
    procedure SetForceAutoHinting(const AValue: Boolean);
    procedure SetHinting(const AValue: TSkFontHinting);
    procedure SetLinearMetrics(const AValue: Boolean);
    procedure SetScaleX(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetSkewX(const AValue: Single);
    procedure SetSubpixel(const AValue: Boolean);
    procedure SetTypeface(const AValue: ISkTypeface);
    function UnicharToGlyph(const AUnichar: Integer): Word;
    function UnicharsToGlyphs(const AUnichars: TArray<Integer>): TArray<Word>;
    property BaselineSnap: Boolean read GetBaselineSnap write SetBaselineSnap;
    property Edging: TSkFontEdging read GetEdging write SetEdging;
    property EmbeddedBitmaps: Boolean read GetEmbeddedBitmaps write SetEmbeddedBitmaps;
    property Embolden: Boolean read GetEmbolden write SetEmbolden;
    property ForceAutoHinting: Boolean read GetForceAutoHinting write SetForceAutoHinting;
    property Hinting: TSkFontHinting read GetHinting write SetHinting;
    property LinearMetrics: Boolean read GetLinearMetrics write SetLinearMetrics;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property Size: Single read GetSize write SetSize;
    property SkewX: Single read GetSkewX write SetSkewX;
    property Spacing: Single read GetSpacing;
    property Subpixel: Boolean read GetSubpixel write SetSubpixel;
    property Typeface: ISkTypeface read GetTypeface write SetTypeface;
  end;

  { TSkFont }

  TSkFont = class(TSkObject, ISkFont)
  strict private
    class procedure getpaths_proc(const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl; static;
  strict protected
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<TRectF>;
    function GetEdging: TSkFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>;
    function GetHinting: TSkFontHinting;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSkFontMetrics): Single;
    function GetOffsets(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetPath(const AGlyph: Word): ISkPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSkFontGlyphPathProc);
    function GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>; overload;
    function GetPositions(const AGlyphs: TArray<Word>; const AOrigin: TPointF): TArray<TPointF>; overload;
    function GetScaleX: Single;
    function GetSize: Single;
    function GetSkewX: Single;
    function GetSpacing: Single;
    function GetSubpixel: Boolean;
    function GetTypeface: ISkTypeface;
    function GetTypefaceOrDefault: ISkTypeface;
    function GetWidths(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<Single>;
    procedure GetWidthsAndBounds(const AGlyphs: TArray<Word>; out AWidths: TArray<Single>; out ABounds: TArray<TRectF>; const APaint: ISkPaint = nil);
    function IsEqual(const AFont: ISkFont): Boolean;
    function MakeWithSize(const ASize: Single): ISkFont;
    function MeasureText(const AText: string; const APaint: ISkPaint = nil): Single; overload;
    function MeasureText(const AText: string; out ABounds: TRectF; const APaint: ISkPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): Single; overload;
    function MeasureTextGlyphs(const AGlyphs: TArray<Word>; out ABounds: TRectF; const APaint: ISkPaint = nil): Single; overload;
    procedure SetBaselineSnap(const AValue: Boolean);
    procedure SetEdging(const AValue: TSkFontEdging);
    procedure SetEmbeddedBitmaps(const AValue: Boolean);
    procedure SetEmbolden(const AValue: Boolean);
    procedure SetForceAutoHinting(const AValue: Boolean);
    procedure SetHinting(const AValue: TSkFontHinting);
    procedure SetLinearMetrics(const AValue: Boolean);
    procedure SetScaleX(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetSkewX(const AValue: Single);
    procedure SetSubpixel(const AValue: Boolean);
    procedure SetTypeface(const AValue: ISkTypeface);
    function UnicharToGlyph(const AUnichar: Integer): Word;
    function UnicharsToGlyphs(const AUnichars: TArray<Integer>): TArray<Word>;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AFont: ISkFont); overload;
    constructor Create(const ATypeface: ISkTypeface = nil; const ASize: Single = 12; const AScaleX: Single = 1; const ASkewX: Single = 0); overload;
  end;

  { TSkRotationScaleMatrix }

  TSkRotationScaleMatrix = record
    SCos: Single;
    SSin: Single;
    TX: Single;
    TY: Single;
    constructor Create(const AScale, ARadians, ATX, ATY, AAnchorX, AAnchorY: Single);
    constructor CreateRotation(const ARadians, AAnchorX, AAnchorY: Single);
    constructor CreateScale(const AScale: Single);
    constructor CreateTranslation(const AX, AY: Single);
    class function Identity: TSkRotationScaleMatrix; static; inline;
  end;

  { ISkRunBuffer }

  ISkRunBuffer = interface
    ['{AE990974-C21C-456D-8F09-E46BD108C87D}']
    procedure FromGlyphArray(const AArray: TArray<Word>);
    function GetCount: Integer;
    function GetGlyph(const AIndex: Integer): Word;
    function GetGlyphsAddress: Pointer;
    function GetPositionsAddress: Pointer;
    procedure SetGlyph(const AIndex: Integer; const AValue: Word);
    function ToGlyphArray: TArray<Word>;
    property Count: Integer read GetCount;
    property Glyphs[const AIndex: Integer]: Word read GetGlyph write SetGlyph;
    property GlyphsAddress: Pointer read GetGlyphsAddress;
    property PositionsAddress: Pointer read GetPositionsAddress;
  end;

  { ISkHorizontalRunBuffer }

  ISkHorizontalRunBuffer = interface(ISkRunBuffer)
    ['{923E3F55-8610-42AA-BFE6-33AD9FDFF0C4}']
    procedure FromHorizontalPositionArray(const AArray: TArray<Single>);
    function GetHorizontalPosition(const AIndex: Integer): Single;
    procedure SetHorizontalPosition(const AIndex: Integer; const AValue: Single);
    function ToHorizontalPositionArray: TArray<Single>;
    property Positions[const AIndex: Integer]: Single read GetHorizontalPosition write SetHorizontalPosition;
  end;

  { ISkPositionedRunBuffer }

  ISkPositionedRunBuffer = interface(ISkRunBuffer)
    ['{36457BC6-703A-487D-928E-9C1C4A4E05CD}']
    procedure FromPositionedPositionArray(const AArray: TArray<TPointF>);
    function GetPositionedPosition(const AIndex: Integer): TPointF;
    procedure SetPositionedPosition(const AIndex: Integer; const AValue: TPointF);
    function ToPositionedPositionArray: TArray<TPointF>;
    property Positions[const AIndex: Integer]: TPointF read GetPositionedPosition write SetPositionedPosition;
  end;

  { ISkRotationScaleRunBuffer }

  ISkRotationScaleRunBuffer = interface(ISkRunBuffer)
    ['{B6E037AE-5569-4CA8-B6E0-341CD2D7A11C}']
    procedure FromRotationScalePositionArray(const AArray: TArray<TSkRotationScaleMatrix>);
    function GetRotationScalePosition(const AIndex: Integer): TSkRotationScaleMatrix;
    procedure SetRotationScalePosition(const AIndex: Integer; const AValue: TSkRotationScaleMatrix);
    function ToRotationScalePositionArray: TArray<TSkRotationScaleMatrix>;
    property Positions[const AIndex: Integer]: TSkRotationScaleMatrix read GetRotationScalePosition write SetRotationScalePosition;
  end;

  { TSkRunBuffer }

  TSkRunBuffer = class(TInterfacedObject, ISkRunBuffer, ISkHorizontalRunBuffer, ISkPositionedRunBuffer, ISkRotationScaleRunBuffer)
  strict private type
    TGlyphs                 = array[0..0] of Word;
    PGlyphs                 = ^TGlyphs;
    THorizontalPositions    = array[0..0] of Single;
    PHorizontalPositions    = ^THorizontalPositions;
    TPositionedPositions    = array[0..0] of TPointF;
    PPositionedPositions    = ^TPositionedPositions;
    TRotationScalePositions = array[0..0] of TSkRotationScaleMatrix;
    PRotationScalePositions = ^TRotationScalePositions;
  strict private
    FCount: Integer;
    FGlyphs: Pointer;
    FPositions: Pointer;
  protected
    constructor Create(const AGlyphs, APositions: Pointer; const ACount: Integer);
  strict protected
    procedure FromGlyphArray(const AArray: TArray<Word>);
    procedure FromHorizontalPositionArray(const AArray: TArray<Single>);
    procedure FromPositionedPositionArray(const AArray: TArray<TPointF>);
    procedure FromRotationScalePositionArray(const AArray: TArray<TSkRotationScaleMatrix>);
    function GetCount: Integer;
    function GetGlyph(const AIndex: Integer): Word;
    function GetGlyphsAddress: Pointer;
    function GetHorizontalPosition(const AIndex: Integer): Single;
    function GetPositionedPosition(const AIndex: Integer): TPointF;
    function GetPositionsAddress: Pointer;
    function GetRotationScalePosition(const AIndex: Integer): TSkRotationScaleMatrix;
    procedure SetGlyph(const AIndex: Integer; const AValue: Word);
    procedure SetHorizontalPosition(const AIndex: Integer; const AValue: Single);
    procedure SetPositionedPosition(const AIndex: Integer; const AValue: TPointF);
    procedure SetRotationScalePosition(const AIndex: Integer; const AValue: TSkRotationScaleMatrix);
    function ToGlyphArray: TArray<Word>;
    function ToHorizontalPositionArray: TArray<Single>;
    function ToPositionedPositionArray: TArray<TPointF>;
    function ToRotationScalePositionArray: TArray<TSkRotationScaleMatrix>;
  end;

  { ISkTextBlob }

  ISkTextBlob = interface(ISkReferenceCounted)
    ['{AB2465A6-1886-4C72-AFC8-E91AB36D73A7}']
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
  end;

  { TSkTextBlob }

  TSkTextBlob = class(TSkReferenceCounted, ISkTextBlob)
  strict protected
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
    class function DoIsUnique(const AHandle: THandle): Boolean; override;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AText: string; const AFont: ISkFont): ISkTextBlob; overload; static;
    class function Make(const AText: string; const AFont: ISkFont; const AOrigin: TPointF): ISkTextBlob; overload; static;
    class function MakeGlyphs(const AGlyphs: TArray<Word>; const AFont: ISkFont): ISkTextBlob; overload; static;
    class function MakeGlyphs(const AGlyphs: TArray<Word>; const AFont: ISkFont; const AOrigin: TPointF): ISkTextBlob; overload; static;
    class function MakeHorizontal(const AText: string; const APositions: TArray<Single>; const AY: Single; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeHorizontalGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<Single>; const AY: Single; const AFont: ISkFont): ISkTextBlob; static;
    class function MakePositioned(const AText: string; const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob; static;
    class function MakePositionedGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeRotationScale(const AText: string; const APositions: TArray<TSkRotationScaleMatrix>; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeRotationScaleGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TSkRotationScaleMatrix>; const AFont: ISkFont): ISkTextBlob; static;
  end;

  { ISkTextBlobBuilder }

  ISkTextBlobBuilder = interface(ISkObject)
    ['{7C310A25-B21F-4AC3-A6F3-2980BC59838B}']
    function AllocateRun(const AFont: ISkFont; const ACount: Integer; const AX, AY: Single): TSkRunBuffer; overload;
    function AllocateRun(const AFont: ISkFont; const ACount: Integer; const AX, AY: Single; const ABounds: TRectF): TSkRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISkFont; const ACount: Integer; const AY: Single): ISkHorizontalRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISkFont; const ACount: Integer; const AY: Single; const ABounds: TRectF): ISkHorizontalRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISkFont; const ACount: Integer): ISkPositionedRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISkFont; const ACount: Integer; const ABounds: TRectF): ISkPositionedRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISkFont; const ACount: Integer): ISkRotationScaleRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISkFont; const ACount: Integer; const ABounds: TRectF): ISkRotationScaleRunBuffer; overload;
    function Detach: ISkTextBlob;
  end;

  { TSkTextBlobBuilder }

  TSkTextBlobBuilder = class(TSkObject, ISkTextBlobBuilder)
  strict protected
    function AllocateRun(const AFont: ISkFont; const ACount: Integer; const AX, AY: Single): TSkRunBuffer; overload;
    function AllocateRun(const AFont: ISkFont; const ACount: Integer; const AX, AY: Single; const ABounds: TRectF): TSkRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISkFont; const ACount: Integer; const AY: Single): ISkHorizontalRunBuffer; overload;
    function AllocateHorizontalRun(const AFont: ISkFont; const ACount: Integer; const AY: Single; const ABounds: TRectF): ISkHorizontalRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISkFont; const ACount: Integer): ISkPositionedRunBuffer; overload;
    function AllocatePositionedRun(const AFont: ISkFont; const ACount: Integer; const ABounds: TRectF): ISkPositionedRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISkFont; const ACount: Integer): ISkRotationScaleRunBuffer; overload;
    function AllocateRotationScaleRun(const AFont: ISkFont; const ACount: Integer; const ABounds: TRectF): ISkRotationScaleRunBuffer; overload;
    function Detach: ISkTextBlob;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { TSkShaperRunRange }

  TSkShaperRunRange = record
    Start: NativeUInt;
    Size: NativeUInt;
  end;

  { TSkShaperRunInfo }

  TSkShaperRunInfo = record
    Font: ISkFont;
    BidiLevel: Byte;
    Advance: TPointF;
    GlyphCount: NativeUInt;
    UTF8Range: TSkShaperRunRange;
  end;

  TSkShaperRunBufferFlag  = (IncludeOffsets, IncludeClusters);
  TSkShaperRunBufferFlags = set of TSkShaperRunBufferFlag;

  { ISkShaperRunBuffer }

  ISkShaperRunBuffer = interface
    ['{9E327EF5-A019-4457-8F44-41C1C32A6242}']
    function GetClusters: TArray<Cardinal>;
    function GetGlyphs: TArray<Word>;
    function GetOffsets: TArray<TPointF>;
    function GetPoint: TPointF;
    function GetPositions: TArray<TPointF>;
    property Clusters: TArray<Cardinal> read GetClusters;
    property Glyphs: TArray<Word> read GetGlyphs;
    property Offsets: TArray<TPointF> read GetOffsets;
    property Point: TPointF read GetPoint;
    property Positions: TArray<TPointF> read GetPositions;
  end;

  { TSkShaperRunBuffer }

  TSkShaperRunBuffer = record
  strict private type
    TGlyphs    = array[0..0] of Word;
    PGlyphs    = ^TGlyphs;
    TPositions = array[0..0] of TPointF;
    PPositions = ^TPositions;
    TOffsets   = array[0..0] of TPointF;
    POffsets   = ^TOffsets;
    TClusters  = array[0..0] of Cardinal;
    PClusters  = ^TClusters;
  public
    Glyphs: PGlyphs;
    Positions: PPositions;
    Offsets: POffsets;
    Clusters: PClusters;
    Point: TPointF;
  end;

  { ISkShaperRunHandler }

  ISkShaperRunHandler = interface(ISkObject)
    ['{A2C13D75-D849-462A-A729-A8364605C695}']
  end;

  { TSkShaperRunHandler }

  TSkShaperRunHandler = class abstract(TSkObject, ISkShaperRunHandler)
  strict protected
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSkShaperRunHandlerBaseClass }

  TSkShaperRunHandlerBaseClass = class abstract(TSkShaperRunHandler)
  strict protected
    procedure BeginLine; virtual; abstract;
    procedure CommitLine; virtual; abstract;
    procedure CommitRunBuffer(const AInfo: TSkShaperRunInfo); virtual; abstract;
    procedure CommitRunInfo; virtual; abstract;
    function RunBuffer(const AInfo: TSkShaperRunInfo): TSkShaperRunBuffer; virtual; abstract;
    procedure RunInfo(const AInfo: TSkShaperRunInfo); virtual; abstract;
    class constructor Create;
    class procedure begin_line_proc(context: Pointer); cdecl; static;
    class procedure commit_line_proc(context: Pointer); cdecl; static;
    class procedure commit_run_buffer_proc(context: Pointer; const info: psk_shaperruninfo_t); cdecl; static;
    class procedure commit_run_info_proc(context: Pointer); cdecl; static;
    class procedure run_buffer_proc(context: Pointer; const info: psk_shaperruninfo_t; out result: sk_shaperrunbuffer_t); cdecl; static;
    class procedure run_info_proc(context: Pointer; const info: psk_shaperruninfo_t); cdecl; static;
  public
    constructor Create;
  end;

  { ISkTextBlobBuilderRunHandler }

  ISkTextBlobBuilderRunHandler = interface(ISkShaperRunHandler)
    ['{811E161D-F607-4144-A46A-1A1ED1A172B2}']
    function Detach: ISkTextBlob;
    function GetEndPoint: TPointF;
    property EndPoint: TPointF read GetEndPoint;
  end;

  { TSkTextBlobBuilderRunHandler }

  TSkTextBlobBuilderRunHandler = class(TSkShaperRunHandler, ISkTextBlobBuilderRunHandler)
  strict protected
    function Detach: ISkTextBlob;
    function GetEndPoint: TPointF;
  public
    constructor Create(const AText: string; const AOffset: TPointF);
  end;

  { ISkShaper }

  ISkShaper = interface(ISkObject)
    ['{00C65408-448D-402B-947A-86F2EFC10324}']
    procedure Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AHandler: ISkShaperRunHandler);
  end;

  { TSkShaper }

  TSkShaper = class(TSkObject, ISkShaper)
  strict protected
    procedure Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AHandler: ISkShaperRunHandler);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkPaintStyle = (Fill, Stroke, StrokeAndFill);

  TSkStrokeCap = (Butt, Round, Square);

  TSkStrokeJoin = (Miter, Round, Bevel);

  { ISkPaint }

  ISkPaint = interface(ISkObject)
    ['{2E5F2D19-D285-4924-A52A-37AD6730FE5B}']
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlendMode: TSkBlendMode;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISkColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISkPath): ISkPath; overload;
    function GetFillPath(const APath: ISkPath; const ACullRect: TRectF; const AResScale: Single = 1): ISkPath; overload;
    function GetFilterQuality: TSkFilterQuality;
    function GetImageFilter: ISkImageFilter;
    function GetMaskFilter: ISkMaskFilter;
    function GetPathEffect: ISkPathEffect;
    function GetShader: ISkShader;
    function GetStrokeCap: TSkStrokeCap;
    function GetStrokeJoin: TSkStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSkPaintStyle;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlendMode(const AMode: TSkBlendMode);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF); overload;
    procedure SetColorF(const AValue: TAlphaColorF; const AColorSpace: ISkColorSpace); overload;
    procedure SetColorFilter(const AValue: ISkColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetFilterQuality(const AValue: TSkFilterQuality);
    procedure SetImageFilter(const AValue: ISkImageFilter);
    procedure SetMaskFilter(const AValue: ISkMaskFilter);
    procedure SetPathEffect(const AValue: ISkPathEffect);
    procedure SetShader(const AValue: ISkShader);
    procedure SetStrokeCap(const AValue: TSkStrokeCap);
    procedure SetStrokeJoin(const AValue: TSkStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSkPaintStyle);
    property Alpha: Byte read GetAlpha write SetAlpha;
    property AlphaF: Single read GetAlphaF write SetAlphaF;
    property AntiAlias: Boolean read GetAntiAlias write SetAntiAlias;
    property BlendMode: TSkBlendMode read GetBlendMode write SetBlendMode;
    property Color: TAlphaColor read GetColor write SetColor;
    property ColorF: TAlphaColorF read GetColorF write SetColorF;
    property ColorFilter: ISkColorFilter read GetColorFilter write SetColorFilter;
    property Dither: Boolean read GetDither write SetDither;
    property FilterQuality: TSkFilterQuality read GetFilterQuality write SetFilterQuality;
    property ImageFilter: ISkImageFilter read GetImageFilter write SetImageFilter;
    property MaskFilter: ISkMaskFilter read GetMaskFilter write SetMaskFilter;
    property PathEffect: ISkPathEffect read GetPathEffect write SetPathEffect;
    property Shader: ISkShader read GetShader write SetShader;
    property StrokeCap: TSkStrokeCap read GetStrokeCap write SetStrokeCap;
    property StrokeJoin: TSkStrokeJoin read GetStrokeJoin write SetStrokeJoin;
    property StrokeMiter: Single read GetStrokeMiter write SetStrokeMiter;
    property StrokeWidth: Single read GetStrokeWidth write SetStrokeWidth;
    property Style: TSkPaintStyle read GetStyle write SetStyle;
  end;

  { TSkPaint }

  TSkPaint = class(TSkObject, ISkPaint)
  strict protected
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlendMode: TSkBlendMode;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISkColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISkPath): ISkPath; overload;
    function GetFillPath(const APath: ISkPath; const ACullRect: TRectF; const AResScale: Single = 1): ISkPath; overload;
    function GetFilterQuality: TSkFilterQuality;
    function GetImageFilter: ISkImageFilter;
    function GetMaskFilter: ISkMaskFilter;
    function GetPathEffect: ISkPathEffect;
    function GetShader: ISkShader;
    function GetStrokeCap: TSkStrokeCap;
    function GetStrokeJoin: TSkStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSkPaintStyle;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlendMode(const AMode: TSkBlendMode);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF); overload;
    procedure SetColorF(const AValue: TAlphaColorF; const AColorSpace: ISkColorSpace); overload;
    procedure SetColorFilter(const AValue: ISkColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetFilterQuality(const AValue: TSkFilterQuality);
    procedure SetImageFilter(const AValue: ISkImageFilter);
    procedure SetMaskFilter(const AValue: ISkMaskFilter);
    procedure SetPathEffect(const AValue: ISkPathEffect);
    procedure SetShader(const AValue: ISkShader);
    procedure SetStrokeCap(const AValue: TSkStrokeCap);
    procedure SetStrokeJoin(const AValue: TSkStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSkPaintStyle);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APaint: ISkPaint); overload;
  end;

  TSkLatticeRectType = (Default, Transparent, FixedColor);

  { TSkLattice }

  TSkLattice = record
    XDivs: TArray<Integer>;
    YDivs: TArray<Integer>;
    RectTypes: TArray<TSkLatticeRectType>;
    Bounds: TArray<TRect>;
    Colors: TArray<TAlphaColor>;
  end;

  TSkClipOp = (Difference, Intersect);

  TSkDrawPointsMode = (Points, Lines, Polygon);

  TSkPatchColors = array[0..3] of TAlphaColor;

  TSkPatchCubics = array[0..11] of TPointF;

  TSkPatchTexCoords = array[0..3] of TPointF;

  TSkSrcRectConstraint = (Close, Fast);

  { ISkCanvas }

  ISkCanvas = interface(ISkObject)
    ['{7ED84E94-1490-4E8E-8649-E926B6EC8C57}']
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISkPath; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISkRoundRect; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISkShader; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue: TBytes);
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure DrawImageLattice(const AImage: ISkImage; const ALattice: TSkLattice; const ADest: TRectF; const APaint: ISkPaint = nil);
    procedure DrawImageNine(const AImage: ISkImage; const ACenter: TRect; const ADest: TRectF; const APaint: ISkPaint = nil);
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISkPaint);  overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISkPaint);  overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISkPaint);
    procedure DrawPaint(const APaint: ISkPaint);
    procedure DrawPatch(const ACubics: TSkPatchCubics; const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords; const APaint: ISkPaint; const ABlendMode: TSkBlendMode = TSkBlendMode.Modulate);
    procedure DrawPath(const APath: ISkPath; const APaint: ISkPaint);
    procedure DrawPicture(const APicture: ISkPicture); overload;
    procedure DrawPicture(const APicture: ISkPicture; const AMatrix: TMatrix; const APaint: ISkPaint); overload;
    procedure DrawPoint(const APoint: TPointF; const APaint: ISkPaint); overload;
    procedure DrawPoint(const AX, AY: Single; const APaint: ISkPaint); overload;
    procedure DrawPoints(const AMode: TSkDrawPointsMode; const APoints: TArray<TPointF>; const APaint: ISkPaint);
    procedure DrawRect(const ARect: TRectF; const APaint: ISkPaint);
    procedure DrawRegion(const ARegion: ISkRegion; const APaint: ISkPaint);
    procedure DrawRoundRect(const ARoundRect: ISkRoundRect; const APaint: ISkPaint); overload;
    procedure DrawRoundRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single; const APaint: ISkPaint); overload;
    procedure DrawRoundRectDifference(const AOuter, AInner: ISkRoundRect; const APaint: ISkPaint); overload;
    procedure DrawSimpleText(const AText: string; const AX, AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
    procedure DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX, AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
    procedure DrawTextBlob(const ATextBlob: ISkTextBlob; const AX, AY: Single; const APaint: ISkPaint); overload;
    procedure DrawVertices(const AVertices: ISkVertices; const APaint: ISkPaint; const ABlendMode: TSkBlendMode = TSkBlendMode.Modulate);
    function FindMarkedCTM(const AName: string; out AMatrix: TMatrix3D): Boolean;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function IsClipEmpty: Boolean;
    function IsClipRect: Boolean;
    procedure MarkCTM(const AName: string);
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure Save;
    procedure SaveLayer(const APaint: ISkPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const SX, SY: Single);
    procedure Skew(const AKX, AKY: Single);
    procedure Translate(const DX, DY: Single);
  end;

  { TSkCanvas }

  TSkCanvas = class(TSkObject, ISkCanvas)
  strict protected
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISkPath; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISkRoundRect; const AOp: TSkClipOp = TSkClipOp.Intersect; const DoAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISkShader; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue: TBytes);
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure DrawImageLattice(const AImage: ISkImage; const ALattice: TSkLattice; const ADest: TRectF; const APaint: ISkPaint = nil);
    procedure DrawImageNine(const AImage: ISkImage; const ACenter: TRect; const ADest: TRectF; const APaint: ISkPaint = nil);
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISkPaint); overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISkPaint); overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISkPaint);
    procedure DrawPaint(const APaint: ISkPaint);
    procedure DrawPatch(const ACubics: TSkPatchCubics; const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords; const APaint: ISkPaint; const ABlendMode: TSkBlendMode = TSkBlendMode.Modulate);
    procedure DrawPath(const APath: ISkPath; const APaint: ISkPaint);
    procedure DrawPicture(const APicture: ISkPicture); overload;
    procedure DrawPicture(const APicture: ISkPicture; const AMatrix: TMatrix; const APaint: ISkPaint); overload;
    procedure DrawPoint(const APoint: TPointF; const APaint: ISkPaint); overload;
    procedure DrawPoint(const AX, AY: Single; const APaint: ISkPaint); overload;
    procedure DrawPoints(const AMode: TSkDrawPointsMode; const APoints: TArray<TPointF>; const APaint: ISkPaint);
    procedure DrawRect(const ARect: TRectF; const APaint: ISkPaint);
    procedure DrawRegion(const ARegion: ISkRegion; const APaint: ISkPaint);
    procedure DrawRoundRect(const ARoundRect: ISkRoundRect; const APaint: ISkPaint); overload;
    procedure DrawRoundRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single; const APaint: ISkPaint); overload;
    procedure DrawRoundRectDifference(const AOuter, AInner: ISkRoundRect; const APaint: ISkPaint); overload;
    procedure DrawSimpleText(const AText: string; const AX, AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
    procedure DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX, AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
    procedure DrawTextBlob(const ATextBlob: ISkTextBlob; const AX, AY: Single; const APaint: ISkPaint); overload;
    procedure DrawVertices(const AVertices: ISkVertices; const APaint: ISkPaint; const ABlendMode: TSkBlendMode = TSkBlendMode.Modulate);
    function FindMarkedCTM(const AName: string; out AMatrix: TMatrix3D): Boolean;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function IsClipEmpty: Boolean;
    function IsClipRect: Boolean;
    procedure MarkCTM(const AName: string);
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure Save;
    procedure SaveLayer(const APaint: ISkPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const SX, SY: Single);
    procedure Skew(const AKX, AKY: Single);
    procedure Translate(const DX, DY: Single);
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSkPDFMetadata }

  TSkPDFMetadata = record
    Title: string;
    Author: string;
    Subject: string;
    Keywords: string;
    Creator: string;
    Producer: string;
    Creation: TDateTime;
    Modified: TDateTime;
    RasterDPI: Single;
    PDFA: Boolean;
    EncodingQuality: Integer;
  end;

  { ISkDocument }

  ISkDocument = interface(ISkReferenceCounted)
    ['{E936B2C0-95A0-4577-BC1D-251768B049C6}']
    function BeginPage(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISkCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
  end;

  { TSkDocument }

  TSkDocument = class(TSkReferenceCounted, ISkDocument)
  strict protected
    function BeginPage(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISkCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
  end;

  { TSkPDFDocument }

  TSkPDFDocument = class(TSkDocument)
  strict private
    FStream: ISkNativeObject;
  public
    constructor Create(const AStream: TStream); overload;
    constructor Create(const AStream: TStream; const AMetadata: TSkPDFMetadata); overload;
  end;

  { TSkGraphics }

  TSkGraphics = record
    class procedure AllowJIT; static;
    class procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump); static;
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

  { ISkSVGDOM }

  ISkSVGDOM = interface(ISkReferenceCounted)
    ['{9509D579-27CC-400D-A289-888E7061B7D0}']
    function GetContainerSize: TSizeF;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const AValue: TSizeF);
    property ContainerSize: TSizeF read GetContainerSize write SetContainerSize;
  end;

  { TSkSVGDOM }

  TSkSVGDOM = class(TSkReferenceCounted, ISkSVGDOM)
  strict protected
    function GetContainerSize: TSizeF;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const AValue: TSizeF);
  public
    class function Make(const AStream: TStream): ISkSVGDOM; static;
  end;

  TSkottieAnimationRenderFlag  = (SkipTopLevelIsolation, DisableTopLevelClipping);
  TSkottieAnimationRenderFlags = set of TSkottieAnimationRenderFlag;

  { ISkottieAnimation }

  ISkottieAnimation = interface(ISkReferenceCounted)
    ['{EE7A640C-5D3B-4EEF-AEBF-3A125ACD5B3A}']
    function GetDuration: Double;
    function GetFPS: Double;
    function GetInPoint: Double;
    function GetOutPoint: Double;
    function GetSize: TSizeF;
    function GetVersion: string;
    procedure Render(const ACanvas: ISkCanvas; const ARenderFlags: TSkottieAnimationRenderFlags = []); overload;
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const ARenderFlags: TSkottieAnimationRenderFlags = []); overload;
    procedure SeekFrame(const ATick: Double);
    procedure SeekFrameTime(const ATick: Double);
    property Duration: Double read GetDuration;
    property FPS: Double read GetFPS;
    property InPoint: Double read GetInPoint;
    property OutPoint: Double read GetOutPoint;
    property Size: TSizeF read GetSize;
    property Version: string read GetVersion;

  end;

  { TSkottieAnimation }

  TSkottieAnimation = class(TSkReferenceCounted, ISkottieAnimation)
  strict protected
    function GetDuration: Double;
    function GetFPS: Double;
    function GetInPoint: Double;
    function GetOutPoint: Double;
    function GetSize: TSizeF;
    function GetVersion: string;
    procedure Render(const ACanvas: ISkCanvas; const ARenderFlags: TSkottieAnimationRenderFlags = []); overload;
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const ARenderFlags: TSkottieAnimationRenderFlags = []); overload;
    procedure SeekFrame(const ATick: Double);
    procedure SeekFrameTime(const ATick: Double);
    class function DoIsUnique(const AHandle: THandle): Boolean; override;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    class function Make(const AData: string): ISkottieAnimation; static;
    class function MakeFromFile(const AFileName: string): ISkottieAnimation; static;
    class function MakeFromStream(const AStream: TStream): ISkottieAnimation; static;
  end;

  { ISkParagraphCache }

  ISkParagraphCache = interface(ISkObject)
    ['{1DC50E80-A117-4432-9D72-A657044BB054}']
    procedure Reset;
    procedure TurnOn(const AValue: Boolean);
  end;

  { TSkParagraphCache }

  TSkParagraphCache = class(TSkObject, ISkParagraphCache)
  strict protected
    procedure Reset;
    procedure TurnOn(const AValue: Boolean);
  end;

  { ISkFontCollection }

  ISkFontCollection = interface(ISkReferenceCounted)
    ['{ACB2F510-FAB3-4CA2-A446-F7160129EF81}']
    function GetFontFallBack: Boolean;
    function GetParagraphCache: ISkParagraphCache;
    procedure SetFontFallBack(const AValue: Boolean);
    property FontFallBack: Boolean read GetFontFallBack write SetFontFallBack;
  end;

  { TSkFontCollection }

  TSkFontCollection = class(TSkReferenceCounted, ISkFontCollection)
  strict protected
    function GetFontFallBack: Boolean;
    function GetParagraphCache: ISkParagraphCache;
    procedure SetFontFallBack(const AValue: Boolean);
  public
    constructor Create;
  end;

  TSkTextBaseline = (Alphabetic, Ideographic);

  TSkTextDecoration  = (Underline, Overline, LineThrough);
  TSkTextDecorations = set of TSkTextDecoration;

  TSkTextDecorationMode = (Gaps, Through);

  TSkTextDecorationStyle = (Solid, Double, Dotted, Dashed, Wavy);

  { TSkTextShadow }

  TSkTextShadow = record
    Color: TAlphaColor;
    Offset: TPointF;
    BlurRadius: Double;
  end;

  { ISkTextStyle }

  ISkTextStyle = interface(ISkObject)
    ['{102CBE75-B4C7-4980-ACEF-8E9CFDC5D225}']
    procedure AddFontFeature(const AFeature: string; const AValue: Integer);
    procedure AddShadow(const AShadow: TSkTextShadow);
    procedure ClearBackgroundColor;
    procedure ClearForegroundColor;
    function GetBackground: ISkPaint;
    function GetColor: TAlphaColor;
    function GetDecorationColor: TAlphaColor;
    function GetDecorationMode: TSkTextDecorationMode;
    function GetDecorations: TSkTextDecorations;
    function GetDecorationStyle: TSkTextDecorationStyle;
    function GetDecorationThicknessMultiplier: Single;
    function GetFontFamilies: TArray<string>;
    function GetFontMetrics: TSkFontMetrics;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForeground: ISkPaint;
    function GetHeight: Single;
    function GetHeightOverride: Boolean;
    function GetLetterSpacing: Single;
    function GetLocale: string;
    function GetTextBaseline: TSkTextBaseline;
    function GetTypeface: ISkTypeface;
    function GetWordSpacing: Single;
    function HasBackground: Boolean;
    function HasForeground: Boolean;
    function IsEqual(const ATextStyle: ISkTextStyle): Boolean;
    function IsPlaceholder: Boolean;
    procedure ResetFontFeatures;
    procedure ResetShadows;
    procedure SetBackgroundColor(const APaint: ISkPaint);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetDecorationColor(const AValue: TAlphaColor);
    procedure SetDecorationMode(const AValue: TSkTextDecorationMode);
    procedure SetDecorations(const AValue: TSkTextDecorations);
    procedure SetDecorationStyle(const AValue: TSkTextDecorationStyle);
    procedure SetDecorationThicknessMultiplier(const AValue: Single);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForegroundColor(const APaint: ISkPaint);
    procedure SetHeight(const AValue: Single);
    procedure SetHeightOverride(const AValue: Boolean);
    procedure SetLetterSpacing(const AValue: Single);
    procedure SetLocale(const AValue: string);
    procedure SetPlaceholder;
    procedure SetTextBaseline(const AValue: TSkTextBaseline);
    procedure SetTypeface(const AValue: ISkTypeface);
    procedure SetWordSpacing(const AValue: Single);
    property Color: TAlphaColor read GetColor write SetColor;
    property DecorationColor: TAlphaColor read GetDecorationColor write SetDecorationColor;
    property DecorationMode: TSkTextDecorationMode read GetDecorationMode write SetDecorationMode;
    property Decorations: TSkTextDecorations read GetDecorations write SetDecorations;
    property DecorationStyle: TSkTextDecorationStyle read GetDecorationStyle write SetDecorationStyle;
    property DecorationThicknessMultiplier: Single read GetDecorationThicknessMultiplier write SetDecorationThicknessMultiplier;
    property FontFamilies: TArray<string> read GetFontFamilies write SetFontFamilies;
    property FontMetrics: TSkFontMetrics read GetFontMetrics;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStyle: TSkFontStyle read GetFontStyle write SetFontStyle;
    property Height: Single read GetHeight write SetHeight;
    property HeightOverride: Boolean read GetHeightOverride write SetHeightOverride;
    property LetterSpacing: Single read GetLetterSpacing write SetLetterSpacing;
    property Locale: string read GetLocale write SetLocale;
    property TextBaseline: TSkTextBaseline read GetTextBaseline write SetTextBaseline;
    property Typeface: ISkTypeface read GetTypeface write SetTypeface;
    property WordSpacing: Single read GetWordSpacing write SetWordSpacing;
  end;

  { TSkTextStyle }

  TSkTextStyle = class(TSkObject, ISkTextStyle)
  strict protected
    procedure AddFontFeature(const AFeature: string; const AValue: Integer);
    procedure AddShadow(const AShadow: TSkTextShadow);
    procedure ClearBackgroundColor;
    procedure ClearForegroundColor;
    function GetBackground: ISkPaint;
    function GetColor: TAlphaColor;
    function GetDecorationColor: TAlphaColor;
    function GetDecorationMode: TSkTextDecorationMode;
    function GetDecorations: TSkTextDecorations;
    function GetDecorationStyle: TSkTextDecorationStyle;
    function GetDecorationThicknessMultiplier: Single;
    function GetFontFamilies: TArray<string>;
    function GetFontMetrics: TSkFontMetrics;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForeground: ISkPaint;
    function GetHeight: Single;
    function GetHeightOverride: Boolean;
    function GetLetterSpacing: Single;
    function GetLocale: string;
    function GetTextBaseline: TSkTextBaseline;
    function GetTypeface: ISkTypeface;
    function GetWordSpacing: Single;
    function HasBackground: Boolean;
    function HasForeground: Boolean;
    function IsEqual(const ATextStyle: ISkTextStyle): Boolean;
    function IsPlaceholder: Boolean;
    procedure ResetFontFeatures;
    procedure ResetShadows;
    procedure SetBackgroundColor(const APaint: ISkPaint);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetDecorationColor(const AValue: TAlphaColor);
    procedure SetDecorationMode(const AValue: TSkTextDecorationMode);
    procedure SetDecorations(const AValue: TSkTextDecorations);
    procedure SetDecorationStyle(const AValue: TSkTextDecorationStyle);
    procedure SetDecorationThicknessMultiplier(const AValue: Single);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForegroundColor(const APaint: ISkPaint);
    procedure SetHeight(const AValue: Single);
    procedure SetHeightOverride(const AValue: Boolean);
    procedure SetLetterSpacing(const AValue: Single);
    procedure SetLocale(const AValue: string);
    procedure SetPlaceholder;
    procedure SetTextBaseline(const AValue: TSkTextBaseline);
    procedure SetTypeface(const AValue: ISkTypeface);
    procedure SetWordSpacing(const AValue: Single);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { ISkStrutStyle }

  ISkStrutStyle = interface(ISkObject)
    ['{7BEA98AE-64CA-4B3A-AD10-4173B9DF5822}']
    function GetEnabled: Boolean;
    function GetFontFamilies: TArray<string>;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForceHeight: Boolean;
    function GetHeight: Single;
    function GetHeightOverride: Boolean;
    function GetLeading: Single;
    function IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForceHeight(const AValue: Boolean);
    procedure SetHeight(const AValue: Single);
    procedure SetHeightOverride(const AValue: Boolean);
    procedure SetLeading(const AValue: Single);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FontFamilies: TArray<string> read GetFontFamilies write SetFontFamilies;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStyle: TSkFontStyle read GetFontStyle write SetFontStyle;
    property ForceHeight: Boolean read GetForceHeight write SetForceHeight;
    property Height: Single read GetHeight write SetHeight;
    property HeightOverride: Boolean read GetHeightOverride write SetHeightOverride;
    property Leading: Single read GetLeading write SetLeading;
  end;

  { TSkStrutStyle }

  TSkStrutStyle = class(TSkObject, ISkStrutStyle)
  strict protected
    function GetEnabled: Boolean;
    function GetFontFamilies: TArray<string>;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForceHeight: Boolean;
    function GetHeight: Single;
    function GetHeightOverride: Boolean;
    function GetLeading: Single;
    function IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForceHeight(const AValue: Boolean);
    procedure SetHeight(const AValue: Single);
    procedure SetHeightOverride(const AValue: Boolean);
    procedure SetLeading(const AValue: Single);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkDrawOptions = (Registry, Replay, Direct);

  TSkTextAlign = (Left, Right, Center, Justify, Start, Terminate);

  TSkTextDirection = (RightToLeft, LeftToRight);

  TSkTextHeightBehavior  = (DisableFirstAscent, DisableLastDescent);
  TSkTextHeightBehaviors = set of TSkTextHeightBehavior;

  { ISkParagraphStyle }

  ISkParagraphStyle = interface(ISkObject)
    ['{1C9937A3-5844-4AFD-9C5A-CC7799C13247}']
    function GetDrawOptions: TSkDrawOptions;
    function GetEffectiveAlign: TSkTextAlign;
    function GetEllipsis: string;
    function GetHeight: Single;
    function GetMaxLines: NativeUInt;
    function GetStrutStyle: ISkStrutStyle;
    function GetTextAlign: TSkTextAlign;
    function GetTextDirection: TSkTextDirection;
    function GetTextHeightBehaviors: TSkTextHeightBehaviors;
    function GetTextStyle: ISkTextStyle;
    function HasUnlimitedLines: Boolean;
    function IsEllipsized: Boolean;
    function IsEqual(const AParagraphStyle: ISkParagraphStyle): Boolean;
    function IsHintingOn: Boolean;
    procedure SetDrawOptions(const AValue: TSkDrawOptions);
    procedure SetEllipsis(const AValue: string);
    procedure SetHeight(const AValue: Single);
    procedure SetMaxLines(const AValue: NativeUInt);
    procedure SetStrutStyle(const AValue: ISkStrutStyle);
    procedure SetTextAlign(const AValue: TSkTextAlign);
    procedure SetTextDirection(const AValue: TSkTextDirection);
    procedure SetTextHeightBehaviors(const AValue: TSkTextHeightBehaviors);
    procedure SetTextStyle(const AValue: ISkTextStyle);
    procedure TurnHintingOff;
    property DrawOptions: TSkDrawOptions read GetDrawOptions write SetDrawOptions;
    property EffectiveAlign: TSkTextAlign read GetEffectiveAlign;
    property Ellipsis: string read GetEllipsis write SetEllipsis;
    property Height: Single read GetHeight write SetHeight;
    property MaxLines: NativeUInt read GetMaxLines write SetMaxLines;
    property StrutStyle: ISkStrutStyle read GetStrutStyle write SetStrutStyle;
    property TextAlign: TSkTextAlign read GetTextAlign write SetTextAlign;
    property TextDirection: TSkTextDirection read GetTextDirection write SetTextDirection;
    property TextHeightBehaviors: TSkTextHeightBehaviors read GetTextHeightBehaviors write SetTextHeightBehaviors;
    property TextStyle: ISkTextStyle read GetTextStyle write SetTextStyle;
  end;

  { TSkParagraphStyle }

  TSkParagraphStyle = class(TSkObject, ISkParagraphStyle)
  strict protected
    function GetDrawOptions: TSkDrawOptions;
    function GetEffectiveAlign: TSkTextAlign;
    function GetEllipsis: string;
    function GetHeight: Single;
    function GetMaxLines: NativeUInt;
    function GetStrutStyle: ISkStrutStyle;
    function GetTextAlign: TSkTextAlign;
    function GetTextDirection: TSkTextDirection;
    function GetTextHeightBehaviors: TSkTextHeightBehaviors;
    function GetTextStyle: ISkTextStyle;
    function HasUnlimitedLines: Boolean;
    function IsEllipsized: Boolean;
    function IsEqual(const AParagraphStyle: ISkParagraphStyle): Boolean;
    function IsHintingOn: Boolean;
    procedure SetDrawOptions(const AValue: TSkDrawOptions);
    procedure SetEllipsis(const AValue: string);
    procedure SetHeight(const AValue: Single);
    procedure SetMaxLines(const AValue: NativeUInt);
    procedure SetStrutStyle(const AValue: ISkStrutStyle);
    procedure SetTextAlign(const AValue: TSkTextAlign);
    procedure SetTextDirection(const AValue: TSkTextDirection);
    procedure SetTextHeightBehaviors(const AValue: TSkTextHeightBehaviors);
    procedure SetTextStyle(const AValue: ISkTextStyle);
    procedure TurnHintingOff;
    property DrawOptions: TSkDrawOptions read GetDrawOptions write SetDrawOptions;
    property EffectiveAlign: TSkTextAlign read GetEffectiveAlign;
    property StrutStyle: ISkStrutStyle read GetStrutStyle write SetStrutStyle;
    property TextStyle: ISkTextStyle read GetTextStyle write SetTextStyle;
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { TSkTextBox }

  TSkTextBox = record
    Rect: TRectF;
    Direction: TSkTextDirection;
  end;

  TSkRectHeightStyle = (Tight, Max, IncludeLineSpacingMiddle, IncludeLineSpacingTop, IncludeLineSpacingBottom, Strut);

  TSkRectWidthStyle = (Tight, Max);

  { ISkParagraph }

  ISkParagraph = interface(ISkObject)
    ['{27462359-56C6-41DF-9D15-840E1DEC25C9}']
    function GetHeight: Single;
    function GetMaxWidth: Single;
    function GetRectsForRange(const AFirst, ALast: Cardinal; const ARectHeightStyle: TSkRectHeightStyle; const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
    procedure GetWordBoundary(const AOffset: Cardinal; out AFirst, ALast: Cardinal);
    procedure Layout(const AWidth: Single);
    procedure Render(const ACanvas: ISkCanvas; const AX, AY: Single);
    property Height: Single read GetHeight;
    property MaxWidth: Single read GetMaxWidth;
  end;

  { TSkParagraph }

  TSkParagraph = class(TSkObject, ISkParagraph)
  strict protected
    function GetHeight: Single;
    function GetMaxWidth: Single;
    function GetRectsForRange(const AFirst, ALast: Cardinal; const ARectHeightStyle: TSkRectHeightStyle; const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
    procedure GetWordBoundary(const AOffset: Cardinal; out AFirst, ALast: Cardinal);
    procedure Layout(const AWidth: Single);
    procedure Render(const ACanvas: ISkCanvas; const AX, AY: Single);
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  TSkPlaceholderAlignment = (Baseline, AboveBaseline, BelowBaseline, Top, Bottom, Middle);

  { TSkPlaceholderStyle }

  TSkPlaceholderStyle = record
    Width: Single;
    Height: Single;
    Alignment: TSkPlaceholderAlignment;
    Baseline: TSkTextBaseline;
    BaselineOffset: Single;
    constructor Create(const AWidth, AHeight: Single; const AAlignment: TSkPlaceholderAlignment; const ABaseline: TSkTextBaseline; const ABaselineOffset: Single);
    class operator Equal(const APlaceholderStyle1, APlaceholderStyle2: TSkPlaceholderStyle): Boolean;
    class operator NotEqual(const APlaceholderStyle1, APlaceholderStyle2: TSkPlaceholderStyle): Boolean;
  end;

  { ISkParagraphBuilder }

  ISkParagraphBuilder = interface(ISkObject)
    ['{6E93A66D-0529-47A6-A994-CA309A5A9A45}']
    procedure Pop;
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Detach: ISkParagraph;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
  end;

  { TSkParagraphBuilder }

  TSkParagraphBuilder = class(TSkObject, ISkParagraphBuilder)
  strict protected
    procedure Pop;
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Detach: ISkParagraph;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create(const AParagraphStyle: ISkParagraphStyle); overload;
    constructor Create(const AParagraphStyle: ISkParagraphStyle; const AFontCollection: ISkFontCollection); overload;
  end;

implementation

uses
  { Delphi }

  System.Math,
  System.TimeSpan;

type
  { TSkImageInfoHelper }

  TSkImageInfoHelper = record helper for sk_imageinfo_t
    procedure FromImageInfo(const AValue: TSkImageInfo); inline;
    function ToImageInfo: TSkImageInfo; inline;
  end;

  {$IFDEF SK_METAL}

  { TGRMTLBackendContextHelper }

  TGRMTLBackendContextHelper = record helper for gr_mtl_backendcontext_t
    procedure FromGRMTLBackendContext(const AValue: TGRMTLBackendContext); inline;
  end;

  { TGRMTLTextureInfoHelper }

  TGRMTLTextureInfoHelper = record helper for gr_mtl_textureinfo_t
    procedure FromGRMTLTextureInfo(const AValue: TGRMTLTextureInfo); inline;
  end;

  {$ENDIF}

  { TSkFontMetricsHelper }

  TSkFontMetricsHelper = record helper for sk_fontmetrics_t
    function ToFontMetrics: TSkFontMetrics; inline;
  end;

  { TSkRunBufferHelper }

  TSkRunBufferHelper = record helper for sk_runbuffer_t
    function ToRunBuffer(const ACount: Integer): TSkRunBuffer; inline;
  end;

  { TSkShaperRunInfoHelper }

  TSkShaperRunInfoHelper = record helper for sk_shaperruninfo_t
    function ToShaperRunInfo: TSkShaperRunInfo; inline;
  end;

  { TSkLatticeHelper }

  TSkLatticeHelper = record helper for sk_lattice_t
    procedure FromLattice(const AValue: TSkLattice); inline;
  end;

  { TSkDateTimeHelper }

  TSkDateTimeHelper = record helper for sk_datetime_t
    procedure FromDateTime(const AValue: TDateTime); inline;
  end;

  { TSkPDFMetadataHelper }

  TSkPDFMetadataHelper = record helper for sk_pdfmetadata_t
    procedure FromPDFMetadata(const AValue: TSkPDFMetadata); inline;
  end;

  { ISkString }

  ISkString = interface(ISkObject)
    ['{88437646-E5F0-4C53-912F-917F29928730}']
    function GetText: string;
    procedure SetText(const AValue: string);
    property Text: string read GetText write SetText;
  end;

  { TSkString }

  TSkString = class(TSkObject, ISkString)
  strict protected
    function GetText: string;
    procedure SetText(const AValue: string);
    class procedure DoDestroy(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const AString: string); overload;
  end;

  { ISkStream }

  ISkStream = interface(ISkObject)
    ['{ACBA8267-D2C8-413C-A1A2-06E2954DBFDB}']
  end;

  { TSkStream }

  TSkStream = class(TSkObject, ISkStream)
  strict protected
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { ISkStreamAsset }

  ISkStreamAsset = interface(ISkStream)
    ['{3EAAE979-C079-4A93-A01F-3A1FF517738E}']
  end;

  { TSkStreamAsset }

  TSkStreamAsset = class(TSkStream, ISkStreamAsset);

  { TSkManagedStream }

  TSkManagedStream = class(TSkStreamAsset)
  strict private
    class constructor Create;
    class function get_length_proc(context: Pointer): size_t; cdecl; static;
    class function get_position_proc(context: Pointer): size_t; cdecl; static;
    class function read_proc(context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl; static;
    class function seek_proc(context: Pointer; position: size_t): bool; cdecl; static;
  public
    constructor Create(const AStream: TStream);
  end;

  { ISkWStream }

  ISkWStream = interface(ISkObject)
    ['{FCF77D49-1CAD-4C4F-988F-5780B7E73CEC}']
  end;

  { TSkWStream }

  TSkWStream = class(TSkObject, ISkWStream)
  strict protected
    class procedure DoDestroy(const AHandle: THandle); override;
  end;

  { TSkManagedWStream }

  TSkManagedWStream = class(TSkWStream)
  strict private
    class constructor Create;
    class function write_proc(context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl; static;
  public
    constructor Create(const AStream: TStream);
  end;

  { ISkData }

  ISkData = interface(ISkReferenceCounted)
    ['{8EAFD037-E378-4B42-A3AF-0B5E9826CBD3}']
    function GetData: Pointer;
    function GetSize: NativeUInt;
    procedure SaveToStream(const AStream: TStream);
    function ToBytes: TBytes;
    property Data: Pointer read GetData;
    property Size: NativeUInt read GetSize;
  end;

  { TSkData }

  TSkData = class(TSkReferenceCounted, ISkData)
  strict protected
    function GetData: Pointer;
    function GetSize: NativeUInt;
    procedure SaveToStream(const AStream: TStream);
    function ToBytes: TBytes;
    class function DoIsUnique(const AHandle: THandle): Boolean; override;
    class procedure DoRef(const AHandle: THandle); override;
    class procedure DoUnref(const AHandle: THandle); override;
  public
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(const AData: Pointer; const ASize: NativeUInt); overload;
    constructor Create(const AStream: TStream); overload;
    constructor Create(const AStream: TStream; const ASize: NativeUInt); overload;
  end;

const
  ArgumentOutOfRange       = 'Argument out of range.';
  CannotBeEmpty            = '%s cannot be empty.';
  DestructorNotImplemented = '%s destructor was not implemented.';
  ElementsCannotBeNil      = '%s elements cannot be nil.';
  EvenNumberOfEntries      = '%s must have an even number of entries.';
  MatchDimensions          = '%s length must match the %s dimensions.';
  MatchLength              = '%s length must match the %s length.';
  MustBeInRange            = '%s must be in the range of %d-%d.';
  ParameterCannotBeNil     = '%s cannot be nil.';

{ TSkImageInfoHelper }

procedure TSkImageInfoHelper.FromImageInfo(const AValue: TSkImageInfo);
begin
  width      := AValue.Width;
  height     := AValue.Height;
  colortype  := sk_colortype_t(AValue.ColorType);
  alphatype  := sk_alphatype_t(AValue.AlphaType);
  colorspace := TSkNativeObject.SafeHandle(AValue.ColorSpace);
end;

function TSkImageInfoHelper.ToImageInfo: TSkImageInfo;
begin
  Result.Width     := width;
  Result.Height    := height;
  Result.ColorType := TSkColorType(colortype);
  Result.AlphaType := TSkAlphaType(alphatype);
  if colorspace = 0 then
    Result.ColorSpace := nil
  else
    Result.ColorSpace := TSkColorSpace.CreateNative(colorspace);
end;

{$IFDEF SK_METAL}

{ TGRMTLBackendContextHelper }

procedure TGRMTLBackendContextHelper.FromGRMTLBackendContext(
  const AValue: TGRMTLBackendContext);
begin
  device := NSObjectToID(AValue.Device);
  queue  := NSObjectToID(AValue.Queue);
end;

{ TGRMTLTextureInfoHelper }

procedure TGRMTLTextureInfoHelper.FromGRMTLTextureInfo(
  const AValue: TGRMTLTextureInfo);
begin
  texture := NSObjectToID(AValue.Texture);
end;

{$ENDIF}

{ TSkFontMetricsHelper }

function TSkFontMetricsHelper.ToFontMetrics: TSkFontMetrics;
begin
  Result.Flags              := TSkFontMetricsFlags(Byte(flags));
  Result.Top                := top;
  Result.Ascent             := descent;
  Result.Descent            := bottom;
  Result.Bottom             := leading;
  Result.Leading            := average_character_width;
  Result.AvgCharWidth       := max_character_width;
  Result.MaxCharWidth       := max_character_width;
  Result.XMin               := x_min;
  Result.XMax               := x_max;
  Result.XHeight            := x_height;
  Result.CapHeight          := cap_height;
  Result.UnderlineThickness := underline_thickness;
  Result.UnderlinePosition  := underline_position;
  Result.StrikeoutThickness := strikeout_thickness;
  Result.StrikeoutPosition  := strikeout_position;
end;

{ TSkRunBufferHelper }

function TSkRunBufferHelper.ToRunBuffer(const ACount: Integer): TSkRunBuffer;
begin
  Result := TSkRunBuffer.Create(glyphs, positions, ACount);
end;

{ TSkShaperRunInfoHelper }

function TSkShaperRunInfoHelper.ToShaperRunInfo: TSkShaperRunInfo;
begin
  Result.Font       := TSkFont.CreateNative(font, False);
  Result.BidiLevel  := bidi_level;
  Result.Advance    := TPointF(advance);
  Result.GlyphCount := glyph_count;
  Result.UTF8Range  := TSkShaperRunRange(utf8_range);
end;

{ TSkLatticeHelper }

procedure TSkLatticeHelper.FromLattice(const AValue: TSkLattice);
begin
  x_divs     := @AValue.XDivs[0];
  y_divs     := @AValue.YDivs[0];
  rect_types := @AValue.RectTypes[0];
  x_count    := Length(AValue.XDivs);
  y_count    := Length(AValue.YDivs);
  bounds     := @sk_irect_t(AValue.Bounds[0]);
  colors     := @AValue.Colors[0];
end;

{ TSkDateTimeHelper }

procedure TSkDateTimeHelper.FromDateTime(const AValue: TDateTime);
var
  LDay: Word;
  LHour: Word;
  LMilliSecond: Word;
  LMinute: Word;
  LMonth: Word;
  LSecond: Word;
  LYear: Word;
begin
  DecodeDateTime(AValue, LYear, LMonth, LDay, LHour, LMinute, LSecond, LMilliSecond);
  time_zone_minutes := Round(TTimeZone.Local.UtcOffset.TotalMinutes);
  year              := LYear;
  month             := LMonth;
  day_of_week       := DayOfWeek(AValue) - 1;
  day               := LDay;
  hour              := LHour;
  minute            := LMinute;
  second            := LSecond;
end;

{ TSkPDFMetadataHelper }

procedure TSkPDFMetadataHelper.FromPDFMetadata(const AValue: TSkPDFMetadata);
begin
  title            := MarshaledAString(UTF8String(AValue.Title));
  author           := MarshaledAString(UTF8String(AValue.Author));
  subject          := MarshaledAString(UTF8String(AValue.Subject));
  keywords         := MarshaledAString(UTF8String(AValue.Keywords));
  creator          := MarshaledAString(UTF8String(AValue.Creator));
  producer         := MarshaledAString(UTF8String(AValue.Producer));
  raster_dpi       := AValue.RasterDPI;
  pdfa             := AValue.PDFA;
  encoding_quality := AValue.EncodingQuality;
  creation.FromDateTime(AValue.Creation);
  modified.FromDateTime(AValue.Modified);
end;

{ TSkString }

constructor TSkString.Create;
begin
  CreateNative(TSkiaApi.sk4d_string_create());
end;

constructor TSkString.Create(const AString: string);
begin
  Create;
  SetText(AString);
end;

class procedure TSkString.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_string_destroy(AHandle);
end;

function TSkString.GetText: string;
begin
  Result := string(TSkiaApi.sk4d_string_get_text(GetHandle));
end;

procedure TSkString.SetText(const AValue: string);
begin
  TSkiaApi.sk4d_string_set_text(GetHandle, MarshaledAString(UTF8String(AValue)));
end;

{ TSkStream }

class procedure TSkStream.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_stream_destroy(AHandle);
end;

{ TSkManagedStream }

constructor TSkManagedStream.Create(const AStream: TStream);
begin
  CreateNative(TSkiaApi.sk4d_managedstream_create(AStream));
end;

class constructor TSkManagedStream.Create;
var
  LProcs: sk_managedstream_procs_t;
begin
  LProcs.get_length   := get_length_proc;
  LProcs.get_position := get_position_proc;
  LProcs.read         := read_proc;
  LProcs.seek         := seek_proc;
  TSkiaApi.sk4d_managedstream_set_procs(@LProcs);
end;

class function TSkManagedStream.get_length_proc(context: Pointer): size_t;
begin
  if context = nil then
    Exit(0);
  Result := TStream(context).Size;
end;

class function TSkManagedStream.get_position_proc(context: Pointer): size_t;
begin
  if context = nil then
    Exit(0);
  Result := TStream(context).Position;
end;

class function TSkManagedStream.read_proc(context, buffer: Pointer;
  size: size_t): size_t;
begin
  if context = nil then
    Exit(0);
  Result := TStream(context).Read(buffer^, size);
end;

class function TSkManagedStream.seek_proc(context: Pointer;
  position: size_t): bool;
begin
  if context = nil then
    Exit(False);
  TStream(context).Position := position;
  Result := True;
end;

{ TSkWStream }

class procedure TSkWStream.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_wstream_destroy(AHandle);
end;

{ TSkManagedWStream }

class constructor TSkManagedWStream.Create;
var
  LProcs: sk_managedwstream_procs_t;
begin
  LProcs.write := write_proc;
  TSkiaApi.sk4d_managedwstream_set_procs(@LProcs);
end;

constructor TSkManagedWStream.Create(const AStream: TStream);
begin
  CreateNative(TSkiaApi.sk4d_managedwstream_create(AStream));
end;

class function TSkManagedWStream.write_proc(context: Pointer;
  const buffer: Pointer; size: size_t): bool;
begin
  if context = nil then
    Exit(False);
  TStream(context).Write(buffer^, size);
  Result := True;
end;

{ TSkData }

constructor TSkData.Create(const ABytes: TBytes);
begin
  Create(@ABytes[0], Length(ABytes));
end;

constructor TSkData.Create(const AData: Pointer; const ASize: NativeUInt);
begin
  CreateNative(TSkiaApi.sk4d_data_make(AData, ASize));
end;

constructor TSkData.Create(const AStream: TStream);
begin
  Create(AStream, AStream.Size - AStream.Position);
end;

constructor TSkData.Create(const AStream: TStream; const ASize: NativeUInt);
var
  LStream: ISkStream;
begin
  LStream := TSkManagedStream.Create(AStream);
  CreateNative(TSkiaApi.sk4d_data_make_from_stream(LStream.Handle, ASize));
end;

class function TSkData.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_data_is_unique(AHandle);
end;

class procedure TSkData.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_data_ref(AHandle);
end;

class procedure TSkData.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_data_unref(AHandle);
end;

function TSkData.GetData: Pointer;
begin
  Result := TSkiaApi.sk4d_data_get_data(GetHandle);
end;

function TSkData.GetSize: NativeUInt;
begin
  Result := TSkiaApi.sk4d_data_get_size(GetHandle);
end;

procedure TSkData.SaveToStream(const AStream: TStream);
begin
  AStream.Write(GetData^, GetSize);
end;

function TSkData.ToBytes: TBytes;
begin
  SetLength(Result, GetSize);
  Move(GetData^, Result[0], Length(Result));
end;

{ TSkVersion }

class function TSkVersion.GetMajorVersion: Integer;
begin
  Result := TSkiaApi.sk4d_version_get_major();
end;

class function TSkVersion.GetMilestone: Integer;
begin
  Result := TSkiaApi.sk4d_version_get_milestone();
end;

class function TSkVersion.GetMinorVersion: Integer;
begin
  Result := TSkiaApi.sk4d_version_get_minor();
end;

{ TSkDebug }

class constructor TSkDebug.Create;
begin
  TSkiaApi.sk4d_set_debug_msg_proc(debug_msg_proc);
end;

class procedure TSkDebug.debug_msg_proc(const msg: MarshaledAString);
begin
  if Assigned(FDebugMessageProc) then
    FDebugMessageProc(string(Msg));
end;

class destructor TSkDebug.Destroy;
begin
  TSkiaApi.sk4d_set_debug_msg_proc(nil);
end;

{ TSkNativeObject }

procedure TSkNativeObject.BeforeDestruction;
begin
  inherited;
  Dispose;
end;

constructor TSkNativeObject.CreateNative(const AHandle: THandle);
begin
  Assert(AHandle <> 0);
  inherited Create;
  FHandle := AHandle;
end;

function TSkNativeObject.GetHandle: THandle;
begin
  Result := FHandle;
end;

class function TSkNativeObject.SafeHandle(
  const AObject: ISkNativeObject): THandle;
begin
  if not Assigned(AObject) then
    Exit(0);
  Result := AObject.Handle;
end;

{ TSkObject }

constructor TSkObject.CreateNative(const AHandle: THandle;
  const AOwnsHandle: Boolean);
begin
  inherited CreateNative(AHandle);
  FOwnsHandle := AOwnsHandle;
end;

procedure TSkObject.Dispose;
begin
  if FOwnsHandle then
    DoDestroy(GetHandle);
end;

class procedure TSkObject.DoDestroy(const AHandle: THandle);
begin
  raise ESkObject.CreateFmt(DestructorNotImplemented, [ClassName]);
end;

function TSkObject.GetOwnsHandle: Boolean;
begin
  Result := FOwnsHandle;
end;

{ TSkReferenceCounted }

constructor TSkReferenceCounted.CreateNative(const AHandle: THandle;
  const AAlreadyReferenced: Boolean);
begin
  inherited CreateNative(AHandle);
  if not AAlreadyReferenced then
    DoRef(GetHandle);
end;

procedure TSkReferenceCounted.DecrementReference;
begin
  DoUnref(GetHandle);
end;

procedure TSkReferenceCounted.Dispose;
begin
  DoUnref(GetHandle);
end;

class function TSkReferenceCounted.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_refcnt_is_unique(AHandle);
end;

class procedure TSkReferenceCounted.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_refcnt_ref(AHandle);
end;

class procedure TSkReferenceCounted.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_refcnt_unref(AHandle);
end;

procedure TSkReferenceCounted.IncrementReference;
begin
  DoRef(GetHandle);
end;

function TSkReferenceCounted.IsUnique: Boolean;
begin
  Result := (RefCount = 1) and DoIsUnique(GetHandle);
end;

{ TSkEnumerator<T> }

constructor TSkEnumerator<T>.Create(const AEnumerable: TSkEnumerable<T>);
begin
  inherited Create;
  FEnumerable := AEnumerable;
end;

function TSkEnumerator<T>.DoGetCurrent: T;
begin
  Result := FEnumerable.GetCurrent;
end;

function TSkEnumerator<T>.DoMoveNext: Boolean;
begin
  Result := FEnumerable.MoveNext;
end;

{ TSkEnumerable<T> }

function TSkEnumerable<T>.GetEnumerator: TSkEnumerator<T>;
begin
  if FRun then
    Reset;
  Result := TSkEnumerator<T>.Create(Self);
  FRun   := True;
end;

procedure TSkEnumerable<T>.Reset;
begin
  raise ESkEnumerable.CreateFmt('%s iterator does not support reset.', [ClassName]);
end;

{ TSkColorSpaceXYZ }

class function TSkColorSpaceXYZ.AdobeRGB: TSkColorSpaceXYZ;
begin
  TSkiaApi.sk4d_colorspacexyz_adobe_rgb(sk_colorspacexyz_t(Result));
end;

procedure TSkColorSpaceXYZ.Concat(const AXYZ: TSkColorSpaceXYZ);
begin
  TSkiaApi.sk4d_colorspacexyz_concat(@sk_colorspacexyz_t(Self), @sk_colorspacexyz_t(AXYZ));
end;

class function TSkColorSpaceXYZ.DisplayP3: TSkColorSpaceXYZ;
begin
  TSkiaApi.sk4d_colorspacexyz_display_p3(sk_colorspacexyz_t(Result));
end;

function TSkColorSpaceXYZ.Invert(out AXYZ: TSkColorSpaceXYZ): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspacexyz_invert(@sk_colorspacexyz_t(Self), sk_colorspacexyz_t(AXYZ));
end;

class function TSkColorSpaceXYZ.Rec2020: TSkColorSpaceXYZ;
begin
  TSkiaApi.sk4d_colorspacexyz_rec_2020(sk_colorspacexyz_t(Result));
end;

class function TSkColorSpaceXYZ.SRGB: TSkColorSpaceXYZ;
begin
  TSkiaApi.sk4d_colorspacexyz_srgb(sk_colorspacexyz_t(Result));
end;

class function TSkColorSpaceXYZ.XYZ: TSkColorSpaceXYZ;
begin
  TSkiaApi.sk4d_colorspacexyz_xyz(sk_colorspacexyz_t(Result));
end;

{ TSkColorSpacePrimaries }

function TSkColorSpacePrimaries.ToXYZ(out AXYZ: TSkColorSpaceXYZ): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspaceprimaries_to_xyz(@sk_colorspaceprimaries_t(Self), sk_colorspacexyz_t(AXYZ));
end;

{ TSkColorSpaceTransferFunction }

class function TSkColorSpaceTransferFunction.HLG: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_hlg(sk_colorspacetransferfn_t(Result));
end;

function TSkColorSpaceTransferFunction.Invert(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspacetransferfn_invert(@sk_colorspacetransferfn_t(Self), sk_colorspacetransferfn_t(ATransferFunction));
end;

class function TSkColorSpaceTransferFunction.Linear: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_linear(sk_colorspacetransferfn_t(Result));
end;

class function TSkColorSpaceTransferFunction.PQ: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_pq(sk_colorspacetransferfn_t(Result));
end;

class function TSkColorSpaceTransferFunction.Rec2020: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_rec2020(sk_colorspacetransferfn_t(Result));
end;

class function TSkColorSpaceTransferFunction.SRGB: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_srgb(sk_colorspacetransferfn_t(Result));
end;

function TSkColorSpaceTransferFunction.Transform(const AX: SIngle): Single;
begin
  Result := TSkiaApi.sk4d_colorspacetransferfn_transform(@sk_colorspacetransferfn_t(Self), AX);
end;

class function TSkColorSpaceTransferFunction.TwoDotTwo: TSkColorSpaceTransferFunction;
begin
  TSkiaApi.sk4d_colorspacetransferfn_two_dot_two(sk_colorspacetransferfn_t(Result));
end;

{ TSkColorSpaceICCProfile }

class procedure TSkColorSpaceICCProfile.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_colorspaceiccprofile_destroy(AHandle);
end;

function TSkColorSpaceICCProfile.GetBuffer: Pointer;
begin
  Result := TSkiaApi.sk4d_colorspaceiccprofile_get_buffer(GetHandle, nil);
end;

function TSkColorSpaceICCProfile.GetBufferSize: Cardinal;
begin
  TSkiaApi.sk4d_colorspaceiccprofile_get_buffer(GetHandle, @Result);
end;

class function TSkColorSpaceICCProfile.Make(const AInput;
  const ASize: NativeUInt): ISkColorSpaceICCProfile;
var
  LHandle: sk_colorspaceiccprofile_t;
begin
  if ASize = 0 then
    raise ESkColorSpaceICCProfile.CreateFmt(CannotBeEmpty, ['AInput']);
  LHandle := TSkiaApi.sk4d_colorspaceiccprofile_make(@AInput, ASize);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpaceICCProfile.CreateNative(LHandle);
end;

class function TSkColorSpaceICCProfile.Make(
  const AInput: TBytes): ISkColorSpaceICCProfile;
begin
  Result := Make(AInput[0], Length(AInput));
end;

procedure TSkColorSpaceICCProfile.ReadBuffer(out ADest; const ACount: Cardinal);
begin
  if ACount > GetBufferSize then
    raise ESkColorSpaceICCProfile.Create(ArgumentOutOfRange);
  Move(GetBuffer^, ADest, ACount);
end;

function TSkColorSpaceICCProfile.ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspaceiccprofile_to_xyz(GetHandle, sk_colorspacexyz_t(ADest));
end;

{ TSkColorSpace }

class function TSkColorSpace.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_is_unique(AHandle);
end;

class procedure TSkColorSpace.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_colorspace_ref(AHandle);
end;

class procedure TSkColorSpace.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_colorspace_unref(AHandle);
end;

function TSkColorSpace.GammaCloseToSRGB: Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_gamma_close_to_srgb(GetHandle);
end;

function TSkColorSpace.GammaIsLinear: Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_gamma_is_linear(GetHandle);
end;

function TSkColorSpace.IsEqual(const AColorSpace: ISkColorSpace): Boolean;
begin
  if not Assigned(AColorSpace) then
    raise ESkColorSpace.CreateFmt(ParameterCannotBeNil, ['AColorSpace']);
  Result := TSkiaApi.sk4d_colorspace_is_equal(GetHandle, AColorSpace.Handle);
end;

function TSkColorSpace.IsNumericalTransferFunction(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_is_numerical_transfer_fn(GetHandle, sk_colorspacetransferfn_t(ATransferFunction));
end;

function TSkColorSpace.IsSRGB: Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_is_srgb(GetHandle);
end;

class function TSkColorSpace.Make(
  const AProfile: ISkColorSpaceICCProfile): ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  if not Assigned(AProfile) then
    raise ESkColorSpace.CreateFmt(ParameterCannotBeNil, ['AProfile']);
  LHandle := TSkiaApi.sk4d_colorspace_make(AProfile.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

function TSkColorSpace.MakeLinearGamma: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_colorspace_make_linear_gamma(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

class function TSkColorSpace.MakeRGB(
  const ATransferFunction: TSkColorSpaceTransferFunction;
  const AToXyzD50: TSkColorSpaceXYZ): ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_colorspace_make_rgb(@sk_colorspacetransferfn_t(ATransferFunction), @sk_colorspacexyz_t(AToXyzD50));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

class function TSkColorSpace.MakeSRGB: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_colorspace_make_srgb();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

function TSkColorSpace.MakeSRGBGamma: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_colorspace_make_srgb_gamma(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

class function TSkColorSpace.MakeSRGBLinear: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_colorspace_make_srgb_linear();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

function TSkColorSpace.ToProfile: ISkColorSpaceICCProfile;
begin
  Result := TSkColorSpaceICCProfile.CreateNative(TSkiaApi.sk4d_colorspace_to_profile(GetHandle));
end;

function TSkColorSpace.ToXYZ(out ADest: TSkColorSpaceXYZ): Boolean;
begin
  Result := TSkiaApi.sk4d_colorspace_to_xyz(GetHandle, sk_colorspacexyz_t(ADest));
end;

{ TSkImageInfo }

function TSkImageInfo.ByteSize(const ARowBytes: NativeUInt): NativeUInt;
begin
  Result := ARowBytes * NativeUInt(Height);
end;

function TSkImageInfo.BytesPerPixel: Integer;
begin
  case ColorType of
    TSkColorType.Alpha8         : Result :=  1;
    TSkColorType.RGB565         : Result :=  2;
    TSkColorType.ARGB4444       : Result :=  2;
    TSkColorType.RGBA8888       : Result :=  4;
    TSkColorType.RGB888X        : Result :=  4;
    TSkColorType.BGRA8888       : Result :=  4;
    TSkColorType.RGBA1010102    : Result :=  4;
    TSkColorType.BGRA1010102    : Result :=  4;
    TSkColorType.RGB101010X     : Result :=  4;
    TSkColorType.BGR101010X     : Result :=  4;
    TSkColorType.Gray8          : Result :=  1;
    TSkColorType.RGBAF16        : Result :=  8;
    TSkColorType.RGBAF16Clamped : Result :=  8;
    TSkColorType.RGBAF32        : Result := 16;
    TSkColorType.RG88           : Result :=  2;
    TSkColorType.AlphaF16       : Result :=  2;
    TSkColorType.RgF16          : Result :=  4;
    TSkColorType.Alpha16        : Result :=  2;
    TSkColorType.RG1616         : Result :=  4;
    TSkColorType.RGBA16161616   : Result :=  8;
  else
    Result := 0;
  end;
end;

constructor TSkImageInfo.Create(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  const AColorSpace: ISkColorSpace);
begin
  Width      := AWidth;
  Height     := AHeight;
  ColorType  := AColorType;
  AlphaType  := AAlphaType;
  ColorSpace := AColorSpace;
end;

class operator TSkImageInfo.Equal(const AImageInfo1,
  AImageInfo2: TSkImageInfo): Boolean;
begin
  Result := (AImageInfo1.Width     = AImageInfo2.Width    ) and
            (AImageInfo1.Height    = AImageInfo2.Height   ) and
            (AImageInfo1.ColorType = AImageInfo2.ColorType) and
            (AImageInfo1.AlphaType = AImageInfo2.AlphaType) and
            (Assigned(AImageInfo1.ColorSpace) = Assigned(AImageInfo1.ColorSpace)) and ((not Assigned(AImageInfo1.ColorSpace)) or (AImageInfo1.ColorSpace.IsEqual(AImageInfo2.ColorSpace)));
end;

function TSkImageInfo.IsEmpty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TSkImageInfo.IsOpaque: Boolean;
begin
  Result := AlphaType = TSkAlphaType.Opaque;
end;

function TSkImageInfo.IsValid: Boolean;
const
  MaxDimension = MaxInt shr 2;
begin
  if (IsEmpty) or (Width > MaxDimension) or (Height > MaxDimension) or (ColorType = TSkColorType.Unknown) or (AlphaType = TSkAlphaType.Unknown) then
    Exit(False);
  case ColorType of
    TSkColorType.Alpha8,
    TSkColorType.AlphaF16,
    TSkColorType.Alpha16:
      begin
        if (AlphaType <> TSkAlphaType.Opaque) and (AlphaType <> TSkAlphaType.Premul) then
          Exit(False);
      end;
    TSkColorType.RGB565,
    TSkColorType.RGB888X,
    TSkColorType.RGB101010X,
    TSkColorType.BGR101010X,
    TSkColorType.Gray8,
    TSkColorType.RG88,
    TSkColorType.RgF16,
    TSkColorType.RG1616:
      begin
        if AlphaType <> TSkAlphaType.Opaque then
          Exit(False);
      end;
  end;
  Result := True;
end;

function TSkImageInfo.IsValidRowBytes(const ARowBytes: NativeUInt): Boolean;
begin
  if ARowBytes < MinRowBytes then
    Exit(False);
  Result := ARowBytes = (ARowBytes shr ShiftPerPixel shl ShiftPerPixel);
end;

function TSkImageInfo.MakeAlphaType(
  const AAlphaType: TSkAlphaType): TSkImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AAlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSkImageInfo.MakeColorSpace(
  const AColorSpace: ISkColorSpace): TSkImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := AColorSpace;
end;

function TSkImageInfo.MakeColorType(
  const AColorType: TSkColorType): TSkImageInfo;
begin
  Result.Width      := Width;
  Result.Height     := Height;
  Result.ColorType  := AColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSkImageInfo.MakeDimensions(const AWidth,
  AHeight: Integer): TSkImageInfo;
begin
  Result.Width      := AWidth;
  Result.Height     := AHeight;
  Result.ColorType  := ColorType;
  Result.AlphaType  := AlphaType;
  Result.ColorSpace := ColorSpace;
end;

function TSkImageInfo.MinByteSize: NativeUInt;
begin
  Result := ByteSize(MinRowBytes);
end;

function TSkImageInfo.MinRowBytes: NativeUInt;
begin
  Result := Width * BytesPerPixel;
end;

class operator TSkImageInfo.NotEqual(const AImageInfo1,
  AImageInfo2: TSkImageInfo): Boolean;
begin
  Result := not (AImageInfo1 = AImageInfo2);
end;

function TSkImageInfo.ShiftPerPixel: Integer;
begin
  case ColorType of
    TSkColorType.Alpha8         : Result := 0;
    TSkColorType.RGB565         : Result := 1;
    TSkColorType.ARGB4444       : Result := 1;
    TSkColorType.RGBA8888       : Result := 2;
    TSkColorType.RGB888X        : Result := 2;
    TSkColorType.BGRA8888       : Result := 2;
    TSkColorType.RGBA1010102    : Result := 2;
    TSkColorType.BGRA1010102    : Result := 2;
    TSkColorType.RGB101010X     : Result := 2;
    TSkColorType.BGR101010X     : Result := 2;
    TSkColorType.Gray8          : Result := 0;
    TSkColorType.RGBAF16        : Result := 3;
    TSkColorType.RGBAF16Clamped : Result := 3;
    TSkColorType.RGBAF32        : Result := 4;
    TSkColorType.RG88           : Result := 1;
    TSkColorType.AlphaF16       : Result := 1;
    TSkColorType.RGF16          : Result := 2;
    TSkColorType.Alpha16        : Result := 1;
    TSkColorType.RG1616         : Result := 2;
    TSkColorType.RGBA16161616   : Result := 3;
  else
    Result := 0;
  end;
end;

{ TSkPixmap }

constructor TSkPixmap.Create(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt);
var
  LImageInfo: sk_imageinfo_t;
begin
  LImageInfo.FromImageInfo(AImageInfo);
  CreateNative(TSkiaApi.sk4d_pixmap_create2(@LImageInfo, APixels, ARowBytes));
end;

constructor TSkPixmap.Create;
begin
  CreateNative(TSkiaApi.sk4d_pixmap_create());
end;

class procedure TSkPixmap.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_pixmap_destroy(AHandle);
end;

function TSkPixmap.Erase(const AColor: TAlphaColor): Boolean;
begin
  Result := TSkiaApi.sk4d_pixmap_erase(GetHandle, AColor, nil);
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF; const ASubset: TRectF;
  const AColorSpace: ISkColorSpace): Boolean;
begin
  Result := TSkiaApi.sk4d_pixmap_erase2(GetHandle, @sk_color4f_t(AColor), SafeHandle(AColorSpace), @sk_irect_t(ASubset));
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF;
  const AColorSpace: ISkColorSpace): Boolean;
begin
  Result := TSkiaApi.sk4d_pixmap_erase2(GetHandle, @sk_color4f_t(AColor), SafeHandle(AColorSpace), nil);
end;

function TSkPixmap.Erase(const AColor: TAlphaColor;
  const ASubset: TRectF): Boolean;
begin
  Result := TSkiaApi.sk4d_pixmap_erase(GetHandle, AColor, @sk_irect_t(ASubset));
end;

function TSkPixmap.ExtractSubset(const ADest: ISkPixmap;
  const AArea: TRect): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkPixmap.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_pixmap_extract_subset(ADest.Handle, GetHandle, @sk_irect_t(AArea));
end;

function TSkPixmap.GetAlpha(const AX, AY: Integer): Single;
begin
  Result := TSkiaApi.sk4d_pixmap_get_alpha(GetHandle, AX, AY);
end;

function TSkPixmap.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(TSkiaApi.sk4d_pixmap_get_alpha_type(GetHandle));
end;

function TSkPixmap.GetColor(const AX, AY: Integer): TAlphaColor;
begin
  Result := TSkiaApi.sk4d_pixmap_get_color(GetHandle, AX, AY);
end;

function TSkPixmap.GetColorSpace: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_pixmap_get_color_space(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

function TSkPixmap.GetColorType: TSkColorType;
begin
  Result := TSkColorType(TSkiaApi.sk4d_pixmap_get_color_type(GetHandle));
end;

function TSkPixmap.GetHeight: Integer;
begin
  Result := TSkiaApi.sk4d_pixmap_get_height(GetHandle);
end;

function TSkPixmap.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  TSkiaApi.sk4d_pixmap_get_image_info(GetHandle, LResult);
  Result := LResult.ToImageInfo;
end;

function TSkPixmap.GetPixelAddr(const AX, AY: Integer): Pointer;
begin
  Result := TSkiaApi.sk4d_pixmap_get_pixel_addr(GetHandle, AX, AY);
end;

function TSkPixmap.GetPixels: Pointer;
begin
  Result := TSkiaApi.sk4d_pixmap_get_pixels(GetHandle);
end;

function TSkPixmap.GetRowBytes: NativeUInt;
begin
  Result := TSkiaApi.sk4d_pixmap_get_row_bytes(GetHandle);
end;

function TSkPixmap.GetWidth: Integer;
begin
  Result := TSkiaApi.sk4d_pixmap_get_width(GetHandle);
end;

function TSkPixmap.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkPixmap.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_pixmap_read_pixels(GetHandle, ADest.Handle, ASrcX, ASrcY);
end;

function TSkPixmap.ReadPixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY);
end;

function TSkPixmap.ScalePixels(const ADest: ISkPixmap;
  const AFilterQuality: TSkFilterQuality): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkPixmap.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_pixmap_scale_pixels(GetHandle, ADest.Handle, sk_filterquality_t(AFilterQuality));
end;

procedure TSkPixmap.SetColorSpace(const AValue: ISkColorSpace);
begin
  TSkiaApi.sk4d_pixmap_set_colorspace(GetHandle, SafeHandle(AValue));
end;

{ TSkRoundRect }

function TSkRoundRect.Contains(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_rrect_contains(GetHandle, @sk_rect_t(ARect));
end;

constructor TSkRoundRect.Create(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  Create;
  SetRect(ARect, ARadiusX, ARadiusY);
end;

constructor TSkRoundRect.Create(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  Create;
  SetRect(ARect, ARadii);
end;

constructor TSkRoundRect.Create(const ARoundRect: ISkRoundRect);
begin
  if not Assigned(ARoundRect) then
    raise ESkRoundRect.CreateFmt(ParameterCannotBeNil, ['ARoundRect']);
  CreateNative(TSkiaApi.sk4d_rrect_create2(ARoundRect.Handle));
end;

constructor TSkRoundRect.Create;
begin
  CreateNative(TSkiaApi.sk4d_rrect_create());
end;

procedure TSkRoundRect.Deflate(const ADX, ADY: Single);
begin
  TSkiaApi.sk4d_rrect_deflate(GetHandle, ADX, ADY);
end;

class procedure TSkRoundRect.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_rrect_destroy(AHandle);
end;

function TSkRoundRect.GetDeflate(const ADX, ADY: Single): ISkRoundRect;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  TSkiaApi.sk4d_rrect_deflate2(GetHandle, ADX, ADY, LRoundRect.Handle);
  Result := LRoundRect;
end;

function TSkRoundRect.GetHeight: Single;
begin
  Result := TSkiaApi.sk4d_rrect_get_height(GetHandle);
end;

function TSkRoundRect.GetInflate(const ADX, ADY: Single): ISkRoundRect;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  TSkiaApi.sk4d_rrect_inflate2(GetHandle, ADX, ADY, LRoundRect.Handle);
  Result := LRoundRect;
end;

function TSkRoundRect.GetOffset(const ADX, ADY: Single): ISkRoundRect;
begin
  Result := TSkRoundRect.CreateNative(TSkiaApi.sk4d_rrect_make_offset(GetHandle, ADX, ADY));
end;

function TSkRoundRect.GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
begin
  TSkiaApi.sk4d_rrect_get_radii(GetHandle, sk_rrectcorner_t(ACorner), sk_vector_t(Result));
end;

function TSkRoundRect.GetRect: TRectF;
begin
  TSkiaApi.sk4d_rrect_get_rect(GetHandle, sk_rect_t(Result));
end;

function TSkRoundRect.GetRoundRectType: TSkRoundRectType;
begin
  Result := TSkRoundRectType(TSkiaApi.sk4d_rrect_get_type(GetHandle));
end;

function TSkRoundRect.GetSimpleRadii: TPointF;
begin
  TSkiaApi.sk4d_rrect_get_simple_radii(GetHandle, sk_vector_t(Result));
end;

function TSkRoundRect.GetWidth: Single;
begin
  Result := TSkiaApi.sk4d_rrect_get_width(GetHandle);
end;

procedure TSkRoundRect.Inflate(const ADX, ADY: Single);
begin
  TSkiaApi.sk4d_rrect_inflate(GetHandle, ADX, ADY);
end;

function TSkRoundRect.IsEqual(const ARoundRect: ISkRoundRect): Boolean;
begin
  if not Assigned(ARoundRect) then
    raise ESkRoundRect.CreateFmt(ParameterCannotBeNil, ['ARoundRect']);
  Result := TSkiaApi.sk4d_rrect_is_equal(GetHandle, ARoundRect.Handle);
end;

function TSkRoundRect.IsValid: Boolean;
begin
  Result := TSkiaApi.sk4d_rrect_is_valid(GetHandle);
end;

procedure TSkRoundRect.Offset(const ADX, ADY: Single);
begin
  TSkiaApi.sk4d_rrect_offset(GetHandle, ADX, ADY);
end;

procedure TSkRoundRect.SetEmpty;
begin
  TSkiaApi.sk4d_rrect_set_empty(GetHandle);
end;

procedure TSkRoundRect.SetNinePatch(const ARect: TRectF; const ARadiusLeft,
  ARadiusTop, ARadiusRight, ARadiusBottom: Single);
begin
  TSkiaApi.sk4d_rrect_set_nine_patch(GetHandle, @sk_rect_t(ARect), ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom);
end;

procedure TSkRoundRect.SetOval(const ARect: TRectF);
begin
  TSkiaApi.sk4d_rrect_set_oval(GetHandle, @sk_rect_t(ARect));
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  TSkiaApi.sk4d_rrect_set_rect3(GetHandle, @sk_rect_t(ARect), ARadiusX, ARadiusY);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  TSkiaApi.sk4d_rrect_set_rect2(GetHandle, @sk_rect_t(ARect), @ARadii);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF);
begin
  TSkiaApi.sk4d_rrect_set_rect(GetHandle, @sk_rect_t(ARect));
end;

function TSkRoundRect.Transform(const AMatrix: TMatrix): ISkRoundRect;
begin
  Result := TSkRoundRect.Create;
  if not TSkiaApi.sk4d_rrect_transform(GetHandle, @sk_matrix_t(AMatrix), Result.Handle) then
    Result := nil;
end;

{ TSkPath }

function TSkPath.Contains(const AX, AY: Single): Boolean;
begin
  Result := TSkiaApi.sk4d_path_contains(GetHandle, AX, AY);
end;

constructor TSkPath.Create;
begin
  CreateNative(TSkiaApi.sk4d_path_create());
end;

constructor TSkPath.Create(const ASVG: string);
begin
  CreateNative(TSkiaApi.sk4d_path_create2(MarshaledAString(UTF8String(ASVG))));
end;

class procedure TSkPath.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_path_destroy(AHandle);
end;

function TSkPath.GetBounds: TRectF;
begin
  TSkiaApi.sk4d_path_get_bounds(GetHandle, sk_rect_t(Result));
end;

function TSkPath.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(TSkiaApi.sk4d_path_get_fill_type(GetHandle));
end;

function TSkPath.GetIterator(const AForceClose: Boolean): ISkPathIterator;
begin
  Result := TPathIterator.Create(Self, AForceClose);
end;

function TSkPath.GetLastPoint: TPointF;
begin
  if not TSkiaApi.sk4d_path_get_last_point(GetHandle, sk_point_t(Result)) then
    Result := TPointF.Create(0, 0);
end;

function TSkPath.GetSegmentMasks: TSkSegmentMasks;
begin
  Result := TSkSegmentMasks(Byte(TSkiaApi.sk4d_path_get_segment_masks(GetHandle)));
end;

function TSkPath.GetTightBounds: TRectF;
begin
  TSkiaApi.sk4d_path_get_tight_bounds(GetHandle, sk_rect_t(Result));
end;

function TSkPath.Interpolate(const AEnding: ISkPath;
  const AWeight: Single): ISkPath;
begin
  if not Assigned(AEnding) then
    raise ESkPath.CreateFmt(ParameterCannotBeNil, ['AEnding']);
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_path_interpolate(GetHandle, AEnding.Handle, AWeight, Result.Handle) then
    Result := nil;
end;

function TSkPath.IsConvex: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_convex(GetHandle);
end;

function TSkPath.IsEmpty: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_empty(GetHandle);
end;

function TSkPath.IsFinite: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_finite(GetHandle);
end;

function TSkPath.IsInterpolatable(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkPath.CreateFmt(ParameterCannotBeNil, ['APath']);
  Result := TSkiaApi.sk4d_path_is_interpolatable(GetHandle, APath.Handle);
end;

function TSkPath.IsLastContourClosed: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_last_contour_closed(GetHandle);
end;

function TSkPath.IsLine(out APoint1, APoint2: TPointF): Boolean;
var
  LLines: array[0..1] of TPointF;
begin
  LLines[0] := APoint1;
  LLines[1] := APoint2;
  Result := TSkiaApi.sk4d_path_is_line(GetHandle, @sk_point_t(LLines[0]));
end;

function TSkPath.IsLine: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_line(GetHandle, nil);
end;

function TSkPath.IsOval: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_oval(GetHandle, nil);
end;

function TSkPath.IsOval(out ARect: TRectF): Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_oval(GetHandle, @sk_rect_t(ARect));
end;

function TSkPath.IsRect: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_rect(GetHandle, nil);
end;

function TSkPath.IsRect(out ARect: TRectF): Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_rect(GetHandle, @sk_rect_t(ARect));
end;

function TSkPath.IsRoundRect(out ARoundRect: ISkRoundRect): Boolean;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Result     := TSkiaApi.sk4d_path_is_rrect(GetHandle, SafeHandle(LRoundRect));
  if Result then
    ARoundRect := LRoundRect;
end;

function TSkPath.IsRoundRect: Boolean;
begin
  Result := TSkiaApi.sk4d_path_is_rrect(GetHandle, 0);
end;

function TSkPath.Offset(const ADX, ADY: Single): ISkPath;
begin
  Result := TSkPath.Create;
  TSkiaApi.sk4d_path_offset(GetHandle, ADX, ADY, Result.Handle);
end;

function TSkPath.Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkPath.CreateFmt(ParameterCannotBeNil, ['APath']);
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_path_op(GetHandle, APath.Handle, sk_pathop_t(AOp), Result.Handle) then
    Result := nil;
end;

function TSkPath.ToSVG: string;
var
  LString: ISkString;
begin
  LString := TSkString.Create;
  TSkiaApi.sk4d_path_to_svg(GetHandle, LString.Handle);
  Result := LString.Text;
end;

function TSkPath.Transform(const AMatrix: TMatrix): ISkPath;
begin
  Result := TSkPath.Create;
  TSkiaApi.sk4d_path_transform(GetHandle, @sk_matrix_t(AMatrix), Result.Handle);
end;

{ TSkPath.TPathIterator }

constructor TSkPath.TPathIterator.Create(const APath: ISkPath;
  const AForceClose: Boolean);
begin
  CreateNative(TSkiaApi.sk4d_pathiterator_create(APath.Handle, AForceClose));
end;

class procedure TSkPath.TPathIterator.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_pathiterator_destroy(AHandle);
end;

function TSkPath.TPathIterator.GetCurrent: TSkPathIteratorElem;
begin
  Result := FCurrent;
end;

function TSkPath.TPathIterator.MoveNext: Boolean;
var
  LCurrent: sk_pathiteratorelem_t;
begin
  Result := TSkiaApi.sk4d_pathiterator_next(GetHandle, LCurrent);
  if Result then
  begin
    FCurrent.Verb := TSkPathVerb(LCurrent.Verb);
    case FCurrent.Verb of
      TSkPathVerb.Move:
        begin
          FCurrent.Move.Point := TPointF(LCurrent.point1);
        end;
      TSkPathVerb.Line:
        begin
          FCurrent.Line.LastPoint := TPointF(LCurrent.point1);
          FCurrent.Line.Point     := TPointF(LCurrent.point2);
        end;
      TSkPathVerb.Quad:
        begin
          FCurrent.Quad.LastPoint := TPointF(LCurrent.point1);
          FCurrent.Quad.Point1    := TPointF(LCurrent.point2);
          FCurrent.Quad.Point2    := TPointF(LCurrent.point2);
        end;
      TSkPathVerb.Conic:
        begin
          FCurrent.Conic.LastPoint := TPointF(LCurrent.point1);
          FCurrent.Conic.Point1    := TPointF(LCurrent.point2);
          FCurrent.Conic.Point2    := TPointF(LCurrent.point2);
          FCurrent.Conic.Weight    := LCurrent.weight;
        end;
      TSkPathVerb.Cubic:
        begin
          FCurrent.Cubic.LastPoint := TPointF(LCurrent.point1);
          FCurrent.Cubic.Point1    := TPointF(LCurrent.point2);
          FCurrent.Cubic.Point2    := TPointF(LCurrent.point2);
          FCurrent.Cubic.Point3    := TPointF(LCurrent.point3);
        end;
    end;
  end;
end;

{ TSkOpBuilder }

procedure TSkOpBuilder.Add(const APath: ISkPath; const AOp: TSkPathOp);
begin
  if not Assigned(APath) then
    raise ESkOpBuilder.CreateFmt(ParameterCannotBeNil, ['APath']);
  TSkiaApi.sk4d_opbuilder_add(GetHandle, APath.Handle, sk_pathop_t(AOp));
end;

constructor TSkOpBuilder.Create;
begin
  CreateNative(TSkiaApi.sk4d_opbuilder_create());
end;

function TSkOpBuilder.Detach: ISkPath;
begin
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_opbuilder_detach(GetHandle, Result.Handle) then
    Result := nil;
end;

class procedure TSkOpBuilder.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_opbuilder_destroy(AHandle);
end;

{ TSkPathMeasure }

constructor TSkPathMeasure.Create(const APath: ISkPath;
  const AForceClosed: Boolean; const AResScale: Single);
begin
  if not Assigned(APath) then
    raise ESkPathMeasure.CreateFmt(ParameterCannotBeNil, ['APath']);
  CreateNative(TSkiaApi.sk4d_pathmeasure_create(APath.Handle, AForceClosed, AResScale));
end;

class procedure TSkPathMeasure.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_pathmeasure_destroy(AHandle);
end;

function TSkPathMeasure.GetLength: Single;
begin
  Result := TSkiaApi.sk4d_pathmeasure_get_length(GetHandle);
end;

function TSkPathMeasure.GetMatrix(const ADistance: Single; out AMatrix: TMatrix;
  const AMatrixFlags: TSkPathMeasureMatrixFlags): Boolean;
begin
  Result := TSkiaApi.sk4d_pathmeasure_get_matrix(GetHandle, ADistance, sk_matrix_t(AMatrix), Byte(AMatrixFlags));
end;

function TSkPathMeasure.GetPositionAndTangent(const ADistance: Single;
  out APosition, ATangent: TPointF): Boolean;
begin
  Result := TSkiaApi.sk4d_pathmeasure_get_position_and_tangent(GetHandle, ADistance, sk_point_t(APosition), sk_vector_t(ATangent));
end;

function TSkPathMeasure.GetSegment(const AStart, AStop: Single;
  const AStartWithMoveTo: Boolean): ISkPath;
begin
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_pathmeasure_get_segment(GetHandle, AStart, AStop, Result.Handle, AStartWithMoveTo) then
    Result := nil;
end;

function TSkPathMeasure.IsClosed: Boolean;
begin
  Result := TSkiaApi.sk4d_pathmeasure_is_closed(GetHandle);
end;

function TSkPathMeasure.NextContour: Boolean;
begin
  Result := TSkiaApi.sk4d_pathmeasure_next_contour(GetHandle);
end;

{ TSkPathEffect }

class function TSkPathEffect.Make1DPath(const APath: ISkPath; const AAdvance,
  APhase: Single; const AStyle: TSkPathEffect1DStyle): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  if not Assigned(APath) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['APath']);
  LHandle := TSkiaApi.sk4d_patheffect_make_1dpath(APath.Handle, AAdvance, APhase, sk_patheffect1dstyle_t(AStyle));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.Make2DLine(const AWidth: Single;
  const AMatrix: TMatrix): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := TSkiaApi.sk4d_patheffect_make_2dline(AWidth, @sk_matrix_t(AMatrix));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.Make2DPath(const AMatrix: TMatrix;
  const APath: ISkPath): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  if not Assigned(APath) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['APath']);
  LHandle := TSkiaApi.sk4d_patheffect_make_2dpath(@sk_matrix_t(AMatrix), APath.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeCompose(const AOuter,
  AInner: ISkPathEffect): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  if not Assigned(AOuter) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['AInner']);
  LHandle := TSkiaApi.sk4d_patheffect_make_compose(AOuter.Handle, AInner.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeCorner(const ARadius: Single): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := TSkiaApi.sk4d_patheffect_make_corner(ARadius);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeDash(const AIntervals: TArray<Single>;
  const APhase: Single): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  if Length(AIntervals) = 0 then
    raise ESkPathEffect.CreateFmt(CannotBeEmpty, ['AIntervals']);
  if Odd(Length(AIntervals)) then
    raise ESkPathEffect.CreateFmt(EvenNumberOfEntries, ['AIntervals']);
  LHandle := TSkiaApi.sk4d_patheffect_make_dash(@AIntervals[0], Length(AIntervals), APhase);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeDiscrete(const ASegLength, ADeviation: Single;
  const ASeedAssist: Cardinal): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := TSkiaApi.sk4d_patheffect_make_discrete(ASegLength, ADeviation, ASeedAssist);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeSum(const AEffect1,
  AEffect2: ISkPathEffect): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  if not Assigned(AEffect1) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['AEffect1']);
  if not Assigned(AEffect2) then
    raise ESkPathEffect.CreateFmt(ParameterCannotBeNil, ['AEffect2']);
  LHandle := TSkiaApi.sk4d_patheffect_make_sum(AEffect1.Handle, AEffect2.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

class function TSkPathEffect.MakeTrim(const AStart, AStop: Single;
  const AMode: TSkPathEffectTrimMode): ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := TSkiaApi.sk4d_patheffect_make_trim(AStart, AStop, sk_patheffecttrimmode_t(AMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

{ TSkPathBuilder }

procedure TSkPathBuilder.AddArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single);
begin
  TSkiaApi.sk4d_pathbuilder_add_arc(GetHandle, @sk_rect_t(AOval), AStartAngle, ASweepAngle);
end;

procedure TSkPathBuilder.AddCircle(const ACenterX, ACenterY, ARadius: Single;
  ADirection: TSkPathDirection);
begin
  TSkiaApi.sk4d_pathbuilder_add_circle(GetHandle,ACenterX, ACenterY, ARadius, sk_pathdirection_t(ADirection));
end;

procedure TSkPathBuilder.AddCircle(const ACenter: TPointF; ARadius: Single;
  ADirection: TSkPathDirection);
begin
  AddCircle(ACenter.X, ACenter.Y, ARadius, ADirection);
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection);
begin
  AddOval(ARect, ADirection, 1);
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  TSkiaApi.sk4d_pathbuilder_add_oval(GetHandle, @sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddPolygon(const APolygon: TPolygon;
  const IsClosed: Boolean);
begin
  TSkiaApi.sk4d_pathbuilder_add_polygon(GetHandle, @sk_point_t(APolygon[0]), Length(APolygon), IsClosed);
end;

procedure TSkPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  if AStartIndex > 3 then
    raise ESkPathBuilder.CreateFmt(MustBeInRange, ['AStartIndex', 0, 3]);
  TSkiaApi.sk4d_pathbuilder_add_rect(GetHandle, @sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSkPathDirection);
begin
  AddRect(ARect, ADirection, 0);
end;

procedure TSkPathBuilder.AddRoundRect(const ARoundRect: ISkRoundRect;
  ADirection: TSkPathDirection);
begin
  AddRoundRect(ARoundRect, ADirection, IfThen(ADirection = TSkPathDirection.CW, 6, 7));
end;

procedure TSkPathBuilder.AddRoundRect(const ARoundRect: ISkRoundRect;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  if not Assigned(ARoundRect) then
    raise ESkPathBuilder.CreateFmt(ParameterCannotBeNil, ['ARoundRect']);
  TSkiaApi.sk4d_pathbuilder_add_rrect(GetHandle, ARoundRect.Handle, sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.ArcTo(const APoint1, APoint2: TPointF;
  const ARadius: Single);
begin
  TSkiaApi.sk4d_pathbuilder_arc_to3(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), ARadius);
end;

procedure TSkPathBuilder.ArcTo(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AForceMoveTo: Boolean);
begin
  TSkiaApi.sk4d_pathbuilder_arc_to2(GetHandle, @sk_rect_t(AOval), AStartAngle, ASweepAngle, AForceMoveTo);
end;

procedure TSkPathBuilder.ArcTo(const ARadius: TPointF;
  const XAxisRotate: Single; const ALargeArc: TSkPathArcSize;
  const ASweep: TSkPathDirection; const AXY: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_arc_to(GetHandle, @sk_point_t(ARadius), XAxisRotate, sk_patharcsize_t(ALargeArc), sk_pathdirection_t(ASweep), @sk_point_t(AXY));
end;

procedure TSkPathBuilder.Close;
begin
  TSkiaApi.sk4d_pathbuilder_close(GetHandle);
end;

procedure TSkPathBuilder.ConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  TSkiaApi.sk4d_pathbuilder_conic_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), AWeight);
end;

procedure TSkPathBuilder.ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  ConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

constructor TSkPathBuilder.Create;
begin
  CreateNative(TSkiaApi.sk4d_pathbuilder_create());
end;

constructor TSkPathBuilder.Create(const AFillType: TSkPathFillType);
begin
  Create;
  SetFillType(AFillType);
end;

constructor TSkPathBuilder.Create(const APathBuilder: ISkPathBuilder);
begin
  if not Assigned(APathBuilder) then
    raise ESkPathBuilder.CreateFmt(ParameterCannotBeNil, ['APathBuilder']);
  CreateNative(TSkiaApi.sk4d_pathbuilder_create2(APathBuilder.Handle));
end;

procedure TSkPathBuilder.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  CubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

procedure TSkPathBuilder.CubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_cubic_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), @sk_point_t(APoint3));
end;

function TSkPathBuilder.Detach: ISkPath;
begin
  Result := TSkPath.CreateNative(TSkiaApi.sk4d_pathbuilder_detach(GetHandle));
end;

class procedure TSkPathBuilder.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_pathbuilder_destroy(AHandle);
end;

function TSkPathBuilder.GetBounds: TRectF;
begin
  TSkiaApi.sk4d_pathbuilder_get_bounds(GetHandle, sk_rect_t(Result));
end;

function TSkPathBuilder.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(TSkiaApi.sk4d_pathbuilder_get_fill_type(GetHandle));
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount,
  AExtraVerbCount: Integer);
begin
  TSkiaApi.sk4d_pathbuilder_inc_reserve(GetHandle, AExtraPointCount, AExtraVerbCount);
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount: Integer);
begin
  IncReserve(AExtraPointCount, AExtraPointCount);
end;

procedure TSkPathBuilder.LineTo(const AX, AY: Single);
begin
  LineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.LineTo(const APoint: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_line_to(GetHandle, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.MoveTo(const AX, AY: Single);
begin
  MoveTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.MoveTo(const APoint: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_move_to(GetHandle, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.Offset(const ADX, ADY: Single);
begin
  TSkiaApi.sk4d_pathbuilder_offset(GetHandle, ADX, ADY);
end;

procedure TSkPathBuilder.PolylineTo(const APoints: TArray<TPointF>);
begin
  if Length(APoints) > 0 then
    TSkiaApi.sk4d_pathbuilder_polyline_to(GetHandle, @sk_point_t(APoints[0]), Length(APoints));
end;

procedure TSkPathBuilder.QuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  QuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.QuadTo(const APoint1, APoint2: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_quad_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2));
end;

procedure TSkPathBuilder.RConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  TSkiaApi.sk4d_pathbuilder_r_conic_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), AWeight);
end;

procedure TSkPathBuilder.RConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  RConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

procedure TSkPathBuilder.RCubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  RCubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

procedure TSkPathBuilder.RCubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_r_cubic_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), @sk_point_t(APoint3));
end;

procedure TSkPathBuilder.Reset;
begin
  TSkiaApi.sk4d_pathbuilder_reset(GetHandle);
end;

procedure TSkPathBuilder.RLineTo(const AX, AY: Single);
begin
  RLineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.RLineTo(const APoint: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_r_line_to(GetHandle, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.RQuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  RQuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.RQuadTo(const APoint1, APoint2: TPointF);
begin
  TSkiaApi.sk4d_pathbuilder_r_quad_to(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2));
end;

procedure TSkPathBuilder.SetFillType(const AValue: TSkPathFillType);
begin
  TSkiaApi.sk4d_pathbuilder_set_filltype(GetHandle, sk_pathfilltype_t(AValue));
end;

function TSkPathBuilder.Snapshot: ISkPath;
begin
  Result := TSkPath.CreateNative(TSkiaApi.sk4d_pathbuilder_snapshot(GetHandle));
end;

procedure TSkPathBuilder.ToggleInverseFillType;
begin
  TSkiaApi.sk4d_pathbuilder_toggle_inverse_filltype(GetHandle);
end;

{ TSkRegion }

function TSkRegion.Contains(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  Result := TSkiaApi.sk4d_region_contains(GetHandle, ARegion.Handle);
end;

function TSkRegion.Contains(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_region_contains2(GetHandle, @sk_irect_t(ARect));
end;

function TSkRegion.Contains(const AX, AY: Integer): Boolean;
begin
  Result := TSkiaApi.sk4d_region_contains3(GetHandle, AX, AY);
end;

constructor TSkRegion.Create(const ARect: TRect);
begin
  Create;
  SetRect(ARect);
end;

constructor TSkRegion.Create(const ARegion: ISkRegion);
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  CreateNative(TSkiaApi.sk4d_region_create2(ARegion.Handle));
end;

constructor TSkRegion.Create;
begin
  CreateNative(TSkiaApi.sk4d_region_create());
end;

class procedure TSkRegion.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_region_destroy(AHandle);
end;

function TSkRegion.GetBoundaryPath: ISkPath;
begin
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_region_get_boundary_path(GetHandle, Result.Handle) then
    Result := nil;
end;

function TSkRegion.GetBounds: TRectF;
begin
  TSkiaApi.sk4d_region_get_bounds(GetHandle, sk_irect_t(Result));
end;

function TSkRegion.GetCliperator(const AClip: TRect): ISkRegionCliperator;
begin
  Result := TRegionCliperator.Create(Self, AClip);
end;

function TSkRegion.GetIterator: ISkRegionIterator;
begin
  Result := TRegionIterator.Create(Self);
end;

function TSkRegion.GetSpanerator(const AY, ALeft,
  ARight: Integer): ISkRegionSpanerator;
begin
  Result := TRegionSpanerator.Create(Self, AY, ALeft, ARight);
end;

function TSkRegion.GetTranslate(const AX, AY: Integer): ISkRegion;
begin
  Result := TSkRegion.Create;
  TSkiaApi.sk4d_region_translate2(GetHandle, AX, AY, Result.Handle);
end;

function TSkRegion.Intersects(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_region_intersects2(GetHandle, @sk_irect_t(ARect));
end;

function TSkRegion.Intersects(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  Result := TSkiaApi.sk4d_region_intersects(GetHandle, ARegion.Handle);
end;

function TSkRegion.IsComplex: Boolean;
begin
  Result := TSkiaApi.sk4d_region_is_complex(GetHandle);
end;

function TSkRegion.IsEmpty: Boolean;
begin
  Result := TSkiaApi.sk4d_region_is_empty(GetHandle);
end;

function TSkRegion.IsEqual(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  Result := TSkiaApi.sk4d_region_is_equal(GetHandle, ARegion.Handle);
end;

function TSkRegion.IsRect: Boolean;
begin
  Result := TSkiaApi.sk4d_region_is_rect(GetHandle);
end;

function TSkRegion.Op(const ARegion: ISkRegion;
  const AOp: TSkRegionOp): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  Result := TSkiaApi.sk4d_region_op(GetHandle, ARegion.Handle, sk_regionop_t(AOp));
end;

function TSkRegion.Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean;
begin
  Result := TSkiaApi.sk4d_region_op2(GetHandle, @sk_irect_t(ARect), sk_regionop_t(AOp));
end;

function TSkRegion.QuickContains(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_region_quick_contains(GetHandle, @sk_irect_t(ARect));
end;

function TSkRegion.QuickReject(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  Result := TSkiaApi.sk4d_region_quick_reject(GetHandle, ARegion.Handle);
end;

function TSkRegion.QuickReject(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_region_quick_reject2(GetHandle, @sk_irect_t(ARect));
end;

procedure TSkRegion.SetEmpty;
begin
  TSkiaApi.sk4d_region_set_empty(GetHandle);
end;

function TSkRegion.SetPath(const APath: ISkPath;
  const AClip: ISkRegion): Boolean;
begin
  if not Assigned(APath) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['APath']);
  if not Assigned(AClip) then
    raise ESkRegion.CreateFmt(ParameterCannotBeNil, ['AClip']);
  Result := TSkiaApi.sk4d_region_set_path(GetHandle, APath.Handle, AClip.Handle);
end;

function TSkRegion.SetRect(const ARect: TRect): Boolean;
begin
  Result := TSkiaApi.sk4d_region_set_rect(GetHandle, @sk_irect_t(ARect));
end;

function TSkRegion.SetRects(const ARects: TArray<TRect>): Boolean;
begin
  if Length(ARects) = 0 then
    raise ESkRegion.CreateFmt(CannotBeEmpty, ['ARects']);
  Result := TSkiaApi.sk4d_region_set_rects(GetHandle, @sk_irect_t(ARects[0]), Length(ARects));
end;

procedure TSkRegion.Translate(const AX, AY: Integer);
begin
  TSkiaApi.sk4d_region_translate(GetHandle, AX, AY);
end;

{ TSkRegion.TRegionCliperator }

constructor TSkRegion.TRegionCliperator.Create(const ARegion: ISkRegion;
  const AClip: TRect);
begin
  CreateNative(TSkiaApi.sk4d_regioncliperator_create(ARegion.Handle, @sk_irect_t(AClip)));
end;

class procedure TSkRegion.TRegionCliperator.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_regioncliperator_destroy(AHandle);
end;

function TSkRegion.TRegionCliperator.GetCurrent: TRect;
begin
  TSkiaApi.sk4d_regioncliperator_get_current(GetHandle, sk_irect_t(Result));
end;

function TSkRegion.TRegionCliperator.MoveNext: Boolean;
begin
  Result := TSkiaApi.sk4d_regioncliperator_move_next(GetHandle);
end;

{ TSkRegion.TRegionIterator }

constructor TSkRegion.TRegionIterator.Create(const ARegion: ISkRegion);
begin
  CreateNative(TSkiaApi.sk4d_regioniterator_create(ARegion.Handle));
end;

class procedure TSkRegion.TRegionIterator.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_regioniterator_destroy(AHandle);
end;

function TSkRegion.TRegionIterator.GetCurrent: TRect;
begin
  TSkiaApi.sk4d_regioniterator_get_current(GetHandle, sk_irect_t(Result));
end;

function TSkRegion.TRegionIterator.MoveNext: Boolean;
begin
  Result := TSkiaApi.sk4d_regioniterator_move_next(GetHandle);
end;

procedure TSkRegion.TRegionIterator.Reset;
begin
  TSkiaApi.sk4d_regioniterator_reset(GetHandle);
end;

{ TSkRegion.TRegionSpanerator }

constructor TSkRegion.TRegionSpanerator.Create(const ARegion: ISkRegion;
  const AY, ALeft, ARight: Integer);
begin
  CreateNative(TSkiaApi.sk4d_regionspanerator_create(ARegion.Handle, AY, ALeft, ARight));
end;

class procedure TSkRegion.TRegionSpanerator.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_regionspanerator_destroy(AHandle);
end;

function TSkRegion.TRegionSpanerator.GetCurrent: TPoint;
begin
  Result := FCurrent;
end;

function TSkRegion.TRegionSpanerator.MoveNext: Boolean;
begin
  Result := TSkiaApi.sk4d_regionspanerator_next(GetHandle, sk_ipoint_t(FCurrent));
end;

{ TSkTraceMemoryDumpBaseClass }

constructor TSkTraceMemoryDumpBaseClass.Create(const ADetailedDump,
  ADumpWrappedObjects: Boolean);
begin
  CreateNative(TSkiaApi.sk4d_tracememorydumpbaseclass_create(ADetailedDump, ADumpWrappedObjects, Self));
end;

class constructor TSkTraceMemoryDumpBaseClass.Create;
var
  LProcs: sk_tracememorydumpbaseclass_procs_t;
begin
  LProcs.dump_numeric_value := dump_numeric_value_proc;
  LProcs.dump_string_value  := dump_string_value_proc;
  TSkiaApi.sk4d_tracememorydumpbaseclass_set_procs(@LProcs);
end;

class procedure TSkTraceMemoryDumpBaseClass.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_tracememorydumpbaseclass_destroy(AHandle);
end;

class procedure TSkTraceMemoryDumpBaseClass.dump_numeric_value_proc(
  context: Pointer; const dump_name, value_name, units: MarshaledAString;
  value: uint64_t);
begin
  if context <> nil then
    TSkTraceMemoryDumpBaseClass(context).DumpNumericValue(string(dump_name), string(value_name), string(units), value);
end;

class procedure TSkTraceMemoryDumpBaseClass.dump_string_value_proc(
  context: Pointer; const dump_name, value_name, value: MarshaledAString);
begin
  if context <> nil then
    TSkTraceMemoryDumpBaseClass(context).DumpStringValue(string(dump_name), string(value_name), string(value));
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
  Result := TGRGLGetProc(context^)(string(name));
end;

function TGRGLInterface.HasExtension(const AName: string): Boolean;
begin
  Result := TSkiaApi.gr4d_gl_interface_has_extension(GetHandle, MarshaledAString(UTF8String(AName)));
end;

class function TGRGLInterface.MakeAssembled(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  if not Assigned(AProc) then
    Exit(nil);
  LHandle := TSkiaApi.gr4d_gl_interface_make_assembled(@AProc, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledGL(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  if not Assigned(AProc) then
    Exit(nil);
  LHandle := TSkiaApi.gr4d_gl_interface_make_assembled_gl(@AProc, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledGLES(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  if not Assigned(AProc) then
    Exit(nil);
  LHandle := TSkiaApi.gr4d_gl_interface_make_assembled_gles(@AProc, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeAssembledWebGL(
  const AProc: TGRGLGetProc): IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  if not Assigned(AProc) then
    Exit(nil);
  LHandle := TSkiaApi.gr4d_gl_interface_make_assembled_webgl(@AProc, get_proc);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

class function TGRGLInterface.MakeNative: IGRGLInterface;
var
  LHandle: gr_gl_interface_t;
begin
  LHandle := TSkiaApi.gr4d_gl_interface_make_native();
  if LHandle = 0 then
    Exit(nil);
  Result := TGRGLInterface.CreateNative(LHandle);
end;

function TGRGLInterface.Validate: Boolean;
begin
  Result := TSkiaApi.gr4d_gl_interface_validate(GetHandle);
end;

{$IFDEF SK_METAL}

{ TGRMTLBackendContext }

constructor TGRMTLBackendContext.Create(const ADevice: MTLDevice;
  const AQueue: MTLCommandQueue);
begin
  Device := ADevice;
  Queue  := AQueue;
end;

{ TGRMTLTextureInfo }

constructor TGRMTLTextureInfo.Create(const ATexture: MTLTexture);
begin
  Texture := ATexture;
end;

{$ENDIF}

{ TGRRecordingContext }

function TGRRecordingContext.GetMaxSurfaceSampleCountForColorType(
  const AColorType: TSkColorType): Integer;
begin
  Result := TSkiaApi.gr4d_recordingcontext_get_max_surface_sample_count_for_color_type(GetHandle, sk_colortype_t(AColorType));
end;

function TGRRecordingContext.IsAbandoned: Boolean;
begin
  Result := TSkiaApi.gr4d_recordingcontext_is_abandoned(GetHandle);
end;

{ TGRDirectContext }

procedure TGRDirectContext.AbandonContext;
begin
  TSkiaApi.gr4d_directcontext_abandon_context(GetHandle);
end;

procedure TGRDirectContext.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  if not Assigned(ATraceMemoryDump) then
    raise EGRDirectContext.CreateFmt(ParameterCannotBeNil, ['ATraceMemoryDump']);
  TSkiaApi.gr4d_directcontext_dump_memory_statistics(GetHandle, ATraceMemoryDump.Handle);
end;

procedure TGRDirectContext.Flush;
begin
  TSkiaApi.gr4d_directcontext_flush(GetHandle);
end;

procedure TGRDirectContext.FlushAndSubmit(const ASyncCPU: Boolean);
begin
  TSkiaApi.gr4d_directcontext_flush_and_submit(GetHandle, ASyncCPU);
end;

procedure TGRDirectContext.FreeGPUResources;
begin
  TSkiaApi.gr4d_directcontext_free_gpu_resources(GetHandle);
end;

function TGRDirectContext.GetResourceCacheLimit: NativeUInt;
begin
  Result := TSkiaApi.gr4d_directcontext_get_resource_cache_limit(GetHandle);
end;

procedure TGRDirectContext.GetResourceCacheUsage(out AMaxResources: Integer;
  out AMaxResourcesBytes: NativeUInt);
begin
  TSkiaApi.gr4d_directcontext_get_resource_cache_usage(GetHandle, AMaxResources, AMaxResourcesBytes);
end;

class function TGRDirectContext.MakeGL(
  const AInterface: IGRGLInterface): IGRDirectContext;
var
  LHandle: gr_directcontext_t;
begin
  LHandle := TSkiaApi.gr4d_directcontext_make_gl(SafeHandle(AInterface));
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;

class function TGRDirectContext.MakeGL(const AOptions: TGRContextOptions;
  const AInterface: IGRGLInterface): IGRDirectContext;
var
  LHandle: gr_directcontext_t;
begin
  LHandle := TSkiaApi.gr4d_directcontext_make_gl2(SafeHandle(AInterface), @gr_contextoptions_t(AOptions));
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;

{$IFDEF SK_METAL}
class function TGRDirectContext.MakeMetal(
  const ABackendContext: TGRMTLBackendContext): IGRDirectContext;
var
  LBackendContext: gr_mtl_backendcontext_t;
  LHandle: gr_directcontext_t;
begin
  LBackendContext.FromGRMTLBackendContext(ABackendContext);
  LHandle := TSkiaApi.gr4d_directcontext_make_metal(@LBackendContext);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;

class function TGRDirectContext.MakeMetal(
  const ABackendContext: TGRMTLBackendContext;
  const AOptions: TGRContextOptions): IGRDirectContext;
var
  LBackendContext: gr_mtl_backendcontext_t;
  LHandle: gr_directcontext_t;
begin
  LBackendContext.FromGRMTLBackendContext(ABackendContext);
  LHandle := TSkiaApi.gr4d_directcontext_make_metal2(@LBackendContext, @gr_contextoptions_t(AOptions));
  if LHandle = 0 then
    Exit(nil);
  Result := TGRDirectContext.CreateNative(LHandle);
end;
{$ENDIF}

procedure TGRDirectContext.PerformDeferredCleanup(const AMilliseconds: Int64);
begin
  TSkiaApi.gr4d_directcontext_perform_deferred_cleanup(GetHandle, AMilliseconds);
end;

procedure TGRDirectContext.PurgeUnlockedResources(
  const AScratchResourcesOnly: Boolean);
begin
  TSkiaApi.gr4d_directcontext_purge_unlocked_resources(GetHandle, AScratchResourcesOnly);
end;

procedure TGRDirectContext.PurgeUnlockedResources(
  const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean);
begin
  TSkiaApi.gr4d_directcontext_purge_unlocked_resources2(GetHandle, ABytesToPurge, APreferScratchResources);
end;

procedure TGRDirectContext.ReleaseResourcesAndAbandonContext;
begin
  TSkiaApi.gr4d_directcontext_release_resources_and_abandon_context(GetHandle);
end;

procedure TGRDirectContext.ResetContext(const AState: Cardinal);
begin
  TSkiaApi.gr4d_directcontext_reset_context(GetHandle, AState);
end;

procedure TGRDirectContext.ResetContext;
begin
  ResetContext($FFFFFFFF);
end;

procedure TGRDirectContext.ResetGLTextureBindings;
begin
  TSkiaApi.gr4d_directcontext_reset_gl_texture_bindings(GetHandle);
end;

procedure TGRDirectContext.SetResourceCacheLimit(const AValue: NativeUInt);
begin
  TSkiaApi.gr4d_directcontext_set_resource_cache_limit(GetHandle, AValue);
end;

function TGRDirectContext.Submit(const ASyncCPU: Boolean): Boolean;
begin
  Result := TSkiaApi.gr4d_directcontext_submit(GetHandle, ASyncCPU);
end;

{ TGRBackendRenderTarget }

constructor TGRBackendRenderTarget.CreateGL(const AWidth, AHeight, ASampleCount,
  AStencilBits: Integer; const AFramebufferInfo: TGRGLFramebufferInfo);
begin
  CreateNative(TSkiaApi.gr4d_backendrendertarget_create_gl(AWidth, AHeight, ASampleCount, AStencilBits, @gr_gl_framebufferinfo_t(AFramebufferInfo)))
end;

{$IFDEF SK_METAL}
constructor TGRBackendRenderTarget.CreateMetal(const AWidth, AHeight: Integer;
  const ATextureInfo: TGRMTLTextureInfo);
var
  LTextureInfo: gr_mtl_textureinfo_t;
begin
  LTextureInfo := gr_mtl_textureinfo_t(ATextureInfo);
  CreateNative(TSkiaApi.gr4d_backendrendertarget_create_mtl(AWidth, AHeight, @LTextureInfo));
end;
{$ENDIF}

class procedure TGRBackendRenderTarget.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.gr4d_backendrendertarget_destroy(AHandle);
end;

function TGRBackendRenderTarget.GetBackendAPI: TGRBackendAPI;
begin
  Result := TGRBackendAPI(TSkiaApi.gr4d_backendrendertarget_get_backend_api(GetHandle));
end;

function TGRBackendRenderTarget.GetGLFramebufferInfo(
  out AFramebufferInfo: TGRGLFramebufferInfo): Boolean;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_get_gl_framebuffer_info(GetHandle, gr_gl_framebufferinfo_t(AFramebufferInfo));
end;

function TGRBackendRenderTarget.GetHeight: Integer;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_get_height(GetHandle);
end;

function TGRBackendRenderTarget.GetSampleCount: Integer;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_get_sample_count(GetHandle);
end;

function TGRBackendRenderTarget.GetStencilBits: Integer;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_get_stencil_bits(GetHandle);
end;

function TGRBackendRenderTarget.GetWidth: Integer;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_get_width(GetHandle);
end;

function TGRBackendRenderTarget.IsValid: Boolean;
begin
  Result := TSkiaApi.gr4d_backendrendertarget_is_valid(GetHandle);
end;

{ TGRBackendTexture }

constructor TGRBackendTexture.CreateGL(const AWidth, AHeight: Integer;
  const AMipmapped: Boolean; const ATextureInfo: TGRGLTextureInfo);
begin
  CreateNative(TSkiaApi.gr4d_backendtexture_create_gl(AWidth, AHeight, AMipmapped, @gr_gl_textureinfo_t(ATextureInfo)));
end;

{$IFDEF SK_METAL}
constructor TGRBackendTexture.CreateMetal(const AWidth, AHeight: Integer;
  const AMipmapped: Boolean; const ATextureInfo: TGRMTLTextureInfo);
var
  LTextureInfo: gr_mtl_textureinfo_t;
begin
  LTextureInfo := gr_mtl_textureinfo_t(ATextureInfo);
  CreateNative(TSkiaApi.gr4d_backendtexture_create_mtl(AWidth, AHeight, AMipmapped, @LTextureInfo));
end;
{$ENDIF}

class procedure TGRBackendTexture.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.gr4d_backendtexture_destroy(AHandle);
end;

function TGRBackendTexture.GetBackendAPI: TGRBackendAPI;
begin
  Result := TGRBackendAPI(TSkiaApi.gr4d_backendtexture_get_backend_api(GetHandle));
end;

function TGRBackendTexture.GetGLTextureInfo(
  out ATextureInfo: TGRGLTextureInfo): Boolean;
begin
  Result := TSkiaApi.gr4d_backendtexture_get_gl_texture_info(GetHandle, gr_gl_textureinfo_t(ATextureInfo));
end;

function TGRBackendTexture.GetHeight: Integer;
begin
  Result := TSkiaApi.gr4d_backendtexture_get_height(GetHandle);
end;

function TGRBackendTexture.GetWidth: Integer;
begin
  Result := TSkiaApi.gr4d_backendtexture_get_width(GetHandle);
end;

function TGRBackendTexture.HasMipmaps: Boolean;
begin
  Result := TSkiaApi.gr4d_backendtexture_has_mipmaps(GetHandle);
end;

function TGRBackendTexture.IsValid: Boolean;
begin
  Result := TSkiaApi.gr4d_backendtexture_is_valid(GetHandle);
end;

{ TSkHighContrastConfig }

constructor TSkHighContrastConfig.Create(const AGrayscale: Boolean;
  const AInvertStyle: TSkHighContrastConfigInvertStyle;
  const AContrast: Single);
begin
  Grayscale   := AGrayscale;
  InvertStyle := AInvertStyle;
  Contrast    := AContrast;
end;

{ TSkColorFilter }

class function TSkColorFilter.MakeBlend(const AColor: TAlphaColor;
  const AMode: TSkBlendMode): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_blend(AColor, sk_blendmode_t(AMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeCompose(const AOuter,
  AInner: ISkColorFilter): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  if not Assigned(AOuter) then
    raise ESkColorFilter.CreateFmt(ParameterCannotBeNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkColorFilter.CreateFmt(ParameterCannotBeNil, ['AInner']);
  LHandle := TSkiaApi.sk4d_colorfilter_make_compose(AOuter.Handle, AInner.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeHighContrast(
  const AConfig: TSkHighContrastConfig): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_high_contrast(@sk_highcontrastconfig_t(AConfig));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeHSLAMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_hsla_matrix(@AMatrix);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeLerp(const AWeight: Single; const ADest,
  ASrc: ISkColorFilter): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_lerp(AWeight, SafeHandle(ADest), SafeHandle(ASrc));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeLighting(const AMultiply,
  AAdd: TAlphaColor): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_lighting(AMultiply, AAdd);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeLinearToSRGBGamma: ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_linear_to_srgb_gamma();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeLumaColor: ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_luma_color();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_matrix(@AMatrix);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeOverdraw(
  const AColors: TSkOverdrawColor): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_overdraw(@AColors);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeSRGBToLinearGamma: ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_srgb_to_linear_gamma();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

class function TSkColorFilter.MakeTable(
  const ATable: TSkTableFilter): ISkColorFilter;
begin
  Result := MakeTable(ATable, ATable, ATable, ATable);
end;

class function TSkColorFilter.MakeTable(const ATableA, ATableR, ATableG,
  ATableB: TSkTableFilter): ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_colorfilter_make_table(@ATableA, @ATableR, @ATableG, @ATableB);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

{ TSkShader }

class function TSkShader.MakeBlend(const AMode: TSkBlendMode; const ADest,
  ASrc: ISkShader): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_blend(sk_blendmode_t(AMode), SafeHandle(ADest), SafeHandle(ASrc));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeColor(const AColor: TAlphaColor): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_color(AColor);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeColor(const AColor: TAlphaColorF;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_color2(@sk_color4f_t(AColor), SafeHandle(AColorSpace));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeEmpty: ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_empty();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const ATileMode: TSkTileMode;
  const APositions: TArray<Single>;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LHandle: sk_shader_t;
  LPoints: array[0..1] of TPointF;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LPoints[0] := AStart;
  LPoints[1] := AEnd;
  LHandle := TSkiaApi.sk4d_shader_make_gradient_linear2(@sk_point_t(LPoints[0]), @sk_color4f_t(AColors[0]), SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColor>; const ATileMode: TSkTileMode;
  const APositions: TArray<Single>): ISkShader;
var
  LHandle: sk_shader_t;
  LPoints: array[0..1] of TPointF;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LPoints[0] := AStart;
  LPoints[1] := AEnd;
  LHandle := TSkiaApi.sk4d_shader_make_gradient_linear(@sk_point_t(LPoints[0]), @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode): ISkShader;
var
  LColors: TArray<TAlphaColor>;
begin
  SetLength(LColors, 2);
  LColors[0] := AColor1;
  LColors[1] := AColor2;
  Result := MakeGradientLinear(AStart, AEnd, LColors, ATileMode);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LColors: TArray<TAlphaColorF>;
begin
  SetLength(LColors, 2);
  LColors[0] := AColor1;
  LColors[1] := AColor2;
  Result := MakeGradientLinear(AStart, AEnd, LColors, ATileMode, nil, AColorSpace);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor;
  const ATileMode: TSkTileMode): ISkShader;
var
  LColors: TArray<TAlphaColor>;
begin
  SetLength(LColors, 2);
  LColors[0] := ACenterColor;
  LColors[1] := AEdgeColor;
  Result := MakeGradientRadial(ACenter, ARadius, LColors, ATileMode);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const ATileMode: TSkTileMode; const APositions: TArray<Single>;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LHandle: sk_shader_t;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LHandle := TSkiaApi.sk4d_shader_make_gradient_radial2(@sk_point_t(ACenter), ARadius, @sk_color4f_t(AColors[0]), SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColor>;
  const ATileMode: TSkTileMode; const APositions: TArray<Single>): ISkShader;
var
  LHandle: sk_shader_t;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LHandle := TSkiaApi.sk4d_shader_make_gradient_radial(@sk_point_t(ACenter), ARadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF;
  const ATileMode: TSkTileMode; const AColorSpace: ISkColorSpace): ISkShader;
var
  LColors: TArray<TAlphaColorF>;
begin
  SetLength(LColors, 2);
  LColors[0] := ACenterColor;
  LColors[1] := AEdgeColor;
  Result := MakeGradientRadial(ACenter, ARadius, LColors, ATileMode, nil, AColorSpace);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColorF;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LColors: TArray<TAlphaColorF>;
begin
  SetLength(LColors, 2);
  LColors[0] := AColor1;
  LColors[1] := AColor2;
  Result := MakeGradientSweep(ACenter, LColors, nil, AColorSpace);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColor): ISkShader;
var
  LColors: TArray<TAlphaColor>;
begin
  SetLength(LColors, 2);
  LColors[0] := AColor1;
  LColors[1] := AColor2;
  Result := MakeGradientSweep(ACenter, LColors);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColor>;
  const APositions: TArray<Single>): ISkShader;
var
  LHandle: sk_shader_t;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LHandle := TSkiaApi.sk4d_shader_make_gradient_sweep(@sk_point_t(ACenter), @AColors[0], @APositions[0], Length(AColors));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const AColorSpace: ISkColorSpace): ISkShader;
var
  LHandle: sk_shader_t;
begin
  if Length(AColors) = 0 then
    raise ESkShader.CreateFmt(CannotBeEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkShader.CreateFmt(MatchLength, ['AColors', 'APositions']);
  LHandle := TSkiaApi.sk4d_shader_make_gradient_sweep2(@sk_point_t(ACenter), @sk_color4f_t(AColors[0]), SafeHandle(AColorSpace), @APositions[0], Length(AColors));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

function TSkShader.MakeWithColorFilter(
  const AFilter: ISkColorFilter): ISkShader;
var
  LHandle: sk_shader_t;
begin
  if not Assigned(AFilter) then
    raise ESkShader.CreateFmt(ParameterCannotBeNil, ['AFilter']);
  LHandle := TSkiaApi.sk4d_shader_make_with_color_filter(GetHandle, AFilter.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

function TSkShader.MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_shader_make_with_local_matrix(GetHandle, @sk_matrix_t(AMatrix));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

{ TSkMaskFilter }

class function TSkMaskFilter.MakeBlur(const AStyle: TSkBlurStyle;
  const ASigma: Single; const ARespectCTM: Boolean): ISkMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := TSkiaApi.sk4d_maskfilter_make_blur(sk_blurstyle_t(AStyle), ASigma, ARespectCTM);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkMaskFilter.CreateNative(LHandle);
end;

class function TSkMaskFilter.MakeTable(
  const ATable: TSkTableFilter): ISkMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := TSkiaApi.sk4d_maskfilter_make_table(@ATable);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkMaskFilter.CreateNative(LHandle);
end;

class function TSkMaskFilter.MakeTableClip(const AMin,
  AMax: Byte): ISkMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := TSkiaApi.sk4d_maskfilter_make_table_clip(AMin, AMax);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkMaskFilter.CreateNative(LHandle);
end;

class function TSkMaskFilter.MakeTableGamma(
  const AGamma: Single): ISkMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := TSkiaApi.sk4d_maskfilter_make_table_gamma(AGamma);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkMaskFilter.CreateNative(LHandle);
end;

{ TSkRuntimeEffectUniform }

function TSkRuntimeEffectUniform.GetByteSize: NativeUInt;
begin
  Result := TSkiaApi.sk4d_runtimeeffectuniform_get_byte_size(GetHandle);
end;

function TSkRuntimeEffectUniform.GetName: string;
begin
  Result := string(TSkiaApi.sk4d_runtimeeffectuniform_get_name(GetHandle));
end;

function TSkRuntimeEffectUniform.GetOffset: NativeUInt;
begin
  Result := TSkiaApi.sk4d_runtimeeffectuniform_get_offset(GetHandle);
end;

{ TSkRuntimeEffect }

function TSkRuntimeEffect.FindUniform(
  const AName: string): ISkRuntimeEffectUniform;
var
  LHandle: sk_runtimeeffectuniform_t;
begin
  LHandle := TSkiaApi.sk4d_runtimeeffect_find_uniform(GetHandle, MarshaledAString(UTF8String(AName)));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkRuntimeEffectUniform.CreateNative(LHandle, False);
end;

function TSkRuntimeEffect.GetChildren(const AIndex: NativeUInt): string;
begin
  Result := string(TSkiaApi.sk4d_runtimeeffect_get_children(GetHandle, AIndex));
end;

function TSkRuntimeEffect.GetChildrenCount: NativeUInt;
begin
  Result := TSkiaApi.sk4d_runtimeeffect_get_children_count(GetHandle);
end;

function TSkRuntimeEffect.GetUniform(
  const AIndex: NativeUInt): ISkRuntimeEffectUniform;
var
  LHandle: sk_runtimeeffectuniform_t;
begin
  LHandle := TSkiaApi.sk4d_runtimeeffect_get_uniform(GetHandle, AIndex);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkRuntimeEffectUniform.CreateNative(LHandle, False);
end;

function TSkRuntimeEffect.GetUniformCount: NativeUInt;
begin
  Result := TSkiaApi.sk4d_runtimeeffect_get_uniform_count(GetHandle);
end;

function TSkRuntimeEffect.GetUniformSize: NativeUInt;
begin
  Result := TSkiaApi.sk4d_runtimeeffect_get_uniform_size(GetHandle);
end;

class function TSkRuntimeEffect.Make(const ASkSL: string): ISkRuntimeEffect;
var
  LHandle: sk_runtimeeffect_t;
begin
  LHandle := TSkiaApi.sk4d_runtimeeffect_make(MarshaledAString(UTF8String(ASkSL)), 0);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkRuntimeEffect.CreateNative(LHandle, False);
end;

class function TSkRuntimeEffect.Make(const ASkSL: string;
  out AError: string): ISkRuntimeEffect;
var
  LError: ISkString;
  LHandle: sk_runtimeeffect_t;
begin
  LError  := TSkString.Create;
  LHandle := TSkiaApi.sk4d_runtimeeffect_make(MarshaledAString(UTF8String(ASkSL)), LError.Handle);
  if LHandle = 0 then
  begin
    AError := LError.Text;
    Exit(nil);
  end;
  Result := TSkRuntimeEffect.CreateNative(LHandle, False);
end;

function TSkRuntimeEffect.MakeColorFilter(const AUniform: TBytes;
  const AChildren: TArray<ISkColorFilter>): ISkColorFilter;
var
  I: Integer;
  LChildren: TArray<sk_colorfilter_t>;
  LHandle: sk_colorfilter_t;
  LUniform: ISkData;
begin
  SetLength(LChildren, Length(AChildren));
  for I := Low(AChildren) to High(AChildren) do
  begin
    if not Assigned(AChildren[I]) then
      raise ESkRuntimeEffect.CreateFmt(ElementsCannotBeNil, ['AChildren']);
    LChildren[I] := AChildren[I].Handle;
  end;
  LUniform := TSkData.Create(AUniform);
  LHandle  := TSkiaApi.sk4d_runtimeeffect_make_color_filter(GetHandle, LUniform.Handle, @LChildren[0], Length(LChildren));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

function TSkRuntimeEffect.MakeShader(const AIsOpaque: Boolean;
  const AUniform: TBytes; const AChildren: TArray<ISkShader>): ISkShader;
var
  I: Integer;
  LChildren: TArray<sk_shader_t>;
  LHandle: sk_shader_t;
  LUniform: ISkData;
begin
  SetLength(LChildren, Length(AChildren));
  for I := Low(AChildren) to High(AChildren) do
  begin
    if not Assigned(AChildren[I]) then
      raise ESkRuntimeEffect.CreateFmt(ElementsCannotBeNil, ['AChildren']);
    LChildren[I] := AChildren[I].Handle;
  end;
  LUniform := TSkData.Create(AUniform);
  LHandle  := TSkiaApi.sk4d_runtimeeffect_make_color_filter(GetHandle, LUniform.Handle, @LChildren[0], Length(LChildren));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

{ TSkPicture }

function TSkPicture.GetCullRect: TRectF;
begin
  TSkiaApi.sk4d_picture_get_cull_rect(GetHandle, sk_rect_t(Result));
end;

function TSkPicture.GetUniqueID: Cardinal;
begin
  Result := TSkiaApi.sk4d_picture_get_unique_id(GetHandle);
end;

class function TSkPicture.MakeFromBytes(const ABytes: TBytes): ISkPicture;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create(ABytes);
  try
    Result := MakeFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

class function TSkPicture.MakeFromStream(const AStream: TStream): ISkPicture;
var
  LHandle: sk_picture_t;
  LStream: ISkStream;
begin
  LStream := TSkManagedStream.Create(AStream);
  LHandle := TSkiaApi.sk4d_picture_make_from_stream(LStream.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPicture.CreateNative(LHandle);
end;

function TSkPicture.MakeShader(const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_picture_make_shader(GetHandle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

function TSkPicture.SerializeToBytes: TBytes;
var
  LStream: TBytesStream;
begin
  LStream := TBytesStream.Create(nil);
  try
    SerializeToStream(LStream);
    Result := LStream.Bytes;
  finally
    LStream.Free;
  end;
end;

procedure TSkPicture.SerializeToStream(const AStream: TStream);
var
  LWStream: ISkWStream;
begin
  LWStream := TSkManagedWStream.Create(AStream);
  TSkiaApi.sk4d_picture_serialize_to_stream(GetHandle, LWStream.Handle);
end;

{ TSkPictureRecorder }

function TSkPictureRecorder.BeginRecording(const ABounds: TRectF): ISkCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := TSkiaApi.sk4d_picturerecorder_begin_recording(GetHandle, @sk_rect_t(ABounds));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkCanvas.CreateNative(LHandle, False);
end;

function TSkPictureRecorder.BeginRecording(const AWidth,
  AHeight: Single): ISkCanvas;
begin
  Result := BeginRecording(TRectF.Create(0, 0, AWidth, AHeight));
end;

constructor TSkPictureRecorder.Create;
begin
  CreateNative(TSkiaApi.sk4d_picturerecorder_create());
end;

class procedure TSkPictureRecorder.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_picturerecorder_destroy(AHandle);
end;

function TSkPictureRecorder.FinishRecording(
  const ACullRect: TRectF): ISkPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := TSkiaApi.sk4d_picturerecorder_finish_recording2(GetHandle, @sk_rect_t(ACullRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPicture.CreateNative(LHandle);
end;

function TSkPictureRecorder.FinishRecording: ISkPicture;
var
  LHandle: sk_picture_t;
begin
  LHandle := TSkiaApi.sk4d_picturerecorder_finish_recording(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPicture.CreateNative(LHandle);
end;

{ TSkImage }

function TSkImage.EncodeToBytes(
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LData: ISkData;
  LHandle: sk_data_t;
begin
  LHandle := TSkiaApi.sk4d_image_encode_to_data(GetHandle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
  if LHandle = 0 then
    Exit(nil);
  LData  := TSkData.CreateNative(LHandle);
  Result := LData.ToBytes;
end;

procedure TSkImage.EncodeToStream(const AStream: TStream;
  const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer);
var
  LData: ISkData;
  LHandle: sk_data_t;
begin
  LHandle := TSkiaApi.sk4d_image_encode_to_data(GetHandle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
  if LHandle = 0 then
    Exit;
  LData := TSkData.CreateNative(LHandle);
  LData.SaveToStream(AStream);
end;

function TSkImage.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(TSkiaApi.sk4d_image_get_alpha_type(GetHandle));
end;

function TSkImage.GetColorSpace: ISkColorSpace;
var
  LHandle: sk_colorspace_t;
begin
  LHandle := TSkiaApi.sk4d_image_get_color_space(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorSpace.CreateNative(LHandle);
end;

function TSkImage.GetColorType: TSkColorType;
begin
  Result := TSkColorType(TSkiaApi.sk4d_image_get_color_type(GetHandle));
end;

function TSkImage.GetHeight: Integer;
begin
  Result := TSkiaApi.sk4d_image_get_height(GetHandle);
end;

function TSkImage.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  TSkiaApi.sk4d_image_get_image_info(GetHandle, LResult);
  Result := LResult.ToImageInfo;
end;

function TSkImage.GetUniqueID: Cardinal;
begin
  Result := TSkiaApi.sk4d_image_get_unique_id(GetHandle);
end;

function TSkImage.GetWidth: Integer;
begin
  Result := TSkiaApi.sk4d_image_get_width(GetHandle);
end;

function TSkImage.IsAlphaOnly: Boolean;
begin
  case GetColorType of
    TSkColorType.Alpha8,
    TSkColorType.AlphaF16,
    TSkColorType.Alpha16: Result := True;
  else
    Result := False;
  end;
end;

function TSkImage.IsLazyGenerated: Boolean;
begin
  Result := TSkiaApi.sk4d_image_is_lazy_generated(GetHandle);
end;

function TSkImage.IsOpaque: Boolean;
begin
  Result := GetAlphaType = TSkAlphaType.Opaque;
end;

function TSkImage.IsTextureBacked: Boolean;
begin
  Result := TSkiaApi.sk4d_image_is_texture_backed(GetHandle);
end;

function TSkImage.IsValid(const AContext: IGRRecordingContext): Boolean;
begin
  Result := TSkiaApi.sk4d_image_is_valid(GetHandle, SafeHandle(AContext));
end;

class function TSkImage.MakeFromEncoded(const ABytes: TBytes): ISkImage;
var
  LData: ISkData;
  LHandle: sk_image_t;
begin
  LData   := TSkData.Create(ABytes);
  LHandle := TSkiaApi.sk4d_image_make_from_encoded(LData.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

class function TSkImage.MakeFromEncoded(const AStream: TStream): ISkImage;
var
  LData: ISkData;
  LHandle: sk_image_t;
begin
  LData   := TSkData.Create(AStream);
  LHandle := TSkiaApi.sk4d_image_make_from_encoded(LData.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.MakeNonTextureImage: ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_non_texture_image(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

class function TSkImage.MakeRaster(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeRaster(LPixmap);
end;

class function TSkImage.MakeRaster(const APixmap: ISkPixmap): ISkImage;
var
  LHandle: sk_image_t;
begin
  if not Assigned(APixmap) then
    raise ESkImage.CreateFmt(ParameterCannotBeNil, ['APixmap']);
  LHandle := TSkiaApi.sk4d_image_make_raster(APixmap.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.MakeRasterImage: ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_raster_image(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.MakeShader(const ATileModeX: TSkTileMode;
  ATileModeY: TSkTileMode): ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_shader(GetHandle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle);
end;

function TSkImage.MakeSubset(const ASubset: TRect;
  const AContext: IGRDirectContext): ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_subset(GetHandle, @sk_irect_t(ASubset), SafeHandle(AContext));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.MakeTextureImage(const AContext: IGRDirectContext;
  const AMipmapped: Boolean): ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_texture_image(GetHandle, SafeHandle(AContext), AMipmapped);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.MakeWithFilter(const AContext: IGRRecordingContext;
  const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect;
  out AOutSubset: TRect; out AOffset: TPoint): ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_image_make_with_filter(GetHandle, SafeHandle(AContext), SafeHandle(AFilter), @sk_irect_t(ASubset), @sk_irect_t(AClipBounds), sk_irect_t(AOutSubset), sk_ipoint_t(AOffset));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkImage.ReadPixels(const AContext: IGRDirectContext;
  const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer;
  const ADestRowBytes: NativeUInt; const ASrcX, ASrcY: Integer;
  const ACachingHint: TSkImageCachingHint): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(AContext, LPixmap, ASrcX, ASrcY);
end;

function TSkImage.ScalePixels(const ADest: ISkPixmap;
  const AFilterQuality: TSkFilterQuality;
  const ACachingHint: TSkImageCachingHint): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkImage.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_image_scale_pixels(GetHandle, ADest.Handle, sk_filterquality_t(AFilterQuality), sk_imagecachinghint_t(ACachingHint));
end;

function TSkImage.ReadPixels(const AContext: IGRDirectContext;
  const ADest: ISkPixmap; const ASrcX, ASrcY: Integer;
  const ACachingHint: TSkImageCachingHint): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkImage.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_image_read_pixels(GetHandle, SafeHandle(AContext), ADest.Handle, ASrcX, ASrcY, sk_imagecachinghint_t(ACachingHint));
end;

{ TSkImageFilter }

class function TSkImageFilter.MakeAlphaThreshold(const ARegion: ISkRegion;
  const AInnerMin, AOuterMax: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ARegion) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_alpha_threshold(ARegion.Handle,  AInnerMin, AOuterMax, SafeHandle(AInput));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeAlphaThreshold(const ARegion: TRect;
  const AInnerMin, AOuterMax: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create;
  LRegion.SetRect(ARegion);
  Result := MakeAlphaThreshold(LRegion, AInnerMin, AOuterMax, AInput);
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  const ACropRect: TRectF; AForeground: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ABackground) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ABackground']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, ABackground.Handle, SafeHandle(AForeground), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  AForeground: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ABackground) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ABackground']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, ABackground.Handle, SafeHandle(AForeground), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground, AForeground: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ABackground) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ABackground']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), ABackground.Handle, SafeHandle(AForeground), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground: ISkImageFilter; const ACropRect: TRectF;
  const AForeground: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ABackground) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ABackground']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), ABackground.Handle, SafeHandle(AForeground), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const AInput: ISkImageFilter; const ATileMode: TSkTileMode): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter;
  const ATileMode: TSkTileMode): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(AColorFilter) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['AColorFilter']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_colorfilter(AColorFilter.Handle, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(AColorFilter) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['AColorFilter']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_colorfilter(AColorFilter.Handle, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeCompose(const AOuter,
  AInner: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(AOuter) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['AInner']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_compose(AOuter.Handle, AInner.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single; const ADisplacement,
  AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ADisplacement) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ADisplacement']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, ADisplacement.Handle, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single;
  const ADisplacement: ISkImageFilter; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if not Assigned(ADisplacement) then
    raise ESkImageFilter.CreateFmt(ParameterCannotBeNil, ['ADisplacement']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, ADisplacement.Handle, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_distant_lit_diffuse(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_distant_lit_diffuse(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_distant_lit_specular(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_distant_lit_specular(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDropShadow(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_drop_shadow(ADX, ADY, ASigmaX, ASigmaY, AColor, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDropShadow(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_drop_shadow(ADX, ADY, ASigmaX, ASigmaY, AColor, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_drop_shadow_only( ADX, ADY, ASigmaX, ASigmaY, AColor, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADX, ADY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_drop_shadow_only(ADX, ADY, ASigmaX, ASigmaY, AColor, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage;
  const AFilterQuality: TSkFilterQuality): ISkImageFilter;
var
  LRect: TRectF;
begin
  if Assigned(AImage) then
    LRect := TRectF.Create(0, 0, AImage.Width, AImage.Height)
  else
    LRect := TRectF.Empty;
  Result := MakeImage(AImage, LRect, LRect, AFilterQuality);
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage; const ASrc,
  ADest: TRectF; const AFilterQuality: TSkFilterQuality): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_image(SafeHandle(AImage), @sk_rect_t(ASrc), @sk_rect_t(ADest), sk_filterquality_t(AFilterQuality));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_magnifier(@sk_rect_t(ASrc), AInset, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_magnifier(@sk_rect_t(ASrc), AInset, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSkTileMode;
  const AConvolveAlpha: Boolean; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if Length(AKernel) <> (AKernelSize.Width * AKernelSize.Height) then
    raise ESkImageFilter.CreateFmt(MatchDimensions, ['AKernel', 'AKernelSize']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_matrix_convolution(@sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, @sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSkTileMode;
  const AConvolveAlpha: Boolean; const ACropRect: TRectF;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  if Length(AKernel) <> (AKernelSize.Width * AKernelSize.Height) then
    raise ESkImageFilter.CreateFmt(MatchDimensions, ['AKernel', 'AKernelSize']);
  LHandle := TSkiaApi.sk4d_imagefilter_make_matrix_convolution(@sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, @sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMatrixTransform(const AMatrix: TMatrix;
  const AFilterQuality: TSkFilterQuality;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_matrix_transform(@sk_matrix_t(AMatrix), sk_filterquality_t(AFilterQuality), SafeHandle(AInput));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISkImageFilter): ISkImageFilter;
var
  LFilters: TArray<ISkImageFilter>;
begin
  SetLength(LFilters, 2);
  LFilters[0] := AFilter1;
  LFilters[1] := AFilter2;
  Result := MakeMerge(LFilters);
end;

class function TSkImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISkImageFilter; const ACropRect: TRectF): ISkImageFilter;
var
  LFilters: TArray<ISkImageFilter>;
begin
  SetLength(LFilters, 2);
  LFilters[0] := AFilter1;
  LFilters[1] := AFilter2;
  Result := MakeMerge(LFilters, ACropRect);
end;

class function TSkImageFilter.MakeMerge(
  const AFilters: TArray<ISkImageFilter>): ISkImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
  LHandle: sk_imagefilter_t;
begin
  if Length(AFilters) = 0 then
    raise ESkImageFilter.CreateFmt(CannotBeEmpty, ['AFilters']);
  SetLength(LFilters, Length(AFilters));
  for I := Low(AFilters) to High(AFilters) do
  begin
    if not Assigned(AFilters[I]) then
      raise ESkImageFilter.CreateFmt(ElementsCannotBeNil, ['AFilters']);
    LFilters[I] := AFilters[I].Handle;
  end;
  LHandle := TSkiaApi.sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeMerge(const AFilters: TArray<ISkImageFilter>;
  const ACropRect: TRectF): ISkImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
  LHandle: sk_imagefilter_t;
begin
  if Length(AFilters) = 0 then
    raise ESkImageFilter.CreateFmt(CannotBeEmpty, ['AFilters']);
  SetLength(LFilters, Length(AFilters));
  for I := Low(AFilters) to High(AFilters) do
  begin
    if not Assigned(AFilters[I]) then
      raise ESkImageFilter.CreateFmt(ElementsCannotBeNil, ['AFilters']);
    LFilters[I] := SafeHandle(AFilters[I]);
  end;
  LHandle := TSkiaApi.sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeOffset(const ADX, ADY: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_offset(ADX, ADY, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeOffset(const ADX, ADY: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_offset(ADX, ADY, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePicture(
  const APicture: ISkPicture): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_picture(SafeHandle(APicture), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePicture(const APicture: ISkPicture;
  const ACropRect: TRectF): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_picture(SafeHandle(APicture), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_point_lit_diffuse(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_point_lit_diffuse(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_point_lit_specular(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_point_lit_specular(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_spot_lit_diffuse(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_spot_lit_diffuse(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_spot_lit_specular(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_spot_lit_specular(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, SafeHandle(AInput), @sk_rect_t(ACropRect));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

class function TSkImageFilter.MakeTile(const ASrc, ADest: TRect;
  const AInput: ISkImageFilter): ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_imagefilter_make_tile(@sk_rect_t(ASrc), @sk_rect_t(ADest), SafeHandle(AInput));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

{ TSkSurfaceProperties }

constructor TSkSurfaceProperties.Create(const AFlags: TSkSurfacePropertiesFlags;
  const APixelGeometry: TSkPixelGeometry);
begin
  CreateNative(TSkiaApi.sk4d_sk_surfaceprops_create(Byte(AFlags), sk_pixelgeometry_t(APixelGeometry)));
end;

class procedure TSkSurfaceProperties.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_surfaceprops_destroy(AHandle);
end;

function TSkSurfaceProperties.GetFlags: TSkSurfacePropertiesFlags;
begin
  Result := TSkSurfacePropertiesFlags(Byte(TSkiaApi.sk4d_surfaceprops_get_flags(GetHandle)));
end;

function TSkSurfaceProperties.GetPixelGeometry: TSkPixelGeometry;
begin
  Result := TSkPixelGeometry(TSkiaApi.sk4d_surfaceprops_get_pixel_geometry(GetHandle));
end;

function TSkSurfaceProperties.IsEqual(
  const AProperties: ISkSurfaceProperties): Boolean;
begin
  if not Assigned(AProperties) then
    raise ESkSurfaceProperties.CreateFmt(ParameterCannotBeNil, ['AProperties']);
  Result := TSkiaApi.sk4d_surfaceprops_is_equal(GetHandle, AProperties.Handle);
end;

{ TSkSurface }

procedure TSkSurface.Draw(const ACanvas: ISkCanvas; const AX, AY: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(ACanvas) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['ACanvas']);
  TSkiaApi.sk4d_surface_draw(GetHandle, ACanvas.Handle, AX, AY, SafeHandle(APaint));
end;

procedure TSkSurface.Flush;
begin
  TSkiaApi.sk4d_surface_flush(GetHandle);
end;

procedure TSkSurface.FlushAndSubmit(const ASyncCPU: Boolean);
begin
  TSkiaApi.sk4d_surface_flush_and_submit(GetHandle, ASyncCPU);
end;

function TSkSurface.GetCanvas: ISkCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := TSkiaApi.sk4d_surface_get_canvas(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkCanvas.CreateNative(LHandle, False);
end;

function TSkSurface.GetHeight: Integer;
begin
  Result := TSkiaApi.sk4d_surface_get_height(GetHandle);
end;

function TSkSurface.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  TSkiaApi.sk4d_surface_get_image_info(GetHandle, LResult);
  Result := LResult.ToImageInfo;
end;

function TSkSurface.GetProperties: ISkSurfaceProperties;
begin
  Result := TSkSurfaceProperties.CreateNative(TSkiaApi.sk4d_surface_get_props(GetHandle), False);
end;

function TSkSurface.GetRecordingContext: IGRRecordingContext;
var
  LHandle: gr_recordingcontext_t;
begin
  LHandle := TSkiaApi.sk4d_surface_get_recording_context(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TGRRecordingContext.CreateNative(LHandle, False);
end;

function TSkSurface.GetWidth: Integer;
begin
  Result := TSkiaApi.sk4d_surface_get_width(GetHandle);
end;

{$IFDEF SK_METAL}
class function TSkSurface.MakeFromCAMetalLayer(
  const AContext: IGRRecordingContext; const ALayer: CAMetalLayer;
  const AOrigin: TGRSurfaceOrigin; const ASampleCount: Integer;
  const AColorType: TSkColorType; const AColorSpace: ISkColorSpace;
  const AProperties: ISkSurfaceProperties;
  out ADrawable: MTLDrawable): ISkSurface;
var
  LDrawable: gr_mtl_handle_t;
  LHandle: sk_surface_t;
begin
  if not Assigned(AContext) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['AContext']);
  LHandle := TSkiaApi.sk4d_surface_make_from_ca_metal_layer(AContext.Handle, NSObjectToID(ALayer), gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), SafeHandle(AColorSpace), SafeHandle(AProperties), LDrawable);
  if LHandle = 0 then
    Exit(nil);
  ADrawable := TMTLDrawable.Wrap(LDrawable);
  Result    := TSkSurface.CreateNative(LHandle);
end;

class function TSkSurface.MakeFromMTKView(const AContext: IGRRecordingContext;
  const AView: MTKView; const AOrigin: TGRSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  const AColorSpace: ISkColorSpace;
  const AProperties: ISkSurfaceProperties): ISkSurface;
var
  LHandle: sk_surface_t;
begin
  if not Assigned(AContext) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['AContext']);
  LHandle := TSkiaApi.sk4d_surface_make_from_mtk_view(AContext.Handle, NSObjectToID(AView), gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), SafeHandle(AColorSpace), SafeHandle(AProperties));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;
{$ENDIF}

class function TSkSurface.MakeFromRenderTarget(
  const AContext: IGRRecordingContext;
  const ARenderTarget: IGRBackendRenderTarget; const AOrigin: TGRSurfaceOrigin;
  const AColorType: TSkColorType; const AColorSpace: ISkColorSpace;
  const AProperties: ISkSurfaceProperties): ISkSurface;
var
  LHandle: sk_surface_t;
begin
  if not Assigned(AContext) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['AContext']);
  if not Assigned(ARenderTarget) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['ARenderTarget']);
  LHandle := TSkiaApi.sk4d_surface_make_from_rendertarget(AContext.Handle, ARenderTarget.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), SafeHandle(AColorSpace), SafeHandle(AProperties));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;

class function TSkSurface.MakeFromTexture(const AContext: IGRRecordingContext;
  const ATexture: IGRBackendTexture; const AOrigin: TGRSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  const AColorSpace: ISkColorSpace;
  const AProperties: ISkSurfaceProperties): ISkSurface;
var
  LHandle: sk_surface_t;
begin
  if not Assigned(AContext) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['ATexture']);
  LHandle := TSkiaApi.sk4d_surface_make_from_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), SafeHandle(AColorSpace), SafeHandle(AProperties));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;

function TSkSurface.MakeImageSnapshot(const ABounds: TRect): ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_surface_make_image_snapshot2(GetHandle, @sk_irect_t(ABounds));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

function TSkSurface.MakeImageSnapshot: ISkImage;
var
  LHandle: sk_image_t;
begin
  LHandle := TSkiaApi.sk4d_surface_make_image_snapshot(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImage.CreateNative(LHandle);
end;

class function TSkSurface.MakeNull(const AWidth, AHeight: Integer): ISkSurface;
var
  LHandle: sk_surface_t;
begin
  LHandle := TSkiaApi.sk4d_surface_make_null(AWidth, AHeight);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;

class function TSkSurface.MakeRaster(const AWidth,
  AHeight: Integer): ISkSurface;
begin
  Result := MakeRaster(TSkImageInfo.Create(AWidth, AHeight));
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const AProperties: ISkSurfaceProperties): ISkSurface;
begin
  Result := MakeRaster(AImageInfo, AImageInfo.MinRowBytes, AProperties) as ISkSurface;
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const ARowBytes: NativeUInt;
  const AProperties: ISkSurfaceProperties): ISkSurface;
var
  LHandle: sk_surface_t;
  LImageInfo: sk_imageinfo_t;
begin
  LImageInfo.FromImageInfo(AImageInfo);
  LHandle := TSkiaApi.sk4d_surface_make_raster(@LImageInfo, ARowBytes, SafeHandle(AProperties));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;

class function TSkSurface.MakeRasterDirect(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc;
  const AProperties: ISkSurfaceProperties): ISkSurface;
var
  LContext: Pointer;
  LHandle: sk_surface_t;
  LImageInfo: sk_imageinfo_t;
begin
  LImageInfo.FromImageInfo(AImageInfo);
  LContext := PPointer(@ARasterReleaseProc)^;
  LHandle  := TSkiaApi.sk4d_surface_make_raster_direct(@LImageInfo, APixels, ARowBytes, raster_release_proc, LContext, SafeHandle(AProperties));
  if LHandle = 0 then
    Exit(nil);
  if LContext <> nil then
    IInterface(LContext)._AddRef;
  Result := TSkSurface.CreateNative(LHandle);
end;

class function TSkSurface.MakeRasterDirect(const APixmap: ISkPixmap;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc;
  const AProperties: ISkSurfaceProperties): ISkSurface;
begin
  Result := MakeRasterDirect(APixmap.ImageInfo, APixmap.Pixels, APixmap.RowBytes, ARasterReleaseProc, AProperties);
end;

class function TSkSurface.MakeRenderTarget(const AContext: IGRDirectContext;
  const ABudgeted: Boolean; const AImageInfo: TSkImageInfo;
  const ASampleCount: Integer; const AOrigin: TGRSurfaceOrigin;
  const AProperties: ISkSurfaceProperties;
  const AShouldCreateWithMips: Boolean): ISkSurface;
var
  LHandle: sk_surface_t;
  LImageInfo: sk_imageinfo_t;
begin
  if not Assigned(AContext) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['AContext']);
  LImageInfo.FromImageInfo(AImageInfo);
  LHandle := TSkiaApi.sk4d_surface_make_render_target(AContext.Handle, ABudgeted, @LImageInfo, ASampleCount, gr_surfaceorigin_t(AOrigin), SafeHandle(AProperties), AShouldCreateWithMips);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSurface.CreateNative(LHandle);
end;

function TSkSurface.PeekPixels: ISkPixmap;
begin
  Result := TSkPixmap.Create;
  if not TSkiaApi.sk4d_surface_peek_pixels(GetHandle, Result.Handle) then
    Result := nil;
end;

class procedure TSkSurface.raster_release_proc(pixels, context: Pointer);
begin
  if context <> nil then
  begin
    TSkSurfaceRasterReleaseProc(context)(pixels);
    IInterface(context)._Release;
  end;
end;

function TSkSurface.ReadPixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer): Boolean;
var
  LPixmap: TSkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY);
end;

function TSkSurface.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['ADest']);
  Result := TSkiaApi.sk4d_surface_read_pixels(GetHandle, ADest.Handle, ASrcX, ASrcY);
end;

procedure TSkSurface.WritePixels(const ASrcImageInfo: TSkImageInfo;
  const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX,
  ADestY: Integer);
var
  LPixmap: TSkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  WritePixels(LPixmap, ADestX, ADestY);
end;

procedure TSkSurface.WritePixels(const ASrc: ISkPixmap; const ADestX,
  ADestY: Integer);
begin
  if not Assigned(ASrc) then
    raise ESkSurface.CreateFmt(ParameterCannotBeNil, ['ASrc']);
  TSkiaApi.sk4d_surface_write_pixels(GetHandle, ASrc.Handle, ADestX, ADestY);
end;

{ TSkVertices }

class function TSkVertices.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_vertices_is_unique(AHandle);
end;

class procedure TSkVertices.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_vertices_ref(AHandle);
end;

class procedure TSkVertices.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_vertices_unref(AHandle);
end;

function TSkVertices.GetUniqueID: Cardinal;
begin
  Result := TSkiaApi.sk4d_vertices_get_unique_id(GetHandle);
end;

class function TSkVertices.MakeCopy(const AVertexMode: TSkVertexMode;
  const APositions, ATextures: TArray<TPointF>;
  const AColors: TArray<TAlphaColor>;
  const AIndices: TArray<Word>): ISkVertices;
var
  LHandle: sk_vertices_t;
begin
  if Length(APositions) = 0 then
    raise ESkVertices.CreateFmt(CannotBeEmpty, ['APositions']);
  if (Length(ATextures) > 0) and (Length(ATextures) <> Length(APositions)) then
    raise ESkVertices.CreateFmt(MatchLength, ['APositions', 'ATextures']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(APositions)) then
    raise ESkVertices.CreateFmt(MatchLength, ['APositions', 'AColors']);
  LHandle := TSkiaApi.sk4d_vertices_make_copy(sk_vertexmode_t(AVerTexMode), Length(APositions), @sk_point_t(APositions[0]), @sk_point_t(ATextures[0]), @AColors[0], Length(AIndices), @AIndices[0]);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkVertices.CreateNative(LHandle)
end;

{ TSkFontMetrics }

class operator TSkFontMetrics.Equal(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
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

class operator TSkFontMetrics.NotEqual(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
begin
  Result := not (AFontMetrics1 = AFontMetrics2);
end;

{ TSkFontStyle }

class function TSkFontStyle.Bold: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Bold, TSkFontWidth.Normal, TSkFontSlant.Upright);
end;

class function TSkFontStyle.BoldItalic: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Bold, TSkFontWidth.Normal, TSkFontSlant.Italic);
end;

constructor TSkFontStyle.Create(const AWeight: TSkFontWeight;
  const AWidth: TSkFontWidth; const ASlant: TSkFontSlant);
begin
  Create(FontWeight[AWeight], FontWidth[AWidth], ASlant);
end;

constructor TSkFontStyle.Create(const AWeight, AWidth: Integer;
  const ASlant: TSkFontSlant);
begin
  Weight := AWeight;
  Width  := AWidth;
  Slant  := ASlant;
end;

class function TSkFontStyle.Italic: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Italic);
end;

class function TSkFontStyle.Normal: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Upright);
end;

{ TSkTypeface }

function TSkTypeface.GetFamilyName: string;
var
  LString: ISkString;
begin
  LString := TSkString.Create;
  TSkiaApi.sk4d_typeface_get_family_name(GetHandle, LString.Handle);
  Result := LString.Text;
end;

function TSkTypeface.GetSlant: TSkFontSlant;
begin
  Result := TSkFontSlant(TSkiaApi.sk4d_typeface_get_slant(GetHandle));
end;

function TSkTypeface.GetStyle: TSkFontStyle;
begin
  TSkiaApi.sk4d_typeface_get_style(GetHandle, sk_fontstyle_t(Result));
end;

function TSkTypeface.GetUniqueID: Cardinal;
begin
  Result := TSkiaApi.sk4d_typeface_get_unique_id(GetHandle);
end;

function TSkTypeface.GetWeight: Integer;
begin
  Result := TSkiaApi.sk4d_typeface_get_weight(GetHandle);
end;

function TSkTypeface.GetWidth: Integer;
begin
  Result := TSkiaApi.sk4d_typeface_get_width(GetHandle);
end;

function TSkTypeface.IsBold: Boolean;
begin
  Result := GetWeight >= TSkFontStyle.FontWeight[TSkFontWeight.SemiBold];
end;

function TSkTypeface.IsItalic: Boolean;
begin
  Result := GetSlant <> TSkFontSlant.Upright;
end;

class function TSkTypeface.MakeDefault: ISkTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := TSkiaApi.sk4d_typeface_make_default();
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

class function TSkTypeface.MakeFromBytes(const ABytes: TBytes;
  const ATTcIndex: Integer): ISkTypeface;
var
  LData: ISkData;
  LHandle: sk_typeface_t;
begin
  LData   := TSkData.Create(ABytes);
  LHandle := TSkiaApi.sk4d_typeface_make_from_data(LData.Handle, ATTcIndex);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

class function TSkTypeface.MakeFromFile(const AFileName: string;
  const ATTcIndex: Integer): ISkTypeface;
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := MakeFromStream(LStream, ATTcIndex);
  finally
    LStream.Free;
  end;
end;

class function TSkTypeface.MakeFromName(const AFamilyName: string;
  const AStyle: TSkFontStyle): ISkTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := TSkiaApi.sk4d_typeface_make_from_name(MarshaledAString(UTF8String(AFamilyName)), @sk_fontstyle_t(AStyle));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

class function TSkTypeface.MakeFromStream(const AStream: TStream;
  const ATTcIndex: Integer): ISkTypeface;
var
  LData: ISkData;
  LHandle: sk_typeface_t;
begin
  LData   := TSkData.Create(AStream);
  LHandle := TSkiaApi.sk4d_typeface_make_from_data(LData.Handle, ATTcIndex);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

{ TSkFont }

constructor TSkFont.Create(const ATypeface: ISkTypeface; const ASize, AScaleX,
  ASkewX: Single);
begin
  CreateNative(TSkiaApi.sk4d_font_create(SafeHandle(ATypeface), ASize, AScaleX, ASkewX));
end;

constructor TSkFont.Create(const AFont: ISkFont);
begin
  if not Assigned(AFont) then
    raise ESkFont.CreateFmt(ParameterCannotBeNil, ['AFont']);
  CreateNative(TSkiaApi.sk4d_font_create2(AFont.Handle));
end;

class procedure TSkFont.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_font_destroy(AHandle);
end;

function TSkFont.GetBaselineSnap: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_baseline_snap(GetHandle);
end;

function TSkFont.GetBounds(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<TRectF>;
begin
  SetLength(Result, Length(AGlyphs));
  TSkiaApi.sk4d_font_get_widths_bounds(GetHandle, @AGlyphs[0], Length(AGlyphs), nil, @sk_rect_t(Result[0]), SafeHandle(APaint));
end;

function TSkFont.GetEdging: TSkFontEdging;
begin
  Result := TSkFontEdging(TSkiaApi.sk4d_font_get_edging(GetHandle));
end;

function TSkFont.GetEmbeddedBitmaps: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_embedded_bitmaps(GetHandle);
end;

function TSkFont.GetEmbolden: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_embolden(GetHandle);
end;

function TSkFont.GetForceAutoHinting: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_force_auto_hinting(GetHandle);
end;

function TSkFont.GetGlyphs(const AText: string): TArray<Word>;
var
  LCount: Integer;
begin
  LCount := TSkiaApi.sk4d_font_get_glyphs_count(GetHandle, @AText[Low(AText)], Length(AText) * 2, UTF16_SK_TEXTENCODING);
  SetLength(Result, LCount);
  TSkiaApi.sk4d_font_get_glyphs(GetHandle, @AText[Low(AText)], Length(AText) * 2, UTF16_SK_TEXTENCODING, @Result[0], LCount);
end;

function TSkFont.GetHinting: TSkFontHinting;
begin
  Result := TSkFontHinting(TSkiaApi.sk4d_font_get_hinting(GetHandle));
end;

function TSkFont.GetLinearMetrics: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_linear_metrics(GetHandle);
end;

function TSkFont.GetMetrics(out AMetrics: TSkFontMetrics): Single;
var
  LMetrics: sk_fontmetrics_t;
begin
  Result   := TSkiaApi.sk4d_font_get_metrics(GetHandle, @LMetrics);
  AMetrics := LMetrics.ToFontMetrics;
end;

function TSkFont.GetOffsets(const AGlyphs: TArray<Word>;
  const AOrigin: Single): TArray<Single>;
begin
  SetLength(Result, Length(AGlyphs));
  TSkiaApi.sk4d_font_get_offsets(GetHandle, @AGlyphs[0], Length(AGlyphs), @Result[0], AOrigin);
end;

function TSkFont.GetPath(const AGlyph: Word): ISkPath;
begin
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_font_get_path(GetHandle, AGlyph, Result.Handle) then
    Result := nil;
end;

procedure TSkFont.GetPaths(const AGlyphs: TArray<Word>;
  const AProc: TSkFontGlyphPathProc);
begin
  if Assigned(AProc) then
    TSkiaApi.sk4d_font_get_paths(GetHandle, @AGlyphs[0], Length(AGlyphs), getpaths_proc, @AProc);
end;

class procedure TSkFont.getpaths_proc(const path: sk_path_t;
  const matrix: psk_matrix_t; context: Pointer);
var
  LPath: ISkPath;
begin
  if path = 0 then
    LPath := nil
  else
    LPath := TSkPath.CreateNative(path, False);
  TSkFontGlyphPathProc(context^)(LPath, TMatrix(matrix^));
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>;
begin
  Result := GetPositions(AGlyphs, TPointF.Create(0, 0));
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>;
  const AOrigin: TPointF): TArray<TPointF>;
begin
  SetLength(Result, Length(AGlyphs));
  TSkiaApi.sk4d_font_get_positions(GetHandle, @AGlyphs[0], Length(AGlyphs), @sk_point_t(Result[0]), @sk_point_t(AOrigin));
end;

function TSkFont.GetScaleX: Single;
begin
  Result := TSkiaApi.sk4d_font_get_scale_x(GetHandle);
end;

function TSkFont.GetSize: Single;
begin
  Result := TSkiaApi.sk4d_font_get_size(GetHandle);
end;

function TSkFont.GetSkewX: Single;
begin
  Result := TSkiaApi.sk4d_font_get_skew_x(GetHandle);
end;

function TSkFont.GetSpacing: Single;
begin
  Result := TSkiaApi.sk4d_font_get_metrics(GetHandle, nil);
end;

function TSkFont.GetSubpixel: Boolean;
begin
  Result := TSkiaApi.sk4d_font_get_subpixel(GetHandle);
end;

function TSkFont.GetTypeface: ISkTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := TSkiaApi.sk4d_font_get_typeface(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

function TSkFont.GetTypefaceOrDefault: ISkTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := TSkiaApi.sk4d_font_get_typeface_or_default(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

function TSkFont.GetWidths(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<Single>;
begin
  SetLength(Result, Length(AGlyphs));
  TSkiaApi.sk4d_font_get_widths_bounds(GetHandle, @AGlyphs[0], Length(AGlyphs), @Result[0], nil, SafeHandle(APaint));
end;

procedure TSkFont.GetWidthsAndBounds(const AGlyphs: TArray<Word>;
  out AWidths: TArray<Single>; out ABounds: TArray<TRectF>;
  const APaint: ISkPaint);
begin
  SetLength(AWidths, Length(AGlyphs));
  SetLength(ABounds, Length(AGlyphs));
  TSkiaApi.sk4d_font_get_widths_bounds(GetHandle, @AGlyphs[0], Length(AGlyphs), @AWidths[0], @sk_rect_t(ABounds[0]), SafeHandle(APaint));
end;

function TSkFont.IsEqual(const AFont: ISkFont): Boolean;
begin
  if not Assigned(AFont) then
    raise ESkFont.CreateFmt(ParameterCannotBeNil, ['AFont']);
  Result := TSkiaApi.sk4d_font_is_equal(GetHandle, AFont.Handle);
end;

function TSkFont.MakeWithSize(const ASize: Single): ISkFont;
begin
  Result := TSkFont.Create(Self);
  Result.Size := ASize;
end;

function TSkFont.MeasureText(const AText: string; out ABounds: TRectF;
  const APaint: ISkPaint): Single;
begin
  Result := TSkiaApi.sk4d_font_measure_text(GetHandle, @AText[Low(AText)], Length(AText) * 2, UTF16_SK_TEXTENCODING, @sk_rect_t(ABounds), SafeHandle(APaint));
end;

function TSkFont.MeasureText(const AText: string;
  const APaint: ISkPaint): Single;
begin
  Result := TSkiaApi.sk4d_font_measure_text(GetHandle, @AText[Low(AText)], Length(AText) * 2, UTF16_SK_TEXTENCODING, nil, SafeHandle(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  out ABounds: TRectF; const APaint: ISkPaint): Single;
begin
  Result := TSkiaApi.sk4d_font_measure_text(GetHandle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, @sk_rect_t(ABounds), SafeHandle(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): Single;
begin
  Result := TSkiaApi.sk4d_font_measure_text(GetHandle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, nil, SafeHandle(APaint));
end;

procedure TSkFont.SetBaselineSnap(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_baseline_snap(GetHandle, AValue);
end;

procedure TSkFont.SetEdging(const AValue: TSkFontEdging);
begin
  TSkiaApi.sk4d_font_set_edging(GetHandle, sk_fontedging_t(AValue));
end;

procedure TSkFont.SetEmbeddedBitmaps(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_embedded_bitmaps(GetHandle, AValue);
end;

procedure TSkFont.SetEmbolden(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_embolden(GetHandle, AValue);
end;

procedure TSkFont.SetForceAutoHinting(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_force_auto_hinting(GetHandle, AValue);
end;

procedure TSkFont.SetHinting(const AValue: TSkFontHinting);
begin
  TSkiaApi.sk4d_font_set_hinting(GetHandle, sk_fonthinting_t(AValue));
end;

procedure TSkFont.SetLinearMetrics(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_linear_metrics(GetHandle, AValue);
end;

procedure TSkFont.SetScaleX(const AValue: Single);
begin
  TSkiaApi.sk4d_font_set_scale_x(GetHandle, AValue);
end;

procedure TSkFont.SetSize(const AValue: Single);
begin
  TSkiaApi.sk4d_font_set_size(GetHandle, AValue);
end;

procedure TSkFont.SetSkewX(const AValue: Single);
begin
  TSkiaApi.sk4d_font_set_skew_x(GetHandle, AValue);
end;

procedure TSkFont.SetSubpixel(const AValue: Boolean);
begin
  TSkiaApi.sk4d_font_set_subpixel(GetHandle, AValue);
end;

procedure TSkFont.SetTypeface(const AValue: ISkTypeface);
begin
  TSkiaApi.sk4d_font_set_typeface(GetHandle, SafeHandle(AValue));
end;

function TSkFont.UnicharsToGlyphs(
  const AUnichars: TArray<Integer>): TArray<Word>;
begin
  SetLength(Result, Length(AUnichars));
  TSkiaApi.sk4d_font_unichars_to_glyphs(GetHandle, @AUnichars[0], Length(AUnichars), @Result[0]);
end;

function TSkFont.UnicharToGlyph(const AUnichar: Integer): Word;
begin
  Result := TSkiaApi.sk4d_font_unichar_to_glyph(GetHandle, AUnichar);
end;

{ TSkRotationScaleMatrix }

constructor TSkRotationScaleMatrix.Create(const AScale, ARadians, ATX, ATY,
  AAnchorX, AAnchorY: Single);
begin
  SCos := Cos(ARadians) * AScale;
  SSin := Sin(ARadians) * AScale;
  TX   := ATX + - SCos * AAnchorX + SSin * AAnchorY;
  TY   := ATY + - SSin * AAnchorX - SCos * AAnchorY;
end;

constructor TSkRotationScaleMatrix.CreateRotation(const ARadians, AAnchorX,
  AAnchorY: Single);
begin
  Create(1, ARadians, 0, 0, AAnchorX, AAnchorY);
end;

constructor TSkRotationScaleMatrix.CreateScale(const AScale: Single);
begin
  SCos := AScale;
  SSin := 0;
  TX   := 0;
  TY   := 0;
end;

constructor TSkRotationScaleMatrix.CreateTranslation(const AX, AY: Single);
begin
  SCos := 1;
  SSin := 0;
  TX   := AX;
  TY   := AY;
end;

class function TSkRotationScaleMatrix.Identity: TSkRotationScaleMatrix;
begin
  Result.SCos := 1;
  Result.SSin := 0;
  Result.TX   := 0;
  Result.TY   := 0;
end;

{ TSkRunBuffer }

constructor TSkRunBuffer.Create(const AGlyphs, APositions: Pointer;
  const ACount: Integer);
begin
  inherited Create;
  FGlyphs    := AGlyphs;
  FPositions := APositions;
  FCount     := ACount;
end;

procedure TSkRunBuffer.FromGlyphArray(const AArray: TArray<Word>);
begin
  if Length(AArray) <> FCount then
    raise ESkRunBuffer.CreateFmt(MatchLength, ['AArray', 'Glyphs']);
  Move(AArray[0], FGlyphs^, FCount * SizeOf(Word));
end;

procedure TSkRunBuffer.FromHorizontalPositionArray(
  const AArray: TArray<Single>);
begin
  if Length(AArray) <> FCount then
    raise ESkRunBuffer.CreateFmt(MatchLength, ['AArray', 'Glyphs']);
  Move(AArray[0], FPositions^, FCount * SizeOf(Single));
end;

procedure TSkRunBuffer.FromPositionedPositionArray(
  const AArray: TArray<TPointF>);
begin
  if Length(AArray) <> FCount then
    raise ESkRunBuffer.CreateFmt(MatchLength, ['AArray', 'Glyphs']);
  Move(AArray[0], FPositions^, FCount * SizeOf(TPointF));
end;

procedure TSkRunBuffer.FromRotationScalePositionArray(
  const AArray: TArray<TSkRotationScaleMatrix>);
begin
  if Length(AArray) <> FCount then
    raise ESkRunBuffer.CreateFmt(MatchLength, ['AArray', 'Glyphs']);
  Move(AArray[0], FPositions^, FCount * SizeOf(TSkRotationScaleMatrix));
end;

function TSkRunBuffer.GetCount: Integer;
begin
  Result := FCount;
end;

function TSkRunBuffer.GetGlyph(const AIndex: Integer): Word;
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  Result := PGlyphs(FGlyphs)[AIndex];
end;

function TSkRunBuffer.GetGlyphsAddress: Pointer;
begin
  Result := FGlyphs;
end;

function TSkRunBuffer.GetHorizontalPosition(const AIndex: Integer): Single;
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  Result := PHorizontalPositions(FGlyphs)[AIndex];
end;

function TSkRunBuffer.GetPositionedPosition(const AIndex: Integer): TPointF;
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  Result := PPositionedPositions(FGlyphs)[AIndex];
end;

function TSkRunBuffer.GetPositionsAddress: Pointer;
begin
  Result := FPositions;
end;

function TSkRunBuffer.GetRotationScalePosition(
  const AIndex: Integer): TSkRotationScaleMatrix;
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  Result := PRotationScalePositions(FGlyphs)[AIndex];
end;

procedure TSkRunBuffer.SetGlyph(const AIndex: Integer; const AValue: Word);
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  PGlyphs(FGlyphs)[AIndex] := AValue;
end;

procedure TSkRunBuffer.SetHorizontalPosition(const AIndex: Integer;
  const AValue: Single);
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  PHorizontalPositions(FGlyphs)[AIndex] := AValue;
end;

procedure TSkRunBuffer.SetPositionedPosition(const AIndex: Integer;
  const AValue: TPointF);
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  PPositionedPositions(FGlyphs)[AIndex] := AValue;
end;

procedure TSkRunBuffer.SetRotationScalePosition(const AIndex: Integer;
  const AValue: TSkRotationScaleMatrix);
begin
  if AIndex >= FCount then
    raise ESkRunBuffer.Create(ArgumentOutOfRange);
  PRotationScalePositions(FGlyphs)[AIndex] := AValue;
end;

function TSkRunBuffer.ToGlyphArray: TArray<Word>;
begin
  SetLength(Result, FCount);
  Move(FGlyphs^, Result[0], FCount * SizeOf(Word));
end;

function TSkRunBuffer.ToHorizontalPositionArray: TArray<Single>;
begin
  SetLength(Result, FCount);
  Move(FGlyphs^, Result[0], FCount * SizeOf(Single));
end;

function TSkRunBuffer.ToPositionedPositionArray: TArray<TPointF>;
begin
  SetLength(Result, FCount);
  Move(FGlyphs^, Result[0], FCount * SizeOf(TPointF));
end;

function TSkRunBuffer.ToRotationScalePositionArray: TArray<TSkRotationScaleMatrix>;
begin
  SetLength(Result, FCount);
  Move(FGlyphs^, Result[0], FCount * SizeOf(TSkRotationScaleMatrix));
end;

{ TSkTextBlob }

class function TSkTextBlob.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_textblob_is_unique(AHandle);
end;

class procedure TSkTextBlob.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_textblob_ref(AHandle);
end;

class procedure TSkTextBlob.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_textblob_unref(AHandle);
end;

function TSkTextBlob.GetIntercepts(const AUpperBounds, ALowerBounds: Single;
  const APaint: ISkPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
  LCount: Integer;
begin
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  LCount := TSkiaApi.sk4d_textblob_get_intercepts(GetHandle, @LBounds[0], nil, SafeHandle(APaint));
  SetLength(Result, LCount);
  TSkiaApi.sk4d_textblob_get_intercepts(GetHandle, @LBounds[0], @Result[0], SafeHandle(APaint));
end;

class function TSkTextBlob.Make(const AText: string; const AFont: ISkFont;
  const AOrigin: TPointF): ISkTextBlob;
var
  LGlyphs: TArray<Word>;
begin
  LGlyphs := AFont.GetGlyphs(AText);
  Result  := MakePositionedGlyphs(LGlyphs, AFont.GetPositions(LGlyphs, AOrigin), AFont);
end;

class function TSkTextBlob.Make(const AText: string;
  const AFont: ISkFont): ISkTextBlob;
begin
  Result := Make(AText, AFont, TPointF.Create(0, 0));
end;

class function TSkTextBlob.MakeGlyphs(const AGlyphs: TArray<Word>;
  const AFont: ISkFont): ISkTextBlob;
begin
  Result := MakeGlyphs(AGlyphs, AFont, TPointF.Create(0, 0));
end;

class function TSkTextBlob.MakeGlyphs(const AGlyphs: TArray<Word>;
  const AFont: ISkFont; const AOrigin: TPointF): ISkTextBlob;
begin
  Result := MakePositionedGlyphs(AGlyphs, AFont.GetPositions(AGlyphs, AOrigin), AFont);
end;

class function TSkTextBlob.MakeHorizontal(const AText: string;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISkFont): ISkTextBlob;
begin
  Result := MakeHorizontalGlyphs(AFont.GetGlyphs(AText), APositions, AY, AFont);
end;

class function TSkTextBlob.MakeHorizontalGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<Single>; const AY: Single;
  const AFont: ISkFont): ISkTextBlob;
var
  LBuffer: ISkHorizontalRunBuffer;
  LBuilder: ISkTextBlobBuilder;
begin
  if not Assigned(AFont) then
    raise ESkTextBlob.CreateFmt(ParameterCannotBeNil, ['AFont']);
  if Length(AGlyphs) <> Length(APositions) then
    raise ESkTextBlob.CreateFmt(MatchLength, ['AGlyphs', 'APositions']);
  LBuilder := TSkTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocateHorizontalRun(AFont, Length(AGlyphs), AY);
  Move(AGlyphs[0], LBuffer.GlyphsAddress^, SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.PositionsAddress^, SizeOf(Single) * Length(APositions));
  Result := LBuilder.Detach;
end;

class function TSkTextBlob.MakePositioned(const AText: string;
  const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob;
begin
  Result := MakePositionedGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSkTextBlob.MakePositionedGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob;
var
  LBuffer: ISkPositionedRunBuffer;
  LBuilder: ISkTextBlobBuilder;
begin
  if not Assigned(AFont) then
    raise ESkTextBlob.CreateFmt(ParameterCannotBeNil, ['AFont']);
  if Length(AGlyphs) <> Length(APositions) then
    raise ESkTextBlob.CreateFmt(MatchLength, ['AGlyphs', 'APositions']);
  LBuilder := TSkTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocatePositionedRun(AFont, Length(AGlyphs));
  Move(AGlyphs[0], LBuffer.GlyphsAddress^, SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.PositionsAddress^, SizeOf(TPointF) * Length(APositions));
  Result := LBuilder.Detach;
end;

class function TSkTextBlob.MakeRotationScale(const AText: string;
  const APositions: TArray<TSkRotationScaleMatrix>;
  const AFont: ISkFont): ISkTextBlob;
begin
  Result := MakeRotationScaleGlyphs(AFont.GetGlyphs(AText), APositions, AFont);
end;

class function TSkTextBlob.MakeRotationScaleGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TSkRotationScaleMatrix>;
  const AFont: ISkFont): ISkTextBlob;
var
  LBuffer: ISkRotationScaleRunBuffer;
  LBuilder: ISkTextBlobBuilder;
begin
  if not Assigned(AFont) then
    raise ESkTextBlob.CreateFmt(ParameterCannotBeNil, ['AFont']);
  if Length(AGlyphs) <> Length(APositions) then
    raise ESkTextBlob.CreateFmt(MatchLength, ['AGlyphs', 'APositions']);
  LBuilder := TSkTextBlobBuilder.Create;
  LBuffer  := LBuilder.AllocateRotationScaleRun(AFont, Length(AGlyphs));
  Move(AGlyphs[0], LBuffer.GlyphsAddress^, SizeOf(Word) * Length(AGlyphs));
  Move(APositions[0], LBuffer.PositionsAddress^, SizeOf(TSkRotationScaleMatrix) * Length(APositions));
  Result := LBuilder.Detach;
end;


{ TSkTextBlobBuilder }

function TSkTextBlobBuilder.AllocateHorizontalRun(const AFont: ISkFont;
  const ACount: Integer; const AY: Single): ISkHorizontalRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_horizontal_run(GetHandle, AFont.Handle, ACount, AY, nil, LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocateHorizontalRun(const AFont: ISkFont;
  const ACount: Integer; const AY: Single;
  const ABounds: TRectF): ISkHorizontalRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_horizontal_run(GetHandle, AFont.Handle, ACount, AY, @sk_rect_t(ABounds), LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocatePositionedRun(const AFont: ISkFont;
  const ACount: Integer; const ABounds: TRectF): ISkPositionedRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_positioned_run(GetHandle, AFont.Handle, ACount, @sk_rect_t(ABounds), LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocatePositionedRun(const AFont: ISkFont;
  const ACount: Integer): ISkPositionedRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_positioned_run(GetHandle, AFont.Handle, ACount, nil, LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocateRotationScaleRun(const AFont: ISkFont;
  const ACount: Integer): ISkRotationScaleRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_rotation_scale_run(GetHandle, AFont.Handle, ACount, LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocateRotationScaleRun(const AFont: ISkFont;
  const ACount: Integer; const ABounds: TRectF): ISkRotationScaleRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_positioned_run(GetHandle, AFont.Handle, ACount, @sk_rect_t(ABounds), LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocateRun(const AFont: ISkFont;
  const ACount: Integer; const AX, AY: Single;
  const ABounds: TRectF): TSkRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_run(GetHandle, AFont.Handle, ACount, AX, AY, @sk_rect_t(ABounds), LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

function TSkTextBlobBuilder.AllocateRun(const AFont: ISkFont;
  const ACount: Integer; const AX, AY: Single): TSkRunBuffer;
var
  LRunBuffer: sk_runbuffer_t;
begin
  if not Assigned(AFont) then
    raise ESkTextBlobBuilder.CreateFmt(ParameterCannotBeNil, ['AFont']);
  TSkiaApi.sk4d_textblobbuilder_alloc_run(GetHandle, AFont.Handle, ACount, AX, AY, nil, LRunBuffer);
  Result := LRunBuffer.ToRunBuffer(ACount);
end;

constructor TSkTextBlobBuilder.Create;
begin
  CreateNative(TSkiaApi.sk4d_textblobbuilder_create());
end;

function TSkTextBlobBuilder.Detach: ISkTextBlob;
var
  LHandle: sk_textblob_t;
begin
  LHandle := TSkiaApi.sk4d_textblobbuilder_detach(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTextBlob.CreateNative(LHandle);
end;

class procedure TSkTextBlobBuilder.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_textblobbuilder_destroy(AHandle);
end;

{ TSkShaperRunHandler }

class procedure TSkShaperRunHandler.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_shaperrunhandler_destroy(AHandle);
end;

{ TSkShaperRunHandlerBaseClass }

class procedure TSkShaperRunHandlerBaseClass.begin_line_proc(context: Pointer);
begin
  TSkShaperRunHandlerBaseClass(context).BeginLine;
end;

class procedure TSkShaperRunHandlerBaseClass.commit_line_proc(context: Pointer);
begin
  TSkShaperRunHandlerBaseClass(context).CommitLine;
end;

class procedure TSkShaperRunHandlerBaseClass.commit_run_buffer_proc(
  context: Pointer; const info: psk_shaperruninfo_t);
begin
  TSkShaperRunHandlerBaseClass(context).CommitRunBuffer(info.ToShaperRunInfo);
end;

class procedure TSkShaperRunHandlerBaseClass.commit_run_info_proc(
  context: Pointer);
begin
  TSkShaperRunHandlerBaseClass(context).CommitRunInfo;
end;

constructor TSkShaperRunHandlerBaseClass.Create;
begin
  CreateNative(TSkiaApi.sk4d_shaperrunhandlerbaseclass_create(Self));
end;

class constructor TSkShaperRunHandlerBaseClass.Create;
var
  LProcs: sk_shaperrunhandlerbaseclass_procs_t;
begin
  LProcs.begin_line        := begin_line_proc;
  LProcs.commit_line       := commit_line_proc;
  LProcs.commit_run_buffer := commit_run_buffer_proc;
  LProcs.commit_run_info   := commit_run_info_proc;
  LProcs.run_buffer        := run_buffer_proc;
  LProcs.run_info          := run_info_proc;
  TSkiaApi.sk4d_shaperrunhandlerbaseclass_set_procs(@LProcs);
end;

class procedure TSkShaperRunHandlerBaseClass.run_buffer_proc(context: Pointer;
  const info: psk_shaperruninfo_t; out result: sk_shaperrunbuffer_t);
begin
  result := sk_shaperrunbuffer_t(TSkShaperRunHandlerBaseClass(context).RunBuffer(info.ToShaperRunInfo));
end;

class procedure TSkShaperRunHandlerBaseClass.run_info_proc(context: Pointer;
  const info: psk_shaperruninfo_t);
begin
  TSkShaperRunHandlerBaseClass(context).RunInfo(info.ToShaperRunInfo);
end;

{ TSkTextBlobBuilderRunHandler }

constructor TSkTextBlobBuilderRunHandler.Create(const AText: string;
  const AOffset: TPointF);
begin
  CreateNative(TSkiaApi.sk4d_textblobbuilderrunhandler_create(MarshaledAString(UTF8String(AText)), @sk_point_t(AOffset)));
end;

function TSkTextBlobBuilderRunHandler.Detach: ISkTextBlob;
var
  LHandle: sk_textblob_t;
begin
  LHandle := TSkiaApi.sk4d_textblobbuilderrunhandler_detach(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTextBlob.CreateNative(LHandle);
end;

function TSkTextBlobBuilderRunHandler.GetEndPoint: TPointF;
begin
  TSkiaApi.sk4d_textblobbuilderrunhandler_get_end_point(GetHandle, sk_point_t(Result));
end;

{ TSkShaper }

constructor TSkShaper.Create;
begin
  CreateNative(TSkiaApi.sk4d_shaper_create());
end;

class procedure TSkShaper.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_shaper_destroy(AHandle);
end;

procedure TSkShaper.Shape(const AText: string; const AFont: ISkFont;
  const ALeftToRight: Boolean; const AWidth: Single;
  const AHandler: ISkShaperRunHandler);
var
  LText: UTF8String;
begin
  if not Assigned(AFont) then
    raise ESkShaper.CreateFmt(ParameterCannotBeNil, ['AFont']);
  LText := UTF8String(AText);
  TSkiaApi.sk4d_shaper_shape(GetHandle, @LText[Low(LText)], Length(LText), AFont.Handle, ALeftToRight, AWidth, SafeHandle(AHandler));
end;

{ TSkPaint }

constructor TSkPaint.Create(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkPaint.CreateFmt(ParameterCannotBeNil, ['APaint']);
  CreateNative(TSkiaApi.sk4d_paint_create2(APaint.Handle));
end;

constructor TSkPaint.Create;
begin
  CreateNative(TSkiaApi.sk4d_paint_create());
end;

class procedure TSkPaint.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_paint_destroy(AHandle);
end;

function TSkPaint.GetAlpha: Byte;
begin
  Result := TSkiaApi.sk4d_paint_get_alpha(GetHandle);
end;

function TSkPaint.GetAlphaF: Single;
begin
  Result := TSkiaApi.sk4d_paint_get_alphaf(GetHandle);
end;

function TSkPaint.GetAntiAlias: Boolean;
begin
  Result := TSkiaApi.sk4d_paint_get_anti_alias(GetHandle);
end;

function TSkPaint.GetBlendMode: TSkBlendMode;
begin
  Result := TSkBlendMode(TSkiaApi.sk4d_paint_get_blend_mode(GetHandle));
end;

function TSkPaint.GetColor: TAlphaColor;
begin
  Result := TSkiaApi.sk4d_paint_get_color(GetHandle);
end;

function TSkPaint.GetColorF: TAlphaColorF;
begin
  TSkiaApi.sk4d_paint_get_colorf(GetHandle, sk_color4f_t(Result));
end;

function TSkPaint.GetColorFilter: ISkColorFilter;
var
  LHandle: sk_colorfilter_t;
begin
  LHandle := TSkiaApi.sk4d_paint_get_color_filter(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkColorFilter.CreateNative(LHandle);
end;

function TSkPaint.GetDither: Boolean;
begin
  Result := TSkiaApi.sk4d_paint_get_dither(GetHandle);
end;

function TSkPaint.GetFillPath(const APath: ISkPath; const ACullRect: TRectF;
  const AResScale: Single): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkPaint.CreateFmt(ParameterCannotBeNil, ['APath']);
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_paint_get_fill_path(GetHandle, APath.Handle, @sk_rect_t(ACullRect), AResScale, Result.Handle) then
    Result := nil;
end;

function TSkPaint.GetFillPath(const APath: ISkPath): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkPaint.CreateFmt(ParameterCannotBeNil, ['APath']);
  Result := TSkPath.Create;
  if not TSkiaApi.sk4d_paint_get_fill_path(GetHandle, APath.Handle, nil, 1, Result.Handle) then
    Result := nil;
end;

function TSkPaint.GetFilterQuality: TSkFilterQuality;
begin
  Result := TSkFilterQuality(TSkiaApi.sk4d_paint_get_filter_quality(GetHandle));
end;

function TSkPaint.GetImageFilter: ISkImageFilter;
var
  LHandle: sk_imagefilter_t;
begin
  LHandle := TSkiaApi.sk4d_paint_get_image_filter(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkImageFilter.CreateNative(LHandle);
end;

function TSkPaint.GetMaskFilter: ISkMaskFilter;
var
  LHandle: sk_maskfilter_t;
begin
  LHandle := TSkiaApi.sk4d_paint_get_mask_filter(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkMaskFilter.CreateNative(LHandle);
end;

function TSkPaint.GetPathEffect: ISkPathEffect;
var
  LHandle: sk_patheffect_t;
begin
  LHandle := TSkiaApi.sk4d_paint_get_path_effect(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkPathEffect.CreateNative(LHandle);
end;

function TSkPaint.GetShader: ISkShader;
var
  LHandle: sk_shader_t;
begin
  LHandle := TSkiaApi.sk4d_paint_get_shader(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkShader.CreateNative(LHandle)
end;

function TSkPaint.GetStrokeCap: TSkStrokeCap;
begin
  Result := TSkStrokeCap(TSkiaApi.sk4d_paint_get_stroke_cap(GetHandle));
end;

function TSkPaint.GetStrokeJoin: TSkStrokeJoin;
begin
  Result := TSkStrokeJoin(TSkiaApi.sk4d_paint_get_stroke_join(GetHandle));
end;

function TSkPaint.GetStrokeMiter: Single;
begin
  Result := TSkiaApi.sk4d_paint_get_stroke_miter(GetHandle);
end;

function TSkPaint.GetStrokeWidth: Single;
begin
  Result := TSkiaApi.sk4d_paint_get_stroke_width(GetHandle);
end;

function TSkPaint.GetStyle: TSkPaintStyle;
begin
  Result := TSkPaintStyle(TSkiaApi.sk4d_paint_get_style(GetHandle))
end;

procedure TSkPaint.SetAlpha(const AValue: Byte);
begin
  TSkiaApi.sk4d_paint_set_alpha(GetHandle, AValue);
end;

procedure TSkPaint.SetAlphaF(const AValue: Single);
begin
  TSkiaApi.sk4d_paint_set_alphaf(GetHandle, AValue);
end;

procedure TSkPaint.SetAntiAlias(const AValue: Boolean);
begin
  TSkiaApi.sk4d_paint_set_antialias(GetHandle, AValue);
end;

procedure TSkPaint.SetARGB(const A, R, G, B: Byte);
begin
  TSkiaApi.sk4d_paint_set_argb(GetHandle, A, R, G, B);
end;

procedure TSkPaint.SetBlendMode(const AMode: TSkBlendMode);
begin
  TSkiaApi.sk4d_paint_set_blend_mode(GetHandle, sk_blendmode_t(AMode));
end;

procedure TSkPaint.SetColor(const AValue: TAlphaColor);
begin
  TSkiaApi.sk4d_paint_set_color(GetHandle, AValue);
end;

procedure TSkPaint.SetColorF(const AValue: TAlphaColorF;
  const AColorSpace: ISkColorSpace);
begin
  TSkiaApi.sk4d_paint_set_colorf(GetHandle, @sk_color4f_t(AValue), SafeHandle(AColorSpace));
end;

procedure TSkPaint.SetColorF(const AValue: TAlphaColorF);
begin
  SetColorF(AValue, nil);
end;

procedure TSkPaint.SetColorFilter(const AValue: ISkColorFilter);
begin
  TSkiaApi.sk4d_paint_set_color_filter(GetHandle, SafeHandle(AValue));
end;

procedure TSkPaint.SetDither(const AValue: Boolean);
begin
  TSkiaApi.sk4d_paint_set_dither(GetHandle, AValue);
end;

procedure TSkPaint.SetFilterQuality(const AValue: TSkFilterQuality);
begin
  TSkiaApi.sk4d_paint_set_filter_quality(GetHandle, sk_filterquality_t(AValue));
end;

procedure TSkPaint.SetImageFilter(const AValue: ISkImageFilter);
begin
  TSkiaApi.sk4d_paint_set_image_filter(GetHandle, SafeHandle(AValue));
end;

procedure TSkPaint.SetMaskFilter(const AValue: ISkMaskFilter);
begin
  TSkiaApi.sk4d_paint_set_mask_filter(GetHandle, SafeHandle(AValue));
end;

procedure TSkPaint.SetPathEffect(const AValue: ISkPathEffect);
begin
  TSkiaApi.sk4d_paint_set_path_effect(GetHandle, SafeHandle(AValue));
end;

procedure TSkPaint.SetShader(const AValue: ISkShader);
begin
  TSkiaApi.sk4d_paint_set_shader(GetHandle, SafeHandle(AValue));
end;

procedure TSkPaint.SetStrokeCap(const AValue: TSkStrokeCap);
begin
  TSkiaApi.sk4d_paint_set_stroke_cap(GetHandle, sk_strokecap_t(AValue));
end;

procedure TSkPaint.SetStrokeJoin(const AValue: TSkStrokeJoin);
begin
  TSkiaApi.sk4d_paint_set_stroke_join(GetHandle, sk_strokejoin_t(AValue));
end;

procedure TSkPaint.SetStrokeMiter(const AValue: Single);
begin
  TSkiaApi.sk4d_paint_set_stroke_miter(GetHandle, AValue);
end;

procedure TSkPaint.SetStrokeWidth(const AValue: Single);
begin
  TSkiaApi.sk4d_paint_set_stroke_width(GetHandle, AValue);
end;

procedure TSkPaint.SetStyle(const AValue: TSkPaintStyle);
begin
  TSkiaApi.sk4d_paint_set_style(GetHandle, sk_paintstyle_t(AValue));
end;

{ TSkCanvas }

procedure TSkCanvas.Clear(const AColor: TAlphaColorF);
begin
  TSkiaApi.sk4d_canvas_clear2(GetHandle, @sk_color4f_t(AColor));
end;

procedure TSkCanvas.Clear(const AColor: TAlphaColor);
begin
  TSkiaApi.sk4d_canvas_clear(GetHandle, AColor);
end;

procedure TSkCanvas.ClipPath(const APath: ISkPath; const AOp: TSkClipOp;
  const DoAntiAlias: Boolean);
begin
  if not Assigned(APath) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APath']);
  TSkiaApi.sk4d_canvas_clip_path(GetHandle, APath.Handle, sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSkCanvas.ClipRect(const ARect: TRectF; const AOp: TSkClipOp;
  const DoAntiAlias: Boolean);
begin
  TSkiaApi.sk4d_canvas_clip_rect(GetHandle, @sk_rect_t(ARect), sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSkCanvas.ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp);
begin
  if not Assigned(ARegion) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  TSkiaApi.sk4d_canvas_clip_region(GetHandle, ARegion.Handle, sk_clipop_t(AOp));
end;

procedure TSkCanvas.ClipRoundRect(const ARoundRect: ISkRoundRect;
  const AOp: TSkClipOp; const DoAntiAlias: Boolean);
begin
  if not Assigned(ARoundRect) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['ARoundRect']);
  TSkiaApi.sk4d_canvas_clip_rrect(GetHandle, ARoundRect.Handle, sk_clipop_t(AOp), DoAntiAlias);
end;

procedure TSkCanvas.ClipShader(const AShader: ISkShader; const AOp: TSkClipOp);
begin
  if not Assigned(AShader) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AShader']);
  TSkiaApi.sk4d_canvas_clip_shader(GetHandle, AShader.Handle, sk_clipop_t(AOp));
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix3D);
begin
  TSkiaApi.sk4d_canvas_concat(GetHandle, @sk_matrix44_t(AMatrix));
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix);
begin
  TSkiaApi.sk4d_canvas_concat2(GetHandle, @sk_matrix_t(AMatrix));
end;

procedure TSkCanvas.Discard;
begin
  TSkiaApi.sk4d_canvas_discard(GetHandle);
end;

class procedure TSkCanvas.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_canvas_destroy(AHandle);
end;

procedure TSkCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string;
  const AValue: TBytes);
var
  LValue: ISkData;
begin
  LValue := TSkData.Create(AValue);
  TSkiaApi.sk4d_canvas_draw_annotation(GetHandle, @sk_rect_t(ARect), MarshaledAString(UTF8String(AKey)), LValue.Handle);
end;

procedure TSkCanvas.DrawArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_arc(GetHandle, @sk_rect_t(AOval), AStartAngle, ASweepAngle, AUseCenter, APaint.Handle);
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ACullRect: TRectF; const AColors: TArray<TAlphaColor>;
  const APaint: ISkPaint);
begin
  if Length(ATansforms) = 0 then
    raise ESkCanvas.CreateFmt(CannotBeEmpty, ['ATansforms']);
  if Length(ATansforms) <> Length(ASprites) then
    raise ESkCanvas.CreateFmt(MatchLength, ['ATansforms', 'ASprites']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(ASprites)) then
    raise ESkCanvas.CreateFmt(MatchLength, ['ASprites', 'AColors']);
  TSkiaApi.sk4d_canvas_draw_atlas(GetHandle, SafeHandle(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), @sk_rect_t(ACullRect), SafeHandle(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const AColors: TArray<TAlphaColor>; const APaint: ISkPaint);
begin
  if Length(ATansforms) = 0 then
    raise ESkCanvas.CreateFmt(CannotBeEmpty, ['ATansforms']);
  if Length(ATansforms) <> Length(ASprites) then
    raise ESkCanvas.CreateFmt(MatchLength, ['ATansforms', 'ASprites']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(ASprites)) then
    raise ESkCanvas.CreateFmt(MatchLength, ['ASprites', 'AColors']);
  TSkiaApi.sk4d_canvas_draw_atlas(GetHandle, SafeHandle(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), nil, SafeHandle(APaint));
end;

procedure TSkCanvas.DrawCircle(const ACenterX, ACenterY, ARadius: Single;
  const APaint: ISkPaint);
begin
  DrawCircle(TPointF.Create(ACenterX, ACenterY), ARadius, APaint);
end;

procedure TSkCanvas.DrawCircle(const ACenter: TPointF; ARadius: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_circle(GetHandle, @sk_point_t(ACenter), ARadius, APaint.Handle);
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColor;
  const ABlendMode: TSkBlendMode);
begin
  TSkiaApi.sk4d_canvas_draw_color(GetHandle, AColor, sk_blendmode_t(ABlendMode));
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColorF;
  const ABlendMode: TSkBlendMode);
begin
  TSkiaApi.sk4d_canvas_draw_color2(GetHandle, @sk_color4f_t(AColor), sk_blendmode_t(ABlendMode));
end;

procedure TSkCanvas.DrawImage(const AImage: ISkImage; const AX, AY: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(AImage) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AImage']);
  TSkiaApi.sk4d_canvas_draw_image(GetHandle, AImage.Handle, AX, AY, SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageLattice(const AImage: ISkImage;
  const ALattice: TSkLattice; const ADest: TRectF; const APaint: ISkPaint);
var
  LLattice: sk_lattice_t;
begin
  if not Assigned(AImage) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AImage']);
  LLattice.FromLattice(ALattice);
  TSkiaApi.sk4d_canvas_draw_image_lattice(GetHandle, AImage.Handle, @LLattice, @sk_rect_t(ADest), SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageNine(const AImage: ISkImage; const ACenter: TRect;
  const ADest: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(AImage) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AImage']);
  TSkiaApi.sk4d_canvas_draw_image_nine(GetHandle, AImage.Handle, @sk_irect_t(ACenter), @sk_rect_t(ADest), SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ADest: TRectF;
  const APaint: ISkPaint; const AConstraint: TSkSrcRectConstraint);
begin
  DrawImageRect(AImage, TRectF.Create(0, 0, AImage.Width, AImage.Height), ADest, APaint, AConstraint);
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ASrc,
  ADest: TRectF; const APaint: ISkPaint;
  const AConstraint: TSkSrcRectConstraint);
begin
  if not Assigned(AImage) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AImage']);
  TSkiaApi.sk4d_canvas_draw_image_rect(GetHandle, AImage.Handle, @sk_rect_t(ASrc), @sk_rect_t(ADest), SafeHandle(APaint), sk_srcrectconstraint_t(AConstraint));
end;

procedure TSkCanvas.DrawLine(const AX1, AY1, AX2, AY2: Single;
  const APaint: ISkPaint);
begin
  DrawLine(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), APaint);
end;

procedure TSkCanvas.DrawLine(const APoint1, APoint2: TPointF;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_line(GetHandle, @sk_point_t(APoint1), @sk_point_t(APoint2), APaint.Handle);
end;

procedure TSkCanvas.DrawOval(const AOval: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_oval(GetHandle, @sk_rect_t(AOval), APaint.Handle);
end;

procedure TSkCanvas.DrawPaint(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_paint(GetHandle, APaint.Handle);
end;

procedure TSkCanvas.DrawPatch(const ACubics: TSkPatchCubics;
  const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords;
  const APaint: ISkPaint; const ABlendMode: TSkBlendMode);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_patch(GetHandle, @sk_point_t(ACubics[0]), @AColors[0], @sk_point_t(ATexCoords[0]), sk_blendmode_t(ABlendMode), APaint.Handle);
end;

procedure TSkCanvas.DrawPath(const APath: ISkPath; const APaint: ISkPaint);
begin
  if not Assigned(APath) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APath']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_path(GetHandle, APath.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture;
  const AMatrix: TMatrix; const APaint: ISkPaint);
begin
  if not Assigned(APicture) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APicture']);
  TSkiaApi.sk4d_canvas_draw_picture2(GetHandle, APicture.Handle, @sk_matrix_t(AMatrix), SafeHandle(APaint));
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture);
begin
  if not Assigned(APicture) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APicture']);
  TSkiaApi.sk4d_canvas_draw_picture(GetHandle, APicture.Handle);
end;

procedure TSkCanvas.DrawPoint(const AX, AY: Single; const APaint: ISkPaint);
begin
  DrawPoint(TPointF.Create(AX, AY), APaint);
end;

procedure TSkCanvas.DrawPoint(const APoint: TPointF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_point(GetHandle, @sk_point_t(APoint), APaint.Handle);
end;

procedure TSkCanvas.DrawPoints(const AMode: TSkDrawPointsMode;
  const APoints: TArray<TPointF>; const APaint: ISkPaint);
begin
  if Length(APoints) = 0 then
    raise ESkCanvas.CreateFmt(CannotBeEmpty, ['APoints']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_points(GetHandle, sk_drawpointsmode_t(AMode), Length(APoints), @sk_point_t(APoints[0]), APaint.Handle);
end;

procedure TSkCanvas.DrawRect(const ARect: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_rect(GetHandle, @sk_rect_t(ARect), APaint.Handle);
end;

procedure TSkCanvas.DrawRegion(const ARegion: ISkRegion;
  const APaint: ISkPaint);
begin
  if not Assigned(ARegion) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['ARegion']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_region(GetHandle, ARegion.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRect(const ARoundRect: ISkRoundRect;
  const APaint: ISkPaint);
begin
  if not Assigned(ARoundRect) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['ARoundRect']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_rrect(GetHandle, ARoundRect.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_rrect2(GetHandle, @sk_rect_t(ARect), ARadiusX, ARadiusY, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRectDifference(const AOuter, AInner: ISkRoundRect;
  const APaint: ISkPaint);
begin
  if not Assigned(AOuter) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AInner']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_rrect_difference(GetHandle, AOuter.Handle, AInner.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawSimpleText(const AText: string; const AX, AY: Single;
  const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_simple_text(GetHandle, @AText[Low(AText)], Length(AText) * 2, UTF16_SK_TEXTENCODING, AX, AY, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX,
  AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_simple_text(GetHandle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), GLYPH_ID_SK_TEXTENCODING, AX, AY, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawTextBlob(const ATextBlob: ISkTextBlob; const AX,
  AY: Single; const APaint: ISkPaint);
begin
  if not Assigned(ATextBlob) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['ATextBlob']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_text_blob(GetHandle, ATextBlob.Handle, AX, AY, APaint.Handle);
end;

procedure TSkCanvas.DrawVertices(const AVertices: ISkVertices;
  const APaint: ISkPaint; const ABlendMode: TSkBlendMode);
begin
  if not Assigned(AVertices) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['AVertices']);
  if not Assigned(APaint) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_canvas_draw_vertices(GetHandle, AVertices.Handle, sk_blendmode_t(ABlendMode), APaint.Handle);
end;

function TSkCanvas.FindMarkedCTM(const AName: string;
  out AMatrix: TMatrix3D): Boolean;
begin
  Result := TSkiaApi.sk4d_canvas_find_marked_ctm(GetHandle, MarshaledAString(UTF8String(AName)), sk_matrix44_t(AMatrix));
end;

function TSkCanvas.GetDeviceClipBounds: TRect;
begin
  TSkiaApi.sk4d_canvas_get_device_clip_bounds(GetHandle, sk_irect_t(Result));
end;

function TSkCanvas.GetLocalClipBounds: TRectF;
begin
  TSkiaApi.sk4d_canvas_get_local_clip_bounds(GetHandle, sk_rect_t(Result));
end;

function TSkCanvas.GetLocalToDevice: TMatrix3D;
begin
  TSkiaApi.sk4d_canvas_get_local_to_device(GetHandle, sk_matrix44_t(Result));
end;

function TSkCanvas.GetLocalToDeviceAs3x3: TMatrix;
begin
  TSkiaApi.sk4d_canvas_get_local_to_device2(GetHandle, sk_matrix_t(Result));
end;

function TSkCanvas.GetSaveCount: Integer;
begin
  Result := TSkiaApi.sk4d_canvas_get_save_count(GetHandle);
end;

function TSkCanvas.IsClipEmpty: Boolean;
begin
  Result := TSkiaApi.sk4d_canvas_is_clip_empty(GetHandle);
end;

function TSkCanvas.IsClipRect: Boolean;
begin
  Result := TSkiaApi.sk4d_canvas_is_clip_rect(GetHandle);
end;

procedure TSkCanvas.MarkCTM(const AName: string);
begin
  TSkiaApi.sk4d_canvas_mark_ctm(GetHandle, MarshaledAString(UTF8String(AName)));
end;

function TSkCanvas.QuickReject(const ARect: TRectF): Boolean;
begin
  Result := TSkiaApi.sk4d_canvas_quick_reject(GetHandle, @sk_rect_t(ARect));
end;

function TSkCanvas.QuickReject(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkCanvas.CreateFmt(ParameterCannotBeNil, ['APath']);
  Result := TSkiaApi.sk4d_canvas_quick_reject2(GetHandle, APath.Handle);
end;

procedure TSkCanvas.ResetMatrix;
begin
  TSkiaApi.sk4d_canvas_reset_matrix(GetHandle);
end;

procedure TSkCanvas.Restore;
begin
  TSkiaApi.sk4d_canvas_restore(GetHandle);
end;

procedure TSkCanvas.RestoreToCount(const ASaveCount: Integer);
begin
  TSkiaApi.sk4d_canvas_restore_to_count(GetHandle, ASaveCount);
end;

procedure TSkCanvas.Rotate(const ADegrees: Single);
begin
  TSkiaApi.sk4d_canvas_rotate(GetHandle, ADegrees);
end;

procedure TSkCanvas.Rotate(const ADegrees, APX, APY: Single);
begin
  TSkiaApi.sk4d_canvas_rotate2(GetHandle, ADegrees, APX, APY);
end;

procedure TSkCanvas.Save;
begin
  TSkiaApi.sk4d_canvas_save(GetHandle);
end;

procedure TSkCanvas.SaveLayer(const APaint: ISkPaint);
begin
  TSkiaApi.sk4d_canvas_save_layer(GetHandle, nil, SafeHandle(APaint));
end;

procedure TSkCanvas.SaveLayer(const ABounds: TRectF; const APaint: ISkPaint);
begin
  TSkiaApi.sk4d_canvas_save_layer(GetHandle, @sk_rect_t(ABounds), SafeHandle(APaint));
end;

procedure TSkCanvas.SaveLayerAlpha(const AAlpha: Byte);
begin
  TSkiaApi.sk4d_canvas_save_layer_alpha(GetHandle, nil, AAlpha);
end;

procedure TSkCanvas.SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte);
begin
  TSkiaApi.sk4d_canvas_save_layer_alpha(GetHandle, @sk_rect_t(ABounds), AAlpha);
end;

procedure TSkCanvas.Scale(const SX, SY: Single);
begin
  TSkiaApi.sk4d_canvas_scale(GetHandle, SX, SY);
end;

procedure TSkCanvas.Skew(const AKX, AKY: Single);
begin
  TSkiaApi.sk4d_canvas_skew(GetHandle, AKX, AKY);
end;

procedure TSkCanvas.Translate(const DX, DY: Single);
begin
  TSkiaApi.sk4d_canvas_translate(GetHandle, DX, DY);
end;

{ TSkDocument }

function TSkDocument.BeginPage(const AWidth, AHeight: Single): ISkCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := TSkiaApi.sk4d_document_begin_page(GetHandle, AWidth, AHeight, nil);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkCanvas.CreateNative(LHandle, False);
end;

function TSkDocument.BeginPage(const AWidth, AHeight: Single;
  const AContent: TRectF): ISkCanvas;
var
  LHandle: sk_canvas_t;
begin
  LHandle := TSkiaApi.sk4d_document_begin_page(GetHandle, AWidth, AHeight, @sk_rect_t(AContent));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkCanvas.CreateNative(LHandle, False);
end;

procedure TSkDocument.Close;
begin
  TSkiaApi.sk4d_document_close(GetHandle);
end;

procedure TSkDocument.EndPage;
begin
  TSkiaApi.sk4d_document_end_page(GetHandle);
end;

procedure TSkDocument.Terminate;
begin
  TSkiaApi.sk4d_document_terminate(GetHandle);
end;

{ TSkPDFDocument }

constructor TSkPDFDocument.Create(const AStream: TStream;
  const AMetadata: TSkPDFMetadata);
var
  LMetadata: sk_pdfmetadata_t;
begin
  FStream := TSkManagedWStream.Create(AStream);
  LMetadata.FromPDFMetadata(AMetadata);
  CreateNative(TSkiaApi.sk4d_document_make_pdf2(FStream.Handle, @LMetadata));
end;

constructor TSkPDFDocument.Create(const AStream: TStream);
begin
  FStream := TSkManagedWStream.Create(AStream);
  CreateNative(TSkiaApi.sk4d_document_make_pdf(FStream.Handle));
end;

{ TSkGraphics }

class procedure TSkGraphics.AllowJIT;
begin
  TSkiaApi.sk4d_graphics_allow_jit();
end;

class procedure TSkGraphics.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  TSkiaApi.sk4d_graphics_dump_memory_statistics(TSkNativeObject.SafeHandle(ATraceMemoryDump));
end;

class function TSkGraphics.GetFontCacheCountLimit: Integer;
begin
  Result := TSkiaApi.sk4d_graphics_get_font_cache_count_limit();
end;

class function TSkGraphics.GetFontCacheCountUsed: Integer;
begin
  Result := TSkiaApi.sk4d_graphics_get_font_cache_count_used();
end;

class function TSkGraphics.GetFontCacheLimit: NativeUInt;
begin
  Result := TSkiaApi.sk4d_graphics_get_font_cache_limit();
end;

class function TSkGraphics.GetFontCacheUsed: NativeUInt;
begin
  Result := TSkiaApi.sk4d_graphics_get_font_cache_used();
end;

class function TSkGraphics.GetResourceCacheSingleAllocationByteLimit: NativeUInt;
begin
  Result := TSkiaApi.sk4d_graphics_get_resource_cache_single_allocation_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalByteLimit: NativeUInt;
begin
  Result := TSkiaApi.sk4d_graphics_get_resource_cache_total_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalBytesUsed: NativeUInt;
begin
  Result := TSkiaApi.sk4d_graphics_get_resource_cache_total_bytes_used();
end;

class procedure TSkGraphics.Init;
begin
  TSkiaApi.sk4d_graphics_init();
end;

class procedure TSkGraphics.PurgeAllCaches;
begin
  TSkiaApi.sk4d_graphics_purge_all_caches();
end;

class procedure TSkGraphics.PurgeFontCache;
begin
  TSkiaApi.sk4d_graphics_purge_font_cache();
end;

class procedure TSkGraphics.PurgeResourceCache;
begin
  TSkiaApi.sk4d_graphics_purge_resource_cache();
end;

class procedure TSkGraphics.SetFontCacheCountLimit(const AValue: Integer);
begin
  TSkiaApi.sk4d_graphics_set_font_cache_count_limit(AValue);
end;

class procedure TSkGraphics.SetFontCacheLimit(const AValue: NativeUInt);
begin
  TSkiaApi.sk4d_graphics_set_font_cache_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheSingleAllocationByteLimit(
  const AValue: NativeUInt);
begin
  TSkiaApi.sk4d_graphics_set_resource_cache_single_allocation_byte_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheTotalByteLimit(
  const AValue: NativeUInt);
begin
  TSkiaApi.sk4d_graphics_set_resource_cache_total_byte_limit(AValue);
end;

{ TSkSVGDOM }

function TSkSVGDOM.GetContainerSize: TSizeF;
begin
  TSkiaApi.sk4d_svgdom_get_container_size(GetHandle, sk_size_t(Result));
end;

class function TSkSVGDOM.Make(const AStream: TStream): ISkSVGDOM;
var
  LHandle: sk_svgdom_t;
  LStream: ISkStream;
begin
  LStream := TSkManagedStream.Create(AStream);
  LHandle := TSkiaApi.sk4d_svgdom_make(LStream.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkSVGDOM.CreateNative(LHandle);
end;

procedure TSkSVGDOM.Render(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkSVGDOM.CreateFmt(ParameterCannotBeNil, ['ACanvas']);
  TSkiaApi.sk4d_svgdom_render(GetHandle, ACanvas.Handle);
end;

procedure TSkSVGDOM.SetContainerSize(const AValue: TSizeF);
begin
  TSkiaApi.sk4d_svgdom_set_container_size(GetHandle, @sk_size_t(AValue));
end;

{ TSkottieAnimation }

class function TSkottieAnimation.DoIsUnique(const AHandle: THandle): Boolean;
begin
  Result := TSkiaApi.sk4d_skottieanimation_is_unique(AHandle);
end;

class procedure TSkottieAnimation.DoRef(const AHandle: THandle);
begin
  TSkiaApi.sk4d_skottieanimation_ref(AHandle);
end;

class procedure TSkottieAnimation.DoUnref(const AHandle: THandle);
begin
  TSkiaApi.sk4d_skottieanimation_unref(AHandle);
end;

function TSkottieAnimation.GetDuration: Double;
begin
  Result := TSkiaApi.sk4d_skottieanimation_get_duration(GetHandle);
end;

function TSkottieAnimation.GetFPS: Double;
begin
  Result := TSkiaApi.sk4d_skottieanimation_get_fps(GetHandle);
end;

function TSkottieAnimation.GetInPoint: Double;
begin
  Result := TSkiaApi.sk4d_skottieanimation_get_in_point(GetHandle);
end;

function TSkottieAnimation.GetOutPoint: Double;
begin
  Result := TSkiaApi.sk4d_skottieanimation_get_out_point(GetHandle);
end;

function TSkottieAnimation.GetSize: TSizeF;
begin
  TSkiaApi.sk4d_skottieanimation_get_size(GetHandle, sk_size_t(Result));
end;

function TSkottieAnimation.GetVersion: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.CreateNative(TSkiaApi.sk4d_skottieanimation_get_version(GetHandle), False);
  Result  := LResult.Text;
end;

class function TSkottieAnimation.Make(const AData: string): ISkottieAnimation;
var
  LData: UTF8String;
  LHandle: sk_skottieanimation_t;
begin
  LData   := UTF8String(AData);
  LHandle := TSkiaApi.sk4d_skottieanimation_make(@LData[Low(LData)], Length(LData));
  if LHandle = 0 then
    Exit(nil);
  Result := TSkottieAnimation.CreateNative(LHandle);
end;

class function TSkottieAnimation.MakeFromFile(
  const AFileName: string): ISkottieAnimation;
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := MakeFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

class function TSkottieAnimation.MakeFromStream(
  const AStream: TStream): ISkottieAnimation;
var
  LHandle: sk_skottieanimation_t;
  LStream: ISkStream;
begin
  LStream := TSkManagedStream.Create(AStream);
  LHandle := TSkiaApi.sk4d_skottieanimation_make_from_stream(LStream.Handle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkottieAnimation.CreateNative(LHandle);
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkottieAnimation.CreateFmt(ParameterCannotBeNil, ['ACanvas']);
  TSkiaApi.sk4d_skottieanimation_render(GetHandle, ACanvas.Handle, nil, Byte(ARenderFlags));
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF; const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkottieAnimation.CreateFmt(ParameterCannotBeNil, ['ACanvas']);
  TSkiaApi.sk4d_skottieanimation_render(GetHandle, ACanvas.Handle, @sk_rect_t(ADest), Byte(ARenderFlags));
end;

procedure TSkottieAnimation.SeekFrame(const ATick: Double);
begin
  TSkiaApi.sk4d_skottieanimation_seek_frame(GetHandle, ATick);
end;

procedure TSkottieAnimation.SeekFrameTime(const ATick: Double);
begin
  TSkiaApi.sk4d_skottieanimation_seek_frame_time(GetHandle, ATick);
end;

{ TSkParagraphCache }

procedure TSkParagraphCache.Reset;
begin
  TSkiaApi.sk4d_paragraphcache_reset(GetHandle);
end;

procedure TSkParagraphCache.TurnOn(const AValue: Boolean);
begin
  TSkiaApi.sk4d_paragraphcache_turn_on(GetHandle, AValue);
end;

{ TSkFontCollection }

constructor TSkFontCollection.Create;
begin
  CreateNative(TSkiaApi.sk4d_fontcollection_create());
end;

function TSkFontCollection.GetFontFallBack: Boolean;
begin
  Result := TSkiaApi.sk4d_fontcollection_get_font_fall_back(GetHandle);
end;

function TSkFontCollection.GetParagraphCache: ISkParagraphCache;
begin
  Result := TSkParagraphCache.CreateNative(TSkiaApi.sk4d_fontcollection_get_paragraph_cache(GetHandle), False);
end;

procedure TSkFontCollection.SetFontFallBack(const AValue: Boolean);
begin
  TSkiaApi.sk4d_fontcollection_set_font_fall_back(GetHandle, AValue);
end;

{ TSkTextStyle }

procedure TSkTextStyle.AddFontFeature(const AFeature: string;
  const AValue: Integer);
var
  LFeature: ISkString;
begin
  LFeature := TSkString.Create(AFeature);
  TSkiaApi.sk4d_textstyle_add_font_feature(GetHandle, LFeature.Handle, AValue);
end;

procedure TSkTextStyle.AddShadow(const AShadow: TSkTextShadow);
begin
  TSkiaApi.sk4d_textstyle_add_shadow(GetHandle, @sk_textshadow_t(AShadow));
end;

procedure TSkTextStyle.ClearBackgroundColor;
begin
  TSkiaApi.sk4d_textstyle_clear_background_color(GetHandle);
end;

procedure TSkTextStyle.ClearForegroundColor;
begin
  TSkiaApi.sk4d_textstyle_clear_foreground_color(GetHandle);
end;

constructor TSkTextStyle.Create;
begin
  CreateNative(TSkiaApi.sk4d_textstyle_create());
end;

class procedure TSkTextStyle.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_textstyle_destroy(AHandle);
end;

function TSkTextStyle.GetBackground: ISkPaint;
begin
  Result := TSkPaint.CreateNative(TSkiaApi.sk4d_textstyle_get_background(GetHandle));
end;

function TSkTextStyle.GetColor: TAlphaColor;
begin
  Result := TSkiaApi.sk4d_textstyle_get_color(GetHandle);
end;

function TSkTextStyle.GetDecorationColor: TAlphaColor;
begin
  Result := TSkiaApi.sk4d_textstyle_get_decoration_color(GetHandle);
end;

function TSkTextStyle.GetDecorationMode: TSkTextDecorationMode;
begin
  Result := TSkTextDecorationMode(TSkiaApi.sk4d_textstyle_get_decoration_mode(GetHandle));
end;

function TSkTextStyle.GetDecorations: TSkTextDecorations;
begin
  Result := TSkTextDecorations(Byte(TSkiaApi.sk4d_textstyle_get_decorations(GetHandle)));
end;

function TSkTextStyle.GetDecorationStyle: TSkTextDecorationStyle;
begin
  Result := TSkTextDecorationStyle(TSkiaApi.sk4d_textstyle_get_decoration_style(GetHandle));
end;

function TSkTextStyle.GetDecorationThicknessMultiplier: Single;
begin
  Result := TSkiaApi.sk4d_textstyle_get_decoration_thickness_multiplier(GetHandle);
end;

function TSkTextStyle.GetFontFamilies: TArray<string>;
var
  I: Integer;
  LCount: NativeUInt;
  LFontFamilies: TArray<sk_string_t>;
  LStrings: TArray<ISkString>;
begin
  LCount := TSkiaApi.sk4d_textstyle_get_font_families(GetHandle, nil);
  SetLength(LStrings, LCount);
  SetLength(LFontFamilies, LCount);
  SetLength(Result, LCount);
  for I := 0 to LCount - 1 do
  begin
    LStrings[I]      := TSkString.Create;
    LFontFamilies[I] := LStrings[I].Handle;
  end;
  TSkiaApi.sk4d_textstyle_get_font_families(GetHandle, @LFontFamilies[0]);
  for I := 0 to LCount - 1 do
    Result[I] := LStrings[I].Text;
end;

function TSkTextStyle.GetFontMetrics: TSkFontMetrics;
var
  LMetrics: sk_fontmetrics_t;
begin
  TSkiaApi.sk4d_textstyle_get_font_metrics(GetHandle, @LMetrics);
  Result := LMetrics.ToFontMetrics;
end;

function TSkTextStyle.GetFontSize: Single;
begin
  Result := TSkiaApi.sk4d_textstyle_get_font_size(GetHandle);
end;

function TSkTextStyle.GetFontStyle: TSkFontStyle;
begin
  TSkiaApi.sk4d_textstyle_get_font_style(GetHandle, sk_fontstyle_t(Result));
end;

function TSkTextStyle.GetForeground: ISkPaint;
begin
  Result := TSkPaint.CreateNative(TSkiaApi.sk4d_textstyle_get_foreground(GetHandle));
end;

function TSkTextStyle.GetHeight: Single;
begin
  Result := TSkiaApi.sk4d_textstyle_get_height(GetHandle);
end;

function TSkTextStyle.GetHeightOverride: Boolean;
begin
  Result := TSkiaApi.sk4d_textstyle_get_height_override(GetHandle);
end;

function TSkTextStyle.GetLetterSpacing: Single;
begin
  Result := TSkiaApi.sk4d_textstyle_get_letter_spacing(GetHandle);
end;

function TSkTextStyle.GetLocale: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.CreateNative(TSkiaApi.sk4d_textstyle_get_locale(GetHandle));
  Result  := LResult.Text;
end;

function TSkTextStyle.GetTextBaseline: TSkTextBaseline;
begin
  Result := TSkTextBaseline(TSkiaApi.sk4d_textstyle_get_text_baseline(GetHandle));
end;

function TSkTextStyle.GetTypeface: ISkTypeface;
var
  LHandle: sk_typeface_t;
begin
  LHandle := TSkiaApi.sk4d_textstyle_get_typeface(GetHandle);
  if LHandle = 0 then
    Exit(nil);
  Result := TSkTypeFace.CreateNative(LHandle);
end;

function TSkTextStyle.GetWordSpacing: Single;
begin
  Result := TSkiaApi.sk4d_textstyle_get_word_spacing(GetHandle);
end;

function TSkTextStyle.HasBackground: Boolean;
begin
  Result := TSkiaApi.sk4d_textstyle_has_background(GetHandle);
end;

function TSkTextStyle.HasForeground: Boolean;
begin
  Result := TSkiaApi.sk4d_textstyle_has_foreground(GetHandle);
end;

function TSkTextStyle.IsEqual(const ATextStyle: ISkTextStyle): Boolean;
begin
  if not Assigned(ATextStyle) then
    raise ESkTextStyle.CreateFmt(ParameterCannotBeNil, ['ATextStyle']);
  Result := TSkiaApi.sk4d_textstyle_is_equal(GetHandle, ATextStyle.Handle);
end;

function TSkTextStyle.IsPlaceholder: Boolean;
begin
  Result := TSkiaApi.sk4d_textstyle_is_placeholder(GetHandle);
end;

procedure TSkTextStyle.ResetFontFeatures;
begin
  TSkiaApi.sk4d_textstyle_reset_font_features(GetHandle);
end;

procedure TSkTextStyle.ResetShadows;
begin
  TSkiaApi.sk4d_textstyle_reset_shadows(GetHandle);
end;

procedure TSkTextStyle.SetBackgroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkTextStyle.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_textstyle_set_background_color(GetHandle, APaint.Handle);
end;

procedure TSkTextStyle.SetColor(const AValue: TAlphaColor);
begin
  TSkiaApi.sk4d_textstyle_set_color(GetHandle, AValue);
end;

procedure TSkTextStyle.SetDecorationColor(const AValue: TAlphaColor);
begin
  TSkiaApi.sk4d_textstyle_set_decoration_color(GetHandle, AValue);
end;

procedure TSkTextStyle.SetDecorationMode(const AValue: TSkTextDecorationMode);
begin
  TSkiaApi.sk4d_textstyle_set_decoration_mode(GetHandle, sk_textdecorationmode_t(AValue));
end;

procedure TSkTextStyle.SetDecorations(const AValue: TSkTextDecorations);
begin
  TSkiaApi.sk4d_textstyle_set_decorations(GetHandle, Byte(AValue));
end;

procedure TSkTextStyle.SetDecorationStyle(const AValue: TSkTextDecorationStyle);
begin
  TSkiaApi.sk4d_textstyle_set_decoration_style(GetHandle, sk_textdecorationstyle_t(AValue));
end;

procedure TSkTextStyle.SetDecorationThicknessMultiplier(const AValue: Single);
begin
  TSkiaApi.sk4d_textstyle_set_decoration_thickness_multiplier(GetHandle, AValue);
end;

procedure TSkTextStyle.SetFontFamilies(const AValue: TArray<string>);
var
  I: Integer;
  LCount: NativeUInt;
  LFontFamilies: TArray<sk_string_t>;
  LStrings: TArray<ISkString>;
begin
  LCount := Length(AValue);
  SetLength(LStrings, LCount);
  SetLength(LFontFamilies, LCount);
  for I := 0 to LCount - 1 do
  begin
    LStrings[I]      := TSkString.Create(AValue[I]);
    LFontFamilies[I] := LStrings[I].Handle;
  end;
  TSkiaApi.sk4d_textstyle_set_font_families(GetHandle, @LFontFamilies[0], LCount);
end;

procedure TSkTextStyle.SetFontSize(const AValue: Single);
begin
  TSkiaApi.sk4d_textstyle_set_font_size(GetHandle, AValue);
end;

procedure TSkTextStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  TSkiaApi.sk4d_textstyle_set_font_style(GetHandle, @sk_fontstyle_t(AValue));
end;

procedure TSkTextStyle.SetForegroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkTextStyle.CreateFmt(ParameterCannotBeNil, ['APaint']);
  TSkiaApi.sk4d_textstyle_set_foreground_color(GetHandle, APaint.Handle);
end;

procedure TSkTextStyle.SetHeight(const AValue: Single);
begin
  TSkiaApi.sk4d_textstyle_set_height(GetHandle, AValue);
end;

procedure TSkTextStyle.SetHeightOverride(const AValue: Boolean);
begin
  TSkiaApi.sk4d_textstyle_set_height_override(GetHandle, AValue);
end;

procedure TSkTextStyle.SetLetterSpacing(const AValue: Single);
begin
  TSkiaApi.sk4d_textstyle_set_letter_spacing(GetHandle, AValue);
end;

procedure TSkTextStyle.SetLocale(const AValue: string);
var
  LString: ISkString;
begin
  LString := TSkString.Create(AValue);
  TSkiaApi.sk4d_textstyle_set_locale(GetHandle, LString.Handle);
end;

procedure TSkTextStyle.SetPlaceholder;
begin
  TSkiaApi.sk4d_textstyle_set_place_holder(GetHandle);
end;

procedure TSkTextStyle.SetTextBaseline(const AValue: TSkTextBaseline);
begin
  TSkiaApi.sk4d_textstyle_set_text_baseline(GetHandle, sk_textbaseline_t(AValue));
end;

procedure TSkTextStyle.SetTypeface(const AValue: ISkTypeface);
begin
  TSkiaApi.sk4d_textstyle_set_typeface(GetHandle, SafeHandle(AValue));
end;

procedure TSkTextStyle.SetWordSpacing(const AValue: Single);
begin
  TSkiaApi.sk4d_textstyle_set_word_spacing(GetHandle, AValue);
end;

{ TSkStrutStyle }

constructor TSkStrutStyle.Create;
begin
  CreateNative(TSkiaApi.sk4d_strutstyle_create());
end;

class procedure TSkStrutStyle.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_strutstyle_destroy(AHandle);
end;

function TSkStrutStyle.GetEnabled: Boolean;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_enabled(GetHandle);
end;

function TSkStrutStyle.GetFontFamilies: TArray<string>;
var
  I: Integer;
  LCount: NativeUInt;
  LFontFamilies: TArray<sk_string_t>;
  LStrings: TArray<ISkString>;
begin
  LCount := TSkiaApi.sk4d_strutstyle_get_font_families(GetHandle, nil);
  SetLength(LStrings, LCount);
  SetLength(LFontFamilies, LCount);
  SetLength(Result, LCount);
  for I := 0 to LCount - 1 do
  begin
    LStrings[I]      := TSkString.Create;
    LFontFamilies[I] := LStrings[I].Handle;
  end;
  TSkiaApi.sk4d_strutstyle_get_font_families(GetHandle, @LFontFamilies[0]);
  for I := 0 to LCount - 1 do
    Result[I] := LStrings[I].Text;
end;

function TSkStrutStyle.GetFontSize: Single;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_font_size(GetHandle);
end;

function TSkStrutStyle.GetFontStyle: TSkFontStyle;
begin
  TSkiaApi.sk4d_strutstyle_get_font_style(GetHandle, sk_fontstyle_t(Result));
end;

function TSkStrutStyle.GetForceHeight: Boolean;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_force_height(GetHandle);
end;

function TSkStrutStyle.GetHeight: Single;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_height(GetHandle);
end;

function TSkStrutStyle.GetHeightOverride: Boolean;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_height_override(GetHandle);
end;

function TSkStrutStyle.GetLeading: Single;
begin
  Result := TSkiaApi.sk4d_strutstyle_get_leading(GetHandle);
end;

function TSkStrutStyle.IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
begin
  if not Assigned(AStrutStyle) then
    raise ESkStrutStyle.CreateFmt(ParameterCannotBeNil, ['AStrutStyle']);
  Result := TSkiaApi.sk4d_strutstyle_is_equal(GetHandle, AStrutStyle.Handle);
end;

procedure TSkStrutStyle.SetEnabled(const AValue: Boolean);
begin
  TSkiaApi.sk4d_strutstyle_set_enabled(GetHandle, AValue);
end;

procedure TSkStrutStyle.SetFontFamilies(const AValue: TArray<string>);
var
  I: Integer;
  LCount: NativeUInt;
  LFontFamilies: TArray<sk_string_t>;
  LStrings: TArray<ISkString>;
begin
  LCount := Length(AValue);
  SetLength(LStrings, LCount);
  SetLength(LFontFamilies, LCount);
  for I := 0 to LCount - 1 do
  begin
    LStrings[I]      := TSkString.Create(AValue[I]);
    LFontFamilies[I] := LStrings[I].Handle;
  end;
  TSkiaApi.sk4d_strutstyle_set_font_families(GetHandle, @LFontFamilies[0], LCount);
end;

procedure TSkStrutStyle.SetFontSize(const AValue: Single);
begin
  TSkiaApi.sk4d_strutstyle_set_font_size(GetHandle, AValue);
end;

procedure TSkStrutStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  TSkiaApi.sk4d_strutstyle_set_font_style(GetHandle, @sk_fontstyle_t(AValue));
end;

procedure TSkStrutStyle.SetForceHeight(const AValue: Boolean);
begin
  TSkiaApi.sk4d_strutstyle_set_force_height(GetHandle, AValue);
end;

procedure TSkStrutStyle.SetHeight(const AValue: Single);
begin
  TSkiaApi.sk4d_strutstyle_set_height(GetHandle, AValue);
end;

procedure TSkStrutStyle.SetHeightOverride(const AValue: Boolean);
begin
  TSkiaApi.sk4d_strutstyle_set_height_override(GetHandle, AValue);
end;

procedure TSkStrutStyle.SetLeading(const AValue: Single);
begin
  TSkiaApi.sk4d_strutstyle_set_leading(GetHandle, AValue);
end;

{ TSkParagraphStyle }

constructor TSkParagraphStyle.Create;
begin
  CreateNative(TSkiaApi.sk4d_paragraphstyle_create());
end;

class procedure TSkParagraphStyle.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_paragraphstyle_destroy(AHandle);
end;

function TSkParagraphStyle.GetDrawOptions: TSkDrawOptions;
begin
  Result := TSkDrawOptions(TSkiaApi.sk4d_paragraphstyle_get_draw_options(GetHandle));
end;

function TSkParagraphStyle.GetEffectiveAlign: TSkTextAlign;
begin
  Result := TSkTextAlign(TSkiaApi.sk4d_paragraphstyle_get_effective_align(GetHandle));
end;

function TSkParagraphStyle.GetEllipsis: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.CreateNative(TSkiaApi.sk4d_paragraphstyle_get_ellipsis(GetHandle));
  Result  := LResult.Text;
end;

function TSkParagraphStyle.GetHeight: Single;
begin
  Result := TSkiaApi.sk4d_paragraphstyle_get_height(GetHandle);
end;

function TSkParagraphStyle.GetMaxLines: NativeUInt;
begin
  Result := TSkiaApi.sk4d_paragraphstyle_get_max_lines(GetHandle);
end;

function TSkParagraphStyle.GetStrutStyle: ISkStrutStyle;
begin
  Result := TSkStrutStyle.CreateNative(TSkiaApi.sk4d_paragraphstyle_get_strut_style(GetHandle), False);
end;

function TSkParagraphStyle.GetTextAlign: TSkTextAlign;
begin
  Result := TSkTextAlign(TSkiaApi.sk4d_paragraphstyle_get_text_align(GetHandle));
end;

function TSkParagraphStyle.GetTextDirection: TSkTextDirection;
begin
  Result := TSkTextDirection(TSkiaApi.sk4d_paragraphstyle_get_text_direction(GetHandle));
end;

function TSkParagraphStyle.GetTextHeightBehaviors: TSkTextHeightBehaviors;
begin
  Result := TSkTextHeightBehaviors(Byte(TSkiaApi.sk4d_paragraphstyle_get_text_height_behaviors(GetHandle)));
end;

function TSkParagraphStyle.GetTextStyle: ISkTextStyle;
begin
  Result := TSkTextStyle.CreateNative(TSkiaApi.sk4d_paragraphstyle_get_text_style(GetHandle), False);
end;

function TSkParagraphStyle.HasUnlimitedLines: Boolean;
begin
  Result := TSkiaApi.sk4d_paragraphstyle_has_unlimited_lines(GetHandle);
end;

function TSkParagraphStyle.IsEllipsized: Boolean;
begin
  Result := TSkiaApi.sk4d_paragraphstyle_is_ellipsized(GetHandle);
end;

function TSkParagraphStyle.IsEqual(
  const AParagraphStyle: ISkParagraphStyle): Boolean;
begin
  if not Assigned(AParagraphStyle) then
    raise ESkParagraphStyle.CreateFmt(ParameterCannotBeNil, ['AParagraphStyle']);
  Result := TSkiaApi.sk4d_paragraphstyle_is_equal(GetHandle, AParagraphStyle.Handle);
end;

function TSkParagraphStyle.IsHintingOn: Boolean;
begin
  Result := TSkiaApi.sk4d_paragraphstyle_is_hinting_on(GetHandle);
end;

procedure TSkParagraphStyle.SetDrawOptions(const AValue: TSkDrawOptions);
begin
  TSkiaApi.sk4d_paragraphstyle_set_draw_options(GetHandle, sk_drawoptions_t(AValue));
end;

procedure TSkParagraphStyle.SetEllipsis(const AValue: string);
var
  LString: ISkString;
begin
  LString := TSkString.Create(AValue);
  TSkiaApi.sk4d_paragraphstyle_set_ellipsis(GetHandle, LString.Handle);
end;

procedure TSkParagraphStyle.SetHeight(const AValue: Single);
begin
  TSkiaApi.sk4d_paragraphstyle_set_height(GetHandle, AValue);
end;

procedure TSkParagraphStyle.SetMaxLines(const AValue: NativeUInt);
begin
  TSkiaApi.sk4d_paragraphstyle_set_max_lines(GetHandle, AValue);
end;

procedure TSkParagraphStyle.SetStrutStyle(const AValue: ISkStrutStyle);
begin
  if not Assigned(AValue) then
    raise ESkParagraphStyle.CreateFmt(ParameterCannotBeNil, ['AValue']);
  TSkiaApi.sk4d_paragraphstyle_set_strut_style(GetHandle, AValue.Handle);
end;

procedure TSkParagraphStyle.SetTextAlign(const AValue: TSkTextAlign);
begin
  TSkiaApi.sk4d_paragraphstyle_set_text_align(GetHandle, sk_textalign_t(AValue));
end;

procedure TSkParagraphStyle.SetTextDirection(const AValue: TSkTextDirection);
begin
  TSkiaApi.sk4d_paragraphstyle_set_text_direction(GetHandle, sk_textdirection_t(AValue));
end;

procedure TSkParagraphStyle.SetTextHeightBehaviors(
  const AValue: TSkTextHeightBehaviors);
begin
  TSkiaApi.sk4d_paragraphstyle_set_text_height_behaviors(GetHandle, Byte(AValue));
end;

procedure TSkParagraphStyle.SetTextStyle(const AValue: ISkTextStyle);
begin
  if not Assigned(AValue) then
    raise ESkParagraphStyle.CreateFmt(ParameterCannotBeNil, ['AValue']);
  TSkiaApi.sk4d_paragraphstyle_set_text_style(GetHandle, AValue.Handle);
end;

procedure TSkParagraphStyle.TurnHintingOff;
begin
  TSkiaApi.sk4d_paragraphstyle_turn_hinting_off(GetHandle);
end;

{ TSkParagraph }

class procedure TSkParagraph.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_paragraph_destroy(AHandle);
end;

function TSkParagraph.GetHeight: Single;
begin
  Result := TSkiaApi.sk4d_paragraph_get_height(GetHandle);
end;

function TSkParagraph.GetMaxWidth: Single;
begin
  Result := TSkiaApi.sk4d_paragraph_get_max_width(GetHandle);
end;

function TSkParagraph.GetRectsForRange(const AFirst, ALast: Cardinal;
  const ARectHeightStyle: TSkRectHeightStyle;
  const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
var
  LCount: NativeUInt;
begin
  LCount := TSkiaApi.sk4d_paragraph_get_rects_for_range(GetHandle, AFirst, ALast, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), nil);
  SetLength(Result, LCount);
  TSkiaApi.sk4d_paragraph_get_rects_for_range(GetHandle, AFirst, ALast, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), @sk_textbox_t(Result[0]));
end;

procedure TSkParagraph.GetWordBoundary(const AOffset: Cardinal; out AFirst,
  ALast: Cardinal);
begin
  TSkiaApi.sk4d_paragraph_get_word_boundary(GetHandle, AOffset, AFirst, ALast);
end;

procedure TSkParagraph.Layout(const AWidth: Single);
begin
  TSkiaApi.sk4d_paragraph_layout(GetHandle, AWidth);
end;

procedure TSkParagraph.Render(const ACanvas: ISkCanvas; const AX, AY: Single);
begin
  if not Assigned(ACanvas) then
    raise ESkSkParagraph.CreateFmt(ParameterCannotBeNil, ['ACanvas']);
  TSkiaApi.sk4d_paragraph_render(GetHandle, ACanvas.Handle, AX, AY);
end;

{ TSkPlaceholderStyle }

constructor TSkPlaceholderStyle.Create(const AWidth, AHeight: Single;
  const AAlignment: TSkPlaceholderAlignment; const ABaseline: TSkTextBaseline;
  const ABaselineOffset: Single);
begin
  Width          := AWidth;
  Height         := AHeight;
  Alignment      := AAlignment;
  Baseline       := ABaseline;
  BaselineOffset := ABaselineOffset;
end;

class operator TSkPlaceholderStyle.Equal(const APlaceholderStyle1,
  APlaceholderStyle2: TSkPlaceholderStyle): Boolean;
begin
  Result := (SameValue(APlaceholderStyle1.Width,  APlaceholderStyle2.Width,  Single.Epsilon)) and
            (SameValue(APlaceholderStyle1.Height, APlaceholderStyle2.Height, Single.Epsilon)) and
            (APlaceholderStyle1.Alignment = APlaceholderStyle2.Alignment                    ) and
            (APlaceholderStyle1.Baseline  = APlaceholderStyle2.Baseline                     ) and
            ((APlaceholderStyle1.Alignment <> TSkPlaceholderAlignment.Baseline) or (SameValue(APlaceholderStyle1.BaselineOffset, APlaceholderStyle2.BaselineOffset, Single.Epsilon)));
end;

class operator TSkPlaceholderStyle.NotEqual(const APlaceholderStyle1,
  APlaceholderStyle2: TSkPlaceholderStyle): Boolean;
begin
  Result := not (APlaceholderStyle1 = APlaceholderStyle2);
end;

{ TSkParagraphBuilder }

procedure TSkParagraphBuilder.AddPlaceholder(
  const APlaceholder: TSkPlaceholderStyle);
begin
  TSkiaApi.sk4d_paragraphbuilder_add_placeholder(GetHandle, @sk_placeholderstyle_t(APlaceholder));
end;

procedure TSkParagraphBuilder.AddText(const AText: string);
var
  LText: UTF8String;
begin
  LText := UTF8String(AText);
  TSkiaApi.sk4d_paragraphbuilder_add_text(GetHandle, @LText[Low(LText)], Length(LText));
end;

constructor TSkParagraphBuilder.Create(const AParagraphStyle: ISkParagraphStyle;
  const AFontCollection: ISkFontCollection);
begin
  if not Assigned(AParagraphStyle) then
    raise ESkParagraphBuilder.CreateFmt(ParameterCannotBeNil, ['AParagraphStyle']);
  if not Assigned(AFontCollection) then
    raise ESkParagraphBuilder.CreateFmt(ParameterCannotBeNil, ['AFontCollection']);
  CreateNative(TSkiaApi.sk4d_paragraphbuilder_create(AParagraphStyle.Handle, AFontCollection.Handle));
end;

constructor TSkParagraphBuilder.Create(
  const AParagraphStyle: ISkParagraphStyle);
var
  LFontCollection: ISkFontCollection;
begin
  LFontCollection := TSkFontCollection.Create;
  Create(AParagraphStyle, LFontCollection);
end;

function TSkParagraphBuilder.Detach: ISkParagraph;
begin
  Result := TSkParagraph.CreateNative(TSkiaApi.sk4d_paragraphbuilder_detach(GetHandle))
end;

class procedure TSkParagraphBuilder.DoDestroy(const AHandle: THandle);
begin
  TSkiaApi.sk4d_paragraphbuilder_destroy(AHandle);
end;

procedure TSkParagraphBuilder.Pop;
begin
  TSkiaApi.sk4d_paragraphbuilder_pop(GetHandle);
end;

procedure TSkParagraphBuilder.PushStyle(const ATextStyle: ISkTextStyle);
begin
  if not Assigned(ATextStyle) then
    raise ESkParagraphBuilder.CreateFmt(ParameterCannotBeNil, ['ATextStyle']);
  TSkiaApi.sk4d_paragraphbuilder_push_style(GetHandle, ATextStyle.Handle);
end;

end.
