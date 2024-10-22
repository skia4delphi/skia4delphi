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
unit System.Skia;

interface

{$IFDEF FPC}
  {$IF DEFINED(FPC_FULLVERSION) and (FPC_FULLVERSION < 30301)}
    {$ERROR Requires at least FPC 3.3.1}
  {$ENDIF}
  {$MODE DELPHI}
  {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$MODESWITCH FUNCTIONREFERENCES}
  {$MODESWITCH UNICODESTRINGS}
  {$IFDEF STANDALONE}
    {$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
    {$WARN 5024 off : Parameter "$1" not used}
    {$WARN 5026 off : Value parameter "$1" is assigned but never used}
    {$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
    {$WARN 5092 off : Variable "$1" of a managed type does not seem to be initialized}
    {$WARN 5093 off : Function result variable of a managed type does not seem to be initialized}
    {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
  {$ENDIF}
{$ENDIF}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$RANGECHECKS OFF}
{$SCOPEDENUMS ON}

uses
  {$IFDEF FPC}
  { FPC }
  Classes,
  System.UITypes,
  SysUtils,
  Types,
  {$ELSE}
  { Delphi }
  System.Classes,
  System.DateUtils,
  System.Math.Vectors,
  System.SysUtils,
  System.Types,
  System.UITypes,
  {$ENDIF}

  { Skia }
  System.Skia.API;

const
  SkVersion = '6.2.0';

{$REGION 'FPC Compatibility'}
{$IFDEF FPC}

type
  TPolygon = array of TPointF;

  TVector3DType = array [0..3] of Single;

  TVectorArray = array [0..2] of Single;

  TVector = record
    case Integer of
      0: (V: TVectorArray);
      1: (X: Single;
          Y: Single;
          W: Single);
  end;

  TMatrixArray = array [0..2] of TVector;

  TMatrix = record
    class function CreateScaling(const AScaleX, AScaleY: Single): TMatrix; static;
    class function CreateTranslation(const ADeltaX, ADeltaY: Single): TMatrix; static;
    class operator Multiply(const AMatrix1, AMatrix2: TMatrix): TMatrix;
    case Integer of
      0: (M: TMatrixArray);
      1: (m11, m12, m13: Single;
          m21, m22, m23: Single;
          m31, m32, m33: Single);
  end;

  TMatrixConstants = record helper for TMatrix
    const
      Identity: TMatrix = (m11: 1; m12: 0; m13: 0;
                           m21: 0; m22: 1; m23: 0;
                           m31: 0; m32: 0; m33: 1);
  end;

  TVector3D = record
    case Integer of
      0: (V: TVector3DType);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single);
  end;

  TMatrix3DType = array [0..3] of TVector3D;

  TMatrix3D = record
    class function CreateScaling(const AScale: TPoint3D): TMatrix3D; static;
    class function CreateTranslation(const ATranslation: TPoint3D): TMatrix3D; static;
    case Integer of
      0: (M: TMatrix3DType);
      1: (m11, m12, m13, m14: Single;
          m21, m22, m23, m24: Single;
          m31, m32, m33, m34: Single;
          m41, m42, m43, m44: Single);
  end;

  TMatrix3DConstants = record helper for TMatrix3D
    const
      Identity: TMatrix3D = (m11: 1; m12: 0; m13: 0; m14: 0;
                             m21: 0; m22: 1; m23: 0; m24: 0;
                             m31: 0; m32: 0; m33: 1; m34: 0;
                             m41: 0; m42: 0; m43: 0; m44: 1);
  end;

{$ENDIF}
{$ENDREGION}

type
  TGrBackendAPI                   = (OpenGl, Vulkan, Metal);
  TGrShaderCacheStrategy          = (SkSL, BackendSource, BackendBinary);
  TGrSurfaceOrigin                = (TopLeft, BottomLeft);
  TGrVkAllocFlag                  = (Noncoherent, Mappable, LazilyAllocated);
  TSkAffinity                     = (Upstream, Downstream);
  TSkAlphaType                    = (Unknown, Opaque, Premul, Unpremul);
  TSkBlendMode                    = (Clear, Src, Dest, SrcOver, DestOver, SrcIn, DestIn, SrcOut, DestOut, SrcATop, DestATop, &Xor, Plus, Modulate, Screen, Overlay, Darken, Lighten, ColorDodge, ColorBurn, HardLight, SoftLight, Difference, Exclusion, Multiply, Hue, Saturation, Color, Luminosity);
  TSkBlurStyle                    = (Normal, Solid, Outer, Inner);
  TSkBreakType                    = (Words, Graphemes, Lines);
  TSkClipOp                       = (Difference, Intersect);
  TSkColorChannel                 = (R, G, B, A);
  TSkColorType                    = (Unknown, Alpha8, RGB565, ARGB4444, RGBA8888, RGB888X, BGRA8888, RGBA1010102, BGRA1010102, RGB101010X, BGR101010X, Gray8, RGBAF16Normalized, RGBAF16, RGBAF32, RG88, AlphaF16, RGF16, Alpha16, RG1616, RGBA16161616, SRGBA8888, R8);
  TSkContrastInvertStyle          = (NoInvert, InvertBrightness, InvertLightness);
  TSkDirection                    = (LeftToRight, RightToLeft);
  TSkDrawPointsMode               = (Points, Lines, Polygon);
  TSkEncodedImageFormat           = (BMP, GIF, ICO, JPEG, PNG, WBMP, WEBP, PKM, KTX, ASTC, DNG, HEIF, AVIF);
  TSkFilterMode                   = (Nearest, Linear);
  TSkFontEdging                   = (Alias, AntiAlias, SubpixelAntiAlias);
  TSkFontHinting                  = (None, Slight, Normal, Full);
  TSkFontMetricsFlag              = (UnderlineThicknessIsValid, UnderlinePositionIsValid, StrikeoutThicknessIsValid, StrikeoutPositionIsValid, BoundsInvalid);
  TSkFontSlant                    = (Upright, Italic, Oblique);
  TSkFontWeight                   = (Invisible, Thin = 100, ExtraLight = 200, Light = 300, Normal = 400, Medium = 500, SemiBold = 600, Bold = 700, ExtraBold = 800, Black = 900, ExtraBlack = 1000);
  TSkFontWidth                    = (UltraCondensed = 1, ExtraCondensed, Condensed, SemiCondensed, Normal, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded);
  TSkImageCachingHint             = (Allow, Disallow);
  TSkLatticeRectType              = (Default, Transparent, FixedColor);
  TSkMipmapMode                   = (None, Nearest, Linear);
  TSkottieAnimationRenderFlag     = (SkipTopLevelIsolation, DisableTopLevelClipping);
  TSkPaintStyle                   = (Fill, Stroke, StrokeAndFill);
  TSkParagraphVisitorFlag         = (WhiteSpace);
  TSkPathArcSize                  = (Small, Large);
  TSkPathDirection                = (CW, CCW);
  TSkPathEffect1DStyle            = (Translate, Rotate, Morph);
  TSkPathEffectTrimMode           = (Normal, Inverted);
  TSkPathFillType                 = (Winding, EvenOdd, InverseWinding, InverseEvenOdd);
  TSkPathMeasureMatrixFlag        = (Position, Tangent);
  TSkPathOp                       = (Difference, Intersect, Union, &Xor, ReverseDifference);
  TSkPathVerb                     = (Move, Line, Quad, Conic, Cubic, Close);
  TSkPixelGeometry                = (Unknown, RGBHorizontal, BGRHorizontal, RGBVertical, BGRVertical);
  TSkPlaceholderAlignment         = (Baseline, AboveBaseline, BelowBaseline, Top, Bottom, Middle);
  TSkRectHeightStyle              = (Tight, Max, IncludeLineSpacingMiddle, IncludeLineSpacingTop, IncludeLineSpacingBottom, Strut);
  TSkRectWidthStyle               = (Tight, Max);
  TSkRegionOp                     = (Difference, Intersect, Union, &Xor, ReverseDifference, Replace);
  TSkRoundRectCorner              = (UpperLeft, UpperRight, LowerRight, LowerLeft);
  TSkRuntimeEffectChildType       = (Shader, ColorFilter, Blender);
  TSkRuntimeEffectUniformType     = (Float, Float2, Float3, Float4, Float2x2, Float3x3, Float4x4, Int, Int2, Int3, Int4);
  TSkSaveLayerFlag                = (InitWithPrevious = 2, F16ColorType = 4);
  TSkSegmentMask                  = (Line, Quad, Conic, Cubic);
  TSkSrcRectConstraint            = (Close, Fast);
  TSkStrokeCap                    = (Butt, Round, Square);
  TSkStrokeJoin                   = (Miter, Round, Bevel);
  TSkSurfacePropertiesFlag        = (UseDeviceIndependentFonts, DynamicMSAA);
  TSkSVGAspectAlign               = (XMinYMin, XMidYMin, XMaxYMin, XMinYMid = 4, XMidYMid, XMaxYMid, XMinYMax = 8, XMidYMax, XMaxYMax, None = 16);
  TSkSVGAspectScale               = (Meet, Slice);
  TSkSVGCanvasFlag                = (ConvertTextToPaths, NoPrettyXML, RelativePathEncoding);
  TSkSVGLengthUnit                = (Unknown, Number, Percentage, EmUnit, ExUnit, Pixel, Centimeter, Millimeter, Inch, Point, Pica);
  TSkTextAlign                    = (Left, Right, Center, Justify, Start, Terminate);
  TSkTextBaseline                 = (Alphabetic, Ideographic);
  TSkTextDecoration               = (Underline, Overline, LineThrough);
  TSkTextDecorationStyle          = (Solid, Double, Dotted, Dashed, Wavy);
  TSkTextDirection                = (RightToLeft, LeftToRight);
  TSkTextHeightBehavior           = (DisableFirstAscent, DisableLastDescent);
  TSkTileMode                     = (Clamp, &Repeat, Mirror, Decal);
  TSkVertexMode                   = (Triangles, TriangleStrip, TriangleFan);

  TGrVkAllocFlags                 = set of TGrVkAllocFlag;
  TSkFontMetricsFlags             = set of TSkFontMetricsFlag;
  TSkottieAnimationRenderFlags    = set of TSkottieAnimationRenderFlag;
  TSkParagraphVisitorFlags        = set of TSkParagraphVisitorFlag;
  TSkPathMeasureMatrixFlags       = set of TSkPathMeasureMatrixFlag;
  TSkSaveLayerFlags               = set of TSkSaveLayerFlag;
  TSkSegmentMasks                 = set of TSkSegmentMask;
  TSkSurfacePropertiesFlags       = set of TSkSurfacePropertiesFlag;
  TSkSVGCanvasFlags               = set of TSkSVGCanvasFlag;
  TSkTextDecorations              = set of TSkTextDecoration;
  TSkTextHeightBehaviors          = set of TSkTextHeightBehavior;

  IGrGlInterface                  = interface;
  IGrPersistentCache              = interface;
  IGrShaderErrorHandler           = interface;
  IGrVkExtensions                 = interface;
  ISkColorSpace                   = interface;
  ISkColorSpaceICCProfile         = interface;
  ISkFont                         = interface;
  ISkImage                        = interface;
  ISkImageFilter                  = interface;
  ISkPaint                        = interface;
  ISkParagraphStyle               = interface;
  ISkPath                         = interface;
  ISkPathEffect                   = interface;
  ISkPicture                      = interface;
  ISkPixmap                       = interface;
  ISkRegion                       = interface;
  ISkResourceProvider             = interface;
  ISkRoundRect                    = interface;
  ISkRuntimeShaderBuilder         = interface;
  ISkShader                       = interface;
  ISkSurface                      = interface;
  ISkSVGNode                      = interface;
  ISkSVGSVG                       = interface;
  ISkTextBlob                     = interface;
  ISkTextStyle                    = interface;
  ISkTraceMemoryDump              = interface;
  ISkTypeface                     = interface;
  ISkTypefaceFontProvider         = interface;
  ISkVertices                     = interface;

  GrGlenum                        = Cardinal;
  GrGluint                        = Cardinal;
  GrMtlHandle                     = Pointer;
  GrVkBool32                      = Cardinal;
  GrVkChromaLocation              = Integer;
  GrVkDevice                      = NativeInt;
  GrVkDeviceMemory                = UInt64;
  GrVkDeviceSize                  = UInt64;
  GrVkFilter                      = Integer;
  GrVkFormat                      = Integer;
  GrVkFormatFeatureFlags          = Cardinal;
  GrVkImage                       = UInt64;
  GrVkImageLayout                 = Integer;
  GrVkImageTiling                 = Integer;
  GrVkImageUsageFlags             = Cardinal;
  GrVkInstance                    = NativeInt;
  GrVkPhysicalDevice              = NativeInt;
  GrVkQueue                       = NativeInt;
  GrVkSamplerYcbcrModelConversion = Integer;
  GrVkSamplerYcbcrRange           = Integer;
  GrVkSemaphore                   = UInt64;
  GrVkSharingMode                 = Integer;

  TSkOverdrawColor                = array[0..5] of TAlphaColor;
  TSkParticleUniformData          = array[0..0] of Single;
  TSkPatchColors                  = array[0..3] of TAlphaColor;
  TSkPatchCubics                  = array[0..11] of TPointF;
  TSkPatchTexCoords               = array[0..3] of TPointF;
  TSkPathPoints                   = array[0..3] of TPointF;
  TSkRoundRectRadii               = array[TSkRoundRectCorner] of TPointF;
  TSkTableFilter                  = array[0..255] of Byte;

  {$POINTERMATH ON}
  PCardinalArray                  = ^Cardinal;
  PPointFArray                    = ^TPointF;
  PSingleArray                    = ^Single;
  PWordArray                      = ^Word;
  {$POINTERMATH OFF}

  TGrGlGetProc                    = reference to function  (const AName: MarshaledAString): Pointer;
  TSkFontPathProc                 = reference to procedure (const APathOrNil: ISkPath; const AMatrix: TMatrix);
  TSkImageRasterReleaseProc       = reference to procedure (const APixels: Pointer);
  TSkImageTextureReleaseProc      = reference to procedure ();
  TSkUnicodeBidiRegionProc        = reference to procedure (const AStart, AEnd: Integer; const ALevel: Byte);
  TGrVkGetProc                    = reference to function  (const AName: MarshaledAString; const AInstance: GrVkInstance; const ADevice: GrVkDevice): Pointer;
  TSkSurfaceRasterReleaseProc     = reference to procedure (const APixels: Pointer);
  TSkUnicodeBreakProc             = reference to procedure (const APosition, AStatus: Integer);
  TSkUnicodeCodepointProc         = reference to procedure (const AUnichar, AStart, AEnd: Integer);

  ESkArgumentException            = class(Exception);
  ESkException                    = class(Exception);

  { TGrContextOptions }

  TGrContextOptions = record
    BufferMapThreshold: Integer;
    DoManualMipmapping: Boolean;
    AllowPathMaskCaching: Boolean;
    GlyphCacheTextureMaximumBytes: NativeUInt;
    AvoidStencilBuffers: Boolean;
    RuntimeProgramCacheSize: Integer;
    PersistentCache: IGrPersistentCache;
    ShaderCacheStrategy: TGrShaderCacheStrategy;
    ShaderErrorHandler: IGrShaderErrorHandler;
  end;

  { TGrGlFramebufferInfo }

  TGrGlFramebufferInfo = record
    FBOID: GrGluint;
    Format: GrGlenum;
  end;

  { TGrGlTextureInfo }

  TGrGlTextureInfo = record
    Target: GrGlenum;
    ID: GrGluint;
    Format: GrGlenum;
  end;

  { TGrMtlBackendContext }

  TGrMtlBackendContext = record
    Device: GrMtlHandle;
    Queue: GrMtlHandle;
    BinaryArchive: GrMtlHandle;
  end;

  { TGrMtlTextureInfo }

  TGrMtlTextureInfo = record
    Texture: GrMtlHandle;
  end;

  { TGrVkAlloc }

  TGrVkAlloc = record
    DeviceMemory: GrVkDeviceMemory;
    Offset: GrVkDeviceSize;
    Size: GrVkDeviceSize;
    Flags: TGrVkAllocFlags;
    Memory: sk_handle_t;
  end;

  { TGrVkBackendContext }

  TGrVkBackendContext = record
    Instance: GrVkInstance;
    PhysicalDevice: GrVkPhysicalDevice;
    Device: GrVkDevice;
    Queue: GrVkQueue;
    GraphicsQueueIndex: Cardinal;
    MaxApiVersion: Cardinal;
    Extensions: IGrVkExtensions;
    PhysicalDeviceFeatures: Pointer;
    PhysicalDeviceFeatures2: Pointer;
    GetProc: TGrVkGetProc;
    ProtectedContext: Boolean;
  end;

  { TGrVkYcbcrConversionInfo }

  TGrVkYcbcrConversionInfo = record
    Format: GrVkFormat;
    FExternalFormat: UInt64;
    YcbcrModel: GrVkSamplerYcbcrModelConversion;
    YcbcrRange: GrVkSamplerYcbcrRange;
    XChromaOffset: GrVkChromaLocation;
    YChromaOffset: GrVkChromaLocation;
    ChromaFilter: GrVkFilter;
    ForceExplicitReconstruction: GrVkBool32;
    FormatFeatures: GrVkFormatFeatureFlags;
  end;

  { TGrVkImageInfo }

  TGrVkImageInfo = record
    Image: GrVkImage;
    Alloc: TGrVkAlloc;
    ImageTiling: GrVkImageTiling;
    ImageLayout: GrVkImageLayout;
    Format: GrVkFormat;
    ImageUsageFlags: GrVkImageUsageFlags;
    SampleCount: Cardinal;
    LevelCount: Cardinal;
    CurrentQueueFamily: Cardinal;
    ProtectedImage: Boolean;
    YcbcrConversionInfo: TGrVkYcbcrConversionInfo;
    SharingMode: GrVkSharingMode;
  end;

  { TSkColorMatrix }

  TSkColorMatrix = record
    constructor Create(const AM11, AM12, AM13, AM14, AM15, AM21, AM22, AM23, AM24, AM25, AM31, AM32, AM33, AM34, AM35, AM41, AM42, AM43, AM44, AM45: Single);
    class function CreateSaturation(const ASaturation: Single): TSkColorMatrix; static;
    class function CreateScale(const AScaleR, AScaleG, AScaleB: Single; const AScaleA: Single = 1): TSkColorMatrix; static;
    class function Identity: TSkColorMatrix; static; inline;
    class operator Equal(const AColorMatrix1, AColorMatrix2: TSkColorMatrix): Boolean;
    class operator NotEqual(const AColorMatrix1, AColorMatrix2: TSkColorMatrix): Boolean;
    class operator Multiply(const AColorMatrix1, AColorMatrix2: TSkColorMatrix): TSkColorMatrix;
    case Integer of
      0: (M11, M12, M13, M14, M15,
          M21, M22, M23, M24, M25,
          M31, M32, M33, M34, M35,
          M41, M42, M43, M44, M45: Single);
      1: (Vector: array[0..3] of array[0..4] of Single);
  end;

  { TSkColorSpaceXyz }

  TSkColorSpaceXyz = record
  public const
    FixedToFloat = 1.52587890625e-5;
  public
    constructor Create(const AM11, AM12, AM13, AM21, AM22, AM23, AM31, AM32, AM33: Single);
    function Adjoint: TSkColorSpaceXyz;
    function Determinant: Single;
    function Inverse: TSkColorSpaceXyz;
    class function AdobeRGB: TSkColorSpaceXyz; static; inline;
    class function DisplayP3: TSkColorSpaceXyz; static; inline;
    class function Identity: TSkColorSpaceXyz; static; inline;
    class function Rec2020: TSkColorSpaceXyz; static; inline;
    class function SRGB: TSkColorSpaceXyz; static; inline;
    class operator Equal(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
    class operator NotEqual(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
    class operator Multiply(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): TSkColorSpaceXyz;
    case Integer of
      0: (M11, M12, M13,
          M21, M22, M23,
          M31, M32, M33: Single);
      1: (Vector: array[0..2] of array[0..2] of Single);
  end;

  { TSkColorSpacePrimaries }

  TSkColorSpacePrimaries = record
    RX: Single;
    RY: Single;
    GX: Single;
    GY: Single;
    BX: Single;
    BY: Single;
    WX: Single;
    WY: Single;
    constructor Create(const ARX, ARY, AGX, AGY, ABX, ABY, AWX, AWY: Single);
    class operator Equal(const AColorSpacePrimaries1, AColorSpacePrimaries2: TSkColorSpacePrimaries): Boolean;
    class operator NotEqual(const AColorSpacePrimaries1, AColorSpacePrimaries2: TSkColorSpacePrimaries): Boolean;
    function ToColorSpaceXyz: TSkColorSpaceXyz;
  end;

  { TSkColorSpaceTransferFunction }

  TSkColorSpaceTransferFunction = record
    G: Single;
    A: Single;
    B: Single;
    C: Single;
    D: Single;
    E: Single;
    F: Single;
    constructor Create(const AG, AA, AB, AC, AD, AE, AF: Single);
    function Invert(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function Transform(const AX: Single): Single;
    class function HLG: TSkColorSpaceTransferFunction; static; inline;
    class function Linear: TSkColorSpaceTransferFunction; static; inline;
    class function PQ: TSkColorSpaceTransferFunction; static; inline;
    class function Rec2020: TSkColorSpaceTransferFunction; static; inline;
    class function SRGB: TSkColorSpaceTransferFunction; static; inline;
    class function TwoDotTwo: TSkColorSpaceTransferFunction; static; inline;
    class operator Equal(const AColorSpaceTransferFunction1, AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
    class operator NotEqual(const AColorSpaceTransferFunction1, AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
  end;

  { TSkCubicResampler }

  TSkCubicResampler = record
    B: Single;
    C: Single;
    constructor Create(const AB, AC: Single);
    class function CatmullRom: TSkCubicResampler; static; inline;
    class function Mitchell: TSkCubicResampler; static; inline;
    class operator Equal(const ACubicResampler1, ACubicResampler2: TSkCubicResampler): Boolean;
    class operator NotEqual(const ACubicResampler1, ACubicResampler2: TSkCubicResampler): Boolean;
  end;

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

  { TSkFontStyle }

  TSkFontStyle = record
    Weight: Integer;
    Width: Integer;
    Slant: TSkFontSlant;
    constructor Create(const AWeight, AWidth: Integer; const ASlant: TSkFontSlant); overload;
    constructor Create(const AWeight: TSkFontWeight; const AWidth: TSkFontWidth; const ASlant: TSkFontSlant); overload;
    class function Bold: TSkFontStyle; static; inline;
    class function BoldItalic: TSkFontStyle; static; inline;
    class function Italic: TSkFontStyle; static; inline;
    class function Normal: TSkFontStyle; static; inline;
    class operator Equal(const AFontStyle1, AFontStyle2: TSkFontStyle): Boolean;
    class operator NotEqual(const AFontStyle1, AFontStyle2: TSkFontStyle): Boolean;
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

  { TSkHighContrastConfig }

  TSkHighContrastConfig = record
    Grayscale: Boolean;
    InvertStyle: TSkContrastInvertStyle;
    Contrast: Single;
    constructor Create(const AGrayscale: Boolean; const AInvertStyle: TSkContrastInvertStyle; const AContrast: Single);
    class operator Equal(const AHighContrastConfig1, AHighContrastConfig2: TSkHighContrastConfig): Boolean;
    class operator NotEqual(const AHighContrastConfig1, AHighContrastConfig2: TSkHighContrastConfig): Boolean;
  end;

  { TSkImageInfo }

  TSkImageInfo = record
    Width: Integer;
    Height: Integer;
    ColorType: TSkColorType;
    AlphaType: TSkAlphaType;
    ColorSpace: ISkColorSpace;
    constructor Create(const AWidth, AHeight: Integer); overload;
    constructor Create(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil); overload;
    function ByteSize(const ARowBytes: NativeUInt): NativeUInt;
    function BytesPerPixel: Integer;
    function IsEmpty: Boolean;
    function IsOpaque: Boolean;
    function IsValid: Boolean;
    function IsValidRowBytes(const ARowBytes: NativeUInt): Boolean;
    function MakeAlphaType(const AAlphaType: TSkAlphaType): TSkImageInfo;
    function MakeColorSpace(AColorSpace: ISkColorSpace): TSkImageInfo;
    function MakeColorType(const AColorType: TSkColorType): TSkImageInfo;
    function MakeDimensions(const AWidth, AHeight: Integer): TSkImageInfo;
    function MinByteSize: NativeUInt;
    function MinRowBytes: NativeUInt;
    function ShiftPerPixel: Integer;
    class operator Equal(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
    class operator NotEqual(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
  end;

  { TSkFrame }

  TSkFrame = record
    Pixmap: ISkPixmap;
    Duration: Integer;
    constructor Create(const APixmap: ISkPixmap; const ADuration: Integer); overload;
    constructor Create(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ADuration: Integer); overload;
  end;

  { TSkAnimatedWebPEncoder }

  TSkAnimatedWebPEncoder = record
    class function Encode(const ASrc: TArray<TSkFrame>; const AQuality: Integer = 80): TBytes; static;
    class function EncodeToFile(const AFileName: string; const ASrc: TArray<TSkFrame>; const AQuality: Integer = 80): Boolean; static;
    class function EncodeToStream(const AStream: TStream; const ASrc: TArray<TSkFrame>; const AQuality: Integer = 80): Boolean; static;
  end;

  { TSkImageEncoder }

  TSkImageEncoder = record
    class function Encode(const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes; overload; static;
    class function Encode(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes; overload; static;
    class function EncodeToFile(const AFileName: string; const ASrc: ISkPixmap; const AQuality: Integer = 80): Boolean; overload; static;
    class function EncodeToFile(const AFileName: string; const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80): Boolean; overload; static;
    class function EncodeToFile(const AFileName: string; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AQuality: Integer = 80): Boolean; overload; static;
    class function EncodeToFile(const AFileName: string; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80): Boolean; overload; static;
    class function EncodeToStream(const AStream: TStream; const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): Boolean; overload; static;
    class function EncodeToStream(const AStream: TStream; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): Boolean; overload; static;
  end;

  { TSkLattice }

  TSkLattice = record
    XDivs: TArray<Integer>;
    YDivs: TArray<Integer>;
    RectTypes: TArray<TSkLatticeRectType>;
    Colors: TArray<TAlphaColor>;
    UseBounds: Boolean;
    Bounds: TRect;
    constructor Create(const AXDivs, AYDivs: TArray<Integer>; const ARectTypes: TArray<TSkLatticeRectType> = nil; const AColors: TArray<TAlphaColor> = nil); overload;
    constructor Create(const AXDivs, AYDivs: TArray<Integer>; ABounds: TRect; const ARectTypes: TArray<TSkLatticeRectType> = nil; const AColors: TArray<TAlphaColor> = nil); overload;
  end;

  { TSkMetrics }

  TSkMetrics = record
    StartIndex: NativeUInt;
    EndIndex: NativeUInt;
    EndExcludingWhitespaces: NativeUInt;
    EndIncludingNewline: NativeUInt;
    IsHardBreak: Boolean;
    Ascent: Double;
    Descent: Double;
    Height: Double;
    Width: Double;
    Left: Double;
    Baseline: Double;
    LineNumber: NativeUInt;
    class operator Equal(const AMetrics1, AMetrics2: TSkMetrics): Boolean;
    class operator NotEqual(const AMetrics1, AMetrics2: TSkMetrics): Boolean;
  end;

  { TSkParagraphVisitorInfo }

  TSkParagraphVisitorInfo = record
    Font: ISkFont;
    Origin: TPointF;
    AdvanceX: Single;
    Count: Integer;
    Glyphs: PWordArray;
    Positions: PPointFArray;
    Utf8Starts: PCardinalArray;
    Flags: TSkParagraphVisitorFlags;
  end;

  { TSkParticleUniform }

  TSkParticleUniform = record
    Columns: Integer;
    Rows: Integer;
    Slot: Integer;
  end;

  { TSkPathIteratorElem }

  TSkPathIteratorElem = record
    Verb: TSkPathVerb;
    Points: TSkPathPoints;
    ConicWeight: Single;
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
    constructor Create(const ATitle, AAuthor, ASubject, AKeywords, ACreator: string; const AProducer: string = 'Skia/PDF'; const ARasterDPI: Single = 72; const APDFA: Boolean = False; const AEncodingQuality: Integer = 101); overload;
    constructor Create(const ATitle, AAuthor, ASubject, AKeywords, ACreator: string; const ACreation, AModified: TDateTime; const AProducer: string = 'Skia/PDF'; const ARasterDPI: Single = 72; const APDFA: Boolean = False; const AEncodingQuality: Integer = 101); overload;
    class operator Equal(const APDFMetadata1, APDFMetadata2: TSkPDFMetadata): Boolean;
    class operator NotEqual(const APDFMetadata1, APDFMetadata2: TSkPDFMetadata): Boolean;
  end;

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

  { TSkPositionAffinity }

  TSkPositionAffinity = record
    Position: Integer;
    Affinity: TSkAffinity;
    class operator Equal(const APositionAffinity1, APositionAffinity2: TSkPositionAffinity): Boolean;
    class operator NotEqual(const APositionAffinity1, APositionAffinity2: TSkPositionAffinity): Boolean;
  end;

  { TSkRotationScaleMatrix }

  TSkRotationScaleMatrix = record
    SCosinus: Single;
    SSinus: Single;
    TranslateX: Single;
    TranslateY: Single;
    constructor Create(const ASCosinus, ASSinus, ATranslateX, ATranslateY: Single);
    class function CreateDegrees(const AScale, ADegrees, ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix; static;
    class function CreateRadians(const AScale, ARadians, ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix; static;
    class function Identity: TSkRotationScaleMatrix; static; inline;
    class operator Equal(const ARotationScaleMatrix1, ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
    class operator NotEqual(const ARotationScaleMatrix1, ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
    function ToMatrix: TMatrix;
  end;

  { TSkRuntimeEffectFloat2 }

  TSkRuntimeEffectFloat2 = packed record
    constructor Create(const AV1, AV2: Single);
    case Integer of
      0: (V1, V2: Single);
      1: (Values: array[0..1] of Single);
  end;

  { TSkRuntimeEffectFloat3 }

  TSkRuntimeEffectFloat3 = packed record
    constructor Create(const AV1, AV2, AV3: Single);
    case Integer of
      0: (V1, V2, V3: Single);
      1: (Values: array[0..2] of Single);
  end;

  { TSkRuntimeEffectFloat4 }

  TSkRuntimeEffectFloat4 = packed record
    constructor Create(const AV1, AV2, AV3, AV4: Single);
    case Integer of
      0: (V1, V2, V3, V4: Single);
      1: (Values: array[0..3] of Single);
  end;

  { TSkRuntimeEffectFloat2x2 }

  TSkRuntimeEffectFloat2x2 = packed record
    constructor Create(const AV11, AV12, AV21, AV22: Single);
    case Integer of
      0: (V11, V12,
          V21, V22: Single);
      1: (Values: array[0..3] of Single);
  end;

  { TSkRuntimeEffectFloat3x3 }

  TSkRuntimeEffectFloat3x3 = packed record
    constructor Create(const AV11, AV12, AV13, AV21, AV22, AV23, AV31, AV32, AV33: Single);
    case Integer of
      0: (V11, V12, V13,
          V21, V22, V23,
          V31, V32, V33: Single);
      1: (Values: array[0..8] of Single);
  end;

  { TSkRuntimeEffectFloat4x4 }

  TSkRuntimeEffectFloat4x4 = packed record
    constructor Create(const AV11, AV12, AV13, AV14, AV21, AV22, AV23, AV24, AV31, AV32, AV33, AV34, AV41, AV42, AV43, AV44: Single);
    case Integer of
      0: (V11, V12, V13, V14,
          V21, V22, V23, V24,
          V31, V32, V33, V34,
          V41, V42, V43, V44: Single);
      1: (Values: array[0..15] of Single);
  end;

  { TSkRuntimeEffectInt2 }

  TSkRuntimeEffectInt2 = packed record
    constructor Create(const AV1, AV2: Integer);
    case Integer of
      0: (V1, V2: Integer);
      1: (Values: array[0..1] of Integer);
  end;

  { TSkRuntimeEffectInt3 }

  TSkRuntimeEffectInt3 = packed record
    constructor Create(const AV1, AV2, AV3: Integer);
    case Integer of
      0: (V1, V2, V3: Integer);
      1: (Values: array[0..2] of Integer);
  end;

  { TSkRuntimeEffectInt4 }

  TSkRuntimeEffectInt4 = packed record
    constructor Create(const AV1, AV2, AV3, AV4: Integer);
    case Integer of
      0: (V1, V2, V3, V4: Integer);
      1: (Values: array[0..3] of Integer);
  end;

  { TSkSamplingOptions }

  TSkSamplingOptions = record
    MaxAnisotropic: Integer;
    UseCubic: Boolean;
    Cubic: TSkCubicResampler;
    Filter: TSkFilterMode;
    Mipmap: TSkMipmapMode;
    constructor Create(const ACubic: TSkCubicResampler); overload;
    constructor Create(const AFilter: TSkFilterMode; const AMipmap: TSkMipmapMode); overload;
    constructor Create(const AMaxAnisotropic: Integer); overload;
    class function High: TSkSamplingOptions; static; inline;
    class function Low: TSkSamplingOptions; static; inline;
    class function Medium: TSkSamplingOptions; static; inline;
    class operator Equal(const ASamplingOptions1, ASamplingOptions2: TSkSamplingOptions): Boolean;
    class operator NotEqual(const ASamplingOptions1, ASamplingOptions2: TSkSamplingOptions): Boolean;
  end;

  { TSkSurfaceProperties }

  TSkSurfaceProperties = record
    Flags: TSkSurfacePropertiesFlags;
    PixelGeometry: TSkPixelGeometry;
    constructor Create(const AFlags: TSkSurfacePropertiesFlags; const APixelGeometry: TSkPixelGeometry);
    class operator Equal(const ASurfaceProperties1, ASurfaceProperties2: TSkSurfaceProperties): Boolean;
    class operator NotEqual(const ASurfaceProperties1, ASurfaceProperties2: TSkSurfaceProperties): Boolean;
  end;

  { TSkSVGLength }

  TSkSVGLength = record
    Value: Single;
    &Unit: TSkSVGLengthUnit;
    constructor Create(const AValue: Single; const AUnit: TSkSVGLengthUnit = TSkSVGLengthUnit.Number);
    class operator Equal(const ASVGLength1, ASVGLength2: TSkSVGLength): Boolean;
    class operator NotEqual(const ASVGLength1, ASVGLength2: TSkSVGLength): Boolean;
  end;

  { TSkSVGPreserveAspectRatio }

  TSkSVGPreserveAspectRatio = record
    Align: TSkSVGAspectAlign;
    Scale: TSkSVGAspectScale;
    constructor Create(const AAlign: TSkSVGAspectAlign; const AScale: TSkSVGAspectScale);
    class operator Equal(const ASVGPreserveAspectRatio1, ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
    class operator NotEqual(const ASVGPreserveAspectRatio1, ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
  end;

  { TSkTextBox }

  TSkTextBox = record
    Rect: TRectF;
    Direction: TSkTextDirection;
    class operator Equal(const ATextBox1, ATextBox2: TSkTextBox): Boolean;
    class operator NotEqual(const ATextBox1, ATextBox2: TSkTextBox): Boolean;
  end;

  { TSkTextShadow }

  TSkTextShadow = record
    Color: TAlphaColor;
    Offset: TPointF;
    BlurRadius: Double;
    constructor Create(const AColor: TAlphaColor; const AOffset: TPointF; const ABlurRadius: Double);
    class operator Equal(const ATextShadow1, ATextShadow2: TSkTextShadow): Boolean;
    class operator NotEqual(const ATextShadow1, ATextShadow2: TSkTextShadow): Boolean;
  end;

  { TSkUnicodeBreakIteratorElem }

  TSkUnicodeBreakIteratorElem = record
    Position: Integer;
    Status: Integer;
  end;

  TSkParagraphVisitProc = reference to procedure (const ALineNumber: Integer; const AInfo: TSkParagraphVisitorInfo);

  (*$HPPEMIT END '#define __SkCreate(AClass, AIntf, ...) \' *)
  (*$HPPEMIT END '    ({ \' *)
  (*$HPPEMIT END '        ::System::Skia::_di_##AIntf __Result; \' *)
  (*$HPPEMIT END '        (new ::System::Skia::AClass(__VA_ARGS__))->GetInterface(__Result); \' *)
  (*$HPPEMIT END '        __Result; \' *)
  (*$HPPEMIT END '    })' *)

  { ISkObject }

  ISkObject = interface
    function GetHandle: sk_handle_t;
    property Handle: sk_handle_t read GetHandle;
  end;

  { TSkObject }

  TSkObject = class abstract(TObject, ISkObject)
  {$IFNDEF AUTOREFCOUNT}
  strict private const
    objDestroyingFlag = Integer($80000000);
  {$ENDIF}
  strict private
    FHandle: sk_handle_t;
    FOwnsHandle: Boolean;
    {$IFNDEF AUTOREFCOUNT}
    {$IFNDEF FPC}[Volatile]{$ENDIF} FRefCount: Integer;
    {$ENDIF}
    {$IFNDEF AUTOREFCOUNT}
    function GetRefCount: Integer; inline;
    {$ENDIF}
    class constructor Create;
    class destructor Destroy;
  strict protected
    function GetHandle: sk_handle_t;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): {$IFDEF FPC}Longint{$ELSE}HRESULT{$ENDIF}; {$IF DEFINED(FPC) and NOT DEFINED(WINDOWS)}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: {$IFDEF FPC}Longint{$ELSE}Integer{$ENDIF}; {$IF DEFINED(FPC) and NOT DEFINED(WINDOWS)}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: {$IFDEF FPC}Longint{$ELSE}Integer{$ENDIF}; {$IF DEFINED(FPC) and NOT DEFINED(WINDOWS)}cdecl{$ELSE}stdcall{$ENDIF};
    class procedure __DestroyHandle(const AHandle: sk_handle_t); virtual;
    {$IFNDEF AUTOREFCOUNT}
    class procedure __MarkDestroying(const Obj); static;
    {$ENDIF}
    class procedure __RefHandle(const {%H-}AHandle: sk_handle_t); virtual;
    class procedure __UnrefHandle(const {%H-}AHandle: sk_handle_t); virtual;
  public
    constructor Create(const AHandle: sk_handle_t);
    constructor Wrap(const AHandle: sk_handle_t; const AOwnsHandle: Boolean = True);
    destructor Destroy; override;
    {$IFNDEF AUTOREFCOUNT}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    {$ENDIF}
    property Handle: sk_handle_t read FHandle;
    {$IFNDEF AUTOREFCOUNT}
    property RefCount: Integer read GetRefCount;
    class function NewInstance: TObject; override;
    {$ENDIF}
    class function __ReleaseHandle(const AObject: ISkObject): sk_handle_t;
  end;

  { ISkReferenceCounted }

  ISkReferenceCounted = interface(ISkObject)
  end;

  { TSkReferenceCounted }

  TSkReferenceCounted = class abstract(TSkObject, ISkReferenceCounted)
  public
    class procedure __RefHandle(const AHandle: sk_handle_t); override; final;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override; final;
  end;

  { ISkNonVirtualReferenceCounted }

  ISkNonVirtualReferenceCounted = interface(ISkObject)
  end;

  { TSkNonVirtualReferenceCounted }

  TSkNonVirtualReferenceCounted = class abstract(TSkObject, ISkNonVirtualReferenceCounted);

  { ISkEnumerable }

  ISkEnumerable = interface(ISkObject)
    function MoveNext: Boolean;
  end;

  { TSkEnumerable<T> }

  TSkEnumerable<T> = class abstract(TSkObject, ISkEnumerable)
  public type
      TEnumerator = class
      strict private
        FEnumerable: TSkEnumerable<T>;
      public
        constructor Create(const AEnumerable: TSkEnumerable<T>);
        function GetCurrent: T;
        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    function GetCurrent: T; virtual; abstract;
    function GetEnumerator: TEnumerator;
    function MoveNext: Boolean; virtual; abstract;
  end;

  { IGrBackendRenderTarget }

  IGrBackendRenderTarget = interface(ISkObject)
    ['{2E56D489-6BCB-472F-8A69-93B2B18624DB}']
    function GetBackendAPI: TGrBackendAPI;
    function GetHeight: Integer;
    function GetSampleCount: Integer;
    function GetStencilBits: Integer;
    function GetWidth: Integer;
    function IsValid: Boolean;
    property BackendAPI: TGrBackendAPI read GetBackendAPI;
    property Height: Integer read GetHeight;
    property SampleCount: Integer read GetSampleCount;
    property StencilBits: Integer read GetStencilBits;
    property Width: Integer read GetWidth;
  end;

  { TGrBackendRenderTarget }

  TGrBackendRenderTarget = class(TSkObject, IGrBackendRenderTarget)
  strict private
    function GetBackendAPI: TGrBackendAPI;
    function GetHeight: Integer;
    function GetSampleCount: Integer;
    function GetStencilBits: Integer;
    function GetWidth: Integer;
    function IsValid: Boolean;
  public
    constructor CreateGl(const AWidth, AHeight, ASampleCount, AStencilBits: Integer; const AFramebufferInfo: TGrGlFramebufferInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const ATextureInfo: TGrMtlTextureInfo);
    constructor CreateVulkan(const AWidth, AHeight: Integer; const AImageInfo: TGrVkImageInfo);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define GrBackendRenderTarget(...) __SkCreate(TGrBackendRenderTarget, IGrBackendRenderTarget, __VA_ARGS__)'}

  { IGrBackendSemaphore }

  IGrBackendSemaphore = interface(ISkObject)
    ['{BC30D2B6-DB5A-4670-8597-D4883B063925}']
    procedure InitVulkan(const ASemaphore: GrVkSemaphore);
  end;

  { TGrBackendSemaphore }

  TGrBackendSemaphore = class(TSkObject, IGrBackendSemaphore)
  strict private
    procedure InitVulkan(const ASemaphore: GrVkSemaphore);
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define GrBackendRenderTarget(...) __SkCreate(TGrBackendSemaphore, IGrBackendSemaphore, __VA_ARGS__)'}

  { IGrBackendSurfaceMutableState }

  IGrBackendSurfaceMutableState = interface(ISkObject)
    ['{803C5038-6AA7-4F82-9454-D77210345C58}']
  end;

  TGrBackendSurfaceMutableState = class(TSkObject, IGrBackendSurfaceMutableState)
  public
    constructor Create(const AImageLayout: GrVkImageLayout; const AQueueFamilyIndex: Cardinal);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define GrBackendRenderTarget(...) __SkCreate(TGrBackendSurfaceMutableState, IGrBackendSurfaceMutableState, __VA_ARGS__)'}

  { IGrBackendTexture }

  IGrBackendTexture = interface(ISkObject)
    ['{2725242E-7CA7-41A5-A79B-3802E99557C4}']
    function GetBackendAPI: TGrBackendAPI;
    function GetGlTextureInfo(out ATextureInfo: TGrGlTextureInfo): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasMipmaps: Boolean;
    function IsValid: Boolean;
    property BackendAPI: TGrBackendAPI read GetBackendAPI;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TGrBackendTexture }

  TGrBackendTexture = class(TSkObject, IGrBackendTexture)
  strict private
    function GetBackendAPI: TGrBackendAPI;
    function GetGlTextureInfo(out ATextureInfo: TGrGlTextureInfo): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasMipmaps: Boolean;
    function IsValid: Boolean;
  public
    constructor CreateGl(const AWidth, AHeight: Integer; const AIsMipmapped: Boolean; const ATextureInfo: TGrGlTextureInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const AIsMipmapped: Boolean; const ATextureInfo: TGrMtlTextureInfo);
    constructor CreateVulkan(const AWidth, AHeight: Integer; const AImageInfo: TGrVkImageInfo);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define GrBackendTexture(...) __SkCreate(TGrBackendTexture, IGrBackendTexture, __VA_ARGS__)'}

  { IGrDirectContext }

  IGrDirectContext = interface(ISkReferenceCounted)
    ['{8B4304D7-385A-4165-ADE2-F052DAAA70D3}']
    procedure AbandonContext;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AColor: TAlphaColor; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AColor: TAlphaColorF; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    procedure DeleteTexture(const ATexture: IGrBackendTexture);
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCpu: Boolean = False);
    procedure FreeGpuResources;
    function GetBackendAPI: TGrBackendAPI;
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSkColorType): Integer;
    function GetResourceCacheLimit: NativeUInt;
    procedure GetResourceCacheUsage(out AResources: Integer; out AResourcesBytes: NativeUInt);
    function IsAbandoned: Boolean;
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const AScratchResourcesOnly: Boolean); overload;
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
    function Submit(const ASyncCpu: Boolean = False): Boolean;
    property BackendAPI: TGrBackendAPI read GetBackendAPI;
    property ResourceCacheLimit: NativeUInt read GetResourceCacheLimit write SetResourceCacheLimit;
  end;

  { TGrDirectContext }

  TGrDirectContext = class(TSkReferenceCounted, IGrDirectContext)
  private
    class function gr_gl_get_proc(context: Pointer; const name: MarshaledAString): Pointer; cdecl; static;
    class function gr_vk_get_proc(context: Pointer; const name: MarshaledAString; instance: gr_vk_instance_t; device: gr_vk_device_t): Pointer; cdecl; static;
  strict private
    function GetBackendAPI: TGrBackendAPI;
    function GetResourceCacheLimit: NativeUInt;
    procedure SetResourceCacheLimit(const AValue: NativeUInt);
  public
    procedure AbandonContext;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AColor: TAlphaColor; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    function CreateTexture(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const AColor: TAlphaColorF; const AIsMipmapped, AIsRenderable: Boolean; const AIsProtected: Boolean = False): IGrBackendTexture; overload;
    procedure DeleteTexture(const ATexture: IGrBackendTexture);
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCpu: Boolean = False);
    procedure FreeGpuResources;
    function GetMaxSurfaceSampleCountForColorType(const AColorType: TSkColorType): Integer;
    procedure GetResourceCacheUsage(out AResources: Integer; out AResourcesBytes: NativeUInt);
    function IsAbandoned: Boolean;
    procedure PerformDeferredCleanup(const AMilliseconds: Int64);
    procedure PurgeUnlockedResources(const AScratchResourcesOnly: Boolean); overload;
    procedure PurgeUnlockedResources(const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean); overload;
    procedure ReleaseResourcesAndAbandonContext;
    procedure ResetContext;
    function Submit(const ASyncCpu: Boolean = False): Boolean;
    property BackendAPI: TGrBackendAPI read GetBackendAPI;
    property ResourceCacheLimit: NativeUInt read GetResourceCacheLimit write SetResourceCacheLimit;
    class function MakeGl(AInterface: IGrGlInterface = nil): IGrDirectContext; overload; static;
    class function MakeGl(const AOptions: TGrContextOptions; const AInterface: IGrGlInterface = nil): IGrDirectContext; overload; static;
    class function MakeMetal(const ABackendContext: TGrMtlBackendContext): IGrDirectContext; overload; static;
    class function MakeMetal(const ABackendContext: TGrMtlBackendContext; const AOptions: TGrContextOptions): IGrDirectContext; overload; static;
    class function MakeVulkan(const ABackendContext: TGrVkBackendContext): IGrDirectContext; overload; static;
    class function MakeVulkan(const ABackendContext: TGrVkBackendContext; const AOptions: TGrContextOptions): IGrDirectContext; overload; static;
  end;

  { IGrGlInterface }

  IGrGlInterface = interface(ISkReferenceCounted)
    ['{77860D76-5A77-45FB-9F62-03C1DD694489}']
    function HasExtension(const AName: MarshaledAString): Boolean;
    function Validate: Boolean;
  end;

  { TGrGlInterface }

  TGrGlInterface = class(TSkReferenceCounted, IGrGlInterface)
  public
    function HasExtension(const AName: MarshaledAString): Boolean;
    function Validate: Boolean;
    class function MakeAssembled(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledGl(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledGles(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledWebGl(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeNative: IGrGlInterface; static;
  end;

  { IGrPersistentCache }

  IGrPersistentCache = interface(ISkObject)
    ['{AF624E98-7525-44E4-90EB-D6D21826011A}']
  end;

  { TGrPersistentCacheBaseClass }

  TGrPersistentCacheBaseClass = class abstract(TSkObject, IGrPersistentCache)
  strict private
    class constructor Create;
    class function load_proc(context: Pointer; const key_data: Pointer; key_size: size_t): sk_data_t; cdecl; static;
    class procedure store_proc(context: Pointer; const key_data: Pointer; key_size: size_t; const data: Pointer; size: size_t); cdecl; static;
  strict protected
    function Load(const AKey: TBytes): TBytes; virtual; abstract;
    procedure Store(const AKey, AData: TBytes); virtual; abstract;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { IGrShaderErrorHandler }

  IGrShaderErrorHandler = interface(ISkObject)
    ['{7CCA8C48-CB40-4FBD-B70D-986BC2FC1D58}']
  end;

  { TGrShaderErrorHandlerBaseClass }

  TGrShaderErrorHandlerBaseClass = class abstract(TSkObject, IGrShaderErrorHandler)
  strict private
    class constructor Create;
    class procedure compile_error_proc(context: Pointer; const shader, errors: MarshaledAString); cdecl; static;
  strict protected
    procedure CompileError(const AShader, AErrors: string); virtual; abstract;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { IGrVkExtensions }

  IGrVkExtensions = interface(ISkObject)
    ['{A3F62993-F17A-4D8D-BA85-725AFE2ACDF4}']
    function HasExtension(const AName: MarshaledAString; const AMinApiVersion: Cardinal): Boolean;
    procedure Init(const AProc: TGrVkGetProc; const AInstance: GrVkInstance; const APhysicalDevice: GrVkPhysicalDevice; const AInstanceExtensions, ADeviceExtensions: TArray<MarshaledAString>);
  end;

  { TGrVkExtensions }

  TGrVkExtensions = class(TSkObject, IGrVkExtensions)
  strict private
    function HasExtension(const AName: MarshaledAString; const AMinApiVersion: Cardinal): Boolean;
    procedure Init(const AProc: TGrVkGetProc; const AInstance: GrVkInstance; const APhysicalDevice: GrVkPhysicalDevice; const AInstanceExtensions, ADeviceExtensions: TArray<MarshaledAString>);
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define GrVkExtensions(...) __SkCreate(TGrVkExtensions, IGrVkExtensions, __VA_ARGS__)'}

  { ISkFlattenable }

  ISkFlattenable = interface(ISkReferenceCounted)
    ['{5CE62F93-1D06-4880-96D2-D9BAEF4C13D3}']
  end;

  { ISkBlender }

  ISkBlender = interface(ISkFlattenable)
    ['{78342141-22D6-4D38-A87D-38D70DE65A8C}']
  end;

  { TSkBlender }

  TSkBlender = class(TSkReferenceCounted, ISkBlender)
  public
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean): ISkBlender; static;
    class function MakeMode(const AMode: TSkBlendMode): ISkBlender; static;
  end;

  { ISkCanvas }

  ISkCanvas = interface(ISkObject)
    ['{FC12079A-38A3-4078-88D7-30F550F1B49E}']
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISkPath; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISkRoundRect; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISkShader; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue; const ASize: NativeUInt); overload;
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ASampling: TSkSamplingOptions; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ASampling: TSkSamplingOptions; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AOrigin: TPointF; const AFont: ISkFont; const APaint: ISkPaint); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const AMatrices: TArray<TSkRotationScaleMatrix>; const AOrigin: TPointF; const AFont: ISkFont; const APaint: ISkPaint); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const APaint: ISkPaint = nil); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil); overload;
    procedure DrawImageLattice(const AImage: ISkImage; const ALattice: TSkLattice; const ADest: TRectF; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear; const APaint: ISkPaint = nil);
    procedure DrawImageNine(const AImage: ISkImage; const ACenter: TRect; const ADest: TRectF; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear; const APaint: ISkPaint = nil);
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISkPaint); overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISkPaint); overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISkPaint);
    procedure DrawPaint(const APaint: ISkPaint);
    procedure DrawPatch(const ACubics: TSkPatchCubics; const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords; const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
    procedure DrawPath(const APath: ISkPath; const APaint: ISkPaint);
    procedure DrawPicture(const APicture: ISkPicture; const APaint: ISkPaint = nil); overload;
    procedure DrawPicture(const APicture: ISkPicture; const AMatrix: TMatrix; const APaint: ISkPaint = nil); overload;
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
    procedure DrawVertices(const AVertices: ISkVertices; const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
    function GetBaseProperties: TSkSurfaceProperties;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function GetTopProperties: TSkSurfaceProperties;
    function MakeSurface(const AImageInfo: TSkImageInfo): ISkSurface; overload;
    function MakeSurface(const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties): ISkSurface; overload;
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure RotateRadians(const ARadians: Single);
    function Save: Integer;
    function SaveLayer(const APaint: ISkPaint = nil; const ABackdrop: ISkImageFilter = nil; const AFlags: TSkSaveLayerFlags = []): Integer; overload;
    function SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil; const ABackdrop: ISkImageFilter = nil; const AFlags: TSkSaveLayerFlags = []): Integer; overload;
    function SaveLayerAlpha(const AAlpha: Byte): Integer; overload;
    function SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte): Integer; overload;
    procedure Scale(const AScaleX, AScaleY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const ASkewX, ASkewY: Single);
    procedure Translate(const ADeltaX, ADeltaY: Single);
    property BaseProperties: TSkSurfaceProperties read GetBaseProperties;
    property TopProperties: TSkSurfaceProperties read GetTopProperties;
  end;

  { TSkCanvas }

  TSkCanvas = class(TSkObject, ISkCanvas)
  strict private
    procedure Clear(const AColor: TAlphaColor); overload;
    procedure Clear(const AColor: TAlphaColorF); overload;
    procedure Discard;
    procedure ClipPath(const APath: ISkPath; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipRect(const ARect: TRectF; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure ClipRoundRect(const ARoundRect: ISkRoundRect; const AOp: TSkClipOp = TSkClipOp.Intersect; const AAntiAlias: Boolean = False);
    procedure ClipShader(const AShader: ISkShader; const AOp: TSkClipOp = TSkClipOp.Intersect);
    procedure Concat(const AMatrix: TMatrix); overload;
    procedure Concat(const AMatrix: TMatrix3D); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string); overload;
    procedure DrawAnnotation(const ARect: TRectF; const AKey: string; const AValue; const ASize: NativeUInt); overload;
    procedure DrawArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ASampling: TSkSamplingOptions; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawAtlas(const AAtlas: ISkImage; const ATansforms: TArray<TSkRotationScaleMatrix>; const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode; const ASampling: TSkSamplingOptions; const ACullRect: TRectF; const AColors: TArray<TAlphaColor> = nil; const APaint: ISkPaint = nil); overload;
    procedure DrawCircle(const ACenter: TPointF; ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawCircle(const ACenterX, ACenterY, ARadius: Single; const APaint: ISkPaint); overload;
    procedure DrawColor(const AColor: TAlphaColor; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawColor(const AColor: TAlphaColorF; const ABlendMode: TSkBlendMode = TSkBlendMode.SrcOver); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AOrigin: TPointF; const AFont: ISkFont; const APaint: ISkPaint); overload;
    procedure DrawGlyphs(const AGlyphs: TArray<Word>; const AMatrices: TArray<TSkRotationScaleMatrix>; const AOrigin: TPointF; const AFont: ISkFont; const APaint: ISkPaint); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const APaint: ISkPaint = nil); overload;
    procedure DrawImage(const AImage: ISkImage; const AX, AY: Single; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil); overload;
    procedure DrawImageLattice(const AImage: ISkImage; const ALattice: TSkLattice; const ADest: TRectF; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear; const APaint: ISkPaint = nil);
    procedure DrawImageNine(const AImage: ISkImage; const ACenter: TRect; const ADest: TRectF; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear; const APaint: ISkPaint = nil);
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISkPaint); overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISkPaint); overload;
    procedure DrawOval(const AOval: TRectF; const APaint: ISkPaint);
    procedure DrawPaint(const APaint: ISkPaint);
    procedure DrawPatch(const ACubics: TSkPatchCubics; const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords; const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
    procedure DrawPath(const APath: ISkPath; const APaint: ISkPaint);
    procedure DrawPicture(const APicture: ISkPicture; const APaint: ISkPaint = nil); overload;
    procedure DrawPicture(const APicture: ISkPicture; const AMatrix: TMatrix; const APaint: ISkPaint = nil); overload;
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
    procedure DrawVertices(const AVertices: ISkVertices; const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
    function GetBaseProperties: TSkSurfaceProperties;
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function GetTopProperties: TSkSurfaceProperties;
    function MakeSurface(const AImageInfo: TSkImageInfo): ISkSurface; overload;
    function MakeSurface(const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties): ISkSurface; overload;
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure RotateRadians(const ARadians: Single);
    function Save: Integer;
    function SaveLayer(const APaint: ISkPaint = nil; const ABackdrop: ISkImageFilter = nil; const AFlags: TSkSaveLayerFlags = []): Integer; overload;
    function SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil; const ABackdrop: ISkImageFilter = nil; const AFlags: TSkSaveLayerFlags = []): Integer; overload;
    function SaveLayerAlpha(const AAlpha: Byte): Integer; overload;
    function SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte): Integer; overload;
    procedure Scale(const AScaleX, AScaleY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const ASkewX, ASkewY: Single);
    procedure Translate(const ADeltaX, ADeltaY: Single);
  public
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkCodec }

  ISkCodec = interface(ISkObject)
    ['{7FE13F05-959F-462B-8001-0DE5A0878CD8}']
    function GetDimensions: TSize;
    function GetEncodedImageFormat: TSkEncodedImageFormat;
    function GetHeight: Integer;
    function GetImage(const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkImage;
    function GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt; const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): Boolean;
    function GetWidth: Integer;
    property Dimensions: TSize read GetDimensions;
    property EncodedImageFormat: TSkEncodedImageFormat read GetEncodedImageFormat;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TSkCodec }

  TSkCodec = class(TSkObject, ISkCodec)
  strict private
    function GetDimensions: TSize;
    function GetEncodedImageFormat: TSkEncodedImageFormat;
    function GetHeight: Integer;
    function GetImage(const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkImage;
    function GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt; const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): Boolean;
    function GetWidth: Integer;
  public
    class function MakeFromFile(const AFileName: string): ISkCodec; static;
    class function MakeFromStream(const AStream: TStream): ISkCodec; static;
    class function MakeWithCopy(const AData: Pointer; const ASize: NativeUInt): ISkCodec; static;
    class function MakeWithoutCopy(const AData: Pointer; const ASize: NativeUInt): ISkCodec; static;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkAnimationCodecPlayer }

  ISkAnimationCodecPlayer = interface(ISkObject)
    ['{F900E95B-3B21-4CA1-B2BC-B6D060A2773D}']
    function GetDimensions: TSize;
    function GetDuration: Cardinal;
    function GetFrame: ISkImage;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function Seek(const AMilliseconds: Cardinal): Boolean;
    property Dimensions: TSize read GetDimensions;
    property Duration: Cardinal read GetDuration;
    property Frame: ISkImage read GetFrame;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TSkAnimationCodecPlayer }

  TSkAnimationCodecPlayer = class(TSkObject, ISkAnimationCodecPlayer)
  strict private
    function GetDimensions: TSize;
    function GetDuration: Cardinal;
    function GetFrame: ISkImage;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function Seek(const AMilliseconds: Cardinal): Boolean;
  public
    class function MakeFromFile(const AFileName: string): ISkAnimationCodecPlayer; static;
    class function MakeFromStream(const AStream: TStream): ISkAnimationCodecPlayer; static;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkColorFilter }

  ISkColorFilter = interface(ISkFlattenable)
    ['{A7F1A527-A796-4387-8957-5D5C2DAB0924}']
  end;

  { TSkColorFilter }

  TSkColorFilter = class(TSkReferenceCounted, ISkColorFilter)
  public
    class function MakeBlend(const AColor: TAlphaColor; const AMode: TSkBlendMode): ISkColorFilter; overload; static;
    class function MakeBlend(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace; const AMode: TSkBlendMode): ISkColorFilter; overload; static;
    class function MakeCompose(const AOuter, AInner: ISkColorFilter): ISkColorFilter; static;
    class function MakeHighContrast(const AConfig: TSkHighContrastConfig): ISkColorFilter; static;
    class function MakeHSLAMatrix(const AMatrix: TSkColorMatrix): ISkColorFilter; static;
    class function MakeLighting(const AMultiply, AAdd: TAlphaColor): ISkColorFilter; static;
    class function MakeLinearToSRGBGamma: ISkColorFilter;
    class function MakeLumaColor: ISkColorFilter; static;
    class function MakeMatrix(const AMatrix: TSkColorMatrix): ISkColorFilter; static;
    class function MakeOverdraw(const AColors: TSkOverdrawColor): ISkColorFilter; static;
    class function MakeTable(const ATable: TSkTableFilter): ISkColorFilter; overload; static;
    class function MakeTable(const ATableA, ATableR, ATableG, ATableB: TSkTableFilter): ISkColorFilter; overload; static;
  end;

  { ISkColorSpace }

  ISkColorSpace = interface(ISkNonVirtualReferenceCounted)
    ['{0581D967-BDFC-4F6D-A821-D0BFA2DCCBAC}']
    function GammaCloseToSRGB: Boolean;
    function GammaIsLinear: Boolean;
    function IsEqual(const AColorSpace: ISkColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeLinearGamma: ISkColorSpace;
    function MakeSRGBGamma: ISkColorSpace;
    function ToProfile: ISkColorSpaceICCProfile;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
  end;

  { TSkColorSpace }

  TSkColorSpace = class(TSkNonVirtualReferenceCounted, ISkColorSpace)
  public
    function GammaCloseToSRGB: Boolean;
    function GammaIsLinear: Boolean;
    function IsEqual(const AColorSpace: ISkColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeLinearGamma: ISkColorSpace;
    function MakeSRGBGamma: ISkColorSpace;
    function ToProfile: ISkColorSpaceICCProfile;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
    class function Make(const AProfile: ISkColorSpaceICCProfile): ISkColorSpace; static;
    class function MakeRGB(const ATransferFunction: TSkColorSpaceTransferFunction; const AToXyzD50: TSkColorSpaceXyz): ISkColorSpace; static;
    class function MakeSRGB: ISkColorSpace; static;
    class function MakeSRGBLinear: ISkColorSpace; static;
    class procedure __RefHandle(const AHandle: sk_handle_t); override;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkColorSpaceICCProfile }

  ISkColorSpaceICCProfile = interface(ISkObject)
    ['{F5645A9F-F9AE-4B59-BA03-931E56B32422}']
    function ToBytes: TBytes;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
  end;

  { TSkColorSpaceICCProfile }

  TSkColorSpaceICCProfile = class(TSkObject, ISkColorSpaceICCProfile)
  strict private
    function ToBytes: TBytes;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
  public
    class function MakeFromBytes(const ABytes: TBytes): ISkColorSpaceICCProfile; static;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkDocument }

  ISkDocument = interface(ISkReferenceCounted)
    ['{E41A23F2-F67A-487A-9830-21908617EC48}']
    function BeginPage(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISkCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
  end;

  { TSkDocument }

  TSkDocument = class(TSkReferenceCounted, ISkDocument)
  strict private
    FHolder: ISkObject;
  public
    function BeginPage(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginPage(const AWidth, AHeight: Single; const AContent: TRectF): ISkCanvas; overload;
    procedure Close;
    procedure EndPage;
    procedure Terminate;
    class function MakePDF(const AStream: TStream): ISkDocument; overload;
    class function MakePDF(const AStream: TStream; const AMetadata: TSkPDFMetadata): ISkDocument; overload;
    class function MakeXPS(const AStream: TStream; const ADPI: Single = 72): ISkDocument;
  end;

  { ISkFont }

  ISkFont = interface(ISkObject)
    ['{034205CA-029A-428B-9037-45CBF160C3F1}']
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<TRectF>;
    function GetEdging: TSkFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>;
    function GetHinting: TSkFontHinting;
    function GetHorizontalPositions(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetIntercepts(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSkFontMetrics): Single;
    function GetPath(const AGlyph: Word): ISkPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSkFontPathProc);
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
    procedure SetTypeface(AValue: ISkTypeface);
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
    function GetBaselineSnap: Boolean;
    function GetBounds(const AGlyphs: TArray<Word>; const APaint: ISkPaint = nil): TArray<TRectF>;
    function GetEdging: TSkFontEdging;
    function GetEmbeddedBitmaps: Boolean;
    function GetEmbolden: Boolean;
    function GetForceAutoHinting: Boolean;
    function GetGlyphs(const AText: string): TArray<Word>;
    function GetHinting: TSkFontHinting;
    function GetHorizontalPositions(const AGlyphs: TArray<Word>; const AOrigin: Single = 0): TArray<Single>;
    function GetIntercepts(const AGlyphs: TArray<Word>; const APositions: TArray<TPointF>; const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
    function GetLinearMetrics: Boolean;
    function GetMetrics(out AMetrics: TSkFontMetrics): Single;
    function GetPath(const AGlyph: Word): ISkPath;
    procedure GetPaths(const AGlyphs: TArray<Word>; const AProc: TSkFontPathProc);
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
    procedure SetTypeface(AValue: ISkTypeface);
    function UnicharToGlyph(const AUnichar: Integer): Word;
    function UnicharsToGlyphs(const AUnichars: TArray<Integer>): TArray<Word>;
    class procedure path_proc(const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl; static;
  public
    constructor Create(ATypeface: ISkTypeface = nil; const ASize: Single = 12; const AScaleX: Single = 1; const ASkewX: Single = 0); overload;
    constructor Create(const AFont: ISkFont); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkFont(...) __SkCreate(TSkFont, ISkFont, __VA_ARGS__)'}

  { ISkImage }

  ISkImage = interface(ISkReferenceCounted)
    ['{E10DF07F-583F-4700-B14F-4DA7433FFB0F}']
    function Encode(const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes;
    function EncodeToFile(const AFileName: string; const AQuality: Integer = 80): Boolean; overload;
    function EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80): Boolean; overload;
    function EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): Boolean;
    function GetAlphaType: TSkAlphaType;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetUniqueId: NativeUInt;
    function GetWidth: Integer;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(AContext: IGrDirectContext = nil): Boolean;
    function MakeNonTextureImage: ISkImage;
    function MakeRasterImage: ISkImage;
    function MakeRawShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeSubset(const ASubset: TRect; AContext: IGrDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGrDirectContext; const AIsMipmapped: Boolean = False): ISkImage;
    function MakeWithFilter(const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint; AContext: IGrDirectContext = nil): ISkImage;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    property AlphaType: TSkAlphaType read GetAlphaType;
    property ColorSpace: ISkColorSpace read GetColorSpace;
    property ColorType: TSkColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSkImageInfo read GetImageInfo;
    property UniqueId: NativeUInt read GetUniqueId;
    property Width: Integer read GetWidth;
  end;

  { TSkImage }

  TSkImage = class(TSkReferenceCounted, ISkImage)
  strict private
    function GetAlphaType: TSkAlphaType;
    function GetColorSpace: ISkColorSpace;
    function GetColorType: TSkColorType;
    function GetHeight: Integer;
    function GetImageInfo: TSkImageInfo;
    function GetUniqueId: NativeUInt;
    function GetWidth: Integer;
    class procedure raster_release_proc(const pixels: Pointer; context: Pointer); cdecl; static;
    class procedure texture_release_proc(context: Pointer); cdecl; static;
  public
    function Encode(const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes;
    function EncodeToFile(const AFileName: string; const AQuality: Integer = 80): Boolean; overload;
    function EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80): Boolean; overload;
    function EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): Boolean;
    function IsAlphaOnly: Boolean;
    function IsLazyGenerated: Boolean;
    function IsOpaque: Boolean;
    function IsTextureBacked: Boolean;
    function IsValid(AContext: IGrDirectContext = nil): Boolean;
    function MakeNonTextureImage: ISkImage;
    function MakeRasterImage: ISkImage;
    function MakeRawShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeRawShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeSubset(const ASubset: TRect; AContext: IGrDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGrDirectContext; const AIsMipmapped: Boolean = False): ISkImage;
    function MakeWithFilter(const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint; AContext: IGrDirectContext = nil): ISkImage;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    property AlphaType: TSkAlphaType read GetAlphaType;
    property ColorSpace: ISkColorSpace read GetColorSpace;
    property ColorType: TSkColorType read GetColorType;
    property Height: Integer read GetHeight;
    property ImageInfo: TSkImageInfo read GetImageInfo;
    property UniqueId: NativeUInt read GetUniqueId;
    property Width: Integer read GetWidth;
    class function MakeCrossContext(const AContext: IGrDirectContext; const APixmap: ISkPixmap; const ABuildMips: Boolean; const ALimitToMaxTextureSize: Boolean = False): ISkImage; overload; static;
    class function MakeCrossContext(const AContext: IGrDirectContext; const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ABuildMips: Boolean; const ALimitToMaxTextureSize: Boolean = False): ISkImage; overload; static;
    class function MakeFromAdoptedTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkImage; static;
    class function MakeFromEncoded(const ABytes: TBytes): ISkImage; overload; static;
    class function MakeFromEncodedFile(const AFileName: string): ISkImage; overload; static;
    class function MakeFromEncodedStream(const AStream: TStream): ISkImage; overload; static;
    class function MakeFromPicture(const APicture: ISkPicture; const ADimensions: TSize; AColorSpace: ISkColorSpace; const APaint: ISkPaint = nil): ISkImage; overload; static;
    class function MakeFromPicture(const APicture: ISkPicture; const ADimensions: TSize; AColorSpace: ISkColorSpace; const AMatrix: TMatrix; const APaint: ISkPaint = nil): ISkImage; overload; static;
    class function MakeFromPicture(const APicture: ISkPicture; const ADimensions: TSize; AColorSpace: ISkColorSpace; const AProperties: TSkSurfaceProperties; const APaint: ISkPaint = nil): ISkImage; overload; static;
    class function MakeFromPicture(const APicture: ISkPicture; const ADimensions: TSize; AColorSpace: ISkColorSpace; const AMatrix: TMatrix; const AProperties: TSkSurfaceProperties; const APaint: ISkPaint = nil): ISkImage; overload; static;
    class function MakeFromRaster(const APixmap: ISkPixmap; const ARasterReleaseProc: TSkImageRasterReleaseProc = nil): ISkImage; overload; static;
    class function MakeFromRaster(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSkImageRasterReleaseProc = nil): ISkImage; overload; static;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil; const ATextureReleaseProc: TSkImageTextureReleaseProc = nil): ISkImage; static;
    class function MakeRasterCopy(const APixmap: ISkPixmap): ISkImage; overload; static;
    class function MakeRasterCopy(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage; overload; static;
  end;

  { ISkImageFilter }

  ISkImageFilter = interface(ISkReferenceCounted)
    ['{C3156B37-98A5-489F-BFB7-C0EB5DDDEDB6}']
    function CanComputeFastBounds: Boolean;
    function ComputeFastBounds(const ABounds: TRectF): TRectF;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkImageFilter;
  end;

  { TSkImageFilter }

  TSkImageFilter = class(TSkReferenceCounted, ISkImageFilter)
  public
    function CanComputeFastBounds: Boolean;
    function ComputeFastBounds(const ABounds: TRectF): TRectF;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkImageFilter;
    class function MakeAlphaThreshold(const ARegion: TRect; const AInnerMin, AOuterMax: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeAlphaThreshold(const ARegion: ISkRegion; const AInnerMin, AOuterMax: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter; const ACropRect: TRectF; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlend(const AMode: TSkBlendMode; const ABackground: ISkImageFilter; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlend(const AMode: TSkBlendMode; const ABackground: ISkImageFilter; const ACropRect: TRectF; AForeground: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; AInput: ISkImageFilter = nil; const ATileMode: TSkTileMode = TSkTileMode.Decal): ISkImageFilter; overload; static;
    class function MakeBlur(const ASigmaX, ASigmaY: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil; const ATileMode: TSkTileMode = TSkTileMode.Decal): ISkImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISkColorFilter; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeColorFilter(const AColorFilter: ISkColorFilter; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeCompose(const AOuter, AInner: ISkImageFilter): ISkImageFilter; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDilate(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSkColorChannel; const AScale: Single; const ADisplacement: ISkImageFilter; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDisplacementMap(const AXChannelSelector, AYChannelSelector: TSkColorChannel; const AScale: Single; const ADisplacement: ISkImageFilter; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitDiffuse(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDistantLitSpecular(const ADirection: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADeltaX, ADeltaY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeDropShadowOnly(const ADeltaX, ADeltaY, ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeErode(const ARadiusX, ARadiusY: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage; const ASampling: TSkSamplingOptions): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage; const ASrc, ADest: TRectF): ISkImageFilter; overload; static;
    class function MakeImage(const AImage: ISkImage; const ASrc, ADest: TRectF; const ASampling: TSkSamplingOptions): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixTransform(const AMatrix: TMatrix; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixTransform(const AMatrix: TMatrix; const ASampling: TSkSamplingOptions; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISkImageFilter>): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilters: TArray<ISkImageFilter>; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeOffset(const ADeltaX, ADeltaY: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeOffset(const ADeltaX, ADeltaY: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePicture(const APicture: ISkPicture): ISkImageFilter; overload; static;
    class function MakePicture(const APicture: ISkPicture; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakePointLitDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitDiffuse(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakePointLitSpecular(const ALocation: TPoint3D; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeRuntimeShader(const AEffectBuilder: ISkRuntimeShaderBuilder; const AChild: string = ''; const AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeRuntimeShader(const AEffectBuilder: ISkRuntimeShaderBuilder; const AChildren: TArray<string>; const AInputs: TArray<ISkImageFilter>): ISkImageFilter; overload; static;
    class function MakeShader(const AShader: ISkShader; const ADither: Boolean): ISkImageFilter; overload; static;
    class function MakeShader(const AShader: ISkShader; const ADither: Boolean; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeTile(const ASrc, ADest: TRect; AInput: ISkImageFilter = nil): ISkImageFilter; static;
  end;

  { ISkMaskFilter }

  ISkMaskFilter = interface(ISkReferenceCounted)
    ['{BBA0BC39-23F1-43B2-A924-526E7D089CAF}']
  end;

  { TSkMaskFilter }

  TSkMaskFilter = class(TSkReferenceCounted, ISkMaskFilter)
  public
    class function MakeBlur(const AStyle: TSkBlurStyle; const ASigma: Single; const ARespectCTM: Boolean = True): ISkMaskFilter; static;
    class function MakeShader(const AShader: ISkShader): ISkMaskFilter; static;
    class function MakeTable(const ATable: TSkTableFilter): ISkMaskFilter; static;
    class function MakeTableClip(const AMin, AMax: Byte): ISkMaskFilter; static;
    class function MakeTableGamma(const AGamma: Single): ISkMaskFilter; static;
  end;

  { ISkPaint }

  ISkPaint = interface(ISkObject)
    ['{C95825F8-0D51-4BCE-8945-84AFE6264213}']
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlender: ISkBlender;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISkColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISkPath): ISkPath; overload;
    function GetFillPath(const APath: ISkPath; const ACullRect: TRectF; const AResScale: Single = 1): ISkPath; overload;
    function GetImageFilter: ISkImageFilter;
    function GetMaskFilter: ISkMaskFilter;
    function GetPathEffect: ISkPathEffect;
    function GetShader: ISkShader;
    function GetStrokeCap: TSkStrokeCap;
    function GetStrokeJoin: TSkStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSkPaintStyle;
    procedure Reset;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlender(AValue: ISkBlender);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF; AColorSpace: ISkColorSpace = nil);
    procedure SetColorFilter(AValue: ISkColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetImageFilter(AValue: ISkImageFilter);
    procedure SetMaskFilter(AValue: ISkMaskFilter);
    procedure SetPathEffect(AValue: ISkPathEffect);
    procedure SetShader(AValue: ISkShader);
    procedure SetStrokeCap(const AValue: TSkStrokeCap);
    procedure SetStrokeJoin(const AValue: TSkStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSkPaintStyle);
    property Alpha: Byte read GetAlpha write SetAlpha;
    property AlphaF: Single read GetAlphaF write SetAlphaF;
    property AntiAlias: Boolean read GetAntiAlias write SetAntiAlias;
    property Blender: ISkBlender read GetBlender write SetBlender;
    property Color: TAlphaColor read GetColor write SetColor;
    property ColorFilter: ISkColorFilter read GetColorFilter write SetColorFilter;
    property Dither: Boolean read GetDither write SetDither;
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
  strict private
    function GetAlpha: Byte;
    function GetAlphaF: Single;
    function GetAntiAlias: Boolean;
    function GetBlender: ISkBlender;
    function GetColor: TAlphaColor;
    function GetColorF: TAlphaColorF;
    function GetColorFilter: ISkColorFilter;
    function GetDither: Boolean;
    function GetFillPath(const APath: ISkPath): ISkPath; overload;
    function GetFillPath(const APath: ISkPath; const ACullRect: TRectF; const AResScale: Single = 1): ISkPath; overload;
    function GetImageFilter: ISkImageFilter;
    function GetMaskFilter: ISkMaskFilter;
    function GetPathEffect: ISkPathEffect;
    function GetShader: ISkShader;
    function GetStrokeCap: TSkStrokeCap;
    function GetStrokeJoin: TSkStrokeJoin;
    function GetStrokeMiter: Single;
    function GetStrokeWidth: Single;
    function GetStyle: TSkPaintStyle;
    procedure Reset;
    procedure SetAlpha(const AValue: Byte);
    procedure SetAlphaF(const AValue: Single);
    procedure SetAntiAlias(const AValue: Boolean);
    procedure SetARGB(const A, R, G, B: Byte);
    procedure SetBlender(AValue: ISkBlender);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetColorF(const AValue: TAlphaColorF; AColorSpace: ISkColorSpace = nil);
    procedure SetColorFilter(AValue: ISkColorFilter);
    procedure SetDither(const AValue: Boolean);
    procedure SetImageFilter(AValue: ISkImageFilter);
    procedure SetMaskFilter(AValue: ISkMaskFilter);
    procedure SetPathEffect(AValue: ISkPathEffect);
    procedure SetShader(AValue: ISkShader);
    procedure SetStrokeCap(const AValue: TSkStrokeCap);
    procedure SetStrokeJoin(const AValue: TSkStrokeJoin);
    procedure SetStrokeMiter(const AValue: Single);
    procedure SetStrokeWidth(const AValue: Single);
    procedure SetStyle(const AValue: TSkPaintStyle);
  public
    constructor Create; overload;
    constructor Create(const APaint: ISkPaint); overload;
    constructor Create(const AStyle: TSkPaintStyle); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPaint(...) __SkCreate(TSkPaint, ISkPaint, __VA_ARGS__)'}

  { ISkOpBuilder }

  ISkOpBuilder = interface(ISkObject)
    ['{A0F0AB29-477D-4AA2-A4AD-4AEBC3C6D630}']
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
  end;

  { TSkOpBuilder }

  TSkOpBuilder = class(TSkObject, ISkOpBuilder)
  strict private
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkOpBuilder(...) __SkCreate(TSkOpBuilder, ISkOpBuilder, __VA_ARGS__)'}


  TSkPathIteratorElemEnumerator = TSkEnumerable<TSkPathIteratorElem>.TEnumerator;

  { ISkPathIterator }

  ISkPathIterator = interface(ISkEnumerable)
    ['{AD3EC8F2-01C0-4685-B098-6850AEA6D8A6}']
    function GetCurrent: TSkPathIteratorElem;
    function GetEnumerator: TSkPathIteratorElemEnumerator;
  end;

  { ISkPath }

  ISkPath = interface(ISkObject)
    ['{0801E1D4-5426-4B7E-A3CD-9B40B21C26F0}']
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
    function IsLine(const APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISkRoundRect): Boolean; overload;
    function Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
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
    public
      constructor Create(const APath: ISkPath; const AForceClose: Boolean);
      function GetCurrent: TSkPathIteratorElem; override;
      function MoveNext: Boolean; override;
      class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
    end;

  strict private
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
    function IsLine(const APoint1, APoint2: TPointF): Boolean; overload;
    function IsOval: Boolean; overload;
    function IsOval(out ARect: TRectF): Boolean; overload;
    function IsRect: Boolean; overload;
    function IsRect(out ARect: TRectF): Boolean; overload;
    function IsRoundRect: Boolean; overload;
    function IsRoundRect(out ARoundRect: ISkRoundRect): Boolean; overload;
    function Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
    function ToSVG: string;
    function Transform(const AMatrix: TMatrix): ISkPath;
  public
    constructor Create(const ASVG: string); overload;
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(const AStream: TStream); overload;
    class function ConvertConicToQuads(const APoint1, APoint2, APoint3: TPointF; const AWeight: Single; const APower2: Integer): TArray<TPointF>; static;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPath(...) __SkCreate(TSkPath, ISkPath, __VA_ARGS__)'}

  { ISkPathBuilder }

  ISkPathBuilder = interface(ISkObject)
    ['{F0976C6D-4474-4290-B9CD-22DB130F8EB5}']
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPath(const APath: ISkPath);
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSkPathArcSize; const ASweep: TSkPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISkPath;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
    procedure IncReserve(const AExtraPointCount: Integer); overload;
    procedure IncReserve(const AExtraPointCount, AExtraVerbCount: Integer);  overload;
    procedure LineTo(const APoint: TPointF); overload;
    procedure LineTo(const AX, AY: Single); overload;
    procedure MoveTo(const APoint: TPointF); overload;
    procedure MoveTo(const AX, AY: Single); overload;
    procedure Offset(const ADeltaX, ADeltaY: Single);
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
  strict private
    procedure AddArc(const AOval: TRectF; const AStartAngle, ASweepAngle: Single);
    procedure AddCircle(const ACenter: TPointF; ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddCircle(const ACenterX, ACenterY, ARadius: Single; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddOval(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddPath(const APath: ISkPath);
    procedure AddPolygon(const APolygon: TPolygon; const IsClosed: Boolean);
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRect(const ARect: TRectF; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection = TSkPathDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: ISkRoundRect; ADirection: TSkPathDirection; AStartIndex: Cardinal); overload;
    procedure ArcTo(const APoint1, APoint2: TPointF; const ARadius: Single); overload;
    procedure ArcTo(const AOval: TRectF; const AStartAngle, ASweepAngle: Single; const AForceMoveTo: Boolean); overload;
    procedure ArcTo(const ARadius: TPointF; const XAxisRotate: Single; const ALargeArc: TSkPathArcSize; const ASweep: TSkPathDirection; const AXY: TPointF); overload;
    procedure Close;
    procedure ConicTo(const APoint1, APoint2: TPointF; const AWeight: Single); overload;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single); overload;
    procedure CubicTo(const APoint1, APoint2, APoint3: TPointF); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single); overload;
    function Detach: ISkPath;
    function GetBounds: TRectF;
    function GetFillType: TSkPathFillType;
    procedure IncReserve(const AExtraPointCount: Integer); overload;
    procedure IncReserve(const AExtraPointCount, AExtraVerbCount: Integer);  overload;
    procedure LineTo(const APoint: TPointF); overload;
    procedure LineTo(const AX, AY: Single); overload;
    procedure MoveTo(const APoint: TPointF); overload;
    procedure MoveTo(const AX, AY: Single); overload;
    procedure Offset(const ADeltaX, ADeltaY: Single);
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
  public
    constructor Create; overload;
    constructor Create(const APathBuilder: ISkPathBuilder); overload;
    constructor Create(const AFillType: TSkPathFillType); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPathBuilder(...) __SkCreate(TSkPathBuilder, ISkPathBuilder, __VA_ARGS__)'}

  { ISkPathEffect }

  ISkPathEffect = interface(ISkReferenceCounted)
    ['{6229C1C7-5DBB-4525-B9B8-9E50F644AB3F}']
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
    class function MakeMatrix(const AMatrix: TMatrix): ISkPathEffect; static;
    class function MakeMerge(const AEffect1, AEffect2: ISkPathEffect; const AOp: TSkPathOp): ISkPathEffect; static;
    class function MakeStroke(const AWidth: Single; const AJoin: TSkStrokeJoin; const ACap: TSkStrokeCap; const AMiter: Single = 4): ISkPathEffect; static;
    class function MakeStrokeAndFill: ISkPathEffect; static;
    class function MakeSum(const AEffect1, AEffect2: ISkPathEffect): ISkPathEffect; static;
    class function MakeTranslate(const ADeltaX, ADeltaY: Single): ISkPathEffect; static;
    class function MakeTrim(const AStart, AStop: Single; const AMode: TSkPathEffectTrimMode): ISkPathEffect; static;
  end;

  { ISkPathMeasure }

  ISkPathMeasure = interface(ISkObject)
    ['{33A4DFAF-069C-44BE-8AEF-333B88187B78}']
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
  strict private
    function GetLength: Single;
    function GetMatrix(const ADistance: Single; out AMatrix: TMatrix; const AMatrixFlags: TSkPathMeasureMatrixFlags = [TSkPathMeasureMatrixFlag.Position, TSkPathMeasureMatrixFlag.Tangent]): Boolean;
    function GetPositionAndTangent(const ADistance: Single; out APosition, ATangent: TPointF): Boolean;
    function GetSegment(const AStart, AStop: Single; const AStartWithMoveTo: Boolean): ISkPath;
    function IsClosed: Boolean;
    function NextContour: Boolean;
  public
    constructor Create(const APath: ISkPath; const AForceClosed: Boolean = False; const AResScale: Single = 1);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPathMeasure(...) __SkCreate(TSkPathMeasure, ISkPathMeasure, __VA_ARGS__)'}

  { ISkPicture }

  ISkPicture = interface(ISkReferenceCounted)
    ['{5FBF6D75-3A44-4012-AAC9-AD22E1C955D2}']
    function ApproximateBytesUsed: NativeUInt;
    function ApproximateOpCount(const ANested: Boolean = False): Integer;
    function GetCullRect: TRectF;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    procedure Playback(const ACanvas: ISkCanvas);
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
    property CullRect: TRectF read GetCullRect;
  end;

  { TSkPicture }

  TSkPicture = class(TSkReferenceCounted, ISkPicture)
  strict private
    function GetCullRect: TRectF;
  public
    function ApproximateBytesUsed: NativeUInt;
    function ApproximateOpCount(const ANested: Boolean = False): Integer;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    procedure Playback(const ACanvas: ISkCanvas);
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
    property CullRect: TRectF read GetCullRect;
    class function MakeFromBytes(const ABytes: TBytes): ISkPicture; static;
    class function MakeFromStream(const AStream: TStream): ISkPicture; static;
  end;

  { ISkPictureRecorder }

  ISkPictureRecorder = interface(ISkObject)
    ['{46663FA5-57B9-4289-9381-0433E3A70EB9}']
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
  end;

  { TSkPictureRecorder }

  TSkPictureRecorder = class(TSkObject, ISkPictureRecorder)
  strict private
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPictureRecorder(...) __SkCreate(TSkPictureRecorder, ISkPictureRecorder, __VA_ARGS__)'}

  { ISkPixmap }

  ISkPixmap = interface(ISkObject)
    ['{EFEDBF69-4C8D-4124-83A6-68A9B1DF239E}']
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISkPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSkAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorF(const AX, AY: Integer): TAlphaColorF;
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
    function ScalePixels(const ADest: ISkPixmap): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions): Boolean; overload;
    procedure SetColorSpace(AValue: ISkColorSpace);
    property Alphas[const AX, AY: Integer]: Single read GetAlpha;
    property AlphaType: TSkAlphaType read GetAlphaType;
    property Colors[const AX, AY: Integer]: TAlphaColor read GetColor;
    property ColorsF[const AX, AY: Integer]: TAlphaColorF read GetColorF;
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
  strict private
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function ExtractSubset(const ADest: ISkPixmap; const AArea: TRect): Boolean;
    function GetAlpha(const AX, AY: Integer): Single;
    function GetAlphaType: TSkAlphaType;
    function GetColor(const AX, AY: Integer): TAlphaColor;
    function GetColorF(const AX, AY: Integer): TAlphaColorF;
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
    function ScalePixels(const ADest: ISkPixmap): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions): Boolean; overload;
    procedure SetColorSpace(AValue: ISkColorSpace);
  public
    constructor Create(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkPixmap(...) __SkCreate(TSkPixmap, ISkPixmap, __VA_ARGS__)'}

  TSkRectEnumerator = TSkEnumerable<TRect>.TEnumerator;

  { ISkRegionCliperator }

  ISkRegionCliperator = interface(ISkEnumerable)
    ['{2A8DA4E7-1B52-4311-9398-E3A93BACDB02}']
    function GetCurrent: TRect;
    function GetEnumerator: TSkRectEnumerator;
  end;

  { ISkRegionIterator }

  ISkRegionIterator = interface(ISkEnumerable)
    ['{2A0AC6C3-6101-4A9C-83BE-8F56B67F4239}']
    function GetCurrent: TRect;
    function GetEnumerator: TSkRectEnumerator;
  end;

  TSkPointEnumerator = TSkEnumerable<TPoint>.TEnumerator;

  { ISkRegionSpanerator }

  ISkRegionSpanerator = interface(ISkEnumerable)
    ['{E6980A05-9F49-4B3E-BD03-7A305C69B9AE}']
    function GetCurrent: TPoint;
    function GetEnumerator: TSkPointEnumerator;
  end;

  { ISkRegion }

  ISkRegion = interface(ISkObject)
    ['{9ED1C7B2-BF4E-4E4E-9A69-DDDB615C0319}']
    function Contains(const AX, AY: Integer): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const ARegion: ISkRegion): Boolean; overload;
    function GetBoundaryPath: ISkPath;
    function GetBounds: TRect;
    function GetCliperator(const AClip: TRect): ISkRegionCliperator;
    function GetIterator: ISkRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISkRegionSpanerator;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISkRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISkRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean; overload;
    function Op(const ARegion: ISkRegion; const AOp: TSkRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARect: TRect): Boolean; overload;
    function QuickReject(const ARegion: ISkRegion): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISkPath; const AClip: ISkRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const ADeltaX, ADeltaY: Integer);
    property Bounds: TRect read GetBounds;
  end;

  { TSkRegion }

  TSkRegion = class(TSkObject, ISkRegion)
  strict private type
    TRegionCliperator = class(TSkEnumerable<TRect>, ISkRegionCliperator)
    public
      constructor Create(const ARegion: ISkRegion; const AClip: TRect);
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
    end;

    TRegionIterator = class(TSkEnumerable<TRect>, ISkRegionIterator)
    public
      constructor Create(const ARegion: ISkRegion);
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
    end;

    TRegionSpanerator = class(TSkEnumerable<TPoint>, ISkRegionSpanerator)
    strict private
      FCurrent: TPoint;
    public
      constructor Create(const ARegion: ISkRegion; const AY, ALeft, ARight: Integer);
      function GetCurrent: TPoint; override;
      function MoveNext: Boolean; override;
      class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
    end;

  strict private
    function Contains(const AX, AY: Integer): Boolean; overload;
    function Contains(const ARect: TRect): Boolean; overload;
    function Contains(const ARegion: ISkRegion): Boolean; overload;
    function GetBoundaryPath: ISkPath;
    function GetBounds: TRect;
    function GetCliperator(const AClip: TRect): ISkRegionCliperator;
    function GetIterator: ISkRegionIterator;
    function GetSpanerator(const AY, ALeft, ARight: Integer): ISkRegionSpanerator;
    function Intersects(const ARect: TRect): Boolean; overload;
    function Intersects(const ARegion: ISkRegion): Boolean; overload;
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARegion: ISkRegion): Boolean;
    function IsRect: Boolean;
    function Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean; overload;
    function Op(const ARegion: ISkRegion; const AOp: TSkRegionOp): Boolean; overload;
    function QuickContains(const ARect: TRect): Boolean;
    function QuickReject(const ARect: TRect): Boolean; overload;
    function QuickReject(const ARegion: ISkRegion): Boolean; overload;
    procedure SetEmpty;
    function SetPath(const APath: ISkPath; const AClip: ISkRegion): Boolean;
    function SetRect(const ARect: TRect): Boolean;
    function SetRects(const ARects: TArray<TRect>): Boolean;
    procedure Translate(const ADeltaX, ADeltaY: Integer);
  public
    constructor Create; overload;
    constructor Create(const ARegion: ISkRegion); overload;
    constructor Create(const ARect: TRect); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkRegion(...) __SkCreate(TSkRegion, ISkRegion, __VA_ARGS__)'}

  { ISkRoundRect }

  ISkRoundRect = interface(ISkObject)
    ['{A828D0E5-98C6-42EC-A44B-494DF41183DA}']
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADeltaX, ADeltaY: Single);
    function GetHeight: Single;
    function GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADeltaX, ADeltaY: Single);
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARoundRect: ISkRoundRect): Boolean;
    function IsNinePatch: Boolean;
    function IsOval: Boolean;
    function IsRect: Boolean;
    function IsSimple: Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADeltaX, ADeltaY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    function Transform(const AMatrix: TMatrix): ISkRoundRect;
    property Height: Single read GetHeight;
    property Radii[const ACorner: TSkRoundRectCorner]: TPointF read GetRadii;
    property Rect: TRectF read GetRect;
    property SimpleRadii: TPointF read GetSimpleRadii;
    property Width: Single read GetWidth;
  end;

  { TSkRoundRect }

  TSkRoundRect = class(TSkObject, ISkRoundRect)
  strict private
    function Contains(const ARect: TRect): Boolean;
    procedure Deflate(const ADeltaX, ADeltaY: Single);
    function GetHeight: Single;
    function GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
    function GetRect: TRectF;
    function GetSimpleRadii: TPointF;
    function GetWidth: Single;
    procedure Inflate(const ADeltaX, ADeltaY: Single);
    function IsComplex: Boolean;
    function IsEmpty: Boolean;
    function IsEqual(const ARoundRect: ISkRoundRect): Boolean;
    function IsNinePatch: Boolean;
    function IsOval: Boolean;
    function IsRect: Boolean;
    function IsSimple: Boolean;
    function IsValid: Boolean;
    procedure Offset(const ADeltaX, ADeltaY: Single);
    procedure SetEmpty;
    procedure SetNinePatch(const ARect: TRectF; const ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom: Single);
    procedure SetOval(const ARect: TRectF);
    procedure SetRect(const ARect: TRectF); overload;
    procedure SetRect(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    procedure SetRect(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    function Transform(const AMatrix: TMatrix): ISkRoundRect;
  public
    constructor Create; overload;
    constructor Create(const ARoundRect: ISkRoundRect); overload;
    constructor Create(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    constructor Create(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkRoundRect(...) __SkCreate(TSkRoundRect, ISkRoundRect, __VA_ARGS__)'}

  { ISkRuntimeEffect }

  ISkRuntimeEffect = interface(ISkReferenceCounted)
    ['{206A8478-49E2-44DE-B198-014FEC68B721}']
    function ChildExists(const AName: string): Boolean;
    function IndexOfChild(const AName: string): Integer;
    function IndexOfUniform(const AName: string): Integer;
    function IsUniformTypeOrdinal(const AIndex: Integer): Boolean;
    function IsUniformTypeOrdinalByName(const AName: string): Boolean;
    function MakeBlender(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkBlender;
    function MakeColorFilter(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkColorFilter;
    function MakeImage(const AUniforms; const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeImage(const AUniforms; const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo; const ALocalMatrix: TMatrix; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeShader(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkShader; overload;
    function MakeShader(const AUniforms; const AChildren: TArray<ISkFlattenable>; const ALocalMatrix: TMatrix): ISkShader; overload;
    function UniformExists(const AName: string): Boolean;
    function GetChildCount: Integer;
    function GetChildName(const AIndex: Integer): string;
    function GetChildType(const AIndex: Integer): TSkRuntimeEffectChildType;
    function GetChildTypeByName(const AName: string): TSkRuntimeEffectChildType;
    function GetUniformCount: Integer;
    function GetUniformDataSize: NativeUInt;
    function GetUniformName(const AIndex: Integer): string;
    function GetUniformOffset(const AIndex: Integer): NativeUInt;
    function GetUniformOffsetByName(const AName: string): NativeUInt;
    function GetUniformType(const AIndex: Integer): TSkRuntimeEffectUniformType;
    function GetUniformTypeByName(const AName: string): TSkRuntimeEffectUniformType;
    function GetUniformTypeCount(const AIndex: Integer): Integer;
    function GetUniformTypeCountByName(const AName: string): Integer;
    property ChildCount: Integer read GetChildCount;
    property ChildrenNames[const AIndex: Integer]: string read GetChildName;
    property ChildType[const AIndex: Integer]: TSkRuntimeEffectChildType read GetChildType;
    property ChildTypeByName[const AName: string]: TSkRuntimeEffectChildType read GetChildTypeByName;
    property UniformCount: Integer read GetUniformCount;
    property UniformDataSize: NativeUInt read GetUniformDataSize;
    property UniformNames[const AIndex: Integer]: string read GetUniformName;
    property UniformOffset[const AIndex: Integer]: NativeUInt read GetUniformOffset;
    property UniformOffsetByName[const AName: string]: NativeUInt read GetUniformOffsetByName;
    property UniformType[const AIndex: Integer]: TSkRuntimeEffectUniformType read GetUniformType;
    property UniformTypeByName[const AName: string]: TSkRuntimeEffectUniformType read GetUniformTypeByName;
    property UniformTypeCount[const AIndex: Integer]: Integer read GetUniformTypeCount;
    property UniformTypeCountByName[const AName: string]: Integer read GetUniformTypeCountByName;
  end;

  { TSkRuntimeEffect }

  TSkRuntimeEffect = class(TSkReferenceCounted, ISkRuntimeEffect)
  strict private
    function GetChildCount: Integer;
    function GetChildName(const AIndex: Integer): string;
    function GetChildType(const AIndex: Integer): TSkRuntimeEffectChildType;
    function GetChildTypeByName(const AName: string): TSkRuntimeEffectChildType;
    function GetUniformCount: Integer;
    function GetUniformDataSize: NativeUInt;
    function GetUniformName(const AIndex: Integer): string;
    function GetUniformOffset(const AIndex: Integer): NativeUInt;
    function GetUniformOffsetByName(const AName: string): NativeUInt;
    function GetUniformType(const AIndex: Integer): TSkRuntimeEffectUniformType;
    function GetUniformTypeByName(const AName: string): TSkRuntimeEffectUniformType;
    function GetUniformTypeCount(const AIndex: Integer): Integer;
    function GetUniformTypeCountByName(const AName: string): Integer;
  public
    function ChildExists(const AName: string): Boolean;
    function IndexOfChild(const AName: string): Integer;
    function IndexOfUniform(const AName: string): Integer;
    function IsUniformTypeOrdinal(const AIndex: Integer): Boolean;
    function IsUniformTypeOrdinalByName(const AName: string): Boolean;
    function MakeBlender(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkBlender;
    function MakeColorFilter(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkColorFilter;
    function MakeImage(const AUniforms; const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeImage(const AUniforms; const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo; const ALocalMatrix: TMatrix; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeShader(const AUniforms; const AChildren: TArray<ISkFlattenable>): ISkShader; overload;
    function MakeShader(const AUniforms; const AChildren: TArray<ISkFlattenable>; const ALocalMatrix: TMatrix): ISkShader; overload;
    function UniformExists(const AName: string): Boolean;
    property ChildCount: Integer read GetChildCount;
    property ChildrenNames[const AIndex: Integer]: string read GetChildName;
    property ChildType[const AIndex: Integer]: TSkRuntimeEffectChildType read GetChildType;
    property ChildTypeByName[const AName: string]: TSkRuntimeEffectChildType read GetChildTypeByName;
    property UniformCount: Integer read GetUniformCount;
    property UniformDataSize: NativeUInt read GetUniformDataSize;
    property UniformNames[const AIndex: Integer]: string read GetUniformName;
    property UniformOffset[const AIndex: Integer]: NativeUInt read GetUniformOffset;
    property UniformOffsetByName[const AName: string]: NativeUInt read GetUniformOffsetByName;
    property UniformType[const AIndex: Integer]: TSkRuntimeEffectUniformType read GetUniformType;
    property UniformTypeByName[const AName: string]: TSkRuntimeEffectUniformType read GetUniformTypeByName;
    property UniformTypeCount[const AIndex: Integer]: Integer read GetUniformTypeCount;
    property UniformTypeCountByName[const AName: string]: Integer read GetUniformTypeCountByName;
    class function MakeForBlender(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForBlender(const ASkSL: MarshaledAString): ISkRuntimeEffect; overload; static;
    class function MakeForBlender(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForBlender(const ASkSL: MarshaledAString; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: MarshaledAString): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: MarshaledAString; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: MarshaledAString): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: MarshaledAString; out AErrorText: string): ISkRuntimeEffect; overload; static;
  end;

  { ISkRuntimeEffectBuilder }

  ISkRuntimeEffectBuilder = interface(ISkObject)
    ['{49AE29A9-1C5A-4D67-AF39-AB54CECFA89E}']
    function GetEffect: TSkRuntimeEffect;
    procedure SetChild(const AName: string; const AChild: ISkShader); overload;
    procedure SetChild(const AName: string; const AChild: ISkColorFilter); overload;
    procedure SetChild(const AName: string; const AChild: ISkBlender); overload;
    procedure SetUniform(const AName: string; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AName: string; const AValue: Integer); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AName: string; const AValue: TPoint); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AName: string; const AValue: Single); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AName: string; const AValue: TPointF); overload;
    procedure SetUniform(const AName: string; const AValue: TAlphaColorF); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4x4); overload;
    procedure SetUniform(const AName: string; const AValue: TMatrix); overload;
    property Effect: TSkRuntimeEffect read GetEffect;
  end;

  { TSkRuntimeEffectBuilder }

  TSkRuntimeEffectBuilder = class abstract(TSkObject, ISkRuntimeEffectBuilder)
  private
    FEffect: ISkRuntimeEffect;
    function GetEffect: TSkRuntimeEffect;
    procedure SetChild(const AName: string; const AChild: ISkShader); overload;
    procedure SetChild(const AName: string; const AChild: ISkColorFilter); overload;
    procedure SetChild(const AName: string; const AChild: ISkBlender); overload;
    procedure SetUniform(const AName: string; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AName: string; const AValue: Integer); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AName: string; const AValue: TPoint); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: Single); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TPointF); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TAlphaColorF); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TSkRuntimeEffectFloat4x4); overload;
    procedure SetUniform(const AName: string; const {%H-}AValue: TMatrix); overload;
  public
    procedure AfterConstruction; override;
  end;

  { ISkRuntimeBlenderBuilder }

  ISkRuntimeBlenderBuilder = interface(ISkRuntimeEffectBuilder)
    ['{BEFC3953-2989-44C0-AC08-FA5D3BEF284A}']
    function MakeBlender: ISkBlender;
  end;

  { TSkRuntimeBlenderBuilder }

  TSkRuntimeBlenderBuilder = class(TSkRuntimeEffectBuilder, ISkRuntimeBlenderBuilder)
  strict private
    function MakeBlender: ISkBlender;
  public
    constructor Create(const AEffect: ISkRuntimeEffect);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkRuntimeBlenderBuilder(...) __SkCreate(TSkRuntimeBlenderBuilder, ISkRuntimeBlenderBuilder, __VA_ARGS__)'}

  { ISkRuntimeShaderBuilder }

  ISkRuntimeShaderBuilder = interface(ISkRuntimeEffectBuilder)
    ['{2360AC47-2928-4D9F-AF04-8505D881D88A}']
    function MakeImage(const AImageInfo: TSkImageInfo; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeImage(const AImageInfo: TSkImageInfo; const ALocalMatrix: TMatrix; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeShader: ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix): ISkShader; overload;
  end;

  { TSkRuntimeShaderBuilder }

  TSkRuntimeShaderBuilder = class(TSkRuntimeEffectBuilder, ISkRuntimeShaderBuilder)
  strict private
    function MakeImage(const AImageInfo: TSkImageInfo; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeImage(const AImageInfo: TSkImageInfo; const ALocalMatrix: TMatrix; const AMipmapped: Boolean = False; const AContext: IGrDirectContext = nil): ISkImage; overload;
    function MakeShader: ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix): ISkShader; overload;
  public
    constructor Create(const AEffect: ISkRuntimeEffect);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkRuntimeShaderBuilder(...) __SkCreate(TSkRuntimeShaderBuilder, ISkRuntimeShaderBuilder, __VA_ARGS__)'}

  { ISkShader }

  ISkShader = interface(ISkFlattenable)
    ['{7CB71D93-131A-4C89-96C5-B966D39F43AF}']
    function MakeWithColorFilter(const AColorFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
  end;

  { TSkShader }

  TSkShader = class(TSkReferenceCounted, ISkShader)
  public
    function MakeWithColorFilter(const AColorFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
    class function MakeBlend(const AMode: TSkBlendMode; const ADest, ASrc: ISkShader): ISkShader; static;
    class function MakeColor(const AColor: TAlphaColor): ISkShader; overload; static;
    class function MakeColor(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeEmpty: ISkShader; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeImage(const AImage: ISkImage; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    class function MakeImage(const AImage: ISkImage; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    class function MakeImage(const AImage: ISkImage; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    class function MakeImage(const AImage: ISkImage; const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISkShader; overload; static;
    class function MakePerlinNoiseFractalNoise(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISkShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single): ISkShader; overload; static;
    class function MakePerlinNoiseTurbulence(const ABaseFrequencyX, ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single; const ATileSize: TSize): ISkShader; overload; static;
    class function MakePicture(const APicture: ISkPicture; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    class function MakePicture(const APicture: ISkPicture; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    class function MakePicture(const APicture: ISkPicture; const ATileRect: TRectF; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    class function MakePicture(const APicture: ISkPicture; const ATileRect: TRectF; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
  end;

  { ISkSurface }

  ISkSurface = interface(ISkReferenceCounted)
    ['{69826677-0632-4D9E-ABAB-B210BE9309C3}']
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCpu: Boolean = False); overload;
    procedure FlushAndSubmit(const ASemaphores: TArray<IGrBackendSemaphore>; const ANewState: IGrBackendSurfaceMutableState; const ASyncCpu: Boolean = False); overload;
    function GetCanvas: ISkCanvas;
    function GetProperties: TSkSurfaceProperties;
    function MakeImageSnapshot: ISkImage; overload;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Wait(const ASemaphores: TArray<IGrBackendSemaphore>);
    procedure WritePixels(const ASrc: ISkPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    property Canvas: ISkCanvas read GetCanvas;
    property Properties: TSkSurfaceProperties read GetProperties;
  end;

  { TSkSurface }

  TSkSurface = class(TSkReferenceCounted, ISkSurface)
  strict private
    FCanvas: ISkCanvas;
    function GetCanvas: ISkCanvas;
    function GetProperties: TSkSurfaceProperties;
    class procedure raster_release_proc(pixels: Pointer; context: Pointer); cdecl; static;
  public
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCpu: Boolean = False); overload;
    procedure FlushAndSubmit(const ASemaphores: TArray<IGrBackendSemaphore>; const ANewState: IGrBackendSurfaceMutableState; const ASyncCpu: Boolean = False); overload;
    function MakeImageSnapshot: ISkImage; overload;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Wait(const ASemaphores: TArray<IGrBackendSemaphore>);
    procedure WritePixels(const ASrc: ISkPixmap; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    procedure WritePixels(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX: Integer = 0; const ADestY: Integer = 0); overload;
    property Canvas: ISkCanvas read GetCanvas;
    property Properties: TSkSurfaceProperties read GetProperties;
    class function MakeFromMTKView(const AContext: IGrDirectContext; const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromMTKView(const AContext: IGrDirectContext; const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromRenderTarget(const AContext: IGrDirectContext; const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromRenderTarget(const AContext: IGrDirectContext; const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeRaster(const AWidth, AHeight: Integer; const AColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF}; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeRaster(const AWidth, AHeight: Integer; const AProperties: TSkSurfaceProperties; const AColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF}; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const ARowBytes: NativeUInt): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const ARowBytes: NativeUInt; const AProperties: TSkSurfaceProperties): ISkSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISkPixmap; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISkPixmap; const AProperties: TSkSurfaceProperties; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const AProperties: TSkSurfaceProperties; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGrDirectContext; const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo; const ASampleCount: Integer = 0; const AOrigin: TGrSurfaceOrigin = TGrSurfaceOrigin.BottomLeft; const AShouldCreateWithMips: Boolean = False): ISkSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGrDirectContext; const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties; const ASampleCount: Integer = 0; const AOrigin: TGrSurfaceOrigin = TGrSurfaceOrigin.BottomLeft; const AShouldCreateWithMips: Boolean = False): ISkSurface; overload; static;
  end;

  { ISkTextBlob }

  ISkTextBlob = interface(ISkNonVirtualReferenceCounted)
    ['{EC17FBCB-9578-402C-86B9-55ECA69E78E0}']
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
  end;

  { TSkTextBlob }

  TSkTextBlob = class(TSkNonVirtualReferenceCounted, ISkTextBlob)
  public
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
    class function MakeFromText(const AText: string; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextHorizontallyPositioned(const AText: string; const AXPositions: TArray<Single>; const AY: Single; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextPositioned(const AText: string; const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextTransform(const AText: string; const AMatrices: TArray<TSkRotationScaleMatrix>; const AFont: ISkFont): ISkTextBlob; static;
    class procedure __RefHandle(const AHandle: sk_handle_t); override;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkTraceMemoryDump }

  ISkTraceMemoryDump = interface(ISkObject)
    ['{7736D6EF-9FCA-46C5-AD45-AFBB92086D33}']
  end;

  { TSkTraceMemoryDumpBaseClass }

  TSkTraceMemoryDumpBaseClass = class abstract(TSkObject, ISkTraceMemoryDump)
  strict private
    class constructor Create;
    class procedure dump_numeric_value_proc(context: Pointer; const dump_name, value_name, units: MarshaledAString; value: uint64_t); cdecl; static;
    class procedure dump_string_value_proc(context: Pointer; const dump_name, value_name, value: MarshaledAString); cdecl; static;
  strict protected
    procedure DumpNumericValue(const ADumpName, AValueName, AUnits: string; const AValue: UInt64); virtual; abstract;
    procedure DumpStringValue(const ADumpName, AValueName, AValue: string); virtual; abstract;
  public
    constructor Create(const ADetailedDump, ADumpWrappedObjects: Boolean);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkTypeface }

  ISkTypeface = interface(ISkReferenceCounted)
    ['{A6ECEB69-E0EB-4CFF-941F-75B8D1702FE5}']
    function GetFamilyName: string;
    function GetSlant: TSkFontSlant;
    function GetStyle: TSkFontStyle;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
    property FamilyName: string read GetFamilyName;
    property Slant: TSkFontSlant read GetSlant;
    property Style: TSkFontStyle read GetStyle;
    property Weight: Integer read GetWeight;
    property Width: Integer read GetWidth;
  end;

  { TSkTypeface }

  TSkTypeface = class(TSkReferenceCounted, ISkTypeface)
  strict private
    function GetFamilyName: string;
    function GetSlant: TSkFontSlant;
    function GetStyle: TSkFontStyle;
    function GetWeight: Integer;
    function GetWidth: Integer;
  public
    function IsBold: Boolean;
    function IsItalic: Boolean;
    property FamilyName: string read GetFamilyName;
    property Slant: TSkFontSlant read GetSlant;
    property Style: TSkFontStyle read GetStyle;
    property Weight: Integer read GetWeight;
    property Width: Integer read GetWidth;
    class function MakeDefault: ISkTypeface; static;
    class function MakeFromFile(const AFileName: string; const ATTCIndex: Integer = 0): ISkTypeface; static;
    class function MakeFromName(const AFamilyName: string; const AStyle: TSkFontStyle): ISkTypeface; static;
    class function MakeFromStream(const AStream: TStream; const ATTCIndex: Integer = 0): ISkTypeface; static;
  end;

  { ISkVertices }

  ISkVertices = interface(ISkNonVirtualReferenceCounted)
    ['{C4D307F3-E77A-4270-8559-E27ED245E86A}']
  end;

  { TSkVertices }

  TSkVertices = class(TSkNonVirtualReferenceCounted, ISkVertices)
  public
    class function MakeCopy(const AVertexMode: TSkVertexMode; const APositions, ATextures: TArray<TPointF>; const AColors: TArray<TAlphaColor>; const AIndices: TArray<Word> = nil): ISkVertices; static;
    class procedure __RefHandle(const AHandle: sk_handle_t); override;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkParticleEffect }

  ISkParticleEffect = interface(ISkReferenceCounted)
    ['{A8AAC230-E76B-4BBE-84F2-82D09D38E0D4}']
    function GetPosition: TPointF;
    function GetRate: Single;
    function GetUniform(const AIndex: NativeUInt): TSkParticleUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformData: PSingleArray;
    function GetUniformDataCount: Integer;
    function GetUniformName(const AIndex: NativeUInt): string;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetPosition(const AValue: TPointF);
    procedure SetRate(const AValue: Single);
    function SetUniform(const AName: string; const AData: TArray<Single>): Boolean;
    procedure Start(const ANow: Double; const ALooping: Boolean);
    procedure Update(const ANow: Double);
    property Position: TPointF read GetPosition write SetPosition;
    property Rate: Single read GetRate write SetRate;
    property Uniforms[const AIndex: NativeUInt]: TSkParticleUniform read GetUniform;
    property UniformCount: NativeUInt read GetUniformCount;
    property UniformData: PSingleArray read GetUniformData;
    property UniformDataCount: Integer read GetUniformDataCount;
    property UniformName[const AIndex: NativeUInt]: string read GetUniformName;
  end;

  { TSkParticleEffect }

  TSkParticleEffect = class(TSkReferenceCounted, ISkParticleEffect)
  strict private
    function GetPosition: TPointF;
    function GetRate: Single;
    function GetUniform(const AIndex: NativeUInt): TSkParticleUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformData: PSingleArray;
    function GetUniformDataCount: Integer;
    function GetUniformName(const AIndex: NativeUInt): string;
    procedure SetPosition(const AValue: TPointF);
    procedure SetRate(const AValue: Single);
    class constructor Create;
  public
    procedure Render(const ACanvas: ISkCanvas);
    function SetUniform(const AName: string; const AData: TArray<Single>): Boolean;
    procedure Start(const ANow: Double; const ALooping: Boolean);
    procedure Update(const ANow: Double);
    property Position: TPointF read GetPosition write SetPosition;
    property Rate: Single read GetRate write SetRate;
    property Uniforms[const AIndex: NativeUInt]: TSkParticleUniform read GetUniform;
    property UniformCount: NativeUInt read GetUniformCount;
    property UniformData: PSingleArray read GetUniformData;
    property UniformDataCount: Integer read GetUniformDataCount;
    property UniformName[const AIndex: NativeUInt]: string read GetUniformName;
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil): ISkParticleEffect; static; deprecated;
    class function MakeFromFile(const AFileName: string): ISkParticleEffect; static; deprecated;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil): ISkParticleEffect; static; deprecated;
  end;

  { ISkottieAnimation }

  ISkottieAnimation = interface(ISkNonVirtualReferenceCounted)
    ['{3B87EFFF-8184-4576-A4AC-B73342834211}']
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

  TSkottieAnimation = class(TSkNonVirtualReferenceCounted, ISkottieAnimation)
  strict private
    function GetDuration: Double;
    function GetFPS: Double;
    function GetInPoint: Double;
    function GetOutPoint: Double;
    function GetSize: TSizeF;
    function GetVersion: string;
  public
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
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil; const AFontProvider: ISkTypefaceFontProvider = nil): ISkottieAnimation; static;
    class function MakeFromFile(const AFileName: string; const AFontProvider: ISkTypefaceFontProvider = nil): ISkottieAnimation; static;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil; const AFontProvider: ISkTypefaceFontProvider = nil): ISkottieAnimation; static;
    class procedure __RefHandle(const AHandle: sk_handle_t); override;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkParagraph }

  ISkParagraph = interface(ISkObject)
    ['{CB618EF9-C99E-4079-8637-9E282FAA64DD}']
    function DidExceedMaxLines: Boolean;
    function GetAlphabeticBaseline: Single;
    function GetGlyphPositionAtCoordinate(const ADeltaX, ADeltaY: Single): TSkPositionAffinity;
    function GetHeight: Single;
    function GetIdeographicBaseline: Single;
    function GetLineMetrics: TArray<TSkMetrics>;
    function GetLongestLine: Single;
    function GetMaxIntrinsicWidth: Single;
    function GetMaxWidth: Single;
    function GetMinIntrinsicWidth: Single;
    function GetRectsForPlaceholders: TArray<TSkTextBox>;
    function GetRectsForRange(const AStart, AEnd: Cardinal; const ARectHeightStyle: TSkRectHeightStyle; const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
    procedure GetWordBoundary(const AOffset: Cardinal; out AStart, AEnd: Cardinal);
    procedure Layout(const AWidth: Single);
    procedure Paint(const ACanvas: ISkCanvas; const AX, AY: Single);
    function ToPath: ISkPath;
    procedure Visit(const AProc: TSkParagraphVisitProc);
    property AlphabeticBaseline: Single read GetAlphabeticBaseline;
    property Height: Single read GetHeight;
    property IdeographicBaseline: Single read GetIdeographicBaseline;
    property LineMetrics: TArray<TSkMetrics> read GetLineMetrics;
    property LongestLine: Single read GetLongestLine;
    property MaxIntrinsicWidth: Single read GetMaxIntrinsicWidth;
    property MaxWidth: Single read GetMaxWidth;
    property MinIntrinsicWidth: Single read GetMinIntrinsicWidth;
  end;

  { TSkParagraph }

  TSkParagraph = class(TSkObject, ISkParagraph)
  strict private
    function DidExceedMaxLines: Boolean;
    function GetAlphabeticBaseline: Single;
    function GetGlyphPositionAtCoordinate(const ADeltaX, ADeltaY: Single): TSkPositionAffinity;
    function GetHeight: Single;
    function GetIdeographicBaseline: Single;
    function GetLineMetrics: TArray<TSkMetrics>;
    function GetLongestLine: Single;
    function GetMaxIntrinsicWidth: Single;
    function GetMaxWidth: Single;
    function GetMinIntrinsicWidth: Single;
    function GetRectsForPlaceholders: TArray<TSkTextBox>;
    function GetRectsForRange(const AStart, AEnd: Cardinal; const ARectHeightStyle: TSkRectHeightStyle; const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
    procedure GetWordBoundary(const AOffset: Cardinal; out AStart, AEnd: Cardinal);
    procedure Layout(const AWidth: Single);
    procedure Paint(const ACanvas: ISkCanvas; const AX, AY: Single);
    function ToPath: ISkPath;
    procedure Visit(const AProc: TSkParagraphVisitProc);
    class procedure visit_proc(line_number: int32_t; const info: psk_paragraphvisitorinfo_t; context: Pointer); cdecl;  static;
  public

    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkParagraph(...) __SkCreate(TSkParagraph, ISkParagraph, __VA_ARGS__)'}

  { ISkParagraphBuilder }

  ISkParagraphBuilder = interface(ISkObject)
    ['{1D484C30-D4E1-49BC-AF2E-7D7E7FF568BE}']
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Build: ISkParagraph;
    procedure Pop;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
  end;

  { TSkParagraphBuilder }

  TSkParagraphBuilder = class(TSkObject, ISkParagraphBuilder)
  strict private
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Build: ISkParagraph;
    procedure Pop;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
  public
    constructor Create(const AParagraphStyle: ISkParagraphStyle); overload;
    constructor Create(const AParagraphStyle: ISkParagraphStyle; const AFontProvider: ISkTypefaceFontProvider; const AEnableFontFallback: Boolean = True); overload;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkParagraphBuilder(...) __SkCreate(TSkParagraphBuilder, ISkParagraphBuilder, __VA_ARGS__)'}

  { ISkStrutStyle }

  ISkStrutStyle = interface(ISkObject)
    ['{B5275BA1-4FEE-4300-9BCE-6B6C4C6681A8}']
    function GetEnabled: Boolean;
    function GetFontFamilies: TArray<string>;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForceHeight: Boolean;
    function GetHalfLeading: Boolean;
    function GetHeightMultiplier: Single;
    function GetLeading: Single;
    function IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForceHeight(const AValue: Boolean);
    procedure SetHalfLeading(const AValue: Boolean);
    procedure SetHeightMultiplier(const AValue: Single);
    procedure SetLeading(const AValue: Single);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FontFamilies: TArray<string> read GetFontFamilies write SetFontFamilies;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStyle: TSkFontStyle read GetFontStyle write SetFontStyle;
    property ForceHeight: Boolean read GetForceHeight write SetForceHeight;
    property HalfLeading: Boolean read GetHalfLeading write SetHalfLeading;
    property HeightMultiplier: Single read GetHeightMultiplier write SetHeightMultiplier;
    property Leading: Single read GetLeading write SetLeading;
  end;

  { TSkStrutStyle }

  TSkStrutStyle = class(TSkObject, ISkStrutStyle)
  strict private
    function GetEnabled: Boolean;
    function GetFontFamilies: TArray<string>;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForceHeight: Boolean;
    function GetHalfLeading: Boolean;
    function GetHeightMultiplier: Single;
    function GetLeading: Single;
    function IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForceHeight(const AValue: Boolean);
    procedure SetHalfLeading(const AValue: Boolean);
    procedure SetHeightMultiplier(const AValue: Single);
    procedure SetLeading(const AValue: Single);
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkStrutStyle(...) __SkCreate(TSkStrutStyle, ISkStrutStyle, __VA_ARGS__)'}

  { ISkParagraphStyle }

  ISkParagraphStyle = interface(ISkObject)
    ['{A0D8108C-233E-4DF0-B728-6961730E57B9}']
    procedure DisableHinting;
    function GetEllipsis: string;
    function GetHeight: Single;
    function GetMaxLines: NativeUInt;
    function GetStrutStyle: ISkStrutStyle;
    function GetTextAlign: TSkTextAlign;
    function GetTextDirection: TSkTextDirection;
    function GetTextHeightBehaviors: TSkTextHeightBehaviors;
    function GetTextStyle: ISkTextStyle;
    procedure SetEllipsis(const AValue: string);
    procedure SetHeight(const AValue: Single);
    procedure SetMaxLines(const AValue: NativeUInt);
    procedure SetStrutStyle(AValue: ISkStrutStyle);
    procedure SetTextAlign(const AValue: TSkTextAlign);
    procedure SetTextDirection(const AValue: TSkTextDirection);
    procedure SetTextHeightBehaviors(const AValue: TSkTextHeightBehaviors);
    procedure SetTextStyle(AValue: ISkTextStyle);
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
  strict private
    procedure DisableHinting;
    function GetEllipsis: string;
    function GetHeight: Single;
    function GetMaxLines: NativeUInt;
    function GetStrutStyle: ISkStrutStyle;
    function GetTextAlign: TSkTextAlign;
    function GetTextDirection: TSkTextDirection;
    function GetTextHeightBehaviors: TSkTextHeightBehaviors;
    function GetTextStyle: ISkTextStyle;
    procedure SetEllipsis(const AValue: string);
    procedure SetHeight(const AValue: Single);
    procedure SetMaxLines(const AValue: NativeUInt);
    procedure SetStrutStyle(AValue: ISkStrutStyle);
    procedure SetTextAlign(const AValue: TSkTextAlign);
    procedure SetTextDirection(const AValue: TSkTextDirection);
    procedure SetTextHeightBehaviors(const AValue: TSkTextHeightBehaviors);
    procedure SetTextStyle(AValue: ISkTextStyle);
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkParagraphStyle(...) __SkCreate(TSkParagraphStyle, ISkParagraphStyle, __VA_ARGS__)'}

  { ISkTextStyle }

  ISkTextStyle = interface(ISkObject)
    ['{36B7F057-D721-4FDF-84C0-AF0864732F29}']
    procedure AddFontFeature(const AFeature: string; const AValue: Integer);
    procedure AddShadow(const AShadow: TSkTextShadow);
    procedure ClearBackgroundColor;
    procedure ClearForegroundColor;
    function GetBackground: ISkPaint;
    function GetColor: TAlphaColor;
    function GetDecorationColor: TAlphaColor;
    function GetDecorations: TSkTextDecorations;
    function GetDecorationStyle: TSkTextDecorationStyle;
    function GetDecorationThickness: Single;
    function GetFontFamilies: TArray<string>;
    function GetFontMetrics: TSkFontMetrics;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForeground: ISkPaint;
    function GetHalfLeading: Boolean;
    function GetHeightMultiplier: Single;
    function GetLetterSpacing: Single;
    function GetLocale: string;
    function GetWordSpacing: Single;
    function IsEqual(const ATextStyle: ISkTextStyle): Boolean;
    procedure ResetFontFeatures;
    procedure ResetShadows;
    procedure SetBackgroundColor(const APaint: ISkPaint);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetDecorationColor(const AValue: TAlphaColor);
    procedure SetDecorations(const AValue: TSkTextDecorations);
    procedure SetDecorationStyle(const AValue: TSkTextDecorationStyle);
    procedure SetDecorationThickness(const AValue: Single);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForegroundColor(const APaint: ISkPaint);
    procedure SetHalfLeading(const AValue: Boolean);
    procedure SetHeightMultiplier(const AValue: Single);
    procedure SetLetterSpacing(const AValue: Single);
    procedure SetLocale(const AValue: string);
    procedure SetWordSpacing(const AValue: Single);
    property Color: TAlphaColor read GetColor write SetColor;
    property DecorationColor: TAlphaColor read GetDecorationColor write SetDecorationColor;
    property Decorations: TSkTextDecorations read GetDecorations write SetDecorations;
    property DecorationStyle: TSkTextDecorationStyle read GetDecorationStyle write SetDecorationStyle;
    property DecorationThickness: Single read GetDecorationThickness write SetDecorationThickness;
    property FontFamilies: TArray<string> read GetFontFamilies write SetFontFamilies;
    property FontMetrics: TSkFontMetrics read GetFontMetrics;
    property FontSize: Single read GetFontSize write SetFontSize;
    property FontStyle: TSkFontStyle read GetFontStyle write SetFontStyle;
    property HalfLeading: Boolean read GetHalfLeading write SetHalfLeading;
    property HeightMultiplier: Single read GetHeightMultiplier write SetHeightMultiplier;
    property LetterSpacing: Single read GetLetterSpacing write SetLetterSpacing;
    property Locale: string read GetLocale write SetLocale;
    property WordSpacing: Single read GetWordSpacing write SetWordSpacing;
  end;

  { TSkTextStyle }

  TSkTextStyle = class(TSkObject, ISkTextStyle)
  strict private
    procedure AddFontFeature(const AFeature: string; const AValue: Integer);
    procedure AddShadow(const AShadow: TSkTextShadow);
    procedure ClearBackgroundColor;
    procedure ClearForegroundColor;
    function GetBackground: ISkPaint;
    function GetColor: TAlphaColor;
    function GetDecorationColor: TAlphaColor;
    function GetDecorations: TSkTextDecorations;
    function GetDecorationStyle: TSkTextDecorationStyle;
    function GetDecorationThickness: Single;
    function GetFontFamilies: TArray<string>;
    function GetFontMetrics: TSkFontMetrics;
    function GetFontSize: Single;
    function GetFontStyle: TSkFontStyle;
    function GetForeground: ISkPaint;
    function GetHalfLeading: Boolean;
    function GetHeightMultiplier: Single;
    function GetLetterSpacing: Single;
    function GetLocale: string;
    function GetWordSpacing: Single;
    function IsEqual(const ATextStyle: ISkTextStyle): Boolean;
    procedure ResetFontFeatures;
    procedure ResetShadows;
    procedure SetBackgroundColor(const APaint: ISkPaint);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetDecorationColor(const AValue: TAlphaColor);
    procedure SetDecorations(const AValue: TSkTextDecorations);
    procedure SetDecorationStyle(const AValue: TSkTextDecorationStyle);
    procedure SetDecorationThickness(const AValue: Single);
    procedure SetFontFamilies(const AValue: TArray<string>);
    procedure SetFontSize(const AValue: Single);
    procedure SetFontStyle(const AValue: TSkFontStyle);
    procedure SetForegroundColor(const APaint: ISkPaint);
    procedure SetHalfLeading(const AValue: Boolean);
    procedure SetHeightMultiplier(const AValue: Single);
    procedure SetLetterSpacing(const AValue: Single);
    procedure SetLocale(const AValue: string);
    procedure SetWordSpacing(const AValue: Single);
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkTextStyle(...) __SkCreate(TSkTextStyle, ISkTextStyle, __VA_ARGS__)'}

  { ISkTypefaceFontProvider }

  ISkTypefaceFontProvider = interface(ISkReferenceCounted)
    ['{04D29164-65B8-4733-A742-9FBE0B2178A2}']
    procedure RegisterTypeface(const ATypeface: ISkTypeface); overload;
    procedure RegisterTypeface(const ATypeface: ISkTypeface; const AFamilyName: string); overload;
  end;

  { TSkTypefaceFontProvider }

  TSkTypefaceFontProvider = class(TSkReferenceCounted, ISkTypefaceFontProvider)
  strict protected
    procedure RegisterTypeface(const ATypeface: ISkTypeface); overload; virtual;
    procedure RegisterTypeface(const ATypeface: ISkTypeface; const AFamilyName: string); overload; virtual;
  public
    constructor Create;
  end;

  {$HPPEMIT END '#define SkTypefaceFontProvider(...) __SkCreate(TSkTypefaceFontProvider, ISkTypefaceFontProvider, __VA_ARGS__)'}

  { ISkResourceProvider }

  ISkResourceProvider = interface(ISkReferenceCounted)
    ['{0CC1E668-7D3A-4E7E-9F08-F967499E8381}']
  end;

  { TSkResourceProviderBaseClass }

  TSkResourceProviderBaseClass = class abstract(TSkReferenceCounted, ISkResourceProvider)
  strict private
    class constructor Create;
    class function load_proc(context: Pointer; const path, name: MarshaledAString): sk_data_t; cdecl; static;
  strict protected
    function Load(const APath, AName: string): TBytes; virtual; abstract;
  public
    constructor Create(const APredecode: Boolean = False);
  end;

  { ISkFileResourceProvider }

  ISkFileResourceProvider = interface(ISkResourceProvider)
    ['{2B7EFDD2-9E20-4E50-ACFF-FFBCB5E532A1}']
  end;

  { TSkFileResourceProvider }

  TSkFileResourceProvider = class(TSkResourceProviderBaseClass, ISkFileResourceProvider)
  strict private
    FBaseDir: string;
  strict protected
    function Load(const APath, AName: string): TBytes; override;
  public
    constructor Create(const ABaseDir: string; const APredecode: Boolean = False);
  end;

  {$HPPEMIT END '#define SkFileResourceProvider(...) __SkCreate(TSkFileResourceProvider, ISkResourceProvider, __VA_ARGS__)'}

  { ISkShaper }

  ISkShaper = interface(ISkObject)
    ['{2FDC50D7-2024-45A3-B281-6DD6904FC301}']
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob; overload;
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF; out AEndPoint: TPointF): ISkTextBlob; overload;
  end;

  { TSkShaper }

  TSkShaper = class(TSkObject, ISkShaper)
  strict private
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob; overload;
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF; out AEndPoint: TPointF): ISkTextBlob; overload;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkShaper(...) __SkCreate(TSkShaper, ISkShaper, __VA_ARGS__)'}

  { TSkSVGCanvas }

  TSkSVGCanvas = class(TSkCanvas)
  strict private
    FHolder: ISkObject;
  public
     class function Make(const ABounds: TRectF; const AStream: TStream; const AFlags: TSkSVGCanvasFlags = []): ISkCanvas; static;
  end;

  { ISkSVGDOM }

  ISkSVGDOM = interface(ISkReferenceCounted)
    ['{5CF1CAA2-825B-4538-A64C-522134DB0415}']
    function FindNodeById(const AId: string): ISkSVGNode;
    function GetRoot: ISkSVGSVG;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const ASize: TSizeF);
    property Root: ISkSVGSVG read GetRoot;
  end;

  { TSkSVGDOM }

  TSkSVGDOM = class(TSkReferenceCounted, ISkSVGDOM)
  strict private
    FRoot: ISkSVGSVG;
    function GetRoot: ISkSVGSVG;
  public
    function FindNodeById(const AId: string): ISkSVGNode;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const ASize: TSizeF);
    property Root: ISkSVGSVG read GetRoot;
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil; const AFontProvider: ISkTypefaceFontProvider = nil): ISkSVGDOM; static;
    class function MakeFromFile(const AFileName: string; const AFontProvider: ISkTypefaceFontProvider = nil): ISkSVGDOM; static;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil; const AFontProvider: ISkTypefaceFontProvider = nil): ISkSVGDOM; static;
  end;

  { ISkSVGNode }

  ISkSVGNode = interface(ISkReferenceCounted)
    ['{44A79F6C-4A16-49BA-937D-D892BA5FB453}']
    function TrySetAttribute(const AName, AValue: string): Boolean;
  end;

  { TSkSVGNode }

  TSkSVGNode = class(TSkReferenceCounted, ISkSVGNode)
  public
    function TrySetAttribute(const AName, AValue: string): Boolean;
  end;

  { ISkSVGSVG }

  ISkSVGSVG = interface(ISkSVGNode)
    ['{4DC9309C-664F-4997-B826-B65347C63944}']
    function GetHeight: TSkSVGLength;
    function GetIntrinsicSize(const AViewPort: TSizeF; const ADPI: Single = 90): TSizeF;
    function GetPreserveAspectRatio: TSkSVGPreserveAspectRatio;
    function GetWidth: TSkSVGLength;
    function GetX: TSkSVGLength;
    function GetY: TSkSVGLength;
    procedure SetHeight(const AValue: TSkSVGLength);
    procedure SetPreserveAspectRatio(const AValue: TSkSVGPreserveAspectRatio);
    procedure SetViewBox(const AViewBox: TRectF);
    procedure SetWidth(const AValue: TSkSVGLength);
    procedure SetX(const AValue: TSkSVGLength);
    procedure SetY(const AValue: TSkSVGLength);
    function TryGetViewBox(out AViewBox: TRectF): Boolean;
    property Height: TSkSVGLength read GetHeight write SetHeight;
    property PreserveAspectRatio: TSkSVGPreserveAspectRatio read GetPreserveAspectRatio write SetPreserveAspectRatio;
    property Width: TSkSVGLength read GetWidth write SetWidth;
    property X: TSkSVGLength read GetX write SetX;
    property Y: TSkSVGLength read GetY write SetY;
  end;

  { TSkSVGSVG }

  TSkSVGSVG = class(TSkSVGNode, ISkSVGSVG)
  strict private
    function GetHeight: TSkSVGLength;
    function GetIntrinsicSize(const AViewPort: TSizeF; const ADPI: Single = 90): TSizeF;
    function GetPreserveAspectRatio: TSkSVGPreserveAspectRatio;
    function GetWidth: TSkSVGLength;
    function GetX: TSkSVGLength;
    function GetY: TSkSVGLength;
    procedure SetHeight(const AValue: TSkSVGLength);
    procedure SetPreserveAspectRatio(const AValue: TSkSVGPreserveAspectRatio);
    procedure SetViewBox(const AViewBox: TRectF);
    procedure SetWidth(const AValue: TSkSVGLength);
    procedure SetX(const AValue: TSkSVGLength);
    procedure SetY(const AValue: TSkSVGLength);
    function TryGetViewBox(out AViewBox: TRectF): Boolean;
  end;

  TSkUnicodeBreakIteratorElemEnumerator = TSkEnumerable<TSkUnicodeBreakIteratorElem>.TEnumerator;

  { ISkUnicodeBreakIterator }

  ISkUnicodeBreakIterator = interface(ISkEnumerable)
    ['{A21F8622-2A3B-413D-BF03-18D393DEF49A}']
    function GetCurrent: TSkUnicodeBreakIteratorElem;
    function GetEnumerator: TSkUnicodeBreakIteratorElemEnumerator;
  end;

  { ISkUnicode }

  ISkUnicode = interface(ISkObject)
    ['{DFD6FE56-0C2E-45C9-BA18-95C750981554}']
    procedure ForEachBidiRegion(const AText: string; const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
    procedure ForEachBreak(const AText: string; const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
    procedure ForEachCodepoint(const AText: string; const AProc: TSkUnicodeCodepointProc);
    function GetBreakIterator(const AType: TSkBreakType; const AText: string): ISkUnicodeBreakIterator;
    function GetBreakIteratorUTF8(const AType: TSkBreakType; const AText: UTF8String): ISkUnicodeBreakIterator;
    function GetBreaks(const AText: string; const AType: TSkBreakType): TArray<string>;
  end;

  { TSkUnicode }

  TSkUnicode = class(TSkObject, ISkUnicode)
  strict private type
    TUnicodeBreakIterator = class(TSkEnumerable<TSkUnicodeBreakIteratorElem>, ISkUnicodeBreakIterator)
    strict private
      FCurrent: TSkUnicodeBreakIteratorElem;
    public
      constructor Create(const AUnicode: ISkUnicode; const AType: TSkBreakType; const AText: string); overload;
      constructor Create(const AUnicode: ISkUnicode; const AType: TSkBreakType; const AText: UTF8String); overload;
      function GetCurrent: TSkUnicodeBreakIteratorElem; override;
      function MoveNext: Boolean; override;
      class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
    end;

  strict private
    procedure ForEachBidiRegion(const AText: string; const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
    procedure ForEachBreak(const AText: string; const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
    procedure ForEachCodepoint(const AText: string; const AProc: TSkUnicodeCodepointProc);
    function GetBreakIterator(const AType: TSkBreakType; const AText: string): ISkUnicodeBreakIterator;
    function GetBreakIteratorUTF8(const AType: TSkBreakType; const AText: UTF8String): ISkUnicodeBreakIterator;
    function GetBreaks(const AText: string; const AType: TSkBreakType): TArray<string>;
    class procedure bidi_region_proc(start, &end: int32_t; level: uint8_t; context: Pointer); cdecl; static;
    class procedure break_proc(position, status: int32_t; context: Pointer); cdecl; static;
    class procedure codepoint_proc(unichar: sk_unichar_t; start, &end: int32_t; context: Pointer); cdecl; static;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  {$HPPEMIT END '#define SkUnicode(...) __SkCreate(TSkUnicode, ISkUnicode, __VA_ARGS__)'}

function ExtensionToEncodedImageFormat(const AValue: string): TSkEncodedImageFormat;

const
  SkBytesPerPixel: array[TSkColorType] of Integer = (
    { Unknown           }  0,
    { Alpha8            }  1,
    { RGB565            }  2,
    { ARGB4444          }  2,
    { RGBA8888          }  4,
    { RGB888X           }  4,
    { BGRA8888          }  4,
    { RGBA1010102       }  4,
    { BGRA1010102       }  4,
    { RGB101010X        }  4,
    { BGR101010X        }  4,
    { Gray8             }  1,
    { RGBAF16Normalized }  8,
    { RGBAF16           }  8,
    { RGBAF32           } 16,
    { RG88              }  2,
    { AlphaF16          }  2,
    { RGF16             }  4,
    { Alpha16           }  2,
    { RG1616            }  4,
    { RGBA16161616      }  8,
    { SRGBA8888         }  4,
    { R8                }  1
  );

  SkColorMatrixIdentity: TSkColorMatrix = (M11: 1; M12: 0; M13: 0; M14: 0; M15: 0;
                                           M21: 0; M22: 1; M23: 0; M24: 0; M25: 0;
                                           M31: 0; M32: 0; M33: 1; M34: 0; M35: 0;
                                           M41: 0; M42: 0; M43: 0; M44: 1; M45: 0);

  SkColorSpaceTransferFunctionHLG      : TSkColorSpaceTransferFunction = (G: -3; A: 2; B: 2; C: 1 / 0.17883277; D: 0.28466892; E: 0.55991073; F: 0);
  SkColorSpaceTransferFunctionLinear   : TSkColorSpaceTransferFunction = (G: 1; A: 1; B: 0; C: 0; D: 0; E: 0; F: 0);
  SkColorSpaceTransferFunctionPQ       : TSkColorSpaceTransferFunction = (G: -2; A: -107 / 128; B: 1; C: 32 / 2523; D: 2413 / 128; E: -2392 / 128; F: 8192 / 1305);
  SkColorSpaceTransferFunctionRec2020  : TSkColorSpaceTransferFunction = (G: 2.22222; A: 0.909672; B: 0.0903276; C: 0.222222; D: 0.0812429; E: 0; F: 0);
  SkColorSpaceTransferFunctionSRGB     : TSkColorSpaceTransferFunction = (G: 2.4; A: 1 / 1.055; B: 0.055 / 1.055; C: 1 / 12.92; D: 0.04045; E: 0; F: 0);
  SkColorSpaceTransferFunctionTwoDotTwo: TSkColorSpaceTransferFunction = (G: 2.2; A: 1; B: 0; C: 0; D: 0; E: 0; F: 0);

  SkColorSpaceXyzAdobeRGB : TSkColorSpaceXyz = (M11: $9C18 * TSkColorSpaceXyz.FixedToFloat; M12: $348D * TSkColorSpaceXyz.FixedToFloat; M13: $2631 * TSkColorSpaceXyz.FixedToFloat;
                                                M21: $4FA5 * TSkColorSpaceXyz.FixedToFloat; M22: $A02C * TSkColorSpaceXyz.FixedToFloat; M23: $102F * TSkColorSpaceXyz.FixedToFloat;
                                                M31: $04FC * TSkColorSpaceXyz.FixedToFloat; M32: $0F95 * TSkColorSpaceXyz.FixedToFloat; M33: $BE9C * TSkColorSpaceXyz.FixedToFloat);

  SkColorSpaceXyzDisplayP3: TSkColorSpaceXyz = (M11:  0.515102;   M12: 0.291965;  M13: 0.157153;
                                                M21:  0.241182;   M22: 0.692236;  M23: 0.0665819;
                                                M31: -0.00104941; M32: 0.0418818; M33: 0.784378);

  SkColorSpaceXyzIdentity : TSkColorSpaceXyz = (M11: 1; M12: 0; M13: 0;
                                                M21: 0; M22: 1; M23: 0;
                                                M31: 0; M32: 0; M33: 1);

  SkColorSpaceXyzRec2020  : TSkColorSpaceXyz = (M11:  0.673459;   M12: 0.165661;  M13: 0.125100;
                                                M21:  0.279033;   M22: 0.675338;  M23: 0.0456288;
                                                M31: -0.00193139; M32: 0.0299794; M33: 0.797162);

  SkColorSpaceXyzSRGB     : TSkColorSpaceXyz = (M11: $6FA2 * TSkColorSpaceXyz.FixedToFloat; M12: $6299 * TSkColorSpaceXyz.FixedToFloat; M13: $24A0 * TSkColorSpaceXyz.FixedToFloat;
                                                M21: $38F5 * TSkColorSpaceXyz.FixedToFloat; M22: $B785 * TSkColorSpaceXyz.FixedToFloat; M23: $0F84 * TSkColorSpaceXyz.FixedToFloat;
                                                M31: $0390 * TSkColorSpaceXyz.FixedToFloat; M32: $18DA * TSkColorSpaceXyz.FixedToFloat; M33: $B6CF * TSkColorSpaceXyz.FixedToFloat);

  SkCubicResamplerCatmullRom: TSkCubicResampler = (B: 0; C: 1 / 2);
  SkCubicResamplerMitchell  : TSkCubicResampler = (B: 1 / 3; C: 1 / 3);

  SkFontSlantRegular = TSkFontSlant.Upright;

  SkFontStyleBold      : TSkFontStyle = (Weight: Ord(TSkFontWeight.Bold); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Upright);
  SkFontStyleBoldItalic: TSkFontStyle = (Weight: Ord(TSkFontWeight.Bold); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Italic);
  SkFontStyleItalic    : TSkFontStyle = (Weight: Ord(TSkFontWeight.Normal); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Italic);
  SkFontStyleNormal    : TSkFontStyle = (Weight: Ord(TSkFontWeight.Normal); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Upright);

  SkFontWeightDemiBold   = TSkFontWeight.SemiBold;
  SkFontWeightHairline   = TSkFontWeight.Thin;
  SkFontWeightHeavy      = TSkFontWeight.Black;
  SkFontWeightRegular    = TSkFontWeight.Normal;
  SkFontWeightUltraBlack = TSkFontWeight.ExtraBlack;
  SkFontWeightUltraBold  = TSkFontWeight.ExtraBold;
  SkFontWeightUltraLight = TSkFontWeight.ExtraLight;

  SkFontWidthRegular = TSkFontWidth.Normal;

  SkNative32ColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF};

  SkRotationScaleMatrixIdentity: TSkRotationScaleMatrix = (SCosinus   : 1; SSinus     : 0;
                                                           TranslateX : 0; TranslateY : 0);

  SkSamplingOptionsHigh  : TSkSamplingOptions = (MaxAnisotropic: 0; UseCubic: True; Cubic: (B: 1 / 3; C: 1 / 3); Filter: TSkFilterMode.Nearest; Mipmap: TSkMipmapMode.None);
  SkSamplingOptionsLow   : TSkSamplingOptions = (MaxAnisotropic: 0; UseCubic: False; Cubic: (B: 0; C: 0); Filter: TSkFilterMode.Nearest; Mipmap: TSkMipmapMode.Nearest);
  SkSamplingOptionsMedium: TSkSamplingOptions = (MaxAnisotropic: 0; UseCubic: False; Cubic: (B: 0; C: 0); Filter: TSkFilterMode.Linear; Mipmap: TSkMipmapMode.Nearest);

  SkShiftPerPixel: array[TSkColorType] of Integer = (
    { Unknown           } 0,
    { Alpha8            } 0,
    { RGB565            } 1,
    { ARGB4444          } 1,
    { RGBA8888          } 2,
    { RGB888X           } 2,
    { BGRA8888          } 2,
    { RGBA1010102       } 2,
    { BGRA1010102       } 2,
    { RGB101010X        } 2,
    { BGR101010X        } 2,
    { Gray8             } 0,
    { RGBAF16Normalized } 3,
    { RGBAF16           } 3,
    { RGBAF32           } 4,
    { RG88              } 1,
    { AlphaF16          } 1,
    { RGF16             } 2,
    { Alpha16           } 1,
    { RG1616            } 2,
    { RGBA16161616      } 3,
    { SRGBA8888         } 2,
    { R8                } 0
  );

  SFileNameIsEmpty   = 'File name cannot be empty';
  SInvalidOperation  = 'Operation is invalid';
  SOutOfMemory       = 'Out of memory';
  SParamIsEmpty      = 'Parameter %s cannot be empty';
  SParamIsNil        = 'Parameter %s cannot be nil';
  SParamOutOfRange   = 'Parameter %s out of range (%d). Must be >= %d and < %d';
  SParamSizeIsOdd    = 'Parameter %s size cannot be odd';
  SParamSizeMismatch = 'Parameter %s size mismatch';

implementation

uses
  {$IFDEF FPC}
  { FPC }
  DateUtils,
  Generics.Collections,
  Math;
  {$ELSE}
  { Delphi }
  System.Generics.Collections,
  System.Math,
  System.TimeSpan;
  {$ENDIF}

type
  { TSkBindings }

  TSkBindings = record
    class function SafeCreate<T: TSkObject, constructor>(const AHandle: sk_handle_t; const AOwnsHandle: Boolean = True): T; static;
    class function SafeHandle(const AObject: ISkObject): sk_handle_t; static;
  end;

  { TSkProcWrapper<T> }

  TSkProcWrapper<T> = record
    Proc: T;
  end;

  TSkImageRasterReleaseProcWrapper = TSkProcWrapper<TSkImageRasterReleaseProc>;
  PSkImageRasterReleaseProcWrapper = ^TSkImageRasterReleaseProcWrapper;

  TSkImageTextureReleaseProcWrapper = TSkProcWrapper<TSkImageTextureReleaseProc>;
  PSkImageTextureReleaseProcWrapper = ^TSkImageTextureReleaseProcWrapper;

  TSkSurfaceRasterReleaseProcWrapper = TSkProcWrapper<TSkSurfaceRasterReleaseProc>;
  PSkSurfaceRasterReleaseProcWrapper = ^TSkSurfaceRasterReleaseProcWrapper;

  { ISkData }

  ISkData = interface(ISkNonVirtualReferenceCounted)
    ['{7C1D6B87-6DCC-4398-B5F1-954E60BDB085}']
  end;

  { TSkData }

  TSkData = class(TSkNonVirtualReferenceCounted, ISkData)
  public
    class function MakeFromBytes(const ABytes: TBytes): ISkData; static;
    class procedure __RefHandle(const AHandle: sk_handle_t); override;
    class procedure __UnrefHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkStream }

  ISkStream = interface(ISkObject)
    ['{E99AB460-4CEE-4F50-85A7-29896A1407AC}']
  end;

  { TSkStreamAdapter }

  TSkStreamAdapter = class(TSkObject, ISkStream)
  strict private
    class constructor Create;
    class function get_length_proc(context: Pointer): size_t; cdecl; static;
    class function get_position_proc(context: Pointer): size_t; cdecl; static;
    class function read_proc(context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl; static;
    class function seek_proc(context: Pointer; position: size_t): _bool; cdecl; static;
  public
    constructor Create(const AStream: TStream);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkWStream }

  ISkWStream = interface(ISkObject)
    ['{C20D663E-459B-495A-9E19-017FE7A08227}']
  end;

  { TSkWStreamAdapter }

  TSkWStreamAdapter = class(TSkObject, ISkWStream)
  strict private
    class constructor Create;
    class function write_proc(context: Pointer; const buffer: Pointer; size: size_t): _bool; cdecl; static;
  public
    constructor Create(const AStream: TStream);
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

  { ISkString }

  ISkString = interface(ISkObject)
    ['{2648646C-8DC1-4289-A9E2-2F658BB11C1C}']
    function GetText: string;
    property Text: string read GetText;
  end;

  { TSkString }

  TSkString = class(TSkObject, ISkString)
  strict private
    function GetText: string;
  public
    constructor Create;
    class procedure __DestroyHandle(const AHandle: sk_handle_t); override;
  end;

{$REGION 'FPC Compatibility'}
{$IFDEF FPC}

{ TMatrix }

class function TMatrix.CreateScaling(const AScaleX, AScaleY: Single): TMatrix;
begin
  Result := TMatrix.Identity;
  Result.m11 := AScaleX;
  Result.m22 := AScaleY;
end;

class function TMatrix.CreateTranslation(const ADeltaX,
  ADeltaY: Single): TMatrix;
begin
  Result := TMatrix.Identity;
  Result.m31 := ADeltaX;
  Result.m32 := ADeltaY;
end;

class operator TMatrix.Multiply(const AMatrix1, AMatrix2: TMatrix): TMatrix;
begin
  Result.m11 := AMatrix1.m11 * AMatrix2.m11 + AMatrix1.m12 * AMatrix2.m21 + AMatrix1.m13 * AMatrix2.m31;
  Result.m12 := AMatrix1.m11 * AMatrix2.m12 + AMatrix1.m12 * AMatrix2.m22 + AMatrix1.m13 * AMatrix2.m32;
  Result.m13 := AMatrix1.m11 * AMatrix2.m13 + AMatrix1.m12 * AMatrix2.m23 + AMatrix1.m13 * AMatrix2.m33;
  Result.m21 := AMatrix1.m21 * AMatrix2.m11 + AMatrix1.m22 * AMatrix2.m21 + AMatrix1.m23 * AMatrix2.m31;
  Result.m22 := AMatrix1.m21 * AMatrix2.m12 + AMatrix1.m22 * AMatrix2.m22 + AMatrix1.m23 * AMatrix2.m32;
  Result.m23 := AMatrix1.m21 * AMatrix2.m13 + AMatrix1.m22 * AMatrix2.m23 + AMatrix1.m23 * AMatrix2.m33;
  Result.m31 := AMatrix1.m31 * AMatrix2.m11 + AMatrix1.m32 * AMatrix2.m21 + AMatrix1.m33 * AMatrix2.m31;
  Result.m32 := AMatrix1.m31 * AMatrix2.m12 + AMatrix1.m32 * AMatrix2.m22 + AMatrix1.m33 * AMatrix2.m32;
  Result.m33 := AMatrix1.m31 * AMatrix2.m13 + AMatrix1.m32 * AMatrix2.m23 + AMatrix1.m33 * AMatrix2.m33;
end;

{ TMatrix3D }

class function TMatrix3D.CreateScaling(const AScale: TPoint3D): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m11 := AScale.X;
  Result.m22 := AScale.Y;
  Result.m33 := AScale.Z;
end;

class function TMatrix3D.CreateTranslation(
  const ATranslation: TPoint3D): TMatrix3D;
begin
  Result := TMatrix3D.Identity;
  Result.m41 := ATranslation.X;
  Result.m42 := ATranslation.Y;
  Result.m43 := ATranslation.Z;
end;

{$ENDIF}
{$ENDREGION}

function DateTimeToSkDateTime(const AValue: TDateTime): sk_datetime_t; inline;
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
  Result.time_zone_minutes := {$IFDEF FPC}GetLocalTimeOffset{$ELSE}Round(TTimeZone.Local.UtcOffset.TotalMinutes){$ENDIF};
  Result.year              := LYear;
  Result.month             := LMonth;
  Result.day_of_week       := DayOfWeek(AValue) - 1;
  Result.day               := LDay;
  Result.hour              := LHour;
  Result.minute            := LMinute;
  Result.second            := LSecond;
end;

procedure MapContextOptions(const ASrc: TGrContextOptions;
  out ADest: gr_contextoptions_t); inline;
begin
  ADest.buffer_map_threshold              := ASrc.BufferMapThreshold;
  ADest.do_manual_mipmapping              := ASrc.DoManualMipmapping;
  ADest.allow_path_mask_caching           := ASrc.AllowPathMaskCaching;
  ADest.glyph_cache_texture_maximum_bytes := ASrc.GlyphCacheTextureMaximumBytes;
  ADest.avoid_stencil_buffers             := ASrc.AvoidStencilBuffers;
  ADest.runtime_program_cache_size        := ASrc.RuntimeProgramCacheSize;
  ADest.persistent_cache                  := TSkBindings.SafeHandle(ASrc.PersistentCache);
  ADest.shader_cache_strategy             := gr_shadercachestrategy_t(ASrc.ShaderCacheStrategy);
  ADest.shader_error_handler              := TSkBindings.SafeHandle(ASrc.ShaderErrorHandler);
end;

procedure MapFrame(const ASrc: TSkFrame; out ADest: sk_frame_t); inline;
begin
  ADest.pixmap   := ASrc.Pixmap.Handle;
  ADest.duration := ASrc.Duration;
end;

procedure MapImageInfo(const ASrc: TSkImageInfo;
  out ADest: sk_imageinfo_t); inline;
begin
  ADest.width       := ASrc.Width;
  ADest.height      := ASrc.Height;
  ADest.color_type  := sk_colortype_t(ASrc.ColorType);
  ADest.alpha_type  := sk_alphatype_t(ASrc.AlphaType);
  ADest.color_space := TSkBindings.SafeHandle(ASrc.ColorSpace);
end;

procedure MapSurfaceProperties(const ASrc: TSkSurfaceProperties;
  out ADest: sk_surfaceprops_t); inline;
begin
  ADest.flags          := Byte(ASrc.Flags);
  ADest.pixel_geometry := sk_pixelgeometry_t(ASrc.PixelGeometry);
end;

{ TSkObject }

{$IFNDEF AUTOREFCOUNT}

procedure TSkObject.AfterConstruction;
begin
  inherited;
  AtomicDecrement(FRefCount);
end;

procedure TSkObject.BeforeDestruction;
begin
  if {%H-}RefCount <> 0 then
    System.Error(reInvalidPtr);
  inherited;
end;

{$ENDIF}

class constructor TSkObject.Create;
begin
  SkInitialize;
end;

constructor TSkObject.Create(const AHandle: sk_handle_t);
begin
  Wrap(AHandle);
end;

destructor TSkObject.Destroy;
begin
  if FOwnsHandle then
    __DestroyHandle(FHandle);
  inherited;
end;

class destructor TSkObject.Destroy;
begin
  SkFinalize;
end;

function TSkObject.GetHandle: sk_handle_t;
begin
  Result := FHandle;
end;

{$IFNDEF AUTOREFCOUNT}

function TSkObject.GetRefCount: Integer;
begin
  Result := FRefCount and not objDestroyingFlag;
end;

class function TSkObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSkObject(Result).FRefCount := 1;
end;

{$ENDIF}

function TSkObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): {$IFDEF FPC}Longint{$ELSE}HRESULT{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

constructor TSkObject.Wrap(const AHandle: sk_handle_t; const AOwnsHandle: Boolean);
begin
  inherited Create;
  FHandle     := AHandle;
  FOwnsHandle := AOwnsHandle;
end;

function TSkObject._AddRef: {$IFDEF FPC}Longint{$ELSE}Integer{$ENDIF};
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := AtomicIncrement(FRefCount);
  {$ELSE}
  Result := __ObjAddRef;
  {$ENDIF}
  if Result > 1 then
    __RefHandle(FHandle);
end;

function TSkObject._Release: {$IFDEF FPC}Longint{$ELSE}Integer{$ENDIF};
begin
  {$IFNDEF AUTOREFCOUNT}
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    __MarkDestroying(Self);
    Destroy;
  end;
  {$ELSE}
  Result := __ObjRelease;
  {$ENDIF}
  if Result > 0 then
    __UnrefHandle(FHandle);
end;

class procedure TSkObject.__DestroyHandle(const AHandle: sk_handle_t);
begin
  __UnrefHandle(AHandle);
end;

{$IFNDEF AUTOREFCOUNT}

class procedure TSkObject.__MarkDestroying(const Obj);
var
  LRef: Integer;
begin
  repeat
    LRef := TSkObject(Obj).FRefCount;
  until AtomicCmpExchange(TSkObject(Obj).FRefCount, LRef or objDestroyingFlag, LRef) = LRef;
end;

{$ENDIF}

class procedure TSkObject.__RefHandle(const AHandle: sk_handle_t);
begin
end;

class function TSkObject.__ReleaseHandle(const AObject: ISkObject): sk_handle_t;
var
  LObject: TSkObject;
begin
  if not Assigned(AObject) then
    Exit(0);
  LObject := AObject as TSkObject;
  Result  := LObject.GetHandle;
  if not LObject.FOwnsHandle then
    raise ESkException.Create('Current object is not owner of the handle');
  LObject.FOwnsHandle := False;
end;

class procedure TSkObject.__UnrefHandle(const AHandle: sk_handle_t);
begin
end;

{ TSkReferenceCounted }

class procedure TSkReferenceCounted.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_refcnt_ref(AHandle);
end;

class procedure TSkReferenceCounted.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_refcnt_unref(AHandle);
end;

{ TSkEnumerable<T>.TEnumerator }

constructor TSkEnumerable<T>.TEnumerator.Create(const AEnumerable: TSkEnumerable<T>);
begin
  inherited Create;
  FEnumerable := AEnumerable;
end;

function TSkEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := FEnumerable.GetCurrent;
end;

function TSkEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerable.MoveNext;
end;

{ TSkEnumerable<T> }

function TSkEnumerable<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

{ TSkColorMatrix }

constructor TSkColorMatrix.Create(const AM11, AM12, AM13, AM14, AM15, AM21,
  AM22, AM23, AM24, AM25, AM31, AM32, AM33, AM34, AM35, AM41, AM42, AM43, AM44,
  AM45: Single);
begin
  M11 := AM11;
  M12 := AM12;
  M13 := AM13;
  M14 := AM14;
  M15 := AM15;
  M21 := AM21;
  M22 := AM22;
  M23 := AM23;
  M24 := AM24;
  M25 := AM25;
  M31 := AM31;
  M32 := AM32;
  M33 := AM33;
  M34 := AM34;
  M35 := AM35;
  M41 := AM41;
  M42 := AM42;
  M43 := AM43;
  M44 := AM44;
  M45 := AM45;
end;

class function TSkColorMatrix.CreateSaturation(
  const ASaturation: Single): TSkColorMatrix;
const
  HueR = 0.213;
  HueG = 0.715;
  HueB = 0.072;
var
  LB: Single;
  LG: Single;
  LR: Single;
begin
  LR := HueR * (1 - ASaturation);
  LG := HueG * (1 - ASaturation);
  LB := HueB * (1 - ASaturation);
  Result.M11 := LR + ASaturation;
  Result.M12 := LG;
  Result.M13 := LB;
  Result.M14 := 0;
  Result.M15 := 0;
  Result.M21 := LR;
  Result.M22 := LG + ASaturation;
  Result.M23 := LB;
  Result.M24 := 0;
  Result.M25 := 0;
  Result.M31 := LR;
  Result.M32 := LG;
  Result.M33 := LB + ASaturation;
  Result.M34 := 0;
  Result.M35 := 0;
  Result.M41 := 0;
  Result.M42 := 0;
  Result.M43 := 0;
  Result.M44 := 1;
  Result.M45 := 0;
end;

class function TSkColorMatrix.CreateScale(const AScaleR, AScaleG, AScaleB,
  AScaleA: Single): TSkColorMatrix;
begin
  Result.M11 := AScaleR;
  Result.M12 := 0;
  Result.M13 := 0;
  Result.M14 := 0;
  Result.M15 := 0;
  Result.M21 := 0;
  Result.M22 := AScaleG;
  Result.M23 := 0;
  Result.M24 := 0;
  Result.M25 := 0;
  Result.M31 := 0;
  Result.M32 := 0;
  Result.M33 := AScaleB;
  Result.M34 := 0;
  Result.M35 := 0;
  Result.M41 := 0;
  Result.M42 := 0;
  Result.M43 := 0;
  Result.M44 := AScaleA;
  Result.M45 := 0;
end;

class operator TSkColorMatrix.Equal(const AColorMatrix1,
  AColorMatrix2: TSkColorMatrix): Boolean;
begin
  Result := (SameValue(AColorMatrix1.M11, AColorMatrix2.M11)) and
            (SameValue(AColorMatrix1.M12, AColorMatrix2.M12)) and
            (SameValue(AColorMatrix1.M13, AColorMatrix2.M13)) and
            (SameValue(AColorMatrix1.M14, AColorMatrix2.M14)) and
            (SameValue(AColorMatrix1.M15, AColorMatrix2.M15)) and
            (SameValue(AColorMatrix1.M21, AColorMatrix2.M21)) and
            (SameValue(AColorMatrix1.M22, AColorMatrix2.M22)) and
            (SameValue(AColorMatrix1.M23, AColorMatrix2.M23)) and
            (SameValue(AColorMatrix1.M24, AColorMatrix2.M24)) and
            (SameValue(AColorMatrix1.M25, AColorMatrix2.M25)) and
            (SameValue(AColorMatrix1.M31, AColorMatrix2.M31)) and
            (SameValue(AColorMatrix1.M32, AColorMatrix2.M32)) and
            (SameValue(AColorMatrix1.M33, AColorMatrix2.M33)) and
            (SameValue(AColorMatrix1.M34, AColorMatrix2.M34)) and
            (SameValue(AColorMatrix1.M35, AColorMatrix2.M35)) and
            (SameValue(AColorMatrix1.M41, AColorMatrix2.M41)) and
            (SameValue(AColorMatrix1.M42, AColorMatrix2.M42)) and
            (SameValue(AColorMatrix1.M43, AColorMatrix2.M43)) and
            (SameValue(AColorMatrix1.M44, AColorMatrix2.M44)) and
            (SameValue(AColorMatrix1.M45, AColorMatrix2.M45));
end;

class function TSkColorMatrix.Identity: TSkColorMatrix;
begin
  Result := SkColorMatrixIdentity;
end;

class operator TSkColorMatrix.Multiply(const AColorMatrix1,
  AColorMatrix2: TSkColorMatrix): TSkColorMatrix;
begin
  Result.M11 := AColorMatrix2.M11 * AColorMatrix1.M11 + AColorMatrix2.M12 * AColorMatrix1.M21 + AColorMatrix2.M13 * AColorMatrix1.M31 + AColorMatrix2.M14 * AColorMatrix1.M41;
  Result.M12 := AColorMatrix2.M11 * AColorMatrix1.M12 + AColorMatrix2.M12 * AColorMatrix1.M22 + AColorMatrix2.M13 * AColorMatrix1.M32 + AColorMatrix2.M14 * AColorMatrix1.M42;
  Result.M13 := AColorMatrix2.M11 * AColorMatrix1.M13 + AColorMatrix2.M12 * AColorMatrix1.M23 + AColorMatrix2.M13 * AColorMatrix1.M33 + AColorMatrix2.M14 * AColorMatrix1.M43;
  Result.M14 := AColorMatrix2.M11 * AColorMatrix1.M14 + AColorMatrix2.M12 * AColorMatrix1.M24 + AColorMatrix2.M13 * AColorMatrix1.M34 + AColorMatrix2.M14 * AColorMatrix1.M44;
  Result.M15 := AColorMatrix2.M11 * AColorMatrix1.M15 + AColorMatrix2.M12 * AColorMatrix1.M25 + AColorMatrix2.M13 * AColorMatrix1.M35 + AColorMatrix2.M14 * AColorMatrix1.M45 + AColorMatrix2.M15;
  Result.M21 := AColorMatrix2.M21 * AColorMatrix1.M11 + AColorMatrix2.M22 * AColorMatrix1.M21 + AColorMatrix2.M23 * AColorMatrix1.M31 + AColorMatrix2.M24 * AColorMatrix1.M41;
  Result.M22 := AColorMatrix2.M21 * AColorMatrix1.M12 + AColorMatrix2.M22 * AColorMatrix1.M22 + AColorMatrix2.M23 * AColorMatrix1.M32 + AColorMatrix2.M24 * AColorMatrix1.M42;
  Result.M23 := AColorMatrix2.M21 * AColorMatrix1.M13 + AColorMatrix2.M22 * AColorMatrix1.M23 + AColorMatrix2.M23 * AColorMatrix1.M33 + AColorMatrix2.M24 * AColorMatrix1.M43;
  Result.M24 := AColorMatrix2.M21 * AColorMatrix1.M14 + AColorMatrix2.M22 * AColorMatrix1.M24 + AColorMatrix2.M23 * AColorMatrix1.M34 + AColorMatrix2.M24 * AColorMatrix1.M44;
  Result.M25 := AColorMatrix2.M21 * AColorMatrix1.M15 + AColorMatrix2.M22 * AColorMatrix1.M25 + AColorMatrix2.M23 * AColorMatrix1.M35 + AColorMatrix2.M24 * AColorMatrix1.M45  + AColorMatrix2.M25;
  Result.M31 := AColorMatrix2.M31 * AColorMatrix1.M11 + AColorMatrix2.M32 * AColorMatrix1.M21 + AColorMatrix2.M33 * AColorMatrix1.M31 + AColorMatrix2.M34 * AColorMatrix1.M41;
  Result.M32 := AColorMatrix2.M31 * AColorMatrix1.M12 + AColorMatrix2.M32 * AColorMatrix1.M22 + AColorMatrix2.M33 * AColorMatrix1.M32 + AColorMatrix2.M34 * AColorMatrix1.M42;
  Result.M33 := AColorMatrix2.M31 * AColorMatrix1.M13 + AColorMatrix2.M32 * AColorMatrix1.M23 + AColorMatrix2.M33 * AColorMatrix1.M33 + AColorMatrix2.M34 * AColorMatrix1.M43;
  Result.M34 := AColorMatrix2.M31 * AColorMatrix1.M14 + AColorMatrix2.M32 * AColorMatrix1.M24 + AColorMatrix2.M33 * AColorMatrix1.M34 + AColorMatrix2.M34 * AColorMatrix1.M44;
  Result.M35 := AColorMatrix2.M31 * AColorMatrix1.M15 + AColorMatrix2.M32 * AColorMatrix1.M25 + AColorMatrix2.M33 * AColorMatrix1.M35 + AColorMatrix2.M34 * AColorMatrix1.M45  + AColorMatrix2.M35;
  Result.M41 := AColorMatrix2.M41 * AColorMatrix1.M11 + AColorMatrix2.M42 * AColorMatrix1.M21 + AColorMatrix2.M43 * AColorMatrix1.M31 + AColorMatrix2.M44 * AColorMatrix1.M41;
  Result.M42 := AColorMatrix2.M41 * AColorMatrix1.M12 + AColorMatrix2.M42 * AColorMatrix1.M22 + AColorMatrix2.M43 * AColorMatrix1.M32 + AColorMatrix2.M44 * AColorMatrix1.M42;
  Result.M43 := AColorMatrix2.M41 * AColorMatrix1.M13 + AColorMatrix2.M42 * AColorMatrix1.M23 + AColorMatrix2.M43 * AColorMatrix1.M33 + AColorMatrix2.M44 * AColorMatrix1.M43;
  Result.M44 := AColorMatrix2.M41 * AColorMatrix1.M14 + AColorMatrix2.M42 * AColorMatrix1.M24 + AColorMatrix2.M43 * AColorMatrix1.M34 + AColorMatrix2.M44 * AColorMatrix1.M44;
  Result.M45 := AColorMatrix2.M41 * AColorMatrix1.M15 + AColorMatrix2.M42 * AColorMatrix1.M25 + AColorMatrix2.M43 * AColorMatrix1.M35 + AColorMatrix2.M44 * AColorMatrix1.M45  + AColorMatrix2.M45;
end;

class operator TSkColorMatrix.NotEqual(const AColorMatrix1,
  AColorMatrix2: TSkColorMatrix): Boolean;
begin
  Result := not (AColorMatrix1 = AColorMatrix2);
end;

{ TSkColorSpaceXyz }

function TSkColorSpaceXyz.Adjoint: TSkColorSpaceXyz;
begin
  Result.M11 :=  (M22 * M33 - M32 * M23);
  Result.M12 := -(M12 * M33 - M32 * M13);
  Result.M13 :=  (M12 * M23 - M22 * M13);
  Result.M21 := -(M21 * M33 - M31 * M23);
  Result.M22 :=  (M11 * M33 - M31 * M13);
  Result.M23 := -(M11 * M23 - M21 * M13);
  Result.M31 :=  (M21 * M32 - M31 * M22);
  Result.M32 := -(M11 * M32 - M31 * M12);
  Result.M33 :=  (M11 * M22 - M21 * M12);
end;

class function TSkColorSpaceXyz.AdobeRGB: TSkColorSpaceXyz;
begin
  Result := SkColorSpaceXyzAdobeRGB;
end;

constructor TSkColorSpaceXyz.Create(const AM11, AM12, AM13, AM21, AM22, AM23,
  AM31, AM32, AM33: Single);
begin
  M11 := AM11;
  M12 := AM12;
  M13 := AM13;
  M21 := AM21;
  M22 := AM22;
  M23 := AM23;
  M31 := AM31;
  M32 := AM32;
  M33 := AM33;
end;

function TSkColorSpaceXyz.Determinant: Single;
begin
  Result := M11 * (M22 * M33 - M32 * M23) -
            M12 * (M21 * M33 - M31 * M23) +
            M13 * (M21 * M32 - M31 * M22);
end;

class function TSkColorSpaceXyz.DisplayP3: TSkColorSpaceXyz;
begin
  Result := SkColorSpaceXyzDisplayP3;
end;

class operator TSkColorSpaceXyz.Equal(const AColorSpaceXyz1,
  AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
begin
  Result := (SameValue(AColorSpaceXyz1.M11, AColorSpaceXyz2.M11)) and
            (SameValue(AColorSpaceXyz1.M12, AColorSpaceXyz2.M12)) and
            (SameValue(AColorSpaceXyz1.M13, AColorSpaceXyz2.M13)) and
            (SameValue(AColorSpaceXyz1.M21, AColorSpaceXyz2.M21)) and
            (SameValue(AColorSpaceXyz1.M22, AColorSpaceXyz2.M22)) and
            (SameValue(AColorSpaceXyz1.M23, AColorSpaceXyz2.M23)) and
            (SameValue(AColorSpaceXyz1.M31, AColorSpaceXyz2.M31)) and
            (SameValue(AColorSpaceXyz1.M32, AColorSpaceXyz2.M32)) and
            (SameValue(AColorSpaceXyz1.M33, AColorSpaceXyz2.M33));
end;

class function TSkColorSpaceXyz.Identity: TSkColorSpaceXyz;
begin
  Result := SkColorSpaceXyzIdentity;
end;

function TSkColorSpaceXyz.Inverse: TSkColorSpaceXyz;
var
  LDeterminant: Single;
  LFactor: Single;
begin
  LDeterminant := Self.Determinant;
  if IsZero(LDeterminant) then
    Exit(TSkColorSpaceXyz.Identity);
  LFactor := 1 / LDeterminant;
  Result.M11 := M11 * LFactor;
  Result.M12 := M12 * LFactor;
  Result.M13 := M13 * LFactor;
  Result.M21 := M21 * LFactor;
  Result.M22 := M22 * LFactor;
  Result.M23 := M23 * LFactor;
  Result.M31 := M31 * LFactor;
  Result.M32 := M32 * LFactor;
  Result.M33 := M33 * LFactor;
end;

class operator TSkColorSpaceXyz.Multiply(const AColorSpaceXyz1,
  AColorSpaceXyz2: TSkColorSpaceXyz): TSkColorSpaceXyz;
begin
  Result.M11 := AColorSpaceXyz1.M11 * AColorSpaceXyz2.M11 + AColorSpaceXyz1.M12 * AColorSpaceXyz2.M21 + AColorSpaceXyz1.M13 * AColorSpaceXyz2.M31;
  Result.M12 := AColorSpaceXyz1.M11 * AColorSpaceXyz2.M12 + AColorSpaceXyz1.M12 * AColorSpaceXyz2.M22 + AColorSpaceXyz1.M13 * AColorSpaceXyz2.M32;
  Result.M13 := AColorSpaceXyz1.M11 * AColorSpaceXyz2.M13 + AColorSpaceXyz1.M12 * AColorSpaceXyz2.M23 + AColorSpaceXyz1.M13 * AColorSpaceXyz2.M33;
  Result.M21 := AColorSpaceXyz1.M21 * AColorSpaceXyz2.M11 + AColorSpaceXyz1.M22 * AColorSpaceXyz2.M21 + AColorSpaceXyz1.M23 * AColorSpaceXyz2.M31;
  Result.M22 := AColorSpaceXyz1.M21 * AColorSpaceXyz2.M12 + AColorSpaceXyz1.M22 * AColorSpaceXyz2.M22 + AColorSpaceXyz1.M23 * AColorSpaceXyz2.M32;
  Result.M23 := AColorSpaceXyz1.M21 * AColorSpaceXyz2.M13 + AColorSpaceXyz1.M22 * AColorSpaceXyz2.M23 + AColorSpaceXyz1.M23 * AColorSpaceXyz2.M33;
  Result.M31 := AColorSpaceXyz1.M31 * AColorSpaceXyz2.M11 + AColorSpaceXyz1.M32 * AColorSpaceXyz2.M21 + AColorSpaceXyz1.M33 * AColorSpaceXyz2.M31;
  Result.M32 := AColorSpaceXyz1.M31 * AColorSpaceXyz2.M12 + AColorSpaceXyz1.M32 * AColorSpaceXyz2.M22 + AColorSpaceXyz1.M33 * AColorSpaceXyz2.M32;
  Result.M33 := AColorSpaceXyz1.M31 * AColorSpaceXyz2.M13 + AColorSpaceXyz1.M32 * AColorSpaceXyz2.M23 + AColorSpaceXyz1.M33 * AColorSpaceXyz2.M33;
end;

class operator TSkColorSpaceXyz.NotEqual(const AColorSpaceXyz1,
  AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
begin
  Result := not (AColorSpaceXyz1 = AColorSpaceXyz2);
end;

class function TSkColorSpaceXyz.Rec2020: TSkColorSpaceXyz;
begin
  Result :=  SkColorSpaceXyzRec2020;
end;

class function TSkColorSpaceXyz.SRGB: TSkColorSpaceXyz;
begin
  Result := SkColorSpaceXyzSRGB;
end;

{ TSkColorSpacePrimaries }

constructor TSkColorSpacePrimaries.Create(const ARX, ARY, AGX, AGY, ABX, ABY,
  AWX, AWY: Single);
begin
  RX := ARX;
  RY := ARY;
  GX := AGX;
  GY := AGY;
  BX := ABX;
  BY := ABY;
  WX := AWX;
  WY := AWY;
end;

class operator TSkColorSpacePrimaries.Equal(const AColorSpacePrimaries1,
  AColorSpacePrimaries2: TSkColorSpacePrimaries): Boolean;
begin
  Result := (SameValue(AColorSpacePrimaries1.RX, AColorSpacePrimaries2.RX)) and
            (SameValue(AColorSpacePrimaries1.RY, AColorSpacePrimaries2.RY)) and
            (SameValue(AColorSpacePrimaries1.GX, AColorSpacePrimaries2.GX)) and
            (SameValue(AColorSpacePrimaries1.GY, AColorSpacePrimaries2.GY)) and
            (SameValue(AColorSpacePrimaries1.BX, AColorSpacePrimaries2.BX)) and
            (SameValue(AColorSpacePrimaries1.BY, AColorSpacePrimaries2.BY)) and
            (SameValue(AColorSpacePrimaries1.WX, AColorSpacePrimaries2.WX)) and
            (SameValue(AColorSpacePrimaries1.WY, AColorSpacePrimaries2.WY));
end;

class operator TSkColorSpacePrimaries.NotEqual(const AColorSpacePrimaries1,
  AColorSpacePrimaries2: TSkColorSpacePrimaries): Boolean;
begin
  Result := not (AColorSpacePrimaries1 = AColorSpacePrimaries2);
end;

function TSkColorSpacePrimaries.ToColorSpaceXyz: TSkColorSpaceXyz;
begin
  if not sk4d_colorspaceprimaries_to_xyz(@Self, sk_colorspacexyz_t(Result)) then
    Result := TSkColorSpaceXyz.Identity;
end;

{ TSkColorSpaceTransferFunction }

constructor TSkColorSpaceTransferFunction.Create(const AG, AA, AB, AC, AD, AE,
  AF: Single);
begin
  G := AG;
  A := AA;
  B := AB;
  C := AC;
  D := AD;
  E := AE;
  F := AF;
end;

class operator TSkColorSpaceTransferFunction.Equal(
  const AColorSpaceTransferFunction1,
  AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := (SameValue(AColorSpaceTransferFunction1.G, AColorSpaceTransferFunction2.G)) and
            (SameValue(AColorSpaceTransferFunction1.A, AColorSpaceTransferFunction2.A)) and
            (SameValue(AColorSpaceTransferFunction1.B, AColorSpaceTransferFunction2.B)) and
            (SameValue(AColorSpaceTransferFunction1.C, AColorSpaceTransferFunction2.C)) and
            (SameValue(AColorSpaceTransferFunction1.D, AColorSpaceTransferFunction2.D)) and
            (SameValue(AColorSpaceTransferFunction1.E, AColorSpaceTransferFunction2.E)) and
            (SameValue(AColorSpaceTransferFunction1.F, AColorSpaceTransferFunction2.F));
end;

class function TSkColorSpaceTransferFunction.HLG: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionHLG;
end;

function TSkColorSpaceTransferFunction.Invert(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := sk4d_colorspacetransferfn_invert(@Self, sk_colorspacetransferfn_t(ATransferFunction));
end;

class function TSkColorSpaceTransferFunction.Linear: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionLinear;
end;

class operator TSkColorSpaceTransferFunction.NotEqual(
  const AColorSpaceTransferFunction1,
  AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := not (AColorSpaceTransferFunction1 = AColorSpaceTransferFunction2);
end;

class function TSkColorSpaceTransferFunction.PQ: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionPQ;
end;

class function TSkColorSpaceTransferFunction.Rec2020: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionRec2020;
end;

class function TSkColorSpaceTransferFunction.SRGB: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionSRGB;
end;

function TSkColorSpaceTransferFunction.Transform(const AX: Single): Single;
begin
  Result := sk4d_colorspacetransferfn_transform(@Self, AX);
end;

class function TSkColorSpaceTransferFunction.TwoDotTwo: TSkColorSpaceTransferFunction;
begin
  Result := SkColorSpaceTransferFunctionTwoDotTwo;
end;

{ TSkCubicResampler }

class function TSkCubicResampler.CatmullRom: TSkCubicResampler;
begin
  Result := SkCubicResamplerCatmullRom;
end;

constructor TSkCubicResampler.Create(const AB, AC: Single);
begin
  B := AB;
  C := AC;
end;

class operator TSkCubicResampler.Equal(const ACubicResampler1,
  ACubicResampler2: TSkCubicResampler): Boolean;
begin
  Result := (SameValue(ACubicResampler1.B, ACubicResampler2.B)) and
            (SameValue(ACubicResampler1.C, ACubicResampler2.C));
end;

class function TSkCubicResampler.Mitchell: TSkCubicResampler;
begin
  Result := SkCubicResamplerMitchell;
end;

class operator TSkCubicResampler.NotEqual(const ACubicResampler1,
  ACubicResampler2: TSkCubicResampler): Boolean;
begin
  Result := not (ACubicResampler1 = ACubicResampler2);
end;

{ TSkFontMetrics }

class operator TSkFontMetrics.Equal(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
begin
  Result := (AFontMetrics1.Flags = AFontMetrics2.Flags                                    ) and
            (SameValue(AFontMetrics1.Top,                AFontMetrics2.Top)               ) and
            (SameValue(AFontMetrics1.Ascent,             AFontMetrics2.Ascent)            ) and
            (SameValue(AFontMetrics1.Descent,            AFontMetrics2.Descent)           ) and
            (SameValue(AFontMetrics1.Bottom,             AFontMetrics2.Bottom)            ) and
            (SameValue(AFontMetrics1.Leading,            AFontMetrics2.Leading)           ) and
            (SameValue(AFontMetrics1.AvgCharWidth,       AFontMetrics2.AvgCharWidth)      ) and
            (SameValue(AFontMetrics1.MaxCharWidth,       AFontMetrics2.MaxCharWidth)      ) and
            (SameValue(AFontMetrics1.XMin,               AFontMetrics2.XMin)              ) and
            (SameValue(AFontMetrics1.XMax,               AFontMetrics2.XMax)              ) and
            (SameValue(AFontMetrics1.XMax,               AFontMetrics2.XMax)              ) and
            (SameValue(AFontMetrics1.XHeight,            AFontMetrics2.XHeight)           ) and
            (SameValue(AFontMetrics1.UnderlineThickness, AFontMetrics2.UnderlineThickness)) and
            (SameValue(AFontMetrics1.UnderlinePosition,  AFontMetrics2.UnderlinePosition) ) and
            (SameValue(AFontMetrics1.StrikeoutThickness, AFontMetrics2.StrikeoutThickness)) and
            (SameValue(AFontMetrics1.StrikeoutPosition,  AFontMetrics2.StrikeoutPosition) );
end;

class operator TSkFontMetrics.NotEqual(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
begin
  Result := not (AFontMetrics1 = AFontMetrics2);
end;

{ TSkFontStyle }

class function TSkFontStyle.Bold: TSkFontStyle;
begin
  Result := SkFontStyleBold;
end;

class function TSkFontStyle.BoldItalic: TSkFontStyle;
begin
  Result := SkFontStyleBoldItalic;
end;

constructor TSkFontStyle.Create(const AWeight, AWidth: Integer;
  const ASlant: TSkFontSlant);
begin
  Weight := AWeight;
  Width  := AWidth;
  Slant  := ASlant;
end;

constructor TSkFontStyle.Create(const AWeight: TSkFontWeight;
  const AWidth: TSkFontWidth; const ASlant: TSkFontSlant);
begin
  Weight := Ord(AWeight);
  Width  := Ord(AWidth);
  Slant  := ASlant;
end;

class operator TSkFontStyle.Equal(const AFontStyle1,
  AFontStyle2: TSkFontStyle): Boolean;
begin
  Result := (AFontStyle1.Weight = AFontStyle2.Weight) and
            (AFontStyle1.Width  = AFontStyle2.Width ) and
            (AFontStyle1.Slant  = AFontStyle2.Slant );
end;

class function TSkFontStyle.Italic: TSkFontStyle;
begin
  Result := SkFontStyleItalic;
end;

class function TSkFontStyle.Normal: TSkFontStyle;
begin
  Result := SkFontStyleNormal;
end;

class operator TSkFontStyle.NotEqual(const AFontStyle1,
  AFontStyle2: TSkFontStyle): Boolean;
begin
  Result := not (AFontStyle1 = AFontStyle2);
end;

{ TSkGraphics }

class procedure TSkGraphics.AllowJIT;
begin
  sk4d_graphics_allow_jit();
end;

class procedure TSkGraphics.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  if not Assigned(ATraceMemoryDump) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATraceMemoryDump']);
  sk4d_graphics_dump_memory_statistics(ATraceMemoryDump.Handle);
end;

class function TSkGraphics.GetFontCacheCountLimit: Integer;
begin
  Result := sk4d_graphics_get_font_cache_count_limit();
end;

class function TSkGraphics.GetFontCacheCountUsed: Integer;
begin
  Result := sk4d_graphics_get_font_cache_count_used();
end;

class function TSkGraphics.GetFontCacheLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_font_cache_limit();
end;

class function TSkGraphics.GetFontCacheUsed: NativeUInt;
begin
  Result := sk4d_graphics_get_font_cache_used();
end;

class function TSkGraphics.GetResourceCacheSingleAllocationByteLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_single_allocation_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalByteLimit: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_total_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalBytesUsed: NativeUInt;
begin
  Result := sk4d_graphics_get_resource_cache_total_bytes_used();
end;

class procedure TSkGraphics.Init;
begin
  sk4d_graphics_init();
end;

class procedure TSkGraphics.PurgeAllCaches;
begin
  sk4d_graphics_purge_all_caches();
end;

class procedure TSkGraphics.PurgeFontCache;
begin
  sk4d_graphics_purge_font_cache();
end;

class procedure TSkGraphics.PurgeResourceCache;
begin
  sk4d_graphics_purge_resource_cache();
end;

class procedure TSkGraphics.SetFontCacheCountLimit(const AValue: Integer);
begin
  sk4d_graphics_set_font_cache_count_limit(AValue);
end;

class procedure TSkGraphics.SetFontCacheLimit(const AValue: NativeUInt);
begin
  sk4d_graphics_set_font_cache_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheSingleAllocationByteLimit(
  const AValue: NativeUInt);
begin
  sk4d_graphics_set_resource_cache_single_allocation_byte_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheTotalByteLimit(
  const AValue: NativeUInt);
begin
  sk4d_graphics_set_resource_cache_total_byte_limit(AValue);
end;

{ TSkHighContrastConfig }

constructor TSkHighContrastConfig.Create(const AGrayscale: Boolean;
  const AInvertStyle: TSkContrastInvertStyle; const AContrast: Single);
begin
  Grayscale   := AGrayscale;
  InvertStyle := AInvertStyle;
  Contrast    := AContrast;
end;

class operator TSkHighContrastConfig.Equal(const AHighContrastConfig1,
  AHighContrastConfig2: TSkHighContrastConfig): Boolean;
begin
  Result := (AHighContrastConfig1.Grayscale   = AHighContrastConfig2.Grayscale      ) and
            (AHighContrastConfig1.InvertStyle = AHighContrastConfig2.InvertStyle    ) and
            (SameValue(AHighContrastConfig1.Contrast, AHighContrastConfig2.Contrast));
end;

class operator TSkHighContrastConfig.NotEqual(const AHighContrastConfig1,
  AHighContrastConfig2: TSkHighContrastConfig): Boolean;
begin
  Result := not (AHighContrastConfig1 <> AHighContrastConfig2);
end;

{ TSkImageInfo }

function TSkImageInfo.ByteSize(const ARowBytes: NativeUInt): NativeUInt;
begin
  Result := ARowBytes * NativeUInt(Height);
end;

function TSkImageInfo.BytesPerPixel: Integer;
begin
  Result := SkBytesPerPixel[ColorType];
end;

constructor TSkImageInfo.Create(const AWidth, AHeight: Integer);
begin
  Width      := AWidth;
  Height     := AHeight;
  ColorType  := SkNative32ColorType;
  AlphaType  := TSkAlphaType.Premul;
  ColorSpace := nil;
end;

constructor TSkImageInfo.Create(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  AColorSpace: ISkColorSpace);
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
  Result := (AImageInfo1.Width                = AImageInfo2.Width               ) and
            (AImageInfo1.Height               = AImageInfo2.Height              ) and
            (AImageInfo1.ColorType            = AImageInfo2.ColorType           ) and
            (AImageInfo1.AlphaType            = AImageInfo2.AlphaType           ) and
            (Assigned(AImageInfo1.ColorSpace) = Assigned(AImageInfo1.ColorSpace)) and
            ((not Assigned(AImageInfo1.ColorSpace)) or (AImageInfo1.ColorSpace.IsEqual(AImageInfo2.ColorSpace)));
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
  Result := False;
  if (IsEmpty) or (Width > MaxDimension) or (Height > MaxDimension) or (ColorType = TSkColorType.Unknown) or (AlphaType = TSkAlphaType.Unknown) then
    Exit;
  case ColorType of
    TSkColorType.Alpha8,
    TSkColorType.AlphaF16,
    TSkColorType.Alpha16:
      begin
        if (AlphaType <> TSkAlphaType.Opaque) and (AlphaType <> TSkAlphaType.Premul) then
          Exit;
      end;
    TSkColorType.RGB565,
    TSkColorType.RGB888X,
    TSkColorType.RGB101010X,
    TSkColorType.BGR101010X,
    TSkColorType.Gray8,
    TSkColorType.RG88,
    TSkColorType.RGF16,
    TSkColorType.RG1616:
      begin
        if AlphaType <> TSkAlphaType.Opaque then
          Exit;
      end;
  else
    Result := True;
  end;
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

function TSkImageInfo.MakeColorSpace(AColorSpace: ISkColorSpace): TSkImageInfo;
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
  Result := SkShiftPerPixel[ColorType];
end;

{ TSkFrame }

constructor TSkFrame.Create(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ADuration: Integer);
begin
  Pixmap   := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Duration := ADuration;
end;

constructor TSkFrame.Create(const APixmap: ISkPixmap; const ADuration: Integer);
begin
  Pixmap   := APixmap;
  Duration := ADuration;
end;

{ TSkAnimatedWebPEncoder }

class function TSkAnimatedWebPEncoder.Encode(const ASrc: TArray<TSkFrame>;
  const AQuality: Integer): TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    EncodeToStream(LBytesStream, ASrc, AQuality);
    Result := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

class function TSkAnimatedWebPEncoder.EncodeToFile(const AFileName: string;
  const ASrc: TArray<TSkFrame>; const AQuality: Integer): Boolean;
var
  I: Integer;
  LSrc: TArray<sk_frame_t>;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  if Length(ASrc) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASrc']);
  SetLength(LSrc{%H-}, Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
  begin
    if not Assigned(ASrc[I].Pixmap) then
      raise ESkException.Create(SInvalidOperation);
    MapFrame(ASrc[I], LSrc[I]);
  end;
  Result := sk4d_animatedwebpencoder_encode_to_file(MarshaledAString(UTF8String(AFileName)), @LSrc[0], Length(ASrc), AQuality);
end;

class function TSkAnimatedWebPEncoder.EncodeToStream(const AStream: TStream;
  const ASrc: TArray<TSkFrame>; const AQuality: Integer): Boolean;
var
  I: Integer;
  LSrc: TArray<sk_frame_t>;
  LWStream: ISkWStream;
begin
  if Length(ASrc) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASrc']);
  SetLength(LSrc{%H-}, Length(ASrc));
  for I := 0 to Length(ASrc) - 1 do
  begin
    if not Assigned(ASrc[I].Pixmap) then
      raise ESkException.Create(SInvalidOperation);
    MapFrame(ASrc[I], LSrc[I]);
  end;
  LWStream := TSkWStreamAdapter.Create(AStream);
  Result   := sk4d_animatedwebpencoder_encode_to_stream(LWStream.Handle, @LSrc[0], Length(ASrc), AQuality);
end;

{ TSkImageEncoder }

class function TSkImageEncoder.Encode(const ASrc: ISkPixmap;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    EncodeToStream(LBytesStream, ASrc, AEncodedImageFormat, AQuality);
    Result := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

class function TSkImageEncoder.Encode(const ASrcImageInfo: TSkImageInfo;
  const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  Result  := Encode(LPixmap, AEncodedImageFormat, AQuality);
end;

class function TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt; const AQuality: Integer): Boolean;
begin
  Result := EncodeToFile(AFileName, ASrcImageInfo, ASrcPixels, ASrcRowBytes, ExtensionToEncodedImageFormat(ExtractFileExt(AFileName)), AQuality);
end;

class function TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  Result := sk4d_imageencoder_encode_to_file(MarshaledAString(UTF8String(AFileName)), ASrc.Handle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

class function TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrc: ISkPixmap; const AQuality: Integer): Boolean;
begin
  Result := EncodeToFile(AFileName, ASrc, ExtensionToEncodedImageFormat(ExtractFileExt(AFileName)), AQuality);
end;

class function TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  Result  := EncodeToFile(AFileName, LPixmap, AEncodedImageFormat, AQuality);
end;

class function TSkImageEncoder.EncodeToStream(const AStream: TStream;
  const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LWStream: ISkWStream;
begin
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  LWStream := TSkWStreamAdapter.Create(AStream);
  Result   := sk4d_imageencoder_encode_to_stream(LWStream.Handle, ASrc.Handle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

class function TSkImageEncoder.EncodeToStream(const AStream: TStream;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  Result  := EncodeToStream(AStream, LPixmap, AEncodedImageFormat, AQuality);
end;

{ TSkLattice }

constructor TSkLattice.Create(const AXDivs, AYDivs: TArray<Integer>;
  ABounds: TRect; const ARectTypes: TArray<TSkLatticeRectType>;
  const AColors: TArray<TAlphaColor>);
begin
  if Length(AXDivs) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AXDivs']);
  if Length(AYDivs) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AYDivs']);
  if (Length(ARectTypes) > 0) then
  begin
    if Length(ARectTypes) <> ((Length(AXDivs) + 1) * (Length(AYDivs) + 1)) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ARectTypes']);
    if Length(AColors) <> Length(ARectTypes) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
  end;
  XDivs     := AXDivs;
  YDivs     := AYDivs;
  RectTypes := ARectTypes;
  Colors    := AColors;
  UseBounds := True;
  Bounds    := ABounds;
end;

constructor TSkLattice.Create(const AXDivs, AYDivs: TArray<Integer>;
  const ARectTypes: TArray<TSkLatticeRectType>;
  const AColors: TArray<TAlphaColor>);
begin
  if Length(AXDivs) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AXDivs']);
  if Length(AYDivs) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AYDivs']);
  if (Length(ARectTypes) > 0) then
  begin
    if Length(ARectTypes) <> ((Length(AXDivs) + 1) * (Length(AYDivs) + 1)) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ARectTypes']);
    if Length(AColors) <> Length(ARectTypes) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
  end;
  XDivs     := AXDivs;
  YDivs     := AYDivs;
  RectTypes := ARectTypes;
  Colors    := AColors;
  UseBounds := False;
end;

{ TSkMetrics }

class operator TSkMetrics.Equal(const AMetrics1,
  AMetrics2: TSkMetrics): Boolean;
begin
  Result := (AMetrics1.StartIndex              = AMetrics2.StartIndex             ) and
            (AMetrics1.EndIndex                = AMetrics2.EndIndex               ) and
            (AMetrics1.EndExcludingWhitespaces = AMetrics2.EndExcludingWhitespaces) and
            (AMetrics1.EndIncludingNewline     = AMetrics2.EndIncludingNewline    ) and
            (AMetrics1.IsHardBreak             = AMetrics2.IsHardBreak            ) and
            (AMetrics1.LineNumber              = AMetrics2.LineNumber             ) and
            (SameValue(AMetrics1.Ascent,   AMetrics2.Ascent)                      ) and
            (SameValue(AMetrics1.Descent,  AMetrics2.Descent)                     ) and
            (SameValue(AMetrics1.Height,   AMetrics2.Height)                      ) and
            (SameValue(AMetrics1.Width,    AMetrics2.Width)                       ) and
            (SameValue(AMetrics1.Left,     AMetrics2.Left)                        ) and
            (SameValue(AMetrics1.Baseline, AMetrics2.Baseline)                    );
end;

class operator TSkMetrics.NotEqual(const AMetrics1,
  AMetrics2: TSkMetrics): Boolean;
begin
  Result := not (AMetrics1 = AMetrics2);
end;

{ TSkPDFMetadata }

constructor TSkPDFMetadata.Create(const ATitle, AAuthor, ASubject, AKeywords,
  ACreator, AProducer: string; const ARasterDPI: Single; const APDFA: Boolean;
  const AEncodingQuality: Integer);
begin
  Title           := ATitle;
  Author          := AAuthor;
  Subject         := ASubject;
  Keywords        := AKeywords;
  Creator         := ACreator;
  Producer        := AProducer;
  Creation        := Now;
  Modified        := Now;
  RasterDPI       := ARasterDPI;
  PDFA            := APDFA;
  EncodingQuality := AEncodingQuality;
end;

constructor TSkPDFMetadata.Create(const ATitle, AAuthor, ASubject, AKeywords,
  ACreator: string; const ACreation, AModified: TDateTime;
  const AProducer: string; const ARasterDPI: Single; const APDFA: Boolean;
  const AEncodingQuality: Integer);
begin
  Title           := ATitle;
  Author          := AAuthor;
  Subject         := ASubject;
  Keywords        := AKeywords;
  Creator         := ACreator;
  Producer        := AProducer;
  Creation        := ACreation;
  Modified        := AModified;
  RasterDPI       := ARasterDPI;
  PDFA            := APDFA;
  EncodingQuality := AEncodingQuality;
end;

class operator TSkPDFMetadata.Equal(const APDFMetadata1,
  APDFMetadata2: TSkPDFMetadata): Boolean;
begin
  Result := (APDFMetadata1.Title           = APDFMetadata2.Title          ) and
            (APDFMetadata1.Author          = APDFMetadata2.Author         ) and
            (APDFMetadata1.Subject         = APDFMetadata2.Subject        ) and
            (APDFMetadata1.Keywords        = APDFMetadata2.Keywords       ) and
            (APDFMetadata1.Creator         = APDFMetadata2.Creator        ) and
            (APDFMetadata1.Producer        = APDFMetadata2.Producer       ) and
            (APDFMetadata1.Creation        = APDFMetadata2.Creation       ) and
            (APDFMetadata1.Modified        = APDFMetadata2.Modified       ) and
            (APDFMetadata1.PDFA            = APDFMetadata2.PDFA           ) and
            (APDFMetadata1.EncodingQuality = APDFMetadata2.EncodingQuality) and
            (SameValue(APDFMetadata1.RasterDPI, APDFMetadata2.RasterDPI)  );
end;

class operator TSkPDFMetadata.NotEqual(const APDFMetadata1,
  APDFMetadata2: TSkPDFMetadata): Boolean;
begin
  Result := not (APDFMetadata1 = APDFMetadata2);
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
  Result := (APlaceholderStyle1.Alignment = APlaceholderStyle2.Alignment    ) and
            (APlaceholderStyle1.Baseline  = APlaceholderStyle2.Baseline     ) and
            (SameValue(APlaceholderStyle1.Width,  APlaceholderStyle2.Width) ) and
            (SameValue(APlaceholderStyle1.Height, APlaceholderStyle2.Height)) and
            ((APlaceholderStyle1.Alignment <> TSkPlaceholderAlignment.Baseline) or (SameValue(APlaceholderStyle1.BaselineOffset, APlaceholderStyle2.BaselineOffset)));
end;

class operator TSkPlaceholderStyle.NotEqual(const APlaceholderStyle1,
  APlaceholderStyle2: TSkPlaceholderStyle): Boolean;
begin
  Result := not (APlaceholderStyle1 = APlaceholderStyle2);
end;

{ TSkPositionAffinity }

class operator TSkPositionAffinity.Equal(const APositionAffinity1,
  APositionAffinity2: TSkPositionAffinity): Boolean;
begin
  Result := (APositionAffinity1.Position = APositionAffinity2.Position) and
            (APositionAffinity1.Affinity = APositionAffinity2.Affinity);
end;

class operator TSkPositionAffinity.NotEqual(const APositionAffinity1,
  APositionAffinity2: TSkPositionAffinity): Boolean;
begin
  Result := not (APositionAffinity1 = APositionAffinity2);
end;

{ TSkRotationScaleMatrix }

constructor TSkRotationScaleMatrix.Create(const ASCosinus, ASSinus, ATranslateX,
  ATranslateY: Single);
begin
  SCosinus   := ASCosinus;
  SSinus     := ASSinus;
  TranslateX := ATranslateX;
  TranslateY := ATranslateY;
end;

class function TSkRotationScaleMatrix.CreateDegrees(const AScale, ADegrees,
  ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix;
begin
  SinCos(DegToRad(ADegrees), Result.SSinus, Result.SCosinus);
  Result.SSinus     := Result.SSinus   * AScale;
  Result.SCosinus   := Result.SCosinus * AScale;
  Result.TranslateX := ATranslateX + -Result.SCosinus * AAnchorX + Result.SSinus   * AAnchorY;
  Result.TranslateY := ATranslateY + -Result.SSinus   * AAnchorX - Result.SCosinus * AAnchorY;
end;

class function TSkRotationScaleMatrix.CreateRadians(const AScale, ARadians,
  ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix;
begin
  SinCos(ARadians, Result.SSinus, Result.SCosinus);
  Result.SSinus     := Result.SSinus   * AScale;
  Result.SCosinus   := Result.SCosinus * AScale;
  Result.TranslateX := ATranslateX + -Result.SCosinus * AAnchorX + Result.SSinus   * AAnchorY;
  Result.TranslateY := ATranslateY + -Result.SSinus   * AAnchorX - Result.SCosinus * AAnchorY;
end;

class operator TSkRotationScaleMatrix.Equal(const ARotationScaleMatrix1,
  ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
begin
  Result := (SameValue(ARotationScaleMatrix1.SCosinus,   ARotationScaleMatrix2.SCosinus)  ) and
            (SameValue(ARotationScaleMatrix1.SSinus,     ARotationScaleMatrix2.SSinus)    ) and
            (SameValue(ARotationScaleMatrix1.TranslateX, ARotationScaleMatrix2.TranslateX)) and
            (SameValue(ARotationScaleMatrix1.TranslateY, ARotationScaleMatrix2.TranslateY));
end;

class function TSkRotationScaleMatrix.Identity: TSkRotationScaleMatrix;
begin
  Result := SkRotationScaleMatrixIdentity;
end;

class operator TSkRotationScaleMatrix.NotEqual(const ARotationScaleMatrix1,
  ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
begin
  Result := not (ARotationScaleMatrix1 = ARotationScaleMatrix2);
end;

function TSkRotationScaleMatrix.ToMatrix: TMatrix;
begin
  Result.m11 := SCosinus;
  Result.m12 := SSinus;
  Result.m13 := 0;
  Result.m21 := -SSinus;
  Result.m22 := SCosinus;
  Result.m23 := 0;
  Result.m31 := TranslateX;
  Result.m32 := TranslateY;
  Result.m33 := 1;
end;

{ TSkRuntimeEffectFloat2 }

constructor TSkRuntimeEffectFloat2.Create(const AV1, AV2: Single);
begin
  V1 := AV1;
  V2 := AV2;
end;

{ TSkRuntimeEffectFloat3 }

constructor TSkRuntimeEffectFloat3.Create(const AV1, AV2, AV3: Single);
begin
  V1 := AV1;
  V2 := AV2;
  V3 := AV3;
end;

{ TSkRuntimeEffectFloat4 }

constructor TSkRuntimeEffectFloat4.Create(const AV1, AV2, AV3, AV4: Single);
begin
  V1 := AV1;
  V2 := AV2;
  V3 := AV3;
  V4 := AV4;
end;

{ TSkRuntimeEffectFloat2x2 }

constructor TSkRuntimeEffectFloat2x2.Create(const AV11, AV12, AV21,
  AV22: Single);
begin
  V11 := AV11;
  V12 := AV12;
  V21 := AV21;
  V22 := AV22;
end;

{ TSkRuntimeEffectFloat3x3 }

constructor TSkRuntimeEffectFloat3x3.Create(const AV11, AV12, AV13, AV21, AV22,
  AV23, AV31, AV32, AV33: Single);
begin
  V11 := AV11;
  V12 := AV12;
  V13 := AV13;
  V21 := AV21;
  V22 := AV22;
  V23 := AV23;
  V31 := AV31;
  V32 := AV32;
  V33 := AV33;
end;

{ TSkRuntimeEffectFloat4x4 }

constructor TSkRuntimeEffectFloat4x4.Create(const AV11, AV12, AV13, AV14, AV21,
  AV22, AV23, AV24, AV31, AV32, AV33, AV34, AV41, AV42, AV43, AV44: Single);
begin
  V11 := AV11;
  V12 := AV12;
  V13 := AV13;
  V14 := AV14;
  V21 := AV21;
  V22 := AV22;
  V23 := AV23;
  V24 := AV24;
  V31 := AV31;
  V32 := AV32;
  V33 := AV33;
  V34 := AV34;
  V41 := AV41;
  V42 := AV42;
  V43 := AV43;
  V44 := AV44;
end;

{ TSkRuntimeEffectInt2 }

constructor TSkRuntimeEffectInt2.Create(const AV1, AV2: Integer);
begin
  V1 := AV1;
  V2 := AV2;
end;

{ TSkRuntimeEffectInt3 }

constructor TSkRuntimeEffectInt3.Create(const AV1, AV2, AV3: Integer);
begin
  V1 := AV1;
  V2 := AV2;
  V3 := AV3;
end;

{ TSkRuntimeEffectInt4 }

constructor TSkRuntimeEffectInt4.Create(const AV1, AV2, AV3, AV4: Integer);
begin
  V1 := AV1;
  V2 := AV2;
  V3 := AV3;
  V4 := AV4;
end;

{ TSkSamplingOptions }

constructor TSkSamplingOptions.Create(const ACubic: TSkCubicResampler);
begin
  MaxAnisotropic := 0;
  UseCubic       := True;
  Cubic          := ACubic;
  Filter         := TSkFilterMode.Nearest;
  Mipmap         := TSkMipmapMode.None;
end;

constructor TSkSamplingOptions.Create(const AFilter: TSkFilterMode;
  const AMipmap: TSkMipmapMode);
begin
  MaxAnisotropic := 0;
  UseCubic       := False;
  Cubic          := TSkCubicResampler.Create(0, 0);
  Filter         := AFilter;
  Mipmap         := AMipmap;
end;

constructor TSkSamplingOptions.Create(const AMaxAnisotropic: Integer);
begin
  MaxAnisotropic := Max(AMaxAnisotropic, 1);
  UseCubic       := False;
  Cubic          := TSkCubicResampler.Create(0, 0);
  Filter         := TSkFilterMode.Nearest;
  Mipmap         := TSkMipmapMode.None;
end;

class operator TSkSamplingOptions.Equal(const ASamplingOptions1,
  ASamplingOptions2: TSkSamplingOptions): Boolean;
begin
  Result := (ASamplingOptions1.MaxAnisotropic = ASamplingOptions2.MaxAnisotropic) and
            (ASamplingOptions1.UseCubic       = ASamplingOptions2.UseCubic      ) and
            (ASamplingOptions1.Cubic          = ASamplingOptions2.Cubic         ) and
            (ASamplingOptions1.Filter         = ASamplingOptions2.Filter        ) and
            (ASamplingOptions1.Mipmap         = ASamplingOptions2.Mipmap        );
end;

class function TSkSamplingOptions.High: TSkSamplingOptions;
begin
  Result := SkSamplingOptionsHigh;
end;

class function TSkSamplingOptions.Low: TSkSamplingOptions;
begin
  Result := SkSamplingOptionsLow;
end;

class function TSkSamplingOptions.Medium: TSkSamplingOptions;
begin
  Result := SkSamplingOptionsMedium;
end;

class operator TSkSamplingOptions.NotEqual(const ASamplingOptions1,
  ASamplingOptions2: TSkSamplingOptions): Boolean;
begin
  Result := not (ASamplingOptions1 = ASamplingOptions2);
end;

{ TSkSurfaceProperties }

constructor TSkSurfaceProperties.Create(const AFlags: TSkSurfacePropertiesFlags;
  const APixelGeometry: TSkPixelGeometry);
begin
  Flags         := AFlags;
  PixelGeometry := APixelGeometry;
end;

class operator TSkSurfaceProperties.Equal(const ASurfaceProperties1,
  ASurfaceProperties2: TSkSurfaceProperties): Boolean;
begin
  Result := (ASurfaceProperties1.Flags         = ASurfaceProperties2.Flags        ) and
            (ASurfaceProperties1.PixelGeometry = ASurfaceProperties2.PixelGeometry);
end;

class operator TSkSurfaceProperties.NotEqual(const ASurfaceProperties1,
  ASurfaceProperties2: TSkSurfaceProperties): Boolean;
begin
  Result := not (ASurfaceProperties1 = ASurfaceProperties2);
end;

{ TSkSVGLength }

constructor TSkSVGLength.Create(const AValue: Single;
  const AUnit: TSkSVGLengthUnit);
begin
  Value := AValue;
  &Unit := AUnit;
end;

class operator TSkSVGLength.Equal(const ASVGLength1,
  ASVGLength2: TSkSVGLength): Boolean;
begin
  Result := (ASVGLength1.&Unit = ASVGLength2.&Unit          ) and
            (SameValue(ASVGLength1.Value, ASVGLength2.Value));
end;

class operator TSkSVGLength.NotEqual(const ASVGLength1,
  ASVGLength2: TSkSVGLength): Boolean;
begin
  Result := not (ASVGLength1 = ASVGLength2);
end;

{ TSkSVGPreserveAspectRatio }

constructor TSkSVGPreserveAspectRatio.Create(const AAlign: TSkSVGAspectAlign;
  const AScale: TSkSVGAspectScale);
begin
  Align := AAlign;
  Scale := AScale;
end;

class operator TSkSVGPreserveAspectRatio.Equal(const ASVGPreserveAspectRatio1,
  ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
begin
  Result := (ASVGPreserveAspectRatio1.Align = ASVGPreserveAspectRatio2.Align) and
            (ASVGPreserveAspectRatio1.Scale = ASVGPreserveAspectRatio2.Scale);
end;

class operator TSkSVGPreserveAspectRatio.NotEqual(
  const ASVGPreserveAspectRatio1,
  ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
begin
  Result := not (ASVGPreserveAspectRatio1 = ASVGPreserveAspectRatio2);
end;

{ TSkTextBox }

class operator TSkTextBox.Equal(const ATextBox1,
  ATextBox2: TSkTextBox): Boolean;
begin
  Result := (ATextBox1.Rect      = ATextBox2.Rect     ) and
            (ATextBox1.Direction = ATextBox2.Direction);
end;

class operator TSkTextBox.NotEqual(const ATextBox1,
  ATextBox2: TSkTextBox): Boolean;
begin
  Result := not (ATextBox1 = ATextBox2)
end;

{ TSkTextShadow }

constructor TSkTextShadow.Create(const AColor: TAlphaColor;
  const AOffset: TPointF; const ABlurRadius: Double);
begin
  Color      := AColor;
  Offset     := AOffset;
  BlurRadius := ABlurRadius;
end;

class operator TSkTextShadow.Equal(const ATextShadow1,
  ATextShadow2: TSkTextShadow): Boolean;
begin
  Result := (ATextShadow1.Color  = ATextShadow2.Color                   ) and
            (ATextShadow1.Offset = ATextShadow2.Offset                  ) and
            (SameValue(ATextShadow1.BlurRadius, ATextShadow2.BlurRadius));
end;

class operator TSkTextShadow.NotEqual(const ATextShadow1,
  ATextShadow2: TSkTextShadow): Boolean;
begin
  Result := not (ATextShadow1 = ATextShadow2);
end;

{ TGrBackendRenderTarget }

constructor TGrBackendRenderTarget.CreateGl(const AWidth, AHeight, ASampleCount,
  AStencilBits: Integer; const AFramebufferInfo: TGrGlFramebufferInfo);
begin
  inherited Create(gr4d_backendrendertarget_create_gl(AWidth, AHeight, ASampleCount, AStencilBits, @AFramebufferInfo))
end;

constructor TGrBackendRenderTarget.CreateMetal(const AWidth, AHeight: Integer;
  const ATextureInfo: TGrMtlTextureInfo);
begin
  inherited Create(gr4d_backendrendertarget_create_mtl(AWidth, AHeight, @ATextureInfo));
end;

constructor TGrBackendRenderTarget.CreateVulkan(const AWidth, AHeight: Integer;
  const AImageInfo: TGrVkImageInfo);
var
  LImageInfo: gr_vk_imageinfo_t;
begin
  LImageInfo.image                 := AImageInfo.Image;
  LImageInfo.alloc.device_memory   := AImageInfo.Alloc.DeviceMemory;
  LImageInfo.alloc.offset          := AImageInfo.Alloc.Offset;
  LImageInfo.alloc.size            := AImageInfo.Alloc.Size;
  LImageInfo.alloc.flags           := Byte(AImageInfo.Alloc.Flags);
  LImageInfo.alloc.memory          := AImageInfo.Alloc.Memory;
  LImageInfo.image_tiling          := AImageInfo.ImageTiling;
  LImageInfo.image_layout          := AImageInfo.ImageLayout;
  LImageInfo.format                := AImageInfo.Format;
  LImageInfo.image_usage_flags     := AImageInfo.ImageUsageFlags;
  LImageInfo.sample_count          := AImageInfo.SampleCount;
  LImageInfo.level_count           := AImageInfo.LevelCount;
  LImageInfo.current_queue_family  := AImageInfo.CurrentQueueFamily;
  LImageInfo.protected_image       := AImageInfo.ProtectedImage;
  LImageInfo.ycbcr_conversion_info := gr_vk_ycbcrconversioninfo_t(AImageInfo.YcbcrConversionInfo);
  LImageInfo.sharing_mode          := AImageInfo.SharingMode;
  inherited Create(gr4d_backendrendertarget_create_vk(AWidth, AHeight, @LImageInfo));
end;

function TGrBackendRenderTarget.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(gr4d_backendrendertarget_get_backend_api(Handle));
end;

function TGrBackendRenderTarget.GetHeight: Integer;
begin
  Result := gr4d_backendrendertarget_get_height(Handle);
end;

function TGrBackendRenderTarget.GetSampleCount: Integer;
begin
  Result := gr4d_backendrendertarget_get_sample_count(Handle);
end;

function TGrBackendRenderTarget.GetStencilBits: Integer;
begin
  Result := gr4d_backendrendertarget_get_stencil_bits(Handle);
end;

function TGrBackendRenderTarget.GetWidth: Integer;
begin
  Result := gr4d_backendrendertarget_get_width(Handle);
end;

function TGrBackendRenderTarget.IsValid: Boolean;
begin
  Result := gr4d_backendrendertarget_is_valid(Handle);
end;

class procedure TGrBackendRenderTarget.__DestroyHandle(const AHandle: sk_handle_t);
begin
  gr4d_backendrendertarget_destroy(AHandle);
end;

{ TGrBackendSemaphore }

constructor TGrBackendSemaphore.Create;
begin
  inherited Create(gr4d_backendsemaphore_create());
end;

procedure TGrBackendSemaphore.InitVulkan(const ASemaphore: GrVkSemaphore);
begin
  gr4d_backendsemaphore_init_vulkan(Handle, ASemaphore);
end;

class procedure TGrBackendSemaphore.__DestroyHandle(const AHandle: sk_handle_t);
begin
  gr4d_backendsemaphore_destroy(AHandle);
end;

{ TGrBackendSurfaceMutableState }

constructor TGrBackendSurfaceMutableState.Create(
  const AImageLayout: GrVkImageLayout; const AQueueFamilyIndex: Cardinal);
begin
  inherited Create(gr4d_backendsurfacemutablestate_create(AImageLayout, AQueueFamilyIndex));
end;

class procedure TGrBackendSurfaceMutableState.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  gr4d_backendsurfacemutablestate_destroy(AHandle);
end;

{ TGrBackendTexture }

constructor TGrBackendTexture.CreateGl(const AWidth, AHeight: Integer;
  const AIsMipmapped: Boolean; const ATextureInfo: TGrGlTextureInfo);
begin
  inherited Create(gr4d_backendtexture_create_gl(AWidth, AHeight, AIsMipmapped, @ATextureInfo));
end;

constructor TGrBackendTexture.CreateMetal(const AWidth, AHeight: Integer;
  const AIsMipmapped: Boolean; const ATextureInfo: TGrMtlTextureInfo);
begin
  inherited Create(gr4d_backendtexture_create_mtl(AWidth, AHeight, AIsMipmapped, @ATextureInfo));
end;

constructor TGrBackendTexture.CreateVulkan(const AWidth, AHeight: Integer;
  const AImageInfo: TGrVkImageInfo);
var
  LImageInfo: gr_vk_imageinfo_t;
begin
  LImageInfo.image                 := AImageInfo.Image;
  LImageInfo.alloc.device_memory   := AImageInfo.Alloc.DeviceMemory;
  LImageInfo.alloc.offset          := AImageInfo.Alloc.Offset;
  LImageInfo.alloc.size            := AImageInfo.Alloc.Size;
  LImageInfo.alloc.flags           := Byte(AImageInfo.Alloc.Flags);
  LImageInfo.alloc.memory          := AImageInfo.Alloc.Memory;
  LImageInfo.image_tiling          := AImageInfo.ImageTiling;
  LImageInfo.image_layout          := AImageInfo.ImageLayout;
  LImageInfo.format                := AImageInfo.Format;
  LImageInfo.image_usage_flags     := AImageInfo.ImageUsageFlags;
  LImageInfo.sample_count          := AImageInfo.SampleCount;
  LImageInfo.level_count           := AImageInfo.LevelCount;
  LImageInfo.current_queue_family  := AImageInfo.CurrentQueueFamily;
  LImageInfo.protected_image       := AImageInfo.ProtectedImage;
  LImageInfo.ycbcr_conversion_info := gr_vk_ycbcrconversioninfo_t(AImageInfo.YcbcrConversionInfo);
  LImageInfo.sharing_mode          := AImageInfo.SharingMode;
  inherited Create(gr4d_backendtexture_create_vk(AWidth, AHeight, @LImageInfo));
end;

function TGrBackendTexture.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(gr4d_backendtexture_get_backend_api(Handle));
end;

function TGrBackendTexture.GetGlTextureInfo(
  out ATextureInfo: TGrGlTextureInfo): Boolean;
begin
  Result := gr4d_backendtexture_get_gl_framebuffer_info(Handle, gr_gl_textureinfo_t(ATextureInfo));
end;

function TGrBackendTexture.GetHeight: Integer;
begin
  Result := gr4d_backendtexture_get_height(Handle);
end;

function TGrBackendTexture.GetWidth: Integer;
begin
  Result := gr4d_backendtexture_get_width(Handle);
end;

function TGrBackendTexture.HasMipmaps: Boolean;
begin
  Result := gr4d_backendtexture_has_mipmaps(Handle);
end;

function TGrBackendTexture.IsValid: Boolean;
begin
  Result := gr4d_backendtexture_is_valid(Handle);
end;

class procedure TGrBackendTexture.__DestroyHandle(const AHandle: sk_handle_t);
begin
  gr4d_backendtexture_destroy(AHandle);
end;

{ TGrDirectContext }

procedure TGrDirectContext.AbandonContext;
begin
  gr4d_directcontext_abandon_context(Handle);
end;

function TGrDirectContext.CreateTexture(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AColor: TAlphaColorF;
  const AIsMipmapped, AIsRenderable, AIsProtected: Boolean): IGrBackendTexture;
begin
  Result := TGrBackendTexture.Wrap(gr4d_directcontext_create_texture3(Handle, AWidth, AHeight, sk_colortype_t(AColorType), @AColor, AIsMipmapped, AIsRenderable, AIsProtected));
end;

function TGrDirectContext.CreateTexture(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AColor: TAlphaColor; const AIsMipmapped,
  AIsRenderable, AIsProtected: Boolean): IGrBackendTexture;
begin
  Result := TGrBackendTexture.Wrap(gr4d_directcontext_create_texture2(Handle, AWidth, AHeight, sk_colortype_t(AColorType), AColor, AIsMipmapped, AIsRenderable, AIsProtected));
end;

function TGrDirectContext.CreateTexture(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AIsMipmapped, AIsRenderable,
  AIsProtected: Boolean): IGrBackendTexture;
begin
  Result := TGrBackendTexture.Wrap(gr4d_directcontext_create_texture(Handle, AWidth, AHeight, sk_colortype_t(AColorType), AIsMipmapped, AIsRenderable, AIsProtected));
end;

procedure TGrDirectContext.DeleteTexture(const ATexture: IGrBackendTexture);
begin
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  gr4d_directcontext_delete_texture(Handle, ATexture.Handle);
end;

procedure TGrDirectContext.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  if not Assigned(ATraceMemoryDump) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATraceMemoryDump']);
  gr4d_directcontext_dump_memory_statistics(Handle, ATraceMemoryDump.Handle);
end;

procedure TGrDirectContext.Flush;
begin
  gr4d_directcontext_flush(Handle);
end;

procedure TGrDirectContext.FlushAndSubmit(const ASyncCpu: Boolean);
begin
  gr4d_directcontext_flush_and_submit(Handle, ASyncCpu);
end;

procedure TGrDirectContext.FreeGpuResources;
begin
  gr4d_directcontext_free_gpu_resources(Handle);
end;

function TGrDirectContext.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(gr4d_directcontext_get_backend_api(Handle));
end;

function TGrDirectContext.GetMaxSurfaceSampleCountForColorType(
  const AColorType: TSkColorType): Integer;
begin
  Result := gr4d_directcontext_get_max_surface_sample_count_for_color_type(Handle, sk_colortype_t(AColorType));
end;

function TGrDirectContext.GetResourceCacheLimit: NativeUInt;
begin
  Result := gr4d_directcontext_get_resource_cache_limit(Handle);
end;

procedure TGrDirectContext.GetResourceCacheUsage(out AResources: Integer;
  out AResourcesBytes: NativeUInt);
begin
  gr4d_directcontext_get_resource_cache_usage(Handle, AResources, AResourcesBytes);
end;

class function TGrDirectContext.gr_gl_get_proc(context: Pointer;
  const name: MarshaledAString): Pointer;
begin
  Result := TGrGlGetProc(context^)(name);
end;

class function TGrDirectContext.gr_vk_get_proc(context: Pointer;
  const name: MarshaledAString; instance: gr_vk_instance_t;
  device: gr_vk_device_t): Pointer;
begin
  Result := TGrVkGetProc(context^)(name, instance, device);
end;

function TGrDirectContext.IsAbandoned: Boolean;
begin
  Result := gr4d_directcontext_is_abandoned(Handle);
end;

class function TGrDirectContext.MakeGl(const AOptions: TGrContextOptions;
  const AInterface: IGrGlInterface): IGrDirectContext;
var
  LOptions: gr_contextoptions_t;
begin
  MapContextOptions(AOptions, LOptions);
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_gl(TSkBindings.SafeHandle(AInterface), @LOptions));
end;

class function TGrDirectContext.MakeGl(
  AInterface: IGrGlInterface): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_gl(TSkBindings.SafeHandle(AInterface), nil));
end;

class function TGrDirectContext.MakeMetal(
  const ABackendContext: TGrMtlBackendContext): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_metal(@ABackendContext, nil));
end;

class function TGrDirectContext.MakeMetal(
  const ABackendContext: TGrMtlBackendContext;
  const AOptions: TGrContextOptions): IGrDirectContext;
var
  LOptions: gr_contextoptions_t;
begin
  MapContextOptions(AOptions, LOptions);
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_metal(@ABackendContext, @LOptions));
end;

class function TGrDirectContext.MakeVulkan(
  const ABackendContext: TGrVkBackendContext;
  const AOptions: TGrContextOptions): IGrDirectContext;
var
  LBackendContext: gr_vk_backendcontext_t;
  LOptions: gr_contextoptions_t;
begin
  LBackendContext.instance                  := ABackendContext.Instance;
  LBackendContext.physical_device           := ABackendContext.PhysicalDevice;
  LBackendContext.device                    := ABackendContext.Device;
  LBackendContext.queue                     := ABackendContext.Queue;
  LBackendContext.graphics_queue_index      := ABackendContext.GraphicsQueueIndex;
  LBackendContext.max_api_version           := ABackendContext.MaxApiVersion;
  LBackendContext.extensions                := TSkBindings.SafeHandle(ABackendContext.Extensions);
  LBackendContext.physical_device_features  := ABackendContext.PhysicalDeviceFeatures;
  LBackendContext.physical_device_features2 := ABackendContext.PhysicalDeviceFeatures2;
  LBackendContext.get_proc                  := TGrDirectContext.gr_vk_get_proc;
  LBackendContext.protected_context         := ABackendContext.ProtectedContext;
  LBackendContext.get_context               := @ABackendContext.GetProc;
  MapContextOptions(AOptions, LOptions);
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_vulkan(@LBackendContext, @LOptions));
end;

class function TGrDirectContext.MakeVulkan(
  const ABackendContext: TGrVkBackendContext): IGrDirectContext;
var
  LBackendContext: gr_vk_backendcontext_t;
begin
  LBackendContext.instance                  := ABackendContext.Instance;
  LBackendContext.physical_device           := ABackendContext.PhysicalDevice;
  LBackendContext.device                    := ABackendContext.Device;
  LBackendContext.queue                     := ABackendContext.Queue;
  LBackendContext.graphics_queue_index      := ABackendContext.GraphicsQueueIndex;
  LBackendContext.max_api_version           := ABackendContext.MaxApiVersion;
  LBackendContext.extensions                := TSkBindings.SafeHandle(ABackendContext.Extensions);
  LBackendContext.physical_device_features  := ABackendContext.PhysicalDeviceFeatures;
  LBackendContext.physical_device_features2 := ABackendContext.PhysicalDeviceFeatures2;
  LBackendContext.get_proc                  := TGrDirectContext.gr_vk_get_proc;
  LBackendContext.protected_context         := ABackendContext.ProtectedContext;
  LBackendContext.get_context               := @ABackendContext.GetProc;
  Result := TSkBindings.SafeCreate<TGrDirectContext>(gr4d_directcontext_make_vulkan(@LBackendContext, nil));
end;

procedure TGrDirectContext.PerformDeferredCleanup(const AMilliseconds: Int64);
begin
  gr4d_directcontext_perform_deferred_cleanup(Handle, AMilliseconds);
end;

procedure TGrDirectContext.PurgeUnlockedResources(
  const AScratchResourcesOnly: Boolean);
begin
  gr4d_directcontext_purge_unlocked_resources(Handle, AScratchResourcesOnly);
end;

procedure TGrDirectContext.PurgeUnlockedResources(
  const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean);
begin
  gr4d_directcontext_purge_unlocked_resources2(Handle, ABytesToPurge, APreferScratchResources);
end;

procedure TGrDirectContext.ReleaseResourcesAndAbandonContext;
begin
  gr4d_directcontext_release_resources_and_abandon_context(Handle);
end;

procedure TGrDirectContext.ResetContext;
begin
  gr4d_directcontext_reset_context(Handle);
end;

procedure TGrDirectContext.SetResourceCacheLimit(const AValue: NativeUInt);
begin
  gr4d_directcontext_set_resource_cache_limit(Handle, AValue);
end;

function TGrDirectContext.Submit(const ASyncCpu: Boolean): Boolean;
begin
  Result := gr4d_directcontext_submit(Handle, ASyncCpu);
end;

{ TGrGlInterface }

function TGrGlInterface.HasExtension(const AName: MarshaledAString): Boolean;
begin
  Result := gr4d_gl_interface_has_extension(Handle, AName);
end;

class function TGrGlInterface.MakeAssembled(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(gr4d_gl_interface_make_assembled(@AProc, TGrDirectContext.gr_gl_get_proc));
end;

class function TGrGlInterface.MakeAssembledGl(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(gr4d_gl_interface_make_assembled_gl(@AProc, TGrDirectContext.gr_gl_get_proc));
end;

class function TGrGlInterface.MakeAssembledGles(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(gr4d_gl_interface_make_assembled_gles(@AProc, TGrDirectContext.gr_gl_get_proc));
end;

class function TGrGlInterface.MakeAssembledWebGl(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(gr4d_gl_interface_make_assembled_webgl(@AProc, TGrDirectContext.gr_gl_get_proc));
end;

class function TGrGlInterface.MakeNative: IGrGlInterface;
begin
  Result := TSkBindings.SafeCreate<TGrGlInterface>(gr4d_gl_interface_make_native());
end;

function TGrGlInterface.Validate: Boolean;
begin
  Result := gr4d_gl_interface_validate(Handle);
end;

{ TGrPersistentCacheBaseClass }

class constructor TGrPersistentCacheBaseClass.Create;
var
  LProcs: gr_persistentcachebaseclass_procs_t;
begin
  LProcs.load  := load_proc;
  LProcs.store := store_proc;
  gr4d_persistentcachebaseclass_set_procs(@LProcs);
end;

constructor TGrPersistentCacheBaseClass.Create;
begin
  inherited Create(gr4d_persistentcachebaseclass_create(Self));
end;

class function TGrPersistentCacheBaseClass.load_proc(context: Pointer;
  const key_data: Pointer; key_size: size_t): sk_data_t;
var
  LKey: TBytes;
begin
  SetLength(LKey{%H-}, key_size);
  Move(key_data^, LKey[0], Length(LKey));
  Result := TSkObject.__ReleaseHandle(TSkData.MakeFromBytes(TGrPersistentCacheBaseClass(context).Load(LKey)));
end;

class procedure TGrPersistentCacheBaseClass.store_proc(context: Pointer;
  const key_data: Pointer; key_size: size_t; const data: Pointer; size: size_t);
var
  LData: TBytes;
  LKey: TBytes;
begin
  SetLength(LKey{%H-}, key_size);
  Move(key_data^, LKey[0], Length(LKey));
  SetLength(LData{%H-}, size);
  Move(data^, LData[0], Length(LData));
  TGrPersistentCacheBaseClass(context).Store(LKey, LData);
end;

class procedure TGrPersistentCacheBaseClass.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  gr4d_persistentcachebaseclass_destroy(AHandle);
end;

{ TGrShaderErrorHandlerBaseClass }

class procedure TGrShaderErrorHandlerBaseClass.compile_error_proc(
  context: Pointer; const shader, errors: MarshaledAString);
begin
  TGrShaderErrorHandlerBaseClass(context).CompileError(string(shader), string(errors));
end;

class constructor TGrShaderErrorHandlerBaseClass.Create;
var
  LProcs: gr_shadererrorhandlerbaseclass_procs_t;
begin
  LProcs.compile_error := compile_error_proc;
  gr4d_shadererrorhandlerbaseclass_set_procs(@LProcs);
end;

constructor TGrShaderErrorHandlerBaseClass.Create;
begin
  inherited Create(gr4d_shadererrorhandlerbaseclass_create(Self));
end;

class procedure TGrShaderErrorHandlerBaseClass.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  gr4d_shadererrorhandlerbaseclass_destroy(AHandle);
end;

{ TGrVkExtensions }

constructor TGrVkExtensions.Create;
begin
  inherited Create(gr4d_vk_extensions_create());
end;

function TGrVkExtensions.HasExtension(const AName: MarshaledAString;
  const AMinApiVersion: Cardinal): Boolean;
begin
  Result := gr4d_vk_extensions_has_extension(Handle, AName, AMinApiVersion);
end;

procedure TGrVkExtensions.Init(const AProc: TGrVkGetProc;
  const AInstance: GrVkInstance; const APhysicalDevice: GrVkPhysicalDevice;
  const AInstanceExtensions, ADeviceExtensions: TArray<MarshaledAString>);
begin
  if not Assigned(AProc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AProc']);
  gr4d_vk_extensions_init(Handle, @AProc, TGrDirectContext.gr_vk_get_proc, AInstance, APhysicalDevice, Length(AInstanceExtensions), @AInstanceExtensions[0], Length(ADeviceExtensions), @ADeviceExtensions[0]);
end;

class procedure TGrVkExtensions.__DestroyHandle(const AHandle: sk_handle_t);
begin
  gr4d_vk_extensions_destroy(AHandle);
end;

{ TSkBlender }

class function TSkBlender.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean): ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(sk4d_blender_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor));
end;

class function TSkBlender.MakeMode(const AMode: TSkBlendMode): ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(sk4d_blender_make_mode(sk_blendmode_t(AMode)));
end;

{ TSkCanvas }

procedure TSkCanvas.Clear(const AColor: TAlphaColor);
begin
  sk4d_canvas_clear(Handle, AColor);
end;

procedure TSkCanvas.Clear(const AColor: TAlphaColorF);
begin
  sk4d_canvas_clear2(Handle, @AColor);
end;

procedure TSkCanvas.ClipPath(const APath: ISkPath; const AOp: TSkClipOp;
  const AAntiAlias: Boolean);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  sk4d_canvas_clip_path(Handle, APath.Handle, sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipRect(const ARect: TRectF; const AOp: TSkClipOp;
  const AAntiAlias: Boolean);
begin
  sk4d_canvas_clip_rect(Handle, @ARect, sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp);
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  sk4d_canvas_clip_region(Handle, ARegion.Handle, sk_clipop_t(AOp));
end;

procedure TSkCanvas.ClipRoundRect(const ARoundRect: ISkRoundRect;
  const AOp: TSkClipOp; const AAntiAlias: Boolean);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  sk4d_canvas_clip_rrect(Handle, ARoundRect.Handle, sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipShader(const AShader: ISkShader; const AOp: TSkClipOp);
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  sk4d_canvas_clip_shader(Handle, AShader.Handle, sk_clipop_t(AOp));
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix);
begin
  sk4d_canvas_concat2(Handle, @AMatrix);
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix3D);
begin
  sk4d_canvas_concat(Handle, @AMatrix);
end;

procedure TSkCanvas.Discard;
begin
  sk4d_canvas_discard(Handle);
end;

procedure TSkCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string;
  const AValue; const ASize: NativeUInt);
begin
  sk4d_canvas_draw_annotation(Handle, @ARect, MarshaledAString(UTF8String(AKey)), @AValue, ASize);
end;

procedure TSkCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string);
begin
  sk4d_canvas_draw_annotation(Handle, @ARect, MarshaledAString(UTF8String(AKey)), nil, 0);
end;

procedure TSkCanvas.DrawArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_arc(Handle, @AOval, AStartAngle, ASweepAngle, AUseCenter, APaint.Handle);
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ASampling: TSkSamplingOptions; const AColors: TArray<TAlphaColor>;
  const APaint: ISkPaint);
begin
  if not Assigned(AAtlas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AAtlas']);
  if Length(ATansforms) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ATansforms']);
  if Length(ASprites) <> Length(ATansforms) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ASprites']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(ASprites)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
  sk4d_canvas_draw_atlas(Handle, AAtlas.Handle, @ATansforms[0], @ASprites[0], @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), @ASampling, nil, TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ACullRect: TRectF; const AColors: TArray<TAlphaColor>;
  const APaint: ISkPaint);
begin
  DrawAtlas(AAtlas, ATansforms, ASprites, ABlendMode, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ACullRect, AColors, APaint);
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ASampling: TSkSamplingOptions; const ACullRect: TRectF;
  const AColors: TArray<TAlphaColor>; const APaint: ISkPaint);
begin
  if not Assigned(AAtlas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AAtlas']);
  if Length(ATansforms) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ATansforms']);
  if Length(ASprites) <> Length(ATansforms) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ASprites']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(ASprites)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
  sk4d_canvas_draw_atlas(Handle, AAtlas.Handle, @ATansforms[0], @ASprites[0], @AColors[0], Length(ATansforms), sk_blendmode_t(ABlendMode), @ASampling, @ACullRect, TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const AColors: TArray<TAlphaColor>; const APaint: ISkPaint);
begin
  DrawAtlas(AAtlas, ATansforms, ASprites, ABlendMode, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), AColors, APaint);
end;

procedure TSkCanvas.DrawCircle(const ACenter: TPointF; ARadius: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_circle(Handle, @ACenter, ARadius, APaint.Handle);
end;

procedure TSkCanvas.DrawCircle(const ACenterX, ACenterY, ARadius: Single;
  const APaint: ISkPaint);
begin
  DrawCircle(TPointF.Create(ACenterX, ACenterY), ARadius, APaint);
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColorF;
  const ABlendMode: TSkBlendMode);
begin
  sk4d_canvas_draw_color2(Handle, @AColor, sk_blendmode_t(ABlendMode));
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColor;
  const ABlendMode: TSkBlendMode);
begin
  sk4d_canvas_draw_color(Handle, AColor, sk_blendmode_t(ABlendMode));
end;

procedure TSkCanvas.DrawGlyphs(const AGlyphs: TArray<Word>;
  const AMatrices: TArray<TSkRotationScaleMatrix>; const AOrigin: TPointF;
  const AFont: ISkFont; const APaint: ISkPaint);
begin
  if Length(AMatrices) <> Length(AGlyphs) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AMatrices']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_glyphs2(Handle, Length(AGlyphs), @AGlyphs[0], @AMatrices[0], @AOrigin, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawGlyphs(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AOrigin: TPointF;
  const AFont: ISkFont; const APaint: ISkPaint);
begin
  if Length(APositions) <> Length(AGlyphs) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_glyphs(Handle, Length(AGlyphs), @AGlyphs[0], @APositions[0], @AOrigin, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawImage(const AImage: ISkImage; const AX, AY: Single;
  const APaint: ISkPaint);
begin
  DrawImage(AImage, AX, AY, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), APaint);
end;

procedure TSkCanvas.DrawImage(const AImage: ISkImage; const AX, AY: Single;
  const ASampling: TSkSamplingOptions; const APaint: ISkPaint);
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  sk4d_canvas_draw_image(Handle, AImage.Handle, AX, AY, @ASampling, TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageLattice(const AImage: ISkImage;
  const ALattice: TSkLattice; const ADest: TRectF;
  const AFilterMode: TSkFilterMode; const APaint: ISkPaint);
var
  LLattice: sk_lattice_t;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  LLattice.x_divs     := @ALattice.XDivs[0];
  LLattice.y_divs     := @ALattice.YDivs[0];
  LLattice.x_count    := Length(ALattice.XDivs);
  LLattice.y_count    := Length(ALattice.YDivs);
  LLattice.rect_types := @ALattice.RectTypes[0];
  LLattice.colors     := @ALattice.Colors[0];
  if ALattice.UseBounds then
    LLattice.bounds := @ALattice.Bounds
  else
    LLattice.bounds := nil;
  sk4d_canvas_draw_image_lattice(Handle, AImage.Handle, @LLattice, @ADest, sk_filtermode_t(AFilterMode), TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageNine(const AImage: ISkImage; const ACenter: TRect;
  const ADest: TRectF; const AFilterMode: TSkFilterMode;
  const APaint: ISkPaint);
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  sk4d_canvas_draw_image_nine(Handle, AImage.Handle, @ACenter, @ADest, sk_filtermode_t(AFilterMode), TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ADest: TRectF;
  const ASampling: TSkSamplingOptions; const APaint: ISkPaint;
  const AConstraint: TSkSrcRectConstraint);
begin
  DrawImageRect(AImage, TRectF.Create(0, 0, AImage.Width, AImage.Height), ADest, ASampling, APaint, AConstraint);
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ASrc,
  ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint;
  const AConstraint: TSkSrcRectConstraint);
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  sk4d_canvas_draw_image_rect(Handle, AImage.Handle, @ASrc, @ADest, @ASampling, TSkBindings.SafeHandle(APaint), sk_srcrectconstraint_t(AConstraint));
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ASrc,
  ADest: TRectF; const APaint: ISkPaint;
  const AConstraint: TSkSrcRectConstraint);
begin
  DrawImageRect(AImage, ASrc, ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), APaint, AConstraint);
end;

procedure TSkCanvas.DrawImageRect(const AImage: ISkImage; const ADest: TRectF;
  const APaint: ISkPaint; const AConstraint: TSkSrcRectConstraint);
begin
  DrawImageRect(AImage, ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), APaint, AConstraint);
end;

procedure TSkCanvas.DrawLine(const APoint1, APoint2: TPointF;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_line(Handle, @APoint1, @APoint2, APaint.Handle);
end;

procedure TSkCanvas.DrawLine(const AX1, AY1, AX2, AY2: Single;
  const APaint: ISkPaint);
begin
  DrawLine(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), APaint);
end;

procedure TSkCanvas.DrawOval(const AOval: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_oval(Handle, @AOval, APaint.Handle);
end;

procedure TSkCanvas.DrawPaint(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_paint(Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawPatch(const ACubics: TSkPatchCubics;
  const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords;
  const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_patch(Handle, @ACubics[0], @AColors[0], @ATexCoords[0], sk_blendmode_t(ABlendMode), APaint.Handle);
end;

procedure TSkCanvas.DrawPath(const APath: ISkPath; const APaint: ISkPaint);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_path(Handle, APath.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture;
  const APaint: ISkPaint);
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  sk4d_canvas_draw_picture(Handle, APicture.Handle, nil, TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture;
  const AMatrix: TMatrix; const APaint: ISkPaint);
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  sk4d_canvas_draw_picture(Handle, APicture.Handle, @AMatrix, TSkBindings.SafeHandle(APaint));
end;

procedure TSkCanvas.DrawPoint(const APoint: TPointF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_point(Handle, @APoint, APaint.Handle);
end;

procedure TSkCanvas.DrawPoint(const AX, AY: Single; const APaint: ISkPaint);
begin
  DrawPoint(TPointF.Create(AX, AY), APaint);
end;

procedure TSkCanvas.DrawPoints(const AMode: TSkDrawPointsMode;
  const APoints: TArray<TPointF>; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_points(Handle, sk_drawpointsmode_t(AMode), Length(APoints), @APoints[0], APaint.Handle);
end;

procedure TSkCanvas.DrawRect(const ARect: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_rect(Handle, @ARect, APaint.Handle);
end;

procedure TSkCanvas.DrawRegion(const ARegion: ISkRegion;
  const APaint: ISkPaint);
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_region(Handle, ARegion.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_rrect2(Handle, @ARect, ARadiusX, ARadiusY, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRect(const ARoundRect: ISkRoundRect;
  const APaint: ISkPaint);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_rrect(Handle, ARoundRect.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawRoundRectDifference(const AOuter, AInner: ISkRoundRect;
  const APaint: ISkPaint);
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_rrect_difference(Handle, AOuter.Handle, AInner.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawSimpleText(const AText: string; const AX, AY: Single;
  const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_simple_text(Handle, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, AX, AY, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX,
  AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  if Length(AGlyphs) > 0 then
    sk4d_canvas_draw_simple_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, AX, AY, AFont.Handle, APaint.Handle);
end;

procedure TSkCanvas.DrawTextBlob(const ATextBlob: ISkTextBlob; const AX,
  AY: Single; const APaint: ISkPaint);
begin
  if not Assigned(ATextBlob) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextBlob']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_text_blob(Handle, ATextBlob.Handle, AX, AY, APaint.Handle);
end;

procedure TSkCanvas.DrawVertices(const AVertices: ISkVertices;
  const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
begin
  if not Assigned(AVertices) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AVertices']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_canvas_draw_vertices(Handle, AVertices.Handle, sk_blendmode_t(ABlendMode), APaint.Handle);
end;

function TSkCanvas.GetBaseProperties: TSkSurfaceProperties;
var
  LResult: sk_surfaceprops_t;
begin
  sk4d_canvas_get_base_props(Handle, LResult);
  Result.Flags         := TSkSurfacePropertiesFlags(Byte(LResult.flags));
  Result.PixelGeometry := TSkPixelGeometry(LResult.pixel_geometry);
end;

function TSkCanvas.GetDeviceClipBounds: TRect;
begin
  sk4d_canvas_get_device_clip_bounds(Handle, sk_irect_t(Result));
end;

function TSkCanvas.GetLocalClipBounds: TRectF;
begin
  sk4d_canvas_get_local_clip_bounds(Handle, sk_rect_t(Result));
end;

function TSkCanvas.GetLocalToDevice: TMatrix3D;
begin
  sk4d_canvas_get_local_to_device(Handle, sk_matrix44_t(Result));
end;

function TSkCanvas.GetLocalToDeviceAs3x3: TMatrix;
begin
  sk4d_canvas_get_local_to_device_as_3x3(Handle, sk_matrix_t(Result));
end;

function TSkCanvas.GetSaveCount: Integer;
begin
  Result := sk4d_canvas_get_save_count(Handle);
end;

function TSkCanvas.GetTopProperties: TSkSurfaceProperties;
var
  LResult: sk_surfaceprops_t;
begin
  sk4d_canvas_get_top_props(Handle, LResult);
  Result.Flags         := TSkSurfacePropertiesFlags(Byte(LResult.flags));
  Result.PixelGeometry := TSkPixelGeometry(LResult.pixel_geometry);
end;

function TSkCanvas.MakeSurface(const AImageInfo: TSkImageInfo): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_canvas_make_surface(Handle, @LImageInfo, nil));
end;

function TSkCanvas.MakeSurface(const AImageInfo: TSkImageInfo;
  const AProperties: TSkSurfaceProperties): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
  LProperties: sk_surfaceprops_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_canvas_make_surface(Handle, @LImageInfo, @LProperties));
end;


function TSkCanvas.QuickReject(const ARect: TRectF): Boolean;
begin
  Result := sk4d_canvas_quick_reject(Handle, @ARect);
end;

function TSkCanvas.QuickReject(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := sk4d_canvas_quick_reject2(Handle, APath.Handle);
end;

procedure TSkCanvas.ResetMatrix;
begin
  sk4d_canvas_reset_matrix(Handle);
end;

procedure TSkCanvas.Restore;
begin
  sk4d_canvas_restore(Handle);
end;

procedure TSkCanvas.RestoreToCount(const ASaveCount: Integer);
begin
  sk4d_canvas_restore_to_count(Handle, ASaveCount);
end;

procedure TSkCanvas.Rotate(const ADegrees, APX, APY: Single);
begin
  sk4d_canvas_rotate2(Handle, ADegrees, APX, APY);
end;

procedure TSkCanvas.Rotate(const ADegrees: Single);
begin
  sk4d_canvas_rotate(Handle, ADegrees);
end;

procedure TSkCanvas.RotateRadians(const ARadians: Single);
begin
  Rotate(RadToDeg(ARadians));
end;

function TSkCanvas.Save: Integer;
begin
  Result := sk4d_canvas_save(Handle);
end;

function TSkCanvas.SaveLayer(const APaint: ISkPaint;
  const ABackdrop: ISkImageFilter; const AFlags: TSkSaveLayerFlags): Integer;
begin
  Result := sk4d_canvas_save_layer(Handle, nil, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(ABackdrop), Byte(AFlags));
end;

function TSkCanvas.SaveLayer(const ABounds: TRectF; const APaint: ISkPaint;
  const ABackdrop: ISkImageFilter; const AFlags: TSkSaveLayerFlags): Integer;
begin
  Result := sk4d_canvas_save_layer(Handle, @ABounds, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(ABackdrop), Byte(AFlags));
end;

function TSkCanvas.SaveLayerAlpha(const AAlpha: Byte): Integer;
begin
  Result := sk4d_canvas_save_layer_alpha(Handle, nil, AAlpha);
end;

function TSkCanvas.SaveLayerAlpha(const ABounds: TRectF;
  const AAlpha: Byte): Integer;
begin
  Result := sk4d_canvas_save_layer_alpha(Handle, @ABounds, AAlpha);
end;

procedure TSkCanvas.Scale(const AScaleX, AScaleY: Single);
begin
  sk4d_canvas_scale(Handle, AScaleX, AScaleY);
end;

procedure TSkCanvas.SetMatrix(const AMatrix: TMatrix);
begin
  sk4d_canvas_set_matrix2(Handle, @AMatrix);
end;

procedure TSkCanvas.SetMatrix(const AMatrix: TMatrix3D);
begin
  sk4d_canvas_set_matrix(Handle, @AMatrix);
end;

procedure TSkCanvas.Skew(const ASkewX, ASkewY: Single);
begin
  sk4d_canvas_skew(Handle, ASkewX, ASkewY);
end;

procedure TSkCanvas.Translate(const ADeltaX, ADeltaY: Single);
begin
  sk4d_canvas_translate(Handle, ADeltaX, ADeltaY);
end;

class procedure TSkCanvas.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_canvas_destroy(AHandle);
end;

{ TSkCodec }

function TSkCodec.GetDimensions: TSize;
begin
  sk4d_codec_get_dimensions(Handle, sk_isize_t(Result));
end;

function TSkCodec.GetEncodedImageFormat: TSkEncodedImageFormat;
begin
  Result := TSkEncodedImageFormat(sk4d_codec_get_encoded_image_format(Handle));
end;

function TSkCodec.GetHeight: Integer;
begin
  Result := GetDimensions.cy;
end;

function TSkCodec.GetImage(const AColorType: TSkColorType;
  const AAlphaType: TSkAlphaType; AColorSpace: ISkColorSpace): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_codec_get_image(Handle, sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeHandle(AColorSpace)));
end;

function TSkCodec.GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt;
  const AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  AColorSpace: ISkColorSpace): Boolean;
begin
  Result := sk4d_codec_get_pixels(Handle, APixels, ARowBytes, sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeHandle(AColorSpace));
end;

function TSkCodec.GetWidth: Integer;
begin
  Result := GetDimensions.cx;
end;

class function TSkCodec.MakeFromFile(const AFileName: string): ISkCodec;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkCodec>(sk4d_codec_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkCodec.MakeFromStream(const AStream: TStream): ISkCodec;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkCodec>(sk4d_codec_make_from_stream(TSkObject.__ReleaseHandle(LStream)));
end;

class function TSkCodec.MakeWithCopy(const AData: Pointer;
  const ASize: NativeUInt): ISkCodec;
begin
  if ASize = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkCodec>(sk4d_codec_make_with_copy(AData, ASize));
end;

class function TSkCodec.MakeWithoutCopy(const AData: Pointer;
  const ASize: NativeUInt): ISkCodec;
begin
  if ASize = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkCodec>(sk4d_codec_make_without_copy(AData, ASize));
end;

class procedure TSkCodec.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_codec_destroy(AHandle);
end;

{ TSkAnimationCodecPlayer }

function TSkAnimationCodecPlayer.GetDimensions: TSize;
begin
  sk4d_animcodecplayer_get_dimensions(Handle, sk_isize_t(Result));
end;

function TSkAnimationCodecPlayer.GetDuration: Cardinal;
begin
  Result := sk4d_animcodecplayer_get_duration(Handle);
end;

function TSkAnimationCodecPlayer.GetFrame: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_animcodecplayer_get_frame(Handle));
end;

function TSkAnimationCodecPlayer.GetHeight: Integer;
begin
  Result := GetDimensions.cy;
end;

function TSkAnimationCodecPlayer.GetWidth: Integer;
begin
  Result := GetDimensions.cx;
end;

class function TSkAnimationCodecPlayer.MakeFromFile(
  const AFileName: string): ISkAnimationCodecPlayer;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkAnimationCodecPlayer>(sk4d_animcodecplayer_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkAnimationCodecPlayer.MakeFromStream(
  const AStream: TStream): ISkAnimationCodecPlayer;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkAnimationCodecPlayer>(sk4d_animcodecplayer_make_from_stream(TSkObject.__ReleaseHandle(LStream)));
end;

function TSkAnimationCodecPlayer.Seek(const AMilliseconds: Cardinal): Boolean;
begin
  Result := sk4d_animcodecplayer_seek(Handle, AMilliseconds);
end;

class procedure TSkAnimationCodecPlayer.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_animcodecplayer_destroy(AHandle);
end;

{ TSkColorFilter }

class function TSkColorFilter.MakeBlend(const AColor: TAlphaColorF;
  AColorSpace: ISkColorSpace; const AMode: TSkBlendMode): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_blend2(@AColor, TSkBindings.SafeHandle(AColorSpace), sk_blendmode_t(AMode)));
end;

class function TSkColorFilter.MakeBlend(const AColor: TAlphaColor;
  const AMode: TSkBlendMode): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_blend(AColor, sk_blendmode_t(AMode)));
end;

class function TSkColorFilter.MakeCompose(const AOuter,
  AInner: ISkColorFilter): ISkColorFilter;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_compose(AOuter.Handle, AInner.Handle));
end;

class function TSkColorFilter.MakeHighContrast(
  const AConfig: TSkHighContrastConfig): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_high_contrast(@AConfig));
end;

class function TSkColorFilter.MakeHSLAMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_hsla_matrix(@AMatrix));
end;

class function TSkColorFilter.MakeLighting(const AMultiply,
  AAdd: TAlphaColor): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_lighting(AMultiply, AAdd));
end;

class function TSkColorFilter.MakeLinearToSRGBGamma: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_linear_to_srgb_gamma());
end;

class function TSkColorFilter.MakeLumaColor: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_luma_color());
end;

class function TSkColorFilter.MakeMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_matrix(@AMatrix));
end;

class function TSkColorFilter.MakeOverdraw(
  const AColors: TSkOverdrawColor): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_overdraw(@AColors));
end;

class function TSkColorFilter.MakeTable(const ATableA, ATableR, ATableG,
  ATableB: TSkTableFilter): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_colorfilter_make_table(@ATableA, @ATableR, @ATableG, @ATableB));
end;

class function TSkColorFilter.MakeTable(
  const ATable: TSkTableFilter): ISkColorFilter;
begin
  Result := MakeTable(ATable, ATable, ATable, ATable);
end;

{ TSkColorSpace }

function TSkColorSpace.GammaCloseToSRGB: Boolean;
begin
  Result := sk4d_colorspace_gamma_close_to_srgb(Handle);
end;

function TSkColorSpace.GammaIsLinear: Boolean;
begin
  Result := sk4d_colorspace_gamma_is_linear(Handle);
end;

function TSkColorSpace.IsEqual(const AColorSpace: ISkColorSpace): Boolean;
begin
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  Result := sk4d_colorspace_is_equal(Handle, AColorSpace.Handle);
end;

function TSkColorSpace.IsNumericalTransferFunction(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := sk4d_colorspace_is_numerical_transfer_fn(Handle, sk_colorspacetransferfn_t(ATransferFunction));
end;

function TSkColorSpace.IsSRGB: Boolean;
begin
  Result := sk4d_colorspace_is_srgb(Handle);
end;

class function TSkColorSpace.Make(
  const AProfile: ISkColorSpaceICCProfile): ISkColorSpace;
begin
  if not Assigned(AProfile) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AProfile']);
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make(AProfile.Handle));
end;

function TSkColorSpace.MakeLinearGamma: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make_linear_gamma(Handle));
end;

class function TSkColorSpace.MakeRGB(
  const ATransferFunction: TSkColorSpaceTransferFunction;
  const AToXyzD50: TSkColorSpaceXyz): ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make_rgb(@ATransferFunction, @AToXyzD50));
end;

class function TSkColorSpace.MakeSRGB: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make_srgb());
end;

function TSkColorSpace.MakeSRGBGamma: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make_srgb_gamma(Handle));
end;

class function TSkColorSpace.MakeSRGBLinear: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_colorspace_make_srgb_linear());
end;

function TSkColorSpace.ToProfile: ISkColorSpaceICCProfile;
begin
  Result := TSkColorSpaceICCProfile.Wrap(sk4d_colorspace_to_profile(Handle));
end;

function TSkColorSpace.ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
begin
  Result := sk4d_colorspace_to_xyz(Handle, sk_colorspacexyz_t(ADest));
end;

class procedure TSkColorSpace.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_colorspace_ref(AHandle);
end;

class procedure TSkColorSpace.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_colorspace_unref(AHandle);
end;

{ TSkColorSpaceICCProfile }

class function TSkColorSpaceICCProfile.MakeFromBytes(
  const ABytes: TBytes): ISkColorSpaceICCProfile;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpaceICCProfile>(sk4d_colorspaceiccprofile_make_with_parse(@ABytes[0], Length(ABytes)));
end;

function TSkColorSpaceICCProfile.ToBytes: TBytes;
var
  LBuffer: Pointer;
  LSize: Cardinal;
begin
  LBuffer := sk4d_colorspaceiccprofile_get_buffer(Handle, @LSize);
  SetLength(Result{%H-}, LSize);
  Move(LBuffer^, Result[0], Length(Result));
end;

function TSkColorSpaceICCProfile.ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
begin
  Result := sk4d_colorspaceiccprofile_to_xyz(Handle, sk_colorspacexyz_t(ADest));
end;

class procedure TSkColorSpaceICCProfile.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_colorspaceiccprofile_destroy(AHandle);
end;

{ TSkDocument }

function TSkDocument.BeginPage(const AWidth, AHeight: Single): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(sk4d_document_begin_page(Handle, AWidth, AHeight, nil), False);
end;

function TSkDocument.BeginPage(const AWidth, AHeight: Single;
  const AContent: TRectF): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(sk4d_document_begin_page(Handle, AWidth, AHeight, @AContent), False);
end;

procedure TSkDocument.Close;
begin
  sk4d_document_close(Handle);
end;

procedure TSkDocument.EndPage;
begin
  sk4d_document_end_page(Handle);
end;

class function TSkDocument.MakePDF(const AStream: TStream): ISkDocument;
var
  LDocument: TSkDocument;
  LHandle: sk_handle_t;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := sk4d_document_make_pdf(LWStream.Handle);
  if LHandle = 0 then
    Exit(nil);
  LDocument := TSkDocument.Wrap(LHandle);
  LDocument.FHolder := LWStream;
  Result := LDocument;
end;

class function TSkDocument.MakePDF(const AStream: TStream;
  const AMetadata: TSkPDFMetadata): ISkDocument;
var
  LDocument: TSkDocument;
  LHandle: sk_handle_t;
  LMetadata: sk_pdfmetadata_t;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LMetadata.title            := MarshaledAString(UTF8String(AMetadata.Title));
  LMetadata.author           := MarshaledAString(UTF8String(AMetadata.Author));
  LMetadata.subject          := MarshaledAString(UTF8String(AMetadata.Subject));
  LMetadata.keywords         := MarshaledAString(UTF8String(AMetadata.Keywords));
  LMetadata.creator          := MarshaledAString(UTF8String(AMetadata.Creator));
  LMetadata.producer         := MarshaledAString(UTF8String(AMetadata.Producer));
  LMetadata.creation         := DateTimeToSkDateTime(AMetadata.Creation);
  LMetadata.modified         := DateTimeToSkDateTime(AMetadata.Modified);
  LMetadata.raster_dpi       := AMetadata.RasterDPI;
  LMetadata.pdfa             := AMetadata.PDFA;
  LMetadata.encoding_quality := AMetadata.EncodingQuality;
  LHandle := sk4d_document_make_pdf2(LWStream.Handle, @LMetadata);
  if LHandle = 0 then
    Exit(nil);
  LDocument := TSkDocument.Wrap(LHandle);
  LDocument.FHolder := LWStream;
  Result := LDocument;
end;

class function TSkDocument.MakeXPS(const AStream: TStream;
  const ADPI: Single): ISkDocument;
var
  LDocument: TSkDocument;
  LHandle: sk_handle_t;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := sk4d_document_make_xps(LWStream.Handle, ADPI);
  if LHandle = 0 then
    Exit(nil);
  LDocument := TSkDocument.Wrap(LHandle);
  LDocument.FHolder := LWStream;
  Result := LDocument;
end;

procedure TSkDocument.Terminate;
begin
  sk4d_document_terminate(Handle);
end;

{ TSkFont }

constructor TSkFont.Create(ATypeface: ISkTypeface; const ASize, AScaleX,
  ASkewX: Single);
begin
  inherited Create(sk4d_font_create(TSkBindings.SafeHandle(ATypeface), ASize, AScaleX, ASkewX));
end;

constructor TSkFont.Create(const AFont: ISkFont);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  inherited Create(sk4d_font_create2(AFont.Handle));
end;

function TSkFont.GetBaselineSnap: Boolean;
begin
  Result := sk4d_font_get_baseline_snap(Handle);
end;

function TSkFont.GetBounds(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<TRectF>;
begin
  SetLength(Result{%H-}, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), nil, @Result[0], TSkBindings.SafeHandle(APaint));
end;

function TSkFont.GetEdging: TSkFontEdging;
begin
  Result := TSkFontEdging(sk4d_font_get_edging(Handle));
end;

function TSkFont.GetEmbeddedBitmaps: Boolean;
begin
  Result := sk4d_font_get_embedded_bitmaps(Handle);
end;

function TSkFont.GetEmbolden: Boolean;
begin
  Result := sk4d_font_get_embolden(Handle);
end;

function TSkFont.GetForceAutoHinting: Boolean;
begin
  Result := sk4d_font_get_force_auto_hinting(Handle);
end;

function TSkFont.GetGlyphs(const AText: string): TArray<Word>;
begin
  SetLength(Result{%H-}, sk4d_font_get_glyphs_count(Handle, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING));
  sk4d_font_get_glyphs(Handle, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, @Result[0], Length(Result));
end;

function TSkFont.GetHinting: TSkFontHinting;
begin
  Result := TSkFontHinting(sk4d_font_get_hinting(Handle));
end;

function TSkFont.GetHorizontalPositions(const AGlyphs: TArray<Word>;
  const AOrigin: Single): TArray<Single>;
begin
  SetLength(Result{%H-}, Length(AGlyphs));
  sk4d_font_get_horizontal_positions(Handle, @AGlyphs[0], Length(AGlyphs), @Result[0], AOrigin);
end;

function TSkFont.GetIntercepts(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AUpperBounds, ALowerBounds: Single;
  const APaint: ISkPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
begin
  if Length(APositions) <> Length(AGlyphs) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  SetLength(Result{%H-}, sk4d_font_get_intercepts(Handle, @AGlyphs[0], Length(AGlyphs), @APositions[0], @LBounds[0], nil, TSkBindings.SafeHandle(APaint)));
  sk4d_font_get_intercepts(Handle, @AGlyphs[0], Length(AGlyphs), @APositions[0], @LBounds[0], @Result[0], TSkBindings.SafeHandle(APaint));
end;

function TSkFont.GetLinearMetrics: Boolean;
begin
  Result := sk4d_font_get_linear_metrics(Handle);
end;

function TSkFont.GetMetrics(out AMetrics: TSkFontMetrics): Single;
var
  LMetrics: sk_fontmetrics_t;
begin
  Result := sk4d_font_get_metrics(Handle, @LMetrics);
  AMetrics.Flags              := TSkFontMetricsFlags(Byte(LMetrics.flags));
  AMetrics.Top                := LMetrics.top;
  AMetrics.Ascent             := LMetrics.ascent;
  AMetrics.Descent            := LMetrics.descent;
  AMetrics.Bottom             := LMetrics.bottom;
  AMetrics.Leading            := LMetrics.leading;
  AMetrics.AvgCharWidth       := LMetrics.avg_char_width;
  AMetrics.MaxCharWidth       := LMetrics.max_char_width;
  AMetrics.XMin               := LMetrics.x_min;
  AMetrics.XMax               := LMetrics.x_max;
  AMetrics.XHeight            := LMetrics.x_height;
  AMetrics.CapHeight          := LMetrics.cap_height;
  AMetrics.UnderlineThickness := LMetrics.underline_thickness;
  AMetrics.UnderlinePosition  := LMetrics.underline_position;
  AMetrics.StrikeoutThickness := LMetrics.strikeout_thickness;
  AMetrics.StrikeoutPosition  := LMetrics.strikeout_position;
end;

function TSkFont.GetPath(const AGlyph: Word): ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_font_get_path(Handle, AGlyph));
end;

procedure TSkFont.GetPaths(const AGlyphs: TArray<Word>;
  const AProc: TSkFontPathProc);
begin
  if Assigned(AProc) then
    sk4d_font_get_paths(Handle, @AGlyphs[0], Length(AGlyphs), path_proc, @AProc);
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>;
begin
  Result := GetPositions(AGlyphs, TPointF.Create(0, 0));
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>;
  const AOrigin: TPointF): TArray<TPointF>;
begin
  SetLength(Result{%H-}, Length(AGlyphs));
  sk4d_font_get_positions(Handle, @AGlyphs[0], Length(AGlyphs), @Result[0], @AOrigin);
end;

function TSkFont.GetScaleX: Single;
begin
  Result := sk4d_font_get_scale_x(Handle);
end;

function TSkFont.GetSize: Single;
begin
  Result := sk4d_font_get_size(Handle);
end;

function TSkFont.GetSkewX: Single;
begin
  Result := sk4d_font_get_skew_x(Handle);
end;

function TSkFont.GetSpacing: Single;
begin
  Result := sk4d_font_get_metrics(Handle, nil);
end;

function TSkFont.GetSubpixel: Boolean;
begin
  Result := sk4d_font_get_subpixel(Handle);
end;

function TSkFont.GetTypeface: ISkTypeface;
begin
  Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_font_get_typeface(Handle));
end;

function TSkFont.GetTypefaceOrDefault: ISkTypeface;
begin
  Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_font_get_typeface_or_default(Handle));
end;

function TSkFont.GetWidths(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<Single>;
begin
  SetLength(Result{%H-}, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), @Result[0], nil, TSkBindings.SafeHandle(APaint));
end;

procedure TSkFont.GetWidthsAndBounds(const AGlyphs: TArray<Word>;
  out AWidths: TArray<Single>; out ABounds: TArray<TRectF>;
  const APaint: ISkPaint);
begin
  SetLength(AWidths{%H-}, Length(AGlyphs));
  SetLength(ABounds{%H-}, Length(AGlyphs));
  sk4d_font_get_widths_bounds(Handle, @AGlyphs[0], Length(AGlyphs), @AWidths[0], @ABounds[0], TSkBindings.SafeHandle(APaint));
end;

function TSkFont.IsEqual(const AFont: ISkFont): Boolean;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := sk4d_font_is_equal(Handle, AFont.Handle);
end;

function TSkFont.MakeWithSize(const ASize: Single): ISkFont;
begin
  Result := TSkFont.Create(Self);
  Result.Size := ASize;
end;

function TSkFont.MeasureText(const AText: string; out ABounds: TRectF;
  const APaint: ISkPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, @ABounds, TSkBindings.SafeHandle(APaint));
end;

function TSkFont.MeasureText(const AText: string;
  const APaint: ISkPaint): Single;
begin
  Result := sk4d_font_measure_text(Handle, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, nil, TSkBindings.SafeHandle(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  out ABounds: TRectF; const APaint: ISkPaint): Single;
begin
  if Length(AGlyphs) = 0 then
  begin
    ABounds := TRectF.Empty;
    Exit(0);
  end;
  Result := sk4d_font_measure_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, @ABounds, TSkBindings.SafeHandle(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): Single;
begin
  if Length(AGlyphs) = 0 then
    Exit(0);
  Result := sk4d_font_measure_text(Handle, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, nil, TSkBindings.SafeHandle(APaint));
end;

class procedure TSkFont.path_proc(const path: sk_path_t;
  const matrix: psk_matrix_t; context: Pointer);
var
  LPath: ISkPath;
begin
  LPath := TSkBindings.SafeCreate<TSkPath>(path, False);
  TSkFontPathProc(context^)(LPath, TMatrix(matrix^));
end;

procedure TSkFont.SetBaselineSnap(const AValue: Boolean);
begin
  sk4d_font_set_baseline_snap(Handle, AValue);
end;

procedure TSkFont.SetEdging(const AValue: TSkFontEdging);
begin
  sk4d_font_set_edging(Handle, sk_fontedging_t(AValue));
end;

procedure TSkFont.SetEmbeddedBitmaps(const AValue: Boolean);
begin
  sk4d_font_set_embedded_bitmaps(Handle, AValue);
end;

procedure TSkFont.SetEmbolden(const AValue: Boolean);
begin
  sk4d_font_set_embolden(Handle, AValue);
end;

procedure TSkFont.SetForceAutoHinting(const AValue: Boolean);
begin
  sk4d_font_set_force_auto_hinting(Handle, AValue);
end;

procedure TSkFont.SetHinting(const AValue: TSkFontHinting);
begin
  sk4d_font_set_hinting(Handle, sk_fonthinting_t(AValue));
end;

procedure TSkFont.SetLinearMetrics(const AValue: Boolean);
begin
  sk4d_font_set_linear_metrics(Handle, AValue);
end;

procedure TSkFont.SetScaleX(const AValue: Single);
begin
  sk4d_font_set_scale_x(Handle, AValue);
end;

procedure TSkFont.SetSize(const AValue: Single);
begin
  sk4d_font_set_size(Handle, AValue);
end;

procedure TSkFont.SetSkewX(const AValue: Single);
begin
  sk4d_font_set_skew_x(Handle, AValue);
end;

procedure TSkFont.SetSubpixel(const AValue: Boolean);
begin
  sk4d_font_set_subpixel(Handle, AValue);
end;

procedure TSkFont.SetTypeface(AValue: ISkTypeface);
begin
  sk4d_font_set_typeface(Handle, TSkBindings.SafeHandle(AValue));
end;

function TSkFont.UnicharsToGlyphs(
  const AUnichars: TArray<Integer>): TArray<Word>;
begin
  SetLength(Result{%H-}, Length(AUnichars));
  sk4d_font_unichars_to_glyphs(Handle, @AUnichars[0], Length(AUnichars), @Result[0]);
end;

function TSkFont.UnicharToGlyph(const AUnichar: Integer): Word;
begin
  Result := sk4d_font_unichar_to_glyph(Handle, AUnichar);
end;

class procedure TSkFont.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_font_destroy(AHandle);
end;

{ TSkImage }

function TSkImage.Encode(const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    EncodeToStream(LBytesStream, AEncodedImageFormat, AQuality);
    Result := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

function TSkImage.EncodeToFile(const AFileName: string;
  const AQuality: Integer): Boolean;
begin
  Result := EncodeToFile(AFileName, ExtensionToEncodedImageFormat(ExtractFileExt(AFileName)), AQuality);
end;

function TSkImage.EncodeToFile(const AFileName: string;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := sk4d_image_encode_to_file(Handle, MarshaledAString(UTF8String(AFileName)), sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

function TSkImage.EncodeToStream(const AStream: TStream;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): Boolean;
var
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  Result   := sk4d_image_encode_to_stream(Handle, LWStream.Handle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

function TSkImage.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(sk4d_image_get_alpha_type(Handle));
end;

function TSkImage.GetColorSpace: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_image_get_color_space(Handle));
end;

function TSkImage.GetColorType: TSkColorType;
begin
  Result := TSkColorType(sk4d_image_get_color_type(Handle));
end;

function TSkImage.GetHeight: Integer;
begin
  Result := sk4d_image_get_height(Handle);
end;

function TSkImage.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_image_get_image_info(Handle, LResult);
  Result.Width      := LResult.width;
  Result.Height     := LResult.height;
  Result.ColorType  := TSkColorType(LResult.color_type);
  Result.AlphaType  := TSkAlphaType(LResult.alpha_type);
  Result.ColorSpace := TSkBindings.SafeCreate<TSkColorSpace>(LResult.color_space);
end;

function TSkImage.GetUniqueId: NativeUInt;
begin
  Result := sk4d_image_get_unique_id(Handle);
end;

function TSkImage.GetWidth: Integer;
begin
  Result := sk4d_image_get_width(Handle);
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
  Result := sk4d_image_is_lazy_generated(Handle);
end;

function TSkImage.IsOpaque: Boolean;
begin
  Result := GetAlphaType = TSkAlphaType.Opaque;
end;

function TSkImage.IsTextureBacked: Boolean;
begin
  Result := sk4d_image_is_texture_backed(Handle);
end;

function TSkImage.IsValid(AContext: IGrDirectContext): Boolean;
begin
  Result := sk4d_image_is_valid(Handle, TSkBindings.SafeHandle(AContext));
end;

class function TSkImage.MakeCrossContext(const AContext: IGrDirectContext;
  const APixmap: ISkPixmap; const ABuildMips,
  ALimitToMaxTextureSize: Boolean): ISkImage;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_cross_context(AContext.Handle, APixmap.Handle, ABuildMips, ALimitToMaxTextureSize));
end;

class function TSkImage.MakeCrossContext(const AContext: IGrDirectContext;
  const AImageInfo: TSkImageInfo; const APixels: Pointer;
  const ARowBytes: NativeUInt; const ABuildMips,
  ALimitToMaxTextureSize: Boolean): ISkImage;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeCrossContext(AContext, LPixmap, ABuildMips, ALimitToMaxTextureSize);
end;

class function TSkImage.MakeFromAdoptedTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  AColorSpace: ISkColorSpace): ISkImage;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_adopted_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeHandle(AColorSpace)));
end;

class function TSkImage.MakeFromEncoded(const ABytes: TBytes): ISkImage;
var
  LStream: TStream;
begin
  if Length(ABytes) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ABytes']);
  LStream := TBytesStream.Create(ABytes);
  try
    Result := MakeFromEncodedStream(LStream);
  finally
    LStream.Free;
  end;
end;

class function TSkImage.MakeFromEncodedFile(const AFileName: string): ISkImage;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkImage.MakeFromEncodedStream(const AStream: TStream): ISkImage;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_encoded_stream(LStream.Handle));
end;

class function TSkImage.MakeFromPicture(const APicture: ISkPicture;
  const ADimensions: TSize; AColorSpace: ISkColorSpace; const AMatrix: TMatrix;
  const AProperties: TSkSurfaceProperties; const APaint: ISkPaint): ISkImage;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_picture(APicture.Handle, @ADimensions, @AMatrix, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(AColorSpace), @LProperties));
end;

class function TSkImage.MakeFromPicture(const APicture: ISkPicture;
  const ADimensions: TSize; AColorSpace: ISkColorSpace; const AProperties: TSkSurfaceProperties;
  const APaint: ISkPaint): ISkImage;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_picture(APicture.Handle, @ADimensions, nil, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(AColorSpace), @LProperties));
end;

class function TSkImage.MakeFromPicture(const APicture: ISkPicture;
  const ADimensions: TSize; AColorSpace: ISkColorSpace; const APaint: ISkPaint): ISkImage;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_picture(APicture.Handle, @ADimensions, nil, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(AColorSpace), nil));
end;

class function TSkImage.MakeFromPicture(const APicture: ISkPicture;
  const ADimensions: TSize; AColorSpace: ISkColorSpace; const AMatrix: TMatrix; const APaint: ISkPaint): ISkImage;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_picture(APicture.Handle, @ADimensions, @AMatrix, TSkBindings.SafeHandle(APaint), TSkBindings.SafeHandle(AColorSpace), nil));
end;

class function TSkImage.MakeFromRaster(const APixmap: ISkPixmap;
  const ARasterReleaseProc: TSkImageRasterReleaseProc): ISkImage;
var
  LProcWrapper: PSkImageRasterReleaseProcWrapper;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  if Assigned(ARasterReleaseProc) then
  begin
    New(LProcWrapper);
    LProcWrapper.Proc := ARasterReleaseProc;
    Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_raster(APixmap.Handle, raster_release_proc, LProcWrapper));
  end
  else
    Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_raster(APixmap.Handle, nil, nil));
end;

class function TSkImage.MakeFromRaster(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ARasterReleaseProc: TSkImageRasterReleaseProc): ISkImage;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeFromRaster(LPixmap, ARasterReleaseProc);
end;

class function TSkImage.MakeFromTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  AColorSpace: ISkColorSpace;
  const ATextureReleaseProc: TSkImageTextureReleaseProc): ISkImage;
var
  LProcWrapper: PSkImageTextureReleaseProcWrapper;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  if Assigned(ATextureReleaseProc) then
  begin
    New(LProcWrapper);
    LProcWrapper.Proc := ATextureReleaseProc;
    Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeHandle(AColorSpace), texture_release_proc, LProcWrapper));
  end
  else
    Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_from_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeHandle(AColorSpace), nil, nil));
end;

function TSkImage.MakeNonTextureImage: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_non_texture_image(Handle));
end;

class function TSkImage.MakeRasterCopy(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeRasterCopy(LPixmap);
end;

class function TSkImage.MakeRasterCopy(const APixmap: ISkPixmap): ISkImage;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_raster_copy(APixmap.Handle));
end;

function TSkImage.MakeRasterImage: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_raster_image(Handle));
end;

function TSkImage.MakeRawShader(const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeRawShader(ALocalMatrix, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeRawShader(const ASampling: TSkSamplingOptions;
  const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_image_make_raw_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @ASampling, nil));
end;

function TSkImage.MakeRawShader(const ALocalMatrix: TMatrix;
  const ASampling: TSkSamplingOptions; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_image_make_raw_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @ASampling, @ALocalMatrix));
end;

function TSkImage.MakeRawShader(const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeRawShader(TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeShader(const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeShader(TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeShader(const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeShader(ALocalMatrix, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeShader(const ASampling: TSkSamplingOptions;
  const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_image_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @ASampling, nil));
end;

function TSkImage.MakeShader(const ALocalMatrix: TMatrix;
  const ASampling: TSkSamplingOptions; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_image_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @ASampling, @ALocalMatrix));
end;

function TSkImage.MakeSubset(const ASubset: TRect;
  AContext: IGrDirectContext): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_subset(Handle, @ASubset, TSkBindings.SafeHandle(AContext)));
end;

function TSkImage.MakeTextureImage(const AContext: IGrDirectContext;
  const AIsMipmapped: Boolean): ISkImage;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_texture_image(Handle, AContext.Handle, AIsMipmapped));
end;

function TSkImage.MakeWithFilter(const AFilter: ISkImageFilter; const ASubset,
  AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint;
  AContext: IGrDirectContext): ISkImage;
begin
  if not Assigned(AFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFilter']);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_image_make_with_filter(Handle, TSkBindings.SafeHandle(AContext), AFilter.Handle, @ASubset, @AClipBounds, sk_irect_t(AOutSubset), sk_ipoint_t(AOffset)));
end;

function TSkImage.PeekPixels: ISkPixmap;
begin
  Result := TSkBindings.SafeCreate<TSkPixmap>(sk4d_image_peek_pixels(Handle));
end;

class procedure TSkImage.raster_release_proc(const pixels: Pointer;
  context: Pointer);
begin
  PSkImageRasterReleaseProcWrapper(context)^.Proc(pixels);
  Dispose(PSkImageRasterReleaseProcWrapper(context));
end;

function TSkImage.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer; const ACachingHint: TSkImageCachingHint;
  AContext: IGrDirectContext): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_image_read_pixels(Handle, TSkBindings.SafeHandle(AContext), ADest.Handle, ASrcX, ASrcY, sk_imagecachinghint_t(ACachingHint));
end;

function TSkImage.ReadPixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer; const ACachingHint: TSkImageCachingHint;
  AContext: IGrDirectContext): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY, ACachingHint, AContext);
end;

function TSkImage.ScalePixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt;
  const ASampling: TSkSamplingOptions;
  const ACachingHint: TSkImageCachingHint): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ScalePixels(LPixmap, ASampling, ACachingHint);
end;

function TSkImage.ScalePixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt;
  const ACachingHint: TSkImageCachingHint): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ScalePixels(LPixmap, ACachingHint);
end;

function TSkImage.ScalePixels(const ADest: ISkPixmap;
  const ASampling: TSkSamplingOptions;
  const ACachingHint: TSkImageCachingHint): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_image_scale_pixels(Handle, ADest.Handle, @ASampling, sk_imagecachinghint_t(ACachingHint));
end;

function TSkImage.ScalePixels(const ADest: ISkPixmap;
  const ACachingHint: TSkImageCachingHint): Boolean;
begin
  Result := ScalePixels(ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ACachingHint);
end;

class procedure TSkImage.texture_release_proc(context: Pointer);
begin
  PSkImageTextureReleaseProcWrapper(context)^.Proc();
  Dispose(PSkImageTextureReleaseProcWrapper(context));
end;

{ TSkImageFilter }

function TSkImageFilter.CanComputeFastBounds: Boolean;
begin
  Result := sk4d_imagefilter_can_compute_fast_bounds(Handle);
end;

function TSkImageFilter.ComputeFastBounds(const ABounds: TRectF): TRectF;
begin
  sk4d_imagefilter_compute_fast_bounds(Handle, @ABounds, sk_rect_t(Result));
end;

class function TSkImageFilter.MakeAlphaThreshold(const ARegion: TRect;
  const AInnerMin, AOuterMax: Single; AInput: ISkImageFilter): ISkImageFilter;
var
  LRegion: ISkRegion;
begin
  LRegion := TSkRegion.Create;
  LRegion.SetRect(ARegion);
  Result := MakeAlphaThreshold(LRegion, AInnerMin, AOuterMax, AInput);
end;

class function TSkImageFilter.MakeAlphaThreshold(const ARegion: ISkRegion;
  const AInnerMin, AOuterMax: Single; AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_alpha_threshold(ARegion.Handle, AInnerMin, AOuterMax, TSkBindings.SafeHandle(AInput)));
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, ABackground.Handle, TSkBindings.SafeHandle(AForeground), nil));
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  const ACropRect: TRectF; AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, ABackground.Handle, TSkBindings.SafeHandle(AForeground), @ACropRect));
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground: ISkImageFilter;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), ABackground.Handle, TSkBindings.SafeHandle(AForeground), nil));
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground: ISkImageFilter; const ACropRect: TRectF;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), ABackground.Handle, TSkBindings.SafeHandle(AForeground), @ACropRect));
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  AInput: ISkImageFilter; const ATileMode: TSkTileMode): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter;
  const ATileMode: TSkTileMode): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AColorFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorFilter']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_colorfilter(AColorFilter.Handle, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter; AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AColorFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorFilter']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_colorfilter(AColorFilter.Handle, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeCompose(const AOuter,
  AInner: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_compose(AOuter.Handle, AInner.Handle));
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single;
  const ADisplacement: ISkImageFilter; AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ADisplacement) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADisplacement']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, ADisplacement.Handle, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single;
  const ADisplacement: ISkImageFilter; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ADisplacement) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADisplacement']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, ADisplacement.Handle, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_distant_lit_diffuse(@ADirection, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_distant_lit_diffuse(@ADirection, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_distant_lit_specular(@ADirection, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_distant_lit_specular(@ADirection, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_drop_shadow(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_drop_shadow(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADeltaX, ADeltaY,
  ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_drop_shadow_only(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADeltaX, ADeltaY,
  ASigmaX, ASigmaY: Single; const AColor: TAlphaColor;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_drop_shadow_only(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage;
  const ASampling: TSkSamplingOptions): ISkImageFilter;
begin
  Result := MakeImage(AImage, TRectF.Create(0, 0, AImage.Width, AImage.Height), TRectF.Create(0, 0, AImage.Width, AImage.Height), ASampling);
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage): ISkImageFilter;
begin
  Result := MakeImage(AImage, TRectF.Create(0, 0, AImage.Width, AImage.Height), TRectF.Create(0, 0, AImage.Width, AImage.Height), TSkSamplingOptions.Create(TSkCubicResampler.Mitchell));
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage; const ASrc,
  ADest: TRectF): ISkImageFilter;
begin
  Result := MakeImage(AImage, ASrc, ADest, TSkSamplingOptions.Create(TSkCubicResampler.Mitchell));
end;

class function TSkImageFilter.MakeImage(const AImage: ISkImage; const ASrc,
  ADest: TRectF; const ASampling: TSkSamplingOptions): ISkImageFilter;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_image(AImage.Handle, @ASrc, @ADest, @ASampling));
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_magnifier(@ASrc, AInset, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_magnifier(@ASrc, AInset, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSkTileMode;
  const AConvolveAlpha: Boolean; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  if Length(AKernel) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AKernel']);
  if Length(AKernel) <> (AKernelSize.Width * AKernelSize.Height) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AKernel']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_matrix_convolution(@AKernelSize, @AKernel[0], AGain, ABias, @AKernelOffset, sk_tilemode_t(ATileMode), AConvolveAlpha, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeMatrixConvolution(const AKernelSize: TSize;
  const AKernel: TArray<Single>; const AGain, ABias: Single;
  const AKernelOffset: TPoint; const ATileMode: TSkTileMode;
  const AConvolveAlpha: Boolean; AInput: ISkImageFilter): ISkImageFilter;
begin
  if Length(AKernel) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AKernel']);
  if Length(AKernel) <> (AKernelSize.Width * AKernelSize.Height) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AKernel']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_matrix_convolution(@AKernelSize, @AKernel[0], AGain, ABias, @AKernelOffset, sk_tilemode_t(ATileMode), AConvolveAlpha, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeMatrixTransform(const AMatrix: TMatrix;
  const ASampling: TSkSamplingOptions; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_matrix_transform(@AMatrix, @ASampling, TSkBindings.SafeHandle(AInput)));
end;

class function TSkImageFilter.MakeMatrixTransform(const AMatrix: TMatrix;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := MakeMatrixTransform(AMatrix, AInput);
end;

class function TSkImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISkImageFilter): ISkImageFilter;
begin
  Result := MakeMerge([AFilter1, AFilter2]);
end;

class function TSkImageFilter.MakeMerge(const AFilters: TArray<ISkImageFilter>;
  const ACropRect: TRectF): ISkImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
begin
  SetLength(LFilters{%H-}, Length(AFilters));
  for I := 0 to Length(AFilters) - 1 do
    LFilters[I] := TSkBindings.SafeHandle(AFilters[I]);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), @ACropRect));
end;

class function TSkImageFilter.MakeMerge(const AFilter1,
  AFilter2: ISkImageFilter; const ACropRect: TRectF): ISkImageFilter;
begin
  Result := MakeMerge([AFilter1, AFilter2], ACropRect);
end;

class function TSkImageFilter.MakeMerge(
  const AFilters: TArray<ISkImageFilter>): ISkImageFilter;
var
  I: Integer;
  LFilters: TArray<sk_imagefilter_t>;
begin
  SetLength(LFilters{%H-}, Length(AFilters));
  for I := 0 to Length(AFilters) - 1 do
    LFilters[I] := TSkBindings.SafeHandle(AFilters[I]);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), nil));
end;

class function TSkImageFilter.MakeOffset(const ADeltaX, ADeltaY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_offset(ADeltaX, ADeltaY, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeOffset(const ADeltaX, ADeltaY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_offset(ADeltaX, ADeltaY, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakePicture(const APicture: ISkPicture;
  const ACropRect: TRectF): ISkImageFilter;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_picture(APicture.Handle, @ACropRect));
end;

class function TSkImageFilter.MakePicture(
  const APicture: ISkPicture): ISkImageFilter;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_picture(APicture.Handle, nil));
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_point_lit_diffuse(@ALocation, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_point_lit_diffuse(@ALocation, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_point_lit_specular(@ALocation, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_point_lit_specular(@ALocation, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeRuntimeShader(
  const AEffectBuilder: ISkRuntimeShaderBuilder;
  const AChildren: TArray<string>;
  const AInputs: TArray<ISkImageFilter>): ISkImageFilter;
var
  I: Integer;
  LChildren: TArray<MarshaledAString>;
  LChildrenStr: TArray<UTF8String>;
  LInputs: TArray<sk_imagefilter_t>;
begin
  if not Assigned(AEffectBuilder) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffectBuilder']);
  if Length(AChildren) <> Length(AInputs) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AInputs']);
  SetLength(LChildren{%H-}, Length(AChildren));
  SetLength(LChildrenStr{%H-}, Length(AChildren));
  SetLength(LInputs{%H-}, Length(AInputs));
  for I := 0 to Length(AChildren) - 1 do
  begin
    LChildrenStr[I] := UTF8String(AChildren[I]);
    LChildren[I]    := MarshaledAString(LChildrenStr[I]);
    LInputs[I]      := TSkBindings.SafeHandle(AInputs[I]);
  end;
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_runtime_shader2(AEffectBuilder.Handle, @LChildren[0], @LInputs[0], Length(AChildren)));
end;

class function TSkImageFilter.MakeRuntimeShader(
  const AEffectBuilder: ISkRuntimeShaderBuilder; const AChild: string;
  const AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AEffectBuilder) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffectBuilder']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_runtime_shader(AEffectBuilder.Handle, MarshaledAString(UTF8String(AChild)), TSkBindings.SafeHandle(AInput)));
end;

class function TSkImageFilter.MakeShader(const AShader: ISkShader;
  const ADither: Boolean; const ACropRect: TRectF): ISkImageFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_shader(AShader.Handle, ADither, @ACropRect));
end;

class function TSkImageFilter.MakeShader(const AShader: ISkShader;
  const ADither: Boolean): ISkImageFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_shader(AShader.Handle, ADither, nil));
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_spot_lit_diffuse(@ALocation, @ATarget, AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_spot_lit_diffuse(@ALocation, @ATarget, AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_spot_lit_specular(@ALocation, @ATarget, AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), nil));
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_spot_lit_specular(@ALocation, @ATarget, AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeHandle(AInput), @ACropRect));
end;

class function TSkImageFilter.MakeTile(const ASrc, ADest: TRect;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_tile(@ASrc, @ADest, TSkBindings.SafeHandle(AInput)));
end;

function TSkImageFilter.MakeWithLocalMatrix(
  const AMatrix: TMatrix): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_imagefilter_make_with_local_matrix(Handle, @AMatrix));
end;

{ TSkMaskFilter }

class function TSkMaskFilter.MakeBlur(const AStyle: TSkBlurStyle;
  const ASigma: Single; const ARespectCTM: Boolean): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_maskfilter_make_blur(sk_blurstyle_t(AStyle), ASigma, ARespectCTM));
end;

class function TSkMaskFilter.MakeShader(
  const AShader: ISkShader): ISkMaskFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_maskfilter_make_shader(AShader.Handle));
end;

class function TSkMaskFilter.MakeTable(
  const ATable: TSkTableFilter): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_maskfilter_make_table(@ATable));
end;

class function TSkMaskFilter.MakeTableClip(const AMin,
  AMax: Byte): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_maskfilter_make_table_clip(AMin, AMax));
end;

class function TSkMaskFilter.MakeTableGamma(
  const AGamma: Single): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_maskfilter_make_table_gamma(AGamma));
end;

{ TSkPaint }

constructor TSkPaint.Create;
begin
  inherited Create(sk4d_paint_create());
end;

constructor TSkPaint.Create(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  inherited Create(sk4d_paint_create2(APaint.Handle));
end;

constructor TSkPaint.Create(const AStyle: TSkPaintStyle);
begin
  Create;
  SetStyle(AStyle);
end;

function TSkPaint.GetAlpha: Byte;
begin
  Result := sk4d_paint_get_alpha(Handle);
end;

function TSkPaint.GetAlphaF: Single;
begin
  Result := sk4d_paint_get_alphaf(Handle);
end;

function TSkPaint.GetAntiAlias: Boolean;
begin
  Result := sk4d_paint_get_anti_alias(Handle);
end;

function TSkPaint.GetBlender: ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(sk4d_paint_get_blender(Handle));
end;

function TSkPaint.GetColor: TAlphaColor;
begin
  Result := sk4d_paint_get_color(Handle);
end;

function TSkPaint.GetColorF: TAlphaColorF;
begin
  sk4d_paint_get_colorf(Handle, sk_color4f_t(Result));
end;

function TSkPaint.GetColorFilter: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_paint_get_color_filter(Handle));
end;

function TSkPaint.GetDither: Boolean;
begin
  Result := sk4d_paint_get_dither(Handle);
end;

function TSkPaint.GetFillPath(const APath: ISkPath; const ACullRect: TRectF;
  const AResScale: Single): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_paint_get_fill_path(Handle, APath.Handle, @ACullRect, AResScale));
end;

function TSkPaint.GetFillPath(const APath: ISkPath): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_paint_get_fill_path(Handle, APath.Handle, nil, 1));
end;

function TSkPaint.GetImageFilter: ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(sk4d_paint_get_image_filter(Handle));
end;

function TSkPaint.GetMaskFilter: ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(sk4d_paint_get_mask_filter(Handle));
end;

function TSkPaint.GetPathEffect: ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_paint_get_path_effect(Handle));
end;

function TSkPaint.GetShader: ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_paint_get_shader(Handle));
end;

function TSkPaint.GetStrokeCap: TSkStrokeCap;
begin
  Result := TSkStrokeCap(sk4d_paint_get_stroke_cap(Handle));
end;

function TSkPaint.GetStrokeJoin: TSkStrokeJoin;
begin
  Result := TSkStrokeJoin(sk4d_paint_get_stroke_join(Handle));
end;

function TSkPaint.GetStrokeMiter: Single;
begin
  Result := sk4d_paint_get_stroke_miter(Handle);
end;

function TSkPaint.GetStrokeWidth: Single;
begin
  Result := sk4d_paint_get_stroke_width(Handle);
end;

function TSkPaint.GetStyle: TSkPaintStyle;
begin
  Result := TSkPaintStyle(sk4d_paint_get_style(Handle))
end;

procedure TSkPaint.Reset;
begin
  sk4d_paint_reset(Handle);
end;

procedure TSkPaint.SetAlpha(const AValue: Byte);
begin
  sk4d_paint_set_alpha(Handle, AValue);
end;

procedure TSkPaint.SetAlphaF(const AValue: Single);
begin
  sk4d_paint_set_alphaf(Handle, AValue);
end;

procedure TSkPaint.SetAntiAlias(const AValue: Boolean);
begin
  sk4d_paint_set_antialias(Handle, AValue);
end;

procedure TSkPaint.SetARGB(const A, R, G, B: Byte);
begin
  sk4d_paint_set_argb(Handle, A, R, G, B);
end;

procedure TSkPaint.SetBlender(AValue: ISkBlender);
begin
  sk4d_paint_set_blender(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetColor(const AValue: TAlphaColor);
begin
  sk4d_paint_set_color(Handle, AValue);
end;

procedure TSkPaint.SetColorF(const AValue: TAlphaColorF;
  AColorSpace: ISkColorSpace);
begin
  sk4d_paint_set_colorf(Handle, @AValue, TSkBindings.SafeHandle(AColorSpace));
end;

procedure TSkPaint.SetColorFilter(AValue: ISkColorFilter);
begin
  sk4d_paint_set_color_filter(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetDither(const AValue: Boolean);
begin
  sk4d_paint_set_dither(Handle, AValue);
end;

procedure TSkPaint.SetImageFilter(AValue: ISkImageFilter);
begin
  sk4d_paint_set_image_filter(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetMaskFilter(AValue: ISkMaskFilter);
begin
  sk4d_paint_set_mask_filter(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetPathEffect(AValue: ISkPathEffect);
begin
  sk4d_paint_set_path_effect(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetShader(AValue: ISkShader);
begin
  sk4d_paint_set_shader(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkPaint.SetStrokeCap(const AValue: TSkStrokeCap);
begin
  sk4d_paint_set_stroke_cap(Handle, sk_strokecap_t(AValue));
end;

procedure TSkPaint.SetStrokeJoin(const AValue: TSkStrokeJoin);
begin
  sk4d_paint_set_stroke_join(Handle, sk_strokejoin_t(AValue));
end;

procedure TSkPaint.SetStrokeMiter(const AValue: Single);
begin
  sk4d_paint_set_stroke_miter(Handle, AValue);
end;

procedure TSkPaint.SetStrokeWidth(const AValue: Single);
begin
  sk4d_paint_set_stroke_width(Handle, AValue);
end;

procedure TSkPaint.SetStyle(const AValue: TSkPaintStyle);
begin
  sk4d_paint_set_style(Handle, sk_paintstyle_t(AValue));
end;

class procedure TSkPaint.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_paint_destroy(AHandle);
end;

{ TSkOpBuilder }

procedure TSkOpBuilder.Add(const APath: ISkPath; const AOp: TSkPathOp);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  sk4d_opbuilder_add(Handle, APath.Handle, sk_pathop_t(AOp));
end;

constructor TSkOpBuilder.Create;
begin
  inherited Create(sk4d_opbuilder_create());
end;

function TSkOpBuilder.Detach: ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_opbuilder_detach(Handle));
end;

class procedure TSkOpBuilder.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_opbuilder_destroy(AHandle);
end;

{ TSkPath }

function TSkPath.Contains(const AX, AY: Single): Boolean;
begin
  Result := sk4d_path_contains(Handle, AX, AY);
end;

class function TSkPath.ConvertConicToQuads(const APoint1, APoint2,
  APoint3: TPointF; const AWeight: Single;
  const APower2: Integer): TArray<TPointF>;
begin
  SetLength(Result{%H-}, 1 + 2 * (1 shl APower2));
  sk4d_path_convert_conic_to_quads(@APoint1, @APoint2, @APoint3, AWeight, @Result[0], APower2);
end;

constructor TSkPath.Create(const ASVG: string);
begin
  inherited Create(sk4d_path_create(MarshaledAString(MarshaledAString(UTF8String(ASVG)))));
end;

constructor TSkPath.Create(const ABytes: TBytes);
var
  LStream: TStream;
begin
  LStream := TBytesStream.Create(ABytes);
  try
    Create(LStream);
  finally
    LStream.Free;
  end;
end;

constructor TSkPath.Create(const AStream: TStream);
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  inherited Create(sk4d_path_create2(LStream.Handle));
end;

function TSkPath.GetBounds: TRectF;
begin
  sk4d_path_get_bounds(Handle, sk_rect_t(Result));
end;

function TSkPath.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(sk4d_path_get_fill_type(Handle));
end;

function TSkPath.GetIterator(const AForceClose: Boolean): ISkPathIterator;
begin
  Result := TPathIterator.Create(Self, AForceClose);
end;

function TSkPath.GetLastPoint: TPointF;
begin
  if not sk4d_path_get_last_point(Handle, sk_point_t(Result)) then
    Result := TPointF.Create(0, 0);
end;

function TSkPath.GetSegmentMasks: TSkSegmentMasks;
begin
  Result := TSkSegmentMasks(Byte(sk4d_path_get_segment_masks(Handle)));
end;

function TSkPath.GetTightBounds: TRectF;
begin
  sk4d_path_get_tight_bounds(Handle, sk_rect_t(Result));
end;

function TSkPath.Interpolate(const AEnding: ISkPath;
  const AWeight: Single): ISkPath;
begin
  if not Assigned(AEnding) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEnding']);
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_path_interpolate(Handle, AEnding.Handle, AWeight));
end;

function TSkPath.IsConvex: Boolean;
begin
  Result := sk4d_path_is_convex(Handle);
end;

function TSkPath.IsEmpty: Boolean;
begin
  Result := sk4d_path_is_empty(Handle);
end;

function TSkPath.IsFinite: Boolean;
begin
  Result := sk4d_path_is_finite(Handle);
end;

function TSkPath.IsInterpolatable(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := sk4d_path_is_interpolatable(Handle, APath.Handle);
end;

function TSkPath.IsLastContourClosed: Boolean;
begin
  Result := sk4d_path_is_last_contour_closed(Handle);
end;

function TSkPath.IsLine: Boolean;
begin
  Result := sk4d_path_is_line(Handle, nil);
end;

function TSkPath.IsLine(const APoint1, APoint2: TPointF): Boolean;
var
  LLines: array[0..1] of TPointF;
begin
  LLines[0] := APoint1;
  LLines[1] := APoint2;
  Result := sk4d_path_is_line(Handle, @LLines[0]);
end;

function TSkPath.IsOval(out ARect: TRectF): Boolean;
begin
  Result := sk4d_path_is_oval(Handle, @ARect);
end;

function TSkPath.IsOval: Boolean;
begin
  Result := sk4d_path_is_oval(Handle, nil);
end;

function TSkPath.IsRect(out ARect: TRectF): Boolean;
begin
  Result := sk4d_path_is_rect(Handle, @ARect);
end;

function TSkPath.IsRect: Boolean;
begin
  Result := sk4d_path_is_rect(Handle, nil);
end;

function TSkPath.IsRoundRect(out ARoundRect: ISkRoundRect): Boolean;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Result     := sk4d_path_is_rrect(Handle, TSkBindings.SafeHandle(LRoundRect));
  if Result then
    ARoundRect := LRoundRect;
end;

function TSkPath.IsRoundRect: Boolean;
begin
  Result := sk4d_path_is_rrect(Handle, 0);
end;

function TSkPath.Op(const APath: ISkPath; const AOp: TSkPathOp): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_path_op(Handle, APath.Handle, sk_pathop_t(AOp)));
end;

function TSkPath.Serialize: TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    SerializeToStream(LBytesStream);
    Result := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

procedure TSkPath.SerializeToStream(const AStream: TStream);
var
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  sk4d_path_serialize_to_stream(Handle, LWStream.Handle);
end;

function TSkPath.ToSVG: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(sk4d_path_to_svg(Handle));
  Result  := LResult.Text;
end;

function TSkPath.Transform(const AMatrix: TMatrix): ISkPath;
begin
  Result := TSkPath.Wrap(sk4d_path_transform(Handle, @AMatrix));
end;

class procedure TSkPath.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_path_destroy(AHandle);
end;

{ TSkPath.TPathIterator }

constructor TSkPath.TPathIterator.Create(const APath: ISkPath;
  const AForceClose: Boolean);
begin
  inherited Create(sk4d_pathiterator_create(APath.Handle, AForceClose));
end;

function TSkPath.TPathIterator.GetCurrent: TSkPathIteratorElem;
begin
  Result := FCurrent;
end;

function TSkPath.TPathIterator.MoveNext: Boolean;
begin
  Result := sk4d_pathiterator_next(Handle, sk_pathiteratorelem_t(FCurrent));
end;

class procedure TSkPath.TPathIterator.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_pathiterator_destroy(AHandle);
end;

{ TSkPathBuilder }

procedure TSkPathBuilder.AddArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single);
begin
  sk4d_pathbuilder_add_arc(Handle, @AOval, AStartAngle, ASweepAngle);
end;

procedure TSkPathBuilder.AddCircle(const ACenterX, ACenterY, ARadius: Single;
  ADirection: TSkPathDirection);
begin
  sk4d_pathbuilder_add_circle(Handle,ACenterX, ACenterY, ARadius, sk_pathdirection_t(ADirection));
end;

procedure TSkPathBuilder.AddCircle(const ACenter: TPointF; ARadius: Single;
  ADirection: TSkPathDirection);
begin
  AddCircle(ACenter.X, ACenter.Y, ARadius, ADirection);
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  sk4d_pathbuilder_add_oval(Handle, @ARect, sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection);
begin
  AddOval(ARect, ADirection, 1);
end;

procedure TSkPathBuilder.AddPath(const APath: ISkPath);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  sk4d_pathbuilder_add_path(Handle, APath.Handle);
end;

procedure TSkPathBuilder.AddPolygon(const APolygon: TPolygon;
  const IsClosed: Boolean);
begin
  sk4d_pathbuilder_add_polygon(Handle, @APolygon[0], Length(APolygon), IsClosed);
end;

procedure TSkPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  if AStartIndex > 3 then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AStartIndex', AStartIndex, 0, 4]);
  sk4d_pathbuilder_add_rect(Handle, @ARect, sk_pathdirection_t(ADirection), AStartIndex);
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
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  sk4d_pathbuilder_add_rrect(Handle, ARoundRect.Handle, sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.ArcTo(const APoint1, APoint2: TPointF;
  const ARadius: Single);
begin
  sk4d_pathbuilder_arc_to3(Handle, @APoint1, @APoint2, ARadius);
end;

procedure TSkPathBuilder.ArcTo(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AForceMoveTo: Boolean);
begin
  sk4d_pathbuilder_arc_to2(Handle, @AOval, AStartAngle, ASweepAngle, AForceMoveTo);
end;

procedure TSkPathBuilder.ArcTo(const ARadius: TPointF;
  const XAxisRotate: Single; const ALargeArc: TSkPathArcSize;
  const ASweep: TSkPathDirection; const AXY: TPointF);
begin
  sk4d_pathbuilder_arc_to(Handle, @ARadius, XAxisRotate, sk_patharcsize_t(ALargeArc), sk_pathdirection_t(ASweep), @AXY);
end;

procedure TSkPathBuilder.Close;
begin
  sk4d_pathbuilder_close(Handle);
end;

procedure TSkPathBuilder.ConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  sk4d_pathbuilder_conic_to(Handle, @APoint1, @APoint2, AWeight);
end;

procedure TSkPathBuilder.ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  ConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

constructor TSkPathBuilder.Create;
begin
  inherited Create(sk4d_pathbuilder_create());
end;

constructor TSkPathBuilder.Create(const AFillType: TSkPathFillType);
begin
  Create;
  SetFillType(AFillType);
end;

constructor TSkPathBuilder.Create(const APathBuilder: ISkPathBuilder);
begin
  if not Assigned(APathBuilder) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APathBuilder']);
  inherited Create(sk4d_pathbuilder_create2(APathBuilder.Handle));
end;

procedure TSkPathBuilder.CubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  sk4d_pathbuilder_cubic_to(Handle, @APoint1, @APoint2, @APoint3);
end;

procedure TSkPathBuilder.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  CubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

function TSkPathBuilder.Detach: ISkPath;
begin
  Result := TSkPath.Wrap(sk4d_pathbuilder_detach(Handle));
end;

function TSkPathBuilder.GetBounds: TRectF;
begin
  sk4d_pathbuilder_get_bounds(Handle, sk_rect_t(Result));
end;

function TSkPathBuilder.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(sk4d_pathbuilder_get_fill_type(Handle));
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount: Integer);
begin
  IncReserve(AExtraPointCount, AExtraPointCount);
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount,
  AExtraVerbCount: Integer);
begin
  sk4d_pathbuilder_inc_reserve(Handle, AExtraPointCount, AExtraVerbCount);
end;

procedure TSkPathBuilder.LineTo(const AX, AY: Single);
begin
  LineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.LineTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_line_to(Handle, @APoint);
end;

procedure TSkPathBuilder.MoveTo(const AX, AY: Single);
begin
  MoveTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.MoveTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_move_to(Handle, @APoint);
end;

procedure TSkPathBuilder.Offset(const ADeltaX, ADeltaY: Single);
begin
  sk4d_pathbuilder_offset(Handle, ADeltaX, ADeltaY);
end;

procedure TSkPathBuilder.PolylineTo(const APoints: TArray<TPointF>);
begin
  sk4d_pathbuilder_polyline_to(Handle, @APoints[0], Length(APoints));
end;

procedure TSkPathBuilder.QuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  QuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.QuadTo(const APoint1, APoint2: TPointF);
begin
  sk4d_pathbuilder_quad_to(Handle, @APoint1, @APoint2);
end;

procedure TSkPathBuilder.RConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  sk4d_pathbuilder_r_conic_to(Handle, @APoint1, @APoint2, AWeight);
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
  sk4d_pathbuilder_r_cubic_to(Handle, @APoint1, @APoint2, @APoint3);
end;

procedure TSkPathBuilder.Reset;
begin
  sk4d_pathbuilder_reset(Handle);
end;

procedure TSkPathBuilder.RLineTo(const AX, AY: Single);
begin
  RLineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.RLineTo(const APoint: TPointF);
begin
  sk4d_pathbuilder_r_line_to(Handle, @APoint);
end;

procedure TSkPathBuilder.RQuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  RQuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.RQuadTo(const APoint1, APoint2: TPointF);
begin
  sk4d_pathbuilder_r_quad_to(Handle, @APoint1, @APoint2);
end;

procedure TSkPathBuilder.SetFillType(const AValue: TSkPathFillType);
begin
  sk4d_pathbuilder_set_filltype(Handle, sk_pathfilltype_t(AValue));
end;

function TSkPathBuilder.Snapshot: ISkPath;
begin
  Result := TSkPath.Wrap(sk4d_pathbuilder_snapshot(Handle));
end;

procedure TSkPathBuilder.ToggleInverseFillType;
begin
  sk4d_pathbuilder_toggle_inverse_filltype(Handle);
end;

class procedure TSkPathBuilder.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_pathbuilder_destroy(AHandle);
end;

{ TSkPathEffect }

class function TSkPathEffect.Make1DPath(const APath: ISkPath; const AAdvance,
  APhase: Single; const AStyle: TSkPathEffect1DStyle): ISkPathEffect;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_1dpath(APath.Handle, AAdvance, APhase, sk_patheffect1dstyle_t(AStyle)));
end;

class function TSkPathEffect.Make2DLine(const AWidth: Single;
  const AMatrix: TMatrix): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_2dline(AWidth, @AMatrix));
end;

class function TSkPathEffect.Make2DPath(const AMatrix: TMatrix;
  const APath: ISkPath): ISkPathEffect;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_2dpath(@AMatrix, APath.Handle));
end;

class function TSkPathEffect.MakeCompose(const AOuter,
  AInner: ISkPathEffect): ISkPathEffect;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_compose(AOuter.Handle, AInner.Handle));
end;

class function TSkPathEffect.MakeCorner(const ARadius: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_corner(ARadius));
end;

class function TSkPathEffect.MakeDash(const AIntervals: TArray<Single>;
  const APhase: Single): ISkPathEffect;
begin
  if Length(AIntervals) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AIntervals']);
  if Odd(Length(AIntervals)) then
    raise ESkArgumentException.CreateFmt(SParamSizeIsOdd, ['AIntervals']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_dash(@AIntervals[0], Length(AIntervals), APhase));
end;

class function TSkPathEffect.MakeDiscrete(const ASegLength, ADeviation: Single;
  const ASeedAssist: Cardinal): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_discrete(ASegLength, ADeviation, ASeedAssist));
end;

class function TSkPathEffect.MakeMatrix(const AMatrix: TMatrix): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_matrix(@AMatrix));
end;

class function TSkPathEffect.MakeMerge(const AEffect1, AEffect2: ISkPathEffect;
  const AOp: TSkPathOp): ISkPathEffect;
begin
  if not Assigned(AEffect1) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect1']);
  if not Assigned(AEffect2) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect2']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_merge(AEffect1.Handle, AEffect2.Handle, sk_pathop_t(AOp)));
end;

class function TSkPathEffect.MakeStroke(const AWidth: Single;
  const AJoin: TSkStrokeJoin; const ACap: TSkStrokeCap;
  const AMiter: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_stroke(AWidth, sk_strokejoin_t(AJoin), sk_strokecap_t(ACap), AMiter));
end;

class function TSkPathEffect.MakeStrokeAndFill: ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_stroke_and_fill());
end;

class function TSkPathEffect.MakeSum(const AEffect1,
  AEffect2: ISkPathEffect): ISkPathEffect;
begin
  if not Assigned(AEffect1) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect1']);
  if not Assigned(AEffect2) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect2']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_sum(AEffect1.Handle, AEffect2.Handle));
end;

class function TSkPathEffect.MakeTranslate(const ADeltaX,
  ADeltaY: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_translate(ADeltaX, ADeltaY));
end;

class function TSkPathEffect.MakeTrim(const AStart, AStop: Single;
  const AMode: TSkPathEffectTrimMode): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(sk4d_patheffect_make_trim(AStart, AStop, sk_patheffecttrimmode_t(AMode)));
end;

{ TSkPathMeasure }

constructor TSkPathMeasure.Create(const APath: ISkPath;
  const AForceClosed: Boolean; const AResScale: Single);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  inherited Create(sk4d_pathmeasure_create(APath.Handle, AForceClosed, AResScale));
end;

function TSkPathMeasure.GetLength: Single;
begin
  Result := sk4d_pathmeasure_get_length(Handle);
end;

function TSkPathMeasure.GetMatrix(const ADistance: Single; out AMatrix: TMatrix;
  const AMatrixFlags: TSkPathMeasureMatrixFlags): Boolean;
begin
  Result := sk4d_pathmeasure_get_matrix(Handle, ADistance, sk_matrix_t(AMatrix), Byte(AMatrixFlags));
end;

function TSkPathMeasure.GetPositionAndTangent(const ADistance: Single;
  out APosition, ATangent: TPointF): Boolean;
begin
  Result := sk4d_pathmeasure_get_position_and_tangent(Handle, ADistance, sk_point_t(APosition), sk_vector_t(ATangent));
end;

function TSkPathMeasure.GetSegment(const AStart, AStop: Single;
  const AStartWithMoveTo: Boolean): ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_pathmeasure_get_segment(Handle, AStart, AStop, AStartWithMoveTo));
end;

function TSkPathMeasure.IsClosed: Boolean;
begin
  Result := sk4d_pathmeasure_is_closed(Handle);
end;

function TSkPathMeasure.NextContour: Boolean;
begin
  Result := sk4d_pathmeasure_next_contour(Handle);
end;

class procedure TSkPathMeasure.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_pathmeasure_destroy(AHandle);
end;

{ TSkPicture }

function TSkPicture.ApproximateBytesUsed: NativeUInt;
begin
  Result := sk4d_picture_approximate_bytes_used(Handle);
end;

function TSkPicture.ApproximateOpCount(const ANested: Boolean): Integer;
begin
  Result := sk4d_picture_approximate_op_count(Handle, ANested);
end;

function TSkPicture.GetCullRect: TRectF;
begin
  sk4d_picture_get_cull_rect(Handle, sk_rect_t(Result));
end;

class function TSkPicture.MakeFromBytes(const ABytes: TBytes): ISkPicture;
var
  LStream: TStream;
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
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkPicture>(sk4d_picture_make_from_stream(LStream.Handle));
end;

function TSkPicture.MakeShader(const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode; const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_picture_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), @ALocalMatrix, nil));
end;

function TSkPicture.MakeShader(const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_picture_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), nil, nil));
end;

function TSkPicture.MakeShader(const ATileRect: TRectF; const ATileModeX,
  ATileModeY: TSkTileMode; const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_picture_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), nil, @ATileRect));
end;

function TSkPicture.MakeShader(const ATileRect: TRectF;
  const ALocalMatrix: TMatrix; const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_picture_make_shader(Handle, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), @ALocalMatrix, @ATileRect));
end;

procedure TSkPicture.Playback(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_picture_playback(Handle, ACanvas.Handle);
end;

function TSkPicture.Serialize: TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create;
  try
    SerializeToStream(LBytesStream);
    Result := Copy(LBytesStream.Bytes, 0, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

procedure TSkPicture.SerializeToStream(const AStream: TStream);
var
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  sk4d_picture_serialize_to_stream(Handle, LWStream.Handle);
end;

{ TSkPictureRecorder }

function TSkPictureRecorder.BeginRecording(const AWidth,
  AHeight: Single): ISkCanvas;
begin
  Result := BeginRecording(TRectF.Create(0, 0, AWidth, AHeight));
end;

function TSkPictureRecorder.BeginRecording(const ABounds: TRectF): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(sk4d_picturerecorder_begin_recording(Handle, @ABounds), False);
end;

constructor TSkPictureRecorder.Create;
begin
  inherited Create(sk4d_picturerecorder_create());
end;

function TSkPictureRecorder.FinishRecording: ISkPicture;
begin
  Result := TSkBindings.SafeCreate<TSkPicture>(sk4d_picturerecorder_finish_recording(Handle));
end;

function TSkPictureRecorder.FinishRecording(
  const ACullRect: TRectF): ISkPicture;
begin
  Result := TSkBindings.SafeCreate<TSkPicture>(sk4d_picturerecorder_finish_recording2(Handle, @ACullRect));
end;

class procedure TSkPictureRecorder.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_picturerecorder_destroy(AHandle);
end;

{ TSkPixmap }

constructor TSkPixmap.Create(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt);
var
  LImageInfo: sk_imageinfo_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  inherited Create(sk4d_pixmap_create(@LImageInfo, APixels, ARowBytes));
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF; const ASubset: TRectF;
  AColorSpace: ISkColorSpace): Boolean;
begin
  Result := sk4d_pixmap_erase2(Handle, @AColor, TSkBindings.SafeHandle(AColorSpace), @ASubset);
end;

function TSkPixmap.Erase(const AColor: TAlphaColor): Boolean;
begin
  Result := sk4d_pixmap_erase(Handle, AColor, nil);
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF;
  AColorSpace: ISkColorSpace): Boolean;
begin
  Result := sk4d_pixmap_erase2(Handle, @AColor, TSkBindings.SafeHandle(AColorSpace), nil);
end;

function TSkPixmap.Erase(const AColor: TAlphaColor;
  const ASubset: TRectF): Boolean;
begin
  Result := sk4d_pixmap_erase(Handle, AColor, @ASubset);
end;

function TSkPixmap.ExtractSubset(const ADest: ISkPixmap;
  const AArea: TRect): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_pixmap_extract_subset(Handle, ADest.Handle, @AArea);
end;

function TSkPixmap.GetAlpha(const AX, AY: Integer): Single;
begin
  Result := sk4d_pixmap_get_alpha(Handle, AX, AY);
end;

function TSkPixmap.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(sk4d_pixmap_get_alpha_type(Handle));
end;

function TSkPixmap.GetColor(const AX, AY: Integer): TAlphaColor;
begin
  Result := sk4d_pixmap_get_color(Handle, AX, AY);
end;

function TSkPixmap.GetColorF(const AX, AY: Integer): TAlphaColorF;
begin
  sk4d_pixmap_get_colorf(Handle, AX, AY, sk_color4f_t(Result));
end;

function TSkPixmap.GetColorSpace: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(sk4d_pixmap_get_color_space(Handle));
end;

function TSkPixmap.GetColorType: TSkColorType;
begin
  Result := TSkColorType(sk4d_pixmap_get_color_type(Handle));
end;

function TSkPixmap.GetHeight: Integer;
begin
  Result := sk4d_pixmap_get_height(Handle);
end;

function TSkPixmap.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  sk4d_pixmap_get_image_info(Handle, LResult);
  Result.Width      := LResult.width;
  Result.Height     := LResult.height;
  Result.ColorType  := TSkColorType(LResult.color_type);
  Result.AlphaType  := TSkAlphaType(LResult.alpha_type);
  Result.ColorSpace := TSkBindings.SafeCreate<TSkColorSpace>(LResult.color_space);
end;

function TSkPixmap.GetPixelAddr(const AX, AY: Integer): Pointer;
begin
  Result := sk4d_pixmap_get_pixel_addr(Handle, AX, AY);
end;

function TSkPixmap.GetPixels: Pointer;
begin
  Result := sk4d_pixmap_get_pixels(Handle);
end;

function TSkPixmap.GetRowBytes: NativeUInt;
begin
  Result := sk4d_pixmap_get_row_bytes(Handle);
end;

function TSkPixmap.GetWidth: Integer;
begin
  Result := sk4d_pixmap_get_width(Handle);
end;

function TSkPixmap.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_pixmap_read_pixels(Handle, ADest.Handle, ASrcX, ASrcY);
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

function TSkPixmap.ScalePixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt;
  const ASampling: TSkSamplingOptions): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ScalePixels(LPixmap, ASampling);
end;

function TSkPixmap.ScalePixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ScalePixels(LPixmap);
end;

function TSkPixmap.ScalePixels(const ADest: ISkPixmap;
  const ASampling: TSkSamplingOptions): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_pixmap_scale_pixels(Handle, ADest.Handle, @ASampling);
end;

function TSkPixmap.ScalePixels(const ADest: ISkPixmap): Boolean;
begin
  Result := ScalePixels(ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None));
end;

procedure TSkPixmap.SetColorSpace(AValue: ISkColorSpace);
begin
  sk4d_pixmap_set_colorspace(Handle, TSkBindings.SafeHandle(AValue));
end;

class procedure TSkPixmap.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_pixmap_destroy(AHandle);
end;

{ TSkRegion }

function TSkRegion.Contains(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := sk4d_region_contains(Handle, ARegion.Handle);
end;

function TSkRegion.Contains(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_contains2(Handle, @ARect);
end;

function TSkRegion.Contains(const AX, AY: Integer): Boolean;
begin
  Result := sk4d_region_contains3(Handle, AX, AY);
end;

constructor TSkRegion.Create(const ARect: TRect);
begin
  Create;
  SetRect(ARect);
end;

constructor TSkRegion.Create(const ARegion: ISkRegion);
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  inherited Create(sk4d_region_create2(ARegion.Handle));
end;

constructor TSkRegion.Create;
begin
  inherited Create(sk4d_region_create());
end;

function TSkRegion.GetBoundaryPath: ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(sk4d_region_get_boundary_path(Handle));
end;

function TSkRegion.GetBounds: TRect;
begin
  sk4d_region_get_bounds(Handle, sk_irect_t(Result));
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

function TSkRegion.Intersects(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_intersects2(Handle, @ARect);
end;

function TSkRegion.Intersects(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := sk4d_region_intersects(Handle, ARegion.Handle);
end;

function TSkRegion.IsComplex: Boolean;
begin
  Result := sk4d_region_is_complex(Handle);
end;

function TSkRegion.IsEmpty: Boolean;
begin
  Result := sk4d_region_is_empty(Handle);
end;

function TSkRegion.IsEqual(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := sk4d_region_is_equal(Handle, ARegion.Handle);
end;

function TSkRegion.IsRect: Boolean;
begin
  Result := sk4d_region_is_rect(Handle);
end;

function TSkRegion.Op(const ARegion: ISkRegion;
  const AOp: TSkRegionOp): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := sk4d_region_op(Handle, ARegion.Handle, sk_regionop_t(AOp));
end;

function TSkRegion.Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean;
begin
  Result := sk4d_region_op2(Handle, @ARect, sk_regionop_t(AOp));
end;

function TSkRegion.QuickContains(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_quick_contains(Handle, @ARect);
end;

function TSkRegion.QuickReject(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_quick_reject2(Handle, @ARect);
end;

function TSkRegion.QuickReject(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := sk4d_region_quick_reject(Handle, ARegion.Handle);
end;

procedure TSkRegion.SetEmpty;
begin
  sk4d_region_set_empty(Handle);
end;

function TSkRegion.SetPath(const APath: ISkPath;
  const AClip: ISkRegion): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  if not Assigned(AClip) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AClip']);
  Result := sk4d_region_set_path(Handle, APath.Handle, AClip.Handle);
end;

function TSkRegion.SetRect(const ARect: TRect): Boolean;
begin
  Result := sk4d_region_set_rect(Handle, @ARect);
end;

function TSkRegion.SetRects(const ARects: TArray<TRect>): Boolean;
begin
  if Length(ARects) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ARects']);
  Result := sk4d_region_set_rects(Handle, @ARects[0], Length(ARects));
end;

procedure TSkRegion.Translate(const ADeltaX, ADeltaY: Integer);
begin
  sk4d_region_translate(Handle, ADeltaX, ADeltaY);
end;

class procedure TSkRegion.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_region_destroy(AHandle);
end;

{ TSkRegion.TRegionCliperator }

constructor TSkRegion.TRegionCliperator.Create(const ARegion: ISkRegion;
  const AClip: TRect);
begin
  inherited Create(sk4d_regioncliperator_create(ARegion.Handle, @AClip));
end;

function TSkRegion.TRegionCliperator.GetCurrent: TRect;
begin
  sk4d_regioncliperator_get_current(Handle, sk_irect_t(Result));
end;

function TSkRegion.TRegionCliperator.MoveNext: Boolean;
begin
  Result := sk4d_regioncliperator_move_next(Handle);
end;

class procedure TSkRegion.TRegionCliperator.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_regioncliperator_destroy(AHandle);
end;

{ TSkRegion.TRegionIterator }

constructor TSkRegion.TRegionIterator.Create(const ARegion: ISkRegion);
begin
  inherited Create(sk4d_regioniterator_create(ARegion.Handle));
end;

function TSkRegion.TRegionIterator.GetCurrent: TRect;
begin
  sk4d_regioniterator_get_current(Handle, sk_irect_t(Result));
end;

function TSkRegion.TRegionIterator.MoveNext: Boolean;
begin
  Result := sk4d_regioniterator_move_next(Handle);
end;

class procedure TSkRegion.TRegionIterator.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_regioniterator_destroy(AHandle);
end;

{ TSkRegion.TRegionSpanerator }

constructor TSkRegion.TRegionSpanerator.Create(const ARegion: ISkRegion;
  const AY, ALeft, ARight: Integer);
begin
  inherited Create(sk4d_regionspanerator_create(ARegion.Handle, AY, ALeft, ARight));
end;

function TSkRegion.TRegionSpanerator.GetCurrent: TPoint;
begin
  Result := FCurrent;
end;

function TSkRegion.TRegionSpanerator.MoveNext: Boolean;
begin
  Result := sk4d_regionspanerator_next(Handle, sk_ipoint_t(FCurrent));
end;

class procedure TSkRegion.TRegionSpanerator.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_regionspanerator_destroy(AHandle);
end;

{ TSkRoundRect }

function TSkRoundRect.Contains(const ARect: TRect): Boolean;
begin
  Result := sk4d_rrect_contains(Handle, @ARect);
end;

constructor TSkRoundRect.Create(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  Create;
  SetRect(ARect, ARadiusX, ARadiusY);
end;

constructor TSkRoundRect.Create(const ARoundRect: ISkRoundRect);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  inherited Create(sk4d_rrect_create2(ARoundRect.Handle));
end;

constructor TSkRoundRect.Create;
begin
  inherited Create(sk4d_rrect_create());
end;

constructor TSkRoundRect.Create(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  Create;
  SetRect(ARect, ARadii);
end;

procedure TSkRoundRect.Deflate(const ADeltaX, ADeltaY: Single);
begin
  sk4d_rrect_deflate(Handle, ADeltaX, ADeltaY);
end;

function TSkRoundRect.GetHeight: Single;
begin
  Result := sk4d_rrect_get_height(Handle);
end;

function TSkRoundRect.GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
begin
  sk4d_rrect_get_radii(Handle, sk_rrectcorner_t(ACorner), sk_vector_t(Result));
end;

function TSkRoundRect.GetRect: TRectF;
begin
  sk4d_rrect_get_rect(Handle, sk_rect_t(Result));
end;

function TSkRoundRect.GetSimpleRadii: TPointF;
begin
  sk4d_rrect_get_simple_radii(Handle, sk_vector_t(Result));
end;

function TSkRoundRect.GetWidth: Single;
begin
  Result := sk4d_rrect_get_width(Handle);
end;

procedure TSkRoundRect.Inflate(const ADeltaX, ADeltaY: Single);
begin
  sk4d_rrect_inflate(Handle, ADeltaX, ADeltaY);
end;

function TSkRoundRect.IsComplex: Boolean;
begin
  Result := sk4d_rrect_is_complex(Handle);
end;

function TSkRoundRect.IsEmpty: Boolean;
begin
  Result := sk4d_rrect_is_empty(Handle);
end;

function TSkRoundRect.IsEqual(const ARoundRect: ISkRoundRect): Boolean;
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  Result := sk4d_rrect_is_equal(Handle, ARoundRect.Handle);
end;

function TSkRoundRect.IsNinePatch: Boolean;
begin
  Result := sk4d_rrect_is_nine_patch(Handle);
end;

function TSkRoundRect.IsOval: Boolean;
begin
  Result := sk4d_rrect_is_oval(Handle);
end;

function TSkRoundRect.IsRect: Boolean;
begin
  Result := sk4d_rrect_is_rect(Handle);
end;

function TSkRoundRect.IsSimple: Boolean;
begin
  Result := sk4d_rrect_is_simple(Handle);
end;

function TSkRoundRect.IsValid: Boolean;
begin
  Result := sk4d_rrect_is_valid(Handle);
end;

procedure TSkRoundRect.Offset(const ADeltaX, ADeltaY: Single);
begin
  sk4d_rrect_offset(Handle, ADeltaX, ADeltaY);
end;

procedure TSkRoundRect.SetEmpty;
begin
  sk4d_rrect_set_empty(Handle);
end;

procedure TSkRoundRect.SetNinePatch(const ARect: TRectF; const ARadiusLeft,
  ARadiusTop, ARadiusRight, ARadiusBottom: Single);
begin
  sk4d_rrect_set_nine_patch(Handle, @ARect, ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom);
end;

procedure TSkRoundRect.SetOval(const ARect: TRectF);
begin
  sk4d_rrect_set_oval(Handle, @ARect);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  sk4d_rrect_set_rect2(Handle, @ARect, @ARadii);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  sk4d_rrect_set_rect3(Handle, @ARect, ARadiusX, ARadiusY);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF);
begin
  sk4d_rrect_set_rect(Handle, @ARect);
end;

function TSkRoundRect.Transform(const AMatrix: TMatrix): ISkRoundRect;
begin
  Result := TSkBindings.SafeCreate<TSkRoundRect>(sk4d_rrect_transform(Handle, @AMatrix));
end;

class procedure TSkRoundRect.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_rrect_destroy(AHandle);
end;

{ TSkRuntimeEffect }

function TSkRuntimeEffect.ChildExists(const AName: string): Boolean;
begin
  Result := IndexOfChild(AName) >= 0;
end;

function TSkRuntimeEffect.GetChildCount: Integer;
begin
  Result := sk4d_runtimeeffect_get_child_count(Handle);
end;

function TSkRuntimeEffect.GetChildName(const AIndex: Integer): string;
var
  LResult: ISkString;
begin
  if (AIndex < 0) or (AIndex >= GetChildCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetChildCount]);
  LResult := TSkString.Wrap(sk4d_runtimeeffect_get_child_name(Handle, AIndex));
  Result  := LResult.Text;
end;

function TSkRuntimeEffect.GetChildType(
  const AIndex: Integer): TSkRuntimeEffectChildType;
begin
  if (AIndex < 0) or (AIndex >= GetChildCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetChildCount]);
  Result := TSkRuntimeEffectChildType(sk4d_runtimeeffect_get_child_type(Handle, AIndex));
end;

function TSkRuntimeEffect.GetChildTypeByName(
  const AName: string): TSkRuntimeEffectChildType;
begin
  Result := GetChildType(IndexOfChild(AName));
end;

function TSkRuntimeEffect.GetUniformCount: Integer;
begin
  Result := sk4d_runtimeeffect_get_uniform_count(Handle);
end;

function TSkRuntimeEffect.GetUniformDataSize: NativeUInt;
begin
  Result := sk4d_runtimeeffect_get_uniform_data_size(Handle);
end;

function TSkRuntimeEffect.GetUniformName(const AIndex: Integer): string;
var
  LResult: ISkString;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetUniformCount]);
  LResult := TSkString.Wrap(sk4d_runtimeeffect_get_uniform_name(Handle, AIndex));
  Result  := LResult.Text;
end;

function TSkRuntimeEffect.GetUniformOffset(const AIndex: Integer): NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetUniformCount]);
  Result := sk4d_runtimeeffect_get_uniform_offset(Handle, AIndex);
end;

function TSkRuntimeEffect.GetUniformOffsetByName(
  const AName: string): NativeUInt;
begin
  Result := GetUniformOffset(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniformType(
  const AIndex: Integer): TSkRuntimeEffectUniformType;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetUniformCount]);
  Result := TSkRuntimeEffectUniformType(sk4d_runtimeeffect_get_uniform_type(Handle, AIndex));
end;

function TSkRuntimeEffect.GetUniformTypeByName(
  const AName: string): TSkRuntimeEffectUniformType;
begin
  Result := GetUniformType(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniformTypeCount(const AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', AIndex, 0, GetUniformCount]);
  Result := sk4d_runtimeeffect_get_uniform_type_count(Handle, AIndex);
end;

function TSkRuntimeEffect.GetUniformTypeCountByName(
  const AName: string): Integer;
begin
  Result := GetUniformTypeCount(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.IndexOfChild(const AName: string): Integer;
begin
  Result := sk4d_runtimeeffect_index_of_child(Handle, MarshaledAString(UTF8String(AName)));
end;

function TSkRuntimeEffect.IndexOfUniform(const AName: string): Integer;
begin
  Result := sk4d_runtimeeffect_index_of_uniform(Handle, MarshaledAString(UTF8String(AName)));
end;

function TSkRuntimeEffect.IsUniformTypeOrdinal(const AIndex: Integer): Boolean;
begin
  Result := (GetUniformType(AIndex) in [TSkRuntimeEffectUniformType.Int, TSkRuntimeEffectUniformType.Int2, TSkRuntimeEffectUniformType.Int3, TSkRuntimeEffectUniformType.Int4]);
end;

function TSkRuntimeEffect.IsUniformTypeOrdinalByName(
  const AName: string): Boolean;
begin
  Result := IsUniformTypeOrdinal(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.MakeBlender(const AUniforms;
  const AChildren: TArray<ISkFlattenable>): ISkBlender;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  Result := TSkBindings.SafeCreate<TSkBlender>(sk4d_runtimeeffect_make_blender(Handle, @AUniforms, @LChildren[0]));
end;

function TSkRuntimeEffect.MakeColorFilter(const AUniforms;
  const AChildren: TArray<ISkFlattenable>): ISkColorFilter;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  Result := TSkBindings.SafeCreate<TSkColorFilter>(sk4d_runtimeeffect_make_color_filter(Handle, @AUniforms, @LChildren[0]));
end;

class function TSkRuntimeEffect.MakeForBlender(
  const ASkSL: MarshaledAString): ISkRuntimeEffect;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_blender(ASkSL, 0));
end;

class function TSkRuntimeEffect.MakeForBlender(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
begin
  Result := MakeForBlender(MarshaledAString(UTF8String(ASkSL)), AErrorText);
end;

class function TSkRuntimeEffect.MakeForBlender(const ASkSL: MarshaledAString;
  out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_blender(ASkSL, LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForBlender(
  const ASkSL: string): ISkRuntimeEffect;
begin
  Result := MakeForBlender(MarshaledAString(UTF8String(ASkSL)));
end;

class function TSkRuntimeEffect.MakeForColorFilter(
  const ASkSL: string): ISkRuntimeEffect;
begin
  Result := MakeForColorFilter(MarshaledAString(UTF8String(ASkSL)));
end;

class function TSkRuntimeEffect.MakeForColorFilter(
  const ASkSL: MarshaledAString; out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_color_filter(ASKSL, LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForColorFilter(
  const ASkSL: MarshaledAString): ISkRuntimeEffect;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_color_filter(ASkSL, 0));
end;

class function TSkRuntimeEffect.MakeForColorFilter(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
begin
  Result := MakeForColorFilter(MarshaledAString(UTF8String(ASkSL)), AErrorText);
end;

class function TSkRuntimeEffect.MakeForShader(
  const ASkSL: string): ISkRuntimeEffect;
begin
  Result := MakeForShader(MarshaledAString(UTF8String(ASkSL)));
end;

class function TSkRuntimeEffect.MakeForShader(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
begin
  Result := MakeForShader(MarshaledAString(UTF8String(ASkSL)), AErrorText);
end;

class function TSkRuntimeEffect.MakeForShader(const ASkSL: MarshaledAString;
  out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_shader(ASkSL, LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForShader(
  const ASkSL: MarshaledAString): ISkRuntimeEffect;
begin
  if Length(ASKSL) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(sk4d_runtimeeffect_make_for_shader(ASkSL, 0));
end;

function TSkRuntimeEffect.MakeImage(const AUniforms;
  const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo;
  const AMipmapped: Boolean; const AContext: IGrDirectContext): ISkImage;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
  LImageInfo: sk_imageinfo_t;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_runtimeeffect_make_image(Handle, TSkBindings.SafeHandle(AContext), @AUniforms, @LChildren[0], nil, @LImageInfo, AMipmapped));
end;

function TSkRuntimeEffect.MakeImage(const AUniforms;
  const AChildren: TArray<ISkFlattenable>; const AImageInfo: TSkImageInfo;
  const ALocalMatrix: TMatrix; const AMipmapped: Boolean;
  const AContext: IGrDirectContext): ISkImage;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
  LImageInfo: sk_imageinfo_t;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_runtimeeffect_make_image(Handle, TSkBindings.SafeHandle(AContext), @AUniforms, @LChildren[0], @ALocalMatrix, @LImageInfo, AMipmapped));
end;

function TSkRuntimeEffect.MakeShader(const AUniforms;
  const AChildren: TArray<ISkFlattenable>): ISkShader;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_runtimeeffect_make_shader(Handle, @AUniforms, @LChildren[0], nil));
end;

function TSkRuntimeEffect.MakeShader(const AUniforms;
  const AChildren: TArray<ISkFlattenable>;
  const ALocalMatrix: TMatrix): ISkShader;
var
  I: Integer;
  LChildren: TArray<sk_handle_t>;
begin
  SetLength(LChildren{%H-}, Length(AChildren));
  for I := 0 to Length(AChildren) - 1 do
    LChildren[I] := TSkBindings.SafeHandle(AChildren[I]);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_runtimeeffect_make_shader(Handle, @AUniforms, @LChildren[0], @ALocalMatrix));
end;

function TSkRuntimeEffect.UniformExists(const AName: string): Boolean;
begin
  Result := IndexOfUniform(AName) >= 0;
end;

{ TSkRuntimeEffectBuilder }

procedure TSkRuntimeEffectBuilder.AfterConstruction;
begin
  inherited;
  FEffect := TSkRuntimeEffect.Wrap(sk4d_runtimeeffectbuilder_get_effect(Handle), False);
end;

function TSkRuntimeEffectBuilder.GetEffect: TSkRuntimeEffect;
begin
  Result := FEffect as TSkRuntimeEffect;
end;

procedure TSkRuntimeEffectBuilder.SetChild(const AName: string;
  const AChild: ISkBlender);
begin
  if FEffect.GetChildTypeByName(AName) <> TSkRuntimeEffectChildType.Blender then
    raise ESkException.Create(SInvalidOperation);
  sk4d_runtimeeffectbuilder_set_child3(Handle, MarshaledAString(UTF8String(AName)), TSkBindings.SafeHandle(AChild));
end;

procedure TSkRuntimeEffectBuilder.SetChild(const AName: string;
  const AChild: ISkShader);
begin
  if FEffect.GetChildTypeByName(AName) <> TSkRuntimeEffectChildType.Shader then
    raise ESkException.Create(SInvalidOperation);
  sk4d_runtimeeffectbuilder_set_child(Handle, MarshaledAString(UTF8String(AName)), TSkBindings.SafeHandle(AChild));
end;

procedure TSkRuntimeEffectBuilder.SetChild(const AName: string;
  const AChild: ISkColorFilter);
begin
  if FEffect.GetChildTypeByName(AName) <> TSkRuntimeEffectChildType.ColorFilter then
    raise ESkException.Create(SInvalidOperation);
  sk4d_runtimeeffectbuilder_set_child2(Handle, MarshaledAString(UTF8String(AName)), TSkBindings.SafeHandle(AChild));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt4);
var
  LValue: TSkRuntimeEffectFloat4;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    LValue := TSkRuntimeEffectFloat4.Create(AValue.V1, AValue.V2, AValue.V3, AValue.V4);
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt3);
var
  LValue: TSkRuntimeEffectFloat3;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    LValue := TSkRuntimeEffectFloat3.Create(AValue.V1, AValue.V2, AValue.V3);
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: Single);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TArray<Single>);
begin
  SetUniform(AName, AValue[0], Length(AValue) * SizeOf(Single));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt2);
var
  LValue: TSkRuntimeEffectFloat2;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    LValue := TSkRuntimeEffectFloat2.Create(AValue.V1, AValue.V2);
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string; const AData;
  const ASize: NativeUInt);
const
  UniformCount: array[TSkRuntimeEffectUniformType] of Integer = (1, 2, 3, 4, 4, 9, 16, 1, 2, 3, 4);
var
  LSize: NativeUInt;
begin
  if FEffect.IsUniformTypeOrdinalByName(AName) then
    LSize := UniformCount[FEffect.GetUniformTypeByName(AName)] * FEffect.GetUniformTypeCountByName(AName) * SizeOf(Integer)
  else
    LSize := UniformCount[FEffect.GetUniformTypeByName(AName)] * FEffect.GetUniformTypeCountByName(AName) * SizeOf(Single);
  if ASize <> LSize then
    raise ESkException.Create(SInvalidOperation);
  sk4d_runtimeeffectbuilder_set_uniform(Handle, MarshaledAString(UTF8String(AName)), @AData);
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: Integer);
var
  LValue: Single;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    LValue := AValue;
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TArray<Integer>);
var
  I: Integer;
  LValue: TArray<Single>;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    SetLength(LValue{%H-}, Length(AValue));
    for I := 0 to Length(AValue) - 1 do
      LValue[I] := AValue[I];
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue[0], Length(AValue) * SizeOf(Integer));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TPoint);
var
  LValue: TSkRuntimeEffectFloat2;
begin
  if not FEffect.IsUniformTypeOrdinalByName(AName) then
  begin
    LValue := TSkRuntimeEffectFloat2.Create(AValue.X, AValue.Y);
    SetUniform(AName, LValue);
  end
  else
    SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat2x2);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat3x3);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat4x4);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TMatrix);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat4);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TPointF);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TAlphaColorF);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat2);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffectBuilder.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat3);
begin
  SetUniform(AName, AValue, SizeOf(AValue));
end;

{ TSkRuntimeBlenderBuilder }

constructor TSkRuntimeBlenderBuilder.Create(const AEffect: ISkRuntimeEffect);
begin
  if not Assigned(AEffect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect']);
  inherited Create(sk4d_runtimeblendbuilder_create(AEffect.Handle));
end;

function TSkRuntimeBlenderBuilder.MakeBlender: ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(sk4d_runtimeblendbuilder_make_blender(Handle));
end;

class procedure TSkRuntimeBlenderBuilder.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_runtimeblendbuilder_destroy(AHandle);
end;

{ TSkRuntimeShaderBuilder }

constructor TSkRuntimeShaderBuilder.Create(const AEffect: ISkRuntimeEffect);
begin
  if not Assigned(AEffect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect']);
  inherited Create(sk4d_runtimeshaderbuilder_create(AEffect.Handle));
end;

function TSkRuntimeShaderBuilder.MakeImage(const AImageInfo: TSkImageInfo;
  const AMipmapped: Boolean; const AContext: IGrDirectContext): ISkImage;
var
  LImageInfo: sk_imageinfo_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_runtimeshaderbuilder_make_image(Handle, TSkBindings.SafeHandle(AContext), nil, @LImageInfo, AMipmapped));
end;

function TSkRuntimeShaderBuilder.MakeImage(const AImageInfo: TSkImageInfo;
  const ALocalMatrix: TMatrix; const AMipmapped: Boolean;
  const AContext: IGrDirectContext): ISkImage;
var
  LImageInfo: sk_imageinfo_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_runtimeshaderbuilder_make_image(Handle, TSkBindings.SafeHandle(AContext), @ALocalMatrix, @LImageInfo, AMipmapped));
end;

function TSkRuntimeShaderBuilder.MakeShader(
  const ALocalMatrix: TMatrix): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_runtimeshaderbuilder_make_shader(Handle, @ALocalMatrix));
end;

function TSkRuntimeShaderBuilder.MakeShader: ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_runtimeshaderbuilder_make_shader(Handle, nil));
end;

class procedure TSkRuntimeShaderBuilder.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_runtimeshaderbuilder_destroy(AHandle);
end;

{ TSkShader }

class function TSkShader.MakeBlend(const AMode: TSkBlendMode; const ADest,
  ASrc: ISkShader): ISkShader;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_blend(sk_blendmode_t(AMode), ADest.Handle, ASrc.Handle));
end;

class function TSkShader.MakeColor(const AColor: TAlphaColor): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_color(AColor));
end;

class function TSkShader.MakeColor(const AColor: TAlphaColorF;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_color2(@AColor, TSkBindings.SafeHandle(AColorSpace)));
end;

class function TSkShader.MakeEmpty: ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_empty());
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], ALocalMatrix, nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], ALocalMatrix, nil, ATileMode);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientLinear(AStart, AEnd, [AColor1, AColor2], nil, ATileMode);
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColor>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_linear(@LPoints[0], @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_linear2(@LPoints[0], @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_linear2(@LPoints[0], @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_linear(@LPoints[0], @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF;
  const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], ALocalMatrix, nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColor>;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_radial(@ACenter, ARadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor;
  const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], ALocalMatrix, nil, ATileMode);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor;
  const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientRadial(ACenter, ARadius, [ACenterColor, AEdgeColor], nil, ATileMode);
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColor>;
  const ALocalMatrix: TMatrix; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_radial(@ACenter, ARadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const ALocalMatrix: TMatrix; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_radial2(@ACenter, ARadius, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_radial2(@ACenter, ARadius, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single; AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_sweep2(ACenter.X, ACenter.Y, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, @ALocalMatrix));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; const AStartAngle, AEndAngle: Single;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_sweep2(ACenter.X, ACenter.Y, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, nil));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode; const AStartAngle,
  AEndAngle: Single): ISkShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2], ALocalMatrix, nil, ATileMode, AStartAngle, AEndAngle);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single): ISkShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2], nil, ATileMode, AStartAngle, AEndAngle);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColor>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; const AStartAngle,
  AEndAngle: Single): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_sweep(ACenter.X, ACenter.Y, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, nil));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_sweep(ACenter.X, ACenter.Y, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, @ALocalMatrix));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode; const AStartAngle, AEndAngle: Single;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2], ALocalMatrix, nil, ATileMode, AStartAngle, AEndAngle, AColorSpace);
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single; AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientSweep(ACenter, [AColor1, AColor2], nil, ATileMode, AStartAngle, AEndAngle, AColorSpace);
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColor>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_two_point_conical(@AStart, AStartRadius, @AEnd, AEndRadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_two_point_conical(@AStart, AStartRadius, @AEnd, AEndRadius, @AColors[0], @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientTwoPointConical(AStart, AStartRadius, AEnd, AEndRadius, [AColor1, AColor2], ALocalMatrix, nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := MakeGradientTwoPointConical(AStart, AStartRadius, AEnd, AEndRadius, [AColor1, AColor2], nil, ATileMode, AColorSpace);
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientTwoPointConical(AStart, AStartRadius, AEnd, AEndRadius, [AColor1, AColor2], nil, ATileMode);
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix;
  const ATileMode: TSkTileMode): ISkShader;
begin
  Result := MakeGradientTwoPointConical(AStart, AStartRadius, AEnd, AEndRadius, [AColor1, AColor2], ALocalMatrix, nil, ATileMode);
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_two_point_conical2(@AStart, AStartRadius, @AEnd, AEndRadius, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), @ALocalMatrix));
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if (Length(APositions) > 0) and (Length(APositions) <> Length(AColors)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_gradient_two_point_conical2(@AStart, AStartRadius, @AEnd, AEndRadius, @AColors[0], TSkBindings.SafeHandle(AColorSpace), @APositions[0], Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeImage(const AImage: ISkImage;
  const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions;
  const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  Result := AImage.MakeShader(ALocalMatrix, ASampling, ATileModeX, ATileModeY);
end;

class function TSkShader.MakeImage(const AImage: ISkImage;
  const ASampling: TSkSamplingOptions; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  Result := AImage.MakeShader(ASampling, ATileModeX, ATileModeY);
end;

class function TSkShader.MakeImage(const AImage: ISkImage; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  Result := AImage.MakeShader(ATileModeX, ATileModeY);
end;

class function TSkShader.MakeImage(const AImage: ISkImage;
  const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  Result := AImage.MakeShader(ALocalMatrix, ATileModeX, ATileModeY);
end;

class function TSkShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @ATileSize));
end;

class function TSkShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil));
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @ATileSize));
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil));
end;

class function TSkShader.MakePicture(const APicture: ISkPicture;
  const ATileRect: TRectF; const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode; const AFilterMode: TSkFilterMode): ISkShader;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := APicture.MakeShader(ATileRect, ALocalMatrix, ATileModeX, ATileModeY, AFilterMode);
end;

class function TSkShader.MakePicture(const APicture: ISkPicture;
  const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := APicture.MakeShader(ATileModeX, ATileModeY, AFilterMode);
end;

class function TSkShader.MakePicture(const APicture: ISkPicture;
  const ALocalMatrix: TMatrix; const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := APicture.MakeShader(ALocalMatrix, ATileModeX, ATileModeY, AFilterMode);
end;

class function TSkShader.MakePicture(const APicture: ISkPicture;
  const ATileRect: TRectF; const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := APicture.MakeShader(ATileRect, ATileModeX, ATileModeY, AFilterMode);
end;

function TSkShader.MakeWithColorFilter(
  const AColorFilter: ISkColorFilter): ISkShader;
begin
  if not Assigned(AColorFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorFilter']);
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_with_color_filter(Handle, AColorFilter.Handle));
end;

function TSkShader.MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(sk4d_shader_make_with_local_matrix(Handle, @AMatrix));
end;

{ TSkSurface }

procedure TSkSurface.Draw(const ACanvas: ISkCanvas; const AX, AY: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_surface_draw(Handle, ACanvas.Handle, AX, AY, TSkBindings.SafeHandle(APaint));
end;

procedure TSkSurface.Flush;
begin
  sk4d_surface_flush(Handle);
end;

procedure TSkSurface.FlushAndSubmit(const ASyncCpu: Boolean);
begin
  sk4d_surface_flush_and_submit(Handle, nil, 0, 0, ASyncCpu);
end;

procedure TSkSurface.FlushAndSubmit(
  const ASemaphores: TArray<IGrBackendSemaphore>;
  const ANewState: IGrBackendSurfaceMutableState; const ASyncCpu: Boolean);
var
  I: Integer;
  LSemaphores: TArray<gr_backendsemaphore_t>;
begin
  SetLength(LSemaphores{%H-}, Length(ASemaphores));
  for I := 0 to Length(ASemaphores) - 1 do
  begin
    if not Assigned(ASemaphores[I]) then
      raise ESkException.Create(SInvalidOperation);
    LSemaphores[I] := ASemaphores[I].Handle;
  end;
  sk4d_surface_flush_and_submit(Handle, @LSemaphores[0], Length(ASemaphores), TSkBindings.SafeHandle(ANewState), ASyncCpu);
end;

function TSkSurface.GetCanvas: ISkCanvas;
begin
  if not Assigned(FCanvas) then
    FCanvas := TSkBindings.SafeCreate<TSkCanvas>(sk4d_surface_get_canvas(Handle), False);
  Result := FCanvas;
end;

function TSkSurface.GetProperties: TSkSurfaceProperties;
var
  LResult: sk_surfaceprops_t;
begin
  sk4d_surface_get_props(Handle, LResult);
  Result.Flags         := TSkSurfacePropertiesFlags(Byte(LResult.flags));
  Result.PixelGeometry := TSkPixelGeometry(LResult.pixel_geometry);
end;

class function TSkSurface.MakeFromMTKView(const AContext: IGrDirectContext;
  const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  const AProperties: TSkSurfaceProperties;
  AColorSpace: ISkColorSpace): ISkSurface;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_mtk_view(AContext.Handle, AView, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), @LProperties));
end;

class function TSkSurface.MakeFromMTKView(const AContext: IGrDirectContext;
  const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_mtk_view(AContext.Handle, AView, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), nil));
end;

class function TSkSurface.MakeFromRenderTarget(const AContext: IGrDirectContext;
  const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin;
  const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties;
  AColorSpace: ISkColorSpace): ISkSurface;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ARenderTarget) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARenderTarget']);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_render_target(AContext.Handle, ARenderTarget.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), @LProperties));
end;

class function TSkSurface.MakeFromRenderTarget(const AContext: IGrDirectContext;
  const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin;
  const AColorType: TSkColorType; AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ARenderTarget) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARenderTarget']);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_render_target(AContext.Handle, ARenderTarget.Handle, gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), nil));
end;

class function TSkSurface.MakeFromTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  const AProperties: TSkSurfaceProperties;
  AColorSpace: ISkColorSpace): ISkSurface;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), @LProperties));
end;

class function TSkSurface.MakeFromTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_from_texture(AContext.Handle, ATexture.Handle, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeHandle(AColorSpace), nil));
end;

function TSkSurface.MakeImageSnapshot(const ABounds: TRect): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_surface_make_image_snapshot2(Handle, @ABounds));
end;

function TSkSurface.MakeImageSnapshot: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(sk4d_surface_make_image_snapshot(Handle));
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const ARowBytes: NativeUInt): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster(@LImageInfo, ARowBytes, nil));
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const ARowBytes: NativeUInt;
  const AProperties: TSkSurfaceProperties): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
  LProperties: sk_surfaceprops_t;
begin
  MapImageInfo(AImageInfo, LImageInfo);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster(@LImageInfo, ARowBytes, @LProperties));
end;

class function TSkSurface.MakeRaster(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  AColorSpace: ISkColorSpace): ISkSurface;
begin
  Result := MakeRaster(TSkImageInfo.Create(AWidth, AHeight, AColorType, AAlphaType, AColorSpace));
end;

class function TSkSurface.MakeRaster(const AWidth, AHeight: Integer;
  const AProperties: TSkSurfaceProperties; const AColorType: TSkColorType;
  const AAlphaType: TSkAlphaType; AColorSpace: ISkColorSpace): ISkSurface;
begin
  Result := MakeRaster(TSkImageInfo.Create(AWidth, AHeight, AColorType, AAlphaType, AColorSpace), AProperties);
end;

class function TSkSurface.MakeRaster(
  const AImageInfo: TSkImageInfo): ISkSurface;
begin
  Result := MakeRaster(AImageInfo, AImageInfo.MinRowBytes);
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const AProperties: TSkSurfaceProperties): ISkSurface;
begin
  Result := MakeRaster(AImageInfo, AImageInfo.MinRowBytes, AProperties);
end;

class function TSkSurface.MakeRasterDirect(const APixmap: ISkPixmap;
  const AProperties: TSkSurfaceProperties;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc): ISkSurface;
var
  LProcWrapper: PSkSurfaceRasterReleaseProcWrapper;
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  MapSurfaceProperties(AProperties, LProperties);
  if Assigned(ARasterReleaseProc) then
  begin
    New(LProcWrapper);
    LProcWrapper.Proc := ARasterReleaseProc;
    Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster_direct(APixmap.Handle, raster_release_proc, LProcWrapper, @LProperties));
  end
  else
    Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster_direct(APixmap.Handle, nil, nil, @LProperties));
end;

class function TSkSurface.MakeRasterDirect(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc): ISkSurface;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeRasterDirect(LPixmap, ARasterReleaseProc);
end;

class function TSkSurface.MakeRasterDirect(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt;
  const AProperties: TSkSurfaceProperties;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc): ISkSurface;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(AImageInfo, APixels, ARowBytes);
  Result  := MakeRasterDirect(LPixmap, AProperties, ARasterReleaseProc);
end;

class function TSkSurface.MakeRasterDirect(const APixmap: ISkPixmap;
  const ARasterReleaseProc: TSkSurfaceRasterReleaseProc): ISkSurface;
var
  LProcWrapper: PSkSurfaceRasterReleaseProcWrapper;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  if Assigned(ARasterReleaseProc) then
  begin
    New(LProcWrapper);
    LProcWrapper.Proc := ARasterReleaseProc;
    Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster_direct(APixmap.Handle, raster_release_proc, LProcWrapper, nil));
  end
  else
    Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_raster_direct(APixmap.Handle, nil, nil, nil));
end;

class function TSkSurface.MakeRenderTarget(const AContext: IGrDirectContext;
  const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo;
  const ASampleCount: Integer; const AOrigin: TGrSurfaceOrigin;
  const AShouldCreateWithMips: Boolean): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  MapImageInfo(AImageInfo, LImageInfo);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_render_target(AContext.Handle, AIsBudgeted, @LImageInfo, ASampleCount, gr_surfaceorigin_t(AOrigin), nil, AShouldCreateWithMips));
end;

class function TSkSurface.MakeRenderTarget(const AContext: IGrDirectContext;
  const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo;
  const AProperties: TSkSurfaceProperties; const ASampleCount: Integer;
  const AOrigin: TGrSurfaceOrigin;
  const AShouldCreateWithMips: Boolean): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  MapImageInfo(AImageInfo, LImageInfo);
  MapSurfaceProperties(AProperties, LProperties);
  Result := TSkBindings.SafeCreate<TSkSurface>(sk4d_surface_make_render_target(AContext.Handle, AIsBudgeted, @LImageInfo, ASampleCount, gr_surfaceorigin_t(AOrigin), @LProperties, AShouldCreateWithMips));
end;

function TSkSurface.PeekPixels: ISkPixmap;
begin
  Result := TSkBindings.SafeCreate<TSkPixmap>(sk4d_surface_peek_pixels(Handle));
end;

class procedure TSkSurface.raster_release_proc(pixels, context: Pointer);
begin
  PSkSurfaceRasterReleaseProcWrapper(context)^.Proc(pixels);
  Dispose(PSkSurfaceRasterReleaseProcWrapper(context));
end;

function TSkSurface.ReadPixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX,
  ASrcY: Integer): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ReadPixels(LPixmap, ASrcX, ASrcY);
end;

function TSkSurface.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := sk4d_surface_read_pixels(Handle, ADest.Handle, ASrcX, ASrcY);
end;

procedure TSkSurface.Wait(const ASemaphores: TArray<IGrBackendSemaphore>);
var
  I: Integer;
  LSemaphores: TArray<gr_backendsemaphore_t>;
begin
  SetLength(LSemaphores{%H-}, Length(ASemaphores));
  for I := 0 to Length(ASemaphores) - 1 do
  begin
    if not Assigned(ASemaphores[I]) then
      raise ESkException.Create(SInvalidOperation);
    LSemaphores[I] := ASemaphores[I].Handle;
  end;
  sk4d_surface_wait(Handle, @LSemaphores[0], Length(ASemaphores));
end;

procedure TSkSurface.WritePixels(const ASrcImageInfo: TSkImageInfo;
  const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const ADestX,
  ADestY: Integer);
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  WritePixels(LPixmap, ADestX, ADestY);
end;

procedure TSkSurface.WritePixels(const ASrc: ISkPixmap; const ADestX,
  ADestY: Integer);
begin
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  sk4d_surface_write_pixels(Handle, ASrc.Handle, ADestX, ADestY);
end;

{ TSkTextBlob }

function TSkTextBlob.GetIntercepts(const AUpperBounds, ALowerBounds: Single;
  const APaint: ISkPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
begin
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  SetLength(Result{%H-}, sk4d_textblob_get_intercepts(Handle, @LBounds[0], nil, TSkBindings.SafeHandle(APaint)));
  sk4d_textblob_get_intercepts(Handle, @LBounds[0], @Result[0], TSkBindings.SafeHandle(APaint));
end;

class function TSkTextBlob.MakeFromText(const AText: string;
  const AFont: ISkFont): ISkTextBlob;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_textblob_make_from_text(@AText[Low(AText)], Length(AText) * 2, AFont.Handle, sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextHorizontallyPositioned(
  const AText: string; const AXPositions: TArray<Single>; const AY: Single;
  const AFont: ISkFont): ISkTextBlob;
begin
  if Length(AXPositions) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AXPositions']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_textblob_make_from_text_horizontally_positioned(@AText[Low(AText)], Length(AText) * 2, @AXPositions[0], AY, AFont.Handle, sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextPositioned(const AText: string;
  const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob;
begin
  if Length(APositions) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_textblob_make_from_text_positioned(@AText[Low(AText)], Length(AText) * 2, @APositions[0], AFont.Handle, sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextTransform(const AText: string;
  const AMatrices: TArray<TSkRotationScaleMatrix>;
  const AFont: ISkFont): ISkTextBlob;
begin
  if Length(AMatrices) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AMatrices']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_textblob_make_from_text_transform(@AText[Low(AText)], Length(AText) * 2, @AMatrices[0], AFont.Handle, sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class procedure TSkTextBlob.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_textblob_ref(AHandle);
end;

class procedure TSkTextBlob.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_textblob_unref(AHandle);
end;

{ TSkTraceMemoryDumpBaseClass }

constructor TSkTraceMemoryDumpBaseClass.Create(const ADetailedDump,
  ADumpWrappedObjects: Boolean);
begin
  inherited Create(sk4d_tracememorydumpbaseclass_create(ADetailedDump, ADumpWrappedObjects, Self));
end;

class constructor TSkTraceMemoryDumpBaseClass.Create;
var
  LProcs: sk_tracememorydumpbaseclass_procs_t;
begin
  LProcs.dump_numeric_value := dump_numeric_value_proc;
  LProcs.dump_string_value  := dump_string_value_proc;
  sk4d_tracememorydumpbaseclass_set_procs(@LProcs);
end;

class procedure TSkTraceMemoryDumpBaseClass.dump_numeric_value_proc(
  context: Pointer; const dump_name, value_name, units: MarshaledAString;
  value: uint64_t);
begin
  TSkTraceMemoryDumpBaseClass(context).DumpNumericValue(string(dump_name), string(value_name), string(units), value);
end;

class procedure TSkTraceMemoryDumpBaseClass.dump_string_value_proc(
  context: Pointer; const dump_name, value_name, value: MarshaledAString);
begin
  TSkTraceMemoryDumpBaseClass(context).DumpStringValue(string(dump_name), string(value_name), string(value));
end;

class procedure TSkTraceMemoryDumpBaseClass.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_tracememorydumpbaseclass_destroy(AHandle);
end;

{ TSkTypeface }

function TSkTypeface.GetFamilyName: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(sk4d_typeface_get_family_name(Handle));
  Result  := LResult.Text;
end;

function TSkTypeface.GetSlant: TSkFontSlant;
begin
  Result := TSkFontSlant(sk4d_typeface_get_slant(Handle));
end;

function TSkTypeface.GetStyle: TSkFontStyle;
begin
  sk4d_typeface_get_style(Handle, sk_fontstyle_t(Result));
end;

function TSkTypeface.GetWeight: Integer;
begin
  Result := sk4d_typeface_get_weight(Handle);
end;

function TSkTypeface.GetWidth: Integer;
begin
  Result := sk4d_typeface_get_width(Handle);
end;

function TSkTypeface.IsBold: Boolean;
begin
  Result := GetWeight >= Ord(TSkFontWeight.SemiBold);
end;

function TSkTypeface.IsItalic: Boolean;
begin
  Result := GetSlant <> TSkFontSlant.Upright;
end;

class function TSkTypeface.MakeDefault: ISkTypeface;
begin
  Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_typeface_make_default());
end;

class function TSkTypeface.MakeFromFile(const AFileName: string;
  const ATTCIndex: Integer): ISkTypeface;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_typeface_make_from_file(MarshaledAString(UTF8String(AFileName)), ATTCIndex));
end;

class function TSkTypeface.MakeFromName(const AFamilyName: string;
  const AStyle: TSkFontStyle): ISkTypeface;
begin
  if Length(AFamilyName) = 0 then
    Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_typeface_make_from_name(nil, @AStyle))
  else
    Result := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_typeface_make_from_name(MarshaledAString(UTF8String(AFamilyName)), @AStyle));
end;

class function TSkTypeface.MakeFromStream(const AStream: TStream;
  const ATTCIndex: Integer): ISkTypeface;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkTypeFace>(sk4d_typeface_make_from_stream(LStream.Handle, ATTCIndex));
end;

{ TSkVertices }

class function TSkVertices.MakeCopy(const AVertexMode: TSkVertexMode;
  const APositions, ATextures: TArray<TPointF>;
  const AColors: TArray<TAlphaColor>;
  const AIndices: TArray<Word>): ISkVertices;
begin
  if Length(APositions) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['APositions']);
  if (Length(ATextures) > 0) and (Length(ATextures) <> Length(APositions)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ATextures']);
  if (Length(AColors) > 0) and (Length(AColors) <> Length(APositions)) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
  Result := TSkBindings.SafeCreate<TSkVertices>(sk4d_vertices_make_copy(sk_vertexmode_t(AVerTexMode), Length(APositions), @APositions[0], @ATextures[0], @AColors[0], Length(AIndices), @AIndices[0]));
end;

class procedure TSkVertices.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_vertices_ref(AHandle);
end;

class procedure TSkVertices.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_vertices_unref(AHandle);
end;

{ TSkParticleEffect }

class constructor TSkParticleEffect.Create;
begin
  sk4d_particleeffect_init();
end;

function TSkParticleEffect.GetPosition: TPointF;
begin
  sk4d_particleeffect_get_position(Handle, sk_point_t(Result));
end;

function TSkParticleEffect.GetRate: Single;
begin
  Result := sk4d_particleeffect_get_rate(Handle);
end;

function TSkParticleEffect.GetUniform(
  const AIndex: NativeUInt): TSkParticleUniform;
begin
  sk4d_particleeffect_get_uniform(Handle, AIndex, sk_particleuniform_t(Result));
end;

function TSkParticleEffect.GetUniformCount: NativeUInt;
begin
  Result := sk4d_particleeffect_get_uniform_count(Handle);
end;

function TSkParticleEffect.GetUniformData: PSingleArray;
begin
  Result := PSingleArray(sk4d_particleeffect_get_uniform_data(Handle));
end;

function TSkParticleEffect.GetUniformDataCount: Integer;
begin
  Result := sk4d_particleeffect_get_uniform_data_count(Handle);
end;

function TSkParticleEffect.GetUniformName(const AIndex: NativeUInt): string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(sk4d_particleeffect_get_uniform_name(Handle, AIndex));
  Result  := LResult.Text;
end;

class function TSkParticleEffect.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider): ISkParticleEffect;
var
  LStream: ISkStream;
  LStringStream: TStream;
begin
  LStringStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    LStream := TSkStreamAdapter.Create(LStringStream);
    Result  := TSkBindings.SafeCreate<TSkParticleEffect>(sk4d_particleeffect_make_from_stream(LStream.Handle, TSkBindings.SafeHandle(AResourceProvider)));
  finally
    LStringStream.Free;
  end;
end;

class function TSkParticleEffect.MakeFromFile(
  const AFileName: string): ISkParticleEffect;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkParticleEffect>(sk4d_particleeffect_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkParticleEffect.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider): ISkParticleEffect;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkParticleEffect>(sk4d_particleeffect_make_from_stream(LStream.Handle, TSkBindings.SafeHandle(AResourceProvider)));
end;

procedure TSkParticleEffect.Render(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_particleeffect_render(Handle, ACanvas.Handle);
end;

procedure TSkParticleEffect.SetPosition(const AValue: TPointF);
begin
  sk4d_particleeffect_set_position(Handle, @AValue);
end;

procedure TSkParticleEffect.SetRate(const AValue: Single);
begin
  sk4d_particleeffect_set_rate(Handle, AValue);
end;

function TSkParticleEffect.SetUniform(const AName: string;
  const AData: TArray<Single>): Boolean;
begin
  if Length(AData) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AData']);
  Result := sk4d_particleeffect_set_uniform(Handle, MarshaledAString(UTF8String(AName)), @AData[0], Length(AData));
end;

procedure TSkParticleEffect.Start(const ANow: Double; const ALooping: Boolean);
begin
  sk4d_particleeffect_start(Handle, ANow, ALooping);
end;

procedure TSkParticleEffect.Update(const ANow: Double);
begin
  sk4d_particleeffect_update(Handle, ANow);
end;

{ TSkottieAnimation }

function TSkottieAnimation.GetDuration: Double;
begin
  Result := sk4d_skottieanimation_get_duration(Handle);
end;

function TSkottieAnimation.GetFPS: Double;
begin
  Result := sk4d_skottieanimation_get_fps(Handle);
end;

function TSkottieAnimation.GetInPoint: Double;
begin
  Result := sk4d_skottieanimation_get_in_point(Handle);
end;

function TSkottieAnimation.GetOutPoint: Double;
begin
  Result := sk4d_skottieanimation_get_out_point(Handle);
end;

function TSkottieAnimation.GetSize: TSizeF;
begin
  sk4d_skottieanimation_get_size(Handle, sk_size_t(Result));
end;

function TSkottieAnimation.GetVersion: string;
begin
  Result := string(sk4d_skottieanimation_get_version(Handle));
end;

class function TSkottieAnimation.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider;
  const AFontProvider: ISkTypefaceFontProvider): ISkottieAnimation;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    Result := MakeFromStream(LStream, AResourceProvider, AFontProvider);
  finally
    LStream.Free;
  end;
end;

class function TSkottieAnimation.MakeFromFile(const AFileName: string;
  const AFontProvider: ISkTypefaceFontProvider): ISkottieAnimation;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkottieAnimation>(sk4d_skottieanimation_make_from_file(MarshaledAString(UTF8String(AFileName)), TSkBindings.SafeHandle(AFontProvider)));
end;

class function TSkottieAnimation.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider;
  const AFontProvider: ISkTypefaceFontProvider): ISkottieAnimation;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkottieAnimation>(sk4d_skottieanimation_make_from_stream(LStream.Handle, TSkBindings.SafeHandle(AResourceProvider), TSkBindings.SafeHandle(AFontProvider)));
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_skottieanimation_render(Handle, ACanvas.Handle, nil, Byte(ARenderFlags));
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF; const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_skottieanimation_render(Handle, ACanvas.Handle, @ADest, Byte(ARenderFlags));
end;

procedure TSkottieAnimation.SeekFrame(const ATick: Double);
begin
  sk4d_skottieanimation_seek_frame(Handle, ATick);
end;

procedure TSkottieAnimation.SeekFrameTime(const ATick: Double);
begin
  sk4d_skottieanimation_seek_frame_time(Handle, ATick);
end;

class procedure TSkottieAnimation.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_skottieanimation_ref(AHandle);
end;

class procedure TSkottieAnimation.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_skottieanimation_unref(AHandle);
end;

{ TSkParagraph }

function TSkParagraph.DidExceedMaxLines: Boolean;
begin
  Result := sk4d_paragraph_did_exceed_max_lines(Handle);
end;

function TSkParagraph.GetAlphabeticBaseline: Single;
begin
  Result := sk4d_paragraph_get_alphabetic_baseline(Handle);
end;

function TSkParagraph.GetGlyphPositionAtCoordinate(const ADeltaX,
  ADeltaY: Single): TSkPositionAffinity;
begin
  sk4d_paragraph_get_glyph_position_at_coordinate(Handle, ADeltaX, ADeltaY, sk_positionaffinity_t(Result));
end;

function TSkParagraph.GetHeight: Single;
begin
  Result := sk4d_paragraph_get_height(Handle);
end;

function TSkParagraph.GetIdeographicBaseline: Single;
begin
  Result := sk4d_paragraph_get_ideographic_baseline(Handle);
end;

function TSkParagraph.GetLineMetrics: TArray<TSkMetrics>;
begin
  SetLength(Result{%H-}, sk4d_paragraph_get_line_metrics(Handle, nil));
  sk4d_paragraph_get_line_metrics(Handle, @Result[0]);
end;

function TSkParagraph.GetLongestLine: Single;
begin
  Result := sk4d_paragraph_get_longest_line(Handle);
end;

function TSkParagraph.GetMaxIntrinsicWidth: Single;
begin
  Result := sk4d_paragraph_get_max_intrinsic_width(Handle);
end;

function TSkParagraph.GetMaxWidth: Single;
begin
  Result := sk4d_paragraph_get_max_width(Handle);
end;

function TSkParagraph.GetMinIntrinsicWidth: Single;
begin
  Result := sk4d_paragraph_get_min_intrinsic_width(Handle);
end;

function TSkParagraph.GetRectsForPlaceholders: TArray<TSkTextBox>;
begin
  SetLength(Result{%H-}, sk4d_paragraph_get_rects_for_placeholders(Handle, nil));
  sk4d_paragraph_get_rects_for_placeholders(Handle, @Result[0]);
end;

function TSkParagraph.GetRectsForRange(const AStart, AEnd: Cardinal;
  const ARectHeightStyle: TSkRectHeightStyle;
  const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
begin
  SetLength(Result{%H-}, sk4d_paragraph_get_rects_for_range(Handle, AStart, AEnd, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), nil));
  sk4d_paragraph_get_rects_for_range(Handle, AStart, AEnd, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), @Result[0]);
end;

procedure TSkParagraph.GetWordBoundary(const AOffset: Cardinal; out AStart,
  AEnd: Cardinal);
begin
  sk4d_paragraph_get_word_boundary(Handle, AOffset, AStart, AEnd);
end;

procedure TSkParagraph.Layout(const AWidth: Single);
begin
  sk4d_paragraph_layout(Handle, AWidth);
end;

procedure TSkParagraph.Paint(const ACanvas: ISkCanvas; const AX, AY: Single);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_paragraph_paint(Handle, ACanvas.Handle, AX, AY);
end;

function TSkParagraph.ToPath: ISkPath;
begin
  Result := TSkPath.Wrap(sk4d_paragraph_to_path(Handle));
end;

procedure TSkParagraph.Visit(const AProc: TSkParagraphVisitProc);
begin
  sk4d_paragraph_visit(Handle, visit_proc, @AProc);
end;

class procedure TSkParagraph.visit_proc(line_number: int32_t;
  const info: psk_paragraphvisitorinfo_t; context: Pointer);
var
  LInfo: TSkParagraphVisitorInfo;
begin
  if info <> nil then
  begin
    LInfo.Font       := TSkFont.Wrap(info.font, False);
    LInfo.Origin     := TPointF(info.origin);
    LInfo.AdvanceX   := info.advance_x;
    LInfo.Count      := info.count;
    LInfo.Glyphs     := PWordArray(info.glyphs);
    LInfo.Positions  := PPointFArray(info.positions);
    LInfo.Utf8Starts := PCardinalArray(info.utf8_starts);
    LInfo.Flags      := TSkParagraphVisitorFlags(Byte(info.flags));
    TSkParagraphVisitProc(context^)(line_number, LInfo);
  end;
end;

class procedure TSkParagraph.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_paragraph_destroy(AHandle);
end;

{ TSkParagraphBuilder }

procedure TSkParagraphBuilder.AddPlaceholder(
  const APlaceholder: TSkPlaceholderStyle);
begin
  sk4d_paragraphbuilder_add_placeholder(Handle, @APlaceholder);
end;

procedure TSkParagraphBuilder.AddText(const AText: string);
begin
  sk4d_paragraphbuilder_add_text(Handle, MarshaledAString(UTF8String(AText)));
end;

function TSkParagraphBuilder.Build: ISkParagraph;
begin
  Result := TSkParagraph.Wrap(sk4d_paragraphbuilder_build(Handle));
end;

constructor TSkParagraphBuilder.Create(const AParagraphStyle: ISkParagraphStyle;
  const AFontProvider: ISkTypefaceFontProvider;
  const AEnableFontFallback: Boolean);
begin
  if not Assigned(AParagraphStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AParagraphStyle']);
  if not Assigned(AFontProvider) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFontProvider']);
  inherited Create(sk4d_paragraphbuilder_create2(AParagraphStyle.Handle, AFontProvider.Handle, AEnableFontFallback));
end;

constructor TSkParagraphBuilder.Create(
  const AParagraphStyle: ISkParagraphStyle);
begin
  if not Assigned(AParagraphStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AParagraphStyle']);
  inherited Create(sk4d_paragraphbuilder_create(AParagraphStyle.Handle));
end;

procedure TSkParagraphBuilder.Pop;
begin
  sk4d_paragraphbuilder_pop(Handle);
end;

procedure TSkParagraphBuilder.PushStyle(const ATextStyle: ISkTextStyle);
begin
  if not Assigned(ATextStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextStyle']);
  sk4d_paragraphbuilder_push_style(Handle, ATextStyle.Handle);
end;

class procedure TSkParagraphBuilder.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_paragraphbuilder_destroy(AHandle);
end;

{ TSkStrutStyle }

constructor TSkStrutStyle.Create;
begin
  inherited Create(sk4d_strutstyle_create());
end;

function TSkStrutStyle.GetEnabled: Boolean;
begin
  Result := sk4d_strutstyle_get_enabled(Handle);
end;

function TSkStrutStyle.GetFontFamilies: TArray<string>;
var
  I: Integer;
  LFontFamilies: TArray<MarshaledAString>;
begin
  SetLength(LFontFamilies{%H-}, sk4d_strutstyle_get_font_families(Handle, nil));
  sk4d_strutstyle_get_font_families(Handle, @LFontFamilies[0]);
  SetLength(Result{%H-}, Length(LFontFamilies));
  for I := 0 to Length(LFontFamilies) - 1 do
    Result[I] := string(LFontFamilies[I]);
end;

function TSkStrutStyle.GetFontSize: Single;
begin
  Result := sk4d_strutstyle_get_font_size(Handle);
end;

function TSkStrutStyle.GetFontStyle: TSkFontStyle;
begin
  sk4d_strutstyle_get_font_style(Handle, sk_fontstyle_t(Result));
end;

function TSkStrutStyle.GetForceHeight: Boolean;
begin
  Result := sk4d_strutstyle_get_force_height(Handle);
end;

function TSkStrutStyle.GetHalfLeading: Boolean;
begin
  Result := sk4d_strutstyle_get_half_leading(Handle);
end;

function TSkStrutStyle.GetHeightMultiplier: Single;
begin
  Result := sk4d_strutstyle_get_height_multiplier(Handle);
end;

function TSkStrutStyle.GetLeading: Single;
begin
  Result := sk4d_strutstyle_get_leading(Handle);
end;

function TSkStrutStyle.IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
begin
  if not Assigned(AStrutStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AStrutStyle']);
  Result := sk4d_strutstyle_is_equal(Handle, AStrutStyle.Handle);
end;

procedure TSkStrutStyle.SetEnabled(const AValue: Boolean);
begin
  sk4d_strutstyle_set_enabled(Handle, AValue);
end;

procedure TSkStrutStyle.SetFontFamilies(const AValue: TArray<string>);
var
  I: Integer;
  LFontFamilies: TArray<MarshaledAString>;
  LFontFamiliesStr: TArray<UTF8String>;
begin
  SetLength(LFontFamilies{%H-}, Length(AValue));
  SetLength(LFontFamiliesStr{%H-}, Length(AValue));
  for I := 0 to Length(AValue) - 1 do
  begin
    LFontFamiliesStr[I] := UTF8String(AValue[I]);
    LFontFamilies[I]    := MarshaledAString(LFontFamiliesStr[I]);
  end;
  sk4d_strutstyle_set_font_families(Handle, @LFontFamilies[0], Length(AValue));
end;

procedure TSkStrutStyle.SetFontSize(const AValue: Single);
begin
  sk4d_strutstyle_set_font_size(Handle, AValue);
end;

procedure TSkStrutStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  sk4d_strutstyle_set_font_style(Handle, @AValue);
end;

procedure TSkStrutStyle.SetForceHeight(const AValue: Boolean);
begin
  sk4d_strutstyle_set_force_height(Handle, AValue);
end;

procedure TSkStrutStyle.SetHalfLeading(const AValue: Boolean);
begin
  sk4d_strutstyle_set_half_leading(Handle, AValue);
end;

procedure TSkStrutStyle.SetHeightMultiplier(const AValue: Single);
begin
  sk4d_strutstyle_set_height_multiplier(Handle, AValue);
end;

procedure TSkStrutStyle.SetLeading(const AValue: Single);
begin
  sk4d_strutstyle_set_leading(Handle, AValue);
end;

class procedure TSkStrutStyle.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_strutstyle_destroy(AHandle);
end;

{ TSkParagraphStyle }

constructor TSkParagraphStyle.Create;
begin
  inherited Create(sk4d_paragraphstyle_create());
end;

procedure TSkParagraphStyle.DisableHinting;
begin
  sk4d_paragraphstyle_disable_hinting(Handle);
end;

function TSkParagraphStyle.GetEllipsis: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(sk4d_paragraphstyle_get_ellipsis(Handle));
  Result  := LResult.Text;
end;

function TSkParagraphStyle.GetHeight: Single;
begin
  Result := sk4d_paragraphstyle_get_height(Handle);
end;

function TSkParagraphStyle.GetMaxLines: NativeUInt;
begin
  Result := sk4d_paragraphstyle_get_max_lines(Handle);
end;

function TSkParagraphStyle.GetStrutStyle: ISkStrutStyle;
begin
  Result := TSkStrutStyle.Wrap(sk4d_paragraphstyle_get_strut_style(Handle), False);
end;

function TSkParagraphStyle.GetTextAlign: TSkTextAlign;
begin
  Result := TSkTextAlign(sk4d_paragraphstyle_get_text_align(Handle));
end;

function TSkParagraphStyle.GetTextDirection: TSkTextDirection;
begin
  Result := TSkTextDirection(sk4d_paragraphstyle_get_text_direction(Handle));
end;

function TSkParagraphStyle.GetTextHeightBehaviors: TSkTextHeightBehaviors;
begin
  Result := TSkTextHeightBehaviors(Byte(sk4d_paragraphstyle_get_text_height_behaviors(Handle)));
end;

function TSkParagraphStyle.GetTextStyle: ISkTextStyle;
begin
  Result := TSkTextStyle.Wrap(sk4d_paragraphstyle_get_text_style(Handle), False);
end;

procedure TSkParagraphStyle.SetEllipsis(const AValue: string);
begin
  if Length(AValue) = 0 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AValue']);
  sk4d_paragraphstyle_set_ellipsis(Handle, MarshaledAString(UTF8String(AValue)));
end;

procedure TSkParagraphStyle.SetHeight(const AValue: Single);
begin
  sk4d_paragraphstyle_set_height(Handle, AValue);
end;

procedure TSkParagraphStyle.SetMaxLines(const AValue: NativeUInt);
begin
  sk4d_paragraphstyle_set_max_lines(Handle, AValue);
end;

procedure TSkParagraphStyle.SetStrutStyle(AValue: ISkStrutStyle);
begin
  sk4d_paragraphstyle_set_strut_style(Handle, TSkBindings.SafeHandle(AValue));
end;

procedure TSkParagraphStyle.SetTextAlign(const AValue: TSkTextAlign);
begin
  sk4d_paragraphstyle_set_text_align(Handle, sk_textalign_t(AValue));
end;

procedure TSkParagraphStyle.SetTextDirection(const AValue: TSkTextDirection);
begin
  sk4d_paragraphstyle_set_text_direction(Handle, sk_textdirection_t(AValue));
end;

procedure TSkParagraphStyle.SetTextHeightBehaviors(
  const AValue: TSkTextHeightBehaviors);
begin
  sk4d_paragraphstyle_set_text_height_behaviors(Handle, Byte(AValue));
end;

procedure TSkParagraphStyle.SetTextStyle(AValue: ISkTextStyle);
begin
  sk4d_paragraphstyle_set_text_style(Handle, TSkBindings.SafeHandle(AValue));
end;

class procedure TSkParagraphStyle.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_paragraphstyle_destroy(AHandle);
end;

{ TSkTextStyle }

procedure TSkTextStyle.AddFontFeature(const AFeature: string;
  const AValue: Integer);
begin
  if Length(AFeature) = 0 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AFeature']);
  sk4d_textstyle_add_font_feature(Handle, MarshaledAString(UTF8String(AFeature)), AValue);
end;

procedure TSkTextStyle.AddShadow(const AShadow: TSkTextShadow);
begin
  sk4d_textstyle_add_shadow(Handle, @AShadow);
end;

procedure TSkTextStyle.ClearBackgroundColor;
begin
  sk4d_textstyle_clear_background_color(Handle);
end;

procedure TSkTextStyle.ClearForegroundColor;
begin
  sk4d_textstyle_clear_foreground_color(Handle);
end;


constructor TSkTextStyle.Create;
begin
  inherited Create(sk4d_textstyle_create());
end;

function TSkTextStyle.GetBackground: ISkPaint;
begin
  Result := TSkPaint.Wrap(sk4d_textstyle_get_background(Handle));
end;

function TSkTextStyle.GetColor: TAlphaColor;
begin
  Result := sk4d_textstyle_get_color(Handle);
end;

function TSkTextStyle.GetDecorationColor: TAlphaColor;
begin
  Result := sk4d_textstyle_get_decoration_color(Handle);
end;

function TSkTextStyle.GetDecorations: TSkTextDecorations;
begin
  Result := TSkTextDecorations(Byte(sk4d_textstyle_get_decorations(Handle)));
end;

function TSkTextStyle.GetDecorationStyle: TSkTextDecorationStyle;
begin
  Result := TSkTextDecorationStyle(sk4d_textstyle_get_decoration_style(Handle));
end;

function TSkTextStyle.GetDecorationThickness: Single;
begin
  Result := sk4d_textstyle_get_decoration_thickness(Handle);
end;

function TSkTextStyle.GetFontFamilies: TArray<string>;
var
  I: Integer;
  LFontFamilies: TArray<MarshaledAString>;
begin
  SetLength(LFontFamilies{%H-}, sk4d_textstyle_get_font_families(Handle, nil));
  sk4d_textstyle_get_font_families(Handle, @LFontFamilies[0]);
  SetLength(Result{%H-}, Length(LFontFamilies));
  for I := 0 to Length(LFontFamilies) - 1 do
    Result[I] := string(LFontFamilies[I]);
end;

function TSkTextStyle.GetFontMetrics: TSkFontMetrics;
var
  LResult: sk_fontmetrics_t;
begin
  sk4d_textstyle_get_font_metrics(Handle, LResult);
  Result.Flags              := TSkFontMetricsFlags(Byte(LResult.flags));
  Result.Top                := LResult.top;
  Result.Ascent             := LResult.ascent;
  Result.Descent            := LResult.descent;
  Result.Bottom             := LResult.bottom;
  Result.Leading            := LResult.leading;
  Result.AvgCharWidth       := LResult.avg_char_width;
  Result.MaxCharWidth       := LResult.max_char_width;
  Result.XMin               := LResult.x_min;
  Result.XMax               := LResult.x_max;
  Result.XHeight            := LResult.x_height;
  Result.CapHeight          := LResult.cap_height;
  Result.UnderlineThickness := LResult.underline_thickness;
  Result.UnderlinePosition  := LResult.underline_position;
  Result.StrikeoutThickness := LResult.strikeout_thickness;
  Result.StrikeoutPosition  := LResult.strikeout_position;
end;

function TSkTextStyle.GetFontSize: Single;
begin
  Result := sk4d_textstyle_get_font_size(Handle);
end;

function TSkTextStyle.GetFontStyle: TSkFontStyle;
begin
  sk4d_textstyle_get_font_style(Handle, sk_fontstyle_t(Result));
end;

function TSkTextStyle.GetForeground: ISkPaint;
begin
  Result := TSkPaint.Wrap(sk4d_textstyle_get_foreground(Handle));
end;

function TSkTextStyle.GetHalfLeading: Boolean;
begin
  Result := sk4d_textstyle_get_half_leading(Handle);
end;

function TSkTextStyle.GetHeightMultiplier: Single;
begin
  Result := sk4d_textstyle_get_height_multiplier(Handle);
end;

function TSkTextStyle.GetLetterSpacing: Single;
begin
  Result := sk4d_textstyle_get_letter_spacing(Handle);
end;

function TSkTextStyle.GetLocale: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(sk4d_textstyle_get_locale(Handle));
  Result  := LResult.Text;
end;

function TSkTextStyle.GetWordSpacing: Single;
begin
  Result := sk4d_textstyle_get_word_spacing(Handle);
end;

function TSkTextStyle.IsEqual(const ATextStyle: ISkTextStyle): Boolean;
begin
  if not Assigned(ATextStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextStyle']);
  Result := sk4d_textstyle_is_equal(Handle, ATextStyle.Handle);
end;

procedure TSkTextStyle.ResetFontFeatures;
begin
  sk4d_textstyle_reset_font_features(Handle);
end;

procedure TSkTextStyle.ResetShadows;
begin
  sk4d_textstyle_reset_shadows(Handle);
end;

procedure TSkTextStyle.SetBackgroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_textstyle_set_background_color(Handle, APaint.Handle);
end;

procedure TSkTextStyle.SetColor(const AValue: TAlphaColor);
begin
  sk4d_textstyle_set_color(Handle, AValue);
end;

procedure TSkTextStyle.SetDecorationColor(const AValue: TAlphaColor);
begin
  sk4d_textstyle_set_decoration_color(Handle, AValue);
end;

procedure TSkTextStyle.SetDecorations(const AValue: TSkTextDecorations);
begin
  sk4d_textstyle_set_decorations(Handle, Byte(AValue));
end;

procedure TSkTextStyle.SetDecorationStyle(const AValue: TSkTextDecorationStyle);
begin
  sk4d_textstyle_set_decoration_style(Handle, sk_textdecorationstyle_t(AValue));
end;

procedure TSkTextStyle.SetDecorationThickness(const AValue: Single);
begin
  sk4d_textstyle_set_decoration_thickness(Handle, AValue);
end;

procedure TSkTextStyle.SetFontFamilies(const AValue: TArray<string>);
var
  I: Integer;
  LFontFamilies: TArray<MarshaledAString>;
  LFontFamiliesStr: TArray<UTF8String>;
begin
  SetLength(LFontFamilies{%H-}, Length(AValue));
  SetLength(LFontFamiliesStr{%H-}, Length(AValue));
  for I := 0 to Length(AValue) - 1 do
  begin
    LFontFamiliesStr[I] := UTF8String(AValue[I]);
    LFontFamilies[I]    := MarshaledAString(LFontFamiliesStr[I]);
  end;
  sk4d_textstyle_set_font_families(Handle, @LFontFamilies[0], Length(AValue));
end;

procedure TSkTextStyle.SetFontSize(const AValue: Single);
begin
  sk4d_textstyle_set_font_size(Handle, AValue);
end;

procedure TSkTextStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  sk4d_textstyle_set_font_style(Handle, @AValue);
end;

procedure TSkTextStyle.SetForegroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  sk4d_textstyle_set_foreground_color(Handle, APaint.Handle);
end;

procedure TSkTextStyle.SetHalfLeading(const AValue: Boolean);
begin
  sk4d_textstyle_set_half_leading(Handle, AValue);
end;

procedure TSkTextStyle.SetHeightMultiplier(const AValue: Single);
begin
  sk4d_textstyle_set_height_multiplier(Handle, AValue);
end;

procedure TSkTextStyle.SetLetterSpacing(const AValue: Single);
begin
  sk4d_textstyle_set_letter_spacing(Handle, AValue);
end;

procedure TSkTextStyle.SetLocale(const AValue: string);
begin
  if Length(AValue) = 0 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AValue']);
  sk4d_textstyle_set_locale(Handle, MarshaledAString(UTF8String(AValue)));
end;

procedure TSkTextStyle.SetWordSpacing(const AValue: Single);
begin
  sk4d_textstyle_set_word_spacing(Handle, AValue);
end;

class procedure TSkTextStyle.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_textstyle_destroy(AHandle);
end;

{ TSkTypefaceFontProvider }

constructor TSkTypefaceFontProvider.Create;
begin
  inherited Create(sk4d_typefacefontprovider_create());
end;

procedure TSkTypefaceFontProvider.RegisterTypeface(
  const ATypeface: ISkTypeface);
begin
  if not Assigned(ATypeface) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATypeface']);
  sk4d_typefacefontprovider_register_typeface(Handle, ATypeface.Handle);
end;

procedure TSkTypefaceFontProvider.RegisterTypeface(const ATypeface: ISkTypeface;
  const AFamilyName: string);
begin
  if not Assigned(ATypeface) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATypeface']);
  if Length(AFamilyName) = 0 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AFamilyName']);
  sk4d_typefacefontprovider_register_typeface2(Handle, ATypeface.Handle, MarshaledAString(UTF8String(AFamilyName)));
end;

{ TSkResourceProviderBaseClass }

class constructor TSkResourceProviderBaseClass.Create;
var
  LProcs: sk_resourceproviderbaseclass_procs_t;
begin
  LProcs.load := load_proc;
  sk4d_resourceproviderbaseclass_set_procs(@LProcs);
end;

constructor TSkResourceProviderBaseClass.Create(const APredecode: Boolean);
begin
  inherited Create(sk4d_resourceproviderbaseclass_create(APredecode, Self));
end;

class function TSkResourceProviderBaseClass.load_proc(context: Pointer;
  const path, name: MarshaledAString): sk_data_t;
begin
  Result := TSkObject.__ReleaseHandle(TSkData.MakeFromBytes(TSkResourceProviderBaseClass(context).Load(string(path), string(name))));
end;

{ TSkFileResourceProvider }

constructor TSkFileResourceProvider.Create(const ABaseDir: string;
  const APredecode: Boolean);
begin
  inherited Create(APredecode);
  FBaseDir := ABaseDir;
end;

function TSkFileResourceProvider.Load(const APath, AName: string): TBytes;
var
  LFileName: string;
  LFileStream: TFileStream;
begin
  LFileName := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FBaseDir) + APath) + AName;
  if FileExists(LFileName) then
  begin
    LFileStream := TFileStream.Create(LFileName{%H-}, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result{%H-}, LFileStream.Size);
      LFileStream.Read(Result[0], Length(Result));
    finally
      LFileStream.Free;
    end;
  end
  else
    Result := nil;
end;

{ TSkShaper }

constructor TSkShaper.Create;
begin
  inherited Create(sk4d_shaper_create());
end;

function TSkShaper.Shape(const AText: string; const AFont: ISkFont;
  const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_shaper_shape(Handle, MarshaledAString(UTF8String(AText)), AFont.Handle, ALeftToRight, AWidth, nil, nil));
end;

function TSkShaper.Shape(const AText: string; const AFont: ISkFont;
  const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF;
  out AEndPoint: TPointF): ISkTextBlob;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(sk4d_shaper_shape(Handle, MarshaledAString(UTF8String(AText)), AFont.Handle, ALeftToRight, AWidth, @AOffset, @AEndPoint));
end;

class procedure TSkShaper.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_shaper_destroy(AHandle);
end;

{ TSkSVGCanvas }

class function TSkSVGCanvas.Make(const ABounds: TRectF; const AStream: TStream;
  const AFlags: TSkSVGCanvasFlags): ISkCanvas;
var
  LCanvas: TSkSVGCanvas;
  LHandle: sk_handle_t;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := sk4d_svgcanvas_make(@ABounds, LWStream.Handle, Byte(AFlags));
  if LHandle = 0 then
    Exit(nil);
  LCanvas := TSkSVGCanvas.Wrap(LHandle);
  LCanvas.FHolder := LWStream;
  Result := LCanvas;
end;

{ TSkSVGDOM }

function TSkSVGDOM.FindNodeById(const AId: string): ISkSVGNode;
begin
  Result := TSkBindings.SafeCreate<TSkSVGNode>(sk4d_svgdom_find_node_by_id(Handle, MarshaledAString(UTF8String(AId))), False);
end;

function TSkSVGDOM.GetRoot: ISkSVGSVG;
begin
  if not Assigned(FRoot) then
    FRoot := TSkBindings.SafeCreate<TSkSVGSVG>(sk4d_svgdom_get_root(Handle), False);
  Result := FRoot;
end;

class function TSkSVGDOM.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider;
  const AFontProvider: ISkTypefaceFontProvider): ISkSVGDOM;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    Result := MakeFromStream(LStream, AResourceProvider, AFontProvider);
  finally
    LStream.Free;
  end;
end;

class function TSkSVGDOM.MakeFromFile(const AFileName: string;
  const AFontProvider: ISkTypefaceFontProvider): ISkSVGDOM;
begin
  if Length(AFileName) = 0 then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkSVGDOM>(sk4d_svgdom_make_from_file(MarshaledAString(UTF8String(AFileName)), TSkBindings.SafeHandle(AFontProvider)));
end;

class function TSkSVGDOM.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider;
  const AFontProvider: ISkTypefaceFontProvider): ISkSVGDOM;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkSVGDOM>(sk4d_svgdom_make_from_stream(LStream.Handle, TSkBindings.SafeHandle(AResourceProvider), TSkBindings.SafeHandle(AFontProvider)));
end;

procedure TSkSVGDOM.Render(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  sk4d_svgdom_render(Handle, ACanvas.Handle);
end;

procedure TSkSVGDOM.SetContainerSize(const ASize: TSizeF);
begin
  sk4d_svgdom_set_container_size(Handle, @ASize);
end;

{ TSkSVGNode }

function TSkSVGNode.TrySetAttribute(const AName, AValue: string): Boolean;
begin
  Result := sk4d_svgnode_set_attribute(Handle, MarshaledAString(UTF8String(AName)), MarshaledAString(UTF8String(AValue)));
end;

{ TSkSVGSVG }

function TSkSVGSVG.GetHeight: TSkSVGLength;
begin
  sk4d_svgsvg_get_height(Handle, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetIntrinsicSize(const AViewPort: TSizeF;
  const ADPI: Single): TSizeF;
begin
  sk4d_svgsvg_get_intrinsic_size(Handle, @AViewPort, ADPI, sk_size_t(Result));
end;

function TSkSVGSVG.GetPreserveAspectRatio: TSkSVGPreserveAspectRatio;
begin
  sk4d_svgsvg_get_preserve_aspect_ratio(Handle, sk_svgpreserveaspectratio_t(Result));
end;

function TSkSVGSVG.GetWidth: TSkSVGLength;
begin
  sk4d_svgsvg_get_width(Handle, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetX: TSkSVGLength;
begin
  sk4d_svgsvg_get_x(Handle, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetY: TSkSVGLength;
begin
  sk4d_svgsvg_get_y(Handle, sk_svglength_t(Result));
end;

procedure TSkSVGSVG.SetHeight(const AValue: TSkSVGLength);
begin
  sk4d_svgsvg_set_height(Handle, @AValue);
end;

procedure TSkSVGSVG.SetPreserveAspectRatio(
  const AValue: TSkSVGPreserveAspectRatio);
begin
  sk4d_svgsvg_set_preserve_aspect_ratio(Handle, @AValue);
end;

procedure TSkSVGSVG.SetViewBox(const AViewBox: TRectF);
begin
  sk4d_svgsvg_set_view_box(Handle, @AViewBox);
end;

procedure TSkSVGSVG.SetWidth(const AValue: TSkSVGLength);
begin
  sk4d_svgsvg_set_width(Handle, @AValue);
end;

procedure TSkSVGSVG.SetX(const AValue: TSkSVGLength);
begin
  sk4d_svgsvg_set_x(Handle, @AValue);
end;

procedure TSkSVGSVG.SetY(const AValue: TSkSVGLength);
begin
  sk4d_svgsvg_set_y(Handle, @AValue);
end;

function TSkSVGSVG.TryGetViewBox(out AViewBox: TRectF): Boolean;
begin
  Result := sk4d_svgsvg_get_view_box(Handle, sk_rect_t(AViewBox));
end;

{ TSkUnicode }

class procedure TSkUnicode.bidi_region_proc(start, &end: int32_t;
  level: uint8_t; context: Pointer);
begin
  TSkUnicodeBidiRegionProc(context^)(start, &end, level);
end;

class procedure TSkUnicode.break_proc(position, status: int32_t;
  context: Pointer);
begin
  TSkUnicodeBreakProc(context^)(position, status);
end;

class procedure TSkUnicode.codepoint_proc(unichar: sk_unichar_t; start,
  &end: int32_t; context: Pointer);
begin
  TSkUnicodeCodepointProc(context^)(unichar, start, &end);
end;

constructor TSkUnicode.Create;
begin
  inherited Create(sk4d_unicode_create());
end;

procedure TSkUnicode.ForEachBidiRegion(const AText: string;
  const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
begin
  if Assigned(AProc) then
    sk4d_unicode_for_each_bidi_region(Handle, @AText[Low(AText)], Length(AText), sk_direction_t(ADirection), bidi_region_proc, @AProc);
end;

procedure TSkUnicode.ForEachBreak(const AText: string;
  const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
begin
  if Assigned(AProc) then
    sk4d_unicode_for_each_break(Handle, @AText[Low(AText)], Length(AText), sk_breaktype_t(AType), break_proc, @AProc);
end;

procedure TSkUnicode.ForEachCodepoint(const AText: string;
  const AProc: TSkUnicodeCodepointProc);
begin
  if Assigned(AProc) then
    sk4d_unicode_for_each_codepoint(Handle, @AText[Low(AText)], Length(AText), codepoint_proc, @AProc);
end;

function TSkUnicode.GetBreakIterator(const AType: TSkBreakType;
  const AText: string): ISkUnicodeBreakIterator;
begin
  Result := TUnicodeBreakIterator.Create(Self, AType, AText);
end;

function TSkUnicode.GetBreakIteratorUTF8(const AType: TSkBreakType;
  const AText: UTF8String): ISkUnicodeBreakIterator;
begin
  Result := TUnicodeBreakIterator.Create(Self, AType, AText);
end;

function TSkUnicode.GetBreaks(const AText: string;
  const AType: TSkBreakType): TArray<string>;
var
  LPStartIndex: PInteger;
  LResult: TList<string>;
  LStartIndex: Integer;
begin
  LStartIndex  := 0;
  LPStartIndex := @LStartIndex;
  LResult := TList<string>.Create;
  try
    ForEachBreak(AText, AType,
      procedure (const APosition, {%H-}AStatus: Integer)
      begin
        if APosition = 0 then
          Exit;
        LResult.Add(Copy(AText, LPStartIndex^ + 1, APosition - LPStartIndex^));
        LPStartIndex^ := APosition;
      end);
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

class procedure TSkUnicode.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_unicode_destroy(AHandle);
end;

{ TSkUnicode.TUnicodeBreakIterator }

constructor TSkUnicode.TUnicodeBreakIterator.Create(const AUnicode: ISkUnicode;
  const AType: TSkBreakType; const AText: string);
var
  LText: pchar16_t;
begin
  if Length(AText) = 0 then
    LText := nil
  else
    LText := @AText[Low(AText)];
  inherited Create(sk4d_unicodebreakiterator_create2(AUnicode.Handle, sk_breaktype_t(AType), LText, Length(AText)));
end;

constructor TSkUnicode.TUnicodeBreakIterator.Create(const AUnicode: ISkUnicode;
  const AType: TSkBreakType; const AText: UTF8String);
var
  LText: _pchar;
begin
  if Length(AText) = 0 then
    LText := nil
  else
    LText := @AText[Low(AText)];
  inherited Create(sk4d_unicodebreakiterator_create(AUnicode.Handle, sk_breaktype_t(AType), LText, Length(AText)));
end;

function TSkUnicode.TUnicodeBreakIterator.GetCurrent: TSkUnicodeBreakIteratorElem;
begin
  Result := FCurrent;
end;

function TSkUnicode.TUnicodeBreakIterator.MoveNext: Boolean;
begin
  Result := sk4d_unicodebreakiterator_next(Handle, sk_unicodebreakiteratorelem_t(FCurrent));
end;

class procedure TSkUnicode.TUnicodeBreakIterator.__DestroyHandle(
  const AHandle: sk_handle_t);
begin
  sk4d_unicodebreakiterator_destroy(AHandle);
end;

function ExtensionToEncodedImageFormat(
  const AValue: string): TSkEncodedImageFormat;
begin
  if SameText('.jpg', AValue{%H-}) or SameText('.jpeg', AValue{%H-}) then
    Result := TSkEncodedImageFormat.JPEG
  else if SameText('.webp', AValue{%H-}) then
    Result := TSkEncodedImageFormat.WEBP
  else if SameText('.png', AValue{%H-}) then
    Result := TSkEncodedImageFormat.PNG
  else
    raise ESkArgumentException.Create('Invalid extension');
end;

{ TSkBindings }

class function TSkBindings.SafeCreate<T>(const AHandle: sk_handle_t;
  const AOwnsHandle: Boolean): T;
begin
  if AHandle = 0 then
    Exit(nil);
  Result := T.Wrap(AHandle, AOwnsHandle);
end;

class function TSkBindings.SafeHandle(const AObject: ISkObject): sk_handle_t;
begin
  if not Assigned(AObject) then
    Exit(0);
  Result := AObject.Handle;
end;

{ TSkData }

class function TSkData.MakeFromBytes(const ABytes: TBytes): ISkData;
begin
  Result := TSkBindings.SafeCreate<TSkData>(sk4d_data_make_with_copy(@ABytes[0], Length(ABytes)));
end;

class procedure TSkData.__RefHandle(const AHandle: sk_handle_t);
begin
  sk4d_data_ref(AHandle);
end;

class procedure TSkData.__UnrefHandle(const AHandle: sk_handle_t);
begin
  sk4d_data_unref(AHandle);
end;

{ TSkStreamAdapter }

class constructor TSkStreamAdapter.Create;
var
  LProcs: sk_streamadapter_procs_t;
begin
  LProcs.get_length   := get_length_proc;
  LProcs.get_position := get_position_proc;
  LProcs.read         := read_proc;
  LProcs.seek         := seek_proc;
  sk4d_streamadapter_set_procs(@LProcs);
end;

constructor TSkStreamAdapter.Create(const AStream: TStream);
begin
  inherited Create(sk4d_streamadapter_create(AStream));
end;

class function TSkStreamAdapter.get_length_proc(context: Pointer): size_t;
begin
  Result := TStream(context).Size;
end;

class function TSkStreamAdapter.get_position_proc(context: Pointer): size_t;
begin
  Result := TStream(context).Position;
end;

class function TSkStreamAdapter.read_proc(context, buffer: Pointer;
  size: size_t): size_t;
begin
  Result := TStream(context).Read(buffer^, size);
end;

class function TSkStreamAdapter.seek_proc(context: Pointer;
  position: size_t): _bool;
begin
  TStream(context).Position := position;
  Result := True;
end;

class procedure TSkStreamAdapter.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_streamadapter_destroy(AHandle);
end;

{ TSkWStreamAdapter }

class constructor TSkWStreamAdapter.Create;
var
  LProcs: sk_wstreamadapter_procs_t;
begin
  LProcs.write := write_proc;
  sk4d_wstreamadapter_set_procs(@LProcs);
end;

constructor TSkWStreamAdapter.Create(const AStream: TStream);
begin
  inherited Create(sk4d_wstreamadapter_create(AStream));
end;

class function TSkWStreamAdapter.write_proc(context: Pointer;
  const buffer: Pointer; size: size_t): _bool;
begin
  TStream(context).Write(buffer^, size);
  Result := True;
end;

class procedure TSkWStreamAdapter.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_wstreamadapter_destroy(AHandle);
end;

{ TSkString }

constructor TSkString.Create;
begin
  inherited Create(sk4d_string_create());
end;

function TSkString.GetText: string;
begin
  Result := string(sk4d_string_get_text(Handle));
end;

class procedure TSkString.__DestroyHandle(const AHandle: sk_handle_t);
begin
  sk4d_string_destroy(AHandle);
end;

{$HPPEMIT NOUSINGNAMESPACE}
{$HPPEMIT END '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SYSTEM_SKIA)'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrBackendRenderTarget;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrBackendSemaphore;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrBackendSurfaceMutableState;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrBackendTexture;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrDirectContext;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrGlInterface;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrPersistentCache;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrShaderErrorHandler;'}
{$HPPEMIT END '    using ::System::Skia::_di_IGrVkExtensions;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkAnimationCodecPlayer;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkBlender;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkCanvas;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkCodec;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkColorFilter;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkColorSpace;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkColorSpaceICCProfile;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkDocument;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkEnumerable;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkFileResourceProvider;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkFlattenable;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkFont;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkImage;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkImageFilter;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkMaskFilter;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkNonVirtualReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkObject;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkOpBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkottieAnimation;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPaint;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkParagraph;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkParagraphBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkParagraphStyle;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkParticleEffect;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPath;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPathBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPathEffect;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPathIterator;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPathMeasure;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPicture;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPictureRecorder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkPixmap;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRegion;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRegionCliperator;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRegionIterator;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRegionSpanerator;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkResourceProvider;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRoundRect;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRuntimeBlenderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRuntimeEffect;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRuntimeEffectBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkRuntimeShaderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkShader;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkShaper;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkStrutStyle;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkSurface;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkSVGDOM;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkSVGNode;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkSVGSVG;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkTextBlob;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkTextStyle;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkTraceMemoryDump;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkTypeface;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkTypefaceFontProvider;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkUnicode;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkUnicodeBreakIterator;'}
{$HPPEMIT END '    using ::System::Skia::_di_ISkVertices;'}
{$HPPEMIT END '    using ::System::Skia::_di_TGrGlGetProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TGrVkGetProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkFontPathProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkImageRasterReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkImageTextureReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkParagraphVisitProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkSurfaceRasterReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkUnicodeBidiRegionProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkUnicodeBreakProc;'}
{$HPPEMIT END '    using ::System::Skia::_di_TSkUnicodeCodepointProc;'}
{$HPPEMIT END '    using ::System::Skia::ESkArgumentException;'}
{$HPPEMIT END '    using ::System::Skia::ESkException;'}
{$HPPEMIT END '    using ::System::Skia::GrGlenum;'}
{$HPPEMIT END '    using ::System::Skia::GrGluint;'}
{$HPPEMIT END '    using ::System::Skia::GrMtlHandle;'}
{$HPPEMIT END '    using ::System::Skia::GrVkBool32;'}
{$HPPEMIT END '    using ::System::Skia::GrVkChromaLocation;'}
{$HPPEMIT END '    using ::System::Skia::GrVkDevice;'}
{$HPPEMIT END '    using ::System::Skia::GrVkDeviceMemory;'}
{$HPPEMIT END '    using ::System::Skia::GrVkDeviceSize;'}
{$HPPEMIT END '    using ::System::Skia::GrVkFilter;'}
{$HPPEMIT END '    using ::System::Skia::GrVkFormat;'}
{$HPPEMIT END '    using ::System::Skia::GrVkFormatFeatureFlags;'}
{$HPPEMIT END '    using ::System::Skia::GrVkImage;'}
{$HPPEMIT END '    using ::System::Skia::GrVkImageLayout;'}
{$HPPEMIT END '    using ::System::Skia::GrVkImageTiling;'}
{$HPPEMIT END '    using ::System::Skia::GrVkImageUsageFlags;'}
{$HPPEMIT END '    using ::System::Skia::GrVkInstance;'}
{$HPPEMIT END '    using ::System::Skia::GrVkPhysicalDevice;'}
{$HPPEMIT END '    using ::System::Skia::GrVkQueue;'}
{$HPPEMIT END '    using ::System::Skia::GrVkSamplerYcbcrModelConversion;'}
{$HPPEMIT END '    using ::System::Skia::GrVkSamplerYcbcrRange;'}
{$HPPEMIT END '    using ::System::Skia::GrVkSemaphore;'}
{$HPPEMIT END '    using ::System::Skia::GrVkSharingMode;'}
{$HPPEMIT END '    using ::System::Skia::IGrBackendRenderTarget;'}
{$HPPEMIT END '    using ::System::Skia::IGrBackendSemaphore;'}
{$HPPEMIT END '    using ::System::Skia::IGrBackendSurfaceMutableState;'}
{$HPPEMIT END '    using ::System::Skia::IGrBackendTexture;'}
{$HPPEMIT END '    using ::System::Skia::IGrDirectContext;'}
{$HPPEMIT END '    using ::System::Skia::IGrGlInterface;'}
{$HPPEMIT END '    using ::System::Skia::IGrPersistentCache;'}
{$HPPEMIT END '    using ::System::Skia::IGrShaderErrorHandler;'}
{$HPPEMIT END '    using ::System::Skia::IGrVkExtensions;'}
{$HPPEMIT END '    using ::System::Skia::ISkAnimationCodecPlayer;'}
{$HPPEMIT END '    using ::System::Skia::ISkBlender;'}
{$HPPEMIT END '    using ::System::Skia::ISkCanvas;'}
{$HPPEMIT END '    using ::System::Skia::ISkCodec;'}
{$HPPEMIT END '    using ::System::Skia::ISkColorFilter;'}
{$HPPEMIT END '    using ::System::Skia::ISkColorSpace;'}
{$HPPEMIT END '    using ::System::Skia::ISkColorSpaceICCProfile;'}
{$HPPEMIT END '    using ::System::Skia::ISkDocument;'}
{$HPPEMIT END '    using ::System::Skia::ISkEnumerable;'}
{$HPPEMIT END '    using ::System::Skia::ISkFileResourceProvider;'}
{$HPPEMIT END '    using ::System::Skia::ISkFlattenable;'}
{$HPPEMIT END '    using ::System::Skia::ISkFont;'}
{$HPPEMIT END '    using ::System::Skia::ISkImage;'}
{$HPPEMIT END '    using ::System::Skia::ISkImageFilter;'}
{$HPPEMIT END '    using ::System::Skia::ISkMaskFilter;'}
{$HPPEMIT END '    using ::System::Skia::ISkNonVirtualReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::ISkObject;'}
{$HPPEMIT END '    using ::System::Skia::ISkOpBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkottieAnimation;'}
{$HPPEMIT END '    using ::System::Skia::ISkPaint;'}
{$HPPEMIT END '    using ::System::Skia::ISkParagraph;'}
{$HPPEMIT END '    using ::System::Skia::ISkParagraphBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkParagraphStyle;'}
{$HPPEMIT END '    using ::System::Skia::ISkParticleEffect;'}
{$HPPEMIT END '    using ::System::Skia::ISkPath;'}
{$HPPEMIT END '    using ::System::Skia::ISkPathBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkPathEffect;'}
{$HPPEMIT END '    using ::System::Skia::ISkPathIterator;'}
{$HPPEMIT END '    using ::System::Skia::ISkPathMeasure;'}
{$HPPEMIT END '    using ::System::Skia::ISkPicture;'}
{$HPPEMIT END '    using ::System::Skia::ISkPictureRecorder;'}
{$HPPEMIT END '    using ::System::Skia::ISkPixmap;'}
{$HPPEMIT END '    using ::System::Skia::ISkReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::ISkRegion;'}
{$HPPEMIT END '    using ::System::Skia::ISkRegionCliperator;'}
{$HPPEMIT END '    using ::System::Skia::ISkRegionIterator;'}
{$HPPEMIT END '    using ::System::Skia::ISkRegionSpanerator;'}
{$HPPEMIT END '    using ::System::Skia::ISkResourceProvider;'}
{$HPPEMIT END '    using ::System::Skia::ISkRoundRect;'}
{$HPPEMIT END '    using ::System::Skia::ISkRuntimeBlenderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkRuntimeEffect;'}
{$HPPEMIT END '    using ::System::Skia::ISkRuntimeEffectBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkRuntimeShaderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::ISkShader;'}
{$HPPEMIT END '    using ::System::Skia::ISkShaper;'}
{$HPPEMIT END '    using ::System::Skia::ISkStrutStyle;'}
{$HPPEMIT END '    using ::System::Skia::ISkSurface;'}
{$HPPEMIT END '    using ::System::Skia::ISkSVGDOM;'}
{$HPPEMIT END '    using ::System::Skia::ISkSVGNode;'}
{$HPPEMIT END '    using ::System::Skia::ISkSVGSVG;'}
{$HPPEMIT END '    using ::System::Skia::ISkTextBlob;'}
{$HPPEMIT END '    using ::System::Skia::ISkTextStyle;'}
{$HPPEMIT END '    using ::System::Skia::ISkTraceMemoryDump;'}
{$HPPEMIT END '    using ::System::Skia::ISkTypeface;'}
{$HPPEMIT END '    using ::System::Skia::ISkTypefaceFontProvider;'}
{$HPPEMIT END '    using ::System::Skia::ISkUnicode;'}
{$HPPEMIT END '    using ::System::Skia::ISkUnicodeBreakIterator;'}
{$HPPEMIT END '    using ::System::Skia::ISkVertices;'}
{$HPPEMIT END '    using ::System::Skia::PCardinalArray;'}
{$HPPEMIT END '    using ::System::Skia::PPointFArray;'}
{$HPPEMIT END '    using ::System::Skia::PSingleArray;'}
{$HPPEMIT END '    using ::System::Skia::PWordArray;'}
{$HPPEMIT END '    using ::System::Skia::TGrBackendAPI;'}
{$HPPEMIT END '    using ::System::Skia::TGrBackendRenderTarget;'}
{$HPPEMIT END '    using ::System::Skia::TGrBackendSemaphore;'}
{$HPPEMIT END '    using ::System::Skia::TGrBackendSurfaceMutableState;'}
{$HPPEMIT END '    using ::System::Skia::TGrBackendTexture;'}
{$HPPEMIT END '    using ::System::Skia::TGrContextOptions;'}
{$HPPEMIT END '    using ::System::Skia::TGrDirectContext;'}
{$HPPEMIT END '    using ::System::Skia::TGrGlFramebufferInfo;'}
{$HPPEMIT END '    using ::System::Skia::TGrGlGetProc;'}
{$HPPEMIT END '    using ::System::Skia::TGrGlInterface;'}
{$HPPEMIT END '    using ::System::Skia::TGrGlTextureInfo;'}
{$HPPEMIT END '    using ::System::Skia::TGrMtlBackendContext;'}
{$HPPEMIT END '    using ::System::Skia::TGrMtlTextureInfo;'}
{$HPPEMIT END '    using ::System::Skia::TGrPersistentCacheBaseClass;'}
{$HPPEMIT END '    using ::System::Skia::TGrShaderCacheStrategy;'}
{$HPPEMIT END '    using ::System::Skia::TGrShaderErrorHandlerBaseClass;'}
{$HPPEMIT END '    using ::System::Skia::TGrSurfaceOrigin;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkAlloc;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkAllocFlag;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkAllocFlags;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkBackendContext;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkExtensions;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkGetProc;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkImageInfo;'}
{$HPPEMIT END '    using ::System::Skia::TGrVkYcbcrConversionInfo;'}
{$HPPEMIT END '    using ::System::Skia::TSkAffinity;'}
{$HPPEMIT END '    using ::System::Skia::TSkAlphaType;'}
{$HPPEMIT END '    using ::System::Skia::TSkAnimatedWebPEncoder;'}
{$HPPEMIT END '    using ::System::Skia::TSkAnimationCodecPlayer;'}
{$HPPEMIT END '    using ::System::Skia::TSkBlender;'}
{$HPPEMIT END '    using ::System::Skia::TSkBlendMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkBlurStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkBreakType;'}
{$HPPEMIT END '    using ::System::Skia::TSkCanvas;'}
{$HPPEMIT END '    using ::System::Skia::TSkClipOp;'}
{$HPPEMIT END '    using ::System::Skia::TSkCodec;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorChannel;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorFilter;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorMatrix;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorSpace;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorSpaceICCProfile;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorSpacePrimaries;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorSpaceTransferFunction;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorSpaceXyz;'}
{$HPPEMIT END '    using ::System::Skia::TSkColorType;'}
{$HPPEMIT END '    using ::System::Skia::TSkContrastInvertStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkCubicResampler;'}
{$HPPEMIT END '    using ::System::Skia::TSkDirection;'}
{$HPPEMIT END '    using ::System::Skia::TSkDocument;'}
{$HPPEMIT END '    using ::System::Skia::TSkDrawPointsMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkEncodedImageFormat;'}
{$HPPEMIT END '    using ::System::Skia::TSkFileResourceProvider;'}
{$HPPEMIT END '    using ::System::Skia::TSkFilterMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkFont;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontEdging;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontHinting;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontMetrics;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontMetricsFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontMetricsFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontPathProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontSlant;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontWeight;'}
{$HPPEMIT END '    using ::System::Skia::TSkFontWidth;'}
{$HPPEMIT END '    using ::System::Skia::TSkFrame;'}
{$HPPEMIT END '    using ::System::Skia::TSkGraphics;'}
{$HPPEMIT END '    using ::System::Skia::TSkHighContrastConfig;'}
{$HPPEMIT END '    using ::System::Skia::TSkImage;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageCachingHint;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageEncoder;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageFilter;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageInfo;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageRasterReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkImageTextureReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkLattice;'}
{$HPPEMIT END '    using ::System::Skia::TSkLatticeRectType;'}
{$HPPEMIT END '    using ::System::Skia::TSkMaskFilter;'}
{$HPPEMIT END '    using ::System::Skia::TSkMetrics;'}
{$HPPEMIT END '    using ::System::Skia::TSkMipmapMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkNonVirtualReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::TSkObject;'}
{$HPPEMIT END '    using ::System::Skia::TSkOpBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkottieAnimation;'}
{$HPPEMIT END '    using ::System::Skia::TSkottieAnimationRenderFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkottieAnimationRenderFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkOverdrawColor;'}
{$HPPEMIT END '    using ::System::Skia::TSkPaint;'}
{$HPPEMIT END '    using ::System::Skia::TSkPaintStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraph;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphVisitorFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphVisitorFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphVisitorInfo;'}
{$HPPEMIT END '    using ::System::Skia::TSkParagraphVisitProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkParticleEffect;'}
{$HPPEMIT END '    using ::System::Skia::TSkParticleUniform;'}
{$HPPEMIT END '    using ::System::Skia::TSkParticleUniformData;'}
{$HPPEMIT END '    using ::System::Skia::TSkPatchColors;'}
{$HPPEMIT END '    using ::System::Skia::TSkPatchCubics;'}
{$HPPEMIT END '    using ::System::Skia::TSkPatchTexCoords;'}
{$HPPEMIT END '    using ::System::Skia::TSkPath;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathArcSize;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathDirection;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathEffect;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathEffect1DStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathEffectTrimMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathFillType;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathIteratorElem;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathIteratorElemEnumerator;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathMeasure;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathMeasureMatrixFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathMeasureMatrixFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathOp;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathPoints;'}
{$HPPEMIT END '    using ::System::Skia::TSkPathVerb;'}
{$HPPEMIT END '    using ::System::Skia::TSkPDFMetadata;'}
{$HPPEMIT END '    using ::System::Skia::TSkPicture;'}
{$HPPEMIT END '    using ::System::Skia::TSkPictureRecorder;'}
{$HPPEMIT END '    using ::System::Skia::TSkPixelGeometry;'}
{$HPPEMIT END '    using ::System::Skia::TSkPixmap;'}
{$HPPEMIT END '    using ::System::Skia::TSkPlaceholderAlignment;'}
{$HPPEMIT END '    using ::System::Skia::TSkPlaceholderStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkPointEnumerator;'}
{$HPPEMIT END '    using ::System::Skia::TSkPositionAffinity;'}
{$HPPEMIT END '    using ::System::Skia::TSkRectEnumerator;'}
{$HPPEMIT END '    using ::System::Skia::TSkRectHeightStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkRectWidthStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkReferenceCounted;'}
{$HPPEMIT END '    using ::System::Skia::TSkRegion;'}
{$HPPEMIT END '    using ::System::Skia::TSkRegionOp;'}
{$HPPEMIT END '    using ::System::Skia::TSkResourceProviderBaseClass;'}
{$HPPEMIT END '    using ::System::Skia::TSkRotationScaleMatrix;'}
{$HPPEMIT END '    using ::System::Skia::TSkRoundRect;'}
{$HPPEMIT END '    using ::System::Skia::TSkRoundRectCorner;'}
{$HPPEMIT END '    using ::System::Skia::TSkRoundRectRadii;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeBlenderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffect;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectChildType;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat2;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat2x2;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat3;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat3x3;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat4;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectFloat4x4;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectInt2;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectInt3;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectInt4;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeEffectUniformType;'}
{$HPPEMIT END '    using ::System::Skia::TSkRuntimeShaderBuilder;'}
{$HPPEMIT END '    using ::System::Skia::TSkSamplingOptions;'}
{$HPPEMIT END '    using ::System::Skia::TSkSaveLayerFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkSaveLayerFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkSegmentMask;'}
{$HPPEMIT END '    using ::System::Skia::TSkSegmentMasks;'}
{$HPPEMIT END '    using ::System::Skia::TSkShader;'}
{$HPPEMIT END '    using ::System::Skia::TSkShaper;'}
{$HPPEMIT END '    using ::System::Skia::TSkSrcRectConstraint;'}
{$HPPEMIT END '    using ::System::Skia::TSkStrokeCap;'}
{$HPPEMIT END '    using ::System::Skia::TSkStrokeJoin;'}
{$HPPEMIT END '    using ::System::Skia::TSkStrutStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkSurface;'}
{$HPPEMIT END '    using ::System::Skia::TSkSurfaceProperties;'}
{$HPPEMIT END '    using ::System::Skia::TSkSurfacePropertiesFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkSurfacePropertiesFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkSurfaceRasterReleaseProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGAspectAlign;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGAspectScale;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGCanvas;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGCanvasFlag;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGCanvasFlags;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGDOM;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGLength;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGLengthUnit;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGNode;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGPreserveAspectRatio;'}
{$HPPEMIT END '    using ::System::Skia::TSkSVGSVG;'}
{$HPPEMIT END '    using ::System::Skia::TSkTableFilter;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextAlign;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextBaseline;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextBlob;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextBox;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextDecoration;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextDecorations;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextDecorationStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextDirection;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextHeightBehavior;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextHeightBehaviors;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextShadow;'}
{$HPPEMIT END '    using ::System::Skia::TSkTextStyle;'}
{$HPPEMIT END '    using ::System::Skia::TSkTileMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkTraceMemoryDumpBaseClass;'}
{$HPPEMIT END '    using ::System::Skia::TSkTypeface;'}
{$HPPEMIT END '    using ::System::Skia::TSkTypefaceFontProvider;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicode;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicodeBidiRegionProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicodeBreakIteratorElem;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicodeBreakIteratorElemEnumerator;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicodeBreakProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkUnicodeCodepointProc;'}
{$HPPEMIT END '    using ::System::Skia::TSkVertexMode;'}
{$HPPEMIT END '    using ::System::Skia::TSkVertices;'}
{$HPPEMIT END '    typedef TSkEncodedImageFormat (__fastcall *TExtensionToEncodedImageFormatFunc)(const ::System::UnicodeString AValue);'}
{$HPPEMIT END '    static const TSkFontSlant SkFontSlantRegular = ::System::Skia::SkFontSlantRegular;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightDemiBold = ::System::Skia::SkFontWeightDemiBold;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightHairline = ::System::Skia::SkFontWeightHairline;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightHeavy = ::System::Skia::SkFontWeightHeavy;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightRegular = ::System::Skia::SkFontWeightRegular;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightUltraBlack = ::System::Skia::SkFontWeightUltraBlack;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightUltraBold = ::System::Skia::SkFontWeightUltraBold;'}
{$HPPEMIT END '    static const TSkFontWeight SkFontWeightUltraLight = ::System::Skia::SkFontWeightUltraLight;'}
{$HPPEMIT END '    static const TSkFontWidth SkFontWidthRegular = ::System::Skia::SkFontWidthRegular;'}
{$HPPEMIT END '    static ::System::StaticArray<int, 23>& SkBytesPerPixel = ::System::Skia::SkBytesPerPixel;'}
{$HPPEMIT END '    static TSkColorMatrix& SkColorMatrixIdentity = ::System::Skia::SkColorMatrixIdentity;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionHLG = ::System::Skia::SkColorSpaceTransferFunctionHLG;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionLinear = ::System::Skia::SkColorSpaceTransferFunctionLinear;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionPQ = ::System::Skia::SkColorSpaceTransferFunctionPQ;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionRec2020 = ::System::Skia::SkColorSpaceTransferFunctionRec2020;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionSRGB = ::System::Skia::SkColorSpaceTransferFunctionSRGB;'}
{$HPPEMIT END '    static TSkColorSpaceTransferFunction& SkColorSpaceTransferFunctionTwoDotTwo = ::System::Skia::SkColorSpaceTransferFunctionTwoDotTwo;'}
{$HPPEMIT END '    static TSkColorSpaceXyz& SkColorSpaceXyzAdobeRGB = ::System::Skia::SkColorSpaceXyzAdobeRGB;'}
{$HPPEMIT END '    static TSkColorSpaceXyz& SkColorSpaceXyzDisplayP3 = ::System::Skia::SkColorSpaceXyzDisplayP3;'}
{$HPPEMIT END '    static TSkColorSpaceXyz& SkColorSpaceXyzIdentity = ::System::Skia::SkColorSpaceXyzIdentity;'}
{$HPPEMIT END '    static TSkColorSpaceXyz& SkColorSpaceXyzRec2020 = ::System::Skia::SkColorSpaceXyzRec2020;'}
{$HPPEMIT END '    static TSkColorSpaceXyz& SkColorSpaceXyzSRGB = ::System::Skia::SkColorSpaceXyzSRGB;'}
{$HPPEMIT END '    static TSkCubicResampler& SkCubicResamplerCatmullRom = ::System::Skia::SkCubicResamplerCatmullRom;'}
{$HPPEMIT END '    static TSkCubicResampler& SkCubicResamplerMitchell = ::System::Skia::SkCubicResamplerMitchell;'}
{$HPPEMIT END '    static TSkFontStyle& SkFontStyleBold = ::System::Skia::SkFontStyleBold;'}
{$HPPEMIT END '    static TSkFontStyle& SkFontStyleBoldItalic = ::System::Skia::SkFontStyleBoldItalic;'}
{$HPPEMIT END '    static TSkFontStyle& SkFontStyleItalic = ::System::Skia::SkFontStyleItalic;'}
{$HPPEMIT END '    static TSkFontStyle& SkFontStyleNormal = ::System::Skia::SkFontStyleNormal;'}
{$HPPEMIT END '    static TSkColorType& SkNative32ColorType = ::System::Skia::SkNative32ColorType;'}
{$HPPEMIT END '    static TSkRotationScaleMatrix& SkRotationScaleMatrixIdentity = ::System::Skia::SkRotationScaleMatrixIdentity;'}
{$HPPEMIT END '    static TSkSamplingOptions& SkSamplingOptionsHigh = ::System::Skia::SkSamplingOptionsHigh;'}
{$HPPEMIT END '    static TSkSamplingOptions& SkSamplingOptionsLow = ::System::Skia::SkSamplingOptionsLow;'}
{$HPPEMIT END '    static TSkSamplingOptions& SkSamplingOptionsMedium = ::System::Skia::SkSamplingOptionsMedium;'}
{$HPPEMIT END '    static ::System::StaticArray<int, 23>& SkShiftPerPixel = ::System::Skia::SkShiftPerPixel;'}
{$HPPEMIT END '    static const TExtensionToEncodedImageFormatFunc ExtensionToEncodedImageFormat = ::System::Skia::ExtensionToEncodedImageFormat;'}
{$HPPEMIT END '#endif'}
end.
