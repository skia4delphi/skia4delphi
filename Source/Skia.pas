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
unit Skia;

interface

{$ALIGN ON}
{$MINENUMSIZE 4}
{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes,
  System.DateUtils,
  System.Math.Vectors,
  System.SysUtils,
  System.Types,
  System.UITypes,

  { Skia }
  Skia.API,
  Skia.Bindings;

type
  ESkException = class(Exception);

  ESkArgumentException = class(ESkException);

  TGrBackendAPI = (OpenGl, Metal = 2);

  GrGlenum = Cardinal;
  GrGluint = Cardinal;

  { TGrGlFramebufferInfo }

  TGrGlFramebufferInfo = record
    FBOID: GrGluint;
    Format: GrGlenum;
    constructor Create(const AFBOID: GrGluint; const AFormat: GrGlenum);
    class operator Equal(const AGlFramebufferInfo1, AGlFramebufferInfo2: TGrGlFramebufferInfo): Boolean;
    class operator NotEqual(const AGlFramebufferInfo1, AGlFramebufferInfo2: TGrGlFramebufferInfo): Boolean;
  end;

  GrMtlHandle = Pointer;

  { TGrMtlTextureInfo }

  TGrMtlTextureInfo = record
    Texture: GrMtlHandle;
    constructor Create(const ATexture: GrMtlHandle);
    class operator Equal(const AMtlTextureInfo1, AMtlTextureInfo2: TGrMtlTextureInfo): Boolean;
    class operator NotEqual(const AMtlTextureInfo1, AMtlTextureInfo2: TGrMtlTextureInfo): Boolean;
  end;

  { IGrBackendRenderTarget }

  IGrBackendRenderTarget = interface(ISkObject)
    ['{499243CD-E98D-4DFB-9606-92E3E679E6CF}']
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
  strict protected
    function GetBackendAPI: TGrBackendAPI;
    function GetHeight: Integer;
    function GetSampleCount: Integer;
    function GetStencilBits: Integer;
    function GetWidth: Integer;
    function IsValid: Boolean;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor CreateGl(const AWidth, AHeight, ASampleCount, AStencilBits: Integer; const AFramebufferInfo: TGrGlFramebufferInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const ATextureInfo: TGrMtlTextureInfo);
  end;

  { TGrGlTextureInfo }

  TGrGlTextureInfo = record
    Target: GrGlenum;
    ID: GrGluint;
    Format: GrGlenum;
    constructor Create(const ATarget: GrGlenum; const AID: GrGluint; const AFormat: GrGlenum);
    class operator Equal(const AGlTextureInfo1, AGlTextureInfo2: TGrGlTextureInfo): Boolean;
    class operator NotEqual(const AGlTextureInfo1, AGlTextureInfo2: TGrGlTextureInfo): Boolean;
  end;

  { IGrBackendTexture }

  IGrBackendTexture = interface(ISkObject)
    ['{9E4E938C-0513-4690-851E-7F4BC1323B84}']
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
  strict protected
    function GetBackendAPI: TGrBackendAPI;
    function GetGlTextureInfo(out ATextureInfo: TGrGlTextureInfo): Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasMipmaps: Boolean;
    function IsValid: Boolean;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor CreateGl(const AWidth, AHeight: Integer; const AIsMipmapped: Boolean; const ATextureInfo: TGrGlTextureInfo);
    constructor CreateMetal(const AWidth, AHeight: Integer; const AIsMipmapped: Boolean; const ATextureInfo: TGrMtlTextureInfo);
  end;

  { TGrContextOptions }

  TGrContextOptions = record
    BufferMapThreshold: Integer;
    DoManualMipmapping: Boolean;
    AllowPathMaskCaching: Boolean;
    GlyphCacheTextureMaximumBytes: NativeUInt;
    AvoidStencilBuffers: Boolean;
    RuntimeProgramCacheSize: Integer;
    class operator Equal(const AContextOptions1, AContextOptions2: TGrContextOptions): Boolean;
    class operator NotEqual(const AContextOptions1, AContextOptions2: TGrContextOptions): Boolean;
  end;

  { TGrMtlBackendContext }

  TGrMtlBackendContext = record
    Device: GrMtlHandle;
    Queue: GrMtlHandle;
    BinaryArchive: GrMtlHandle;
    constructor Create(const ADevice, AQueue, ABinaryArchive: GrMtlHandle);
    class operator Equal(const AMtlBackendContext1, AMtlBackendContext2: TGrMtlBackendContext): Boolean;
    class operator NotEqual(const AMtlBackendContext1, AMtlBackendContext2: TGrMtlBackendContext): Boolean;
  end;

  ISkTraceMemoryDump = interface;

  TSkColorType = (
    Unknown,
    Alpha8,
    RGB565,
    ARGB4444,
    RGBA8888,
    RGB888X,
    BGRA8888,
    RGBA1010102,
    BGRA1010102,
    RGB101010X,
    BGR101010X,
    Gray8,
    RGBAF16,
    RGBAF16Clamped,
    RGBAF32,
    RG88,
    AlphaF16,
    RGF16,
    Alpha16,
    RG1616,
    RGBA16161616,
    SRGBA8888
  );

  IGrGlInterface = interface;


  { IGrDirectContext }

  IGrDirectContext = interface(ISkReferenceCounted)
    ['{BDB6E966-B0BE-4B32-96EA-6BAE5127D90F}']
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure FreeGPUResources;
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
    function Submit(const ASyncCPU: Boolean = False): Boolean;
    property BackendAPI: TGrBackendAPI read GetBackendAPI;
    property ResourceCacheLimit: NativeUInt read GetResourceCacheLimit write SetResourceCacheLimit;
  end;

  { TGrDirectContext }

  TGrDirectContext = class(TSkReferenceCounted, IGrDirectContext)
  strict protected
    procedure AbandonContext;
    procedure DumpMemoryStatistics(const ATraceMemoryDump: ISkTraceMemoryDump);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    procedure FreeGPUResources;
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
    function Submit(const ASyncCPU: Boolean = False): Boolean;
  public
    class function MakeGl(AInterface: IGrGlInterface = nil): IGrDirectContext; overload; static;
    class function MakeGl(const AOptions: TGrContextOptions; const AInterface: IGrGlInterface = nil): IGrDirectContext; overload; static;
    class function MakeMetal(const ABackendContext: TGrMtlBackendContext): IGrDirectContext; overload; static;
    class function MakeMetal(const ABackendContext: TGrMtlBackendContext; const AOptions: TGrContextOptions): IGrDirectContext; overload; static;
  end;

  TGrGlGetProc = reference to function (const AName: string): Pointer;

  { IGrGlInterface }

  IGrGlInterface = interface(ISkReferenceCounted)
    ['{24AAE766-2B36-43DC-911A-98842BB81277}']
    function HasExtension(const AName: string): Boolean;
    function Validate: Boolean;
  end;

  { TGrGlInterface }

  TGrGlInterface = class(TSkReferenceCounted, IGrGlInterface)
  strict private
    class function get_proc(context: Pointer; const name: MarshaledAString): Pointer; cdecl; static;
  strict protected
    function HasExtension(const AName: string): Boolean;
    function Validate: Boolean;
  public
    class function MakeAssembled(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledGl(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledGles(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeAssembledWebGl(const AProc: TGrGlGetProc): IGrGlInterface; static;
    class function MakeNative: IGrGlInterface; static;
  end;

  TSkBlendMode = (
    Clear,
    Src,
    Dest,
    SrcOver,
    DestOver,
    SrcIn,
    DestIn,
    SrcOut,
    DestOut,
    SrcATop,
    DestATop,
    &Xor,
    Plus,
    Modulate,
    Screen,
    Overlay,
    Darken,
    Lighten,
    ColorDodge,
    ColorBurn,
    HardLight,
    SoftLight,
    Difference,
    Exclusion,
    Multiply,
    Hue,
    Saturation,
    Color,
    Luminosity
  );

  { ISkBlender }

  ISkBlender = interface(ISkReferenceCounted)
    ['{C8C7B026-3768-4EA5-A3F0-718CBE7F092E}']
  end;

  { TSkBlender }

  TSkBlender = class(TSkReferenceCounted, ISkBlender)
  public
    class function MakeArithmetic(const AK1, AK2, AK3, AK4: Single; const AEnforcePremultipliedColor: Boolean): ISkBlender; static;
    class function MakeMode(const AMode: TSkBlendMode): ISkBlender; static;
  end;

  ISkPath = interface;

  TSkClipOp = (Difference, Intersect);

  ISkRegion = interface;

  ISkRoundRect = interface;

  ISkShader = interface;

  ISkPaint = interface;

  ISkImage = interface;

  { TSkRotationScaleMatrix }

  TSkRotationScaleMatrix = record
    SCosinus: Single;
    SSinus: Single;
    TranslateX: Single;
    TranslateY: Single;
    constructor Create(const ASCosinus, ASSinus, ATranslateX, ATranslateY: Single);
    class function CreateDegrees(const AScale, ADegrees, ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix; static;
    class function CreateRadians(const AScale, ARadians, ATranslateX, ATranslateY, AAnchorX, AAnchorY: Single): TSkRotationScaleMatrix; static;
    class operator Equal(const ARotationScaleMatrix1, ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
    class operator NotEqual(const ARotationScaleMatrix1, ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
    class operator Implicit(const ARotationScaleMatrix: TSkRotationScaleMatrix): TMatrix;
  end;

  { TSkRotationScaleMatrixConstants }

  TSkRotationScaleMatrixConstants = record helper for TSkRotationScaleMatrix
  const
    Identity: TSkRotationScaleMatrix = (SCosinus   : 1; SSinus     : 0;
                                        TranslateX : 0; TranslateY : 0);
  end;

  { TSkCubicResampler }

  TSkCubicResampler = record
    B: Single;
    C: Single;
    constructor Create(const AB, AC: Single);
    class operator Equal(const ACubicResampler1, ACubicResampler2: TSkCubicResampler): Boolean;
    class operator NotEqual(const ACubicResampler1, ACubicResampler2: TSkCubicResampler): Boolean;
  end;

  { TSkCubicResamplerConstants }

  TSkCubicResamplerConstants = record helper for TSkCubicResampler
  const
    CatmullRom : TSkCubicResampler = (B: 0; C: 1 / 2);
    Mitchell   : TSkCubicResampler = (B: 1 / 3; C: 1 / 3);
  end;

  TSkFilterMode = (Nearest, Linear);

  TSkMipmapMode = (None, Nearest, Linear);

  { TSkSamplingOptions }

  TSkSamplingOptions = record
  strict private
    FUseCubic: Boolean;
    FCubic: TSkCubicResampler;
    FFilter: TSkFilterMode;
    FMipmap: TSkMipmapMode;
  public
    constructor Create(const ACubic: TSkCubicResampler); overload;
    constructor Create(const AFilter: TSkFilterMode; const AMipmap: TSkMipmapMode); overload;
    property UseCubic: Boolean read FUseCubic;
    property Cubic: TSkCubicResampler read FCubic;
    property Filter: TSkFilterMode read FFilter;
    property Mipmap: TSkMipmapMode read FMipmap;
    class operator Equal(const ASamplingOptions1, ASamplingOptions2: TSkSamplingOptions): Boolean;
    class operator NotEqual(const ASamplingOptions1, ASamplingOptions2: TSkSamplingOptions): Boolean;
  end;

  { TSkSamplingOptionsConstants }

  TSkSamplingOptionsConstants = record helper for TSkSamplingOptions
  const
    Low    : TSkSamplingOptions = (FUseCubic: False; FCubic: (B: 0; C: 0); FFilter: TSkFilterMode.Nearest; FMipmap: TSkMipmapMode.Nearest);
    Medium : TSkSamplingOptions = (FUseCubic: False; FCubic: (B: 0; C: 0); FFilter: TSkFilterMode.Linear; FMipmap: TSkMipmapMode.Nearest);
    High   : TSkSamplingOptions = (FUseCubic: True; FCubic: (B: 1 / 3; C: 1 / 3); FFilter: TSkFilterMode.Nearest; FMipmap: TSkMipmapMode.None);
  end;

  ISkFont = interface;

  TSkLatticeRectType = (Default, Transparent, FixedColor);

  { TSkLattice }

  TSkLattice = record
  private
    FXDivs: TArray<Integer>;
    FYDivs: TArray<Integer>;
    FRectTypes: TArray<TSkLatticeRectType>;
    FColors: TArray<TAlphaColor>;
    FUseBounds: Boolean;
    FBounds: TRect;
    function GetXDivs: TArray<Integer>;
    function GetYDivs: TArray<Integer>;
    function GetRectTypes: TArray<TSkLatticeRectType>;
    function GetColors: TArray<TAlphaColor>;
  public
    constructor Create(const AXDivs, AYDivs: TArray<Integer>; const ARectTypes: TArray<TSkLatticeRectType> = nil; const AColors: TArray<TAlphaColor> = nil); overload;
    constructor Create(const AXDivs, AYDivs: TArray<Integer>; ABounds: TRect; const ARectTypes: TArray<TSkLatticeRectType> = nil; const AColors: TArray<TAlphaColor> = nil); overload;
    property XDivs: TArray<Integer> read GetXDivs;
    property YDivs: TArray<Integer> read GetYDivs;
    property RectTypes: TArray<TSkLatticeRectType> read GetRectTypes;
    property Colors: TArray<TAlphaColor> read GetColors;
    property UseBounds: Boolean read FUseBounds;
    property Bounds: TRect read FBounds;
    class operator Equal(const ALattice1, ALattice2: TSkLattice): Boolean;
    class operator NotEqual(const ALattice1, ALattice2: TSkLattice): Boolean;
  end;

  TSkSrcRectConstraint = (Close, Fast);

  TSkPatchCubics = array[0..11] of TPointF;

  TSkPatchColors = array[0..3] of TAlphaColor;

  TSkPatchTexCoords = array[0..3] of TPointF;

  ISkPicture = interface;

  TSkDrawPointsMode = (Points, Lines, Polygon);

  ISkTextBlob = interface;

  ISkVertices = interface;

  { ISkCanvas }

  ISkCanvas = interface(ISkObject)
    ['{E960928D-B73D-40EE-A4B6-049D7D9474C9}']
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
    procedure DrawLine(const APoint1, APoint2: TPointF; const APaint: ISkPaint);  overload;
    procedure DrawLine(const AX1, AY1, AX2, AY2: Single; const APaint: ISkPaint);  overload;
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
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure RotateRadians(const ARadians: Single);
    function Save: Integer;
    procedure SaveLayer(const APaint: ISkPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const AScaleX, AScaleY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const ASkewX, ASkewY: Single);
    procedure Translate(const ADeltaX, ADeltaY: Single);
  end;

  { TSkCanvas }

  TSkCanvas = class(TSkObject, ISkCanvas)
  strict protected
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
    procedure DrawImageRect(const AImage: ISkImage; const ASrc, ADest: TRectF; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
    procedure DrawImageRect(const AImage: ISkImage; const ADest: TRectF; const ASampling: TSkSamplingOptions; const APaint: ISkPaint = nil; const AConstraint: TSkSrcRectConstraint = TSkSrcRectConstraint.Fast); overload;
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
    function GetDeviceClipBounds: TRect;
    function GetLocalClipBounds: TRectF;
    function GetLocalToDevice: TMatrix3D;
    function GetLocalToDeviceAs3x3: TMatrix;
    function GetSaveCount: Integer;
    function QuickReject(const ARect: TRectF): Boolean; overload;
    function QuickReject(const APath: ISkPath): Boolean; overload;
    procedure ResetMatrix;
    procedure Restore;
    procedure RestoreToCount(const ASaveCount: Integer);
    procedure Rotate(const ADegrees: Single); overload;
    procedure Rotate(const ADegrees, APX, APY: Single); overload;
    procedure RotateRadians(const ARadians: Single);
    function Save: Integer;
    procedure SaveLayer(const APaint: ISkPaint = nil); overload;
    procedure SaveLayer(const ABounds: TRectF; const APaint: ISkPaint = nil); overload;
    procedure SaveLayerAlpha(const AAlpha: Byte); overload;
    procedure SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte); overload;
    procedure Scale(const AScaleX, AScaleY: Single);
    procedure SetMatrix(const AMatrix: TMatrix); overload;
    procedure SetMatrix(const AMatrix: TMatrix3D); overload;
    procedure Skew(const ASkewX, ASkewY: Single);
    procedure Translate(const ADeltaX, ADeltaY: Single);
    class procedure DestroyHandle(const AHandle: THandle); override;
  end;

  TSkAlphaType = (Unknown, Opaque, Premul, Unpremul);

  ISkColorSpace = interface;

  TSkImage = class;

  { ISkCodec }

  ISkCodec = interface(ISkObject)
    ['{EAFB4293-A036-4B9B-9157-049819923F13}']
    function GetDimensions: TSize;
    function GetHeight: Integer;
    function GetImage(const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil): ISkImage;
    function GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt; const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil): Boolean;
    function GetWidth: Integer;
    property Dimensions: TSize read GetDimensions;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TSkCodec }

  TSkCodec = class(TSkObject, ISkCodec)
  strict protected
    function GetDimensions: TSize;
    function GetHeight: Integer;
    function GetImage(const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil): ISkImage;
    function GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt; const AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil): Boolean;
    function GetWidth: Integer;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    class function MakeFromFile(const AFileName: string): ISkCodec; static;
    class function MakeWithCopy(const AData: Pointer; const ASize: NativeUInt): ISkCodec; static;
    class function MakeWithoutCopy(const AData: Pointer; const ASize: NativeUInt): ISkCodec; static;
  end;

  { ISkAnimationCodecPlayer }

  ISkAnimationCodecPlayer = interface(ISkObject)
    ['{56A3230F-1499-4DBE-A416-6304A6A60E7F}']
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
  strict protected
    function GetDimensions: TSize;
    function GetDuration: Cardinal;
    function GetFrame: ISkImage;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function Seek(const AMilliseconds: Cardinal): Boolean;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    class function MakeFromFile(const AFileName: string): ISkAnimationCodecPlayer; static;
    class function MakeFromStream(const AStream: TStream): ISkAnimationCodecPlayer; static;
  end;

  TSkContrastInvertStyle = (NoInvert, InvertBrightness, InvertLightness);

  { TSkHighContrastConfig }

  TSkHighContrastConfig = record
    Grayscale: Boolean;
    InvertStyle: TSkContrastInvertStyle;
    Contrast: Single;
    constructor Create(const AGrayscale: Boolean; const AInvertStyle: TSkContrastInvertStyle; const AContrast: Single);
    class operator Equal(const AHighContrastConfig1, AHighContrastConfig2: TSkHighContrastConfig): Boolean;
    class operator NotEqual(const AHighContrastConfig1, AHighContrastConfig2: TSkHighContrastConfig): Boolean;
  end;

  { TSkColorMatrix }

  TSkColorMatrix = record
    constructor Create(const AM11, AM12, AM13, AM14, AM15, AM21, AM22, AM23, AM24, AM25, AM31, AM32, AM33, AM34, AM35, AM41, AM42, AM43, AM44, AM45: Single);
    class function CreateSaturation(const ASaturation: Single): TSkColorMatrix; static;
    class function CreateScale(const AScaleR, AScaleG, AScaleB: Single; const AScaleA: Single = 1): TSkColorMatrix; static;
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

  { TSkColorMatrixConstants }

  TSkColorMatrixConstants = record helper for TSkColorMatrix
  const
    Identity: TSkColorMatrix = (M11: 1; M12: 0; M13: 0; M14: 0; M15: 0;
                                M21: 0; M22: 1; M23: 0; M24: 0; M25: 0;
                                M31: 0; M32: 0; M33: 1; M34: 0; M35: 0;
                                M41: 0; M42: 0; M43: 0; M44: 1; M45: 0);
  end;

  TSkOverdrawColor = array[0..5] of TAlphaColor;

  TSkTableFilter = array[0..255] of Byte;

  { ISkColorFilter }

  ISkColorFilter = interface(ISkReferenceCounted)
    ['{5CA5AB17-1B1E-4190-8496-6D2D951C0EDE}']
  end;

  { TSkColorFilter }

  TSkColorFilter = class(TSkReferenceCounted, ISkColorFilter)
  public
    class function MakeBlend(const AColor: TAlphaColor; const AMode: TSkBlendMode): ISkColorFilter; static;
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

  { TSkColorSpaceXyz }

  TSkColorSpaceXyz = record
  public const
    FixedToFloat = 1.52587890625e-5;
  public
    constructor Create(const AM11, AM12, AM13, AM21, AM22, AM23, AM31, AM32, AM33: Single);
    function Adjoint: TSkColorSpaceXyz;
    function Determinant: Single;
    function Inverse: TSkColorSpaceXyz;
    class operator Equal(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
    class operator NotEqual(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
    class operator Multiply(const AColorSpaceXyz1, AColorSpaceXyz2: TSkColorSpaceXyz): TSkColorSpaceXyz;
    case Integer of
      0: (M11, M12, M13,
          M21, M22, M23,
          M31, M32, M33: Single);
      1: (Vector: array[0..2] of array[0..2] of Single);
  end;

  { TSkColorSpaceXyzConstants }

  TSkColorSpaceXyzConstants = record helper for TSkColorSpaceXyz
  const
    Identity  : TSkColorSpaceXyz = (M11: 1; M12: 0; M13: 0;
                                    M21: 0; M22: 1; M23: 0;
                                    M31: 0; M32: 0; M33: 1);

    SRGB      : TSkColorSpaceXyz = (M11: $6FA2 * TSkColorSpaceXyz.FixedToFloat; M12: $6299 * TSkColorSpaceXyz.FixedToFloat; M13: $24A0 * TSkColorSpaceXyz.FixedToFloat;
                                    M21: $38F5 * TSkColorSpaceXyz.FixedToFloat; M22: $B785 * TSkColorSpaceXyz.FixedToFloat; M23: $0F84 * TSkColorSpaceXyz.FixedToFloat;
                                    M31: $0390 * TSkColorSpaceXyz.FixedToFloat; M32: $18DA * TSkColorSpaceXyz.FixedToFloat; M33: $B6CF * TSkColorSpaceXyz.FixedToFloat);

    AdobeRGB  : TSkColorSpaceXyz = (M11: $9C18 * TSkColorSpaceXyz.FixedToFloat; M12: $348D * TSkColorSpaceXyz.FixedToFloat; M13: $2631 * TSkColorSpaceXyz.FixedToFloat;
                                    M21: $4FA5 * TSkColorSpaceXyz.FixedToFloat; M22: $A02C * TSkColorSpaceXyz.FixedToFloat; M23: $102F * TSkColorSpaceXyz.FixedToFloat;
                                    M31: $04FC * TSkColorSpaceXyz.FixedToFloat; M32: $0F95 * TSkColorSpaceXyz.FixedToFloat; M33: $BE9C * TSkColorSpaceXyz.FixedToFloat);

    DisplayP3 : TSkColorSpaceXyz = (M11:  0.515102;   M12: 0.291965;  M13: 0.157153;
                                    M21:  0.241182;   M22: 0.692236;  M23: 0.0665819;
                                    M31: -0.00104941; M32: 0.0418818; M33: 0.784378);

    Rec2020   : TSkColorSpaceXyz = (M11:  0.673459;   M12: 0.165661;  M13: 0.125100;
                                    M21:  0.279033;   M22: 0.675338;  M23: 0.0456288;
                                    M31: -0.00193139; M32: 0.0299794; M33: 0.797162);
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
    class operator Equal(const AColorSpaceTransferFunction1, AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
    class operator NotEqual(const AColorSpaceTransferFunction1, AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
  end;

  { TSkColorSpaceTransferFunctionConstants }

  TSkColorSpaceTransferFunctionConstants = record helper for TSkColorSpaceTransferFunction
  const
    SRGB      : TSkColorSpaceTransferFunction = (G: 2.4; A: 1 / 1.055; B: 0.055 / 1.055; C: 1 / 12.92; D: 0.04045; E: 0; F: 0);
    TwoDotTwo : TSkColorSpaceTransferFunction = (G: 2.2; A: 1; B: 0; C: 0; D: 0; E: 0; F: 0);
    Linear    : TSkColorSpaceTransferFunction = (G: 1; A: 1; B: 0; C: 0; D: 0; E: 0; F: 0);
    Rec2020   : TSkColorSpaceTransferFunction = (G: 2.22222; A: 0.909672; B: 0.0903276; C: 0.222222; D: 0.0812429; E: 0; F: 0);
    PQ        : TSkColorSpaceTransferFunction = (G: -2; A: -107 / 128; B: 1; C: 32 / 2523; D: 2413 / 128; E: -2392 / 128; F: 8192 / 1305);
    HLG       : TSkColorSpaceTransferFunction = (G: -3; A: 2; B: 2; C: 1 / 0.17883277; D: 0.28466892; E: 0.55991073; F: 0);
  end;

  ISkColorSpaceICCProfile = interface;

  { ISkColorSpace }

  ISkColorSpace = interface(ISkNonVirtualReferenceCounted)
    ['{3A1330D4-E2B2-4A62-A87A-C26FDB104310}']
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
  strict protected
    function GammaCloseToSRGB: Boolean;
    function GammaIsLinear: Boolean;
    function IsEqual(const AColorSpace: ISkColorSpace): Boolean;
    function IsNumericalTransferFunction(out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
    function IsSRGB: Boolean;
    function MakeLinearGamma: ISkColorSpace;
    function MakeSRGBGamma: ISkColorSpace;
    function ToProfile: ISkColorSpaceICCProfile;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
    class procedure RefHandle(const AHandle: THandle); override;
    class procedure UnrefHandle(const AHandle: THandle); override;
  public
    class function Make(const AProfile: ISkColorSpaceICCProfile): ISkColorSpace; static;
    class function MakeRGB(const ATransferFunction: TSkColorSpaceTransferFunction; const AToXyzD50: TSkColorSpaceXyz): ISkColorSpace; static;
    class function MakeSRGB: ISkColorSpace; static;
    class function MakeSRGBLinear: ISkColorSpace; static;
  end;

  { ISkColorSpaceICCProfile }

  ISkColorSpaceICCProfile = interface(ISkObject)
    ['{81FAF49C-4699-4E16-AF20-4AA5F3E56408}']
    function ToBytes: TBytes;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
  end;

  { TSkColorSpaceICCProfile }

  TSkColorSpaceICCProfile = class(TSkObject, ISkColorSpaceICCProfile)
  strict protected
    function ToBytes: TBytes;
    function ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    class function MakeFromBytes(const ABytes: TBytes): ISkColorSpaceICCProfile; static;
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
    class operator Implicit(const AColorSpacePrimaries: TSkColorSpacePrimaries): TSkColorSpaceXyz;
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

  { ISkDocument }

  ISkDocument = interface(ISkReferenceCounted)
    ['{98B62FC5-FA4D-42A2-8D28-AF3AF08BE35F}']
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
  public
    class function MakePDF(const AStream: TStream): ISkDocument; overload;
    class function MakePDF(const AStream: TStream; const AMetadata: TSkPDFMetadata): ISkDocument; overload;
    class function MakeXPS(const AStream: TStream; const ADPI: Single = 72): ISkDocument;
  end;

  TSkFontEdging = (Alias, AntiAlias, SubpixelAntiAlias);

  TSkFontHinting = (None, Slight, Normal, Full);

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

  TSkFontPathProc = reference to procedure (const APathOrNil: ISkPath; const AMatrix: TMatrix);

  ISkTypeface = interface;

  { ISkFont }

  ISkFont = interface(ISkObject)
    ['{A8FE9892-368D-4BF7-BD71-FB81FF651098}']
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
    class procedure path_proc(const path: sk_path_t; const matrix: psk_matrix_t; context: Pointer); cdecl; static;
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(ATypeface: ISkTypeface = nil; const ASize: Single = 12; const AScaleX: Single = 1; const ASkewX: Single = 0); overload;
    constructor Create(const AFont: ISkFont); overload;
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

  TSkEncodedImageFormat = (JPEG = 3, PNG, WEBP = 6);

  { TSkEncodedImageFormatHelper }

  TSkEncodedImageFormatHelper = record helper for TSkEncodedImageFormat
    class function FromExtension(const AExtension: string): TSkEncodedImageFormat; static;
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
    function BytesPerPixel: Integer; inline;
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
    function ShiftPerPixel: Integer; inline;
    class operator Equal(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
    class operator NotEqual(const AImageInfo1, AImageInfo2: TSkImageInfo): Boolean;
  end;

  TSkTileMode = (Clamp, &Repeat, Mirror, Decal);

  ISkImageFilter = interface;

  ISkPixmap = interface;

  TSkImageCachingHint = (Allow, Disallow);

  TGrSurfaceOrigin = (TopLeft, BottomLeft);

  TSkImageRasterReleaseProc = reference to procedure (const APixels: Pointer);

  TSkImageTextureReleaseProc = reference to procedure ();

  { ISkImage }

  ISkImage = interface(ISkReferenceCounted)
    ['{5BA7E4F4-9A74-4025-8691-D4E6F1FE15D0}']
    function Encode(const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes;
    procedure EncodeToFile(const AFileName: string; const AQuality: Integer = 80); overload;
    procedure EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80); overload;
    procedure EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80);
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
    function MakeNonTextureImage(out AImage: ISkImage): Boolean;
    function MakeRasterImage(out AImage: ISkImage): Boolean;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeSubset(const ASubset: TRect; AContext: IGrDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGrDirectContext; out AImage: ISkImage; const AIsMipmapped: Boolean = False): Boolean;
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
    class procedure raster_release_proc(const pixels: Pointer; context: Pointer); cdecl; static;
    class procedure texture_release_proc(context: Pointer); cdecl; static;
  strict protected
    function Encode(const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes;
    procedure EncodeToFile(const AFileName: string; const AQuality: Integer = 80); overload;
    procedure EncodeToFile(const AFileName: string; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80); overload;
    procedure EncodeToStream(const AStream: TStream; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80);
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
    function MakeNonTextureImage(out AImage: ISkImage): Boolean;
    function MakeRasterImage(out AImage: ISkImage): Boolean;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ASampling: TSkSamplingOptions; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload;
    function MakeSubset(const ASubset: TRect; AContext: IGrDirectContext = nil): ISkImage;
    function MakeTextureImage(const AContext: IGrDirectContext; out AImage: ISkImage; const AIsMipmapped: Boolean = False): Boolean;
    function MakeWithFilter(const AFilter: ISkImageFilter; const ASubset, AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint; AContext: IGrDirectContext = nil): ISkImage;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow; AContext: IGrDirectContext = nil): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions; const ACachingHint: TSkImageCachingHint = TSkImageCachingHint.Allow): Boolean; overload;
  public
    class function MakeFromAdoptedTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil): ISkImage; static;
    class function MakeFromEncoded(const ABytes: TBytes): ISkImage; overload; static;
    class function MakeFromEncodedFile(const AFileName: string): ISkImage; overload; static;
    class function MakeFromEncodedStream(const AStream: TStream): ISkImage; overload; static;
    class function MakeFromRaster(const APixmap: ISkPixmap; const ARasterReleaseProc: TSkImageRasterReleaseProc = nil): ISkImage; overload; static;
    class function MakeFromRaster(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSkImageRasterReleaseProc = nil): ISkImage; overload; static; inline;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; AColorType: TSkColorType; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; const AColorSpace: ISkColorSpace = nil; const ATextureReleaseProc: TSkImageTextureReleaseProc = nil): ISkImage; static;
    class function MakeRasterCopy(const APixmap: ISkPixmap): ISkImage; overload; static;
    class function MakeRasterCopy(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage; overload; static; inline;
  end;

  { TSkImageEncoder }

  TSkImageEncoder = record
    class function Encode(const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes; overload; static;
    class function Encode(const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80): TBytes; overload; static; inline;
    class procedure EncodeToFile(const AFileName: string; const ASrc: ISkPixmap; const AQuality: Integer = 80); overload; static;
    class procedure EncodeToFile(const AFileName: string; const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80); overload; static;
    class procedure EncodeToFile(const AFileName: string; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AQuality: Integer = 80); overload; static;
    class procedure EncodeToFile(const AFileName: string; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer = 80); overload; static; inline;
    class procedure EncodeToStream(const AStream: TStream; const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80); overload; static;
    class procedure EncodeToStream(const AStream: TStream; const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer; const ASrcRowBytes: NativeUInt; const AEncodedImageFormat: TSkEncodedImageFormat = TSkEncodedImageFormat.PNG; const AQuality: Integer = 80); overload; static; inline;
  end;

  TSkColorChannel = (R, G, B, A);

  { ISkImageFilter }

  ISkImageFilter = interface(ISkReferenceCounted)
    ['{4EF1950D-0AAA-4D99-9E58-CD78809C55DB}']
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkImageFilter;
  end;

  { TSkImageFilter }

  TSkImageFilter = class(TSkReferenceCounted, ISkImageFilter)
  strict protected
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkImageFilter;
  public
    class function MakeAlphaThreshold(const ARegion: TRect; const AInnerMin, AOuterMax: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static; inline;
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
    class function MakeImage(const AImage: ISkImage): ISkImageFilter; overload; static; inline;
    class function MakeImage(const AImage: ISkImage; const ASampling: TSkSamplingOptions): ISkImageFilter; overload; static; inline;
    class function MakeImage(const AImage: ISkImage; const ASrc, ADest: TRectF): ISkImageFilter; overload; static; inline;
    class function MakeImage(const AImage: ISkImage; const ASrc, ADest: TRectF; const ASampling: TSkSamplingOptions): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMagnifier(const ASrc: TRectF; const AInset: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixConvolution(const AKernelSize: TSize; const AKernel: TArray<Single>; const AGain, ABias: Single; const AKernelOffset: TPoint; const ATileMode: TSkTileMode; const AConvolveAlpha: Boolean; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMatrixTransform(const AMatrix: TMatrix; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static; inline;
    class function MakeMatrixTransform(const AMatrix: TMatrix; const ASampling: TSkSamplingOptions; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter): ISkImageFilter; overload; static; inline;
    class function MakeMerge(const AFilter1, AFilter2: ISkImageFilter; const ACropRect: TRectF): ISkImageFilter; overload; static; inline;
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
    class function MakeShader(const AShader: ISkShader; const ADither: Boolean): ISkImageFilter; overload; static;
    class function MakeShader(const AShader: ISkShader; const ADither: Boolean; const ACropRect: TRectF): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitDiffuse(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeSpotLitSpecular(const ALocation, ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single; const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single; const ACropRect: TRectF; AInput: ISkImageFilter = nil): ISkImageFilter; overload; static;
    class function MakeTile(const ASrc, ADest: TRect; AInput: ISkImageFilter = nil): ISkImageFilter; static;
  end;

  TSkBlurStyle = (Normal, Solid, Outer, Inner);

  { ISkMaskFilter }

  ISkMaskFilter = interface(ISkReferenceCounted)
    ['{95610E43-0B23-411D-BA4B-D522FC724365}']
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

  ISkPathEffect = interface;

  TSkStrokeCap = (Butt, Round, Square);

  TSkStrokeJoin = (Miter, Round, Bevel);

  TSkPaintStyle = (Fill, Stroke, StrokeAndFill);

  { ISkPaint }

  ISkPaint = interface(ISkObject)
    ['{CE841D6E-DD17-488C-8A07-05F65E8DB03E}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APaint: ISkPaint); overload;
    constructor Create(const AStyle: TSkPaintStyle); overload;
  end;

  TSkPathOp = (Difference, Intersect, Union, &Xor, ReverseDifference);

  { ISkOpBuilder }

  ISkOpBuilder = interface(ISkObject)
    ['{A100B404-CA30-4A30-807E-6D641326816A}']
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
  end;

  { TSkOpBuilder }

  TSkOpBuilder = class(TSkObject, ISkOpBuilder)
  strict protected
    procedure Add(const APath: ISkPath; const AOp: TSkPathOp);
    function Detach: ISkPath;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkPathVerb = (Move, Line, Quad, Conic, Cubic, Close);

  TSkPathPoints = array[0..3] of TPointF;

  { TSkPathIteratorElem }

  TSkPathIteratorElem = record
    Verb: TSkPathVerb;
    Points: TSkPathPoints;
    ConicWeight: Single;
  end;

  TSkPathFillType = (Winding, EvenOdd, InverseWinding, InverseEvenOdd);

  { ISkPathIterator }

  ISkPathIterator = interface(ISkEnumerable<TSkPathIteratorElem>)
    ['{5FE88F86-9913-4594-B41D-4A4DA84CB54B}']
  end;

  TSkSegmentMask  = (Line, Quad, Conic, Cubic);
  TSkSegmentMasks = set of TSkSegmentMask;

  { ISkPath }

  ISkPath = interface(ISkObject)
    ['{0C027CE6-EDF6-4E6D-950C-FABA79C2251A}']
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
    strict protected
      function GetCurrent: TSkPathIteratorElem; override;
      function MoveNext: Boolean; override;
      class procedure DestroyHandle(const AHandle: THandle); override;
    public
      constructor Create(const APath: ISkPath; const AForceClose: Boolean);
    end;

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
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
    function ToSVG: string;
    function Transform(const AMatrix: TMatrix): ISkPath;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const ASVG: string); overload;
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(const AStream: TStream); overload;

    class function ConvertConicToQuads(const APoint1, APoint2, APoint3: TPointF; const AWeight: Single; const APower2: Integer): TArray<TPointF>; static;
  end;

  TSkPathDirection = (CW, CCW);

  TSkPathArcSize = (Small, Large);

  { ISkPathBuilder }

  ISkPathBuilder = interface(ISkObject)
    ['{9C587A80-9302-4FAB-86A9-4F068607F74E}']
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
  strict protected
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
    procedure IncReserve(const AExtraPointCount, AExtraVerbCount: Integer); overload;
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const APathBuilder: ISkPathBuilder); overload;
    constructor Create(const AFillType: TSkPathFillType); overload;
  end;

  TSkPathEffect1DStyle = (Translate, Rotate, Morph);

  TSkPathEffectTrimMode = (Normal, Inverted);

  { ISkPathEffect }

  ISkPathEffect = interface(ISkReferenceCounted)
    ['{FBC6D035-F485-4E05-8891-AED313592692}']
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

  TSkPathMeasureMatrixFlag  = (Position, Tangent);
  TSkPathMeasureMatrixFlags = set of TSkPathMeasureMatrixFlag;

  { ISkPathMeasure }

  ISkPathMeasure = interface(ISkObject)
    ['{6D963FCB-E879-499F-AF27-9A92904B66B6}']
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const APath: ISkPath; const AForceClosed: Boolean = False; const AResScale: Single = 1);
  end;

  { ISkPicture }

  ISkPicture = interface(ISkReferenceCounted)
    ['{12564CCD-119B-473F-8585-56D2A1F9DB43}']
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
  strict protected
    function GetCullRect: TRectF;
    function MakeShader(const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    function MakeShader(const ATileRect: TRectF; const ALocalMatrix: TMatrix; const ATileModeX: TSkTileMode = TSkTileMode.Clamp; const ATileModeY: TSkTileMode = TSkTileMode.Clamp; const AFilterMode: TSkFilterMode = TSkFilterMode.Linear): ISkShader; overload;
    procedure Playback(const ACanvas: ISkCanvas);
    function Serialize: TBytes;
    procedure SerializeToStream(const AStream: TStream);
  public
    class function MakeFromBytes(const ABytes: TBytes): ISkPicture; static;
    class function MakeFromStream(const AStream: TStream): ISkPicture; static;
  end;

  { ISkPictureRecorder }

  ISkPictureRecorder = interface(ISkObject)
    ['{A6EB80CE-B348-4B65-9C5D-770FBA557F1E}']
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
  end;

  { TSkPictureRecorder }

  TSkPictureRecorder = class(TSkObject, ISkPictureRecorder)
  strict protected
    function BeginRecording(const AWidth, AHeight: Single): ISkCanvas; overload;
    function BeginRecording(const ABounds: TRectF): ISkCanvas; overload;
    function FinishRecording: ISkPicture; overload;
    function FinishRecording(const ACullRect: TRectF): ISkPicture; overload;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { ISkPixmap }

  ISkPixmap = interface(ISkObject)
    ['{296CE950-B7CC-4510-A877-1C86CD0AD0B1}']
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
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
    function ScalePixels(const ADest: ISkPixmap): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions): Boolean; overload;
    procedure SetColorSpace(AValue: ISkColorSpace);
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
  strict protected
    function Erase(const AColor: TAlphaColor): Boolean; overload;
    function Erase(const AColor: TAlphaColor; const ASubset: TRectF): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
    function Erase(const AColor: TAlphaColorF; const ASubset: TRectF; AColorSpace: ISkColorSpace = nil): Boolean; overload;
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
    function ScalePixels(const ADest: ISkPixmap): Boolean; overload;
    function ScalePixels(const ADest: ISkPixmap; const ASampling: TSkSamplingOptions): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean; overload;
    function ScalePixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASampling: TSkSamplingOptions): Boolean; overload;
    procedure SetColorSpace(AValue: ISkColorSpace);
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt);
  end;

  { ISkRegionCliperator }

  ISkRegionCliperator = interface(ISkEnumerable<TRect>)
    ['{344C7AA5-6D8C-4EC3-AA0E-AAB9C05C2D17}']
  end;

  { ISkRegionIterator }

  ISkRegionIterator = interface(ISkEnumerable<TRect>)
    ['{39EC8853-79A8-4428-B924-9945C3777150}']
  end;

  { ISkRegionSpanerator }

  ISkRegionSpanerator = interface(ISkEnumerable<TPoint>)
    ['{87F952A9-C5AE-4013-A312-046C8F3B19D7}']
  end;

  TSkRegionOp = (Difference, Intersect, Union, &Xor, ReverseDifference, Replace);

  { ISkRegion }

  ISkRegion = interface(ISkObject)
    ['{B26070A8-22D1-4FED-9F04-9D84F1133E56}']
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
    strict protected
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      class procedure DestroyHandle(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion; const AClip: TRect);
    end;

    TRegionIterator = class(TSkEnumerable<TRect>, ISkRegionIterator)
    strict protected
      function GetCurrent: TRect; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
      class procedure DestroyHandle(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion);
    end;

    TRegionSpanerator = class(TSkEnumerable<TPoint>, ISkRegionSpanerator)
    strict private
      FCurrent: TPoint;
    strict protected
      function GetCurrent: TPoint; override;
      function MoveNext: Boolean; override;
      class procedure DestroyHandle(const AHandle: THandle); override;
    public
      constructor Create(const ARegion: ISkRegion; const AY, ALeft, ARight: Integer);
    end;

  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARegion: ISkRegion); overload;
    constructor Create(const ARect: TRect); overload;
  end;

  TSkRoundRectCorner = (UpperLeft, UpperRight, LowerRight, LowerLeft);

  TSkRoundRectRadii = array[TSkRoundRectCorner] of Single;

  { ISkRoundRect }

  ISkRoundRect = interface(ISkObject)
    ['{574DC656-4D76-4576-B7E2-E7A27D73B01F}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const ARoundRect: ISkRoundRect); overload;
    constructor Create(const ARect: TRectF; const ARadiusX, ARadiusY: Single); overload;
    constructor Create(const ARect: TRectF; const ARadii: TSkRoundRectRadii); overload;
  end;

  { TSkRuntimeEffectFloat2 }

  TSkRuntimeEffectFloat2 = packed record
    constructor Create(const AV1, AV2: Single);
    class operator Implicit(const APoint: TPointF): TSkRuntimeEffectFloat2;
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
    class operator Implicit(const AColor: TAlphaColorF): TSkRuntimeEffectFloat4;
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
    class operator Implicit(const APoint: TPoint): TSkRuntimeEffectInt2;
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

  TSkRuntimeEffectChildType = (Shader, ColorFilter, Blender);

  TSkRuntimeEffectUniformType = (Float, Float2, Float3, Float4, Float2x2, Float3x3, Float4x4, Int, Int2, Int3, Int4);

  { ISkRuntimeEffect }

  ISkRuntimeEffect = interface(ISkReferenceCounted)
    ['{E9EEAD6A-2B77-4797-AEC9-85C01965F0B6}']
    function ChildExists(const AName: string): Boolean;
    function GetChildColorFilter(const AIndex: Integer): ISkColorFilter; overload;
    function GetChildColorFilter(const AName: string): ISkColorFilter; overload;
    function GetChildCount: Integer;
    function GetChildName(const AIndex: Integer): string;
    function GetChildBlender(const AIndex: Integer): ISkBlender; overload;
    function GetChildBlender(const AName: string): ISkBlender; overload;
    function GetChildShader(const AIndex: Integer): ISkShader; overload;
    function GetChildShader(const AName: string): ISkShader; overload;
    function GetChildType(const AIndex: Integer): TSkRuntimeEffectChildType; overload;
    function GetChildType(const AName: string): TSkRuntimeEffectChildType; overload;
    function GetUniform(const AIndex: Integer): Pointer; overload;
    function GetUniform(const AName: string): Pointer; overload;
    function GetUniformCount: Integer;
    function GetUniformData: Pointer;
    function GetUniformDataSize: NativeUInt;
    function GetUniformName(const AIndex: Integer): string;
    function GetUniformOffset(const AIndex: Integer): NativeUInt; overload;
    function GetUniformOffset(const AName: string): NativeUInt; overload;
    function GetUniformType(const AIndex: Integer): TSkRuntimeEffectUniformType; overload;
    function GetUniformType(const AName: string): TSkRuntimeEffectUniformType; overload;
    function GetUniformTypeCount(const AIndex: Integer): Integer; overload;
    function GetUniformTypeCount(const AName: string): Integer; overload;
    function IndexOfChild(const AName: string): Integer;
    function IndexOfUniform(const AName: string): Integer;
    function IsUniformTypeOrdinal(const AIndex: Integer): Boolean; overload;
    function IsUniformTypeOrdinal(const AName: string): Boolean; overload;
    function MakeBlender: ISkBlender;
    function MakeColorFilter: ISkColorFilter;
    function MakeShader(const AOpaque: Boolean = False): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const AOpaque: Boolean = False): ISkShader; overload;
    procedure SetChildBlender(const AIndex: Integer; const AValue: ISkBlender); overload;
    procedure SetChildBlender(const AName: string; const AValue: ISkBlender); overload;
    procedure SetChildColorFilter(const AIndex: Integer; const AValue: ISkColorFilter); overload;
    procedure SetChildColorFilter(const AName: string; const AValue: ISkColorFilter); overload;
    procedure SetChildShader(const AIndex: Integer; const AValue: ISkShader); overload;
    procedure SetChildShader(const AName: string; const AValue: ISkShader); overload;
    procedure SetUniform(const AIndex: Integer; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: Integer); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: Single); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat4x4); overload;
    procedure SetUniform(const AName: string; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AName: string; const AValue: Integer); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AName: string; const AValue: Single); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4x4); overload;
    function UniformExists(const AName: string): Boolean;
    procedure WriteUniform(const AOffset: NativeUInt; const AData; const ASize: NativeUInt);
    property ChildCount: Integer read GetChildCount;
    property ChildrenBlenders[const AIndex: Integer]: ISkBlender read GetChildBlender write SetChildBlender;
    property ChildrenBlenders[const AName: string]: ISkBlender read GetChildBlender write SetChildBlender;
    property ChildrenColorFilters[const AIndex: Integer]: ISkColorFilter read GetChildColorFilter write SetChildColorFilter;
    property ChildrenColorFilters[const AName: string]: ISkColorFilter read GetChildColorFilter write SetChildColorFilter;
    property ChildrenNames[const AIndex: Integer]: string read GetChildName;
    property ChildrenShaders[const AIndex: Integer]: ISkShader read GetChildShader write SetChildShader;
    property ChildrenShaders[const AName: string]: ISkShader read GetChildShader write SetChildShader;
    property ChildType[const AIndex: Integer]: TSkRuntimeEffectChildType read GetChildType;
    property ChildType[const AName: string]: TSkRuntimeEffectChildType read GetChildType;
    property UniformCount: Integer read GetUniformCount;
    property UniformData: Pointer read GetUniformData;
    property UniformDataSize: NativeUInt read GetUniformDataSize;
    property UniformNames[const AIndex: Integer]: string read GetUniformName;
    property UniformOffset[const AIndex: Integer]: NativeUInt read GetUniformOffset;
    property UniformOffset[const AName: string]: NativeUInt read GetUniformOffset;
    property Uniforms[const AIndex: Integer]: Pointer read GetUniform;
    property Uniforms[const AName: string]: Pointer read GetUniform;
    property UniformType[const AIndex: Integer]: TSkRuntimeEffectUniformType read GetUniformType;
    property UniformType[const AName: string]: TSkRuntimeEffectUniformType read GetUniformType;
    property UniformTypeCount[const AIndex: Integer]: Integer read GetUniformTypeCount;
    property UniformTypeCount[const AName: string]: Integer read GetUniformTypeCount;
  end;

  { TSkRuntimeEffect }

  TSkRuntimeEffect = class(TSkReferenceCounted, ISkRuntimeEffect)
  strict private
    FChildren: TArray<ISkNativeObject>;
    FUniformData: Pointer;
  strict protected
    function ChildExists(const AName: string): Boolean;
    function GetChildColorFilter(const AIndex: Integer): ISkColorFilter; overload;
    function GetChildColorFilter(const AName: string): ISkColorFilter; overload;
    function GetChildCount: Integer; inline;
    function GetChildName(const AIndex: Integer): string;
    function GetChildBlender(const AIndex: Integer): ISkBlender; overload;
    function GetChildBlender(const AName: string): ISkBlender; overload;
    function GetChildShader(const AIndex: Integer): ISkShader; overload;
    function GetChildShader(const AName: string): ISkShader; overload;
    function GetChildType(const AIndex: Integer): TSkRuntimeEffectChildType; overload;
    function GetChildType(const AName: string): TSkRuntimeEffectChildType; overload;
    function GetUniform(const AIndex: Integer): Pointer; overload;
    function GetUniform(const AName: string): Pointer; overload;
    function GetUniformCount: Integer; inline;
    function GetUniformData: Pointer; inline;
    function GetUniformDataSize: NativeUInt; inline;
    function GetUniformName(const AIndex: Integer): string;
    function GetUniformOffset(const AIndex: Integer): NativeUInt; overload;
    function GetUniformOffset(const AName: string): NativeUInt; overload;
    function GetUniformType(const AIndex: Integer): TSkRuntimeEffectUniformType; overload;
    function GetUniformType(const AName: string): TSkRuntimeEffectUniformType; overload;
    function GetUniformTypeCount(const AIndex: Integer): Integer; overload;
    function GetUniformTypeCount(const AName: string): Integer; overload;
    function IndexOfChild(const AName: string): Integer; inline;
    function IndexOfUniform(const AName: string): Integer; inline;
    function IsUniformTypeOrdinal(const AIndex: Integer): Boolean; overload; inline;
    function IsUniformTypeOrdinal(const AName: string): Boolean; overload;
    function MakeBlender: ISkBlender;
    function MakeColorFilter: ISkColorFilter;
    function MakeShader(const AOpaque: Boolean = False): ISkShader; overload;
    function MakeShader(const ALocalMatrix: TMatrix; const AOpaque: Boolean = False): ISkShader; overload;
    procedure SetChildBlender(const AIndex: Integer; const AValue: ISkBlender); overload;
    procedure SetChildBlender(const AName: string; const AValue: ISkBlender); overload;
    procedure SetChildColorFilter(const AIndex: Integer; const AValue: ISkColorFilter); overload;
    procedure SetChildColorFilter(const AName: string; const AValue: ISkColorFilter); overload;
    procedure SetChildShader(const AIndex: Integer; const AValue: ISkShader); overload;
    procedure SetChildShader(const AName: string; const AValue: ISkShader); overload;
    procedure SetUniform(const AIndex: Integer; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: Integer); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: Single); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AIndex: Integer; const AValue: TSkRuntimeEffectFloat4x4); overload;
    procedure SetUniform(const AName: string; const AData; const ASize: NativeUInt); overload;
    procedure SetUniform(const AName: string; const AValue: Integer); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Integer>); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectInt4); overload;
    procedure SetUniform(const AName: string; const AValue: Single); overload;
    procedure SetUniform(const AName: string; const AValue: TArray<Single>); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat2x2); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat3x3); overload;
    procedure SetUniform(const AName: string; const AValue: TSkRuntimeEffectFloat4x4); overload;
    function UniformExists(const AName: string): Boolean;
    procedure WriteUniform(const AOffset: NativeUInt; const AData; const ASize: NativeUInt);
  public
    constructor Wrap(const AHandle: THandle; const AOwnsHandle: Boolean = True); override;
    destructor Destroy; override;
    class function MakeForBlender(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForBlender(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForColorFilter(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: string): ISkRuntimeEffect; overload; static;
    class function MakeForShader(const ASkSL: string; out AErrorText: string): ISkRuntimeEffect; overload; static;
  end;

  { ISkShader }

  ISkShader = interface(ISkReferenceCounted)
    ['{8453635B-9DE2-4E03-88F7-8EA16FA99620}']
    function MakeWithColorFilter(const AColorFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
  end;

  { TSkShader }

  TSkShader = class(TSkReferenceCounted, ISkShader)
  strict protected
    function MakeWithColorFilter(const AColorFilter: ISkColorFilter): ISkShader;
    function MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
  public
    class function MakeBlend(const AMode: TSkBlendMode; const ADest, ASrc: ISkShader): ISkShader; static;
    class function MakeColor(const AColor: TAlphaColor): ISkShader; overload; static;
    class function MakeColor(const AColor: TAlphaColorF; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientLinear(const AStart, AEnd: TPointF; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const ACenterColor, AEdgeColor: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientRadial(const ACenter: TPointF; const ARadius: Single; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static; inline;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static; inline;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientSweep(const ACenter: TPointF; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientSweep(const ACenter: TPointF; const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix; const APositions: TArray<Single> = nil; const ATileMode: TSkTileMode = TSkTileMode.Clamp; const AStartAngle: Single = 0; const AEndAngle: Single = 360; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColor; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColor; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp): ISkShader; overload; static; inline;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColorF; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
    class function MakeGradientTwoPointConical(const AStart: TPointF; const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single; const AColor1, AColor2: TAlphaColorF; const ALocalMatrix: TMatrix; const ATileMode: TSkTileMode = TSkTileMode.Clamp; AColorSpace: ISkColorSpace = nil): ISkShader; overload; static; inline;
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

  TSkSurfacePropertiesFlag  = (UseDeviceIndependentFonts, DynamicMSAA);
  TSkSurfacePropertiesFlags = set of TSkSurfacePropertiesFlag;

  TSkPixelGeometry = (Unknown, RGBHorizontal, BGRHorizontal, RGBVertical, BGRVertical);

  { TSkSurfaceProperties }

  TSkSurfaceProperties = record
    Flags: TSkSurfacePropertiesFlags;
    PixelGeometry: TSkPixelGeometry;
    constructor Create(const AFlags: TSkSurfacePropertiesFlags; const APixelGeometry: TSkPixelGeometry);
    class operator Equal(const ASurfaceProperties1, ASurfaceProperties2: TSkSurfaceProperties): Boolean;
    class operator NotEqual(const ASurfaceProperties1, ASurfaceProperties2: TSkSurfaceProperties): Boolean;
  end;

  TSkSurfaceRasterReleaseProc = reference to procedure (const APixels: Pointer);

  { ISkSurface }

  ISkSurface = interface(ISkReferenceCounted)
    ['{8E740BAA-320F-494C-939C-5982E0643DBB}']
    function GetCanvas: ISkCanvas;
    function GetProperties: TSkSurfaceProperties;
    function MakeImageSnapshot: ISkImage; overload;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
    property Canvas: ISkCanvas read GetCanvas;
    property Properties: TSkSurfaceProperties read GetProperties;
  end;

  { TSkSurface }

  TSkSurface = class(TSkReferenceCounted, ISkSurface)
  strict private
    FCanvasHolder: ISkCanvas;
    class procedure raster_release_proc(pixels: Pointer; context: Pointer); cdecl; static;
  strict protected
    function GetCanvas: ISkCanvas;
    function GetProperties: TSkSurfaceProperties;
    function MakeImageSnapshot: ISkImage; overload;
    function MakeImageSnapshot(const ABounds: TRect): ISkImage; overload;
    function PeekPixels: ISkPixmap;
    function ReadPixels(const ADest: ISkPixmap; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    function ReadPixels(const ADestImageInfo: TSkImageInfo; const ADestPixels: Pointer; const ADestRowBytes: NativeUInt; const ASrcX: Integer = 0; const ASrcY: Integer = 0): Boolean; overload;
    procedure Draw(const ACanvas: ISkCanvas; const AX, AY: Single; const APaint: ISkPaint = nil);
    procedure Flush;
    procedure FlushAndSubmit(const ASyncCPU: Boolean = False);
  public
    class function MakeFromCAMetalLayer(const AContext: IGrDirectContext; const ALayer: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; out ADrawable: GrMtlHandle; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromCAMetalLayer(const AContext: IGrDirectContext; const ALayer: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; out ADrawable: GrMtlHandle; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromMTKView(const AContext: IGrDirectContext; const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromMTKView(const AContext: IGrDirectContext; const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromRenderTarget(const AContext: IGrDirectContext; const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromRenderTarget(const AContext: IGrDirectContext; const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeFromTexture(const AContext: IGrDirectContext; const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin; const ASampleCount: Integer; const AColorType: TSkColorType; const AProperties: TSkSurfaceProperties; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static;
    class function MakeRaster(const AWidth, AHeight: Integer; const AColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF}; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static; inline;
    class function MakeRaster(const AWidth, AHeight: Integer; const AProperties: TSkSurfaceProperties; const AColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF}; const AAlphaType: TSkAlphaType = TSkAlphaType.Premul; AColorSpace: ISkColorSpace = nil): ISkSurface; overload; static; inline;
    class function MakeRaster(const AImageInfo: TSkImageInfo): ISkSurface; overload; static; inline;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties): ISkSurface; overload; static; inline;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const ARowBytes: NativeUInt): ISkSurface; overload; static;
    class function MakeRaster(const AImageInfo: TSkImageInfo; const ARowBytes: NativeUInt; const AProperties: TSkSurfaceProperties): ISkSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISkPixmap; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const APixmap: ISkPixmap; const AProperties: TSkSurfaceProperties; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRasterDirect(const AImageInfo: TSkImageInfo; const APixels: Pointer; const ARowBytes: NativeUInt; const AProperties: TSkSurfaceProperties; const ARasterReleaseProc: TSkSurfaceRasterReleaseProc = nil): ISkSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGrDirectContext; const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo; const ASampleCount: Integer = 0; const AOrigin: TGrSurfaceOrigin = TGrSurfaceOrigin.BottomLeft; const AShouldCreateWithMips: Boolean = False): ISkSurface; overload; static;
    class function MakeRenderTarget(const AContext: IGrDirectContext; const AIsBudgeted: Boolean; const AImageInfo: TSkImageInfo; const AProperties: TSkSurfaceProperties; const ASampleCount: Integer = 0; const AOrigin: TGrSurfaceOrigin = TGrSurfaceOrigin.BottomLeft; const AShouldCreateWithMips: Boolean = False): ISkSurface; overload; static;
  end;

  TSkSVGCanvasFlag  = (ConvertTextToPaths, NoPrettyXML, RelativePathEncoding);
  TSkSVGCanvasFlags = set of TSkSVGCanvasFlag;

  { TSkSVGCanvas }

  TSkSVGCanvas = record
     class function Make(const ABounds: TRectF; const AStream: TStream; const AFlags: TSkSVGCanvasFlags = []): ISkCanvas; static;
  end;

  { ISkTextBlob }

  ISkTextBlob = interface(ISkNonVirtualReferenceCounted)
    ['{43D18069-AEA6-47E1-8EA2-1BE0265D75C2}']
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
  end;

  { TSkTextBlob }

  TSkTextBlob = class(TSkNonVirtualReferenceCounted, ISkTextBlob)
  strict protected
    function GetIntercepts(const AUpperBounds, ALowerBounds: Single; const APaint: ISkPaint = nil): TArray<Single>;
    class procedure RefHandle(const AHandle: THandle); override;
    class procedure UnrefHandle(const AHandle: THandle); override;
  public
    class function MakeFromText(const AText: string; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextHorizontallyPositioned(const AText: string; const AXPositions: TArray<Single>; const AY: Single; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextPositioned(const AText: string; const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob; static;
    class function MakeFromTextTransform(const AText: string; const AMatrices: TArray<TSkRotationScaleMatrix>; const AFont: ISkFont): ISkTextBlob; static;
  end;

  { ISkTraceMemoryDump }

  ISkTraceMemoryDump = interface(ISkObject)
    ['{96A1FCB6-246E-4E49-958C-95897319FA57}']
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const ADetailedDump, ADumpWrappedObjects: Boolean);
  end;

  TSkFontWeight = (
    Invisible  =    0,
    Thin       =  100,
    ExtraLight =  200,
    Light      =  300,
    Normal     =  400,
    Medium     =  500,
    SemiBold   =  600,
    Bold       =  700,
    ExtraBold  =  800,
    Black      =  900,
    ExtraBlack = 1000
  );

  { TSkFontWeightConstants }

  TSkFontWeightConstants = record helper for TSkFontWeight
  const
    Hairline   = TSkFontWeight.Thin;
    UltraLight = TSkFontWeight.ExtraLight;
    Regular    = TSkFontWeight.Normal;
    DemiBold   = TSkFontWeight.SemiBold;
    UltraBold  = TSkFontWeight.ExtraBold;
    Heavy      = TSkFontWeight.Black;
    UltraBlack = TSkFontWeight.ExtraBlack;
  end;

  TSkFontWidth = (
    UltraCondensed = 1,
    ExtraCondensed = 2,
    Condensed      = 3,
    SemiCondensed  = 4,
    Normal         = 5,
    SemiExpanded   = 6,
    Expanded       = 7,
    ExtraExpanded  = 8,
    UltraExpanded  = 9
  );

  { TSkFontWidthConstants }

  TSkFontWidthConstants = record helper for TSkFontWidth
  const
    Regular = TSkFontWidth.Normal;
  end;

  TSkFontSlant = (Upright, Italic, Oblique);

  { TSkFontSlantConstants }

  TSkFontSlantConstants = record helper for TSkFontSlant
  const
    Regular = TSkFontSlant.Upright;
  end;

  { TSkFontStyle }

  TSkFontStyle = record
    Weight: Integer;
    Width: Integer;
    Slant: TSkFontSlant;
    constructor Create(const AWeight, AWidth: Integer; const ASlant: TSkFontSlant); overload;
    constructor Create(const AWeight: TSkFontWeight; const AWidth: TSkFontWidth; const ASlant: TSkFontSlant); overload;
    class function Normal: TSkFontStyle; static; inline;
    class function Bold: TSkFontStyle; static; inline;
    class function Italic: TSkFontStyle; static; inline;
    class function BoldItalic: TSkFontStyle; static; inline;
    class operator Equal(const AFontStyle1, AFontStyle2: TSkFontStyle): Boolean;
    class operator NotEqual(const AFontStyle1, AFontStyle2: TSkFontStyle): Boolean;
  end;

  { TSkFontStyleConstants }

  TSkFontStyleConstants = record helper for TSkFontStyle
  const
    Normal     : TSkFontStyle = (Weight: Ord(TSkFontWeight.Normal); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Upright);
    Bold       : TSkFontStyle = (Weight: Ord(TSkFontWeight.Bold); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Upright);
    Italic     : TSkFontStyle = (Weight: Ord(TSkFontWeight.Normal); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Italic);
    BoldItalic : TSkFontStyle = (Weight: Ord(TSkFontWeight.Bold); Width: Ord(TSkFontWidth.Normal); Slant: TSkFontSlant.Italic);
  end;

  { ISkTypeface }

  ISkTypeface = interface(ISkReferenceCounted)
    ['{DD3BF5F5-2090-4753-8FCE-336AA93D3CA1}']
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
  strict protected
    function GetFamilyName: string;
    function GetSlant: TSkFontSlant;
    function GetStyle: TSkFontStyle;
    function GetWeight: Integer;
    function GetWidth: Integer;
    function IsBold: Boolean;
    function IsItalic: Boolean;
  public
    class function MakeDefault: ISkTypeface; static;
    class function MakeFromFile(const AFileName: string; const ATTCIndex: Integer = 0): ISkTypeface; static;
    class function MakeFromName(const AFamilyName: string; const AStyle: TSkFontStyle): ISkTypeface; static;
    class function MakeFromStream(const AStream: TStream; const ATTCIndex: Integer = 0): ISkTypeface; static;
  end;

  { TSkVersion }

  TSkVersion = record
  strict private
    class function GetMajor: Integer; static;
    class function GetMinor: Integer; static;
    class function GetBuild: Integer; static;
    class function GetMilestone: Integer; static;
  public
    class property Major: Integer read GetMajor;
    class property Minor: Integer read GetMinor;
    class property Build: Integer read GetBuild;
    class property Milestone: Integer read GetMilestone;
  end;

  TSkVertexMode = (Triangles, TriangleStrip, TriangleFan);

  { ISkVertices }

  ISkVertices = interface(ISkNonVirtualReferenceCounted)
    ['{C0EDA7D9-C6E2-449D-9E51-4C5B07301D40}']
  end;

  { TSkVertices }

  TSkVertices = class(TSkNonVirtualReferenceCounted, ISkVertices)
  strict protected
    class procedure RefHandle(const AHandle: THandle); override;
    class procedure UnrefHandle(const AHandle: THandle); override;
  public
    class function MakeCopy(const AVertexMode: TSkVertexMode; const APositions, ATextures: TArray<TPointF>; const AColors: TArray<TAlphaColor>; const AIndices: TArray<Word> = nil): ISkVertices; static;
  end;

  { TSkParticleUniform }

  TSkParticleUniform = record
    Columns: Integer;
    Rows: Integer;
    Slot: Integer;
  end;

  TSkParticleUniformData = array[0..0] of Single;
  PSkParticleUniformData = ^TSkParticleUniformData;

  ISkResourceProvider = interface;

  { ISkParticleEffect }

  ISkParticleEffect = interface(ISkReferenceCounted)
    ['{9BDF58BE-1ECF-4B18-B538-68A190E4C8AF}']
    function GetPosition: TPointF;
    function GetRate: Single;
    function GetUniform(const AIndex: NativeUInt): TSkParticleUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformData: PSkParticleUniformData;
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
    property UniformData: PSkParticleUniformData read GetUniformData;
    property UniformDataCount: Integer read GetUniformDataCount;
    property UniformName[const AIndex: NativeUInt]: string read GetUniformName;
  end;

  { TSkParticleEffect }

  TSkParticleEffect = class(TSkReferenceCounted, ISkParticleEffect)
  strict private
    class constructor Create;
  strict protected
    function GetPosition: TPointF;
    function GetRate: Single;
    function GetUniform(const AIndex: NativeUInt): TSkParticleUniform;
    function GetUniformCount: NativeUInt;
    function GetUniformData: PSkParticleUniformData;
    function GetUniformDataCount: Integer;
    function GetUniformName(const AIndex: NativeUInt): string;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetPosition(const AValue: TPointF);
    procedure SetRate(const AValue: Single);
    function SetUniform(const AName: string; const AData: TArray<Single>): Boolean;
    procedure Start(const ANow: Double; const ALooping: Boolean);
    procedure Update(const ANow: Double);
  public
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil): ISkParticleEffect; static;
    class function MakeFromFile(const AFileName: string): ISkParticleEffect; static;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil): ISkParticleEffect; static;
  end;

  TSkottieAnimationRenderFlag  = (SkipTopLevelIsolation, DisableTopLevelClipping);
  TSkottieAnimationRenderFlags = set of TSkottieAnimationRenderFlag;

  { ISkottieAnimation }

  ISkottieAnimation = interface(ISkNonVirtualReferenceCounted)
    ['{E3381710-6350-4A2B-8DA8-E71EA31E8E71}']
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
    class procedure RefHandle(const AHandle: THandle); override;
    class procedure UnrefHandle(const AHandle: THandle); override;
  public
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil): ISkottieAnimation; static;
    class function MakeFromFile(const AFileName: string): ISkottieAnimation; static;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil): ISkottieAnimation; static;
  end;

  TSkAffinity = (Upstream, Downstream);

  { TSkPositionAffinity }

  TSkPositionAffinity = record
    Position: Integer;
    Affinity: TSkAffinity;
    class operator Equal(const APositionAffinity1, APositionAffinity2: TSkPositionAffinity): Boolean;
    class operator NotEqual(const APositionAffinity1, APositionAffinity2: TSkPositionAffinity): Boolean;
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

  TSkTextDirection = (RightToLeft, LeftToRight);

  { TSkTextBox }

  TSkTextBox = record
    Rect: TRectF;
    Direction: TSkTextDirection;
    class operator Equal(const ATextBox1, ATextBox2: TSkTextBox): Boolean;
    class operator NotEqual(const ATextBox1, ATextBox2: TSkTextBox): Boolean;
  end;

  TSkRectHeightStyle = (Tight, Max, IncludeLineSpacingMiddle, IncludeLineSpacingTop, IncludeLineSpacingBottom, Strut);

  TSkRectWidthStyle = (Tight, Max);

  { ISkParagraph }

  ISkParagraph = interface(ISkObject)
    ['{27462359-56C6-41DF-9D15-840E1DEC25C9}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  end;

  TSkPlaceholderAlignment = (Baseline, AboveBaseline, BelowBaseline, Top, Bottom, Middle);

  TSkTextBaseline = (Alphabetic, Ideographic);

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

  ISkTextStyle = interface;

  ISkParagraphStyle = interface;

  ISkTypefaceFontProvider = interface;

  { ISkParagraphBuilder }

  ISkParagraphBuilder = interface(ISkObject)
    ['{8214ABB8-CAB4-462C-A855-868230F35EE8}']
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Build: ISkParagraph;
    procedure Pop;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
  end;

  { TSkParagraphBuilder }

  TSkParagraphBuilder = class(TSkObject, ISkParagraphBuilder)
  strict protected
    procedure AddPlaceholder(const APlaceholder: TSkPlaceholderStyle);
    procedure AddText(const AText: string);
    function Build: ISkParagraph;
    procedure Pop;
    procedure PushStyle(const ATextStyle: ISkTextStyle);
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const AParagraphStyle: ISkParagraphStyle); overload;
    constructor Create(const AParagraphStyle: ISkParagraphStyle; const AFontProvider: ISkTypefaceFontProvider; const AEnableFontFallback: Boolean = True); overload;
  end;

  { ISkStrutStyle }

  ISkStrutStyle = interface(ISkObject)
    ['{6A3B8422-7960-437C-B074-2719909566FF}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkTextAlign = (Left, Right, Center, Justify, Start, Terminate);

  TSkTextHeightBehavior  = (DisableFirstAscent, DisableLastDescent);
  TSkTextHeightBehaviors = set of TSkTextHeightBehavior;

  { ISkParagraphStyle }

  ISkParagraphStyle = interface(ISkObject)
    ['{79C3E735-F3D1-4276-A8B7-BC0CF422A543}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
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

  TSkTextDecoration  = (Underline, Overline, LineThrough);
  TSkTextDecorations = set of TSkTextDecoration;

  TSkTextDecorationStyle = (Solid, Double, Dotted, Dashed, Wavy);

  { ISkTextStyle }

  ISkTextStyle = interface(ISkObject)
    ['{C7482800-C941-42C3-AAE9-6D59E6D9BE3A}']
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
  strict protected
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
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  { ISkTypefaceFontProvider }

  ISkTypefaceFontProvider = interface(ISkReferenceCounted)
    ['{6761FD5D-F04D-4C4F-87EE-CBA82B9544F1}']
    procedure RegisterTypeface(const ATypeface: ISkTypeface); overload;
    procedure RegisterTypeface(const ATypeface: ISkTypeface; const AFamilyName: string); overload;
  end;

  { TSkTypefaceFontProvider }

  TSkTypefaceFontProvider = class(TSkReferenceCounted, ISkTypefaceFontProvider)
  strict protected
    procedure RegisterTypeface(const ATypeface: ISkTypeface); overload;
    procedure RegisterTypeface(const ATypeface: ISkTypeface; const AFamilyName: string); overload;
  public
    constructor Create;
  end;

  { ISkResourceProvider }

  ISkResourceProvider = interface(ISkReferenceCounted)
    ['{A2C2EEC5-57E8-4141-817D-356F3084E7B1}']
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
    ['{D0654C50-A719-4ED9-8B9D-F57E2A644E13}']
  end;

  { TSkFileResourceProvider }

  TSkFileResourceProvider = class(TSkResourceProviderBaseClass, ISkFileResourceProvider)
  strict private
    FBaseDir: string;
  strict protected
    function Load(const APath, AName: string): TBytes; override;
  public
    constructor Create(const APredecode: Boolean = False); overload;
    constructor Create(const ABaseDir: string; const APredecode: Boolean = False); overload;
  end;

  { ISkShaper }

  ISkShaper = interface(ISkObject)
    ['{29BF4EF9-8F70-4456-8ABB-4EDCF9075CDB}']
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob; overload;
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF; out AEndPoint: TPointF): ISkTextBlob; overload;
  end;

  { TSkShaper }

  TSkShaper = class(TSkObject, ISkShaper)
  strict protected
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob; overload;
    function Shape(const AText: string; const AFont: ISkFont; const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF; out AEndPoint: TPointF): ISkTextBlob; overload;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  TSkDirection = (LeftToRight, RightToLeft);

  TSkUnicodeBidiRegionProc = reference to procedure (const AStart, AEnd: Integer; const ALevel: Byte);

  TSkBreakType = (Words, Graphemes, Lines);

  TSkUnicodeBreakProc = reference to procedure (const APosition, AStatus: Integer);

  TSkUnicodeCodepointProc = reference to procedure (const AUnichar, AStart, AEnd: Integer);

  { ISkUnicode }

  ISkUnicode = interface(ISkObject)
    ['{DA2335ED-0665-44C2-B305-2B38FDEE8EB9}']
    procedure ForEachBidiRegion(const AText: string; const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
    procedure ForEachBreak(const AText: string; const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
    procedure ForEachCodepoint(const AText: string; const AProc: TSkUnicodeCodepointProc);
    function GetBreaks(const AText: string; const AType: TSkBreakType): TArray<string>;
  end;

  { TSkUnicode }

  TSkUnicode = class(TSkObject, ISkUnicode)
  strict private
    class procedure bidi_region_proc(start, &end: int32_t; level: uint8_t; context: Pointer); cdecl; static;
    class procedure break_proc(position, status: int32_t; context: Pointer); cdecl; static;
    class procedure codepoint_proc(unichar: sk_unichar_t; start, &end: int32_t; context: Pointer); cdecl; static;
  strict protected
    procedure ForEachBidiRegion(const AText: string; const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
    procedure ForEachBreak(const AText: string; const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
    procedure ForEachCodepoint(const AText: string; const AProc: TSkUnicodeCodepointProc);
    function GetBreaks(const AText: string; const AType: TSkBreakType): TArray<string>;
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create;
  end;

  ISkSVGNode = interface;

  ISkSVGSVG = interface;

  { ISkSVGDOM }

  ISkSVGDOM = interface(ISkReferenceCounted)
    ['{112EAB88-0340-490E-96F7-21263A751173}']
    function FindNodeById(const AId: string): ISkSVGNode;
    function GetRoot: ISkSVGSVG;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const AValue: TSizeF);
    property Root: ISkSVGSVG read GetRoot;
  end;

  { TSkSVGDOM }

  TSkSVGDOM = class(TSkReferenceCounted, ISkSVGDOM)
  strict private
    FRootHolder: ISkSVGSVG;
  strict protected
    function FindNodeById(const AId: string): ISkSVGNode;
    function GetRoot: ISkSVGSVG;
    procedure Render(const ACanvas: ISkCanvas);
    procedure SetContainerSize(const ASize: TSizeF);
  public
    class function Make(const AData: string; const AResourceProvider: ISkResourceProvider = nil): ISkSVGDOM; static;
    class function MakeFromFile(const AFileName: string): ISkSVGDOM; static;
    class function MakeFromStream(const AStream: TStream; const AResourceProvider: ISkResourceProvider = nil): ISkSVGDOM; static;
  end;

  { ISkSVGNode }

  ISkSVGNode = interface(ISkReferenceCounted)
    ['{BFC94F54-A2C3-4BC8-AC2A-B64425E25CC1}']
    function TrySetAttribute(const AName, AValue: string): Boolean;
  end;

  { TSkSVGNode }

  TSkSVGNode = class(TSkReferenceCounted, ISkSVGNode)
  strict protected
    function TrySetAttribute(const AName, AValue: string): Boolean;
  end;

  TSkSVGLengthUnit = (Unknown, Number, Percentage, EMS, EXS, PX, CM, MM, &IN, PT, PC);

  { TSkSVGLength }

  TSkSVGLength = record
    Value: Single;
    &Unit: TSkSVGLengthUnit;
    constructor Create(const AValue: Single; const AUnit: TSkSVGLengthUnit = TSkSVGLengthUnit.Number);
    class operator Equal(const ASVGLength1, ASVGLength2: TSkSVGLength): Boolean;
    class operator NotEqual(const ASVGLength1, ASVGLength2: TSkSVGLength): Boolean;
  end;

  TSkSVGAspectAlign = (XMinYMin, XMidYMin, XMaxYMin, XMinYMid = 4, XMidYMid, XMaxYMid, XMinYMax = 8, XMidYMax, XMaxYMax, None = 16);

  TSkSVGAspectScale = (Meet, Slice);

  { TSkSVGPreserveAspectRatio }

  TSkSVGPreserveAspectRatio = record
    Align: TSkSVGAspectAlign;
    Scale: TSkSVGAspectScale;
    constructor Create(const AAlign: TSkSVGAspectAlign; const AScale: TSkSVGAspectScale);
    class operator Equal(const ASVGPreserveAspectRatio1, ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
    class operator NotEqual(const ASVGPreserveAspectRatio1, ASVGPreserveAspectRatio2: TSkSVGPreserveAspectRatio): Boolean;
  end;

  { ISkSVGSVG }

  ISkSVGSVG = interface(ISkSVGNode)
    ['{C9B7359F-74C7-4877-A276-964FA62CEC31}']
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
  strict protected
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

const
  SkNative32ColorType: TSkColorType = {$IFDEF BIGENDIAN}TSkColorType.RGBA8888{$ELSE}TSkColorType.BGRA8888{$ENDIF};

  SkBytesPerPixel: array[TSkColorType] of Integer = (
    { Unknown        }  0,
    { Alpha8         }  1,
    { RGB565         }  2,
    { ARGB4444       }  2,
    { RGBA8888       }  4,
    { RGB888X        }  4,
    { BGRA8888       }  4,
    { RGBA1010102    }  4,
    { BGRA1010102    }  4,
    { RGB101010X     }  4,
    { BGR101010X     }  4,
    { Gray8          }  1,
    { RGBAF16        }  8,
    { RGBAF16Clamped }  8,
    { RGBAF32        } 16,
    { RG88           }  2,
    { AlphaF16       }  2,
    { RGF16          }  4,
    { Alpha16        }  2,
    { RG1616         }  4,
    { RGBA16161616   }  8,
    { SRGBA8888      }  4
  );

  SkShiftPerPixel: array[TSkColorType] of Integer = (
    { Unknown        } 0,
    { Alpha8         } 0,
    { RGB565         } 1,
    { ARGB4444       } 1,
    { RGBA8888       } 2,
    { RGB888X        } 2,
    { BGRA8888       } 2,
    { RGBA1010102    } 2,
    { BGRA1010102    } 2,
    { RGB101010X     } 2,
    { BGR101010X     } 2,
    { Gray8          } 0,
    { RGBAF16        } 3,
    { RGBAF16Clamped } 3,
    { RGBAF32        } 4,
    { RG88           } 1,
    { AlphaF16       } 1,
    { RGF16          } 2,
    { Alpha16        } 1,
    { RG1616         } 2,
    { RGBA16161616   } 3,
    { SRGBA8888      } 2
  );

  GrGlTexture2D        = $0DE1;
  GrGlTextureRectangle = $84F5;
  GrGlTextureExternal  = $8D65;

  GrGlSizedFormat: array[TSkColorType] of GrGlenum = (
    { Unknown                           } 0,
    { Alpha8         - GR_GL_ALPHA8     } $803C,
    { RGB565         - GR_GL_RGB565     } $8D62,
    { ARGB4444       - GR_GL_RGBA4      } $8056,
    { RGBA8888       - GR_GL_RGBA8      } $8058,
    { RGB888X                           } 0,
    { BGRA8888       - GR_GL_BGRA8      } $93A1,
    { RGBA1010102    - GR_GL_RGB10_A2   } $8059,
    { BGRA1010102                       } 0,
    { RGB101010X                        } 0,
    { BGR101010X                        } 0,
    { Gray8          - GR_GL_LUMINANCE8 } $8040,
    { RGBAF16        - GR_GL_RGBA16F    } $881A,
    { RGBAF16Clamped - GR_GL_RGBA16F    } $881A,
    { RGBAF32                           } 0,
    { RG88           - GR_GL_RG8        } $822B,
    { AlphaF16       - GR_GL_R16F       } $822D,
    { RGF16          - GR_GL_RG16F      } $822F,
    { Alpha16        - GR_GL_R16        } $822A,
    { RG1616         - GR_GL_RG16       } $822C,
    { RGBA16161616   - GR_GL_RGBA16     } $805B,
    { SRGBA8888                         } 0
  );

implementation

uses
  { Delphi }
  System.Generics.Collections,
  System.IOUtils,
  System.Math,
  System.RTLConsts,
  System.TimeSpan;

type
  { ISkString }

  ISkString = interface(ISkObject)
    ['{F6A12950-1C84-4D26-A7CA-FE83D564D0F7}']
    function GetText: string;
    procedure SetText(const AValue: string);
    property Text: string read GetText write SetText;
  end;

  { TSkString }

  TSkString = class(TSkObject, ISkString)
  strict protected
    function GetText: string;
    procedure SetText(const AValue: string);
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create; overload;
    constructor Create(const AString: string); overload;
  end;

  { ISkData }

  ISkData = interface(ISkNonVirtualReferenceCounted)
    ['{01A42CFE-C79D-436D-89D5-D6067824B0B9}']
  end;

  { TSkData }

  TSkData = class(TSkNonVirtualReferenceCounted, ISkData)
  public
    constructor Create(const ABytes: TBytes);
  end;

  { ISkStream }

  ISkStream = interface(ISkObject)
    ['{F710777C-FFEE-4089-8BEA-06B78FE4A5C4}']
  end;

  { TSkStreamAdapter }

  TSkStreamAdapter = class(TSkObject, ISkStream)
  strict private
    class constructor Create;
    class function get_length_proc(context: Pointer): size_t; cdecl; static;
    class function get_position_proc(context: Pointer): size_t; cdecl; static;
    class function read_proc(context: Pointer; buffer: Pointer; size: size_t): size_t; cdecl; static;
    class function seek_proc(context: Pointer; position: size_t): bool; cdecl; static;
  strict protected
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const AStream: TStream);
  end;

  { ISkWStream }

  ISkWStream = interface(ISkObject)
    ['{F608143C-DF48-4CBB-BD42-DC3E4DCC5EDA}']
  end;

  { TSkWStreamAdapter }

  TSkWStreamAdapter = class(TSkObject, ISkWStream)
  strict private
    class constructor Create;
    class function write_proc(context: Pointer; const buffer: Pointer; size: size_t): bool; cdecl; static;
  strict protected
    class procedure DestroyHandle(const AHandle: THandle); override;
  public
    constructor Create(const AStream: TStream);
  end;

  { TSkManagedDocument }

  TSkManagedDocument = class(TSkDocument)
  strict private
    FWStream: ISkWStream;
  public
    class function MakePDF(const AStream: TStream): ISkDocument; overload;
    class function MakePDF(const AStream: TStream; const AMetadata: TSkPDFMetadata): ISkDocument; overload;
    class function MakeXPS(const AStream: TStream; const ADPI: Single = 72): ISkDocument;
  end;

  { TSkManagedSVGCanvas }

  TSkManagedSVGCanvas = class(TSkCanvas)
  strict private
    FWStream: ISkWStream;
  public
    class function Make(const ABounds: TRectF; const AStream: TStream; const AFlags: TSkSVGCanvasFlags = []): ISkCanvas;
  end;

  { TSkMapping }

  TSkMapping = record
    class function AsDateTime(const AValue: TDateTime): sk_datetime_t; static; inline;
    class function AsImageInfo(const AValue: TSkImageInfo): sk_imageinfo_t; static; inline;
    class function AsLattice(const AValue: TSkLattice): sk_lattice_t; static; inline;
    class function AsPDFMetadata(const AValue: TSkPDFMetadata): sk_pdfmetadata_t; static; inline;
    class function AsSurfaceProperties(const AValue: TSkSurfaceProperties): sk_surfaceprops_t; static; inline;
    class function ToFontMetrics(const AValue: sk_fontmetrics_t): TSkFontMetrics; static; inline;
    class function ToImageInfo(const AValue: sk_imageinfo_t): TSkImageInfo; static; inline;
    class function ToSurfaceProperties(const AValue: sk_surfaceprops_t): TSkSurfaceProperties; static; inline;
  end;

const
  SFileNameIsEmpty   = 'File name cannot be empty';
  SInvalidExtension  = 'Invalid extension';
  SInvalidOperation  = 'Operation is invalid';
  SOutOfMemory       = 'Out of memory';
  SParamElementIsNil = 'Parameter %s[%d] cannot be nil';
  SParamIsEmpty      = 'Parameter %s cannot be empty';
  SParamIsNil        = 'Parameter %s cannot be nil';
  SParamOutOfRange   = 'Parameter %s out of range (%d). Must be >= %d and < %d';
  SParamSizeIsOdd    = 'Parameter %s size cannot be odd';
  SParamSizeMismatch = 'Parameter %s size mismatch';

{ TSkString }

constructor TSkString.Create(const AString: string);
begin
  Create;
  SetText(AString);
end;

constructor TSkString.Create;
begin
  Wrap(TSkiaAPI.sk4d_string_create());
end;

class procedure TSkString.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_string_destroy(AHandle);
end;

function TSkString.GetText: string;
begin
  Result := string(TSkiaAPI.sk4d_string_get_text(GetSelf));
end;

procedure TSkString.SetText(const AValue: string);
begin
  TSkiaAPI.sk4d_string_set_text(GetSelf, MarshaledAString(UTF8String(AValue)));
end;

{ TSkData }

constructor TSkData.Create(const ABytes: TBytes);
var
  LHandle: THandle;
begin
  if Length(ABytes) = 0 then
    LHandle := TSkiaAPI.sk4d_data_make_empty()
  else
    LHandle := TSkiaAPI.sk4d_data_make_with_copy(@ABytes[0], Length(ABytes));
  inherited Wrap(LHandle, False)
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
  TSkiaAPI.sk4d_streamadapter_set_procs(@LProcs);
end;

constructor TSkStreamAdapter.Create(const AStream: TStream);
begin
  inherited Wrap(TSkiaAPI.sk4d_streamadapter_create(AStream));
end;

class procedure TSkStreamAdapter.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_streamadapter_destroy(AHandle);
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
  position: size_t): bool;
begin
  TStream(context).Position := position;
  Result := True;
end;

{ TSkWStreamAdapter }

class constructor TSkWStreamAdapter.Create;
var
  LProcs: sk_wstreamadapter_procs_t;
begin
  LProcs.write := write_proc;
  TSkiaAPI.sk4d_wstreamadapter_set_procs(@LProcs);
end;

constructor TSkWStreamAdapter.Create(const AStream: TStream);
begin
  inherited Wrap(TSkiaAPI.sk4d_wstreamadapter_create(AStream));
end;

class procedure TSkWStreamAdapter.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_wstreamadapter_destroy(AHandle);
end;

class function TSkWStreamAdapter.write_proc(context: Pointer;
  const buffer: Pointer; size: size_t): bool;
begin
  TStream(context).Write(buffer^, size);
  Result := True;
end;

{ TSkManagedDocument }

class function TSkManagedDocument.MakePDF(const AStream: TStream): ISkDocument;
var
  LHandle: THandle;
  LManagedDocument: TSkManagedDocument;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := TSkiaAPI.sk4d_document_make_pdf(LWStream.Handle);
  if LHandle = 0 then
    Exit(nil);
  LManagedDocument := TSkManagedDocument.Wrap(LHandle);
  LManagedDocument.FWStream := LWStream;
  Result := LManagedDocument;
end;

class function TSkManagedDocument.MakePDF(const AStream: TStream;
  const AMetadata: TSkPDFMetadata): ISkDocument;
var
  LHandle: THandle;
  LManagedDocument: TSkManagedDocument;
  LMetadata: sk_pdfmetadata_t;
  LWStream: ISkWStream;
begin
  LWStream  := TSkWStreamAdapter.Create(AStream);
  LMetadata := TSkMapping.AsPDFMetadata(AMetadata);
  LHandle   := TSkiaAPI.sk4d_document_make_pdf2(LWStream.Handle, @LMetadata);
  if LHandle = 0 then
    Exit(nil);
  LManagedDocument := TSkManagedDocument.Wrap(LHandle);
  LManagedDocument.FWStream := LWStream;
  Result := LManagedDocument;
end;

class function TSkManagedDocument.MakeXPS(const AStream: TStream;
  const ADPI: Single): ISkDocument;
var
  LHandle: THandle;
  LManagedDocument: TSkManagedDocument;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := TSkiaAPI.sk4d_document_make_xps(LWStream.Handle, ADPI);
  if LHandle = 0 then
    Exit(nil);
  LManagedDocument := TSkManagedDocument.Wrap(LHandle);
  LManagedDocument.FWStream := LWStream;
  Result := LManagedDocument;
end;

{ TSkManagedSVGCanvas }

class function TSkManagedSVGCanvas.Make(const ABounds: TRectF;
  const AStream: TStream; const AFlags: TSkSVGCanvasFlags): ISkCanvas;
var
  LHandle: THandle;
  LManagedSVGCanvas: TSkManagedSVGCanvas;
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  LHandle  := TSkiaAPI.sk4d_svgcanvas_make(@sk_rect_t(ABounds), LWStream.Handle, Byte(AFlags));
  if LHandle = 0 then
    Exit(nil);
  LManagedSVGCanvas := TSkManagedSVGCanvas.Wrap(LHandle);
  LManagedSVGCanvas.FWStream := LWStream;
  Result := LManagedSVGCanvas;
end;

{ TSkMapping }

class function TSkMapping.AsDateTime(const AValue: TDateTime): sk_datetime_t;
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
  Result.time_zone_minutes := Round(TTimeZone.Local.UtcOffset.TotalMinutes);
  Result.year              := LYear;
  Result.month             := LMonth;
  Result.day_of_week       := DayOfWeek(AValue) - 1;
  Result.day               := LDay;
  Result.hour              := LHour;
  Result.minute            := LMinute;
  Result.second            := LSecond;
end;

class function TSkMapping.AsImageInfo(
  const AValue: TSkImageInfo): sk_imageinfo_t;
begin
  Result.width       := AValue.Width;
  Result.height      := AValue.Height;
  Result.color_type  := sk_colortype_t(AValue.ColorType);
  Result.alpha_type  := sk_alphatype_t(AValue.AlphaType);
  Result.color_space := TSkBindings.SafeGetSelf(AValue.ColorSpace);
end;

class function TSkMapping.AsLattice(const AValue: TSkLattice): sk_lattice_t;
begin
  Result.x_divs  := @AValue.FXDivs[0];
  Result.y_divs  := @AValue.FYDivs[0];
  Result.x_count := Length(AValue.FXDivs);
  Result.y_count := Length(AValue.FYDivs);
  if Length(AValue.FRectTypes) > 0 then
  begin
    Result.rect_types := @AValue.FRectTypes[0];
    Result.colors     := @AValue.FColors[0];
  end
  else
  begin
    Result.rect_types := nil;
    Result.colors     := nil;
  end;
  if AValue.FUseBounds then
    Result.bounds := @sk_irect_t(AValue.FBounds)
  else
    Result.bounds := nil;
end;

class function TSkMapping.AsPDFMetadata(
  const AValue: TSkPDFMetadata): sk_pdfmetadata_t;
begin
  Result.title            := MarshaledAString(UTF8String(AValue.Title));
  Result.author           := MarshaledAString(UTF8String(AValue.Author));
  Result.subject          := MarshaledAString(UTF8String(AValue.Subject));
  Result.keywords         := MarshaledAString(UTF8String(AValue.Keywords));
  Result.creator          := MarshaledAString(UTF8String(AValue.Creator));
  Result.producer         := MarshaledAString(UTF8String(AValue.Producer));
  Result.creation         := TSkMapping.AsDateTime(AValue.Creation);
  Result.modified         := TSkMapping.AsDateTime(AValue.Modified);
  Result.raster_dpi       := AValue.RasterDPI;
  Result.pdfa             := AValue.PDFA;
  Result.encoding_quality := AValue.EncodingQuality;
end;

class function TSkMapping.AsSurfaceProperties(
  const AValue: TSkSurfaceProperties): sk_surfaceprops_t;
begin
  Result.flags          := Byte(AValue.Flags);
  Result.pixel_geometry := sk_pixelgeometry_t(AValue.PixelGeometry);
end;

class function TSkMapping.ToFontMetrics(
  const AValue: sk_fontmetrics_t): TSkFontMetrics;
begin
  Result.Flags              := TSkFontMetricsFlags(Byte(AValue.flags));
  Result.Top                := AValue.top;
  Result.Ascent             := AValue.ascent;
  Result.Descent            := AValue.descent;
  Result.Bottom             := AValue.bottom;
  Result.Leading            := AValue.leading;
  Result.AvgCharWidth       := AValue.avg_char_width;
  Result.MaxCharWidth       := AValue.max_char_width;
  Result.XMin               := AValue.x_min;
  Result.XMax               := AValue.x_max;
  Result.XHeight            := AValue.x_height;
  Result.CapHeight          := AValue.cap_height;
  Result.UnderlineThickness := AValue.underline_thickness;
  Result.UnderlinePosition  := AValue.underline_position;
  Result.StrikeoutThickness := AValue.strikeout_thickness;
  Result.StrikeoutPosition  := AValue.strikeout_position;
end;

class function TSkMapping.ToImageInfo(
  const AValue: sk_imageinfo_t): TSkImageInfo;
begin
  Result.Width      := AValue.width;
  Result.Height     := AValue.height;
  Result.ColorType  := TSkColorType(AValue.color_type);
  Result.AlphaType  := TSkAlphaType(AValue.alpha_type);
  Result.ColorSpace := TSkBindings.SafeCreate<TSkColorSpace>(AValue.color_space);
end;

class function TSkMapping.ToSurfaceProperties(
  const AValue: sk_surfaceprops_t): TSkSurfaceProperties;
begin
  Result.Flags         := TSkSurfacePropertiesFlags(Byte(AValue.flags));
  Result.PixelGeometry := TSkPixelGeometry(AValue.pixel_geometry);
end;

{ TGrGlFramebufferInfo }

constructor TGrGlFramebufferInfo.Create(const AFBOID: GrGluint;
  const AFormat: GrGlenum);
begin
  FBOID  := AFBOID;
  Format := AFormat;
end;

class operator TGrGlFramebufferInfo.Equal(const AGlFramebufferInfo1,
  AGlFramebufferInfo2: TGrGlFramebufferInfo): Boolean;
begin
  Result := (AGlFramebufferInfo1.FBOID  = AGlFramebufferInfo2.FBOID ) and
            (AGlFramebufferInfo1.Format = AGlFramebufferInfo2.Format);
end;

class operator TGrGlFramebufferInfo.NotEqual(const AGlFramebufferInfo1,
  AGlFramebufferInfo2: TGrGlFramebufferInfo): Boolean;
begin
  Result := not (AGlFramebufferInfo1 = AGlFramebufferInfo2);
end;

{ TGrMtlTextureInfo }

constructor TGrMtlTextureInfo.Create(const ATexture: GrMtlHandle);
begin
  Texture := ATexture;
end;

class operator TGrMtlTextureInfo.Equal(const AMtlTextureInfo1,
  AMtlTextureInfo2: TGrMtlTextureInfo): Boolean;
begin
  Result := (AMtlTextureInfo1.Texture = AMtlTextureInfo2.Texture);
end;

class operator TGrMtlTextureInfo.NotEqual(const AMtlTextureInfo1,
  AMtlTextureInfo2: TGrMtlTextureInfo): Boolean;
begin
  Result := not (AMtlTextureInfo1 = AMtlTextureInfo2);
end;

{ TGrBackendRenderTarget }

constructor TGrBackendRenderTarget.CreateGl(const AWidth, AHeight, ASampleCount,
  AStencilBits: Integer; const AFramebufferInfo: TGrGlFramebufferInfo);
begin
  inherited Wrap(TSkiaAPI.gr4d_backendrendertarget_create_gl(AWidth, AHeight, ASampleCount, AStencilBits, @gr_gl_framebufferinfo_t(AFramebufferInfo)), True)
end;

constructor TGrBackendRenderTarget.CreateMetal(const AWidth, AHeight: Integer;
  const ATextureInfo: TGrMtlTextureInfo);
begin
  inherited Wrap(TSkiaAPI.gr4d_backendrendertarget_create_mtl(AWidth, AHeight, @gr_mtl_textureinfo_t(ATextureInfo)));
end;

class procedure TGrBackendRenderTarget.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.gr4d_backendrendertarget_destroy(AHandle);
end;

function TGrBackendRenderTarget.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(TSkiaAPI.gr4d_backendrendertarget_get_backend_api(GetSelf));
end;

function TGrBackendRenderTarget.GetHeight: Integer;
begin
  Result := TSkiaAPI.gr4d_backendrendertarget_get_height(GetSelf);
end;

function TGrBackendRenderTarget.GetSampleCount: Integer;
begin
  Result := TSkiaAPI.gr4d_backendrendertarget_get_sample_count(GetSelf);
end;

function TGrBackendRenderTarget.GetStencilBits: Integer;
begin
  Result := TSkiaAPI.gr4d_backendrendertarget_get_stencil_bits(GetSelf);
end;

function TGrBackendRenderTarget.GetWidth: Integer;
begin
  Result := TSkiaAPI.gr4d_backendrendertarget_get_width(GetSelf);
end;

function TGrBackendRenderTarget.IsValid: Boolean;
begin
  Result := TSkiaAPI.gr4d_backendrendertarget_is_valid(GetSelf);
end;

{ TGrGlTextureInfo }

constructor TGrGlTextureInfo.Create(const ATarget: GrGlenum;
  const AID: GrGluint; const AFormat: GrGlenum);
begin
  Target := ATarget;
  ID     := AID;
  Format := AFormat;
end;

class operator TGrGlTextureInfo.Equal(const AGlTextureInfo1,
  AGlTextureInfo2: TGrGlTextureInfo): Boolean;
begin
  Result := (AGlTextureInfo1.Target = AGlTextureInfo2.Target) and
            (AGlTextureInfo1.ID     = AGlTextureInfo2.ID    ) and
            (AGlTextureInfo1.Format = AGlTextureInfo2.Format);
end;

class operator TGrGlTextureInfo.NotEqual(const AGlTextureInfo1,
  AGlTextureInfo2: TGrGlTextureInfo): Boolean;
begin
  Result := not (AGlTextureInfo1 = AGlTextureInfo2);
end;

{ TGrBackendTexture }

constructor TGrBackendTexture.CreateGl(const AWidth, AHeight: Integer;
  const AIsMipmapped: Boolean; const ATextureInfo: TGrGlTextureInfo);
begin
  inherited Wrap(TSkiaAPI.gr4d_backendtexture_create_gl(AWidth, AHeight, AIsMipmapped, @gr_gl_textureinfo_t(ATextureInfo)));
end;

constructor TGrBackendTexture.CreateMetal(const AWidth, AHeight: Integer;
  const AIsMipmapped: Boolean; const ATextureInfo: TGrMtlTextureInfo);
begin
  inherited Wrap(TSkiaAPI.gr4d_backendtexture_create_mtl(AWidth, AHeight, AIsMipmapped, @gr_mtl_textureinfo_t(ATextureInfo)));
end;

class procedure TGrBackendTexture.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.gr4d_backendtexture_destroy(AHandle);
end;

function TGrBackendTexture.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(TSkiaAPI.gr4d_backendtexture_get_backend_api(GetSelf));
end;

function TGrBackendTexture.GetGlTextureInfo(
  out ATextureInfo: TGrGlTextureInfo): Boolean;
begin
  Result := TSkiaAPI.gr4d_backendtexture_get_gl_framebuffer_info(GetSelf, gr_gl_textureinfo_t(ATextureInfo));
end;

function TGrBackendTexture.GetHeight: Integer;
begin
  Result := TSkiaAPI.gr4d_backendtexture_get_height(GetSelf);
end;

function TGrBackendTexture.GetWidth: Integer;
begin
  Result := TSkiaAPI.gr4d_backendtexture_get_width(GetSelf);
end;

function TGrBackendTexture.HasMipmaps: Boolean;
begin
  Result := TSkiaAPI.gr4d_backendtexture_has_mipmaps(GetSelf);
end;

function TGrBackendTexture.IsValid: Boolean;
begin
  Result := TSkiaAPI.gr4d_backendtexture_is_valid(GetSelf);
end;

{ TGrContextOptions }

class operator TGrContextOptions.Equal(const AContextOptions1,
  AContextOptions2: TGrContextOptions): Boolean;
begin
  Result := (AContextOptions1.BufferMapThreshold            = AContextOptions2.BufferMapThreshold           ) and
            (AContextOptions1.DoManualMipmapping            = AContextOptions2.DoManualMipmapping           ) and
            (AContextOptions1.AllowPathMaskCaching          = AContextOptions2.AllowPathMaskCaching         ) and
            (AContextOptions1.GlyphCacheTextureMaximumBytes = AContextOptions2.GlyphCacheTextureMaximumBytes) and
            (AContextOptions1.AvoidStencilBuffers           = AContextOptions2.AvoidStencilBuffers          ) and
            (AContextOptions1.RuntimeProgramCacheSize       = AContextOptions2.RuntimeProgramCacheSize      );
end;

class operator TGrContextOptions.NotEqual(const AContextOptions1,
  AContextOptions2: TGrContextOptions): Boolean;
begin
  Result := not (AContextOptions1 = AContextOptions2);
end;

{ TGrMtlBackendContext }

constructor TGrMtlBackendContext.Create(const ADevice, AQueue,
  ABinaryArchive: GrMtlHandle);
begin
  Device        := ADevice;
  Queue         := AQueue;
  BinaryArchive := ABinaryArchive;
end;

class operator TGrMtlBackendContext.Equal(const AMtlBackendContext1,
  AMtlBackendContext2: TGrMtlBackendContext): Boolean;
begin
  Result := (AMtlBackendContext1.Device        = AMtlBackendContext2.Device       ) and
            (AMtlBackendContext1.Queue         = AMtlBackendContext2.Queue        ) and
            (AMtlBackendContext1.BinaryArchive = AMtlBackendContext2.BinaryArchive);
end;

class operator TGrMtlBackendContext.NotEqual(const AMtlBackendContext1,
  AMtlBackendContext2: TGrMtlBackendContext): Boolean;
begin
  Result := not (AMtlBackendContext1 = AMtlBackendContext2);
end;

{ TGrDirectContext }

procedure TGrDirectContext.AbandonContext;
begin
  TSkiaAPI.gr4d_directcontext_abandon_context(GetSelf);
end;

procedure TGrDirectContext.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  if not Assigned(ATraceMemoryDump) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATraceMemoryDump']);
  TSkiaAPI.gr4d_directcontext_dump_memory_statistics(GetSelf, TSkBindings.GetSelf(ATraceMemoryDump));
end;

procedure TGrDirectContext.Flush;
begin
  TSkiaAPI.gr4d_directcontext_flush(GetSelf);
end;

procedure TGrDirectContext.FlushAndSubmit(const ASyncCPU: Boolean);
begin
  TSkiaAPI.gr4d_directcontext_flush_and_submit(GetSelf, ASyncCPU);
end;

procedure TGrDirectContext.FreeGPUResources;
begin
  TSkiaAPI.gr4d_directcontext_free_gpu_resources(GetSelf);
end;

function TGrDirectContext.GetBackendAPI: TGrBackendAPI;
begin
  Result := TGrBackendAPI(TSkiaAPI.gr4d_directcontext_get_backend_api(GetSelf));
end;

function TGrDirectContext.GetMaxSurfaceSampleCountForColorType(
  const AColorType: TSkColorType): Integer;
begin
  Result := TSkiaAPI.gr4d_directcontext_get_max_surface_sample_count_for_color_type(GetSelf, sk_colortype_t(AColorType));
end;

function TGrDirectContext.GetResourceCacheLimit: NativeUInt;
begin
  Result := TSkiaAPI.gr4d_directcontext_get_resource_cache_limit(GetSelf);
end;

procedure TGrDirectContext.GetResourceCacheUsage(out AResources: Integer;
  out AResourcesBytes: NativeUInt);
begin
  TSkiaAPI.gr4d_directcontext_get_resource_cache_usage(GetSelf, AResources, AResourcesBytes);
end;

function TGrDirectContext.IsAbandoned: Boolean;
begin
  Result := TSkiaAPI.gr4d_directcontext_is_abandoned(GetSelf);
end;

class function TGrDirectContext.MakeGl(
  AInterface: IGrGlInterface): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(TSkiaAPI.gr4d_directcontext_make_gl(TSkBindings.SafeGetSelf(AInterface), nil));
end;

class function TGrDirectContext.MakeGl(const AOptions: TGrContextOptions;
  const AInterface: IGrGlInterface): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(TSkiaAPI.gr4d_directcontext_make_gl(TSkBindings.SafeGetSelf(AInterface), @gr_contextoptions_t(AOptions)));
end;

class function TGrDirectContext.MakeMetal(
  const ABackendContext: TGrMtlBackendContext): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(TSkiaAPI.gr4d_directcontext_make_metal(@gr_mtl_backendcontext_t(ABackendContext), nil));
end;

class function TGrDirectContext.MakeMetal(
  const ABackendContext: TGrMtlBackendContext;
  const AOptions: TGrContextOptions): IGrDirectContext;
begin
  Result := TSkBindings.SafeCreate<TGrDirectContext>(TSkiaAPI.gr4d_directcontext_make_metal(@gr_mtl_backendcontext_t(ABackendContext), @gr_contextoptions_t(AOptions)));
end;

procedure TGrDirectContext.PerformDeferredCleanup(const AMilliseconds: Int64);
begin
  TSkiaAPI.gr4d_directcontext_perform_deferred_cleanup(GetSelf, AMilliseconds);
end;

procedure TGrDirectContext.PurgeUnlockedResources(
  const AScratchResourcesOnly: Boolean);
begin
  TSkiaAPI.gr4d_directcontext_purge_unlocked_resources(GetSelf, AScratchResourcesOnly);
end;

procedure TGrDirectContext.PurgeUnlockedResources(
  const ABytesToPurge: NativeUInt; const APreferScratchResources: Boolean);
begin
  TSkiaAPI.gr4d_directcontext_purge_unlocked_resources2(GetSelf, ABytesToPurge, APreferScratchResources);
end;

procedure TGrDirectContext.ReleaseResourcesAndAbandonContext;
begin
  TSkiaAPI.gr4d_directcontext_release_resources_and_abandon_context(GetSelf);
end;

procedure TGrDirectContext.ResetContext;
begin
  TSkiaAPI.gr4d_directcontext_reset_context(GetSelf);
end;

procedure TGrDirectContext.SetResourceCacheLimit(const AValue: NativeUInt);
begin
  TSkiaAPI.gr4d_directcontext_set_resource_cache_limit(GetSelf, AValue);
end;

function TGrDirectContext.Submit(const ASyncCPU: Boolean): Boolean;
begin
  Result := TSkiaAPI.gr4d_directcontext_submit(GetSelf, ASyncCPU);
end;

{ TGrGlInterface }

class function TGrGlInterface.get_proc(context: Pointer;
  const name: MarshaledAString): Pointer;
begin
  Result := TGrGlGetProc(context^)(string(name));
end;

function TGrGlInterface.HasExtension(const AName: string): Boolean;
begin
  Result := TSkiaAPI.gr4d_gl_interface_has_extension(GetSelf, MarshaledAString(UTF8String(AName)));
end;

class function TGrGlInterface.MakeAssembled(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(TSkiaAPI.gr4d_gl_interface_make_assembled(@AProc, get_proc));
end;

class function TGrGlInterface.MakeAssembledGl(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(TSkiaAPI.gr4d_gl_interface_make_assembled_gl(@AProc, get_proc));
end;

class function TGrGlInterface.MakeAssembledGles(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(TSkiaAPI.gr4d_gl_interface_make_assembled_gles(@AProc, get_proc));
end;

class function TGrGlInterface.MakeAssembledWebGl(
  const AProc: TGrGlGetProc): IGrGlInterface;
begin
  if not Assigned(AProc) then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TGrGlInterface>(TSkiaAPI.gr4d_gl_interface_make_assembled_webgl(@AProc, get_proc));
end;

class function TGrGlInterface.MakeNative: IGrGlInterface;
begin
  Result := TSkBindings.SafeCreate<TGrGlInterface>(TSkiaAPI.gr4d_gl_interface_make_native());
end;

function TGrGlInterface.Validate: Boolean;
begin
  Result := TSkiaAPI.gr4d_gl_interface_validate(GetSelf);
end;

{ TSkBlender }

class function TSkBlender.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean): ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(TSkiaAPI.sk4d_blender_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor));
end;

class function TSkBlender.MakeMode(const AMode: TSkBlendMode): ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(TSkiaAPI.sk4d_blender_make_mode(sk_blendmode_t(AMode)));
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
  Result := (SameValue(ARotationScaleMatrix1.SCosinus,   ARotationScaleMatrix2.SCosinus,   Epsilon)) and
            (SameValue(ARotationScaleMatrix1.SSinus,     ARotationScaleMatrix2.SSinus,     Epsilon)) and
            (SameValue(ARotationScaleMatrix1.TranslateX, ARotationScaleMatrix2.TranslateX, Epsilon)) and
            (SameValue(ARotationScaleMatrix1.TranslateY, ARotationScaleMatrix2.TranslateY, Epsilon));
end;

class operator TSkRotationScaleMatrix.Implicit(
  const ARotationScaleMatrix: TSkRotationScaleMatrix): TMatrix;
begin
  Result.m11 := ARotationScaleMatrix.SCosinus;
  Result.m12 := ARotationScaleMatrix.SSinus;
  Result.m13 := 0;
  Result.m21 := -ARotationScaleMatrix.SSinus;
  Result.m22 := ARotationScaleMatrix.SCosinus;
  Result.m23 := 0;
  Result.m31 := ARotationScaleMatrix.TranslateX;
  Result.m32 := ARotationScaleMatrix.TranslateY;
  Result.m33 := 1;
end;

class operator TSkRotationScaleMatrix.NotEqual(const ARotationScaleMatrix1,
  ARotationScaleMatrix2: TSkRotationScaleMatrix): Boolean;
begin
  Result := not (ARotationScaleMatrix1 = ARotationScaleMatrix2);
end;

{ TSkCubicResampler }

constructor TSkCubicResampler.Create(const AB, AC: Single);
begin
  B := AB;
  C := AC;
end;

class operator TSkCubicResampler.Equal(const ACubicResampler1,
  ACubicResampler2: TSkCubicResampler): Boolean;
begin
  Result := (SameValue(ACubicResampler1.B, ACubicResampler2.B, Epsilon)) and
            (SameValue(ACubicResampler1.C, ACubicResampler2.C, Epsilon));
end;

class operator TSkCubicResampler.NotEqual(const ACubicResampler1,
  ACubicResampler2: TSkCubicResampler): Boolean;
begin
  Result := not (ACubicResampler1 = ACubicResampler2);
end;

{ TSkSamplingOptions }

constructor TSkSamplingOptions.Create(const ACubic: TSkCubicResampler);
begin
  FUseCubic := True;
  FCubic    := ACubic;
  FFilter   := TSkFilterMode.Nearest;
  FMipmap   := TSkMipmapMode.None;
end;

constructor TSkSamplingOptions.Create(const AFilter: TSkFilterMode;
  const AMipmap: TSkMipmapMode);
begin
  FUseCubic := False;
  FCubic    := TSkCubicResampler.Create(0, 0);
  FFilter   := AFilter;
  FMipmap   := AMipmap;
end;

class operator TSkSamplingOptions.Equal(const ASamplingOptions1,
  ASamplingOptions2: TSkSamplingOptions): Boolean;
begin
  Result := (ASamplingOptions1.FUseCubic = ASamplingOptions2.FUseCubic) and
            (ASamplingOptions1.FCubic    = ASamplingOptions2.FCubic   ) and
            (ASamplingOptions1.FFilter   = ASamplingOptions2.FFilter  ) and
            (ASamplingOptions1.FMipmap   = ASamplingOptions2.FMipmap  );
end;

class operator TSkSamplingOptions.NotEqual(const ASamplingOptions1,
  ASamplingOptions2: TSkSamplingOptions): Boolean;
begin
  Result := not (ASamplingOptions1 = ASamplingOptions2);
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
  FXDivs     := AXDivs;
  FYDivs     := AYDivs;
  FRectTypes := ARectTypes;
  FColors    := AColors;
  FUseBounds := True;
  FBounds    := ABounds;
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
  FXDivs     := AXDivs;
  FYDivs     := AYDivs;
  FRectTypes := ARectTypes;
  FColors    := AColors;
  FUseBounds := False;
end;

class operator TSkLattice.Equal(const ALattice1,
  ALattice2: TSkLattice): Boolean;
begin
  Result := (ALattice1.FXDivs     = ALattice2.FXDivs    ) and
            (ALattice1.FYDivs     = ALattice2.FYDivs    ) and
            (ALattice1.FRectTypes = ALattice2.FRectTypes) and
            (ALattice1.FColors    = ALattice2.FColors   ) and
            (ALattice1.FUseBounds = ALattice2.FUseBounds) and
            ((not ALattice1.FUseBounds) or (ALattice1.Bounds = ALattice2.Bounds));
end;

function TSkLattice.GetColors: TArray<TAlphaColor>;
begin
  Result := Copy(FColors);
end;

function TSkLattice.GetRectTypes: TArray<TSkLatticeRectType>;
begin
  Result := Copy(FRectTypes);
end;

function TSkLattice.GetXDivs: TArray<Integer>;
begin
  Result := Copy(FXDivs);
end;

function TSkLattice.GetYDivs: TArray<Integer>;
begin
  Result := Copy(FYDivs);
end;

class operator TSkLattice.NotEqual(const ALattice1,
  ALattice2: TSkLattice): Boolean;
begin
  Result := not (ALattice1 = ALattice2);
end;

{ TSkCanvas }

procedure TSkCanvas.Clear(const AColor: TAlphaColor);
begin
  TSkiaAPI.sk4d_canvas_clear(GetSelf, AColor);
end;

procedure TSkCanvas.Clear(const AColor: TAlphaColorF);
begin
  TSkiaAPI.sk4d_canvas_clear2(GetSelf, @sk_color4f_t(AColor));
end;

procedure TSkCanvas.ClipPath(const APath: ISkPath; const AOp: TSkClipOp;
  const AAntiAlias: Boolean);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  TSkiaAPI.sk4d_canvas_clip_path(GetSelf, TSkBindings.GetSelf(APath), sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipRect(const ARect: TRectF; const AOp: TSkClipOp;
  const AAntiAlias: Boolean);
begin
  TSkiaAPI.sk4d_canvas_clip_rect(GetSelf, @sk_rect_t(ARect), sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipRegion(const ARegion: ISkRegion; const AOp: TSkClipOp);
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  TSkiaAPI.sk4d_canvas_clip_region(GetSelf, TSkBindings.GetSelf(ARegion), sk_clipop_t(AOp));
end;

procedure TSkCanvas.ClipRoundRect(const ARoundRect: ISkRoundRect;
  const AOp: TSkClipOp; const AAntiAlias: Boolean);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  TSkiaAPI.sk4d_canvas_clip_rrect(GetSelf, TSkBindings.GetSelf(ARoundRect), sk_clipop_t(AOp), AAntiAlias);
end;

procedure TSkCanvas.ClipShader(const AShader: ISkShader; const AOp: TSkClipOp);
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  TSkiaAPI.sk4d_canvas_clip_shader(GetSelf, TSkBindings.GetSelf(AShader), sk_clipop_t(AOp));
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix);
begin
  TSkiaAPI.sk4d_canvas_concat2(GetSelf, @sk_matrix_t(AMatrix));
end;

procedure TSkCanvas.Concat(const AMatrix: TMatrix3D);
begin
  TSkiaAPI.sk4d_canvas_concat(GetSelf, @sk_matrix44_t(AMatrix));
end;

class procedure TSkCanvas.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_canvas_destroy(AHandle);
end;

procedure TSkCanvas.Discard;
begin
  TSkiaAPI.sk4d_canvas_discard(GetSelf);
end;

procedure TSkCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string;
  const AValue; const ASize: NativeUInt);
begin
  TSkiaAPI.sk4d_canvas_draw_annotation(GetSelf, @sk_rect_t(ARect), MarshaledAString(UTF8String(AKey)), @AValue, ASize);
end;

procedure TSkCanvas.DrawAnnotation(const ARect: TRectF; const AKey: string);
begin
  TSkiaAPI.sk4d_canvas_draw_annotation(GetSelf, @sk_rect_t(ARect), MarshaledAString(UTF8String(AKey)), nil, 0);
end;

procedure TSkCanvas.DrawArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AUseCenter: Boolean; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_arc(GetSelf, @sk_rect_t(AOval), AStartAngle, ASweepAngle, AUseCenter, TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ASampling: TSkSamplingOptions; const AColors: TArray<TAlphaColor>;
  const APaint: ISkPaint);
var
  LColors: psk_color_t;
begin
  if not Assigned(AAtlas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AAtlas']);
  if Length(ATansforms) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ATansforms']);
  if Length(ASprites) <> Length(ATansforms) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ASprites']);
  if Length(AColors) > 0 then
  begin
    if Length(AColors) <> Length(ASprites) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
    LColors := @AColors[0];
  end
  else
    LColors := nil;
  TSkiaAPI.sk4d_canvas_draw_atlas(GetSelf, TSkBindings.GetSelf(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), LColors, Length(ATansforms), sk_blendmode_t(ABlendMode), @sk_samplingoptions_t(ASampling), nil, TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ASampling: TSkSamplingOptions; const ACullRect: TRectF;
  const AColors: TArray<TAlphaColor>; const APaint: ISkPaint);
var
  LColors: psk_color_t;
begin
  if not Assigned(AAtlas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AAtlas']);
  if Length(ATansforms) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ATansforms']);
  if Length(ASprites) <> Length(ATansforms) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ASprites']);
  if Length(AColors) > 0 then
  begin
    if Length(AColors) <> Length(ASprites) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
    LColors := @AColors[0];
  end
  else
    LColors := nil;
  TSkiaAPI.sk4d_canvas_draw_atlas(GetSelf, TSkBindings.GetSelf(AAtlas), @sk_rotationscalematrix_t(ATansforms[0]), @sk_rect_t(ASprites[0]), LColors, Length(ATansforms), sk_blendmode_t(ABlendMode), @sk_samplingoptions_t(ASampling), @sk_rect_t(ACullRect), TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const AColors: TArray<TAlphaColor>; const APaint: ISkPaint);
begin
  DrawAtlas(AAtlas, ATansforms, ASprites, ABlendMode, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), AColors, APaint);
end;

procedure TSkCanvas.DrawAtlas(const AAtlas: ISkImage;
  const ATansforms: TArray<TSkRotationScaleMatrix>;
  const ASprites: TArray<TRectF>; const ABlendMode: TSkBlendMode;
  const ACullRect: TRectF; const AColors: TArray<TAlphaColor>;
  const APaint: ISkPaint);
begin
  DrawAtlas(AAtlas, ATansforms, ASprites, ABlendMode, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ACullRect, AColors, APaint);
end;

procedure TSkCanvas.DrawCircle(const ACenter: TPointF; ARadius: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_circle(GetSelf, @sk_point_t(ACenter), ARadius, TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawCircle(const ACenterX, ACenterY, ARadius: Single;
  const APaint: ISkPaint);
begin
  DrawCircle(TPointF.Create(ACenterX, ACenterY), ARadius, APaint);
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColor;
  const ABlendMode: TSkBlendMode);
begin
  TSkiaAPI.sk4d_canvas_draw_color(GetSelf, AColor, sk_blendmode_t(ABlendMode));
end;

procedure TSkCanvas.DrawColor(const AColor: TAlphaColorF;
  const ABlendMode: TSkBlendMode);
begin
  TSkiaAPI.sk4d_canvas_draw_color2(GetSelf, @sk_color4f_t(AColor), sk_blendmode_t(ABlendMode));
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
  if Length(AGlyphs) > 0 then
    TSkiaAPI.sk4d_canvas_draw_glyphs2(GetSelf, Length(AGlyphs), @AGlyphs[0], @sk_rotationscalematrix_t(AMatrices[0]), @sk_point_t(AOrigin), TSkBindings.GetSelf(AFont), TSkBindings.GetSelf(APaint));
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
  if Length(AGlyphs) > 0 then
    TSkiaAPI.sk4d_canvas_draw_glyphs(GetSelf, Length(AGlyphs), @AGlyphs[0], @sk_point_t(APositions[0]), @sk_point_t(AOrigin), TSkBindings.GetSelf(AFont), TSkBindings.GetSelf(APaint));
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
  TSkiaAPI.sk4d_canvas_draw_image(GetSelf, TSkBindings.GetSelf(AImage), AX, AY, @sk_samplingoptions_t(ASampling), TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawImageLattice(const AImage: ISkImage;
  const ALattice: TSkLattice; const ADest: TRectF;
  const AFilterMode: TSkFilterMode; const APaint: ISkPaint);
var
  LLattice: sk_lattice_t;
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  LLattice := TSkMapping.AsLattice(ALattice);
  TSkiaAPI.sk4d_canvas_draw_image_lattice(GetSelf, TSkBindings.GetSelf(AImage), @LLattice, @sk_rect_t(ADest), sk_filtermode_t(AFilterMode), TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawImageNine(const AImage: ISkImage; const ACenter: TRect;
  const ADest: TRectF; const AFilterMode: TSkFilterMode;
  const APaint: ISkPaint);
begin
  if not Assigned(AImage) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AImage']);
  TSkiaAPI.sk4d_canvas_draw_image_nine(GetSelf, TSkBindings.GetSelf(AImage), @sk_irect_t(ACenter), @sk_rect_t(ADest), sk_filtermode_t(AFilterMode), TSkBindings.SafeGetSelf(APaint));
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
  TSkiaAPI.sk4d_canvas_draw_image_rect(GetSelf, TSkBindings.GetSelf(AImage), @sk_rect_t(ASrc), @sk_rect_t(ADest), @sk_samplingoptions_t(ASampling), TSkBindings.SafeGetSelf(APaint), sk_srcrectconstraint_t(AConstraint));
end;

procedure TSkCanvas.DrawLine(const APoint1, APoint2: TPointF;
  const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_line(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), TSkBindings.GetSelf(APaint));
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
  TSkiaAPI.sk4d_canvas_draw_oval(GetSelf, @sk_rect_t(AOval), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawPaint(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_paint(GetSelf, TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawPatch(const ACubics: TSkPatchCubics;
  const AColors: TSkPatchColors; const ATexCoords: TSkPatchTexCoords;
  const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_patch(GetSelf, @sk_point_t(ACubics[0]), @AColors[0], @sk_point_t(ATexCoords[0]), sk_blendmode_t(ABlendMode), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawPath(const APath: ISkPath; const APaint: ISkPaint);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_path(GetSelf, TSkBindings.GetSelf(APath), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture;
  const AMatrix: TMatrix; const APaint: ISkPaint);
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  TSkiaAPI.sk4d_canvas_draw_picture(GetSelf, TSkBindings.GetSelf(APicture), @sk_matrix_t(AMatrix), TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawPicture(const APicture: ISkPicture;
  const APaint: ISkPaint);
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  TSkiaAPI.sk4d_canvas_draw_picture(GetSelf, TSkBindings.GetSelf(APicture), nil, TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.DrawPoint(const AX, AY: Single; const APaint: ISkPaint);
begin
  DrawPoint(TPointF.Create(AX, AY), APaint);
end;

procedure TSkCanvas.DrawPoint(const APoint: TPointF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_point(GetSelf, @sk_point_t(APoint), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawPoints(const AMode: TSkDrawPointsMode;
  const APoints: TArray<TPointF>; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  if Length(APoints) > 0 then
    TSkiaAPI.sk4d_canvas_draw_points(GetSelf, sk_drawpointsmode_t(AMode), Length(APoints), @sk_point_t(APoints[0]), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawRect(const ARect: TRectF; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_rect(GetSelf, @sk_rect_t(ARect), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawRegion(const ARegion: ISkRegion;
  const APaint: ISkPaint);
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_region(GetSelf, TSkBindings.GetSelf(ARegion), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawRoundRect(const ARoundRect: ISkRoundRect;
  const APaint: ISkPaint);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_rrect(GetSelf, TSkBindings.GetSelf(ARoundRect), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawRoundRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single; const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_rrect2(GetSelf, @sk_rect_t(ARect), ARadiusX, ARadiusY, TSkBindings.GetSelf(APaint));
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
  TSkiaAPI.sk4d_canvas_draw_rrect_difference(GetSelf, TSkBindings.GetSelf(AOuter), TSkBindings.GetSelf(AInner), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawSimpleText(const AText: string; const AX, AY: Single;
  const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  if not AText.IsEmpty then
    TSkiaAPI.sk4d_canvas_draw_simple_text(GetSelf, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, AX, AY, TSkBindings.GetSelf(AFont), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawSimpleTextGlyphs(const AGlyphs: TArray<Word>; const AX,
  AY: Single; const AFont: ISkFont; const APaint: ISkPaint);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  if Length(AGlyphs) > 0 then
    TSkiaAPI.sk4d_canvas_draw_simple_text(GetSelf, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, AX, AY, TSkBindings.GetSelf(AFont), TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawTextBlob(const ATextBlob: ISkTextBlob; const AX,
  AY: Single; const APaint: ISkPaint);
begin
  if not Assigned(ATextBlob) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextBlob']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_text_blob(GetSelf, TSkBindings.GetSelf(ATextBlob), AX, AY, TSkBindings.GetSelf(APaint));
end;

procedure TSkCanvas.DrawVertices(const AVertices: ISkVertices;
  const ABlendMode: TSkBlendMode; const APaint: ISkPaint);
begin
  if not Assigned(AVertices) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AVertices']);
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_canvas_draw_vertices(GetSelf, TSkBindings.GetSelf(AVertices), sk_blendmode_t(ABlendMode), TSkBindings.GetSelf(APaint));
end;

function TSkCanvas.GetDeviceClipBounds: TRect;
begin
  TSkiaAPI.sk4d_canvas_get_device_clip_bounds(GetSelf, sk_irect_t(Result));
end;

function TSkCanvas.GetLocalClipBounds: TRectF;
begin
  TSkiaAPI.sk4d_canvas_get_local_clip_bounds(GetSelf, sk_rect_t(Result));
end;

function TSkCanvas.GetLocalToDevice: TMatrix3D;
begin
  TSkiaAPI.sk4d_canvas_get_local_to_device(GetSelf, sk_matrix44_t(Result));
end;

function TSkCanvas.GetLocalToDeviceAs3x3: TMatrix;
begin
  TSkiaAPI.sk4d_canvas_get_local_to_device_as_3x3(GetSelf, sk_matrix_t(Result));
end;

function TSkCanvas.GetSaveCount: Integer;
begin
  Result := TSkiaAPI.sk4d_canvas_get_save_count(GetSelf);
end;

function TSkCanvas.QuickReject(const ARect: TRectF): Boolean;
begin
  Result := TSkiaAPI.sk4d_canvas_quick_reject(GetSelf, @sk_rect_t(ARect));
end;

function TSkCanvas.QuickReject(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkiaAPI.sk4d_canvas_quick_reject2(GetSelf, TSkBindings.GetSelf(APath));
end;

procedure TSkCanvas.ResetMatrix;
begin
  TSkiaAPI.sk4d_canvas_reset_matrix(GetSelf);
end;

procedure TSkCanvas.Restore;
begin
  TSkiaAPI.sk4d_canvas_restore(GetSelf);
end;

procedure TSkCanvas.RestoreToCount(const ASaveCount: Integer);
begin
  TSkiaAPI.sk4d_canvas_restore_to_count(GetSelf, ASaveCount);
end;

procedure TSkCanvas.Rotate(const ADegrees, APX, APY: Single);
begin
  TSkiaAPI.sk4d_canvas_rotate2(GetSelf, ADegrees, APX, APY);
end;

procedure TSkCanvas.Rotate(const ADegrees: Single);
begin
  TSkiaAPI.sk4d_canvas_rotate(GetSelf, ADegrees);
end;

procedure TSkCanvas.RotateRadians(const ARadians: Single);
begin
  Rotate(RadToDeg(ARadians));
end;

function TSkCanvas.Save: Integer;
begin
  Result := TSkiaAPI.sk4d_canvas_save(GetSelf);
end;

procedure TSkCanvas.SaveLayer(const ABounds: TRectF; const APaint: ISkPaint);
begin
  TSkiaAPI.sk4d_canvas_save_layer(GetSelf, @sk_rect_t(ABounds), TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.SaveLayer(const APaint: ISkPaint);
begin
  TSkiaAPI.sk4d_canvas_save_layer(GetSelf, nil, TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkCanvas.SaveLayerAlpha(const ABounds: TRectF; const AAlpha: Byte);
begin
  TSkiaAPI.sk4d_canvas_save_layer_alpha(GetSelf, @sk_rect_t(ABounds), AAlpha);
end;

procedure TSkCanvas.SaveLayerAlpha(const AAlpha: Byte);
begin
  TSkiaAPI.sk4d_canvas_save_layer_alpha(GetSelf, nil, AAlpha);
end;

procedure TSkCanvas.Scale(const AScaleX, AScaleY: Single);
begin
  TSkiaAPI.sk4d_canvas_scale(GetSelf, AScaleX, AScaleY);
end;

procedure TSkCanvas.SetMatrix(const AMatrix: TMatrix);
begin
  TSkiaAPI.sk4d_canvas_set_matrix2(GetSelf, @sk_matrix_t(AMatrix));
end;

procedure TSkCanvas.SetMatrix(const AMatrix: TMatrix3D);
begin
  TSkiaAPI.sk4d_canvas_set_matrix(GetSelf, @sk_matrix44_t(AMatrix));
end;

procedure TSkCanvas.Skew(const ASkewX, ASkewY: Single);
begin
  TSkiaAPI.sk4d_canvas_skew(GetSelf, ASkewX, ASkewY);
end;

procedure TSkCanvas.Translate(const ADeltaX, ADeltaY: Single);
begin
  TSkiaAPI.sk4d_canvas_translate(GetSelf, ADeltaX, ADeltaY);
end;

{ TSkCodec }

class procedure TSkCodec.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_codec_destroy(AHandle);
end;

function TSkCodec.GetDimensions: TSize;
begin
  TSkiaAPI.sk4d_codec_get_dimensions(GetSelf, sk_isize_t(Result));
end;

function TSkCodec.GetHeight: Integer;
begin
  Result := GetDimensions.cy;
end;

function TSkCodec.GetImage(const AColorType: TSkColorType;
  const AAlphaType: TSkAlphaType; const AColorSpace: ISkColorSpace): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_codec_get_image(GetSelf, sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeGetSelf(AColorSpace)));
end;

function TSkCodec.GetPixels(const APixels: Pointer; const ARowBytes: NativeUInt;
  const AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  const AColorSpace: ISkColorSpace): Boolean;
begin
  Result := TSkiaAPI.sk4d_codec_get_pixels(GetSelf, APixels, ARowBytes, sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeGetSelf(AColorSpace));
end;

function TSkCodec.GetWidth: Integer;
begin
  Result := GetDimensions.cx;
end;

class function TSkCodec.MakeFromFile(const AFileName: string): ISkCodec;
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkCodec>(TSkiaAPI.sk4d_codec_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkCodec.MakeWithCopy(const AData: Pointer;
  const ASize: NativeUInt): ISkCodec;
begin
  if ASize = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkCodec>(TSkiaAPI.sk4d_codec_make_with_copy(AData, ASize));
end;

class function TSkCodec.MakeWithoutCopy(const AData: Pointer;
  const ASize: NativeUInt): ISkCodec;
begin
  if ASize = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkCodec>(TSkiaAPI.sk4d_codec_make_without_copy(AData, ASize));
end;

{ TSkAnimationCodecPlayer }

class procedure TSkAnimationCodecPlayer.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_animcodecplayer_destroy(AHandle);
end;

function TSkAnimationCodecPlayer.GetDimensions: TSize;
begin
  TSkiaAPI.sk4d_animcodecplayer_get_dimensions(GetSelf, sk_isize_t(Result));
end;

function TSkAnimationCodecPlayer.GetDuration: Cardinal;
begin
  Result := TSkiaAPI.sk4d_animcodecplayer_get_duration(GetSelf);
end;

function TSkAnimationCodecPlayer.GetFrame: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_animcodecplayer_get_frame(GetSelf));
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
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkAnimationCodecPlayer>(TSkiaAPI.sk4d_animcodecplayer_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkAnimationCodecPlayer.MakeFromStream(
  const AStream: TStream): ISkAnimationCodecPlayer;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkAnimationCodecPlayer>(TSkiaAPI.sk4d_animcodecplayer_make_from_stream(TSkBindings.RevokeOwnership(LStream)));
end;

function TSkAnimationCodecPlayer.Seek(const AMilliseconds: Cardinal): Boolean;
begin
  Result := TSkiaAPI.sk4d_animcodecplayer_seek(GetSelf, AMilliseconds);
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
  Result := (AHighContrastConfig1.Grayscale   = AHighContrastConfig2.Grayscale  ) and
            (AHighContrastConfig1.InvertStyle = AHighContrastConfig2.InvertStyle) and
            (SameValue(AHighContrastConfig1.Contrast, AHighContrastConfig2.Contrast, Epsilon));
end;

class operator TSkHighContrastConfig.NotEqual(const AHighContrastConfig1,
  AHighContrastConfig2: TSkHighContrastConfig): Boolean;
begin
  Result := not (AHighContrastConfig1 <> AHighContrastConfig2);
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
  Result := (SameValue(AColorMatrix1.M11, AColorMatrix2.M11, Epsilon)) and
            (SameValue(AColorMatrix1.M12, AColorMatrix2.M12, Epsilon)) and
            (SameValue(AColorMatrix1.M13, AColorMatrix2.M13, Epsilon)) and
            (SameValue(AColorMatrix1.M14, AColorMatrix2.M14, Epsilon)) and
            (SameValue(AColorMatrix1.M15, AColorMatrix2.M15, Epsilon)) and
            (SameValue(AColorMatrix1.M21, AColorMatrix2.M21, Epsilon)) and
            (SameValue(AColorMatrix1.M22, AColorMatrix2.M22, Epsilon)) and
            (SameValue(AColorMatrix1.M23, AColorMatrix2.M23, Epsilon)) and
            (SameValue(AColorMatrix1.M24, AColorMatrix2.M24, Epsilon)) and
            (SameValue(AColorMatrix1.M25, AColorMatrix2.M25, Epsilon)) and
            (SameValue(AColorMatrix1.M31, AColorMatrix2.M31, Epsilon)) and
            (SameValue(AColorMatrix1.M32, AColorMatrix2.M32, Epsilon)) and
            (SameValue(AColorMatrix1.M33, AColorMatrix2.M33, Epsilon)) and
            (SameValue(AColorMatrix1.M34, AColorMatrix2.M34, Epsilon)) and
            (SameValue(AColorMatrix1.M35, AColorMatrix2.M35, Epsilon)) and
            (SameValue(AColorMatrix1.M41, AColorMatrix2.M41, Epsilon)) and
            (SameValue(AColorMatrix1.M42, AColorMatrix2.M42, Epsilon)) and
            (SameValue(AColorMatrix1.M43, AColorMatrix2.M43, Epsilon)) and
            (SameValue(AColorMatrix1.M44, AColorMatrix2.M44, Epsilon)) and
            (SameValue(AColorMatrix1.M45, AColorMatrix2.M45, Epsilon));
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

{ TSkColorFilter }

class function TSkColorFilter.MakeBlend(const AColor: TAlphaColor;
  const AMode: TSkBlendMode): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_blend(AColor, sk_blendmode_t(AMode)));
end;

class function TSkColorFilter.MakeCompose(const AOuter,
  AInner: ISkColorFilter): ISkColorFilter;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_compose(TSkBindings.GetSelf(AOuter), TSkBindings.GetSelf(AInner)));
end;

class function TSkColorFilter.MakeHighContrast(
  const AConfig: TSkHighContrastConfig): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_high_contrast(@sk_highcontrastconfig_t(AConfig)));
end;

class function TSkColorFilter.MakeHSLAMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_hsla_matrix(@sk_colormatrix_t(AMatrix)));
end;

class function TSkColorFilter.MakeLighting(const AMultiply,
  AAdd: TAlphaColor): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_lighting(AMultiply, AAdd));
end;

class function TSkColorFilter.MakeLinearToSRGBGamma: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_linear_to_srgb_gamma());
end;

class function TSkColorFilter.MakeLumaColor: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_luma_color());
end;

class function TSkColorFilter.MakeMatrix(
  const AMatrix: TSkColorMatrix): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_matrix(@sk_colormatrix_t(AMatrix)));
end;

class function TSkColorFilter.MakeOverdraw(
  const AColors: TSkOverdrawColor): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_overdraw(@AColors));
end;

class function TSkColorFilter.MakeTable(const ATableA, ATableR, ATableG,
  ATableB: TSkTableFilter): ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_colorfilter_make_table(@ATableA, @ATableR, @ATableG, @ATableB));
end;

class function TSkColorFilter.MakeTable(
  const ATable: TSkTableFilter): ISkColorFilter;
begin
  Result := MakeTable(ATable, ATable, ATable, ATable);
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

class operator TSkColorSpaceXyz.Equal(const AColorSpaceXyz1,
  AColorSpaceXyz2: TSkColorSpaceXyz): Boolean;
begin
  Result := (SameValue(AColorSpaceXyz1.M11, AColorSpaceXyz2.M11, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M12, AColorSpaceXyz2.M12, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M13, AColorSpaceXyz2.M13, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M21, AColorSpaceXyz2.M21, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M22, AColorSpaceXyz2.M22, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M23, AColorSpaceXyz2.M23, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M31, AColorSpaceXyz2.M31, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M32, AColorSpaceXyz2.M32, Epsilon)) and
            (SameValue(AColorSpaceXyz1.M33, AColorSpaceXyz2.M33, Epsilon));
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
  Result := (SameValue(AColorSpaceTransferFunction1.G, AColorSpaceTransferFunction2.G, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.A, AColorSpaceTransferFunction2.A, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.B, AColorSpaceTransferFunction2.B, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.C, AColorSpaceTransferFunction2.C, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.D, AColorSpaceTransferFunction2.D, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.E, AColorSpaceTransferFunction2.E, Epsilon)) and
            (SameValue(AColorSpaceTransferFunction1.F, AColorSpaceTransferFunction2.F, Epsilon));
end;

function TSkColorSpaceTransferFunction.Invert(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspacetransferfn_invert(@sk_colorspacetransferfn_t(Self), sk_colorspacetransferfn_t(ATransferFunction));
end;

class operator TSkColorSpaceTransferFunction.NotEqual(
  const AColorSpaceTransferFunction1,
  AColorSpaceTransferFunction2: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := not (AColorSpaceTransferFunction1 = AColorSpaceTransferFunction2);
end;

function TSkColorSpaceTransferFunction.Transform(const AX: Single): Single;
begin
  Result := TSkiaAPI.sk4d_colorspacetransferfn_transform(@sk_colorspacetransferfn_t(Self), AX);
end;

{ TSkColorSpace }

function TSkColorSpace.GammaCloseToSRGB: Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspace_gamma_close_to_srgb(GetSelf);
end;

function TSkColorSpace.GammaIsLinear: Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspace_gamma_is_linear(GetSelf);
end;

function TSkColorSpace.IsEqual(const AColorSpace: ISkColorSpace): Boolean;
begin
  if not Assigned(AColorSpace) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorSpace']);
  Result := TSkiaAPI.sk4d_colorspace_is_equal(GetSelf, TSkBindings.GetSelf(AColorSpace));
end;

function TSkColorSpace.IsNumericalTransferFunction(
  out ATransferFunction: TSkColorSpaceTransferFunction): Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspace_is_numerical_transfer_fn(GetSelf, sk_colorspacetransferfn_t(ATransferFunction));
end;

function TSkColorSpace.IsSRGB: Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspace_is_srgb(GetSelf);
end;

class function TSkColorSpace.Make(
  const AProfile: ISkColorSpaceICCProfile): ISkColorSpace;
begin
  if not Assigned(AProfile) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AProfile']);
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make(TSkBindings.GetSelf(AProfile)));
end;

function TSkColorSpace.MakeLinearGamma: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make_linear_gamma(GetSelf));
end;

class function TSkColorSpace.MakeRGB(
  const ATransferFunction: TSkColorSpaceTransferFunction;
  const AToXyzD50: TSkColorSpaceXyz): ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make_rgb(@sk_colorspacetransferfn_t(ATransferFunction), @sk_colorspacexyz_t(AToXyzD50)));
end;

class function TSkColorSpace.MakeSRGB: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make_srgb());
end;

function TSkColorSpace.MakeSRGBGamma: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make_srgb_gamma(GetSelf));
end;

class function TSkColorSpace.MakeSRGBLinear: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_colorspace_make_srgb_linear());
end;

class procedure TSkColorSpace.RefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_colorspace_ref(AHandle);
end;

function TSkColorSpace.ToProfile: ISkColorSpaceICCProfile;
begin
  Result := TSkColorSpaceICCProfile.Wrap(TSkiaAPI.sk4d_colorspace_to_profile(GetSelf));
end;

function TSkColorSpace.ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspace_to_xyz(GetSelf, sk_colorspacexyz_t(ADest));
end;

class procedure TSkColorSpace.UnrefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_colorspace_unref(AHandle);
end;

{ TSkColorSpaceICCProfile }

class procedure TSkColorSpaceICCProfile.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_colorspaceiccprofile_destroy(AHandle);
end;

class function TSkColorSpaceICCProfile.MakeFromBytes(
  const ABytes: TBytes): ISkColorSpaceICCProfile;
begin
  if Length(ABytes) < 1 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkColorSpaceICCProfile>(TSkiaAPI.sk4d_colorspaceiccprofile_make_with_parse(@ABytes[0], Length(ABytes)));
end;

function TSkColorSpaceICCProfile.ToBytes: TBytes;
var
  LBuffer: Pointer;
  LSize: Cardinal;
begin
  LBuffer := TSkiaAPI.sk4d_colorspaceiccprofile_get_buffer(GetSelf, @LSize);
  SetLength(Result, LSize);
  if Length(Result) > 0 then
    Move(LBuffer^, Result[0], Length(Result));
end;

function TSkColorSpaceICCProfile.ToXyz(out ADest: TSkColorSpaceXyz): Boolean;
begin
  Result := TSkiaAPI.sk4d_colorspaceiccprofile_to_xyz(GetSelf, sk_colorspacexyz_t(ADest));
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
  Result := (SameValue(AColorSpacePrimaries1.RX, AColorSpacePrimaries2.RX, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.RY, AColorSpacePrimaries2.RY, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.GX, AColorSpacePrimaries2.GX, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.GY, AColorSpacePrimaries2.GY, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.BX, AColorSpacePrimaries2.BX, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.BY, AColorSpacePrimaries2.BY, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.WX, AColorSpacePrimaries2.WX, Epsilon)) and
            (SameValue(AColorSpacePrimaries1.WY, AColorSpacePrimaries2.WY, Epsilon));
end;

class operator TSkColorSpacePrimaries.Implicit(
  const AColorSpacePrimaries: TSkColorSpacePrimaries): TSkColorSpaceXyz;
begin
  if not TSkiaAPI.sk4d_colorspaceprimaries_to_xyz(@sk_colorspaceprimaries_t(AColorSpacePrimaries), sk_colorspacexyz_t(Result)) then
    Result := TSkColorSpaceXyz.Identity;
end;

class operator TSkColorSpacePrimaries.NotEqual(const AColorSpacePrimaries1,
  AColorSpacePrimaries2: TSkColorSpacePrimaries): Boolean;
begin
  Result := not (AColorSpacePrimaries1 = AColorSpacePrimaries2);
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
            (SameValue(APDFMetadata1.RasterDPI, APDFMetadata2.RasterDPI, Epsilon));
end;

class operator TSkPDFMetadata.NotEqual(const APDFMetadata1,
  APDFMetadata2: TSkPDFMetadata): Boolean;
begin
  Result := not (APDFMetadata1 = APDFMetadata2);
end;

{ TSkDocument }

function TSkDocument.BeginPage(const AWidth, AHeight: Single): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(TSkiaAPI.sk4d_document_begin_page(GetSelf, AWidth, AHeight, nil), False);
end;

function TSkDocument.BeginPage(const AWidth, AHeight: Single;
  const AContent: TRectF): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(TSkiaAPI.sk4d_document_begin_page(GetSelf, AWidth, AHeight, @sk_rect_t(AContent)), False);
end;

procedure TSkDocument.Close;
begin
  TSkiaAPI.sk4d_document_close(GetSelf);
end;

procedure TSkDocument.EndPage;
begin
  TSkiaAPI.sk4d_document_end_page(GetSelf);
end;

class function TSkDocument.MakePDF(const AStream: TStream): ISkDocument;
begin
  Result := TSkManagedDocument.MakePDF(AStream);
end;

class function TSkDocument.MakePDF(const AStream: TStream;
  const AMetadata: TSkPDFMetadata): ISkDocument;
begin
  Result := TSkManagedDocument.MakePDF(AStream, AMetadata);
end;

class function TSkDocument.MakeXPS(const AStream: TStream;
  const ADPI: Single): ISkDocument;
begin
  Result := TSkManagedDocument.MakeXPS(AStream, ADPI);
end;

procedure TSkDocument.Terminate;
begin
  TSkiaAPI.sk4d_document_terminate(GetSelf);
end;

{ TSkFontMetrics }

class operator TSkFontMetrics.Equal(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
begin
  Result := (AFontMetrics1.Flags = AFontMetrics2.Flags) and
            (SameValue(AFontMetrics1.Top,                AFontMetrics2.Top,                Epsilon)) and
            (SameValue(AFontMetrics1.Ascent,             AFontMetrics2.Ascent,             Epsilon)) and
            (SameValue(AFontMetrics1.Descent,            AFontMetrics2.Descent,            Epsilon)) and
            (SameValue(AFontMetrics1.Bottom,             AFontMetrics2.Bottom,             Epsilon)) and
            (SameValue(AFontMetrics1.Leading,            AFontMetrics2.Leading,            Epsilon)) and
            (SameValue(AFontMetrics1.AvgCharWidth,       AFontMetrics2.AvgCharWidth,       Epsilon)) and
            (SameValue(AFontMetrics1.MaxCharWidth,       AFontMetrics2.MaxCharWidth,       Epsilon)) and
            (SameValue(AFontMetrics1.XMin,               AFontMetrics2.XMin,               Epsilon)) and
            (SameValue(AFontMetrics1.XMax,               AFontMetrics2.XMax,               Epsilon)) and
            (SameValue(AFontMetrics1.XMax,               AFontMetrics2.XMax,               Epsilon)) and
            (SameValue(AFontMetrics1.XHeight,            AFontMetrics2.XHeight,            Epsilon)) and
            (SameValue(AFontMetrics1.UnderlineThickness, AFontMetrics2.UnderlineThickness, Epsilon)) and
            (SameValue(AFontMetrics1.UnderlinePosition,  AFontMetrics2.UnderlinePosition,  Epsilon)) and
            (SameValue(AFontMetrics1.StrikeoutThickness, AFontMetrics2.StrikeoutThickness, Epsilon)) and
            (SameValue(AFontMetrics1.StrikeoutPosition,  AFontMetrics2.StrikeoutPosition,  Epsilon));
end;

class operator TSkFontMetrics.NotEqual(const AFontMetrics1,
  AFontMetrics2: TSkFontMetrics): Boolean;
begin
  Result := not (AFontMetrics1 = AFontMetrics2);
end;

{ TSkFont }

constructor TSkFont.Create(const AFont: ISkFont);
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  inherited Wrap(TSkiaAPI.sk4d_font_create2(TSkBindings.GetSelf(AFont)));
end;

constructor TSkFont.Create(ATypeface: ISkTypeface; const ASize, AScaleX,
  ASkewX: Single);
begin
  inherited Wrap(TSkiaAPI.sk4d_font_create(TSkBindings.SafeGetSelf(ATypeface), ASize, AScaleX, ASkewX));
end;

class procedure TSkFont.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_font_destroy(AHandle);
end;

function TSkFont.GetBaselineSnap: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_baseline_snap(GetSelf);
end;

function TSkFont.GetBounds(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<TRectF>;
begin
  if Length(AGlyphs) = 0 then
    Exit(nil);
  SetLength(Result, Length(AGlyphs));
  TSkiaAPI.sk4d_font_get_widths_bounds(GetSelf, @AGlyphs[0], Length(AGlyphs), nil, @sk_rect_t(Result[0]), TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.GetEdging: TSkFontEdging;
begin
  Result := TSkFontEdging(TSkiaAPI.sk4d_font_get_edging(GetSelf));
end;

function TSkFont.GetEmbeddedBitmaps: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_embedded_bitmaps(GetSelf);
end;

function TSkFont.GetEmbolden: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_embolden(GetSelf);
end;

function TSkFont.GetForceAutoHinting: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_force_auto_hinting(GetSelf);
end;

function TSkFont.GetGlyphs(const AText: string): TArray<Word>;
var
  LCount: Integer;
begin
  if AText.IsEmpty then
    Exit(nil);
  LCount := TSkiaAPI.sk4d_font_get_glyphs_count(GetSelf, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING);
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_font_get_glyphs(GetSelf, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, @Result[0], LCount);
end;

function TSkFont.GetHinting: TSkFontHinting;
begin
  Result := TSkFontHinting(TSkiaAPI.sk4d_font_get_hinting(GetSelf));
end;

function TSkFont.GetHorizontalPositions(const AGlyphs: TArray<Word>;
  const AOrigin: Single): TArray<Single>;
begin
  if Length(AGlyphs) = 0 then
    Exit(nil);
  SetLength(Result, Length(AGlyphs));
  TSkiaAPI.sk4d_font_get_horizontal_positions(GetSelf, @AGlyphs[0], Length(AGlyphs), @Result[0], AOrigin);
end;

function TSkFont.GetIntercepts(const AGlyphs: TArray<Word>;
  const APositions: TArray<TPointF>; const AUpperBounds, ALowerBounds: Single;
  const APaint: ISkPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
  LCount: Integer;
begin
  if Length(AGlyphs) = 0 then
    Exit(nil);
  if Length(APositions) <> Length(AGlyphs) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  LCount := TSkiaAPI.sk4d_textblob_get_intercepts(GetSelf, @LBounds[0], nil, TSkBindings.SafeGetSelf(APaint));
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_font_get_intercepts(GetSelf, @AGlyphs[0], Length(AGlyphs), @sk_point_t(APositions[0]), @LBounds[0], @Result[0], TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.GetLinearMetrics: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_linear_metrics(GetSelf);
end;

function TSkFont.GetMetrics(out AMetrics: TSkFontMetrics): Single;
var
  LMetrics: sk_fontmetrics_t;
begin
  Result   := TSkiaAPI.sk4d_font_get_metrics(GetSelf, @LMetrics);
  AMetrics := TSkMapping.ToFontMetrics(LMetrics);
end;

function TSkFont.GetPath(const AGlyph: Word): ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_font_get_path(GetSelf, AGlyph));
end;

procedure TSkFont.GetPaths(const AGlyphs: TArray<Word>;
  const AProc: TSkFontPathProc);
begin
  if (Length(AGlyphs) > 0) and (Assigned(AProc)) then
    TSkiaAPI.sk4d_font_get_paths(GetSelf, @AGlyphs[0], Length(AGlyphs), path_proc, @AProc);
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>;
  const AOrigin: TPointF): TArray<TPointF>;
begin
  if Length(AGlyphs) = 0 then
    Exit(nil);
  SetLength(Result, Length(AGlyphs));
  TSkiaAPI.sk4d_font_get_positions(GetSelf, @AGlyphs[0], Length(AGlyphs), @sk_point_t(Result[0]), @sk_point_t(AOrigin));
end;

function TSkFont.GetPositions(const AGlyphs: TArray<Word>): TArray<TPointF>;
begin
  Result := GetPositions(AGlyphs, TPointF.Create(0, 0));
end;

function TSkFont.GetScaleX: Single;
begin
  Result := TSkiaAPI.sk4d_font_get_scale_x(GetSelf);
end;

function TSkFont.GetSize: Single;
begin
  Result := TSkiaAPI.sk4d_font_get_size(GetSelf);
end;

function TSkFont.GetSkewX: Single;
begin
  Result := TSkiaAPI.sk4d_font_get_skew_x(GetSelf);
end;

function TSkFont.GetSpacing: Single;
begin
  Result := TSkiaAPI.sk4d_font_get_metrics(GetSelf, nil);
end;

function TSkFont.GetSubpixel: Boolean;
begin
  Result := TSkiaAPI.sk4d_font_get_subpixel(GetSelf);
end;

function TSkFont.GetTypeface: ISkTypeface;
begin
  Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_font_get_typeface(GetSelf));
end;

function TSkFont.GetTypefaceOrDefault: ISkTypeface;
begin
  Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_font_get_typeface_or_default(GetSelf));
end;

function TSkFont.GetWidths(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): TArray<Single>;
begin
  if Length(AGlyphs) = 0 then
    Exit(nil);
  SetLength(Result, Length(AGlyphs));
  TSkiaAPI.sk4d_font_get_widths_bounds(GetSelf, @AGlyphs[0], Length(AGlyphs), @Result[0], nil, TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkFont.GetWidthsAndBounds(const AGlyphs: TArray<Word>;
  out AWidths: TArray<Single>; out ABounds: TArray<TRectF>;
  const APaint: ISkPaint);
begin
  SetLength(AWidths, Length(AGlyphs));
  SetLength(ABounds, Length(AGlyphs));
  if Length(AGlyphs) = 0 then
    Exit;
  TSkiaAPI.sk4d_font_get_widths_bounds(GetSelf, @AGlyphs[0], Length(AGlyphs), @AWidths[0], @sk_rect_t(ABounds[0]), TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.IsEqual(const AFont: ISkFont): Boolean;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  Result := TSkiaAPI.sk4d_font_is_equal(GetSelf, TSkBindings.GetSelf(AFont));
end;

function TSkFont.MakeWithSize(const ASize: Single): ISkFont;
begin
  Result := TSkFont.Create(Self);
  Result.Size := ASize;
end;

function TSkFont.MeasureText(const AText: string; out ABounds: TRectF;
  const APaint: ISkPaint): Single;
begin
  if AText.IsEmpty then
  begin
    ABounds := TRectF.Empty;
    Exit(0);
  end;
  Result := TSkiaAPI.sk4d_font_measure_text(GetSelf, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, @sk_rect_t(ABounds), TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.MeasureText(const AText: string;
  const APaint: ISkPaint): Single;
begin
  if AText.IsEmpty then
    Exit(0);
  Result := TSkiaAPI.sk4d_font_measure_text(GetSelf, @AText[Low(AText)], Length(AText) * 2, sk_textencoding_t.UTF16_SK_TEXTENCODING, nil, TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  const APaint: ISkPaint): Single;
begin
  if Length(AGlyphs) = 0 then
    Exit(0);
  Result := TSkiaAPI.sk4d_font_measure_text(GetSelf, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, nil, TSkBindings.SafeGetSelf(APaint));
end;

function TSkFont.MeasureTextGlyphs(const AGlyphs: TArray<Word>;
  out ABounds: TRectF; const APaint: ISkPaint): Single;
begin
  if Length(AGlyphs) = 0 then
  begin
    ABounds := TRectF.Empty;
    Exit(0);
  end;
  Result := TSkiaAPI.sk4d_font_measure_text(GetSelf, @AGlyphs[Low(AGlyphs)], Length(AGlyphs) * SizeOf(Word), sk_textencoding_t.GLYPH_ID_SK_TEXTENCODING, @sk_rect_t(ABounds), TSkBindings.SafeGetSelf(APaint));
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
  TSkiaAPI.sk4d_font_set_baseline_snap(GetSelf, AValue);
end;

procedure TSkFont.SetEdging(const AValue: TSkFontEdging);
begin
  TSkiaAPI.sk4d_font_set_edging(GetSelf, sk_fontedging_t(AValue));
end;

procedure TSkFont.SetEmbeddedBitmaps(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_font_set_embedded_bitmaps(GetSelf, AValue);
end;

procedure TSkFont.SetEmbolden(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_font_set_embolden(GetSelf, AValue);
end;

procedure TSkFont.SetForceAutoHinting(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_font_set_force_auto_hinting(GetSelf, AValue);
end;

procedure TSkFont.SetHinting(const AValue: TSkFontHinting);
begin
  TSkiaAPI.sk4d_font_set_hinting(GetSelf, sk_fonthinting_t(AValue));
end;

procedure TSkFont.SetLinearMetrics(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_font_set_linear_metrics(GetSelf, AValue);
end;

procedure TSkFont.SetScaleX(const AValue: Single);
begin
  TSkiaAPI.sk4d_font_set_scale_x(GetSelf, AValue);
end;

procedure TSkFont.SetSize(const AValue: Single);
begin
  TSkiaAPI.sk4d_font_set_size(GetSelf, AValue);
end;

procedure TSkFont.SetSkewX(const AValue: Single);
begin
  TSkiaAPI.sk4d_font_set_skew_x(GetSelf, AValue);
end;

procedure TSkFont.SetSubpixel(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_font_set_subpixel(GetSelf, AValue);
end;

procedure TSkFont.SetTypeface(AValue: ISkTypeface);
begin
  TSkiaAPI.sk4d_font_set_typeface(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

function TSkFont.UnicharsToGlyphs(
  const AUnichars: TArray<Integer>): TArray<Word>;
begin
  if Length(AUnichars) = 0 then
    Exit(nil);
  SetLength(Result, Length(AUnichars));
  TSkiaAPI.sk4d_font_unichars_to_glyphs(GetSelf, @AUnichars[0], Length(AUnichars), @Result[0]);
end;

function TSkFont.UnicharToGlyph(const AUnichar: Integer): Word;
begin
  Result := TSkiaAPI.sk4d_font_unichar_to_glyph(GetSelf, AUnichar);
end;

{ TSkGraphics }

class procedure TSkGraphics.AllowJIT;
begin
  TSkiaAPI.sk4d_graphics_allow_jit();
end;

class procedure TSkGraphics.DumpMemoryStatistics(
  const ATraceMemoryDump: ISkTraceMemoryDump);
begin
  if not Assigned(ATraceMemoryDump) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATraceMemoryDump']);
  TSkiaAPI.sk4d_graphics_dump_memory_statistics(TSkBindings.GetSelf(ATraceMemoryDump));
end;

class function TSkGraphics.GetFontCacheCountLimit: Integer;
begin
  Result := TSkiaAPI.sk4d_graphics_get_font_cache_count_limit();
end;

class function TSkGraphics.GetFontCacheCountUsed: Integer;
begin
  Result := TSkiaAPI.sk4d_graphics_get_font_cache_count_used();
end;

class function TSkGraphics.GetFontCacheLimit: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_graphics_get_font_cache_limit();
end;

class function TSkGraphics.GetFontCacheUsed: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_graphics_get_font_cache_used();
end;

class function TSkGraphics.GetResourceCacheSingleAllocationByteLimit: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_graphics_get_resource_cache_single_allocation_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalByteLimit: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_graphics_get_resource_cache_total_byte_limit();
end;

class function TSkGraphics.GetResourceCacheTotalBytesUsed: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_graphics_get_resource_cache_total_bytes_used();
end;

class procedure TSkGraphics.Init;
begin
  TSkiaAPI.sk4d_graphics_init();
end;

class procedure TSkGraphics.PurgeAllCaches;
begin
  TSkiaAPI.sk4d_graphics_purge_all_caches();
end;

class procedure TSkGraphics.PurgeFontCache;
begin
  TSkiaAPI.sk4d_graphics_purge_font_cache();
end;

class procedure TSkGraphics.PurgeResourceCache;
begin
  TSkiaAPI.sk4d_graphics_purge_resource_cache();
end;

class procedure TSkGraphics.SetFontCacheCountLimit(const AValue: Integer);
begin
  TSkiaAPI.sk4d_graphics_set_font_cache_count_limit(AValue);
end;

class procedure TSkGraphics.SetFontCacheLimit(const AValue: NativeUInt);
begin
  TSkiaAPI.sk4d_graphics_set_font_cache_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheSingleAllocationByteLimit(
  const AValue: NativeUInt);
begin
  TSkiaAPI.sk4d_graphics_set_resource_cache_single_allocation_byte_limit(AValue);
end;

class procedure TSkGraphics.SetResourceCacheTotalByteLimit(
  const AValue: NativeUInt);
begin
  TSkiaAPI.sk4d_graphics_set_resource_cache_total_byte_limit(AValue);
end;

{ TSkEncodedImageFormatHelper }

class function TSkEncodedImageFormatHelper.FromExtension(
  const AExtension: string): TSkEncodedImageFormat;
begin
  if (string.Compare('.jpg', AExtension) = 0) or (string.Compare('.jpeg', AExtension) = 0) then
    Result := TSkEncodedImageFormat.JPEG
  else if string.Compare('.webp', AExtension) = 0 then
    Result := TSkEncodedImageFormat.WEBP
  else if string.Compare('.png', AExtension) = 0 then
    Result := TSkEncodedImageFormat.PNG
  else
    raise ESkArgumentException.Create(SInvalidExtension);
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
    TSkColorType.RGF16,
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

{ TSkImage }

function TSkImage.Encode(const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create(nil);
  try
    EncodeToStream(LBytesStream, AEncodedImageFormat, AQuality);
    Result := LBytesStream.Bytes;
  finally
    LBytesStream.Free;
  end;
end;

procedure TSkImage.EncodeToFile(const AFileName: string;
  const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer);
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  TSkiaAPI.sk4d_image_encode_to_file(GetSelf, MarshaledAString(UTF8String(AFileName)), sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

procedure TSkImage.EncodeToFile(const AFileName: string;
  const AQuality: Integer);
begin
  EncodeToFile(AFileName, TSkEncodedImageFormat.FromExtension(TPath.GetExtension(AFileName)), AQuality);
end;

procedure TSkImage.EncodeToStream(const AStream: TStream;
  const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer);
var
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  TSkiaAPI.sk4d_image_encode_to_stream(GetSelf, LWStream.Handle, sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

function TSkImage.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(TSkiaAPI.sk4d_image_get_alpha_type(GetSelf));
end;

function TSkImage.GetColorSpace: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_image_get_color_space(GetSelf));
end;

function TSkImage.GetColorType: TSkColorType;
begin
  Result := TSkColorType(TSkiaAPI.sk4d_image_get_color_type(GetSelf));
end;

function TSkImage.GetHeight: Integer;
begin
  Result := TSkiaAPI.sk4d_image_get_height(GetSelf);
end;

function TSkImage.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  TSkiaAPI.sk4d_image_get_image_info(GetSelf, LResult);
  Result := TSkMapping.ToImageInfo(LResult);
end;

function TSkImage.GetUniqueId: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_image_get_unique_id(GetSelf);
end;

function TSkImage.GetWidth: Integer;
begin
  Result := TSkiaAPI.sk4d_image_get_width(GetSelf);
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
  Result := TSkiaAPI.sk4d_image_is_lazy_generated(GetSelf);
end;

function TSkImage.IsOpaque: Boolean;
begin
  Result := GetAlphaType = TSkAlphaType.Opaque;
end;

function TSkImage.IsTextureBacked: Boolean;
begin
  Result := TSkiaAPI.sk4d_image_is_texture_backed(GetSelf);
end;

function TSkImage.IsValid(AContext: IGrDirectContext): Boolean;
begin
  Result := TSkiaAPI.sk4d_image_is_valid(GetSelf, TSkBindings.SafeGetSelf(AContext));
end;

class function TSkImage.MakeFromAdoptedTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  const AColorSpace: ISkColorSpace): ISkImage;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_adopted_texture(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ATexture), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeGetSelf(AColorSpace)));
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
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_encoded_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkImage.MakeFromEncodedStream(const AStream: TStream): ISkImage;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_encoded_stream(LStream.Handle));
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

class function TSkImage.MakeFromRaster(const APixmap: ISkPixmap;
  const ARasterReleaseProc: TSkImageRasterReleaseProc): ISkImage;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  if Assigned(ARasterReleaseProc) then
  begin
    Result := TSkDelegate<TSkImageRasterReleaseProc>.Initialize<ISkImage>(ARasterReleaseProc,
      function (const AContextProc: Pointer): ISkImage
      begin
        Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_raster(TSkBindings.GetSelf(APixmap), raster_release_proc, AContextProc));
      end);
  end
  else
    Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_raster(TSkBindings.GetSelf(APixmap), nil, nil));
end;

class function TSkImage.MakeFromTexture(const AContext: IGrDirectContext;
  const ATexture: IGrBackendTexture; const AOrigin: TGrSurfaceOrigin;
  AColorType: TSkColorType; const AAlphaType: TSkAlphaType;
  const AColorSpace: ISkColorSpace;
  const ATextureReleaseProc: TSkImageTextureReleaseProc): ISkImage;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ATexture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATexture']);
  if Assigned(ATextureReleaseProc) then
  begin
    Result := TSkDelegate<TSkImageTextureReleaseProc>.Initialize<ISkImage>(ATextureReleaseProc,
      function (const AContextProc: Pointer): ISkImage
      begin
        Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_texture(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ATexture), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeGetSelf(AColorSpace), texture_release_proc, AContextProc));
      end);
  end
  else
    Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_from_texture(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ATexture), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), sk_alphatype_t(AAlphaType), TSkBindings.SafeGetSelf(AColorSpace), nil, nil));
end;

function TSkImage.MakeNonTextureImage(out AImage: ISkImage): Boolean;
begin
  AImage := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_non_texture_image(GetSelf));
  Result := Assigned(AImage);
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
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_raster_copy(TSkBindings.GetSelf(APixmap)));
end;

function TSkImage.MakeRasterImage(out AImage: ISkImage): Boolean;
begin
  AImage := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_raster_image(GetSelf));
  Result := Assigned(AImage);
end;

function TSkImage.MakeShader(const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeShader(ALocalMatrix, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeShader(const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := MakeShader(TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ATileModeX, ATileModeY);
end;

function TSkImage.MakeShader(const ALocalMatrix: TMatrix;
  const ASampling: TSkSamplingOptions; const ATileModeX,
  ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_image_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @sk_samplingoptions_t(ASampling), @sk_matrix_t(ALocalMatrix)));
end;

function TSkImage.MakeShader(const ASampling: TSkSamplingOptions;
  const ATileModeX, ATileModeY: TSkTileMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_image_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), @sk_samplingoptions_t(ASampling), nil));
end;

function TSkImage.MakeSubset(const ASubset: TRect;
  AContext: IGrDirectContext): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_subset(GetSelf, @sk_irect_t(ASubset), TSkBindings.SafeGetSelf(AContext)));
end;

function TSkImage.MakeTextureImage(const AContext: IGrDirectContext;
  out AImage: ISkImage; const AIsMipmapped: Boolean): Boolean;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  AImage := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_texture_image(GetSelf, TSkBindings.GetSelf(AContext), AIsMipmapped));
  Result := Assigned(AImage);
end;

function TSkImage.MakeWithFilter(const AFilter: ISkImageFilter; const ASubset,
  AClipBounds: TRect; out AOutSubset: TRect; out AOffset: TPoint;
  AContext: IGrDirectContext): ISkImage;
begin
  if not Assigned(AFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFilter']);
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_image_make_with_filter(GetSelf, TSkBindings.SafeGetSelf(AContext), TSkBindings.GetSelf(AFilter), @sk_irect_t(ASubset), @sk_irect_t(AClipBounds), sk_irect_t(AOutSubset), sk_ipoint_t(AOffset)));
end;

function TSkImage.PeekPixels: ISkPixmap;
begin
  Result := TSkBindings.SafeCreate<TSkPixmap>(TSkiaAPI.sk4d_image_peek_pixels(GetSelf));
end;

class procedure TSkImage.raster_release_proc(const pixels: Pointer;
  context: Pointer);
begin
  TSkDelegate<TSkImageRasterReleaseProc>.Invoke(context,
    procedure (const AProc: TSkImageRasterReleaseProc)
    begin
      AProc(pixels);
    end);
  TSkDelegate<TSkImageRasterReleaseProc>.Finalize(context);
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

function TSkImage.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer; const ACachingHint: TSkImageCachingHint;
  AContext: IGrDirectContext): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := TSkiaAPI.sk4d_image_read_pixels(GetSelf, TSkBindings.SafeGetSelf(AContext), TSkBindings.GetSelf(ADest), ASrcX, ASrcY, sk_imagecachinghint_t(ACachingHint));
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
  Result := TSkiaAPI.sk4d_image_scale_pixels(GetSelf, TSkBindings.GetSelf(ADest), @sk_samplingoptions_t(ASampling), sk_imagecachinghint_t(ACachingHint));
end;

function TSkImage.ScalePixels(const ADest: ISkPixmap;
  const ACachingHint: TSkImageCachingHint): Boolean;
begin
  Result := ScalePixels(ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None), ACachingHint);
end;

class procedure TSkImage.texture_release_proc(context: Pointer);
begin
  TSkDelegate<TSkImageTextureReleaseProc>.Invoke(context,
    procedure (const AProc: TSkImageTextureReleaseProc)
    begin
      AProc();
    end);
  TSkDelegate<TSkImageTextureReleaseProc>.Finalize(context);
end;

{ TSkImageEncoder }

class function TSkImageEncoder.Encode(const ASrc: ISkPixmap;
  const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer): TBytes;
var
  LBytesStream: TBytesStream;
begin
  LBytesStream := TBytesStream.Create(nil);
  try
    EncodeToStream(LBytesStream, ASrc, AEncodedImageFormat, AQuality);
    Result := LBytesStream.Bytes;
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

class procedure TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt; const AQuality: Integer);
begin
  EncodeToFile(AFileName, ASrcImageInfo, ASrcPixels, ASrcRowBytes, TSkEncodedImageFormat.FromExtension(TPath.GetExtension(AFileName)), AQuality);
end;

class procedure TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer);
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  TSkiaAPI.sk4d_imageencoder_encode_to_file(MarshaledAString(UTF8String(AFileName)), TSkBindings.GetSelf(ASrc), sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

class procedure TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrc: ISkPixmap; const AQuality: Integer);
begin
  EncodeToFile(AFileName, ASrc, TSkEncodedImageFormat.FromExtension(TPath.GetExtension(AFileName)), AQuality);
end;

class procedure TSkImageEncoder.EncodeToFile(const AFileName: string;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt;
  const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer);
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  EncodeToFile(AFileName, LPixmap, AEncodedImageFormat, AQuality);
end;

class procedure TSkImageEncoder.EncodeToStream(const AStream: TStream;
  const ASrc: ISkPixmap; const AEncodedImageFormat: TSkEncodedImageFormat;
  const AQuality: Integer);
var
  LWStream: ISkWStream;
begin
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  LWStream := TSkWStreamAdapter.Create(AStream);
  TSkiaAPI.sk4d_imageencoder_encode_to_stream(LWStream.Handle, TSkBindings.GetSelf(ASrc), sk_encodedimageformat_t(AEncodedImageFormat), AQuality);
end;

class procedure TSkImageEncoder.EncodeToStream(const AStream: TStream;
  const ASrcImageInfo: TSkImageInfo; const ASrcPixels: Pointer;
  const ASrcRowBytes: NativeUInt;
  const AEncodedImageFormat: TSkEncodedImageFormat; const AQuality: Integer);
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ASrcImageInfo, ASrcPixels, ASrcRowBytes);
  EncodeToStream(AStream, LPixmap, AEncodedImageFormat, AQuality);
end;

{ TSkImageFilter }

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
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_alpha_threshold(TSkBindings.GetSelf(ARegion), AInnerMin, AOuterMax, TSkBindings.SafeGetSelf(AInput)));
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, TSkBindings.GetSelf(ABackground), TSkBindings.SafeGetSelf(AForeground), nil));
end;

class function TSkImageFilter.MakeArithmetic(const AK1, AK2, AK3, AK4: Single;
  const AEnforcePremultipliedColor: Boolean; const ABackground: ISkImageFilter;
  const ACropRect: TRectF; AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_arithmetic(AK1, AK2, AK3, AK4, AEnforcePremultipliedColor, TSkBindings.GetSelf(ABackground), TSkBindings.SafeGetSelf(AForeground), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground: ISkImageFilter;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), TSkBindings.GetSelf(ABackground), TSkBindings.SafeGetSelf(AForeground), nil));
end;

class function TSkImageFilter.MakeBlend(const AMode: TSkBlendMode;
  const ABackground: ISkImageFilter; const ACropRect: TRectF;
  AForeground: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ABackground) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ABackground']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_blend(sk_blendmode_t(AMode), TSkBindings.GetSelf(ABackground), TSkBindings.SafeGetSelf(AForeground), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  AInput: ISkImageFilter; const ATileMode: TSkTileMode): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeBlur(const ASigmaX, ASigmaY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter;
  const ATileMode: TSkTileMode): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_blur(ASigmaX, ASigmaY, sk_tilemode_t(ATileMode), TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AColorFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorFilter']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_colorfilter(TSkBindings.GetSelf(AColorFilter), TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeColorFilter(
  const AColorFilter: ISkColorFilter; AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AColorFilter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AColorFilter']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_colorfilter(TSkBindings.GetSelf(AColorFilter), TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeCompose(const AOuter,
  AInner: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_compose(TSkBindings.GetSelf(AOuter), TSkBindings.GetSelf(AInner)));
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeDilate(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_dilate(ARadiusX, ARadiusY, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single;
  const ADisplacement: ISkImageFilter; AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ADisplacement) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADisplacement']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, TSkBindings.GetSelf(ADisplacement), TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeDisplacementMap(const AXChannelSelector,
  AYChannelSelector: TSkColorChannel; const AScale: Single;
  const ADisplacement: ISkImageFilter; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  if not Assigned(ADisplacement) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADisplacement']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_displacement_map(sk_colorchannel_t(AXChannelSelector), sk_colorchannel_t(AYChannelSelector), AScale, TSkBindings.GetSelf(ADisplacement), TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_distant_lit_diffuse(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDistantLitDiffuse(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_distant_lit_diffuse(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_distant_lit_specular(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeDistantLitSpecular(const ADirection: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_distant_lit_specular(@sk_point3_t(ADirection), ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_drop_shadow(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDropShadow(const ADeltaX, ADeltaY, ASigmaX,
  ASigmaY: Single; const AColor: TAlphaColor;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_drop_shadow(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADeltaX, ADeltaY,
  ASigmaX, ASigmaY: Single; const AColor: TAlphaColor; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_drop_shadow_only(ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeDropShadowOnly(const ADeltaX, ADeltaY,
  ASigmaX, ASigmaY: Single; const AColor: TAlphaColor;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_drop_shadow_only( ADeltaX, ADeltaY, ASigmaX, ASigmaY, AColor, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeErode(const ARadiusX, ARadiusY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_erode(ARadiusX, ARadiusY, TSkBindings.SafeGetSelf(AInput), nil));
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
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_image(TSkBindings.GetSelf(AImage), @sk_rect_t(ASrc), @sk_rect_t(ADest), @sk_samplingoptions_t(ASampling)));
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; const ACropRect: TRectF;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_magnifier(@sk_rect_t(ASrc), AInset, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeMagnifier(const ASrc: TRectF;
  const AInset: Single; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_magnifier(@sk_rect_t(ASrc), AInset, TSkBindings.SafeGetSelf(AInput), nil));
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
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_matrix_convolution(@sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, @sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
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
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_matrix_convolution(@sk_isize_t(AKernelSize), @AKernel[0], AGain, ABias, @sk_ipoint_t(AKernelOffset), sk_tilemode_t(ATileMode), AConvolveAlpha, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeMatrixTransform(const AMatrix: TMatrix;
  const ASampling: TSkSamplingOptions; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_matrix_transform(@sk_matrix_t(AMatrix), @sk_samplingoptions_t(ASampling), TSkBindings.SafeGetSelf(AInput)));
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
  if Length(AFilters) < 2 then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AFilters']);
  SetLength(LFilters, Length(AFilters));
  for I := 0 to Length(AFilters) - 1 do
  begin
    if not Assigned(AFilters[I]) then
      raise ESkArgumentException.CreateFmt(SParamElementIsNil, ['AFilters', I]);
    LFilters[I] := TSkBindings.GetSelf(AFilters[I]);
  end;
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), @sk_rect_t(ACropRect)));
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
  if Length(AFilters) < 2 then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AFilters']);
  SetLength(LFilters, Length(AFilters));
  for I := 0 to Length(AFilters) - 1 do
  begin
    if not Assigned(AFilters[I]) then
      raise ESkArgumentException.CreateFmt(SParamElementIsNil, ['AFilters', I]);
    LFilters[I] := TSkBindings.GetSelf(AFilters[I]);
  end;
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_merge(@LFilters[0], Length(LFilters), nil));
end;

class function TSkImageFilter.MakeOffset(const ADeltaX, ADeltaY: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_offset(ADeltaX, ADeltaY, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeOffset(const ADeltaX, ADeltaY: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_offset(ADeltaX, ADeltaY, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakePicture(const APicture: ISkPicture;
  const ACropRect: TRectF): ISkImageFilter;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_picture(TSkBindings.GetSelf(APicture), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakePicture(
  const APicture: ISkPicture): ISkImageFilter;
begin
  if not Assigned(APicture) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APicture']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_picture(TSkBindings.GetSelf(APicture), nil));
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_point_lit_diffuse(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakePointLitDiffuse(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_point_lit_diffuse(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_point_lit_specular(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakePointLitSpecular(const ALocation: TPoint3D;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_point_lit_specular(@sk_point3_t(ALocation), ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeShader(const AShader: ISkShader;
  const ADither: Boolean; const ACropRect: TRectF): ISkImageFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_shader(TSkBindings.GetSelf(AShader), ADither, @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeShader(const AShader: ISkShader;
  const ADither: Boolean): ISkImageFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_shader(TSkBindings.GetSelf(AShader), ADither, nil));
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_spot_lit_diffuse(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeSpotLitDiffuse(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKd: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_spot_lit_diffuse(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKd, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_spot_lit_specular(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), nil));
end;

class function TSkImageFilter.MakeSpotLitSpecular(const ALocation,
  ATarget: TPoint3D; const AFalloffExponent, ACutoffAngle: Single;
  const ALightColor: TAlphaColor; const ASurfaceScale, AKs, AShininess: Single;
  const ACropRect: TRectF; AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_spot_lit_specular(@sk_point3_t(ALocation), @sk_point3_t(ATarget), AFalloffExponent, ACutoffAngle, ALightColor, ASurfaceScale, AKs, AShininess, TSkBindings.SafeGetSelf(AInput), @sk_rect_t(ACropRect)));
end;

class function TSkImageFilter.MakeTile(const ASrc, ADest: TRect;
  AInput: ISkImageFilter): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_tile(@sk_rect_t(ASrc), @sk_rect_t(ADest), TSkBindings.SafeGetSelf(AInput)));
end;

function TSkImageFilter.MakeWithLocalMatrix(
  const AMatrix: TMatrix): ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_imagefilter_make_with_local_matrix(GetSelf, @sk_matrix_t(AMatrix)));
end;

{ TSkMaskFilter }

class function TSkMaskFilter.MakeBlur(const AStyle: TSkBlurStyle;
  const ASigma: Single; const ARespectCTM: Boolean): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_maskfilter_make_blur(sk_blurstyle_t(AStyle), ASigma, ARespectCTM));
end;

class function TSkMaskFilter.MakeShader(
  const AShader: ISkShader): ISkMaskFilter;
begin
  if not Assigned(AShader) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AShader']);
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_maskfilter_make_shader(TSkBindings.GetSelf(AShader)));
end;

class function TSkMaskFilter.MakeTable(
  const ATable: TSkTableFilter): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_maskfilter_make_table(@ATable));
end;

class function TSkMaskFilter.MakeTableClip(const AMin,
  AMax: Byte): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_maskfilter_make_table_clip(AMin, AMax));
end;

class function TSkMaskFilter.MakeTableGamma(
  const AGamma: Single): ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_maskfilter_make_table_gamma(AGamma));
end;

{ TSkPaint }

constructor TSkPaint.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_paint_create());
end;

constructor TSkPaint.Create(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  inherited Wrap(TSkiaAPI.sk4d_paint_create2(TSkBindings.GetSelf(APaint)));
end;

constructor TSkPaint.Create(const AStyle: TSkPaintStyle);
begin
  Create;
  SetStyle(AStyle);
end;

class procedure TSkPaint.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_paint_destroy(AHandle);
end;

function TSkPaint.GetAlpha: Byte;
begin
  Result := TSkiaAPI.sk4d_paint_get_alpha(GetSelf);
end;

function TSkPaint.GetAlphaF: Single;
begin
  Result := TSkiaAPI.sk4d_paint_get_alphaf(GetSelf);
end;

function TSkPaint.GetAntiAlias: Boolean;
begin
  Result := TSkiaAPI.sk4d_paint_get_anti_alias(GetSelf);
end;

function TSkPaint.GetBlender: ISkBlender;
begin
  Result := TSkBindings.SafeCreate<TSkBlender>(TSkiaAPI.sk4d_paint_get_blender(GetSelf));
end;

function TSkPaint.GetColor: TAlphaColor;
begin
  Result := TSkiaAPI.sk4d_paint_get_color(GetSelf);
end;

function TSkPaint.GetColorF: TAlphaColorF;
begin
  TSkiaAPI.sk4d_paint_get_colorf(GetSelf, sk_color4f_t(Result));
end;

function TSkPaint.GetColorFilter: ISkColorFilter;
begin
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_paint_get_color_filter(GetSelf));
end;

function TSkPaint.GetDither: Boolean;
begin
  Result := TSkiaAPI.sk4d_paint_get_dither(GetSelf);
end;

function TSkPaint.GetFillPath(const APath: ISkPath; const ACullRect: TRectF;
  const AResScale: Single): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_paint_get_fill_path(GetSelf, TSkBindings.GetSelf(APath), @sk_rect_t(ACullRect), AResScale));
end;

function TSkPaint.GetFillPath(const APath: ISkPath): ISkPath;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_paint_get_fill_path(GetSelf, TSkBindings.GetSelf(APath), nil, 1));
end;

function TSkPaint.GetImageFilter: ISkImageFilter;
begin
  Result := TSkBindings.SafeCreate<TSkImageFilter>(TSkiaAPI.sk4d_paint_get_image_filter(GetSelf));
end;

function TSkPaint.GetMaskFilter: ISkMaskFilter;
begin
  Result := TSkBindings.SafeCreate<TSkMaskFilter>(TSkiaAPI.sk4d_paint_get_mask_filter(GetSelf));
end;

function TSkPaint.GetPathEffect: ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_paint_get_path_effect(GetSelf));
end;

function TSkPaint.GetShader: ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_paint_get_shader(GetSelf));
end;

function TSkPaint.GetStrokeCap: TSkStrokeCap;
begin
  Result := TSkStrokeCap(TSkiaAPI.sk4d_paint_get_stroke_cap(GetSelf));
end;

function TSkPaint.GetStrokeJoin: TSkStrokeJoin;
begin
  Result := TSkStrokeJoin(TSkiaAPI.sk4d_paint_get_stroke_join(GetSelf));
end;

function TSkPaint.GetStrokeMiter: Single;
begin
  Result := TSkiaAPI.sk4d_paint_get_stroke_miter(GetSelf);
end;

function TSkPaint.GetStrokeWidth: Single;
begin
  Result := TSkiaAPI.sk4d_paint_get_stroke_width(GetSelf);
end;

function TSkPaint.GetStyle: TSkPaintStyle;
begin
  Result := TSkPaintStyle(TSkiaAPI.sk4d_paint_get_style(GetSelf))
end;

procedure TSkPaint.Reset;
begin
  TSkiaAPI.sk4d_paint_reset(GetSelf);
end;

procedure TSkPaint.SetAlpha(const AValue: Byte);
begin
  TSkiaAPI.sk4d_paint_set_alpha(GetSelf, AValue);
end;

procedure TSkPaint.SetAlphaF(const AValue: Single);
begin
  TSkiaAPI.sk4d_paint_set_alphaf(GetSelf, AValue);
end;

procedure TSkPaint.SetAntiAlias(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_paint_set_antialias(GetSelf, AValue);
end;

procedure TSkPaint.SetARGB(const A, R, G, B: Byte);
begin
  TSkiaAPI.sk4d_paint_set_argb(GetSelf, A, R, G, B);
end;

procedure TSkPaint.SetBlender(AValue: ISkBlender);
begin
  TSkiaAPI.sk4d_paint_set_blender(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetColor(const AValue: TAlphaColor);
begin
  TSkiaAPI.sk4d_paint_set_color(GetSelf, AValue);
end;

procedure TSkPaint.SetColorF(const AValue: TAlphaColorF;
  AColorSpace: ISkColorSpace);
begin
  TSkiaAPI.sk4d_paint_set_colorf(GetSelf, @sk_color4f_t(AValue), TSkBindings.SafeGetSelf(AColorSpace));
end;

procedure TSkPaint.SetColorFilter(AValue: ISkColorFilter);
begin
  TSkiaAPI.sk4d_paint_set_color_filter(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetDither(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_paint_set_dither(GetSelf, AValue);
end;

procedure TSkPaint.SetImageFilter(AValue: ISkImageFilter);
begin
  TSkiaAPI.sk4d_paint_set_image_filter(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetMaskFilter(AValue: ISkMaskFilter);
begin
  TSkiaAPI.sk4d_paint_set_mask_filter(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetPathEffect(AValue: ISkPathEffect);
begin
  TSkiaAPI.sk4d_paint_set_path_effect(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetShader(AValue: ISkShader);
begin
  TSkiaAPI.sk4d_paint_set_shader(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkPaint.SetStrokeCap(const AValue: TSkStrokeCap);
begin
  TSkiaAPI.sk4d_paint_set_stroke_cap(GetSelf, sk_strokecap_t(AValue));
end;

procedure TSkPaint.SetStrokeJoin(const AValue: TSkStrokeJoin);
begin
  TSkiaAPI.sk4d_paint_set_stroke_join(GetSelf, sk_strokejoin_t(AValue));
end;

procedure TSkPaint.SetStrokeMiter(const AValue: Single);
begin
  TSkiaAPI.sk4d_paint_set_stroke_miter(GetSelf, AValue);
end;

procedure TSkPaint.SetStrokeWidth(const AValue: Single);
begin
  TSkiaAPI.sk4d_paint_set_stroke_width(GetSelf, AValue);
end;

procedure TSkPaint.SetStyle(const AValue: TSkPaintStyle);
begin
  TSkiaAPI.sk4d_paint_set_style(GetSelf, sk_paintstyle_t(AValue));
end;

{ TSkOpBuilder }

procedure TSkOpBuilder.Add(const APath: ISkPath; const AOp: TSkPathOp);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  TSkiaAPI.sk4d_opbuilder_add(GetSelf, TSkBindings.GetSelf(APath), sk_pathop_t(AOp));
end;

constructor TSkOpBuilder.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_opbuilder_create());
end;

class procedure TSkOpBuilder.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_opbuilder_destroy(AHandle);
end;

function TSkOpBuilder.Detach: ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_opbuilder_detach(GetSelf));
end;

{ TSkPath }

function TSkPath.Contains(const AX, AY: Single): Boolean;
begin
  Result := TSkiaAPI.sk4d_path_contains(GetSelf, AX, AY);
end;

class function TSkPath.ConvertConicToQuads(const APoint1, APoint2,
  APoint3: TPointF; const AWeight: Single;
  const APower2: Integer): TArray<TPointF>;
begin
  SetLength(Result, 1 + 2 * (1 shl APower2));
  if Length(Result) > 0 then
    TSkiaAPI.sk4d_path_convert_conic_to_quads(@sk_point_t(APoint1), @sk_point_t(APoint2), @sk_point_t(APoint3), AWeight, @sk_point_t(Result[0]), APower2);
end;

constructor TSkPath.Create(const ASVG: string);
begin
  inherited Wrap(TSkiaAPI.sk4d_path_create(MarshaledAString(UTF8String(ASVG))));
end;

constructor TSkPath.Create(const ABytes: TBytes);
var
  LStream: TBytesStream;
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
  inherited Wrap(TSkiaAPI.sk4d_path_create2(LStream.Handle));
end;

class procedure TSkPath.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_path_destroy(AHandle);
end;

function TSkPath.GetBounds: TRectF;
begin
  TSkiaAPI.sk4d_path_get_bounds(GetSelf, sk_rect_t(Result));
end;

function TSkPath.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(TSkiaAPI.sk4d_path_get_fill_type(GetSelf));
end;

function TSkPath.GetIterator(const AForceClose: Boolean): ISkPathIterator;
begin
  Result := TPathIterator.Create(Self, AForceClose);
end;

function TSkPath.GetLastPoint: TPointF;
begin
  if not TSkiaAPI.sk4d_path_get_last_point(GetSelf, sk_point_t(Result)) then
    Result := TPointF.Create(0, 0);
end;

function TSkPath.GetSegmentMasks: TSkSegmentMasks;
begin
  Result := TSkSegmentMasks(Byte(TSkiaAPI.sk4d_path_get_segment_masks(GetSelf)));
end;

function TSkPath.GetTightBounds: TRectF;
begin
  TSkiaAPI.sk4d_path_get_tight_bounds(GetSelf, sk_rect_t(Result));
end;

function TSkPath.Interpolate(const AEnding: ISkPath;
  const AWeight: Single): ISkPath;
begin
  if not Assigned(AEnding) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEnding']);
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_path_interpolate(GetSelf, TSkBindings.GetSelf(AEnding), AWeight));
end;

function TSkPath.IsConvex: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_convex(GetSelf);
end;

function TSkPath.IsEmpty: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_empty(GetSelf);
end;

function TSkPath.IsFinite: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_finite(GetSelf);
end;

function TSkPath.IsInterpolatable(const APath: ISkPath): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkiaAPI.sk4d_path_is_interpolatable(GetSelf, TSkBindings.GetSelf(APath));
end;

function TSkPath.IsLastContourClosed: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_last_contour_closed(GetSelf);
end;

function TSkPath.IsLine(out APoint1, APoint2: TPointF): Boolean;
var
  LLines: array[0..1] of TPointF;
begin
  LLines[0] := APoint1;
  LLines[1] := APoint2;
  Result := TSkiaAPI.sk4d_path_is_line(GetSelf, @sk_point_t(LLines[0]));
end;

function TSkPath.IsLine: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_line(GetSelf, nil);
end;

function TSkPath.IsOval(out ARect: TRectF): Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_oval(GetSelf, @sk_rect_t(ARect));
end;

function TSkPath.IsOval: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_oval(GetSelf, nil);
end;

function TSkPath.IsRect: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_rect(GetSelf, nil);
end;

function TSkPath.IsRect(out ARect: TRectF): Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_rect(GetSelf, @sk_rect_t(ARect));
end;

function TSkPath.IsRoundRect(out ARoundRect: ISkRoundRect): Boolean;
var
  LRoundRect: ISkRoundRect;
begin
  LRoundRect := TSkRoundRect.Create;
  Result     := TSkiaAPI.sk4d_path_is_rrect(GetSelf, TSkBindings.SafeGetSelf(LRoundRect));
  if Result then
    ARoundRect := LRoundRect;
end;

function TSkPath.IsRoundRect: Boolean;
begin
  Result := TSkiaAPI.sk4d_path_is_rrect(GetSelf, 0);
end;

function TSkPath.Serialize: TBytes;
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

procedure TSkPath.SerializeToStream(const AStream: TStream);
var
  LWStream: ISkWStream;
begin
  LWStream := TSkWStreamAdapter.Create(AStream);
  TSkiaAPI.sk4d_path_serialize_to_stream(GetSelf, LWStream.Handle);
end;

function TSkPath.ToSVG: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(TSkiaAPI.sk4d_path_to_svg(GetSelf));
  Result  := LResult.Text;
end;

function TSkPath.Transform(const AMatrix: TMatrix): ISkPath;
begin
  Result := TSkPath.Wrap(TSkiaAPI.sk4d_path_transform(GetSelf, @sk_matrix_t(AMatrix)));
end;

{ TSkPath.TPathIterator }

constructor TSkPath.TPathIterator.Create(const APath: ISkPath;
  const AForceClose: Boolean);
begin
  inherited Wrap(TSkiaAPI.sk4d_pathiterator_create(TSkBindings.GetSelf(APath), AForceClose));
end;

class procedure TSkPath.TPathIterator.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_pathiterator_destroy(AHandle);
end;

function TSkPath.TPathIterator.GetCurrent: TSkPathIteratorElem;
begin
  Result := FCurrent;
end;

function TSkPath.TPathIterator.MoveNext: Boolean;
begin
  Result := TSkiaAPI.sk4d_pathiterator_next(GetSelf, sk_pathiteratorelem_t(FCurrent));
end;

{ TSkPathBuilder }

procedure TSkPathBuilder.AddArc(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single);
begin
  TSkiaAPI.sk4d_pathbuilder_add_arc(GetSelf, @sk_rect_t(AOval), AStartAngle, ASweepAngle);
end;

procedure TSkPathBuilder.AddCircle(const ACenter: TPointF; ARadius: Single;
  ADirection: TSkPathDirection);
begin
  AddCircle(ACenter.X, ACenter.Y, ARadius, ADirection);
end;

procedure TSkPathBuilder.AddCircle(const ACenterX, ACenterY, ARadius: Single;
  ADirection: TSkPathDirection);
begin
  TSkiaAPI.sk4d_pathbuilder_add_circle(GetSelf,ACenterX, ACenterY, ARadius, sk_pathdirection_t(ADirection));
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection);
begin
  AddOval(ARect, ADirection, 1);
end;

procedure TSkPathBuilder.AddOval(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  TSkiaAPI.sk4d_pathbuilder_add_oval(GetSelf, @sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddPath(const APath: ISkPath);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  TSkiaAPI.sk4d_pathbuilder_add_path(GetSelf, TSkBindings.GetSelf(APath));
end;

procedure TSkPathBuilder.AddPolygon(const APolygon: TPolygon;
  const IsClosed: Boolean);
begin
  if Length(APolygon) > 0 then
    TSkiaAPI.sk4d_pathbuilder_add_polygon(GetSelf, @sk_point_t(APolygon[0]), Length(APolygon), IsClosed);
end;

procedure TSkPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  if AStartIndex > 3 then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AStartIndex', AStartIndex, 0, 4]);
  TSkiaAPI.sk4d_pathbuilder_add_rect(GetSelf, @sk_rect_t(ARect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddRect(const ARect: TRectF;
  ADirection: TSkPathDirection);
begin
  AddRect(ARect, ADirection, 0);
end;

procedure TSkPathBuilder.AddRoundRect(const ARoundRect: ISkRoundRect;
  ADirection: TSkPathDirection; AStartIndex: Cardinal);
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  TSkiaAPI.sk4d_pathbuilder_add_rrect(GetSelf, TSkBindings.GetSelf(ARoundRect), sk_pathdirection_t(ADirection), AStartIndex);
end;

procedure TSkPathBuilder.AddRoundRect(const ARoundRect: ISkRoundRect;
  ADirection: TSkPathDirection);
begin
  AddRoundRect(ARoundRect, ADirection, IfThen(ADirection = TSkPathDirection.CW, 6, 7));
end;

procedure TSkPathBuilder.ArcTo(const APoint1, APoint2: TPointF;
  const ARadius: Single);
begin
  TSkiaAPI.sk4d_pathbuilder_arc_to3(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), ARadius);
end;

procedure TSkPathBuilder.ArcTo(const AOval: TRectF; const AStartAngle,
  ASweepAngle: Single; const AForceMoveTo: Boolean);
begin
  TSkiaAPI.sk4d_pathbuilder_arc_to2(GetSelf, @sk_rect_t(AOval), AStartAngle, ASweepAngle, AForceMoveTo);
end;

procedure TSkPathBuilder.ArcTo(const ARadius: TPointF;
  const XAxisRotate: Single; const ALargeArc: TSkPathArcSize;
  const ASweep: TSkPathDirection; const AXY: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_arc_to(GetSelf, @sk_point_t(ARadius), XAxisRotate, sk_patharcsize_t(ALargeArc), sk_pathdirection_t(ASweep), @sk_point_t(AXY));
end;

procedure TSkPathBuilder.Close;
begin
  TSkiaAPI.sk4d_pathbuilder_close(GetSelf);
end;

procedure TSkPathBuilder.ConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  ConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

procedure TSkPathBuilder.ConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  TSkiaAPI.sk4d_pathbuilder_conic_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), AWeight);
end;

constructor TSkPathBuilder.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_pathbuilder_create());
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
  inherited Wrap(TSkiaAPI.sk4d_pathbuilder_create2(TSkBindings.GetSelf(APathBuilder)));
end;

procedure TSkPathBuilder.CubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_cubic_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), @sk_point_t(APoint3));
end;

procedure TSkPathBuilder.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  CubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

class procedure TSkPathBuilder.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_pathbuilder_destroy(AHandle);
end;

function TSkPathBuilder.Detach: ISkPath;
begin
  Result := TSkPath.Wrap(TSkiaAPI.sk4d_pathbuilder_detach(GetSelf));
end;

function TSkPathBuilder.GetBounds: TRectF;
begin
  TSkiaAPI.sk4d_pathbuilder_get_bounds(GetSelf, sk_rect_t(Result));
end;

function TSkPathBuilder.GetFillType: TSkPathFillType;
begin
  Result := TSkPathFillType(TSkiaAPI.sk4d_pathbuilder_get_fill_type(GetSelf));
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount,
  AExtraVerbCount: Integer);
begin
  TSkiaAPI.sk4d_pathbuilder_inc_reserve(GetSelf, AExtraPointCount, AExtraVerbCount);
end;

procedure TSkPathBuilder.IncReserve(const AExtraPointCount: Integer);
begin
  IncReserve(AExtraPointCount, AExtraPointCount);
end;

procedure TSkPathBuilder.LineTo(const APoint: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_line_to(GetSelf, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.LineTo(const AX, AY: Single);
begin
  LineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.MoveTo(const AX, AY: Single);
begin
  MoveTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.MoveTo(const APoint: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_move_to(GetSelf, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.Offset(const ADeltaX, ADeltaY: Single);
begin
  TSkiaAPI.sk4d_pathbuilder_offset(GetSelf, ADeltaX, ADeltaY);
end;

procedure TSkPathBuilder.PolylineTo(const APoints: TArray<TPointF>);
begin
  if Length(APoints) > 0 then
    TSkiaAPI.sk4d_pathbuilder_polyline_to(GetSelf, @sk_point_t(APoints[0]), Length(APoints));
end;

procedure TSkPathBuilder.QuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  QuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.QuadTo(const APoint1, APoint2: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_quad_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2));
end;

procedure TSkPathBuilder.RConicTo(const AX1, AY1, AX2, AY2, AWeight: Single);
begin
  RConicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), AWeight);
end;

procedure TSkPathBuilder.RConicTo(const APoint1, APoint2: TPointF;
  const AWeight: Single);
begin
  TSkiaAPI.sk4d_pathbuilder_r_conic_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), AWeight);
end;

procedure TSkPathBuilder.RCubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Single);
begin
  RCubicTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2), TPointF.Create(AX3, AY3));
end;

procedure TSkPathBuilder.RCubicTo(const APoint1, APoint2, APoint3: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_r_cubic_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2), @sk_point_t(APoint3));
end;

procedure TSkPathBuilder.Reset;
begin
  TSkiaAPI.sk4d_pathbuilder_reset(GetSelf);
end;

procedure TSkPathBuilder.RLineTo(const APoint: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_r_line_to(GetSelf, @sk_point_t(APoint));
end;

procedure TSkPathBuilder.RLineTo(const AX, AY: Single);
begin
  RLineTo(TPointF.Create(AX, AY));
end;

procedure TSkPathBuilder.RQuadTo(const APoint1, APoint2: TPointF);
begin
  TSkiaAPI.sk4d_pathbuilder_r_quad_to(GetSelf, @sk_point_t(APoint1), @sk_point_t(APoint2));
end;

procedure TSkPathBuilder.RQuadTo(const AX1, AY1, AX2, AY2: Single);
begin
  RQuadTo(TPointF.Create(AX1, AY1), TPointF.Create(AX2, AY2));
end;

procedure TSkPathBuilder.SetFillType(const AValue: TSkPathFillType);
begin
  TSkiaAPI.sk4d_pathbuilder_set_filltype(GetSelf, sk_pathfilltype_t(AValue));
end;

function TSkPathBuilder.Snapshot: ISkPath;
begin
  Result := TSkPath.Wrap(TSkiaAPI.sk4d_pathbuilder_snapshot(GetSelf));
end;

procedure TSkPathBuilder.ToggleInverseFillType;
begin
  TSkiaAPI.sk4d_pathbuilder_toggle_inverse_filltype(GetSelf);
end;

{ TSkPathEffect }

class function TSkPathEffect.Make1DPath(const APath: ISkPath; const AAdvance,
  APhase: Single; const AStyle: TSkPathEffect1DStyle): ISkPathEffect;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_1dpath(TSkBindings.GetSelf(APath), AAdvance, APhase, sk_patheffect1dstyle_t(AStyle)));
end;

class function TSkPathEffect.Make2DLine(const AWidth: Single;
  const AMatrix: TMatrix): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_2dline(AWidth, @sk_matrix_t(AMatrix)));
end;

class function TSkPathEffect.Make2DPath(const AMatrix: TMatrix;
  const APath: ISkPath): ISkPathEffect;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_2dpath(@sk_matrix_t(AMatrix), TSkBindings.GetSelf(APath)));
end;

class function TSkPathEffect.MakeCompose(const AOuter,
  AInner: ISkPathEffect): ISkPathEffect;
begin
  if not Assigned(AOuter) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AOuter']);
  if not Assigned(AInner) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AInner']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_compose(TSkBindings.GetSelf(AOuter), TSkBindings.GetSelf(AInner)));
end;

class function TSkPathEffect.MakeCorner(const ARadius: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_corner(ARadius));
end;

class function TSkPathEffect.MakeDash(const AIntervals: TArray<Single>;
  const APhase: Single): ISkPathEffect;
begin
  if Length(AIntervals) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AIntervals']);
  if Odd(Length(AIntervals)) then
    raise ESkArgumentException.CreateFmt(SParamSizeIsOdd, ['AIntervals']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_dash(@AIntervals[0], Length(AIntervals), APhase));
end;

class function TSkPathEffect.MakeDiscrete(const ASegLength, ADeviation: Single;
  const ASeedAssist: Cardinal): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_discrete(ASegLength, ADeviation, ASeedAssist));
end;

class function TSkPathEffect.MakeMatrix(const AMatrix: TMatrix): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_matrix(@sk_matrix_t(AMatrix)));
end;

class function TSkPathEffect.MakeMerge(const AEffect1, AEffect2: ISkPathEffect;
  const AOp: TSkPathOp): ISkPathEffect;
begin
  if not Assigned(AEffect1) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect1']);
  if not Assigned(AEffect2) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect2']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_merge(TSkBindings.GetSelf(AEffect1), TSkBindings.GetSelf(AEffect2), sk_pathop_t(AOp)));
end;

class function TSkPathEffect.MakeStroke(const AWidth: Single;
  const AJoin: TSkStrokeJoin; const ACap: TSkStrokeCap;
  const AMiter: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_stroke(AWidth, sk_strokejoin_t(AJoin), sk_strokecap_t(ACap), AMiter));
end;

class function TSkPathEffect.MakeStrokeAndFill: ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_stroke_and_fill());
end;

class function TSkPathEffect.MakeSum(const AEffect1,
  AEffect2: ISkPathEffect): ISkPathEffect;
begin
  if not Assigned(AEffect1) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect1']);
  if not Assigned(AEffect2) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AEffect2']);
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_sum(TSkBindings.GetSelf(AEffect1), TSkBindings.GetSelf(AEffect2)));
end;

class function TSkPathEffect.MakeTranslate(const ADeltaX,
  ADeltaY: Single): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_translate(ADeltaX, ADeltaY));
end;

class function TSkPathEffect.MakeTrim(const AStart, AStop: Single;
  const AMode: TSkPathEffectTrimMode): ISkPathEffect;
begin
  Result := TSkBindings.SafeCreate<TSkPathEffect>(TSkiaAPI.sk4d_patheffect_make_trim(AStart, AStop, sk_patheffecttrimmode_t(AMode)));
end;

{ TSkPathMeasure }

constructor TSkPathMeasure.Create(const APath: ISkPath;
  const AForceClosed: Boolean; const AResScale: Single);
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  inherited Wrap(TSkiaAPI.sk4d_pathmeasure_create(TSkBindings.GetSelf(APath), AForceClosed, AResScale));
end;

class procedure TSkPathMeasure.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_pathmeasure_destroy(AHandle);
end;

function TSkPathMeasure.GetLength: Single;
begin
  Result := TSkiaAPI.sk4d_pathmeasure_get_length(GetSelf);
end;

function TSkPathMeasure.GetMatrix(const ADistance: Single; out AMatrix: TMatrix;
  const AMatrixFlags: TSkPathMeasureMatrixFlags): Boolean;
begin
  Result := TSkiaAPI.sk4d_pathmeasure_get_matrix(GetSelf, ADistance, sk_matrix_t(AMatrix), Byte(AMatrixFlags));
end;

function TSkPathMeasure.GetPositionAndTangent(const ADistance: Single;
  out APosition, ATangent: TPointF): Boolean;
begin
  Result := TSkiaAPI.sk4d_pathmeasure_get_position_and_tangent(GetSelf, ADistance, sk_point_t(APosition), sk_vector_t(ATangent));
end;

function TSkPathMeasure.GetSegment(const AStart, AStop: Single;
  const AStartWithMoveTo: Boolean): ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_pathmeasure_get_segment(GetSelf, AStart, AStop, AStartWithMoveTo));
end;

function TSkPathMeasure.IsClosed: Boolean;
begin
  Result := TSkiaAPI.sk4d_pathmeasure_is_closed(GetSelf);
end;

function TSkPathMeasure.NextContour: Boolean;
begin
  Result := TSkiaAPI.sk4d_pathmeasure_next_contour(GetSelf);
end;

{ TSkPicture }

function TSkPicture.GetCullRect: TRectF;
begin
  TSkiaAPI.sk4d_picture_get_cull_rect(GetSelf, sk_rect_t(Result));
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
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkPicture>(TSkiaAPI.sk4d_picture_make_from_stream(LStream.Handle));
end;

function TSkPicture.MakeShader(const ALocalMatrix: TMatrix; const ATileModeX,
  ATileModeY: TSkTileMode; const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_picture_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), @sk_matrix_t(ALocalMatrix), nil));
end;

function TSkPicture.MakeShader(const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_picture_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), nil, nil));
end;

function TSkPicture.MakeShader(const ATileRect: TRectF; const ATileModeX,
  ATileModeY: TSkTileMode; const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_picture_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), nil, @sk_rect_t(ATileRect)));
end;

function TSkPicture.MakeShader(const ATileRect: TRectF;
  const ALocalMatrix: TMatrix; const ATileModeX, ATileModeY: TSkTileMode;
  const AFilterMode: TSkFilterMode): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_picture_make_shader(GetSelf, sk_tilemode_t(ATileModeX), sk_tilemode_t(ATileModeY), sk_filtermode_t(AFilterMode), @sk_matrix_t(ALocalMatrix), @sk_rect_t(ATileRect)));
end;

procedure TSkPicture.Playback(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_picture_playback(GetSelf, TSkBindings.GetSelf(ACanvas));
end;

function TSkPicture.Serialize: TBytes;
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
  LWStream := TSkWStreamAdapter.Create(AStream);
  TSkiaAPI.sk4d_picture_serialize_to_stream(GetSelf, LWStream.Handle);
end;

{ TSkPictureRecorder }

function TSkPictureRecorder.BeginRecording(const AWidth,
  AHeight: Single): ISkCanvas;
begin
  Result := BeginRecording(TRectF.Create(0, 0, AWidth, AHeight));
end;

function TSkPictureRecorder.BeginRecording(const ABounds: TRectF): ISkCanvas;
begin
  Result := TSkBindings.SafeCreate<TSkCanvas>(TSkiaAPI.sk4d_picturerecorder_begin_recording(GetSelf, @sk_rect_t(ABounds)), False);
end;

constructor TSkPictureRecorder.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_picturerecorder_create());
end;

class procedure TSkPictureRecorder.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_picturerecorder_destroy(AHandle);
end;

function TSkPictureRecorder.FinishRecording: ISkPicture;
begin
  Result := TSkBindings.SafeCreate<TSkPicture>(TSkiaAPI.sk4d_picturerecorder_finish_recording(GetSelf));
end;

function TSkPictureRecorder.FinishRecording(
  const ACullRect: TRectF): ISkPicture;
begin
  Result := TSkBindings.SafeCreate<TSkPicture>(TSkiaAPI.sk4d_picturerecorder_finish_recording2(GetSelf, @sk_rect_t(ACullRect)));
end;

{ TSkPixmap }

constructor TSkPixmap.Create(const AImageInfo: TSkImageInfo;
  const APixels: Pointer; const ARowBytes: NativeUInt);
var
  LImageInfo: sk_imageinfo_t;
begin
  LImageInfo := TSkMapping.AsImageInfo(AImageInfo);
  inherited Wrap(TSkiaAPI.sk4d_pixmap_create(@LImageInfo, APixels, ARowBytes));
end;

class procedure TSkPixmap.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_pixmap_destroy(AHandle);
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF; const ASubset: TRectF;
  AColorSpace: ISkColorSpace): Boolean;
begin
  Result := TSkiaAPI.sk4d_pixmap_erase2(GetSelf, @sk_color4f_t(AColor), TSkBindings.SafeGetSelf(AColorSpace), @sk_irect_t(ASubset));
end;

function TSkPixmap.Erase(const AColor: TAlphaColor;
  const ASubset: TRectF): Boolean;
begin
  Result := TSkiaAPI.sk4d_pixmap_erase(GetSelf, AColor, @sk_irect_t(ASubset));
end;

function TSkPixmap.Erase(const AColor: TAlphaColorF;
  AColorSpace: ISkColorSpace): Boolean;
begin
  Result := TSkiaAPI.sk4d_pixmap_erase2(GetSelf, @sk_color4f_t(AColor), TSkBindings.SafeGetSelf(AColorSpace), nil);
end;

function TSkPixmap.Erase(const AColor: TAlphaColor): Boolean;
begin
  Result := TSkiaAPI.sk4d_pixmap_erase(GetSelf, AColor, nil);
end;

function TSkPixmap.ExtractSubset(const ADest: ISkPixmap;
  const AArea: TRect): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := TSkiaAPI.sk4d_pixmap_extract_subset(GetSelf, TSkBindings.GetSelf(ADest), @sk_irect_t(AArea));
end;

function TSkPixmap.GetAlpha(const AX, AY: Integer): Single;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_alpha(GetSelf, AX, AY);
end;

function TSkPixmap.GetAlphaType: TSkAlphaType;
begin
  Result := TSkAlphaType(TSkiaAPI.sk4d_pixmap_get_alpha_type(GetSelf));
end;

function TSkPixmap.GetColor(const AX, AY: Integer): TAlphaColor;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_color(GetSelf, AX, AY);
end;

function TSkPixmap.GetColorSpace: ISkColorSpace;
begin
  Result := TSkBindings.SafeCreate<TSkColorSpace>(TSkiaAPI.sk4d_pixmap_get_color_space(GetSelf));
end;

function TSkPixmap.GetColorType: TSkColorType;
begin
  Result := TSkColorType(TSkiaAPI.sk4d_pixmap_get_color_type(GetSelf));
end;

function TSkPixmap.GetHeight: Integer;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_height(GetSelf);
end;

function TSkPixmap.GetImageInfo: TSkImageInfo;
var
  LResult: sk_imageinfo_t;
begin
  TSkiaAPI.sk4d_pixmap_get_image_info(GetSelf, LResult);
  Result := TSkMapping.ToImageInfo(LResult);
end;

function TSkPixmap.GetPixelAddr(const AX, AY: Integer): Pointer;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_pixel_addr(GetSelf, AX, AY);
end;

function TSkPixmap.GetPixels: Pointer;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_pixels(GetSelf);
end;

function TSkPixmap.GetRowBytes: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_row_bytes(GetSelf);
end;

function TSkPixmap.GetWidth: Integer;
begin
  Result := TSkiaAPI.sk4d_pixmap_get_width(GetSelf);
end;

function TSkPixmap.ReadPixels(const ADest: ISkPixmap; const ASrcX,
  ASrcY: Integer): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := TSkiaAPI.sk4d_pixmap_read_pixels(GetSelf, TSkBindings.GetSelf(ADest), ASrcX, ASrcY);
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

function TSkPixmap.ScalePixels(const ADest: ISkPixmap): Boolean;
begin
  Result := ScalePixels(ADest, TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None));
end;

function TSkPixmap.ScalePixels(const ADest: ISkPixmap;
  const ASampling: TSkSamplingOptions): Boolean;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  Result := TSkiaAPI.sk4d_pixmap_scale_pixels(GetSelf, TSkBindings.GetSelf(ADest), @sk_samplingoptions_t(ASampling));
end;

function TSkPixmap.ScalePixels(const ADestImageInfo: TSkImageInfo;
  const ADestPixels: Pointer; const ADestRowBytes: NativeUInt): Boolean;
var
  LPixmap: ISkPixmap;
begin
  LPixmap := TSkPixmap.Create(ADestImageInfo, ADestPixels, ADestRowBytes);
  Result  := ScalePixels(LPixmap);
end;

procedure TSkPixmap.SetColorSpace(AValue: ISkColorSpace);
begin
  TSkiaAPI.sk4d_pixmap_set_colorspace(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

{ TSkRegion }

function TSkRegion.Contains(const AX, AY: Integer): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_contains3(GetSelf, AX, AY);
end;

function TSkRegion.Contains(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_contains2(GetSelf, @sk_irect_t(ARect));
end;

function TSkRegion.Contains(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkiaAPI.sk4d_region_contains(GetSelf, TSkBindings.GetSelf(ARegion));
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
  inherited Wrap(TSkiaAPI.sk4d_region_create2(TSkBindings.GetSelf(ARegion)));
end;

constructor TSkRegion.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_region_create());
end;

class procedure TSkRegion.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_region_destroy(AHandle);
end;

function TSkRegion.GetBoundaryPath: ISkPath;
begin
  Result := TSkBindings.SafeCreate<TSkPath>(TSkiaAPI.sk4d_region_get_boundary_path(GetSelf));
end;

function TSkRegion.GetBounds: TRect;
begin
  TSkiaAPI.sk4d_region_get_bounds(GetSelf, sk_irect_t(Result));
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

function TSkRegion.Intersects(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkiaAPI.sk4d_region_intersects(GetSelf, TSkBindings.GetSelf(ARegion));
end;

function TSkRegion.Intersects(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_intersects2(GetSelf, @sk_irect_t(ARect));
end;

function TSkRegion.IsComplex: Boolean;
begin
  Result := TSkiaAPI.sk4d_region_is_complex(GetSelf);
end;

function TSkRegion.IsEmpty: Boolean;
begin
  Result := TSkiaAPI.sk4d_region_is_empty(GetSelf);
end;

function TSkRegion.IsEqual(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkiaAPI.sk4d_region_is_equal(GetSelf, TSkBindings.GetSelf(ARegion));
end;

function TSkRegion.IsRect: Boolean;
begin
  Result := TSkiaAPI.sk4d_region_is_rect(GetSelf);
end;

function TSkRegion.Op(const ARegion: ISkRegion;
  const AOp: TSkRegionOp): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkiaAPI.sk4d_region_op(GetSelf, TSkBindings.GetSelf(ARegion), sk_regionop_t(AOp));
end;

function TSkRegion.Op(const ARect: TRect; const AOp: TSkRegionOp): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_op2(GetSelf, @sk_irect_t(ARect), sk_regionop_t(AOp));
end;

function TSkRegion.QuickContains(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_quick_contains(GetSelf, @sk_irect_t(ARect));
end;

function TSkRegion.QuickReject(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_quick_reject2(GetSelf, @sk_irect_t(ARect));
end;

function TSkRegion.QuickReject(const ARegion: ISkRegion): Boolean;
begin
  if not Assigned(ARegion) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARegion']);
  Result := TSkiaAPI.sk4d_region_quick_reject(GetSelf, TSkBindings.GetSelf(ARegion));
end;

procedure TSkRegion.SetEmpty;
begin
  TSkiaAPI.sk4d_region_set_empty(GetSelf);
end;

function TSkRegion.SetPath(const APath: ISkPath;
  const AClip: ISkRegion): Boolean;
begin
  if not Assigned(APath) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APath']);
  if not Assigned(AClip) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AClip']);
  Result := TSkiaAPI.sk4d_region_set_path(GetSelf, TSkBindings.GetSelf(APath), TSkBindings.GetSelf(AClip));
end;

function TSkRegion.SetRect(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_region_set_rect(GetSelf, @sk_irect_t(ARect));
end;

function TSkRegion.SetRects(const ARects: TArray<TRect>): Boolean;
begin
  if Length(ARects) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ARects']);
  Result := TSkiaAPI.sk4d_region_set_rects(GetSelf, @sk_rect_t(ARects[0]), Length(ARects));
end;

procedure TSkRegion.Translate(const ADeltaX, ADeltaY: Integer);
begin
  TSkiaAPI.sk4d_region_translate(GetSelf, ADeltaX, ADeltaY);
end;

{ TSkRegion.TRegionCliperator }

constructor TSkRegion.TRegionCliperator.Create(const ARegion: ISkRegion;
  const AClip: TRect);
begin
  inherited Wrap(TSkiaAPI.sk4d_regioncliperator_create(TSkBindings.GetSelf(ARegion), @sk_irect_t(AClip)));
end;

class procedure TSkRegion.TRegionCliperator.DestroyHandle(
  const AHandle: THandle);
begin
  TSkiaAPI.sk4d_regioncliperator_destroy(AHandle);
end;

function TSkRegion.TRegionCliperator.GetCurrent: TRect;
begin
  TSkiaAPI.sk4d_regioncliperator_get_current(GetSelf, sk_irect_t(Result));
end;

function TSkRegion.TRegionCliperator.MoveNext: Boolean;
begin
  Result := TSkiaAPI.sk4d_regioncliperator_move_next(GetSelf);
end;

{ TSkRegion.TRegionIterator }

constructor TSkRegion.TRegionIterator.Create(const ARegion: ISkRegion);
begin
  inherited Wrap(TSkiaAPI.sk4d_regioniterator_create(TSkBindings.GetSelf(ARegion)));
end;

class procedure TSkRegion.TRegionIterator.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_regioniterator_destroy(AHandle);
end;

function TSkRegion.TRegionIterator.GetCurrent: TRect;
begin
  TSkiaAPI.sk4d_regioniterator_get_current(GetSelf, sk_irect_t(Result));
end;

function TSkRegion.TRegionIterator.MoveNext: Boolean;
begin
  Result := TSkiaAPI.sk4d_regioniterator_move_next(GetSelf);
end;

procedure TSkRegion.TRegionIterator.Reset;
begin
  TSkiaAPI.sk4d_regioniterator_reset(GetSelf);
end;

{ TSkRegion.TRegionSpanerator }

constructor TSkRegion.TRegionSpanerator.Create(const ARegion: ISkRegion;
  const AY, ALeft, ARight: Integer);
begin
  inherited Wrap(TSkiaAPI.sk4d_regionspanerator_create(TSkBindings.GetSelf(ARegion), AY, ALeft, ARight));
end;

class procedure TSkRegion.TRegionSpanerator.DestroyHandle(
  const AHandle: THandle);
begin
  TSkiaAPI.sk4d_regionspanerator_destroy(AHandle);
end;

function TSkRegion.TRegionSpanerator.GetCurrent: TPoint;
begin
  Result := FCurrent;
end;

function TSkRegion.TRegionSpanerator.MoveNext: Boolean;
begin
  Result := TSkiaAPI.sk4d_regionspanerator_next(GetSelf, sk_ipoint_t(FCurrent));
end;

{ TSkRoundRect }

function TSkRoundRect.Contains(const ARect: TRect): Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_contains(GetSelf, @sk_rect_t(ARect));
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
  inherited Wrap(TSkiaAPI.sk4d_rrect_create2(TSkBindings.GetSelf(ARoundRect)));
end;

constructor TSkRoundRect.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_rrect_create());
end;

constructor TSkRoundRect.Create(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  Create;
  SetRect(ARect, ARadii);
end;

procedure TSkRoundRect.Deflate(const ADeltaX, ADeltaY: Single);
begin
  TSkiaAPI.sk4d_rrect_deflate(GetSelf, ADeltaX, ADeltaY);
end;

class procedure TSkRoundRect.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_rrect_destroy(AHandle);
end;

function TSkRoundRect.GetHeight: Single;
begin
  Result := TSkiaAPI.sk4d_rrect_get_height(GetSelf);
end;

function TSkRoundRect.GetRadii(const ACorner: TSkRoundRectCorner): TPointF;
begin
  TSkiaAPI.sk4d_rrect_get_radii(GetSelf, sk_rrectcorner_t(ACorner), sk_vector_t(Result));
end;

function TSkRoundRect.GetRect: TRectF;
begin
  TSkiaAPI.sk4d_rrect_get_rect(GetSelf, sk_rect_t(Result));
end;

function TSkRoundRect.GetSimpleRadii: TPointF;
begin
  TSkiaAPI.sk4d_rrect_get_simple_radii(GetSelf, sk_vector_t(Result));
end;

function TSkRoundRect.GetWidth: Single;
begin
  Result := TSkiaAPI.sk4d_rrect_get_width(GetSelf);
end;

procedure TSkRoundRect.Inflate(const ADeltaX, ADeltaY: Single);
begin
  TSkiaAPI.sk4d_rrect_inflate(GetSelf, ADeltaX, ADeltaY);
end;

function TSkRoundRect.IsComplex: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_complex(GetSelf);
end;

function TSkRoundRect.IsEmpty: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_empty(GetSelf);
end;

function TSkRoundRect.IsEqual(const ARoundRect: ISkRoundRect): Boolean;
begin
  if not Assigned(ARoundRect) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARoundRect']);
  Result := TSkiaAPI.sk4d_rrect_is_equal(GetSelf, TSkBindings.GetSelf(ARoundRect));
end;

function TSkRoundRect.IsNinePatch: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_nine_patch(GetSelf);
end;

function TSkRoundRect.IsOval: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_oval(GetSelf);
end;

function TSkRoundRect.IsRect: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_rect(GetSelf);
end;

function TSkRoundRect.IsSimple: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_simple(GetSelf);
end;

function TSkRoundRect.IsValid: Boolean;
begin
  Result := TSkiaAPI.sk4d_rrect_is_valid(GetSelf);
end;

procedure TSkRoundRect.Offset(const ADeltaX, ADeltaY: Single);
begin
  TSkiaAPI.sk4d_rrect_offset(GetSelf, ADeltaX, ADeltaY);
end;

procedure TSkRoundRect.SetEmpty;
begin
  TSkiaAPI.sk4d_rrect_set_empty(GetSelf);
end;

procedure TSkRoundRect.SetNinePatch(const ARect: TRectF; const ARadiusLeft,
  ARadiusTop, ARadiusRight, ARadiusBottom: Single);
begin
  TSkiaAPI.sk4d_rrect_set_nine_patch(GetSelf, @sk_rect_t(ARect), ARadiusLeft, ARadiusTop, ARadiusRight, ARadiusBottom);
end;

procedure TSkRoundRect.SetOval(const ARect: TRectF);
begin
  TSkiaAPI.sk4d_rrect_set_oval(GetSelf, @sk_rect_t(ARect));
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF;
  const ARadii: TSkRoundRectRadii);
begin
  TSkiaAPI.sk4d_rrect_set_rect2(GetSelf, @sk_rect_t(ARect), @ARadii);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF; const ARadiusX,
  ARadiusY: Single);
begin
  TSkiaAPI.sk4d_rrect_set_rect3(GetSelf, @sk_rect_t(ARect), ARadiusX, ARadiusY);
end;

procedure TSkRoundRect.SetRect(const ARect: TRectF);
begin
  TSkiaAPI.sk4d_rrect_set_rect(GetSelf, @sk_rect_t(ARect));
end;

function TSkRoundRect.Transform(const AMatrix: TMatrix): ISkRoundRect;
begin
  Result := TSkBindings.SafeCreate<TSkRoundRect>(TSkiaAPI.sk4d_rrect_transform(GetSelf, @sk_matrix_t(AMatrix)));
end;

{ TSkRuntimeEffectFloat2 }

constructor TSkRuntimeEffectFloat2.Create(const AV1, AV2: Single);
begin
  V1 := AV1;
  V2 := AV2;
end;

class operator TSkRuntimeEffectFloat2.Implicit(
  const APoint: TPointF): TSkRuntimeEffectFloat2;
begin
  Result.V1 := APoint.X;
  Result.V2 := APoint.Y;
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

class operator TSkRuntimeEffectFloat4.Implicit(
  const AColor: TAlphaColorF): TSkRuntimeEffectFloat4;
begin
  Result.V1 := AColor.R;
  Result.V2 := AColor.G;
  Result.V3 := AColor.B;
  Result.V4 := AColor.A;
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

class operator TSkRuntimeEffectInt2.Implicit(
  const APoint: TPoint): TSkRuntimeEffectInt2;
begin
  Result.V1 := APoint.X;
  Result.V2 := APoint.Y;
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

{ TSkRuntimeEffect }

function TSkRuntimeEffect.ChildExists(const AName: string): Boolean;
begin
  Result := IndexOfChild(AName) >= 0;
end;

destructor TSkRuntimeEffect.Destroy;
begin
  FreeMem(FUniformData);
  inherited;
end;

function TSkRuntimeEffect.GetChildBlender(const AIndex: Integer): ISkBlender;
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.Blender then
    raise ESkException.Create(SInvalidOperation);
  Result := FChildren[AIndex] as ISkBlender;
end;

function TSkRuntimeEffect.GetChildBlender(const AName: string): ISkBlender;
begin
  Result := GetChildBlender(IndexOfChild(AName));
end;

function TSkRuntimeEffect.GetChildColorFilter(
  const AIndex: Integer): ISkColorFilter;
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.ColorFilter then
    raise ESkException.Create(SInvalidOperation);
  Result := FChildren[AIndex] as ISkColorFilter;
end;

function TSkRuntimeEffect.GetChildColorFilter(
  const AName: string): ISkColorFilter;
begin
  Result := GetChildColorFilter(IndexOfChild(AName));
end;

function TSkRuntimeEffect.GetChildCount: Integer;
begin
  Result := TSkiaAPI.sk4d_runtimeeffect_get_child_count(GetSelf);
end;

function TSkRuntimeEffect.GetChildName(const AIndex: Integer): string;
begin
  if (AIndex < 0) or (AIndex >= GetChildCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetChildCount]);
  Result := string(TSkiaAPI.sk4d_runtimeeffect_get_child_name(GetSelf, AIndex));
end;

function TSkRuntimeEffect.GetChildShader(const AName: string): ISkShader;
begin
  Result := GetChildShader(IndexOfChild(AName));
end;

function TSkRuntimeEffect.GetChildShader(const AIndex: Integer): ISkShader;
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.Shader then
    raise ESkException.Create(SInvalidOperation);
  Result := FChildren[AIndex] as ISkShader;
end;

function TSkRuntimeEffect.GetChildType(
  const AIndex: Integer): TSkRuntimeEffectChildType;
begin
  if (AIndex < 0) or (AIndex >= GetChildCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetChildCount]);
  Result := TSkRuntimeEffectChildType(TSkiaAPI.sk4d_runtimeeffect_get_child_type(GetSelf, AIndex));
end;

function TSkRuntimeEffect.GetChildType(
  const AName: string): TSkRuntimeEffectChildType;
begin
  Result := GetChildType(IndexOfChild(AName));
end;

function TSkRuntimeEffect.GetUniform(const AName: string): Pointer;
begin
  Result := GetUniform(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniform(const AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetUniformCount]);
  Result := Pointer(NativeUInt(FUniformData) + GetUniformOffset(AIndex));
end;

function TSkRuntimeEffect.GetUniformCount: Integer;
begin
  Result := TSkiaAPI.sk4d_runtimeeffect_get_uniform_count(GetSelf);
end;

function TSkRuntimeEffect.GetUniformData: Pointer;
begin
  Result := FUniformData;
end;

function TSkRuntimeEffect.GetUniformDataSize: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_runtimeeffect_get_uniform_data_size(GetSelf);
end;

function TSkRuntimeEffect.GetUniformName(const AIndex: Integer): string;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetUniformCount]);
  Result := string(TSkiaAPI.sk4d_runtimeeffect_get_uniform_name(GetSelf, AIndex));
end;

function TSkRuntimeEffect.GetUniformOffset(const AIndex: Integer): NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetUniformCount]);
  Result := TSkiaAPI.sk4d_runtimeeffect_get_uniform_offset(GetSelf, AIndex);
end;

function TSkRuntimeEffect.GetUniformOffset(const AName: string): NativeUInt;
begin
  Result := GetUniformOffset(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniformType(
  const AIndex: Integer): TSkRuntimeEffectUniformType;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetUniformCount]);
  Result := TSkRuntimeEffectUniformType(TSkiaAPI.sk4d_runtimeeffect_get_uniform_type(GetSelf, AIndex));
end;

function TSkRuntimeEffect.GetUniformType(
  const AName: string): TSkRuntimeEffectUniformType;
begin
  Result := GetUniformType(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniformTypeCount(const AName: string): Integer;
begin
  Result := GetUniformTypeCount(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.GetUniformTypeCount(const AIndex: Integer): Integer;
begin
  if (AIndex < 0) or (AIndex >= GetUniformCount) then
    raise ESkArgumentException.CreateFmt(SParamOutOfRange, ['AIndex', 0, GetUniformCount]);
  Result := TSkiaAPI.sk4d_runtimeeffect_get_uniform_type_count(GetSelf, AIndex);
end;

function TSkRuntimeEffect.IndexOfChild(const AName: string): Integer;
begin
  Result := TSkiaAPI.sk4d_runtimeeffect_index_of_child(GetSelf, MarshaledAString(UTF8String(AName)));
end;

function TSkRuntimeEffect.IndexOfUniform(const AName: string): Integer;
begin
  Result := TSkiaAPI.sk4d_runtimeeffect_index_of_uniform(GetSelf, MarshaledAString(UTF8String(AName)));
end;

function TSkRuntimeEffect.IsUniformTypeOrdinal(const AIndex: Integer): Boolean;
begin
  Result := (GetUniformType(AIndex) in [TSkRuntimeEffectUniformType.Int, TSkRuntimeEffectUniformType.Int2, TSkRuntimeEffectUniformType.Int3, TSkRuntimeEffectUniformType.Int4]);
end;

function TSkRuntimeEffect.IsUniformTypeOrdinal(const AName: string): Boolean;
begin
  Result := IsUniformTypeOrdinal(IndexOfUniform(AName));
end;

function TSkRuntimeEffect.MakeBlender: ISkBlender;
var
  I: Integer;
  LChildren: Pointer;
  LHandles: TArray<THandle>;
begin
  if Length(FChildren) > 0 then
  begin
    SetLength(LHandles, Length(FChildren));
    for I := 0 to Length(FChildren) - 1 do
      LHandles[I] := TSkBindings.SafeGetSelf(FChildren[I]);
    LChildren := @LHandles[0];
  end
  else
    LChildren := nil;
  Result := TSkBindings.SafeCreate<TSkBlender>(TSkiaAPI.sk4d_runtimeeffect_make_blender(GetSelf, GetUniformData, LChildren));
end;

function TSkRuntimeEffect.MakeColorFilter: ISkColorFilter;
var
  I: Integer;
  LChildren: Pointer;
  LHandles: TArray<THandle>;
begin
  if Length(FChildren) > 0 then
  begin
    SetLength(LHandles, Length(FChildren));
    for I := 0 to Length(FChildren) - 1 do
      LHandles[I] := TSkBindings.SafeGetSelf(FChildren[I]);
    LChildren := @LHandles[0];
  end
  else
    LChildren := nil;
  Result := TSkBindings.SafeCreate<TSkColorFilter>(TSkiaAPI.sk4d_runtimeeffect_make_color_filter(GetSelf, GetUniformData, LChildren));
end;

class function TSkRuntimeEffect.MakeForBlender(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_blender(MarshaledAString(UTF8String(ASkSL)), LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForBlender(
  const ASkSL: string): ISkRuntimeEffect;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_blender(MarshaledAString(UTF8String(ASkSL)), 0));
end;

class function TSkRuntimeEffect.MakeForColorFilter(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_color_filter(MarshaledAString(UTF8String(ASkSL)), LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForColorFilter(
  const ASkSL: string): ISkRuntimeEffect;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_color_filter(MarshaledAString(UTF8String(ASkSL)), 0));
end;

class function TSkRuntimeEffect.MakeForShader(const ASkSL: string;
  out AErrorText: string): ISkRuntimeEffect;
var
  LErrorText: ISkString;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  LErrorText := TSkString.Create;
  Result     := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_shader(MarshaledAString(UTF8String(ASkSL)), LErrorText.Handle));
  AErrorText := LErrorText.Text;
end;

class function TSkRuntimeEffect.MakeForShader(
  const ASkSL: string): ISkRuntimeEffect;
begin
  if ASKSL.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['ASKSL']);
  Result := TSkBindings.SafeCreate<TSkRuntimeEffect>(TSkiaAPI.sk4d_runtimeeffect_make_for_shader(MarshaledAString(UTF8String(ASkSL)), 0));
end;

function TSkRuntimeEffect.MakeShader(const AOpaque: Boolean): ISkShader;
var
  I: Integer;
  LChildren: Pointer;
  LHandles: TArray<THandle>;
begin
  if Length(FChildren) > 0 then
  begin
    SetLength(LHandles, Length(FChildren));
    for I := 0 to Length(FChildren) - 1 do
      LHandles[I] := TSkBindings.SafeGetSelf(FChildren[I]);
    LChildren := @LHandles[0];
  end
  else
    LChildren := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_runtimeeffect_make_shader(GetSelf, GetUniformData, LChildren, nil, AOpaque));
end;

function TSkRuntimeEffect.MakeShader(const ALocalMatrix: TMatrix;
  const AOpaque: Boolean): ISkShader;
var
  I: Integer;
  LChildren: Pointer;
  LHandles: TArray<THandle>;
begin
  if Length(FChildren) > 0 then
  begin
    SetLength(LHandles, Length(FChildren));
    for I := 0 to Length(FChildren) - 1 do
      LHandles[I] := TSkBindings.SafeGetSelf(FChildren[I]);
    LChildren := @LHandles[0];
  end
  else
    LChildren := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_runtimeeffect_make_shader(GetSelf, GetUniformData, LChildren, @sk_matrix_t(ALocalMatrix), AOpaque));
end;

procedure TSkRuntimeEffect.SetChildBlender(const AIndex: Integer;
  const AValue: ISkBlender);
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.Blender then
    raise ESkException.Create(SInvalidOperation);
  FChildren[AIndex] := AValue;
end;

procedure TSkRuntimeEffect.SetChildBlender(const AName: string;
  const AValue: ISkBlender);
begin
  SetChildBlender(IndexOfChild(AName), AValue);
end;

procedure TSkRuntimeEffect.SetChildColorFilter(const AIndex: Integer;
  const AValue: ISkColorFilter);
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.ColorFilter then
    raise ESkException.Create(SInvalidOperation);
  FChildren[AIndex] := AValue;
end;

procedure TSkRuntimeEffect.SetChildColorFilter(const AName: string;
  const AValue: ISkColorFilter);
begin
  SetChildColorFilter(IndexOfChild(AName), AValue);
end;

procedure TSkRuntimeEffect.SetChildShader(const AIndex: Integer;
  const AValue: ISkShader);
begin
  if GetChildType(AIndex) <> TSkRuntimeEffectChildType.Shader then
    raise ESkException.Create(SInvalidOperation);
  FChildren[AIndex] := AValue;
end;

procedure TSkRuntimeEffect.SetChildShader(const AName: string;
  const AValue: ISkShader);
begin
  SetChildShader(IndexOfChild(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TArray<Integer>);
begin
  if Length(AValue) = 0 then
    raise ESkException.Create(SInvalidOperation);
  SetUniform(AIndex, AValue[0], Length(AValue) * SizeOf(Integer));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectInt2);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex, AValue: Integer);
var
  LValue: Single;
begin
  if not IsUniformTypeOrdinal(AIndex) then
  begin
    LValue := AValue;
    SetUniform(AIndex, LValue);
  end
  else
    SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer; const AData;
  const ASize: NativeUInt);
const
  UniformCount: array[TSkRuntimeEffectUniformType] of Integer = (1, 2, 3, 4, 4, 9, 16, 1, 2, 3, 4);
var
  LSize: NativeUInt;
begin
  if IsUniformTypeOrdinal(AIndex) then
    LSize := UniformCount[GetUniformType(AIndex)] * GetUniformTypeCount(AIndex) * SizeOf(Integer)
  else
    LSize := UniformCount[GetUniformType(AIndex)] * GetUniformTypeCount(AIndex) * SizeOf(Single);
  if ASize <> LSize then
    raise ESkException.Create(SInvalidOperation);
  WriteUniform(GetUniformOffset(AIndex), AData, ASize);
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectInt3);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TArray<Single>);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: Single);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt4);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt3);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectInt2);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat2);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat4x4);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat3x3);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat2x2);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat4);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TSkRuntimeEffectFloat3);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: TArray<Integer>);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat2);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat3);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat4);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectInt4);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: Single);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TArray<Single>);
begin
  if Length(AValue) = 0 then
    raise ESkException.Create(SInvalidOperation);
  SetUniform(AIndex, AValue[0], Length(AValue) * SizeOf(Single));
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string;
  const AValue: Integer);
begin
  SetUniform(IndexOfUniform(AName), AValue);
end;

procedure TSkRuntimeEffect.SetUniform(const AName: string; const AData;
  const ASize: NativeUInt);
begin
  SetUniform(IndexOfUniform(AName), AData, ASize);
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat4x4);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat3x3);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

procedure TSkRuntimeEffect.SetUniform(const AIndex: Integer;
  const AValue: TSkRuntimeEffectFloat2x2);
begin
  SetUniform(AIndex, AValue, SizeOf(AValue));
end;

function TSkRuntimeEffect.UniformExists(const AName: string): Boolean;
begin
  Result := IndexOfUniform(AName) >= 0;
end;

constructor TSkRuntimeEffect.Wrap(const AHandle: THandle;
  const AOwnsHandle: Boolean);
begin
  inherited;
  SetLength(FChildren, GetChildCount);
  FUniformData := AllocMem(GetUniformDataSize);
end;

procedure TSkRuntimeEffect.WriteUniform(const AOffset: NativeUInt; const AData;
  const ASize: NativeUInt);
begin
  if (AOffset + ASize) > GetUniformDataSize then
    raise ESkException.Create(SOutOfMemory);
  Move(AData, Pointer(NativeUInt(FUniformData) + AOffset)^, ASize);
end;

{ TSkShader }

class function TSkShader.MakeBlend(const AMode: TSkBlendMode; const ADest,
  ASrc: ISkShader): ISkShader;
begin
  if not Assigned(ADest) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ADest']);
  if not Assigned(ASrc) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ASrc']);
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_blend(sk_blendmode_t(AMode), TSkBindings.GetSelf(ADest), TSkBindings.GetSelf(ASrc)));
end;

class function TSkShader.MakeColor(const AColor: TAlphaColor): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_color(AColor));
end;

class function TSkShader.MakeColor(const AColor: TAlphaColorF;
  AColorSpace: ISkColorSpace): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_color2(@sk_color4f_t(AColor), TSkBindings.SafeGetSelf(AColorSpace)));
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
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_linear(@LPoints[0], @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_linear2(@LPoints[0], @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_linear2(@LPoints[0], @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientLinear(const AStart, AEnd: TPointF;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode): ISkShader;
var
  LPoints: array[0..1] of sk_point_t;
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  LPoints[0] := sk_point_t(AStart);
  LPoints[1] := sk_point_t(AEnd);
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_linear(@LPoints[0], @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
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
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_radial(@sk_point_t(ACenter), ARadius, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
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
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_radial(@sk_point_t(ACenter), ARadius, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const ALocalMatrix: TMatrix; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_radial2(@sk_point_t(ACenter), ARadius, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
end;

class function TSkShader.MakeGradientRadial(const ACenter: TPointF;
  const ARadius: Single; const AColors: TArray<TAlphaColorF>;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  AColorSpace: ISkColorSpace): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_radial2(@sk_point_t(ACenter), ARadius, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single; AColorSpace: ISkColorSpace): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_sweep2(ACenter.X, ACenter.Y, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, @sk_matrix_t(ALocalMatrix)));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; const AStartAngle, AEndAngle: Single;
  AColorSpace: ISkColorSpace): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_sweep2(ACenter.X, ACenter.Y, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, nil));
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
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_sweep(ACenter.X, ACenter.Y, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, nil));
end;

class function TSkShader.MakeGradientSweep(const ACenter: TPointF;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode;
  const AStartAngle, AEndAngle: Single): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_sweep(ACenter.X, ACenter.Y, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), AStartAngle, AEndAngle, @sk_matrix_t(ALocalMatrix)));
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
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_two_point_conical(@sk_point_t(AStart), AStartRadius, @sk_point_t(AEnd), AEndRadius, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColor>; const ALocalMatrix: TMatrix;
  const APositions: TArray<Single>; const ATileMode: TSkTileMode): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_two_point_conical(@sk_point_t(AStart), AStartRadius, @sk_point_t(AEnd), AEndRadius, @AColors[0], LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
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
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_two_point_conical2(@sk_point_t(AStart), AStartRadius, @sk_point_t(AEnd), AEndRadius, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), @sk_matrix_t(ALocalMatrix)));
end;

class function TSkShader.MakeGradientTwoPointConical(const AStart: TPointF;
  const AStartRadius: Single; AEnd: TPointF; const AEndRadius: Single;
  const AColors: TArray<TAlphaColorF>; const APositions: TArray<Single>;
  const ATileMode: TSkTileMode; AColorSpace: ISkColorSpace): ISkShader;
var
  LPositions: pfloat;
begin
  if Length(AColors) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AColors']);
  if Length(APositions) > 0 then
  begin
    if Length(APositions) <> Length(AColors) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
    LPositions := @APositions[0];
  end
  else
    LPositions := nil;
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_gradient_two_point_conical2(@sk_point_t(AStart), AStartRadius, @sk_point_t(AEnd), AEndRadius, @sk_color4f_t(AColors[0]), TSkBindings.SafeGetSelf(AColorSpace), LPositions, Length(AColors), sk_tilemode_t(ATileMode), nil));
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
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize)));
end;

class function TSkShader.MakePerlinNoiseFractalNoise(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_perlin_noise_fractal_noise(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil));
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer; const ASeed: Single;
  const ATileSize: TSize): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, @sk_isize_t(ATileSize)));
end;

class function TSkShader.MakePerlinNoiseTurbulence(const ABaseFrequencyX,
  ABaseFrequencyY: Single; const ANumOctaves: Integer;
  const ASeed: Single): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_perlin_noise_turbulence(ABaseFrequencyX, ABaseFrequencyY, ANumOctaves, ASeed, nil));
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
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_with_color_filter(GetSelf, TSkBindings.GetSelf(AColorFilter)));
end;

function TSkShader.MakeWithLocalMatrix(const AMatrix: TMatrix): ISkShader;
begin
  Result := TSkBindings.SafeCreate<TSkShader>(TSkiaAPI.sk4d_shader_make_with_local_matrix(GetSelf, @sk_matrix_t(AMatrix)));
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

{ TSkSurface }

procedure TSkSurface.Draw(const ACanvas: ISkCanvas; const AX, AY: Single;
  const APaint: ISkPaint);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_surface_draw(GetSelf, TSkBindings.GetSelf(ACanvas), AX, AY, TSkBindings.SafeGetSelf(APaint));
end;

procedure TSkSurface.Flush;
begin
  TSkiaAPI.sk4d_surface_flush(GetSelf);
end;

procedure TSkSurface.FlushAndSubmit(const ASyncCPU: Boolean);
begin
  TSkiaAPI.sk4d_surface_flush_and_submit(GetSelf, ASyncCPU);
end;

function TSkSurface.GetCanvas: ISkCanvas;
begin
  if not Assigned(FCanvasHolder) then
    FCanvasHolder := TSkBindings.SafeCreate<TSkCanvas>(TSkiaAPI.sk4d_surface_get_canvas(GetSelf), False);
  Result := FCanvasHolder;
end;

function TSkSurface.GetProperties: TSkSurfaceProperties;
var
  LResult: sk_surfaceprops_t;
begin
  TSkiaAPI.sk4d_surface_get_props(GetSelf, LResult);
  Result := TSkMapping.ToSurfaceProperties(LResult);
end;

class function TSkSurface.MakeFromCAMetalLayer(const AContext: IGrDirectContext;
  const ALayer: GrMtlHandle; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  out ADrawable: GrMtlHandle; const AProperties: TSkSurfaceProperties;
  AColorSpace: ISkColorSpace): ISkSurface;
var
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_ca_metal_layer(TSkBindings.GetSelf(AContext), ALayer, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), @LProperties, ADrawable));
end;

class function TSkSurface.MakeFromCAMetalLayer(const AContext: IGrDirectContext;
  const ALayer: GrMtlHandle; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  out ADrawable: GrMtlHandle; AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_ca_metal_layer(TSkBindings.GetSelf(AContext), ALayer, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), nil, ADrawable));
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
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_mtk_view(TSkBindings.GetSelf(AContext), AView, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), @LProperties));
end;

class function TSkSurface.MakeFromMTKView(const AContext: IGrDirectContext;
  const AView: GrMtlHandle; const AOrigin: TGrSurfaceOrigin;
  const ASampleCount: Integer; const AColorType: TSkColorType;
  AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_mtk_view(TSkBindings.GetSelf(AContext), AView, gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), nil));
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
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_render_target(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ARenderTarget), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), @LProperties));
end;

class function TSkSurface.MakeFromRenderTarget(const AContext: IGrDirectContext;
  const ARenderTarget: IGrBackendRenderTarget; const AOrigin: TGrSurfaceOrigin;
  const AColorType: TSkColorType; AColorSpace: ISkColorSpace): ISkSurface;
begin
  if not Assigned(AContext) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AContext']);
  if not Assigned(ARenderTarget) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ARenderTarget']);
  Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_render_target(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ARenderTarget), gr_surfaceorigin_t(AOrigin), sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), nil));
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
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_texture(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ATexture), gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), @LProperties));
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
  Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_from_texture(TSkBindings.GetSelf(AContext), TSkBindings.GetSelf(ATexture), gr_surfaceorigin_t(AOrigin), ASampleCount, sk_colortype_t(AColorType), TSkBindings.SafeGetSelf(AColorSpace), nil));
end;

function TSkSurface.MakeImageSnapshot(const ABounds: TRect): ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_surface_make_image_snapshot2(GetSelf, @sk_irect_t(ABounds)));
end;

function TSkSurface.MakeImageSnapshot: ISkImage;
begin
  Result := TSkBindings.SafeCreate<TSkImage>(TSkiaAPI.sk4d_surface_make_image_snapshot(GetSelf));
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const ARowBytes: NativeUInt): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
begin
  LImageInfo := TSkMapping.AsImageInfo(AImageInfo);
  Result     := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster(@LImageInfo, ARowBytes, nil));
end;

class function TSkSurface.MakeRaster(const AImageInfo: TSkImageInfo;
  const ARowBytes: NativeUInt;
  const AProperties: TSkSurfaceProperties): ISkSurface;
var
  LImageInfo: sk_imageinfo_t;
  LProperties: sk_surfaceprops_t;
begin
  LImageInfo  := TSkMapping.AsImageInfo(AImageInfo);
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster(@LImageInfo, ARowBytes, @LProperties));
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
  LProperties: sk_surfaceprops_t;
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  if Assigned(ARasterReleaseProc) then
  begin
    Result := TSkDelegate<TSkSurfaceRasterReleaseProc>.Initialize<ISkSurface>(ARasterReleaseProc,
      function (const AContextProc: Pointer): ISkSurface
      begin
        Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster_direct(TSkBindings.GetSelf(APixmap), raster_release_proc, AContextProc, @LProperties));
      end);
  end
  else
    Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster_direct(TSkBindings.GetSelf(APixmap), nil, nil, @LProperties));
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
begin
  if not Assigned(APixmap) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APixmap']);
  if Assigned(ARasterReleaseProc) then
  begin
    Result := TSkDelegate<TSkSurfaceRasterReleaseProc>.Initialize<ISkSurface>(ARasterReleaseProc,
      function (const AContextProc: Pointer): ISkSurface
      begin
        Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster_direct(TSkBindings.GetSelf(APixmap), raster_release_proc, AContextProc, nil));
      end);
  end
  else
    Result := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_raster_direct(TSkBindings.GetSelf(APixmap), nil, nil, nil));
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
  LImageInfo := TSkMapping.AsImageInfo(AImageInfo);
  Result     := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_render_target(TSkBindings.GetSelf(AContext), AIsBudgeted, @LImageInfo, ASampleCount, gr_surfaceorigin_t(AOrigin), nil, AShouldCreateWithMips));
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
  LImageInfo  := TSkMapping.AsImageInfo(AImageInfo);
  LProperties := TSkMapping.AsSurfaceProperties(AProperties);
  Result      := TSkBindings.SafeCreate<TSkSurface>(TSkiaAPI.sk4d_surface_make_render_target(TSkBindings.GetSelf(AContext), AIsBudgeted, @LImageInfo, ASampleCount, gr_surfaceorigin_t(AOrigin), @LProperties, AShouldCreateWithMips));
end;

function TSkSurface.PeekPixels: ISkPixmap;
begin
  Result := TSkBindings.SafeCreate<TSkPixmap>(TSkiaAPI.sk4d_surface_peek_pixels(GetSelf));
end;

class procedure TSkSurface.raster_release_proc(pixels, context: Pointer);
begin
  TSkDelegate<TSkSurfaceRasterReleaseProc>.Invoke(context,
    procedure (const AProc: TSkSurfaceRasterReleaseProc)
    begin
      AProc(pixels);
    end);
  TSkDelegate<TSkSurfaceRasterReleaseProc>.Finalize(context);
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
  Result := TSkiaAPI.sk4d_surface_read_pixels(GetSelf, TSkBindings.GetSelf(ADest), ASrcX, ASrcY);
end;

{ TSkSVGCanvas }

class function TSkSVGCanvas.Make(const ABounds: TRectF; const AStream: TStream;
  const AFlags: TSkSVGCanvasFlags): ISkCanvas;
begin
  Result := TSkManagedSVGCanvas.Make(ABounds, AStream, AFlags);
end;

{ TSkTextBlob }

function TSkTextBlob.GetIntercepts(const AUpperBounds, ALowerBounds: Single;
  const APaint: ISkPaint): TArray<Single>;
var
  LBounds: array[0..1] of Single;
  LCount: Integer;
begin
  LBounds[0] := AUpperBounds;
  LBounds[1] := ALowerBounds;
  LCount := TSkiaAPI.sk4d_textblob_get_intercepts(GetSelf, @LBounds[0], nil, TSkBindings.SafeGetSelf(APaint));
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_textblob_get_intercepts(GetSelf, @LBounds[0], @Result[0], TSkBindings.SafeGetSelf(APaint));
end;

class function TSkTextBlob.MakeFromText(const AText: string;
  const AFont: ISkFont): ISkTextBlob;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if AText.IsEmpty then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_textblob_make_from_text(@AText[Low(AText)], Length(AText) * 2, TSkBindings.GetSelf(AFont), sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextHorizontallyPositioned(
  const AText: string; const AXPositions: TArray<Single>; const AY: Single;
  const AFont: ISkFont): ISkTextBlob;
begin
  if Length(AXPositions) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AXPositions']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if AText.IsEmpty then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_textblob_make_from_text_horizontally_positioned(@AText[Low(AText)], Length(AText) * 2, @AXPositions[0], AY, TSkBindings.GetSelf(AFont), sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextPositioned(const AText: string;
  const APositions: TArray<TPointF>; const AFont: ISkFont): ISkTextBlob;
begin
  if Length(APositions) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['APositions']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if AText.IsEmpty then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_textblob_make_from_text_positioned(@AText[Low(AText)], Length(AText) * 2, @sk_point_t(APositions[0]), TSkBindings.GetSelf(AFont), sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class function TSkTextBlob.MakeFromTextTransform(const AText: string;
  const AMatrices: TArray<TSkRotationScaleMatrix>;
  const AFont: ISkFont): ISkTextBlob;
begin
  if Length(AMatrices) <> Length(AText) then
    raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AMatrices']);
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  if AText.IsEmpty then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_textblob_make_from_text_transform(@AText[Low(AText)], Length(AText) * 2, @sk_rotationscalematrix_t(AMatrices[0]), TSkBindings.GetSelf(AFont), sk_textencoding_t.UTF16_SK_TEXTENCODING));
end;

class procedure TSkTextBlob.RefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_textblob_ref(AHandle);
end;

class procedure TSkTextBlob.UnrefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_textblob_unref(AHandle);
end;

{ TSkTraceMemoryDumpBaseClass }

constructor TSkTraceMemoryDumpBaseClass.Create(const ADetailedDump,
  ADumpWrappedObjects: Boolean);
begin
  inherited Wrap(TSkiaAPI.sk4d_tracememorydumpbaseclass_create(ADetailedDump, ADumpWrappedObjects, Self));
end;

class constructor TSkTraceMemoryDumpBaseClass.Create;
var
  LProcs: sk_tracememorydumpbaseclass_procs_t;
begin
  LProcs.dump_numeric_value := dump_numeric_value_proc;
  LProcs.dump_string_value  := dump_string_value_proc;
  TSkiaAPI.sk4d_tracememorydumpbaseclass_set_procs(@LProcs);
end;

class procedure TSkTraceMemoryDumpBaseClass.DestroyHandle(
  const AHandle: THandle);
begin
  TSkiaAPI.sk4d_tracememorydumpbaseclass_destroy(AHandle);
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

{ TSkFontStyle }

class function TSkFontStyle.Bold: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Bold, TSkFontWidth.Normal, TSkFontSlant.Upright);
end;

class function TSkFontStyle.BoldItalic: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Bold, TSkFontWidth.Normal, TSkFontSlant.Italic);
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
  Result := TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Italic);
end;

class function TSkFontStyle.Normal: TSkFontStyle;
begin
  Result := TSkFontStyle.Create(TSkFontWeight.Normal, TSkFontWidth.Normal, TSkFontSlant.Upright);
end;

class operator TSkFontStyle.NotEqual(const AFontStyle1,
  AFontStyle2: TSkFontStyle): Boolean;
begin
  Result := not (AFontStyle1 = AFontStyle2);
end;

{ TSkTypeface }

function TSkTypeface.GetFamilyName: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(TSkiaAPI.sk4d_typeface_get_family_name(GetSelf));
  Result  := LResult.Text;
end;

function TSkTypeface.GetSlant: TSkFontSlant;
begin
  Result := TSkFontSlant(TSkiaAPI.sk4d_typeface_get_slant(GetSelf));
end;

function TSkTypeface.GetStyle: TSkFontStyle;
begin
  TSkiaAPI.sk4d_typeface_get_style(GetSelf, sk_fontstyle_t(Result));
end;

function TSkTypeface.GetWeight: Integer;
begin
  Result := TSkiaAPI.sk4d_typeface_get_weight(GetSelf);
end;

function TSkTypeface.GetWidth: Integer;
begin
  Result := TSkiaAPI.sk4d_typeface_get_width(GetSelf);
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
  Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_typeface_make_default());
end;

class function TSkTypeface.MakeFromFile(const AFileName: string;
  const ATTCIndex: Integer): ISkTypeface;
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_typeface_make_from_file(MarshaledAString(UTF8String(AFileName)), ATTCIndex));
end;

class function TSkTypeface.MakeFromName(const AFamilyName: string;
  const AStyle: TSkFontStyle): ISkTypeface;
begin
  if AFamilyName.IsEmpty then
    Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_typeface_make_from_name(nil, @sk_fontstyle_t(AStyle)))
  else
    Result := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_typeface_make_from_name(MarshaledAString(UTF8String(AFamilyName)), @sk_fontstyle_t(AStyle)));
end;

class function TSkTypeface.MakeFromStream(const AStream: TStream;
  const ATTCIndex: Integer): ISkTypeface;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkTypeFace>(TSkiaAPI.sk4d_typeface_make_from_stream(LStream.Handle, ATTCIndex));
end;

{ TSkVersion }

class function TSkVersion.GetBuild: Integer;
begin
  Result := TSkiaAPI.sk4d_version_get_build();
end;

class function TSkVersion.GetMajor: Integer;
begin
  Result := TSkiaAPI.sk4d_version_get_major();
end;

class function TSkVersion.GetMilestone: Integer;
begin
  Result := TSkiaAPI.sk4d_version_get_milestone();
end;

class function TSkVersion.GetMinor: Integer;
begin
  Result := TSkiaAPI.sk4d_version_get_minor();
end;

{ TSkVertices }

class function TSkVertices.MakeCopy(const AVertexMode: TSkVertexMode;
  const APositions, ATextures: TArray<TPointF>;
  const AColors: TArray<TAlphaColor>;
  const AIndices: TArray<Word>): ISkVertices;
var
  LColors: psk_color_t;
  LIndices: puint16_t;
  LTextures: psk_point_t;
begin
  if Length(APositions) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['APositions']);
  if Length(ATextures) > 0 then
  begin
    if Length(ATextures) <> Length(APositions) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['ATextures']);
    LTextures := @sk_point_t(ATextures[0]);
  end
  else
    LTextures := nil;
  if Length(AColors) > 0 then
  begin
    if Length(AColors) <> Length(APositions) then
      raise ESkArgumentException.CreateFmt(SParamSizeMismatch, ['AColors']);
    LColors := @AColors[0];
  end
  else
    LColors := nil;
  if Length(AIndices) > 0 then
    LIndices := @AIndices[0]
  else
    LIndices := nil;
  Result := TSkBindings.SafeCreate<TSkVertices>(TSkiaAPI.sk4d_vertices_make_copy(sk_vertexmode_t(AVerTexMode), Length(APositions), @sk_point_t(APositions[0]), LTextures, LColors, Length(AIndices), LIndices));
end;

class procedure TSkVertices.RefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_vertices_ref(AHandle);
end;

class procedure TSkVertices.UnrefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_vertices_unref(AHandle);
end;

{ TSkParticleEffect }

class constructor TSkParticleEffect.Create;
begin
  TSkiaAPI.sk4d_particleeffect_init();
end;

function TSkParticleEffect.GetPosition: TPointF;
begin
  TSkiaAPI.sk4d_particleeffect_get_position(GetSelf, sk_point_t(Result));
end;

function TSkParticleEffect.GetRate: Single;
begin
  Result := TSkiaAPI.sk4d_particleeffect_get_rate(GetSelf);
end;

function TSkParticleEffect.GetUniform(
  const AIndex: NativeUInt): TSkParticleUniform;
begin
  TSkiaAPI.sk4d_particleeffect_get_uniform(GetSelf, AIndex, sk_particleuniform_t(Result));
end;

function TSkParticleEffect.GetUniformCount: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_particleeffect_get_uniform_count(GetSelf);
end;

function TSkParticleEffect.GetUniformData: PSkParticleUniformData;
begin
  Result := PSkParticleUniformData(TSkiaAPI.sk4d_particleeffect_get_uniform_data(GetSelf));
end;

function TSkParticleEffect.GetUniformDataCount: Integer;
begin
  Result := TSkiaAPI.sk4d_particleeffect_get_uniform_data_count(GetSelf);
end;

function TSkParticleEffect.GetUniformName(const AIndex: NativeUInt): string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(TSkiaAPI.sk4d_particleeffect_get_uniform_name(GetSelf, AIndex));
  Result  := LResult.Text;
end;

class function TSkParticleEffect.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider): ISkParticleEffect;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    Result := MakeFromStream(LStream, AResourceProvider);
  finally
    LStream.Free;
  end;
end;

class function TSkParticleEffect.MakeFromFile(
  const AFileName: string): ISkParticleEffect;
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkParticleEffect>(TSkiaAPI.sk4d_particleeffect_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkParticleEffect.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider): ISkParticleEffect;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkParticleEffect>(TSkiaAPI.sk4d_particleeffect_make_from_stream(LStream.Handle, TSkBindings.SafeGetSelf(AResourceProvider)));
end;

procedure TSkParticleEffect.Render(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_particleeffect_render(GetSelf, TSkBindings.GetSelf(ACanvas));
end;

procedure TSkParticleEffect.SetPosition(const AValue: TPointF);
begin
  TSkiaAPI.sk4d_particleeffect_set_position(GetSelf, @sk_point_t(AValue));
end;

procedure TSkParticleEffect.SetRate(const AValue: Single);
begin
  TSkiaAPI.sk4d_particleeffect_set_rate(GetSelf, AValue);
end;

function TSkParticleEffect.SetUniform(const AName: string;
  const AData: TArray<Single>): Boolean;
begin
  if Length(AData) < 1 then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AData']);
  Result := TSkiaAPI.sk4d_particleeffect_set_uniform(GetSelf, MarshaledAString(UTF8String(AName)), @AData[0], Length(AData));
end;

procedure TSkParticleEffect.Start(const ANow: Double; const ALooping: Boolean);
begin
  TSkiaAPI.sk4d_particleeffect_start(GetSelf, ANow, ALooping);
end;

procedure TSkParticleEffect.Update(const ANow: Double);
begin
  TSkiaAPI.sk4d_particleeffect_update(GetSelf, ANow);
end;

{ TSkottieAnimation }

function TSkottieAnimation.GetDuration: Double;
begin
  Result := TSkiaAPI.sk4d_skottieanimation_get_duration(GetSelf);
end;

function TSkottieAnimation.GetFPS: Double;
begin
  Result := TSkiaAPI.sk4d_skottieanimation_get_fps(GetSelf);
end;

function TSkottieAnimation.GetInPoint: Double;
begin
  Result := TSkiaAPI.sk4d_skottieanimation_get_in_point(GetSelf);
end;

function TSkottieAnimation.GetOutPoint: Double;
begin
  Result := TSkiaAPI.sk4d_skottieanimation_get_out_point(GetSelf);
end;

function TSkottieAnimation.GetSize: TSizeF;
begin
  TSkiaAPI.sk4d_skottieanimation_get_size(GetSelf, sk_size_t(Result));
end;

function TSkottieAnimation.GetVersion: string;
begin
  Result := string(TSkiaAPI.sk4d_skottieanimation_get_version(GetSelf));
end;

class function TSkottieAnimation.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider): ISkottieAnimation;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    Result := MakeFromStream(LStream, AResourceProvider);
  finally
    LStream.Free;
  end;
end;

class function TSkottieAnimation.MakeFromFile(
  const AFileName: string): ISkottieAnimation;
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkottieAnimation>(TSkiaAPI.sk4d_skottieanimation_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkottieAnimation.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider): ISkottieAnimation;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result := TSkBindings.SafeCreate<TSkottieAnimation>(TSkiaAPI.sk4d_skottieanimation_make_from_stream(LStream.Handle, TSkBindings.SafeGetSelf(AResourceProvider)));
end;

class procedure TSkottieAnimation.RefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_skottieanimation_ref(AHandle);
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_skottieanimation_render(GetSelf, TSkBindings.GetSelf(ACanvas), nil, Byte(ARenderFlags));
end;

procedure TSkottieAnimation.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF; const ARenderFlags: TSkottieAnimationRenderFlags);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_skottieanimation_render(GetSelf, TSkBindings.GetSelf(ACanvas), @sk_rect_t(ADest), Byte(ARenderFlags));
end;

procedure TSkottieAnimation.SeekFrame(const ATick: Double);
begin
  TSkiaAPI.sk4d_skottieanimation_seek_frame(GetSelf, ATick);
end;

procedure TSkottieAnimation.SeekFrameTime(const ATick: Double);
begin
  TSkiaAPI.sk4d_skottieanimation_seek_frame_time(GetSelf, ATick);
end;

class procedure TSkottieAnimation.UnrefHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_skottieanimation_unref(AHandle);
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
            (SameValue(AMetrics1.Ascent,   AMetrics2.Ascent,   Epsilon)) and
            (SameValue(AMetrics1.Descent,  AMetrics2.Descent,  Epsilon)) and
            (SameValue(AMetrics1.Height,   AMetrics2.Height,   Epsilon)) and
            (SameValue(AMetrics1.Width,    AMetrics2.Width,    Epsilon)) and
            (SameValue(AMetrics1.Left,     AMetrics2.Left,     Epsilon)) and
            (SameValue(AMetrics1.Baseline, AMetrics2.Baseline, Epsilon));
end;

class operator TSkMetrics.NotEqual(const AMetrics1,
  AMetrics2: TSkMetrics): Boolean;
begin
  Result := not (AMetrics1 = AMetrics2);
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

{ TSkParagraph }

class procedure TSkParagraph.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_paragraph_destroy(AHandle);
end;

function TSkParagraph.DidExceedMaxLines: Boolean;
begin
  Result := TSkiaAPI.sk4d_paragraph_did_exceed_max_lines(GetSelf);
end;

function TSkParagraph.GetAlphabeticBaseline: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_alphabetic_baseline(GetSelf);
end;

function TSkParagraph.GetGlyphPositionAtCoordinate(const ADeltaX,
  ADeltaY: Single): TSkPositionAffinity;
begin
  TSkiaAPI.sk4d_paragraph_get_glyph_position_at_coordinate(GetSelf, ADeltaX, ADeltaY, sk_positionaffinity_t(Result));
end;

function TSkParagraph.GetHeight: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_height(GetSelf);
end;

function TSkParagraph.GetIdeographicBaseline: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_ideographic_baseline(GetSelf);
end;

function TSkParagraph.GetLineMetrics: TArray<TSkMetrics>;
var
  LCount: NativeUInt;
begin
  LCount := TSkiaAPI.sk4d_paragraph_get_line_metrics(GetSelf, nil);
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_paragraph_get_line_metrics(GetSelf, @sk_metrics_t(Result[0]));
end;

function TSkParagraph.GetLongestLine: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_longest_line(GetSelf);
end;

function TSkParagraph.GetMaxIntrinsicWidth: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_max_intrinsic_width(GetSelf);
end;

function TSkParagraph.GetMaxWidth: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_max_width(GetSelf);
end;

function TSkParagraph.GetMinIntrinsicWidth: Single;
begin
  Result := TSkiaAPI.sk4d_paragraph_get_min_intrinsic_width(GetSelf);
end;

function TSkParagraph.GetRectsForPlaceholders: TArray<TSkTextBox>;
var
  LCount: NativeUInt;
begin
  LCount := TSkiaAPI.sk4d_paragraph_get_rects_for_placeholders(GetSelf, nil);
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_paragraph_get_rects_for_placeholders(GetSelf, @sk_textbox_t(Result[0]));
end;

function TSkParagraph.GetRectsForRange(const AStart, AEnd: Cardinal;
  const ARectHeightStyle: TSkRectHeightStyle;
  const ARectWidthStyle: TSkRectWidthStyle): TArray<TSkTextBox>;
var
  LCount: NativeUInt;
begin
  LCount := TSkiaAPI.sk4d_paragraph_get_rects_for_range(GetSelf, AStart, AEnd, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), nil);
  if LCount = 0 then
    Exit(nil);
  SetLength(Result, LCount);
  TSkiaAPI.sk4d_paragraph_get_rects_for_range(GetSelf, AStart, AEnd, sk_rectheightstyle_t(ARectHeightStyle), sk_rectwidthstyle_t(ARectWidthStyle), @sk_textbox_t(Result[0]));
end;

procedure TSkParagraph.GetWordBoundary(const AOffset: Cardinal; out AStart,
  AEnd: Cardinal);
begin
  TSkiaAPI.sk4d_paragraph_get_word_boundary(GetSelf, AOffset, AStart, AEnd);
end;

procedure TSkParagraph.Layout(const AWidth: Single);
begin
  TSkiaAPI.sk4d_paragraph_layout(GetSelf, AWidth);
end;

procedure TSkParagraph.Paint(const ACanvas: ISkCanvas; const AX, AY: Single);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_paragraph_paint(GetSelf, TSkBindings.GetSelf(ACanvas), AX, AY);
end;

function TSkParagraph.ToPath: ISkPath;
begin
  Result := TSkPath.Wrap(TSkiaAPI.sk4d_paragraph_to_path(GetSelf));
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
  Result := (APlaceholderStyle1.Alignment = APlaceholderStyle2.Alignment) and
            (APlaceholderStyle1.Baseline  = APlaceholderStyle2.Baseline ) and
            (SameValue(APlaceholderStyle1.Width,  APlaceholderStyle2.Width,  Epsilon)) and
            (SameValue(APlaceholderStyle1.Height, APlaceholderStyle2.Height, Epsilon)) and
            ((APlaceholderStyle1.Alignment <> TSkPlaceholderAlignment.Baseline) or (SameValue(APlaceholderStyle1.BaselineOffset, APlaceholderStyle2.BaselineOffset, Epsilon)));
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
  TSkiaAPI.sk4d_paragraphbuilder_add_placeholder(GetSelf, @sk_placeholderstyle_t(APlaceholder));
end;

procedure TSkParagraphBuilder.AddText(const AText: string);
var
  LText: UTF8String;
begin
  if AText.IsEmpty then
    Exit;
  LText := UTF8String(AText);
  TSkiaAPI.sk4d_paragraphbuilder_add_text(GetSelf, @LText[Low(LText)], Length(LText));
end;

function TSkParagraphBuilder.Build: ISkParagraph;
begin
  Result := TSkParagraph.Wrap(TSkiaAPI.sk4d_paragraphbuilder_build(GetSelf));
end;

constructor TSkParagraphBuilder.Create(const AParagraphStyle: ISkParagraphStyle;
  const AFontProvider: ISkTypefaceFontProvider;
  const AEnableFontFallback: Boolean);
begin
  if not Assigned(AParagraphStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AParagraphStyle']);
  if not Assigned(AFontProvider) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFontProvider']);
  inherited Wrap(TSkiaAPI.sk4d_paragraphbuilder_create2(TSkBindings.GetSelf(AParagraphStyle), TSkBindings.GetSelf(AFontProvider), AEnableFontFallback));
end;

constructor TSkParagraphBuilder.Create(
  const AParagraphStyle: ISkParagraphStyle);
begin
  if not Assigned(AParagraphStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AParagraphStyle']);
  inherited Wrap(TSkiaAPI.sk4d_paragraphbuilder_create(TSkBindings.GetSelf(AParagraphStyle)));
end;

class procedure TSkParagraphBuilder.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_paragraphbuilder_destroy(AHandle);
end;

procedure TSkParagraphBuilder.Pop;
begin
  TSkiaAPI.sk4d_paragraphbuilder_pop(GetSelf);
end;

procedure TSkParagraphBuilder.PushStyle(const ATextStyle: ISkTextStyle);
begin
  if not Assigned(ATextStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextStyle']);
  TSkiaAPI.sk4d_paragraphbuilder_push_style(GetSelf, TSkBindings.GetSelf(ATextStyle));
end;

{ TSkStrutStyle }

constructor TSkStrutStyle.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_strutstyle_create());
end;

class procedure TSkStrutStyle.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_strutstyle_destroy(AHandle);
end;

function TSkStrutStyle.GetEnabled: Boolean;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_enabled(GetSelf);
end;

function TSkStrutStyle.GetFontFamilies: TArray<string>;
begin
  Result := TSkBindings.GetStrings(GetSelf,
    function (const AHandle: THandle; const AStrings: PMarshaledAString): Integer
    begin
      Result := TSkiaAPI.sk4d_strutstyle_get_font_families(AHandle, AStrings);
    end);
end;

function TSkStrutStyle.GetFontSize: Single;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_font_size(GetSelf);
end;

function TSkStrutStyle.GetFontStyle: TSkFontStyle;
begin
  TSkiaAPI.sk4d_strutstyle_get_font_style(GetSelf, sk_fontstyle_t(Result));
end;

function TSkStrutStyle.GetForceHeight: Boolean;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_force_height(GetSelf);
end;

function TSkStrutStyle.GetHalfLeading: Boolean;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_half_leading(GetSelf);
end;

function TSkStrutStyle.GetHeightMultiplier: Single;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_height_multiplier(GetSelf);
end;

function TSkStrutStyle.GetLeading: Single;
begin
  Result := TSkiaAPI.sk4d_strutstyle_get_leading(GetSelf);
end;

function TSkStrutStyle.IsEqual(const AStrutStyle: ISkStrutStyle): Boolean;
begin
  if not Assigned(AStrutStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AStrutStyle']);
  Result := TSkiaAPI.sk4d_strutstyle_is_equal(GetSelf, TSkBindings.GetSelf(AStrutStyle));
end;

procedure TSkStrutStyle.SetEnabled(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_strutstyle_set_enabled(GetSelf, AValue);
end;

procedure TSkStrutStyle.SetFontFamilies(const AValue: TArray<string>);
begin
  TSkBindings.SetStrings(GetSelf, AValue,
    procedure (const AHandle: THandle; const AStrings: PMarshaledAString; const ALength: Integer)
    begin
      TSkiaAPI.sk4d_strutstyle_set_font_families(AHandle, AStrings, ALength);
    end);
end;

procedure TSkStrutStyle.SetFontSize(const AValue: Single);
begin
  TSkiaAPI.sk4d_strutstyle_set_font_size(GetSelf, AValue);
end;

procedure TSkStrutStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  TSkiaAPI.sk4d_strutstyle_set_font_style(GetSelf, @sk_fontstyle_t(AValue));
end;

procedure TSkStrutStyle.SetForceHeight(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_strutstyle_set_force_height(GetSelf, AValue);
end;

procedure TSkStrutStyle.SetHalfLeading(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_strutstyle_set_half_leading(GetSelf, AValue);
end;

procedure TSkStrutStyle.SetHeightMultiplier(const AValue: Single);
begin
  TSkiaAPI.sk4d_strutstyle_set_height_multiplier(GetSelf, AValue);
end;

procedure TSkStrutStyle.SetLeading(const AValue: Single);
begin
  TSkiaAPI.sk4d_strutstyle_set_leading(GetSelf, AValue);
end;

{ TSkParagraphStyle }

constructor TSkParagraphStyle.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_paragraphstyle_create());
end;

class procedure TSkParagraphStyle.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_paragraphstyle_destroy(AHandle);
end;

procedure TSkParagraphStyle.DisableHinting;
begin
  TSkiaAPI.sk4d_paragraphstyle_disable_hinting(GetSelf);
end;

function TSkParagraphStyle.GetEllipsis: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(TSkiaAPI.sk4d_paragraphstyle_get_ellipsis(GetSelf));
  Result  := LResult.Text;
end;

function TSkParagraphStyle.GetHeight: Single;
begin
  Result := TSkiaAPI.sk4d_paragraphstyle_get_height(GetSelf);
end;

function TSkParagraphStyle.GetMaxLines: NativeUInt;
begin
  Result := TSkiaAPI.sk4d_paragraphstyle_get_max_lines(GetSelf);
end;

function TSkParagraphStyle.GetStrutStyle: ISkStrutStyle;
begin
  Result := TSkStrutStyle.Wrap(TSkiaAPI.sk4d_paragraphstyle_get_strut_style(GetSelf), False);
end;

function TSkParagraphStyle.GetTextAlign: TSkTextAlign;
begin
  Result := TSkTextAlign(TSkiaAPI.sk4d_paragraphstyle_get_text_align(GetSelf));
end;

function TSkParagraphStyle.GetTextDirection: TSkTextDirection;
begin
  Result := TSkTextDirection(TSkiaAPI.sk4d_paragraphstyle_get_text_direction(GetSelf));
end;

function TSkParagraphStyle.GetTextHeightBehaviors: TSkTextHeightBehaviors;
begin
  Result := TSkTextHeightBehaviors(Byte(TSkiaAPI.sk4d_paragraphstyle_get_text_height_behaviors(GetSelf)));
end;

function TSkParagraphStyle.GetTextStyle: ISkTextStyle;
begin
  Result := TSkTextStyle.Wrap(TSkiaAPI.sk4d_paragraphstyle_get_text_style(GetSelf), False);
end;

procedure TSkParagraphStyle.SetEllipsis(const AValue: string);
begin
  if AValue.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AValue']);
  TSkiaAPI.sk4d_paragraphstyle_set_ellipsis(GetSelf, MarshaledAString(UTF8String(AValue)));
end;

procedure TSkParagraphStyle.SetHeight(const AValue: Single);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_height(GetSelf, AValue);
end;

procedure TSkParagraphStyle.SetMaxLines(const AValue: NativeUInt);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_max_lines(GetSelf, AValue);
end;

procedure TSkParagraphStyle.SetStrutStyle(AValue: ISkStrutStyle);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_strut_style(GetSelf, TSkBindings.SafeGetSelf(AValue));
end;

procedure TSkParagraphStyle.SetTextAlign(const AValue: TSkTextAlign);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_text_align(GetSelf, sk_textalign_t(AValue));
end;

procedure TSkParagraphStyle.SetTextDirection(const AValue: TSkTextDirection);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_text_direction(GetSelf, sk_textdirection_t(AValue));
end;

procedure TSkParagraphStyle.SetTextHeightBehaviors(
  const AValue: TSkTextHeightBehaviors);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_text_height_behaviors(GetSelf, Byte(AValue));
end;

procedure TSkParagraphStyle.SetTextStyle(AValue: ISkTextStyle);
begin
  TSkiaAPI.sk4d_paragraphstyle_set_text_style(GetSelf, TSkBindings.SafeGetSelf(AValue));
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
  Result := (ATextShadow1.Color  = ATextShadow2.Color ) and
            (ATextShadow1.Offset = ATextShadow2.Offset) and
            (SameValue(ATextShadow1.BlurRadius, ATextShadow2.BlurRadius, Epsilon));
end;

class operator TSkTextShadow.NotEqual(const ATextShadow1,
  ATextShadow2: TSkTextShadow): Boolean;
begin
  Result := not (ATextShadow1 = ATextShadow2);
end;

{ TSkTextStyle }

procedure TSkTextStyle.AddFontFeature(const AFeature: string;
  const AValue: Integer);
begin
  if AFeature.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AFeature']);
  TSkiaAPI.sk4d_textstyle_add_font_feature(GetSelf, MarshaledAString(UTF8String(AFeature)), AValue);
end;

procedure TSkTextStyle.AddShadow(const AShadow: TSkTextShadow);
begin
  TSkiaAPI.sk4d_textstyle_add_shadow(GetSelf, @sk_textshadow_t(AShadow));
end;

procedure TSkTextStyle.ClearBackgroundColor;
begin
  TSkiaAPI.sk4d_textstyle_clear_background_color(GetSelf);
end;

procedure TSkTextStyle.ClearForegroundColor;
begin
  TSkiaAPI.sk4d_textstyle_clear_foreground_color(GetSelf);
end;

constructor TSkTextStyle.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_textstyle_create());
end;

class procedure TSkTextStyle.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_textstyle_destroy(AHandle);
end;

function TSkTextStyle.GetBackground: ISkPaint;
begin
  Result := TSkPaint.Wrap(TSkiaAPI.sk4d_textstyle_get_background(GetSelf));
end;

function TSkTextStyle.GetColor: TAlphaColor;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_color(GetSelf);
end;

function TSkTextStyle.GetDecorationColor: TAlphaColor;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_decoration_color(GetSelf);
end;

function TSkTextStyle.GetDecorations: TSkTextDecorations;
begin
  Result := TSkTextDecorations(Byte(TSkiaAPI.sk4d_textstyle_get_decorations(GetSelf)));
end;

function TSkTextStyle.GetDecorationStyle: TSkTextDecorationStyle;
begin
  Result := TSkTextDecorationStyle(TSkiaAPI.sk4d_textstyle_get_decoration_style(GetSelf));
end;

function TSkTextStyle.GetDecorationThickness: Single;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_decoration_thickness(GetSelf);
end;

function TSkTextStyle.GetFontFamilies: TArray<string>;
begin
  Result := TSkBindings.GetStrings(GetSelf,
    function (const AHandle: THandle; const AStrings: PMarshaledAString): Integer
    begin
      Result := TSkiaAPI.sk4d_textstyle_get_font_families(AHandle, AStrings);
    end);
end;

function TSkTextStyle.GetFontMetrics: TSkFontMetrics;
var
  LResult: sk_fontmetrics_t;
begin
  TSkiaAPI.sk4d_textstyle_get_font_metrics(GetSelf, LResult);
  Result := TSkMapping.ToFontMetrics(LResult);
end;

function TSkTextStyle.GetFontSize: Single;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_font_size(GetSelf);
end;

function TSkTextStyle.GetFontStyle: TSkFontStyle;
begin
  TSkiaAPI.sk4d_textstyle_get_font_style(GetSelf, sk_fontstyle_t(Result));
end;

function TSkTextStyle.GetForeground: ISkPaint;
begin
  Result := TSkPaint.Wrap(TSkiaAPI.sk4d_textstyle_get_foreground(GetSelf));
end;

function TSkTextStyle.GetHalfLeading: Boolean;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_half_leading(GetSelf);
end;

function TSkTextStyle.GetHeightMultiplier: Single;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_height_multiplier(GetSelf);
end;

function TSkTextStyle.GetLetterSpacing: Single;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_letter_spacing(GetSelf);
end;

function TSkTextStyle.GetLocale: string;
var
  LResult: ISkString;
begin
  LResult := TSkString.Wrap(TSkiaAPI.sk4d_textstyle_get_locale(GetSelf));
  Result  := LResult.Text;
end;

function TSkTextStyle.GetWordSpacing: Single;
begin
  Result := TSkiaAPI.sk4d_textstyle_get_word_spacing(GetSelf);
end;

function TSkTextStyle.IsEqual(const ATextStyle: ISkTextStyle): Boolean;
begin
  if not Assigned(ATextStyle) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATextStyle']);
  Result := TSkiaAPI.sk4d_textstyle_is_equal(GetSelf, TSkBindings.GetSelf(ATextStyle));
end;

procedure TSkTextStyle.ResetFontFeatures;
begin
  TSkiaAPI.sk4d_textstyle_reset_font_features(GetSelf);
end;

procedure TSkTextStyle.ResetShadows;
begin
  TSkiaAPI.sk4d_textstyle_reset_shadows(GetSelf);
end;

procedure TSkTextStyle.SetBackgroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_textstyle_set_background_color(GetSelf, TSkBindings.GetSelf(APaint));
end;

procedure TSkTextStyle.SetColor(const AValue: TAlphaColor);
begin
  TSkiaAPI.sk4d_textstyle_set_color(GetSelf, AValue);
end;

procedure TSkTextStyle.SetDecorationColor(const AValue: TAlphaColor);
begin
  TSkiaAPI.sk4d_textstyle_set_decoration_color(GetSelf, AValue);
end;

procedure TSkTextStyle.SetDecorations(const AValue: TSkTextDecorations);
begin
  TSkiaAPI.sk4d_textstyle_set_decorations(GetSelf, Byte(AValue));
end;

procedure TSkTextStyle.SetDecorationStyle(const AValue: TSkTextDecorationStyle);
begin
  TSkiaAPI.sk4d_textstyle_set_decoration_style(GetSelf, sk_textdecorationstyle_t(AValue));
end;

procedure TSkTextStyle.SetDecorationThickness(const AValue: Single);
begin
  TSkiaAPI.sk4d_textstyle_set_decoration_thickness(GetSelf, AValue);
end;

procedure TSkTextStyle.SetFontFamilies(const AValue: TArray<string>);
begin
  TSkBindings.SetStrings(GetSelf, AValue,
    procedure (const AHandle: THandle; const AStrings: PMarshaledAString; const ALength: Integer)
    begin
      TSkiaAPI.sk4d_textstyle_set_font_families(AHandle, AStrings, ALength);
    end);
end;

procedure TSkTextStyle.SetFontSize(const AValue: Single);
begin
  TSkiaAPI.sk4d_textstyle_set_font_size(GetSelf, AValue);
end;

procedure TSkTextStyle.SetFontStyle(const AValue: TSkFontStyle);
begin
  TSkiaAPI.sk4d_textstyle_set_font_style(GetSelf, @sk_fontstyle_t(AValue));
end;

procedure TSkTextStyle.SetForegroundColor(const APaint: ISkPaint);
begin
  if not Assigned(APaint) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['APaint']);
  TSkiaAPI.sk4d_textstyle_set_foreground_color(GetSelf, TSkBindings.GetSelf(APaint));
end;

procedure TSkTextStyle.SetHalfLeading(const AValue: Boolean);
begin
  TSkiaAPI.sk4d_textstyle_set_half_leading(GetSelf, AValue);
end;

procedure TSkTextStyle.SetHeightMultiplier(const AValue: Single);
begin
  TSkiaAPI.sk4d_textstyle_set_height_multiplier(GetSelf, AValue);
end;

procedure TSkTextStyle.SetLetterSpacing(const AValue: Single);
begin
  TSkiaAPI.sk4d_textstyle_set_letter_spacing(GetSelf, AValue);
end;

procedure TSkTextStyle.SetLocale(const AValue: string);
begin
  if AValue.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AValue']);
  TSkiaAPI.sk4d_textstyle_set_locale(GetSelf, MarshaledAString(UTF8String(AValue)));
end;

procedure TSkTextStyle.SetWordSpacing(const AValue: Single);
begin
  TSkiaAPI.sk4d_textstyle_set_word_spacing(GetSelf, AValue);
end;

{ TSkTypefaceFontProvider }

constructor TSkTypefaceFontProvider.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_typefacefontprovider_create());
end;

procedure TSkTypefaceFontProvider.RegisterTypeface(
  const ATypeface: ISkTypeface);
begin
  if not Assigned(ATypeface) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATypeface']);
  TSkiaAPI.sk4d_typefacefontprovider_register_typeface(GetSelf, TSkBindings.GetSelf(ATypeface));
end;

procedure TSkTypefaceFontProvider.RegisterTypeface(const ATypeface: ISkTypeface;
  const AFamilyName: string);
begin
  if not Assigned(ATypeface) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ATypeface']);
  if AFamilyName.IsEmpty then
    raise ESkArgumentException.CreateFmt(SParamIsEmpty, ['AFamilyName']);
  TSkiaAPI.sk4d_typefacefontprovider_register_typeface2(GetSelf, TSkBindings.GetSelf(ATypeface), MarshaledAString(UTF8String(AFamilyName)));
end;

{ TSkResourceProviderBaseClass }

class constructor TSkResourceProviderBaseClass.Create;
var
  LProcs: sk_resourceproviderbaseclass_procs_t;
begin
  LProcs.load := load_proc;
  TSkiaAPI.sk4d_resourceproviderbaseclass_set_procs(@LProcs);
end;

constructor TSkResourceProviderBaseClass.Create(const APredecode: Boolean);
begin
  inherited Wrap(TSkiaAPI.sk4d_resourceproviderbaseclass_create(APredecode, Self));
end;

class function TSkResourceProviderBaseClass.load_proc(context: Pointer;
  const path, name: MarshaledAString): sk_data_t;
var
  LResult: ISkData;
begin
  LResult := TSkData.Create(TSkResourceProviderBaseClass(context).Load(string(path), string(name)));
  Result  := LResult.Handle;
end;

{ TSkFileResourceProvider }

constructor TSkFileResourceProvider.Create(const APredecode: Boolean);
begin
  Create(TPath.GetDocumentsPath, APredecode);
end;

constructor TSkFileResourceProvider.Create(const ABaseDir: string;
  const APredecode: Boolean);
begin
  inherited Create(APredecode);
  FBaseDir := ABaseDir;
end;

function TSkFileResourceProvider.Load(const APath, AName: string): TBytes;
begin
  Result := TFile.ReadAllBytes(TPath.Combine(TPath.Combine(FBaseDir, APath), AName));
end;

{ TSkShaper }

constructor TSkShaper.Create;
begin
  inherited Wrap(TSkiaAPI.sk4d_shaper_create());
end;

class procedure TSkShaper.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_shaper_destroy(AHandle);
end;

function TSkShaper.Shape(const AText: string; const AFont: ISkFont;
  const ALeftToRight: Boolean; const AWidth: Single): ISkTextBlob;
var
  LText: UTF8String;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  LText := UTF8String(AText);
  if Length(LText) = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_shaper_shape(GetSelf, @LText[Low(LText)], Length(LText), TSkBindings.GetSelf(AFont), ALeftToRight, AWidth, nil, nil));
end;

function TSkShaper.Shape(const AText: string; const AFont: ISkFont;
  const ALeftToRight: Boolean; const AWidth: Single; const AOffset: TPointF;
  out AEndPoint: TPointF): ISkTextBlob;
var
  LText: UTF8String;
begin
  if not Assigned(AFont) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['AFont']);
  LText := UTF8String(AText);
  if Length(LText) = 0 then
    Exit(nil);
  Result := TSkBindings.SafeCreate<TSkTextBlob>(TSkiaAPI.sk4d_shaper_shape(GetSelf, @LText[Low(LText)], Length(LText), TSkBindings.GetSelf(AFont), ALeftToRight, AWidth, @sk_point_t(AOffset), @sk_point_t(AEndPoint)));
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
  inherited Wrap(TSkiaAPI.sk4d_unicode_create());
end;

class procedure TSkUnicode.DestroyHandle(const AHandle: THandle);
begin
  TSkiaAPI.sk4d_unicode_destroy(AHandle);
end;

procedure TSkUnicode.ForEachBidiRegion(const AText: string;
  const ADirection: TSkDirection; const AProc: TSkUnicodeBidiRegionProc);
begin
  if (not AText.IsEmpty) and (Assigned(AProc)) then
    TSkiaAPI.sk4d_unicode_for_each_bidi_region(GetSelf, @AText[Low(AText)], Length(AText), sk_direction_t(ADirection), bidi_region_proc, @AProc);
end;

procedure TSkUnicode.ForEachBreak(const AText: string;
  const AType: TSkBreakType; const AProc: TSkUnicodeBreakProc);
begin
  if (not AText.IsEmpty) and (Assigned(AProc)) then
    TSkiaAPI.sk4d_unicode_for_each_break(GetSelf, @AText[Low(AText)], Length(AText), sk_breaktype_t(AType), break_proc, @AProc);
end;

procedure TSkUnicode.ForEachCodepoint(const AText: string;
  const AProc: TSkUnicodeCodepointProc);
begin
  if (not AText.IsEmpty) and (Assigned(AProc)) then
    TSkiaAPI.sk4d_unicode_for_each_codepoint(GetSelf, @AText[Low(AText)], Length(AText), codepoint_proc, @AProc);
end;

function TSkUnicode.GetBreaks(const AText: string;
  const AType: TSkBreakType): TArray<string>;
var
  LPStartIndex: PInteger;
  LResult: TList<string>;
  LStartIndex: Integer;
begin
  LStartIndex := 0;
  LPStartIndex := @LStartIndex;
  LResult := TList<string>.Create;
  try
    ForEachBreak(AText, TSkBreakType.Graphemes,
      procedure (const APosition, AStatus: Integer)
      begin
        if APosition = 0 then
          Exit;
        LResult.Add(AText.Substring(LPStartIndex^, APosition - LPStartIndex^));
        LPStartIndex^ := APosition;
      end);
    Result := LResult.ToArray;
  finally
    LResult.Free;
  end;
end;

{ TSkSVGDOM }

function TSkSVGDOM.FindNodeById(const AId: string): ISkSVGNode;
begin
  Result := TSkBindings.SafeCreate<TSkSVGNode>(TSkiaAPI.sk4d_svgdom_find_node_by_id(GetSelf, MarshaledAString(UTF8String(AId))), False);
end;

function TSkSVGDOM.GetRoot: ISkSVGSVG;
begin
  if not Assigned(FRootHolder) then
    FRootHolder := TSkBindings.SafeCreate<TSkSVGSVG>(TSkiaAPI.sk4d_svgdom_get_root(GetSelf), False);
  Result := FRootHolder;
end;

class function TSkSVGDOM.Make(const AData: string;
  const AResourceProvider: ISkResourceProvider): ISkSVGDOM;
var
  LStream: TStream;
begin
  LStream := TStringStream.Create(AData, TEncoding.UTF8);
  try
    Result := MakeFromStream(LStream, AResourceProvider);
  finally
    LStream.Free;
  end;
end;

class function TSkSVGDOM.MakeFromFile(const AFileName: string): ISkSVGDOM;
begin
  if AFileName.IsEmpty then
    raise ESkException.Create(SFileNameIsEmpty);
  Result := TSkBindings.SafeCreate<TSkSVGDOM>(TSkiaAPI.sk4d_svgdom_make_from_file(MarshaledAString(UTF8String(AFileName))));
end;

class function TSkSVGDOM.MakeFromStream(const AStream: TStream;
  const AResourceProvider: ISkResourceProvider): ISkSVGDOM;
var
  LStream: ISkStream;
begin
  LStream := TSkStreamAdapter.Create(AStream);
  Result  := TSkBindings.SafeCreate<TSkSVGDOM>(TSkiaAPI.sk4d_svgdom_make_from_stream(LStream.Handle, TSkBindings.SafeGetSelf(AResourceProvider)));
end;

procedure TSkSVGDOM.Render(const ACanvas: ISkCanvas);
begin
  if not Assigned(ACanvas) then
    raise ESkArgumentException.CreateFmt(SParamIsNil, ['ACanvas']);
  TSkiaAPI.sk4d_svgdom_render(GetSelf, TSkBindings.GetSelf(ACanvas));
end;

procedure TSkSVGDOM.SetContainerSize(const ASize: TSizeF);
begin
  TSkiaAPI.sk4d_svgdom_set_container_size(GetSelf, @sk_size_t(ASize));
end;

{ TSkSVGNode }

function TSkSVGNode.TrySetAttribute(const AName, AValue: string): Boolean;
begin
  Result := TSkiaAPI.sk4d_svgnode_set_attribute(GetSelf, MarshaledAString(UTF8String(AName)), MarshaledAString(UTF8String(AValue)));
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
  Result := (ASVGLength1.&Unit = ASVGLength2.&Unit) and
            (SameValue(ASVGLength1.Value, ASVGLength2.Value, Epsilon));
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

{ TSkSVGSVG }

function TSkSVGSVG.GetHeight: TSkSVGLength;
begin
  TSkiaAPI.sk4d_svgsvg_get_height(GetSelf, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetIntrinsicSize(const AViewPort: TSizeF;
  const ADPI: Single): TSizeF;
begin
  TSkiaAPI.sk4d_svgsvg_get_intrinsic_size(GetSelf, @sk_size_t(AViewPort), ADPI, sk_size_t(Result));
end;

function TSkSVGSVG.GetPreserveAspectRatio: TSkSVGPreserveAspectRatio;
begin
  TSkiaAPI.sk4d_svgsvg_get_preserve_aspect_ratio(GetSelf, sk_svgpreserveaspectratio_t(Result));
end;

function TSkSVGSVG.GetWidth: TSkSVGLength;
begin
  TSkiaAPI.sk4d_svgsvg_get_width(GetSelf, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetX: TSkSVGLength;
begin
  TSkiaAPI.sk4d_svgsvg_get_x(GetSelf, sk_svglength_t(Result));
end;

function TSkSVGSVG.GetY: TSkSVGLength;
begin
  TSkiaAPI.sk4d_svgsvg_get_y(GetSelf, sk_svglength_t(Result));
end;

procedure TSkSVGSVG.SetHeight(const AValue: TSkSVGLength);
begin
  TSkiaAPI.sk4d_svgsvg_set_height(GetSelf, @sk_svglength_t(AValue));
end;

procedure TSkSVGSVG.SetPreserveAspectRatio(
  const AValue: TSkSVGPreserveAspectRatio);
begin
  TSkiaAPI.sk4d_svgsvg_set_preserve_aspect_ratio(GetSelf, @sk_svgpreserveaspectratio_t(AValue));
end;

procedure TSkSVGSVG.SetViewBox(const AViewBox: TRectF);
begin
  TSkiaAPI.sk4d_svgsvg_set_view_box(GetSelf, @sk_rect_t(AViewBox));
end;

procedure TSkSVGSVG.SetWidth(const AValue: TSkSVGLength);
begin
  TSkiaAPI.sk4d_svgsvg_set_width(GetSelf, @sk_svglength_t(AValue));
end;

procedure TSkSVGSVG.SetX(const AValue: TSkSVGLength);
begin
  TSkiaAPI.sk4d_svgsvg_set_x(GetSelf, @sk_svglength_t(AValue));
end;

procedure TSkSVGSVG.SetY(const AValue: TSkSVGLength);
begin
  TSkiaAPI.sk4d_svgsvg_set_y(GetSelf, @sk_svglength_t(AValue));
end;

function TSkSVGSVG.TryGetViewBox(out AViewBox: TRectF): Boolean;
begin
  Result := TSkiaAPI.sk4d_svgsvg_get_view_box(GetSelf, sk_rect_t(AViewBox));
end;

end.
