{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit FMX.Skia.Canvas;

interface

{$SCOPEDENUMS ON}

{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion111))}
  {$DEFINE MODULATE_CANVAS}
{$ENDIF}

uses
  { Delphi }
  FMX.Graphics,
  FMX.TextLayout,
  FMX.Types,
  System.Classes,
  System.Generics.Defaults,
  System.Math.Vectors,
  System.Messaging,
  System.SysUtils,
  System.Types,
  System.UITypes,

  { Skia }
  System.Skia;

type
  ESkCanvas = class(Exception);

  EGrCanvas = class(ESkCanvas);

  { ISkCanvasWrapper }

  /// <summary>
  ///   Wrapper for encapsulating a <c>ISkCanvas</c> in a <c>TCanvas</c>.
  ///   Numerous Skia features return the <c>ISkCanvas</c>, and with the
  ///   assistance of this wrapper, any class can be transformed into a
  ///   <c>TCanvas</c>. This enables the utilization of highly useful functions,
  ///   such as <b>PaintTo</b> of <c>TControl</c>.
  /// </summary>
  ISkCanvasWrapper = interface
    ['{C9CDCC18-9C92-4F45-98FE-E3F5E60A397D}']
    function CanvasBegin(const ACanvas: TCanvas): ISkCanvas;
    procedure CanvasEnd;
    function GetCanvasHeight: Integer;
    function GetCanvasScale: Single;
    function GetCanvasWidth: integer;
    property CanvasHeight: Integer read GetCanvasHeight;
    property CanvasScale: Single read GetCanvasScale;
    property CanvasWidth: integer read GetCanvasWidth;
  end;

  { TSkCanvasCustom }

  TSkCanvasCustom = class(TCanvas{$IFDEF MODULATE_CANVAS}, IModulateCanvas{$ENDIF})
  strict private type
    TSaveState = class(TCanvasSaveState)
    strict protected
      procedure AssignTo(ADest: TPersistent); override;
    public
      procedure Assign(ASource: TPersistent); override;
    end;

  strict protected type
    TBrushData = record
      Brush: TBrush;
      Paint: ISkPaint;
      BitmapMapped: Boolean;
      BitmapData: TBitmapData;
    end;

  {$IFDEF MODULATE_CANVAS}
  strict private
    FModulateColor: TAlphaColor;
    function GetModulateColor: TAlphaColor;
    procedure SetModulateColor(const AColor: TAlphaColor);
  {$ENDIF}
  strict private
    FCanvas: ISkCanvas;
    FContextHandle: THandle;
    procedure BeginPaint(const ARect: TRectF; const AOpacity: Single; var ABrushData: TBrushData);
  strict protected
    FWrapper: ISkCanvasWrapper;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    // Since FMX effects use TContext3D, on systems using OpenGLES it makes the
    // current context the context it created, and does not revert to the old
    // one after it has finished its task.
    procedure BeforeRestore; virtual;
    function BeginCanvas(const AContextHandle: THandle): ISkCanvas; virtual;
    function BeginPaintWithBrush(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single; out ABrushData: TBrushData): TSkPaint;
    function BeginPaintWithStrokeBrush(const ABrush: TStrokeBrush; const ARect: TRectF; const AOpacity: Single; out ABrushData: TBrushData): TSkPaint;
    function CreateSaveState: TCanvasSaveState; override;
    function DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override; final;
    {$IF CompilerVersion >= 35}
    procedure DoClear(const AColor: TAlphaColor); override;
    procedure DoClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    {$ENDIF}
    procedure DoDrawBitmap(const ABitmap: TBitmap; const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawLine(const APoint1, APoint2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoEndScene; override; final;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    {$IF CompilerVersion >= 30}
    procedure DoSetMatrix(const AMatrix: TMatrix); override;
    {$ENDIF}
    procedure EndCanvas(const AContextHandle: THandle); virtual;
    procedure EndPaint(var ABrushData: TBrushData);
    function GetCachedImage(const ABitmap: TBitmap): TSkImage; virtual;
    function GetCanvasScale: Single; override;
    function GetSamplingOptions(const AHighSpeed: Boolean = False): TSkSamplingOptions; inline;
    procedure Resized; virtual;
    function SupportsCachedImage: Boolean; virtual;
    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
    class function DoInitializeBitmap(const AWidth, AHeight: Integer; const AScale: Single; var APixelFormat: TPixelFormat): THandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const ABitmapHandle: THandle; var ABitmapData: TBitmapData); override;
  public
    constructor Wrap(const AWrapper: ISkCanvasWrapper);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    {$IF CompilerVersion < 35}
    procedure Clear(const AColor: TAlphaColor); override;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    {$ENDIF}
    procedure ExcludeClipRect(const ARect: TRectF); override;
    procedure IntersectClipRect(const ARect: TRectF); override;
    function LoadFontFromStream(const AStream: TStream): Boolean; override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    {$IF CompilerVersion < 30}
    procedure SetMatrix(const AMatrix: TMatrix); override;
    {$ENDIF}
    property Canvas: ISkCanvas read FCanvas;
    procedure SetSize(const AWidth, AHeight: Integer); override; final;
    class function GetCanvasStyle: TCanvasStyles; override;
    class function QualityToSamplingOptions(const AQuality: TCanvasQuality; const AHighSpeed: Boolean = False): TSkSamplingOptions;
  end;

  TSkCanvasCustomClass = class of TSkCanvasCustom;

  { TSkBitmapHandle }

  TSkBitmapHandle = class
  private
    FPixels: Pointer;
  strict private
    FHeight: Integer;
    FPixelFormat: TPixelFormat;
    FWidth: Integer;
  public
    constructor Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
    destructor Destroy; override;
    property Height: Integer read FHeight;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property Pixels: Pointer read FPixels;
    property Width: Integer read FWidth;
  end;

  { TSkCanvasBase }

  TSkCanvasBase = class abstract(TSkCanvasCustom)
  strict private
    FBitmapSurface: ISkSurface;
    FSurface: TSkSurface;
  strict protected
    {$IFDEF MSWINDOWS}
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    {$ENDIF}
    function BeginCanvas(const AContextHandle: THandle): ISkCanvas; override;
    procedure EndCanvas(const AContextHandle: THandle); override;
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; virtual; abstract;
    {$IFDEF MSWINDOWS}
    procedure Resized; override;
    {$ENDIF}
    procedure SwapBuffers(const AContextHandle: THandle); virtual;
    class function CreateBitmap(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat): TSkBitmapHandle; virtual;
    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
    class function DoInitializeBitmap(const AWidth, AHeight: Integer; const AScale: Single; var APixelFormat: TPixelFormat): THandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const ABitmapHandle: THandle; var ABitmapData: TBitmapData); override;
  end;

  TSkCanvasBaseClass = class of TSkCanvasBase;

  TGrBeforeSharedContextDestructionMessage = class(TMessage);

  { IGrSharedContext }

  /// <summary>
  ///   GPU shared context for utilization among multiple threads and windows.
  /// </summary>
  /// <remarks>
  ///   This is designed to be created once per application after the first
  ///   canvas is created from a window by the main thread. Prior to its
  ///   destruction, the main thread will send the message
  ///   <c>TGrBeforeSharedContextDestructionMessage</c>, at this point, all
  ///   shared context resources should be released, and it is recommended to
  ///   release all Skia-related resources as well.
  /// </remarks>
  IGrSharedContext = interface
    ['{8C8A3BD1-43AC-4872-B254-F38630F81395}']
    procedure BeginContext;
    procedure EndContext;
    function GetGrDirectContext: TGrDirectContext;
    function GetTextureColorType: TSkColorType;
    function GetTextureOrigin: TGrSurfaceOrigin;
    // Currently, this function is only significant when using an OpenGL
    // backend, as it updates the current context of the thread to the shared
    // context when working with multiple contexts.
    procedure RefreshContext;
    /// <summary>Direct access to the shared GPU context.</summary>
    /// <remarks>
    ///   This property can only be used between the <b>BeginContext</b> and
    ///   <b>EndContext</b> call.
    /// </remarks>
    property GrDirectContext: TGrDirectContext read GetGrDirectContext;
    /// <summary>Color type for use in texture creation.</summary>
    property TextureColorType: TSkColorType read GetTextureColorType;
    /// <summary>Surface origin for use in texture creation.</summary>
    property TextureOrigin: TGrSurfaceOrigin read GetTextureOrigin;
  end;

  TGrBitmapHandle = class;

  { TGrSharedContext }

  TGrSharedContext = class abstract(TInterfacedObject, IGrSharedContext)
  strict private
    function GetGrDirectContext: TGrDirectContext;
  strict protected
    FGrDirectContext: IGrDirectContext;
    procedure BeginContext; virtual;
    procedure DestroyContext; virtual;
    procedure EndContext; virtual;
    function GetTextureColorType: TSkColorType; virtual; abstract;
    function GetTextureOrigin: TGrSurfaceOrigin; virtual; abstract;
    procedure RefreshContext; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FinalizeTextureCache(const ABitmap: TGrBitmapHandle); virtual;
    procedure InitializeTextureCache(const ABitmap: TGrBitmapHandle); virtual;
    property GrDirectContext: IGrDirectContext read FGrDirectContext;
  end;

  { TGrBitmapHandle }

  TGrBitmapHandle = class(TSkBitmapHandle)
  private
    FSharedContext: IGrSharedContext;
  strict private
    FCache: ISkImage;
  public
    property Cache: ISkImage read FCache write FCache;
    property SharedContext: IGrSharedContext read FSharedContext;
  end;

  /// <summary>
  ///   To be used in conjunction with the <c>ISkCanvasWrapper</c>, indicating
  ///   the use of the shared context (e.g. when drawing on textures).
  /// </summary>
  /// <remarks>
  ///   Utilize the <b>DefaultSkiaRenderCanvasClass</b> function to obtain the
  ///   default registered Skia canvas class, and subsequently call the
  ///   <b>Wrap</b> function.
  /// </remarks>
  IGrCanvasWrapper = interface
    ['{D3381B1A-06FD-4DB2-A09A-330D49BD34A7}']
  end;

  { TGrCanvas }

  TGrCanvas = class abstract(TSkCanvasBase)
  strict private class var
    FInitialized: Boolean;
    FSharedContext: IGrSharedContext;
  strict protected
    FGrDirectContext: IGrDirectContext;
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    function BeginCanvas(const AContextHandle: THandle): ISkCanvas; override;
    function CreateSharedContext: IGrSharedContext; virtual; abstract;
    procedure EndCanvas(const AContextHandle: THandle); override;
    function GetCachedImage(const ABitmap: TBitmap): TSkImage; override; final;
    function SupportsCachedImage: Boolean; override;
    class function CreateBitmap(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat): TSkBitmapHandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
  public
    /// <summary>Direct access to the GPU context.</summary>
    /// <remarks>
    ///   This property can only be used between the <b>BeginScene</b> and
    //    <b>EndScene</b> call.
    /// </remarks>
    property GrDirectContext: IGrDirectContext read FGrDirectContext;
    /// <summary>
    ///   Indicates in a thread-safe manner whether the shared context has been
    ///   initialized.
    /// </summary>
    /// <remarks>
    ///   The shared context is initialized once a canvas is created from a
    ///   window via the main thread. If initialized, the <b>SharedContext</b>
    ///   property is accessible and not nil.
    /// </remarks>
    class property Initialized: Boolean read FInitialized;
    /// <summary>
    ///   Shared context for utilization across multiple windows and/or threads.
    /// </summary>
    /// <remarks>
    ///   If the shared context has not been initialized, the property will
    ///  return nil. To check the initialization status, use the
    ///  <b>Initialized</b> property.
    /// </remarks>
    class property SharedContext: IGrSharedContext read FSharedContext;
  end;

  TGrCanvasClass = class of TGrCanvas;

  { TSkTextLayout }

  TSkTextLayout = class(TTextLayout)

  {$REGION ' - Workaround RSP-36975'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround intended to fix a bug involving the
  // -   TContextMetal.DoCopyToBits.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-36975
  // -
  // - -------------------------------------------------------------------------
  {$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and NOT DECLARED(RTLVersion113))}
  strict private type
    TRSP36975Workaround = record
    strict private
      FIgnoreNeedUpdate: Boolean;
    public
      procedure Apply(const ATextLayout: TTextLayout); inline;
      property IgnoreNeedUpdate: Boolean read FIgnoreNeedUpdate;
    end;

  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$ENDREGION}

  {$REGION ' - Workaround RSP-38480'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround intended to fix a bug involving the
  // -   TContextMetal.DoCopyToBits.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-38480
  // -
  // - -------------------------------------------------------------------------
  {$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and NOT DECLARED(RTLVersion112))}
  strict private type
    TRSP38480Workaround = record
      class procedure Apply(var AText: string); static; inline;
    end;

  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$ENDREGION}

  strict private type
    TParagraph = record
      Bounds: TRectF;
      Index: Integer;
      Offset: TPointF;
      Paragraph: ISkParagraph;
      Range: TTextRange;
    end;

    TGraphemesMap = record
    strict private
      FCharIndexInGrapheme: TBytes;
      FText: string;
      function CreateGraphemesMapping(const AText: string): TBytes;
      procedure SetText(const AText: string); inline;
    public
      procedure FindNextGraphemeBoundary(var AIndex: Integer); inline;
      procedure FindPreviousGraphemeBoundary(var AIndex: Integer); inline;
      property Text: string read FText write SetText;
    end;

  strict private class var
    FAttributesRangeComparer: IComparer<TTextAttributedRange>;
    FParagraphTextRangeComparer: IComparer<TParagraph>;
  private
    class procedure Initialize;
  strict private
    FColor: TAlphaColor;
    FGraphemesMap: TGraphemesMap;
    FMaxLines: Integer;
    FOpacity: Single;
    FParagraphs: TArray<TParagraph>;
    {$IF DECLARED(TRSP36975Workaround))}
    FRSP36975Workaround: TRSP36975Workaround;
    {$ENDIF}
    FTextRect: TRectF;
    function GetParagraphsInRange(const APos, ALength: Integer): TArray<TParagraph>;
    function NeedHorizontalAlignment: Boolean;
    procedure SetMaxLines(AValue: Integer);
    procedure UpdateParagraph;
    class constructor Create;
  strict protected
    procedure DoDrawLayout(const ACanvas: TCanvas); overload; override;
    procedure DoDrawLayout(const ACanvas: ISkCanvas); reintroduce; overload;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
    procedure DoRenderLayout; override;
    function GetTextHeight: Single; override;
    function GetTextRect: TRectF; override;
    function GetTextWidth: Single; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    procedure ConvertToPath(const APath: TPathData); override;
    procedure RenderLayout(const ACanvas: ISkCanvas); overload;
    /// <summary>
    ///   Max lines allowed in text. When zero (default) it is unlimited.
    /// </summary>
    property MaxLines: Integer read FMaxLines write SetMaxLines;
  end;

/// <summary>Get the registered default Skia canvas class.</summary>
/// <returns>
///   Nil if the Skia canvas class has not been registered or if Skia has not
//    been selected as the default canvas; otherwise it returns the registered
///   Skia canvas class.
/// </returns>
function DefaultSkiaRenderCanvasClass: TSkCanvasBaseClass;

procedure RegisterSkiaRenderCanvas(const ACanvasClass: TSkCanvasBaseClass; const APriority: Boolean; const AIsSupportedFunc: TFunc<Boolean> = nil);

const
  CanvasQualitySampleCount: array[TCanvasQuality] of Integer = (2, 1, 4);

resourcestring
  SVWEBPImages   = 'WebP Images';
  SVWBMPImages   = 'WBMP Images';
  SVRawCanon     = 'Raw Canon';
  SVRawDNG       = 'Raw Adobe DNG Digital Negative';
  SVRawNikon     = 'Raw Nikon';
  SVRawORF       = 'Raw Olympus ORF';
  SVRawPanasonic = 'Raw Panasonic';
  SVRawPEF       = 'Raw Pentax PEF';
  SVRawRAF       = 'Raw Fujifilm RAF';
  SVRawSony      = 'Raw Sony';
  SVRawSRW       = 'Raw Samsung SRW';

implementation

{$IF CompilerVersion >= 36}
  {$DEFINE SKIA_ANIMATED_CODEC}
  {$DEFINE SKIA_FILTER}
  {$DEFINE SKIA_PRINTER}
  {$DEFINE SKIA_VULKAN}
{$ENDIF}

{$IF DEFINED(LINUX) or (DEFINED(MACOS) and NOT DEFINED(IOS)) or DEFINED(MSWINDOWS)}
  {$DEFINE SKIA_RASTER}
{$ENDIF}

uses
  { Delphi }
  FMX.Consts,
  FMX.Forms,
  FMX.Platform,
  FMX.Printer,
  FMX.Surfaces,
  {$IF DEFINED(LINUX)}
  FMUX.Api,
  {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
  Macapi.CocoaTypes,
  Macapi.CoreGraphics,
  {$ELSEIF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.Windows,
  {$ENDIF}
  System.Generics.Collections,
  System.IOUtils,
  System.Math,
  System.UIConsts,

  { Workarounds }
  {$IF DEFINED(MACOS)}
  FMX.Context.Metal,
  FMX.Types3D,
  FMX.Utils,
  Macapi.Metal,
  Macapi.MetalKit,
  Posix.SysMman,
  Posix.Unistd,
  System.Rtti,
  {$IFDEF IOS}
  FMX.Context.GLES,
  FMX.Controls,
  FMX.Helpers.iOS,
  FMX.MediaLibrary,
  FMX.MediaLibrary.IOS,
  iOSapi.AssetsLibrary,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
  iOSapi.OpenGLES,
  iOSapi.UIKit,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  {$ENDIF}
  {$ELSEIF DEFINED(ANDROID)}
  FMX.Context.GLES,
  FMX.Types3D,
  FMX.Utils,
  Androidapi.Gles2,
  Posix.SysMman,
  Posix.Unistd,
  System.Rtti,
  {$ENDIF}

  { Skia }
  FMX.Skia,
  {$IFDEF SKIA_ANIMATED_CODEC}
  FMX.Skia.AnimatedCodec,
  {$ENDIF}
  {$IFDEF SKIA_FILTER}
  FMX.Skia.Filter,
  {$ENDIF}
  {$IFDEF SKIA_PRINTER}
  FMX.Skia.Printer,
  {$ENDIF}
  {$IFDEF SKIA_VULKAN}
  FMX.Skia.Canvas.Vulkan,
  {$ENDIF}
  FMX.Skia.Canvas.GL,
  FMX.Skia.Canvas.Metal,
  System.Skia.API;

{$REGION ' - Workaround RSP-36957'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a bug involving the
// -   TCustomContextOpenGL.DoCopyToBits.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-36957
// -
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
{$IF DEFINED(ANDROID) or DEFINED(IOS)}

type
  { TRSP36957Workaround }

  TRSP36957Workaround = record
  private type
    {$RTTI EXPLICIT METHODS([vcProtected])}
    TContextOpenGLPatch = class(TCustomContextOpenGL)
    protected
      procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    end;
    {$RTTI METHODS([vcPublic, vcPublished])}
  public
    class procedure Apply; static;
  end;

{ TRSP36957Workaround }

class procedure TRSP36957Workaround.Apply;

  function HookVMT(const AVMTEntry, ATargetAddress: Pointer): Boolean;
  var
    LOriginEnd: Pointer;
    LOriginStart: Pointer;
    LPageSize: Integer;
    LTargetEnd: Pointer;
    LTargetStart: Pointer;
  begin
    LPageSize    := sysconf(_SC_PAGESIZE);
    LOriginStart := Pointer(NativeUInt(AVMTEntry) and not (LPageSize - 1));
    LOriginEnd   := Pointer((NativeUInt(AVMTEntry) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    LTargetStart := Pointer(NativeUInt(ATargetAddress) and not (LPageSize - 1));
    LTargetEnd   := Pointer((NativeUInt(ATargetAddress) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    Result := (mprotect(LOriginStart, PByte(LOriginEnd) - PByte(LOriginStart), PROT_READ or PROT_WRITE) = 0) and (mprotect(LTargetStart, PByte(LTargetEnd) - PByte(LTargetStart), PROT_READ or PROT_WRITE) = 0);
    if Result then
      PPointer(AVMTEntry)^ := PPointer(ATargetAddress)^;
  end;

  function TryGetVMT(const AClass: TClass; const AMethodName: string; out AVMT: Integer): Boolean;
  var
    LRttiContext: TRttiContext;
    LRttiMethod: TRttiMethod;
  begin
    LRttiContext := TRttiContext.Create;
    try
      LRttiMethod := LRttiContext.GetType(AClass).AsInstance.GetMethod(AMethodName);
      Result := Assigned(LRttiMethod);
      if Result then
        AVMT := LRttiMethod.VirtualIndex * SizeOf(Pointer);
    finally
      LRttiContext.Free;
    end;
  end;

var
  LVMT: Integer;
begin
  if TContextManager.DefaultContextClass.InheritsFrom(TCustomContextOpenGL) and TryGetVMT(TContextOpenGLPatch, 'DoCopyToBits', LVMT) then
    HookVMT(PByte(TContextManager.DefaultContextClass) + LVMT, PByte(TContextOpenGLPatch) + LVMT);
end;

{ TRSP36957Workaround.TContextOpenGLPatch }

procedure TRSP36957Workaround.TContextOpenGLPatch.DoCopyToBits(
  const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  I: Integer;
  OldFBO: GLuint;
  BitmapBuffer: PAlphaColorArray;
  BitmapBufferLen: Integer;
  Rect: TRect;
begin
  if Valid then
  begin
    BitmapBufferLen := Width * Height * 4;
    GetMem(BitmapBuffer, BitmapBufferLen);
    try
      if FFrameBuf <> 0 then
      begin
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);
      end;
      glReadPixels(0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, BitmapBuffer);
      Rect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));
      for I := Rect.Top to Rect.Bottom - 1 do
        Move(BitmapBuffer[(Height - 1 - I) * Width + Rect.Left], PAlphaColorArray(Bits)[I * (Pitch div 4) + Rect.Left],
          Rect.Width * 4);
      if FFrameBuf <> 0 then
        glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    finally
      FreeMem(BitmapBuffer);
    end;
    {$IF CompilerVersion < 34}
    if GLHasAnyErrors then
      RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoCopyBits']);
    {$ELSE}
    TGlesDiagnostic.RaiseIfHasError(@SErrorInContextMethod, ['DoCopyBits']);
    {$ENDIF}
  end;
end;

{$ENDIF}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{$REGION ' - Workaround RSP-37147'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a bug involving the
// -   TContextMetal.DoCopyToBits.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-37147
// -
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
{$IFDEF MACOS}

type
  { TRSP37147Workaround }

  TRSP37147Workaround = record
  private type
    {$RTTI EXPLICIT METHODS([vcProtected])}
    TContextMetalPatch = class(TCustomContextMetal)
    protected
      FPipelineStateConfiguration: TPipelineStateConfiguration;
      FDepthStencilStateConfiguration: TDepthStencilStateConfiguration;
      FCommandQueue: MTLCommandQueue;
      FCommandBuffer: MTLCommandBuffer;
      FRenderPassDescriptor: MTLRenderPassDescriptor;
      FRenderCommandEncoder: MTLRenderCommandEncoder;
      FCurrentDrawable: CAMetalDrawable;
      FOnScreenTexture: MTLTexture;
      FStencilReferenceValue: LongWord;
      FSampleCount: NSUInteger;
      procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    end;
    {$RTTI METHODS([vcPublic, vcPublished])}
  public
    class procedure Apply; static;
  end;

{ TRSP37147Workaround }

class procedure TRSP37147Workaround.Apply;

  function HookVMT(const AVMTEntry, ATargetAddress: Pointer): Boolean;
  var
    LOriginEnd: Pointer;
    LOriginStart: Pointer;
    LPageSize: Integer;
    LTargetEnd: Pointer;
    LTargetStart: Pointer;
  begin
    LPageSize    := sysconf(_SC_PAGESIZE);
    LOriginStart := Pointer(NativeUInt(AVMTEntry) and not (LPageSize - 1));
    LOriginEnd   := Pointer((NativeUInt(AVMTEntry) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    LTargetStart := Pointer(NativeUInt(ATargetAddress) and not (LPageSize - 1));
    LTargetEnd   := Pointer((NativeUInt(ATargetAddress) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    Result := (mprotect(LOriginStart, PByte(LOriginEnd) - PByte(LOriginStart), PROT_READ or PROT_WRITE) = 0) and (mprotect(LTargetStart, PByte(LTargetEnd) - PByte(LTargetStart), PROT_READ or PROT_WRITE) = 0);
    if Result then
      PPointer(AVMTEntry)^ := PPointer(ATargetAddress)^;
  end;

  function TryGetVMT(const AClass: TClass; const AMethodName: string; out AVMT: Integer): Boolean;
  var
    LRttiContext: TRttiContext;
    LRttiMethod: TRttiMethod;
  begin
    LRttiContext := TRttiContext.Create;
    try
      LRttiMethod := LRttiContext.GetType(AClass).AsInstance.GetMethod(AMethodName);
      Result := Assigned(LRttiMethod);
      if Result then
        AVMT := LRttiMethod.VirtualIndex * SizeOf(Pointer);
    finally
      LRttiContext.Free;
    end;
  end;

var
  LVMT: Integer;
begin
  if TContextManager.DefaultContextClass.InheritsFrom(TCustomContextMetal) and TryGetVMT(TContextMetalPatch, 'DoCopyToBits', LVMT) then
    HookVMT(PByte(TContextManager.DefaultContextClass) + LVMT, PByte(TContextMetalPatch) + LVMT);
end;

{ TRSP37147Workaround.TContextMetalPatch }

procedure TRSP37147Workaround.TContextMetalPatch.DoCopyToBits(
  const Bits: Pointer; const Pitch: Integer; const ARect: TRect);

  function CreateRegion(const ARect: TRect; const AScale: Single): MTLRegion;
  begin
    if SameValue(AScale, 1, TEpsilon.Scale) then
    begin
      Result.origin.x := ARect.left;
      Result.origin.y := ARect.top;
      Result.size.width := ARect.Width;
      Result.size.height := ARect.Height;
    end
    else
    begin
      Result.origin.x := Round(ARect.Left * Scale);
      Result.origin.y := Round(ARect.Top * Scale);
      Result.size.width := Round(ARect.Width * Scale);
      Result.size.height := Round(ARect.Height * Scale);
    end;
    Result.origin.z := 0;
    Result.size.depth := 1;
  end;

  procedure SynchronizeResources(const ATexture: MTLTexture);
  var
    CommandBuffer: MTLCommandBuffer;
    LBlitCommandEncoder: MTLBlitCommandEncoder;
  begin
    CommandBuffer := FCommandQueue.CommandBuffer;

    if CommandBuffer = nil then
      Exit;

    LBlitCommandEncoder := CommandBuffer.blitCommandEncoder;
    LBlitCommandEncoder.synchronizeResource(ATexture);
    LBlitCommandEncoder.endEncoding;
    CommandBuffer.commit;
    CommandBuffer.waitUntilCompleted;
  end;

var
  LCopyRect: TRect;
  LTexture: MTLTexture;
  LRegion: MTLRegion;
begin
  LTexture := nil;

  if FCommandBuffer <> nil then
    FCommandBuffer.waitUntilCompleted;

  LCopyRect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));
  if Texture <> nil then
  begin
    LTexture := TMTLTexture.Wrap(Pointer(Texture.Handle));
    LRegion := CreateRegion(LCopyRect, 1);
  end
  else if FOnScreenTexture <> nil then
  begin
    LTexture := FOnScreenTexture;
    LRegion := CreateRegion(LCopyRect, Scale);
  end;

  if LTexture <> nil then
  begin
    // Synchronizing a Managed Resource between GPU and CPU
    if LTexture.storageMode = MTLStorageModeManaged then
      SynchronizeResources(LTexture);
    // Get texture data
    LTexture.getBytesBytesPerRowFromRegionMipmapLevel(
      Bits, // pixelBytes: Pointer;
      Pitch, // bytesPerRow: NSUInteger;
      LRegion, // fromRegion: MTLRegion;
      0); // mipmapLevel: NSUInteger
  end;
end;

{$ENDIF}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{$REGION ' - Workaround RSP-37660'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   The functions BitmapToUIImage and UIImageToBitmap of unit FMX.Helpers.iOS
// -   are ignoring the PixelFormat of bitmaps. This issue generates wrong
// -   colors in several parts of the code, and this workaround will fix some
// -   important services that use these functions: IFMXTakenImageService,
// -   IFMXCameraService, IFMXPhotoLibrary and IFMXShareSheetActionsService.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-37651
// -   https://quality.embarcadero.com/browse/RSP-37660
// -
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
{$IFDEF IOS}

type
  { TRSP37660Workaround }

  TRSP37660Workaround = record
  private type
    TImageDelegate = class;
    TImageManagerCocoa = class;

    TSaveImageRequest = class
    private
      [Weak] FImageManager: TImageManagerCocoa;
      FImage: UIImage;
      FOnCompletion: TWriteImageCompletionEvent;
      procedure PerformResultOfSavingPhoto(assetURL: NSURL; error: NSError);
    public
      constructor Create(const AImageManager: TImageManagerCocoa; const AImage: UIImage; const AHandler: TWriteImageCompletionEvent);
      procedure Save;
    end;

    TImageManagerCocoa = class(TInterfacedObject, IFMXTakenImageService, IFMXCameraService, IFMXPhotoLibrary)
    private
      FImageDelegate: TImageDelegate;
      FImagePicker: UIImagePickerController;
      FSaveImageRequests: TList<TSaveImageRequest>;
    protected
      procedure TakeImage(const AControl: TControl; const ASourceType: UIImagePickerControllerSourceType);
      procedure SaveImageToSavedPhotosAlbum(const AImage: UIImage; const AOnResult: TWriteImageCompletionEvent = nil);
    public
      constructor Create;
      destructor Destroy; override;
      class function IsAvailableSourceType(const ASourceType: UIImagePickerControllerSourceType): Boolean;
      { IFMXTakenImage }
      procedure TakeImageFromLibrary(const AControl: TControl; const ARequiredResolution: TSize; const AEditable: Boolean;
        const AOnDidFinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking); overload;
      procedure TakeImageFromLibrary(const AControl: TControl; const AParams: TParamsPhotoQuery); overload;
      { IFMXPhotoLibrary }
      procedure AddImageToSavedPhotosAlbum(const ABitmap: TBitmap; const AWriteImageCompletionEvent: TWriteImageCompletionEvent = nil);
      { IFMXCameraService }
      procedure TakePhoto(const AControl: TControl; const ARequiredResolution: TSize; const AEditable: Boolean;
        const AOnDidFinishTaking: TOnDidFinishTaking; const AOnDidCancelTaking: TOnDidCancelTaking); overload;
      procedure TakePhoto(const AControl: TControl; const AParams: TParamsPhotoQuery); overload;
    end;

    TImageDelegate = class(TOCLocal, UIImagePickerControllerDelegate)
    private
      [Weak] FImageManager: TImageManagerCocoa;
      FParams: TParamsPhotoQuery;
    protected
      procedure DoDidFinishTaking(const AImage: TBitmap);
      procedure DoDidCancelTaking;
      procedure HidePicker(const APicker: UIImagePickerController);
      function GetAngleOfImageOrientation(const AImage: UIImage): Single;
    public
      constructor Create(const AImageManager: TImageManagerCocoa);
      property Params: TParamsPhotoQuery read FParams write FParams;
      { UIImagePickerControllerDelegate }
      procedure imagePickerController(picker: UIImagePickerController; didFinishPickingImage: UIImage; editingInfo: NSDictionary); overload; cdecl;
      procedure imagePickerController(picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary); overload; cdecl;
      procedure imagePickerControllerDidCancel(picker: UIImagePickerController); cdecl;
    end;

    TShareService = class(TInterfacedObject, IFMXShareSheetActionsService)
    private
      FActivityViewController: UIActivityViewController;
      FActivityItems: NSMutableArray;
      FPopoverController: UIPopoverController;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ReleaseOldSharingController;
      { IFMXShareSheetActionsService }
      procedure Share(const AControl: TControl; const AText: string; const ABitmap: TBitmap);
    end;

  private
    class procedure SetPixelFormatRGBA(const ABitmap: TBitmap); static;
  public
    class procedure Apply; static;
  end;

{ TRSP37660Workaround }

class procedure TRSP37660Workaround.Apply;
var
  LImageManager: TImageManagerCocoa;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXShareSheetActionsService);
  TPlatformServices.Current.RemovePlatformService(IFMXPhotoLibrary);
  TPlatformServices.Current.RemovePlatformService(IFMXTakenImageService);
  TPlatformServices.Current.RemovePlatformService(IFMXCameraService);
  LImageManager := TImageManagerCocoa.Create;
  if TImageManagerCocoa.IsAvailableSourceType(UIImagePickerControllerSourceTypeCamera) then
    TPlatformServices.Current.AddPlatformService(IFMXCameraService, LImageManager);
  if TImageManagerCocoa.IsAvailableSourceType(UIImagePickerControllerSourceTypeSavedPhotosAlbum) then
    TPlatformServices.Current.AddPlatformService(IFMXTakenImageService, LImageManager);
  TPlatformServices.Current.AddPlatformService(IFMXPhotoLibrary, LImageManager);
  if TOSVersion.Check(6, 0) then
    TPlatformServices.Current.AddPlatformService(IFMXShareSheetActionsService, IInterface(TShareService.Create));
end;

class procedure TRSP37660Workaround.SetPixelFormatRGBA(const ABitmap: TBitmap);
var
  LBitmapData: TBitmapData;
begin
  if (ABitmap <> nil) and (not ABitmap.IsEmpty) and ABitmap.Map(TMapAccess.ReadWrite, LBitmapData) then
  begin
    try
      ChangePixelFormat(LBitmapData.Data, LBitmapData.Data, (LBitmapData.Pitch * LBitmapData.Height) div LBitmapData.BytesPerPixel,
        LBitmapData.PixelFormat, TPixelFormat.RGBA);
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  end;
end;

{ TRSP37660Workaround.TSaveImageRequest }

constructor TRSP37660Workaround.TSaveImageRequest.Create(
  const AImageManager: TImageManagerCocoa; const AImage: UIImage;
  const AHandler: TWriteImageCompletionEvent);
begin
  if AImageManager = nil then
    raise EArgumentNilException.CreateFmt(SWrongParameter, ['AImageManager']);
  if AImage = nil then
    raise EArgumentNilException.CreateFmt(SWrongParameter, ['AImage']);
  FImageManager := AImageManager;
  FImage := AImage;
  FOnCompletion := AHandler;
end;

procedure TRSP37660Workaround.TSaveImageRequest.PerformResultOfSavingPhoto(
  assetURL: NSURL; error: NSError);
begin
  try
    if Assigned(FOnCompletion) then
      if error <> nil then
        FOnCompletion(False, NSStrToStr(error.localizedDescription))
      else
        FOnCompletion(True, SImageSaved);
  finally
    // ARC will remove instance of this object, when we remove the single link from FSaveQueue
    FImageManager.FSaveImageRequests.Remove(Self);
  end;
end;

procedure TRSP37660Workaround.TSaveImageRequest.Save;
var
  AssetsLibrary: ALAssetsLibrary;
begin
  AssetsLibrary := TALAssetsLibrary.Create;
  try
    AssetsLibrary.writeImageToSavedPhotosAlbum(FImage.CGImage, FImage.imageOrientation, PerformResultOfSavingPhoto);
  finally
    AssetsLibrary.release;
  end;
end;

{ TRSP37660Workaround.TImageManagerCocoa }

procedure TRSP37660Workaround.TImageManagerCocoa.AddImageToSavedPhotosAlbum(
  const ABitmap: TBitmap;
  const AWriteImageCompletionEvent: TWriteImageCompletionEvent);
var
  OCImage: UIImage;
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Assign(ABitmap);
    SetPixelFormatRGBA(LBitmap);
    OCImage := BitmapToUIImage(LBitmap);
    try
      SaveImageToSavedPhotosAlbum(OCImage, AWriteImageCompletionEvent);
    finally
      OCImage.release;
    end;
  finally
    LBitmap.Free;
  end;
end;

constructor TRSP37660Workaround.TImageManagerCocoa.Create;
begin
  inherited Create;
  FImageDelegate := TImageDelegate.Create(Self);
  FSaveImageRequests := TList<TSaveImageRequest>.Create;
end;

destructor TRSP37660Workaround.TImageManagerCocoa.Destroy;
begin
  FSaveImageRequests.Free;
  if FImagePicker <> nil then
  begin
    FImagePicker.release;
    FImagePicker.setDelegate(nil);
  end;
  FImageDelegate.DisposeOf;
  inherited Destroy;
end;

class function TRSP37660Workaround.TImageManagerCocoa.IsAvailableSourceType(
  const ASourceType: UIImagePickerControllerSourceType): Boolean;
begin
  Result := TUIImagePickerController.OCClass.isSourceTypeAvailable(ASourceType);
end;

procedure TRSP37660Workaround.TImageManagerCocoa.SaveImageToSavedPhotosAlbum(
  const AImage: UIImage; const AOnResult: TWriteImageCompletionEvent);
var
  SaveImageRequest: TSaveImageRequest;
begin
  SaveImageRequest := TSaveImageRequest.Create(Self, AImage, AOnResult);
  FSaveImageRequests.Add(SaveImageRequest);
  SaveImageRequest.Save;
end;

procedure TRSP37660Workaround.TImageManagerCocoa.TakeImage(
  const AControl: TControl;
  const ASourceType: UIImagePickerControllerSourceType);
var
  Window: UIWindow;
begin
  if IsAvailableSourceType(ASourceType) then
  begin
    if FImagePicker <> nil then
    begin
      FImagePicker.setDelegate(nil);
      FImagePicker.release;
    end;
    FImagePicker := TUIImagePickerController.Create;
    FImagePicker.retain;
    FImagePicker.setDelegate(FImageDelegate.GetObjectID);
    FImagePicker.setSourceType(ASourceType);
    FImagePicker.setAllowsEditing(FImageDelegate.Params.Editable);
    if IsPad and (ASourceType <> UIImagePickerControllerSourceTypeCamera) then
      FImagePicker.setModalPresentationStyle(UIModalPresentationFormSheet);
    Window := SharedApplication.keyWindow;
    if (Window <> nil) and (Window.rootViewController <> nil) then
      Window.rootViewController.presentModalViewController(FImagePicker, True);
  end;
end;

procedure TRSP37660Workaround.TImageManagerCocoa.TakeImageFromLibrary(
  const AControl: TControl; const AParams: TParamsPhotoQuery);
var
  ParamsTmp: TParamsPhotoQuery;
begin
  ParamsTmp := AParams;
  ParamsTmp.NeedSaveToAlbum := False;
  FImageDelegate.Params := ParamsTmp;
  TakeImage(AControl, UIImagePickerControllerSourceTypePhotoLibrary)
end;

procedure TRSP37660Workaround.TImageManagerCocoa.TakeImageFromLibrary(
  const AControl: TControl; const ARequiredResolution: TSize;
  const AEditable: Boolean; const AOnDidFinishTaking: TOnDidFinishTaking;
  const AOnDidCancelTaking: TOnDidCancelTaking);
var
  ParamTmp: TParamsPhotoQuery;
begin
  ParamTmp.Editable := AEditable;
  ParamTmp.RequiredResolution := ARequiredResolution;
  ParamTmp.NeedSaveToAlbum := False;
  ParamTmp.OnDidFinishTaking := AOnDidFinishTaking;
  ParamTmp.OnDidCancelTaking := AOnDidCancelTaking;
  TakeImageFromLibrary(AControl, ParamTmp);
end;

procedure TRSP37660Workaround.TImageManagerCocoa.TakePhoto(
  const AControl: TControl; const ARequiredResolution: TSize;
  const AEditable: Boolean; const AOnDidFinishTaking: TOnDidFinishTaking;
  const AOnDidCancelTaking: TOnDidCancelTaking);
var
  ParamTmp: TParamsPhotoQuery;
begin
  ParamTmp.Editable := AEditable;
  ParamTmp.RequiredResolution := ARequiredResolution;
  ParamTmp.NeedSaveToAlbum := False;
  ParamTmp.OnDidFinishTaking := AOnDidFinishTaking;
  ParamTmp.OnDidCancelTaking := AOnDidCancelTaking;
  TakePhoto(AControl, ParamTmp);
end;

procedure TRSP37660Workaround.TImageManagerCocoa.TakePhoto(
  const AControl: TControl; const AParams: TParamsPhotoQuery);
begin
  FImageDelegate.Params := AParams;
  TakeImage(AControl, UIImagePickerControllerSourceTypeCamera);
end;

{ TRSP37660Workaround.TImageDelegate }

constructor TRSP37660Workaround.TImageDelegate.Create(
  const AImageManager: TImageManagerCocoa);
begin
  inherited Create;
  FImageManager := AImageManager;
end;

procedure TRSP37660Workaround.TImageDelegate.DoDidCancelTaking;
begin
  if Assigned(Params.OnDidCancelTaking) then
    Params.OnDidCancelTaking;
end;

procedure TRSP37660Workaround.TImageDelegate.DoDidFinishTaking(
  const AImage: TBitmap);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.Assign(AImage);
    SetPixelFormatRGBA(LBitmap);
    if Assigned(Params.OnDidFinishTaking) then
      Params.OnDidFinishTaking(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

function TRSP37660Workaround.TImageDelegate.GetAngleOfImageOrientation(
  const AImage: UIImage): Single;
begin
  case AImage.imageOrientation of
    UIImageOrientationDown,
    UIImageOrientationDownMirrored:
      Result := 180;
    UIImageOrientationLeft,
    UIImageOrientationLeftMirrored:
      Result := -90;
    UIImageOrientationRight,
    UIImageOrientationRightMirrored:
      Result := 90;
    UIImageOrientationUp,
    UIImageOrientationUpMirrored:
      Result := 0;
  else
    Result := 0;
  end;
end;

procedure TRSP37660Workaround.TImageDelegate.HidePicker(
  const APicker: UIImagePickerController);
begin
  APicker.dismissModalViewControllerAnimated(True);
end;

procedure TRSP37660Workaround.TImageDelegate.imagePickerController(
  picker: UIImagePickerController; didFinishPickingImage: UIImage;
  editingInfo: NSDictionary);
var
  Bitmap: TBitmap;
  RotationAngle: Single;
begin
  HidePicker(picker);
  RotationAngle := GetAngleOfImageOrientation(didFinishPickingImage);
  Bitmap := UIImageToBitmap(didFinishPickingImage, RotationAngle, Params.RequiredResolution);
  try
    DoDidFinishTaking(Bitmap);
    if Params.NeedSaveToAlbum then
      FImageManager.SaveImageToSavedPhotosAlbum(didFinishPickingImage);
  finally
    Bitmap.DisposeOf;
  end;
end;

procedure TRSP37660Workaround.TImageDelegate.imagePickerController(
  picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary);
var
  Bitmap: TBitmap;
  ImageTmp: UIImage;
  RotationAngle: Single;
begin
  HidePicker(picker);
  if Params.Editable then
    ImageTmp := TUIImage.Wrap(didFinishPickingMediaWithInfo.objectForKey((UIImagePickerControllerEditedImage as ILocalObject).GetObjectID))
  else
    ImageTmp := TUIImage.Wrap(didFinishPickingMediaWithInfo.objectForKey((UIImagePickerControllerOriginalImage as ILocalObject).GetObjectID));
  RotationAngle := GetAngleOfImageOrientation(ImageTmp);
  Bitmap := UIImageToBitmap(ImageTmp, RotationAngle, Params.RequiredResolution);
  try
    DoDidFinishTaking(Bitmap);
    if Params.NeedSaveToAlbum then
      FImageManager.SaveImageToSavedPhotosAlbum(ImageTmp);
  finally
    Bitmap.DisposeOf;
  end;
end;

procedure TRSP37660Workaround.TImageDelegate.imagePickerControllerDidCancel(
  picker: UIImagePickerController);
begin
  DoDidCancelTaking;
  HidePicker(picker);
end;

{ TRSP37660Workaround.TShareService }

constructor TRSP37660Workaround.TShareService.Create;
begin
  inherited;
  FActivityItems := TNSMutableArray.Create;
end;

destructor TRSP37660Workaround.TShareService.Destroy;
begin
  ReleaseOldSharingController;
  FActivityItems.release;
  inherited;
end;

procedure TRSP37660Workaround.TShareService.ReleaseOldSharingController;
var
  Pop: UIPopoverController;
begin
  if FPopoverController <> nil then
    FPopoverController.release;
  Pop := FPopoverController;

  if FActivityViewController <> nil then
  begin
    FActivityViewController.release;
    FActivityViewController := nil;
  end;
end;

procedure TRSP37660Workaround.TShareService.Share(const AControl: TControl;
  const AText: string; const ABitmap: TBitmap);

  procedure ShowForPhone;
  var
    Window: UIWindow;
  begin
    Window := SharedApplication.keyWindow;
    if (Window <> nil) and (Window.rootViewController <> nil) then
      Window.rootViewController.presentModalViewController(FActivityViewController, True);
  end;

  procedure ShowForPad;
  var
    Window: UIWindow;
    PopoverRect: CGRect;
    AbsolutePos: TPointF;
  begin
    Window := SharedApplication.keyWindow;
    if AControl <> nil then
    begin
      AbsolutePos := AControl.LocalToAbsolute(PointF(0, 0));
      if AControl.Scene <> nil then
        AbsolutePos := AControl.Scene.LocalToScreen(AbsolutePos);
      PopoverRect := CGRectMake(AbsolutePos.X, AbsolutePos.Y, AControl.Width, AControl.Height);
    end
    else
      PopoverRect := CGRectMake(0, 0, 0, 0);
    FPopoverController := TUIPopoverController.Alloc;
    FPopoverController.initWithContentViewController(FActivityViewController);
    FPopoverController.presentPopoverFromRect(PopoverRect, Window.rootViewController.View, UIPopoverArrowDirectionAny, True);
  end;

  procedure ShowActionsSheet;
  begin
    if IsPad then
      ShowForPad
    else
      ShowForPhone;
  end;

var
  OCImage: UIImage;
  LBitmap: TBitmap;
begin
  Assert((ABitmap <> nil) or not AText.IsEmpty);
  FActivityItems.removeAllObjects;

  if not AText.IsEmpty then
    FActivityItems.addObject((StrToNSStr(AText) as ILocalObject).GetObjectID);

  if (ABitmap <> nil) and not ABitmap.IsEmpty then
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.Assign(ABitmap);
      SetPixelFormatRGBA(LBitmap);
      OCImage := BitmapToUIImage(LBitmap);
    finally
      LBitmap.Free;
    end;
    FActivityItems.addObject((OCImage as ILocalObject).GetObjectID);
  end;

  try
    if FActivityItems.count > 0 then
    begin
      ReleaseOldSharingController;
      FActivityViewController := TUIActivityViewController.alloc;
      FActivityViewController.initWithActivityItems(FActivityItems , nil);
      ShowActionsSheet;
    end;
  finally
    if OCImage <> nil then
      OCImage.release;
  end;
end;

{$ENDIF}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{$REGION ' - Workaround RSP-37829'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a bug involving the
// -   wrong render of effects/filters using Metal.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-37829
// -
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
{$IFDEF MACOS}

type
  { TRSP37829Workaround }

  TRSP37829Workaround = record
  private class var
    FOriginalDoEndScene: Pointer;
  private type
    TDoEndSceneProc = procedure of object;

    {$RTTI EXPLICIT METHODS([vcProtected])}
    TContextMetalPatch = class(TCustomContextMetal)
    protected
      FPipelineStateConfiguration: TPipelineStateConfiguration;
      FDepthStencilStateConfiguration: TDepthStencilStateConfiguration;
      FCommandQueue: MTLCommandQueue;
      FCommandBuffer: MTLCommandBuffer;
      FRenderPassDescriptor: MTLRenderPassDescriptor;
      FRenderCommandEncoder: MTLRenderCommandEncoder;
      FCurrentDrawable: CAMetalDrawable;
      FOnScreenTexture: MTLTexture;
      FStencilReferenceValue: LongWord;
      FSampleCount: NSUInteger;
      procedure DoEndScene; override;
    end;
    {$RTTI METHODS([vcPublic, vcPublished])}
  public
    class procedure Apply; static;
  end;

{ TRSP37829Workaround }

class procedure TRSP37829Workaround.Apply;

  function HookVMT(const AVMTEntry, ATargetAddress: Pointer): Boolean;
  var
    LOriginEnd: Pointer;
    LOriginStart: Pointer;
    LPageSize: Integer;
    LTargetEnd: Pointer;
    LTargetStart: Pointer;
  begin
    LPageSize    := sysconf(_SC_PAGESIZE);
    LOriginStart := Pointer(NativeUInt(AVMTEntry) and not (LPageSize - 1));
    LOriginEnd   := Pointer((NativeUInt(AVMTEntry) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    LTargetStart := Pointer(NativeUInt(ATargetAddress) and not (LPageSize - 1));
    LTargetEnd   := Pointer((NativeUInt(ATargetAddress) + SizeOf(Pointer) + Cardinal(LPageSize) - 1) and not (LPageSize - 1));
    Result := (mprotect(LOriginStart, PByte(LOriginEnd) - PByte(LOriginStart), PROT_READ or PROT_WRITE) = 0) and (mprotect(LTargetStart, PByte(LTargetEnd) - PByte(LTargetStart), PROT_READ or PROT_WRITE) = 0);
    if Result then
      PPointer(AVMTEntry)^ := PPointer(ATargetAddress)^;
  end;

  function TryGetVMT(const AClass: TClass; const AMethodName: string; out AVMT: Integer): Boolean;
  var
    LRttiContext: TRttiContext;
    LRttiMethod: TRttiMethod;
  begin
    LRttiContext := TRttiContext.Create;
    try
      LRttiMethod := LRttiContext.GetType(AClass).AsInstance.GetMethod(AMethodName);
      Result := Assigned(LRttiMethod);
      if Result then
        AVMT := LRttiMethod.VirtualIndex * SizeOf(Pointer);
    finally
      LRttiContext.Free;
    end;
  end;

var
  LVMT: Integer;
begin
  if TContextManager.DefaultContextClass.InheritsFrom(TCustomContextMetal) and TryGetVMT(TContextMetalPatch, 'DoEndScene', LVMT) then
  begin
    FOriginalDoEndScene := PPointer(PByte(TContextManager.DefaultContextClass) + LVMT)^;
    HookVMT(PByte(TContextManager.DefaultContextClass) + LVMT, PByte(TContextMetalPatch) + LVMT);
  end;
end;

{ TRSP37829Workaround.TContextMetalPatch }

procedure TRSP37829Workaround.TContextMetalPatch.DoEndScene;
var
  LOriginalMethod: TMethod;
begin
  LOriginalMethod.Data := Self;
  LOriginalMethod.Code := FOriginalDoEndScene;
  TDoEndSceneProc(LOriginalMethod)();
  if FCommandBuffer <> nil then
    FCommandBuffer.waitUntilCompleted;
end;

{$ENDIF}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

type
  { TSkBitmapCodec }

  TSkBitmapCodec = class(TCustomBitmapCodec)
  private class var
    FFileExtensions: TStringList;
  private
    class constructor Create;
    class destructor Destroy;
    class procedure Initialize;
  private
    function FitSize(const AWidth, AHeight: Integer; const AFitWidth, AFitHeight: Single): TSize; inline;
    class procedure RegisterCodec(const AFileExtension, ADescription: string; const ACanSave: Boolean; const AOverride: Boolean = False);
  public
    function LoadFromFile(const AFileName: string; const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal = 0): Boolean; override;
    function LoadFromStream(const AStream: TStream; const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal = 0): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single; const AUseEmbedded: Boolean; const ABitmapSurface: TBitmapSurface): Boolean; override;
    function SaveToFile(const AFileName: string; const ABitmapSurface: TBitmapSurface; const ASaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    function SaveToStream(const AStream: TStream; const ABitmapSurface: TBitmapSurface; const AExtension: string; const ASaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
  end;

{$IFDEF SKIA_RASTER}

  { TSkRasterCanvas }

  TSkRasterCanvas = class(TSkCanvasBase)
  private
    FBackBufferSurface: ISkSurface;
    {$IF DEFINED(LINUX)}
    FBitmap: Pointer;
    FBitmapBits: Pointer;
    {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
    FBitmap: CGContextRef;
    FColorSpace: CGColorSpaceRef;
    {$ELSEIF DEFINED(MSWINDOWS)}
    FBitmap: HBITMAP;
    {$ENDIF}
    procedure ReleaseBitmap; inline;
  protected
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; override;
    procedure Resized; override;
    procedure SwapBuffers(const AContextHandle: THandle); override;
  public
    destructor Destroy; override;
  end;

{$ENDIF}

function GetPixelFormat: TPixelFormat; inline;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := TPixelFormat.BGRA;
  {$ELSEIF DEFINED(IOS)}
  if GlobalUseMetal then
    Result := TPixelFormat.BGRA
  else
    Result := TPixelFormat.RGBA;
  {$ELSEIF DEFINED(MACOS)}
  Result := TPixelFormat.BGRA;
  {$ELSE}
  Result := TPixelFormat.RGBA;
  {$ENDIF}
end;

{ TSkCanvasCustom }

procedure TSkCanvasCustom.AfterConstruction;
begin
  SkInitialize;
  inherited;
end;

procedure TSkCanvasCustom.BeforeRestore;
begin
end;

function TSkCanvasCustom.BeginCanvas(const AContextHandle: THandle): ISkCanvas;
begin
  if FWrapper <> nil then
    Result := FWrapper.CanvasBegin(Self)
  else
    Result := nil;
end;

procedure TSkCanvasCustom.BeginPaint(const ARect: TRectF;
  const AOpacity: Single; var ABrushData: TBrushData);
const
  WrapMode: array[TWrapMode.Tile..TWrapMode.TileOriginal] of TSkTileMode = (TSkTileMode.Repeat, TSkTileMode.Clamp);
var
  I: Integer;
  LCache: TSkImage;
  LCenter: TPointF;
  LColors: TArray<TAlphaColor>;
  LImage: ISkImage;
  LMatrix: TMatrix;
  LPositions: TArray<Single>;
  LRadius: Single;
  LRadiusX: Single;
  LRadiusY: Single;
begin
  ABrushData.Paint.AntiAlias := Quality <> TCanvasQuality.HighPerformance;
  case ABrushData.Brush.Kind of
    TBrushKind.Solid: ABrushData.Paint.Color := MakeColor(ABrushData.Brush.Color, AOpacity);
    TBrushKind.Gradient:
      begin
        SetLength(LColors, ABrushData.Brush.Gradient.Points.Count);
        SetLength(LPositions, ABrushData.Brush.Gradient.Points.Count);
        case ABrushData.Brush.Gradient.Style of
          TGradientStyle.Linear:
            begin
              for I := 0 to ABrushData.Brush.Gradient.Points.Count - 1 do
              begin
                LColors[I]    := MakeColor(ABrushData.Brush.Gradient.Points[I].Color, AOpacity);
                LPositions[I] := ABrushData.Brush.Gradient.Points[I].Offset;
              end;
              ABrushData.Paint.Shader := TSkShader.MakeGradientLinear(TPointF.Create(ARect.Left + ABrushData.Brush.Gradient.StartPosition.X * ARect.Width, ARect.Top + ABrushData.Brush.Gradient.StartPosition.Y * ARect.Height), TPointF.Create(ARect.Left + ABrushData.Brush.Gradient.StopPosition.X * ARect.Width, ARect.Top + ABrushData.Brush.Gradient.StopPosition.Y * ARect.Height), LColors, LPositions);
            end;
          TGradientStyle.Radial:
            begin
              for I := 0 to ABrushData.Brush.Gradient.Points.Count - 1 do
              begin
                LColors[ABrushData.Brush.Gradient.Points.Count - 1 - I]    := MakeColor(ABrushData.Brush.Gradient.Points[I].Color, AOpacity);
                LPositions[ABrushData.Brush.Gradient.Points.Count - 1 - I] := 1 - ABrushData.Brush.Gradient.Points[I].Offset;
              end;
              LCenter  := TPointF.Create(ARect.Width * ABrushData.Brush.Gradient.RadialTransform.RotationCenter.X, ARect.Height * ABrushData.Brush.Gradient.RadialTransform.RotationCenter.Y) + ARect.TopLeft;
              LRadiusX := ABrushData.Brush.Gradient.RadialTransform.Scale.X * (ARect.Width  / 2);
              LRadiusY := ABrushData.Brush.Gradient.RadialTransform.Scale.Y * (ARect.Height / 2);
              if not SameValue(LRadiusX, LRadiusY, Epsilon) then
              begin
                if LRadiusX < LRadiusY then
                begin
                  LRadius := LRadiusY;
                  LMatrix := TMatrix.CreateScaling(LRadiusX / LRadiusY, 1) * TMatrix.CreateTranslation(LCenter.X - (LCenter.X * (LRadiusX / LRadiusY)), 0);
                end
                else
                begin
                  LRadius := LRadiusX;
                  LMatrix := TMatrix.CreateScaling(1, LRadiusY / LRadiusX) * TMatrix.CreateTranslation(0, LCenter.Y - (LCenter.Y * (LRadiusY / LRadiusX)));
                end;
                ABrushData.Paint.Shader := TSkShader.MakeGradientRadial(LCenter, LRadius, LColors, LMatrix, LPositions);
              end
              else
                ABrushData.Paint.Shader := TSkShader.MakeGradientRadial(LCenter, LRadiusX, LColors, LPositions);
            end;
        end;
      end;
    TBrushKind.Bitmap:
      begin
        ABrushData.Paint.Alpha := 0;
        if ABrushData.Brush.Bitmap.Bitmap.HandleAllocated then
        begin
          LCache := nil;
          if (ABrushData.Brush.Bitmap.Bitmap.CanvasClass.InheritsFrom(TSkCanvasCustom)) and (SupportsCachedImage) then
          begin
            LCache := GetCachedImage(ABrushData.Brush.Bitmap.Bitmap);
            if LCache <> nil then
            begin
              ABrushData.BitmapMapped := False;
              ABrushData.Paint.AlphaF := AOpacity;
              if ABrushData.Brush.Bitmap.WrapMode = TWrapMode.TileStretch then
                ABrushData.Paint.Shader := LCache.MakeShader(TMatrix.CreateScaling(ARect.Width / LCache.Width, ARect.Height / LCache.Height) * TMatrix.CreateTranslation(ARect.Left, ARect.Top), GetSamplingOptions)
              else
                ABrushData.Paint.Shader := LCache.MakeShader(GetSamplingOptions, WrapMode[ABrushData.Brush.Bitmap.WrapMode], WrapMode[ABrushData.Brush.Bitmap.WrapMode]);
            end;
          end;
          if LCache = nil then
          begin
            ABrushData.BitmapMapped := ABrushData.Brush.Bitmap.Bitmap.Map(TMapAccess.Read, ABrushData.BitmapData);
            if ABrushData.BitmapMapped then
            begin
              ABrushData.Paint.AlphaF := AOpacity;
              LImage := TSkImage.MakeFromRaster(TSkImageInfo.Create(ABrushData.BitmapData.Width, ABrushData.BitmapData.Height, SkFmxColorType[ABrushData.BitmapData.PixelFormat]), ABrushData.BitmapData.Data, ABrushData.BitmapData.Pitch);
              if LImage <> nil then
              begin
                if ABrushData.Brush.Bitmap.WrapMode = TWrapMode.TileStretch then
                  ABrushData.Paint.Shader := LImage.MakeShader(TMatrix.CreateScaling(ARect.Width / LImage.Width, ARect.Height / LImage.Height) * TMatrix.CreateTranslation(ARect.Left, ARect.Top), GetSamplingOptions)
                else
                  ABrushData.Paint.Shader := LImage.MakeShader(GetSamplingOptions, WrapMode[ABrushData.Brush.Bitmap.WrapMode], WrapMode[ABrushData.Brush.Bitmap.WrapMode]);
              end;
            end;
          end;
        end;
      end;
  end;
end;

function TSkCanvasCustom.BeginPaintWithBrush(const ABrush: TBrush;
  const ARect: TRectF; const AOpacity: Single;
  out ABrushData: TBrushData): TSkPaint;
begin
  ABrushData.Brush := ABrush;
  while (ABrushData.Brush <> nil) and (ABrushData.Brush.Kind = TBrushKind.Resource) do
    ABrushData.Brush := ABrushData.Brush.Resource.Brush;
  if (ABrushData.Brush = nil) or (ABrushData.Brush.Kind = TBrushKind.None) then
    Exit(nil);
  ABrushData.Paint := TSkPaint.Create(TSkPaintStyle.Fill);
  BeginPaint(ARect, AOpacity, ABrushData);
  Result := TSkPaint(ABrushData.Paint);
end;

function TSkCanvasCustom.BeginPaintWithStrokeBrush(const ABrush: TStrokeBrush;
  const ARect: TRectF; const AOpacity: Single;
  out ABrushData: TBrushData): TSkPaint;
const
  StrokeCap : array[TStrokeCap] of TSkStrokeCap = (TSkStrokeCap.Square, TSkStrokeCap.Round);
  StrokeJoin: array[TStrokeJoin] of TSkStrokeJoin = (TSkStrokeJoin.Miter, TSkStrokeJoin.Round, TSkStrokeJoin.Bevel);
var
  I: Integer;
  LCap: Single;
  LDash: TDashArray;
begin
  ABrushData.Brush := ABrush;
  while (ABrushData.Brush <> nil) and (ABrushData.Brush.Kind = TBrushKind.Resource) do
    ABrushData.Brush := ABrushData.Brush.Resource.Brush;
  if (ABrushData.Brush = nil) or (ABrushData.Brush.Kind = TBrushKind.None) or (SameValue(TStrokeBrush(ABrushData.Brush).Thickness, 0, TEpsilon.Position)) then
    Exit(nil);
  ABrushData.Paint := TSkPaint.Create(TSkPaintStyle.Stroke);
  BeginPaint(ARect, AOpacity, ABrushData);
  ABrushData.Paint.StrokeCap   := StrokeCap[TStrokeBrush(ABrushData.Brush).Cap];
  ABrushData.Paint.StrokeJoin  := StrokeJoin[TStrokeBrush(ABrushData.Brush).Join];
  ABrushData.Paint.StrokeWidth := TStrokeBrush(ABrushData.Brush).Thickness;
  LDash := TStrokeBrush(ABrushData.Brush).DashArray;
  if Length(LDash) > 0 then
  begin
    if TStrokeBrush(ABrushData.Brush).Dash = TStrokeDash.Custom then
      LCap := TStrokeBrush(ABrushData.Brush).Thickness
    else
      LCap := 0;
    for I := 0 to Length(LDash) - 1 do
    begin
      if Odd(I) then
        LDash[I] := (LDash[I]  + 1) * TStrokeBrush(ABrushData.Brush).Thickness - LCap
      else
        LDash[I] := (LDash[I] - 1) * TStrokeBrush(ABrushData.Brush).Thickness + LCap;
    end;
    ABrushData.Paint.PathEffect := TSkPathEffect.MakeDash(TArray<Single>(LDash), TStrokeBrush(ABrushData.Brush).DashOffset * TStrokeBrush(ABrushData.Brush).Thickness);
  end;
  Result := TSkPaint(ABrushData.Paint);
end;

{$IF CompilerVersion < 35}

procedure TSkCanvasCustom.Clear(const AColor: TAlphaColor);
begin
  if BeginSceneCount > 0 then
    Canvas.Clear(AColor);
end;

procedure TSkCanvasCustom.ClearRect(const ARect: TRectF;
  const AColor: TAlphaColor);
begin
  if BeginSceneCount > 0 then
  begin
    Canvas.Save;
    try
      Canvas.ClipRect(ARect);
      Canvas.Clear(AColor);
    finally
      Canvas.Restore;
    end;
  end;
end;

{$ENDIF}

constructor TSkCanvasCustom.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  inherited;
  if not Supports(APrinter, ISkCanvasWrapper, FWrapper) then
    raise EPrinter.CreateResFmt(@SInvalidPrinterClass, [APrinter.ClassName]);
  FWidth  := FWrapper.CanvasWidth;
  FHeight := FWrapper.CanvasHeight;
end;

function TSkCanvasCustom.CreateSaveState: TCanvasSaveState;
begin
  Result := TSaveState.Create;
end;

destructor TSkCanvasCustom.Destroy;
begin
  SkFinalize;
  inherited;
end;

function TSkCanvasCustom.DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects;
  AContextHandle: THandle): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    FCanvas := BeginCanvas(AContextHandle);
    Result  := FCanvas <> nil;
    if Result then
    begin
      FContextHandle := AContextHandle;
      Canvas.SetMatrix(Matrix * TMatrix.CreateScaling(Scale, Scale));
    end;
  end;
end;

{$IF CompilerVersion >= 35}

procedure TSkCanvasCustom.DoClear(const AColor: TAlphaColor);
begin
  Canvas.Clear(AColor);
end;

procedure TSkCanvasCustom.DoClearRect(const ARect: TRectF;
  const AColor: TAlphaColor);
begin
  Canvas.Save;
  try
    Canvas.ClipRect(ARect);
    Canvas.Clear(AColor);
  finally
    Canvas.Restore;
  end;
end;

{$ENDIF}

procedure TSkCanvasCustom.DoDrawBitmap(const ABitmap: FMX.Graphics.TBitmap;
  const ASrcRect, ADestRect: TRectF; const AOpacity: Single;
  const AHighSpeed: Boolean);
var
  LBitmapData: TBitmapData;
  LCache: TSkImage;
  LImage: ISkImage;
  LPaint: ISkPaint;
  LSrcRect: TRectF;
begin
  LSrcRect := ASrcRect * TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height);
  if ABitmap.HandleAllocated and (not LSrcRect.IsEmpty) and (not ADestRect.IsEmpty) then
  begin
    LPaint := TSkPaint.Create;
    LPaint.AlphaF := AOpacity;
    {$IFDEF MODULATE_CANVAS}
    if FModulateColor <> TAlphaColors.Null then
      LPaint.ColorFilter := TSkColorFilter.MakeBlend(FModulateColor, TSkBlendMode.SrcIn);
    {$ENDIF}
    LCache := nil;
    if (ABitmap.CanvasClass.InheritsFrom(TSkCanvasCustom)) and (SupportsCachedImage) then
    begin
      LCache := GetCachedImage(ABitmap);
      if LCache <> nil then
        Canvas.DrawImageRect(LCache, LSrcRect, ADestRect, GetSamplingOptions(AHighSpeed), LPaint);
    end;
    if LCache = nil then
    begin
      if ABitmap.Map(TMapAccess.Read, LBitmapData) then
      begin
        try
          LImage := TSkImage.MakeFromRaster(TSkImageInfo.Create(LBitmapData.Width, LBitmapData.Height, SkFmxColorType[LBitmapData.PixelFormat]), LBitmapData.Data, LBitmapData.Pitch);
          if LImage <> nil then
            Canvas.DrawImageRect(LImage, LSrcRect, ADestRect, GetSamplingOptions(AHighSpeed), LPaint);
        finally
          ABitmap.Unmap(LBitmapData);
        end;
      end;
    end;
  end;
end;

procedure TSkCanvasCustom.DoDrawEllipse(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
  LPathBuilder: ISkPathBuilder;
begin
  LPaint := BeginPaintWithStrokeBrush(ABrush, ARect, AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      LPathBuilder := TSkPathBuilder.Create;
      LPathBuilder.AddOval(ARect, TSkPathDirection.CW, 3);
      Canvas.DrawPath(LPathBuilder.Detach, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoDrawLine(const APoint1, APoint2: TPointF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
begin
  LPaint := BeginPaintWithStrokeBrush(ABrush, TRectF.Create(APoint1, APoint2), AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      Canvas.DrawLine(APoint1, APoint2, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoDrawPath(const APath: TPathData;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
begin
  LPaint := BeginPaintWithStrokeBrush(ABrush, APath.GetBounds, AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      Canvas.DrawPath(APath.ToSkPath, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoDrawRect(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
begin
  LPaint := BeginPaintWithStrokeBrush(ABrush, ARect, AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      Canvas.DrawRect(ARect, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoEndScene;
begin
  EndCanvas(FContextHandle);
  inherited;
end;

procedure TSkCanvasCustom.DoFillEllipse(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
  LPathBuilder: ISkPathBuilder;
begin
  LPaint := BeginPaintWithBrush(ABrush, ARect, AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      LPathBuilder := TSkPathBuilder.Create;
      LPathBuilder.AddOval(ARect, TSkPathDirection.CW, 3);
      Canvas.DrawPath(LPathBuilder.Detach, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoFillPath(const APath: TPathData;
  const AOpacity: Single; const ABrush: TBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
begin
  LPaint := BeginPaintWithBrush(ABrush, APath.GetBounds, AOpacity, LBrushData);
  if LPaint <> nil then
  begin
    try
      Canvas.DrawPath(APath.ToSkPath, LPaint);
    finally
      EndPaint(LBrushData);
    end;
  end;
end;

procedure TSkCanvasCustom.DoFillRect(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TBrush);
var
  LBrushData: TBrushData;
  LPaint: TSkPaint;
begin
  LPaint := BeginPaintWithBrush(ABrush, ARect, AOpacity, LBrushData);
  if LPaint <> nil then
  try
    Canvas.DrawRect(ARect, LPaint);
  finally
    EndPaint(LBrushData);
  end;
end;

class procedure TSkCanvasCustom.DoFinalizeBitmap(var ABitmapHandle: THandle);
begin
end;

class function TSkCanvasCustom.DoInitializeBitmap(const AWidth,
  AHeight: Integer; const AScale: Single;
  var APixelFormat: TPixelFormat): THandle;
begin
  Result := 0;
end;

class function TSkCanvasCustom.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
begin
  Result := False;
end;

class procedure TSkCanvasCustom.DoUnmapBitmap(const ABitmapHandle: THandle;
  var ABitmapData: TBitmapData);
begin
end;

{$IF CompilerVersion >= 30}

procedure TSkCanvasCustom.DoSetMatrix(const AMatrix: TMatrix);
begin
  if BeginSceneCount > 0 then
    Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
end;

{$ENDIF}

procedure TSkCanvasCustom.EndCanvas(const AContextHandle: THandle);
begin
  if FWrapper <> nil then
    FWrapper.CanvasEnd;
  FCanvas := nil;
end;

procedure TSkCanvasCustom.EndPaint(var ABrushData: TBrushData);
begin
  ABrushData.Paint := nil;
  if (ABrushData.Brush.Kind = TBrushKind.Bitmap) and (ABrushData.BitmapMapped) then
    ABrushData.Brush.Bitmap.Bitmap.Unmap(ABrushData.BitmapData);
end;

procedure TSkCanvasCustom.ExcludeClipRect(const ARect: TRectF);
begin
  Inc(FClippingChangeCount);
  Canvas.ClipRect(ARect, TSkClipOp.Difference);
end;

function TSkCanvasCustom.GetCachedImage(
  const ABitmap: FMX.Graphics.TBitmap): TSkImage;
begin
  Result := nil;
end;

function TSkCanvasCustom.GetCanvasScale: Single;
begin
  if FWrapper <> nil then
    Result := FWrapper.CanvasScale
  else
    Result := inherited;
end;

class function TSkCanvasCustom.GetCanvasStyle: TCanvasStyles;
begin
  Result := [];
end;

{$IFDEF MODULATE_CANVAS}

function TSkCanvasCustom.GetModulateColor: TAlphaColor;
begin
  Result := FModulateColor;
end;

{$ENDIF}

function TSkCanvasCustom.GetSamplingOptions(
  const AHighSpeed: Boolean): TSkSamplingOptions;
begin
  Result := QualityToSamplingOptions(Quality, AHighSpeed);
end;

procedure TSkCanvasCustom.IntersectClipRect(const ARect: TRectF);
begin
  Inc(FClippingChangeCount);
  Canvas.ClipRect(ARect);
end;

function TSkCanvasCustom.LoadFontFromStream(const AStream: TStream): Boolean;
begin
  TSkDefaultProviders.RegisterTypeface(AStream);
  Result := True;
end;

function TSkCanvasCustom.PtInPath(const APoint: TPointF;
  const APath: TPathData): Boolean;
var
  LPaint: ISkPaint;
  LPath: ISkPath;
begin
  LPaint := TSkPaint.Create;
  LPath  := LPaint.GetFillPath(APath.ToSkPath);
  Result := (LPath <> nil) and (LPath.Contains(APoint.X, APoint.Y));
end;

class function TSkCanvasCustom.QualityToSamplingOptions(
  const AQuality: TCanvasQuality;
  const AHighSpeed: Boolean): TSkSamplingOptions;
begin
  if AHighSpeed then
    Result := TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None)
  else
  begin
    case AQuality of
      TCanvasQuality.SystemDefault,
      TCanvasQuality.HighQuality: Result := TSkSamplingOptions.High;
    else
      Result := TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.Nearest);
    end;
  end;
end;

procedure TSkCanvasCustom.Resized;
begin
end;

{$IF CompilerVersion < 30}

procedure TSkCanvasCustom.SetMatrix(const AMatrix: TMatrix);
begin
  inherited;
  if BeginSceneCount > 0 then
    Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
end;

{$ENDIF}

{$IFDEF MODULATE_CANVAS}

procedure TSkCanvasCustom.SetModulateColor(const AColor: TAlphaColor);
begin
  FModulateColor := AColor;
end;

{$ENDIF}

procedure TSkCanvasCustom.SetSize(const AWidth, AHeight: Integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    inherited;
    Resized;
  end;
end;

function TSkCanvasCustom.SupportsCachedImage: Boolean;
begin
  Result := False;
end;

constructor TSkCanvasCustom.Wrap(const AWrapper: ISkCanvasWrapper);
begin
  inherited Create;
  FWrapper := AWrapper;
  FWidth   := FWrapper.CanvasWidth;
  FHeight  := FWrapper.CanvasHeight;
  Initialize;
end;

{ TSkCanvasCustom.TSaveState }

procedure TSkCanvasCustom.TSaveState.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TSkCanvasCustom then
    TSkCanvasCustom(ASource).Canvas.Save;
end;

procedure TSkCanvasCustom.TSaveState.AssignTo(ADest: TPersistent);
begin
  if ADest is TSkCanvasCustom then
  begin
    TSkCanvasCustom(ADest).BeforeRestore;
    TSkCanvasCustom(ADest).Canvas.Restore;
  end;
  inherited;
end;

{ TSkBitmapHandle }

constructor TSkBitmapHandle.Create(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat);
begin
  inherited Create;
  FWidth       := AWidth;
  FHeight      := AHeight;
  FPixelFormat := APixelFormat;
end;

destructor TSkBitmapHandle.Destroy;
begin
  FreeMem(FPixels);
  inherited;
end;

{ TSkCanvasBase }

function TSkCanvasBase.BeginCanvas(const AContextHandle: THandle): ISkCanvas;
var
  LBitmap: TSkBitmapHandle;
begin
  if Parent <> nil then
  begin
    FSurface := GetSurfaceFromWindow(AContextHandle);
    if FSurface = nil then
      Exit(nil);
    Result := FSurface.Canvas;
  end
  else if Bitmap <> nil then
  begin
    LBitmap := TSkBitmapHandle(Bitmap.Handle);
    if LBitmap.Pixels = nil then
      LBitmap.FPixels := AllocMem(LBitmap.Width * LBitmap.Height * PixelFormatBytes[LBitmap.PixelFormat]);
    FBitmapSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(LBitmap.Width, LBitmap.Height, SkFmxColorType[LBitmap.PixelFormat]), LBitmap.Pixels, LBitmap.Width * PixelFormatBytes[LBitmap.PixelFormat]);
    Result         := FBitmapSurface.Canvas;
  end
  else
    Result := inherited;
end;

class function TSkCanvasBase.CreateBitmap(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat): TSkBitmapHandle;
begin
  Result := TSkBitmapHandle.Create(AWidth, AHeight, APixelFormat);
end;

{$IFDEF MSWINDOWS}

constructor TSkCanvasBase.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  if WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency then
    WindowHandleToPlatform(Parent).CreateBuffer({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF});
end;

{$ENDIF}

class procedure TSkCanvasBase.DoFinalizeBitmap(var ABitmapHandle: THandle);
var
  LBitmap: TSkBitmapHandle;
begin
  LBitmap := TSkBitmapHandle(ABitmapHandle);
  {$IFDEF AUTOREFCOUNT}
  LBitmap.__ObjRelease;
  {$ENDIF}
  LBitmap.Free;
end;

class function TSkCanvasBase.DoInitializeBitmap(const AWidth, AHeight: Integer;
  const AScale: Single; var APixelFormat: TPixelFormat): THandle;
var
  LBitmap: TSkBitmapHandle;
begin
  if (APixelFormat = TPixelFormat.None) or (SkFmxColorType[APixelFormat] = TSkColorType.Unknown) then
    APixelFormat := GetPixelFormat;
  LBitmap := CreateBitmap(AWidth, AHeight, APixelFormat);
  {$IFDEF AUTOREFCOUNT}
  LBitmap.__ObjAddRef;
  {$ENDIF}
  Result := THandle(LBitmap);
end;

class function TSkCanvasBase.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
var
  LBitmap: TSkBitmapHandle;
begin
  LBitmap := TSkBitmapHandle(ABitmapHandle);
  if LBitmap.Pixels = nil then
  begin
    if AAccess = TMapAccess.Write then
      GetMem(LBitmap.FPixels, LBitmap.Width * LBitmap.Height * PixelFormatBytes[LBitmap.PixelFormat])
    else
      LBitmap.FPixels := AllocMem(LBitmap.Width * LBitmap.Height * PixelFormatBytes[LBitmap.PixelFormat]);
  end;
  ABitmapData.Data  := LBitmap.Pixels;
  ABitmapData.Pitch := LBitmap.Width * PixelFormatBytes[LBitmap.PixelFormat];
  Result := True;
end;

class procedure TSkCanvasBase.DoUnmapBitmap(const ABitmapHandle: THandle;
  var ABitmapData: TBitmapData);
begin
end;

procedure TSkCanvasBase.EndCanvas(const AContextHandle: THandle);
begin
  if Parent <> nil then
  begin
    FSurface.FlushAndSubmit;
    {$IFDEF MSWINDOWS}
    if WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency then
      FSurface.ReadPixels(TSkImageInfo.Create({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF}, TSkColorType.BGRA8888), WindowHandleToPlatform(Parent).BufferBits, {$IF CompilerVersion < 31}Width{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width{$ENDIF} * SkBytesPerPixel[TSkColorType.BGRA8888]);
    {$ENDIF}
    FSurface := nil;
    SwapBuffers(AContextHandle);
  end
  else if Bitmap <> nil then
    FBitmapSurface := nil;
  inherited;
end;

{$IFDEF MSWINDOWS}

procedure TSkCanvasBase.Resized;
begin
  inherited;
  if (Parent <> nil) and (WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency) then
    WindowHandleToPlatform(Parent).ResizeBuffer({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF});
end;

{$ENDIF}

procedure TSkCanvasBase.SwapBuffers(const AContextHandle: THandle);
begin
end;

{ TGrSharedContext }

procedure TGrSharedContext.BeginContext;
begin
  TMonitor.Enter(Self);
end;

constructor TGrSharedContext.Create;
begin
  inherited;
  SkInitialize;
end;

destructor TGrSharedContext.Destroy;
begin
  TThread.Synchronize(nil, DestroyContext);
  SkFinalize;
  inherited;
end;

procedure TGrSharedContext.DestroyContext;
begin
  TMessageManager.DefaultManager.SendMessage(TGrDirectContext(FGrDirectContext), TGrBeforeSharedContextDestructionMessage.Create);
  FGrDirectContext.AbandonContext;
  FGrDirectContext := nil;
end;

procedure TGrSharedContext.EndContext;
begin
  TMonitor.Exit(Self);
end;

procedure TGrSharedContext.FinalizeTextureCache(const ABitmap: TGrBitmapHandle);
begin
  ABitmap.Cache := nil;
end;

function TGrSharedContext.GetGrDirectContext: TGrDirectContext;
begin
  Result := TGrDirectContext(FGrDirectContext);
end;

procedure TGrSharedContext.InitializeTextureCache(
  const ABitmap: TGrBitmapHandle);
var
  LImage: ISkImage;
begin
  LImage := TSkImage.MakeFromRaster(TSkImageInfo.Create(ABitmap.Width, ABitmap.Height, SkFmxColorType[ABitmap.PixelFormat]), ABitmap.Pixels, ABitmap.Width * PixelFormatBytes[ABitmap.PixelFormat]);
  ABitmap.Cache := LImage.MakeTextureImage(ABitmap.SharedContext.GrDirectContext);
end;

procedure TGrSharedContext.RefreshContext;
begin
end;

{ TGrCanvas }

function TGrCanvas.BeginCanvas(const AContextHandle: THandle): ISkCanvas;
var
  LBitmap: TGrBitmapHandle;
begin
  if Supports(FWrapper, IGrCanvasWrapper) then
  begin
    if FSharedContext = nil then
      Exit(nil);
    FSharedContext.BeginContext;
    Result := inherited;
    if Result = nil then
      FSharedContext.EndContext;
  end
  else
  begin
    Result := inherited;
    if (Result <> nil) and (Bitmap <> nil) then
    begin
      LBitmap := TGrBitmapHandle(Bitmap.Handle);
      if LBitmap.Cache <> nil then
        TGrSharedContext(LBitmap.FSharedContext).FinalizeTextureCache(LBitmap);
    end;
  end;
end;

class function TGrCanvas.CreateBitmap(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat): TSkBitmapHandle;
begin
  Result := TGrBitmapHandle.Create(AWidth, AHeight, APixelFormat);
end;

constructor TGrCanvas.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  if not FInitialized then
  begin
    FSharedContext := CreateSharedContext;
    FInitialized   := True;
  end;
end;

class procedure TGrCanvas.DoFinalizeBitmap(var ABitmapHandle: THandle);
var
  LBitmap: TGrBitmapHandle;
begin
  LBitmap := TGrBitmapHandle(ABitmapHandle);
  if LBitmap.Cache <> nil then
    TGrSharedContext(LBitmap.FSharedContext).FinalizeTextureCache(LBitmap);
  inherited;
end;

class function TGrCanvas.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
var
  LBitmap: TGrBitmapHandle;
begin
  Result  := inherited;
  if (Result) and (AAccess <> TMapAccess.Read) then
  begin
    LBitmap := TGrBitmapHandle(ABitmapHandle);
    if (AAccess <> TMapAccess.Read) and (LBitmap.Cache <> nil) then
      TGrSharedContext(LBitmap.FSharedContext).FinalizeTextureCache(LBitmap);
  end;
end;

procedure TGrCanvas.EndCanvas(const AContextHandle: THandle);
begin
  inherited;
  if Supports(FWrapper, IGrCanvasWrapper) then
    FSharedContext.EndContext;
end;

function TGrCanvas.GetCachedImage(
  const ABitmap: FMX.Graphics.TBitmap): TSkImage;
var
  LBitmap: TGrBitmapHandle;
begin
  LBitmap := TGrBitmapHandle(ABitmap.Handle);
  if LBitmap.Cache = nil then
  begin
    LBitmap.FSharedContext := FSharedContext;
    TGrSharedContext(LBitmap.FSharedContext).InitializeTextureCache(LBitmap);
  end;
  Result := TSkImage(LBitmap.Cache);
end;

function TGrCanvas.SupportsCachedImage: Boolean;
begin
  Result := (Parent <> nil) or (Supports(FWrapper, IGrCanvasWrapper));
end;

type
  { TSkTextAttributedRange }

{$IF CompilerVersion >= 32}
  TSkTextAttributedRange = TTextAttributedRange;
{$ELSE}
  TSkTextAttributedRange = class(TTextAttributedRange)
  public
    destructor Destroy; override;
  end;

{ TSkTextAttributedRange }

destructor TSkTextAttributedRange.Destroy;
begin
  Attribute.Font := nil;
  inherited;
end;
{$ENDIF}

{ TSkTextLayout }

procedure TSkTextLayout.ConvertToPath(const APath: TPathData);
var
  I: Integer;
  LPath: TPathData;
begin
  if (Length(FParagraphs) = 0) or (APath = nil) then
    Exit;
  for I := 0 to Length(FParagraphs) - 1 do
  begin
    if FParagraphs[I].Paragraph <> nil then
    begin
      LPath := TPathData.Create;
      try
        LPath.AddSkPath(FParagraphs[I].Paragraph.ToPath);
        LPath.Translate(FParagraphs[I].Offset + TopLeft);
        APath.AddPath(LPath);
      finally
        LPath.Free;
      end;
    end;
  end;
end;

class constructor TSkTextLayout.Create;
begin
  FAttributesRangeComparer := TComparer<TTextAttributedRange>.Construct(
    function(const ALeft, ARight: TTextAttributedRange): Integer
    begin
      if (ALeft.Range.Pos + ALeft.Range.Length) <= ARight.Range.Pos then
        Result := -1
      else if ALeft.Range.Pos >= (ARight.Range.Pos + ARight.Range.Length) then
        Result := 1
      else
        Result := 0;
    end);
  FParagraphTextRangeComparer := TComparer<TParagraph>.Construct(
    function(const ALeft, ARight: TParagraph): Integer
    begin
      if (ALeft.Range.Length = 0) or (ARight.Range.Length = 0) then
      begin
        if InRange(ALeft.Range.Pos, ARight.Range.Pos, ARight.Range.Pos + ARight.Range.Length) or
          InRange(ARight.Range.Pos, ALeft.Range.Pos, ALeft.Range.Pos + ALeft.Range.Length) then
        begin
          Result := 0;
        end
        else
          Result := CompareValue(ALeft.Range.Pos, ARight.Range.Pos);
      end
      else if (ALeft.Range.Pos + ALeft.Range.Length) <= ARight.Range.Pos then
        Result := -1
      else if ALeft.Range.Pos >= (ARight.Range.Pos + ARight.Range.Length) then
        Result := 1
      else
        Result := 0;
    end);
end;

constructor TSkTextLayout.Create(const ACanvas: TCanvas);
begin
  inherited;
  SkInitialize;
  {$IF DECLARED(TRSP36975Workaround))}
  FRSP36975Workaround.Apply(Self);
  {$ENDIF}
end;

destructor TSkTextLayout.Destroy;
begin
  FParagraphs := nil;
  SkFinalize;
  inherited;
end;

procedure TSkTextLayout.DoDrawLayout(const ACanvas: ISkCanvas);
var
  I: Integer;
begin
  if (Length(FParagraphs) > 0) and (ACanvas <> nil) then
  begin
    if (FColor <> Color) or (FOpacity <> Opacity) then
      UpdateParagraph;
    ACanvas.Save;
    try
      ACanvas.ClipRect(TRectF.Create(TopLeft, MaxSize.X, MaxSize.Y));
      for I := 0 to Length(FParagraphs) - 1 do
      begin
        if FParagraphs[I].Paragraph <> nil then
          FParagraphs[I].Paragraph.Paint(ACanvas, FParagraphs[I].Offset.X + TopLeft.X, FParagraphs[I].Offset.Y + TopLeft.Y);
      end;
    finally
      ACanvas.Restore;
    end;
  end;
end;

procedure TSkTextLayout.DoDrawLayout(const ACanvas: TCanvas);
var
  I: Integer;
  LTextLayout: TTextLayout;
  LTextLayoutClass: TTextLayoutClass;
begin
  if ACanvas = nil then
    Exit;
  if ACanvas is TSkCanvasCustom then
  begin
    if ACanvas.BeginSceneCount > 0 then
      DoDrawLayout(TSkCanvasCustom(ACanvas).Canvas);
  end
  else
  begin
    LTextLayoutClass := TTextLayoutManager.TextLayoutByCanvas(ACanvas.ClassType);
    if LTextLayoutClass <> nil then
    begin
      LTextLayout := LTextLayoutClass.Create;
      try
        LTextLayout.BeginUpdate;
        try
          LTextLayout.Text            := Text;
          LTextLayout.Padding         := Padding;
          LTextLayout.WordWrap        := WordWrap;
          LTextLayout.HorizontalAlign := HorizontalAlign;
          LTextLayout.VerticalAlign   := VerticalAlign;
          LTextLayout.Color           := Color;
          LTextLayout.Font            := Font;
          LTextLayout.Opacity         := Opacity;
          LTextLayout.Trimming        := Trimming;
          LTextLayout.RightToLeft     := RightToLeft;
          LTextLayout.MaxSize         := MaxSize;
          LTextLayout.TopLeft         := TopLeft;
          for I := 0 to AttributesCount - 1 do
            LTextLayout.AddAttribute(TTextAttributedRange.Create(Attributes[I].Range, Attributes[I].Attribute));
        finally
          LTextLayout.EndUpdate;
        end;
        LTextLayout.RenderLayout(ACanvas);
      finally
        LTextLayout.Free;
      end;
    end;
  end;
end;


function TSkTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;

  function TryGetNearestParagraph(const APoint: TPointF;
    out AParagraph: TParagraph): Boolean;
  var
    I: Integer;
    LDistance: Single;
    LMinDistance: Single;
    LPoint: TPointF;
  begin
    Result       := False;
    LPoint       := APoint - TopLeft;
    LMinDistance := MaxSingle;
    for I := 0 to Length(FParagraphs) - 1 do
    begin
      if FParagraphs[I].Paragraph <> nil then
      begin
        if FParagraphs[I].Bounds.Contains(APoint) then
          LDistance := -1
        else
        begin
          LDistance := FParagraphs[I].Bounds.TopLeft.Distance(LPoint);
          LDistance := Min(LDistance, FParagraphs[I].Bounds.BottomRight.Distance(LPoint));
          LDistance := Min(LDistance, PointF(FParagraphs[I].Bounds.Left, FParagraphs[I].Bounds.Bottom).Distance(LPoint));
          LDistance := Min(LDistance, PointF(FParagraphs[I].Bounds.Right, FParagraphs[I].Bounds.Top).Distance(LPoint));
        end;
        if LDistance < 0 then
        begin
          AParagraph := FParagraphs[I];
          Exit(True);
        end;
        if LDistance < LMinDistance then
        begin
          LMinDistance := LDistance;
          AParagraph   := FParagraphs[I];
          Result       := True;
        end;
      end;
    end;
  end;

var
  LParagraph: TParagraph;
begin
  if (Length(FParagraphs) = 0) or (not TryGetNearestParagraph(APoint, LParagraph)) then
    Exit(-1);
  Result := LParagraph.Paragraph.GetGlyphPositionAtCoordinate(APoint.X - TopLeft.X - LParagraph.Offset.X, APoint.Y - TopLeft.Y - LParagraph.Offset.Y).Position;
end;

function TSkTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;

  // SkParagraph returns one rectangle for each grapheme, while other TextLayout
  // implementations consider a single rectangle containing graphemes side by
  // side on the same line. Simulating this behavior avoids issues in text
  // editing controls.
  function SummarizeRegion(const ARegion: TRegion): TRegion;
  var
    I: Integer;
    LLastRect: PRectF;
    LResultCount: Integer;
  begin
    Assert(Length(ARegion) > 1);
    LResultCount := 1;
    SetLength(Result, Length(ARegion));
    Result[0] := ARegion[0];
    LLastRect := @Result[0];
    for I := 1 to Length(ARegion) - 1 do
    begin
      if (SameValue(LLastRect.Right, ARegion[I].Left, 1)) and (InRange(ARegion[I].CenterPoint.Y, LLastRect.Top, LLastRect.Bottom) or InRange(LLastRect.CenterPoint.Y, ARegion[I].Top, ARegion[I].Bottom)) then
      begin
        LLastRect.Right  := ARegion[I].Right;
        LLastRect.Top    := Min(LLastRect.Top, ARegion[I].Top);
        LLastRect.Bottom := Max(LLastRect.Bottom, ARegion[I].Bottom);
      end
      else
      begin
        Result[LResultCount] := ARegion[I];
        Inc(LResultCount);
        Inc(LLastRect);
      end;
    end;
    SetLength(Result, LResultCount);
  end;

  function GetRegionForRange(const AStartIndex, AEndIndex: Integer): TRegion;

    function GetRegionBesidePreviousVisibleChar(const ALastParagraphProcessed: TParagraph): TRegion;
    var
      I: Integer;
      J: Integer;
      LFoundPreviousVisibleChar: Boolean;
      LLineMetrics: TArray<TSkMetrics>;
      LPreviousVisibleChar: Integer;
    begin
      Result := [];
      LPreviousVisibleChar := -1;
      LFoundPreviousVisibleChar := False;
      for I := ALastParagraphProcessed.Index downto 0 do
      begin
        if (AEndIndex - AStartIndex = 1) and FParagraphs[I].Bounds.IsEmpty then
        begin
          Result := [FParagraphs[I].Bounds];
          Exit;
        end;
        LLineMetrics := FParagraphs[I].Paragraph.LineMetrics;
        for J := Length(LLineMetrics) - 1 downto 0 do
        begin
          if AStartIndex - FParagraphs[I].Range.Pos <= NativeInt(LLineMetrics[J].StartIndex) then
          begin
            LFoundPreviousVisibleChar := True;
            Break;
          end;
          LPreviousVisibleChar := Integer(LLineMetrics[J].EndIndex) + FParagraphs[I].Range.Pos;
        end;
        if LFoundPreviousVisibleChar then
          Break;
      end;
      if LPreviousVisibleChar > -1 then
        Result := DoRegionForRange(TTextRange.Create(LPreviousVisibleChar, 0));
    end;

  var
    I: Integer;
    LLength: Integer;
    LParagraph: TParagraph;
    LParagraphsInRange: TArray<TParagraph>;
    LTextBoxes: TArray<TSkTextBox>;
  begin
    Result := nil;
    LParagraphsInRange := GetParagraphsInRange(AStartIndex, AEndIndex - AStartIndex);
    for LParagraph in LParagraphsInRange do
    begin
      if LParagraph.Paragraph <> nil then
      begin
        LTextBoxes := LParagraph.Paragraph.GetRectsForRange(AStartIndex - LParagraph.Range.Pos, AEndIndex - LParagraph.Range.Pos, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
        LLength    := Length(Result);
        SetLength(Result, LLength + Length(LTextBoxes));
        for I := LLength to Length(Result) - 1 do
        begin
          Result[I] := LTextBoxes[I - LLength].Rect;
          Result[I].Offset(LParagraph.Offset + TopLeft);
        end;
      end;
    end;
    // Simulate the same behavior as FMX: when a range is a character that does not appear on the screen, and therefore
    // shouldn't have a region, FMX returns an empty region on the right side of the last visible char on the same line.
    if (Length(Result) = 0) and (Length(LParagraphsInRange) > 0) then
      Result := GetRegionBesidePreviousVisibleChar(LParagraphsInRange[High(LParagraphsInRange)]);
    if Length(Result) > 1 then
      Result := SummarizeRegion(Result);
  end;

var
  LEndIndex: Integer;
  LStartIndex: Integer;
begin
  if Length(FParagraphs) = 0 then
    Exit(nil);
  FGraphemesMap.Text := Text;
  LStartIndex := EnsureRange(ARange.Pos, 0, Text.Length);
  LEndIndex := Min(LStartIndex + Max(ARange.Length, 0), Text.Length);
  FGraphemesMap.FindPreviousGraphemeBoundary(LStartIndex);
  FGraphemesMap.FindPreviousGraphemeBoundary(LEndIndex);

  if LStartIndex = LEndIndex then
  begin
    if LStartIndex = 0 then
    begin
      // Right grapheme
      LEndIndex := 1;
      FGraphemesMap.FindNextGraphemeBoundary(LEndIndex);
      Result := GetRegionForRange(LStartIndex, LEndIndex);
      if Length(Result) > 0 then
        Result := [RectF(Result[0].Left, Result[0].Top, Result[0].Left, Result[0].Bottom)];
    end
    else
    begin
      // Left grapheme
      Dec(LStartIndex);
      FGraphemesMap.FindPreviousGraphemeBoundary(LStartIndex);
      Result := GetRegionForRange(LStartIndex, LEndIndex);
      if Length(Result) > 0 then
        Result := [RectF(Result[0].Right, Result[0].Top, Result[0].Right, Result[0].Bottom)];
    end;
  end
  else
    Result := GetRegionForRange(LStartIndex, LEndIndex);
end;

procedure TSkTextLayout.DoRenderLayout;
type
  THorizontalAlign = (Left, Center, Right);
const
  HorizontalTextAlign: array[TTextAlign, Boolean] of THorizontalAlign = ((THorizontalAlign.Center, THorizontalAlign.Center), (THorizontalAlign.Left,   THorizontalAlign.Right), (THorizontalAlign.Right,  THorizontalAlign.Left));

  function GetTextRect: TRectF;
  var
    I: Integer;
    LOffset: TPointF;
    LTextBox: TSkTextBox;
    LTextBoxes: TArray<TSkTextBox>;
  begin
    Result  := TRectF.Empty;
    LOffset := PointF(0, 0);
    for I := 0 to Length(FParagraphs) - 1 do
    begin
      if FParagraphs[I].Paragraph <> nil then
      begin
        FParagraphs[I].Bounds := TRectF.Empty;
        LTextBoxes := FParagraphs[I].Paragraph.GetRectsForRange(0, FParagraphs[I].Range.Length, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
        for LTextBox in LTextBoxes do
        begin
          if FParagraphs[I].Bounds.IsEmpty then
            FParagraphs[I].Bounds := LTextBox.Rect
          else
            FParagraphs[I].Bounds := FParagraphs[I].Bounds + LTextBox.Rect;
        end;
        if FParagraphs[I].Bounds.IsEmpty then
          FParagraphs[I].Bounds.Height := FParagraphs[I].Paragraph.Height;
        FParagraphs[I].Bounds.Offset(LOffset);
        if Result.IsEmpty then
          Result := FParagraphs[I].Bounds
        else
          Result := Result + FParagraphs[I].Bounds;
        LOffset.Y := FParagraphs[I].Bounds.Bottom;
      end;
    end;
  end;

  function GetParagraphOffset(ATextRect: TRectF): TPointF;
  var
    LBounds: TRectF;
  begin
    ATextRect := RectF(0, 0, ATextRect.Width, ATextRect.Height);
    if not NeedHorizontalAlignment then
    begin
      case HorizontalTextAlign[HorizontalAlign, RightToLeft] of
        THorizontalAlign.Center: ATextRect.Offset((MaxSize.X - Padding.Left - Padding.Right - ATextRect.Width) / 2, 0);
        THorizontalAlign.Right : ATextRect.Offset(MaxSize.X - Padding.Left - Padding.Right - ATextRect.Right, 0);
      end;
    end;

    LBounds := RectF(0, 0, ATextRect.Width, ATextRect.Height);
    case HorizontalTextAlign[HorizontalAlign, RightToLeft] of
      THorizontalAlign.Center: LBounds.Offset((MaxSize.X - Padding.Left - Padding.Right - LBounds.Width) / 2, 0);
      THorizontalAlign.Right : LBounds.Offset(MaxSize.X - Padding.Left - Padding.Right - LBounds.Right, 0);
    end;
    case VerticalAlign of
      TTextAlign.Center  : LBounds.Offset(0, (MaxSize.Y - Padding.Top - Padding.Bottom - LBounds.Height) / 2);
      TTextAlign.Trailing: LBounds.Offset(0, MaxSize.Y - Padding.Top - Padding.Bottom - LBounds.Bottom);
    end;
    LBounds.Offset(Padding.Rect.TopLeft);
    Result := LBounds.TopLeft - ATextRect.TopLeft;
  end;

var
  I: Integer;
  LOffset: TPointF;
begin
  {$IF DECLARED(TRSP36975Workaround))}
  if FRSP36975Workaround.IgnoreNeedUpdate then
    Exit;
  {$ENDIF}
  UpdateParagraph;
  FTextRect := GetTextRect;
  LOffset   := GetParagraphOffset(FTextRect);
  FTextRect.Offset(LOffset);
  for I := 0 to Length(FParagraphs) - 1 do
  begin
    if FParagraphs[I].Paragraph <> nil then
      FParagraphs[I].Bounds.Offset(LOffset);
  end;
  for I := 0 to Length(FParagraphs) - 1 do
  begin
    if FParagraphs[I].Paragraph <> nil then
    begin
      FParagraphs[I].Offset := LOffset;
      LOffset := LOffset + PointF(0, FParagraphs[I].Bounds.Height);
    end;
  end;
  case VerticalAlign of
    TTextAlign.Leading : FTextRect.Bottom := Min(MaxSize.Y - Padding.Top - Padding.Bottom, FTextRect.Bottom);
    TTextAlign.Center  : FTextRect.Inflate(0, Min(((MaxSize.Y - Padding.Top - Padding.Bottom) - FTextRect.Height) / 2, 0));
    TTextAlign.Trailing: FTextRect.Top := Max(FTextRect.Bottom - (MaxSize.Y - Padding.Top - Padding.Bottom), FTextRect.Top);
  end;
  case HorizontalTextAlign[HorizontalAlign, RightToLeft] of
    THorizontalAlign.Left  : FTextRect.Right := Min(MaxSize.X - Padding.Left - Padding.Right, FTextRect.Right);
    THorizontalAlign.Center: FTextRect.Inflate(Min(((MaxSize.X - Padding.Left - Padding.Right) - FTextRect.Width) / 2, 0), 0);
    THorizontalAlign.Right : FTextRect.Left := Max(FTextRect.Right - (MaxSize.X - Padding.Left - Padding.Right), FTextRect.Left);
  end;
end;

function TSkTextLayout.GetParagraphsInRange(const APos,
  ALength: Integer): TArray<TParagraph>;
var
  LFoundIndex: Integer;
  LParagraph: TParagraph;
begin
  Result := nil;
  LParagraph.Range := TTextRange.Create(APos, ALength);
  if TArray.BinarySearch<TParagraph>(FParagraphs, LParagraph, LFoundIndex, FParagraphTextRangeComparer) then
  begin
    repeat
      Result := Result + [FParagraphs[LFoundIndex]];
      Inc(LFoundIndex);
    until (LFoundIndex >= Length(FParagraphs)) or (FParagraphTextRangeComparer.Compare(FParagraphs[LFoundIndex], LParagraph) <> 0);
  end;
end;

function TSkTextLayout.GetTextHeight: Single;
begin
  Result := FTextRect.Height;
end;

function TSkTextLayout.GetTextRect: TRectF;
begin
  Result := FTextRect;
  Result.Offset(TopLeft);
end;

function TSkTextLayout.GetTextWidth: Single;
begin
  Result := FTextRect.Width;
end;

class procedure TSkTextLayout.Initialize;
{$IFDEF ANDROID}
const
  FontFilesFilter = '*.ttf'; // Do not localize
var
  LFileName: string;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  for LFileName in TDirectory.GetFiles(TPath.GetDocumentsPath, FontFilesFilter) do
    TSkDefaultProviders.RegisterTypeface(LFileName);
  {$ENDIF}
end;

// The SkParagraph normally doesn't cut a word in half, it cuts the whole word.
// In some scenarios it can cause some errors, for example in edits where it is
// mandatory to cut in the middle of the word. To fix this we will readjust the
// MaxWidth, align horizontally manually and cut with ClipRect.
function TSkTextLayout.NeedHorizontalAlignment: Boolean;
begin
  Result := (not WordWrap) and (Trimming = TTextTrimming.None);
end;

procedure TSkTextLayout.RenderLayout(const ACanvas: ISkCanvas);
begin
  RenderLayout(nil);
  DoDrawLayout(ACanvas);
end;

procedure TSkTextLayout.SetMaxLines(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FMaxLines <> AValue then
  begin
    FMaxLines := AValue;
    BeginUpdate;
    try
      {$IF CompilerVersion >= 29}
      SetNeedUpdate;
      {$ELSE}
      RightToLeft := not RightToLeft;
      RightToLeft := not RightToLeft;
      {$ENDIF}
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSkTextLayout.UpdateParagraph;
const
{$IF CompilerVersion >= 31}
  SkFontSlant : array[TFontSlant] of TSkFontSlant = (TSkFontSlant.Upright, TSkFontSlant.Italic, TSkFontSlant.Oblique);
  SkFontWeight: array[TFontWeight] of Integer = (100, 200, 300, 350, 400, 500, 600, 700, 800, 900, 950);
  SkFontWidth : array[TFontStretch] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9);
{$ENDIF}
  ZeroWidthChar = #8203;

  function GetFontFamilies(const AValue: string): TArray<string>; inline;
  begin
    Result := AValue.Split([', ', ','], TStringSplitOptions.ExcludeEmpty){$IFDEF MACOS} + ['Helvetica Neue']{$ELSEIF DEFINED(LINUX)} + ['Ubuntu']{$ENDIF};
  end;

  function GetNormalizedAttributes(const ASubText: string;
    const ASubTextPosition: Integer): TArray<TTextAttributedRange>;
  var
    I: Integer;
    LAttribute: TTextAttributedRange;
    LAttributes: TList<TTextAttributedRange>;
    LIndex: Integer;
    LNeighborAttribute: TTextAttributedRange;
  begin
    if AttributesCount = 0 then
      Exit(nil);
    LAttributes := TList<TTextAttributedRange>.Create;
    try
      for I := 0 to AttributesCount - 1 do
      begin
        LAttribute := TSkTextAttributedRange.Create(TTextRange.Create(Attributes[I].Range.Pos - ASubTextPosition, Attributes[I].Range.Length), Attributes[I].Attribute);
        if LAttribute.Range.Pos < 0 then
          LAttribute.Range := TTextRange.Create(0, LAttribute.Range.Pos + LAttribute.Range.Length);
        if (LAttribute.Range.Pos + LAttribute.Range.Length) > ASubText.Length then
          LAttribute.Range := TTextRange.Create(LAttribute.Range.Pos, LAttribute.Range.Length - ((LAttribute.Range.Pos + LAttribute.Range.Length) - ASubText.Length));
        if LAttribute.Range.Length <= 0 then
        begin
          LAttribute.Free;
          Continue;
        end;
        if LAttributes.BinarySearch(LAttribute, LIndex, FAttributesRangeComparer) then
        begin
          if (LAttributes[LIndex].Range.Pos < LAttribute.Range.Pos) and ((LAttributes[LIndex].Range.Pos + LAttributes[LIndex].Range.Length) > (LAttribute.Range.Pos + LAttribute.Range.Length)) then
          begin
            LNeighborAttribute := TSkTextAttributedRange.Create(TTextRange.Create(LAttribute.Range.Pos + LAttribute.Range.Length, LAttributes[LIndex].Range.Pos + LAttributes[LIndex].Range.Length - (LAttribute.Range.Pos + LAttribute.Range.Length)), LAttributes[LIndex].Attribute);
            LAttributes.Insert(LIndex + 1, LNeighborAttribute);
            LAttributes.Insert(LIndex + 1, LAttribute);
            LNeighborAttribute := LAttributes[LIndex];
            LNeighborAttribute.Range.Length := LAttribute.Range.Pos - LNeighborAttribute.Range.Pos;
            Continue;
          end;
          while (LIndex < LAttributes.Count) and
            ((LAttributes[LIndex].Range.Pos < LAttribute.Range.Pos) or
            ((LAttributes[LIndex].Range.Pos = LAttribute.Range.Pos) and (LAttributes[LIndex].Range.Length < LAttribute.Range.Length))) do
          begin
            Inc(LIndex);
          end;
        end;
        LAttributes.Insert(LIndex, LAttribute);
        if LIndex > 0 then
        begin
          LNeighborAttribute := LAttributes[LIndex - 1];
          if (LNeighborAttribute.Range.Pos + LNeighborAttribute.Range.Length) > LAttribute.Range.Pos then
          begin
            LNeighborAttribute.Range := TTextRange.Create(LNeighborAttribute.Range.Pos, LNeighborAttribute.Range.Length - ((LNeighborAttribute.Range.Pos + LNeighborAttribute.Range.Length) - LAttribute.Range.Pos));
            if LNeighborAttribute.Range.Length <= 0 then
            begin
              LNeighborAttribute.Free;
              LAttributes.Delete(LIndex - 1);
              Dec(LIndex);
            end;
          end;
        end;
        if LIndex < LAttributes.Count - 1 then
        begin
          LNeighborAttribute := LAttributes[LIndex + 1];
          if LNeighborAttribute.Range.Pos < (LAttribute.Range.Pos + LAttribute.Range.Length) then
          begin
            LNeighborAttribute.Range := TTextRange.Create(LAttribute.Range.Pos + LAttribute.Range.Length, LNeighborAttribute.Range.Length - ((LAttribute.Range.Pos + LAttribute.Range.Length) - LNeighborAttribute.Range.Pos));
            if LNeighborAttribute.Range.Length <= 0 then
            begin
              LNeighborAttribute.Free;
              LAttributes.Delete(LIndex + 1);
            end;
          end;
        end;
      end;
      Result := LAttributes.ToArray;
    finally
      LAttributes.Free;
    end;
  end;

  procedure InitializeTextStyle(const ATextStyle: ISkTextStyle; const AFont: TFont; const AColor: TAlphaColor);
  begin
    ATextStyle.Color        := MakeColor(AColor, FOpacity);
    ATextStyle.FontFamilies := GetFontFamilies(AFont.Family);
    ATextStyle.FontSize     := AFont.Size;
    {$IF CompilerVersion < 31}
    if (TFontStyle.fsBold in AFont.Style) and (TFontStyle.fsItalic in AFont.Style) then
      ATextStyle.FontStyle := TSkFontStyle.BoldItalic
    else if TFontStyle.fsBold in AFont.Style then
      ATextStyle.FontStyle := TSkFontStyle.Bold
    else if TFontStyle.fsItalic in AFont.Style then
      ATextStyle.FontStyle := TSkFontStyle.Italic
    else
      ATextStyle.FontStyle := TSkFontStyle.Normal;
    {$ELSE}
    ATextStyle.FontStyle := TSkFontStyle.Create(SkFontWeight[AFont.StyleExt.Weight], SkFontWidth[AFont.StyleExt.Stretch], SkFontSlant[AFont.StyleExt.Slant]);
    {$ENDIF}
    if (TFontStyle.fsUnderline in AFont.Style) or (TFontStyle.fsStrikeOut in AFont.Style) then
    begin
      ATextStyle.DecorationColor := MakeColor(AColor, FOpacity);
      if TFontStyle.fsUnderline in AFont.Style then
        ATextStyle.Decorations := ATextStyle.Decorations + [TSkTextDecoration.Underline];
      if TFontStyle.fsStrikeOut in AFont.Style then
        ATextStyle.Decorations := ATextStyle.Decorations + [TSkTextDecoration.LineThrough];
    end;
  end;

  function CreateTextStyle(const AAttribute: TTextAttribute): ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    if AAttribute.Font <> nil then
      InitializeTextStyle(Result, AAttribute.Font, AAttribute.Color)
    else
      InitializeTextStyle(Result, Font, AAttribute.Color);
  end;

  function CreateDefaultTextStyle: ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    InitializeTextStyle(Result, Font, Color);
  end;

  function CreateParagraphStyle(const AAttributes: TArray<TTextAttributedRange>;
    AMaxLines: Integer): ISkParagraphStyle;
  const
    SkTextAlign: array[TTextAlign] of TSkTextAlign = (TSkTextAlign.Center, TSkTextAlign.Start, TSkTextAlign.Terminate);
  var
    LAttribute: TTextAttributedRange;
    LMinFontSize: Single;
  begin
    Result := TSkParagraphStyle.Create;
    if RightToLeft then
      Result.TextDirection := TSkTextDirection.RightToLeft;
    if Trimming in [TTextTrimming.Character, TTextTrimming.Word] then
      Result.Ellipsis := '...';
    if WordWrap then
    begin
      if AMaxLines <= 0 then
        Result.MaxLines := High(Integer)
      else
        Result.MaxLines := AMaxLines;
    end
    else
      Result.MaxLines := 1;
    if NeedHorizontalAlignment then
    begin
      if RightToLeft then
        Result.TextAlign := TSkTextAlign.Terminate
      else
        Result.TextAlign := TSkTextAlign.Start;
    end
    else
      Result.TextAlign := SkTextAlign[HorizontalAlign];
    Result.TextStyle := CreateDefaultTextStyle;
    if Result.MaxLines = NativeUInt(High(Integer)) then
    begin
      LMinFontSize := Result.TextStyle.FontSize;
      for LAttribute in AAttributes do
        LMinFontSize := Min(LMinFontSize, LAttribute.Attribute.Font.Size);
      if LMinFontSize > 0.1 then
      begin
        // Avoid invalid float point operation
        if MaxSize.Y > High(Integer) then
          AMaxLines := High(Integer)
        else
          AMaxLines := Ceil(MaxSize.Y / LMinFontSize);
        if AMaxLines > 0 then
          Result.MaxLines := AMaxLines;
      end;
    end;
  end;

  // Temporary solution to fix an issue with Skia
  // https://bugs.chromium.org/p/skia/issues/detail?id=13117
  //
  // SkParagraph has several issues with the #13 line break, so the best thing
  // to do is replace it with #10 or a zero-widh character (#8203)
  function NormalizeParagraphText(const AText: string): string; inline;
  begin
    Result := AText.Replace(#13#10, ZeroWidthChar + #10).Replace(#13, #10);
  end;

  function CreateParagraph(const AMaxLines: Integer; const ASubText: string;
    const ASubTextPosition: Integer): ISkParagraph;
  var
    LAttribute: TTextAttributedRange;
    LAttributes: TArray<TTextAttributedRange>;
    LBuilder: ISkParagraphBuilder;
    LLastAttributeEndIndex: Integer;
    LText: string;
  begin
    FColor      := Color;
    FOpacity    := Opacity;
    LAttributes := GetNormalizedAttributes(ASubText, ASubTextPosition);
    try
      LBuilder := TSkParagraphBuilder.Create(CreateParagraphStyle(LAttributes, AMaxLines), TSkDefaultProviders.TypefaceFont);
      LLastAttributeEndIndex := 0;
      for LAttribute in LAttributes do
      begin
        if LLastAttributeEndIndex < LAttribute.Range.Pos then
          LBuilder.AddText(ASubText.Substring(LLastAttributeEndIndex, LAttribute.Range.Pos - LLastAttributeEndIndex));
        LText := NormalizeParagraphText(ASubText.Substring(LAttribute.Range.Pos, LAttribute.Range.Length));
        if not LText.IsEmpty then
        begin
          LBuilder.PushStyle(CreateTextStyle(LAttribute.Attribute));
          LBuilder.AddText(LText);
          LBuilder.Pop;
        end;
        LLastAttributeEndIndex := LAttribute.Range.Pos + LAttribute.Range.Length;
      end;
      if LLastAttributeEndIndex < ASubText.Length then
        LBuilder.AddText(ASubText.Substring(LLastAttributeEndIndex, ASubText.Length - LLastAttributeEndIndex));
    finally
      for LAttribute in LAttributes do
        LAttribute.DisposeOf;
    end;
    Result := LBuilder.Build;
  end;

  procedure ParagraphLayout(const AParagraph: ISkParagraph; AMaxWidth: Single);
  var
    LMetrics: TSkMetrics;
  begin
    AMaxWidth := Max(AMaxWidth, 0);
    if not SameValue(AMaxWidth, 0, TEpsilon.Position) then
    begin
      // Try to add extra value to avoid trimming when put the same value (or near) to the MaxIntrinsicWidth
      AParagraph.Layout(AMaxWidth + 1);
      for LMetrics in AParagraph.LineMetrics do
      begin
        if InRange(AMaxWidth, LMetrics.Width - TEpsilon.Position, LMetrics.Width + 1) then
          Exit;
      end;
    end;
    AParagraph.Layout(AMaxWidth);
  end;

  procedure DoUpdateParagraph(var AParagraph: TParagraph;
    const ASubText: string; const AMaxLines: Integer);
  {$IF CompilerVersion < 29}
  const
    MaxLayoutSize: TPointF = (X: $FFFF; Y: $FFFF);
  {$ENDIF}
  var
    LMetrics: TSkMetrics;
  begin
    AParagraph.Paragraph := CreateParagraph(AMaxLines, ASubText, AParagraph.Range.Pos);
    if NeedHorizontalAlignment then
      ParagraphLayout(AParagraph.Paragraph, MaxLayoutSize.X)
    else
      ParagraphLayout(AParagraph.Paragraph, MaxSize.X - Padding.Left - Padding.Right);
    if WordWrap and (AParagraph.Paragraph.Height > MaxSize.Y - Padding.Top - Padding.Bottom) then
    begin
      for LMetrics in AParagraph.Paragraph.LineMetrics do
      begin
        if (LMetrics.LineNumber <> 0) and (LMetrics.Baseline + LMetrics.Descent > MaxSize.Y - Padding.Top - Padding.Bottom) then
        begin
          AParagraph.Paragraph := CreateParagraph(LMetrics.LineNumber, ASubText, AParagraph.Range.Pos);
          ParagraphLayout(AParagraph.Paragraph, MaxSize.X - Padding.Left - Padding.Right);
          Break;
        end;
      end;
    end;
  end;

var
  I: Integer;
  LLimitedLines: Boolean;
  LLines: TArray<string>;
  LMaxLines: Integer;
  LPos: Integer;
  LText: string;
begin
  LText := Text;
  {$IF DECLARED(TRSP38480Workaround)}
  TRSP38480Workaround.Apply(LText);
  {$ENDIF}
  if WordWrap or LText.IsEmpty then
    LLines := [LText]
  else
    LLines := LText.Replace(#13#10, ZeroWidthChar + #10).Replace(#13, #10).Replace(#10, ZeroWidthChar + #10).Split([#10]);
  LPos          := 0;
  LMaxLines     := FMaxLines;
  LLimitedLines := FMaxLines <> 0;
  SetLength(FParagraphs, Length(LLines));
  for I := 0 to Length(LLines) - 1 do
  begin
    FParagraphs[I].Range := TTextRange.Create(LPos, LLines[I].Length);
    FParagraphs[I].Index := I;
    if LMaxLines = -1 then
    begin
      FParagraphs[I].Bounds    := TRectF.Empty;
      FParagraphs[I].Paragraph := nil;
      FParagraphs[I].Offset    := PointF(0, 0);
    end
    else
    begin
      DoUpdateParagraph(FParagraphs[I], LLines[I], LMaxLines);
      if LLimitedLines then
      begin
        Dec(LMaxLines, Length(FParagraphs[I].Paragraph.LineMetrics));
        if LMaxLines <= 0 then
          LMaxLines :=  -1;
      end;
    end;
    Inc(LPos, LLines[I].Length);
  end;
end;

{$REGION ' - Workaround RSP-36975'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion113))}

{ TSkTextLayout.TRSP36975Workaround }

procedure TSkTextLayout.TRSP36975Workaround.Apply(
  const ATextLayout: TTextLayout);
begin
  FIgnoreNeedUpdate := True;
  try
    ATextLayout.RightToLeft := Application.BiDiMode = TBiDiMode.bdRightToLeft;
  finally
    FIgnoreNeedUpdate := False;
  end;
end;

{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{$REGION ' - Workaround RSP-38480'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}

{ TSkTextLayout.TRSP38480Workaround }

class procedure TSkTextLayout.TRSP38480Workaround.Apply(var AText: string);
const
  ZeroWidthChar = #8203;
begin
  if AText.EndsWith(#0) then
    AText := AText.Substring(0, AText.Length - 1) + ZeroWidthChar;
end;

{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{ TSkTextLayout.TGraphemesMap }

function TSkTextLayout.TGraphemesMap.CreateGraphemesMapping(
  const AText: string): TBytes;
var
  I: Integer;
  LCharIndex: Integer;
  LGrapheme: TSkUnicodeBreakIteratorElem;
  LGraphemesIterator: ISkUnicodeBreakIterator;
  LUnicode: ISkUnicode;
begin
  // For each char of the text in the same position of this array will have the
  // index that it represents in the current grapheme, that is, index zero means
  // a beginning of the grapheme.
  SetLength(Result, Length(AText) + 1);
  if AText <> '' then
  begin
    LUnicode           := TSkUnicode.Create;
    LGraphemesIterator := LUnicode.GetBreakIterator(TSkBreakType.Graphemes, AText);
    LGraphemesIterator.MoveNext;
    I := 0;
    for LGrapheme in LGraphemesIterator do
    begin
      LCharIndex := 0;
      repeat
        Result[I] := LCharIndex;
        Inc(LCharIndex);
        Inc(I);
      until I = LGrapheme.Position;
    end;
  end;
  Result[High(Result)] := 0;
end;

procedure TSkTextLayout.TGraphemesMap.FindNextGraphemeBoundary(
  var AIndex: Integer);
begin
  if AIndex < Length(FCharIndexInGrapheme) then
  begin
    while FCharIndexInGrapheme[AIndex] <> 0 do
      Inc(AIndex);
  end;
end;

procedure TSkTextLayout.TGraphemesMap.FindPreviousGraphemeBoundary(
  var AIndex: Integer);
begin
  Dec(AIndex, FCharIndexInGrapheme[AIndex]);
end;

procedure TSkTextLayout.TGraphemesMap.SetText(const AText: string);
begin
  if (FText <> AText) or (FCharIndexInGrapheme = nil) then
  begin
    FText                := AText;
    FCharIndexInGrapheme := CreateGraphemesMapping(AText);
  end;
end;

{ TSkBitmapCodec }

class constructor TSkBitmapCodec.Create;
begin
  FFileExtensions := TStringList.Create;
  RegisterCodec('.bmp',  SVBitmaps,      False);
  RegisterCodec('.gif',  SVGIFImages,    False);
  RegisterCodec('.ico',  SVIcons,        False);
  RegisterCodec('.wbmp', SVWBMPImages,   False);
  RegisterCodec('.webp', SVWEBPImages,   True);
  {$IFNDEF MSWINDOWS}
  RegisterCodec('.arw',  SVRawSony,      False);
  RegisterCodec('.cr2',  SVRawCanon,     False);
  RegisterCodec('.dng',  SVRawDNG,       False);
  RegisterCodec('.nef',  SVRawNikon,     False);
  RegisterCodec('.nrw',  SVRawNikon,     False);
  RegisterCodec('.orf',  SVRawORF,       False);
  RegisterCodec('.raf',  SVRawRAF,       False);
  RegisterCodec('.rw2',  SVRawPanasonic, False);
  RegisterCodec('.pef',  SVRawPEF,       False);
  RegisterCodec('.srw',  SVRawSRW,       False);
  {$ENDIF}
end;

class destructor TSkBitmapCodec.Destroy;
var
  LFileExtension: string;
begin
  for LFileExtension in FFileExtensions do
    TBitmapCodecManager.UnregisterBitmapCodecClass(LFileExtension);
  FFileExtensions.Free;
end;

function TSkBitmapCodec.FitSize(const AWidth, AHeight: Integer; const AFitWidth,
  AFitHeight: Single): TSize;
var
  LRatio: Single;
begin
  if (AWidth / AFitWidth) > (AHeight / AFitHeight) then
    LRatio := AWidth / AFitWidth
  else
    LRatio := AHeight / AFitHeight;
  if LRatio < 1 then
    Result := TSize.Create(AWidth, AHeight)
  else
    Result := TSize.Create(Trunc((AWidth + Epsilon) / LRatio), Trunc((AHeight + Epsilon) / LRatio));
end;

class function TSkBitmapCodec.GetImageSize(const AFileName: string): TPointF;
var
  LCodec: ISkCodec;
begin
  LCodec := TSkCodec.MakeFromFile(AFileName);
  if LCodec = nil then
    Exit(TPointF.Create(0, 0));
  Result := TPointF.Create(LCodec.Width, LCodec.Height);
end;

class procedure TSkBitmapCodec.Initialize;
begin
  if not GlobalDisableSkiaCodecsReplacement then
  begin
    RegisterCodec('.jpg',  SVJPGImages, True, True);
    RegisterCodec('.jpeg', SVJPGImages, True, True);
    RegisterCodec('.png',  SVPNGImages, True, True);
  end;
end;

class function TSkBitmapCodec.IsValid(const AStream: TStream): Boolean;

  function IsValid(const AMemoryStream: TCustomMemoryStream): Boolean; inline;
  begin
    Result := TSkCodec.MakeWithoutCopy(AMemoryStream.Memory, AMemoryStream.Size) <> nil;
  end;

var
  LMemoryStream: TMemoryStream;
begin
  if AStream is TCustomMemoryStream then
    Result := IsValid(TCustomMemoryStream(AStream))
  else
  begin
    LMemoryStream := TMemoryStream.Create;
    try
      LMemoryStream.CopyFrom(AStream, 0);
      Result := IsValid(LMemoryStream);
    finally
      LMemoryStream.Free;
    end;
  end;
end;

function TSkBitmapCodec.LoadFromFile(const AFileName: string;
  const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal): Boolean;
var
  LCodec: ISkCodec;
  LImage: ISkImage;
  LSize: TSize;
begin
  LCodec := TSkCodec.MakeFromFile(AFileName);
  if LCodec = nil then
    Exit(False);
  if (AMaxSizeLimit > 0) and ((Cardinal(LCodec.Width) > AMaxSizeLimit) or (Cardinal(LCodec.Height) > AMaxSizeLimit)) then
  begin
    LSize := FitSize(LCodec.Width, LCodec.Height, AMaxSizeLimit, AMaxSizeLimit);
    ABitmapSurface.SetSize(LSize.Width, LSize.Height, GetPixelFormat);
    LImage := LCodec.GetImage(SkFmxColorType[GetPixelFormat]);
    Result := (LImage <> nil) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[GetPixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
  end
  else
  begin
    ABitmapSurface.SetSize(LCodec.Width, LCodec.Height, GetPixelFormat);
    Result := LCodec.GetPixels(ABitmapSurface.Bits, ABitmapSurface.Pitch, SkFmxColorType[GetPixelFormat]);
  end;
end;

function TSkBitmapCodec.LoadFromStream(const AStream: TStream;
  const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal): Boolean;

  function LoadFromStream(const AMemoryStream: TCustomMemoryStream): Boolean;
  var
    LCodec: ISkCodec;
    LImage: ISkImage;
    LSize: TSize;
  begin
    LCodec := TSkCodec.MakeWithoutCopy(AMemoryStream.Memory, AMemoryStream.Size);
    if LCodec = nil then
      Exit(False);
    if (AMaxSizeLimit > 0) and ((Cardinal(LCodec.Width) > AMaxSizeLimit) or (Cardinal(LCodec.Height) > AMaxSizeLimit)) then
    begin
      LSize := FitSize(LCodec.Width, LCodec.Height, AMaxSizeLimit, AMaxSizeLimit);
      ABitmapSurface.SetSize(LSize.Width, LSize.Height, GetPixelFormat);
      LImage := LCodec.GetImage(SkFmxColorType[GetPixelFormat]);
      Result := (LImage <> nil) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[GetPixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
    end
    else
    begin
      ABitmapSurface.SetSize(LCodec.Width, LCodec.Height, GetPixelFormat);
      Result := LCodec.GetPixels(ABitmapSurface.Bits, ABitmapSurface.Pitch, SkFmxColorType[GetPixelFormat]);
    end;
  end;

var
  LMemoryStream: TMemoryStream;
begin
  if AStream is TCustomMemoryStream then
    Result := LoadFromStream(TCustomMemoryStream(AStream))
  else
  begin
    LMemoryStream := TMemoryStream.Create;
    try
      LMemoryStream.CopyFrom(AStream, 0);
      Result := LoadFromStream(LMemoryStream);
    finally
      LMemoryStream.Free;
    end;
  end;
end;

function TSkBitmapCodec.LoadThumbnailFromFile(const AFileName: string;
  const AFitWidth, AFitHeight: Single; const AUseEmbedded: Boolean;
  const ABitmapSurface: TBitmapSurface): Boolean;
var
  LCodec: ISkCodec;
  LImage: ISkImage;
  LSize: TSize;
begin
  LCodec := TSkCodec.MakeFromFile(AFileName);
  if LCodec = nil then
    Exit(False);
  LSize := FitSize(LCodec.Width, LCodec.Height, AFitWidth, AFitHeight);
  ABitmapSurface.SetSize(LSize.Width, LSize.Height, GetPixelFormat);
  LImage := LCodec.GetImage(SkFmxColorType[GetPixelFormat]);
  Result := (LImage <> nil) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[GetPixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
end;

class procedure TSkBitmapCodec.RegisterCodec(const AFileExtension,
  ADescription: string; const ACanSave, AOverride: Boolean);
begin
  if (not TBitmapCodecManager.CodecExists(AFileExtension)) or (AOverride) then
  begin
    if AOverride then
      TBitmapCodecManager.UnregisterBitmapCodecClass(AFileExtension);
    if not TBitmapCodecManager.CodecExists(AFileExtension) then
    begin
      TBitmapCodecManager.RegisterBitmapCodecClass(AFileExtension, ADescription, ACanSave, TSkBitmapCodec);
      FFileExtensions.Add(AFileExtension);
    end;
  end;
end;

function TSkBitmapCodec.SaveToFile(const AFileName: string;
  const ABitmapSurface: TBitmapSurface;
  const ASaveParams: PBitmapCodecSaveParams): Boolean;
var
  LQuality: Integer;
begin
  if ASaveParams <> nil then
    LQuality := ASaveParams.Quality
  else
    LQuality := 100;
  TSkImageEncoder.EncodeToFile(AFileName, TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[ABitmapSurface.PixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, LQuality);
  Result := True;
end;

function TSkBitmapCodec.SaveToStream(const AStream: TStream;
  const ABitmapSurface: TBitmapSurface; const AExtension: string;
  const ASaveParams: PBitmapCodecSaveParams): Boolean;
var
  LQuality: Integer;
begin
  if ASaveParams <> nil then
    LQuality := ASaveParams.Quality
  else
    LQuality := 100;
  TSkImageEncoder.EncodeToStream(AStream, TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[ABitmapSurface.PixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, ExtensionToEncodedImageFormat(AExtension), LQuality);
  Result := True;
end;

{$IFDEF SKIA_RASTER}

{ TSkRasterCanvas }

destructor TSkRasterCanvas.Destroy;
begin
  if Parent <> nil then
  begin
    ReleaseBitmap;
    {$IF DEFINED(MACOS) and NOT DEFINED(IOS)}
    if FColorSpace <> nil then
      CGColorSpaceRelease(FColorSpace);
    {$ENDIF}
  end;
  inherited;
end;

function TSkRasterCanvas.GetSurfaceFromWindow(
  const AContextHandle: THandle): TSkSurface;
var
  {$IF DEFINED(LINUX)}
  LData: Pointer;
  {$ELSEIF DEFINED(MSWINDOWS)}
  LBitmapInfo: TBitmapInfo;
  LBits: Pointer;
  LDIBSection: TDIBSection;
  {$ENDIF}
  LHeight: Integer;
  LWidth: Integer;
begin
  if FBackBufferSurface = nil then
  begin
    {$IF DEFINED(LINUX)}
    if FBitmap = nil then
    begin
      LWidth  := Round(Width  * Scale);
      LHeight := Round(Height * Scale);
      GetMem(FBitmapBits, LWidth * LHeight * 4);
      FBitmap := FmuxBitmapCreate(LWidth, LHeight, FBitmapBits);
    end;
    FmuxGetBitmapInfo(FBitmap, LWidth, LHeight, LData);
    FBackBufferSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(LWidth, LHeight, TSkColorType.RGBA8888), LData, LWidth * 4);
    {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
    if FBitmap = nil then
    begin
      if FColorSpace = nil then
      begin
        FColorSpace := CGColorSpaceCreateDeviceRGB;
        if FColorSpace = nil then
          Exit(nil);
      end;
      LWidth  := Round(Width  * Scale);
      LHeight := Round(Height * Scale);
      FBitmap := CGBitmapContextCreate(nil, LWidth, LHeight, 8, LWidth * SkBytesPerPixel[TSkColorType.RGBA8888], FColorSpace, kCGImageAlphaPremultipliedLast);
      if FBitmap = nil then
        Exit(nil);
    end;
    FBackBufferSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(CGBitmapContextGetWidth(FBitmap), CGBitmapContextGetHeight(FBitmap), TSkColorType.RGBA8888), CGBitmapContextGetData(FBitmap), CGBitmapContextGetBytesPerRow(FBitmap));
    {$ELSEIF DEFINED(MSWINDOWS)}
    if FBitmap = 0 then
    begin
      LWidth  := Round(Width  * Scale);
      LHeight := Round(Height * Scale);
      FillChar(LBitmapInfo, SizeOf(TBitmapInfo), 0);
      LBitmapInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
      LBitmapInfo.bmiHeader.biWidth       := LWidth;
      LBitmapInfo.bmiHeader.biHeight      := -LHeight;
      LBitmapInfo.bmiHeader.biPlanes      := 1;
      LBitmapInfo.bmiHeader.biBitCount    := 32;
      LBitmapInfo.bmiHeader.biCompression := BI_RGB;
      LBitmapInfo.bmiHeader.biSizeImage   := LWidth * LHeight * SkBytesPerPixel[TSkColorType.BGRA8888];
      FBitmap := CreateDIBSection(0, LBitmapInfo, DIB_RGB_COLORS, LBits, 0, 0);
      if FBitmap = 0 then
        Exit(nil);
    end;
    if GetObject(FBitmap, SizeOf(TDIBSection), @LDIBSection) = 0 then
      Exit(nil);
    FBackBufferSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(LDIBSection.dsBm.bmWidth, LDIBSection.dsBm.bmHeight, TSkColorType.BGRA8888), LDIBSection.dsBm.bmBits, LDIBSection.dsBm.bmWidthBytes);
    {$ENDIF}
  end;
  Result := TSkSurface(FBackBufferSurface);
end;

procedure TSkRasterCanvas.ReleaseBitmap;
begin
  FBackBufferSurface := nil;
  {$IF DEFINED(LINUX)}
  if FBitmap <> nil then
  begin
    FmuxBitmapDestroy(FBitmap);
    FreeMem(FBitmapBits);
    FBitmap := nil;
  end;
  {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
  if FBitmap <> nil then
  begin
    CGContextRelease(FBitmap);
    FBitmap := nil;
  end;
  {$ELSEIF DEFINED(MSWINDOWS)}
  if FBitmap <> 0 then
  begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  end;
  {$ENDIF}
end;

procedure TSkRasterCanvas.Resized;
begin
  inherited;
  if Parent <> nil then
    ReleaseBitmap;
end;

procedure TSkRasterCanvas.SwapBuffers(const AContextHandle: THandle);
{$IF DEFINED(MACOS) and NOT DEFINED(IOS)}
var
  LImage: CGImageRef;
{$ELSEIF DEFINED(MSWINDOWS)}
var
  LDC: HDC;
  LOldObj: HGDIOBJ;
{$ENDIF}
begin
  inherited;
  if AContextHandle <> 0 then
  begin
    {$IF DEFINED(LINUX)}
    FmuxCanvasDrawBitmap(Pointer(AContextHandle), FBitmap, TRectF.Create(0, 0, Round(Width * Scale), Round(Height * Scale)), TRectF.Create(0, 0, Round(Width * Scale), Round(Height * Scale)), 1, True);
    {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
    LImage := CGBitmapContextCreateImage(FBitmap);
    if LImage <> nil then
    begin
      try
        CGContextDrawImage(CGContextRef(AContextHandle), CGRectMake(0, 0, Width, Height), LImage);
      finally
        CGImageRelease(LImage);
      end;
    end;
    {$ELSEIF DEFINED(MSWINDOWS)}
    LDC := CreateCompatibleDC(0);
    if LDC <> 0 then
    begin
      try
        LOldObj := SelectObject(LDC, FBitmap);
        try
          BitBlt(HDC(AContextHandle), 0, 0, Round(Width * Scale), Round(Height * Scale), LDC, 0, 0, SRCCOPY);
        finally
          if LOldObj <> 0 then
            SelectObject(LDC, LOldObj);
        end;
      finally
        DeleteDC(LDC);
      end;
    end;
    {$ENDIF}
  end;
end;

{$ENDIF}

{$HPPEMIT NOUSINGNAMESPACE}
{$HPPEMIT END '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_SKIA_CANVAS)'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::_di_IGrCanvasWrapper;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::_di_IGrSharedContext;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::_di_ISkCanvasWrapper;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::EGrCanvas;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::ESkCanvas;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::IGrCanvasWrapper;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::IGrSharedContext;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::ISkCanvasWrapper;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TGrBeforeSharedContextDestructionMessage;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TGrBitmapHandle;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TGrCanvas;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TGrCanvasClass;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TGrSharedContext;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkBitmapHandle;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkCanvasBase;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkCanvasBaseClass;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkCanvasCustom;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkCanvasCustomClass;'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::TSkTextLayout;'}
{$HPPEMIT END '    typedef TSkCanvasBaseClass (__fastcall *TDefaultSkiaRenderCanvasClassFunc)(void);'}
{$HPPEMIT END '    typedef void (__fastcall *TRegisterSkiaRenderCanvasProc)(const TSkCanvasBaseClass ACanvasClass, const bool APriority, '}
{$HPPEMIT END '        const ::System::DelphiInterface<System::Sysutils::TFunc__1<bool> > AIsSupportedFunc = (::System::DelphiInterface<System::Sysutils::TFunc__1<bool> >)(0x0));'}
{$HPPEMIT END '    static ::System::StaticArray<int, 3>& CanvasQualitySampleCount = ::Fmx::Skia::Canvas::CanvasQualitySampleCount;'}
{$HPPEMIT END '    static const TDefaultSkiaRenderCanvasClassFunc DefaultSkiaRenderCanvasClass = ::Fmx::Skia::Canvas::DefaultSkiaRenderCanvasClass;'}
{$HPPEMIT END '    static const TRegisterSkiaRenderCanvasProc RegisterSkiaRenderCanvas = ::Fmx::Skia::Canvas::RegisterSkiaRenderCanvas;'}
{$HPPEMIT END '#endif'}

{$REGION '- Canvas Registration'}

type
  { TSkRenderCanvas }

  TSkRenderCanvas = record
    CanvasClass: TSkCanvasBaseClass;
    IsSupportedFunc: TFunc<Boolean>;
  end;

  { TSkCanvasService }

  TSkCanvasService = class(TInterfacedObject, IFMXCanvasService)
  strict private
    FCurrent: IFMXCanvasService;
    {$IFDEF DEBUG}
    FFormBeforeShownMessageId: Integer;
    FGlobalUseSkiaInRegistration: Boolean;
    procedure FormBeforeShownHandler(const ASender: TObject; const AMessage: System.Messaging.TMessage);
    {$ENDIF}
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
  public
    constructor Create(const ACurrent: IFMXCanvasService);
    {$IFDEF DEBUG}
    destructor Destroy; override;
    {$ENDIF}
  end;

var
  CanvasClass: TSkCanvasBaseClass;
  RenderCanvas: TArray<TSkRenderCanvas>;

function DefaultSkiaRenderCanvasClass: TSkCanvasBaseClass;
begin
  Result := AtomicCmpExchange(PPointer(@CanvasClass)^, nil, nil);
end;

procedure RegisterSkiaRenderCanvas(const ACanvasClass: TSkCanvasBaseClass;
  const APriority: Boolean; const AIsSupportedFunc: TFunc<Boolean>);
var
  LRenderCanvas: TSkRenderCanvas;
begin
  LRenderCanvas.CanvasClass     := ACanvasClass;
  LRenderCanvas.IsSupportedFunc := AIsSupportedFunc;
  if APriority then
    RenderCanvas := [LRenderCanvas] + RenderCanvas
  else
    RenderCanvas := RenderCanvas + [LRenderCanvas];
end;

{ TSkCanvasService }

constructor TSkCanvasService.Create(const ACurrent: IFMXCanvasService);
begin
  inherited Create;
  FCurrent := ACurrent;
  {$IFDEF DEBUG}
  FFormBeforeShownMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormBeforeShownMessage, FormBeforeShownHandler);
  {$ENDIF}
end;

{$IFDEF DEBUG}

destructor TSkCanvasService.Destroy;
begin
  if FFormBeforeShownMessageId > 0 then
    TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, FFormBeforeShownMessageId);
  inherited;
end;

procedure TSkCanvasService.FormBeforeShownHandler(const ASender: TObject;
  const AMessage: System.Messaging.TMessage);
const
  MessageText = 'Your declaration of GlobalUseSkia has no effect because the ' +
                'canvas service has already been started. In this case, just ' +
                'create a unit in the project like "Project.Startup.pas", '    +
                'place the GlobalUseSkia declaration in the initialization of' +
                ' this new unit, and declare this new unit before any other '  +
                'unit of yours in the .dpr, that is, right after FMX.Forms.';
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, FFormBeforeShownMessageId);
  FFormBeforeShownMessageId := 0;
  if FGlobalUseSkiaInRegistration <> GlobalUseSkia then
    raise ESkCanvas.Create(MessageText);
end;

{$ENDIF}

procedure TSkCanvasService.RegisterCanvasClasses;
var
  LCanvasClass: TSkCanvasBaseClass;
  LRenderCanvas: TSkRenderCanvas;
begin
  TSkBitmapCodec.Initialize;
  TSkTextLayout.Initialize;
  if FCurrent <> nil then
  begin
    if GlobalUseSkia then
    begin
      {$IFDEF SKIA_RASTER}
      RegisterSkiaRenderCanvas(TSkRasterCanvas, {$IF DEFINED(MSWINDOWS)}GlobalUseSkiaRasterWhenAvailable{$ELSE}False{$ENDIF});
      {$ENDIF}
      for LRenderCanvas in RenderCanvas do
      begin
        if (not Assigned(LRenderCanvas.IsSupportedFunc)) or LRenderCanvas.IsSupportedFunc() then
        begin
          LCanvasClass := LRenderCanvas.CanvasClass;
          Break;
        end;
      end;
      if LCanvasClass <> nil then
      begin
        AtomicExchange(PPointer(@CanvasClass)^, PPointer(@LCanvasClass)^);

        // Ensuring that our canvas will be chosen as the default
        TCanvasManager.EnableSoftwareCanvas(True);

        TCanvasManager.RegisterCanvas(LCanvasClass, True, {$IFDEF SKIA_PRINTER}True{$ELSE}False{$ENDIF});
        TTextLayoutManager.RegisterTextLayout(TSkTextLayout, LCanvasClass);
        {$IFDEF SKIA_FILTER}
        {$IFDEF SKIA_RASTER}
        if not LCanvasClass.InheritsFrom(TSkRasterCanvas) then
        {$ENDIF}
          RegisterSkiaFilterContextForCanvas(LCanvasClass);
        {$ENDIF}
        {$IFDEF SKIA_PRINTER}
        SetSkiaPrinterClass;
        {$ENDIF}
        {$IF DECLARED(TRSP36957Workaround)}
        TRSP36957Workaround.Apply;
        {$ENDIF}
        {$IF DECLARED(TRSP37147Workaround)}
        TRSP37147Workaround.Apply;
        {$ENDIF}
        {$IF DECLARED(TRSP37660Workaround)}
        TRSP37660Workaround.Apply;
        {$ENDIF}
        {$IF DECLARED(TRSP37829Workaround)}
        TRSP37829Workaround.Apply;
        {$ENDIF}
      end;
    end;
    FCurrent.RegisterCanvasClasses;
  end;
  {$IFDEF DEBUG}
  FGlobalUseSkiaInRegistration := GlobalUseSkia;
  {$ENDIF}
end;

procedure TSkCanvasService.UnregisterCanvasClasses;
begin
  if FCurrent <> nil then
    FCurrent.UnregisterCanvasClasses;
end;

var
  CanvasService: TSkCanvasService;
initialization
  CanvasService := TSkCanvasService.Create(IFMXCanvasService(TPlatformServices.Current.GetPlatformService(IFMXCanvasService)));
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, CanvasService);
{$ENDREGION}
end.
