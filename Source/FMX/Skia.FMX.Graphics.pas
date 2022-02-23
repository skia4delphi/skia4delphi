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
unit Skia.FMX.Graphics;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,
  System.Classes,
  System.Generics.Collections,
  System.Math.Vectors,
  {$IF CompilerVersion >= 35}
  System.SyncObjs,
  {$ENDIF}
  System.SysUtils,
  System.Types,
  System.UITypes,

  { Skia }
  Skia;

type
  ESkCanvas = class(Exception);

  EGrCanvas = class(ESkCanvas);

  { TSkBitmap }

  TSkBitmap = class
  strict private
    FHeight: Integer;
    FPixels: Pointer;
    FWidth: Integer;
  public
    constructor Create(const AWidth, AHeight: Integer);
    destructor Destroy; override;
    property Height: Integer read FHeight;
    property Pixels: Pointer read FPixels;
    property Width: Integer read FWidth;
  end;

  TSkCanvasClass = class of TSkCanvasCustom;

  { TSkCanvasCustom }

  TSkCanvasCustom = class abstract(TCanvas)
  strict private type
    TSaveState = class(TCanvasSaveState)
    strict protected
      procedure AssignTo(ADest: TPersistent); override;
    public
      procedure Assign(ASource: TPersistent); override;
    end;

  strict private
    FDrawableHeight: Integer;
    FDrawableWidth: Integer;
    function GetSamplingOptions(const AHighSpeed: Boolean = False): TSkSamplingOptions;
    procedure SetupBrush(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single; const APaint: ISkPaint);
  protected
    class procedure Finalize; virtual;
    class procedure Initialize; virtual;
  strict protected
    FSurface: ISkSurface;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    {$IFDEF MSWINDOWS}
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    {$ENDIF}
    function BitmapToSkImage(const ABitmap: TBitmap): ISkImage;
    function BrushToSkPaint(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single): ISkPaint;
    function CreateImage(const ABitmapHandle: THandle; const AColorType: TSkColorType): ISkImage; virtual;
    function CreateSaveState: TCanvasSaveState; override;
    function DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override; final;
    function DoBeginWindow(const AContextHandle: THandle): Boolean; virtual; abstract;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawLine(const APoint1, APoint2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoEndScene; override; final;
    procedure DoEndWindow; virtual;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    {$IF CompilerVersion >= 30}
    procedure DoSetMatrix(const AMatrix: TMatrix); override;
    {$ENDIF}
    function GetCachedImage(const ABitmapHandle: THandle): ISkImage; virtual;
    procedure Resized; virtual;
    procedure Restore; virtual;
    procedure Save; virtual;
    function StrokeBrushToSkPaint(const ABrush: TStrokeBrush; const ARect: TRectF; const AOpacity: Single): ISkPaint;
    class procedure ClearCache(const ABitmapHandle: THandle); virtual;
    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
    class function DoInitializeBitmap(const AWidth, AHeight: Integer; const AScale: Single; var APixelFormat: TPixelFormat): THandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const ABitmapHandle: THandle; var ABitmapData: TBitmapData); override;
  public
    procedure Clear(const AColor: TAlphaColor); override;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    procedure ExcludeClipRect(const ARect: TRectF); override;
    procedure IntersectClipRect(const ARect: TRectF); override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    {$IF CompilerVersion < 30}
    procedure SetMatrix(const AMatrix: TMatrix); override;
    {$ENDIF}
    procedure SetSize(const AWidth, AHeight: Integer); override; final;
    property DrawableHeight: Integer read FDrawableHeight;
    property DrawableWidth: Integer read FDrawableWidth;

    // You can access this property between calls to the BeginScene and EndScene
    // procedures.
    //
    // Notes: 1. If your application uses TBitmaps on multiple threads, and is
    //   using Skia based Canvas, you can remove the Global Lock of TCanvas.
    //   Doing this will improve performance considerably; 2. Skia Canvas can be
    //   created from windows (eg Forms, descendants of TControl) or Bitmaps
    //   (TBitmap), and drawings created from a window must be done from the
    //   main thread.
    property Surface: ISkSurface read FSurface;

    class function ColorType: TSkColorType; virtual;
    class function GetCanvasStyle: TCanvasStyles; override;
  end;

  { TSkCanvasRasterCustom }

  TSkCanvasRasterCustom = class abstract(TSkCanvasCustom)
  strict private
    FBufferHandle: THandle;
  strict protected
    function CreateBuffer: THandle; virtual; abstract;
    function CreateWindowSurface(const AContextHandle, ABufferHandle: THandle): ISkSurface; virtual; abstract;
    function DoBeginWindow(const AContextHandle: THandle): Boolean; override; final;
    procedure DoEndWindow; override; final;
    procedure FreeBuffer(const ABufferHandle: THandle); virtual; abstract;
    procedure Resized; override;
    procedure SwapBuffers(const ABufferHandle: THandle); virtual; abstract;
  public
    destructor Destroy; override;
  end;

  { TGrCanvasCustom }

  TGrCanvasCustom = class abstract(TSkCanvasCustom)
  strict private type
    // The Skia architecture works differently from the TCanvas architecture,
    // some adaptations were necessary, but they were done in the best possible
    // way.
    //
    // Images (SkImage) cannot be shared between contexts (GrDirectContext),
    // even though they are created from contexts that belong to the same
    // sharing group, at the same time they must be destroyed before the context
    // (GrDirectContext) and we need to keep it already on the GPU for
    // performance reasons, so we created TImageCache to solve this issue.
    TImageCache = class sealed
    strict private class var
      FCache: TObjectDictionary<TGrCanvasCustom, TDictionary<THandle, ISkImage>>;
      {$IF CompilerVersion < 35}
      FCacheLock: TObject;
      {$ELSE}
      FCacheLock: TLightweightMREW;
      {$ENDIF}
    public
      class procedure Add(const ACanvas: TGrCanvasCustom; const ABitmapHandle: THandle; const AImage: ISkImage);
      class procedure Clear(const ACanvas: TGrCanvasCustom);
      class procedure Finalize;
      class function Get(const ACanvas: TGrCanvasCustom; const ABitmapHandle: THandle): ISkImage;
      class procedure Initialize;
      class procedure Remove(const ABitmapHandle: THandle);
    end;

  strict private
    FContext: IGrDirectContext;
    FContextCount: Integer;
    FContextLock: TObject;
  strict protected
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    procedure AttachToWindow; virtual;
    function CreateContext: IGrDirectContext; virtual; abstract;
    function CreateImage(const ABitmapHandle: THandle; const AColorType: TSkColorType): ISkImage; override;
    procedure DetachFromWindow; virtual;
    function DoBeginWindow(const AContextHandle: THandle): Boolean; override; final;
    procedure DoEndWindow; override; final;
    procedure Flush; virtual; abstract;
    function GetCachedImage(const ABitmapHandle: THandle): ISkImage; override;
    function GetRenderTarget: IGrBackendRenderTarget; virtual; abstract;
    procedure Prepare; virtual;
    procedure Restore; override;
    class procedure ClearCache(const ABitmapHandle: THandle); override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  public
    destructor Destroy; override;
    function BeginContext: Boolean;
    procedure EndContext;

    // You can access this property between calls to the BeginContext and
    // EndContext procedures.
    //
    // Notes: 1. Skia Canvas can be created from windows (eg Forms, descendants
    //   of TControl) or Bitmaps (TBitmap), and only Canvas created from a
    //   window can use this property.
    property Context: IGrDirectContext read FContext;

    class function Origin: TGrSurfaceOrigin; virtual; abstract;
  end;

const
  SVWEBPImages  = 'WebP Images';
  SWBMPImages   = 'WBMP Images';
  SRawCanon     = 'Raw Canon';
  SRawDNG       = 'Raw Adobe DNG Digital Negative';
  SRawNikon     = 'Raw Nikon';
  SRawORF       = 'Raw Olympus ORF';
  SRawPanasonic = 'Raw Panasonic';
  SRawPEF       = 'Raw Pentax PEF';
  SRawRAF       = 'Raw Fujifilm RAF';
  SRawSony      = 'Raw Sony';
  SRawSRW       = 'Raw Samsung SRW';

implementation

uses
  { Delphi }
  FMX.Consts,
  FMX.Platform,
  {$IFDEF MSWINDOWS}
  FMX.Platform.Win,
  {$ENDIF}
  FMX.Surfaces,
  FMX.TextLayout,
  {$IF DEFINED(MACOS) and NOT DEFINED(IOS)}
  Macapi.CocoaTypes,
  Macapi.CoreGraphics,
  {$ENDIF}
  System.Generics.Defaults,
  System.IOUtils,
  System.Math,
  System.UIConsts,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}

  // These units are used for workarounds
  FMX.Forms,
  {$IFDEF MACOS}
  FMX.Context.Metal,
  Macapi.Metal,
  Macapi.MetalKit,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CocoaTypes,
  iOSapi.OpenGLES,
  FMX.Context.GLES,
  {$ELSEIF defined(ANDROID)}
  Androidapi.Gles2,
  FMX.Context.GLES,
  {$ENDIF}
  {$IF defined(ANDROID) or defined(MACOS)}
  FMX.Types3D,
  FMX.Utils,
  Posix.SysMman,
  Posix.Unistd,
  System.Rtti,
  {$ENDIF}

  { Skia }
  Skia.API,
  Skia.FMX,
  Skia.FMX.Canvas.GL,
  Skia.FMX.Canvas.Metal;

type
  {$IF defined(MSWINDOWS)}

  { TSkCanvasRasterWindows }

  TSkCanvasRasterWindows = class(TSkCanvasRasterCustom)
  strict private
    FDC: HDC;
  strict protected
    function CreateBuffer: THandle; override;
    function CreateWindowSurface(const AContextHandle, ABufferHandle: THandle): ISkSurface; override;
    procedure FreeBuffer(const ABufferHandle: THandle); override;
    procedure SwapBuffers(const ABufferHandle: THandle); override;
  end;

  {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}

  { TSkCanvasRasterMacOS }

  TSkCanvasRasterMacOS = class(TSkCanvasRasterCustom)
  strict private class var
    FColorSpace: CGColorSpaceRef;
  strict private
    FContext: CGContextRef;
  strict protected
    function CreateBuffer: THandle; override;
    function CreateWindowSurface(const AContextHandle, ABufferHandle: THandle): ISkSurface; override;
    procedure FreeBuffer(const ABufferHandle: THandle); override;
    procedure SwapBuffers(const ABufferHandle: THandle); override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

  {$ENDIF}

  { TSkTextLayout }

  TSkTextLayout = class(TTextLayout)
  strict private
    FColor: TAlphaColor;
    FIgnoreUpdates: Boolean;
    FOpacity: Single;
    FParagraph: ISkParagraph;
    FParagraphOffset: TPointF;
    FTextRect: TRectF;
    function NeedHorizontalAlignment: Boolean;
    procedure UpdateParagraph;
  strict protected
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
    procedure DoRenderLayout; override;
    function GetTextHeight: Single; override;
    function GetTextRect: TRectF; override;
    function GetTextWidth: Single; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    procedure ConvertToPath(const APath: TPathData); override;
  end;

  { TSkBitmapCodec }

  TSkBitmapCodec = class(TCustomBitmapCodec)
  strict private
    function FitSize(const AWidth, AHeight: Integer; const AFitWidth, AFitHeight: Single): TSize; inline;
    class constructor Create;
    class procedure RegisterIfNotExists(const AFileExtension, ADescription: string; const ACanSave: Boolean); inline;
  public
    function LoadFromFile(const AFileName: string; const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal = 0): Boolean; override;
    function LoadFromStream(const AStream: TStream; const ABitmapSurface: TBitmapSurface; const AMaxSizeLimit: Cardinal = 0): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single; const AUseEmbedded: Boolean; const ABitmapSurface: TBitmapSurface): Boolean; override;
    function SaveToFile(const AFileName: string; const ABitmapSurface: TBitmapSurface; const ASaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    function SaveToStream(const AStream: TStream; const ABitmapSurface: TBitmapSurface; const AExtension: string; const ASaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
  end;

  { TSkCanvasService }

  TSkCanvasService = class(TInterfacedObject, IFMXCanvasService)
  strict private
    FCanvasClass: TSkCanvasClass;
    FCurrent: IFMXCanvasService;
  strict private
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    class function GetCanvasClass: TSkCanvasClass; static; inline;
  public
    constructor Create(const ACurrent: IFMXCanvasService);
  end;

{$IF defined(MSWINDOWS)}

{ TSkCanvasRasterWindows }

function TSkCanvasRasterWindows.CreateBuffer: THandle;
var
  LBitmapInfo: TBitmapInfo;
  LBits: Pointer;
begin
  inherited Create;
  FillChar(LBitmapInfo, SizeOf(TBitmapInfo), 0);
  FillChar(LBitmapInfo, 0, SizeOf(TBitmapInfo));
  LBitmapInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
  LBitmapInfo.bmiHeader.biWidth       := DrawableWidth;
  LBitmapInfo.bmiHeader.biHeight      := -DrawableHeight;
  LBitmapInfo.bmiHeader.biPlanes      := 1;
  LBitmapInfo.bmiHeader.biBitCount    := 32;
  LBitmapInfo.bmiHeader.biCompression := BI_RGB;
  LBitmapInfo.bmiHeader.biSizeImage   := DrawableWidth * DrawableHeight * 4;
  Result := THandle(CreateDIBSection(0, LBitmapInfo, DIB_RGB_COLORS, LBits, 0, 0));
end;

function TSkCanvasRasterWindows.CreateWindowSurface(
  const AContextHandle, ABufferHandle: THandle): ISkSurface;
var
  LBitmap: TBitmap;
begin
  FDC := HDC(AContextHandle);
  GetObject(HBITMAP(ABufferHandle), SizeOf(TBitmap), @LBitmap);
  Result := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(LBitmap.bmWidth, LBitmap.bmHeight), LBitmap.bmBits, LBitmap.bmWidthBytes);
end;

procedure TSkCanvasRasterWindows.FreeBuffer(const ABufferHandle: THandle);
begin
  DeleteObject(HBITMAP(ABufferHandle));
end;

procedure TSkCanvasRasterWindows.SwapBuffers(const ABufferHandle: THandle);
var
  LBufferDC: HDC;
begin
  inherited;
  LBufferDC := CreateCompatibleDC(0);
  if LBufferDC = 0 then
    RaiseLastOSError;
  try
    if SelectObject(LBufferDC, HBITMAP(ABufferHandle)) = 0 then
      RaiseLastOSError;
    if not BitBlt(FDC, 0, 0, DrawableWidth, DrawableHeight, LBufferDC, 0, 0, SRCCOPY) then
      RaiseLastOSError;
  finally
    DeleteDC(LBufferDC);
  end;
end;

{$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}

{ TSkCanvasRasterMacOS }

function TSkCanvasRasterMacOS.CreateBuffer: THandle;
begin
  Result := THandle(CGBitmapContextCreate(nil, DrawableWidth, DrawableHeight, 8, DrawableWidth * 4, FColorSpace, kCGImageAlphaPremultipliedLast));
end;

function TSkCanvasRasterMacOS.CreateWindowSurface(const AContextHandle,
  ABufferHandle: THandle): ISkSurface;
begin
  FContext := CGContextRef(AContextHandle);
  Result   := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(CGBitmapContextGetWidth(CGContextRef(ABufferHandle)), CGBitmapContextGetHeight(CGContextRef(ABufferHandle)), TSkColorType.RGBA8888), CGBitmapContextGetData(CGContextRef(ABufferHandle)), CGBitmapContextGetBytesPerRow(CGContextRef(ABufferHandle)));
end;

class procedure TSkCanvasRasterMacOS.Finalize;
begin
  CGColorSpaceRelease(FColorSpace);
  inherited;
end;

procedure TSkCanvasRasterMacOS.FreeBuffer(const ABufferHandle: THandle);
begin
  CGContextRelease(CGContextRef(ABufferHandle));
end;

class procedure TSkCanvasRasterMacOS.Initialize;
begin
  inherited;
  FColorSpace := CGColorSpaceCreateDeviceRGB;
end;

procedure TSkCanvasRasterMacOS.SwapBuffers(const ABufferHandle: THandle);
var
  LImage: CGImageRef;
begin
  LImage := CGBitmapContextCreateImage(CGContextRef(ABufferHandle));
  try
    CGContextDrawImage(FContext, CGRectMake(0, 0, Width, Height), LImage);
  finally
    CGImageRelease(LImage);
  end;
end;

{$ENDIF}

{ TSkTextLayout }

procedure TSkTextLayout.ConvertToPath(const APath: TPathData);
begin
  if Assigned(FParagraph) then
  begin
    APath.FromSkPath(FParagraph.ToPath);
    APath.Translate(FParagraphOffset + TopLeft);
  end;
end;

constructor TSkTextLayout.Create(const ACanvas: TCanvas);
begin
  inherited;
  {$REGION ' - Workaround RSP-36975'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround intended to fix issues with controls that
  // -   create the TTextLayout but doesn't set the TTextLayout.RightToLeft,
  // -   like the TText control.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-36975
  // -
  // - -------------------------------------------------------------------------
  {$IF CompilerVersion > 35.0}
    {$MESSAGE WARN 'Check if the issue has been fixed'}
  {$ENDIF}
  // - -------------------------------------------------------------------------
  FIgnoreUpdates := True;
  try
    RightToLeft := Application.BiDiMode = TBiDiMode.bdRightToLeft;
  finally
    FIgnoreUpdates := False;
  end;
  // - -------------------------------------------------------------------------
  {$ENDREGION}
end;

procedure TSkTextLayout.DoDrawLayout(const ACanvas: TCanvas);
var
  LCanvas: ISkCanvas;
begin
  if Assigned(FParagraph) then
  begin
    if (FColor <> Color) or (FOpacity <> Opacity) then
      UpdateParagraph;
    LCanvas := TSkCanvasCustom(ACanvas).Surface.Canvas;
    LCanvas.Save;
    try
      LCanvas.ClipRect(TRectF.Create(TopLeft, MaxSize.X, MaxSize.Y));
      FParagraph.Paint(LCanvas, FParagraphOffset.X + TopLeft.X, FParagraphOffset.Y + TopLeft.Y);
    finally
      LCanvas.Restore;
    end;
  end;
end;

function TSkTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;
begin
  if not Assigned(FParagraph) then
    Exit(-1);
  Result := FParagraph.GetGlyphPositionAtCoordinate(APoint.X - TopLeft.X - FParagraphOffset.X,
    APoint.Y - TopLeft.Y - FParagraphOffset.Y).Position;
end;

function TSkTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;

  function GetRegionForRange(const APos, ALength: Integer): TRegion;
  var
    I: Integer;
    LTextBoxes: TArray<TSkTextBox>;
  begin
    LTextBoxes := FParagraph.GetRectsForRange(APos, APos + ALength, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
    SetLength(Result, Length(LTextBoxes));
    for I := 0 to Length(LTextBoxes) - 1 do
    begin
      Result[I] := LTextBoxes[I].Rect;
      Result[I].Offset(FParagraphOffset + TopLeft);
    end;
  end;

begin
  if not Assigned(FParagraph) then
    Exit(nil);
  if ARange.Length = 0 then
  begin
    if ARange.Pos = 0 then
    begin
      Result := GetRegionForRange(0, 1);
      if Length(Result) > 0 then
        Result := [RectF(Result[0].Left, Result[0].Top, Result[0].Left, Result[0].Bottom)];
    end
    else
    begin
      Result := GetRegionForRange(ARange.Pos - 1, 1);
      if Length(Result) > 0 then
        Result := [RectF(Result[0].Right, Result[0].Top, Result[0].Right, Result[0].Bottom)];
    end;
  end
  else
    Result := GetRegionForRange(ARange.Pos, ARange.Length);
end;

procedure TSkTextLayout.DoRenderLayout;
type
  THorizontalAlign = (Left, Center, Right);

const
  RealHorizontalTextAlign: array[TTextAlign, Boolean] of THorizontalAlign = ((THorizontalAlign.Center, THorizontalAlign.Center), (THorizontalAlign.Left,   THorizontalAlign.Right), (THorizontalAlign.Right,  THorizontalAlign.Left));

  function GetTectRect: TRectF;
  var
    LLineBounds: TRectF;
    LLineMetric: TSkMetrics;
  begin
    Result := TRectF.Empty;
    for LLineMetric in FParagraph.LineMetrics do
    begin
      LLineBounds := TRectF.Create(PointF(LLineMetric.Left, LLineMetric.Baseline + LLineMetric.Descent - LLineMetric.Height),
        LLineMetric.Width, LLineMetric.Height);
      if Result.IsEmpty then
        Result := LLineBounds
      else
        Result := Result + LLineBounds;
    end;
    if Result.IsEmpty then
      Result.Height := FParagraph.Height;
  end;

  function GetParagraphOffset(ATextRect: TRectF): TPointF;
  var
    LRealParagraphBounds: TRectF;
  begin
    ATextRect := RectF(0, 0, ATextRect.Width, ATextRect.Height);
    if not NeedHorizontalAlignment then
    begin
      case RealHorizontalTextAlign[HorizontalAlign, RightToLeft] of
        THorizontalAlign.Center : ATextRect.Offset((MaxSize.X - Padding.Left - Padding.Right - ATextRect.Width) / 2, 0);
        THorizontalAlign.Right  : ATextRect.Offset( MaxSize.X - Padding.Left - Padding.Right - ATextRect.Right, 0);
      end;
    end;

    LRealParagraphBounds := RectF(0, 0, ATextRect.Width, ATextRect.Height);
    case RealHorizontalTextAlign[HorizontalAlign, RightToLeft] of
      THorizontalAlign.Center : LRealParagraphBounds.Offset((MaxSize.X - Padding.Left - Padding.Right - LRealParagraphBounds.Width) / 2, 0);
      THorizontalAlign.Right  : LRealParagraphBounds.Offset( MaxSize.X - Padding.Left - Padding.Right - LRealParagraphBounds.Right, 0);
    end;
    case VerticalAlign of
      TTextAlign.Center   : LRealParagraphBounds.Offset(0, (MaxSize.Y - Padding.Top - Padding.Bottom - LRealParagraphBounds.Height) / 2);
      TTextAlign.Trailing : LRealParagraphBounds.Offset(0,  MaxSize.Y - Padding.Top - Padding.Bottom - LRealParagraphBounds.Bottom);
    end;
    LRealParagraphBounds.Offset(Padding.Rect.TopLeft);
    Result := LRealParagraphBounds.TopLeft - ATextRect.TopLeft;
  end;

begin
  if FIgnoreUpdates then
    Exit;
  UpdateParagraph;
  FTextRect := GetTectRect;
  FParagraphOffset := GetParagraphOffset(FTextRect);
  FTextRect.Offset(FParagraphOffset);
  case VerticalAlign of
    TTextAlign.Leading  : FTextRect.Bottom := Min(MaxSize.Y - Padding.Top - Padding.Bottom, FTextRect.Bottom);
    TTextAlign.Center   : FTextRect.Inflate(0, Min(((MaxSize.Y - Padding.Top - Padding.Bottom) - FTextRect.Height) / 2, 0));
    TTextAlign.Trailing : FTextRect.Top := Max(FTextRect.Bottom - (MaxSize.Y - Padding.Top - Padding.Bottom), FTextRect.Top);
  end;
  case RealHorizontalTextAlign[HorizontalAlign, RightToLeft] of
    THorizontalAlign.Left   : FTextRect.Right := Min(MaxSize.X - Padding.Left - Padding.Right, FTextRect.Right);
    THorizontalAlign.Center : FTextRect.Inflate(Min(((MaxSize.X - Padding.Left - Padding.Right) - FTextRect.Width) / 2, 0), 0);
    THorizontalAlign.Right  : FTextRect.Left := Max(FTextRect.Right - (MaxSize.X - Padding.Left - Padding.Right), FTextRect.Left);
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

// The SkParagraph normally doesn't cut a word in half, it cuts the whole word.
// In some scenarios it can cause some errors, for example in edits where it is
// mandatory to cut in the middle of the word. To fix this we will readjust the
// MaxWidth, align horizontally manually and cut with ClipRect.
function TSkTextLayout.NeedHorizontalAlignment: Boolean;
begin
  Result := (not WordWrap) and (Trimming = TTextTrimming.None);
end;

procedure TSkTextLayout.UpdateParagraph;
{$IF CompilerVersion >= 31}
const
  SkFontSlant  : array[TFontSlant] of TSkFontSlant = (TSkFontSlant.Upright, TSkFontSlant.Italic, TSkFontSlant.Oblique);
  SkFontWeight : array[TFontWeight] of Integer = (100, 200, 300, 350, 400, 500, 600, 700, 800, 900, 950);
  SkFontWidth  : array[TFontStretch] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9);
{$ENDIF}

  function GetFontFamilies(const AValue: string): TArray<string>; inline;
  begin
    Result := AValue.Split([', ', ','], TStringSplitOptions.ExcludeEmpty){$IFDEF MACOS} + ['Helvetica Neue']{$ELSEIF DEFINED(LINUX)} + ['Ubuntu']{$ENDIF};
  end;

  function GetNormalizedAttributes: TArray<TTextAttributedRange>;
  var
    I: Integer;
    LAttribute: TTextAttributedRange;
    LAttributes: TList<TTextAttributedRange>;
    LComparer: IComparer<TTextAttributedRange>;
    LIndex: Integer;
    LNeighborAttribute: TTextAttributedRange;
  begin
    if AttributesCount = 0 then
      Exit(nil);
    LComparer := TComparer<TTextAttributedRange>.Construct(
      function(const ALeft, ARight: TTextAttributedRange): Integer
      begin
        if (ALeft.Range.Pos + ALeft.Range.Length) <= ARight.Range.Pos then
          Result := -1
        else if ALeft.Range.Pos >= (ARight.Range.Pos + ARight.Range.Length) then
          Result := 1
        else
          Result := 0;
      end);
    LAttributes := TList<TTextAttributedRange>.Create;
    try
      for I := 0 to AttributesCount - 1 do
      begin
        LAttribute := TTextAttributedRange.Create(Attributes[I].Range, Attributes[I].Attribute);
        if LAttribute.Range.Pos < 0 then
          LAttribute.Range := TTextRange.Create(0, LAttribute.Range.Pos + LAttribute.Range.Length);
        if (LAttribute.Range.Pos + LAttribute.Range.Length) > Text.Length then
          LAttribute.Range := TTextRange.Create(LAttribute.Range.Pos, LAttribute.Range.Length - ((LAttribute.Range.Pos + LAttribute.Range.Length) - Text.Length));
        if LAttribute.Range.Length <= 0 then
        begin
          LAttribute.Free;
          Continue;
        end;
        if LAttributes.BinarySearch(LAttribute, LIndex, LComparer) then
        begin
          if (LAttributes[LIndex].Range.Pos < LAttribute.Range.Pos) and ((LAttributes[LIndex].Range.Pos + LAttributes[LIndex].Range.Length) > (LAttribute.Range.Pos + LAttribute.Range.Length)) then
          begin
            LNeighborAttribute := TTextAttributedRange.Create(TTextRange.Create(LAttribute.Range.Pos + LAttribute.Range.Length, LAttributes[LIndex].Range.Pos + LAttributes[LIndex].Range.Length - (LAttribute.Range.Pos + LAttribute.Range.Length)), LAttributes[LIndex].Attribute);
            LAttributes.Insert(LIndex + 1, LNeighborAttribute);
            LAttributes.Insert(LIndex + 1, LAttribute);
            LNeighborAttribute := LAttributes[LIndex];
            LNeighborAttribute.Range.Length := LAttribute.Range.Pos - LNeighborAttribute.Range.Pos;
            Continue;
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

  function CreateTextStyle(const AAttribute: TTextAttribute): ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    Result.Color := MakeColor(AAttribute.Color, FOpacity);
    if AAttribute.Font <> nil then
    begin
      Result.FontFamilies := GetFontFamilies(AAttribute.Font.Family);
      Result.FontSize     := AAttribute.Font.Size;
      {$IF CompilerVersion < 31}
      if (TFontStyle.fsBold in AAttribute.Font.Style) and (TFontStyle.fsItalic in AAttribute.Font.Style) then
        Result.FontStyle := TSkFontStyle.BoldItalic
      else if TFontStyle.fsBold in AAttribute.Font.Style then
        Result.FontStyle := TSkFontStyle.Bold
      else if TFontStyle.fsItalic in AAttribute.Font.Style then
        Result.FontStyle := TSkFontStyle.Italic
      else
        Result.FontStyle := TSkFontStyle.Normal;
      {$ELSE}
      Result.FontStyle := TSkFontStyle.Create(SkFontWeight[AAttribute.Font.StyleExt.Weight], SkFontWidth[AAttribute.Font.StyleExt.Stretch], SkFontSlant[AAttribute.Font.StyleExt.Slant]);
      {$ENDIF}
      if (TFontStyle.fsUnderline in AAttribute.Font.Style) or (TFontStyle.fsStrikeOut in AAttribute.Font.Style) then
      begin
        Result.DecorationColor := MakeColor(AAttribute.Color, FOpacity);
        if TFontStyle.fsUnderline in AAttribute.Font.Style then
          Result.Decorations := Result.Decorations + [TSkTextDecoration.Underline];
        if TFontStyle.fsStrikeOut in AAttribute.Font.Style then
          Result.Decorations := Result.Decorations + [TSkTextDecoration.LineThrough];
      end;
    end;
  end;

  function CreateDefaultTextStyle: ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    Result.Color        := MakeColor(Color, FOpacity);
    Result.FontFamilies := GetFontFamilies(Font.Family);
    Result.FontSize     := Font.Size;
    {$IF CompilerVersion < 31}
    if (TFontStyle.fsBold in Font.Style) and (TFontStyle.fsItalic in Font.Style) then
      Result.FontStyle := TSkFontStyle.BoldItalic
    else if TFontStyle.fsBold in Font.Style then
      Result.FontStyle := TSkFontStyle.Bold
    else if TFontStyle.fsItalic in Font.Style then
      Result.FontStyle := TSkFontStyle.Italic
    else
      Result.FontStyle := TSkFontStyle.Normal;
    {$ELSE}
    Result.FontStyle := TSkFontStyle.Create(SkFontWeight[Font.StyleExt.Weight], SkFontWidth[Font.StyleExt.Stretch], SkFontSlant[Font.StyleExt.Slant]);
    {$ENDIF}
    if (TFontStyle.fsUnderline in Font.Style) or (TFontStyle.fsStrikeOut in Font.Style) then
    begin
      Result.DecorationColor := MakeColor(Color, FOpacity);
      if TFontStyle.fsUnderline in Font.Style then
        Result.Decorations := Result.Decorations + [TSkTextDecoration.Underline];
      if TFontStyle.fsStrikeOut in Font.Style then
        Result.Decorations := Result.Decorations + [TSkTextDecoration.LineThrough];
    end;
  end;

  function CreateParagraphStyle(const AAttributes: TArray<TTextAttributedRange>; AMaxLines: Integer): ISkParagraphStyle;
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
      Result.MaxLines := High(Integer)
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
    if WordWrap then
    begin
      if AMaxLines = 0 then
      begin
        LMinFontSize := Result.TextStyle.FontSize;
        for LAttribute in AAttributes do
          LMinFontSize := Min(LMinFontSize, LAttribute.Attribute.Font.Size);
        if LMinFontSize > 0.1 then
          AMaxLines := Ceil(MaxSize.Y / LMinFontSize);
      end;
      if AMaxLines > 0 then
        Result.MaxLines := AMaxLines
      else
        Result.MaxLines := High(Integer);
    end
    else
      Result.MaxLines := 1;
  end;

  procedure DoUpdateParagraph(const AMaxLines: Integer);
  var
    LAttribute: TTextAttributedRange;
    LAttributes: TArray<TTextAttributedRange>;
    LBuilder: ISkParagraphBuilder;
    LLastAttributeEndIndex: Integer;
  begin
    FColor   := Color;
    FOpacity := Opacity;
    LAttributes := GetNormalizedAttributes;
    try
      LBuilder := TSkParagraphBuilder.Create(CreateParagraphStyle(LAttributes, AMaxLines), TSkTypefaceManager.Provider);
      LLastAttributeEndIndex := 0;
      for LAttribute in LAttributes do
      begin
        if LLastAttributeEndIndex < LAttribute.Range.Pos then
          LBuilder.AddText(Text.Substring(LLastAttributeEndIndex, LAttribute.Range.Pos - LLastAttributeEndIndex));
        LBuilder.PushStyle(CreateTextStyle(LAttribute.Attribute));
        LBuilder.AddText(Text.Substring(LAttribute.Range.Pos, LAttribute.Range.Length));
        LBuilder.Pop;
        LLastAttributeEndIndex := LAttribute.Range.Pos + LAttribute.Range.Length;
      end;
      if LLastAttributeEndIndex < Text.Length then
        LBuilder.AddText(Text.Substring(LLastAttributeEndIndex, Text.Length - LLastAttributeEndIndex));
    finally
      for LAttribute in LAttributes do
        LAttribute.DisposeOf;
    end;
    FParagraph := LBuilder.Build;
  end;

var
  LLineMetric: TSkMetrics;
begin
  DoUpdateParagraph(0);
  if NeedHorizontalAlignment then
    FParagraph.Layout({$IF CompilerVersion < 29}ClosePolygon.X{$ELSE}TTextLayout.MaxLayoutSize.X{$ENDIF})
  else
    FParagraph.Layout(MaxSize.X - Padding.Left - Padding.Right);
  if WordWrap and (FParagraph.Height > MaxSize.Y - Padding.Top - Padding.Bottom) then
  begin
    for LLineMetric in FParagraph.LineMetrics do
    begin
      if (LLineMetric.LineNumber <> 0) and (LLineMetric.Baseline + LLineMetric.Descent > MaxSize.Y - Padding.Top - Padding.Bottom) then
      begin
        DoUpdateParagraph(LLineMetric.LineNumber);
        FParagraph.Layout(MaxSize.X - Padding.Left - Padding.Right);
        Break;
      end;
    end;
  end;
end;

{ TSkBitmapCodec }

class constructor TSkBitmapCodec.Create;
begin
  RegisterIfNotExists('.bmp', SVBitmaps, False);
  RegisterIfNotExists('.gif', SVGIFImages, False);
  RegisterIfNotExists('.ico', SVIcons, False);
  RegisterIfNotExists('.wbmp', SWBMPImages, False);
  RegisterIfNotExists('.webp', SVWEBPImages, True);
  RegisterIfNotExists('.arw', SRawSony, False);
  RegisterIfNotExists('.cr2', SRawCanon, False);
  RegisterIfNotExists('.dng', SRawDNG, False);
  RegisterIfNotExists('.nef', SRawNikon, False);
  RegisterIfNotExists('.nrw', SRawNikon, False);
  RegisterIfNotExists('.orf', SRawORF, False);
  RegisterIfNotExists('.raf', SRawRAF, False);
  RegisterIfNotExists('.rw2', SRawPanasonic, False);
  RegisterIfNotExists('.pef', SRawPEF, False);
  RegisterIfNotExists('.srw', SRawSRW, False);
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
  if not Assigned(LCodec) then
    Exit(TPointF.Create(0, 0));
  Result := TPointF.Create(LCodec.Width, LCodec.Height);
end;

class function TSkBitmapCodec.IsValid(const AStream: TStream): Boolean;

  function IsValid(const AMemoryStream: TCustomMemoryStream): Boolean; inline;
  begin
    Result := Assigned(TSkCodec.MakeWithoutCopy(AMemoryStream.Memory, AMemoryStream.Size));
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
  if not Assigned(LCodec) then
    Exit(False);
  if AMaxSizeLimit > 0 then
  begin
    LSize := FitSize(LCodec.Width, LCodec.Height, AMaxSizeLimit, AMaxSizeLimit);
    ABitmapSurface.SetSize(LSize.Width, LSize.Height, SkFmxPixelFormat[SkNative32ColorType]);
    LImage := LCodec.GetImage(SkNative32ColorType);
    Result := (Assigned(LImage)) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
  end
  else
  begin
    ABitmapSurface.SetSize(LCodec.Width, LCodec.Height, SkFmxPixelFormat[SkNative32ColorType]);
    Result := LCodec.GetPixels(ABitmapSurface.Bits, ABitmapSurface.Pitch, SkNative32ColorType);
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
    if not Assigned(LCodec) then
      Exit(False);
    if AMaxSizeLimit > 0 then
    begin
      LSize := FitSize(LCodec.Width, LCodec.Height, AMaxSizeLimit, AMaxSizeLimit);
      ABitmapSurface.SetSize(LSize.Width, LSize.Height, SkFmxPixelFormat[SkNative32ColorType]);
      LImage := LCodec.GetImage(SkNative32ColorType);
      Result := (Assigned(LImage)) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
    end
    else
    begin
      ABitmapSurface.SetSize(LCodec.Width, LCodec.Height, SkFmxPixelFormat[SkNative32ColorType]);
      Result := LCodec.GetPixels(ABitmapSurface.Bits, ABitmapSurface.Pitch, SkNative32ColorType);
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
  if not Assigned(LCodec) then
    Exit(False);
  LSize := FitSize(LCodec.Width, LCodec.Height, AFitWidth, AFitHeight);
  ABitmapSurface.SetSize(LSize.Width, LSize.Height, SkFmxPixelFormat[SkNative32ColorType]);
  LImage := LCodec.GetImage(SkNative32ColorType);
  Result := (Assigned(LImage)) and (LImage.ScalePixels(TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkImageCachingHint.Disallow));
end;

class procedure TSkBitmapCodec.RegisterIfNotExists(const AFileExtension,
  ADescription: string; const ACanSave: Boolean);
begin
  if not TBitmapCodecManager.CodecExists(AFileExtension) then
    TBitmapCodecManager.RegisterBitmapCodecClass(AFileExtension, ADescription, ACanSave, TSkBitmapCodec);
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
  TSkImageEncoder.EncodeToStream(AStream, TSkImageInfo.Create(ABitmapSurface.Width, ABitmapSurface.Height, SkFmxColorType[ABitmapSurface.PixelFormat]), ABitmapSurface.Bits, ABitmapSurface.Pitch, TSkEncodedImageFormat.FromExtension(AExtension), LQuality);
  Result := True;
end;

{$REGION ' - Workaround RSP-36957'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
// -
// - Description:
// -   This code is a workaround intended to fix a bug involving the
// -   TCustomContextOpenGL.DoCopyToBits.
// -
// - Note:
// -   To solve it without having to change the FMX source we had to use RTTI.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-36957
// -
// - ---------------------------------------------------------------------------
{$IF CompilerVersion > 35.0}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IF defined(ANDROID) or defined(IOS)}

type
  { TCustomContextOpenGLFix }

  TCustomContextOpenGLFix = class
  strict private
    type
      { TContext3D }

      {$RTTI EXPLICIT METHODS([vcProtected])}
      TContext3D = class(FMX.Types3D.TContext3D)
      protected
        procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
      end;

      { TCustomContextOpenGLAccess }

      TCustomContextOpenGLAccess = class(TCustomContextOpenGL);
  strict private
    class procedure DoCopyToBitsFixed(ASelf: TObject; const Bits: Pointer; const Pitch: Integer; const ARect: TRect); static;
  public
    class procedure TryApply(const AClass: TContextClass); static;
  end;

{ TCustomContextOpenGLFix.TContext3D }

procedure TCustomContextOpenGLFix.TContext3D.DoCopyToBits(const Bits: Pointer; const Pitch: Integer;
  const ARect: TRect);
begin
end;

{ TCustomContextOpenGLFix }

class procedure TCustomContextOpenGLFix.DoCopyToBitsFixed(ASelf: TObject; const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  I: Integer;
  LBuffer: PAlphaColorArray;
  LBufferLen: Integer;
  LCurrentFramebuffer: GLuint;
  LRect: TRect;
begin
  with TCustomContextOpenGLAccess(ASelf) do
  begin
    if Valid then
    begin
      LBufferLen := Width * Height * 4;
      GetMem(LBuffer, LBufferLen);
      try
        if FFrameBuf <> 0 then
        begin
          glGetIntegerv(GL_FRAMEBUFFER_BINDING, @LCurrentFramebuffer);
          glBindFramebuffer(GL_FRAMEBUFFER, FFrameBuf);
        end;
        glReadPixels(0, 0, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, LBuffer);
        LRect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));
        for I := LRect.Top to LRect.Bottom - 1 do
          Move(LBuffer[(Height - 1 - I) * Width + LRect.Left], PAlphaColorArray(Bits)[I * (Pitch div 4) + LRect.Left], LRect.Width * 4);
        if FFrameBuf <> 0 then
          glBindFramebuffer(GL_FRAMEBUFFER, LCurrentFramebuffer);
      finally
        FreeMem(LBuffer);
      end;
      {$IF CompilerVersion < 34}
      if GLHasAnyErrors then
        RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoCopyBits']);
      {$ELSE}
      TGlesDiagnostic.RaiseIfHasError(@SErrorInContextMethod, ['DoCopyBits']);
      {$ENDIF}
    end;
  end;
end;

class procedure TCustomContextOpenGLFix.TryApply(const AClass: TContextClass);

  function HookVMT(const AVMTEntry, AHookAddress: Pointer): Boolean;
  var
    LAlignedCodeAddress: UIntPtr;
    LPageSize: Integer;
  begin
    LPageSize := sysconf(_SC_PAGESIZE);
    LAlignedCodeAddress := UIntPtr(AVMTEntry) and (not (LPageSize - 1));
    Result := (mprotect(Pointer(LAlignedCodeAddress), LPageSize, PROT_READ or PROT_WRITE) = 0);
    if Result then
      PPointer(AVMTEntry)^ := AHookAddress;
  end;

  procedure DoApplyFix;
  var
    LRttiContext: TRttiContext;
    LRttiMethod: TRttiMethod;
  begin
    LRttiContext := TRttiContext.Create;
    try
      for LRttiMethod in LRttiContext.GetType(TContext3D).AsInstance.GetMethods do
      begin
        if SameText(LRttiMethod.Name, 'DoCopyToBits') then
        begin
          HookVMT(Pointer(PByte(AClass) + (LRttiMethod.VirtualIndex * SizeOf(Pointer))), @TCustomContextOpenGLFix.DoCopyToBitsFixed);
          Break;
        end;
      end;
    finally
      LRttiContext.Free;
    end;
  end;

begin
  if Assigned(AClass) and AClass.InheritsFrom(TCustomContextOpenGL) then
    DoApplyFix;
end;

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
// - Note:
// -   To solve it without having to change the FMX source, we had to use RTTI.
// -
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-37147
// -
// - ---------------------------------------------------------------------------
{$IF CompilerVersion > 35.0}
  {$MESSAGE WARN 'Check if the issue has been fixed and if not, check if the fields of the class FMX.Context.Metal.TContextMetal are the same'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IF defined(MACOS)}

type
  { TContextMetalFix }

  TContextMetalFix = class
  strict private
    type
      { TContext3D }

      // Used to capture the virtual index of the DoCopyToBits method via Rtti to apply the fix in runtime
      {$RTTI EXPLICIT METHODS([vcProtected])}
      TContext3D = class(FMX.Types3D.TContext3D)
      protected
        procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
      end;

      { TContextMetal }

      // This is a copy of TContextMetal private fields to give us access to it using hardcast. On each new
      // RAD Studio update check if the declaration is still identical in the FMX.Context.Metal unit.
      TContextMetal = class(TCustomContextMetal)
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
      end;
  strict private
    class procedure DoCopyToBitsFixed(ASelf: TObject; const Bits: Pointer; const Pitch: Integer; const ARect: TRect); static;
  public
    class procedure TryApply(const AClass: TContextClass); static;
  end;

{ TContextMetalFix.TContext3D }

procedure TContextMetalFix.TContext3D.DoCopyToBits(const Bits: Pointer; const Pitch: Integer;
  const ARect: TRect);
begin
end;

{ TContextMetalFix }

class procedure TContextMetalFix.DoCopyToBitsFixed(ASelf: TObject; const Bits: Pointer; const Pitch: Integer; const ARect: TRect);

  function CreateRegion(const ARect: TRect; const AScale: Single): MTLRegion;
  begin
    with TContextMetal(ASelf) do
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
  end;

  procedure SynchronizeResources(const ATexture: MTLTexture);
  var
    CommandBuffer: MTLCommandBuffer;
    LBlitCommandEncoder: MTLBlitCommandEncoder;
  begin
    with TContextMetal(ASelf) do
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
  end;

var
  LCopyRect: TRect;
  LTexture: MTLTexture;
  LRegion: MTLRegion;
begin
  with TContextMetal(ASelf) do
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
      LTexture.getBytesBytesPerRowFromRegionMipmapLevel(Bits, Pitch, LRegion, 0);
    end;
  end;
end;

class procedure TContextMetalFix.TryApply(const AClass: TContextClass);

  function HookVMT(const AVMTEntry, AHookAddress: Pointer): Boolean;
  var
    LAlignedCodeAddress: UIntPtr;
    LPageSize: Integer;
  begin
    LPageSize := sysconf(_SC_PAGESIZE);
    LAlignedCodeAddress := UIntPtr(AVMTEntry) and (not (LPageSize - 1));
    Result := (mprotect(Pointer(LAlignedCodeAddress), LPageSize, PROT_READ or PROT_WRITE) = 0);
    if Result then
      PPointer(AVMTEntry)^ := AHookAddress;
  end;

  procedure DoApplyFix;
  var
    LRttiContext: TRttiContext;
    LRttiMethod: TRttiMethod;
  begin
    LRttiContext := TRttiContext.Create;
    try
      for LRttiMethod in LRttiContext.GetType(TContext3D).AsInstance.GetMethods do
      begin
        if SameText(LRttiMethod.Name, 'DoCopyToBits') then
        begin
          HookVMT(Pointer(PByte(AClass) + (LRttiMethod.VirtualIndex * SizeOf(Pointer))), @TContextMetalFix.DoCopyToBitsFixed);
          Break;
        end;
      end;
    finally
      LRttiContext.Free;
    end;
  end;

begin
  if Assigned(AClass) and AClass.InheritsFrom(TContextMetal) then
    DoApplyFix;
end;

{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

{ TSkCanvasService }

constructor TSkCanvasService.Create(const ACurrent: IFMXCanvasService);
begin
  inherited Create;
  FCurrent := ACurrent;
end;

class function TSkCanvasService.GetCanvasClass: TSkCanvasClass;
begin
  {$IF defined(MSWINDOWS)}
  if GlobalUseSkiaRasterWhenAvailable then
    Result := TSkCanvasRasterWindows
  else
    Result := TGrCanvasGL;
  {$ELSEIF DEFINED(IOS)}
  if GlobalUseMetal then
    Result := TGrCanvasMetal
  else
    Result := TGrCanvasGL;
  {$ELSEIF DEFINED(MACOS)}
  if GlobalUseMetal then
    Result := TGrCanvasMetal
  else
    {$IFDEF IOS}
    Result := TGrCanvasGL;
    {$ELSE}
    Result := TSkCanvasRasterMacOS;
    {$ENDIF}
  {$ELSEIF DEFINED(ANDROID)}
  Result := TGrCanvasGL;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

procedure TSkCanvasService.RegisterCanvasClasses;
begin
  if Assigned(FCurrent) then
  begin
    if GlobalUseSkia then
    begin
      FCanvasClass := GetCanvasClass;
      if Assigned(FCanvasClass) then
      begin
        FCanvasClass.Initialize;
        // Ensuring that our canvas will be chosen as the default
        TCanvasManager.EnableSoftwareCanvas(True);
        TCanvasManager.RegisterCanvas(FCanvasClass, True, False);
        TTextLayoutManager.RegisterTextLayout(TSkTextLayout, FCanvasClass);
      end;
      if not GlobalDisableSkiaCodecsReplacement then
      begin
        TBitmapCodecManager.UnregisterBitmapCodecClass('.jpg');
        TBitmapCodecManager.RegisterBitmapCodecClass('.jpg', SVJPGImages, True, TSkBitmapCodec);
        TBitmapCodecManager.UnregisterBitmapCodecClass('.jpeg');
        TBitmapCodecManager.RegisterBitmapCodecClass('.jpeg', SVJPGImages, True, TSkBitmapCodec);
        TBitmapCodecManager.UnregisterBitmapCodecClass('.png');
        TBitmapCodecManager.RegisterBitmapCodecClass('.png', SVPNGImages, True, TSkBitmapCodec);
      end;
    end;
    FCurrent.RegisterCanvasClasses;

    // Apply workarounds
    {$IF defined(ANDROID) or defined(IOS)}
    if Assigned(FCanvasClass) then
      TCustomContextOpenGLFix.TryApply(TContextManager.DefaultContextClass);
    {$ENDIF}
    {$IFDEF MACOS}
    if Assigned(FCanvasClass) then
      TContextMetalFix.TryApply(TContextManager.DefaultContextClass);
    {$ENDIF}
  end;
end;

procedure TSkCanvasService.UnregisterCanvasClasses;
begin
  if Assigned(FCurrent) then
  begin
    if Assigned(FCanvasClass) then
      FCanvasClass.Finalize;
    FCurrent.UnregisterCanvasClasses;
  end;
end;

{ TSkBitmap }

constructor TSkBitmap.Create(const AWidth, AHeight: Integer);
begin
  inherited Create;
  FWidth  := AWidth;
  FHeight := AHeight;
  GetMem(FPixels, FWidth * FHeight * 4);
end;

destructor TSkBitmap.Destroy;
begin
  FreeMem(FPixels);
  inherited;
end;

{ TSkCanvasCustom }

function TSkCanvasCustom.BitmapToSkImage(
  const ABitmap: FMX.Graphics.TBitmap): ISkImage;
var
  LBitmapData: TBitmapData;
  LBitmapHandle: THandle;
begin
  if not ABitmap.HandleAllocated then
    Exit(nil);
  if ABitmap.CanvasClass.InheritsFrom(ClassType) then
  begin
    LBitmapHandle := ABitmap.Handle;
    if Parent <> nil then
    begin
      Result := GetCachedImage(LBitmapHandle);
      if Assigned(Result) then
        Exit;
      Result := CreateImage(LBitmapHandle, SkFmxColorType[ABitmap.PixelFormat]);
    end
    else
      Result := TSkImage.MakeFromRaster(TSkImageInfo.Create(TSkBitmap(LBitmapHandle).Width, TSkBitmap(LBitmapHandle).Height, SkFmxColorType[ABitmap.PixelFormat]), TSkBitmap(LBitmapHandle).Pixels, TSkBitmap(LBitmapHandle).Width * 4);
  end
  else
  begin
    if not ABitmap.Map(TMapAccess.Read, LBitmapData) then
      Exit(nil);
    try
      Result := TSkImage.MakeRasterCopy(TSkImageInfo.Create(LBitmapData.Width, LBitmapData.Height, SkFmxColorType[LBitmapData.PixelFormat]), LBitmapData.Data, LBitmapData.Pitch);
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  end;
end;

function TSkCanvasCustom.BrushToSkPaint(const ABrush: TBrush;
  const ARect: TRectF; const AOpacity: Single): ISkPaint;
begin
  Result := TSkPaint.Create(TSkPaintStyle.Fill);
  SetupBrush(ABrush, ARect, AOpacity, Result);
end;

procedure TSkCanvasCustom.Clear(const AColor: TAlphaColor);
begin
  FSurface.Canvas.Clear(AColor);
end;

class procedure TSkCanvasCustom.ClearCache(const ABitmapHandle: THandle);
begin
end;

procedure TSkCanvasCustom.ClearRect(const ARect: TRectF;
  const AColor: TAlphaColor);
begin
  FSurface.Canvas.Save;
  try
    FSurface.Canvas.ClipRect(ARect);
    FSurface.Canvas.Clear(AColor);
  finally
    FSurface.Canvas.Restore;
  end;
end;

class function TSkCanvasCustom.ColorType: TSkColorType;
begin
  Result := SkNative32ColorType;
end;

constructor TSkCanvasCustom.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  raise ESkCanvas.Create('Create from printer is not supported');
end;

{$IFDEF MSWINDOWS}

constructor TSkCanvasCustom.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  if WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency then
    WindowHandleToPlatform(Parent).CreateBuffer({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF});
end;

{$ENDIF}

function TSkCanvasCustom.CreateImage(const ABitmapHandle: THandle;
  const AColorType: TSkColorType): ISkImage;
begin
  Result := TSkImage.MakeFromRaster(TSkImageInfo.Create(TSkBitmap(ABitmapHandle).Width, TSkBitmap(ABitmapHandle).Height, AColorType), TSkBitmap(ABitmapHandle).Pixels, TSkBitmap(ABitmapHandle).Width * 4);
end;

function TSkCanvasCustom.CreateSaveState: TCanvasSaveState;
begin
  Result := TSaveState.Create;
end;

function TSkCanvasCustom.DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects;
  AContextHandle: THandle): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    if Bitmap <> nil then
    begin
      ClearCache(Bitmap.Handle);
      FDrawableWidth  := Width;
      FDrawableHeight := Height;
      FSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(TSkBitmap(Bitmap.Handle).Width, TSkBitmap(Bitmap.Handle).Height, SkFmxColorType[Bitmap.PixelFormat]), TSkBitmap(Bitmap.Handle).Pixels, TSkBitmap(Bitmap.Handle).Width * 4);
      Result   := Assigned(FSurface);
    end
    else if Parent <> nil then
    begin
      FDrawableWidth  := Round(Width  * Scale);
      FDrawableHeight := Round(Height * Scale);
      Result          := DoBeginWindow(AContextHandle);
    end
    else
      Exit(False);
    if Result then
      FSurface.Canvas.SetMatrix(TMatrix.CreateScaling(Scale, Scale));
  end;
end;

procedure TSkCanvasCustom.DoDrawBitmap(const ABitmap: FMX.Graphics.TBitmap;
  const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean);
var
  LImage: ISkImage;
  LPaint: ISkPaint;
begin
  LImage := BitmapToSkImage(ABitmap);
  if Assigned(LImage) then
  begin
    LPaint := TSkPaint.Create;
    LPaint.AlphaF := AOpacity;
    FSurface.Canvas.DrawImageRect(LImage, ASrcRect, ADestRect, GetSamplingOptions(AHighSpeed), LPaint);
  end;
end;

procedure TSkCanvasCustom.DoDrawEllipse(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddOval(ARect, TSkPathDirection.CW, 3);
  FSurface.Canvas.DrawPath(LPathBuilder.Detach, StrokeBrushToSkPaint(ABrush, ARect, AOpacity));
end;

procedure TSkCanvasCustom.DoDrawLine(const APoint1, APoint2: TPointF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FSurface.Canvas.DrawLine(APoint1, APoint2, StrokeBrushToSkPaint(ABrush, TRectF.Create(APoint1, APoint2), AOpacity));
end;

procedure TSkCanvasCustom.DoDrawPath(const APath: TPathData;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FSurface.Canvas.DrawPath(APath.ToSkPath, StrokeBrushToSkPaint(ABrush, APath.GetBounds, AOpacity));
end;

procedure TSkCanvasCustom.DoDrawRect(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FSurface.Canvas.DrawRect(ARect, StrokeBrushToSkPaint(ABrush, ARect, AOpacity));
end;

procedure TSkCanvasCustom.DoEndScene;
begin
  if Parent <> nil then
  begin
    {$IFDEF MSWINDOWS}
    if WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency then
      FSurface.ReadPixels(TSkImageInfo.Create({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF}), WindowHandleToPlatform(Parent).BufferBits, {$IF CompilerVersion < 31}Width{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width{$ENDIF} * 4);
    {$ENDIF}
    DoEndWindow;
  end;
  FSurface := nil;
  inherited;
end;

procedure TSkCanvasCustom.DoEndWindow;
begin
end;

procedure TSkCanvasCustom.DoFillEllipse(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TBrush);
var
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddOval(ARect, TSkPathDirection.CW, 3);
  FSurface.Canvas.DrawPath(LPathBuilder.Detach, BrushToSkPaint(ABrush, ARect, AOpacity));
end;

procedure TSkCanvasCustom.DoFillPath(const APath: TPathData;
  const AOpacity: Single; const ABrush: TBrush);
begin
  FSurface.Canvas.DrawPath(APath.ToSkPath, BrushToSkPaint(ABrush, APath.GetBounds, AOpacity));
end;

procedure TSkCanvasCustom.DoFillRect(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TBrush);
begin
  FSurface.Canvas.DrawRect(ARect, BrushToSkPaint(ABrush, ARect, AOpacity));
end;

class procedure TSkCanvasCustom.DoFinalizeBitmap(var ABitmapHandle: THandle);
begin
  ClearCache(ABitmapHandle);
  TSkBitmap(ABitmapHandle).Free;
end;

class function TSkCanvasCustom.DoInitializeBitmap(const AWidth,
  AHeight: Integer; const AScale: Single;
  var APixelFormat: TPixelFormat): THandle;
begin
  Result := THandle(TSkBitmap.Create(AWidth, AHeight));
  if APixelFormat = TPixelFormat.None then
  begin
    // Some methods used in Windows ignore PixelFormat, in Windows we will always use TPixelFormat.BGRA
    APixelFormat := SkFmxPixelFormat[{$IFDEF MSWINDOWS}TSkColorType.BGRA8888{$ELSE}ColorType{$ENDIF}];
  end;
end;

class function TSkCanvasCustom.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
begin
  if AAccess <> TMapAccess.Read then
    ClearCache(ABitmapHandle);
  ABitmapData.Data  := TSkBitmap(ABitmapHandle).Pixels;
  ABitmapData.Pitch := TSkBitmap(ABitmapHandle).Width * 4;
  Result := True;
end;

{$IF CompilerVersion >= 30}

procedure TSkCanvasCustom.DoSetMatrix(const AMatrix: TMatrix);
begin
  if Assigned(FSurface) then
    FSurface.Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
end;

{$ENDIF}

class procedure TSkCanvasCustom.DoUnmapBitmap(const ABitmapHandle: THandle;
  var ABitmapData: TBitmapData);
begin
end;

procedure TSkCanvasCustom.ExcludeClipRect(const ARect: TRectF);
begin
  Inc(FClippingChangeCount);
  FSurface.Canvas.ClipRect(ARect, TSkClipOp.Difference);
end;

class procedure TSkCanvasCustom.Finalize;
begin
  TSkiaAPI.Terminate;
end;

function TSkCanvasCustom.GetCachedImage(const ABitmapHandle: THandle): ISkImage;
begin
  Result := nil;
end;

class function TSkCanvasCustom.GetCanvasStyle: TCanvasStyles;
begin
  Result := [];
end;

function TSkCanvasCustom.GetSamplingOptions(
  const AHighSpeed: Boolean): TSkSamplingOptions;
begin
  if AHighSpeed then
    Result := TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.None)
  else
  begin
    case Quality of
      TCanvasQuality.SystemDefault,
      TCanvasQuality.HighQuality: Result := TSkSamplingOptions.High;
    else
      Result := TSkSamplingOptions.Create(TSkFilterMode.Nearest, TSkMipmapMode.Nearest);
    end;
  end;
end;

class procedure TSkCanvasCustom.Initialize;
begin
  TSkiaAPI.Initialize;
end;

procedure TSkCanvasCustom.IntersectClipRect(const ARect: TRectF);
begin
  Inc(FClippingChangeCount);
  FSurface.Canvas.ClipRect(ARect);
end;

function TSkCanvasCustom.PtInPath(const APoint: TPointF;
  const APath: TPathData): Boolean;
var
  LPaint: ISkPaint;
  LPath: ISkPath;
begin
  LPaint := TSkPaint.Create;
  LPath  := LPaint.GetFillPath(APath.ToSkPath);
  Result := Assigned(LPath) and (LPath.Contains(APoint.X, APoint.Y));
end;

procedure TSkCanvasCustom.Resized;
begin
  {$IFDEF MSWINDOWS}
  if (Parent <> nil) and (WindowHandleToPlatform(Parent){$IF CompilerVersion < 30}.Form{$ENDIF}.Transparency) then
    WindowHandleToPlatform(Parent).ResizeBuffer({$IF CompilerVersion < 31}Width, Height{$ELSE}WindowHandleToPlatform(Parent).WndClientSize.Width, WindowHandleToPlatform(Parent).WndClientSize.Height{$ENDIF});
  {$ENDIF}
end;

procedure TSkCanvasCustom.Restore;
begin
  FSurface.Canvas.Restore;
end;

procedure TSkCanvasCustom.Save;
begin
  FSurface.Canvas.Save;
end;

{$IF CompilerVersion < 30}

procedure TSkCanvasCustom.SetMatrix(const AMatrix: TMatrix);
begin
  inherited;
  if Assigned(FSurface) then
    FSurface.Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
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

procedure TSkCanvasCustom.SetupBrush(const ABrush: TBrush; const ARect: TRectF;
  const AOpacity: Single; const APaint: ISkPaint);
const
  WrapMode: array[TWrapMode.Tile..TWrapMode.TileOriginal] of TSkTileMode = (TSkTileMode.Repeat, TSkTileMode.Clamp);
var
  I: Integer;
  LCenter: TPointF;
  LColors: TArray<TAlphaColor>;
  LImage: ISkImage;
  LMatrix: TMatrix;
  LPositions: TArray<Single>;
  LRadius: Single;
  LRadiusX: Single;
  LRadiusY: Single;
begin
  if ABrush.Kind = TBrushKind.Resource then
  begin
    if ABrush.Resource.Brush = nil then
      Exit;
    ABrush.Assign(ABrush.Resource.Brush);
  end;
  APaint.AntiAlias := Quality <> TCanvasQuality.HighPerformance;
  case ABrush.Kind of
    TBrushKind.Solid: APaint.Color := MakeColor(ABrush.Color, AOpacity);
    TBrushKind.Gradient:
      begin
        SetLength(LColors, ABrush.Gradient.Points.Count);
        SetLength(LPositions, ABrush.Gradient.Points.Count);
        case ABrush.Gradient.Style of
          TGradientStyle.Linear:
            begin
              for I := 0 to ABrush.Gradient.Points.Count - 1 do
              begin
                LColors[I]    := MakeColor(ABrush.Gradient.Points[I].Color, AOpacity);
                LPositions[I] := ABrush.Gradient.Points[I].Offset;
              end;
              APaint.Shader := TSkShader.MakeGradientLinear(TPointF.Create(ARect.Left + ABrush.Gradient.StartPosition.X * ARect.Width, ARect.Top + ABrush.Gradient.StartPosition.Y * ARect.Height), TPointF.Create(ARect.Left + ABrush.Gradient.StopPosition.X * ARect.Width, ARect.Top + ABrush.Gradient.StopPosition.Y * ARect.Height), LColors, LPositions);
            end;
          TGradientStyle.Radial:
            begin
              for I := 0 to ABrush.Gradient.Points.Count - 1 do
              begin
                LColors[ABrush.Gradient.Points.Count - 1 - I]    := MakeColor(ABrush.Gradient.Points[I].Color, AOpacity);
                LPositions[ABrush.Gradient.Points.Count - 1 - I] := 1 - ABrush.Gradient.Points[I].Offset;
              end;
              LCenter  := TPointF.Create(ARect.Width * ABrush.Gradient.RadialTransform.RotationCenter.X, ARect.Height * ABrush.Gradient.RadialTransform.RotationCenter.Y) + ARect.TopLeft;
              LRadiusX := ABrush.Gradient.RadialTransform.Scale.X * (ARect.Width  / 2);
              LRadiusY := ABrush.Gradient.RadialTransform.Scale.Y * (ARect.Height / 2);
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
                APaint.Shader := TSkShader.MakeGradientRadial(LCenter, LRadius, LColors, LMatrix, LPositions);
              end
              else
                APaint.Shader := TSkShader.MakeGradientRadial(LCenter, LRadiusX, LColors, LPositions);
            end;
        end;
      end;
    TBrushKind.Bitmap:
      begin
        LImage := BitmapToSkImage(ABrush.Bitmap.Bitmap);
        if Assigned(LImage) then
        begin
          if ABrush.Bitmap.WrapMode = TWrapMode.TileStretch then
            APaint.Shader := LImage.MakeShader(TMatrix.CreateScaling(ARect.Width / LImage.Width, ARect.Height / LImage.Height) * TMatrix.CreateTranslation(ARect.Left, ARect.Top), GetSamplingOptions)
          else
            APaint.Shader := LImage.MakeShader(GetSamplingOptions, WrapMode[ABrush.Bitmap.WrapMode], WrapMode[ABrush.Bitmap.WrapMode]);
          APaint.AlphaF := AOpacity;
        end;
      end;
  end;
end;

function TSkCanvasCustom.StrokeBrushToSkPaint(const ABrush: TStrokeBrush;
  const ARect: TRectF; const AOpacity: Single): ISkPaint;
const
  StrokeCap  : array[TStrokeCap] of TSkStrokeCap = (TSkStrokeCap.Square, TSkStrokeCap.Round);
  StrokeJoin : array[TStrokeJoin] of TSkStrokeJoin = (TSkStrokeJoin.Miter, TSkStrokeJoin.Round, TSkStrokeJoin.Bevel);
var
  I: Integer;
  LCap: Single;
  LDash: TDashArray;
begin
  Result := TSkPaint.Create(TSkPaintStyle.Stroke);
  SetupBrush(ABrush, ARect, AOpacity, Result);
  Result.StrokeCap   := StrokeCap[ABrush.Cap];
  Result.StrokeJoin  := StrokeJoin[ABrush.Join];
  Result.StrokeWidth := ABrush.Thickness;
  LDash := ABrush.DashArray;
  if Length(LDash) > 0 then
  begin
    if ABrush.Dash = TStrokeDash.Custom then
      LCap := ABrush.Thickness
    else
      LCap := 0;
    for I := 0 to Length(LDash) - 1 do
    begin
      if Odd(I) then
        LDash[I] := (LDash[I]  + 1) * ABrush.Thickness - LCap
      else
        LDash[I] := (LDash[I] - 1) * ABrush.Thickness + LCap;
    end;
    Result.PathEffect := TSkPathEffect.MakeDash(TArray<Single>(LDash), ABrush.DashOffset * ABrush.Thickness);
  end;
end;

{ TSkCanvasCustom.TSaveState }

procedure TSkCanvasCustom.TSaveState.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TSkCanvasCustom then
    TSkCanvasCustom(ASource).Save;
end;

procedure TSkCanvasCustom.TSaveState.AssignTo(ADest: TPersistent);
begin
  inherited;
  if ADest is TSkCanvasCustom then
    TSkCanvasCustom(ADest).Restore;
end;

{ TSkCanvasRasterCustom }

destructor TSkCanvasRasterCustom.Destroy;
begin
  if FBufferHandle <> 0 then
    FreeBuffer(FBufferHandle);
  inherited;
end;

function TSkCanvasRasterCustom.DoBeginWindow(
  const AContextHandle: THandle): Boolean;
begin
  if FBufferHandle = 0 then
    FBufferHandle := CreateBuffer;
  FSurface := CreateWindowSurface(AContextHandle, FBufferHandle);
  Result   := Assigned(FSurface);
end;

procedure TSkCanvasRasterCustom.DoEndWindow;
begin
  SwapBuffers(FBufferHandle);
end;

procedure TSkCanvasRasterCustom.Resized;
begin
  inherited;
  if FBufferHandle <> 0 then
  begin
    FreeBuffer(FBufferHandle);
    FBufferHandle := 0;
  end;
end;

{ TGrCanvasCustom }

procedure TGrCanvasCustom.AttachToWindow;
begin
end;

function TGrCanvasCustom.BeginContext: Boolean;
begin
  TMonitor.Enter(FContextLock);
  if FContextCount = 0 then
  begin
    try
      if not Assigned(FContext) then
      begin
        FContext := CreateContext;
        if not Assigned(FContext) then
        begin
          TMonitor.Exit(FContextLock);
          Exit(False);
        end;
      end;
      AttachToWindow;
      Prepare;
      FSurface := TSkSurface.MakeFromRenderTarget(FContext, GetRenderTarget, Origin, ColorType);
      if not Assigned(FSurface) then
      begin
        DetachFromWindow;
        TMonitor.Exit(FContextLock);
        Exit(False);
      end
      else
        FContextCount := 1;
    except
      TMonitor.Exit(FContextLock);
      raise;
    end;
  end
  else
  begin
    TMonitor.Exit(FContextLock);
    Inc(FContextCount);
  end;
  Result := True;
end;

class procedure TGrCanvasCustom.ClearCache(const ABitmapHandle: THandle);
begin
  TImageCache.Remove(ABitmapHandle);
end;

constructor TGrCanvasCustom.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  FContextLock := TObject.Create;
end;

function TGrCanvasCustom.CreateImage(const ABitmapHandle: THandle;
  const AColorType: TSkColorType): ISkImage;
begin
  inherited.MakeTextureImage(FContext, Result);
  TImageCache.Add(Self, ABitmapHandle, Result);
end;

destructor TGrCanvasCustom.Destroy;
begin
  if Parent <> nil then
  begin
    try
      if Assigned(FContext) then
      begin
        Prepare;
        FContext.Dispose;
        TImageCache.Clear(Self);
      end;
    finally
      FContextLock.Free;
    end;
  end;
  inherited;
end;

procedure TGrCanvasCustom.DetachFromWindow;
begin
end;

function TGrCanvasCustom.DoBeginWindow(const AContextHandle: THandle): Boolean;
begin
  Result := BeginContext;
end;

procedure TGrCanvasCustom.DoEndWindow;
begin
  FSurface.FlushAndSubmit;
  FSurface.Dispose;
  FContext.FlushAndSubmit;
  Flush;
  EndContext;
end;

procedure TGrCanvasCustom.EndContext;
begin
  Dec(FContextCount);
  if FContextCount = 0 then
  begin
    if Assigned(FContext) then
      DetachFromWindow;
    TMonitor.Exit(FContextLock);
  end;
end;

class procedure TGrCanvasCustom.Finalize;
begin
  TImageCache.Finalize;
  inherited;
end;

function TGrCanvasCustom.GetCachedImage(const ABitmapHandle: THandle): ISkImage;
begin
  Result := TImageCache.Get(Self, ABitmapHandle);
end;

class procedure TGrCanvasCustom.Initialize;
begin
  inherited;
  TImageCache.Initialize;
end;

procedure TGrCanvasCustom.Prepare;
begin
end;

procedure TGrCanvasCustom.Restore;
begin
  if Parent <> nil then
    Prepare;
  inherited;
end;

{ TGrCanvasCustom.TImageCache }

class procedure TGrCanvasCustom.TImageCache.Add(const ACanvas: TGrCanvasCustom;
  const ABitmapHandle: THandle; const AImage: ISkImage);
begin
  {$IF CompilerVersion < 35}
  TMonitor.Enter(FCacheLock);
  {$ELSE}
  FCacheLock.BeginWrite;
  {$ENDIF}
  try
    if not FCache.ContainsKey(ACanvas) then
      FCache.Add(ACanvas, TDictionary<THandle, ISkImage>.Create);
    FCache[ACanvas].Add(ABitmapHandle, AImage);
  finally
    {$IF CompilerVersion < 35}
    TMonitor.Exit(FCacheLock);
    {$ELSE}
    FCacheLock.EndWrite;
    {$ENDIF}
  end;
end;

class procedure TGrCanvasCustom.TImageCache.Clear(
  const ACanvas: TGrCanvasCustom);
begin
  {$IF CompilerVersion < 35}
  TMonitor.Enter(FCacheLock);
  {$ELSE}
  FCacheLock.BeginWrite;
  {$ENDIF}
  try
    FCache.Remove(ACanvas)
  finally
    {$IF CompilerVersion < 35}
    TMonitor.Exit(FCacheLock);
    {$ELSE}
    FCacheLock.EndWrite;
    {$ENDIF}
  end;
end;

class procedure TGrCanvasCustom.TImageCache.Finalize;
begin
  {$IF CompilerVersion < 35}
  FCacheLock.Free;
  {$ENDIF}
  FCache.Free;
end;

class function TGrCanvasCustom.TImageCache.Get(const ACanvas: TGrCanvasCustom;
  const ABitmapHandle: THandle): ISkImage;
begin
  {$IF CompilerVersion < 35}
  TMonitor.Enter(FCacheLock);
  {$ELSE}
  FCacheLock.BeginRead;
  {$ENDIF}
  try
    if (FCache.ContainsKey(ACanvas)) and (FCache[ACanvas].ContainsKey(ABitmapHandle)) then
      Result := FCache[ACanvas][ABitmapHandle]
    else
      Result := nil;
  finally
    {$IF CompilerVersion < 35}
    TMonitor.Exit(FCacheLock);
    {$ELSE}
    FCacheLock.EndRead;
    {$ENDIF}
  end;
end;

class procedure TGrCanvasCustom.TImageCache.Initialize;
begin
  FCache     := TObjectDictionary<TGrCanvasCustom, TDictionary<THandle, ISkImage>>.Create([doOwnsValues]);
  {$IF CompilerVersion < 35}
  FCacheLock := TObject.Create;
  {$ENDIF}
end;

class procedure TGrCanvasCustom.TImageCache.Remove(
  const ABitmapHandle: THandle);
var
  LCanvas: TGrCanvasCustom;
begin
  {$IF CompilerVersion < 35}
  TMonitor.Enter(FCacheLock);
  {$ELSE}
  FCacheLock.BeginWrite;
  {$ENDIF}
  try
    for LCanvas in FCache.Keys do
    begin
      if FCache[LCanvas].ContainsKey(ABitmapHandle) then
      begin
        TMonitor.Enter(LCanvas.FContextLock);
        try
          LCanvas.Prepare;
          FCache[LCanvas].Remove(ABitmapHandle);
        finally
          TMonitor.Exit(LCanvas.FContextLock);
        end;
      end;
    end;
  finally
    {$IF CompilerVersion < 35}
    TMonitor.Exit(FCacheLock);
    {$ELSE}
    FCacheLock.EndWrite;
    {$ENDIF}
  end;
end;

var
  CanvasService: TSkCanvasService;
initialization
  CanvasService := TSkCanvasService.Create(IFMXCanvasService(TPlatformServices.Current.GetPlatformService(IFMXCanvasService)));
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, CanvasService);
end.
