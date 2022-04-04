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
{$IF (CompilerVersion < 35) or not DECLARED(RTLVersion111)}
  {$DEFINE MODULATE_CANVAS}
{$ENDIF}

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,
  FMX.TextLayout,
  System.Classes,
  System.Generics.Collections,
  System.Math.Vectors,
  System.SysUtils,
  System.Types,
  System.UITypes,

  { Skia }
  Skia;

type
  ESkCanvas = class(Exception);

  EGrCanvas = class(ESkCanvas);

  { TSkBitmapHandle }

  TSkBitmapHandle = class
  strict private
    FHeight: Integer;
    FPixels: Pointer;
    FWidth: Integer;
  public
    constructor Create(const AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Initialize; inline;
    property Height: Integer read FHeight;
    property Pixels: Pointer read FPixels;
    property Width: Integer read FWidth;
  end;

  TSkCanvasClass = class of TSkCanvasCustom;

  { TSkCanvasCustom }

  TSkCanvasCustom = class abstract(TCanvas{$IFDEF MODULATE_CANVAS}, IModulateCanvas{$ENDIF})
  strict private type
    TSaveState = class(TCanvasSaveState)
    strict protected
      procedure AssignTo(ADest: TPersistent); override;
    public
      procedure Assign(ASource: TPersistent); override;
    end;

  strict private class var
    FImageCache: TObjectDictionary<TSkCanvasCustom, TDictionary<THandle, ISkImage>>;
  {$IFDEF MODULATE_CANVAS}
  strict private
    FModulateColor: TAlphaColor;
    { IModulateCanvas }
    function GetModulateColor: TAlphaColor;
    procedure SetModulateColor(const AColor: TAlphaColor);
  {$ENDIF}
  strict private
    FDrawableHeight: Integer;
    FDrawableWidth: Integer;
    FSurface: ISkSurface;
    function GetSamplingOptions(const AHighSpeed: Boolean = False): TSkSamplingOptions;
    procedure SetupBrush(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single; const APaint: ISkPaint);
  strict protected
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    {$IFDEF MSWINDOWS}
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    {$ENDIF}
    function BeginWindow(const AContextHandle: THandle): ISkSurface; virtual; abstract;
    function BitmapToSkImage(const ABitmap: TBitmap): ISkImage;
    function BrushToSkPaint(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single): ISkPaint;
    function CreateCache(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage; virtual;
    function CreateSaveState: TCanvasSaveState; override;
    procedure DestroyWindow; virtual;
    function DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override; final;
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
    procedure EndWindow; virtual;
    procedure Resized; virtual;
    procedure Restore; virtual;
    procedure Save; virtual;
    function StrokeBrushToSkPaint(const ABrush: TStrokeBrush; const ARect: TRectF; const AOpacity: Single): ISkPaint;
    class procedure ClearCache(const ACanvas: TSkCanvasCustom); virtual;
    class procedure ClearCacheBitmap(const ABitmapHandle: THandle);
    class procedure DoClearCacheBitmap(const ACanvas: TSkCanvasCustom; const ABitmapHandle: THandle); virtual;
    class procedure DoFinalize; virtual;
    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
    class function DoInitialize: Boolean; virtual;
    class function DoInitializeBitmap(const AWidth, AHeight: Integer; const AScale: Single; var APixelFormat: TPixelFormat): THandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const ABitmapHandle: THandle; var ABitmapData: TBitmapData); override;
  public
    procedure BeforeDestruction; override;
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
    /// <summary> Direct access to the Skia Surface. This property just won't be nil between BeginScene and EndScene calls. </summary>
    property Surface: ISkSurface read FSurface;
    class procedure Finalize;
    class function GetCanvasStyle: TCanvasStyles; override;
    class function Initialize: Boolean;
  end;

  { TSkCanvasRasterCustom }

  TSkCanvasRasterCustom = class abstract(TSkCanvasCustom)
  strict private
    FBufferHandle: THandle;
  strict protected
    function BeginWindow(const AContextHandle: THandle): ISkSurface; override;
    function CreateBuffer: THandle; virtual; abstract;
    function CreateWindowSurface(const AContextHandle, ABufferHandle: THandle): ISkSurface; virtual; abstract;
    procedure EndWindow; override;
    procedure Flush(const ABufferHandle: THandle); virtual; abstract;
    procedure FreeBuffer(const ABufferHandle: THandle); virtual; abstract;
    procedure Resized; override;
  public
    destructor Destroy; override;
  end;

  { TGrCanvasCustom }

  TGrCanvasCustom = class abstract(TSkCanvasCustom)
  strict private
    FContext: IGrDirectContext;
    FContextInitialized: Boolean;
  strict protected
    function BeginWindow(const AContextHandle: THandle): ISkSurface; override;
    function CreateContext: IGrDirectContext; virtual; abstract;
    function CreateDirectContext: IGrDirectContext; virtual; abstract;
    function CreateSurfaceFromWindow: ISkSurface; virtual; abstract;
    procedure DestroyWindow; override;
    procedure EndWindow; override;
    procedure FinalizeContext; virtual; abstract;
    procedure Flush; virtual; abstract;
    function InitializeContext: Boolean; virtual; abstract;
    procedure PrepareContext; virtual;
    procedure Restore; override;
    function CreateCache(const AWidth, AHeight: Integer; const AColorType: TSkColorType; const APixels: Pointer; const ARowBytes: NativeUInt): ISkImage; override;
    property Context: IGrDirectContext read FContext;
    class procedure ClearCache(const ACanvas: TSkCanvasCustom); override;
    class procedure DoClearCacheBitmap(const ACanvas: TSkCanvasCustom; const ABitmapHandle: THandle); override;
  public
    destructor Destroy; override;
  end;

  { TSkTextLayout }

  TSkTextLayout = class(TTextLayout)
  strict private
    FColor: TAlphaColor;
    FIgnoreUpdates: Boolean;
    FMaxLines: Integer;
    FOpacity: Single;
    FParagraph: ISkParagraph;
    FParagraphOffset: TPointF;
    FTextRect: TRectF;
    function NeedHorizontalAlignment: Boolean;
    procedure SetMaxLines(const AValue: Integer);
    procedure UpdateParagraph;
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
    procedure ConvertToPath(const APath: TPathData); override;
    procedure RenderLayout(const ACanvas: ISkCanvas); overload;
    /// <summary> Max lines allowed in text. When the value is different from -1, this property will be used instead of the WordWrap property. </summary>
    property MaxLines: Integer read FMaxLines write SetMaxLines default -1;
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
  FMX.Surfaces,
  System.Generics.Defaults,
  System.IOUtils,
  System.Math,
  System.UIConsts,
  {$IFDEF DEBUG}
  System.Messaging,
  {$ENDIF}
  {$IF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.Windows,
  {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
  Macapi.CocoaTypes,
  Macapi.CoreGraphics,
  {$ENDIF}

  // These units are used for workarounds
  FMX.Forms,
  {$IF DEFINED(MACOS)}
  System.Rtti,
  FMX.Context.Metal,
  FMX.Types3D,
  FMX.Utils,
  Macapi.Metal,
  Macapi.MetalKit,
  Posix.SysMman,
  Posix.Unistd,
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
  System.Rtti,
  Androidapi.Gles2,
  FMX.Context.GLES,
  FMX.Types3D,
  FMX.Utils,
  Posix.SysMman,
  Posix.Unistd,
  {$ENDIF}

  { Skia }
  Skia.API,
  Skia.FMX,
  Skia.FMX.Canvas.GL,
  Skia.FMX.Canvas.Metal;

type
  {$IF DEFINED(MSWINDOWS)}

  { TSkCanvasRasterWindows }

  TSkCanvasRasterWindows = class(TSkCanvasRasterCustom)
  strict private
    FDC: HDC;
  strict protected
    function CreateBuffer: THandle; override;
    function CreateWindowSurface(const AContextHandle, ABufferHandle: THandle): ISkSurface; override;
    procedure Flush(const ABufferHandle: THandle); override;
    procedure FreeBuffer(const ABufferHandle: THandle); override;
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
    procedure Flush(const ABufferHandle: THandle); override;
    procedure FreeBuffer(const ABufferHandle: THandle); override;
    class procedure DoFinalize; override;
    class function DoInitialize: Boolean; override;
  end;

  {$ENDIF}

  { TSkBitmapHandleCodec }

  TSkBitmapHandleCodec = class(TCustomBitmapCodec)
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

{$IF DEFINED(MSWINDOWS)}

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

procedure TSkCanvasRasterWindows.Flush(const ABufferHandle: THandle);
var
  LBufferDC: HDC;
begin
  if FDC <> 0 then
  begin
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
end;

procedure TSkCanvasRasterWindows.FreeBuffer(const ABufferHandle: THandle);
begin
  DeleteObject(HBITMAP(ABufferHandle));
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

class procedure TSkCanvasRasterMacOS.DoFinalize;
begin
  CGColorSpaceRelease(FColorSpace);
end;

class function TSkCanvasRasterMacOS.DoInitialize: Boolean;
begin
  FColorSpace := CGColorSpaceCreateDeviceRGB;
  Result      := FColorSpace <> nil;
end;

procedure TSkCanvasRasterMacOS.Flush(const ABufferHandle: THandle);
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

procedure TSkCanvasRasterMacOS.FreeBuffer(const ABufferHandle: THandle);
begin
  CGContextRelease(CGContextRef(ABufferHandle));
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
  FMaxLines := -1;
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
  {$IF CompilerVersion > 35}
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
begin
  if (ACanvas is TSkCanvasCustom) and Assigned(TSkCanvasCustom(ACanvas).Surface) then
    DoDrawLayout(TSkCanvasCustom(ACanvas).Surface.Canvas);
end;

procedure TSkTextLayout.DoDrawLayout(const ACanvas: ISkCanvas);
begin
  if Assigned(FParagraph) and Assigned(ACanvas) then
  begin
    if (FColor <> Color) or (FOpacity <> Opacity) then
      UpdateParagraph;
    ACanvas.Save;
    try
      ACanvas.ClipRect(TRectF.Create(TopLeft, MaxSize.X, MaxSize.Y));
      FParagraph.Paint(ACanvas, FParagraphOffset.X + TopLeft.X, FParagraphOffset.Y + TopLeft.Y);
    finally
      ACanvas.Restore;
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

  function GetTextRect: TRectF;
  var
    LTextBox: TSkTextBox;
  begin
    Result := TRectF.Empty;
    for LTextBox in FParagraph.GetRectsForRange(0, Text.Length, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight) do
    begin
      if Result.IsEmpty then
        Result := LTextBox.Rect
      else
        Result := Result + LTextBox.Rect;
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
  FTextRect := GetTextRect;
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

procedure TSkTextLayout.RenderLayout(const ACanvas: ISkCanvas);
begin
  RenderLayout(nil);
  DoDrawLayout(ACanvas);
end;

procedure TSkTextLayout.SetMaxLines(const AValue: Integer);
begin
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
    if FMaxLines = 0 then
      Result.MaxLines := High(Integer)
    else if FMaxLines > 0 then
      Result.MaxLines := FMaxLines
    else if WordWrap then
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

    if Result.MaxLines <> 1 then
    begin
      if (AMaxLines = 0) and (Result.MaxLines = NativeUInt(High(Integer))) then
      begin
        LMinFontSize := Result.TextStyle.FontSize;
        for LAttribute in AAttributes do
          LMinFontSize := Min(LMinFontSize, LAttribute.Attribute.Font.Size);
        if LMinFontSize > 0.1 then
          AMaxLines := Ceil(MaxSize.Y / LMinFontSize);
      end;
      if AMaxLines > 0 then
        Result.MaxLines := AMaxLines;
    end;
  end;

  // Temporary solution to fix an issue with Skia: https://bugs.chromium.org/p/skia/issues/detail?id=13117
  // SkParagraph has several issues with the #13 line break, so the best thing to do is replace it with #10 or a zero-widh character (#8203)
  function NormalizeParagraphText(const AText: string): string;
  begin
    Result := AText.Replace(#13#10, #8203#10).Replace(#13, #10);
  end;

  procedure DoUpdateParagraph(const AMaxLines: Integer);
  var
    LAttribute: TTextAttributedRange;
    LAttributes: TArray<TTextAttributedRange>;
    LBuilder: ISkParagraphBuilder;
    LLastAttributeEndIndex: Integer;
    LText: string;
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
        LText := NormalizeParagraphText(Text.Substring(LAttribute.Range.Pos, LAttribute.Range.Length));
        if not LText.IsEmpty then
        begin
          LBuilder.PushStyle(CreateTextStyle(LAttribute.Attribute));
          LBuilder.AddText(LText);
          LBuilder.Pop;
        end;
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

{ TSkBitmapHandleCodec }

class constructor TSkBitmapHandleCodec.Create;
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

function TSkBitmapHandleCodec.FitSize(const AWidth, AHeight: Integer; const AFitWidth,
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

class function TSkBitmapHandleCodec.GetImageSize(const AFileName: string): TPointF;
var
  LCodec: ISkCodec;
begin
  LCodec := TSkCodec.MakeFromFile(AFileName);
  if not Assigned(LCodec) then
    Exit(TPointF.Create(0, 0));
  Result := TPointF.Create(LCodec.Width, LCodec.Height);
end;

class function TSkBitmapHandleCodec.IsValid(const AStream: TStream): Boolean;

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

function TSkBitmapHandleCodec.LoadFromFile(const AFileName: string;
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

function TSkBitmapHandleCodec.LoadFromStream(const AStream: TStream;
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

function TSkBitmapHandleCodec.LoadThumbnailFromFile(const AFileName: string;
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

class procedure TSkBitmapHandleCodec.RegisterIfNotExists(const AFileExtension,
  ADescription: string; const ACanSave: Boolean);
begin
  if not TBitmapCodecManager.CodecExists(AFileExtension) then
    TBitmapCodecManager.RegisterBitmapCodecClass(AFileExtension, ADescription, ACanSave, TSkBitmapHandleCodec);
end;

function TSkBitmapHandleCodec.SaveToFile(const AFileName: string;
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

function TSkBitmapHandleCodec.SaveToStream(const AStream: TStream;
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

{ TSkBitmapHandle }

constructor TSkBitmapHandle.Create(const AWidth, AHeight: Integer);
begin
  inherited Create;
  FWidth  := AWidth;
  FHeight := AHeight;
end;

destructor TSkBitmapHandle.Destroy;
begin
  FreeMem(FPixels);
  inherited;
end;

procedure TSkBitmapHandle.Initialize;
begin
  if FPixels = nil then
    FPixels := AllocMem(FWidth * FHeight * 4);
end;

{ TSkCanvasCustom }

procedure TSkCanvasCustom.BeforeDestruction;
begin
  inherited;
  if Parent <> nil then
    DestroyWindow;
end;

function TSkCanvasCustom.BitmapToSkImage(
  const ABitmap: FMX.Graphics.TBitmap): ISkImage;
var
  LBitmapData: TBitmapData;
begin
  if not ABitmap.HandleAllocated then
    Exit(nil);
  if ABitmap.CanvasClass.InheritsFrom(ClassType) then
  begin
    if TSkBitmapHandle(ABitmap.Handle).Pixels = nil then
      Exit(nil);
    if (Parent <> nil) and (Assigned(FImageCache)) then
    begin
      if (FImageCache.ContainsKey(Self)) and (FImageCache[Self].ContainsKey(ABitmap.Handle)) then
        Result := FImageCache[Self][ABitmap.Handle]
      else
      begin
        Result := CreateCache(ABitmap.Width, ABitmap.Height, SkFmxColorType[ABitmap.PixelFormat], TSkBitmapHandle(ABitmap.Handle).Pixels, ABitmap.Width * 4);
        if not FImageCache.ContainsKey(Self) then
          FImageCache.Add(Self, TDictionary<THandle, ISkImage>.Create);
        FImageCache[Self].Add(ABitmap.Handle, Result);
      end;
    end
    else
      Result := TSkImage.MakeFromRaster(TSkImageInfo.Create(ABitmap.Width, ABitmap.Height, SkFmxColorType[ABitmap.PixelFormat]), TSkBitmapHandle(ABitmap.Handle).Pixels, ABitmap.Width * 4);
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
  {$IF CompilerVersion >= 35}
  RaiseIfBeginSceneCountZero;
  {$ELSE}
  if BeginSceneCount > 0 then
  {$ENDIF}
  FSurface.Canvas.Clear(AColor);
end;

class procedure TSkCanvasCustom.ClearCache(const ACanvas: TSkCanvasCustom);
begin
  FImageCache[ACanvas].Clear;
end;

class procedure TSkCanvasCustom.ClearCacheBitmap(const ABitmapHandle: THandle);
begin
  TThread.Queue(nil,
    procedure ()
    var
      LCanvas: TSkCanvasCustom;
    begin
      if Assigned(FImageCache) then
      begin
        for LCanvas in FImageCache.Keys do
        begin
          if FImageCache[LCanvas].ContainsKey(ABitmapHandle) then
            DoClearCacheBitmap(LCanvas, ABitmapHandle);
        end;
      end;
    end);
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

function TSkCanvasCustom.CreateCache(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const APixels: Pointer;
  const ARowBytes: NativeUInt): ISkImage;
begin
  Result := TSkImage.MakeFromRaster(TSkImageInfo.Create(AWidth, AHeight, AColorType), APixels, ARowBytes);
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

function TSkCanvasCustom.CreateSaveState: TCanvasSaveState;
begin
  Result := TSaveState.Create;
end;

procedure TSkCanvasCustom.DestroyWindow;
begin
  if Assigned(FImageCache) then
    FImageCache.Remove(Self);
end;

function TSkCanvasCustom.DoBeginScene({$IF CompilerVersion < 35}const {$ENDIF}AClipRects: PClipRects;
  AContextHandle: THandle): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    if Bitmap <> nil then
    begin
      ClearCacheBitmap(Bitmap.Handle);
      TSkBitmapHandle(Bitmap.Handle).Initialize;
      FDrawableWidth  := Width;
      FDrawableHeight := Height;
      FSurface        := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(Width, Height, SkFmxColorType[Bitmap.PixelFormat]), TSkBitmapHandle(Bitmap.Handle).Pixels, Width * 4);
    end
    else if Parent <> nil then
    begin
      FDrawableWidth  := Round(Width  * Scale);
      FDrawableHeight := Round(Height * Scale);
      FSurface        := BeginWindow(AContextHandle);
    end
    else
      Exit(False);
    Result := Assigned(FSurface);
    if Result then
      FSurface.Canvas.SetMatrix(TMatrix.CreateScaling(Scale, Scale));
  end;
end;

class procedure TSkCanvasCustom.DoClearCacheBitmap(
  const ACanvas: TSkCanvasCustom; const ABitmapHandle: THandle);
begin
  FImageCache[ACanvas].Remove(ABitmapHandle);
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
    {$IFDEF MODULATE_CANVAS}
    if FModulateColor <> TAlphaColors.Null then
      LPaint.ColorFilter := TSkColorFilter.MakeBlend(FModulateColor, TSkBlendMode.SrcIn);
    {$ENDIF}
    FSurface.Canvas.DrawImageRect(LImage, ASrcRect, ADestRect, GetSamplingOptions(AHighSpeed), LPaint);
  end;
end;

procedure TSkCanvasCustom.DoDrawEllipse(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
var
  LPathBuilder: ISkPathBuilder;
begin
  if SameValue(ABrush.Thickness, 0, TEpsilon.Position) then
    Exit;
  LPathBuilder := TSkPathBuilder.Create;
  LPathBuilder.AddOval(ARect, TSkPathDirection.CW, 3);
  FSurface.Canvas.DrawPath(LPathBuilder.Detach, StrokeBrushToSkPaint(ABrush, ARect, AOpacity));
end;

procedure TSkCanvasCustom.DoDrawLine(const APoint1, APoint2: TPointF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if SameValue(ABrush.Thickness, 0, TEpsilon.Position) then
    Exit;
  FSurface.Canvas.DrawLine(APoint1, APoint2, StrokeBrushToSkPaint(ABrush, TRectF.Create(APoint1, APoint2), AOpacity));
end;

procedure TSkCanvasCustom.DoDrawPath(const APath: TPathData;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if SameValue(ABrush.Thickness, 0, TEpsilon.Position) then
    Exit;
  FSurface.Canvas.DrawPath(APath.ToSkPath, StrokeBrushToSkPaint(ABrush, APath.GetBounds, AOpacity));
end;

procedure TSkCanvasCustom.DoDrawRect(const ARect: TRectF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if SameValue(ABrush.Thickness, 0, TEpsilon.Position) then
    Exit;
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
    EndWindow;
  end;
  FSurface := nil;
  inherited;
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

class procedure TSkCanvasCustom.DoFinalize;
begin
end;

class procedure TSkCanvasCustom.DoFinalizeBitmap(var ABitmapHandle: THandle);
begin
  ClearCacheBitmap(ABitmapHandle);
  TSkBitmapHandle(ABitmapHandle).Free;
end;

class function TSkCanvasCustom.DoInitialize: Boolean;
begin
  Result := True;
end;

class function TSkCanvasCustom.DoInitializeBitmap(const AWidth,
  AHeight: Integer; const AScale: Single;
  var APixelFormat: TPixelFormat): THandle;
begin
  Result := THandle(TSkBitmapHandle.Create(AWidth, AHeight));
  if APixelFormat = TPixelFormat.None then
  begin
    {$IF DEFINED(MSWINDOWS)}
    APixelFormat := TPixelFormat.BGRA;
    {$ELSEIF DEFINED(MACOS)}
    if GlobalUseMetal then
      APixelFormat := TPixelFormat.BGRA
    else
      APixelFormat := TPixelFormat.RGBA;
    {$ELSE}
    APixelFormat := TPixelFormat.RGBA;
    {$ENDIF}
  end;
end;

class function TSkCanvasCustom.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
begin
  if AAccess <> TMapAccess.Read then
    ClearCacheBitmap(ABitmapHandle);
  TSkBitmapHandle(ABitmapHandle).Initialize;
  ABitmapData.Data  := TSkBitmapHandle(ABitmapHandle).Pixels;
  ABitmapData.Pitch := TSkBitmapHandle(ABitmapHandle).Width * 4;
  Result := True;
end;

{$IF CompilerVersion >= 30}

procedure TSkCanvasCustom.DoSetMatrix(const AMatrix: TMatrix);
begin
  if BeginSceneCount > 0 then
    FSurface.Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
end;

{$ENDIF}

class procedure TSkCanvasCustom.DoUnmapBitmap(const ABitmapHandle: THandle;
  var ABitmapData: TBitmapData);
begin
end;

procedure TSkCanvasCustom.EndWindow;
begin
end;

procedure TSkCanvasCustom.ExcludeClipRect(const ARect: TRectF);
begin
  Inc(FClippingChangeCount);
  FSurface.Canvas.ClipRect(ARect, TSkClipOp.Difference);
end;

class procedure TSkCanvasCustom.Finalize;
var
  LCanvas: TSkCanvasCustom;
begin
  DoFinalize;
  for LCanvas in FImageCache.Keys do
    ClearCache(LCanvas);
  FImageCache.Free;
  TSkiaAPI.Terminate;
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

class function TSkCanvasCustom.Initialize: Boolean;
begin
  TSkiaAPI.Initialize;
  if not DoInitialize then
  begin
    TSkiaAPI.Terminate;
    Exit(False);
  end;
  FImageCache := TObjectDictionary<TSkCanvasCustom, TDictionary<THandle, ISkImage>>.Create([doOwnsValues]);
  Result      := True;
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
  if BeginSceneCount > 0 then
    FSurface.Canvas.SetMatrix(AMatrix * TMatrix.CreateScaling(Scale, Scale));
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

function TSkCanvasRasterCustom.BeginWindow(
  const AContextHandle: THandle): ISkSurface;
begin
  if FBufferHandle = 0 then
    FBufferHandle := CreateBuffer;
  Result := CreateWindowSurface(AContextHandle, FBufferHandle);
end;

destructor TSkCanvasRasterCustom.Destroy;
begin
  if FBufferHandle <> 0 then
    FreeBuffer(FBufferHandle);
  inherited;
end;

procedure TSkCanvasRasterCustom.EndWindow;
begin
  Flush(FBufferHandle);
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

function TGrCanvasCustom.BeginWindow(const AContextHandle: THandle): ISkSurface;
begin
  if not FContextInitialized then
  begin
    FContextInitialized := InitializeContext;
    if not FContextInitialized then
      Exit(nil);
  end;
  PrepareContext;
  if not Assigned(FContext) then
  begin
    FContext := CreateDirectContext;
    if not Assigned(FContext) then
      Exit(nil);
  end;
  Result := CreateSurfaceFromWindow;
end;

class procedure TGrCanvasCustom.ClearCache(const ACanvas: TSkCanvasCustom);
begin
  TGrCanvasCustom(ACanvas).PrepareContext;
  inherited;
end;

function TGrCanvasCustom.CreateCache(const AWidth, AHeight: Integer;
  const AColorType: TSkColorType; const APixels: Pointer;
  const ARowBytes: NativeUInt): ISkImage;
var
  LTextureImage: ISkImage;
begin
  Result := inherited;
  if Result.MakeTextureImage(FContext, LTextureImage) then
    Result := LTextureImage;
end;

destructor TGrCanvasCustom.Destroy;
begin
  if FContextInitialized then
    FinalizeContext;
  inherited;
end;

procedure TGrCanvasCustom.DestroyWindow;
begin
  if Assigned(FContext) then
  begin
    PrepareContext;
    inherited;
    FContext.Dispose;
  end;
end;

class procedure TGrCanvasCustom.DoClearCacheBitmap(
  const ACanvas: TSkCanvasCustom; const ABitmapHandle: THandle);
begin
  TGrCanvasCustom(ACanvas).PrepareContext;
  inherited;
end;

procedure TGrCanvasCustom.EndWindow;
begin
  Surface.FlushAndSubmit;
  Surface.Dispose;
  FContext.FlushAndSubmit;
  Flush;
end;

procedure TGrCanvasCustom.PrepareContext;
begin
end;

procedure TGrCanvasCustom.Restore;
begin
  if Parent <> nil then
    PrepareContext;
  inherited;
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
// - Bug report:
// -   https://quality.embarcadero.com/browse/RSP-36957
// -
// - ---------------------------------------------------------------------------
{$IF CompilerVersion > 35}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IF DEFINED(ANDROID) or DEFINED(IOS)}

type
  { TRSP36957Workaround }

  TRSP36957Workaround = record
  strict private type
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
{$IF CompilerVersion > 35}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IFDEF MACOS}

type
  { TRSP37147Workaround }

  TRSP37147Workaround = record
  strict private type
    {$RTTI EXPLICIT METHODS([vcProtected])}
    TContextMetalPatch = class(TCustomContextMetal)
    protected
      {$IF CompilerVersion > 35}
        {$MESSAGE WARN 'Check if the private fields of the FMX.Context.Metal.TContextMetal is the same below and in the same order'}
      {$ENDIF}
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
{$IF CompilerVersion > 35}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IFDEF MACOS}

type
  { TRSP37829Workaround }

  TRSP37829Workaround = record
  strict private class var
    FOriginalDoEndScene: Pointer;
  strict private type
    TDoEndSceneProc = procedure of object;

    {$RTTI EXPLICIT METHODS([vcProtected])}
    TContextMetalPatch = class(TCustomContextMetal)
    protected
      {$IF CompilerVersion > 35}
        {$MESSAGE WARN 'Check if the private fields of the FMX.Context.Metal.TContextMetal is the same below and in the same order'}
      {$ENDIF}
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
  if Assigned(FCommandBuffer) then
    FCommandBuffer.waitUntilCompleted;
end;

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
{$IF CompilerVersion > 35}
  {$MESSAGE WARN 'Check if the issue has been fixed'}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$IFDEF IOS}

type
  { TRSP37660Workaround }

  TRSP37660Workaround = record
  strict private type
    {$IF CompilerVersion > 35}
      {$MESSAGE WARN 'Check if the unit "FMX.MediaLibrary.IOS" has been changed, and apply all changes to this workaround'}
    {$ENDIF}
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
    strict private
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

  strict private
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
  if Assigned(ABitmap) and (not ABitmap.IsEmpty) and ABitmap.Map(TMapAccess.ReadWrite, LBitmapData) then
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
// - ---------------------------------------------------------------------------
{$ENDREGION}

{$REGION '- Canvas Registration'}

type
  { TSkCanvasService }

  TSkCanvasService = class(TInterfacedObject, IFMXCanvasService)
  strict private class var
    FDefaultPixelFormat: TPixelFormat;
  strict private
    FCanvasClass: TSkCanvasClass;
    FCurrent: IFMXCanvasService;
  {$IFDEF DEBUG}
  strict private
    FGlobalUseSkiaInRegistration: Boolean;
    FMainFormChangedMessageId: Integer;
    procedure MainFormChangedChangeHandler(const ASender: TObject; const AMessage: System.Messaging.TMessage);
  {$ENDIF}
  strict private
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    class function GetCanvasClass: TSkCanvasClass; static; inline;
  public
    constructor Create(const ACurrent: IFMXCanvasService);
    {$IFDEF DEBUG}
    destructor Destroy; override;
    {$ENDIF}
    class property DefaultPixelFormat: TPixelFormat read FDefaultPixelFormat;
  end;

{ TSkCanvasService }

constructor TSkCanvasService.Create(const ACurrent: IFMXCanvasService);
begin
  inherited Create;
  FCurrent := ACurrent;
  {$IFDEF DEBUG}
  FMainFormChangedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TMainFormChangedMessage, MainFormChangedChangeHandler);
  {$ENDIF}
end;

{$IFDEF DEBUG}

destructor TSkCanvasService.Destroy;
begin
  if FMainFormChangedMessageId > 0 then
    TMessageManager.DefaultManager.Unsubscribe(TMainFormChangedMessage, FMainFormChangedMessageId);
  inherited;
end;

{$ENDIF}

class function TSkCanvasService.GetCanvasClass: TSkCanvasClass;
begin
  {$IF DEFINED(MSWINDOWS)}
  if (GlobalUseSkiaRasterWhenAvailable) or (not TGrCanvasGL.Initialize) then
  begin
    if not TSkCanvasRasterWindows.Initialize then
      Exit(nil);
    Result := TSkCanvasRasterWindows
  end
  else
    Result := TGrCanvasGL;
  {$ELSE}
  {$IF DEFINED(MACOS)}
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
  Exit(nil);
  {$ENDIF}
  if not Result.Initialize then
    Result := nil;
  {$ENDIF}
end;

{$IFDEF DEBUG}

procedure TSkCanvasService.MainFormChangedChangeHandler(const ASender: TObject;
  const AMessage: System.Messaging.TMessage);
begin
  TMessageManager.DefaultManager.Unsubscribe(TMainFormChangedMessage, FMainFormChangedMessageId);
  FMainFormChangedMessageId := 0;
  if FGlobalUseSkiaInRegistration <> GlobalUseSkia then
  begin
    raise ESkCanvas.Create('Your declaration of GlobalUseSkia has no effect because the canvas service '+
      'has already been started. In this case, just create a unit in the project like "Project.Startup.pas", '+
      'place the GlobalUseSkia declaration in the initialization of this new unit, and declare this new unit '+
      'before any other unit of yours in the .dpr, that is, right after FMX.Forms.');
  end;
end;

{$ENDIF}

procedure TSkCanvasService.RegisterCanvasClasses;
begin
  if Assigned(FCurrent) then
  begin
    if GlobalUseSkia then
    begin
      FCanvasClass := GetCanvasClass;
      if Assigned(FCanvasClass) then
      begin
        // Ensuring that our canvas will be chosen as the default
        TCanvasManager.EnableSoftwareCanvas(True);
        TCanvasManager.RegisterCanvas(FCanvasClass, True, False);
        TTextLayoutManager.RegisterTextLayout(TSkTextLayout, FCanvasClass);
        if not GlobalDisableSkiaCodecsReplacement then
        begin
          TBitmapCodecManager.UnregisterBitmapCodecClass('.jpg');
          TBitmapCodecManager.RegisterBitmapCodecClass('.jpg', SVJPGImages, True, TSkBitmapHandleCodec);
          TBitmapCodecManager.UnregisterBitmapCodecClass('.jpeg');
          TBitmapCodecManager.RegisterBitmapCodecClass('.jpeg', SVJPGImages, True, TSkBitmapHandleCodec);
          TBitmapCodecManager.UnregisterBitmapCodecClass('.png');
          TBitmapCodecManager.RegisterBitmapCodecClass('.png', SVPNGImages, True, TSkBitmapHandleCodec);
        end;
        // Apply workarounds
        {$IF DEFINED(ANDROID) or DEFINED(IOS)}
        TRSP36957Workaround.Apply;
        {$ENDIF}
        {$IFDEF MACOS}
        TRSP37147Workaround.Apply;
        {$ENDIF}
        {$IFDEF MACOS}
        TRSP37829Workaround.Apply;
        {$ENDIF}
        {$IFDEF IOS}
        TRSP37660Workaround.Apply;
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
  if Assigned(FCurrent) then
  begin
    if Assigned(FCanvasClass) then
      FCanvasClass.Finalize;
    FCurrent.UnregisterCanvasClasses;
  end;
end;

var
  CanvasService: TSkCanvasService;
initialization
  CanvasService := TSkCanvasService.Create(IFMXCanvasService(TPlatformServices.Current.GetPlatformService(IFMXCanvasService)));
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, CanvasService);
{$ENDREGION}
end.
