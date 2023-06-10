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
unit Skia.FMX.Canvas.Metal;

interface

{$SCOPEDENUMS ON}
{$HPPEMIT NOUSINGNAMESPACE}

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,

  { Skia }
  Skia,
  Skia.FMX.Graphics;

type
  EMtlError = class(EGrCanvas);

  { TMtlCanvas }

  TMtlCanvas = class(TGrCanvas)

  {$REGION ' - Workaround RSP-37935'}
  // - --------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround to a problem when Zoomed setting is enabled
  // -   on the OS then the form does not fit the screen.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-37935
  // -
  // - -------------------------------------------------------------------------
  {$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
  {$IFDEF MACOS}
  strict private type
    { TRSP37935Workaround }

    TRSP37935Workaround = record
      class procedure Apply(const ACanvas: TCanvas; const AParent: TWindowHandle); static; inline;
    end;

  {$ENDIF}
  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$ENDREGION}

  strict private
    FBackBufferSurface: ISkSurface;
  strict protected
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    function CreateSharedContext: IGrSharedContext; override;
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; override;
    {$IF DECLARED(TRSP37935Workaround)}
    procedure Resized; override;
    {$ENDIF}
    procedure SwapBuffers; override;
  public
    destructor Destroy; override;
  end;

implementation

{$IFDEF MACOS}

uses
  { Delphi }
  {$IFDEF IOS}
  FMX.Platform.iOS,
  {$ELSE}
  FMX.Platform.Mac,
  {$ENDIF}
  Macapi.Metal,
  Macapi.MetalKit,
  Macapi.ObjectiveC,
  System.Math,

  { Workarounds }
  {$IFDEF IOS}
  iOSapi.CoreGraphics;
  {$ELSE}
  Macapi.CocoaTypes;
  {$ENDIF}

{$ENDIF}

type
  { TMtlSharedContext }

  TMtlSharedContext = class(TGrSharedContext)
  strict private
    {$IFDEF MACOS}
    FCommandQueue: MTLCommandQueue;
    FDevice: MTLDevice;
    {$ENDIF}
  public
    constructor Create;
    {$IFDEF MACOS}
    property CommandQueue: MTLCommandQueue read FCommandQueue;
    property Device: MTLDevice read FDevice;
    {$ENDIF}
  end;

{ TMtlSharedContext }

constructor TMtlSharedContext.Create;
{$IFDEF MACOS}
var
  LGrMtlBackendContext: TGrMtlBackendContext;
{$ENDIF}
begin
  inherited;
  {$IFDEF MACOS}
  FDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
  if FDevice = nil then
    raise EMtlError.Create('Could not get the default device instance Metal.');
  try
    FCommandQueue := FDevice.newCommandQueue;
    if FCommandQueue = nil then
      raise EMtlError.Create('Could not create the shared command queue.');
    try
      LGrMtlBackendContext.Device        := (FDevice as ILocalObject).GetObjectID;
      LGrMtlBackendContext.Queue         := (FCommandQueue as ILocalObject).GetObjectID;
      LGrMtlBackendContext.BinaryArchive := nil;
      // The direct context will take ownership of the device and the queue.
      FGrDirectContext := TGrDirectContext.MakeMetal(LGrMtlBackendContext);
      if FGrDirectContext = nil then
        raise EGrCanvas.Create('Could not create shared direct context.');
    except
      FCommandQueue.release;
      raise;
    end;
  except
    FDevice.release;
    raise;
  end;
  {$ENDIF}
end;

{ TMtlCanvas }

constructor TMtlCanvas.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  FGrDirectContext := TGrSharedContext(SharedContext).GrDirectContext;
  {$IFDEF MACOS}
  MTKView(WindowHandleToPlatform(Parent).View).setDevice(TMtlSharedContext(SharedContext).Device);
  MTKView(WindowHandleToPlatform(Parent).View).setFramebufferOnly(False);
  {$ENDIF}
  {$IF DECLARED(TRSP37935Workaround)}
  TRSP37935Workaround.Apply(Self, Parent);
  {$ENDIF}
end;

function TMtlCanvas.CreateSharedContext: IGrSharedContext;
begin
  Result := TMtlSharedContext.Create;
end;

destructor TMtlCanvas.Destroy;
begin
  if Parent <> nil then
  begin
    FBackBufferSurface := nil;
    FGrDirectContext   := nil;
  end;
  inherited;
end;

function TMtlCanvas.GetSurfaceFromWindow(
  const AContextHandle: THandle): TSkSurface;
{$IFDEF MACOS}
var
  LColorType: TSkColorType;
  LSampleCount: Integer;
{$ENDIF}
begin
  Result := nil;
  SharedContext.BeginContext;
  try
    {$IFDEF MACOS}
    case MTKView(WindowHandleToPlatform(Parent).View).colorPixelFormat of
      MTLPixelFormatRGBA8Unorm,
      MTLPixelFormatRGBA8Unorm_sRGB: LColorType := TSkColorType.RGBA8888;
      MTLPixelFormatBGRA8Unorm,
      MTLPixelFormatBGRA8Unorm_sRGB: LColorType := TSkColorType.BGRA8888;
    else
      Exit;
    end;
    case Quality of
      TCanvasQuality.SystemDefault: LSampleCount := Min(FGrDirectContext.GetMaxSurfaceSampleCountForColorType(LColorType), 2);
      TCanvasQuality.HighQuality  : LSampleCount := Min(FGrDirectContext.GetMaxSurfaceSampleCountForColorType(LColorType), 4);
    else
      LSampleCount := 1;
    end;
    FBackBufferSurface := TSkSurface.MakeFromMTKView(FGrDirectContext, (MTKView(WindowHandleToPlatform(Parent).View) as ILocalObject).GetObjectID, TGrSurfaceOrigin.TopLeft, LSampleCount, LColorType);
    {$ENDIF}
    Result := TSkSurface(FBackBufferSurface);
  finally
    if Result = nil then
      SharedContext.EndContext;
  end;
end;

{$IF DECLARED(TRSP37935Workaround)}

procedure TMtlCanvas.Resized;
begin
  inherited;
  if Parent <> nil then
    TRSP37935Workaround.Apply(Self, Parent);
  end;
end;

{$ENDIF}

procedure TMtlCanvas.SwapBuffers;
{$IFDEF MACOS}
var
  LCommandBuffer: MTLCommandBuffer;
{$ENDIF}
begin
  inherited;
  FBackBufferSurface := nil;
  {$IFDEF MACOS}
  LCommandBuffer := TMtlSharedContext(SharedContext).CommandQueue.commandBuffer;
  LCommandBuffer.presentDrawable(MTKView(WindowHandleToPlatform(Parent).View).currentDrawable);
  LCommandBuffer.commit;
  {$ENDIF}
  SharedContext.EndContext;
end;

{$REGION ' - Workaround RSP-37935'}
// - ---------------------------------------------------------------------------
// - WORKAROUND
// - ---------------------------------------------------------------------------
{$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
{$IFDEF MACOS}

{ TMtlCanvas.TRSP37935Workaround }

class procedure TMtlCanvas.TRSP37935Workaround.Apply(const ACanvas: TCanvas;
  const AParent: TWindowHandle);
var
  LSize: CGSize;
begin
  LSize.width  := ACanvas.Width  * ACanvas.Scale;
  LSize.height := ACanvas.Height * ACanvas.Scale;
  MTKView(WindowHandleToPlatform(AParent).View).setDrawableSize(LSize);
end;

{$ENDIF}
{$ENDIF}
// - ---------------------------------------------------------------------------
{$ENDREGION}

end.
