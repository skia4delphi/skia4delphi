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
unit FMX.Skia.Canvas.Metal;

interface

{$SCOPEDENUMS ON}
{$HPPEMIT NOUSINGNAMESPACE}

{$IF DEFINED(MACOS)}
  {$DEFINE SKIA_METAL}
{$ENDIF}

{$IFDEF SKIA_METAL}

uses
  { Delphi }
  Macapi.Metal,

  { Skia }
  FMX.Skia.Canvas;

type
  { TMtlSharedContextCustom }

  TMtlSharedContextCustom = class abstract(TGrSharedContext)
  protected
    FCommandQueue: MTLCommandQueue;
    FDevice: MTLDevice;
  public
    property CommandQueue: MTLCommandQueue read FCommandQueue;
    property Device: MTLDevice read FDevice;
  end;

implementation

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,
  {$IFDEF IOS}
  iOSapi.CoreGraphics,
  FMX.Platform.iOS,
  {$ELSE}
  FMX.Platform.Mac,
  {$ENDIF}
  Macapi.MetalKit,
  Macapi.ObjectiveC,
  System.Math,

  { Skia }
  System.Skia;

type
  EMtlError = class(EGrCanvas);

  { TMtlCanvas }

  TMtlCanvas = class(TGrCanvas)
  private
    FBackBufferSurface: ISkSurface;
    FCurrentDrawable: CAMetalDrawable;
    FHasCreatedAnySurface: Boolean;
    procedure BeforeCreateFirstSurface;
  protected
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    function CreateSharedContext: IGrSharedContext; override;
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; override;
    procedure SwapBuffers(const AContextHandle: THandle); override;
  public
    destructor Destroy; override;
  end;

  { TMtlSharedContext }

  TMtlSharedContext = class(TMtlSharedContextCustom)
  protected
    procedure DestroyContext; override;
    function GetTextureColorType: TSkColorType; override;
    function GetTextureOrigin: TGrSurfaceOrigin; override;
  public
    constructor Create;
  end;

{ TMtlSharedContext }

constructor TMtlSharedContext.Create;
var
  LGrMtlBackendContext: TGrMtlBackendContext;
begin
  inherited;
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
      FDevice.retain;
      FCommandQueue.retain;
      FGrDirectContext := TGrDirectContext.MakeMetal(LGrMtlBackendContext);
    except
      FCommandQueue.release;
      raise;
    end;
  except
    FDevice.release;
    raise;
  end;
end;

procedure TMtlSharedContext.DestroyContext;
begin
  inherited;
  FCommandQueue.release;
  FDevice.release;
end;

function TMtlSharedContext.GetTextureColorType: TSkColorType;
begin
  Result := TSkColorType.BGRA8888;
end;

function TMtlSharedContext.GetTextureOrigin: TGrSurfaceOrigin;
begin
  Result := TGrSurfaceOrigin.TopLeft;
end;

{ TMtlCanvas }

procedure TMtlCanvas.BeforeCreateFirstSurface;
begin
{$IFDEF IOS}
  // Adjust the drawable size to prevent issues with the "Display Zoom" option
  {$IF CompilerVersion < 36}
  MTKView(WindowHandleToPlatform(Parent).View).setDrawableSize(CGSizeMake(Width * Scale, Height * Scale));
  {$ELSE}
  WindowHandleToPlatform(Parent).MTView.setDrawableSize(CGSizeMake(Width * Scale, Height * Scale));
  {$ENDIF}
{$ENDIF}
end;

constructor TMtlCanvas.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  FGrDirectContext := TGrSharedContext(SharedContext).GrDirectContext;
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
var
  LGrBackendRenderTarget: IGrBackendRenderTarget;
  LGrMtlTextureInfo: TGrMtlTextureInfo;
  LTexture: MTLTexture;
begin
  Result := nil;
  SharedContext.BeginContext;
  try
    {$IF CompilerVersion < 36}
    FCurrentDrawable := MTKView(WindowHandleToPlatform(Parent).View).currentDrawable;
    {$ELSEIF DEFINED(IOS)}
    FCurrentDrawable := WindowHandleToPlatform(Parent).MTView.currentDrawable;
    {$ELSE}
    FCurrentDrawable := TMacWindowHandle(Parent).CurrentMetalDrawable;
    {$ENDIF}
    if FCurrentDrawable = nil then
      Exit;
    if not FHasCreatedAnySurface then
    begin
      FHasCreatedAnySurface := True;
      BeforeCreateFirstSurface;
    end;
    LTexture := FCurrentDrawable.texture;
    LGrMtlTextureInfo.Texture := (LTexture as ILocalObject).GetObjectID;
    LGrBackendRenderTarget := TGrBackendRenderTarget.CreateMetal(Round(Width * Scale), Round(Height * Scale), LGrMtlTextureInfo);
    FBackBufferSurface     := TSkSurface.MakeFromRenderTarget(FGrDirectContext, LGrBackendRenderTarget, TGrSurfaceOrigin.TopLeft, TSkColorType.BGRA8888);
    FCurrentDrawable.retain;
    Result := TSkSurface(FBackBufferSurface);
  finally
    if Result = nil then
      SharedContext.EndContext;
  end;
end;

procedure TMtlCanvas.SwapBuffers(const AContextHandle: THandle);
{$IF (CompilerVersion < 36) or DEFINED(IOS)}
var
  LCommandBuffer: MTLCommandBuffer;
{$ENDIF}
begin
  inherited;
  FBackBufferSurface := nil;
  {$IF (CompilerVersion < 36) or DEFINED(IOS)}
  LCommandBuffer := TMtlSharedContext(SharedContext).CommandQueue.commandBuffer;
  LCommandBuffer.presentDrawable(FCurrentDrawable);
  LCommandBuffer.commit;
  {$ENDIF}
  FCurrentDrawable.release;
  SharedContext.EndContext;
end;

{$HPPEMIT END '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_SKIA_CANVAS_METAL)'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::Metal::TMtlSharedContextCustom;'}
{$HPPEMIT END '#endif'}

initialization
  RegisterSkiaRenderCanvas(TMtlCanvas, True,
    function: Boolean
    begin
      Result := GlobalUseMetal;
    end);
{$ELSE}
implementation
{$ENDIF}
end.
