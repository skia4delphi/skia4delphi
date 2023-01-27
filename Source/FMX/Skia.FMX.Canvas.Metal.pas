{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2023 Google LLC.                                    }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.FMX.Canvas.Metal;

interface

{$SCOPEDENUMS ON}

{$IFDEF MACOS}

uses
  { Delphi }
  {$IFDEF IOS}
  iOSapi.CoreGraphics,
  FMX.Platform.iOS,
  {$ELSE}
  Macapi.CocoaTypes,
  FMX.Platform.Mac,
  {$ENDIF}
  Macapi.Metal,
  Macapi.MetalKit,
  Macapi.ObjectiveC,
  System.SysUtils,

  { Skia }
  Skia,
  Skia.FMX.Graphics;

type
  { TMtlSharedResources }

  TMtlSharedResources = class(TGrCanvasSharedResources)
  strict protected
    procedure InitializeContext(out ASharedGrDirectContext: IGrDirectContext); override;
  end;

  { TMtlCanvas }

  TMtlCanvas = class(TGrCanvas)
  strict private
    FCommandQueue: MTLCommandQueue;
    FSampleCount: Integer;
  strict protected
    function CreateSurfaceFromWindow(var AGrDirectContext: IGrDirectContext): ISkSurface; override;
    procedure DoEndScene; override;
    class procedure InitializeSharedResources(out ASharedResources: IGrCanvasSharedResources); override;
  end;

{$ENDIF}

implementation

{$IFDEF MACOS}

uses
  { Delphi }
  FMX.Graphics;

{ TMtlSharedResources }

procedure TMtlSharedResources.InitializeContext(
  out ASharedGrDirectContext: IGrDirectContext);
var
  LCommandQueue: MTLCommandQueue;
  LDevice: MTLDevice;
  LGrMtlBackendContext: TGrMtlBackendContext;
begin
  LDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
  if LDevice = nil then
    raise EGrCanvas.Create('Could not get the default device instance Metal.');
  try
    LCommandQueue := LDevice.newCommandQueue;
    if LCommandQueue = nil then
      raise EGrCanvas.Create('Could not create the shared command queue.');
    try
      LGrMtlBackendContext.Device        := (LDevice as ILocalObject).GetObjectID;
      LGrMtlBackendContext.Queue         := (LCommandQueue as ILocalObject).GetObjectID;
      LGrMtlBackendContext.BinaryArchive := nil;
      // The direct context will take ownership of the device and the queue.
      ASharedGrDirectContext := TGrDirectContext.MakeMetal(LGrMtlBackendContext);
    except
      LCommandQueue.release;
      raise;
    end;
  except
    LDevice.release;
    raise;
  end;
end;

{ TMtlCanvas }

function TMtlCanvas.CreateSurfaceFromWindow(
  var AGrDirectContext: IGrDirectContext): ISkSurface;
var
  LDevice: MTLDevice;
  LGrMtlBackendContext: TGrMtlBackendContext;
begin
  if not Assigned(AGrDirectContext) then
  begin
    if WindowHandleToPlatform(Parent).View = nil then
      Exit(nil);
    LDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
    try
      MTKView(WindowHandleToPlatform(Parent).View).setDevice(LDevice);
      MTKView(WindowHandleToPlatform(Parent).View).setFramebufferOnly(False);
      {$REGION ' - Workaround RSP-37935'}
      // - ---------------------------------------------------------------------
      // - WORKAROUND
      // - ---------------------------------------------------------------------
      // -
      // - Description:
      // -   This code is a workaround to a problem when Zoomed setting is
      // -   enabled on the OS then the form does not fit the screen.
      // -
      // - Bug report:
      // -   https://quality.embarcadero.com/browse/RSP-37935
      // -
      // - ---------------------------------------------------------------------
      {$IF (CompilerVersion < 35) or ((CompilerVersion = 35) and not DECLARED(RTLVersion112))}
      var LSize: CGSize;
      LSize.width  := Width  * Scale;
      LSize.height := Height * Scale;
      MTKView(WindowHandleToPlatform(Parent).View).setDrawableSize(LSize);
      {$ENDIF}
      // - ---------------------------------------------------------------------
      {$ENDREGION}
      FCommandQueue := LDevice.newCommandQueue;
      if FCommandQueue = nil then
        raise EGrCanvas.Create('Could not create a command queue.');
      try
        LGrMtlBackendContext.Device        := (LDevice as ILocalObject).GetObjectID;
        LGrMtlBackendContext.Queue         := (FCommandQueue as ILocalObject).GetObjectID;
        LGrMtlBackendContext.BinaryArchive := nil;
        // The direct context will take ownership of the device and the queue.
        AGrDirectContext := TGrDirectContext.MakeMetal(LGrMtlBackendContext);
      except
        FCommandQueue.release;
        raise;
      end;
    except
      LDevice.release;
      raise;
    end;
    if Quality = TCanvasQuality.HighQuality then
      FSampleCount := GrDirectContext.GetMaxSurfaceSampleCountForColorType(TSkColorType.BGRA8888)
    else
      FSampleCount := 1;
  end;
  Result := TSkSurface.MakeFromMTKView(AGrDirectContext, (MTKView(WindowHandleToPlatform(Parent).View) as ILocalObject).GetObjectID, TGrSurfaceOrigin.TopLeft, FSampleCount, TSkColorType.BGRA8888);
end;

procedure TMtlCanvas.DoEndScene;
var
  LCommandBuffer: MTLCommandBuffer;
begin
  inherited;
  if Parent <> nil then
  begin
    LCommandBuffer := FCommandQueue.commandBuffer;
    LCommandBuffer.presentDrawable(MTKView(WindowHandleToPlatform(Parent).View).currentDrawable);
    LCommandBuffer.commit;
  end;
end;

class procedure TMtlCanvas.InitializeSharedResources(
  out ASharedResources: IGrCanvasSharedResources);
begin
  ASharedResources := TMtlSharedResources.Create;
end;

{$ENDIF}

{$HPPEMIT NOUSINGNAMESPACE}
(*$HPPEMIT 'namespace Skia {'*)
(*$HPPEMIT '	namespace Fmx {'*)
(*$HPPEMIT '	  namespace Platform {'*)
{$IFDEF IOS}
  (*$HPPEMIT '		  namespace Ios { using namespace ::Fmx::Platform::Ios; }'*)
{$ELSE}
  (*$HPPEMIT '		  namespace Mac { using namespace ::Fmx::Platform::Mac; }'*)
{$ENDIF}
(*$HPPEMIT '	  }'*)
(*$HPPEMIT '	}'*)
(*$HPPEMIT '}'*)
end.
