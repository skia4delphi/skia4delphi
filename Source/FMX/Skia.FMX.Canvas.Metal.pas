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
unit Skia.FMX.Canvas.Metal;

interface

{$SCOPEDENUMS ON}

// The implementation of Metal is experimental.

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
  System.SysUtils,

  { Skia }
  Skia,
  Skia.FMX.Graphics;

type
  { TGrCanvasMetal }

  TGrCanvasMetal = class(TGrCanvasCustom)
  strict private class var
    FSharedDevice: MTLDevice;
  strict private
    FCommandQueue: MTLCommandQueue;
    FView: MTKView;
  strict protected
    function CreateDirectContext: IGrDirectContext; override;
    function CreateSurfaceFromWindow: ISkSurface; override;
    procedure FinalizeContext; override;
    procedure Flush; override;
    function InitializeContext: Boolean; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

implementation

{ TGrCanvasMetal }

function TGrCanvasMetal.CreateDirectContext: IGrDirectContext;
begin
  FSharedDevice.retain;
  FCommandQueue.retain;
  Result := TGrDirectContext.MakeMetal(TGrMtlBackendContext.Create((FSharedDevice as ILocalObject).GetObjectID, (FCommandQueue as ILocalObject).GetObjectID, nil));
end;

function TGrCanvasMetal.CreateSurfaceFromWindow: ISkSurface;
var
  LDrawable: CAMetalDrawable;
  LRenderTarget: IGrBackendRenderTarget;
begin
  LDrawable := FView.currentDrawable;
  if LDrawable = nil then
    Exit(nil);
  LRenderTarget := TGrBackendRenderTarget.CreateMetal(DrawableWidth, DrawableHeight, TGrMtlTextureInfo.Create((LDrawable.texture as ILocalObject).GetObjectID));
  Result        := TSkSurface.MakeFromRenderTarget(Context, LRenderTarget, TGrSurfaceOrigin.TopLeft, TSkColorType.BGRA8888);
end;

class procedure TGrCanvasMetal.Finalize;
begin
  inherited;
  FSharedDevice.release;
end;

procedure TGrCanvasMetal.FinalizeContext;
begin
  FView.release;
  FCommandQueue.release;
end;

procedure TGrCanvasMetal.Flush;
var
  LCommandBuffer: MTLCommandBuffer;
begin
  LCommandBuffer := FCommandQueue.commandBuffer;
  LCommandBuffer.presentDrawable(MTKView(WindowHandleToPlatform(Parent).View).currentDrawable);
  LCommandBuffer.commit;
end;

class procedure TGrCanvasMetal.Initialize;
begin
  FSharedDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
  if FSharedDevice = nil then
    RaiseLastOSError;
  inherited;
end;

function TGrCanvasMetal.InitializeContext: Boolean;
begin
  {$IFDEF IOS}
  if (not (Parent is TiOSWindowHandle)) or (TiOSWindowHandle(Parent).View = nil) or (not Supports(TiOSWindowHandle(Parent).View, MTKView, FView)) then
    Exit(False);
  {$ELSE}
  if (not (Parent is TMacWindowHandle)) or (TMacWindowHandle(Parent).View = nil) or (not Supports(TMacWindowHandle(Parent).View, MTKView, FView)) then
    Exit(False);
  {$ENDIF}
  FCommandQueue := FSharedDevice.newCommandQueue;
  FView.retain;
  FView.setDevice(FSharedDevice);
  Result := True;
end;

{$ELSE}
implementation
{$ENDIF}
end.
