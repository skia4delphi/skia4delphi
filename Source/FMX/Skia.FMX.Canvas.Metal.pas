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
  strict private
    FCommandQueue: MTLCommandQueue;
  strict protected
    function CreateContext: IGrDirectContext; override;
    procedure Flush; override;
    function GetRenderTarget: IGrBackendRenderTarget; override;
  public
    class function ColorType: TSkColorType; override;
    class function Origin: TGrSurfaceOrigin; override;
  end;

implementation

{ TGrCanvasMetal }

class function TGrCanvasMetal.ColorType: TSkColorType;
begin
  Result := TSkColorType.BGRA8888;
end;

function TGrCanvasMetal.CreateContext: IGrDirectContext;
var
  LDevice: MTLDevice;
  LView: MTKView;
begin
  {$IFDEF IOS}
  if (not (Parent is TiOSWindowHandle)) or (TiOSWindowHandle(Parent).View = nil) or (not Supports(TiOSWindowHandle(Parent).View, MTKView, LView)) then
    Exit(nil);
  {$ELSE}
  if (not (Parent is TMacWindowHandle)) or (TMacWindowHandle(Parent).View = nil) or (not Supports(TMacWindowHandle(Parent).View, MTKView, LView)) then
    Exit(nil);
  {$ENDIF}
  LDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
  if LDevice = nil then
    raise EGrCanvas.Create('Could not create Metal Device');
  LView.setDevice(LDevice);
  FCommandQueue := LDevice.newCommandQueue;
  Result        := TGrDirectContext.MakeMetal(TGrMtlBackendContext.Create((LDevice as ILocalObject).GetObjectID, (FCommandQueue as ILocalObject).GetObjectID, nil));
end;

procedure TGrCanvasMetal.Flush;
var
  LCommandBuffer: MTLCommandBuffer;
begin
  LCommandBuffer := FCommandQueue.commandBuffer;
  LCommandBuffer.presentDrawable(MTKView(WindowHandleToPlatform(Parent).View).currentDrawable);
  LCommandBuffer.commit;
end;

function TGrCanvasMetal.GetRenderTarget: IGrBackendRenderTarget;
begin
  Result := TGrBackendRenderTarget.CreateMetal(DrawableWidth, DrawableHeight, TGrMtlTextureInfo.Create((MTKView(WindowHandleToPlatform(Parent).View).currentDrawable.texture as ILocalObject).GetObjectID));
end;

class function TGrCanvasMetal.Origin: TGrSurfaceOrigin;
begin
  Result := TGrSurfaceOrigin.TopLeft;
end;

{$ELSE}
implementation
{$ENDIF}
end.
