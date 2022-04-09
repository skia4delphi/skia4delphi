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
    class procedure DoFinalize; override;
    class function DoInitialize: Boolean; override;
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

class procedure TGrCanvasMetal.DoFinalize;
begin
  FSharedDevice.release;
end;

class function TGrCanvasMetal.DoInitialize: Boolean;
begin
  FSharedDevice := TMTLDevice.Wrap(MTLCreateSystemDefaultDevice);
  Result        := FSharedDevice <> nil;
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
  {$REGION ' - Workaround RSP-37935'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround to a problem when Zoomed setting is enabled
  // -   on the OS then the form does not fit the screen
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-37935
  // -
  // - -------------------------------------------------------------------------
  {$IF CompilerVersion > 35}
    {$MESSAGE WARN 'Check if the issue has been fixed'}
  {$ENDIF}
  // - -------------------------------------------------------------------------
  var LSize: CGSize;
  LSize.width  := Width  * Scale;
  LSize.height := Height * Scale;
  FView.setDrawableSize(LSize);
  // - -------------------------------------------------------------------------
  {$ENDREGION}
  Result := True;
end;

{$ELSE}
implementation
{$ENDIF}
end.
