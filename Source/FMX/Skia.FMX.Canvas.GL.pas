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
unit Skia.FMX.Canvas.GL;

interface

{$SCOPEDENUMS ON}

{$IF DEFINED(MSWINDOWS) or DEFINED(ANDROID) or DEFINED(IOS)}

uses
  { Delphi }
  FMX.Types,
  {$IF DEFINED(MSWINDOWS)}
  Winapi.OpenGL,
  {$ELSEIF DEFINED(ANDROID)}
  Androidapi.Gles2,
  {$ELSEIF DEFINED(IOS)}
  iOSapi.OpenGLES,
  {$ENDIF}
  System.Generics.Collections,

  { Skia }
  Skia,
  Skia.FMX.Graphics;


type
  { IGrGlContext }

  IGrGlContext = interface
    ['{00129575-EB27-4220-8E3C-ACEA0EE42C4A}']
    procedure MakeCurrent;
    procedure MakeCurrentOffScreen;
    procedure SwapBuffers;
  end;

  { TGrGlContext<T> }

  TGrGlContext<T> = class abstract(TInterfacedObject, IGrGlContext)
  strict private
    FContext: T;
    FNativeWindow: THandle;
    procedure MakeCurrent;
    procedure MakeCurrentOffScreen;
    procedure SwapBuffers;
  strict protected
    class function DoCreateContext(const ANativeWindow: THandle): T; virtual; abstract;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; virtual; abstract;
    class procedure DoDestroyContext(const ANativeWindow: THandle; const AContext: T); virtual; abstract;
    class procedure DoDestroyNativeWindow(const ANativeWindow: THandle); virtual;
    class function DoGetCurrentContext: T; virtual; abstract;
    class procedure DoMakeCurrent(const AContext: T); virtual; abstract;
    class procedure DoMakeCurrentOffScreen(const AContext: T); virtual;
    class procedure DoSwapBuffers(const AContext: T); virtual;
  public
    constructor Create(const ANativeWindow: THandle; const AContext: T);
    destructor Destroy; override;
    class procedure CreateSharedResources; virtual;
    class procedure DestroySharedResources; virtual;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext;
  end;

  { TGrCanvasGl }

  TGrCanvasGl = class(TGrCanvasCustom)
  strict private
    FNativeContext: IGrGlContext;
    FWindowAttached: Boolean;
  strict protected
    function BeginWindow(const AContextHandle: THandle): ISkSurface; override;
    function CreateDirectContext: IGrDirectContext; override;
    function CreateSurfaceFromWindow: ISkSurface; override;
    procedure EndWindow; override;
    procedure FinalizeContext; override;
    procedure Flush; override;
    function InitializeContext: Boolean; override;
    procedure PrepareContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

implementation

uses
  { Delphi }

  {$IF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.OpenGLext,
  Winapi.Windows,
  {$ELSEIF DEFINED(ANDROID)}
  Androidapi.Egl,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.NativeWindow,
  Androidapi.NativeWindowJni,
  FMX.Platform.UI.Android,
  FMX.Presentation.Android.Style,
  {$ELSEIF DEFINED(IOS)}
  FMX.Platform.iOS,
  iOSapi.GLKit,
  Macapi.ObjectiveC,
  {$ENDIF}
  System.SysUtils;

type

{$IF DEFINED(MSWINDOWS)}

  { TGrGlWindowsContext }

  TGrGlWindowsContext = record
    DC: HDC;
    Context: HGLRC;
    constructor Create(const ADC: HDC; const AContext: HGLRC);
  end;

  { TGrGlWindows }

  TGrGlWindows = class(TGrGlContext<TGrGlWindowsContext>)
  strict protected
    class function DoCreateContext(const ANativeWindow: THandle): TGrGlWindowsContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindow: THandle; const AContext: TGrGlWindowsContext); override;
    class function DoGetCurrentContext: TGrGlWindowsContext; override;
    class procedure DoMakeCurrent(const AContext: TGrGlWindowsContext); override;
    class procedure DoSwapBuffers(const AContext: TGrGlWindowsContext); override;
  end;

  TGrGlNativeContext = TGrGlWindows;

{$ELSEIF DEFINED(ANDROID)}

  { TGrGlesAndroidContext }

  TGrGlesAndroidContext = record
    Display: EGLDisplay;
    SurfaceToDraw: EGLSurface;
    SurfaceToRead: EGLSurface;
    Context: EGLContext;
    constructor Create(const ADisplay: EGLDisplay; const ASurfaceToDraw, ASurfaceToRead: EGLSurface; const AContext: EGLContext);
  end;

  { TGrGlesAndroid }

  TGrGlesAndroid = class(TGrGlContext<TGrGlesAndroidContext>)
  strict private class var
    FSharedConfig: EGLConfig;
    FSharedDisplay: EGLDisplay;
  strict private
    class procedure RaiseLastError; inline;
  strict protected
    class function DoCreateContext(const ANativeWindow: THandle): TGrGlesAndroidContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindow: THandle; const AContext: TGrGlesAndroidContext); override;
    class procedure DoDestroyNativeWindow(const ANativeWindow: THandle); override;
    class function DoGetCurrentContext: TGrGlesAndroidContext; override;
    class procedure DoMakeCurrent(const AContext: TGrGlesAndroidContext); override;
    class procedure DoMakeCurrentOffScreen(const AContext: TGrGlesAndroidContext); override;
    class procedure DoSwapBuffers(const AContext: TGrGlesAndroidContext); override;
  public
    class procedure CreateSharedResources; override;
    class procedure DestroySharedResources; override;
  end;

  TGrGlNativeContext = TGrGlesAndroid;

{$ELSEIF DEFINED(IOS)}

  { TGrGlesIOS }

  TGrGlesIOS = class(TGrGlContext<EAGLContext>)
  strict private class var
    FLibModule: HMODULE;
  strict protected
    class function DoCreateContext(const ANativeWindow: THandle): EAGLContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindow: THandle; const AContext: EAGLContext); override;
    class procedure DoDestroyNativeWindow(const ANativeWindow: THandle); override;
    class function DoGetCurrentContext: EAGLContext; override;
    class procedure DoMakeCurrent(const AContext: EAGLContext); override;
  public
    class procedure CreateSharedResources; override;
    class procedure DestroySharedResources; override;
  end;

  TGrGlNativeContext = TGrGlesIOS;

{$ENDIF}

{$IF DEFINED(MSWINDOWS)}

{ TGrGlWindowsContext }

constructor TGrGlWindowsContext.Create(const ADC: HDC; const AContext: HGLRC);
begin
  Context := AContext;
  DC      := ADC;
end;

{ TGrGlWindows }

class function TGrGlWindows.DoCreateContext(
  const ANativeWindow: THandle): TGrGlWindowsContext;
const
  PixelFormatDescriptor: TPixelFormatDescriptor = (
    nSize           : SizeOf(TPixelFormatDescriptor);
    nVersion        : 1;
    dwFlags         : PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType      : PFD_TYPE_RGBA;
    cColorBits      : 32;
    cRedBits        : 0;
    cRedShift       : 0;
    cGreenBits      : 0;
    cGreenShift     : 0;
    cBlueBits       : 0;
    cBlueShift      : 0;
    cAlphaBits      : 0;
    cAlphaShift     : 0;
    cAccumBits      : 0;
    cAccumRedBits   : 0;
    cAccumGreenBits : 0;
    cAccumBlueBits  : 0;
    cAccumAlphaBits : 0;
    cDepthBits      : 0;
    cStencilBits    : 8;
    cAuxBuffers     : 0;
    iLayerType      : PFD_MAIN_PLANE;
    bReserved       : 0;
    dwLayerMask     : 0;
    dwVisibleMask   : 0;
    dwDamageMask    : 0);
var
  LContext: HGLRC;
  LDC: HDC;
  LPixelFormat: Integer;
begin
  LDC := GetDC(ANativeWindow);
  if LDC = 0 then
    RaiseLastOSError;
  try
    LPixelFormat := ChoosePixelFormat(LDC, @PixelFormatDescriptor);
    if (LPixelFormat = 0) or (not SetPixelFormat(LDC, LPixelFormat, @PixelFormatDescriptor)) then
      RaiseLastOSError;
    LContext := wglCreateContext(LDC);
    if LContext = 0 then
      RaiseLastOSError;
    Result := TGrGlWindowsContext.Create(LDC, LContext);
  except
    ReleaseDC(ANativeWindow, LDC);
    raise;
  end;
end;

class function TGrGlWindows.DoCreateNativeWindow(
  const AWindow: TWindowHandle): THandle;
begin
  if not (AWindow is TWinWindowHandle) then
    Exit(0);
  Result := TWinWindowHandle(AWindow).Wnd;
end;

class procedure TGrGlWindows.DoDestroyContext(const ANativeWindow: THandle;
  const AContext: TGrGlWindowsContext);
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(AContext.Context);
  ReleaseDC(ANativeWindow, AContext.DC);
end;

class function TGrGlWindows.DoGetCurrentContext: TGrGlWindowsContext;
begin
  Result := TGrGlWindowsContext.Create(wglGetCurrentDC, wglGetCurrentContext);
end;

class procedure TGrGlWindows.DoMakeCurrent(const AContext: TGrGlWindowsContext);
begin
  if not wglMakeCurrent(AContext.DC, AContext.Context) then
    RaiseLastOSError;
end;

class procedure TGrGlWindows.DoSwapBuffers(const AContext: TGrGlWindowsContext);
begin
  if not Winapi.Windows.SwapBuffers(AContext.DC) then
    RaiseLastOSError;
end;

{$ELSEIF DEFINED(ANDROID)}

{ TGrGlesAndroidContext }

constructor TGrGlesAndroidContext.Create(const ADisplay: EGLDisplay;
  const ASurfaceToDraw, ASurfaceToRead: EGLSurface; const AContext: EGLContext);
begin
  Display       := ADisplay;
  SurfaceToDraw := ASurfaceToDraw;
  SurfaceToRead := ASurfaceToRead;
  Context       := AContext;
end;

{ TGrGlesAndroid }

class procedure TGrGlesAndroid.CreateSharedResources;
const
  ConfigAttributes: array[0..16] of EGLint = (
    EGL_SURFACE_TYPE    , EGL_WINDOW_BIT     ,
    EGL_RENDERABLE_TYPE , EGL_OPENGL_ES2_BIT ,
    EGL_RED_SIZE        , 8                  ,
    EGL_GREEN_SIZE      , 8                  ,
    EGL_BLUE_SIZE       , 8                  ,
    EGL_ALPHA_SIZE      , 8                  ,
    EGL_DEPTH_SIZE      , 0                  ,
    EGL_STENCIL_SIZE    , 8                  ,
    EGL_NONE);

  SurfaceAttributes: array[0..4] of EGLint = (EGL_WIDTH, 1, EGL_HEIGHT, 1, EGL_NONE);
var
  LNumConfig: EGLint;
begin
  FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FSharedDisplay = EGL_NO_DISPLAY then
    RaiseLastError;
  if eglInitialize(FSharedDisplay, nil, nil) = EGL_FALSE then
    RaiseLastError;
  try
    if eglChooseConfig(FSharedDisplay, @ConfigAttributes[0], @FSharedConfig, 1, @LNumConfig) = EGL_FALSE then
      RaiseLastError;
  except
    eglTerminate(FSharedDisplay);
    raise;
  end;
end;

class procedure TGrGlesAndroid.DestroySharedResources;
begin
  eglTerminate(FSharedDisplay);
end;

class function TGrGlesAndroid.DoCreateContext(
  const ANativeWindow: THandle): TGrGlesAndroidContext;
const
  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
var
  LContext: EGLContext;
  LSurface: EGLSurface;
begin
  LContext := eglCreateContext(FSharedDisplay, FSharedConfig, EGL_NO_CONTEXT, @ContextAttributes[0]);
  if LContext = EGL_NO_CONTEXT then
    RaiseLastError;
  try
    LSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, PANativeWindow(ANativeWindow), nil);
    if LSurface = EGL_NO_SURFACE then
      RaiseLastError;
    Result := TGrGlesAndroidContext.Create(FSharedDisplay, LSurface, LSurface, LContext);
  except
    eglDestroyContext(FSharedDisplay, LContext);
    raise;
  end;
end;

class function TGrGlesAndroid.DoCreateNativeWindow(
  const AWindow: TWindowHandle): THandle;
var
  LANativeWindow: PANativeWindow;
  LANativeWindowSurface: JSurface;
  LANativeWindowSurfaceID: Pointer;
begin
  if AWindow is TAndroidWindowHandle then
  begin
    if TAndroidWindowHandle(AWindow).Holder = nil then
      Exit(0);
    LANativeWindowSurfaceID := (TAndroidWindowHandle(AWindow).Holder.getSurface as ILocalObject).GetObjectID;
  end
  else if AWindow is TAndroidHandle then
  begin
    if TAndroidHandle(AWindow).Surface = nil then
      Exit(0);
    LANativeWindowSurface   := TJSurface.JavaClass.init(TAndroidHandle(AWindow).Surface);
    LANativeWindowSurfaceID := TJNIResolver.JavaInstanceToID(LANativeWindowSurface);
  end
  else
    Exit(0);
  LANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, LANativeWindowSurfaceID);
  if LANativeWindow = nil then
    RaiseLastOSError;
  try
    if ANativeWindow_setBuffersGeometry(LANativeWindow, 0, 0, WINDOW_FORMAT_RGBA_8888) <> 0 then
      RaiseLastOSError;
  except
    ANativeWindow_release(LANativeWindow);
    raise;
  end;
  Result := THandle(LANativeWindow);
end;

class procedure TGrGlesAndroid.DoDestroyContext(const ANativeWindow: THandle;
  const AContext: TGrGlesAndroidContext);
begin
  eglMakeCurrent(AContext.Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
  if AContext.SurfaceToDraw = AContext.SurfaceToRead then
    eglDestroySurface(AContext.Display, AContext.SurfaceToDraw)
  else
  begin
    eglDestroySurface(AContext.Display, AContext.SurfaceToDraw);
    eglDestroySurface(AContext.Display, AContext.SurfaceToRead);
  end;
  eglDestroyContext(AContext.Display, AContext.Context);
end;

class procedure TGrGlesAndroid.DoDestroyNativeWindow(
  const ANativeWindow: THandle);
begin
  ANativeWindow_release(PANativeWindow(ANativeWindow));
end;

class function TGrGlesAndroid.DoGetCurrentContext: TGrGlesAndroidContext;
begin
  Result := TGrGlesAndroidContext.Create(eglGetCurrentDisplay, eglGetCurrentSurface(EGL_DRAW), eglGetCurrentSurface(EGL_READ), eglGetCurrentContext);
end;

class procedure TGrGlesAndroid.DoMakeCurrent(
  const AContext: TGrGlesAndroidContext);
begin
  if eglMakeCurrent(AContext.Display, AContext.SurfaceToDraw, AContext.SurfaceToRead, AContext.Context) = EGL_FALSE then
    RaiseLastError;
end;

class procedure TGrGlesAndroid.DoMakeCurrentOffScreen(
  const AContext: TGrGlesAndroidContext);
begin
  if eglMakeCurrent(AContext.Display, EGL_NO_SURFACE, EGL_NO_SURFACE, AContext.Context) = EGL_FALSE then
    RaiseLastError;
end;

class procedure TGrGlesAndroid.DoSwapBuffers(
  const AContext: TGrGlesAndroidContext);
begin
  if eglSwapBuffers(AContext.Display, AContext.SurfaceToDraw) = EGL_FALSE then
    RaiseLastError;
end;

class procedure TGrGlesAndroid.RaiseLastError;
var
  LError: EGLint;
begin
  LError := eglGetError;
  if LError = EGL_SUCCESS then
    raise EGrCanvas.Create('Unknown OpenGLES error')
  else
    raise EGrCanvas.CreateFmt('OpenGLES error: $%x', [LError]);
end;

{$ELSEIF DEFINED(IOS)}

{ TGrGlesIOS }

class procedure TGrGlesIOS.CreateSharedResources;
begin
  FLibModule := LoadLibrary(PChar(libGLKit));
  if FLibModule = 0 then
    RaiseLastOSError;
end;

class procedure TGrGlesIOS.DestroySharedResources;
begin
  FreeLibrary(FLibModule);
end;

class function TGrGlesIOS.DoCreateContext(
  const ANativeWindow: THandle): EAGLContext;
var
  LView: GLKView;
begin
  LView  := TGLKView.Wrap(Pointer(ANativeWindow));
  Result := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
  LView.setContext(Result);
end;

class function TGrGlesIOS.DoCreateNativeWindow(
  const AWindow: TWindowHandle): THandle;
var
  LView: GLKView;
begin
  if (not (AWindow is TiOSWindowHandle)) or (TiOSWindowHandle(AWindow).View = nil) or (not Supports(TiOSWindowHandle(AWindow).View, GLKView, LView)) then
    Exit(0);
  Result := THandle((LView as ILocalObject).GetObjectID);
  LView.retain;
end;

class procedure TGrGlesIOS.DoDestroyContext(const ANativeWindow: THandle;
  const AContext: EAGLContext);
begin
  TEAGLContext.OCClass.setCurrentContext(nil);
  AContext.release;
end;

class procedure TGrGlesIOS.DoDestroyNativeWindow(const ANativeWindow: THandle);
var
  LView: GLKView;
begin
  LView := TGLKView.Wrap(Pointer(ANativeWindow));
  LView.release;
end;

class function TGrGlesIOS.DoGetCurrentContext: EAGLContext;
begin
  Result := TEAGLContext.Wrap(TEAGLContext.OCClass.currentContext);
end;

class procedure TGrGlesIOS.DoMakeCurrent(const AContext: EAGLContext);
begin
  if not TEAGLContext.OCClass.setCurrentContext(AContext) then
    RaiseLastOSError;
end;

{$ENDIF}

{ TGrGlContext<T> }

constructor TGrGlContext<T>.Create(const ANativeWindow: THandle;
  const AContext: T);
begin
  FNativeWindow := ANativeWindow;
  FContext      := AContext;
end;

class procedure TGrGlContext<T>.CreateSharedResources;
begin
end;

destructor TGrGlContext<T>.Destroy;
begin
  DoDestroyContext(FNativeWindow, FContext);
  DoDestroyNativeWindow(FNativeWindow);
  inherited;
end;

class procedure TGrGlContext<T>.DestroySharedResources;
begin
end;

class procedure TGrGlContext<T>.DoDestroyNativeWindow(
  const ANativeWindow: THandle);
begin
end;

class procedure TGrGlContext<T>.DoMakeCurrentOffScreen(const AContext: T);
begin
  DoMakeCurrent(AContext);
end;

class procedure TGrGlContext<T>.DoSwapBuffers(const AContext: T);
begin
end;

procedure TGrGlContext<T>.MakeCurrent;
begin
  DoMakeCurrent(FContext);
end;

procedure TGrGlContext<T>.MakeCurrentOffScreen;
begin
  DoMakeCurrentOffScreen(FContext);
end;

class function TGrGlContext<T>.MakeFromWindow(
  const AWindow: TWindowHandle): IGrGlContext;
var
  LContext: T;
  LNativeHandle: THandle;
begin
  LNativeHandle := DoCreateNativeWindow(AWindow);
  if LNativeHandle = 0 then
    Exit(nil);
  try
    LContext := DoCreateContext(LNativeHandle);
  except
    DoDestroyNativeWindow(LNativeHandle);
    raise;
  end;
  Result := Create(LNativeHandle, LContext);
end;

procedure TGrGlContext<T>.SwapBuffers;
begin
  DoSwapBuffers(FContext);
end;

{ TGrCanvasGl }

function TGrCanvasGl.BeginWindow(const AContextHandle: THandle): ISkSurface;
begin
  FWindowAttached := True;
  try
    Result := inherited;
  finally
    FWindowAttached := Assigned(Result);
  end;
end;

function TGrCanvasGl.CreateDirectContext: IGrDirectContext;
begin
  Result := TGrDirectContext.MakeGl;
end;

function TGrCanvasGl.CreateSurfaceFromWindow: ISkSurface;
var
  LFramebuffer: GLuint;
  LMaxSamples: Integer;
  LRenderTarget: IGrBackendRenderTarget;
  LSamples: GLint;
  LStencilBits: GLint;
begin
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @LFramebuffer);
  glGetIntegerv(GL_STENCIL_BITS, @LStencilBits);
  glGetIntegerv(GL_SAMPLES, @LSamples);
  LMaxSamples := Context.GetMaxSurfaceSampleCountForColorType(TSkColorType.RGBA8888);
  if LSamples > LMaxSamples then
    LSamples := LMaxSamples;
  LRenderTarget := TGrBackendRenderTarget.CreateGl(DrawableWidth, DrawableHeight, LSamples, LStencilBits, TGrGlFramebufferInfo.Create(LFramebuffer, GrGlSizedFormat[TSkColorType.RGBA8888]));
  Result        := TSkSurface.MakeFromRenderTarget(Context, LRenderTarget, TGrSurfaceOrigin.BottomLeft, TSkColorType.RGBA8888);
end;

procedure TGrCanvasGl.EndWindow;
begin
  inherited;
  FWindowAttached := False;
end;

class procedure TGrCanvasGl.Finalize;
begin
  inherited;
  TGrGlNativeContext.DestroySharedResources;
end;

procedure TGrCanvasGl.FinalizeContext;
begin
  FNativeContext := nil;
end;

procedure TGrCanvasGl.Flush;
begin
  FNativeContext.SwapBuffers;
end;

class procedure TGrCanvasGl.Initialize;
begin
  TGrGlNativeContext.CreateSharedResources;
  inherited;
end;

function TGrCanvasGl.InitializeContext: Boolean;
begin
  FNativeContext := TGrGlNativeContext.MakeFromWindow(Parent);
  Result         := Assigned(FNativeContext);
end;

procedure TGrCanvasGl.PrepareContext;
begin
  if FWindowAttached then
    FNativeContext.MakeCurrent
  else
    FNativeContext.MakeCurrentOffScreen;
end;

{$ELSE}
implementation
{$ENDIF}
end.
