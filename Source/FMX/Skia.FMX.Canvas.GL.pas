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
  end;

  { TGrGlContext<T> }

  TGrGlContext<T> = class abstract(TInterfacedObject, IGrGlContext)
  strict private class var
    FGlInterface: IGrGlInterface;
  strict private
    FContext: T;
    FNativeWindowHandle: THandle;
    procedure MakeCurrent;
    procedure MakeCurrentOffScreen;
  strict protected
    class function DoCreateContext(const ANativeWindowHandle: THandle): T; virtual; abstract;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; virtual; abstract;
    class procedure DoDestroyContext(const ANativeWindowHandle: THandle; const AContext: T); virtual; abstract;
    class procedure DoDestroyNativeWindow(const ANativeWindowHandle: THandle); virtual;
    class procedure DoFinalize; virtual; abstract;
    class function DoInitialize: IGrGlInterface; virtual; abstract;
    class procedure DoMakeCurrent(const AContext: T); virtual; abstract;
    class procedure DoMakeCurrentOffScreen(const AContext: T); virtual;
  public
    constructor Create(const ANativeWindowHandle: THandle; const AContext: T);
    destructor Destroy; override;
    class procedure Finalize;
    class function Initialize: Boolean;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext;
    class procedure SwapBuffers; virtual;
    class property GlInterface: IGrGlInterface read FGlInterface;
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
    class procedure DoFinalize; override;
    class function DoInitialize: Boolean; override;
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
  strict private const
    PixelFormatDescriptor: TPixelFormatDescriptor = (
      nSize           : SizeOf(TPixelFormatDescriptor);
      nVersion        : 1;
      dwFlags         : PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
      iPixelType      : PFD_TYPE_RGBA;
      cColorBits      : 24;
      cRedBits        : 0;
      cRedShift       : 0;
      cGreenBits      : 0;
      cGreenShift     : 0;
      cBlueBits       : 0;
      cBlueShift      : 0;
      cAlphaBits      : 8;
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
  strict private class var
    FLibModule: HMODULE;
    FSharedPixelFormat: Integer;
  strict protected
    class function DoCreateContext(const ANativeWindowHandle: THandle): TGrGlWindowsContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindowHandle: THandle; const AContext: TGrGlWindowsContext); override;
    class procedure DoFinalize; override;
    class function DoInitialize: IGrGlInterface; override;
    class procedure DoMakeCurrent(const AContext: TGrGlWindowsContext); override;
  public
    class procedure SwapBuffers; override;
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
  strict private const
    ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
  strict private class var
    FSharedConfig: EGLConfig;
    FSharedDisplay: EGLDisplay;
  strict private
    class procedure RaiseLastError; inline;
  strict protected
    class function DoCreateContext(const ANativeWindowHandle: THandle): TGrGlesAndroidContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindowHandle: THandle; const AContext: TGrGlesAndroidContext); override;
    class procedure DoDestroyNativeWindow(const ANativeWindowHandle: THandle); override;
    class procedure DoFinalize; override;
    class function DoInitialize: IGrGlInterface; override;
    class procedure DoMakeCurrent(const AContext: TGrGlesAndroidContext); override;
    class procedure DoMakeCurrentOffScreen(const AContext: TGrGlesAndroidContext); override;
  public
    class procedure SwapBuffers; override;
  end;

  TGrGlNativeContext = TGrGlesAndroid;

{$ELSEIF DEFINED(IOS)}

  { TGrGlesIOS }

  TGrGlesIOS = class(TGrGlContext<EAGLContext>)
  strict private class var
    FLibModule: HMODULE;
  strict protected
    class function DoCreateContext(const ANativeWindowHandle: THandle): EAGLContext; override;
    class function DoCreateNativeWindow(const AWindow: TWindowHandle): THandle; override;
    class procedure DoDestroyContext(const ANativeWindowHandle: THandle; const AContext: EAGLContext); override;
    class procedure DoDestroyNativeWindow(const ANativeWindowHandle: THandle); override;
    class procedure DoFinalize; override;
    class function DoInitialize: IGrGlInterface; override;
    class procedure DoMakeCurrent(const AContext: EAGLContext); override;
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
  const ANativeWindowHandle: THandle): TGrGlWindowsContext;
var
  LContext: HGLRC;
  LDC: HDC;
begin
  LDC := GetDC(ANativeWindowHandle);
  if LDC = 0 then
    RaiseLastOSError;
  try
    if not SetPixelFormat(LDC, FSharedPixelFormat, @PixelFormatDescriptor) then
      RaiseLastOSError;
    LContext := wglCreateContext(LDC);
    if LContext = 0 then
      RaiseLastOSError;
    Result := TGrGlWindowsContext.Create(LDC, LContext);
  except
    ReleaseDC(ANativeWindowHandle, LDC);
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

class procedure TGrGlWindows.DoDestroyContext(
  const ANativeWindowHandle: THandle; const AContext: TGrGlWindowsContext);
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(AContext.Context);
  ReleaseDC(ANativeWindowHandle, AContext.DC);
end;

class procedure TGrGlWindows.DoFinalize;
begin
  FreeLibrary(FLibModule);
end;

class function TGrGlWindows.DoInitialize: IGrGlInterface;
var
  LClass: TWndClass;
  LContext: HGLRC;
  LDC: HDC;
  LRect: TRect;
  LWindow: HWND;
begin
  FLibModule := SafeLoadLibrary(opengl32);
  if FLibModule = 0 then
    Exit(nil);
  try
    FillChar(LClass, SizeOf(TWndClass), 0);
    LClass.style         := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
    LClass.cbClsExtra    := 0;
    LClass.cbWndExtra    := 0;
    LClass.lpfnWndProc   := @DefWindowProc;
    LClass.hInstance     := HInstance;
    LClass.hIcon         := LoadIcon(0, IDI_WINLOGO);
    LClass.hCursor       := LoadCursor(0, IDC_ARROW);
    LClass.hbrBackground := 0;
    LClass.lpszMenuName  := nil;
    LClass.lpszClassName := '_DummyClass';
    if Winapi.Windows.RegisterClass(LClass) = 0 then
      Exit(nil);
    try
      LRect := TRect.Create(0, 8, 0, 8);
      if not AdjustWindowRectEx(LRect, WS_SYSMENU, False, WS_EX_CLIENTEDGE) then
        RaiseLastOSError;
      LWindow := CreateWindowEx(WS_EX_CLIENTEDGE, LClass.lpszClassName, '_DummyWindow', WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_SYSMENU, 0, 0, LRect.Width, LRect.Height, 0, 0, HInstance, nil);
      if LWindow = 0 then
        Exit(nil);
      try
        ShowWindow(LWindow, SW_HIDE);
        LDC := GetDC(LWindow);
        if LDC = 0 then
          Exit(nil);
        try
          FSharedPixelFormat := ChoosePixelFormat(LDC, @PixelFormatDescriptor);
          if (FSharedPixelFormat = 0) or (not SetPixelFormat(LDC, FSharedPixelFormat, @PixelFormatDescriptor)) then
            Exit(nil);
          LContext := wglCreateContext(LDC);
          if LContext = 0 then
            Exit(nil);
          try
            if not wglMakeCurrent(LDC, LContext) then
              Exit(nil);
            try
              Result := TGrGlInterface.MakeNative;
            finally
              wglMakeCurrent(0, 0);
            end;
          finally
            wglDeleteContext(LContext);
          end;
        finally
          ReleaseDC(LWindow, LDC);
        end;
      finally
        DestroyWindow(LWindow);
      end;
    finally
      Winapi.Windows.UnregisterClass(LClass.lpszClassName, HInstance);
    end;
  finally
    if not Assigned(Result) then
      FreeLibrary(FLibModule);
  end;
end;

class procedure TGrGlWindows.DoMakeCurrent(const AContext: TGrGlWindowsContext);
begin
  if not wglMakeCurrent(AContext.DC, AContext.Context) then
    RaiseLastOSError;
end;

class procedure TGrGlWindows.SwapBuffers;
begin
  if not Winapi.Windows.SwapBuffers(wglGetCurrentDC) then
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

class function TGrGlesAndroid.DoCreateContext(
  const ANativeWindowHandle: THandle): TGrGlesAndroidContext;
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
    LSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, PANativeWindow(ANativeWindowHandle), nil);
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

class procedure TGrGlesAndroid.DoDestroyContext(
  const ANativeWindowHandle: THandle; const AContext: TGrGlesAndroidContext);
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
  const ANativeWindowHandle: THandle);
begin
  ANativeWindow_release(PANativeWindow(ANativeWindowHandle));
end;

class procedure TGrGlesAndroid.DoFinalize;
begin
  eglTerminate(FSharedDisplay);
end;

class function TGrGlesAndroid.DoInitialize: IGrGlInterface;
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
  LANativeWindow: PANativeWindow;
  LANativeWindowSurface: JSurface;
  LContext: EGLContext;
  LNumConfig: EGLint;
  LSurface: EGLSurface;
  LSurfaceTexture: JSurfaceTexture;
begin
  FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FSharedDisplay = EGL_NO_DISPLAY then
    RaiseLastError;
  if eglInitialize(FSharedDisplay, nil, nil) = EGL_FALSE then
    RaiseLastError;
  try
    if eglChooseConfig(FSharedDisplay, @ConfigAttributes[0], @FSharedConfig, 1, @LNumConfig) = EGL_FALSE then
      Exit(nil);
    LContext := eglCreateContext(FSharedDisplay, FSharedConfig, EGL_NO_CONTEXT, @ContextAttributes[0]);
    if LContext = EGL_NO_CONTEXT then
      Exit(nil);
    try
      LSurfaceTexture := TJSurfaceTexture.JavaClass.init(0);
      LSurfaceTexture.setDefaultBufferSize(1, 1);
      LANativeWindowSurface := TJSurface.JavaClass.init(LSurfaceTexture);
      if not LANativeWindowSurface.isValid then
        Exit(nil);
      LANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, (LANativeWindowSurface as ILocalObject).GetObjectID);
      if LANativeWindow = nil then
        Exit(nil);
      try
        if ANativeWindow_setBuffersGeometry(LANativeWindow, 0, 0, WINDOW_FORMAT_RGBA_8888) <> 0 then
          Exit(nil);
        LSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, LANativeWindow, nil);
        if LSurface = EGL_NO_SURFACE then
          Exit(nil);
        try
          if eglMakeCurrent(FSharedDisplay, LSurface, LSurface, LContext) = EGL_FALSE then
            Exit(nil);
          try
            Result := TGrGlInterface.MakeNative;
          finally
            eglMakeCurrent(FSharedDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
          end;
        finally
          eglDestroySurface(FSharedDisplay, LSurface);
        end;
      finally
        ANativeWindow_release(PANativeWindow(LANativeWindow));
      end;
    finally
      eglDestroyContext(FSharedDisplay, LContext);
    end;
  finally
    if not Assigned(Result) then
      eglTerminate(FSharedDisplay);
  end;
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

class procedure TGrGlesAndroid.RaiseLastError;
begin
  raise EGrCanvas.CreateFmt('OpenGLES error (%x)', [eglGetError]);
end;

class procedure TGrGlesAndroid.SwapBuffers;
begin
  if eglSwapBuffers(eglGetCurrentDisplay, eglGetCurrentSurface(EGL_DRAW)) = EGL_FALSE then
    RaiseLastError;
end;

{$ELSEIF DEFINED(IOS)}

{ TGrGlesIOS }

class function TGrGlesIOS.DoCreateContext(
  const ANativeWindowHandle: THandle): EAGLContext;
var
  LView: GLKView;
begin
  LView  := TGLKView.Wrap(Pointer(ANativeWindowHandle));
  Result := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
  LView.setContext(Result);
  LView.bindDrawable;
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

class procedure TGrGlesIOS.DoDestroyContext(const ANativeWindowHandle: THandle;
  const AContext: EAGLContext);
begin
  TEAGLContext.OCClass.setCurrentContext(nil);
  AContext.release;
end;

class procedure TGrGlesIOS.DoDestroyNativeWindow(
  const ANativeWindowHandle: THandle);
var
  LView: GLKView;
begin
  LView := TGLKView.Wrap(Pointer(ANativeWindowHandle));
  LView.release;
end;

class procedure TGrGlesIOS.DoFinalize;
begin
  FreeLibrary(FLibModule);
end;

class function TGrGlesIOS.DoInitialize: IGrGlInterface;
var
  LContext: EAGLContext;
begin
  FLibModule := SafeLoadLibrary(libGLKit);
  if FLibModule = 0 then
    Exit(nil);
  try
    LContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
    if LContext = nil then
      Exit(nil);
    try
      if not TEAGLContext.OCClass.setCurrentContext(LContext) then
        Exit(nil);
      try
        Result := TGrGlInterface.MakeNative;
      finally
        TEAGLContext.OCClass.setCurrentContext(nil);
      end;
    finally
      LContext.release;
    end;
  finally
    if not Assigned(Result) then
      FreeLibrary(FLibModule);
  end;
end;

class procedure TGrGlesIOS.DoMakeCurrent(const AContext: EAGLContext);
begin
  if not TEAGLContext.OCClass.setCurrentContext(AContext) then
    RaiseLastOSError;
end;

{$ENDIF}

{ TGrGlContext<T> }

constructor TGrGlContext<T>.Create(const ANativeWindowHandle: THandle;
  const AContext: T);
begin
  FNativeWindowHandle := ANativeWindowHandle;
  FContext            := AContext;
end;

destructor TGrGlContext<T>.Destroy;
begin
  DoDestroyContext(FNativeWindowHandle, FContext);
  DoDestroyNativeWindow(FNativeWindowHandle);
  inherited;
end;

class procedure TGrGlContext<T>.DoDestroyNativeWindow(
  const ANativeWindowHandle: THandle);
begin
end;

class procedure TGrGlContext<T>.DoMakeCurrentOffScreen(const AContext: T);
begin
  DoMakeCurrent(AContext);
end;

class procedure TGrGlContext<T>.Finalize;
begin
  DoFinalize;
end;

class function TGrGlContext<T>.Initialize: Boolean;
begin
  FGlInterface := DoInitialize;
  Result       := Assigned(FGlInterface);
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

class procedure TGrGlContext<T>.SwapBuffers;
begin
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
  Result := TGrDirectContext.MakeGl(TGrGlNativeContext.GlInterface);
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

class procedure TGrCanvasGl.DoFinalize;
begin
  TGrGlNativeContext.Finalize;
end;

class function TGrCanvasGl.DoInitialize: Boolean;
begin
  Result := TGrGlNativeContext.Initialize;
end;

procedure TGrCanvasGl.EndWindow;
begin
  inherited;
  FWindowAttached := False;
end;

procedure TGrCanvasGl.FinalizeContext;
begin
  FNativeContext := nil;
end;

procedure TGrCanvasGl.Flush;
begin
  TGrGlNativeContext.SwapBuffers;
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
