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
  Winapi.Windows,
  {$ELSEIF DEFINED(ANDROID)}
  Androidapi.Egl,
  Androidapi.NativeWindow,
  {$ELSEIF DEFINED(IOS)}
  iOSapi.OpenGLES,
  {$ENDIF}

  { Skia }
  Skia,
  Skia.FMX.Graphics;

type
{$IF DEFINED(MSWINDOWS)}

  { TGlBackendContext }

  TGlBackendContext = record
    DC: HDC;
    GLRC: HGLRC;
  end;

  { TGlSharedResources }

  TGlSharedResources = class(TGrCanvasSharedResources)
  strict private
    FContext: TGlBackendContext;
    FGrGlInterface: IGrGlInterface;
    FOldContext: TGlBackendContext;
    FWindow: HWND;
  strict protected
    procedure AfterBeginContext; override;
    procedure BeforeEndContext; override;
    procedure FinalizeSharedResources; override;
    procedure InitializeContext(out ASharedGrDirectContext: IGrDirectContext); override;
    procedure InitializeSharedResources; override;
  public
    property Context: TGlBackendContext read FContext;
    property GrGlInterface: IGrGlInterface read FGrGlInterface;
  end;

  { TGlContext }

  TGlContext = record
  strict private
    FContext: TGlBackendContext;
  public
    procedure Finalize(const AWindow: TWindowHandle);
    function Initialize(const ASharedSharedResources: IGrCanvasSharedResources; const AWindow: TWindowHandle): Boolean;
    procedure MakeContextCurrent;
    procedure SwapBuffers;
    property Context: TGlBackendContext read FContext;
  end;

{$ELSEIF DEFINED(ANDROID)}

  { TGlBackendContext }

  TGlBackendContext = record
    Display: EGLDisplay;
    Draw: EGLSurface;
    Read: EGLSurface;
    Context: EGLContext;
  end;

  { TGlSharedResources }

  TGlSharedResources = class(TGrCanvasSharedResources)
  strict private
    FContext: TGlBackendContext;
    FGrGlInterface: IGrGlInterface;
    FOldContext: TGlBackendContext;
  strict protected
    procedure AfterBeginContext; override;
    procedure BeforeEndContext; override;
    procedure FinalizeSharedResources; override;
    procedure InitializeContext(out ASharedGrDirectContext: IGrDirectContext); override;
    procedure InitializeSharedResources; override;
  public
    property Context: TGlBackendContext read FContext;
    property GrGlInterface: IGrGlInterface read FGrGlInterface;
  end;

  { TGlContext }

  TGlContext = record
  strict private
    FANativeWindow: PANativeWindow;
    FContext: TGlBackendContext;
  public
    procedure Finalize(const AWindow: TWindowHandle);
    function Initialize(const ASharedSharedResources: IGrCanvasSharedResources; const AWindow: TWindowHandle): Boolean;
    procedure MakeContextCurrent;
    procedure SwapBuffers;
    property Context: TGlBackendContext read FContext;
  end;

{$ELSEIF DEFINED(IOS)}

  TGlBackendContext = EAGLContext;

  { TGlSharedResources }

  TGlSharedResources = class(TGrCanvasSharedResources)
  strict private
    FContext: TGlBackendContext;
    FGrGlInterface: IGrGlInterface;
    FLibModule: HMODULE;
    FOldContext: TGlBackendContext;
  strict protected
    procedure AfterBeginContext; override;
    procedure BeforeEndContext; override;
    procedure FinalizeSharedResources; override;
    procedure InitializeContext(out ASharedGrDirectContext: IGrDirectContext); override;
    procedure InitializeSharedResources; override;
  public
    property Context: TGlBackendContext read FContext;
    property GrGlInterface: IGrGlInterface read FGrGlInterface;
  end;

  { TGlContext }

  TGlContext = record
  strict private
    FContext: TGlBackendContext;
  public
    procedure Finalize(const AWindow: TWindowHandle);
    function Initialize(const ASharedSharedResources: IGrCanvasSharedResources; const AWindow: TWindowHandle): Boolean;
    procedure MakeContextCurrent;
    procedure SwapBuffers;
    property Context: TGlBackendContext read FContext;
  end;

{$ENDIF}

  { TGlCanvas }

  TGlCanvas = class(TGrCanvas)
  strict private
    FContext: TGlContext;
    FSampleCount: Integer;
  strict protected
    function CreateSurfaceFromWindow(var AGrDirectContext: IGrDirectContext): ISkSurface; override;
    procedure DoEndScene; override;
    procedure SetCurrentContext; override;
    class procedure InitializeSharedResources(out ASharedResources: IGrCanvasSharedResources); override;
  public
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IF DEFINED(MSWINDOWS) or DEFINED(ANDROID) or DEFINED(IOS)}

uses
  { Delphi }
  FMX.Graphics,
  System.SysUtils,
  {$IF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.OpenGL,
  Winapi.OpenGLext;
  {$ELSEIF DEFINED(ANDROID)}
  Androidapi.Gles2,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.NativeWindowJni,
  FMX.Platform.UI.Android,
  FMX.Presentation.Android.Style;
  {$ELSEIF DEFINED(IOS)}
  FMX.Platform.iOS,
  iOSapi.GLKit,
  Macapi.ObjectiveC;
  {$ENDIF}

{$IF DEFINED(MSWINDOWS)}

const
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_MAJOR_VERSION_ARB    = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB    = $2092;
  WGL_CONTEXT_PROFILE_MASK_ARB     = $9126;

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
    cDepthBits      : 24;
    cStencilBits    : 8;
    cAuxBuffers     : 0;
    iLayerType      : PFD_MAIN_PLANE;
    bReserved       : 0;
    dwLayerMask     : 0;
    dwVisibleMask   : 0;
    dwDamageMask    : 0);

var
  PixelFormat: Integer;

  ContextAttributes: array[0..6] of GLInt = (
    WGL_CONTEXT_MAJOR_VERSION_ARB , -1                               ,
    WGL_CONTEXT_MINOR_VERSION_ARB , -1                               ,
    WGL_CONTEXT_PROFILE_MASK_ARB  , WGL_CONTEXT_CORE_PROFILE_BIT_ARB ,
    GL_NONE);

  wglCreateContextAttribsARB: function (hDC: HDC; hShareContext: HGLRC; const attribList: PInteger): HGLRC; stdcall;

{ TGlSharedResources }

procedure TGlSharedResources.AfterBeginContext;
begin
  FOldContext.DC   := wglGetCurrentDC;
  FOldContext.GLRC := wglGetCurrentContext;
  if not wglMakeCurrent(FContext.DC, FContext.GLRC) then
    raise EGrCanvas.Create('Could not make the shared context as current.');
end;

procedure TGlSharedResources.BeforeEndContext;
begin
  wglMakeCurrent(FOldContext.DC, FOldContext.GLRC);
end;

procedure TGlSharedResources.FinalizeSharedResources;
begin
  wglDeleteContext(FContext.GLRC);
  ReleaseDC(FWindow, FContext.DC);
  DestroyWindow(FWindow);
  Winapi.Windows.UnregisterClass('SkClass', HInstance);
end;

procedure TGlSharedResources.InitializeContext(
  out ASharedGrDirectContext: IGrDirectContext);
begin
  FGrGlInterface         := TGrGlInterface.MakeNative;
  ASharedGrDirectContext := TGrDirectContext.MakeGl(FGrGlInterface);
end;

procedure TGlSharedResources.InitializeSharedResources;
const
  CoreVersions: array[0..11] of GLInt = (
    4, 3,
    4, 2,
    4, 1,
    4, 0,
    3, 3,
    3, 2);
var
  I: Integer;
  LClass: TWndClass;
  LGLRC: HGLRC;
  LOldContext: TGlBackendContext;
begin
  FillChar(LClass, SizeOf(TWndClass), 0);
  LClass.lpfnWndProc   := @DefWindowProc;
  LClass.hInstance     := HInstance;
  LClass.lpszClassName := 'SkClass';
  if Winapi.Windows.RegisterClass(LClass) = 0 then
    raise EGrCanvas.Create('Could not register the dummy class.');
  FWindow := CreateWindowEx(WS_EX_TOOLWINDOW, 'SkClass', nil, WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if FWindow = 0 then
    raise EGrCanvas.Create('Could not create the dummy window.');
  try
    FContext.DC := GetDC(FWindow);
    if FContext.DC = 0 then
      raise EGrCanvas.Create('Could not get a DC from the dummy window.');
    try
      PixelFormat := ChoosePixelFormat(FContext.DC, @PixelFormatDescriptor);
      if (PixelFormat = 0) or (not SetPixelFormat(FContext.DC, PixelFormat, @PixelFormatDescriptor)) then
        raise EGrCanvas.Create('Could not choose/set pixel format for the dummy window DC.');
      LGLRC := wglCreateContext(FContext.DC);
      if LGLRC = 0 then
        raise EGrCanvas.Create('Could not create the dummy context.');
      try
        LOldContext.DC   := wglGetCurrentDC;
        LOldContext.GLRC := wglGetCurrentContext;
        if not wglMakeCurrent(FContext.DC, LGLRC) then
          raise EGrCanvas.Create('Could not make the dummy context as current.');
        try
          wglCreateContextAttribsARB := wglGetProcAddress('wglCreateContextAttribsARB');
        finally
          wglMakeCurrent(LOldContext.DC, LOldContext.GLRC);
        end;
        if Assigned(wglCreateContextAttribsARB) then
        begin
          for I := 0 to Length(CoreVersions) div 2 - 1 do
          begin
            ContextAttributes[1] := CoreVersions[I * 2];
            ContextAttributes[3] := CoreVersions[I * 2 + 1];
            FContext.GLRC := wglCreateContextAttribsARB(FContext.DC, 0, @ContextAttributes[0]);
            if FContext.GLRC <> 0 then
            begin
              wglDeleteContext(LGLRC);
              Exit;
            end;
          end;
          wglCreateContextAttribsARB := nil;
        end;
        FContext.GLRC := LGLRC;
      except
        wglDeleteContext(LGLRC);
        raise;
      end;
    except
      ReleaseDC(FWindow, FContext.DC);
      raise;
    end;
  except
    DestroyWindow(FWindow);
    raise;
  end;
end;

{ TGlContext }

procedure TGlContext.Finalize(const AWindow: TWindowHandle);
begin
  if FContext.GLRC <> 0 then
  begin
    wglDeleteContext(FContext.GLRC);
    ReleaseDC(WindowHandleToPlatform(AWindow).Wnd, FContext.DC);
  end;
  wglMakeCurrent(0, 0);
end;

function TGlContext.Initialize(
  const ASharedSharedResources: IGrCanvasSharedResources;
  const AWindow: TWindowHandle): Boolean;
begin
  if FContext.GLRC = 0 then
  begin
    if WindowHandleToPlatform(AWindow).Wnd = 0 then
      Exit(False);
    FContext.DC := GetDC(WindowHandleToPlatform(AWindow).Wnd);
    if FContext.DC = 0 then
      Exit(False);
    SetPixelFormat(FContext.DC, PixelFormat, @PixelFormatDescriptor);
    if Assigned(wglCreateContextAttribsARB) then
    begin
      FContext.GLRC := wglCreateContextAttribsARB(FContext.DC, TGlSharedResources(ASharedSharedResources).Context.GLRC, @ContextAttributes[0]);
      if FContext.GLRC = 0 then
      begin
        ReleaseDC(WindowHandleToPlatform(AWindow).Wnd, FContext.DC);
        Exit(False);
      end;
    end
    else
    begin
      FContext.GLRC := wglCreateContext(FContext.DC);
      if FContext.GLRC = 0 then
      begin
        ReleaseDC(WindowHandleToPlatform(AWindow).Wnd, FContext.DC);
        Exit(False);
      end;
      wglShareLists(TGlSharedResources(ASharedSharedResources).Context.GLRC, FContext.GLRC);
    end;
  end;
  Result := wglMakeCurrent(FContext.DC, FContext.GLRC);
end;

procedure TGlContext.MakeContextCurrent;
begin
  if not wglMakeCurrent(FContext.DC, FContext.GLRC) then
    raise EGrCanvas.Create('Could not make a context as current.');
end;

procedure TGlContext.SwapBuffers;
begin
  if not Winapi.Windows.SwapBuffers(FContext.DC) then
    raise EGrCanvas.Create('Could not swap buffers.');
end;

{$ELSEIF DEFINED(ANDROID)}

const
  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);

var
  Config: EGLConfig;

{ TGlSharedResources }

procedure TGlSharedResources.AfterBeginContext;
begin
  FOldContext.Display := eglGetCurrentDisplay;
  FOldContext.Draw    := eglGetCurrentSurface(EGL_DRAW);
  FOldContext.Read    := eglGetCurrentSurface(EGL_READ);
  FOldContext.Context := eglGetCurrentContext;
  if eglMakeCurrent(FContext.Display, FContext.Draw, FContext.Read, FContext.Context) = EGL_FALSE then
    raise EGrCanvas.Create('Could not make the shared context as current.');
end;

procedure TGlSharedResources.BeforeEndContext;
begin
  eglMakeCurrent(FOldContext.Display, FOldContext.Draw, FOldContext.Read, FOldContext.Context);
end;

procedure TGlSharedResources.FinalizeSharedResources;
begin
  eglDestroyContext(FContext.Display, FContext.Context);
  eglDestroySurface(FContext.Display, FContext.Draw);
  eglTerminate(FContext.Display);
end;

procedure TGlSharedResources.InitializeContext(
  out ASharedGrDirectContext: IGrDirectContext);
begin
  FGrGlInterface         := TGrGlInterface.MakeNative;
  ASharedGrDirectContext := TGrDirectContext.MakeGl(FGrGlInterface);
end;

procedure TGlSharedResources.InitializeSharedResources;
const
  ConfigAttributes: array[0..16] of EGLint = (
    EGL_SURFACE_TYPE    , EGL_WINDOW_BIT     ,
    EGL_RENDERABLE_TYPE , EGL_OPENGL_ES2_BIT ,
    EGL_RED_SIZE        , 8                  ,
    EGL_GREEN_SIZE      , 8                  ,
    EGL_BLUE_SIZE       , 8                  ,
    EGL_ALPHA_SIZE      , 8                  ,
    EGL_DEPTH_SIZE      , 24                 ,
    EGL_STENCIL_SIZE    , 8                  ,
    EGL_NONE);

  SurfaceAttributes: array[0..4] of EGLint = (EGL_WIDTH, 1, EGL_HEIGHT, 1, EGL_NONE);
var
  LNumConfig: EGLint;
begin
  FContext.Display := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if (FContext.Display = EGL_NO_DISPLAY) or (eglInitialize(FContext.Display, nil, nil) = EGL_FALSE) then
    raise EGrCanvas.Create('Could not initialize the default display.');
  try
    if eglChooseConfig(FContext.Display, @ConfigAttributes[0], @Config, 1, @LNumConfig) = EGL_FALSE then
      raise EGrCanvas.Create('Could not get frame buffer configurations list.');
    FContext.Draw := eglCreatePbufferSurface(FContext.Display, Config, @SurfaceAttributes[0]);
    if FContext.Draw = EGL_NO_SURFACE then
      raise EGrCanvas.Create('Could not create a new pixel buffer surface.');
    try
      FContext.Read    := FContext.Draw;
      FContext.Context := eglCreateContext(FContext.Display, Config, EGL_NO_CONTEXT, @ContextAttributes[0]);
      if FContext.Context = EGL_NO_CONTEXT then
        raise EGrCanvas.Create('Could not create the shared context.');
    except
      eglDestroySurface(FContext.Display, FContext.Draw);
      raise;
    end;
  except
    eglTerminate(FContext.Display);
    raise;
  end;
end;

{ TGlContext }

procedure TGlContext.Finalize(const AWindow: TWindowHandle);
begin
  if FContext.Context <> EGL_NO_CONTEXT then
    eglDestroyContext(FContext.Display, FContext.Context);
  if FContext.Draw <> EGL_NO_SURFACE then
  begin
    eglDestroySurface(FContext.Display, FContext.Draw);
    ANativeWindow_release(FANativeWindow);
  end;
  eglMakeCurrent(FContext.Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
end;

function TGlContext.Initialize(
  const ASharedSharedResources: IGrCanvasSharedResources;
  const AWindow: TWindowHandle): Boolean;
var
  LANativeWindowSurface: JSurface;
  LANativeWindowSurfaceID: Pointer;
begin
  if FContext.Draw = EGL_NO_SURFACE then
  begin
    if AWindow is TAndroidWindowHandle then
    begin
      if TAndroidWindowHandle(AWindow).Holder = nil then
        Exit(False);
      LANativeWindowSurfaceID := (TAndroidWindowHandle(AWindow).Holder.getSurface as ILocalObject).GetObjectID;
    end
    else if AWindow is TAndroidHandle then
    begin
      if TAndroidHandle(AWindow).Surface = nil then
        Exit(False);
      LANativeWindowSurface   := TJSurface.JavaClass.init(TAndroidHandle(AWindow).Surface);
      LANativeWindowSurfaceID := TJNIResolver.JavaInstanceToID(LANativeWindowSurface);
    end
    else
      Exit(False);
    FANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, LANativeWindowSurfaceID);
    if FANativeWindow = nil then
      Exit(False);
    if ANativeWindow_setBuffersGeometry(FANativeWindow, 0, 0, WINDOW_FORMAT_RGBA_8888) <> 0 then
    begin
      ANativeWindow_release(FANativeWindow);
      Exit(False);
    end;
    FContext.Draw := eglCreateWindowSurface(TGlSharedResources(ASharedSharedResources).Context.Display, Config, FANativeWindow, nil);
    if FContext.Draw = EGL_NO_SURFACE then
    begin
      ANativeWindow_release(FANativeWindow);
      Exit(False);
    end;
    FContext.Display := TGlSharedResources(ASharedSharedResources).Context.Display;
    FContext.Read    := FContext.Draw;
  end;
  if FContext.Context = EGL_NO_CONTEXT then
  begin
    FContext.Context := eglCreateContext(FContext.Display, Config, TGlSharedResources(ASharedSharedResources).Context.Context, @ContextAttributes[0]);
    if FContext.Context = EGL_NO_CONTEXT then
      Exit(False);
  end;
  Result := eglMakeCurrent(FContext.Display, FContext.Draw, FContext.Read, FContext.Context) <> EGL_FALSE;
end;

procedure TGlContext.MakeContextCurrent;
begin
  if eglMakeCurrent(FContext.Display, FContext.Draw, FContext.Read, FContext.Context) = EGL_FALSE then
    raise EGrCanvas.Create('Could not make a context as current.');
end;

procedure TGlContext.SwapBuffers;
begin
  if eglSwapBuffers(FContext.Display, FContext.Draw) = EGL_FALSE then
    raise EGrCanvas.Create('Could not swap buffers.');
end;

{$ELSEIF DEFINED(IOS)}

{ TGlSharedResources }

procedure TGlSharedResources.AfterBeginContext;
begin
  FOldContext := TEAGLContext.Wrap(TEAGLContext.OCClass.currentContext);
  if not TEAGLContext.OCClass.setCurrentContext(FContext) then
    raise EGrCanvas.Create('Could not make the shared context as current.');
end;

procedure TGlSharedResources.BeforeEndContext;
begin
  TEAGLContext.OCClass.setCurrentContext(FOldContext);
end;

procedure TGlSharedResources.FinalizeSharedResources;
begin
  FContext.release;
  FreeLibrary(FLibModule);
end;

procedure TGlSharedResources.InitializeContext(
  out ASharedGrDirectContext: IGrDirectContext);
begin
  FGrGlInterface         := TGrGlInterface.MakeNative;
  ASharedGrDirectContext := TGrDirectContext.MakeGl(FGrGlInterface);
end;

procedure TGlSharedResources.InitializeSharedResources;
begin
  FLibModule := SafeLoadLibrary(libGLKit);
  if FLibModule = 0 then
    raise EGrCanvas.Create('Could not load "GLKit" framework.');
  try
    FContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
    if FContext = nil then
      raise EGrCanvas.Create('Could not create the shared context.');
  except
    FreeLibrary(FLibModule);
    raise;
  end;
end;

{ TGlContext }

procedure TGlContext.Finalize(const AWindow: TWindowHandle);
begin
  if FContext <> nil then
  begin
    FContext.release;
    TEAGLContext.OCClass.setCurrentContext(nil);
  end;
end;

function TGlContext.Initialize(
  const ASharedSharedResources: IGrCanvasSharedResources;
  const AWindow: TWindowHandle): Boolean;
begin
  if FContext = nil then
  begin
    if WindowHandleToPlatform(AWindow).View = nil then
      Exit(False);
    FContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2, TGlSharedResources(ASharedSharedResources).Context.sharegroup));
    if FContext = nil then
      Exit(False);
    GLKView(WindowHandleToPlatform(AWindow).View).setContext(FContext);
    GLKView(WindowHandleToPlatform(AWindow).View).bindDrawable;
  end;
  Result := TEAGLContext.OCClass.setCurrentContext(FContext);
end;

procedure TGlContext.MakeContextCurrent;
begin
  if not TEAGLContext.OCClass.setCurrentContext(FContext) then
    raise EGrCanvas.Create('Could not make a context as current.');
end;

procedure TGlContext.SwapBuffers;
begin
end;

{$ENDIF}

{ TGlCanvas }

function TGlCanvas.CreateSurfaceFromWindow(
  var AGrDirectContext: IGrDirectContext): ISkSurface;
var
  LGrGlFramebufferInfo: TGrGlFramebufferInfo;
  LGrRenderTarget: IGrBackendRenderTarget;
begin
  if not FContext.Initialize(SharedResources, Parent) then
    Exit(nil);
  if not Assigned(AGrDirectContext) then
  begin
    AGrDirectContext := TGrDirectContext.MakeGl(TGlSharedResources(SharedResources).GrGlInterface);
    if Quality = TCanvasQuality.HighQuality then
      FSampleCount := GrDirectContext.GetMaxSurfaceSampleCountForColorType(TSkColorType.RGBA8888)
    else
      FSampleCount := 1;
  end;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @GLuint(LGrGlFramebufferInfo.FBOID));
  LGrGlFramebufferInfo.Format := GrGlSizedFormat[TSkColorType.RGBA8888];
  LGrRenderTarget := TGrBackendRenderTarget.CreateGl(Round(Width * Scale), Round(Height * Scale), FSampleCount, 8, LGrGlFramebufferInfo);
  Result          := TSkSurface.MakeFromRenderTarget(GrDirectContext, LGrRenderTarget, TGrSurfaceOrigin.BottomLeft, TSkColorType.RGBA8888);
end;

destructor TGlCanvas.Destroy;
begin
  if Parent <> nil then
    FContext.Finalize(Parent);
  inherited;
end;

procedure TGlCanvas.DoEndScene;
begin
  inherited;
  if Parent <> nil then
    FContext.SwapBuffers;
end;

class procedure TGlCanvas.InitializeSharedResources(
  out ASharedResources: IGrCanvasSharedResources);
begin
  ASharedResources := TGlSharedResources.Create;
end;

procedure TGlCanvas.SetCurrentContext;
begin
  FContext.MakeContextCurrent;
end;

{$ENDIF}

{$HPPEMIT NOUSINGNAMESPACE}
{$IF DEFINED(MSWINDOWS) or DEFINED(ANDROID) or DEFINED(IOS)}
  (*$HPPEMIT 'namespace Skia {'*)
  (*$HPPEMIT '	namespace Fmx {'*)
  (*$HPPEMIT '		namespace Types { using namespace ::Fmx::Types; }'*)
  (*$HPPEMIT '	}'*)
  (*$HPPEMIT '}'*)
{$ENDIF}
end.

