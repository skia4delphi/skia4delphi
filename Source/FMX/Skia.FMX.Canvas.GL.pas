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

// Delphi currently uses rasterization on macOS when Metal is off.
// If it uses OpenGL on MacOS in the future, the implementation is ready.

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
  //{$ELSEIF DEFINED(MACOS)}
  //Macapi.CocoaTypes,
  {$ENDIF}

  { Skia }
  Skia,
  Skia.FMX.Graphics;


type
  { TGrGlContext }

  TGrGlContext = class abstract
  strict private
    FWindowAttached: Boolean;
  strict protected
    [Weak] FWindow: TWindowHandle;
  public
    constructor Create(const AWindow: TWindowHandle);
    procedure FlushBuffers; virtual;
    procedure MakeCurrentContext; virtual; abstract;
    property WindowAttached: Boolean read FWindowAttached write FWindowAttached;
    class procedure Finalize; virtual;
    class procedure Initialize; virtual;
  end;

  { TGrCanvasGl }

  TGrCanvasGl = class(TGrCanvasCustom)
  strict private
    FContext: TGrGlContext;
  strict protected
    procedure AttachToWindow; override;
    function CreateContext: IGrDirectContext; override;
    procedure DestroyContext; override;
    procedure DetachFromWindow; override;
    procedure Flush; override;
    function GetRenderTarget: IGrBackendRenderTarget; override;
    procedure Prepare; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  public
    class function ColorType: TSkColorType; override;
    class function Origin: TGrSurfaceOrigin; override;
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
  Androidapi.NativeWindow,
  Androidapi.NativeWindowJni,
  FMX.Platform.UI.Android,
  FMX.Presentation.Android.Style,
  Androidapi.JNIBridge,
  {$ELSEIF DEFINED(IOS)}
  FMX.Platform.iOS,
  iOSapi.GLKit,
  Macapi.ObjectiveC,
  //{$ELSEIF DEFINED(MACOS)}
  //FMX.Platform.Mac,
  //Macapi.AppKit,
  //Macapi.ObjectiveC,
  //Macapi.OpenGL,
  {$ENDIF}
  System.SysUtils;

type

{$IF DEFINED(MSWINDOWS)}

  { TGrGlWindows }

  TGrGlWindows = class(TGrGlContext)
  strict private
    FContext: HGLRC;
    FDC: HDC;
  public
    constructor Create(const AWindow: TWindowHandle);
    destructor Destroy; override;
    procedure FlushBuffers; override;
    procedure MakeCurrentContext; override;
  end;

  TGrGlNativeContext = TGrGlWindows;

{$ELSEIF DEFINED(ANDROID)}

  { TGrGlesAndroid }

  TGrGlesAndroid = class(TGrGlContext)
  strict private class var
    FSharedContext: EGLContext;
    FSharedConfig: EGLConfig;
    FSharedDisplay: EGLDisplay;
    FSharedSurface: EGLSurface;
  strict private
    FANativeWindow: PANativeWindow;
    FSurface: EGLSurface;
  public
    constructor Create(const AWindow: TWindowHandle);
    destructor Destroy; override;
    procedure FlushBuffers; override;
    procedure MakeCurrentContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

  TGrGlNativeContext = TGrGlesAndroid;

{$ELSEIF DEFINED(IOS)}

  { TGrGlesIOS }

  TGrGlesIOS = class(TGrGlContext)
  strict private class var
    FLibModule: HMODULE;
    FSharedContext: EAGLContext;
  public
    constructor Create(const AWindow: TWindowHandle);
    procedure MakeCurrentContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

  TGrGlNativeContext = TGrGlesIOS;

(*

{$ELSEIF DEFINED(MACOS)}


  { TGrGlMacOS }

  TGrGlMacOS = class(TGrGlContext)
  strict private class var
    FLibModule: HMODULE;
    FPixelFormat: NSOpenGLPixelFormat;
  strict private
    FContext: NSOpenGLContext;
  public
    constructor Create(const AWindow: TWindowHandle);
    destructor Destroy; override;
    procedure FlushBuffers; override;
    procedure MakeCurrentContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
  end;

  TGrGlNativeContext = TGrGlMacOS;

*)

{$ENDIF}

{ TGrGlContext }

constructor TGrGlContext.Create(const AWindow: TWindowHandle);
begin
  inherited Create;
  FWindow := AWindow;
end;

class procedure TGrGlContext.Finalize;
begin
end;

procedure TGrGlContext.FlushBuffers;
begin
end;

class procedure TGrGlContext.Initialize;
begin
end;

{$IF DEFINED(MSWINDOWS)}

{ TGrGlWindows }

constructor TGrGlWindows.Create(const AWindow: TWindowHandle);
const
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
var
  LPixelFormat: Integer;
begin
  inherited;
  FDC := GetDC(WindowHandleToPlatform(AWindow).Wnd);
  if FDC = 0 then
    RaiseLastOSError;
  try
    LPixelFormat := ChoosePixelFormat(FDC, @PixelFormatDescriptor);
    if (LPixelFormat = 0) or (not SetPixelFormat(FDC, LPixelFormat, @PixelFormatDescriptor)) then
      RaiseLastOSError;
    FContext := wglCreateContext(FDC);
    if FContext = 0 then
      RaiseLastOSError;
    try
      if not wglMakeCurrent(FDC, FContext) then
        RaiseLastOSError;
    except
      wglDeleteContext(FContext);
      raise;
    end;
  except
    ReleaseDC(WindowHandleToPlatform(AWindow).Wnd, FDC);
    raise;
  end;
end;

destructor TGrGlWindows.Destroy;
begin
  wglDeleteContext(FContext);
  ReleaseDC(WindowHandleToPlatform(FWindow).Wnd, FDC);
  inherited;
end;

procedure TGrGlWindows.FlushBuffers;
begin
  if not SwapBuffers(FDC) then
    RaiseLastOSError;
end;

procedure TGrGlWindows.MakeCurrentContext;
begin
  if not wglMakeCurrent(FDC, FContext) then
    RaiseLastOSError;
end;

{$ELSEIF DEFINED(ANDROID)}

{ TGrGlesAndroid }

constructor TGrGlesAndroid.Create(const AWindow: TWindowHandle);
const
  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
var
  LANativeWindowSurface: JSurface;
  LFormat: GLint;
begin
  inherited;
  if eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext) = EGL_FALSE then
    raise EGrCanvas.Create('Could not make EGL current shared context');
  if AWindow is TAndroidWindowHandle then
    LANativeWindowSurface := TAndroidWindowHandle(AWindow).Holder.getSurface
  else if AWindow is TAndroidHandle then
    LANativeWindowSurface := TJSurface.JavaClass.init(TAndroidHandle(AWindow).Surface)
  else
    raise EGrCanvas.Create('Invalid window');
  FANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, (LANativeWindowSurface as ILocalObject).GetObjectID);
  if FANativeWindow = nil then
    raise EGrCanvas.Create('Could not get ANativeWindow');
  try
    if eglGetConfigAttrib(FSharedDisplay, FSharedConfig, EGL_NATIVE_VISUAL_ID, @LFormat) = EGL_FALSE then
      raise EGrCanvas.Create('Could not get EGL native visual ID');
    if ANativeWindow_setBuffersGeometry(FANativeWindow, 0, 0, LFormat) <> 0 then
      raise EGrCanvas.Create('Could not change the format of the window buffer');
    FSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, FANativeWindow, nil);
    if FSurface = EGL_NO_SURFACE then
      raise EGrCanvas.Create('Could not create a new EGL window surface');
  except
    ANativeWindow_release(FANativeWindow);
    raise;
  end;
end;

destructor TGrGlesAndroid.Destroy;
begin
  eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext);
  eglDestroySurface(FSharedDisplay, FSurface);
  ANativeWindow_release(FANativeWindow);
  inherited;
end;

class procedure TGrGlesAndroid.Finalize;
begin
  eglDestroyContext(FSharedDisplay, FSharedContext);
  eglDestroySurface(FSharedDisplay, FSharedSurface);
  eglTerminate(FSharedDisplay);
end;

procedure TGrGlesAndroid.FlushBuffers;
begin
  if eglSwapBuffers(FSharedDisplay, FSurface) = EGL_FALSE then
    raise EGrCanvas.Create('Could not flush EGL buffers');
end;

class procedure TGrGlesAndroid.Initialize;
const
  ConfigAttributes: array[0..12] of EGLint = (
    EGL_RENDERABLE_TYPE , EGL_OPENGL_ES2_BIT,
    EGL_RED_SIZE        ,  8,
    EGL_GREEN_SIZE      ,  8,
    EGL_BLUE_SIZE       ,  8,
    EGL_ALPHA_SIZE      ,  8,
    EGL_STENCIL_SIZE    ,  8,
    EGL_NONE);

  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);

  SurfaceAttributes: array[0..4] of EGLint = (EGL_WIDTH, 1, EGL_HEIGHT, 1, EGL_NONE);
var
  LNumConfig: EGLint;
begin
  FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FSharedDisplay = EGL_NO_DISPLAY then
    raise EGrCanvas.Create('Could not get EGL display');
  if eglInitialize(FSharedDisplay, nil, nil) = EGL_FALSE then
    raise EGrCanvas.Create('Could not initialize EGL display');
  try
    if eglChooseConfig(FSharedDisplay, @ConfigAttributes[0], @FSharedConfig, 1, @LNumConfig) = EGL_FALSE then
      raise EGrCanvas.Create('Could not choose EGL config');
    FSharedSurface := eglCreatePbufferSurface(FSharedDisplay, FSharedConfig, @SurfaceAttributes[0]);
    if FSharedSurface = EGL_NO_SURFACE then
      raise EGrCanvas.Create('Could not create EGL shared surface');
    try
      FSharedContext := eglCreateContext(FSharedDisplay, FSharedConfig, EGL_NO_CONTEXT, @ContextAttributes[0]);
      if FSharedContext = EGL_NO_CONTEXT then
        raise EGrCanvas.Create('Could not create EGL context');
    except
      eglDestroySurface(FSharedDisplay, FSharedSurface);
      raise;
    end;
  except
    eglTerminate(FSharedDisplay);
    raise;
  end;
end;

procedure TGrGlesAndroid.MakeCurrentContext;
var
  LSurface: EGLSurface;
begin
  if WindowAttached then
    LSurface := FSurface
  else
    LSurface := FSharedSurface;
  if eglMakeCurrent(FSharedDisplay, LSurface, LSurface, FSharedContext) = EGL_FALSE then
    raise EGrCanvas.Create('Could not make EGL current context');
end;

{$ELSEIF DEFINED(IOS)}

{ TGrGlesIOS }

constructor TGrGlesIOS.Create(const AWindow: TWindowHandle);
begin
  inherited;
  if not TEAGLContext.OCClass.setCurrentContext(FSharedContext) then
    raise EGrCanvas.Create('Could not make EGL current shared context');
  GLKView(WindowHandleToPlatform(AWindow).View).setContext(FSharedContext);
  GLKView(WindowHandleToPlatform(AWindow).View).setDrawableColorFormat(GLKViewDrawableColorFormatRGBA8888);
  GLKView(WindowHandleToPlatform(AWindow).View).setDrawableDepthFormat(GLKViewDrawableDepthFormatNone);
  GLKView(WindowHandleToPlatform(AWindow).View).setDrawableStencilFormat(GLKViewDrawableStencilFormat8);
  GLKView(WindowHandleToPlatform(AWindow).View).setDrawableMultisample(GLKViewDrawableMultisampleNone);
end;

class procedure TGrGlesIOS.Finalize;
begin
  FSharedContext.release;
  FreeLibrary(FLibModule);
end;

class procedure TGrGlesIOS.Initialize;
begin
  FLibModule := LoadLibrary(PChar(libGLKit));
  if FLibModule = 0 then
    raise EGrCanvas.Create('Unable to load GLES');
  try
    FSharedContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
    if FSharedContext = nil then
      raise EGrCanvas.Create('Could not create EGL context');
  except
    FreeLibrary(FLibModule);
    raise;
  end;
end;

procedure TGrGlesIOS.MakeCurrentContext;
begin
  if not TEAGLContext.OCClass.setCurrentContext(FSharedContext) then
    raise EGrCanvas.Create('Could not make EGL current context');
end;

(*

{$ELSEIF DEFINED(MACOS)}

{ TGrGlMacOS }

constructor TGrGlMacOS.Create(const AWindow: TWindowHandle);
begin
  inherited;
  FContext := TNSOpenGLContext.Wrap(TNSOpenGLContext.Alloc.initWithFormat(FPixelFormat, nil));
  if FContext = nil then
    raise EGrCanvas.Create('Could not create EGL context');
  FContext.makeCurrentContext;
  FContext.setView(WindowHandleToPlatform(AWindow).View);
end;

destructor TGrGlMacOS.Destroy;
begin
  FContext.release;
  inherited;
end;

class procedure TGrGlMacOS.Finalize;
begin
  FPixelFormat.release;
end;

procedure TGrGlMacOS.FlushBuffers;
begin
  glSwapAPPLE;
end;

class procedure TGrGlMacOS.Initialize;
const
  Options: array[0..9] of NSOpenGLPixelFormatAttribute = (
    NSOpenGLPFADoubleBuffer ,
    NSOpenGLPFAColorSize    , 24,
    NSOpenGLPFAAlphaSize    ,  8,
    NSOpenGLPFAStencilSize  ,  8,
    0);
begin
  FLibModule := InitOpenGL;
  if FLibModule = 0 then
    raise EGrCanvas.Create('Unable to load GL');
  try
    FPixelFormat := TNSOpenGLPixelFormat.Wrap(TNSOpenGLPixelFormat.Alloc.initWithAttributes(@Options[0]));
    if FPixelFormat = nil then
      raise EGrCanvas.Create('Could not create GL pixel format');
  except
    FreeLibrary(FLibModule);
    raise;
  end;
end;

procedure TGrGlMacOS.MakeCurrentContext;
begin
  FContext.makeCurrentContext;
end;

*)

{$ENDIF}

{ TGrCanvasGl }

procedure TGrCanvasGl.AttachToWindow;
begin
  FContext.WindowAttached := True;
end;

class function TGrCanvasGl.ColorType: TSkColorType;
begin
  Result := TSkColorType.RGBA8888;
end;

function TGrCanvasGl.CreateContext: IGrDirectContext;
begin
  FContext := TGrGlNativeContext.Create(Parent);
  Result   := TGrDirectContext.MakeGl;
end;

procedure TGrCanvasGl.DestroyContext;
begin
  inherited;
  FContext.Free;
end;

procedure TGrCanvasGl.DetachFromWindow;
begin
  FContext.WindowAttached := False;
end;

class procedure TGrCanvasGl.Finalize;
begin
  inherited;
  TGrGlNativeContext.Finalize;
end;

procedure TGrCanvasGl.Flush;
begin
  FContext.FlushBuffers;
end;

function TGrCanvasGl.GetRenderTarget: IGrBackendRenderTarget;
var
  LFramebuffer: GLuint;
  LMaxSamples: Integer;
  LSamples: GLint;
  LStencilBits: GLint;
begin
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, @LFramebuffer);
  glGetIntegerv(GL_STENCIL_BITS, @LStencilBits);
  glGetIntegerv(GL_SAMPLES, @LSamples);
  LMaxSamples := Context.GetMaxSurfaceSampleCountForColorType(ColorType);
  if LSamples > LMaxSamples then
    LSamples := LMaxSamples;
  Result := TGrBackendRenderTarget.CreateGl(DrawableWidth, DrawableHeight, LSamples, LStencilBits, TGrGlFramebufferInfo.Create(LFramebuffer, GrGlSizedFormat[ColorType]));
end;

class procedure TGrCanvasGl.Initialize;
begin
  inherited;
  TGrGlNativeContext.Initialize;
end;

class function TGrCanvasGl.Origin: TGrSurfaceOrigin;
begin
  Result := TGrSurfaceOrigin.BottomLeft;
end;

procedure TGrCanvasGl.Prepare;
begin
  FContext.MakeCurrentContext;
end;

{$ELSE}
implementation
{$ENDIF}
end.
