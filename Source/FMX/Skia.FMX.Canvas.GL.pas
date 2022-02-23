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

  { Skia }
  Skia,
  Skia.FMX.Graphics;


type
  { IGrGlContext }

  IGrGlContext = interface
    ['{00129575-EB27-4220-8E3C-ACEA0EE42C4A}']
    procedure AttachToWindow;
    procedure DetachFromWindow;
    procedure FlushBuffers;
    procedure MakeCurrentContext;
  end;

  { TGrGlContext }

  TGrGlContext = class abstract(TInterfacedObject, IGrGlContext)
  strict private
    [unsafe] FWindow: TWindowHandle;
    FWindowAttached: Boolean;
  strict protected
    procedure AttachToWindow;
    procedure DetachFromWindow;
    procedure FlushBuffers; virtual;
    procedure MakeCurrentContext; virtual; abstract;
    property Window: TWindowHandle read FWindow;
    property WindowAttached: Boolean read FWindowAttached;
  public
    constructor Create(const AWindow: TWindowHandle);
    class procedure Finalize; virtual;
    class procedure Initialize; virtual;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext; virtual; abstract;
  end;

  { TGrCanvasGl }

  TGrCanvasGl = class(TGrCanvasCustom)
  strict private
    FContext: IGrGlContext;
  strict protected
    procedure AttachToWindow; override;
    function CreateContext: IGrDirectContext; override;
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

  { TGrGlWindows }

  TGrGlWindows = class(TGrGlContext)
  strict private
    FContext: HGLRC;
    FDC: HDC;
  public
    constructor Create(const AWindow: TWindowHandle; const ADC: HDC; const AContext: HGLRC);
    destructor Destroy; override;
    procedure FlushBuffers; override;
    procedure MakeCurrentContext; override;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext; override;
  end;

  TGrGlNativeContext = TGrGlWindows;

{$ELSEIF DEFINED(ANDROID)}

  { TGrGlesAndroid }

  TGrGlesAndroid = class(TGrGlContext)
  strict private class var
    FSharedConfig: EGLConfig;
    FSharedDisplay: EGLDisplay;
    FSharedSurface: EGLSurface;
  strict private
    FANativeWindow: PANativeWindow;
    FContext: EGLContext;
    FSurface: EGLSurface;
  public
    constructor Create(const AWindow: TWindowHandle; const AANativeWindow: PANativeWindow; const ASurface: EGLSurface; const AContext: EGLContext);
    destructor Destroy; override;
    procedure FlushBuffers; override;
    procedure MakeCurrentContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext; override;
  end;

  TGrGlNativeContext = TGrGlesAndroid;

{$ELSEIF DEFINED(IOS)}

  { TGrGlesIOS }

  TGrGlesIOS = class(TGrGlContext)
  strict private class var
    FLibModule: HMODULE;
  strict private
    FContext: EAGLContext;
  public
    constructor Create(const AWindow: TWindowHandle; const AContext: EAGLContext);
    destructor Destroy; override;
    procedure MakeCurrentContext; override;
    class procedure Finalize; override;
    class procedure Initialize; override;
    class function MakeFromWindow(const AWindow: TWindowHandle): IGrGlContext; override;
  end;

  TGrGlNativeContext = TGrGlesIOS;

{$ENDIF}

{ TGrGlContext }

procedure TGrGlContext.AttachToWindow;
begin
  FWindowAttached := True;
end;

constructor TGrGlContext.Create(const AWindow: TWindowHandle);
begin
  inherited Create;
  FWindow := AWindow;
end;

procedure TGrGlContext.DetachFromWindow;
begin
  FWindowAttached := False;
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

constructor TGrGlWindows.Create(const AWindow: TWindowHandle; const ADC: HDC;
  const AContext: HGLRC);
begin
  inherited Create(AWindow);
  FDC      := ADC;
  FContext := AContext;
end;

destructor TGrGlWindows.Destroy;
begin
  wglDeleteContext(FContext);
  ReleaseDC(WindowHandleToPlatform(Window).Wnd, FDC);
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

class function TGrGlWindows.MakeFromWindow(
  const AWindow: TWindowHandle): IGrGlContext;
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
  LContext: HGLRC;
  LDC: HDC;
  LPixelFormat: Integer;
begin
  if (not (AWindow is TWinWindowHandle)) or (TWinWindowHandle(AWindow).Wnd = 0) then
    Exit(nil);
  LDC := GetDC(TWinWindowHandle(AWindow).Wnd);
  if LDC = 0 then
    RaiseLastOSError;
  try
    LPixelFormat := ChoosePixelFormat(LDC, @PixelFormatDescriptor);
    if (LPixelFormat = 0) or (not SetPixelFormat(LDC, LPixelFormat, @PixelFormatDescriptor)) then
      RaiseLastOSError;
    LContext := wglCreateContext(LDC);
    if LContext = 0 then
      RaiseLastOSError;
    try
      if not wglMakeCurrent(LDC, LContext) then
        RaiseLastOSError;
      Result := TGrGlWindows.Create(AWindow, LDC, LContext);
    except
      wglDeleteContext(LContext);
      raise;
    end;
  except
    ReleaseDC(TWinWindowHandle(AWindow).Wnd, LDC);
    raise;
  end;
end;

{$ELSEIF DEFINED(ANDROID)}

{ TGrGlesAndroid }

constructor TGrGlesAndroid.Create(const AWindow: TWindowHandle;
  const AANativeWindow: PANativeWindow; const ASurface: EGLSurface;
  const AContext: EGLContext);
begin
  inherited Create(AWindow);
  FANativeWindow := AANativeWindow;
  FSurface       := ASurface;
  FContext       := AContext;
end;

destructor TGrGlesAndroid.Destroy;
begin
  eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FContext);
  eglDestroySurface(FSharedDisplay, FSurface);
  ANativeWindow_release(FANativeWindow);
  eglDestroyContext(FSharedDisplay, FContext);
  inherited;
end;

class procedure TGrGlesAndroid.Finalize;
begin
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

  SurfaceAttributes: array[0..4] of EGLint = (EGL_WIDTH, 1, EGL_HEIGHT, 1, EGL_NONE);
var
  LNumConfig: EGLint;
begin
  FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if FSharedDisplay = EGL_NO_DISPLAY then
    raise EGrCanvas.Create('Could not get EGL shared display');
  if eglInitialize(FSharedDisplay, nil, nil) = EGL_FALSE then
    raise EGrCanvas.Create('Could not initialize EGL shared display');
  try
    if eglChooseConfig(FSharedDisplay, @ConfigAttributes[0], @FSharedConfig, 1, @LNumConfig) = EGL_FALSE then
      raise EGrCanvas.Create('Could not choose EGL shared config');
    FSharedSurface := eglCreatePbufferSurface(FSharedDisplay, FSharedConfig, @SurfaceAttributes[0]);
    if FSharedSurface = EGL_NO_SURFACE then
      raise EGrCanvas.Create('Could not create EGL shared surface');
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
  if eglMakeCurrent(FSharedDisplay, LSurface, LSurface, FContext) = EGL_FALSE then
    raise EGrCanvas.Create('Could not make EGL current context');
end;

class function TGrGlesAndroid.MakeFromWindow(
  const AWindow: TWindowHandle): IGrGlContext;
const
  ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
var
  LANativeWindow: PANativeWindow;
  LANativeWindowSurface: JSurface;
  LANativeWindowSurfaceID: Pointer;
  LContext: EGLContext;
  LFormat: GLint;
  LSurface: EGLSurface;
begin
  if AWindow is TAndroidWindowHandle then
  begin
    if TAndroidWindowHandle(AWindow).Holder = nil then
      Exit(nil);
    LANativeWindowSurfaceID := (TAndroidWindowHandle(AWindow).Holder.getSurface as ILocalObject).GetObjectID;
  end
  else if AWindow is TAndroidHandle then
  begin
    if TAndroidHandle(AWindow).Surface = nil then
      Exit(nil);
    LANativeWindowSurface   := TJSurface.JavaClass.init(TAndroidHandle(AWindow).Surface);
    LANativeWindowSurfaceID := TJNIResolver.JavaInstanceToID(LANativeWindowSurface);
  end
  else
    Exit(nil);
  LContext := eglCreateContext(FSharedDisplay, FSharedConfig, EGL_NO_CONTEXT, @ContextAttributes[0]);
  if LContext = EGL_NO_CONTEXT then
    raise EGrCanvas.Create('Could not create EGL context');
  try
    if eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, LContext) = EGL_FALSE then
      raise EGrCanvas.Create('Could not make EGL current context');
    LANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, LANativeWindowSurfaceID);
    if LANativeWindow = nil then
      raise EGrCanvas.Create('Could not get ANativeWindow');
    try
      if eglGetConfigAttrib(FSharedDisplay, FSharedConfig, EGL_NATIVE_VISUAL_ID, @LFormat) = EGL_FALSE then
        raise EGrCanvas.Create('Could not get EGL native visual ID');
      if ANativeWindow_setBuffersGeometry(LANativeWindow, 0, 0, LFormat) <> 0 then
        raise EGrCanvas.Create('Could not change the format of the window buffer');
      LSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, LANativeWindow, nil);
      if LSurface = EGL_NO_SURFACE then
        raise EGrCanvas.Create('Could not create a new EGL window surface');
      Result := TGrGlesAndroid.Create(AWindow, LANativeWindow, LSurface, LContext);
    except
      ANativeWindow_release(LANativeWindow);
      raise;
    end;
  except
    eglDestroyContext(FSharedDisplay, LContext);
    raise;
  end;
end;

{$ELSEIF DEFINED(IOS)}

{ TGrGlesIOS }

constructor TGrGlesIOS.Create(const AWindow: TWindowHandle;
  const AContext: EAGLContext);
begin
  inherited Create(AWindow);
  FContext := AContext;
end;

destructor TGrGlesIOS.Destroy;
begin
  FContext.release;
  inherited;
end;

class procedure TGrGlesIOS.Finalize;
begin
  FreeLibrary(FLibModule);
end;

class procedure TGrGlesIOS.Initialize;
begin
  FLibModule := LoadLibrary(PChar(libGLKit));
  if FLibModule = 0 then
    raise EGrCanvas.Create('Unable to load GLES');
end;

procedure TGrGlesIOS.MakeCurrentContext;
begin
  if not TEAGLContext.OCClass.setCurrentContext(FContext) then
    raise EGrCanvas.Create('Could not make EGL current context');
end;

class function TGrGlesIOS.MakeFromWindow(
  const AWindow: TWindowHandle): IGrGlContext;
var
  LContext: EAGLContext;
  LView: GLKView;
begin
  if (not (AWindow is TiOSWindowHandle)) or (TiOSWindowHandle(AWindow).View = nil) or (not Supports(TiOSWindowHandle(AWindow).View, GLKView, LView)) then
    Exit(nil);
  LContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
  if LContext = nil then
    raise EGrCanvas.Create('Could not create EGL context');
  try
    if not TEAGLContext.OCClass.setCurrentContext(LContext) then
      raise EGrCanvas.Create('Could not make EGL current context');
    LView.setContext(LContext);
    Result := TGrGlesIOS.Create(AWindow, LContext);
  except
    LContext.release;
    raise;
  end;
end;

{$ENDIF}

{ TGrCanvasGl }

procedure TGrCanvasGl.AttachToWindow;
begin
  FContext.AttachToWindow;
end;

class function TGrCanvasGl.ColorType: TSkColorType;
begin
  Result := TSkColorType.RGBA8888;
end;

function TGrCanvasGl.CreateContext: IGrDirectContext;
begin
  FContext := TGrGlNativeContext.MakeFromWindow(Parent);
  if not Assigned(FContext) then
    Exit(nil);
  Result := TGrDirectContext.MakeGl;
end;

procedure TGrCanvasGl.DetachFromWindow;
begin
  FContext.DetachFromWindow;
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
