{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Vcl;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,

  { Skia }
  Skia;

type
  ESkVcl = class(SkException);
  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas);

  { TSkBitmapHelper }

  TSkBitmapHelper = class helper for TBitmap
  strict private
    procedure FlipPixels(const AWidth, AHeight: Integer; const ASrcPixels: PByte; const ASrcStride: Integer; const ADestPixels: PByte; const ADestStride: Integer); inline;
  public
    procedure SkiaDraw(const AProc: TSkDrawProc; const AStartClean: Boolean = True);
    function ToSkImage: ISkImage;
  end;

  TSkDrawEvent = procedure(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single) of object;

  { TSkCustomControl }

  TSkCustomControl = class abstract(TGraphicControl)
  strict private
    FDrawBuffer: HBITMAP;
    FDrawBufferData: Pointer;
    FDrawBufferStride: Integer;
    FDrawCached: Boolean;
    FDrawCacheEnabled: Boolean;
    FOnDraw: TSkDrawEvent;
    FOpacity: Byte;
    procedure CreateBuffer(const AMemDC: HDC; out ABuffer: HBITMAP; out AData: Pointer; out AStride: Integer);
    procedure DeleteBuffers;
    procedure SetDrawCacheEnabled(const AValue: Boolean);
    procedure SetOnDraw(const AValue: TSkDrawEvent);
    procedure SetOpacity(const AValue: Byte);
  {$IF CompilerVersion < 33}
  strict protected
    FScaleFactor: Single;
    procedure ChangeScale(M, D: Integer); override;
    property ScaleFactor: Single read FScaleFactor;
  {$ENDIF}
  strict protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); virtual;
    procedure DrawDesignBorder(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure Paint; override; final;
    procedure Resize; override;
    property DrawCacheEnabled: Boolean read FDrawCacheEnabled write SetDrawCacheEnabled default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDraw: TSkDrawEvent read FOnDraw write SetOnDraw;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TSkPaintBox }

  TSkPaintBox = class(TSkCustomControl);

  TSkSvgSource = type string;

  { TSkSvgBrush }

  TSkSvgBrush = class(TPersistent)
  strict private
    FDOM: ISkSVGDOM;
    FOnChanged: TNotifyEvent;
    FOverrideColor: TAlphaColor;
    FSource: TSkSvgSource;
    function IsOverrideColorStored: Boolean;
    procedure SetOverrideColor(const AValue: TAlphaColor);
    procedure SetSource(const AValue: TSkSvgSource);
  strict protected
    procedure DoChanged; virtual;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Render(const ACanvas: ISkCanvas; const ADestRect: TRectF; const AOpacity: Single);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property OverrideColor: TAlphaColor read FOverrideColor write SetOverrideColor stored IsOverrideColorStored;
    property Source: TSkSvgSource read FSource write SetSource;
  end;

  { TSkSvg }

  TSkSvg = class(TSkCustomControl)
  strict private
    FSvg: TSkSvgBrush;
    procedure SetSvg(const AValue: TSkSvgBrush);
    procedure SvgChanged(ASender: TObject);
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Svg: TSkSvgBrush read FSvg write SetSvg;
  end;

  { TSkCustomAnimatedControl }

  TSkCustomAnimatedControl = class abstract(TSkCustomControl)
  strict private
    FAbsoluteShowing: Boolean;
    FAbsoluteShowingCached: Boolean;
    FAnimation: TTimer;
    FAnimationStartTickCount: Cardinal;
    FFixedProgress: Boolean;
    FLoop: Boolean;
    FOnAnimationFinished: TNotifyEvent;
    FOnAnimationProgress: TNotifyEvent;
    FOnAnimationStart: TNotifyEvent;
    FProgress: Double;
    FProgressChangedManually: Boolean;
    procedure AnimationTimer(ASender: TObject);
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMShowingChanged(var AMessage: TMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;
    function GetAbsoluteShowing: Boolean;
    function GetRunningAnimation: Boolean;
    procedure SetFixedProgress(const AValue: Boolean);
    procedure SetLoop(const AValue: Boolean);
    procedure SetProgress(AValue: Double);
  protected
    function CanRunAnimation: Boolean; virtual;
    procedure CheckAnimation;
    procedure DoAnimationFinished; virtual;
    procedure DoAnimationProgress; virtual;
    procedure DoAnimationStart; virtual;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function GetDuration: Double; virtual; abstract;
    procedure RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single); virtual; abstract;
    property AbsoluteShowing: Boolean read GetAbsoluteShowing;
    property Duration: Double read GetDuration;
    property FixedProgress: Boolean read FFixedProgress write SetFixedProgress;
    property Progress: Double read FProgress write SetProgress;
    property Loop: Boolean read FLoop write SetLoop;
    property OnAnimationFinished: TNotifyEvent read FOnAnimationFinished write FOnAnimationFinished;
    property OnAnimationProgress: TNotifyEvent read FOnAnimationProgress write FOnAnimationProgress;
    property OnAnimationStart: TNotifyEvent read FOnAnimationStart write FOnAnimationStart;
    property RunningAnimation: Boolean read GetRunningAnimation;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSkLottieSource = type string;
  TSkLottieFormat = (Json, Tgs);

  { TSkLottieAnimation }

  TSkLottieAnimation = class(TSkCustomAnimatedControl)
  strict private
    FSkottie: ISkottieAnimation;
    FSource: TSkLottieSource;
    procedure ReadTgs(AStream: TStream);
    procedure SetSource(const AValue: TSkLottieSource);
    procedure WriteTgs(AStream: TStream);
  protected
    function CanRunAnimation: Boolean; override;
    procedure DefineProperties(AFiler: TFiler); override;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function GetDuration: Double; override;
    procedure RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single); override;
  public
    property FixedProgress;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream; const AFormat: TSkLottieFormat = TSkLottieFormat.Json);
    property Progress;
    property RunningAnimation;
    property Skottie: ISkottieAnimation read FSkottie;
  published
    property Loop default True;
    property Source: TSkLottieSource read FSource write SetSource stored False;
    property OnAnimationFinished;
    property OnAnimationProgress;
    property OnAnimationStart;
  end;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.ZLib;

{ TSkBitmapHelper }

procedure TSkBitmapHelper.FlipPixels(const AWidth, AHeight: Integer;
  const ASrcPixels: PByte; const ASrcStride: Integer; const ADestPixels: PByte;
  const ADestStride: Integer);
var
  I: Integer;
begin
  for I := 0 to AHeight - 1 do
    Move(ASrcPixels[I * ASrcStride], ADestPixels[(AHeight - I - 1) * ADestStride], AWidth * 4);
end;

procedure TSkBitmapHelper.SkiaDraw(const AProc: TSkDrawProc; const AStartClean: Boolean);
var
  LPixmap: ISkPixmap;
  LSurface: ISkSurface;
begin
  if Empty then
    raise ESkVcl.Create('Invalid bitmap');
  if not SupportsPartialTransparency then
  begin
    PixelFormat := TPixelFormat.pf32bit;
    AlphaFormat := TAlphaFormat.afPremultiplied;
  end;
  LSurface := TSkSurface.MakeRaster(Width, Height);
  LPixmap  := LSurface.PeekPixels;
  if AStartClean then
    LSurface.Canvas.Clear(TAlphaColors.Null)
  else
    FlipPixels(Width, Height, ScanLine[Height - 1], BytesPerScanLine(Width, 32, 32), LPixmap.Pixels, LPixmap.RowBytes);
  AProc(LSurface.Canvas);
  FlipPixels(Width, Height, LPixmap.Pixels, LPixmap.RowBytes, ScanLine[Height - 1], BytesPerScanLine(Width, 32, 32));{}
end;

function TSkBitmapHelper.ToSkImage: ISkImage;
var
  LPixels: Pointer;
  LStride: Integer;
begin
  if Empty then
    raise ESkVcl.Create('Invalid bitmap');
  if not SupportsPartialTransparency then
  begin
    PixelFormat := TPixelFormat.pf32bit;
    AlphaFormat := TAlphaFormat.afPremultiplied;
  end;
  LStride := BytesPerScanLine(Width, 32, 32);
  GetMem(LPixels, LStride * Height);
  try
    FlipPixels(Width, Height, ScanLine[Height - 1], BytesPerScanLine(Width, 32, 32), LPixels, LStride);
    Result := TSkImage.MakeRaster(TSkImageInfo.Create(Width, Height), LPixels, LStride);
  finally
    FreeMem(LPixels);
  end;
end;

{ TSkCustomControl }

{$IF CompilerVersion < 33}
procedure TSkCustomControl.ChangeScale(M, D: Integer);
begin
  if M <> D then
    FScaleFactor := FScaleFactor * M / D;
  inherited;
end;
{$ENDIF}

constructor TSkCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  FDrawCacheEnabled := True;
  FOpacity := 255;
  {$IF CompilerVersion < 33}
  FScaleFactor := 1;
  {$ENDIF}
end;

procedure TSkCustomControl.CreateBuffer(const AMemDC: HDC; out ABuffer: HBITMAP;
  out AData: Pointer; out AStride: Integer);
const
  ColorMasks: array[0..2] of DWORD = ($00FF0000, $0000FF00, $000000FF);
var
  LBitmapInfo: PBitmapInfo;
begin
  AStride := BytesPerScanline(Width, 32, 32);
  GetMem(LBitmapInfo, SizeOf(TBitmapInfoHeader) + SizeOf(ColorMasks));
  try
    LBitmapInfo.bmiHeader := Default(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth       := Width;
    LBitmapInfo.bmiHeader.biHeight      := -Height;
    LBitmapInfo.bmiHeader.biPlanes      := 1;
    LBitmapInfo.bmiHeader.biBitCount    := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_BITFIELDS;
    LBitmapInfo.bmiHeader.biSizeImage   := AStride * Height;
    Move(ColorMasks[0], LBitmapInfo.bmiColors[0], SizeOf(ColorMasks));
    ABuffer := CreateDIBSection(AMemDC, LBitmapInfo^, DIB_RGB_COLORS, AData, 0, 0);
    if ABuffer <> 0 then
      GdiFlush;
  finally
    FreeMem(LBitmapInfo);
  end;
end;

procedure TSkCustomControl.DeleteBuffers;
begin
  if FDrawBuffer <> 0 then
  begin
    FDrawCached := False;
    DeleteObject(FDrawBuffer);
    FDrawBuffer := 0;
  end;
end;

destructor TSkCustomControl.Destroy;
begin
  DeleteBuffers;
  inherited;
end;

procedure TSkCustomControl.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  if csDesigning in ComponentState then
    DrawDesignBorder(ACanvas, ADest, AOpacity);
end;

procedure TSkCustomControl.DrawDesignBorder(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
const
  DesignBorderColor = $A0909090;
var
  R: TRectF;
  LPaint: ISkPaint;
begin
  R := ADest;
  InflateRect(R, -0.5, -0.5);
  ACanvas.Save;
  try
    LPaint := TSkPaint.Create;
    LPaint.AlphaF := AOpacity;
    LPaint.Color := DesignBorderColor;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.PathEffect := TSKPathEffect.MakeDash(TArray<Single>.Create(3, 1), 0);
    LPaint.StrokeWidth := 1;
    ACanvas.DrawRect(R, LPaint);
  finally
    ACanvas.Restore;
  end;
end;

procedure TSkCustomControl.Paint;

  procedure InternalDraw;
  var
    LSurface: ISkSurface;
    LDestRect: TRectF;
  begin
    LSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(Width, Height), FDrawBufferData, FDrawBufferStride);
    LSurface.Canvas.Clear(TAlphaColors.Null);
    LSurface.Canvas.Concat(TMatrix.CreateScaling(ScaleFactor, ScaleFactor));
    LDestRect := TRectF.Create(0, 0, Width / ScaleFactor, Height / ScaleFactor);
    Draw(LSurface.Canvas, LDestRect, 1);
    if Assigned(FOnDraw) then
      FOnDraw(Self, LSurface.Canvas, LDestRect, 1);
    FDrawCached := True;
  end;

const
  BlendFunction: TBlendFunction = (BlendOp: AC_SRC_OVER; BlendFlags: 0; SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA);
var
  LOldObj: HGDIOBJ;
  LDrawBufferDC: HDC;
  LBlendFunction: TBlendFunction;
begin
  LDrawBufferDC := CreateCompatibleDC(0);
  if LDrawBufferDC <> 0 then
    try
      if FDrawBuffer = 0 then
        CreateBuffer(LDrawBufferDC, FDrawBuffer, FDrawBufferData, FDrawBufferStride);
      if FDrawBuffer <> 0 then
      begin
        LOldObj := SelectObject(LDrawBufferDC, FDrawBuffer);
        try
          if (not FDrawCacheEnabled) or (not FDrawCached) then
            InternalDraw;
          LBlendFunction := BlendFunction;
          LBlendFunction.SourceConstantAlpha := FOpacity;
          AlphaBlend(Canvas.Handle, 0, 0, Width, Height, LDrawBufferDC, 0, 0, Width, Height, LBlendFunction);
        finally
          if LOldObj <> 0 then
            SelectObject(LDrawBufferDC, LOldObj);
        end;
      end;
    finally
      DeleteDC(LDrawBufferDC);
    end;
end;

procedure TSkCustomControl.Redraw;
begin
  FDrawCached := False;
  Repaint;
end;

procedure TSkCustomControl.Resize;
begin
  DeleteBuffers;
  inherited;
end;

procedure TSkCustomControl.SetDrawCacheEnabled(const AValue: Boolean);
begin
  if FDrawCacheEnabled <> AValue then
  begin
    FDrawCacheEnabled := AValue;
    if not AValue then
      Repaint;
  end;
end;

procedure TSkCustomControl.SetOnDraw(const AValue: TSkDrawEvent);
begin
  if TMethod(FOnDraw) <> TMethod(AValue) then
  begin
    FOnDraw := AValue;
    Redraw;
  end;
end;

procedure TSkCustomControl.SetOpacity(const AValue: Byte);
begin
  if FOpacity <> AValue then
  begin
    FOpacity := AValue;
    Repaint;
  end;
end;

{ TSkSvgBrush }

procedure TSkSvgBrush.Assign(ASource: TPersistent);
begin
  if ASource is TSkSvgBrush then
  begin
    FOverrideColor := TSkSvgBrush(ASource).FOverrideColor;
    FSource := TSkSvgBrush(ASource).FSource;
    FDOM := TSkSvgBrush(ASource).FDOM;
    DoChanged;
  end
  else
    inherited;
end;

procedure TSkSvgBrush.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TSkSvgBrush.IsOverrideColorStored: Boolean;
begin
  Result := FOverrideColor <> Default(TAlphaColor);
end;

procedure TSkSvgBrush.Render(const ACanvas: ISkCanvas; const ADestRect: TRectF;
  const AOpacity: Single);

  function PlaceIntoTopLeft(const ASourceRect, ADesignatedArea: TRectF): TRectF;
  begin
    Result := ASourceRect;
    if (ASourceRect.Width > ADesignatedArea.Width) or (ASourceRect.Height > ADesignatedArea.Height) then
      Result := Result.FitInto(ADesignatedArea);
    Result.SetLocation(ADesignatedArea.TopLeft);
  end;

  procedure DrawOverrideColor(const ACanvas: ISkCanvas; const ADOM: ISkSVGDOM;
    const ASvgRect, ADestRect, AWrappedDest: TRectF);
  var
    LSurface: ISkSurface;
    LImage: ISkImage;
    LPaint: ISkPaint;
  begin
    LSurface := TSkSurface.MakeRaster(Round(AWrappedDest.Width), Round(AWrappedDest.Height));
    LSurface.Canvas.Clear(TAlphaColors.Null);
    LSurface.Canvas.Scale(AWrappedDest.Width / ASvgRect.Width, AWrappedDest.Height / ASvgRect.Height);
    FDOM.Render(LSurface.Canvas);
    LImage := LSurface.MakeImageSnapshot;
    LPaint := TSkPaint.Create;
    if FOverrideColor <> TAlphaColors.Null then
      LPaint.ColorFilter := TSkColorFilter.MakeBlend(FOverrideColor, TSkBlendMode.SrcIn);
    LPaint.Style := TSkPaintStyle.Fill;
    ACanvas.DrawImage(LImage, AWrappedDest.Left, AWrappedDest.Top, LPaint);
  end;

var
  LStream: TStringStream;
  LSvgRect: TRectF;
  LWrappedDest: TRectF;
begin
  if (FSource <> '') and not ADestRect.IsEmpty then
  begin
    if not Assigned(FDOM) then
    begin
      LStream := TStringStream.Create(FSource);
      try
        FDOM := TSkSVGDOM.Make(LStream);
      finally
        LStream.Free;
      end;
    end;
    if not Assigned(FDOM) then
      Exit;
    if FDOM.ContainerSize.IsZero then
      FDOM.ContainerSize := ADestRect.Size;
    LSvgRect := TRectF.Create(PointF(0, 0), FDOM.ContainerSize);
    if LSvgRect.IsEmpty then
      Exit;
    LWrappedDest := LSvgRect.FitInto(ADestRect);
    if FOverrideColor <> TAlphaColors.Null then
      DrawOverrideColor(ACanvas, FDOM, LSvgRect, ADestRect, LWrappedDest)
    else
    begin
      ACanvas.Translate(LWrappedDest.Left, LWrappedDest.Top);
      ACanvas.Scale(LWrappedDest.Width / LSvgRect.Width, LWrappedDest.Height / LSvgRect.Height);
      FDOM.Render(ACanvas);
    end;
  end;
end;

procedure TSkSvgBrush.SetOverrideColor(const AValue: TAlphaColor);
begin
  if FOverrideColor <> AValue then
  begin
    FOverrideColor := AValue;
    if FSource <> '' then
      DoChanged;
  end;
end;

procedure TSkSvgBrush.SetSource(const AValue: TSkSvgSource);
begin
  if FSource <> AValue then
  begin
    FSource := AValue;
    FDOM := nil;
    DoChanged;
  end;
end;

{ TSkSvg }

constructor TSkSvg.Create(AOwner: TComponent);
begin
  inherited;
  FSvg := TSkSvgBrush.Create;
  FSvg.OnChanged := SvgChanged;
end;

destructor TSkSvg.Destroy;
begin
  FSvg.Free;
  inherited;
end;

procedure TSkSvg.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  inherited;
  FSvg.Render(ACanvas, ADest, AOpacity);
end;

procedure TSkSvg.SetSvg(const AValue: TSkSvgBrush);
begin
  FSvg.Assign(AValue);
end;

procedure TSkSvg.SvgChanged(ASender: TObject);
begin
  Redraw;
end;

{ TSkCustomAnimatedControl }

procedure TSkCustomAnimatedControl.AnimationTimer(ASender: TObject);
begin
  Repaint;
end;

function TSkCustomAnimatedControl.CanRunAnimation: Boolean;
begin
  Result := Assigned(Parent) and (not FFixedProgress) and
    ([csDestroying, csDesigning] * ComponentState = []) and
    AbsoluteShowing and (FLoop or not SameValue(FProgress, 1, TEpsilon.Matrix));
end;

procedure TSkCustomAnimatedControl.CheckAnimation;

  procedure FixStartTickCount;
  var
    LNewTickCount: Int64;
  begin
    LNewTickCount := TThread.GetTickCount - Round(FProgress * Duration * 1000);
    if LNewTickCount < 0 then
      LNewTickCount := High(Cardinal) + LNewTickCount;
    FAnimationStartTickCount := Cardinal(LNewTickCount);
  end;

var
  LCanRunAnimation: Boolean;
begin
  if Assigned(FAnimation) then
  begin
    LCanRunAnimation := CanRunAnimation;
    if FAnimation.Enabled <> LCanRunAnimation then
    begin
      FAnimation.Enabled := LCanRunAnimation;
      if LCanRunAnimation then
      begin
        Parent.DoubleBuffered := True;
        FixStartTickCount;
        DoAnimationStart;
      end
      else
        DoAnimationFinished;
    end;
  end;
end;

procedure TSkCustomAnimatedControl.CMEnabledChanged(var AMessage: TMessage);
begin
  CheckAnimation;
  inherited;
end;

procedure TSkCustomAnimatedControl.CMShowingChanged(var AMessage: TMessage);
begin
  FAbsoluteShowingCached := False;
  if (not FFixedProgress) and (not FProgressChangedManually) and (not FAbsoluteShowing) and AbsoluteShowing then
  begin
    FProgress := 0;
    FAnimationStartTickCount := TThread.GetTickCount;
  end;
  CheckAnimation;
  inherited;
end;

procedure TSkCustomAnimatedControl.CMVisibleChanged(var AMessage: TMessage);
begin
  FAbsoluteShowingCached := False;
  if (not FFixedProgress) and (not FProgressChangedManually) and (not FAbsoluteShowing) and AbsoluteShowing then
  begin
    FProgress := 0;
    FAnimationStartTickCount := TThread.GetTickCount;
  end;
  CheckAnimation;
end;

constructor TSkCustomAnimatedControl.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    FProgress := 0.5;
    FFixedProgress := True;
  end;
  FLoop := True;
  FAnimation := TTimer.Create(Self);
  FAnimation.Enabled := False;
  FAnimation.Interval := 15;
  FAnimation.OnTimer := AnimationTimer;
  FAbsoluteShowing := Visible;
  FAbsoluteShowingCached := True;
  DrawCacheEnabled := False;
end;

procedure TSkCustomAnimatedControl.DoAnimationFinished;
begin
  if Assigned(FOnAnimationFinished) then
    FOnAnimationFinished(Self);
  FProgressChangedManually := False;
end;

procedure TSkCustomAnimatedControl.DoAnimationProgress;
begin
  if Assigned(FOnAnimationProgress) then
    FOnAnimationProgress(Self);
end;

procedure TSkCustomAnimatedControl.DoAnimationStart;
begin
  if Assigned(FOnAnimationStart) then
    FOnAnimationStart(Self);
end;

procedure TSkCustomAnimatedControl.Draw(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);

  procedure FixElapsedSeconds(const ACurrentTickCount: Cardinal;
    var AStartTickCount: Cardinal; var AElapsedSeconds: Double);
  var
    LMillisecondsElapsed: Int64;
  begin
    Assert(ACurrentTickCount < AStartTickCount);
    if ACurrentTickCount >= Cardinal(Ceil(Duration * 1000)) then
    begin
      if FLoop then
      begin
        LMillisecondsElapsed := ACurrentTickCount + High(Cardinal) - AStartTickCount;
        LMillisecondsElapsed := LMillisecondsElapsed mod Round(Duration * 1000);
        Assert(ACurrentTickCount > LMillisecondsElapsed);
        FAnimationStartTickCount := Cardinal(ACurrentTickCount - LMillisecondsElapsed);
      end
      else
        AStartTickCount := ACurrentTickCount - Cardinal(Ceil(Duration * 1000));
      AElapsedSeconds := (ACurrentTickCount - AStartTickCount) / 1000;
    end
    else
    begin
      LMillisecondsElapsed := ACurrentTickCount + High(Cardinal) - AStartTickCount;
      AElapsedSeconds := LMillisecondsElapsed / 1000;
    end;
    Assert(AElapsedSeconds >= 0);
  end;

  function CalcProgress: Double;
  var
    LElapsedSeconds: Double;
    LCurrentTickCount: Cardinal;
  begin
    if Enabled then
    begin
      LCurrentTickCount := TThread.GetTickCount;
      LElapsedSeconds := (LCurrentTickCount - FAnimationStartTickCount) / 1000;
      if LElapsedSeconds < 0 then
        FixElapsedSeconds(LCurrentTickCount, FAnimationStartTickCount, LElapsedSeconds);
      if FLoop then
      begin
        {$IF CompilerVersion >= 29} // Delphi XE8
        LElapsedSeconds := FMod(LElapsedSeconds, Duration);
        {$ELSE}
        LElapsedSeconds := (Round(LElapsedSeconds * 1000) mod Round(Duration * 1000)) / 1000;
        {$ENDIF}
      end
      else
        LElapsedSeconds := Min(LElapsedSeconds, Duration);
      if SameValue(Duration, 0, TEpsilon.Matrix) then
        Result := 1
      else
        Result := LElapsedSeconds / Duration;
    end
    else
      Result := FProgress;
  end;

var
  LProgress: Double;
begin
  inherited;
  if FFixedProgress then
    LProgress := FProgress
  else
    LProgress := CalcProgress;
  RenderFrame(ACanvas, ADest, LProgress, AOpacity);
  if not SameValue(LProgress, FProgress, TEpsilon.Matrix) then
  begin
    FProgress := LProgress;
    DoAnimationProgress;
  end;
  if (not FLoop) and SameValue(LProgress, 1, TEpsilon.Matrix) then
  begin
    FProgress := 1;
    CheckAnimation;
  end;
end;

function TSkCustomAnimatedControl.GetAbsoluteShowing: Boolean;

  function GetParentedShowing: Boolean;
  var
    LControl: TWinControl;
  begin
    if not Visible then
      Exit(False);
    LControl := Parent;
    while LControl <> nil do
    begin
      if not LControl.Showing then
        Exit(False);
      LControl := LControl.Parent;
    end;
    Result := True;
  end;

begin
  if not FAbsoluteShowingCached then
  begin
    FAbsoluteShowing := GetParentedShowing;
    FAbsoluteShowingCached := True;
  end;
  Result := FAbsoluteShowing;
end;

function TSkCustomAnimatedControl.GetRunningAnimation: Boolean;
begin
  Result := Assigned(FAnimation) and FAnimation.Enabled;
end;

procedure TSkCustomAnimatedControl.SetFixedProgress(const AValue: Boolean);
begin
  if FFixedProgress <> AValue then
  begin
    FFixedProgress := AValue;
    CheckAnimation;
  end;
end;

procedure TSkCustomAnimatedControl.SetLoop(const AValue: Boolean);
begin
  if FLoop <> AValue then
  begin
    FLoop := AValue;
    CheckAnimation;
  end;
end;

procedure TSkCustomAnimatedControl.SetProgress(AValue: Double);
begin
  FProgressChangedManually := True;
  AValue := EnsureRange(AValue, 0, 1);
  if not SameValue(FProgress, AValue, TEpsilon.Matrix) then
  begin
    FProgress := AValue;
    if SameValue(FProgress, 0, TEpsilon.Matrix) then
      FAnimationStartTickCount := TThread.GetTickCount;
    CheckAnimation;
    DoAnimationProgress;
    Repaint;
  end;
end;

{ TSkLottieAnimation }

function TSkLottieAnimation.CanRunAnimation: Boolean;
begin
  Result := Assigned(FSkottie) and inherited;
end;

procedure TSkLottieAnimation.DefineProperties(AFiler: TFiler);

  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TSkLottieAnimation)) or (TSkLottieAnimation(AFiler.Ancestor).Source <> FSource)
    else
      Result := FSource <> '';
  end;

begin
  inherited;
  AFiler.DefineBinaryProperty('TGS', ReadTgs, WriteTgs, DoWrite);
end;

procedure TSkLottieAnimation.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  if Assigned(FSkottie) then
    inherited
  else if csDesigning in ComponentState then
    DrawDesignBorder(ACanvas, ADest, AOpacity);
end;

function TSkLottieAnimation.GetDuration: Double;
begin
  if Assigned(FSkottie) then
    Result := FSkottie.Duration
  else
    Result := 0;
end;

procedure TSkLottieAnimation.LoadFromFile(const AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TSkLottieAnimation.LoadFromStream(const AStream: TStream);

  function IsTgs: Boolean;
  const
    GZipSignature: Word = $8B1F;
  var
    LSignature: Word;
    LSavePosition: Int64;
  begin
    if AStream.Size < 2 then
      Exit(False);
    LSavePosition := AStream.Position;
    try
      Result := (AStream.ReadData(LSignature) = SizeOf(Word)) and (LSignature = GZipSignature);
    finally
      AStream.Position := LSavePosition;
    end;
  end;

  function ReadStreamBuffer(const AStream: TStream): TBytes;
  begin
    SetLength(Result, AStream.Size - AStream.Position);
    if Length(Result) > 0 then
      AStream.ReadBuffer(Result, 0, Length(Result));
  end;

  function ReadTgsStreamBuffer(const AStream: TStream): TBytes;
  var
    LDecompressionStream: TDecompressionStream;
  begin
    LDecompressionStream := TDecompressionStream.Create(AStream, 31);
    try
      Result := ReadStreamBuffer(LDecompressionStream);
    finally
      LDecompressionStream.Free;
    end;
  end;

var
  LBuffer: TBytes;
begin
  if IsTgs then
    LBuffer := ReadTgsStreamBuffer(AStream)
  else
    LBuffer := ReadStreamBuffer(AStream);
  Source := TEncoding.UTF8.GetString(LBuffer);
end;

procedure TSkLottieAnimation.ReadTgs(AStream: TStream);
begin
  if AStream.Size = 0 then
    Source := ''
  else
    LoadFromStream(AStream);
end;

procedure TSkLottieAnimation.RenderFrame(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
begin
  FSkottie.SeekFrameTime(AProgress * Duration);
  FSkottie.Render(ACanvas, ADest);
end;

procedure TSkLottieAnimation.SaveToFile(const AFileName: string);
const
  FormatExtension: array[TSkLottieFormat] of string = ('.json', '.tgs');
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    if AFileName.EndsWith(FormatExtension[TSkLottieFormat.Tgs], True) then
      SaveToStream(LStream, TSkLottieFormat.Tgs)
    else
      SaveToStream(LStream, TSkLottieFormat.Json);
  finally
    LStream.Free;
  end;
end;

procedure TSkLottieAnimation.SaveToStream(const AStream: TStream;
  const AFormat: TSkLottieFormat);

  function JsonToTgs(const ABytes: TBytes): TBytes;
  var
    LMemoryStream: TMemoryStream;
    LCompressStream: TCompressionStream;
  begin
    LMemoryStream := TMemoryStream.Create;
    try
      LCompressStream := TCompressionStream.Create(LMemoryStream, TZCompressionLevel.zcMax, 31);
      try
        LCompressStream.WriteBuffer(ABytes, Length(ABytes));
      finally
        LCompressStream.Free;
      end;
      SetLength(Result, LMemoryStream.Size);
      if LMemoryStream.Size > 0 then
        System.Move(LMemoryStream.Memory^, Result[0], LMemoryStream.Size);
    finally
      LMemoryStream.Free;
    end;
  end;

var
  LBuffer: TBytes;
begin
  LBuffer := TEncoding.UTF8.GetBytes(FSource);
  if AFormat = TSkLottieFormat.Tgs then
    LBuffer := JsonToTgs(LBuffer);
  if Length(LBuffer) > 0 then
    AStream.WriteBuffer(LBuffer, Length(LBuffer));
end;

procedure TSkLottieAnimation.SetSource(const AValue: TSkLottieSource);
begin
  if FSource <> string(AValue).Trim then
  begin
    FSource := string(AValue).Trim;
    if FSource = '' then
      FSkottie := nil
    else
      FSkottie := TSkottieAnimation.Make(FSource);
    if not FixedProgress then
      Progress := 0;
    CheckAnimation;
    Repaint;
  end;
end;

procedure TSkLottieAnimation.WriteTgs(AStream: TStream);
begin
  if FSource <> '' then
    SaveToStream(AStream, TSkLottieFormat.Tgs);
end;

end.

