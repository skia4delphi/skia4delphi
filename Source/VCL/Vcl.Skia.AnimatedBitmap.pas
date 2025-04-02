unit Vcl.Skia.AnimatedBitmap;

interface
uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  System.Messaging,
  System.Generics.Collections,
  Vcl.BaseImageCollection,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ImgList,


  { Skia }
  System.Skia,
  Vcl.Skia.AnimatedImageList,
  Vcl.skia;


type
  TBackgroundBitmapProc = reference to procedure(var ABackground: TBitmap);

  TSkCustomAnimatedBitmap = class abstract(Tcomponent,ISkControlRenderTarget)
  protected type
    { TCustomAnimation }
    TCustomAnimation = class(TSkCustomAnimation)
    strict private
      FInsideDoProcess: Boolean;
    protected
      procedure DoChanged; override;
      procedure DoFinish; override;
      procedure DoProcess; override;
      procedure DoStart; override;
    end;

  public type
    { TSource }
    TSource = class(TPersistent)
    public type
      TChangeProc = procedure of object;
    strict private
      FData: TBytes;
      FOnChange: TChangeProc;
      procedure SetData(const AValue: TBytes);
    public
      constructor Create(const AOnChange: TChangeProc);
      procedure Assign(ASource: TPersistent); override;
      function Equals(AObject: TObject): Boolean; override;
      property Data: TBytes read FData write SetData;
    end;

  strict private
    FOnAnimationBeforeDraw: TSkAnimationDrawEvent;
    FOnAnimationAfterDraw: TSkAnimationDrawEvent;
    FOnAnimationFinish: TNotifyEvent;
    FOnAnimationProcess: TNotifyEvent;
    FOnAnimationStart: TNotifyEvent;

    FCodec: TAnimationCodec;
    FSource: TSource;
    FBitmap: TBitmap;
    FWrapMode: TSkAnimatedImageWrapMode;
    FDrawCacheKind: TSkDrawCacheKind;
    FBackendRender: TSkControlRenderBackend;
    FRender: ISkControlRender;
    FOpacity: byte;

    FBackgroundColor: TAlphaColor;
    FNeedRendering: boolean;
    FAutoDraw: boolean;
    Ftag_pointer: pointer;
    Ftag: NativeUint;

    procedure CheckDuration;
    function GetRender: ISkControlRender;
    function GetRenderedBitmap: TBitmap;
    function GetRenderSize: TSize;
    procedure SetSource(const AValue: TSource);
    procedure SetWrapMode(const AValue: TSkAnimatedImageWrapMode);
    procedure SetDrawCacheKind(const AValue: TSkDrawCacheKind);
    procedure SetBackendRender(const AValue: TSkControlRenderBackend);
    procedure SetOpacity(const Value: byte);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetRenderSize(const Value: TSize);
    procedure SetBackgroundColor(const Value: TAlphaColor);


  strict protected
    FAnimation: TCustomAnimation;
    procedure DefineProperties(AFiler: TFiler); override;
    procedure ReadData(AStream: TStream); virtual;
    procedure WriteData(AStream: TStream); virtual;

    function CanRunAnimation: Boolean; virtual;
    procedure CheckAnimation;
    function CreateAnimation: TCustomAnimation; virtual;

    procedure DoAnimationChanged; virtual;
    procedure DoAnimationFinish; virtual;
    procedure DoAnimationProcess; virtual;
    procedure DoAnimationStart; virtual;
    procedure ReadState(AReader: TReader); override;
    procedure RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single); virtual;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function GetCanvas: TCanvas;
    function GetDeviceContext(var WindowHandle: HWND): HDC;
    function GetDrawCacheKind: TSkDrawCacheKind;
    function GetHeight: Integer;
    function GetScaleFactor: Single;
    function GetWidth: Integer;

    procedure SourceChange; virtual;
    function MakeRender(ABackendRender: TSkControlRenderBackend): ISkControlRender; virtual;


    procedure RealPaint(const ABackground: TBitmap);
    procedure DoPaint;
    procedure Loaded; Override;

    property Codec: TAnimationCodec read FCodec;
    property Render: ISkControlRender read GetRender;


    property Canvas: TCanvas read GetCanvas;
    property DrawCacheKind: TSkDrawCacheKind read GetDrawCacheKind write SetDrawCacheKind;
    property BackendRender: TSkControlRenderBackend read FBackendRender write SetBackendRender default TSkControlRenderBackend.Default;

    property ScaleFactor: Single read GetScaleFactor;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);

    /// <summary>
    ///   Allocate a bitmap at the rendering size, fill it with the background color, and return it.
    ///  The user is responsible to free this allocated bitmap.
    /// </summary>
    function MakeBackground: TBitmap;

    /// <summary>
    ///   Set the size of the rendering surface, optionally provide a background for the surface, and do the rendering.
    ///  The returned bitmap is cached in the object.
    /// </summary>
    function DoRender(const Awidth,Aheight: integer; ABackground: TBackgroundBitmapProc = nil ): TBitmap;

    /// <summary>
    ///   The rendered bitmap
    /// </summary>
    property RenderedBitmap: TBitmap read GetRenderedBitmap;
    /// <summary>
    ///   A user defined opaque tag
    /// </summary>
    property Tag: NativeUint read Ftag write Ftag;
    /// <summary>
    ///   a user defined opaque tag pointer
    ///  Can be used to store a PVirtualNode for instance
    /// </summary>
    property tag_pointer: pointer read Ftag_pointer write Ftag_pointer;
    /// <summary>
    ///   Source of the animation
    /// </summary>
    property Source: TSource read FSource write SetSource;
    /// <summary>
    ///   How the animation must be resized in the render surface
    /// </summary>
    property WrapMode: TSkAnimatedImageWrapMode read FWrapMode write SetWrapMode default TSkAnimatedImageWrapMode.Fit;
    /// <summary>
    ///   Opacity: 0 for transparent, 255 for opaque
    /// </summary>
    property Opacity: byte read FOpacity write SetOpacity default 255;
    /// <summary>
    ///   Rendering height
    /// </summary>
    property Height: Integer read GetHeight write SetHeight;
    /// <summary>
    ///   Rendering width
    /// </summary>
    property Width: Integer read GetWidth write SetWidth;
    /// <summary>
    ///   Size of the rendered bitmap
    /// </summary>
    property RenderSize: TSize read GetRenderSize write SetRenderSize;
    /// <summary>
    ///   If true, the rendering is automatically trigered by the TAnimation.
    ///  otherwise rendering is done if needed when calling DoRender or accessing RenderedBitmap property
    /// </summary>
    property AutoDraw: boolean read FAutoDraw write FAutoDraw;
    /// <summary>
    ///   fill color for the background bitmap
    /// </summary>
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor Default TAlphaColorRec.Null;
    /// <summary>
    ///   Event called when animation is starting
    /// </summary>
    property OnAnimationStart: TNotifyEvent read FOnAnimationStart write FOnAnimationStart;
    /// <summary>
    ///   Event called before real rendering occurs
    /// </summary>
    property OnAnimationBeforeDraw: TSkAnimationDrawEvent read FOnAnimationBeforeDraw write FOnAnimationBeforeDraw;
    /// <summary>
    ///   Event called after real rendering occurs
    /// </summary>
    property OnAnimationAfterDraw: TSkAnimationDrawEvent read FOnAnimationAfterDraw write FOnAnimationAfterDraw;
    /// <summary>
    ///   Event called after the animator even has been processed
    /// </summary>
    property OnAnimationProcess: TNotifyEvent read FOnAnimationProcess write FOnAnimationProcess;
    /// <summary>
    ///   Event called when the animation is finished
    /// </summary>
    property OnAnimationFinish: TNotifyEvent read FOnAnimationFinish write FOnAnimationFinish;
  end;

  TSkAnimatedBitmap = class(TSkCustomAnimatedBitmap)
  public
  Type
    TAnimation = class(TCustomAnimation)
    published
      property AutoReverse;
      property Delay;
      property Duration;
      property Enabled;
      property Inverse;
      property Loop;
      property Progress;
      property Speed;
      property StartFromCurrent;
      property StartProgress;
      property StopProgress;
    end;

  private
    procedure SetAnimation(const Value: TAnimation);
    function GetAnimation: TAnimation;
  strict protected
    function CreateAnimation: TSkCustomAnimatedBitmap.TCustomAnimation; override;


  published
    property Animation: TAnimation read GetAnimation write SetAnimation;
    property Source;
    property WrapMode;
    property Opacity;
    property Height;
    property Width;
    property AutoDraw;
    property Tag;
    property BackgroundColor;
    property OnAnimationStart;
    property OnAnimationBeforeDraw;
    property OnAnimationAfterDraw;
    property OnAnimationProcess;
    property OnAnimationFinish;
  end;

implementation
uses

  System.IOUtils,
  System.DateUtils,
  System.MAth.Vectors,
  Vcl.GraphUtil,
  Winapi.CommCtrl;

{$POINTERMATH ON}

function IsSameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := (ALeft = ARight) or
    ((Length(ALeft) = Length(ARight)) and
    ((Length(ALeft) = 0) or CompareMem(PByte(@ALeft[0]), PByte(@ARight[0]), Length(ALeft))));
end;

function PlaceIntoTopLeft(const ASourceRect, ADesignatedArea: TRectF): TRectF;
begin
  Result := ASourceRect;
  if (ASourceRect.Width > ADesignatedArea.Width) or (ASourceRect.Height > ADesignatedArea.Height) then
    Result := Result.FitInto(ADesignatedArea);
  Result.SetLocation(ADesignatedArea.TopLeft);
end;

{ TSkCustomAnimatedBitmap.TAnimation }

procedure TSkCustomAnimatedBitmap.TCustomAnimation.DoChanged;
begin
  inherited;
  if Created then
    TSkCustomAnimatedBitmap(Owner).DoAnimationChanged;
end;

procedure TSkCustomAnimatedBitmap.TCustomAnimation.DoFinish;
begin
  TSkCustomAnimatedBitmap(Owner).DoAnimationFinish;
end;

procedure TSkCustomAnimatedBitmap.TCustomAnimation.DoProcess;
begin
  if FInsideDoProcess then
    Exit;
  FInsideDoProcess := True;
  try
    TSkCustomAnimatedBitmap(Owner).DoAnimationProcess;
  finally
    FInsideDoProcess := False;
  end;
end;

procedure TSkCustomAnimatedBitmap.TCustomAnimation.DoStart;
begin
  TSkCustomAnimatedBitmap(Owner).DoAnimationStart;
end;

{ TSkCustomAnimatedBitmap.TSource }

procedure TSkCustomAnimatedBitmap.TSource.Assign(ASource: TPersistent);
begin
  if ASource is TSource then
    Data := TSource(ASource).Data
  else if ASource is TSkAnimatedImage.TSource then
    Data := TSkAnimatedImage.TSource(ASource).Data
  else if ASource = nil then
    Data := nil
  else
    inherited;
end;

constructor TSkCustomAnimatedBitmap.TSource.Create(const AOnChange: TChangeProc);
begin
  inherited Create;
  FOnChange := AOnChange;
end;

function TSkCustomAnimatedBitmap.TSource.Equals(AObject: TObject): Boolean;
begin
  Result := (AObject is TSource) and IsSameBytes(FData, TSource(AObject).Data);
end;

procedure TSkCustomAnimatedBitmap.TSource.SetData(const AValue: TBytes);
begin
  if not IsSameBytes(FData, AValue) then
  begin
    FData := Copy(AValue);
    if Assigned(FOnChange) then
      FOnChange();
  end;
end;

{ TSkCustomAnimatedBitmap }

function TSkCustomAnimatedBitmap.CanRunAnimation: Boolean;
begin
  Result := ([csDestroying, csLoading] * ComponentState = []) and
    (Width > 0) and (Height > 0);
end;

procedure TSkCustomAnimatedBitmap.CheckAnimation;
begin
  if Assigned(FAnimation) then
    FAnimation.AllowAnimation := CanRunAnimation;
end;

procedure TSkCustomAnimatedBitmap.CheckDuration;
begin
  if Assigned(FAnimation) then
  begin
    if SameValue(FAnimation.Duration, 0, TCustomAnimation.TimeEpsilon) then
      DrawCacheKind := TSkDrawCacheKind.Raster
    else
      DrawCacheKind := TSkDrawCacheKind.Never;
  end;
end;

constructor TSkCustomAnimatedBitmap.Create(AOwner: TComponent);
begin
  inherited;
  FSource := TSource.Create(SourceChange);
  FBitmap := TBitmap.Create(48,48);
  FBitmap.PixelFormat := pf32bit;
  FBitmap.AlphaFormat := afPremultiplied;
  FAnimation := CreateAnimation;
  FOpacity := 255;
  FWrapMode := TSkAnimatedImageWrapMode.Fit;
  FBackgroundColor := TAlphaColorRec.Null;
  CheckDuration;
end;

function TSkCustomAnimatedBitmap.CreateAnimation: TCustomAnimation;
begin
  Result := TCustomAnimation.Create(Self);
end;

procedure TSkCustomAnimatedBitmap.DefineProperties(AFiler: TFiler);
  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TSkAnimatedImage)) or not TSkAnimatedImage(AFiler.Ancestor).Source.Equals(FSource)
    else
      Result := FSource.Data <> nil;
  end;

begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

destructor TSkCustomAnimatedBitmap.Destroy;
begin
  FCodec.Free;
  FSource.Free;
  FBitmap.Free;
  inherited;
end;

procedure TSkCustomAnimatedBitmap.DoAnimationChanged;
begin
  CheckDuration;
  CheckAnimation;
end;

procedure TSkCustomAnimatedBitmap.DoAnimationFinish;
begin
  if Assigned(FOnAnimationFinish) then
    FOnAnimationFinish(Self);
end;

procedure TSkCustomAnimatedBitmap.DoAnimationProcess;
begin
  CheckAnimation;
  if (width > 0) and (height > 0)  then
  begin
    if Render <> nil then
      Render.Redraw;
    DoPaint;
  end;
  if Assigned(FOnAnimationProcess) then
    FOnAnimationProcess(Self);
end;

procedure TSkCustomAnimatedBitmap.DoAnimationStart;
begin
  inherited;
  if Assigned(FOnAnimationStart) then
    FOnAnimationStart(Self);
end;

procedure TSkCustomAnimatedBitmap.DoPaint;
begin
  FneedRendering := true;
  if FAutoDraw then
    RealPaint(nil);
end;

function TSkCustomAnimatedBitmap.DoRender(const Awidth, Aheight: integer;
  ABackground: TBackgroundBitmapProc): TBitmap;
var
  LBackgroundBitmap: TBitmap;
begin
  SetRenderSize(TSize.Create(Awidth,Aheight));
  if FNeedRendering or assigned(ABackground) then
  begin
    LBackgroundBitmap := FBitmap;
    if assigned(ABackground) then
      ABackground(LBackgroundBitmap);
    RealPaint(LBackgroundBitmap);
  end;
  result := FBitmap;
end;

procedure TSkCustomAnimatedBitmap.RealPaint(Const ABackground: TBitmap);
begin
  if not FRender.TryRender(ABackground, FOpacity)
     and (FBackendRender = TSkControlRenderBackend.HardwareAcceleration) then
  begin
    FRender := MakeRender(TSkControlRenderBackend.Raster);
    RealPaint(ABackground);
  end;
  FNeedRendering := False;
end;

procedure TSkCustomAnimatedBitmap.Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if not FAnimation.AllowAnimation then
    CheckAnimation;
  FAnimation.BeforePaint;
  RenderFrame(ACanvas, ADest, FAnimation.Progress, AOpacity);
end;

function TSkCustomAnimatedBitmap.MakeBackground: TBitmap;
var
  X, Y: Integer;
  LRGBQuad: PRGBQuad;
begin
  result := TBitmap.Create;
  result.PixelFormat := pf32bit;
  result.AlphaFormat := afIgnored;
  for Y := 0 to result.Height - 1 do
  begin
    LRGBQuad := result.ScanLine[Y];
    for X := 0 to result.Width - 1 do
    begin
      LRGBQuad^ := TRGBQuad(FBackgroundColor);
      Inc(LRGBQuad);
    end;
  end;
  Result.AlphaFormat := afPremultiplied;
end;

function TSkCustomAnimatedBitmap.GetRenderedBitmap: TBitmap;
begin
  if FNeedRendering then
    RealPaint(nil);
  result := FBitmap;
end;

function TSkCustomAnimatedBitmap.GetRenderSize: TSize;
begin
  result.cx := FBitmap.Width;
  result.cy := FBitmap.Height;
end;

function TSkCustomAnimatedBitmap.GetCanvas: TCanvas;
begin
  result := FBitmap.Canvas
end;

function TSkCustomAnimatedBitmap.GetDeviceContext(var WindowHandle: HWND): HDC;
begin
  WindowHandle := HWND_TOP;
  result := CreateCompatibleDC(FBitmap.Canvas.Handle);
end;

function TSkCustomAnimatedBitmap.GetDrawCacheKind: TSkDrawCacheKind;
begin
  result := FDrawCacheKind;
end;

function TSkCustomAnimatedBitmap.GetHeight: Integer;
begin
  result := FBitmap.Height
end;

function TSkCustomAnimatedBitmap.GetRender: ISkControlRender;
begin
  if FRender = nil then
    FRender := MakeRender(FBackendRender);
  Result := FRender;
end;

function TSkCustomAnimatedBitmap.GetScaleFactor: Single;
begin
  result := 1;
end;

function TSkCustomAnimatedBitmap.GetWidth: Integer;
begin
  result := FBitmap.Width
end;

procedure TSkCustomAnimatedBitmap.Loaded;
begin
  inherited;
  CheckAnimation;
end;

procedure TSkCustomAnimatedBitmap.LoadFromFile(const AFileName: string);
begin
  FSource.Data := TFile.ReadAllBytes(AFileName);
end;

procedure TSkCustomAnimatedBitmap.LoadFromStream(const AStream: TStream);
var
  LBytes: TBytes;
begin
  SetLength(LBytes, AStream.Size - AStream.Position);
  if Length(LBytes) > 0 then
    AStream.ReadBuffer(LBytes, 0, Length(LBytes));
  FSource.Data := LBytes;
end;

function TSkCustomAnimatedBitmap.MakeRender(ABackendRender: TSkControlRenderBackend): ISkControlRender;
begin
  Result := TSkControlRender.MakeRender(Self, ABackendRender);
end;

procedure TSkCustomAnimatedBitmap.ReadData(AStream: TStream);
begin
  if AStream.Size = 0 then
    FSource.Data := nil
  else
    LoadFromStream(AStream);
end;

procedure TSkCustomAnimatedBitmap.ReadState(AReader: TReader);
begin
  FAnimation.BeginUpdate;
  try
    var lprogress := FAnimation.Progress;
    inherited;
    FAnimation.Progress := lprogress;
  finally
    FAnimation.EndUpdate;
  end;
end;

procedure TSkCustomAnimatedBitmap.RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);

  function GetWrappedRect(const ADest: TRectF): TRectF;
  var
    LImageRect: TRectF;
    LRatio: Single;
  begin
    LImageRect := TRectF.Create(PointF(0, 0), FCodec.Size);
    case FWrapMode of
      TSkAnimatedImageWrapMode.Fit: Result := LImageRect.FitInto(ADest);
      TSkAnimatedImageWrapMode.FitCrop:
        begin
          if (LImageRect.Width / ADest.Width) < (LImageRect.Height / ADest.Height) then
            LRatio := LImageRect.Width / ADest.Width
          else
            LRatio := LImageRect.Height / ADest.Height;
          if SameValue(LRatio, 0, TEpsilon.Vector) then
            Result := ADest
          else
          begin
            Result := RectF(0, 0, Round(LImageRect.Width / LRatio), Round(LImageRect.Height / LRatio));
            RectCenter(Result, ADest);
          end;
        end;
      TSkAnimatedImageWrapMode.Original:
        begin
          Result := LImageRect;
          Result.Offset(ADest.TopLeft);
        end;
      TSkAnimatedImageWrapMode.OriginalCenter:
        begin
          Result := LImageRect;
          RectCenter(Result, ADest);
        end;
      TSkAnimatedImageWrapMode.Place: Result := PlaceIntoTopLeft(LImageRect, ADest);
      TSkAnimatedImageWrapMode.Stretch: Result := ADest;
    end;
  end;

begin
  if Assigned(FOnAnimationBeforeDraw) then
  begin
    FOnAnimationBeforeDraw(Self, ACanvas, ADest, AProgress, AOpacity);
  end;
  if Assigned(FCodec) then
  begin
    if (csDesigning in ComponentState) and (not FAnimation.Running) and (AProgress = 0) then
      FCodec.SeekFrameTime(FAnimation.Duration / 8)
    else
      FCodec.SeekFrameTime(FAnimation.CurrentTime);
    ACanvas.Save;
    try
      ACanvas.ClipRect(ADest);
      FCodec.Render(ACanvas, GetWrappedRect(ADest), AOpacity);
    finally
      ACanvas.Restore;
    end;
  end;
  if Assigned(FOnAnimationAfterDraw) then
    FOnAnimationAfterDraw(Self, ACanvas, ADest, AProgress, AOpacity);
end;

procedure TSkCustomAnimatedBitmap.SetBackendRender(const AValue: TSkControlRenderBackend);
begin
  if FBackendRender <> AValue then
  begin
    FBackendRender := AValue;
    FRender := nil;
  end;
end;

procedure TSkCustomAnimatedBitmap.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TSkCustomAnimatedBitmap.SetRenderSize(const Value: TSize);
begin
  if (FBitmap.Width <> Value.cx) or (FBitmap.Height <> Value.cy) then
  begin
    FRender := nil;
    FBitmap.SetSize(Value.cx,Value.cy);
    Frender := GetRender;
    DoPaint;
  end;
end;

procedure TSkCustomAnimatedBitmap.SetDrawCacheKind(const AValue: TSkDrawCacheKind);
begin
  FDrawCacheKind := AValue;
end;

procedure TSkCustomAnimatedBitmap.SetHeight(const Value: Integer);
begin
  if Value <> FBitmap.Height then
  begin
    FBitmap.Height := Value;
    FRender := nil;
    Frender := GetRender;
    DoPaint;
  end;
end;

procedure TSkCustomAnimatedBitmap.SetOpacity(const Value: byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    DoPaint;
  end;
end;

procedure TSkCustomAnimatedBitmap.SetSource(const AValue: TSource);
begin
  FSource.Assign(AValue);
end;

procedure TSkCustomAnimatedBitmap.SetWidth(const Value: Integer);
begin
  if Value <> FBitmap.Width then
  begin
    FBitmap.Width := Value;
    FRender := nil;
    Frender := GetRender;
    DoPaint;
  end;
end;

procedure TSkCustomAnimatedBitmap.SetWrapMode(const AValue: TSkAnimatedImageWrapMode);
begin
  if FWrapMode <> AValue then
  begin
    FWrapMode := AValue;
    DoPaint;
  end;
end;

procedure TSkCustomAnimatedBitmap.SourceChange;
var
  LCodecClass: TAnimationCodecClass;
  LStream: TStream;
begin
  FreeAndNil(FCodec);
  LStream := TBytesStream.Create(FSource.Data);
  try
    for LCodecClass in RegisteredCodecs do
    begin
      LStream.Position := 0;
      if LCodecClass.TryMakeFromStream(LStream, FCodec) then
        Break;
    end;
  finally
    LStream.Free;
  end;
  if Assigned(FCodec) then
  begin
    FAnimation.SetDuration(FCodec.Duration);
    if FAnimation.Running then
      FAnimation.Start;
  end
  else
    FAnimation.SetDuration(0);
end;

procedure TSkCustomAnimatedBitmap.WriteData(AStream: TStream);
begin
  if FSource.Data <> nil then
    AStream.WriteBuffer(FSource.Data, Length(FSource.Data));
end;

{ TSkAnimatedBitmap }

function TSkAnimatedBitmap.CreateAnimation: TSkCustomAnimatedBitmap.TCustomAnimation;
begin
  result := TAnimation.Create(self);
end;

function TSkAnimatedBitmap.GetAnimation: TAnimation;
begin
  result := TSkAnimatedBitmap.TAnimation(Fanimation);
end;

procedure TSkAnimatedBitmap.SetAnimation(const Value: TAnimation);
begin
  FAnimation.Assign(Value);
end;

end.
