unit Vcl.Skia.AnimatedImageList;

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
  Vcl.skia;


type
    { TFormatInfo }

    TFormatInfo = record
      Description: string;
      Extensions: TArray<string>;
      Name: string;
      constructor Create(const AName, ADescription: string; const AExtensions: TArray<string>);
    end;

    { TAnimationCodec }

    TAnimationCodec = class
    strict protected
      function GetDuration: Double; virtual; abstract;
      function GetFPS: Double; virtual; abstract;
      function GetIsStatic: Boolean; virtual; abstract;
      function GetSize: TSizeF; virtual; abstract;
    public
      procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); virtual; abstract;
      procedure SeekFrameTime(const ATime: Double); virtual; abstract;
      class function SupportedFormats: TArray<TFormatInfo>; virtual; abstract;
      class function TryDetectFormat(const ABytes: TBytes; out AFormat: TFormatInfo): Boolean; virtual; abstract;
      class function TryMakeFromStream(const AStream: TStream; out ACodec: TAnimationCodec): Boolean; virtual; abstract;
      property Duration: Double read GetDuration;
      property FPS: Double read GetFPS;
      property IsStatic: Boolean read GetIsStatic;
      property Size: TSizeF read GetSize;
    end;

    TAnimationCodecClass = class of TAnimationCodec;

  TSkSourceType = (
    anim,        // Animation (lottie)
    svg,         // SVG (vector image)
    image        // Bitmap (Png, JPG, ...)
  );

  TSkAnimatedImageList = class;
  { A collection item capable to hold animation, vector or bitmap }
  TSkAnimatedItem = class(TCollectionItem)
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
      FSource: TSource;
      FName: string;
      FDescription: String;
      Flast_index: integer;
      FGrayIfDisabled: Boolean;
      FSourceType: TSkSourceType;
      FWrapMode: TSkAnimatedImageWrapMode;
      FPauseIfDisabled: Boolean;

      FCodec: TAnimationCodec;
      FSvgBrush: TSkSvgBrush;
      FImage: ISkImage;
      FOnChange: TNotifyEvent;

      function Getduration: double;
      function GetImageList: TSkAnimatedImageList;
      function GetHasCodec: boolean;
      function Getis_animated: boolean;

      procedure SetName(const Value: string);
      procedure SetGrayIfDisabled(AValue: Boolean);
      procedure SetDescription(const AValue: String);
      procedure SetWrapMode(const Value: TSkAnimatedImageWrapMode);
      procedure SetSourceType(const Value: TSkSourceType);
      procedure SetPauseIfDisabled(const Value: Boolean);
    procedure SetSource(const Value: TSource);

    strict protected
      procedure ReadData(AStream: TStream);
      procedure WriteData(AStream: TStream);
      procedure DefineProperties(Filer: TFiler); override;

      procedure SourceChange; virtual;
      procedure DoChanged;

      procedure RenderAnim(const ACanvas: ISkCanvas;
         const ADest, aWrap: TRectF;
         const AProgress: Double;
         const AOpacity: Single;
         const AStyle: Cardinal;
         const AStateDisabled: boolean); virtual;

      procedure RenderImage(const ACanvas: ISkCanvas;
         const ADest, aWrap: TRectF;
         const AProgress: Double;
         const AOpacity: Single;
         const AStyle: Cardinal;
         const AStateDisabled: boolean);  virtual;

      procedure RenderSvg(const ACanvas: ISkCanvas;
         const ADest, AWrap: TRectF;
         const AProgress: Double;
         const AOpacity: Single;
         const AStyle: Cardinal;
         const AStateDisabled: boolean);  virtual;



    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
      procedure LoadFromStream(const AStream: TStream);

      procedure Assign(ASource: TPersistent); override;
      function Equals(AObject: TObject): Boolean; override;
      function need_refresh(and_reset: boolean=true): boolean;

      procedure Render(const ACanvas: ISkCanvas;
         const ADest: TRectF;
         const AProgress: Double;
         const AOpacity: Single;
         const AStyle: Cardinal;
         const AStateDisabled: boolean);

      property ImageList: TSkAnimatedImageList read GetImageList;
      property is_animated: boolean read Getis_animated;
      property has_codec: boolean read GetHasCodec;



    published
      /// <summary>
      ///   Duration in seconds of the animation (information).
      ///  0 if not an animated item
      /// </summary>
      property Duration: double read Getduration;

      /// <summary>
      ///   Binary Data of the item
      /// </summary>
      property Source: TSource read FSource write SetSource;

      /// <summary>
      ///   Type of data this item contains
      /// </summary>
      property SourceType: TSkSourceType read FSourceType write SetSourceType default TSkSourceType.anim;

      /// <summary>
      /// If TRUE, the item is rendered in grayscale when disabled
      /// </summary>
      property GrayIfDisabled: Boolean
        read FGrayIfDisabled write SetGrayIfDisabled default True;

      /// <summary>
      ///   If TRUE, the animation item is paused when disabled
      /// </summary>
      property PauseIfDisabled: Boolean read FPauseIfDisabled write SetPauseIfDisabled default True;

      /// <summary>
      /// Item name.
      /// </summary>
      property Name: String read FName write SetName;

      /// <summary>
      /// Item description.
      /// </summary>
      property Description: String read FDescription write SetDescription;

      /// <summary>
      ///   How the item must be resized
      /// </summary>
      property WrapMode: TSkAnimatedImageWrapMode
        read FWrapMode write SetWrapMode default TSkAnimatedImageWrapMode.Fit;

      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSkAnimatedItemClass = class of TSkAnimatedItem;

  TskAnimatedCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TSkAnimatedItem;
    procedure SetItem(Index: Integer; Value: TSkAnimatedItem);
  protected
      function GetImageList: TSkAnimatedImageList;
    property ImageList: TSkAnimatedImageList read GetImageList;
  public
    function Add: TSkAnimatedItem;
    function Insert(Index: Integer): TSkAnimatedItem;
    function Merge(AskAnimatedCollection: TskAnimatedCollection): Integer;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TSkAnimatedItem read GetItem write SetItem; default;
  end;

  TSkAnimatedImageList = class(TCustomImageList,ISkControlRenderTarget,ISkControlRenderTarget2)
  private
    FImages: TskAnimatedCollection;
    FImageNameAvailable : boolean;
    FOpacity: byte;
    FDisabledOpacity :byte;
    FRenderBackend: TSkControlRenderBackend;

    Ftimer: TTimer;
    FRender: ISkControlRender;
    FrenderIndex: integer;
    FrenderX,
    FRenderY: integer;
    FRenderStyle: Cardinal;
    FRenderEnabledState: boolean;
    FCanvas: TCanvas;
    FDrawCacheKind: TSkDrawCacheKind;
    FStartTime: double;
    FImageListUpdating: boolean;
    FImageListCount: integer;
    FFrameRate: Integer;
    FRunning: boolean;
    Fhas_changed: boolean;

    procedure SetImages(const Value: TskAnimatedCollection);
    procedure SetDisabledOpacity(const Value: byte);
    function GetRender: ISkControlRender;
    procedure SetOpacity(const Value: byte);
    procedure SetRenderBackend(const Value: TSkControlRenderBackend);
    procedure SetFrameRate(const Value: Integer);
    procedure SetRunning(const Value: boolean);

  protected
    { ISkControlRenderTarget2  }
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function GetCanvas: TCanvas;
    function GetDeviceContext(var WindowHandle: HWND): HDC;
    function GetDrawCacheKind: TSkDrawCacheKind;
    function GetScaleFactor: Single;
    function RenderTargetGetHeight: Integer;
    function RenderTargetGetWidth: Integer;
    function ISkControlRenderTarget.GetWidth = RenderTargetGetWidth;
    function ISkControlRenderTarget.GetHeight = RenderTargetGetHeight;
    function ISkControlRenderTarget2.GetWidth = RenderTargetGetWidth;
    function ISkControlRenderTarget2.GetHeight = RenderTargetGetHeight;
    function RenderTargetGetLeft: Integer;
    function RenderTargetGetTop: Integer;
    function ISkControlRenderTarget2.GetLeft = RenderTargetGetLeft;
    function ISkControlRenderTarget2.GetTop = RenderTargetGetTop;

    property Canvas: TCanvas read GetCanvas;
    property DrawCacheKind: TSkDrawCacheKind read GetDrawCacheKind;
    property ScaleFactor: Single read GetScaleFactor;

    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      AStyle: Cardinal; AEnabled: Boolean = True); override;
    function GetCount: Integer; override;
    procedure GetImages(Index: Integer; Image, Mask: TBitmap);

    procedure Initialize; override;
    procedure DoTimer(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Change; override;
    procedure DoChange; override;
    procedure Loaded; override;

    procedure UpdateImageList(ReCreate: boolean);
    procedure UpdateAnims;
    function has_animation: boolean;

    procedure Assign(Source: TPersistent); override;


    function Overlay(ImageIndex: Integer; Overlay: TOverlay): Boolean; override;

    function IsImageNameAvailable: Boolean; override;
    function IsScaled: Boolean; override;
    function GetIndexByName(const AName: TImageName): System.UITypes.TImageIndex; override;
    function GetNameByIndex(AIndex: System.UITypes.TImageIndex): TImageName; override;

  published
    property Images: TskAnimatedCollection read FImages write SetImages;
    property DisabledOpacity: byte read FDisabledOpacity write SetDisabledOpacity default 125;
    property Opacity: byte read FOpacity write SetOpacity default 255;
    property RenderBackend: TSkControlRenderBackend read FRenderBackend write SetRenderBackend default TSkControlRenderBackend.default;
    property Running: boolean read FRunning write SetRunning default true;
    property FrameRate: Integer read FFrameRate write SetFrameRate default 60;

    property width;
    property height;
  end;

function RegisteredCodecs: TArray<TAnimationCodecClass>;


implementation
uses

  System.IOUtils,
  System.DateUtils,
  System.MAth.Vectors,
  Vcl.GraphUtil,
  Winapi.CommCtrl;

var
  GRegisteredCodecs: TArray<TAnimationCodecClass>;

function RegisteredCodecs: TArray<TAnimationCodecClass>;
begin
  result := GRegisteredCodecs;
end;

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

{ TSkAnimatedItem }

procedure TSkAnimatedItem.Assign(ASource: TPersistent);
var
  lsource: TSkAnimatedItem absolute ASource;
begin
  if ASource is TSkAnimatedItem then
  begin
    FOnChange := lsource.FOnChange;
    Fname := lsource.Fname;
    FDescription := lsource.Description;
    FGrayIfDisabled := lsource.FGrayIfDisabled;
    FSourceType := Lsource.SourceType;
    FWrapMode := lsource.FWrapMode;
    FPauseIfDisabled := lsource.FPauseIfDisabled;
    Fsource.Assign(Lsource.FSource);
    Flast_index := -1;
  end;
end;

constructor TSkAnimatedItem.Create(ACollection: TCollection);
begin
  inherited;
  FSource := TSource.Create(SourceChange);
  Fname := 'AnimatedItem' + index.ToString;
  FGrayIfDisabled := True;
  FPauseIfDisabled := True;
  Flast_index := -1;
end;

procedure TSkAnimatedItem.DefineProperties(Filer: TFiler);
  function DoWrite: boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := (not (Filer.Ancestor is TSkAnimatedItem)) or not TSkAnimatedItem(Filer.Ancestor).Equals(self)
    else
      Result := Fsource.Data <> nil;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

destructor TSkAnimatedItem.Destroy;
begin
  FSource.Free;
  FSvgBrush.Free;
  FCodec.Free;
  inherited;
end;

procedure TSkAnimatedItem.DoChanged;
begin
  Flast_index := -1;
  if assigned(FOnChange) then
    FOnChange(Self);
  ImageList.DoChange;
end;

function TSkAnimatedItem.Equals(AObject: TObject): Boolean;
var
  lsource: TSkAnimatedItem absolute AObject;
begin
  result := (AObject is TSkAnimatedItem) and
            (Fname = lsource.Fname) and
            (FDescription = lsource.Description) and
            (FGrayIfDisabled = lsource.FGrayIfDisabled) and
            (FSourceType = lsource.SourceType) and
            (FWrapMode = lsource.FWrapMode) and
            (FPauseIfDisabled = lsource.FPauseIfDisabled) and
            Fsource.Equals(lsource.Fsource);
end;

function TSkAnimatedItem.Getduration: double;
begin
  if assigned(Fcodec) then
    result := Fcodec.Duration
  else
    result := 0;
end;

function TSkAnimatedItem.GetHasCodec: boolean;
begin
  result := assigned(FCodec);
end;

function TSkAnimatedItem.GetImageList: TSkAnimatedImageList;
begin
  result := TSkAnimatedImageList(Collection.owner);
end;

function TSkAnimatedItem.Getis_animated: boolean;
begin
  result := assigned(Fcodec) and (duration > 0.001);
end;

procedure TSkAnimatedItem.LoadFromStream(const AStream: TStream);
var
  LBytes: TBytes;
begin
  SetLength(LBytes, AStream.Size - AStream.Position);
  if Length(LBytes) > 0 then
    AStream.ReadBuffer(LBytes, 0, Length(LBytes));
  FSource.Data := LBytes;
end;

function TSkAnimatedItem.need_refresh(and_reset: boolean): boolean;
begin
  result := is_animated or (Flast_index <> index);
  if result and and_reset then
    Flast_index := index;
end;

procedure TSkAnimatedItem.ReadData(AStream: TStream);
begin
  if AStream.Size = 0 then
    FSource.Data := nil
  else
    LoadFromStream(AStream);
end;

procedure TSkAnimatedItem.Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single; const AStyle: Cardinal;
  const AStateDisabled: boolean);
  function GetWrappedRect(const ADest: TRectF; aSize: TSizeF): TRectF;
  var
    LImageRect: TRectF;
    LRatio: Single;
  begin
    LImageRect := TRectF.Create(PointF(0, 0), aSize);
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
  case FSourceType of
    anim: RenderAnim(ACanvas,ADest,GetWrappedRect(aDest,FCodec.Size),AProgress,AOpacity,AStyle,AStateDisabled);
    svg: RenderSvg(ACanvas,aDest,GetWrappedRect(aDest,FSvgBrush.OriginalSize),AProgress,AOpacity,AStyle,AStateDisabled);
    image: RenderImage(ACanvas,aDest,GetWrappedRect(aDest,TSizeF.Create(FImage.ImageInfo.Width,FImage.ImageInfo.Height)),AProgress,AOpacity,AStyle,AStateDisabled);
  end;

end;

procedure TSkAnimatedItem.RenderAnim(const ACanvas: ISkCanvas;
  const ADest, aWrap: TRectF; const AProgress: Double; const AOpacity: Single;
  const AStyle: Cardinal; const AStateDisabled: boolean);


  var lp: double;

begin
  if Duration = 0 then
  begin
    lp := 0;
  end else begin
    lp := fmod(aProgress, duration);
  end;
  if Assigned(FCodec) then
  begin
    if (aStateDisabled and FPauseIfdisabled) then
    begin
      FCodec.SeekFrameTime(Duration / 2);
    end else begin
      FCodec.SeekFrameTime(lp);
    end;
    ACanvas.Save;
    try
      ACanvas.ClipRect(ADest);
      if AStateDisabled and FGrayIfDisabled then
      begin
        { turn to grayscale for disabled }
        var LSurface : ISkSurface := TSkSurface.MakeRaster(TSkImageInfo.Create(round(aDest.Width), round(aDest.Height)));
        if LSurface <> nil then
        begin
          FCodec.Render(LSurface.Canvas, TrectF.create(0,0,aDest.Width,aDest.Height), AOpacity);
          var LGrayMatrix := TSkColorMatrix.Create(
            0.299,  0.587,  0.114,    0, 0,  // red
            0.299,  0.587,  0.114,    0, 0,  // green
            0.299,  0.587,  0.114,    0, 0,  // blue
                0,      0,      0,    1, 0   // Alpha
          );
//          var LGrayMatrix := TSkColorMatrix.Create(
//            0.21, 0.72, 0.07,    0, 0,  // red
//            0.21, 0.72, 0.07,    0, 0,  // green
//            0.21, 0.72, 0.07,    0, 0,  // blue
//            0,      0,      0,    1, 0   // Alpha
//          );

          Var LFilter := TSkColorFilter.MakeMatrix(LGrayMatrix);
          var LPaint: ISkPaint := TSkPaint.Create;
          Lpaint.SetColorfilter(LFilter);
          LSurface.Draw(aCanvas,aDest.Left,aDest.Top,Lpaint);
        end else begin
          FCodec.Render(ACanvas, aWrap, AOpacity);
        end;
      end else begin
        FCodec.Render(ACanvas, aWrap, AOpacity);
      end;
    finally
      ACanvas.Restore;
    end;
  end;
end;

procedure TSkAnimatedItem.RenderImage(const ACanvas: ISkCanvas; const ADest, aWrap: TRectF; const AProgress: Double; const AOpacity: Single;
  const AStyle: Cardinal; const AStateDisabled: boolean);
var
  LMatrix : TSkColorMatrix;
begin
  if AStateDisabled and FGrayIfDisabled then
  begin
    LMatrix := TSkColorMatrix.Create(
      0.299,  0.587,  0.114,    0, 0,  // red
      0.299,  0.587,  0.114,    0, 0,  // green
      0.299,  0.587,  0.114,    0, 0,  // blue
          0,      0,      0,    AOpacity, 0   // Alpha
    );
//          var LGrayMatrix := TSkColorMatrix.Create(
//            0.21, 0.72, 0.07,    0, 0,  // red
//            0.21, 0.72, 0.07,    0, 0,  // green
//            0.21, 0.72, 0.07,    0, 0,  // blue
//            0,      0,      0,    1, 0   // Alpha
//          );
  end else begin
    LMatrix := TSkColorMatrix.Create(
      1,  0,  0,    0,        0,  // red
      0,  1,  0,    0,        0,  // green
      0,  0,  1,    0,        0,  // blue
      0,  0,  0,    AOpacity, 0   // Alpha
    );
  end;
  Var LFilter := TSkColorFilter.MakeMatrix(LMatrix);
  var LPaint: ISkPaint := TSkPaint.Create;
  aCanvas.DrawImageRect(FImage,AWrap,LPaint);
end;

procedure TSkAnimatedItem.RenderSvg(const ACanvas: ISkCanvas; const ADest, AWrap: TRectF; const AProgress: Double; const AOpacity: Single;
  const AStyle: Cardinal; const AStateDisabled: boolean);
begin
  FSvgBrush.GrayScale := AStateDisabled;
  FSvgBrush.Render(ACanvas,AWrap,AOpacity);
end;

procedure TSkAnimatedItem.SetSourceType(const Value: TSkSourceType);
begin
  if FSourceType <> Value then
  begin
    FSourceType := Value;
    if not (csLoading in ImageList.ComponentState)  then
    begin
      Fsource.Data := [];
    end;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.SetDescription(const AValue: String);
begin
  if FDescription <> AValue then
  begin
    FDescription := AValue;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.SetGrayIfDisabled(AValue: Boolean);
begin
  if FGrayIfDisabled <> AValue then
  begin
    FGrayIfDisabled := AValue;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.Setname(const Value: string);
begin
  if FName <> Value then
  begin
    Fname := Value;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.SetPauseIfDisabled(const Value: Boolean);
begin
  if FPauseIfDisabled <> Value then
  begin
    FPauseIfDisabled := Value;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.SetSource(const Value: TSource);
begin
  FSource.Assign(Value);
end;

procedure TSkAnimatedItem.SetWrapMode(const Value: TSkAnimatedImageWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    DoChanged;
  end;
end;

procedure TSkAnimatedItem.SourceChange;
var
  LCodecClass: TAnimationCodecClass;
  LStream: TStream;
begin
  FreeAndNil(FCodec);
  FreeAndNil(FSvgBrush);
  FImage := nil;
  Flast_index := -1;
  case FSourceType of
    anim: begin
      LStream := TBytesStream.Create(FSource.Data);
      try
        for LCodecClass in GRegisteredCodecs do
        begin
          LStream.Position := 0;
          if LCodecClass.TryMakeFromStream(LStream, FCodec) then
            Break;
        end;
      finally
        LStream.Free;
      end;
    end;
    svg: begin
      FSvgBrush := TSkSvgBrush.Create;
      FSvgBrush.Source := TEncoding.UTF8.GetString(FSource.Data);
    end;
    image: begin
      FImage := TSkImage.MakeFromEncoded(FSource.Data);
    end;
  end;
  DoChanged;
end;

procedure TSkAnimatedItem.WriteData(AStream: TStream);
begin
  if FSource.Data <> nil then
    AStream.WriteBuffer(FSource.Data, Length(FSource.Data));
end;

{ TskAnimatedCollection }

function TskAnimatedCollection.Add: TSkAnimatedItem;
begin
  result := TSkAnimatedItem (inherited Add);
end;

procedure TskAnimatedCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TSkAnimatedCollection.GetImageList: TSkAnimatedImageList;
begin
  Result := TSkAnimatedImageList(Owner);
end;

function TskAnimatedCollection.GetItem(Index: Integer): TSkAnimatedItem;
begin
  Result := TSkAnimatedItem(inherited GetItem(Index));
end;

function TskAnimatedCollection.Insert(Index: Integer): TSkAnimatedItem;
begin
  Result := TSkAnimatedItem(inherited Insert(Index));
end;

function TskAnimatedCollection.Merge(
  AskAnimatedCollection: TskAnimatedCollection): Integer;
var
  I: Integer;
begin
  if AskAnimatedCollection.Count > 0 then
  begin
    Result := Count;

    for I := 0 to AskAnimatedCollection.Count - 1 do
      Add.Assign(AskAnimatedCollection.Items[I]);
  end
  else
    Result := -1;
end;

procedure TskAnimatedCollection.SetItem(Index: Integer; Value: TSkAnimatedItem);
begin
  inherited SetItem(Index, Value);
end;

{ TSkAnimatedImageList }

procedure TSkAnimatedImageList.Assign(Source: TPersistent);
var
  lsource : TSkAnimatedImageList absolute Source;
begin
  if source is TSkAnimatedImageList then
  begin
    FImages.Assign(lsource.FImages);
    FImageNameAvailable  := lsource.FImageNameAvailable;
    FDisabledOpacity     := lsource.FDisabledOpacity;
    FOpacity             := lsource.FOpacity;
    FRenderBackend       := lsource.FRenderBackend;
    FStartTime           := lsource.FStartTime;
    FFrameRate           := FFrameRate;
    Running              := Frunning;
  end else
    inherited;
end;

procedure TSkAnimatedImageList.Change;
begin
  Fhas_changed := true;
  inherited;
  Fhas_changed := false;

//  if not FImageListUpdating then
//    UpdateImageList(True);
end;

constructor TSkAnimatedImageList.Create(AOwner: TComponent);
begin
  inherited;
  FImageNameAvailable := True;
  FDisabledOpacity := 125;
  FOpacity := 255;
  FRenderBackend := TSkControlRenderBackend.default;
  StoreBitmap := False;
  FScaled := True;
  ColorDepth := cd32bit;
  Masked := False;
  FTimer := TTimer.Create(self);
  FTimer.OnTimer := DoTimer;
  FrameRate := 60;
  FRunning := true;
  FDrawCacheKind := TSkDrawCacheKind.Never;
//  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
//  FCollectionChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TImageCollectionChangedMessage, CollectionChangedMessageHandler);
  FImages := TSkAnimatedCollection.Create(Self,TSkAnimatedItem);
end;


destructor TSkAnimatedImageList.Destroy;
begin
  FTimer.Free;
  Fimages.Free;
  inherited;
end;

procedure TSkAnimatedImageList.DoChange;
begin
  if csLoading in ComponentState then exit;
  if not FImageListUpdating then
    UpdateImageList(Fhas_changed);
  inherited;
end;

procedure TSkAnimatedImageList.DoDraw(Index: Integer; Canvas: TCanvas; X,
  Y: Integer; AStyle: Cardinal; AEnabled: Boolean);
begin
  FCanvas := Canvas;
  FrenderIndex := Index;
  FrenderX := X;
  FRenderY := Y;
  FRenderStyle := AStyle;
  FRenderEnabledState := AEnabled;
  var lrender := GetRender;
  if not lrender.TryRender(nil, ifthen(AEnabled,FOpacity,FDisabledOpacity))
     and (FRenderBackEnd = TSkControlRenderBackend.HardwareAcceleration) then
  begin
    FRender := TskControlRender.MakeRender(Self,TSkControlRenderBackend.Raster);
    FRender.TryRender(nil, ifthen(AEnabled,FOpacity,FDisabledOpacity));
  end;

end;

procedure TSkAnimatedImageList.DoTimer(Sender: TObject);
begin
  if CsDesigning in ComponentState then exit;
  if has_animation then
    DoChange;
end;

procedure TSkAnimatedImageList.Draw(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  lsec: double;
begin
  if inrange(FrenderIndex,0,Count - 1)  then
  begin
    if csDesigning in ComponentState then
      lsec := FImages[FRenderIndex].Duration / 4
    else
      lsec := SecondSpan(Now,FStartTime);
    FImages[FRenderIndex].Render(ACanvas, Adest, lsec, AOpacity,FRenderStyle,Not FRenderEnabledState);
  end;
end;

function TSkAnimatedImageList.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TSkAnimatedImageList.GetCount: Integer;
begin
  result := Fimages.count;
end;

function TSkAnimatedImageList.GetDeviceContext(var WindowHandle: HWND): HDC;
begin
  result := Fcanvas.Handle;
end;

function TSkAnimatedImageList.GetDrawCacheKind: TSkDrawCacheKind;
begin
 Result := FDrawCacheKind;
end;

function TSkAnimatedImageList.RenderTargetGetHeight: Integer;
begin
  result := Height;
end;

function TSkAnimatedImageList.RenderTargetGetLeft: Integer;
begin
  result := FRenderX;
end;

function TSkAnimatedImageList.RenderTargetGetTop: Integer;
begin
  Result := FRenderY;
end;

procedure TSkAnimatedImageList.GetImages(Index: Integer; Image, Mask: TBitmap);
begin
  if assigned(Image) then
  begin
    DoDraw(Index,Image.Canvas,0,0,0,true);
  end;
end;

function TSkAnimatedImageList.GetIndexByName(
  const AName: TImageName): System.UITypes.TImageIndex;
begin
  for var i := 0 to FImages.Count - 1 do
    if FImages[i].Name = AName then exit(i);
  result := 1;
end;

function TSkAnimatedImageList.GetNameByIndex(
  AIndex: System.UITypes.TImageIndex): TImageName;
begin
  if InRange(AIndex,0,FImages.Count - 1) then
  begin
    result := FImages[AIndex].Name;
  end else begin
    result := '';
  end;
end;

function TSkAnimatedImageList.GetRender: ISkControlRender;
begin
  if FRender = nil then
    Frender := TSkControlRender.MakeRender(Self, FRenderBackEnd);
  result := FRender;
end;

function TSkAnimatedImageList.GetScaleFactor: Single;
begin
  result := 1;
end;

function TSkAnimatedImageList.has_animation: boolean;
begin
  for var i := 0 to Fimages.count - 1 do
  begin
    if Fimages[i].is_animated then exit(true);
  end;
  result := false;
end;

function TSkAnimatedImageList.RenderTargetGetWidth: Integer;
begin
  result := Width;
end;

procedure TSkAnimatedImageList.Initialize;
begin
  inherited;
  FStartTime := Now;
end;

function TSkAnimatedImageList.IsImageNameAvailable: Boolean;
begin
  result := False;
end;

function TSkAnimatedImageList.IsScaled: Boolean;
begin
  result := true;
end;

procedure TSkAnimatedImageList.Loaded;
begin
  if Frunning and not (csDesigning in ComponentState) then
    Ftimer.Enabled := true;
  inherited;
  if (csDesigning in ComponentState) then
    DoChange;
end;

function TSkAnimatedImageList.Overlay(ImageIndex: Integer;
  Overlay: TOverlay): Boolean;
begin
  result := false;
end;

procedure TSkAnimatedImageList.SetDisabledOpacity(const Value: byte);
begin
  FDisabledOpacity := Value;
end;

procedure TSkAnimatedImageList.SetFrameRate(const Value: Integer);
begin
  if FFrameRate <> Value then
  begin
    FFrameRate := EnsureRange(Value,1,120);
    Ftimer.Interval := 1000 div FFrameRate;
  end;
end;

procedure TSkAnimatedImageList.SetImages(const Value: TskAnimatedCollection);
begin
  FImages.Assign(Value);
end;


procedure TSkAnimatedImageList.SetOpacity(const Value: byte);
begin
  FOpacity := Value;
end;

procedure TSkAnimatedImageList.SetRenderBackend(
  const Value: TSkControlRenderBackend);
begin
  FRenderBackend := Value;
end;

procedure TSkAnimatedImageList.SetRunning(const Value: boolean);
begin
  FRunning := Value;
  Ftimer.Enabled := Value and not (CsDesigning in ComponentState);
end;

procedure TSkAnimatedImageList.UpdateAnims;
var
  I: Integer;
  B: TBitmap;
begin
  FImageListUpdating := True;
  try
    if not assigned(FImages) then exit;
    for I := 0 to FImages.Count - 1 do
    begin
      if not FImages[i].is_animated then
        continue;
      B := TBitmap.Create;
      try
        B.PixelFormat := pf32bit;
        B.SetSize(Width, Height);
        B.AlphaFormat := afPremultiplied;
        InitAlpha(B, 0);
        DoDraw(I,B.Canvas,0,0,0,True);
        ImageList_Replace(Handle, I,B.Handle, 0);
      finally
        B.Free;
      end;
    end;
  finally
    FImageListUpdating := False;
  end;
end;

procedure TSkAnimatedImageList.UpdateImageList(ReCreate: boolean);
var
  I: Integer;
  B: TBitmap;
begin
  FImageListUpdating := True;
  try
    if ReCreate then
    begin
      ImageList_Remove(Handle, -1);
      FImageListCount := 0;
      FRender := nil;
    end;
    if not assigned(FImages) then exit;
    for I := 0 to FImages.Count - 1 do
    begin
      if not (recreate or FImages[i].need_refresh(true)) then continue;
      B := TBitmap.Create;
      try
        B.PixelFormat := pf32bit;
        B.SetSize(Width, Height);
        B.AlphaFormat := afPremultiplied;
        InitAlpha(B, 0);
        DoDraw(I,B.Canvas,0,0,0,True);
        if (I >= FImageListCount) then
        begin
            ImageList_Add(Handle, B.Handle, 0);
            inc(FImageListCount);
        end else begin
            ImageList_Replace(Handle, I,B.Handle, 0);
        end;
      finally
        B.Free;
      end;
    end;
  finally
    FImageListUpdating := False;
  end;
end;

{ TFormatInfo }

constructor TFormatInfo.Create(const AName, ADescription: string;
  const AExtensions: TArray<string>);
begin
  Name := AName;
  Description := ADescription;
  Extensions := AExtensions;
end;

{ TSkAnimatedItem.TSource }

procedure TSkAnimatedItem.TSource.Assign(ASource: TPersistent);
begin
  if ASource is TSource then
    Data := TSource(ASource).Data
  else if ASource is TSkAnimatedItem.TSource then
    Data := TSkAnimatedItem.TSource(ASource).Data
  else if ASource = nil then
    Data := nil
  else
    inherited;
end;

constructor TSkAnimatedItem.TSource.Create(const AOnChange: TChangeProc);
begin
  inherited Create;
  FOnChange := AOnChange;
end;

function TSkAnimatedItem.TSource.Equals(AObject: TObject): Boolean;
begin
  Result := (AObject is TSource) and IsSameBytes(FData, TSource(AObject).Data);
end;

procedure TSkAnimatedItem.TSource.SetData(const AValue: TBytes);
begin
  if not IsSameBytes(FData, AValue) then
  begin
    FData := Copy(AValue);
    if Assigned(FOnChange) then
      FOnChange();
  end;
end;

initialization
  for var c in TSkAnimatedImage.RegisteredCodecs do
    GRegisteredCodecs := GRegisteredCodecs + [TAnimationCodecClass(C)];
end.
