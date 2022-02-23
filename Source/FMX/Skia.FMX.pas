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
unit Skia.FMX;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Ani,
  FMX.ActnList,

  { Skia }
  Skia,
  Skia.FMX.Graphics;

const
  {$IF CompilerVersion < 33}
  SkSupportedPlatformsMask = pidWin32 or pidWin64;
  {$ELSEIF CompilerVersion < 35}
  SkSupportedPlatformsMask = pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm;
  {$ELSE}
  SkSupportedPlatformsMask = pidWin32 or pidWin64 or pidLinux64 or pidAndroidArm32 or pidAndroidArm64 or pidiOSDevice64 or pidOSX64 or pidOSXArm64;
  {$ENDIF}

type
  ESkFMX              = class(Exception);
  ESkBitmapHelper     = class(ESkFMX);
  ESkPersistentData   = class(ESkFMX);
  ESkTextSettingsInfo = class(ESkFMX);
  ESkLabel            = class(ESkFMX);

  TSkDrawProc = reference to procedure(const ACanvas: ISkCanvas);

  { TSkBitmapHelper }

  TSkBitmapHelper = class helper for TBitmap
  public
    procedure SkiaDraw(const AProc: TSkDrawProc; const AStartClean: Boolean = True);
    function ToSkImage: ISkImage;
  end;

  { TSkPathDataHelper }

  TSkPathDataHelper = class helper for TPathData
  public
    procedure FromSkPath(const AValue: ISkPath);
    function ToSkPath: ISkPath;
  end;

  TSkDrawEvent = procedure(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single) of object;
  TSkDrawCacheKind = (Never, Raster, Always);

  { TSkCustomControl }

  TSkCustomControl = class abstract(TControl)
  strict private
    FBuffer: TBitmap;
    FDrawCached: Boolean;
    FDrawCacheKind: TSkDrawCacheKind;
    FOnDraw: TSkDrawEvent;
    procedure SetDrawCacheKind(const AValue: TSkDrawCacheKind);
    procedure SetOnDraw(const AValue: TSkDrawEvent);
  strict protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); virtual;
    procedure DrawDesignBorder(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function NeedsRedraw: Boolean; virtual;
    procedure Paint; override; final;
    property DrawCacheKind: TSkDrawCacheKind read FDrawCacheKind write SetDrawCacheKind default TSkDrawCacheKind.Raster;
    property OnDraw: TSkDrawEvent read FOnDraw write SetOnDraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Enabled default True;
    property EnableDragHighlight default True;
    property Height;
    property Hint;
    property HitTest default False;
    property Locked default False;
    property Margins;
    property Opacity;
    property Padding;
    {$IF CompilerVersion >= 30}
    property ParentShowHint;
    {$ENDIF}
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowHint;
    property Size;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragEnd;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnPainting;
    property OnResize;
    {$IF CompilerVersion >= 32}
    property OnResized;
    {$ENDIF}
  end;

  { TSkStyledControl }

  TSkStyledControl = class abstract(TStyledControl)
  strict private
    FBuffer: TBitmap;
    FDrawCached: Boolean;
    FDrawCacheKind: TSkDrawCacheKind;
    FOnDraw: TSkDrawEvent;
    procedure SetDrawCacheKind(const AValue: TSkDrawCacheKind);
    procedure SetOnDraw(const AValue: TSkDrawEvent);
  strict protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); virtual;
    procedure DrawDesignBorder(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function NeedsRedraw: Boolean; virtual;
    procedure Paint; override; final;
    property DrawCacheKind: TSkDrawCacheKind read FDrawCacheKind write SetDrawCacheKind default TSkDrawCacheKind.Raster;
    property OnDraw: TSkDrawEvent read FOnDraw write SetOnDraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw;
  end;

  { TSkPaintBox }

  [ComponentPlatforms(SkSupportedPlatformsMask)]
  TSkPaintBox = class(TSkCustomControl)
  published
    property OnDraw;
  end;

  TSkSvgSource = type string;
  TSkSvgWrapMode = (Default, Fit, FitCrop, Original, OriginalCenter, Place, Stretch, Tile);

  { TSkSvgBrush }

  TSkSvgBrush = class(TPersistent)
  strict private
    const
      DefaultGrayScale = False;
      DefaultWrapMode = TSkSvgWrapMode.Fit;
  strict private
    FDOM: ISkSVGDOM;
    FGrayScale: Boolean;
    FOnChanged: TNotifyEvent;
    FOriginalSize: TSizeF;
    FOverrideColor: TAlphaColor;
    FSource: TSkSvgSource;
    FWrapMode: TSkSvgWrapMode;
    function GetDOM: ISkSVGDOM;
    function GetOriginalSize: TSizeF;
    function IsGrayScaleStored: Boolean;
    function IsOverrideColorStored: Boolean;
    function IsWrapModeStored: Boolean;
    procedure SetGrayScale(const AValue: Boolean);
    procedure SetOverrideColor(const AValue: TAlphaColor);
    procedure SetSource(const AValue: TSkSvgSource);
    procedure SetWrapMode(const AValue: TSkSvgWrapMode);
  strict protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    procedure Render(const ACanvas: ISkCanvas; const ADestRect: TRectF; const AOpacity: Single);
    property DOM: ISkSVGDOM read GetDOM;
    property OriginalSize: TSizeF read GetOriginalSize;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property GrayScale: Boolean read FGrayScale write SetGrayScale stored IsGrayScaleStored;
    property OverrideColor: TAlphaColor read FOverrideColor write SetOverrideColor stored IsOverrideColorStored;
    property Source: TSkSvgSource read FSource write SetSource;
    property WrapMode: TSkSvgWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
  end;

  { TSkSvg }

  [ComponentPlatforms(SkSupportedPlatformsMask)]
  TSkSvg = class(TSkCustomControl)
  strict private
    FSvg: TSkSvgBrush;
    procedure SetSvg(const AValue: TSkSvgBrush);
    procedure SvgChanged(ASender: TObject);
  strict protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Svg: TSkSvgBrush read FSvg write SetSvg;
    property OnDraw;
  end;

  TSkAnimationDrawEvent = procedure(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single) of object;
  TSkAnimationDrawProc = reference to procedure(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single);

  { TSkCustomAnimatedControl }

  TSkCustomAnimatedControl = class abstract(TSkCustomControl)
  strict private
    FAbsoluteVisible: Boolean;
    FAbsoluteVisibleCached: Boolean;
    FAnimation: TAnimation;
    FAnimationStartTickCount: Cardinal;
    FFixedProgress: Boolean;
    FIsStaticImage: Boolean;
    FLoop: Boolean;
    FOnAnimationDraw: TSkAnimationDrawEvent;
    FOnAnimationFinished: TNotifyEvent;
    FOnAnimationProgress: TNotifyEvent;
    FOnAnimationStart: TNotifyEvent;
    FProgress: Double;
    FProgressChangedManually: Boolean;
    FSuccessRepaint: Boolean;
    function GetAbsoluteVisible: Boolean;
    function GetRunningAnimation: Boolean;
    procedure SetFixedProgress(const AValue: Boolean);
    procedure SetIsStaticImage(const AValue: Boolean);
    procedure SetLoop(const AValue: Boolean);
    procedure SetProgress(AValue: Double);
  private
    procedure ProcessAnimation;
    procedure SetOnAnimationDraw(const Value: TSkAnimationDrawEvent);
  strict protected
    procedure AncestorVisibleChanged(const AVisible: Boolean); override;
    function CanRunAnimation: Boolean; virtual;
    procedure CheckAnimation;
    procedure DoAnimationFinished; virtual;
    procedure DoAnimationProgress; virtual;
    procedure DoAnimationStart; virtual;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function GetDuration: Double; virtual; abstract;
    property IsStaticImage: Boolean read FIsStaticImage write SetIsStaticImage;
    procedure RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single); virtual;
    property AbsoluteVisible: Boolean read GetAbsoluteVisible;
    property Duration: Double read GetDuration;
    property FixedProgress: Boolean read FFixedProgress write SetFixedProgress;
    property Progress: Double read FProgress write SetProgress;
    property Loop: Boolean read FLoop write SetLoop;
    property OnAnimationDraw: TSkAnimationDrawEvent read FOnAnimationDraw write SetOnAnimationDraw;
    property OnAnimationFinished: TNotifyEvent read FOnAnimationFinished write FOnAnimationFinished;
    property OnAnimationProgress: TNotifyEvent read FOnAnimationProgress write FOnAnimationProgress;
    property OnAnimationStart: TNotifyEvent read FOnAnimationStart write FOnAnimationStart;
    property RunningAnimation: Boolean read GetRunningAnimation;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RecalcEnabled; override;
    procedure SetNewScene(AScene: IScene); override;
  end;

  { TSkAnimatedPaintBox }

  TSkAnimatedPaintBox = class(TSkCustomAnimatedControl)
  strict private
    const
      DefaultDuration = 1;
  strict private
    FAnimate: Boolean;
    FDuration: Double;
    function IsDurationStored: Boolean;
    procedure SetAnimate(const AValue: Boolean);
    procedure SetDuration(const AValue: Double);
  strict protected
    function CanRunAnimation: Boolean; override;
    function GetDuration: Double; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Animate: Boolean read FAnimate write SetAnimate default True;
    property Duration: Double read FDuration write SetDuration stored IsDurationStored;
    property Loop default True;
    property OnAnimationDraw;
  end;

  TSkAnimatedImageWrapMode = (Fit, FitCrop, Original, OriginalCenter, Place, Stretch);

  { TSkAnimatedImage }

  [ComponentPlatforms(SkSupportedPlatformsMask)]
  TSkAnimatedImage = class(TSkCustomAnimatedControl)
  public
    type
      { TSource }

      TSource = class(TPersistent)
      strict private
        FData: TBytes;
        FOnChange: TNotifyEvent;
        procedure SetData(const AValue: TBytes);
      public
        constructor Create(const AOnChange: TNotifyEvent);
        procedure Assign(ASource: TPersistent); override;
        function Equals(AObject: TObject): Boolean; override;
        property Data: TBytes read FData write SetData;
      end;

      { TFormatInfo }

      TFormatInfo = record
        Description: string;
        Extensions: TArray<string>;
        Name: string;
        constructor Create(const AName, ADescription: string; const AExtensions: TArray<string>);
      end;

      { TAnimationCodec }

      TAnimationCodec = class
      strict private
        FQuality: TCanvasQuality;
      strict protected
        function GetDuration: Double; virtual; abstract;
        function GetIsStatic: Boolean; virtual; abstract;
        function GetSize: TSizeF; virtual; abstract;
      public
        procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); virtual; abstract;
        procedure SeekFrameTime(const ATime: Double); virtual; abstract;
        class function SupportedFormats: TArray<TFormatInfo>; virtual; abstract;
        class function TryDetectFormat(const ABytes: TBytes; out AFormat: TFormatInfo): Boolean; virtual; abstract;
        class function TryMakeFromStream(const AStream: TStream; out ACodec: TAnimationCodec): Boolean; virtual; abstract;
        property Duration: Double read GetDuration;
        property IsStatic: Boolean read GetIsStatic;
        property Quality: TCanvasQuality read FQuality write FQuality;
        property Size: TSizeF read GetSize;
      end;

      TAnimationCodecClass = class of TAnimationCodec;
  strict private
    class var
      FRegisteredCodecs: TArray<TAnimationCodecClass>;
  strict private
    FCodec: TAnimationCodec;
    FSource: TSource;
    FWrapMode: TSkAnimatedImageWrapMode;
    function GetOriginalSize: TSizeF;
    procedure ReadData(AStream: TStream);
    procedure SetSource(const AValue: TSource);
    procedure SetWrapMode(const AValue: TSkAnimatedImageWrapMode);
    procedure SourceChange(ASender: TObject);
    procedure WriteData(AStream: TStream);
  strict protected
    function CanRunAnimation: Boolean; override;
    procedure DefineProperties(AFiler: TFiler); override;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    function GetDuration: Double; override;
    procedure RenderFrame(const ACanvas: ISkCanvas; const ADest: TRectF; const AProgress: Double; const AOpacity: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);
    class procedure RegisterCodec(const ACodecClass: TAnimationCodecClass); static;
    class property RegisteredCodecs: TArray<TAnimationCodecClass> read FRegisteredCodecs;
    property FixedProgress;
    property OriginalSize: TSizeF read GetOriginalSize;
    property Progress;
    property RunningAnimation;
  published
    property Loop default True;
    property Source: TSource read FSource write SetSource;
    property WrapMode: TSkAnimatedImageWrapMode read FWrapMode write SetWrapMode default TSkAnimatedImageWrapMode.Fit;
    property OnAnimationDraw;
    property OnAnimationFinished;
    property OnAnimationProgress;
    property OnAnimationStart;
  end;

  { TSkPersistentData }

  TSkPersistentData = class(TPersistent)
  strict private
    FChanged: Boolean;
    FCreated: Boolean;
    FIgnoringAllChanges: Boolean;
    FOnChange: TNotifyEvent;
    FUpdatingCount: Integer;
    function GetUpdating: Boolean;
  protected
    procedure DoAssign(ASource: TPersistent); virtual;
    procedure DoChanged; virtual;
    function GetHasChanged: Boolean; virtual;
    procedure SetValue(var AField: Byte; const AValue: Byte); overload;
    procedure SetValue(var AField: Word; const AValue: Word); overload;
    procedure SetValue(var AField: Cardinal; const AValue: Cardinal); overload;
    procedure SetValue(var AField: Boolean; const AValue: Boolean); overload;
    procedure SetValue(var AField: Integer; const AValue: Integer); overload;
    procedure SetValue(var AField: Int64; const AValue: Int64); overload;
    procedure SetValue(var AField: Single; const AValue: Single; const AEpsilon: Single = 0.0); overload;
    procedure SetValue(var AField: Double; const AValue: Double; const AEpsilon: Double = 0.0); overload;
    procedure SetValue(var AField: TBytes; const AValue: TBytes); overload;
    procedure SetValue(var AField: string; const AValue: string); overload;
    procedure SetValue<T>(var AField: T; const AValue: T); overload;
    property Created: Boolean read FCreated;
    property UpdatingCount: Integer read FUpdatingCount;
  public
    procedure AfterConstruction; override;
    procedure Assign(ASource: TPersistent); override; final;
    procedure BeginUpdate; overload;
    procedure BeginUpdate(const AIgnoreAllChanges: Boolean); overload; virtual;
    procedure Change; virtual;
    procedure EndUpdate; overload;
    procedure EndUpdate(const AIgnoreAllChanges: Boolean); overload; virtual;
    property HasChanged: Boolean read GetHasChanged;
    property Updating: Boolean read GetUpdating;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IF CompilerVersion < 31}
  TFontWeight = (Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack);
  TFontSlant = (Regular, Italic, Oblique);
  TFontStretch = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Regular, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded);
  {$ENDIF}

  { TSkFontComponent }

  TSkFontComponent = class(TSkPersistentData)
  strict protected
    const
      DefaultFamilies = '';
      DefaultSize = 14;
      DefaultSlant = TFontSlant.Regular;
      DefaultStretch = TFontStretch.Regular;
      DefaultWeight = TFontWeight.Regular;
  strict private
    FFamilies: string;
    FSize: Single;
    FSlant: TFontSlant;
    FStretch: TFontStretch;
    FWeight: TFontWeight;
    procedure SetFamilies(const AValue: string);
    procedure SetSize(const AValue: Single);
    procedure SetSlant(const AValue: TFontSlant);
    procedure SetStretch(const AValue: TFontStretch);
    procedure SetWeight(const AValue: TFontWeight);
  strict protected
    procedure AssignTo(ADest: TPersistent); override;
    procedure DoAssign(ASource: TPersistent); override;
    function IsFamiliesStored: Boolean; virtual;
    function IsSizeStored: Boolean; virtual;
  public
    constructor Create;
    function Equals(AObject: TObject): Boolean; override;
  published
    property Families: string read FFamilies write SetFamilies stored IsFamiliesStored;
    property Size: Single read FSize write SetSize stored IsSizeStored;
    property Slant: TFontSlant read FSlant write SetSlant default DefaultSlant;
    property Stretch: TFontStretch read FStretch write SetStretch default DefaultStretch;
    property Weight: TFontWeight read FWeight write SetWeight default DefaultWeight;
  end;

  TSkTextHorzAlign = (Center, Leading, Trailing, Justify);

  { TSkTextSettings }

  TSkTextSettings = class(TSkPersistentData)
  public
    type
      { TDecorations }

      TDecorations = class(TSkPersistentData)
      strict protected
        const
          DefaultColor = TAlphaColors.Null;
          DefaultDecorations = [];
          DefaultStyle = TSkTextDecorationStyle.Solid;
          DefaultThickness = 1;
      strict private
        FColor: TAlphaColor;
        FDecorations: TSkTextDecorations;
        FStyle: TSkTextDecorationStyle;
        FThickness: Single;
        procedure SetColor(const AValue: TAlphaColor);
        procedure SetDecorations(const AValue: TSkTextDecorations);
        procedure SetStyle(const AValue: TSkTextDecorationStyle);
        procedure SetThickness(const AValue: Single);
      strict protected
        procedure DoAssign(ASource: TPersistent); override;
        function IsThicknessStored: Boolean; virtual;
      public
        constructor Create;
        function Equals(AObject: TObject): Boolean; override;
      published
        property Color: TAlphaColor read FColor write SetColor default DefaultColor;
        property Decorations: TSkTextDecorations read FDecorations write SetDecorations default DefaultDecorations;
        property Style: TSkTextDecorationStyle read FStyle write SetStyle default DefaultStyle;
        property Thickness: Single read FThickness write SetThickness stored IsThicknessStored;
      end;
  strict protected
    const
      DefaultFontColor = TAlphaColors.Black;
      DefaultHorzAlign = TSkTextHorzAlign.Leading;
      DefaultMaxLines = 1;
      DefaultTrimming = TTextTrimming.Word;
      DefaultVertAlign = TTextAlign.Center;
  strict private
    FDecorations: TDecorations;
    FFont: TSkFontComponent;
    FFontColor: TAlphaColor;
    FHorzAlign: TSkTextHorzAlign;
    FMaxLines: NativeUInt;
    [unsafe] FOwner: TPersistent;
    FTrimming: TTextTrimming;
    FVertAlign: TTextAlign;
    procedure DecorationsChange(ASender: TObject);
    procedure FontChanged(ASender: TObject);
    procedure SetDecorations(const AValue: TDecorations);
    procedure SetFont(const AValue: TSkFontComponent);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetHorzAlign(const AValue: TSkTextHorzAlign);
    procedure SetMaxLines(const AValue: NativeUInt);
    procedure SetTrimming(const AValue: TTextTrimming);
    procedure SetVertAlign(const AValue: TTextAlign);
  strict protected
    procedure DoAssign(ASource: TPersistent); override;
    procedure DoAssignNotStyled(const ATextSettings: TSkTextSettings; const AStyledSettings: TStyledSettings); virtual;
  public
    constructor Create(const AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure AssignNotStyled(const ATextSettings: TSkTextSettings; const AStyledSettings: TStyledSettings);
    function Equals(AObject: TObject): Boolean; override;
    procedure UpdateStyledSettings(const AOldTextSettings, ADefaultTextSettings: TSkTextSettings;
      var AStyledSettings: TStyledSettings); virtual;
    property Owner: TPersistent read FOwner;
  published
    property Decorations: TDecorations read FDecorations write SetDecorations;
    property Font: TSkFontComponent read FFont write SetFont;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default DefaultFontColor;
    property HorzAlign: TSkTextHorzAlign read FHorzAlign write SetHorzAlign default DefaultHorzAlign;
    property MaxLines: NativeUInt read FMaxLines write SetMaxLines default DefaultMaxLines;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default DefaultTrimming;
    property VertAlign: TTextAlign read FVertAlign write SetVertAlign default DefaultVertAlign;
  end;

  TSkTextSettingsClass = class of TSkTextSettings;

  { ISkTextSettings }

  ISkTextSettings = interface
    ['{CE7E837B-F927-4C78-B1D2-C62EF4A93014}']
    function GetDefaultTextSettings: TSkTextSettings;
    function GetResultingTextSettings: TSkTextSettings;
    function GetStyledSettings: TStyledSettings;
    function GetTextSettings: TSkTextSettings;
    procedure SetStyledSettings(const AValue: TStyledSettings);
    procedure SetTextSettings(const AValue: TSkTextSettings);
    property DefaultTextSettings: TSkTextSettings read GetDefaultTextSettings;
    property ResultingTextSettings: TSkTextSettings read GetResultingTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings;
    property TextSettings: TSkTextSettings read GetTextSettings write SetTextSettings;
  end;

  { TSkTextSettingsInfo }

  TSkTextSettingsInfo = class(TPersistent)
  public
    type
      TBaseTextSettings = class (TSkTextSettings)
      strict private
        [unsafe] FControl: TControl;
        [unsafe] FInfo: TSkTextSettingsInfo;
      public
        constructor Create(const AOwner: TPersistent); override;
        property Control: TControl read FControl;
        property Info: TSkTextSettingsInfo read FInfo;
      end;

      TCustomTextSettings = class (TBaseTextSettings)
      public
        constructor Create(const AOwner: TPersistent); override;
      published
        property MaxLines default 0;
      end;

      TCustomTextSettingsClass = class of TCustomTextSettings;
  strict private
    FDefaultTextSettings: TSkTextSettings;
    FDesign: Boolean;
    FOldTextSettings: TSkTextSettings;
    FOnChange: TNotifyEvent;
    [unsafe] FOwner: TPersistent;
    FResultingTextSettings: TSkTextSettings;
    FStyledSettings: TStyledSettings;
    FTextSettings: TSkTextSettings;
    procedure OnCalculatedTextSettings(ASender: TObject);
    procedure OnDefaultChanged(ASender: TObject);
    procedure OnTextChanged(ASender: TObject);
    procedure SetDefaultTextSettings(const AValue: TSkTextSettings);
    procedure SetStyledSettings(const AValue: TStyledSettings);
    procedure SetTextSettings(const AValue: TSkTextSettings);
  strict protected
    procedure DoCalculatedTextSettings; virtual;
    procedure DoDefaultChanged; virtual;
    procedure DoStyledSettingsChanged; virtual;
    procedure DoTextChanged; virtual;
    procedure RecalculateTextSettings; virtual;
  public
    constructor Create(AOwner: TPersistent; ATextSettingsClass: TSkTextSettingsInfo.TCustomTextSettingsClass); virtual;
    destructor Destroy; override;
    property DefaultTextSettings: TSkTextSettings read FDefaultTextSettings write SetDefaultTextSettings;
    property Design: Boolean read FDesign write FDesign;
    property Owner: TPersistent read FOwner;
    property ResultingTextSettings: TSkTextSettings read FResultingTextSettings;
    property StyledSettings: TStyledSettings read FStyledSettings write SetStyledSettings;
    property TextSettings: TSkTextSettings read FTextSettings write SetTextSettings;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { ISkStyleTextObject }

  ISkStyleTextObject = interface(IFreeNotificationBehavior)
    ['{B5584DD4-D568-4A84-BCD5-C8CB7C068E4D}']
    function GetTextSettings: TSkTextSettings;
    procedure SetTextSettings(const AValue: TSkTextSettings);
    property TextSettings: TSkTextSettings read GetTextSettings write SetTextSettings;
  end;

  { TSkStyleTextObject }

  TSkStyleTextObject = class(TFmxObject, ISkStyleTextObject, IObjectState)
  strict private
    FOnChange: TNotifyEvent;
    FTextSettings: TSkTextSettings;
    FSavedTextSettings: TSkTextSettings;
    function GetTextSettings: TSkTextSettings;
    function RestoreState: Boolean;
    function SaveState: Boolean;
    procedure SetTextSettings(const AValue: TSkTextSettings);
    procedure TextSettingsChange(ASender: TObject);
  strict protected
    procedure SetName(const ANewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property TextSettings: TSkTextSettings read FTextSettings write SetTextSettings;
  end;

  { TSkLabel }

  [ComponentPlatforms(SkSupportedPlatformsMask)]
  TSkLabel = class(TSkStyledControl, ISkTextSettings{$IF CompilerVersion >= 29}, ICaption{$ENDIF})
  public
    type
      TWordsCollection = class;
      TCustomWordsItemClass = class of TCustomWordsItem;

      TCustomWordsItem = class(TCollectionItem)
      strict protected
        const
          DefaultBackgroundColor = TAlphaColors.Null;
          DefaultCursor = crDefault;
          DefaultFontColor = TAlphaColors.Black;
          DefaultName = 'Item 0';
          DefaultText = '';
      strict private
        FBackgroundColor: TAlphaColor;
        FChanged: Boolean;
        FCursor: TCursor;
        FIgnoringAllChanges: Boolean;
        FName: string;
        FOnClick: TNotifyEvent;
        FText: string;
        FTextSettingsInfo: TSkTextSettingsInfo;
        FUpdatingCount: Integer;
        [unsafe] FWords: TWordsCollection;
        procedure CheckName(const AName: string; AWordsCollection: TWordsCollection);
        function GetDecorations: TSkTextSettings.TDecorations;
        function GetFont: TSkFontComponent;
        function GetFontColor: TAlphaColor;
        function GetStyledSettings: TStyledSettings;
        function IsFontColorStored: Boolean;
        function IsNameStored: Boolean;
        function IsStyledSettingsStored: Boolean;
        function IsTextStored: Boolean;
        procedure TextSettingsChange(ASender: TObject);
        procedure SetBackgroundColor(const AValue: TAlphaColor);
        procedure SetCursor(const AValue: TCursor);
        procedure SetDecorations(const AValue: TSkTextSettings.TDecorations);
        procedure SetFont(const AValue: TSkFontComponent);
        procedure SetFontColor(const AValue: TAlphaColor);
        procedure SetName(const AValue: string);
        procedure SetStyledSettings(const AValue: TStyledSettings);
        procedure SetText(const AValue: string);
        function UniqueName(const AName: string; const ACollection: TCollection): string;
      strict protected
        procedure DoAssign(ASource: TPersistent); virtual;
        procedure DoChanged; virtual;
        function GetDisplayName: string; override;
        procedure SetCollection(AValue: TCollection); override;
      public
        constructor Create(ACollection: TCollection); override;
        destructor Destroy; override;
        procedure Assign(ASource: TPersistent); override; final;
        procedure BeginUpdate; overload;
        procedure Change; virtual;
        procedure EndUpdate; overload;
        procedure EndUpdate(const AIgnoreAllChanges: Boolean); overload; virtual;
        property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default DefaultBackgroundColor;
        property Cursor: TCursor read FCursor write SetCursor default crDefault;
        property Decorations: TSkTextSettings.TDecorations read GetDecorations write SetDecorations;
        property Font: TSkFontComponent read GetFont write SetFont;
        property FontColor: TAlphaColor read GetFontColor write SetFontColor stored IsFontColorStored;
        /// <summary> The case-insensitive name of the item in the collection. This field cannot be empty and must be unique for his collection </summary>
        property Name: string read FName write SetName stored IsNameStored;
        property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings stored IsStyledSettingsStored;
        property Text: string read FText write SetText stored IsTextStored;
        property Words: TWordsCollection read FWords;
        property OnClick: TNotifyEvent read FOnClick write FOnClick;
      end;

      { TWordsCollection }

      TWordsCollection = class(TOwnedCollection)
      strict protected
        const
          DefaultColor = TAlphaColors.Black;
          DefaultFontSize = 14;
          DefaultFontSlant = TFontSlant.Regular;
          DefaultFontWeight = TFontWeight.Regular;
      strict private
        [unsafe] FLabel: TSkLabel;
        FOnChange: TNotifyEvent;
        function GetItem(AIndex: Integer): TCustomWordsItem;
        function GetItemByName(const AName: string): TCustomWordsItem;
        procedure SetItem(AIndex: Integer; const AValue: TCustomWordsItem);
      strict protected
        procedure Update(AItem: TCollectionItem); override;
      public
        constructor Create(AOwner: TPersistent; AItemClass: TCustomWordsItemClass);
        function Add: TCustomWordsItem; overload;
        function Add(const AText: string; const AColor: TAlphaColor = DefaultColor;
          const AFontSize: Single = DefaultFontSize;
          const AFontWeight: TFontWeight = DefaultFontWeight;
          const AFontSlant: TFontSlant = DefaultFontSlant): TCustomWordsItem; overload;
        function AddOrSet(const AName, AText: string; const AFontColor: TAlphaColor = DefaultColor;
          const AFont: TSkFontComponent = nil; const AOnClick: TNotifyEvent = nil;
          const ACursor: TCursor = crDefault): TCustomWordsItem;
        function Insert(AIndex: Integer): TCustomWordsItem;
        /// <summary> Case-insensitive search of item by name</summary>
        function IndexOf(const AName: string): Integer;
        /// <summary> Case-insensitive search of item by name</summary>
        property ItemByName[const AName: string]: TCustomWordsItem read GetItemByName;
        property Items[AIndex: Integer]: TCustomWordsItem read GetItem write SetItem; default;
        property &Label: TSkLabel read FLabel;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
      end;

      { TWordsItem }

      TWordsItem = class (TCustomWordsItem)
      published
        property BackgroundColor;
        property Cursor;
        property Decorations;
        property Font;
        property FontColor;
        property Name;
        property StyledSettings;
        property Text;
        property OnClick;
      end;
  strict private
    FAutoSize: Boolean;
    FBackgroundPicture: ISkPicture;
    FHasCustomBackground: Boolean;
    FHasCustomCursor: Boolean;
    FLastFillTextFlags: TFillTextFlags;
    FObjectState: IObjectState;
    FParagraph: ISkParagraph;
    FParagraphBounds: TRectF;
    FParagraphLayoutWidth: Single;
    FStyleText: ISkStyleTextObject;
    FTextSettingsInfo: TSkTextSettingsInfo;
    FWords: TWordsCollection;
    FWordsMouseOver: TCustomWordsItem;
    procedure DeleteParagraph;
    procedure GetFitSize(var AWidth, AHeight: Single);
    function GetParagraph: ISkParagraph;
    function GetParagraphBounds: TRectF;
    function GetText: string;
    function HasFitSizeChanged: Boolean;
    procedure ParagraphLayout(const AWidth: Single);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetWords(const AValue: TWordsCollection);
    procedure SetWordsMouseOver(const AValue: TCustomWordsItem);
    procedure TextSettingsChanged(AValue: TObject);
    {$IF CompilerVersion >= 29}
    { ICaption }
    function TextStored: Boolean;
    {$ENDIF}
    procedure WordsChange(ASender: TObject);
  strict private
    { ISkTextSettings }
    function GetDefaultTextSettings: TSkTextSettings;
    function GetResultingTextSettings: TSkTextSettings;
    function GetStyledSettings: TStyledSettings;
    function GetTextSettings: TSkTextSettings;
    procedure SetStyledSettings(const AValue: TStyledSettings);
    procedure SetTextSettings(const AValue: TSkTextSettings);
  strict protected
    procedure ApplyStyle; override;
    procedure Click; override;
    procedure DoEndUpdate; override;
    procedure DoMouseLeave; override;
    function DoSetSize(const ASize: TControlSize; const ANewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
      var ALastWidth, ALastHeight: Single): Boolean; override;
    procedure DoStyleChanged; override;
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    procedure FreeStyle; override;
    function GetDefaultSize: TSizeF; override;
    function GetTextSettingsClass: TSkTextSettingsInfo.TCustomTextSettingsClass; virtual;
    function IsStyledSettingsStored: Boolean; virtual;
    procedure Loaded; override;
    procedure MouseMove(AShift: TShiftState; AX, AY: Single); override;
    function NeedsRedraw: Boolean; override;
    procedure SetAlign(const AValue: TAlignLayout); override;
    procedure SetName(const AValue: TComponentName); override;
    property Paragraph: ISkParagraph read GetParagraph;
    property ParagraphBounds: TRectF read GetParagraphBounds;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DefaultTextSettings: TSkTextSettings read GetDefaultTextSettings;
    property ResultingTextSettings: TSkTextSettings read GetResultingTextSettings;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property DragMode default TDragMode.dmManual;
    property Enabled default True;
    property EnableDragHighlight default True;
    property Height;
    property Hint;
    property HitTest default False;
    property Locked default False;
    property Margins;
    property Opacity;
    property Padding;
    {$IF CompilerVersion >= 30}
    property ParentShowHint;
    {$ENDIF}
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowHint;
    property Size;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragEnd;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnPainting;
    property OnResize;
    {$IF CompilerVersion >= 32}
    property OnResized;
    {$ENDIF}
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property AutoTranslate default True;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings stored IsStyledSettingsStored;
    property StyleLookup;
    property Text: string read GetText write SetText stored False;
    property TextSettings: TSkTextSettings read GetTextSettings write SetTextSettings;
    property TouchTargetExpansion;
    property Words: TWordsCollection read FWords write SetWords;
    property OnApplyStyleLookup;
    property OnDraw;
  end;

  { TSkTypefaceManager }

  TSkTypefaceManager = class sealed
  strict private
    class var FProvider: ISkTypefaceFontProvider;
    class constructor Create;
  public
    class procedure RegisterTypeface(const AFileName: string); overload; static;
    class procedure RegisterTypeface(const AStream: TStream); overload; static;
    class property Provider: ISkTypefaceFontProvider read FProvider;
  end;

const
  SkFmxColorType: array[TPixelFormat] of TSkColorType = (
    { None     } TSkColorType.Unknown,
    { RGB      } TSkColorType.Unknown,
    { RGBA     } TSkColorType.RGBA8888,
    { BGR      } TSkColorType.Unknown,
    { BGRA     } TSkColorType.BGRA8888,
    { RGBA16   } TSkColorType.RGBA16161616,
    { BGR_565  } TSkColorType.RGB565,
    { BGRA4    } TSkColorType.ARGB4444,
    { BGR4     } TSkColorType.Unknown,
    { BGR5_A1  } TSkColorType.Unknown,
    { BGR5     } TSkColorType.Unknown,
    { BGR10_A2 } TSkColorType.BGRA1010102,
    { RGB10_A2 } TSkColorType.RGBA1010102,
    { L        } TSkColorType.Gray8,
    { LA       } TSkColorType.Unknown,
    { LA4      } TSkColorType.Unknown,
    { L16      } TSkColorType.Unknown,
    { A        } TSkColorType.Alpha8,
    { R16F     } TSkColorType.AlphaF16,
    { RG16F    } TSkColorType.RGF16,
    { RGBA16F  } TSkColorType.RGBAF16,
    { R32F     } TSkColorType.Unknown,
    { RG32F    } TSkColorType.Unknown,
    { RGBA32F  } TSkColorType.RGBAF32
  );


  SkFmxPixelFormat: array[TSkColorType] of TPixelFormat = (
    { Unknown        } TPixelFormat.None,
    { Alpha8         } TPixelFormat.A,
    { RGB565         } TPixelFormat.BGR_565,
    { ARGB4444       } TPixelFormat.BGRA4,
    { RGBA8888       } TPixelFormat.RGBA,
    { RGB888X        } TPixelFormat.None,
    { BGRA8888       } TPixelFormat.BGRA,
    { RGBA1010102    } TPixelFormat.RGB10_A2,
    { BGRA1010102    } TPixelFormat.BGR10_A2,
    { RGB101010X     } TPixelFormat.None,
    { BGR101010X     } TPixelFormat.None,
    { Gray8          } TPixelFormat.L,
    { RGBAF16        } TPixelFormat.RGBA16F,
    { RGBAF16Clamped } TPixelFormat.RGBA16F,
    { RGBAF32        } TPixelFormat.RGBA32F,
    { RG88           } TPixelFormat.None,
    { AlphaF16       } TPixelFormat.R16F,
    { RGF16          } TPixelFormat.RG16F,
    { Alpha16        } TPixelFormat.None,
    { RG1616         } TPixelFormat.None,
    { RGBA16161616   } TPixelFormat.RGBA16,
    { SRGBA8888      } TPixelFormat.None
  );

var
  GlobalUseSkia: Boolean;
  GlobalUseSkiaRasterWhenAvailable: Boolean;
  GlobalDisableSkiaCodecsReplacement: Boolean;

procedure Register;

implementation

uses
  { Delphi }
  System.Math,
  System.Math.Vectors,
  System.ZLib,
  System.IOUtils,
  System.TypInfo,
  System.Character,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.RTLConsts,
  {$IF CompilerVersion >= 29}
  FMX.Utils,
  {$ENDIF}
  FMX.BehaviorManager,
  FMX.Forms;

type
  { TSkDefaultAnimationCodec }

  TSkDefaultAnimationCodec = class(TSkAnimatedImage.TAnimationCodec)
  strict private
    type
      TImageFormat = (GIF, WebP);
  strict private
    FAnimationCodec: ISkAnimationCodecPlayer;
    FStream: TStream;
  strict protected
    function GetDuration: Double; override;
    function GetIsStatic: Boolean; override;
    function GetSize: TSizeF; override;
  public
    constructor Create(const AAnimationCodec: ISkAnimationCodecPlayer; const AStream: TStream);
    destructor Destroy; override;
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    procedure SeekFrameTime(const ATime: Double); override;
    class function SupportedFormats: TArray<TSkAnimatedImage.TFormatInfo>; override;
    class function TryDetectFormat(const ABytes: TBytes; out AFormat: TSkAnimatedImage.TFormatInfo): Boolean; override;
    class function TryMakeFromStream(const AStream: TStream; out ACodec: TSkAnimatedImage.TAnimationCodec): Boolean; override;
  end;

  { TSkLottieAnimationCodec }

  TSkLottieAnimationCodec = class(TSkAnimatedImage.TAnimationCodec)
  strict private
    type
      TAnimationFormat = (Lottie, TGS);
  strict private
    FSkottie: ISkottieAnimation;
  strict protected
    function GetDuration: Double; override;
    function GetIsStatic: Boolean; override;
    function GetSize: TSizeF; override;
  public
    constructor Create(const ASkottie: ISkottieAnimation);
    procedure Render(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    procedure SeekFrameTime(const ATime: Double); override;
    class function SupportedFormats: TArray<TSkAnimatedImage.TFormatInfo>; override;
    class function TryDetectFormat(const ABytes: TBytes; out AFormat: TSkAnimatedImage.TFormatInfo): Boolean; override;
    class function TryMakeFromStream(const AStream: TStream; out ACodec: TSkAnimatedImage.TAnimationCodec): Boolean; override;
  end;

  {$IF CompilerVersion < 31}
  { TSkCanvasHelper }

  TSkCanvasHelper = class helper for TCanvas
  public
    function AlignToPixel(const ARect: TRectF): TRectF;
  end;
  {$ENDIF}

  {$IF CompilerVersion < 33}
  { TSkEpsilonHelper }

  TSkEpsilonHelper = record helper for TEpsilon
  const
    Scale = 1E-4;
    FontSize = 1E-2;
    Position = 1E-3;
  end;
  {$ENDIF}

  {$IF CompilerVersion < 32}
  { TSkRectFHelper }

  TSkRectFHelper = record helper for TRectF
  public
    function FitInto(const ADesignatedArea: TRectF): TRectF; overload;
    function FitInto(const ADesignatedArea: TRectF; out ARatio: Single): TRectF; overload;
  end;

  function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF; forward;
  {$ENDIF}

const
  ControlExceededBitmapLimitWarning = 'Skia.FMX: A control has exceeded the size allowed for a bitmap (class %s, name "%s"). We will reduce the drawing quality to avoid this exception. Consider using "GlobalUseSkia := True" to avoid this kind of problem.';
  SkFontSlant: array[TFontSlant] of TSkFontSlant = (TSkFontSlant.Upright, TSkFontSlant.Italic, TSkFontSlant.Oblique);
  SkFontWeightValue: array[TFontWeight] of Integer = (100, 200, 300, 350, 400, 500, 600, 700, 800, 900, 950);
  SkFontWidthValue: array[TFontStretch] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9);

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

{$IF CompilerVersion < 31}
{ TSkCanvasHelper }

function TSkCanvasHelper.AlignToPixel(const ARect: TRectF): TRectF;
begin
  Result.Left := AlignToPixelHorizontally(ARect.Left);
  Result.Top := AlignToPixelVertically(ARect.Top);
  Result.Right := Result.Left + Round(ARect.Width * Scale) / Scale; // keep ratio horizontally
  Result.Bottom := Result.Top + Round(ARect.Height * Scale) / Scale; // keep ratio vertically
end;
{$ENDIF}

{$IF CompilerVersion < 32}
{ TSkRectFHelper }

function TSkRectFHelper.FitInto(const ADesignatedArea: TRectF): TRectF;
var
  LRatio: Single;
begin
  Result := FitInto(ADesignatedArea, LRatio);
end;

function TSkRectFHelper.FitInto(const ADesignatedArea: TRectF; out ARatio: Single): TRectF;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    ARatio := 1;
    Exit(Self);
  end;

  if (Self.Width / ADesignatedArea.Width) > (Self.Height / ADesignatedArea.Height) then
    ARatio := Self.Width / ADesignatedArea.Width
  else
    ARatio := Self.Height / ADesignatedArea.Height;

  if ARatio = 0 then
    Exit(Self)
  else
  begin
    Result := TRectF.Create(0, 0, Self.Width / ARatio, Self.Height / ARatio);
    RectCenter(Result, ADesignatedArea);
  end;
end;

function RectCenter(var R: TRectF; const Bounds: TRectF): TRectF;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, (RectWidth(Bounds)/2 - RectWidth(R)/2), (RectHeight(Bounds)/2 - RectHeight(R)/2));
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;
{$ENDIF}

{ TSkBitmapHelper }

procedure TSkBitmapHelper.SkiaDraw(const AProc: TSkDrawProc; const AStartClean: Boolean);

  procedure DrawInSurface(const ASurface: ISkSurface);
  begin
    if Assigned(ASurface) then
    begin
      if AStartClean then
        ASurface.Canvas.Clear(TAlphaColors.Null);
      AProc(ASurface.Canvas);
    end;
  end;

var
  LColorType: TSkColorType;
  LSurface: ISkSurface;
  LData: TBitmapData;
  LAccess: TMapAccess;
begin
  Assert(Assigned(AProc));
  if IsEmpty then
    raise ESkBitmapHelper.Create('Invalid bitmap');
  if CanvasClass.InheritsFrom(TSkCanvasCustom) then
  begin
    if (Canvas.BeginSceneCount = 0) and Canvas.BeginScene then
    begin
      try
        DrawInSurface(TSkCanvasCustom(Canvas).Surface);
      finally
        Canvas.EndScene;
      end;
    end
    else
      DrawInSurface(TSkCanvasCustom(Canvas).Surface);
  end
  else
  begin
    case PixelFormat of
      TPixelFormat.RGBA: LColorType := TSkColorType.RGBA8888;
      TPixelFormat.BGRA: LColorType := TSkColorType.BGRA8888;
    else
      raise ESkBitmapHelper.Create('Invalid pixelformat');
    end;
    if AStartClean then
      LAccess := TMapAccess.Write
    else
      LAccess := TMapAccess.ReadWrite;
    if Map(LAccess, LData) then
      try
        LSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(LData.Width, LData.Height, LColorType), LData.Data, LData.Pitch);
        DrawInSurface(LSurface);
      finally
        Unmap(LData);
      end;
  end;
end;

function TSkBitmapHelper.ToSkImage: ISkImage;
var
  LColorType: TSkColorType;
  LData: TBitmapData;
begin
  if IsEmpty then
    raise ESkBitmapHelper.Create('Invalid bitmap');
  case PixelFormat of
    TPixelFormat.RGBA: LColorType := TSkColorType.RGBA8888;
    TPixelFormat.BGRA: LColorType := TSkColorType.BGRA8888;
  else
    raise ESkBitmapHelper.Create('Invalid pixelformat');
  end;
  if not Map(TMapAccess.Read, LData) then
    raise ESkBitmapHelper.Create('Could not map the bitmap');
  try
    Result := TSkImage.MakeFromRaster(TSkImageInfo.Create(Width, Height, LColorType), LData.Data, LData.Pitch);
  finally
    Unmap(LData);
  end;
end;

{ TSkPathDataHelper }

procedure TSkPathDataHelper.FromSkPath(const AValue: ISkPath);
var
  LElem: TSkPathIteratorElem;
  LPoints: TArray<TPointF>;
begin
  Clear;
  for LElem in AValue.GetIterator(False) do
  begin
    case LElem.Verb of
      TSkPathVerb.Move  : MoveTo(LElem.Points[0]);
      TSkPathVerb.Line  : LineTo(LElem.Points[1]);
      TSkPathVerb.Quad  : QuadCurveTo(LElem.Points[1], LElem.Points[2]);
      TSkPathVerb.Conic :
        begin
          LPoints := TSkPath.ConvertConicToQuads(LElem.Points[0], LElem.Points[1], LElem.Points[2], LElem.ConicWeight, 1);
          QuadCurveTo(LPoints[1], LPoints[2]);
          QuadCurveTo(LPoints[3], LPoints[4]);
        end;
      TSkPathVerb.Cubic : CurveTo(LElem.Points[1], LElem.Points[2], LElem.Points[3]);
      TSkPathVerb.Close : ClosePath;
    end;
  end;
end;

function TSkPathDataHelper.ToSkPath: ISkPath;
var
  I: Integer;
  LPathBuilder: ISkPathBuilder;
begin
  LPathBuilder := TSkPathBuilder.Create(TSkPathFillType.EvenOdd);
  I := 0;
  while I < Count do
  begin
    case Points[I].Kind of
      TPathPointKind.MoveTo  : LPathBuilder.MoveTo(Points[I].Point);
      TPathPointKind.LineTo  : LPathBuilder.LineTo(Points[I].Point);
      TPathPointKind.Close   : LPathBuilder.Close;
      TPathPointKind.CurveTo :
        begin
          LPathBuilder.CubicTo(Points[I].Point, Points[I + 1].Point, Points[I + 2].Point);
          Inc(I, 2);
        end;
    end;
    Inc(I);
  end;
  Result := LPathBuilder.Detach;
end;

{ TSkCustomControl }

constructor TSkCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  FDrawCacheKind := TSkDrawCacheKind.Raster;
  HitTest := False;
end;

destructor TSkCustomControl.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TSkCustomControl.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  if csDesigning in ComponentState then
    DrawDesignBorder(ACanvas, ADest, AOpacity);
end;

procedure TSkCustomControl.DrawDesignBorder(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
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
    LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
    LPaint.AlphaF := AOpacity;
    LPaint.Color := DesignBorderColor;
    LPaint.StrokeWidth := 1;
    LPaint.PathEffect := TSKPathEffect.MakeDash([3, 1], 0);
    ACanvas.DrawRect(R, LPaint);
  finally
    ACanvas.Restore;
  end;
end;

function TSkCustomControl.NeedsRedraw: Boolean;
begin
  Result := (not FDrawCached) or (FDrawCacheKind = TSkDrawCacheKind.Never) or (FBuffer = nil);
end;

procedure TSkCustomControl.Paint;

  procedure DrawUsingSkia(const ASurface: ISkSurface; const ADestRect: TRectF; const AOpacity: Single);
  begin
    if Assigned(ASurface) then
    begin
      Draw(ASurface.Canvas, ADestRect, AOpacity);
      if Assigned(FOnDraw) then
        FOnDraw(Self, ASurface.Canvas, ADestRect, AOpacity);
    end;
  end;

var
  LSceneScale: Single;
  LAbsoluteScale: TPointF;
  LAbsoluteSize: TSize;
  LAbsoluteBimapSize: TSize;
  LMaxBitmapSize: Integer;
  LExceededRatio: Single;
begin
  if (FDrawCacheKind <> TSkDrawCacheKind.Always) and (Canvas is TSkCanvasCustom) then
  begin
    DrawUsingSkia(TSkCanvasCustom(Canvas).Surface, Canvas.AlignToPixel(LocalRect), AbsoluteOpacity);
    FreeAndNil(FBuffer);
  end
  else
  begin
    if Assigned(Scene) then
      LSceneScale := Scene.GetSceneScale
    else
      LSceneScale := 1;
    LAbsoluteScale := AbsoluteScale;
    LAbsoluteSize := TSize.Create(Round(Width * LAbsoluteScale.X * LSceneScale), Round(Height * LAbsoluteScale.Y * LSceneScale));

    LMaxBitmapSize := Canvas.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    if (LAbsoluteSize.Width > LMaxBitmapSize) or (LAbsoluteSize.Height > LMaxBitmapSize) then
    begin
      LAbsoluteBimapSize := RectF(0, 0, LAbsoluteSize.Width, LAbsoluteSize.Height)
        .FitInto(RectF(0, 0, LMaxBitmapSize, LMaxBitmapSize), LExceededRatio).Size.Round;
      if NeedsRedraw or (TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize) then
        Log.d(Format(ControlExceededBitmapLimitWarning, [ClassName, Name]));
    end
    else
    begin
      LAbsoluteBimapSize := LAbsoluteSize;
      LExceededRatio := 1;
    end;

    if NeedsRedraw or (TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize) then
    begin
      if FBuffer = nil then
        FBuffer := TBitmap.Create(LAbsoluteBimapSize.Width, LAbsoluteBimapSize.Height)
      else if TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize then
        FBuffer.SetSize(LAbsoluteBimapSize.Width, LAbsoluteBimapSize.Height);
      FBuffer.SkiaDraw(
        procedure(const ACanvas: ISkCanvas)
        var
          LAbsoluteScale: TPointF;
          LDestRect: TRectF;
        begin
          ACanvas.Clear(TAlphaColors.Null);
          LAbsoluteScale := AbsoluteScale * LSceneScale / LExceededRatio;
          ACanvas.Concat(TMatrix.CreateScaling(LAbsoluteScale.X, LAbsoluteScale.Y));
          LDestRect := RectF(0, 0, LAbsoluteBimapSize.Width / LAbsoluteScale.X, LAbsoluteBimapSize.Height / LAbsoluteScale.Y);
          Draw(ACanvas, LDestRect, 1);
          if Assigned(FOnDraw) then
            FOnDraw(Self, ACanvas, LDestRect, 1);
        end, False);
      FDrawCached := True;
    end;
    Canvas.DrawBitmap(FBuffer, RectF(0, 0, FBuffer.Width, FBuffer.Height), Canvas.AlignToPixel(LocalRect), AbsoluteOpacity);
  end;
end;

procedure TSkCustomControl.Redraw;
begin
  FDrawCached := False;
  Repaint;
end;

procedure TSkCustomControl.SetDrawCacheKind(const AValue: TSkDrawCacheKind);
begin
  if FDrawCacheKind <> AValue then
  begin
    FDrawCacheKind := AValue;
    if FDrawCacheKind <> TSkDrawCacheKind.Always then
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

{ TSkStyledControl }

constructor TSkStyledControl.Create(AOwner: TComponent);
begin
  inherited;
  FDrawCacheKind := TSkDrawCacheKind.Raster;
  HitTest := False;
end;

destructor TSkStyledControl.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TSkStyledControl.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  if csDesigning in ComponentState then
    DrawDesignBorder(ACanvas, ADest, AOpacity);
end;

procedure TSkStyledControl.DrawDesignBorder(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
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
    LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
    LPaint.AlphaF := AOpacity;
    LPaint.Color := DesignBorderColor;
    LPaint.StrokeWidth := 1;
    LPaint.PathEffect := TSKPathEffect.MakeDash([3, 1], 0);
    ACanvas.DrawRect(R, LPaint);
  finally
    ACanvas.Restore;
  end;
end;

function TSkStyledControl.NeedsRedraw: Boolean;
begin
  Result := (not FDrawCached) or (FDrawCacheKind = TSkDrawCacheKind.Never) or (FBuffer = nil);
end;

procedure TSkStyledControl.Paint;

  procedure DrawUsingSkia(const ASurface: ISkSurface; const ADestRect: TRectF; const AOpacity: Single);
  begin
    if Assigned(ASurface) then
    begin
      Draw(ASurface.Canvas, ADestRect, AOpacity);
      if Assigned(FOnDraw) then
        FOnDraw(Self, ASurface.Canvas, ADestRect, AOpacity);
    end;
  end;

var
  LSceneScale: Single;
  LAbsoluteScale: TPointF;
  LAbsoluteSize: TSize;
  LAbsoluteBimapSize: TSize;
  LMaxBitmapSize: Integer;
  LExceededRatio: Single;
begin
  if (FDrawCacheKind <> TSkDrawCacheKind.Always) and (Canvas is TSkCanvasCustom) then
  begin
    DrawUsingSkia(TSkCanvasCustom(Canvas).Surface, Canvas.AlignToPixel(LocalRect), AbsoluteOpacity);
    FreeAndNil(FBuffer);
  end
  else
  begin
    if Assigned(Scene) then
      LSceneScale := Scene.GetSceneScale
    else
      LSceneScale := 1;
    LAbsoluteScale := AbsoluteScale;
    LAbsoluteSize := TSize.Create(Round(Width * LAbsoluteScale.X * LSceneScale), Round(Height * LAbsoluteScale.Y * LSceneScale));

    LMaxBitmapSize := Canvas.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    if (LAbsoluteSize.Width > LMaxBitmapSize) or (LAbsoluteSize.Height > LMaxBitmapSize) then
    begin
      LAbsoluteBimapSize := RectF(0, 0, LAbsoluteSize.Width, LAbsoluteSize.Height)
        .FitInto(RectF(0, 0, LMaxBitmapSize, LMaxBitmapSize), LExceededRatio).Size.Round;
      if NeedsRedraw or (TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize) then
        Log.d(Format(ControlExceededBitmapLimitWarning, [ClassName, Name]));
    end
    else
    begin
      LAbsoluteBimapSize := LAbsoluteSize;
      LExceededRatio := 1;
    end;

    if NeedsRedraw or (TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize) then
    begin
      if FBuffer = nil then
        FBuffer := TBitmap.Create(LAbsoluteBimapSize.Width, LAbsoluteBimapSize.Height)
      else if TSize.Create(FBuffer.Width, FBuffer.Height) <> LAbsoluteBimapSize then
        FBuffer.SetSize(LAbsoluteBimapSize.Width, LAbsoluteBimapSize.Height);
      FBuffer.SkiaDraw(
        procedure(const ACanvas: ISkCanvas)
        var
          LAbsoluteScale: TPointF;
          LDestRect: TRectF;
        begin
          ACanvas.Clear(TAlphaColors.Null);
          LAbsoluteScale := AbsoluteScale * LSceneScale / LExceededRatio;
          ACanvas.Concat(TMatrix.CreateScaling(LAbsoluteScale.X, LAbsoluteScale.Y));
          LDestRect := RectF(0, 0, LAbsoluteBimapSize.Width / LAbsoluteScale.X, LAbsoluteBimapSize.Height / LAbsoluteScale.Y);
          Draw(ACanvas, LDestRect, 1);
          if Assigned(FOnDraw) then
            FOnDraw(Self, ACanvas, LDestRect, 1);
        end, False);
      FDrawCached := True;
    end;
    Canvas.DrawBitmap(FBuffer, RectF(0, 0, FBuffer.Width, FBuffer.Height), Canvas.AlignToPixel(LocalRect), AbsoluteOpacity);
  end;
end;

procedure TSkStyledControl.Redraw;
begin
  FDrawCached := False;
  Repaint;
end;

procedure TSkStyledControl.SetDrawCacheKind(const AValue: TSkDrawCacheKind);
begin
  if FDrawCacheKind <> AValue then
  begin
    FDrawCacheKind := AValue;
    if AValue <> TSkDrawCacheKind.Always then
      Repaint;
  end;
end;

procedure TSkStyledControl.SetOnDraw(const AValue: TSkDrawEvent);
begin
  if TMethod(FOnDraw) <> TMethod(AValue) then
  begin
    FOnDraw := AValue;
    Redraw;
  end;
end;

{ TSkSvgBrush }

procedure TSkSvgBrush.Assign(ASource: TPersistent);
var
  LSourceSvgBrush: TSkSvgBrush absolute ASource;
begin
  if ASource is TSkSvgBrush then
  begin
    if (FGrayScale <> LSourceSvgBrush.FGrayScale) or
      (FOverrideColor <> LSourceSvgBrush.FOverrideColor) or
      (FWrapMode <> LSourceSvgBrush.FWrapMode) or
      (FSource <> LSourceSvgBrush.FSource) then
    begin
      FDOM := LSourceSvgBrush.FDOM;
      FGrayScale := LSourceSvgBrush.FGrayScale;
      FOriginalSize := LSourceSvgBrush.FOriginalSize;
      FOverrideColor := LSourceSvgBrush.FOverrideColor;
      FSource := LSourceSvgBrush.FSource;
      FWrapMode := LSourceSvgBrush.FWrapMode;
      DoChanged;
    end;
  end
  else
    inherited;
end;

constructor TSkSvgBrush.Create;
begin
  inherited Create;
  FGrayScale := DefaultGrayScale;
  FWrapMode := DefaultWrapMode;
end;

procedure TSkSvgBrush.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TSkSvgBrush.GetDOM: ISkSVGDOM;
var
  LSvgRect: TRectF;
begin
  if (FDOM = nil) and (FSource <> '') then
  begin
    FDOM := TSkSVGDOM.Make(FSource);
    if Assigned(FDOM) then
    begin
      LSvgRect.TopLeft := PointF(0, 0);
      LSvgRect.Size := FDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0));
      if (not LSvgRect.IsEmpty) or (FDOM.Root.TryGetViewBox(LSvgRect) and not LSvgRect.IsEmpty) then
        FOriginalSize := LSvgRect.Size;
    end;
  end;
  Result := FDOM;
end;

function TSkSvgBrush.GetOriginalSize: TSizeF;
begin
  if (FDOM = nil) and (FSource <> '') then
    GetDOM;
  Result := FOriginalSize;
end;

function TSkSvgBrush.IsGrayScaleStored: Boolean;
begin
  Result := FGrayScale <> DefaultGrayScale;
end;

function TSkSvgBrush.IsOverrideColorStored: Boolean;
begin
  Result := FOverrideColor <> Default(TAlphaColor);
end;

function TSkSvgBrush.IsWrapModeStored: Boolean;
begin
  Result := FWrapMode <> DefaultWrapMode;
end;

procedure TSkSvgBrush.Render(const ACanvas: ISkCanvas; const ADestRect: TRectF;
  const AOpacity: Single);

  function GetWrappedDest(const ADOM: ISkSVGDOM; const ASvgRect, ADestRect: TRectF; const AIntrinsicSize: TSizeF): TRectF;
  var
    LRatio: Single;
  begin
    case FWrapMode of
      TSkSvgWrapMode.Default:
        begin
          if AIntrinsicSize.IsZero then
            Result := ADestRect
          else
          begin
            Result := ASvgRect;
            Result.Offset(ADestRect.TopLeft);
          end;
          ADOM.SetContainerSize(ADestRect.Size);
        end;
      TSkSvgWrapMode.Fit: Result := ASvgRect.FitInto(ADestRect);
      TSkSvgWrapMode.FitCrop:
        begin
          if (ASvgRect.Width / ADestRect.Width) < (ASvgRect.Height / ADestRect.Height) then
            LRatio := ASvgRect.Width / ADestRect.Width
          else
            LRatio := ASvgRect.Height / ADestRect.Height;
          if SameValue(LRatio, 0, TEpsilon.Vector) then
            Result := ADestRect
          else
          begin
            Result := RectF(0, 0, Round(ASvgRect.Width / LRatio), Round(ASvgRect.Height / LRatio));
            RectCenter(Result, ADestRect);
          end;
        end;
      TSkSvgWrapMode.Original,
      TSkSvgWrapMode.Tile: Result := ASvgRect;
      TSkSvgWrapMode.OriginalCenter:
        begin
          Result := ASvgRect;
          RectCenter(Result, ADestRect);
        end;
      TSkSvgWrapMode.Place: Result := PlaceIntoTopLeft(ASvgRect, ADestRect);
      TSkSvgWrapMode.Stretch: Result := ADestRect;
    else
      Result := ADestRect;
    end;
  end;

  procedure DrawTileOrCustomColor(const ACanvas: ISkCanvas; const ADOM: ISkSVGDOM;
    const ASvgRect, ADestRect, AWrappedDest: TRectF; const AIntrinsicSize: TSizeF;
    const AWrapMode: TSkSvgWrapMode);
  var
    LPicture: ISkPicture;
    LPictureRecorder: ISkPictureRecorder;
    LCanvas: ISkCanvas;
    LPaint: ISkPaint;
  begin
    LPictureRecorder := TSkPictureRecorder.Create;
    LCanvas := LPictureRecorder.BeginRecording(AWrappedDest.Width, AWrappedDest.Height);
    if AIntrinsicSize.IsZero then
    begin
      if AWrapMode <> TSkSvgWrapMode.Default then
      begin
        ADOM.Root.Width  := TSkSVGLength.Create(AWrappedDest.Width,  TSkSVGLengthUnit.PX);
        ADOM.Root.Height := TSkSVGLength.Create(AWrappedDest.Height, TSkSVGLengthUnit.PX);
      end;
    end
    else
      LCanvas.Scale(AWrappedDest.Width / ASvgRect.Width, AWrappedDest.Height / ASvgRect.Height);
    ADOM.Render(LCanvas);
    LPicture := LPictureRecorder.FinishRecording;
    LPaint := TSkPaint.Create;
    if FGrayScale then
      LPaint.ColorFilter := TSkColorFilter.MakeMatrix(TSkColorMatrix.CreateSaturation(0))
    else if FOverrideColor <> TAlphaColors.Null then
      LPaint.ColorFilter := TSkColorFilter.MakeBlend(FOverrideColor, TSkBlendMode.SrcIn);
    if FWrapMode = TSkSvgWrapMode.Tile then
    begin
      LPaint.Shader := LPicture.MakeShader(TSkTileMode.Repeat, TSkTileMode.Repeat);
      ACanvas.DrawRect(ADestRect, LPaint);
    end
    else
    begin
      ACanvas.Translate(AWrappedDest.Left, AWrappedDest.Top);
      ACanvas.DrawPicture(LPicture, LPaint);
    end;
  end;

var
  LDOM: ISkSVGDOM;
  LSvgRect: TRectF;
  LWrappedDest: TRectF;
  LIntrinsicSize: TSizeF;
begin
  if not ADestRect.IsEmpty then
  begin
    LDOM := DOM;
    if Assigned(LDOM) then
    begin
      LSvgRect.TopLeft := PointF(0, 0);
      LIntrinsicSize := LDOM.Root.GetIntrinsicSize(TSizeF.Create(0, 0));
      LSvgRect.Size := LIntrinsicSize;
      if LSvgRect.IsEmpty and ((not LDOM.Root.TryGetViewBox(LSvgRect)) or LSvgRect.IsEmpty) then
        Exit;

      if SameValue(AOpacity, 1, TEpsilon.Position) then
        ACanvas.Save
      else
        ACanvas.SaveLayerAlpha(Round(AOpacity * 255));
      try
        LWrappedDest := GetWrappedDest(LDOM, LSvgRect, ADestRect, LIntrinsicSize);
        if (FOverrideColor <> TAlphaColors.Null) or (FWrapMode = TSkSvgWrapMode.Tile) or FGrayScale then
          DrawTileOrCustomColor(ACanvas, LDOM, LSvgRect, ADestRect, LWrappedDest, LIntrinsicSize, FWrapMode)
        else
        begin
          ACanvas.Translate(LWrappedDest.Left, LWrappedDest.Top);
          if LIntrinsicSize.IsZero then
          begin
            if FWrapMode <> TSkSvgWrapMode.Default then
            begin
              LDOM.Root.Width  := TSkSVGLength.Create(LWrappedDest.Width,  TSkSVGLengthUnit.PX);
              LDOM.Root.Height := TSkSVGLength.Create(LWrappedDest.Height, TSkSVGLengthUnit.PX);
            end;
          end
          else
            ACanvas.Scale(LWrappedDest.Width / LSvgRect.Width, LWrappedDest.Height / LSvgRect.Height);
          LDOM.Render(ACanvas);
        end;
      finally
        ACanvas.Restore;
      end;
    end;
  end;
end;

procedure TSkSvgBrush.SetGrayScale(const AValue: Boolean);
begin
  if FGrayScale <> AValue then
  begin
    FGrayScale := AValue;
    if FSource <> '' then
      DoChanged;
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
    FOriginalSize := TSizeF.Create(0, 0);
    DoChanged;
  end;
end;

procedure TSkSvgBrush.SetWrapMode(const AValue: TSkSvgWrapMode);
begin
  if FWrapMode <> AValue then
  begin
    FWrapMode := AValue;
    FDOM := nil;
    FOriginalSize := TSizeF.Create(0, 0);
    if FSource <> '' then
      DoChanged;
  end;
end;

{ TSkSvg }

constructor TSkSvg.Create(AOwner: TComponent);
begin
  inherited;
  FSvg := TSkSvgBrush.Create;
  FSvg.OnChanged := SvgChanged;
  DrawCacheKind := TSkDrawCacheKind.Always;
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

type
  { TRepaintAnimation }

  TRepaintAnimation = class(TAnimation)
  protected
    procedure ProcessAnimation; override;
  end;

{ TRepaintAnimation }

procedure TRepaintAnimation.ProcessAnimation;
begin
  TSkCustomAnimatedControl(Parent).ProcessAnimation;
end;

{ TSkCustomAnimatedControl }

procedure TSkCustomAnimatedControl.AncestorVisibleChanged(const AVisible: Boolean);
var
  LLastAbsoluteVisible: Boolean;
begin
  LLastAbsoluteVisible := FAbsoluteVisible;
  if not AVisible then
  begin
    FAbsoluteVisible := False;
    FAbsoluteVisibleCached := True;
  end
  else
    FAbsoluteVisibleCached := False;
  if (not FFixedProgress) and (not FProgressChangedManually) and (not LLastAbsoluteVisible) and AbsoluteVisible then
  begin
    FProgress := 0;
    FAnimationStartTickCount := TThread.GetTickCount;
  end;
  CheckAnimation;
  inherited;
end;

function TSkCustomAnimatedControl.CanRunAnimation: Boolean;
begin
  Result := (not FIsStaticImage) and Assigned(Scene) and (not FFixedProgress) and
    ([csDestroying, csDesigning] * ComponentState = []) and
    AbsoluteVisible and AbsoluteEnabled and
    (AbsoluteWidth > 0) and (AbsoluteHeight > 0) and
    (FLoop or not SameValue(FProgress, 1, TEpsilon.Matrix)) and
    (Scene.GetObject is TCommonCustomForm) and TCommonCustomForm(Scene.GetObject).Visible;
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
        FixStartTickCount;
        DoAnimationStart;
      end
      else
        DoAnimationFinished;
    end;
  end;
end;

constructor TSkCustomAnimatedControl.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    FProgress := 0.5;
    FFixedProgress := True;
  end;
  FIsStaticImage := True;
  FLoop := True;
  FAnimation := TRepaintAnimation.Create(Self);
  FAnimation.Stored := False;
  FAnimation.Loop := True;
  FAnimation.Duration := 30;
  FAnimation.Parent := Self;
  FAbsoluteVisible := Visible;
  FAbsoluteVisibleCached := True;
  if FIsStaticImage then
    DrawCacheKind := TSkDrawCacheKind.Raster
  else
    DrawCacheKind := TSkDrawCacheKind.Never;
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
    LDuration: Double;
    LCurrentTickCount: Cardinal;
  begin
    if Enabled then
    begin
      LCurrentTickCount := TThread.GetTickCount;
      LElapsedSeconds := (LCurrentTickCount - FAnimationStartTickCount) / 1000;
      if LElapsedSeconds < 0 then
        FixElapsedSeconds(LCurrentTickCount, FAnimationStartTickCount, LElapsedSeconds);
      LDuration := Duration;
      if SameValue(LDuration, 0, TEpsilon.Matrix) then
        Result := 1
      else
      begin
        if FLoop then
        begin
          {$IF CompilerVersion >= 29}
          LElapsedSeconds := FMod(LElapsedSeconds, LDuration);
          {$ELSE}
          LElapsedSeconds := (Round(LElapsedSeconds * 1000) mod Round(LDuration * 1000)) / 1000;
          {$ENDIF}
        end
        else
          LElapsedSeconds := Min(LElapsedSeconds, LDuration);
        Result := LElapsedSeconds / LDuration;
      end;
    end
    else
      Result := FProgress;
  end;

var
  LProgress: Double;
begin
  inherited;
  if Assigned(FAnimation) and not FAnimation.Enabled then
    CheckAnimation;
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
  FSuccessRepaint := True;
end;

function TSkCustomAnimatedControl.GetAbsoluteVisible: Boolean;
begin
  if not FAbsoluteVisibleCached then
  begin
    FAbsoluteVisible := GetParentedVisible;
    FAbsoluteVisibleCached := True;
  end;
  Result := FAbsoluteVisible;
end;

function TSkCustomAnimatedControl.GetRunningAnimation: Boolean;
begin
  Result := Assigned(FAnimation) and FAnimation.Enabled;
end;

procedure TSkCustomAnimatedControl.ProcessAnimation;
begin
  if not FSuccessRepaint then
    CheckAnimation;
  FSuccessRepaint := False;
  Repaint;
end;

procedure TSkCustomAnimatedControl.RecalcEnabled;
begin
  inherited;
  CheckAnimation;
end;

procedure TSkCustomAnimatedControl.RenderFrame(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AProgress: Double; const AOpacity: Single);
begin
  if Assigned(FOnAnimationDraw) then
    FOnAnimationDraw(Self, ACanvas, ADest, AProgress, AOpacity);
end;

procedure TSkCustomAnimatedControl.SetFixedProgress(const AValue: Boolean);
begin
  if FFixedProgress <> AValue then
  begin
    FFixedProgress := AValue;
    CheckAnimation;
  end;
end;

procedure TSkCustomAnimatedControl.SetIsStaticImage(const AValue: Boolean);
begin
  if FIsStaticImage <> AValue then
  begin
    FIsStaticImage := AValue;
    if FIsStaticImage then
      DrawCacheKind := TSkDrawCacheKind.Raster
    else
      DrawCacheKind := TSkDrawCacheKind.Never;
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

procedure TSkCustomAnimatedControl.SetNewScene(AScene: IScene);
var
  LCanCheck: Boolean;
begin
  LCanCheck := Scene = nil;
  inherited;
  if LCanCheck then
    CheckAnimation;
end;

procedure TSkCustomAnimatedControl.SetOnAnimationDraw(
  const Value: TSkAnimationDrawEvent);
begin
  FOnAnimationDraw := Value;
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

{ TSkAnimatedPaintBox }

function TSkAnimatedPaintBox.CanRunAnimation: Boolean;
begin
  Result := FAnimate and inherited;
end;

constructor TSkAnimatedPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  FAnimate := True;
  FDuration := DefaultDuration;
  IsStaticImage := not FAnimate;
end;

function TSkAnimatedPaintBox.GetDuration: Double;
begin
  Result := FDuration;
end;

function TSkAnimatedPaintBox.IsDurationStored: Boolean;
begin
  Result := not SameValue(FDuration, DefaultDuration, TEpsilon.Vector);
end;

procedure TSkAnimatedPaintBox.SetAnimate(const AValue: Boolean);
begin
  if FAnimate <> AValue then
  begin
    FAnimate := AValue;
    if FAnimate then
      Progress := 0;
    IsStaticImage := not FAnimate;
    Redraw;
  end;
end;

procedure TSkAnimatedPaintBox.SetDuration(const AValue: Double);
begin
  if not SameValue(FDuration, AValue, TEpsilon.Vector) then
  begin
    FDuration := AValue;
    CheckAnimation;
  end;
end;

{ TSkAnimatedImage.TSource }

procedure TSkAnimatedImage.TSource.Assign(ASource: TPersistent);
begin
  if ASource is TSource then
    Data := TSource(ASource).Data
  else
    inherited;
end;

constructor TSkAnimatedImage.TSource.Create(const AOnChange: TNotifyEvent);
begin
  inherited Create;
  FOnChange := AOnChange;
end;

function TSkAnimatedImage.TSource.Equals(AObject: TObject): Boolean;
begin
  Result := (AObject is TSource) and IsSameBytes(FData, TSource(AObject).Data);
end;

procedure TSkAnimatedImage.TSource.SetData(const AValue: TBytes);
begin
  if not IsSameBytes(FData, AValue) then
  begin
    FData := Copy(AValue);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TSkAnimatedImage.TFormatInfo }

constructor TSkAnimatedImage.TFormatInfo.Create(const AName,
  ADescription: string; const AExtensions: TArray<string>);
begin
  Name := AName;
  Description := ADescription;
  Extensions := AExtensions;
end;

{ TSkAnimatedImage }

function TSkAnimatedImage.CanRunAnimation: Boolean;
begin
  Result := Assigned(FCodec) and inherited;
end;

constructor TSkAnimatedImage.Create(AOwner: TComponent);
begin
  inherited;
  FSource := TSource.Create(SourceChange);
end;

procedure TSkAnimatedImage.DefineProperties(AFiler: TFiler);

  function DoWrite: Boolean;
  begin
    if AFiler.Ancestor <> nil then
      Result := (not (AFiler.Ancestor is TSkAnimatedImage)) or not TSkAnimatedImage(AFiler.Ancestor).Source.Equals(FSource)
    else
      Result := FSource.Data <> nil;
  end;

begin
  inherited;
  AFiler.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

destructor TSkAnimatedImage.Destroy;
begin
  FCodec.Free;
  FSource.Free;
  inherited;
end;

procedure TSkAnimatedImage.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
begin
  if Assigned(FCodec) then
    inherited
  else if csDesigning in ComponentState then
    DrawDesignBorder(ACanvas, ADest, AOpacity);
end;

function TSkAnimatedImage.GetDuration: Double;
begin
  if Assigned(FCodec) then
    Result := FCodec.Duration
  else
    Result := 0;
end;

function TSkAnimatedImage.GetOriginalSize: TSizeF;
begin
  if Assigned(FCodec) then
    Result := FCodec.Size
  else
    Result := TSizeF.Create(0, 0);
end;

procedure TSkAnimatedImage.LoadFromFile(const AFileName: string);
begin
  FSource.Data := TFile.ReadAllBytes(AFileName);
end;

procedure TSkAnimatedImage.LoadFromStream(const AStream: TStream);
var
  LBytes: TBytes;
begin
  SetLength(LBytes, AStream.Size - AStream.Position);
  if Length(LBytes) > 0 then
    AStream.ReadBuffer(LBytes, 0, Length(LBytes));
  FSource.Data := LBytes;
end;

procedure TSkAnimatedImage.ReadData(AStream: TStream);
begin
  if AStream.Size = 0 then
    FSource.Data := nil
  else
    LoadFromStream(AStream);
end;

class procedure TSkAnimatedImage.RegisterCodec(
  const ACodecClass: TAnimationCodecClass);
begin
  FRegisteredCodecs := FRegisteredCodecs + [ACodecClass];
end;

procedure TSkAnimatedImage.RenderFrame(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AProgress: Double; const AOpacity: Single);

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
  FCodec.Quality := Canvas.Quality;
  FCodec.SeekFrameTime(AProgress * Duration);
  FCodec.Render(ACanvas, GetWrappedRect(ADest), AOpacity);
  inherited;
end;

procedure TSkAnimatedImage.SetSource(const AValue: TSource);
begin
  FSource.Assign(AValue);
end;

procedure TSkAnimatedImage.SetWrapMode(const AValue: TSkAnimatedImageWrapMode);
begin
  if FWrapMode <> AValue then
  begin
    FWrapMode := AValue;
    Redraw;
  end;
end;

procedure TSkAnimatedImage.SourceChange(ASender: TObject);
var
  LCodecClass: TAnimationCodecClass;
  LStream: TStream;
begin
  FreeAndNil(FCodec);
  LStream := TBytesStream.Create(FSource.Data);
  try
    for LCodecClass in FRegisteredCodecs do
    begin
      LStream.Position := 0;
      if LCodecClass.TryMakeFromStream(LStream, FCodec) then
        Break;
    end;
  finally
    LStream.Free;
  end;
  if not FixedProgress then
    Progress := 0;
  IsStaticImage := (FCodec = nil) or FCodec.IsStatic;
  Redraw;
end;

procedure TSkAnimatedImage.WriteData(AStream: TStream);
begin
  if FSource.Data <> nil then
    AStream.WriteBuffer(FSource.Data, Length(FSource.Data));
end;

{ TSkDefaultAnimationCodec }

constructor TSkDefaultAnimationCodec.Create(
  const AAnimationCodec: ISkAnimationCodecPlayer; const AStream: TStream);
begin
  inherited Create;
  FAnimationCodec := AAnimationCodec;
  FStream := AStream;
end;

destructor TSkDefaultAnimationCodec.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TSkDefaultAnimationCodec.GetDuration: Double;
begin
  Result := FAnimationCodec.Duration / 1000;
end;

function TSkDefaultAnimationCodec.GetIsStatic: Boolean;
begin
  Result := FAnimationCodec.Duration = 0;
end;

function TSkDefaultAnimationCodec.GetSize: TSizeF;
begin
  Result := FAnimationCodec.Dimensions;
end;

procedure TSkDefaultAnimationCodec.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  LPaint: ISkPaint;
begin
  if SameValue(AOpacity, 1, TEpsilon.Position) then
    LPaint := nil
  else
  begin
    LPaint := TSkPaint.Create;
    LPaint.AlphaF := AOpacity;
  end;
  case Quality of
    TCanvasQuality.SystemDefault: ACanvas.DrawImageRect(FAnimationCodec.Frame, ADest, TSkSamplingOptions.Medium, LPaint);
    TCanvasQuality.HighPerformance: ACanvas.DrawImageRect(FAnimationCodec.Frame, ADest, TSkSamplingOptions.Low, LPaint);
    TCanvasQuality.HighQuality: ACanvas.DrawImageRect(FAnimationCodec.Frame, ADest, TSkSamplingOptions.Medium, LPaint);
  end;
end;

procedure TSkDefaultAnimationCodec.SeekFrameTime(const ATime: Double);
begin
  FAnimationCodec.Seek(Round(ATime * 1000));
end;

class function TSkDefaultAnimationCodec.SupportedFormats: TArray<TSkAnimatedImage.TFormatInfo>;
begin
  SetLength(Result, Ord(High(TImageFormat)) + 1);
  Result[Ord(TImageFormat.GIF)]  := TSkAnimatedImage.TFormatInfo.Create('GIF',  'GIF image',  ['.gif']);
  Result[Ord(TImageFormat.WebP)] := TSkAnimatedImage.TFormatInfo.Create('WebP', 'WebP image', ['.webp']);
end;

class function TSkDefaultAnimationCodec.TryDetectFormat(const ABytes: TBytes;
  out AFormat: TSkAnimatedImage.TFormatInfo): Boolean;

  function IsWebP: Boolean;
  const
    HeaderRiff: array[0..3] of Byte = ($52, $49, $46, $46);
    HeaderWebP: array[0..3] of Byte = ($57, $45, $42, $50);
  begin
    Result := (Length(ABytes) > 12) and
      CompareMem(@HeaderRiff[0], ABytes, Length(HeaderRiff)) and
      CompareMem(@HeaderWebP[0], @ABytes[8], Length(HeaderWebP));
  end;

const
  GIFSignature: array[0..2] of Byte = (71, 73, 70);
begin
  Result := True;
  if (Length(ABytes) >= Length(GIFSignature)) and CompareMem(@GIFSignature[0], ABytes, Length(GIFSignature)) then
    AFormat := SupportedFormats[Ord(TImageFormat.GIF)]
  else if IsWebP then
    AFormat := SupportedFormats[Ord(TImageFormat.WebP)]
  else
    Result := False;
end;

class function TSkDefaultAnimationCodec.TryMakeFromStream(
  const AStream: TStream; out ACodec: TSkAnimatedImage.TAnimationCodec): Boolean;
var
  LAnimationCodec: ISkAnimationCodecPlayer;
  LStream: TMemoryStream;
begin
  Result := False;
  LStream := TMemoryStream.Create;
  try
    LStream.CopyFrom(AStream, 0);
    LStream.Position := 0;
    LAnimationCodec := TSkAnimationCodecPlayer.MakeFromStream(LStream);
    if Assigned(LAnimationCodec) then
    begin
      ACodec := TSkDefaultAnimationCodec.Create(LAnimationCodec, LStream);
      Result := True;
    end
    else
      ACodec := nil;
  finally
    if not Result then
      LStream.Free;
  end;
end;

{ TSkLottieAnimationCodec }

constructor TSkLottieAnimationCodec.Create(const ASkottie: ISkottieAnimation);
begin
  inherited Create;
  FSkottie := ASkottie;
end;

function TSkLottieAnimationCodec.GetDuration: Double;
begin
  Result := FSkottie.Duration;
end;

function TSkLottieAnimationCodec.GetIsStatic: Boolean;
begin
  Result := False;
end;

function TSkLottieAnimationCodec.GetSize: TSizeF;
begin
  Result := FSkottie.Size;
end;

procedure TSkLottieAnimationCodec.Render(const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  LLottieRect: TRectF;
  LNeedSaveLayer: Boolean;
begin
  if ADest.IsEmpty then
    Exit;
  LLottieRect := TRectF.Create(PointF(0, 0), FSkottie.Size).FitInto(ADest);
  if LLottieRect.IsEmpty then
    Exit;
  LNeedSaveLayer := not SameValue(AOpacity, 1, TEpsilon.Position);
  if LNeedSaveLayer then
    ACanvas.SaveLayerAlpha(Round(AOpacity * 255));
  try
    if SameValue(ADest.Width / LLottieRect.Width, ADest.Height / LLottieRect.Height, TEpsilon.Matrix) then
      FSkottie.Render(ACanvas, ADest)
    else
    begin
      if not LNeedSaveLayer then
        ACanvas.Save;
      try
        ACanvas.Scale(ADest.Width / LLottieRect.Width, ADest.Height / LLottieRect.Height);
        ACanvas.Translate((LLottieRect.Width - ADest.Width) / 2, (LLottieRect.Height - ADest.Height) / 2);
        FSkottie.Render(ACanvas, ADest);
      finally
        if not LNeedSaveLayer then
          ACanvas.Restore;
      end;
    end;
  finally
    if LNeedSaveLayer then
      ACanvas.Restore;
  end;
end;

procedure TSkLottieAnimationCodec.SeekFrameTime(const ATime: Double);
begin
  FSkottie.SeekFrameTime(ATime);
end;

class function TSkLottieAnimationCodec.SupportedFormats: TArray<TSkAnimatedImage.TFormatInfo>;
begin
  SetLength(Result, Ord(High(TAnimationFormat)) + 1);
  Result[Ord(TAnimationFormat.Lottie)] := TSkAnimatedImage.TFormatInfo.Create('Lottie', 'Lottie file',      ['.json', '.lottie']);
  Result[Ord(TAnimationFormat.TGS)]    := TSkAnimatedImage.TFormatInfo.Create('TGS',    'Telegram sticker', ['.tgs']);
end;

class function TSkLottieAnimationCodec.TryDetectFormat(const ABytes: TBytes;
  out AFormat: TSkAnimatedImage.TFormatInfo): Boolean;
const
  GZipSignature: Word = $8B1F;
begin
  Result := False;
  if Length(ABytes) > 4 then
  begin
    if PWord(ABytes)^ = GZipSignature then
    begin
      AFormat := SupportedFormats[Ord(TAnimationFormat.TGS)];
      Result := True;
    end
    else if ((ABytes[0] = $7B) and (ABytes[Length(ABytes) - 1] = $7D)) or
        ((PWord(ABytes)^ = $7B00) and (PWord(@ABytes[Length(ABytes) - 2])^ = $7D00)) then
    begin
      AFormat := SupportedFormats[Ord(TAnimationFormat.Lottie)];
      Result := True;
    end;
  end;
end;

class function TSkLottieAnimationCodec.TryMakeFromStream(const AStream: TStream; out ACodec: TSkAnimatedImage.TAnimationCodec): Boolean;

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

  function MakeFromTgsStream(const AStream: TStream): ISkottieAnimation;
  var
    LDecompressionStream: TDecompressionStream;
  begin
    LDecompressionStream := TDecompressionStream.Create(AStream, 31);
    try
      Result := TSkottieAnimation.MakeFromStream(LDecompressionStream);
    finally
      LDecompressionStream.Free;
    end;
  end;

var
  LSkottie: ISkottieAnimation;
begin
  if IsTgs then
    LSkottie := MakeFromTgsStream(AStream)
  else
    LSkottie := TSkottieAnimation.MakeFromStream(AStream);

  Result := Assigned(LSkottie);
  if Result then
    ACodec := TSkLottieAnimationCodec.Create(LSkottie)
  else
    ACodec := nil;
end;

{ TSkPersistentData }

procedure TSkPersistentData.AfterConstruction;
begin
  inherited;
  FCreated := True;
end;

procedure TSkPersistentData.Assign(ASource: TPersistent);
begin
  if ASource <> Self then
  begin
    BeginUpdate;
    try
      DoAssign(ASource);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSkPersistentData.BeginUpdate;
begin
  BeginUpdate(False);
end;

procedure TSkPersistentData.BeginUpdate(const AIgnoreAllChanges: Boolean);
begin
  Inc(FUpdatingCount);
  FIgnoringAllChanges := FIgnoringAllChanges or AIgnoreAllChanges;
end;

procedure TSkPersistentData.Change;
begin
  if FUpdatingCount > 0 then
    FChanged := True
  else
  begin
    FChanged := False;
    DoChanged;
  end;
end;

procedure TSkPersistentData.DoAssign(ASource: TPersistent);
begin
  inherited Assign(ASource);
end;

procedure TSkPersistentData.DoChanged;
begin
  if FCreated and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSkPersistentData.EndUpdate;
begin
  EndUpdate(False);
end;

procedure TSkPersistentData.EndUpdate(const AIgnoreAllChanges: Boolean);
var
  LCallChange: Boolean;
  LIgnoreChanges: Boolean;
begin
  LIgnoreChanges := AIgnoreAllChanges or FIgnoringAllChanges;
  LCallChange := False;
  if FUpdatingCount <= 0 then
    raise ESkPersistentData.Create('The object is not in update state');
  Dec(FUpdatingCount);
  if (not LIgnoreChanges) and HasChanged then
    LCallChange := True
  else
    FChanged := False;
  if FUpdatingCount <= 0 then
    FIgnoringAllChanges := False;
  if LCallChange and (FUpdatingCount = 0) then
  begin
    FChanged := False;
    DoChanged;
  end;
end;

function TSkPersistentData.GetHasChanged: Boolean;
begin
  Result := FChanged;
end;

function TSkPersistentData.GetUpdating: Boolean;
begin
  Result := FUpdatingCount > 0;
end;

procedure TSkPersistentData.SetValue(var AField: Byte; const AValue: Byte);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Word; const AValue: Word);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Double; const AValue,
  AEpsilon: Double);
begin
  if not SameValue(AField, AValue, AEpsilon) then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: TBytes; const AValue: TBytes);
begin
  if not IsSameBytes(AField, AValue) then
  begin
    AField := Copy(AValue);
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: string; const AValue: string);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Single; const AValue,
  AEpsilon: Single);
begin
  if not SameValue(AField, AValue, AEpsilon) then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Boolean;
  const AValue: Boolean);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Cardinal;
  const AValue: Cardinal);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Integer;
  const AValue: Integer);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue(var AField: Int64; const AValue: Int64);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Change;
  end;
end;

procedure TSkPersistentData.SetValue<T>(var AField: T; const AValue: T);
var
  LDifferent: Boolean;
begin
  if Assigned(TypeInfo(T)) and (PTypeInfo(TypeInfo(T)).Kind in [TTypeKind.tkSet, TTypeKind.tkEnumeration, TTypeKind.tkRecord{$IF CompilerVersion >= 33}, TTypeKind.tkMRecord{$ENDIF}]) then
    LDifferent := not CompareMem(@AField, @AValue, SizeOf(T))
  else
    LDifferent := TComparer<T>.Default.Compare(AField, AValue) <> 0;
  if LDifferent then
  begin
    AField := AValue;
    Change;
  end;
end;

{ TSkFontComponent }

procedure TSkFontComponent.AssignTo(ADest: TPersistent);
var
  LDestFont: TFont absolute ADest;
  {$IF CompilerVersion < 31}
  LStyle: TFontStyles;
  {$ENDIF}
begin
  if ADest is TFont then
  begin
    LDestFont.Family := Families;
    LDestFont.Size := Size;
    {$IF CompilerVersion >= 31}
    LDestFont.StyleExt := TFontStyleExt.Create(Weight, Slant, Stretch);
    {$ELSE}
    LStyle := [];
    if SkFontWeightValue[Weight] >= 600 then
      LStyle := LStyle + [TFontStyle.fsBold];
    if Slant <> TFontSlant.Regular then
      LStyle := LStyle + [TFontStyle.fsItalic];
    LDestFont.Style := LStyle;
    {$ENDIF}
  end
  else
    inherited;
end;

constructor TSkFontComponent.Create;
begin
  inherited Create;
  Assign(nil);
end;

procedure TSkFontComponent.DoAssign(ASource: TPersistent);
var
  LSourceFont: TSkFontComponent absolute ASource;
begin
  if ASource = nil then
  begin
    Families := DefaultFamilies;
    Size     := DefaultSize;
    Slant    := DefaultSlant;
    Stretch  := DefaultStretch;
    Weight   := DefaultWeight;
  end
  else if ASource is TSkFontComponent then
  begin
    Families := LSourceFont.Families;
    Size     := LSourceFont.Size;
    Slant    := LSourceFont.Slant;
    Stretch  := LSourceFont.Stretch;
    Weight   := LSourceFont.Weight;
  end
  else if ASource is TFont then
  begin
    Families := TFont(ASource).Family;
    Size     := TFont(ASource).Size;
    {$IF CompilerVersion >= 31}
    Slant    := TFont(ASource).StyleExt.Slant;
    Stretch  := TFont(ASource).StyleExt.Stretch;
    Weight   := TFont(ASource).StyleExt.Weight;
    {$ELSE}
    if TFontStyle.fsItalic in TFont(ASource).Style then
      Slant  := TFontSlant.Italic
    else
      Slant  := TFontSlant.Regular;
    Stretch  := TFontStretch.Regular;
    if TFontStyle.fsBold in TFont(ASource).Style then
      Weight := TFontWeight.Bold
    else
      Weight := TFontWeight.Regular;
    {$ENDIF}
  end
  else
    inherited;
end;

function TSkFontComponent.Equals(AObject: TObject): Boolean;
var
  LFont: TSkFontComponent absolute AObject;
begin
  Result := (AObject is TSkFontComponent) and
    (FFamilies = LFont.Families) and
    (FSize     = LFont.Size) and
    (FSlant    = LFont.Slant) and
    (FStretch  = LFont.Stretch) and
    (FWeight   = LFont.Weight);
end;

function TSkFontComponent.IsFamiliesStored: Boolean;
begin
  Result := FFamilies <> DefaultFamilies;
end;

function TSkFontComponent.IsSizeStored: Boolean;
begin
  Result := not SameValue(FSize, DefaultSize, TEpsilon.FontSize);
end;

procedure TSkFontComponent.SetFamilies(const AValue: string);

  function NormalizeFamilies(const AValue: string): string;
  var
    LSplitted: TArray<string>;
    LFamilies: TArray<string>;
    I: Integer;
  begin
    LSplitted := AValue.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
    LFamilies := [];
    for I := 0 to Length(LSplitted) - 1 do
    begin
      LSplitted[I] := LSplitted[I].Trim;
      if LSplitted[I] <> '' then
        LFamilies := LFamilies + [LSplitted[I]];
    end;
    if LFamilies = nil then
      Exit('');
    Result := string.Join(', ', LFamilies);
  end;

begin
  SetValue(FFamilies, NormalizeFamilies(AValue));
end;

procedure TSkFontComponent.SetSize(const AValue: Single);
begin
  SetValue(FSize, AValue);
end;

procedure TSkFontComponent.SetSlant(const AValue: TFontSlant);
begin
  SetValue<TFontSlant>(FSlant, AValue);
end;

procedure TSkFontComponent.SetStretch(const AValue: TFontStretch);
begin
  SetValue<TFontStretch>(FStretch, AValue);
end;

procedure TSkFontComponent.SetWeight(const AValue: TFontWeight);
begin
  SetValue<TFontWeight>(FWeight, AValue);
end;

{ TSkTextSettings.TDecorations }

constructor TSkTextSettings.TDecorations.Create;
begin
  inherited Create;
  Assign(nil);
end;

procedure TSkTextSettings.TDecorations.DoAssign(ASource: TPersistent);
var
  LSourceDecorations: TDecorations absolute ASource;
begin
  if ASource = nil then
  begin
    Color               := DefaultColor;
    Decorations         := DefaultDecorations;
    Style               := DefaultStyle;
    Thickness := DefaultThickness;
  end
  else if ASource is TDecorations then
  begin
    Color               := LSourceDecorations.Color;
    Decorations         := LSourceDecorations.Decorations;
    Style               := LSourceDecorations.Style;
    Thickness := LSourceDecorations.Thickness;
  end
  else
    inherited;
end;

function TSkTextSettings.TDecorations.Equals(AObject: TObject): Boolean;
var
  LDecorations: TDecorations absolute AObject;
begin
  Result := (AObject is TDecorations) and
    (Color               = LDecorations.Color) and
    (Decorations         = LDecorations.Decorations) and
    (Style               = LDecorations.Style) and
    (Thickness = LDecorations.Thickness);
end;

function TSkTextSettings.TDecorations.IsThicknessStored: Boolean;
begin
  Result := not SameValue(FThickness, DefaultThickness, TEpsilon.Scale);
end;

procedure TSkTextSettings.TDecorations.SetColor(const AValue: TAlphaColor);
begin
  SetValue<TAlphaColor>(FColor, AValue);
end;

procedure TSkTextSettings.TDecorations.SetDecorations(
  const AValue: TSkTextDecorations);
begin
  SetValue<TSkTextDecorations>(FDecorations, AValue);
end;

procedure TSkTextSettings.TDecorations.SetStyle(
  const AValue: TSkTextDecorationStyle);
begin
  SetValue<TSkTextDecorationStyle>(FStyle, AValue);
end;

procedure TSkTextSettings.TDecorations.SetThickness(
  const AValue: Single);
begin
  SetValue(FThickness, AValue);
end;

{ TSkTextSettings }

procedure TSkTextSettings.AssignNotStyled(const ATextSettings: TSkTextSettings;
  const AStyledSettings: TStyledSettings);
var
  LTextSettings: TSkTextSettings;
begin
  if AStyledSettings <> AllStyledSettings then
  begin
    if AStyledSettings = [] then
      Assign(ATextSettings)
    else
    begin
      if ATextSettings = nil then
        LTextSettings := TSkTextSettingsClass(ClassType).Create(Owner)
      else
        LTextSettings := ATextSettings;
      try
        BeginUpdate;
        try
          DoAssignNotStyled(LTextSettings, AStyledSettings);
        finally
          EndUpdate;
        end;
      finally
        if ATextSettings = nil then
          LTextSettings.Free;
      end;
    end;
  end;
end;

constructor TSkTextSettings.Create(const AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TSkFontComponent.Create;
  FFont.OnChange := FontChanged;
  FDecorations := TDecorations.Create;
  FDecorations.OnChange := DecorationsChange;
  Assign(nil);
end;

procedure TSkTextSettings.DecorationsChange(ASender: TObject);
begin
  Change;
end;

destructor TSkTextSettings.Destroy;
begin
  FDecorations.Free;
  FFont.Free;
  inherited;
end;

procedure TSkTextSettings.DoAssign(ASource: TPersistent);
var
  LSourceTextSettings: TSkTextSettings absolute ASource;
begin
  if ASource = nil then
  begin
    Decorations := nil;
    Font        := nil;
    FontColor   := DefaultFontColor;
    HorzAlign   := DefaultHorzAlign;
    MaxLines    := DefaultMaxLines;
    Trimming    := DefaultTrimming;
    VertAlign   := DefaultVertAlign;
  end
  else if ASource is TSkTextSettings then
  begin
    Decorations := LSourceTextSettings.Decorations;
    Font        := LSourceTextSettings.Font;
    FontColor   := LSourceTextSettings.FontColor;
    HorzAlign   := LSourceTextSettings.HorzAlign;
    MaxLines    := LSourceTextSettings.MaxLines;
    Trimming    := LSourceTextSettings.Trimming;
    VertAlign   := LSourceTextSettings.VertAlign;
  end
  else
    inherited;
end;

procedure TSkTextSettings.DoAssignNotStyled(
  const ATextSettings: TSkTextSettings; const AStyledSettings: TStyledSettings);
begin
  Font.BeginUpdate;
  try
    if not (TStyledSetting.Family in AStyledSettings) then
      Font.Families := ATextSettings.Font.Families;
    if not (TStyledSetting.Size in AStyledSettings) then
      Font.Size := ATextSettings.Font.Size;
    if not (TStyledSetting.Style in AStyledSettings) then
    begin
      Font.Slant   := ATextSettings.Font.Slant;
      Font.Stretch := ATextSettings.Font.Stretch;
      Font.Weight  := ATextSettings.Font.Weight;
    end;
  finally
    Font.EndUpdate;
  end;
  if not (TStyledSetting.FontColor in AStyledSettings) then
    FontColor := ATextSettings.FontColor;
  if not (TStyledSetting.Other in AStyledSettings) then
  begin
    Decorations := ATextSettings.Decorations;
    HorzAlign   := ATextSettings.HorzAlign;
    VertAlign   := ATextSettings.VertAlign;
    MaxLines    := ATextSettings.MaxLines;
    Trimming    := ATextSettings.Trimming;
  end;
end;

function TSkTextSettings.Equals(AObject: TObject): Boolean;
var
  LTextSettings: TSkTextSettings absolute AObject;
begin
  Result := (AObject is TSkTextSettings) and
    FDecorations.Equals(LTextSettings.Decorations) and
    FFont.Equals(LTextSettings.Font) and
    (FFontColor = LTextSettings.FontColor) and
    (FHorzAlign = LTextSettings.HorzAlign) and
    (FMaxLines  = LTextSettings.MaxLines) and
    (FTrimming  = LTextSettings.Trimming) and
    (FVertAlign = LTextSettings.VertAlign);
end;

procedure TSkTextSettings.FontChanged(ASender: TObject);
begin
  Change;
end;

procedure TSkTextSettings.SetDecorations(const AValue: TDecorations);
begin
  FDecorations.Assign(AValue);
end;

procedure TSkTextSettings.SetFont(const AValue: TSkFontComponent);
begin
  FFont.Assign(AValue);
end;

procedure TSkTextSettings.SetFontColor(const AValue: TAlphaColor);
begin
  SetValue<TAlphaColor>(FFontColor, AValue);
end;

procedure TSkTextSettings.SetHorzAlign(const AValue: TSkTextHorzAlign);
begin
  SetValue<TSkTextHorzAlign>(FHorzAlign, AValue);
end;

procedure TSkTextSettings.SetMaxLines(const AValue: NativeUInt);
begin
  SetValue<NativeUInt>(FMaxLines, AValue);
end;

procedure TSkTextSettings.SetTrimming(const AValue: TTextTrimming);
begin
  SetValue<TTextTrimming>(FTrimming, AValue);
end;

procedure TSkTextSettings.SetVertAlign(const AValue: TTextAlign);
begin
  SetValue<TTextAlign>(FVertAlign, AValue);
end;

procedure TSkTextSettings.UpdateStyledSettings(const AOldTextSettings,
  ADefaultTextSettings: TSkTextSettings; var AStyledSettings: TStyledSettings);
begin
  // If the user changed the value of the property, and it differs from the default,
  // then delete the corresponding value from AStyledSettings
  if (not SameText(AOldTextSettings.Font.Families, Font.Families)) and
    (not SameText(ADefaultTextSettings.Font.Families, Font.Families)) then
  begin
    Exclude(AStyledSettings, TStyledSetting.Family);
  end;

  if (not SameValue(AOldTextSettings.Font.Size, Font.Size, TEpsilon.FontSize)) and
    (not SameValue(ADefaultTextSettings.Font.Size, Font.Size, TEpsilon.FontSize)) then
  begin
    Exclude(AStyledSettings, TStyledSetting.Size);
  end;

  if ((AOldTextSettings.Font.Slant <> Font.Slant) or (AOldTextSettings.Font.Stretch <> Font.Stretch) or
    (AOldTextSettings.Font.Weight <> Font.Weight)) and
    ((ADefaultTextSettings.Font.Slant <> Font.Slant) or (ADefaultTextSettings.Font.Stretch <> Font.Stretch) or
    (ADefaultTextSettings.Font.Weight <> Font.Weight)) then
  begin
    Exclude(AStyledSettings, TStyledSetting.Style);
  end;

  if ((AOldTextSettings.FontColor <> FontColor) and (ADefaultTextSettings.FontColor <> FontColor)) then
    Exclude(AStyledSettings, TStyledSetting.FontColor);

  if ((not AOldTextSettings.Decorations.Equals(Decorations)) or
    (AOldTextSettings.HorzAlign <> HorzAlign) or (AOldTextSettings.VertAlign <> VertAlign) or
    (AOldTextSettings.Trimming <> Trimming) or (AOldTextSettings.MaxLines <> MaxLines)) and
    ((not ADefaultTextSettings.Decorations.Equals(Decorations)) or
    (ADefaultTextSettings.HorzAlign <> HorzAlign) or (ADefaultTextSettings.VertAlign <> VertAlign) or
    (ADefaultTextSettings.Trimming <> Trimming) or (ADefaultTextSettings.MaxLines <> MaxLines)) then
  begin
    Exclude(AStyledSettings, TStyledSetting.Other);
  end;
end;

{ TSkTextSettingsInfo.TBaseTextSettings }

constructor TSkTextSettingsInfo.TBaseTextSettings.Create(
  const AOwner: TPersistent);
begin
  inherited;
  if AOwner is TSkTextSettingsInfo then
  begin
    FInfo := TSkTextSettingsInfo(AOwner);
    if FInfo.Owner is TControl then
      FControl := TControl(FInfo.Owner);
  end;
end;

{ TSkTextSettingsInfo.TCustomTextSettings }

constructor TSkTextSettingsInfo.TCustomTextSettings.Create(
  const AOwner: TPersistent);
begin
  inherited;
  MaxLines := 0;
end;

{ TSkTextSettingsInfo }

constructor TSkTextSettingsInfo.Create(AOwner: TPersistent;
  ATextSettingsClass: TSkTextSettingsInfo.TCustomTextSettingsClass);
var
  LClass: TSkTextSettingsInfo.TCustomTextSettingsClass;
begin
  if AOwner = nil then
    raise ESkTextSettingsInfo.Create(SArgumentNil);
  inherited Create;
  FOwner := AOwner;
  FStyledSettings := DefaultStyledSettings;
  if ATextSettingsClass = nil then
    LClass := TCustomTextSettings
  else
    LClass := ATextSettingsClass;

  FDefaultTextSettings := LClass.Create(Self);
  FDefaultTextSettings.OnChange := OnDefaultChanged;
  FTextSettings := LClass.Create(Self);
  FTextSettings.OnChange := OnTextChanged;
  FResultingTextSettings := LClass.Create(Self);
  FResultingTextSettings.OnChange := OnCalculatedTextSettings;
  FOldTextSettings := LClass.Create(Self);
  FOldTextSettings.Assign(FTextSettings);
end;

destructor TSkTextSettingsInfo.Destroy;
begin
  FDefaultTextSettings.Free;
  FTextSettings.Free;
  FResultingTextSettings.Free;
  FOldTextSettings.Free;
  inherited;
end;

procedure TSkTextSettingsInfo.DoCalculatedTextSettings;
begin
end;

procedure TSkTextSettingsInfo.DoDefaultChanged;
begin
  RecalculateTextSettings;
end;

procedure TSkTextSettingsInfo.DoStyledSettingsChanged;
begin
  RecalculateTextSettings;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSkTextSettingsInfo.DoTextChanged;
var
  LDesign: Boolean;
begin
  try
    LDesign := Design and (not (Owner is TComponent) or
      (TComponent(Owner).ComponentState * [csLoading, csDestroying, csReading] = []));
    if LDesign then
      TextSettings.UpdateStyledSettings(FOldTextSettings, DefaultTextSettings, FStyledSettings);
    RecalculateTextSettings;
  finally
    if FOldTextSettings <> nil then
      FOldTextSettings.Assign(FTextSettings);
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSkTextSettingsInfo.OnCalculatedTextSettings(ASender: TObject);
begin
  DoCalculatedTextSettings;
end;

procedure TSkTextSettingsInfo.OnDefaultChanged(ASender: TObject);
begin
  DoDefaultChanged;
end;

procedure TSkTextSettingsInfo.OnTextChanged(ASender: TObject);
begin
  DoTextChanged;
end;

procedure TSkTextSettingsInfo.RecalculateTextSettings;
var
  TmpResultingTextSettings: TSkTextSettings;
begin
  if ResultingTextSettings <> nil then
  begin
    TmpResultingTextSettings := TSkTextSettingsClass(TextSettings.ClassType).Create(Self);
    try
      TmpResultingTextSettings.Assign(DefaultTextSettings);
      TmpResultingTextSettings.AssignNotStyled(TextSettings, StyledSettings);
      ResultingTextSettings.Assign(TmpResultingTextSettings);
    finally
      TmpResultingTextSettings.Free;
    end;
  end;
end;

procedure TSkTextSettingsInfo.SetDefaultTextSettings(
  const AValue: TSkTextSettings);
begin
  FDefaultTextSettings.Assign(AValue);
end;

procedure TSkTextSettingsInfo.SetStyledSettings(const AValue: TStyledSettings);
begin
  if FStyledSettings <> AValue then
  begin
    FStyledSettings := AValue;
    DoStyledSettingsChanged;
  end;
end;

procedure TSkTextSettingsInfo.SetTextSettings(const AValue: TSkTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{ TSkStyleTextObject }

constructor TSkStyleTextObject.Create(AOwner: TComponent);
begin
  inherited;
  FTextSettings := TSkTextSettings.Create(Self);
  FTextSettings.OnChange := TextSettingsChange;
end;

destructor TSkStyleTextObject.Destroy;
begin
  FTextSettings.Free;
  FSavedTextSettings.Free;
  inherited;
end;

function TSkStyleTextObject.GetTextSettings: TSkTextSettings;
begin
  Result := FTextSettings;
end;

function TSkStyleTextObject.RestoreState: Boolean;
begin
  Result := False;
  if (FSavedTextSettings <> nil) and (FTextSettings <> nil) then
  begin
    TextSettings := FSavedTextSettings;
    FreeAndNil(FSavedTextSettings);
    Result := True;
  end;
end;

function TSkStyleTextObject.SaveState: Boolean;
begin
  Result := False;
  if FTextSettings <> nil then
  begin
    if FSavedTextSettings = nil then
      FSavedTextSettings := TSkTextSettingsClass(FTextSettings.ClassType).Create(nil);
    FSavedTextSettings.Assign(FTextSettings);
    Result := True;
  end;
end;

procedure TSkStyleTextObject.SetName(const ANewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

procedure TSkStyleTextObject.SetTextSettings(const AValue: TSkTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

procedure TSkStyleTextObject.TextSettingsChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{$IF CompilerVersion < 29}
procedure ValidateInheritance(const AValue: TPersistent; const AClass: TClass; const CanBeNil: Boolean = True);
const
  SEUseHeirs = 'You can use only the inheritors of class "%s"';
begin
  if (AClass <> nil) and (CanBeNil or (AValue <> nil)) then
  begin
    if (AValue <> nil) and not AValue.InheritsFrom(AClass) then
      raise EArgumentException.CreateFMT(SEUseHeirs, [AClass.ClassName])at ReturnAddress;
  end
  else
    raise EArgumentNilException.Create(SArgumentNil)at ReturnAddress;
end;
{$ENDIF}

{ TSkLabel.TCustomWordsItem }

procedure TSkLabel.TCustomWordsItem.Assign(ASource: TPersistent);
begin
  if ASource <> Self then
  begin
    BeginUpdate;
    try
      DoAssign(ASource);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSkLabel.TCustomWordsItem.BeginUpdate;
begin
  Inc(FUpdatingCount);
end;

procedure TSkLabel.TCustomWordsItem.Change;
begin
  if FUpdatingCount > 0 then
    FChanged := True
  else
  begin
    FChanged := False;
    DoChanged;
  end;
end;

procedure TSkLabel.TCustomWordsItem.CheckName(const AName: string;
  AWordsCollection: TWordsCollection);
var
  I: Integer;
begin
  if AName.Trim.IsEmpty then
    raise ESkLabel.CreateFmt(SInvalidName, [AName]);
  if AWordsCollection <> nil then
    for I := 0 to AWordsCollection.Count - 1 do
      if (AWordsCollection.Items[I] <> Self) and (string.Compare(AName, AWordsCollection.Items[I].Name, True) = 0) then
        raise ESkLabel.CreateFmt(SDuplicateName, [AWordsCollection.Items[I].Name]);
end;

constructor TSkLabel.TCustomWordsItem.Create(ACollection: TCollection);
var
  LTextSettingsInfoOwner: TPersistent;
begin
  inherited;
  if (ACollection is TWordsCollection) and Assigned(TWordsCollection(ACollection).&Label) then
    LTextSettingsInfoOwner := TWordsCollection(ACollection).&Label
  else
    LTextSettingsInfoOwner := Self;
  FTextSettingsInfo := TSkTextSettingsInfo.Create(LTextSettingsInfoOwner, nil);
  if LTextSettingsInfoOwner is TSkLabel then
    FTextSettingsInfo.Design := True;
  Assign(nil);
  FTextSettingsInfo.OnChange := TextSettingsChange;
end;

destructor TSkLabel.TCustomWordsItem.Destroy;
begin
  FTextSettingsInfo.Free;
  inherited;
end;

procedure TSkLabel.TCustomWordsItem.DoAssign(ASource: TPersistent);
var
  LSourceItem: TCustomWordsItem absolute ASource;
begin
  if ASource = nil then
  begin
    BackgroundColor := DefaultBackgroundColor;
    Cursor          := DefaultCursor;
    Font            := nil;
    FontColor       := DefaultFontColor;
    Name            := UniqueName(DefaultName, Collection);
    StyledSettings  := DefaultStyledSettings;
    Text            := DefaultText;
    OnClick         := nil;
  end
  else if ASource is TCustomWordsItem then
  begin
    BackgroundColor := LSourceItem.BackgroundColor;
    Cursor          := LSourceItem.Cursor;
    Font            := LSourceItem.Font;
    FontColor       := LSourceItem.FontColor;
    Name            := UniqueName(LSourceItem.Name, Collection);
    StyledSettings  := LSourceItem.StyledSettings;
    Text            := LSourceItem.Text;
    OnClick         := LSourceItem.OnClick;
  end
  else
    inherited Assign(ASource);
end;

procedure TSkLabel.TCustomWordsItem.DoChanged;
begin
  Changed(False);
end;

procedure TSkLabel.TCustomWordsItem.EndUpdate;
begin
  EndUpdate(False);
end;

procedure TSkLabel.TCustomWordsItem.EndUpdate(const AIgnoreAllChanges: Boolean);
var
  LCallChange: Boolean;
  LIgnoreChanges: Boolean;
begin
  LIgnoreChanges := AIgnoreAllChanges or FIgnoringAllChanges;
  LCallChange := False;
  if FUpdatingCount <= 0 then
    raise ESkLabel.Create('The object is not in update state');
  Dec(FUpdatingCount);
  if (not LIgnoreChanges) and FChanged then
    LCallChange := True
  else
    FChanged := False;
  if FUpdatingCount <= 0 then
    FIgnoringAllChanges := False;
  if LCallChange and (FUpdatingCount = 0) then
  begin
    FChanged := False;
    DoChanged;
  end;
end;

function TSkLabel.TCustomWordsItem.GetDecorations: TSkTextSettings.TDecorations;
begin
  Result := FTextSettingsInfo.TextSettings.Decorations;
end;

function TSkLabel.TCustomWordsItem.GetDisplayName: string;
begin
  Result := Name;
end;

function TSkLabel.TCustomWordsItem.GetFont: TSkFontComponent;
begin
  Result := FTextSettingsInfo.TextSettings.Font;
end;

function TSkLabel.TCustomWordsItem.GetFontColor: TAlphaColor;
begin
  Result := FTextSettingsInfo.TextSettings.FontColor;
end;

function TSkLabel.TCustomWordsItem.GetStyledSettings: TStyledSettings;
begin
  Result := FTextSettingsInfo.StyledSettings;
end;

function TSkLabel.TCustomWordsItem.IsFontColorStored: Boolean;
begin
  Result := (FontColor <> DefaultFontColor) or not (TStyledSetting.FontColor in StyledSettings);
end;

function TSkLabel.TCustomWordsItem.IsNameStored: Boolean;
begin
  Result := (Assigned(Collection) and (Collection.Count <> 1)) or (Name <> 'Item 0');
end;

function TSkLabel.TCustomWordsItem.IsStyledSettingsStored: Boolean;
begin
  Result := StyledSettings <> DefaultStyledSettings;
end;

function TSkLabel.TCustomWordsItem.IsTextStored: Boolean;
begin
  Result := Text <> DefaultText;
end;

procedure TSkLabel.TCustomWordsItem.SetBackgroundColor(
  const AValue: TAlphaColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Change;
  end;
end;

procedure TSkLabel.TCustomWordsItem.SetCollection(AValue: TCollection);
var
  S: string;
begin
  ValidateInheritance(AValue, TWordsCollection);
  S := UniqueName(FName, AValue);
  if string.Compare(S, FName, True) <> 0 then
    CheckName(S, TWordsCollection(AValue));
  FName := S;
  inherited;
  FWords := TWordsCollection(Collection);
end;

procedure TSkLabel.TCustomWordsItem.SetCursor(const AValue: TCursor);
begin
  if FCursor <> AValue then
  begin
    FCursor := AValue;
    Change;
  end;
end;

procedure TSkLabel.TCustomWordsItem.SetDecorations(
  const AValue: TSkTextSettings.TDecorations);
begin
  FTextSettingsInfo.TextSettings.Decorations.Assign(AValue);
end;

procedure TSkLabel.TCustomWordsItem.SetFont(const AValue: TSkFontComponent);
begin
  FTextSettingsInfo.TextSettings.Font.Assign(AValue);
end;

procedure TSkLabel.TCustomWordsItem.SetFontColor(const AValue: TAlphaColor);
begin
  FTextSettingsInfo.TextSettings.FontColor := AValue;
end;

procedure TSkLabel.TCustomWordsItem.SetName(const AValue: string);
var
  LValue: string;
begin
  LValue := AValue.Trim;
  if FName <> LValue then
  begin
    if string.Compare(LValue, FName, True) <> 0 then
      CheckName(LValue, Words);
    FName := LValue;
    Change;
  end;
end;

procedure TSkLabel.TCustomWordsItem.SetStyledSettings(
  const AValue: TStyledSettings);
begin
  FTextSettingsInfo.StyledSettings := AValue;
end;

procedure TSkLabel.TCustomWordsItem.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Change;
  end;
end;

procedure TSkLabel.TCustomWordsItem.TextSettingsChange(ASender: TObject);
begin
  Change;
end;

function TSkLabel.TCustomWordsItem.UniqueName(const AName: string;
  const ACollection: TCollection): string;
var
  S: string;
  I, J, LIndex, LMaxIndex: Integer;
  LFound: Boolean;
  LOriginalName: string;

  procedure SeparateNameIndex(var S: string; var AIndex: Integer);
  var
    I, N: integer;
  begin
    AIndex := -1;
    I := S.Length - 1;
    N := 0;
    while (N <= 9) and (I >= 0) and S.Chars[I].IsDigit do
    begin
      Dec(I);
      Inc(N);
    end;
    if (I >= 0) and InRange(N, 1, 5) then
    begin
      AIndex := S.Substring(I + 1).ToInteger;
      S := S.Substring(0, I + 1);
    end;
  end;

begin
  LOriginalName := AName.Trim;
  Result := LOriginalName;
  if ACollection <> nil then
  begin
    SeparateNameIndex(Result, LIndex);
    LMaxIndex := -1;
    LFound := False;
    for I := 0 to ACollection.Count - 1 do
      if (ACollection.Items[I] <> Self) and (ACollection.Items[I] is TCustomWordsItem) then
      begin
        S := TCustomWordsItem(ACollection.Items[I]).Name;
        SeparateNameIndex(S, J);
        if string.Compare(S, Result, True) = 0 then
        begin
          LMaxIndex := Max(LMaxIndex, J);
          if (LIndex = J) then
            LFound := True;
        end;
      end;
    if LFound then
    begin
      LMaxIndex := Max(LMaxIndex + 1, 1);
      Result := Result + LMaxIndex.ToString;
    end
    else
      Result := LOriginalName;
  end;
end;

{ TSkLabel.TWordsCollection }

function TSkLabel.TWordsCollection.Add: TCustomWordsItem;
begin
  Result := TCustomWordsItem(inherited Add);
end;

function TSkLabel.TWordsCollection.Add(const AText: string;
  const AColor: TAlphaColor; const AFontSize: Single;
  const AFontWeight: TFontWeight;
  const AFontSlant: TFontSlant): TCustomWordsItem;
begin
  Result := Add;
  Result.BeginUpdate;
  try
    Result.Text := AText;
    Result.Font.BeginUpdate;
    try
      Result.Font.Size := AFontSize;
      Result.Font.Weight := AFontWeight;
      Result.Font.Slant := AFontSlant;
    finally
      Result.Font.EndUpdate;
    end;
    Result.FontColor := AColor;
  finally
    Result.EndUpdate;
  end;
end;

function TSkLabel.TWordsCollection.AddOrSet(const AName, AText: string;
  const AFontColor: TAlphaColor; const AFont: TSkFontComponent;
  const AOnClick: TNotifyEvent; const ACursor: TCursor): TCustomWordsItem;
begin
  Result := ItemByName[AName];
  if not Assigned(Result) then
    Result := Add;
  Result.BeginUpdate;
  try
    if not AName.IsEmpty then
      Result.Name := AName;
    Result.Font := AFont;
    Result.FontColor := AFontColor;
    Result.Text := AText;
    Result.OnClick := AOnClick;
    Result.Cursor := ACursor;
  finally
    Result.EndUpdate;
  end;
end;

constructor TSkLabel.TWordsCollection.Create(AOwner: TPersistent;
  AItemClass: TCustomWordsItemClass);
begin
  ValidateInheritance(AOwner, TSkLabel, False);
  inherited Create(AOwner, AItemClass);
  FLabel := TSkLabel(AOwner);
end;

function TSkLabel.TWordsCollection.GetItem(AIndex: Integer): TCustomWordsItem;
begin
  Result := TCustomWordsItem(inherited GetItem(AIndex));
end;

function TSkLabel.TWordsCollection.GetItemByName(
  const AName: string): TCustomWordsItem;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AName);
  if LIndex = -1 then
    Result := nil
  else
    Result := Items[LIndex];
end;

function TSkLabel.TWordsCollection.IndexOf(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if string.Compare(AName, Items[I].Name, True) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function TSkLabel.TWordsCollection.Insert(AIndex: Integer): TCustomWordsItem;
begin
  Result := TCustomWordsItem(inherited Insert(AIndex));
end;

procedure TSkLabel.TWordsCollection.SetItem(AIndex: Integer;
  const AValue: TCustomWordsItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TSkLabel.TWordsCollection.Update(AItem: TCollectionItem);
begin
  inherited;
  if (FLabel <> nil) and not (csDestroying in FLabel.ComponentState) then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{ TSkLabel }

procedure TSkLabel.ApplyStyle;
var
  LFontBehavior: IFontBehavior;

  procedure SetupDefaultTextSetting(const AObject: TFmxObject;
    const ADefaultTextSettings: TSkTextSettings);
  var
    LNewFamily: string;
    LNewSize: Single;
  begin
    if (AObject <> nil) and AObject.GetInterface(IObjectState, FObjectState) then
      FObjectState.SaveState
    else
      FObjectState := nil;

    FStyleText := nil;
    if ADefaultTextSettings <> nil then
    begin
      if (AObject <> nil) and Supports(AObject, ISkStyleTextObject, FStyleText) then
        ADefaultTextSettings.Assign(FStyleText.TextSettings)
      else
        ADefaultTextSettings.Assign(nil);

      if LFontBehavior <> nil then
      begin
        LNewFamily := '';
        LFontBehavior.GetDefaultFontFamily(Scene.GetObject, LNewFamily);
        if not LNewFamily.IsEmpty then
          ADefaultTextSettings.Font.Families := LNewFamily;

        LNewSize := 0;
        LFontBehavior.GetDefaultFontSize(Scene.GetObject, LNewSize);
        if not SameValue(LNewSize, 0, TEpsilon.FontSize) then
          ADefaultTextSettings.Font.Size := LNewSize;
      end;
    end;
  end;

var
  LInterface: IInterface;
  LNewText: string;
  I: Integer;
begin
  LFontBehavior := nil;
  BeginUpdate;
  try
    ResultingTextSettings.BeginUpdate;
    try
      FTextSettingsInfo.Design := False;
      { behavior }
      if (Scene <> nil) and TBehaviorServices.Current.SupportsBehaviorService(IFontBehavior, LInterface, Scene.GetObject) then
        Supports(LInterface, IFontBehavior, LFontBehavior);
      { from text }
      SetupDefaultTextSetting(FindStyleResource('text'), FTextSettingsInfo.DefaultTextSettings);
      inherited;
      ResultingTextSettings.Change;
    finally
      ResultingTextSettings.EndUpdate;
      FTextSettingsInfo.Design := True;//csDesigning in ComponentState;
    end;
    if AutoTranslate and not Text.IsEmpty then
    begin
      for I := 0 to FWords.Count - 1 do
      begin
        LNewText := Translate(FWords[I].Text); // need for collection texts
        if not (csDesigning in ComponentState) then
          FWords[I].Text := LNewText;
      end;
    end;
    DeleteParagraph;
    if (not IsUpdating) and FAutoSize and HasFitSizeChanged then
      SetSize(Width, Height);
    Redraw;
  finally
    EndUpdate;
  end;
end;

procedure TSkLabel.Click;
begin
  if Assigned(FWordsMouseOver) and Assigned(FWordsMouseOver.OnClick) then
    FWordsMouseOver.OnClick(FWordsMouseOver)
  else
    inherited;
end;

constructor TSkLabel.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
  FAutoSize := True;
  AutoTranslate := True;
  FTextSettingsInfo := TSkTextSettingsInfo.Create(Self, nil);
  FTextSettingsInfo.Design := True;//csDesigning in ComponentState;
  FTextSettingsInfo.OnChange := TextSettingsChanged;
  FWords := TWordsCollection.Create(Self, TWordsItem);
  FWords.OnChange := WordsChange;
end;

procedure TSkLabel.DeleteParagraph;
begin
  FParagraph := nil;
  FParagraphBounds := TRectF.Empty;
  FParagraphLayoutWidth := 0;
end;

destructor TSkLabel.Destroy;
begin
  FTextSettingsInfo.Free;
  FWords.Free;
  inherited;
end;

procedure TSkLabel.DoEndUpdate;
begin
  if (not (csLoading in ComponentState)) and FAutoSize and HasFitSizeChanged then
    SetSize(Width, Height)
  else
    inherited;
end;

procedure TSkLabel.DoMouseLeave;
begin
  inherited;
  SetWordsMouseOver(nil);
end;

function TSkLabel.DoSetSize(const ASize: TControlSize;
  const ANewPlatformDefault: Boolean; ANewWidth, ANewHeight: Single;
  var ALastWidth, ALastHeight: Single): Boolean;
begin
  if FAutoSize and not (csLoading in ComponentState) then
    GetFitSize(ANewWidth, ANewHeight);
  Result := inherited;
end;

procedure TSkLabel.DoStyleChanged;
var
  LNewText: string;
  I: Integer;
begin
  inherited;
  if AutoTranslate and not Text.IsEmpty then
  begin
    for I := 0 to FWords.Count - 1 do
    begin
      LNewText := Translate(FWords[I].Text); // need for collection texts
      if not (csDesigning in ComponentState) then
        FWords[I].Text := LNewText;
    end;
  end;
end;

procedure TSkLabel.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);

  function CreateBackgroundPicture(const AParagraph: ISkParagraph): ISkPicture;
  var
    LPictureRecorder: ISkPictureRecorder;
    LCanvas: ISkCanvas;
    LPaint: ISkPaint;
    I: Integer;
    LTextEndIndex: Integer;
    LTextBox: TSkTextBox;
  begin
    LPictureRecorder := TSkPictureRecorder.Create;
    LCanvas := LPictureRecorder.BeginRecording(ADest);
    LPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;
    LTextEndIndex := 0;
    for I := 0 to FWords.Count - 1 do
    begin
      Inc(LTextEndIndex, FWords[I].Text.Length);
      if TAlphaColorRec(FWords[I].BackgroundColor).A = 0 then
        Continue;
      LPaint.Color := FWords[I].BackgroundColor;
      for LTextBox in AParagraph.GetRectsForRange(LTextEndIndex - FWords[I].Text.Length, LTextEndIndex, TSkRectHeightStyle.Tight, TSkRectWidthStyle.Tight) do
        LCanvas.DrawRoundRect(Canvas.AlignToPixel(LTextBox.Rect), 2, 2, LPaint);
    end;
    Result := LPictureRecorder.FinishRecording;
  end;

var
  LParagraph: ISkParagraph;
  LPositionY: Single;
  LCurrentFillTextFlags: TFillTextFlags;
begin
  LCurrentFillTextFlags := FillTextFlags;
  if FLastFillTextFlags <> LCurrentFillTextFlags then
  begin
    DeleteParagraph;
    FLastFillTextFlags := LCurrentFillTextFlags;
  end;
  LParagraph := Paragraph;
  if Assigned(LParagraph) then
  begin
    ParagraphLayout(ADest.Width);
    LPositionY := ADest.Top;
    case ResultingTextSettings.VertAlign of
      TTextAlign.Center: LPositionY := LPositionY + ((ADest.Height - ParagraphBounds.Height) / 2);
      TTextAlign.Leading: ;
      TTextAlign.Trailing: LPositionY := LPositionY + (ADest.Height - ParagraphBounds.Height);
    end;

    if SameValue(AOpacity, 1, TEpsilon.Position) then
      ACanvas.Save
    else
      ACanvas.SaveLayerAlpha(Round(AOpacity * 255));
    try
      ACanvas.ClipRect(ADest);
      ACanvas.Translate(ADest.Left, LPositionY);
      if FHasCustomBackground then
      begin
        if FBackgroundPicture = nil then
          FBackgroundPicture := CreateBackgroundPicture(LParagraph);
        ACanvas.DrawPicture(FBackgroundPicture);
      end;
      LParagraph.Paint(ACanvas, 0, 0);
    finally
      ACanvas.Restore;
    end;
  end;
end;

procedure TSkLabel.FreeStyle;
begin
  if FObjectState <> nil then
  begin
    FObjectState.RestoreState;
    FObjectState := nil;
  end;
  FStyleText := nil;
  inherited;
end;

function TSkLabel.GetDefaultSize: TSizeF;
{$IF CompilerVersion >= 30}
var
  LDeviceInfo: IDeviceBehavior;
{$ENDIF}
begin
  {$IF CompilerVersion >= 30}
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, LDeviceInfo, Self) then
    case LDeviceInfo.GetOSPlatform(Self) of
      TOSPlatform.Windows: Result := TSizeF.Create(120, 19);
      TOSPlatform.OSX: Result := TSizeF.Create(120, 19);
      TOSPlatform.iOS: Result := TSize.Create(82, 21);
      TOSPlatform.Android: Result := TSize.Create(82, 23);
      {$IF CompilerVersion >= 34}
      TOSPlatform.Linux: Result := TSizeF.Create(120, 19);
      {$ENDIF}
    end
  else
  {$ENDIF}
    Result := TSizeF.Create(120, 19);
end;

function TSkLabel.GetDefaultTextSettings: TSkTextSettings;
begin
  Result := FTextSettingsInfo.DefaultTextSettings;
end;

procedure TSkLabel.GetFitSize(var AWidth, AHeight: Single);
var
  LFinalScale: TPointF;

  function GetFitHeight: Single;
  begin
    case Align of
      TAlignLayout.Client,
      TAlignLayout.Contents,
      TAlignLayout.Left,
      TAlignLayout.MostLeft,
      TAlignLayout.Right,
      TAlignLayout.MostRight,
      TAlignLayout.FitLeft,
      TAlignLayout.FitRight,
      TAlignLayout.HorzCenter,
      TAlignLayout.Vertical: Result := AHeight;
    else
      Result := Ceil(ParagraphBounds.Height * LFinalScale.Y) / LFinalScale.Y;
    end;
  end;

  function GetFitWidth: Single;
  begin
    case Align of
      TAlignLayout.Client,
      TAlignLayout.Contents,
      TAlignLayout.Top,
      TAlignLayout.MostTop,
      TAlignLayout.Bottom,
      TAlignLayout.MostBottom,
      TAlignLayout.VertCenter,
      TAlignLayout.Horizontal: Result := AWidth;
    else
      Result := Ceil(ParagraphBounds.Width * LFinalScale.X) / LFinalScale.X;
    end;
  end;

  function GetFinalScale: TPointF;
  begin
    if Assigned(Scene) then
      Result := AbsoluteScale * Scene.GetSceneScale
    else
      Result := AbsoluteScale;
  end;

var
  LParagraph: ISkParagraph;
begin
  LParagraph := Paragraph;
  LFinalScale := GetFinalScale;
  if Assigned(LParagraph) then
  begin
    if Align in [TAlignLayout.Top, TAlignLayout.MostTop, TAlignLayout.Bottom,
      TAlignLayout.MostBottom, TAlignLayout.VertCenter, TAlignLayout.Horizontal] then
    begin
      ParagraphLayout(AWidth);
    end
    else
      ParagraphLayout(High(Integer));
  end;
  try
    AWidth := GetFitWidth;
    AHeight := GetFitHeight;
  finally
    if Assigned(LParagraph) then
      ParagraphLayout(AWidth);
  end;
end;

function TSkLabel.GetParagraph: ISkParagraph;
var
  LFontBehavior: IFontBehavior;

  function GetFontFamilies(const AValue: string): TArray<string>;
  var
    LInterface: IInterface;
  begin
    Result := AValue.Split([', ', ','], TStringSplitOptions.ExcludeEmpty);
    if Result = nil then
    begin
      if (LFontBehavior = nil) and (Scene <> nil) and TBehaviorServices.Current.SupportsBehaviorService(IFontBehavior, LInterface, Scene.GetObject) then
        Supports(LInterface, IFontBehavior, LFontBehavior);
      if Assigned(LFontBehavior) then
      begin
        SetLength(Result, 1);
        LFontBehavior.GetDefaultFontFamily(Scene.GetObject, Result[0]);
        if Result[0].IsEmpty then
          Result := [];
      end;
    end;
    {$IFDEF MACOS}
    Result := Result + ['Helvetica Neue'];
    {$ELSEIF DEFINED(LINUX)}
    Result := Result + ['Ubuntu'];
    {$ENDIF}
  end;

  function GetFontSize(const AValue: Single): Single;
  var
    LInterface: IInterface;
  begin
    Result := AValue;
    if SameValue(AValue, 0, TEpsilon.FontSize) then
    begin
      if (LFontBehavior = nil) and (Scene <> nil) and TBehaviorServices.Current.SupportsBehaviorService(IFontBehavior, LInterface, Scene.GetObject) then
        Supports(LInterface, IFontBehavior, LFontBehavior);
      if Assigned(LFontBehavior) then
        LFontBehavior.GetDefaultFontSize(Scene.GetObject, Result);
    end;
  end;

  procedure SetTextStyleDecorations(var ATextStyle: ISkTextStyle;
    const ADecorations: TSkTextSettings.TDecorations);
  begin
    if ADecorations.Decorations <> [] then
    begin
      if ADecorations.Color = TAlphaColors.Null then
        ATextStyle.DecorationColor := ATextStyle.Color
      else
        ATextStyle.DecorationColor := ADecorations.Color;
      ATextStyle.Decorations := ADecorations.Decorations;
      ATextStyle.DecorationStyle := ADecorations.Style;
      ATextStyle.DecorationThickness := ADecorations.Thickness;
    end;
  end;

  function CreateTextStyle(const AWordsItem: TCustomWordsItem; const ADefaultTextStyle: ISkTextStyle): ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    if TStyledSetting.FontColor in AWordsItem.StyledSettings then
      Result.Color := ADefaultTextStyle.Color
    else
      Result.Color := AWordsItem.FontColor;
    if TStyledSetting.Family in AWordsItem.StyledSettings then
      Result.FontFamilies := ADefaultTextStyle.FontFamilies
    else
      Result.FontFamilies := GetFontFamilies(AWordsItem.Font.Families);
    if TStyledSetting.Size in AWordsItem.StyledSettings then
      Result.FontSize := ADefaultTextStyle.FontSize
    else
      Result.FontSize := GetFontSize(AWordsItem.Font.Size);
    if TStyledSetting.Style in AWordsItem.StyledSettings then
      Result.FontStyle := ADefaultTextStyle.FontStyle
    else
      Result.FontStyle := TSkFontStyle.Create(SkFontWeightValue[AWordsItem.Font.Weight], SkFontWidthValue[AWordsItem.Font.Stretch], SkFontSlant[AWordsItem.Font.Slant]);
    if TStyledSetting.Other in AWordsItem.StyledSettings then
      SetTextStyleDecorations(Result, ResultingTextSettings.Decorations)
    else
      SetTextStyleDecorations(Result, AWordsItem.Decorations);
  end;

  function CreateDefaultTextStyle: ISkTextStyle;
  begin
    Result := TSkTextStyle.Create;
    Result.Color := ResultingTextSettings.FontColor;
    Result.FontFamilies := GetFontFamilies(ResultingTextSettings.Font.Families);
    Result.FontSize := GetFontSize(ResultingTextSettings.Font.Size);
    Result.FontStyle := TSkFontStyle.Create(SkFontWeightValue[ResultingTextSettings.Font.Weight], SkFontWidthValue[ResultingTextSettings.Font.Stretch], SkFontSlant[ResultingTextSettings.Font.Slant]);
    SetTextStyleDecorations(Result, ResultingTextSettings.Decorations);
  end;

  function CreateParagraphStyle(const ADefaultTextStyle: ISkTextStyle): ISkParagraphStyle;
  const
    SkTextAlign: array[TSkTextHorzAlign] of TSkTextAlign = (TSkTextAlign.Center, TSkTextAlign.Start, TSkTextAlign.Terminate, TSkTextAlign.Justify);
  begin
    Result := TSkParagraphStyle.Create;
    if ((Root = nil) and (Application.BiDiMode = TBiDiMode.bdRightToLeft)) or (TFillTextFlag.RightToLeft in FillTextFlags) then
      Result.TextDirection := TSkTextDirection.RightToLeft;
    if ResultingTextSettings.Trimming in [TTextTrimming.Character, TTextTrimming.Word] then
      Result.Ellipsis := '...';
    if ResultingTextSettings.MaxLines = 0 then
      Result.MaxLines := High(Integer)
    else
      Result.MaxLines := ResultingTextSettings.MaxLines;
    Result.TextAlign := SkTextAlign[ResultingTextSettings.HorzAlign];
    Result.TextStyle := ADefaultTextStyle;
  end;

  function CreateParagraph: ISkParagraph;
  var
    LBuilder: ISkParagraphBuilder;
    LDefaultTextStyle: ISkTextStyle;
    I: Integer;
  begin
    LFontBehavior := nil;
    LDefaultTextStyle := CreateDefaultTextStyle;
    LBuilder := TSkParagraphBuilder.Create(CreateParagraphStyle(LDefaultTextStyle), TSkTypefaceManager.Provider);
    for I := 0 to FWords.Count- 1 do
    begin
      if FWords[I].Text = '' then
        Continue;
      if FWords[I].StyledSettings = AllStyledSettings then
        LBuilder.AddText(FWords[I].Text)
      else
      begin
        LBuilder.PushStyle(CreateTextStyle(FWords[I], LDefaultTextStyle));
        LBuilder.AddText(FWords[I].Text);
        LBuilder.Pop;
      end;
      FHasCustomBackground := FHasCustomBackground or (FWords[I].BackgroundColor <> TAlphaColors.Null);
    end;
    Result := LBuilder.Build;
  end;

begin
  if (FParagraph = nil) and (Text <> '') then
  begin
    FBackgroundPicture := nil;
    FHasCustomBackground := False;
    FParagraph := CreateParagraph;
    ParagraphLayout(Width);
  end;
  Result := FParagraph;
end;

function TSkLabel.GetParagraphBounds: TRectF;

  function CalculateParagraphBounds: TRectF;
  var
    LParagraph: ISkParagraph;
  begin
    LParagraph := Paragraph;
    if Assigned(LParagraph) then
      Result := RectF(0, 0, Ceil(LParagraph.MaxIntrinsicWidth), Ceil(LParagraph.Height))
    else
      Result := TRectF.Empty;
  end;

begin
  if FParagraphBounds.IsEmpty then
    FParagraphBounds := CalculateParagraphBounds;
  Result := FParagraphBounds;
end;

function TSkLabel.GetResultingTextSettings: TSkTextSettings;
begin
  Result := FTextSettingsInfo.ResultingTextSettings;
end;

function TSkLabel.GetStyledSettings: TStyledSettings;
begin
  Result := FTextSettingsInfo.StyledSettings;
end;

function TSkLabel.GetText: string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(FWords) and (FWords.Count > 0) then
    for I := 0 to FWords.Count - 1 do
      Result := Result + FWords[I].Text;
end;

function TSkLabel.GetTextSettings: TSkTextSettings;
begin
  Result := FTextSettingsInfo.TextSettings;
end;

function TSkLabel.GetTextSettingsClass: TSkTextSettingsInfo.TCustomTextSettingsClass;
begin
  Result := TSkTextSettingsInfo.TCustomTextSettings;
end;

function TSkLabel.HasFitSizeChanged: Boolean;
var
  LNewWidth: Single;
  LNewHeight: Single;
begin
  LNewWidth := Width;
  LNewHeight := Height;
  GetFitSize(LNewWidth, LNewHeight);
  Result := (not SameValue(LNewWidth, Width, TEpsilon.Position)) or (not SameValue(LNewHeight, Height, TEpsilon.Position));
end;

function TSkLabel.IsStyledSettingsStored: Boolean;
begin
  Result := StyledSettings <> DefaultStyledSettings;
end;

procedure TSkLabel.Loaded;
begin
  inherited;
  if (not IsUpdating) and FAutoSize and HasFitSizeChanged then
    SetSize(Width, Height);
end;

procedure TSkLabel.MouseMove(AShift: TShiftState; AX, AY: Single);
var
  I, J: Integer;
  LTextIndex: Integer;
  LTextBoxes: TArray<TSkTextBox>;
  LNewWordsMouseOver: TCustomWordsItem;
  LParagraph: ISkParagraph;
  LParagraphPoint: TPointF;
begin
  if FHasCustomCursor then
  begin
    LParagraph := Paragraph;
    if Assigned(LParagraph) then
    begin
      case ResultingTextSettings.VertAlign of
        TTextAlign.Center: LParagraphPoint := PointF(AX, AY - (Height - ParagraphBounds.Height) / 2);
        TTextAlign.Trailing: LParagraphPoint := PointF(AX, AY - Height - ParagraphBounds.Height);
      else
        LParagraphPoint := PointF(AX, AY);
      end;
      LNewWordsMouseOver := nil;
      LTextIndex := 0;
      for I := 0 to FWords.Count - 1 do
      begin
        if FWords[I].Text.Length = 0 then
          Continue;
        LTextBoxes := LParagraph.GetRectsForRange(LTextIndex, LTextIndex + FWords[I].Text.Length, TSkRectHeightStyle.Max, TSkRectWidthStyle.Tight);
        for J := 0 to Length(LTextBoxes) - 1 do
        begin
          if LTextBoxes[J].Rect.Contains(LParagraphPoint) then
          begin
            LNewWordsMouseOver := FWords[I];
            Break;
          end;
        end;
        if Assigned(LNewWordsMouseOver) then
          Break;
        LTextIndex := LTextIndex + FWords[I].Text.Length;
      end;
      SetWordsMouseOver(LNewWordsMouseOver);
    end;
  end;
  inherited;
end;

function TSkLabel.NeedsRedraw: Boolean;
begin
  Result := inherited or (FLastFillTextFlags <> FillTextFlags);
end;

procedure TSkLabel.ParagraphLayout(const AWidth: Single);
var
  LParagraph: ISkParagraph;
begin
  if not SameValue(FParagraphLayoutWidth, AWidth, TEpsilon.Position) then
  begin
    LParagraph := Paragraph;
    if Assigned(LParagraph) then
    begin
      LParagraph.Layout(AWidth);
      FParagraphLayoutWidth := AWidth;
      FParagraphBounds := TRectF.Empty;
      FBackgroundPicture := nil;
    end;
  end;
end;

procedure TSkLabel.SetAlign(const AValue: TAlignLayout);
begin
  if (Align <> AValue) and AutoSize then
  begin
    inherited;
    SetSize(Width, Height);
  end
  else
    inherited;
end;

procedure TSkLabel.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize <> AValue then
  begin
    FAutoSize := AValue;
    SetSize(Width, Height);
  end;
end;

procedure TSkLabel.SetName(const AValue: TComponentName);
var
  LChangeText: Boolean;
begin
  LChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(AValue);
  if LChangeText then
    Text := AValue;
end;

procedure TSkLabel.SetStyledSettings(const AValue: TStyledSettings);
begin
  FTextSettingsInfo.StyledSettings := AValue;
end;

procedure TSkLabel.SetText(const AValue: string);
begin
  if Assigned(FWords) then
  begin
    FWords.BeginUpdate;
    try
      if FWords.Count = 1 then
        FWords[0].Text := AValue
      else
      begin
        FWords.Clear;
        FWords.Add.Text := AValue;
      end;
    finally
      FWords.EndUpdate;
    end;
  end;
end;

procedure TSkLabel.SetTextSettings(const AValue: TSkTextSettings);
begin
  FTextSettingsInfo.TextSettings := AValue;
end;

procedure TSkLabel.SetWords(const AValue: TWordsCollection);
begin
  FWords.Assign(AValue);
end;

procedure TSkLabel.SetWordsMouseOver(const AValue: TCustomWordsItem);
begin
  if FWordsMouseOver <> AValue then
  begin
    FWordsMouseOver := AValue;
    if not (csDesigning in ComponentState) and IsMouseOver then
    begin
      if Assigned(FWordsMouseOver) and (FWordsMouseOver.Cursor <> crDefault) then
        Cursor := FWordsMouseOver.Cursor
      else
        Cursor := crDefault;
    end;
  end;
end;

procedure TSkLabel.TextSettingsChanged(AValue: TObject);
begin
  DeleteParagraph;
  if not (csLoading in ComponentState) then
  begin
    if (not IsUpdating) and FAutoSize and HasFitSizeChanged then
      SetSize(Width, Height)
    else
      Redraw;
  end;
end;

{$IF CompilerVersion >= 29}
function TSkLabel.TextStored: Boolean;
begin
  Result := Text <> '';
end;
{$ENDIF}

procedure TSkLabel.WordsChange(ASender: TObject);
var
  I: Integer;
begin
  FHasCustomCursor := False;
  for I := 0 to FWords.Count - 1 do
  begin
    if FWords[I].Cursor <> crDefault then
    begin
      FHasCustomCursor := True;
      Break;
    end;
  end;
  if FWords.Count = 0 then
    FWords.Add
  else
    TextSettingsChanged(nil);
end;

{ TSkTypefaceManager }

class constructor TSkTypefaceManager.Create;
begin
  FProvider := TSkTypefaceFontProvider.Create;
end;

class procedure TSkTypefaceManager.RegisterTypeface(const AFileName: string);
begin
  FProvider.RegisterTypeface(TSkTypeFace.MakeFromFile(AFileName));
end;

class procedure TSkTypefaceManager.RegisterTypeface(const AStream: TStream);
begin
  FProvider.RegisterTypeface(TSkTypeFace.MakeFromStream(AStream));
end;

{ Register }

procedure Register;
begin
  RegisterComponents('Skia', [TSkAnimatedImage, TSkAnimatedPaintBox, TSkLabel, TSkPaintBox, TSkStyleTextObject, TSkSvg]);
end;

initialization
  RegisterFMXClasses([TSkAnimatedImage, TSkAnimatedPaintBox, TSkCustomControl, TSkLabel, TSkPaintBox,
    TSkStyledControl, TSkStyleTextObject, TSkSvgBrush, TSkSvg]);
  RegisterFMXClasses([TSkFontComponent, TSkTextSettings, TSkTextSettingsInfo, TSkTextSettings.TDecorations,
    TSkLabel.TCustomWordsItem, TSkLabel.TWordsCollection]);
  TSkAnimatedImage.RegisterCodec(TSkLottieAnimationCodec);
  TSkAnimatedImage.RegisterCodec(TSkDefaultAnimationCodec);
end.
