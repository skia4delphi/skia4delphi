{************************************************************************}
{                                                                        }
{                               MobileBars                               }
{                                                                        }
{ Copyright (c) 2021-2023 MobileBars                                     }
{ https://github.com/viniciusfbb/mobilebars                              }
{                                                                        }
{ Use of this source code is governed by a MIT license that can be found }
{ at https://opensource.org/licenses/MIT                                 }
{                                                                        }
{************************************************************************}
unit MobileBars;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.Messaging,
  System.Types,
  System.UITypes,
  System.Generics.Collections,
  FMX.Forms;

type
  { TFormMobileBars }

  TFormMobileBars = class(TPersistent)
  public type
    TVisibilityMode = TFormSystemStatusBar.TVisibilityMode;
  public const
    DefaultNavigationBarBackgroundColor = TAlphaColorRec.Null;
    DefaultStatusBarBackgroundColor = TFormSystemStatusBar.DefaultBackgroundColor;
    DefaultVisibility = TFormSystemStatusBar.DefaultVisibility;
  {$REGION 'internal'}
  public type
    Exception = class(System.SysUtils.Exception);

    /// <summary>Called from a form as Sender always when the form's insets has changed</summary>
    TInsetsChangeMessage = class(TMessage)
    private
      FInsets: TRectF;
      FTappableInsets: TRectF;
    public
      constructor Create(const AInsets, ATappableInsets: TRectF);
      property Insets: TRectF read FInsets;
      property TappableInsets: TRectF read FTappableInsets;
    end;

    /// <summary>Service for working with native system navigation bar</summary>
    IFMXWindowMobileBarsService = interface
    ['{124BEBCA-0F61-4A94-92E4-CA279E1BE2E3}']
      /// <summary>Sizes of all current system bars</summary>
      function GetInsets(const AForm: TCommonCustomForm): TRectF;
      /// <summary>Sizes of all current system bars without gesture bar</summary>
      function GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
      /// <summary>Sets background color of system status bar</summary>
      procedure SetStatusBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
      /// <summary>Sets background color of system navigation bar</summary>
      procedure SetNavigationBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
      /// <summary>Sets how system bars will be shown. See TFormMobileBars.TVisibilityMode</summary>
      procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TFormMobileBars.TVisibilityMode);
    end;
  strict private
    [Weak] FForm: TCommonCustomForm;
    FFormInsetsChangeMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
    FInsets: TRectF;
    FNavigationBarBackgroundColor: TAlphaColor;
    FOnInsetsChange: TNotifyEvent;
    FTappableInsets: TRectF;
    procedure FormInsetsChange(const ASender: TObject; const AMessage: TMessage);
    function GetStatusBarBackgroundColor: TAlphaColor;
    function GetVisibility: TVisibilityMode;
    procedure SetNavigationBarBackgroundColor(const AValue: TAlphaColor);
    procedure SetStatusBarBackgroundColor(const AValue: TAlphaColor);
    procedure SetVisibility(const AValue: TVisibilityMode);
  protected
    procedure AssignTo(ADest: TPersistent); override;
  {$ENDREGION}
  public
    constructor Create(const AForm: TCommonCustomForm);
    destructor Destroy; override;
    /// <summary>Distances in which the system bars are overlapping the sides of the form (top, left, right and bottom)</summary>
    property Insets: TRectF read FInsets;
    /// <summary>Distances in which the system bars, without gesture bar, are overlapping the sides of the form (top, left, right and bottom)</summary>
    property TappableInsets: TRectF read FTappableInsets;
    /// <summary>When system bars overlapping distances change, like navigation bar going to right when the phone go to landscape</summary>
    property OnInsetsChange: TNotifyEvent read FOnInsetsChange write FOnInsetsChange;
  published
    /// <summary>Background color of system navigation bar</summary>
    property NavigationBarBackgroundColor: TAlphaColor read FNavigationBarBackgroundColor write SetNavigationBarBackgroundColor default DefaultNavigationBarBackgroundColor;
    /// <summary>Background color of system status bar</summary>
    property StatusBarBackgroundColor: TAlphaColor read GetStatusBarBackgroundColor write SetStatusBarBackgroundColor default DefaultStatusBarBackgroundColor;
    /// <summary>Different modes of showing system bars</summary>
    property Visibility: TVisibilityMode read GetVisibility write SetVisibility default DefaultVisibility;
  end;

  { TFormHelper }

  TFormHelper = class helper for TCommonCustomForm
  {$REGION 'internal'}
  // The correct solution would be to change the FMX source to insert the MobileBars
  // property in TCommonCustomForm. But to avoid patches on Embarcadero's source, we
  // made this helper for the forms
  strict private
    class var
      FAfterCreateFormHandleMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
      FDictionary: TObjectDictionary<TCommonCustomForm, TFormMobileBars>;
      FFormReleasedMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
    class procedure AfterCreateFormHandle(const ASender: TObject; const AMessage: TMessage); static;
    class constructor Create;
    class destructor Destroy;
    class procedure FormReleased(const ASender: TObject; const AMessage: TMessage); static;
    class function GetFormMobileBars(AForm: TCommonCustomForm): TFormMobileBars; static;
  strict private
    function GetOnMobileBarsInsetsChange: TNotifyEvent;
    function GetMobileBars: TFormMobileBars;
    procedure SetOnMobileBarsInsetsChange(const AValue: TNotifyEvent);
    procedure SetMobileBars(const AValue: TFormMobileBars);
  {$ENDREGION}
  public
    /// <summary>Settings of system bars on mobile platforms</summary>
    property MobileBars: TFormMobileBars read GetMobileBars write SetMobileBars;
    /// <summary>When system bars overlapping distances change, like navigation bar going to right when the phone go to landscape</summary>
    property OnMobileBarsInsetsChange: TNotifyEvent read GetOnMobileBarsInsetsChange write SetOnMobileBarsInsetsChange;
  end;

implementation

uses
  { Delphi }
  FMX.Platform,

  { MobileBars }
  MobileBars.Android,
  MobileBars.iOS;

{ TFormMobileBars }

procedure TFormMobileBars.AssignTo(ADest: TPersistent);
var
  LDestMobileBars: TFormMobileBars;
begin
  if ADest is TFormMobileBars then
  begin
    LDestMobileBars := TFormMobileBars(ADest);
    LDestMobileBars.NavigationBarBackgroundColor := NavigationBarBackgroundColor;
    LDestMobileBars.StatusBarBackgroundColor := StatusBarBackgroundColor;
    LDestMobileBars.Visibility := Visibility;
  end
  else
    inherited;
end;

constructor TFormMobileBars.Create(const AForm: TCommonCustomForm);
begin
  FForm := AForm;
  FNavigationBarBackgroundColor := DefaultNavigationBarBackgroundColor;
  FFormInsetsChangeMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TInsetsChangeMessage, FormInsetsChange);
end;

destructor TFormMobileBars.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TInsetsChangeMessage, FFormInsetsChangeMessageId);
  inherited;
end;

procedure TFormMobileBars.FormInsetsChange(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender = FForm then
  begin
    FInsets := TInsetsChangeMessage(AMessage).Insets;
    FTappableInsets := TInsetsChangeMessage(AMessage).TappableInsets;
    if Assigned(FOnInsetsChange) then
      FOnInsetsChange(FForm);
  end;
end;

function TFormMobileBars.GetStatusBarBackgroundColor: TAlphaColor;
begin
  Result := FForm.SystemStatusBar.BackgroundColor;
end;

function TFormMobileBars.GetVisibility: TVisibilityMode;
begin
  Result := FForm.SystemStatusBar.Visibility;
end;

procedure TFormMobileBars.SetNavigationBarBackgroundColor(
  const AValue: TAlphaColor);
var
  LService: IFMXWindowMobileBarsService;
begin
  if FNavigationBarBackgroundColor <> AValue then
  begin
    FNavigationBarBackgroundColor := AValue;
    if TPlatformServices.Current.SupportsPlatformService(IFMXWindowMobileBarsService, LService) then
      LService.SetNavigationBarBackgroundColor(FForm, FNavigationBarBackgroundColor);
  end;
end;

procedure TFormMobileBars.SetStatusBarBackgroundColor(
  const AValue: TAlphaColor);
begin
  FForm.SystemStatusBar.BackgroundColor := AValue;
end;

procedure TFormMobileBars.SetVisibility(const AValue: TVisibilityMode);
begin
  FForm.SystemStatusBar.Visibility := AValue;
end;

{ TFormMobileBars.TInsetsChangeMessage }

constructor TFormMobileBars.TInsetsChangeMessage.Create(const AInsets,
  ATappableInsets: TRectF);
begin
  inherited Create;
  FInsets := AInsets;
  FTappableInsets := ATappableInsets;
end;

{ TFormHelper }

class procedure TFormHelper.AfterCreateFormHandle(const ASender: TObject;
  const AMessage: TMessage);
begin
  // To approach a simulation of the creation of the system bars property in TCommonCustomForm.Create,
  // because the TMobileBars need to subscribe to the TInsetsChangeMessage as soon as possible
  if ASender is TCommonCustomForm then
    TCommonCustomForm(ASender).MobileBars;
end;

class constructor TFormHelper.Create;
begin
  FDictionary := TObjectDictionary<TCommonCustomForm, TFormMobileBars>.Create([doOwnsValues]);
  FAfterCreateFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  FFormReleasedMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormReleasedMessage, FormReleased);
end;

class destructor TFormHelper.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormReleasedMessage, FFormReleasedMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FAfterCreateFormHandleMessageId);
  FDictionary.Free;
end;

class procedure TFormHelper.FormReleased(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender is TCommonCustomForm then
    FDictionary.Remove(TCommonCustomForm(ASender));
end;

class function TFormHelper.GetFormMobileBars(
  AForm: TCommonCustomForm): TFormMobileBars;
begin
  if not Assigned(AForm) then
    Exit(nil);
  if not FDictionary.TryGetValue(AForm, Result) then
  begin
    if (csDestroying in AForm.ComponentState) or (TFmxFormState.Released in AForm.FormState) then
      Exit(nil);
    Result := TFormMobileBars.Create(AForm);
    FDictionary.Add(AForm, Result);
  end;
end;

function TFormHelper.GetOnMobileBarsInsetsChange: TNotifyEvent;
begin
  Result := MobileBars.OnInsetsChange;
end;

function TFormHelper.GetMobileBars: TFormMobileBars;
begin
  Result := GetFormMobileBars(Self);
end;

procedure TFormHelper.SetOnMobileBarsInsetsChange(const AValue: TNotifyEvent);
begin
  MobileBars.OnInsetsChange := AValue;
end;

procedure TFormHelper.SetMobileBars(const AValue: TFormMobileBars);
begin
  MobileBars.Assign(AValue);
end;

end.
