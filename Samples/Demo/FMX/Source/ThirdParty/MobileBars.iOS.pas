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
unit MobileBars.iOS;

interface

{$SCOPEDENUMS ON}
{$IFDEF iOS}

implementation

uses
  { Delphi }
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.Math.Vectors,
  System.Messaging,
  System.Rtti,
  System.TypInfo,
  iOSapi.Helpers,
  iOSapi.UIKit,
  iOSapi.Foundation,
  FMX.Forms,
  FMX.Platform,

  { MobileBars }
  MobileBars;

type
  { TMobileBarsServiceiOS }

  TMobileBarsServiceiOS = class(TInterfacedObject, TFormMobileBars.IFMXWindowMobileBarsService, IFMXWindowSystemStatusBarService)
  private
    FAfterCreateFormHandleMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
    FFormActivateMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
    FDefaultStatusBarService: IFMXWindowSystemStatusBarService;
    FGestureBarChecked: Boolean;
    FGestureBarOffset: Single;
    FRegisteredBarsService: Boolean;
    FVirtualKeyboardBounds: TRect;
    FVirtualKeyboardMessageId: {$IF CompilerVersion >= 36}TMessageSubscriptionId{$ELSE}Integer{$ENDIF};
    procedure AfterCreateFormHandle(const ASender: TObject; const AMessage: TMessage);
    procedure CheckInsetsChanges(const AForm: TCommonCustomForm);
    procedure FormActivate(const ASender: TObject; const AMessage: TMessage);
    function GetGestureBarOffset(const AForm: TCommonCustomForm): Single;
    function GetStatusBarOffset(const AForm: TCommonCustomForm): Single;
    function RemoveKeyboardOverlappedBars(const AInsets: TRectF): TRectF;
    procedure VirtualKeyboardChangeHandler(const ASender: TObject; const AMessage: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXWindowMobileBarsService }
    function GetInsets(const AForm: TCommonCustomForm): TRectF;
    function GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
    procedure SetNavigationBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    { IFMXWindowSystemStatusBarService / IFMXWindowMobileBarsService }
    procedure IFMXWindowSystemStatusBarService.SetBackgroundColor = SetStatusBarBackgroundColor;
    procedure SetStatusBarBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TFormSystemStatusBar.TVisibilityMode);
  end;

var
  FMobileBarsServiceiOS: TMobileBarsServiceiOS;

{ TMobileBarsServiceiOS }

procedure TMobileBarsServiceiOS.AfterCreateFormHandle(const ASender: TObject;
  const AMessage: TMessage);
begin
  if (ASender is TCommonCustomForm) and (TFmxFormState.Recreating in TCommonCustomForm(ASender).FormState) then
    CheckInsetsChanges(TCommonCustomForm(ASender));
end;

procedure TMobileBarsServiceiOS.CheckInsetsChanges(
  const AForm: TCommonCustomForm);
var
  LNewInsets: TRectF;
  LNewTappableInsets: TRectF;
  LFormMobileBars: TFormMobileBars;
begin
  if Assigned(AForm) and AForm.Active then
  begin
    LFormMobileBars := AForm.MobileBars;
    if Assigned(LFormMobileBars) then
    begin
      LNewInsets := GetInsets(AForm);
      LNewTappableInsets := GetTappableInsets(AForm);
      if (not LNewInsets.EqualsTo(LFormMobileBars.Insets, TEpsilon.Position)) or
        (not LNewTappableInsets.EqualsTo(LFormMobileBars.TappableInsets, TEpsilon.Position)) then
      begin
        TMessageManager.DefaultManager.SendMessage(AForm, TFormMobileBars.TInsetsChangeMessage.Create(LNewInsets, LNewTappableInsets));
      end;
    end;
  end;
end;

constructor TMobileBarsServiceiOS.Create;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowSystemStatusBarService, FDefaultStatusBarService) then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXWindowSystemStatusBarService);
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, Self);
  end
  else
    raise TFormMobileBars.Exception.Create('Cannot possible to find the service IFMXWindowSystemStatusBarService');
  if not TPlatformServices.Current.SupportsPlatformService(TFormMobileBars.IFMXWindowMobileBarsService) then
  begin
    TPlatformServices.Current.AddPlatformService(TFormMobileBars.IFMXWindowMobileBarsService, Self);
    FRegisteredBarsService := True;
  end;
  FAfterCreateFormHandleMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandle);
  FFormActivateMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TFormActivateMessage, FormActivate);
  FVirtualKeyboardMessageId := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VirtualKeyboardChangeHandler);
end;

destructor TMobileBarsServiceiOS.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FVirtualKeyboardMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FFormActivateMessageId);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, FAfterCreateFormHandleMessageId);
  if FRegisteredBarsService then
    TPlatformServices.Current.RemovePlatformService(TFormMobileBars.IFMXWindowMobileBarsService);
  if Assigned(FDefaultStatusBarService) then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXWindowSystemStatusBarService);
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, FDefaultStatusBarService);
  end;
  inherited;
end;

procedure TMobileBarsServiceiOS.FormActivate(const ASender: TObject;
  const AMessage: TMessage);
begin
  if ASender is TCommonCustomForm then
    CheckInsetsChanges(TCommonCustomForm(ASender));
end;

function TMobileBarsServiceiOS.GetGestureBarOffset(
  const AForm: TCommonCustomForm): Single;

  procedure CalcGestureBarOffset;
  begin
    TThread.Synchronize(nil,
      procedure()
      var
        LSharedApplication: UIApplication;
        LWindows: NSArray;
        LWindow: UIWindow;
        LViewController: UIViewController;
        LView: UIView;
      begin
        if not FMobileBarsServiceiOS.FGestureBarChecked then
        begin
          LSharedApplication := TiOSHelper.SharedApplication;
          if Assigned(LSharedApplication) then
          begin
            LWindows := LSharedApplication.windows;
            if Assigned(LWindows) and (LWindows.count > 0) then
            begin
              LWindow := TUIWindow.Wrap(LWindows.objectAtIndex(0));
              if Assigned(LWindow) then
              begin
                LViewController := LWindow.rootViewController;
                if Assigned(LViewController) then
                begin
                  LView := LViewController.view;
                  if Assigned(LView) then
                  begin
                    FMobileBarsServiceiOS.FGestureBarOffset := (LView.bounds.origin.y + LView.bounds.size.height) -
                      (LWindow.safeAreaLayoutGuide.layoutFrame.origin.y + LWindow.safeAreaLayoutGuide.layoutFrame.size.height);
                    FMobileBarsServiceiOS.FGestureBarChecked := True;
                  end;
                end;
              end;
            end;
          end;
        end;
      end);
  end;

  function HasFormGestureBar(const AForm: TCommonCustomForm): Boolean;
  begin
    Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None);
  end;

begin
  Result := 0;
  if HasFormGestureBar(AForm) then
  begin
    if not FGestureBarChecked then
      CalcGestureBarOffset;
    Result := FGestureBarOffset;
  end;
end;

function TMobileBarsServiceiOS.GetInsets(const AForm: TCommonCustomForm): TRectF;
begin
  Result := GetTappableInsets(AForm);
  Result.Bottom := Max(Result.Bottom, GetGestureBarOffset(AForm));
  Result := RemoveKeyboardOverlappedBars(Result);
end;

function TMobileBarsServiceiOS.GetStatusBarOffset(const AForm: TCommonCustomForm): Single;

  function HasFormStatusBarOffset(const AForm: TCommonCustomForm): Boolean;
  begin
    Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None) and
      (AForm.SystemStatusBar.Visibility = TFormSystemStatusBar.TVisibilityMode.VisibleAndOverlap);
  end;

  function GetStatusBarOffsetUsingRtti: Single;
  const
    CLASS_NOT_FOUND = 'Cannot possible to find the class FMX.Platform.iOS.TCocoaTouchWindowManager';
    PROPERTY_NOT_FOUND = 'Cannot possible to find the property "StatusBarOffset: Single" in class FMX.Platform.iOS.TCocoaTouchWindowManager';
  var
    LRttiContext: TRttiContext;
    LRttiType: TRttiType;
    LRttiProperty: TRttiProperty;
    LCocoaTouchWindowManager: TObject;
  begin
    {$IF CompilerVersion > 35} // RAD Studio 11 Alexandria
      {$MESSAGE WARN 'Check in file FMX.Platform.iOS.pas if the class TCocoaTouchWindowManager have already the property "StatusBarOffset: Single" and adjust the IFDEF'}
    {$ENDIF}
    LCocoaTouchWindowManager := TObject(FDefaultStatusBarService);
    Assert(LCocoaTouchWindowManager.ClassName = 'TCocoaTouchWindowManager', CLASS_NOT_FOUND);
    LRttiContext := TRttiContext.Create;
    try
      LRttiType := LRttiContext.GetType(LCocoaTouchWindowManager.ClassType);
      if not (LRttiType is TRttiInstanceType) then
        raise TFormMobileBars.Exception.Create(CLASS_NOT_FOUND);
      LRttiProperty := LRttiType.GetProperty('StatusBarOffset');
      if (not Assigned(LRttiProperty)) or (LRttiProperty.PropertyType.Handle <> TypeInfo(Single)) then
        raise TFormMobileBars.Exception.Create(PROPERTY_NOT_FOUND);
      Result := LRttiProperty.GetValue(LCocoaTouchWindowManager).AsExtended;
    finally
      LRttiContext.Free;
    end;
  end;

begin
  if HasFormStatusBarOffset(AForm) and Assigned(FDefaultStatusBarService) then
    Result := GetStatusBarOffsetUsingRtti
  else
    Result := 0;
end;

function TMobileBarsServiceiOS.GetTappableInsets(const AForm: TCommonCustomForm): TRectF;
begin
  Result := TRectF.Create(0, GetStatusBarOffset(AForm), 0, 0);
  Result := RemoveKeyboardOverlappedBars(Result);
end;

function TMobileBarsServiceiOS.RemoveKeyboardOverlappedBars(
  const AInsets: TRectF): TRectF;
var
  LScreenSize: TSizeF;
begin
  Result := AInsets;
  if (not FVirtualKeyboardBounds.IsEmpty) and (AInsets <> TRectF.Empty) then
  begin
    LScreenSize := Screen.Size;
    // Check if virtual keyboard is in bottom
    if SameValue(LScreenSize.Height, FVirtualKeyboardBounds.Bottom, TEpsilon.Position) and (FVirtualKeyboardBounds.Left = 0) and
      SameValue(FVirtualKeyboardBounds.Right, LScreenSize.Width, TEpsilon.Position) then
    begin
      // Removing bottom system bars
      Result.Bottom := Max(Result.Bottom - FVirtualKeyboardBounds.Height, 0);
    end;
  end;
end;

procedure TMobileBarsServiceiOS.SetNavigationBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
begin
end;

procedure TMobileBarsServiceiOS.SetStatusBarBackgroundColor(
  const AForm: TCommonCustomForm; const AColor: TAlphaColor);
begin
  FDefaultStatusBarService.SetBackgroundColor(AForm, AColor);
end;

procedure TMobileBarsServiceiOS.SetVisibility(const AForm: TCommonCustomForm;
  const AMode: TFormSystemStatusBar.TVisibilityMode);
var
  LWindowService: IFMXWindowService;
begin
  if Assigned(AForm) then
  begin
    FDefaultStatusBarService.SetVisibility(AForm, AMode);
    if AForm.Active and TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, LWindowService) then
      LWindowService.ShowWindow(AForm); // Force update
    TMessageManager.DefaultManager.SendMessage(AForm,
      TFormMobileBars.TInsetsChangeMessage.Create(GetInsets(AForm), GetTappableInsets(AForm)));
  end;
end;

procedure TMobileBarsServiceiOS.VirtualKeyboardChangeHandler(
  const ASender: TObject; const AMessage: TMessage);
begin
  if AMessage is TVKStateChangeMessage then
  begin
    FVirtualKeyboardBounds := TVKStateChangeMessage(AMessage).KeyboardBounds;
    if Assigned(Screen) then
      CheckInsetsChanges(Screen.ActiveForm);
  end;
end;

initialization
  FMobileBarsServiceiOS := TMobileBarsServiceiOS.Create;
finalization
  FMobileBarsServiceiOS.Free;
{$ELSE}
implementation
{$ENDIF}
end.
