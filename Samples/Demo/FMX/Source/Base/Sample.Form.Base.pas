{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Base;

interface

{$SCOPEDENUMS ON}
{$IF (CompilerVersion >= 33) and (CompilerVersion <= 35)} // RAD Studio 10.3 Rio to 11 Alexandria
  {$DEFINE AdvancedSystemBars}
{$ENDIF}

uses
  { Delphi }
  System.SysUtils, System.UITypes, System.Classes, System.Generics.Collections,
  System.Math.Vectors, FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts,
  FMX.Dialogs, FMX.Objects, FMX.Graphics, FMX.StdCtrls, System.IOUtils,
  FMX.ListBox, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia;

type
  { TScrollBox }

  TScrollBox = class(FMX.Layouts.TScrollBox)
  protected
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
  end;

  { TfrmBase }

  TfrmBase = class(TForm)
    rctContent: TRectangle;
    rctHeader: TRectangle;
    lblTitle: TSkLabel;
    svgBackArrow: TSkSvg;
    rctTip: TRectangle;
    lblTipDescription: TSkLabel;
    svgTipIcon: TSkSvg;
    lytTipContent: TLayout;
    sbxContent: TScrollBox;
    btnBack: TSpeedButton;
    rctGestureBar: TRectangle;
    rctNavigationBar: TRectangle;
    rctStatusBar: TRectangle;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure rctContentResized(Sender: TObject);
  private
    class var
      FCreatedFormsList: TList<TfrmBase>;
      FShowingFormsList: TList<TfrmBase>;
    class function GetAssetsPath: string; static;
    class function GetOutputPath: string; static;
  private
    FAllowScrollBoundsAnimation: Boolean;
    function CreateForm<T: TForm>: T;
    procedure SetAllowScrollBoundsAnimation(const AValue: Boolean);
    {$IFDEF AdvancedSystemBars}
    procedure SystemBarsInsetsChange(ASender: TObject);
    {$ENDIF}
  protected
    class procedure CloseForm(const AForm: TfrmBase); static;
    class constructor Create;
    class destructor Destroy;
    class function FormBackgroundColor: TAlphaColor; virtual;
    class function FormBorderColor: TAlphaColor; static;
    class function GetCurrentForm: TfrmBase; static;
    class property AssetsPath: string read GetAssetsPath;
    class property OutputPath: string read GetOutputPath;
  protected
    function ChildForm<T: TForm>: T;
    procedure DoShow; override;
    procedure DoSystemBarsInsetsChange; virtual;
    function HasBottomContent: Boolean; virtual;
    {$IF CompilerVersion >= 34}
    procedure PaintBackground; override;
    {$ENDIF}
    procedure ShowMessage(const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show; reintroduce;
    property AllowScrollBoundsAnimation: Boolean read FAllowScrollBoundsAnimation write SetAllowScrollBoundsAnimation;
  end;

  {$IF CompilerVersion < 33}
  { TSkEpsilonHelper }

  TSkEpsilonHelper = record helper for TEpsilon
  const
    Scale = 1E-4;
    FontSize = 1E-2;
    Position = 1E-3;
  end;
  {$ENDIF}

implementation

uses
  {$IFDEF AdvancedSystemBars}
  { Third party }
  MobileBars,
  {$ENDIF}

  { Delphi }
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ENDIF}
  System.Rtti;

{$R *.fmx}

type
  TControlAccess = class(TControl);

{ TfrmBase }

function TfrmBase.ChildForm<T>: T;
var
  LSelfIndex: Integer;
begin
  Assert(T.InheritsFrom(TfrmBase));
  LSelfIndex := FCreatedFormsList.IndexOf(Self);
  if (LSelfIndex >= 0) and (LSelfIndex < FCreatedFormsList.Count - 1) and (FCreatedFormsList[LSelfIndex + 1].ClassType = T) then
    Exit(T(FCreatedFormsList[LSelfIndex + 1]));
  Result := CreateForm<T>;
  TfrmBase(Result).rctContent.Align := TAlignLayout.Client;
  TfrmBase(Result).btnBack.Visible := FShowingFormsList.Count > 0;
  TfrmBase(Result).rctGestureBar.Height := rctGestureBar.Height;
  FCreatedFormsList.Add(TfrmBase(Result));
end;

procedure TfrmBase.btnBackClick(Sender: TObject);
begin
  {$REGION ' - Workaround RSP-36959'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   This code is a workaround intended to fix an access violation that
  // -   occurs in iOS when the TComboBox is detroyed when its DroppedDown is
  // -   true, that is, with the picker of TComboBox opened.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-36959
  // -
  // - -------------------------------------------------------------------------
  {$IF CompilerVersion > 35.0}
    {$MESSAGE WARN 'Check if the issue has been fixed'}
  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$IFDEF iOS}
  if (Application.MainForm.Focused is TCustomComboBox) and TCustomComboBox(Application.MainForm.Focused).DroppedDown then
  begin
    Application.MainForm.Focused := nil;
      TThread.CreateAnonymousThread(
        procedure
        begin
          Sleep(500);
          TThread.Queue(nil,
            procedure
            begin
              btnBackClick(nil);
            end);
        end).Start;
    Exit;
  end;
  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$ENDREGION}

  {$IF CompilerVersion >= 32}
  TThread.ForceQueue(nil,
    procedure
    begin
      CloseForm(Self);
    end);
  {$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        begin
          CloseForm(Self);
        end);
    end).Start;
  {$ENDIF}
end;

class procedure TfrmBase.CloseForm(const AForm: TfrmBase);
var
  LFormIndex: Integer;
  LAction: TCloseAction;
  I: Integer;
begin
  LFormIndex := FShowingFormsList.IndexOf(AForm);
  if LFormIndex < 0 then
    Exit;
  LAction := TCloseAction.caFree;
  AForm.DoClose(LAction);
  if LAction = TCloseAction.caNone then
    Exit;
  if LFormIndex = 0 then
    Application.Terminate
  else
  begin
    (Application.MainForm as IRoot).SetCaptured(nil);
    LFormIndex := FCreatedFormsList.IndexOf(AForm);
    Assert(LFormIndex > -1);
    for I := FCreatedFormsList.Count - 1 downto LFormIndex do
    begin
      FShowingFormsList.Remove(FCreatedFormsList[I]);
      FCreatedFormsList[I].DisposeOf;
      FCreatedFormsList.Delete(I);
    end;
    FShowingFormsList.Last.rctContent.Enabled := True;
    FShowingFormsList.Last.rctContent.Visible := True;
    FShowingFormsList.Last.rctContent.BringToFront;
    if FShowingFormsList.Count > 1 then
      FShowingFormsList[FShowingFormsList.Count - 2].rctContent.Visible := True;
    if FShowingFormsList.Last = Application.MainForm then
      Application.MainForm.BiDiMode := Application.BiDiMode
    else
      Application.MainForm.BiDiMode := FShowingFormsList.Last.BiDiMode;
  end;
end;

constructor TfrmBase.Create(AOwner: TComponent);
begin
  FAllowScrollBoundsAnimation := True;
  inherited Create(AOwner);
end;

function TfrmBase.CreateForm<T>: T;
{$IF CompilerVersion < 34}
var
  LRttiContext: TRttiContext;
begin
  LRttiContext := TRttiContext.Create;
  try
    Result := LRttiContext.GetType(TClass(T)).GetMethod('Create').Invoke(TClass(T), [TValue.From(Application)]).AsType<T>;
  finally
    LRttiContext.Free;
  end;
{$ELSE}
begin
  Result := T.Create(Application);
{$ENDIF}
end;

class constructor TfrmBase.Create;
begin
  FCreatedFormsList := TList<TfrmBase>.Create;
  FShowingFormsList := TList<TfrmBase>.Create;
end;

class destructor TfrmBase.Destroy;
begin
  FShowingFormsList.Free;
  FCreatedFormsList.Free;
end;

procedure TfrmBase.DoShow;
begin
  rctTip.Visible := not lblTipDescription.Text.IsEmpty;
  if rctTip.Visible then
    rctTip.Position.Y := -100;
  if Self = Application.MainForm then
  begin
    btnBack.Visible := False;
    FShowingFormsList.Add(Self);
    {$IFDEF AdvancedSystemBars}
      {$IFDEF ANDROID}
      MobileBars.StatusBarBackgroundColor := $00FFFFFF;
      if TOSVersion.Check(7) then
        MobileBars.NavigationBarBackgroundColor := $00FFFFFF
      else
        MobileBars.NavigationBarBackgroundColor := $7F000000;
      MobileBars.Visibility := TFormSystemStatusBar.TVisibilityMode.VisibleAndOverlap;
      {$ELSEIF DEFINED(IOS)}
      MobileBars.StatusBarBackgroundColor := FormBorderColor;
      MobileBars.NavigationBarBackgroundColor := MobileBars.StatusBarBackgroundColor;
      {$ENDIF}
    MobileBars.OnInsetsChange := SystemBarsInsetsChange;
    {$ENDIF}
    DoSystemBarsInsetsChange;
  end
  else
  begin
    DoSystemBarsInsetsChange;
    if Assigned(Application.MainForm) then
    begin
      //StyleBook := Application.MainForm.StyleBook;
      Application.MainForm.BiDiMode := BiDiMode;
    end;
    if FShowingFormsList.Count > 0 then
    begin
      FShowingFormsList.Last.rctContent.Visible := False;
      TControlAccess(FShowingFormsList.Last.rctContent).DisabledOpacity := 1;
      FShowingFormsList.Last.rctContent.Enabled := False;
      if FShowingFormsList.Count > 1 then
        FShowingFormsList[FShowingFormsList.Count - 2].rctContent.Visible := False;
    end;
    FShowingFormsList.Add(Self);
    rctContent.Parent := Application.MainForm;
    rctContent.BringToFront;
    if FShowingFormsList.Count > 1 then
      FShowingFormsList[FShowingFormsList.Count - 2].rctContent.Visible := True;
  end;
  inherited;
  rctContentResized(nil);
end;

procedure TfrmBase.DoSystemBarsInsetsChange;
begin
  {$IFDEF AdvancedSystemBars}
  rctStatusBar.Height := Application.MainForm.MobileBars.Insets.Top;
  rctNavigationBar.Height := Application.MainForm.MobileBars.TappableInsets.Bottom;
  if HasBottomContent then
    rctGestureBar.Height := Application.MainForm.MobileBars.Insets.Bottom -
      Application.MainForm.MobileBars.TappableInsets.Bottom
  else
    rctGestureBar.Height := 0;
  {$ELSE}
  rctStatusBar.Height := 0;
  rctNavigationBar.Height := 0;
  rctGestureBar.Height := 0;
  {$ENDIF}
end;

class function TfrmBase.FormBackgroundColor: TAlphaColor;
begin
  {$IFDEF MSWINDOWS}
  Result := $FFFBFBFB; // Mica material
  {$ELSE}
  Result := $FFF2F2F7;
  {$ENDIF}
end;

class function TfrmBase.FormBorderColor: TAlphaColor;
begin
  {$IFDEF MSWINDOWS}
  Result := $FFF2F3F6; // Mica material
  {$ELSE}
  Result := TAlphaColors.White;
  {$ENDIF}
end;

procedure TfrmBase.FormCreate(Sender: TObject);
begin
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  rctHeader.Height := 48;
  {$ENDIF}
  Fill.Color := FormBackgroundColor;
  rctContent.Fill.Color := FormBackgroundColor;
  rctHeader.Fill.Color := FormBorderColor;
  rctTip.Fill.Color := FormBorderColor;
end;

procedure TfrmBase.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkEscape, vkBack, vkHardwareBack:
      if (GetCurrentForm <> Application.MainForm) and GetCurrentForm.btnBack.ParentedVisible then
      begin
        CloseForm(GetCurrentForm);
        Key := 0;
      end;
  else
  end;
end;

class function TfrmBase.GetAssetsPath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.GetFullPath('..\..\..\..\Assets\');
  {$ELSEIF DEFINED(IOS) or DEFINED(ANDROID)}
  Result := TPath.GetDocumentsPath;
  {$ELSEIF defined(MACOS)}
  Result := TPath.GetFullPath('../Resources/');
  {$ELSE}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

class function TfrmBase.GetCurrentForm: TfrmBase;
begin
  Result := FShowingFormsList.Last;
end;

class function TfrmBase.GetOutputPath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := ExtractFilePath(ParamStr(0));
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
  {$ELSEIF DEFINED(MACOS) and NOT DEFINED(IOS)}
  Result := TPath.GetTempPath;
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
  {$ELSE}
  Result := GetAssetsPath;
  {$ENDIF}
end;

function TfrmBase.HasBottomContent: Boolean;
begin
  Result := rctTip.Visible;
end;

{$IF CompilerVersion >= 34}
procedure TfrmBase.PaintBackground;
begin
  // Specific workaround of this project to a known issue (RSP-34435)
  if (not (csDesigning in ComponentState)) and (Fill.Kind = TBrushKind.Solid) and not Transparency then
    Canvas.Clear(Fill.Color)
  else
    inherited;
end;
{$ENDIF}

procedure TfrmBase.rctContentResized(Sender: TObject);
begin
  if rctTip.Visible and (rctTip.Height <> lblTipDescription.Height + 16) then
  begin
    rctTip.Height := lblTipDescription.Height + 16;
    {$IF CompilerVersion >= 32}
    TThread.ForceQueue(nil,
      procedure
      begin
        TControlAccess(rctTip).Realign;
        TControlAccess(rctContent).Realign;
      end);
    {$ELSE}
    TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Queue(nil,
          procedure
          begin
            TControlAccess(rctTip).Realign;
            TControlAccess(rctContent).Realign;
          end);
      end).Start;
    {$ENDIF}
  end;
end;

procedure TfrmBase.SetAllowScrollBoundsAnimation(const AValue: Boolean);
begin
  if FAllowScrollBoundsAnimation <> AValue then
  begin
    FAllowScrollBoundsAnimation := AValue;
    sbxContent.UpdateAniCalculations;
  end;
end;

procedure TfrmBase.Show;
begin
  DoShow;
end;

procedure TfrmBase.ShowMessage(const AMessage: string);
begin
  {$IF CompilerVersion >= 31}
  TDialogService.MessageDialog(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
  {$ELSE}
  MessageDlg(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
  {$ENDIF}
end;

{$IFDEF AdvancedSystemBars}
procedure TfrmBase.SystemBarsInsetsChange(ASender: TObject);
var
  I: Integer;
begin
  DoSystemBarsInsetsChange;
  for I := 0 to FCreatedFormsList.Count - 1 do
    FCreatedFormsList[I].DoSystemBarsInsetsChange;
end;
{$ENDIF}

{ TScrollBox }

procedure TScrollBox.DoUpdateAniCalculations(
  const AAniCalculations: TScrollCalculations);
begin
  inherited;
  AAniCalculations.BoundsAnimation := AAniCalculations.BoundsAnimation and TfrmBase(Owner).AllowScrollBoundsAnimation;
end;

end.
