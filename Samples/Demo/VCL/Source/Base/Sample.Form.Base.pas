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

uses
  { Delphi }
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Types,
  System.UITypes, System.IOUtils, System.Generics.Collections, System.Math.Vectors,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Themes,

  { Skia }
  System.Skia, Vcl.Skia;

type
  { TScrollBox }

  TScrollBox = class(Vcl.Forms.TScrollBox)
  protected
    procedure WMEraseBkgnd(var AMessage: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMHScroll(var AMessage: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var AMessage: TWMVScroll); message WM_VSCROLL;
  end;

  { TfrmBase }

  TfrmBase = class(TForm)
    pnlTitle: TPanel;
    lblTitle: TSkLabel;
    pnlTitleLine: TPanel;
    sbxContent: TScrollBox;
    svgBackArrow: TSkSvg;
    pnlContent: TPanel;
    pnlTip: TPanel;
    svgTipIcon: TSkSvg;
    pnlTipLine: TPanel;
    pnlTipContent: TPanel;
    lblTipDescription: TSkLabel;
    pnlBack: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure pnlBackClick(Sender: TObject);
    procedure pnlContentResize(Sender: TObject);
  private
    class var
      FCreatedFormsList: TList<TfrmBase>;
      FShowingFormsList: TList<TfrmBase>;
    class function GetAssetsPath: string; static;
    class function GetOutputPath: string; static;
    procedure CMBiDiModeChanged(var AMessage: TMessage); message CM_BIDIMODECHANGED;
    function CreateForm<T: TForm>: T;
  protected
    class procedure CloseForm(const AForm: TfrmBase); static;
    class constructor Create;
    class destructor Destroy;
    class function FormBackgroundColor: TColor; virtual;
    class function FormBorderColor: TColor; static;
    class function GetCurrentForm: TfrmBase; static;
    class property AssetsPath: string read GetAssetsPath;
    class property OutputPath: string read GetOutputPath;
  protected
    procedure BeginUpdate;
    function ChildForm<T: TForm>: T;
    procedure DoShow; override;
    procedure EndUpdate;
    procedure ScrollBoxChanged(ASender: TObject); virtual;
    procedure ScrollBoxEraseBackground(ASender: TObject; const ADC: HDC); virtual;
    procedure ShowMessage(const AMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Show; reintroduce;
    function ShowModal: Integer; override;
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
  { Delphi }
  System.Rtti;

{$R *.dfm}

{ TfrmBase }

procedure TfrmBase.BeginUpdate;
begin
  SendMessage(Application.MainForm.Handle, WM_SETREDRAW, Integer(False), 0);
  Application.MainForm.DisableAlign;
end;

function TfrmBase.ChildForm<T>: T;
var
  LSelfIndex: Integer;
begin
  Assert(T.InheritsFrom(TfrmBase));
  LSelfIndex := FCreatedFormsList.IndexOf(Self);
  if (LSelfIndex >= 0) and (LSelfIndex < FCreatedFormsList.Count - 1) and (FCreatedFormsList[LSelfIndex + 1].ClassType = T) then
    Exit(T(FCreatedFormsList[LSelfIndex + 1]));
  Result := CreateForm<T>;
  TfrmBase(Result).pnlContent.Align := TAlign.alClient;
  TfrmBase(Result).pnlBack.Visible := FShowingFormsList.Count > 0;
  FCreatedFormsList.Add(TfrmBase(Result));
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
    LFormIndex := FCreatedFormsList.IndexOf(AForm);
    Assert(LFormIndex > -1);
    TfrmBase(Application.MainForm).BeginUpdate;
    try
      for I := FCreatedFormsList.Count - 1 downto LFormIndex do
      begin
        FCreatedFormsList[I].Free;
        FShowingFormsList.Remove(FCreatedFormsList[I]);
        FCreatedFormsList.Delete(I);
      end;
      FShowingFormsList.Last.pnlContent.Visible := True;
    finally
      TfrmBase(Application.MainForm).EndUpdate;
    end;
  end;
end;

procedure TfrmBase.CMBiDiModeChanged(var AMessage: TMessage);
begin
  inherited;
  pnlContent.BiDiMode := BiDiMode;
  pnlContent.ParentBiDiMode := False;
end;

class constructor TfrmBase.Create;
begin
  FCreatedFormsList := TList<TfrmBase>.Create;
  FShowingFormsList := TList<TfrmBase>.Create;
end;

constructor TfrmBase.Create(AOwner: TComponent);
begin
  if Application.MainForm = nil then
    TStyleManager.TrySetStyle('Windows11 Modern Light', False);
  inherited;
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

class destructor TfrmBase.Destroy;
begin
  FShowingFormsList.Free;
  FCreatedFormsList.Free;
end;

procedure TfrmBase.DoShow;
var
  LPreventUpdates: Boolean;
begin
  LPreventUpdates := Assigned(Application.MainForm) and Application.MainForm.Active;
  if LPreventUpdates then
    BeginUpdate;
  try
    pnlTip.Visible := not lblTipDescription.Caption.IsEmpty;
    if Self = Application.MainForm then
    begin
      pnlBack.Visible := False;
      FShowingFormsList.Add(Self);
    end
    else
    begin
      if FShowingFormsList.Count > 0 then
        FShowingFormsList.Last.pnlContent.Visible := False;
      FShowingFormsList.Add(Self);
      pnlContent.Parent := Application.MainForm;
    end;
    inherited;
    pnlContentResize(nil);
  finally
    if LPreventUpdates then
      EndUpdate;
  end;
end;

procedure TfrmBase.EndUpdate;
begin
  Application.MainForm.EnableAlign;
  SendMessage(Application.MainForm.Handle, WM_SETREDRAW, Integer(True), 0);
  RedrawWindow(Application.MainForm.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;

class function TfrmBase.FormBackgroundColor: TColor;
begin
  Result := $00FBFBFB; // Mica material
end;

class function TfrmBase.FormBorderColor: TColor;
begin
  Result := $00F6F3F2; // Mica material
end;

procedure TfrmBase.FormCreate(Sender: TObject);
begin
  Color := FormBackgroundColor;
  pnlContent.Color := FormBackgroundColor;
  sbxContent.Color := FormBackgroundColor;
  pnlTitle.Color := FormBorderColor;
  pnlTip.Color := FormBorderColor;
end;

procedure TfrmBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkEscape, vkBack, vkHardwareBack:
      if (GetCurrentForm <> Application.MainForm) and GetCurrentForm.pnlBack.Showing then
      begin
        CloseForm(GetCurrentForm);
        Key := 0;
      end;
  else
  end;
end;

procedure TfrmBase.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Self <> GetCurrentForm then
    GetCurrentForm.DoMouseWheel(Shift, WheelDelta, MousePos)
  else
  begin
    sbxContent.VertScrollBar.Position := sbxContent.VertScrollBar.Position - WheelDelta;
    Handled := True;
  end;
end;

class function TfrmBase.GetAssetsPath: string;
begin
  Result := TPath.GetFullPath('..\..\..\..\Assets\');
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

class function TfrmBase.GetCurrentForm: TfrmBase;
begin
  Result := FShowingFormsList.Last;
end;

class function TfrmBase.GetOutputPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if (Result <> '') and not Result.EndsWith(PathDelim) then
    Result := Result + PathDelim;
end;

procedure TfrmBase.pnlBackClick(Sender: TObject);
begin
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

procedure TfrmBase.pnlContentResize(Sender: TObject);
begin
  if pnlTip.Visible then
    pnlTip.Height := lblTipDescription.Height + 16;
end;

procedure TfrmBase.ScrollBoxChanged(ASender: TObject);
begin
  TScrollBox(ASender).Repaint;
end;

procedure TfrmBase.ScrollBoxEraseBackground(ASender: TObject; const ADC: HDC);
begin
end;

procedure TfrmBase.Show;
begin
  DoShow;
end;

procedure TfrmBase.ShowMessage(const AMessage: string);
begin
  MessageDlg(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

function TfrmBase.ShowModal: Integer;
begin
  Show;
  Result := 0;
end;

{ TScrollBox }

procedure TScrollBox.WMEraseBkgnd(var AMessage: TWMEraseBkgnd);
begin
  inherited;
  if Owner is TfrmBase then
    TfrmBase(Owner).ScrollBoxEraseBackground(Self, AMessage.DC);
end;

procedure TScrollBox.WMHScroll(var AMessage: TWMHScroll);
begin
  inherited;
  if Owner is TfrmBase then
    TfrmBase(Owner).ScrollBoxChanged(Self);
end;

procedure TScrollBox.WMVScroll(var AMessage: TWMVScroll);
begin
  inherited;
  if Owner is TfrmBase then
    TfrmBase(Owner).ScrollBoxChanged(Self);
end;

end.
