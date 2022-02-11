// **************************************************************************************************
//
// Unit Vcl.Styles.InnoSetup
// unit for the VCL Styles Plugin for Inno Setup
// https://github.com/RRUZ/vcl-styles-plugins
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is  Vcl.Styles.InnoSetup.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.InnoSetup;

interface

Procedure Done;

implementation

{
  TODO:

  TNewEdit = class(TEdit)            ok
  TEdit                              ok
  TPasswordEdit                      ok
  TNewMemo = class(TMemo)            ok
  TNewComboBox = class(TComboBox)    ok
  TNewListBox = class(TListBox)      ok
  TListBox                           ok
  TNewButton = class(TButton)        ok
  TNewCheckBox = class(TCheckBox)    ok
  TNewRadioButton = class(TRadioButton)
  TSelectFolderForm                  ok
  TFolderTreeView                    ok
  TStartMenuFolderTreeView           ok
  TRichEditViewer                    ok
  TNewStaticText                     ok
  TNewNotebook                       ok
  TNewNotebookPage                   ok
  TPanel                             ok
}

{ .$DEFINE USEGENERICS }   // -->Reduce the final exe/dll size

uses
  DDetours,
  Winapi.Windows,
  Winapi.CommDlg,
  Winapi.Messages,
{$IFDEF USEGENERICS}
  System.Generics.Collections,
{$ENDIF}
  System.SysUtils,
  System.Classes,
{$IFDEF DEBUG}
  System.IOUtils,
{$ENDIF}
  Vcl.Themes,
  Vcl.Styles.InnoSetup.StyleHooks,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.ComCtrls,
  Vcl.Styles.Utils.StdCtrls, uLogExcept;

type
  TThemedInnoControls = class
  private
    class var FHook_WH_CALLWNDPROC: HHook;
    class var FHook_WH_CBT: HHook;
  protected
    class function HookActionCallBackWndProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall; static;
    class function HookActionCallBackCBT(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT; stdcall; static;
    procedure InstallHook;
    procedure RemoveHook;
    procedure InstallHook_WH_CBT;
    procedure RemoveHook_WH_CBT;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

{$IFNDEF USEGENERICS}

  TDictionary = class
  private
    FKeys, FValues: TList;
  public
    procedure Add(hwnd: hwnd; StyleHook: TSysStyleHook);
    function ContainsKey(hwnd: Winapi.Windows.hwnd): Boolean;
    constructor Create; overload;
    destructor Destroy; override;
  end;
{$ENDIF}

var
{$IFDEF USEGENERICS}
  InnoSetupControlsList: TObjectDictionary<hwnd, TSysStyleHook>;
{$ELSE}
  InnoSetupControlsList: TDictionary;
{$ENDIF}
  ClassesList: TStrings; // use a  TStrings to avoid the use of generics
  ThemedInnoControls: TThemedInnoControls;


{$IFNDEF USEGENERICS}
{ TDictionary }

procedure TDictionary.Add(hwnd: hwnd; StyleHook: TSysStyleHook);
begin
  FKeys.Add(Pointer(hwnd));
  FValues.Add(StyleHook);
end;

function TDictionary.ContainsKey(hwnd: Winapi.Windows.hwnd): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FKeys.Count - 1 do
    if Winapi.Windows.hwnd(FKeys[i]) = hwnd then
      Exit(True);
end;

constructor TDictionary.Create;
begin
  FKeys := TList.Create;
  FValues := TList.Create;
end;

destructor TDictionary.Destroy;
var
  i: Integer;
begin
  FKeys.Free;
  for i := 0 to FValues.Count - 1 do
    TSysStyleHook(FValues[i]).Free;

  FValues.Free;
  inherited;
end;
{$ENDIF}
{ TThemedSysControls }

constructor TThemedInnoControls.Create;
begin
  inherited;
  FHook_WH_CALLWNDPROC := 0;
  InstallHook;
  InstallHook_WH_CBT;
{$IFDEF USEGENERICS}
  InnoSetupControlsList := TObjectDictionary<hwnd, TSysStyleHook>.Create([doOwnsValues]);
{$ELSE}
  InnoSetupControlsList := TDictionary.Create;
{$ENDIF}
  ClassesList := TStringList.Create;
end;

destructor TThemedInnoControls.Destroy;
begin
  RemoveHook;
  RemoveHook_WH_CBT;
  InnoSetupControlsList.Free;
  ClassesList.Free;
  inherited;
end;

class function TThemedInnoControls.HookActionCallBackCBT(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT;
var
  LHWND: hwnd;
  LClassName: string;
begin
  if (StyleServices.Enabled) and not(StyleServices.IsSystemStyle) then
    case nCode of
      HCBT_ACTIVATE:
        begin
          //TLogFile.Add('HookActionCallBackCBT');
          LHWND := hwnd(wParam);
          if (LHWND > 0) then
          begin
            LClassName := GetWindowClassName(LHWND);
            if (LClassName <> '') and (not TSysStyleManager.SysStyleHookList.ContainsKey(LHWND))
            { and SameText(LClassName,'#32770') } then
            begin
              TSysStyleManager.AddControlDirectly(LHWND, LClassName);
              InvalidateRect(LHWND, nil, False);
            end;
          end;
        end;
    end;
  Result := CallNextHookEx(TThemedInnoControls.FHook_WH_CBT, nCode, wParam, lParam);
end;

class function TThemedInnoControls.HookActionCallBackWndProc(nCode: Integer; wParam: wParam; lParam: lParam): LRESULT;
var
  C: array [0 .. 256] of Char;
  sClassName: string;
begin
  Result := CallNextHookEx(FHook_WH_CALLWNDPROC, nCode, wParam, lParam);
  try
    if (nCode < 0) then
      Exit;

    if (StyleServices.Enabled) and not(StyleServices.IsSystemStyle) then
    begin

      if ClassesList.IndexOfName(IntToStr(PCWPStruct(lParam)^.hwnd)) = -1 then
      begin
        GetClassName(PCWPStruct(lParam)^.hwnd, C, 256);
        // Addlog('GetClassName ' + C);
        ClassesList.Add(Format('%d=%s', [PCWPStruct(lParam)^.hwnd, C]));
      end;

      if ClassesList.IndexOfName(IntToStr(PCWPStruct(lParam)^.hwnd)) >= 0 then
      begin
        sClassName := ClassesList.Values[IntToStr(PCWPStruct(lParam)^.hwnd)];
        // ClassesList[PCWPStruct(lParam)^.hwnd];

{$IFDEF DEBUG}
        // if (SameText(sClassName,'LiteUI_Label')) then
        // Addlog(sClassName+' '+WM_To_String(PCWPStruct(lParam)^.message)+
        // ' WParam '+IntToHex(PCWPStruct(lParam)^.wParam, 8) +
        // ' lParam '+IntToHex(PCWPStruct(lParam)^.lParam, 8) +
        // ' hwnd: '+ IntToHex(PCWPStruct(lParam)^.hwnd, 8) +
        // ' WNDPROC: ' + IntToHex(GetWindowLongPtr(PCWPStruct(lParam)^.hwnd, GWL_WNDPROC), 8 )
        // );
{$ENDIF}
        if SameText(sClassName, 'TNewButton') then
        begin
          if (PCWPStruct(lParam)^.message = WM_CREATE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNewButtonStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else

          if SameText(sClassName, 'TWizardForm') or SameText(sClassName, 'TSetupForm') or
          SameText(sClassName, 'TSelectFolderForm') or SameText(sClassName, 'TSelectLanguageForm') or
          SameText(sClassName, 'TUninstallProgressForm') then
        begin
          if (PCWPStruct(lParam)^.message = WM_NCCALCSIZE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TWizardFormStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else if SameText(sClassName, 'TNewComboBox') then
        begin
          if (PCWPStruct(lParam)^.message = WM_CREATE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysComboBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else if SameText(sClassName, 'TNewCheckBox') then
        begin
          if (PCWPStruct(lParam)^.message = WM_CREATE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysCheckBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else if SameText(sClassName, 'TNewRadioButton') then
        begin
          if (PCWPStruct(lParam)^.message = WM_CREATE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd,
              TSysRadioButtonStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else if SameText(sClassName, 'TEdit') or SameText(sClassName, 'TNewEdit') or
          SameText(sClassName, 'TPasswordEdit') then
        begin
          if (PCWPStruct(lParam)^.message = WM_CREATE) and
            not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysEditStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else // TSysScrollingStyleHook.PaintNC<>TScrollingStyleHook.PaintNC
          if SameText(sClassName, 'TNewMemo') or SameText(sClassName, 'TMemo') then
          begin
            if (PCWPStruct(lParam)^.message = WM_NCCALCSIZE) and
              not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            begin
              InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNewMemoStyleHook.Create(PCWPStruct(lParam)^.hwnd));
            end;
          end
          else if SameText(sClassName, 'TNewListBox') or SameText(sClassName, 'TListBox') then
          begin
            if (PCWPStruct(lParam)^.message = WM_NCCALCSIZE) and
              not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd,
                TNewListBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
          end
          else if SameText(sClassName, 'TNewCheckListBox') then
          begin
            if (PCWPStruct(lParam)^.message = WM_NCCALCSIZE) and
              not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd,
                TNewCheckListBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
          end
          else if SameText(sClassName, 'TRichEditViewer') then
          begin
            if (PCWPStruct(lParam)^.message = WM_NCCALCSIZE) and
              not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd,
                TRichEditViewerStyleHook.Create(PCWPStruct(lParam)^.hwnd));
          end
          else
            // if SameText(sClassName,'TNewStaticText') then
            // begin
            // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TStaticTextWnd.Create(PCWPStruct(lParam)^.hwnd));
            // end
            // else
            // if (SameText(sClassName,'TNewProgressBar')) then  The Vcl.Styles.Hooks unit is used to paint the progressbar.
            // begin
            // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
            // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysProgressBarStyleHook.Create(PCWPStruct(lParam)^.hwnd));
            // end
            // else
            if (SameText(sClassName, 'TStartMenuFolderTreeView')) or (SameText(sClassName, 'TFolderTreeView')) then
            begin
              if (PCWPStruct(lParam)^.message = WM_CREATE) and
                not(InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
                InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd,
                  TFolderTreeViewStyleHook.Create(PCWPStruct(lParam)^.hwnd));
            end
            else
              // if (SameText(sClassName,'LiteUI_Panel')) then
              // begin
              // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TPanelComponentStyleHook.Create(PCWPStruct(lParam)^.hwnd));
              // end
              // else
              // if (SameText(sClassName,'LiteUI_Label')) then
              // begin
              // if (PCWPStruct(lParam)^.message=WM_NCCREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TLabelComponentStyleHook.Create(PCWPStruct(lParam)^.hwnd));
              // end
              // else
              // if (SameText(sClassName,'TNewNotebook')) then     //TNewNotebook is handled by the Getsyscolors hook
              // begin
              // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNotebookWnd.Create(PCWPStruct(lParam)^.hwnd));
              // end
              // else
              // if (SameText(sClassName,'TNewNotebookPage')) then   //TNewNotebookPage is handled by the Getsyscolors hook
              // begin
              // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysStyleHook.Create(PCWPStruct(lParam)^.hwnd));
              // end
              // else
              // if (SameText(sClassName,'TPanel')) then   //TPanel is handled by the Getsyscolors hook
              // begin
              // if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
              // InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TPanelWnd.Create(PCWPStruct(lParam)^.hwnd));
              // end;
      end;
    end;
  except
    on e: Exception do
      TLogFile.Add(e);
  end;

end;

procedure TThemedInnoControls.InstallHook;
begin
  FHook_WH_CALLWNDPROC := SetWindowsHookEx(WH_CALLWNDPROC, @TThemedInnoControls.HookActionCallBackWndProc, 0,
    GetCurrentThreadId);
end;

procedure TThemedInnoControls.InstallHook_WH_CBT;
begin
  FHook_WH_CBT := SetWindowsHookEx(WH_CBT, @TThemedInnoControls.HookActionCallBackCBT, 0, GetCurrentThreadId);
end;

procedure TThemedInnoControls.RemoveHook;
begin
  if FHook_WH_CALLWNDPROC <> 0 then
    UnhookWindowsHookEx(FHook_WH_CALLWNDPROC);
end;

procedure TThemedInnoControls.RemoveHook_WH_CBT;
begin
  if FHook_WH_CBT <> 0 then
    UnhookWindowsHookEx(FHook_WH_CBT);
end;

Procedure Done;
begin
  if Assigned(ThemedInnoControls) then
  begin
    ThemedInnoControls.Free;
    ThemedInnoControls := nil;
  end;
end;

const
  commdlg32 = 'comdlg32.dll';

var
  TrampolineGetOpenFileNameW: function(var OpenFile: TOpenFilenameW): Bool; stdcall;
  TrampolineGetOpenFileNameA: function(var OpenFile: TOpenFilenameA): Bool; stdcall;

function DialogHook(Wnd: hwnd; msg: UINT; wParam: wParam; lParam: lParam): UINT_PTR; stdcall;
begin
  Exit(0);
end;

function DetourGetOpenFileNameW(var OpenFile: TOpenFilename): Bool; stdcall;
begin
  OpenFile.lpfnHook := @DialogHook;
  OpenFile.Flags := OpenFile.Flags or OFN_ENABLEHOOK or OFN_EXPLORER;
  Exit(TrampolineGetOpenFileNameW(OpenFile));
end;

function DetourGetOpenFileNameA(var OpenFile: TOpenFilenameA): Bool; stdcall;
begin
  OpenFile.lpfnHook := @DialogHook;
  OpenFile.Flags := OpenFile.Flags or OFN_ENABLEHOOK or OFN_EXPLORER;
  Exit(TrampolineGetOpenFileNameA(OpenFile));
end;

procedure HookFileDialogs;
begin
  @TrampolineGetOpenFileNameW := InterceptCreate(commdlg32, 'GetOpenFileNameW', @DetourGetOpenFileNameW);
  @TrampolineGetOpenFileNameA := InterceptCreate(commdlg32, 'GetOpenFileNameA', @DetourGetOpenFileNameA);
end;

procedure UnHookFileDialogs;
begin
  InterceptRemove(@TrampolineGetOpenFileNameW);
  InterceptRemove(@TrampolineGetOpenFileNameA);
end;

initialization

ThemedInnoControls := nil;
if StyleServices.Available then
begin
  ThemedInnoControls := TThemedInnoControls.Create;
  TSysStyleManager.HookVclControls := True;
  HookFileDialogs;
end;

finalization

Done;
UnHookFileDialogs;

end.
