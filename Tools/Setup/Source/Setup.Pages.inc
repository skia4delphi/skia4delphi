#ifndef UnitSetupPages
#define UnitSetupPages

[Code]
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
// unit Setup.Pages;

// interface

// uses
  #include "Source\RADStudio.inc"

/// <summary> Create the RAD Studio versions choise page </summary>
procedure CreateRADStudioVersionsChoicePage(const AAfterID: Integer; const ARADStudioInfos: TRADStudioInfos); forward;
/// <summary> Get selected RAD Studio versions </summary>
function GetSelectedRADStudioVersions: TRADStudioInfos; forward;

// implementation

// uses
  #include "Source\Setup.Utils.inc"

/// <summary> Get selected RAD Studio versions in versions choise page </summary>
function _GetSelectedVersions: TRADStudioInfos; forward;
/// <summary> On versions choise page click </summary>
function _OnRADStudioVersionsPageNextButtonClick(ASender: TWizardPage): Boolean; forward;

var
  _FRADStudioVersionsChoicePage: TInputOptionWizardPage;
  _FRADStudioVersionsChoicePageOptions: TRADStudioInfos;
  _FSelectedRADStudioVersions: TRADStudioInfos;

procedure CreateRADStudioVersionsChoicePage(const AAfterID: Integer; const ARADStudioInfos: TRADStudioInfos);
var
  I: Integer;
  LDefaultIndex: Integer;
  LHasRADStudioVersionsParam: Boolean;
begin
  if WizardSilent then
    _FSelectedRADStudioVersions := ARADStudioInfos
  else
  begin
    LHasRADStudioVersionsParam := HasParam('/RADStudioVersions');
    _FRADStudioVersionsChoicePageOptions := ARADStudioInfos;
    _FRADStudioVersionsChoicePage := CreateInputOptionPage(AAfterID, CustomMessage('SetupPagesRADStudioVersionsChoiceTitle'), '',
      FmtMessage(CustomMessage('SetupPagesRADStudioVersionsChoiceMessage'), ['{#LibraryName}']), False, False);

    LDefaultIndex := -1;
    for I := GetArrayLength(ARADStudioInfos) - 1 downto 0 do
    begin
      if ARADStudioInfos[I].Status in [riToolchainNotSupported, riNormal] then
      begin
        LDefaultIndex := I;
        Break;
      end;
    end;

    for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
    begin
      case ARADStudioInfos[I].Status of
        riNotFound: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, '', 0, False, False, False, False, nil);
        riNeedOpenFirst: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, CustomMessage('SetupPagesRADStudioVersionsChoiceAlertNeedOpenFirst'), 0, False, False, False, False, nil);
        riToolchainNotSupported: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, '', 0, LHasRADStudioVersionsParam or (I = LDefaultIndex), True, False, False, nil);
        riNormal: _FRADStudioVersionsChoicePage.CheckListBox.AddCheckBox(ARADStudioInfos[I].Version.Name, '', 0, LHasRADStudioVersionsParam or (I = LDefaultIndex), True, False, False, nil);
      else
      end;
    end;
    _FRADStudioVersionsChoicePage.OnNextButtonClick := @_OnRADStudioVersionsPageNextButtonClick;
  end;
end;

function GetSelectedRADStudioVersions: TRADStudioInfos;
begin
  Result := _FSelectedRADStudioVersions;
end;

function _GetSelectedVersions: TRADStudioInfos;
var
  I: Integer;
begin
  Result := [];
  if _FRADStudioVersionsChoicePage <> nil then
  begin
    for I := 0 to GetArrayLength(_FRADStudioVersionsChoicePageOptions) - 1 do
    begin
      if _FRADStudioVersionsChoicePage.CheckListBox.ItemEnabled[I] and (_FRADStudioVersionsChoicePage.CheckListBox.State[I] = cbChecked) then
      begin
        SetArrayLength(Result, GetArrayLength(Result) + 1);
        Result[GetArrayLength(Result) - 1] := _FRADStudioVersionsChoicePageOptions[I];
      end;
    end;
  end;
end;

function _OnRADStudioVersionsPageNextButtonClick(ASender: TWizardPage): Boolean;
begin
  _FSelectedRADStudioVersions := _GetSelectedVersions;
  Result := GetArrayLength(_FSelectedRADStudioVersions) <> 0;
  if (not Result) and (not IsVerySilent) then
    Result := MsgBox(CustomMessage('SetupPagesNoneRADStudioVersionsSelectedMessage'), mbConfirmation, MB_YESNO or MB_DEFBUTTON1) = IDYES;
end;

// end.
#endif
