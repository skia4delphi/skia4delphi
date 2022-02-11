#ifndef UnitRADStudio
#define UnitRADStudio

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
// unit RADStudio;

// interface

// uses
  #include "Source\RADStudio.Project.inc"

type
  TRADStudioVersion = record
    Name: string;
    RegVersion: string;
    MaxDprojVersion: string;
    MinDprojVersion: string;
    PackageVersion: string;
    HasGetItCmd: Boolean;
    GetItCmdListInstalledParams: string;
    GetItCmdUninstallParams: string;
  end;

  TRADStudioInstallationStatus = (riNotFound, riNeedOpenFirst, riToolchainNotSupported, riNormal);

  TRADStudioInfo = record
    AppFileName: string;
    RootDir: string;
    Status: TRADStudioInstallationStatus;
    UserAppDataDir: string;
    Version: TRADStudioVersion;
  end;

  TRADStudioInfos = array of TRADStudioInfo;

/// <summary> Compare two versions of RAD Studio </summary>
function CompareRADStudioVersions(const AVersion1, AVersion2: TRADStudioVersion): Integer; forward;
/// <summary> Get the RAD Studio info </summary>
function GetRADStudioInfo(const ARADStudioVersion: TRADStudioVersion): TRADStudioInfo; forward;
/// <summary> Get RAD Studio infos from the RAD Studio versions of the packages of each group project </summary>
function GetRADStudioInfosOfGroupProjects(const AGroupProjects: TRADStudioGroupProjects): TRADStudioInfos; forward;
/// <summary> Get the RAD Studio path from the registry </summary>
function GetRADStudioRegKey(const ARADStudioVersion: TRADStudioVersion): string; forward;
/// <summary> Check if there is any RAD Studio instance running in machine </summary>
function HaveRADStudioInstanceRunning: Boolean; forward;
/// <summary> Try to add an environment variable exclusive of RAD Studio application </summary>
function TryAddRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName, AValue: string): Boolean; forward;
/// <summary> Try to add a path to the library browsing path of the RAD Studio </summary>
function TryAddRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to add a path to the library debug dcu path of the RAD Studio </summary>
function TryAddRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to add a path to the library search path of the RAD Studio </summary>
function TryAddRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to add a path to the PATH environment variable exclusive of RAD Studio application </summary>
function TryAddRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean; forward;
/// <summary> Try get default BPL output filename of a RAD Studio project </summary>
function TryGetRADStudioProjectBplOutputFileName(const AProject: TRADStudioProject; out AFileName: string): Boolean; forward;
/// <summary> Try to get the "rsvars.bat" filename, used to compile projects with MSBuild </summary>
function TryGetRADStudioRSVarsBatchFileName(const ARADStudioInfo: TRADStudioInfo; out ARsvarsFileName: string): Boolean; forward;
/// <summary> Try to get the RAD Studio version by product version in registry </summary>
function TryGetRADStudioVersionByRegVersion(const ARegVersion: string; out ARADStudioVersion: TRADStudioVersion): Boolean; forward;
/// <summary> Try to get the RAD Studio version of a project </summary>
function TryGetRADStudioVersionOfProject(const AProject: TRADStudioProject; out ARADStudioVersion: TRADStudioVersion): Boolean; forward;
/// <summary> Try to register a Bpl in RAD Studio </summary>
function TryRegisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const ABplFileName, ADescription: string): Boolean; forward;
/// <summary> Try to remove a library instaled using the GetIt </summary>
function TryRemoveFromGetIt(const AInfos: TRADStudioInfos; const ALibraryName: string): Boolean; forward;
/// <summary> Try to remove an environment variable exclusive of RAD Studio application </summary>
function TryRemoveRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName: string): Boolean; forward;
/// <summary> Try to remove a path from the library browsing path of the RAD Studio </summary>
function TryRemoveRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path from the library debug dcu path of the RAD Studio </summary>
function TryRemoveRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path from the library search path of the RAD Studio </summary>
function TryRemoveRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean; forward;
/// <summary> Try to remove a path to the PATH environment variable exclusive of RAD Studio application </summary>
function TryRemoveRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean; forward;
/// <summary> Try to unregister a Bpl in RAD Studio </summary>
function TryUnregisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean; forward;

// implementation

// uses
  #include "Source\Setup.Utils.inc"
  #include "Source\String.Utils.inc"

type
  _TRADStudioVersions = array of TRADStudioVersion;

/// <summary> Filter allowed version defined by the parameter /RADStudioVersions=all for example </summary>
function _FilterAllowedRADStudioVersions(const ARADStudioVersions: _TRADStudioVersions): _TRADStudioVersions; forward;
/// <summary> Get the RAD Studio versions that the installer is able to recognize </summary>
function _GetRADStudioVersions: _TRADStudioVersions; forward;
/// <summary> Initialization method of the unit </summary>
procedure _InitializationUnitRADStudio; forward;
/// <summary> Sort RAD Studio infos by version </summary>
function _SortRADStudioInfos(AInfos: TRADStudioInfos): TRADStudioInfos; forward;
/// <summary> Try to add a path to the Library registry (in a specific registry name) of the RAD Studio </summary>
function _TryAddRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean; forward;
/// <summary> Try to get the RADStudio installations info </summary>
function _TryGetRADStudioInstallation(const ARADStudioVersion: TRADStudioVersion; const ARootKey: Integer; out AAppFileName, ARootDir: string): Boolean; forward;
/// <summary> Try to remove a bpl from the "disabled packages" registry of the RAD Studio </summary>
function _TryRemoveRADStudioBplFromDisabledPackages(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean; forward;
/// <summary> Try to remove a path from the Library registry (in a specific registry name) of the RAD Studio </summary>
function _TryRemoveRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean; forward;

var
  _FRADStudioVersions: _TRADStudioVersions;

function CompareRADStudioVersions(const AVersion1, AVersion2: TRADStudioVersion): Integer;
var
  LVersion1, LVersion2: Int64;
begin
  if not StrToVersion(AVersion1.RegVersion, LVersion1) then
    RaiseException(FmtMessage('RADStudio.CompareRADStudioVersions: Invalid version "%1"', [AVersion1.RegVersion]));
  if not StrToVersion(AVersion2.RegVersion, LVersion2) then
    RaiseException(FmtMessage('RADStudio.CompareRADStudioVersions: Invalid version "%1"', [AVersion2.RegVersion]));
  Result := ComparePackedVersion(LVersion1, LVersion2);
end;

function _FilterAllowedRADStudioVersions(const ARADStudioVersions: _TRADStudioVersions): _TRADStudioVersions;
var
  LAllowedVersionsText: TArrayOfString;
  LAllowedVersions: array of Int64;
  LVersion: Int64;
  I, J: Integer;
begin
  LAllowedVersionsText := SplitString(ExpandConstant('{param:RADStudioVersions|all}'), ',');
  LAllowedVersions := [];
  for I := 0 to GetArrayLength(LAllowedVersionsText) - 1 do
  begin
    if SameText(LAllowedVersionsText[I], 'all') then
    begin
      Result := ARADStudioVersions;
      Exit;
    end;
    if StrToVersion(LAllowedVersionsText[I], LVersion) then
    begin
      SetArrayLength(LAllowedVersions, GetArrayLength(LAllowedVersions) + 1);
      LAllowedVersions[GetArrayLength(LAllowedVersions) - 1] := LVersion;
    end;
  end;

  Result := [];
  for I := 0 to GetArrayLength(ARADStudioVersions) - 1 do
  begin
    if StrToVersion(ARADStudioVersions[I].RegVersion, LVersion) then
    begin
      for J := 0 to GetArrayLength(LAllowedVersions) - 1 do
      begin
        if LVersion = LAllowedVersions[J] then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := ARADStudioVersions[I];
          Break;
        end;
      end;
    end;
  end;
end;

function GetRADStudioInfo(const ARADStudioVersion: TRADStudioVersion): TRADStudioInfo;
var
  LResultCode: Integer;
begin
  Result.Version := ARADStudioVersion;
  if _TryGetRADStudioInstallation(ARADStudioVersion, HKEY_CURRENT_USER, Result.AppFileName, Result.RootDir) then
  begin
    if (not Exec(ExpandConstant('{cmd}'), Format('/C "%s" --test > nul 2>&1', [ExtractFilePath(Result.AppFileName) + 'DCC32.EXE']), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or (LResultCode = 0) then
      Result.Status := riToolchainNotSupported
    else
      Result.Status := riNormal;
  end
  else if _TryGetRADStudioInstallation(ARADStudioVersion, HKEY_LOCAL_MACHINE, Result.AppFileName, Result.RootDir) then
    Result.Status := riNeedOpenFirst
  else
    Result.Status := riNotFound;
end;

function GetRADStudioInfosOfGroupProjects(const AGroupProjects: TRADStudioGroupProjects): TRADStudioInfos;
var
  LVersion: TRADStudioVersion;
  LFound: Boolean;
  I, J, K: Integer;
begin
  Result := [];
  // Search RAD Studio versions in packages
  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
  begin
    for J := 0 to GetArrayLength(AGroupProjects[I].Items) - 1 do
    begin
      if TryGetRADStudioVersionOfProject(AGroupProjects[I].Items[J].Project, LVersion) then
      begin
        // Avoiding duplications before insertion
        LFound := False;
        for K := 0 to GetArrayLength(Result) - 1 do
        begin
          if CompareRADStudioVersions(Result[K].Version, LVersion) = 0 then
          begin
            LFound := True;
            Break;
          end;
        end;
        if not LFound then
        begin
          SetArrayLength(Result, GetArrayLength(Result) + 1);
          Result[GetArrayLength(Result) - 1] := GetRADStudioInfo(LVersion);
        end;
      end;
    end;
  end;
  // Sort results
  Result := _SortRADStudioInfos(Result);
end;

function GetRADStudioRegKey(const ARADStudioVersion: TRADStudioVersion): string;
begin
  Result := 'Software\Embarcadero\BDS\' + ARADStudioVersion.RegVersion;
end;

function _GetRADStudioVersions: _TRADStudioVersions;
begin
  _InitializationUnitRADStudio;
  Result := _FRADStudioVersions;
end;

function HaveRADStudioInstanceRunning: Boolean;
begin
  Result := IsAppRunning('bds.exe');
end;

procedure _InitializationUnitRADStudio;
var
  LRADStudioVersion: TRADStudioVersion;
begin
  if GetArrayLength(_FRADStudioVersions) <> 0 then
    Exit;
  // Note: Keep the _FRADStudioVersions array in order, from the oldest version to the newest

  // RAD Studio XE2
  LRADStudioVersion.Name := 'RAD Studio XE2';
  LRADStudioVersion.RegVersion := '9.0';
  LRADStudioVersion.MaxDprojVersion := '13.4';
  LRADStudioVersion.MinDprojVersion := '13.4';
  LRADStudioVersion.PackageVersion := '160';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE3
  LRADStudioVersion.Name := 'RAD Studio XE3';
  LRADStudioVersion.RegVersion := '10.0';
  LRADStudioVersion.MaxDprojVersion := '14.4'; // Conflicted
  LRADStudioVersion.MinDprojVersion := '14.3';
  LRADStudioVersion.PackageVersion := '170';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE4
  LRADStudioVersion.Name := 'RAD Studio XE4';
  LRADStudioVersion.RegVersion := '11.0';
  LRADStudioVersion.MaxDprojVersion := '14.6';
  LRADStudioVersion.MinDprojVersion := '14.4'; // Conflicted
  LRADStudioVersion.PackageVersion := '180';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE5
  LRADStudioVersion.Name := 'RAD Studio XE5';
  LRADStudioVersion.RegVersion := '12.0';
  LRADStudioVersion.MaxDprojVersion := '15.3';
  LRADStudioVersion.MinDprojVersion := '15.1';
  LRADStudioVersion.PackageVersion := '190';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE6
  LRADStudioVersion.Name := 'RAD Studio XE6';
  LRADStudioVersion.RegVersion := '14.0';
  LRADStudioVersion.MaxDprojVersion := '15.4';
  LRADStudioVersion.MinDprojVersion := '15.4';
  LRADStudioVersion.PackageVersion := '200';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE7
  LRADStudioVersion.Name := 'RAD Studio XE7';
  LRADStudioVersion.RegVersion := '15.0';
  LRADStudioVersion.MaxDprojVersion := '16.1';
  LRADStudioVersion.MinDprojVersion := '16.0';
  LRADStudioVersion.PackageVersion := '210';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio XE8
  LRADStudioVersion.Name := 'RAD Studio XE8';
  LRADStudioVersion.RegVersion := '16.0';
  LRADStudioVersion.MaxDprojVersion := '17.2';
  LRADStudioVersion.MinDprojVersion := '17.1';
  LRADStudioVersion.PackageVersion := '220';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.0 Seattle
  LRADStudioVersion.Name := 'RAD Studio 10.0 Seattle';
  LRADStudioVersion.RegVersion := '17.0';
  LRADStudioVersion.MaxDprojVersion := '18.1'; // Conflicted
  LRADStudioVersion.MinDprojVersion := '18.0';
  LRADStudioVersion.PackageVersion := '230';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.1 Berlin
  LRADStudioVersion.Name := 'RAD Studio 10.1 Berlin';
  LRADStudioVersion.RegVersion := '18.0';
  LRADStudioVersion.MaxDprojVersion := '18.2'; // Conflicted
  LRADStudioVersion.MinDprojVersion := '18.1'; // Conflicted
  LRADStudioVersion.PackageVersion := '240';
  LRADStudioVersion.HasGetItCmd := False;
  LRADStudioVersion.GetItCmdListInstalledParams := '';
  LRADStudioVersion.GetItCmdUninstallParams := '';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.2 Tokyo
  LRADStudioVersion.Name := 'RAD Studio 10.2 Tokyo';
  LRADStudioVersion.RegVersion := '19.0';
  LRADStudioVersion.MaxDprojVersion := '18.4';
  LRADStudioVersion.MinDprojVersion := '18.2'; // Conflicted
  LRADStudioVersion.PackageVersion := '250';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-listavailable:"%s" -filter:Installed';
  LRADStudioVersion.GetItCmdUninstallParams := '-u"%s"';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.3 Rio
  LRADStudioVersion.Name := 'RAD Studio 10.3 Rio';
  LRADStudioVersion.RegVersion := '20.0';
  LRADStudioVersion.MaxDprojVersion := '18.8';
  LRADStudioVersion.MinDprojVersion := '18.5';
  LRADStudioVersion.PackageVersion := '260';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-listavailable:"%s" -filter:Installed';
  LRADStudioVersion.GetItCmdUninstallParams := '-u"%s"';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 10.4 Sydney
  LRADStudioVersion.Name := 'RAD Studio 10.4 Sydney';
  LRADStudioVersion.RegVersion := '21.0';
  LRADStudioVersion.MaxDprojVersion := '19.2';
  LRADStudioVersion.MinDprojVersion := '19.0';
  LRADStudioVersion.PackageVersion := '270';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -f=installed';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s"';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  // RAD Studio 11.0 Alexandria
  LRADStudioVersion.Name := 'RAD Studio 11.0 Alexandria';
  LRADStudioVersion.RegVersion := '22.0';
  LRADStudioVersion.MaxDprojVersion := '19.3';
  LRADStudioVersion.MinDprojVersion := '19.3';
  LRADStudioVersion.PackageVersion := '280';
  LRADStudioVersion.HasGetItCmd := True;
  LRADStudioVersion.GetItCmdListInstalledParams := '-l="%s" -f=installed';
  LRADStudioVersion.GetItCmdUninstallParams := '-u="%s"';
  SetArrayLength(_FRADStudioVersions, GetArrayLength(_FRADStudioVersions) + 1);
  _FRADStudioVersions[GetArrayLength(_FRADStudioVersions) - 1] := LRADStudioVersion;

  _FRADStudioVersions := _FilterAllowedRADStudioVersions(_FRADStudioVersions);
end;

function _SortRADStudioInfos(AInfos: TRADStudioInfos): TRADStudioInfos;
var
  I: Integer;
  J: Integer;
  LMinIndex: Integer;
  LVersion: TRADStudioVersion;
  LResult: TRADStudioInfos;
begin
  SetArrayLength(LResult, GetArrayLength(AInfos));
  for I := 0 to GetArrayLength(AInfos) - 1 do
  begin
    LMinIndex := I;
    for J := 0 to GetArrayLength(AInfos) - 1 do
    begin
      if (AInfos[J].Version.RegVersion <> '') and ((AInfos[LMinIndex].Version.RegVersion = '') or
        (CompareRADStudioVersions(AInfos[J].Version, AInfos[LMinIndex].Version) < 0)) then
      begin
        LMinIndex := J;
      end;
    end;
    LResult[I] := AInfos[LMinIndex];
    LVersion := AInfos[LMinIndex].Version;
    LVersion.RegVersion := '';
    AInfos[LMinIndex].Version := LVersion;
  end;
  Result := LResult;
end;

function TryAddRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName, AValue: string): Boolean;
begin
  Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName, AValue);
  if not Result then
    Log(Format('RADStudio.TryAddRADStudioEnvVariable: Failed to add an environment variable to the RAD Studio registry (Name: "%s", Value: "%s")', [AName, AValue]));
end;

function TryAddRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Browsing Path', APath);
end;

function TryAddRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Debug DCU Path', APath);
end;

function _TryAddRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean;
var
  LNewValue: string;
  LPathsToInsert: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I: Integer;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform));
  if Result then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue)) or (LNewValue = '') then
      LNewValue := APath
    else
    begin
      LPathsToInsert := SplitString(APath, ';');
      LCurrentPaths := SplitString(LNewValue, ';');
      for I := 0 to GetArrayLength(LPathsToInsert) - 1 do
      begin
        LNewValue := LPathsToInsert[I];
        LCurrentPaths := RemoveString(LCurrentPaths, LNewValue, False);
        LCurrentPaths := RemoveString(LCurrentPaths, LNewValue + '\', False);
        LCurrentPaths := AppendString(LCurrentPaths, LNewValue, False);
      end;
      LNewValue := JoinStrings(LCurrentPaths, ';', False);
    end;
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue);
  end;  
  if not Result then
    Log(Format('RADStudio._TryAddRADStudioLibraryPath: Failed to add a library path to the RAD Studio registry (RegName: "%s", Path: "%s")', [ARegName, APath]));
end;

function TryAddRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryAddRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Search Path', APath);
end;

function TryAddRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean;
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion));
  if Result then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LPaths := InsertStringAtBeginning(LPaths, APath);
    LValue := JoinStrings(LPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue);
  end;   
  if not Result then
    Log(Format('RADStudio.TryAddRADStudioPathEnvVariable: Failed to add a path environment variable to the RAD Studio registry (Path: "%s")', [APath]));
end;

function TryGetRADStudioProjectBplOutputFileName(const AProject: TRADStudioProject; out AFileName: string): Boolean;
var
  LBplSuffix: string;
  LVersion: TRADStudioVersion;
begin
  LBplSuffix := AProject.DllSuffix;
  if SameText(LBplSuffix, '$(Auto)') then
  begin
    Result := TryGetRADStudioVersionOfProject(AProject, LVersion);
    if not Result then
    begin
      AFileName := '';
      Exit;
    end;
    LBplSuffix := LVersion.PackageVersion;
  end;
  AFileName := AddBackslash(AProject.BplOutputPath) + ExtractFileName(ChangeFileExt(AProject.FileName, LBplSuffix + '.bpl'));
  Result := True;
end;

function TryGetRADStudioRSVarsBatchFileName(const ARADStudioInfo: TRADStudioInfo; out ARsvarsFileName: string): Boolean;
begin
  if ARADStudioInfo.RootDir = '' then
    Result := False
  else
  begin
    ARsvarsFileName := AddBackslash(ARADStudioInfo.RootDir) + 'bin\rsvars.bat';
    Result := FileExists(ARsvarsFileName);
  end;
  if not Result then
    ARsvarsFileName := '';
end;

function TryGetRADStudioVersionByRegVersion(const ARegVersion: string; out ARADStudioVersion: TRADStudioVersion): Boolean;
var
  LVersion: Int64;
  LRADVersion: Int64;
  LRADStudioVersions: _TRADStudioVersions;
  I: Integer;
begin
  Result := False;
  if not StrToVersion(ARegVersion, LVersion) then
    Exit;
  LRADStudioVersions := _GetRADStudioVersions;
  for I := 0 to GetArrayLength(LRADStudioVersions) - 1 do
  begin
    ARADStudioVersion := LRADStudioVersions[I];
    if StrToVersion(ARADStudioVersion.RegVersion, LRADVersion) and SamePackedVersion(LRADVersion, LVersion) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function _TryGetRADStudioInstallation(const ARADStudioVersion: TRADStudioVersion; const ARootKey: Integer; out AAppFileName, ARootDir: string): Boolean;
begin
  try
    if not RegQueryStringValue(ARootKey, GetRADStudioRegKey(ARADStudioVersion), 'RootDir', ARootDir) then
    begin
      Result := False;
      Exit;
    end;
    if not RegQueryStringValue(ARootKey, GetRADStudioRegKey(ARADStudioVersion), 'App', AAppFileName) then
      AAppFileName := AddBackslash(AddBackslash(ARootDir) + 'bin') + 'bds.exe';
    if not FileExists(AAppFileName) then
    begin
      Result := False;
      Exit;
    end;
    ARootDir := RemoveBackslashUnlessRoot(ARootDir);
    Result := True;
  finally
    if not Result then
    begin
      AAppFileName := '';
      ARootDir := '';
    end;
  end;
end;

function TryGetRADStudioVersionOfProject(const AProject: TRADStudioProject; out ARADStudioVersion: TRADStudioVersion): Boolean;
var
  LProjVersion: Int64;
  LMaxVersion: Int64;
  LMinVersion: Int64;
  LRADStudioVersions: _TRADStudioVersions;
  I: Integer;
begin
  Result := False;
  if not StrToVersion(AProject.ProjectVersion, LProjVersion) then
    Exit;
  LRADStudioVersions := _GetRADStudioVersions;
  for I := 0 to GetArrayLength(LRADStudioVersions) - 1 do
  begin
    ARADStudioVersion := LRADStudioVersions[I];
    if AProject.Personality = ppDelphi then
    begin
      if (not StrToVersion(ARADStudioVersion.MaxDprojVersion, LMaxVersion)) or
        (not StrToVersion(ARADStudioVersion.MinDprojVersion, LMinVersion)) then
      begin
        Continue;
      end;
      if (LProjVersion >= LMinVersion) and (LProjVersion <= LMaxVersion) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TryRegisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const ABplFileName, ADescription: string): Boolean;
begin
  Result := TryUnregisterRADStudioBpl(ARADStudioVersion, ABplFileName) and
    RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Known Packages', ABplFileName, ADescription);
end;

function TryRemoveFromGetIt(const AInfos: TRADStudioInfos; const ALibraryName: string): Boolean;
var
  I, J: Integer;
  LResultCode: Integer;
  LTempFileName: string;
  LResultLines: TStringList;
  LLineSplitted: TArrayOfString;
  LItemId: string;
  LLibrarySearchName: string;
begin
  Result := True;
  if ALibraryName = '' then
    Exit;
  if Pos('-', ALibraryName) = 0 then
    LLibrarySearchName := ALibraryName + '-'
  else
    LLibrarySearchName := ALibraryName;
  for I := 0 to GetArrayLength(AInfos) - 1 do
  begin
    if (AInfos[I].Status <> riNotFound) and AInfos[I].Version.HasGetItCmd then
    begin
      LTempFileName := AddBackslash(ExpandConstant('{tmp}')) + Format('getit_list_installed_%s_%s_output.txt', [ALibraryName, AInfos[I].Version.RegVersion]);
      DeleteFile(LTempFileName);
      if (not Exec(ExpandConstant('{cmd}'), Format('/C ""%sbin\GetItCmd.exe" ' + AInfos[I].Version.GetItCmdListInstalledParams + '>"%s""', [AddBackslash(AInfos[I].RootDir), LLibrarySearchName, LTempFileName]), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or
        (LResultCode <> 0) or (not FileExists(LTempFileName)) then
      begin
        Log(Format('RADStudio.TryRemoveFromGetIt: Cannot possible to list installed libraries with GetIt of "%s"', [AInfos[I].Version.Name]));
        Result := False;
        Continue;
      end;
      LResultLines := TStringList.Create;
      try
        LResultLines.LoadFromFile(LTempFileName);
        for J := 0 to LResultLines.Count - 1 do
        begin
          if StartsWithText(LResultLines[J], LLibrarySearchName) then
          begin
            LLineSplitted := SplitString(LResultLines[J], ' ');
            if GetArrayLength(LLineSplitted) > 0 then
            begin
              LItemId := LLineSplitted[0];
              if (not Exec(ExpandConstant('{cmd}'), Format('/C ""%sbin\GetItCmd.exe" ' + AInfos[I].Version.GetItCmdUninstallParams + '"', [AddBackslash(AInfos[I].RootDir), LItemId]), '', SW_HIDE, ewWaitUntilTerminated, LResultCode)) or (LResultCode <> 0) then
              begin
                Log(Format('RADStudio.TryRemoveFromGetIt: Cannot possible to uninstall the library "%s" with GetIt of "%s"', [ALibraryName, AInfos[I].Version.Name]));
                Result := False;
              end;
            end;
          end;
        end;
      finally
        LResultLines.Free;
      end;
    end;
  end;
end;

function _TryRemoveRADStudioBplFromDisabledPackages(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean;
var
  LRegisteredBpls: TArrayOfString;
  LBplName: string;
  I: Integer;
begin
  Result := RegGetValueNames(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Disabled Packages', LRegisteredBpls);
  if Result then
  begin
    LBplName := ExtractFileName(ABplFileName);
    for I := 0 to GetArrayLength(LRegisteredBpls) - 1 do
    begin
      if SameText(LBplName, ExtractFileName(LRegisteredBpls[I])) then
        Result := RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Disabled Packages', LRegisteredBpls[I]) and Result;
    end;
  end;
end;

function TryRemoveRADStudioEnvVariable(const ARADStudioVersion: TRADStudioVersion; const AName: string): Boolean;
begin
  Result := (not RegValueExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName)) or
    RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', AName);   
  if not Result then
    Log(Format('RADStudio.TryRemoveRADStudioEnvVariable: Failed to remove an environment variable to the RAD Studio registry (Name: "%s")', [AName]));
end;

function TryRemoveRADStudioLibraryBrowsingPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Browsing Path', APath);
end;

function TryRemoveRADStudioLibraryDebugDCUPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Debug DCU Path', APath);
end;

function _TryRemoveRADStudioLibraryPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const ARegName, APath: string): Boolean;
var
  LNewValue: string;
  LPathsToRemove: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I: Integer;
begin
  Result := True;
  if RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue) and (LNewValue <> '') then
  begin
    LPathsToRemove := SplitString(APath, ';');
    LCurrentPaths := SplitString(LNewValue, ';');
    for I := 0 to GetArrayLength(LPathsToRemove) - 1 do
    begin
      LNewValue := LPathsToRemove[I];
      LCurrentPaths := RemoveString(LCurrentPaths, LNewValue, False);
      LCurrentPaths := RemoveString(LCurrentPaths, LNewValue + '\', False);
    end;
    LNewValue := JoinStrings(LCurrentPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Library\' + GetProjectPlatformLibraryName(APlatform), ARegName, LNewValue);
  end; 
  if not Result then
    Log(Format('RADStudio._TryRemoveRADStudioLibraryPath: Failed to remove a library path to the RAD Studio registry (RegName: "%s", Path: "%s")', [ARegName, APath]));
end;

function TryRemoveRADStudioLibrarySearchPath(const ARADStudioVersion: TRADStudioVersion; const APlatform: TProjectPlatform; const APath: string): Boolean;
begin
  Result := _TryRemoveRADStudioLibraryPath(ARADStudioVersion, APlatform, 'Search Path', APath);
end;

function TryRemoveRADStudioPathEnvVariable(const ARADStudioVersion: TRADStudioVersion; const APath: string): Boolean;
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  if RegValueExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH') then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LValue := JoinStrings(LPaths, ';', False);
    Result := RegWriteStringValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables', 'PATH', LValue);
  end
  else
    Result := RegKeyExists(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Environment Variables');     
  if not Result then
    Log(Format('RADStudio.TryRemoveRADStudioPathEnvVariable: Failed to remove a path environment variable to the RAD Studio registry (Path: "%s")', [APath]));
end;

function TryUnregisterRADStudioBpl(const ARADStudioVersion: TRADStudioVersion; const ABplFileName: string): Boolean;
var
  LRegisteredBpls: TArrayOfString;
  LBplName: string;
  I: Integer;
begin
  Result := RegGetValueNames(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Known Packages', LRegisteredBpls);
  if Result then
  begin
    LBplName := ExtractFileName(ABplFileName);
    for I := 0 to GetArrayLength(LRegisteredBpls) - 1 do
    begin
      if SameText(LBplName, ExtractFileName(LRegisteredBpls[I])) then
        Result := RegDeleteValue(HKEY_CURRENT_USER, GetRADStudioRegKey(ARADStudioVersion) + '\Known Packages', LRegisteredBpls[I]) and Result;
    end;
  end;
  Result := _TryRemoveRADStudioBplFromDisabledPackages(ARADStudioVersion, ABplFileName) and Result;
end;

// end.
#endif
