#ifndef UnitRADStudioBuild
#define UnitRADStudioBuild

[Code]
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
// unit RADStudio.Build;

// interface

// uses
  #include "Source\RADStudio.inc"
  #include "Source\RADStudio.Project.inc"

type
  TBeforeProjectBuildProc = procedure(const AProject: TRADStudioProject; const APlatform: TProjectPlatform; const AInfo: TRADStudioInfo);
  TTryExtractPreBuildObjectsProc = function(const AInfo: TRADStudioInfo): Boolean;

/// <summary> Get platforms that are allowed to build in specific platform. Current, allowed only windows platforms to avoid problems with sdk. </summary>
function GetPlatformsAllowedToBuild(const ARADStudioInfo: TRADStudioInfo): TProjectPlatforms; forward;
/// <summary> Get steps of TryBuildRADStudioPackages used in WizardForm.ProgressGauge.Position. Use this to set WizardForm.ProgressGauge.Max before call the TryBuildRADStudioPackages. </summary>
function GetTryBuildRADStudioPackagesSteps(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Integer; forward;
/// <summary> Try to build RAD Studio packages, but only in allowed platforms. </summary>
function TryBuildRADStudioPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects; const ACustomParameter: string; const ABeforeProjectBuild: TBeforeProjectBuildProc; const ATryExtractPreBuildObjects: TTryExtractPreBuildObjectsProc): Boolean; forward;

// implementation

// uses
  #include "Source\IO.Utils.inc"
  #include "Source\Setup.Utils.inc"

const
  _BuilLogsFileName = '{app}\Build.Logs.txt';

/// <summary> Get the MSBuild parameters. The custom parameter is optionally. </summary>
function _GetMSBuildParams(const AConfig: TProjectConfig; const APlatform: TProjectPlatform; const ACustomParameter: string): string; forward;
/// <summary> Try to get the comand to build a project. The custom parameter is optionally. </summary>
function _TryBuildRADStudioProject(const ARADStudioInfo: TRADStudioInfo; const AConfig: TProjectConfig; const APlatform: TProjectPlatform; const AProject: TRADStudioProject; const ACustomParameter, ALogFileName: string; const ABeforeProjectBuild: TBeforeProjectBuildProc; out AResultCode: Integer): Boolean; forward;
/// <summary> Try to get the comand to build a project. The custom parameter is optionally. </summary>
function _TryGetBuildCommand(const ARADStudioInfo: TRADStudioInfo; const AConfig: TProjectConfig; const APlatform: TProjectPlatform; const AProjectFileName, ACustomParameter, ALogFileName: string; out ACommand: string): Boolean; forward;

function _GetMSBuildParams(const AConfig: TProjectConfig; const APlatform: TProjectPlatform; const ACustomParameter: string): string;
begin
  Result := Format('/p:Config=%s /p:Platform=%s /p:DCC_BuildAllUnits=true', [GetProjectConfigName(AConfig), GetProjectPlatformName(APlatform)]);
  if ACustomParameter <> '' then
    Result := Result + Format(' /p:%s', [ACustomParameter]);
end;

function GetPlatformsAllowedToBuild(const ARADStudioInfo: TRADStudioInfo): TProjectPlatforms;
begin
  Result := [pfWin32, pfWin64];
end;

function GetTryBuildRADStudioPackagesSteps(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Integer;
var
  I, J, K: Integer;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LPlatform: TProjectPlatform;
begin
  Result := 0;
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    LInfo := ARADStudioInfos[I];
    if LInfo.Status <> riNormal then
      Continue;
    for LPlatform := LowProjectPlatform to HighProjectPlatform do
    begin
      if not (LPlatform in GetPlatformsAllowedToBuild(LInfo)) then
        Continue;
      for J := 0 to GetArrayLength(AGroupProjects) - 1 do
      begin
        for K := 0 to GetArrayLength(AGroupProjects[J].Items) - 1 do
        begin
          LProject := AGroupProjects[J].Items[K].Project;
          if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) and (LPlatform in LProject.Platforms) then
            Result := Result + 1;
        end;
      end;
    end;
  end;
end;

function TryBuildRADStudioPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects; const ACustomParameter: string; 
  const ABeforeProjectBuild: TBeforeProjectBuildProc; const ATryExtractPreBuildObjects: TTryExtractPreBuildObjectsProc): Boolean;
var
  I, J, K: Integer;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LPlatform: TProjectPlatform;
  LResultCode: Integer;
begin
  Log('RADStudio.Build.TryBuildRADStudioPackages: Starting...');
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    LInfo := ARADStudioInfos[I];
    if not (LInfo.Status in [riNormal, riToolchainNotSupported]) then
      Continue;
    for LPlatform := LowProjectPlatform to HighProjectPlatform do
    begin
      if not (LPlatform in GetPlatformsAllowedToBuild(LInfo)) then
        Continue;
      for J := 0 to GetArrayLength(AGroupProjects) - 1 do
      begin
        for K := 0 to GetArrayLength(AGroupProjects[J].Items) - 1 do
        begin
          LProject := AGroupProjects[J].Items[K].Project;
          if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) and (LPlatform in LProject.Platforms) then
          begin
            Log(Format('RADStudio.Build.TryBuildRADStudioPackages: Trying to build the package "%s" to %s platform for %s', [ExtractFileName(LProject.FileName), GetProjectPlatformName(LPlatform), LInfo.Version.Name]));
            WizardForm.StatusLabel.Caption := FmtMessage(CustomMessage('RADStudioBuildBuildingFor'), [LInfo.Version.Name, GetProjectPlatformName(LPlatform)]);
            WizardForm.FilenameLabel.Caption := ExtractFileName(LProject.FileName);
            if (LInfo.Status = riToolchainNotSupported) or (not _TryBuildRADStudioProject(LInfo, LInfo.Version.BuildConfig, LPlatform, LProject, ACustomParameter, ExpandConstant(_BuilLogsFileName), ABeforeProjectBuild, LResultCode)) then
            begin
              Log(Format('RADStudio.Build.TryBuildRADStudioPackages: Failed to build the package "%s" to %s platform for %s', [ExtractFileName(LProject.FileName), GetProjectPlatformName(LPlatform), LInfo.Version.Name]));
              if (ATryExtractPreBuildObjects <> nil) and ATryExtractPreBuildObjects(LInfo) then
              begin
                Log(Format('RADStudio.Build.TryBuildRADStudioPackages: Extracted pre-build objects for %s', [LInfo.Version.Name]));
                Break;
              end
              else
              begin
                Log(Format('RADStudio.Build.TryBuildRADStudioPackages: Failed to extract pre-build objects for %s', [LInfo.Version.Name]));
                if LInfo.Status = riToolchainNotSupported then
                  TryShowErrorFmt(CustomMessage('RADStudioBuildErrorNotSupportCommandLineBuild'), [LInfo.Version.Name])
                else
                begin
                  TryShowErrorFmt(CustomMessage('RADStudioBuildErrorBuilding'), [LInfo.Version.Name + ' - ' + ExtractFileName(LProject.FileName) + ' - ' + GetProjectPlatformName(LPlatform), InttoStr(LResultCode)]);
                  TryOpenInNotepad(ExpandConstant(_BuilLogsFileName));
                end;
              end;
              Result := False;
              Exit;
            end;
            WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Position + 1;
          end;
        end;
      end;
    end;
  end;
  Result := True;
end;

function _TryBuildRADStudioProject(const ARADStudioInfo: TRADStudioInfo; const AConfig: TProjectConfig; const APlatform: TProjectPlatform; 
  const AProject: TRADStudioProject; const ACustomParameter, ALogFileName: string; const ABeforeProjectBuild: TBeforeProjectBuildProc; out AResultCode: Integer): Boolean;
var
  LProjectDir: string;
  LCommand: string;
begin
  DeleteFile(ALogFileName);
  Result := _TryGetBuildCommand(ARADStudioInfo, AConfig, APlatform, AProject.FileName, ACustomParameter, ALogFileName, LCommand);
  if Result then
  begin
    AResultCode := 0;
    LProjectDir := ExtractFilePath(AProject.FileName);
    if ABeforeProjectBuild <> nil then
      ABeforeProjectBuild(AProject, APlatform, ARADStudioInfo);
    Result := Exec(ExpandConstant('{cmd}'), '/C ' + LCommand, LProjectDir, SW_HIDE, ewWaitUntilTerminated, AResultCode) and (AResultCode = 0);
  end;
  if Result then
    DeleteFile(ALogFileName)
  else
    Log(Format('RADStudio.Build._TryBuildRADStudioProject: Failed executing command "%s"', [LCommand]));
end;

function _TryGetBuildCommand(const ARADStudioInfo: TRADStudioInfo; const AConfig: TProjectConfig; const APlatform: TProjectPlatform; const AProjectFileName, ACustomParameter, ALogFileName: string; out ACommand: string): Boolean;
var
  LFrameworkDir: string;
  LResultCode: Integer;
  LRSVarsBatchFileName: string;
begin
  Result := TryGetRADStudioRSVarsBatchFileName(ARADStudioInfo, LRSVarsBatchFileName);
  if not Result then
    TryShowErrorFmt(CustomMessage('RADStudioBuildErrorCantFoundRSVars'), [LRsVarsBatchFileName, ARADStudioInfo.Version.Name])
  else
  begin
    if Exec(ExpandConstant('{cmd}'), Format('/C ""%s" && MSBuild /version>nul 2>&1"', [LRSVarsBatchFileName]), '', SW_HIDE, ewWaitUntilTerminated, LResultCode) and (LResultCode = 0) then
      ACommand := Format('""%s" && MSBuild "%s" %s>"%s" 2>&1"', [LRSVarsBatchFileName, AProjectFileName, _GetMSBuildParams(AConfig, APlatform, ACustomParameter), ALogFileName])
    else
    begin
      Result := IsDotNetInstalled(ARADStudioInfo.Version.DotNetVersion, ARADStudioInfo.Version.DotNetServicePack);
      if not Result then
      begin
        case ARADStudioInfo.Version.DotNetVersion of
          net35: TryShowError(CustomMessage('DotNet35NotFound'));
          net45: TryShowError(CustomMessage('DotNet45NotFound'));
        end;
      end
      else
      begin
        case ARADStudioInfo.Version.DotNetVersion of
          net35: LFrameworkDir := ExpandConstant('{dotnet20}');
          net45: LFrameworkDir := ExpandConstant('{dotnet40}');
        end;
        ACommand := Format('""%s" && @set "FrameworkDir=%s" && @set "PATH=%s;%%PATH%%" && MSBuild "%s" %s>"%s" 2>&1"', [LRSVarsBatchFileName, LFrameworkDir, LFrameworkDir, AProjectFileName, _GetMSBuildParams(AConfig, APlatform, ACustomParameter), ALogFileName]);
      end;
    end;
  end;
end;

// end.
#endif
