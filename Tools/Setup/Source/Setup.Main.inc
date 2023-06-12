#ifndef UnitSetupMain
#define UnitSetupMain

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
// unit Setup.Main;

// interface

// implementation

// uses
  #include "Source\RADStudio.inc"
  #include "Source\RADStudio.Build.inc"
  #include "Source\RADStudio.Project.inc"
  #include "Source\IO.Utils.inc"
  #include "Source\String.Utils.inc"
  #include "Source\Setup.Pages.inc"
  #include "Source\Setup.Style.inc"
  #include "Source\Setup.Utils.inc"

type
  TGetBuildCustonParamEvent = function(): string;
  TTryPrepareProjectInstallationEvent = function(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
  TTryPrepareProjectUninstallationEvent = function(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;

/// <summary> [Event] Called to perform custom pre-install and post-install tasks </summary>
procedure CurStepChanged(ACurStep: TSetupStep); forward;
/// <summary> [Event] Called to perform custom pre-uninstall and post-uninstall tasks </summary>
procedure CurUninstallStepChanged(ACurUninstallStep: TUninstallStep); forward;
/// <summary> Get the default installation directory </summary>
function GetDefaultDirName(AParam: string): string; forward;
/// <summary> [Event] First event, triggered when setup initialized just in installation mode </summary>
function InitializeSetup: Boolean; forward;
/// <summary> [Event] Triggered when initialize wizard (after InitializeSetup) </summary>
procedure InitializeWizard; forward;
/// <summary> Install the packages </summary>
function InstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean; forward;
/// <summary> Check if the setup can create the uninstall registry key to be able to uninstall in windows settings. This reg key can be disabled by executing the setup with the parameter /CreateUninstallRegKey=no </summary>
function NeedsUninstallRegKey: Boolean; forward;
/// <summary> [Event] Called to determine whether or not a particular page should be shown at all </summary>
function ShouldSkipPage(APageID: Integer): Boolean; forward;
/// <summary> Uninstall the packages </summary>
function UninstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean; forward;

var
  FGroupProjects: TRADStudioGroupProjects;
  FMaxVersionInGroupProjects: TRADStudioVersion;
  FOnBeforeProjectBuild: TBeforeProjectBuildProc;
  FOnGetBuildCustonParam: TGetBuildCustonParamEvent;
  FOnTryExtractPreBuildObjects: TTryExtractPreBuildObjectsProc;
  FOnTryPrepareProjectInstallation: TTryPrepareProjectInstallationEvent;
  FOnTryPrepareProjectUninstallation: TTryPrepareProjectUninstallationEvent;
  FRADStudioInfos: TRADStudioInfos;

// implementation

/// <summary> Make project files (dproj, dpr, groupproj) for unofficial versions supported </summary>
procedure _MakeProjectsForUnofficialVersionsSupported(var AGroupProjects: TRADStudioGroupProjects; const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion); forward;
/// <summary> Check if needs directory page by checking the define FilesEmbedded </summary>
function _NeedsDirPage: Boolean; forward;

procedure CurStepChanged(ACurStep: TSetupStep);
var
  LSelectedRADStudioVersions: TRADStudioInfos;
begin
  case ACurStep of
    ssInstall:
      begin
        LSelectedRADStudioVersions := GetSelectedRADStudioVersions;
        Log(Format('Start installation at folder "%s"...', [WizardDirValue]));
        #ifdef FilesEmbedded
        ReplaceRootPathOfRADStudioGroupProjects(ExpandConstant('{tmp}\') + '{app}', ExpandConstant('{app}'), FGroupProjects);
        #endif
        SetAbsolutePathsInRADStudioGroupProjects(ExpandConstant('{app}'), FGroupProjects);
        if IsUpgrade then
        begin
          WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingDetectedVersion');
          if NeedsUninstallRegKey then
          begin
            if (not IsVerySilent) and HaveRADStudioInstanceRunning then
            begin
              TryShowErrorEx(CustomMessage('SetupMainUninstallAbortedToCloseRADStudioInstance'), False);
              Abort;
            end;
            if not TryUninstallCurrentVersion then
            begin
              TryShowErrorFmt(CustomMessage('SetupMainCouldNotUninstallDetectedVersion'), ['{#LibraryName}']);
              Abort;
            end;
          end;
        end;
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingFromGetIt');
        TryRemoveFromGetIt(LSelectedRADStudioVersions, '{#LibraryName}');
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingPackages');
        if not UninstallPackages(LSelectedRADStudioVersions, FGroupProjects) then
        begin
          TryShowError(CustomMessage('SetupMainCouldNotUninstallPackages'));
          Abort;
        end;
      end;
    ssPostInstall:
      begin
        LSelectedRADStudioVersions := GetSelectedRADStudioVersions;
        if GetArrayLength(LSelectedRADStudioVersions) <> 0 then
        begin
          Log('Start installation post extraction files...');
          #ifdef FilesEmbedded
          CopyDirectory(ExpandConstant('{tmp}\') +'{app}\{#LibraryPackagesFolder}', ExpandConstant('{app}\') + '{#LibraryPackagesFolder}', False);
          #endif
          if not InstallPackages(GetSelectedRADStudioVersions, FGroupProjects) then
            Abort;
          if HaveRADStudioInstanceRunning and not IsSilent then
            ShowMessage(CustomMessage('SetupMainInstallationSuccesfullyRestartRADStudio'));
        end;
      end;
  else
  end;
end;

procedure CurUninstallStepChanged(ACurUninstallStep: TUninstallStep);
begin
  case ACurUninstallStep of
    usUninstall: ;
    usPostUninstall:
      begin
        SetAbsolutePathsInRADStudioGroupProjects(ExpandConstant('{app}'), FGroupProjects);
        if not UninstallPackages(FRADStudioInfos, FGroupProjects) then
        begin
          TryShowError(CustomMessage('SetupMainCouldNotUninstallPackages'));
          Abort;
        end;
      end;
  else
  end;
end;

function GetDefaultDirName(AParam: string): string;
begin
  if _NeedsDirPage then
    Result := ExpandConstant('{userdocs}\{#LibraryName}')
  else
    Result := GetCurrentDir;
end;

function InitializeSetup: Boolean;
var
  LPackagesPath: string;
begin
  Result := IsVerySilent or (not HaveRADStudioInstanceRunning);
  if not Result then
  begin
    TryShowMessage(CustomMessage('SetupMainInstallAbortedToCloseRADStudioInstance'));
    Exit;
  end;
  LPackagesPath := AddBackslash('{#LibraryPackagesFolder}');
  #ifdef FilesEmbedded
  LPackagesPath := AddBackslash(CombinePath('{app}', LPackagesPath));
  ExtractTemporaryFiles(LPackagesPath + '*');
  LPackagesPath := ExpandConstant('{tmp}\') + LPackagesPath;
  #else
  LPackagesPath := CombinePath(GetCurrentDir, LPackagesPath);
  #endif
  Result := TryGetRADStudioGroupProjectsFromPath(LPackagesPath, FGroupProjects);
  if Result then
  begin
    FRADStudioInfos := GetRADStudioInfosSupportedByGroupProjects(FGroupProjects);
    TryGetMaxRADStudioVersionInGroupProjects(FGroupProjects, FMaxVersionInGroupProjects);
    Result := GetArrayLength(FRADStudioInfos) > 0;
    if Result then
      _MakeProjectsForUnofficialVersionsSupported(FGroupProjects, FRADStudioInfos, FMaxVersionInGroupProjects)
    else
      TryShowError(CustomMessage('SetupMainErrorNoRADStudioVersionAvailable'));
  end
  else
    TryShowError(CustomMessage('SetupMainErrorFailedToGetGroupProjects'));
end;

// Uninstall initialization
function InitializeUninstall: Boolean;
begin
  Result := IsVerySilent or (not HaveRADStudioInstanceRunning);
  if not Result then
  begin
    TryShowMessage(CustomMessage('SetupMainUninstallAbortedToCloseRADStudioInstance'));
    Exit;
  end;
  Result := TryGetRADStudioGroupProjectsFromPath(AddBackslash(CombinePath(ExpandConstant('{app}'), '{#LibraryPackagesFolder}')), FGroupProjects);
  if Result then
  begin
    FRADStudioInfos := GetRADStudioInfosSupportedByGroupProjects(FGroupProjects);
    TryGetMaxRADStudioVersionInGroupProjects(FGroupProjects, FMaxVersionInGroupProjects);
  end
  else
    TryShowError(CustomMessage('SetupMainErrorFailedToGetGroupProjects'));
end;

// Create wizard custom pages
procedure InitializeWizard;
begin
  CreateRADStudioVersionsChoicePage(wpWelcome, FRADStudioInfos, FMaxVersionInGroupProjects);
  WizardForm.LicenseAcceptedRadio.Checked := True;
end;

function InstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean;
var
  I, J, K, L: Integer;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LBplFileName: string;
  LDcpFileName: string;
  LPlatform: TProjectPlatform;
  LCustomParam: string;
begin
  Log('Setup.Main.InstallPackages: Starting...');
  WizardForm.ProgressGauge.Min := 0;
  WizardForm.ProgressGauge.Max := GetTryBuildRADStudioPackagesSteps(ARADStudioInfos, AGroupProjects) + 1;
  WizardForm.ProgressGauge.Position := 0;
  if FOnGetBuildCustonParam <> nil then
    LCustomParam := FOnGetBuildCustonParam()
  else
    LCustomParam := '';
  Result := TryBuildRADStudioPackages(ARADStudioInfos, AGroupProjects, LCustomParam, FOnBeforeProjectBuild, FOnTryExtractPreBuildObjects);
  if Result then
  begin
    WizardForm.StatusLabel.Caption := CustomMessage('SetupMainInstallingPackages');
    try
      for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
      begin
        LInfo := ARADStudioInfos[I];
        if LInfo.Status in [riNotFound, riNeedOpenFirst] then
          Continue;
        Log(Format('Setup.Main.InstallPackages: Starting installation in %s...', [LInfo.Version.Name]));
        for J := 0 to GetArrayLength(AGroupProjects) - 1 do
        begin
          for K := 0 to GetArrayLength(AGroupProjects[J].Items) - 1 do
          begin
            LProject := AGroupProjects[J].Items[K].Project;
            if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) then
            begin
              LVersion := LInfo.Version;
              Log(Format('Setup.Main.InstallPackages: Starting installation of package "%s" in "%s"...', [LProject.FileName, LVersion.Name]));

              if (FOnTryPrepareProjectInstallation <> nil) and
                not FOnTryPrepareProjectInstallation(AGroupProjects[J].Items[K], LInfo) then
              begin
                TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']);
                Result := False;
                Exit;
              end;
              LProject := AGroupProjects[J].Items[K].Project;

              // Getting Bpl filename
              if not TryGetRADStudioProjectBplOutputFileName(LProject, LBplFileName) then
              begin
                TryShowErrorFmt(CustomMessage('SetupMainCouldNotDetectProjectBplOutputFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]);
                Result := False;
                Exit;
              end;
              if (not LProject.IsDesignOnly) and (pfWin64 in LProject.Platforms) and (GetRADStudioCommonBplDir(LInfo, pfWin64) <> '') then
                CopyFileToDir(ExpandProjectPath(LBplFileName, LVersion.BuildConfig, pfWin64), GetRADStudioCommonBplDir(LInfo, pfWin64));
              LBplFileName := ExpandProjectPath(LBplFileName, LVersion.BuildConfig, pfWin32);
              if not FileExists(LBplFileName) then
              begin
                TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '2']);
                Result := False;
                Exit;
              end;
              if (not LProject.IsDesignOnly) and (GetRADStudioCommonBplDir(LInfo, pfWin32) <> '') then
                CopyFileToDir(LBplFileName, GetRADStudioCommonBplDir(LInfo, pfWin32));

              // Registering Bpl
              if LProject.IsInstallable then
              begin
                if not TryRegisterRADStudioBpl(LVersion, LBplFileName, LProject.Description) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']);
                  Result := False;
                  Exit;
                end;
                if not TryAddRADStudioPathEnvVariable(LVersion, RemoveBackslash(ExtractFilePath(LBplFileName))) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '4']);
                  Result := False;
                  Exit;
                end;
              end;

              // Adding search paths
              for LPlatform := LowProjectPlatform to HighProjectPlatform do
              begin
                if (not (LPlatform in LProject.Platforms)) or (not CheckIfRADStudioSupportsPlatform(LInfo, LPlatform)) then
                  Continue;
                for L := 0 to GetArrayLength(LProject.SourcePaths) - 1 do
                begin
                  if not TryAddRADStudioLibrarySearchPath(LVersion, LPlatform, LProject.SourcePaths[L]) then
                  begin
                    TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '5']);
                    Result := False;
                    Exit;
                  end;
                end;
                if (LPlatform in GetPlatformsAllowedToBuild(LInfo)) then
                begin
                  LDcpFileName := ExpandProjectPath(GetRADStudioProjectDcpOutputFileName(LProject), LVersion.BuildConfig, LPlatform);
                  if not FileExists(LDcpFileName) then
                  begin
                    TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '6']);
                    Result := False;
                    Exit;
                  end;
                  if not TryAddRADStudioLibrarySearchPath(LVersion, LPlatform, ExpandProjectPath(LProject.DCUOutputPath, LVersion.BuildConfig, LPlatform)) then
                  begin
                    TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '7']);
                    Result := False;
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      if not Result then
      begin
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainRevertingPackagesInstallationAfterFailure');
        Log(CustomMessage('SetupMainRevertingPackagesInstallationAfterFailure'));
        UninstallPackages(ARADStudioInfos, AGroupProjects);
      end;
    end;
  end;
  WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Max;
end;

procedure _MakeProjectsForUnofficialVersionsSupported(var AGroupProjects: TRADStudioGroupProjects;
  const ARADStudioInfos: TRADStudioInfos; const AMaxVersionOfficiallySupported: TRADStudioVersion);
var
  I, J, K, L, M: Integer;
  LNewGroupProject: TRADStudioGroupProject;
  LNewPath: string;
  LFiles: TArrayOfString;
begin
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    if CompareRADStudioVersions(ARADStudioInfos[I].Version, AMaxVersionOfficiallySupported) > 0 then
    begin
      for J := 0 to GetArrayLength(AGroupProjects) - 1 do
      begin
        if ContainsString(SplitString(AGroupProjects[J].FileName, '\'), AMaxVersionOfficiallySupported.Name, False) then
        begin
          LNewPath := ExtractFilePath(ReplaceFolderNameInPath(AGroupProjects[J].FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False));
          if CopyDirectory(ExtractFilePath(AGroupProjects[J].FileName), LNewPath, True) then
          begin
            LFiles := GetFiles(LNewPath, '*', soAllDirectories);
            for K := 0 to GetArrayLength(LFiles) - 1 do
              ReplaceStringInFile(LFiles[K], AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            LNewGroupProject := CopyRADStudioGroupProject(AGroupProjects[J]);
            LNewGroupProject.FileName := ReplaceFolderNameInPath(LNewGroupProject.FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            for L := 0 to GetArrayLength(LNewGroupProject.Items) - 1 do
            begin
              LNewGroupProject.Items[L].Project.ProjectVersion := ARADStudioInfos[I].Version.MaxDprojVersion;
              LNewGroupProject.Items[L].Project.FileName := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.FileName, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              LNewGroupProject.Items[L].Project.BplOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.BplOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              LNewGroupProject.Items[L].Project.DcpOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.DcpOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              LNewGroupProject.Items[L].Project.DCUOutputPath := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.DCUOutputPath, AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
              for M := 0 to GetArrayLength(LNewGroupProject.Items[L].Project.SourcePaths) - 1 do
                LNewGroupProject.Items[L].Project.SourcePaths[M] := ReplaceFolderNameInPath(LNewGroupProject.Items[L].Project.SourcePaths[M], AMaxVersionOfficiallySupported.Name, ARADStudioInfos[I].Version.Name, False);
            end;
            SetArrayLength(AGroupProjects, GetArrayLength(AGroupProjects) + 1);
            AGroupProjects[GetArrayLength(AGroupProjects) - 1] := LNewGroupProject;
          end;
        end;
      end;
    end;
  end;

  for I := 0 to GetArrayLength(AGroupProjects) - 1 do
    for J := 0 to GetArrayLength(AGroupProjects[I].Items) - 1 do
      Log(Format('_MakeProjectsForUnofficialVersionsSupported: f "%s" g "%s"...', [AGroupProjects[I].Items[J].Project.FileName, AGroupProjects[I].FileName]));
end;

function _NeedsDirPage: Boolean;
begin
  #ifdef FilesEmbedded
  Result := True
  #else
  Result := False;
  #endif
end;

function NeedsUninstallRegKey: Boolean;
begin
  #ifdef FilesEmbedded
  Result := StrToBoolDef(ExpandConstant('{param:CreateUninstallRegKey|yes}'), True);
  #else
  Result := False;
  #endif
end;

function ShouldSkipPage(APageID: Integer): Boolean;
begin
  Result := (APageID = wpSelectDir) and not _NeedsDirPage;
end;

function UninstallPackages(const ARADStudioInfos: TRADStudioInfos; const AGroupProjects: TRADStudioGroupProjects): Boolean;
var
  I, J, K, L: Integer;
  LBplFileName: string;
  LConfig: TProjectConfig;
  LFileName: string;
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LPlatform: TProjectPlatform;
begin
  Result := True;
  Log('Setup.Main.UninstallPackages: Starting...');
  for I := 0 to GetArrayLength(ARADStudioInfos) - 1 do
  begin
    LInfo := ARADStudioInfos[I];
    if LInfo.Status in [riNotFound, riNeedOpenFirst] then
      Continue;
    for J := 0 to GetArrayLength(AGroupProjects) - 1 do
    begin
      for K := 0 to GetArrayLength(AGroupProjects[J].Items) - 1 do
      begin
        LProject := AGroupProjects[J].Items[K].Project;
        if TryGetRADStudioVersionOfProject(LProject, LVersion) and (CompareRADStudioVersions(LVersion, LInfo.Version) = 0) then
        begin
          LVersion := LInfo.Version;
          Log(Format('Setup.Main.UninstallPackages: Starting uninstallation of package "%s" in "%s"...', [LProject.FileName, LVersion.Name]));

          // Unregistering Bpl
          if TryGetRADStudioProjectBplOutputFileName(LProject, LBplFileName) then
          begin
            if GetRADStudioCommonBplDir(LInfo, pfWin32) <> '' then
              DeleteFile(CombinePath(GetRADStudioCommonBplDir(LInfo, pfWin32), ExtractFileName(LBplFileName)));
            if GetRADStudioCommonBplDir(LInfo, pfWin64) <> '' then
              DeleteFile(CombinePath(GetRADStudioCommonBplDir(LInfo, pfWin64), ExtractFileName(LBplFileName)));
            for LConfig := LowProjectConfig to HighProjectConfig do
            begin
              LFileName := ExpandProjectPath(LBplFileName, LConfig, pfWin32);
              if not TryUnregisterRADStudioBpl(LVersion, LFileName) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
                Result := False;
              end;
              if not TryRemoveRADStudioPathEnvVariable(LVersion, RemoveBackslash(ExtractFilePath(LFileName))) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '2']));
                Result := False;
              end;
            end;
          end
          else
          begin
            Log(FmtMessage(CustomMessage('SetupMainCouldNotDetectProjectBplOutputFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]));
            Result := False;
          end;

          if (FOnTryPrepareProjectUninstallation <> nil) and not FOnTryPrepareProjectUninstallation(AGroupProjects[J].Items[K], LInfo) then
          begin
            Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']));
            Result := False;
          end;
          LProject := AGroupProjects[J].Items[K].Project;
            
          // Removing search paths
          for LPlatform := LowProjectPlatform to HighProjectPlatform do
          begin
            if (not (LPlatform in LProject.Platforms)) or (not CheckIfRADStudioSupportsPlatform(LInfo, LPlatform)) then
              Continue;
            if LPlatform in GetPlatformsAllowedToBuild(LInfo) then
            begin
              if (not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, ExpandProjectPath(LProject.DCUOutputPath, pcRelease, LPlatform))) or
                (not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, ExpandProjectPath(LProject.DCUOutputPath, pcDebug, LPlatform))) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '4']));
                Result := False;
              end;
            end;
            for L := 0 to GetArrayLength(LProject.SourcePaths) - 1 do
            begin
              if not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, LProject.SourcePaths[L]) then
              begin
                Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '5']));
                Result := False;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

// end.
#endif
