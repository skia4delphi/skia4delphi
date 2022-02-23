#ifndef UnitSetupMain
#define UnitSetupMain

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
  FOnGetBuildCustonParam: TGetBuildCustonParamEvent;
  FOnTryExtractPreBuildObjects: TTryExtractPreBuildObjectsProc;
  FOnTryPrepareProjectInstallation: TTryPrepareProjectInstallationEvent;
  FOnTryPrepareProjectUninstallation: TTryPrepareProjectUninstallationEvent;
  FRADStudioInfos: TRADStudioInfos;

// implementation

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
              TryShowErrorFmt(CustomMessage('SetupMainCannotPossibleToUninstallDetectedVersion'), ['{#LibraryName}']);
              Abort;
            end;
          end;
        end;
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingFromGetIt');
        TryRemoveFromGetIt(LSelectedRADStudioVersions, '{#LibraryName}');
        WizardForm.StatusLabel.Caption := CustomMessage('SetupMainUninstallingPackages');
        if not UninstallPackages(LSelectedRADStudioVersions, FGroupProjects) then
        begin
          TryShowError(CustomMessage('SetupMainCannotPossibleToUninstallPackages'));
          Abort;
        end;
      end;
    ssPostInstall:
      begin                   
        LSelectedRADStudioVersions := GetSelectedRADStudioVersions;
        if GetArrayLength(LSelectedRADStudioVersions) <> 0 then
        begin
          Log('Start installation post extraction files...');
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
          TryShowError(CustomMessage('SetupMainCannotPossibleToUninstallPackages'));
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
    FRADStudioInfos := GetRADStudioInfosOfGroupProjects(FGroupProjects);
    Result := GetArrayLength(FRADStudioInfos) > 0;
    if not Result then
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
    FRADStudioInfos := GetRADStudioInfosOfGroupProjects(FGroupProjects)
  else
    TryShowError(CustomMessage('SetupMainErrorFailedToGetGroupProjects'));
end;

// Create wizard custom pages
procedure InitializeWizard;
begin
  CreateRADStudioVersionsChoicePage(wpWelcome, FRADStudioInfos);
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
  Result := TryBuildRADStudioPackages(ARADStudioInfos, AGroupProjects, LCustomParam, FOnTryExtractPreBuildObjects);
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
              Log(Format('Setup.Main.InstallPackages: Starting installation of package "%s"...', [AGroupProjects[J].Items[K].Project.FileName]));
              if (FOnTryPrepareProjectInstallation <> nil) and
                not FOnTryPrepareProjectInstallation(AGroupProjects[J].Items[K], LInfo) then
              begin      
                TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']);
                Result := False;
                Exit;
              end;
              LProject := AGroupProjects[J].Items[K].Project;

              // Registering Bpl
              if LProject.IsInstallable then
              begin
                if not TryGetRADStudioProjectBplOutputFileName(LProject, LBplFileName) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainCannotPossibleDetectProjectBplOutputFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]);
                  Result := False;
                  Exit;
                end;
                LBplFileName := ExpandProjectPath(LBplFileName, pcRelease, pfWin32);
                if not FileExists(LBplFileName) then
                begin
                  TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '2']);
                  Result := False;
                  Exit;
                end;
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
                  LDcpFileName := ExpandProjectPath(GetRADStudioProjectDcpOutputFileName(LProject), pcRelease, LPlatform);
                  if not FileExists(LDcpFileName) then
                  begin
                    TryShowErrorFmt(CustomMessage('SetupMainFailedToInstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '6']);
                    Result := False;
                    Exit;
                  end;
                  if not TryAddRADStudioLibrarySearchPath(LVersion, LPlatform, ExpandProjectPath(LProject.DCUOutputPath, pcRelease, LPlatform)) then
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
  LInfo: TRADStudioInfo;
  LProject: TRADStudioProject;
  LVersion: TRADStudioVersion;
  LBplFileName: string;
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
          // Unregistering Bpl
          if TryGetRADStudioProjectBplOutputFileName(LProject, LBplFileName) then
          begin
            LBplFileName := ExpandProjectPath(LBplFileName, pcRelease, pfWin32);
            if not TryUnregisterRADStudioBpl(LVersion, LBplFileName) then
            begin
              Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '1']));
              Result := False;
            end; 
            if not TryRemoveRADStudioPathEnvVariable(LVersion, RemoveBackslash(ExtractFilePath(LBplFileName))) then
            begin
              Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '2']));
              Result := False;
            end;
          end
          else
          begin
            Log(FmtMessage(CustomMessage('SetupMainCannotPossibleDetectProjectBplOutputFileName'), [ExtractFileName(LProject.FileName), LVersion.Name]));
            Result := False;
          end;

          if (FOnTryPrepareProjectUninstallation <> nil) and not FOnTryPrepareProjectUninstallation(AGroupProjects[J].Items[K], LInfo) then
          begin
            Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name, '3']));
            Result := False;
          end;
          LProject := AGroupProjects[J].Items[K].Project;
            
          // Adding search paths
          for LPlatform := LowProjectPlatform to HighProjectPlatform do
          begin   
            if (not (LPlatform in LProject.Platforms)) or (not CheckIfRADStudioSupportsPlatform(LInfo, LPlatform)) then
              Continue;
            if (LPlatform in GetPlatformsAllowedToBuild(LInfo)) and not TryRemoveRADStudioLibrarySearchPath(LVersion, LPlatform, ExpandProjectPath(LProject.DCUOutputPath, pcRelease, LPlatform)) then
            begin
              Log(FmtMessage(CustomMessage('SetupMainFailedToUninstallPackage'), [ExtractFileName(LProject.FileName), LVersion.Name + ' - ' + GetProjectPlatformName(LPlatform), '4']));
              Result := False;
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
