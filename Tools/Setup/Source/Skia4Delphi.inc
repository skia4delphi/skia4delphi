#ifndef UnitSkia4Delphi
#define UnitSkia4Delphi

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
// unit Skia4Delphi;

// interface

// implementation

// uses
  #include "Source\IO.Utils.inc"
  #include "Source\RADStudio.inc"
  #include "Source\RADStudio.Project.inc"
  #include "Source\Setup.Main.inc"
  #include "Source\String.Utils.inc"

const
  LibraryDirVariable = 'SKIADIR';
  LibraryDirDefine = '$(' + LibraryDirVariable + ')';
  LibraryWin32BinaryDir = 'Binary\Shared\Win32';
  LibraryStaticBinaryDir = 'Binary\Static';

/// <summary> Get platforms where the library is static </summary>
function _GetLibraryStaticPlatforms: TProjectPlatforms; forward;
/// <summary> Make custom changes before project build </summary>
procedure _OnBeforeProjectBuild(const AProject: TRADStudioProject; const APlatform: TProjectPlatform; const AInfo: TRADStudioInfo); forward;
/// <summary> Get the custom param on build </summary>
function _OnGetBuildCustonParam(): string; forward;
/// <summary> Try to extract pre-build objects binaries when the command line compilation has failed </summary>
function _OnTryExtractPreBuildObjects(const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Make custom changes before the installation </summary>
function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Make custom changes before the uninstallation </summary>
function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Remove the generated hpp files of old setups </summary>
procedure _RemoveGeneratedHppFiles(const AInfo: TRADStudioInfo); forward;
/// <summary> Remove old library search path </summary>
procedure _RemoveOldLibrarySearchPath(const AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo); forward;
/// <summary> Try get old RAD Studio name used in our old setup </summary>
function _TryGetOldRADStudioName(const ARADStudioVersion: TRADStudioVersion; out AName: string): Boolean; forward;
/// <summary> Try to unregister old bpl </summary>
procedure _UnregisterOldBpl(const AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo); forward;

var
  _FRADStudioInstalledList: TArrayOfString;
  _FRADStudioUninstalledList: TArrayOfString;

function _GetLibraryStaticPlatforms: TProjectPlatforms;
begin
  Result := [pfiOSDevice64, pfiOSSimARM64];
end;

procedure _OnBeforeProjectBuild(const AProject: TRADStudioProject; const APlatform: TProjectPlatform; const AInfo: TRADStudioInfo);
var
  I, J: Integer;
  LFileFound: Boolean;
  LNativeSkiaUnits: TArrayOfString;
  LNativeUnit: string;
  LSourcePath: string;
begin
  // Check if it is newer than RAD Studio 11 Alexandria
  if CompareRADStudioVersion(AInfo.Version, '22.0') > 0 then
  begin 
    // Try copy exclusive units from RAD Studio source to recompile it
    if SameText(ExtractFileName(AProject.FileName), 'Skia.Package.FMX.dproj') and (APlatform = LowProjectPlatform) and (GetArrayLength(AProject.SourcePaths) > 0) then
    begin
      LSourcePath := AProject.SourcePaths[0];
      StringChangeEx(LSourcePath, LibraryDirDefine, '{app}', True);
      LSourcePath := ExpandConstant(LSourcePath);
      LNativeSkiaUnits := GetFiles(CombinePath(AInfo.RootDir, 'Source\FMX\'), 'FMX.Skia.*.pas', soTopDirectoryOnly);
      Log(Format('Skia4Delphi._OnBeforeProjectBuild: copying exclusive files to "%s"', [LSourcePath]));
      for I := 0 to GetArrayLength(LNativeSkiaUnits) - 1 do
      begin
        LNativeUnit := LNativeSkiaUnits[I];
        LFileFound := False;
        for J := 0 to GetArrayLength(AProject.SourceFiles) - 1 do
        begin
          if SameText(ExtractFileName(LNativeUnit), ExtractFileName(AProject.SourceFiles[J])) then
          begin
            LFileFound := True;
            Break;
          end;
        end;
        if not LFileFound then
          CopyFileToDir(LNativeUnit, LSourcePath);
      end;
    end;
  end;
end;

function _OnGetBuildCustonParam(): string;
begin
  Result := 'Skia4Delphi_Setup=true';
end;

function _OnTryExtractPreBuildObjects(const AInfo: TRADStudioInfo): Boolean;
#ifdef UseLibraryDCUFolder
#ifdef FilesEmbedded
var
  LDCUPath: string;
  LExtractedPath: string;
#endif
begin
  #ifdef FilesEmbedded
  LDCUPath := AddBackslash(AddBackslash('{#LibraryDCUFolder}') + AInfo.Version.Name);
  LDCUPath := AddBackslash(CombinePath('{app}', LDCUPath));
  LExtractedPath := ExpandConstant('{tmp}\') + LDCUPath;
  if DirExists(LExtractedPath) then
  begin
    Result := DelTree(LExtractedPath, True, True, True);
    if not Result then
    begin
      Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: Failed to delete the directory "%s"', [LExtractedPath]));
      Exit;
    end;
  end;
  Result := (ExtractTemporaryFiles(CombinePath('{app}', AddBackslash('{#LibraryDCUFolder}')) + '*') > 0);
  if not Result then
  begin
    Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: Failed to extract the temporary files of "%s"', [CombinePath('{app}', AddBackslash('{#LibraryDCUFolder}'))]));
    Exit;
  end;
  Result := (GetArrayLength(GetFiles(LExtractedPath, '*', soAllDirectories)) > 0);
  if not Result then
  begin
    Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: No pre-build objects found for %s', [AInfo.Version.Name]));
    Exit;
  end;
  LDCUPath := ExpandConstant(LDCUPath);
  if DirExists(LDCUPath) then
  begin
    Result := DelTree(LDCUPath, True, True, True);
    if not Result then
    begin
      Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: Failed to delete the directory "%s"', [LDCUPath]));
      Exit;
    end;
  end;
  Result := CopyDirectory(LExtractedPath, LDCUPath, True);
  if not Result then
    Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: Failed to copy the directory "%s" to "%s"', [LExtractedPath, LDCUPath]));
  #else
  Result := False;
  #endif
end;
#else
begin
  Log(Format('Skia4Delphi._OnTryExtractPreBuildObjects: No pre-build objects found for %s', [AInfo.Version.Name]));
  Result := False;
end;
#endif

function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
  LPlatform: TProjectPlatform;
begin
  Log(Format('Skia4Delphi._OnTryPrepareProjectInstallation: Preparing package "%s" before install...', [AProjectItem.Project.FileName]));
  if not ContainsString(_FRADStudioInstalledList, AInfo.Version.RegVersion, False) then
  begin
    _FRADStudioInstalledList := AppendString(_FRADStudioInstalledList, AInfo.Version.RegVersion, False);
    _RemoveGeneratedHppFiles(AInfo);
    // Removing old path
    TryRemoveRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, 'Binary\iOSDevice64'));
  end;
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryAddRADStudioEnvVariable(AInfo.Version, LibraryDirVariable, ExpandConstant('{app}')) and
    TryAddRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir));
  if Result then
  begin
    for LPlatform := LowProjectPlatform to HighProjectPlatform do
      if (LPlatform in _GetLibraryStaticPlatforms) and (LPlatform in AInfo.Version.SupportedPlatforms) then
        TryAddRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(CombinePath(LibraryDirDefine, LibraryStaticBinaryDir), GetProjectPlatformName(LPlatform)));
  end
  else
    Log(Format('Skia4Delphi._OnTryPrepareProjectInstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
  LPlatform: TProjectPlatform;
begin
  Log(Format('Skia4Delphi._OnTryPrepareProjectUninstallation: Preparing package "%s" to uninstall...', [AProjectItem.Project.FileName]));
  if not ContainsString(_FRADStudioUninstalledList, AInfo.Version.RegVersion, False) then
  begin
    _FRADStudioUninstalledList := AppendString(_FRADStudioUninstalledList, AInfo.Version.RegVersion, False);
    _RemoveGeneratedHppFiles(AInfo);
  end;
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryRemoveRADStudioEnvVariable(AInfo.Version, LibraryDirVariable);
  Result := TryRemoveRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir)) and Result;
  for LPlatform := LowProjectPlatform to HighProjectPlatform do
    if (LPlatform in _GetLibraryStaticPlatforms) and (LPlatform in AInfo.Version.SupportedPlatforms) then
      TryRemoveRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(CombinePath(LibraryDirDefine, LibraryStaticBinaryDir), GetProjectPlatformName(LPlatform)));
  _RemoveOldLibrarySearchPath(AProjectItem, AInfo);
  _UnregisterOldBpl(AProjectItem, AInfo);
  if not Result then
    Log(Format('Skia4Delphi._OnTryPrepareProjectUninstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

procedure _RemoveGeneratedHppFiles(const AInfo: TRADStudioInfo);
var
  LGeneratedHppDir: string;
  LGeneratedHppPlatformDir: string;
  LPlatform: TProjectPlatform;
begin
  if (AInfo.BdsCommonDir <> '') and (AInfo.BdsInclude <> '') then
  begin
    LGeneratedHppDir := CombinePath(AInfo.BdsCommonDir, 'hpp');
    if DirExists(LGeneratedHppDir) and (LGeneratedHppDir <> AInfo.BdsInclude) then
    begin
      Log(Format('Skia4Delphi._RemoveGeneratedHppFiles: Removing old generated hpp of "%s"...', [AInfo.Version.Name]));
      for LPlatform := LowProjectPlatform to HighProjectPlatform do
      begin
        LGeneratedHppPlatformDir := CombinePath(LGeneratedHppDir, GetProjectPlatformName(LPlatform));
        if not DirExists(LGeneratedHppPlatformDir) then
          Continue;
        // Removes the generated hpp from old versions - Old namespace
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.API.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Bindings.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.FMX.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.FMX.Canvas.GL.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.FMX.Canvas.Metal.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.FMX.Graphics.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Vcl.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.FMX.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.RTL.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.VCL.hpp'));
        // Removes the generated hpp from old versions - New namespace (v6+)
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'System.Skia.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'System.Skia.API.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'FMX.Skia.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'FMX.Skia.Canvas.GL.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'FMX.Skia.Canvas.Metal.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'FMX.Skia.Canvas.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Vcl.Skia.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.FMX.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.RTL.hpp'));
        DeleteFile(CombinePath(LGeneratedHppPlatformDir, 'Skia.Package.VCL.hpp'));
      end;
    end
    else
      Log(Format('Skia4Delphi._RemoveGeneratedHppFiles: Invalid paths in "%s" (LGeneratedHppDir: "%s", BdsInclude: "%s")', [AInfo.Version.Name, LGeneratedHppDir, AInfo.BdsInclude]));
  end
  else
    Log(Format('Skia4Delphi._RemoveGeneratedHppFiles: Invalid paths in "%s" (BdsCommonDir: "%s", BdsInclude: "%s")', [AInfo.Version.Name, AInfo.BdsCommonDir, AInfo.BdsInclude]));
end;

procedure _RemoveOldLibrarySearchPath(const AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo);
var
  LOldRADStudioName: string;
  LPlatform: TProjectPlatform;
begin
  Log(Format('Skia4Delphi._RemoveOldLibrarySearchPath: Removing old search path of "%s" project...', [AProjectItem.Project.FileName]));
  if SameText(ExtractFileName(AProjectItem.Project.FileName), 'Skia.Package.RTL.dproj') then
  begin
    if _TryGetOldRADStudioName(AInfo.Version, LOldRADStudioName) then
    begin
      for LPlatform := LowProjectPlatform to HighProjectPlatform do
      begin
        if not CheckIfRADStudioSupportsPlatform(AInfo, LPlatform) then
          Continue;
        TryRemoveRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(LibraryDirDefine, 'Library\' + LOldRADStudioName + '\' + GetProjectPlatformName(LPlatform) + '\Release'));
        TryRemoveRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(LibraryDirDefine, 'Library\' + LOldRADStudioName + '\' + GetProjectPlatformName(LPlatform) + '\Debug'));
      end;
    end;
    if AInfo.Version.RegVersion = '22.0' then
    begin
      for LPlatform := LowProjectPlatform to HighProjectPlatform do
      begin
        if not (LPlatform in GetPlatformsAllowedToBuild(AInfo)) then
          Continue;
        TryRemoveRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(LibraryDirDefine, 'Library\RAD Studio 11.0 Alexandria\' + GetProjectPlatformName(LPlatform) + '\Release'));
        TryRemoveRADStudioLibrarySearchPath(AInfo.Version, LPlatform, CombinePath(LibraryDirDefine, 'Library\RAD Studio 11.0 Alexandria\' + GetProjectPlatformName(LPlatform) + '\Debug'));
      end;
      TryRemoveRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, 'Binary\iOSDevice64\Release'));
    end;
  end;
end;

<event('InitializeSetup')>
function _Skia4DelphiInitializeSetup: Boolean;
begin
  FOnBeforeProjectBuild := @_OnBeforeProjectBuild;
  FOnGetBuildCustonParam := @_OnGetBuildCustonParam;
  FOnTryExtractPreBuildObjects := @_OnTryExtractPreBuildObjects;
  FOnTryPrepareProjectInstallation := @_OnTryPrepareProjectInstallation;
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  Result := True;
end;

<event('InitializeUninstall')>
function _Skia4DelphiInitializeUninstall: Boolean;
begin
  FOnTryPrepareProjectUninstallation := @_OnTryPrepareProjectUninstallation;
  Result := True;
end;

function _TryGetOldRADStudioName(const ARADStudioVersion: TRADStudioVersion; out AName: string): Boolean;
var
  LVersion: Int64;
  LMajor, LMinor, LRevision, LBuild: Word;
begin
  if not StrToVersion(ARADStudioVersion.RegVersion, LVersion) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  UnpackVersionComponents(LVersion, LMajor, LMinor, LRevision, LBuild);
  case LMajor of
    15: AName := 'DelphiXE7';
    16: AName := 'DelphiXE8';
    17: AName := 'Delphi10Seattle';
    18: AName := 'Delphi10Berlin';
    19: AName := 'Delphi10Tokyo';
    20: AName := 'Delphi10Rio';
    21: AName := 'Delphi10Sydney';
    22: AName := 'Delphi11Alexandria';
  else
    Result := False;
  end;
end;

procedure _UnregisterOldBpl(const AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo);
var
  LOldRADStudioName: string;
begin
  Log(Format('Skia4Delphi._UnregisterOldBpl: Unregistering old bpl of "%s" project...', [AProjectItem.Project.FileName]));
  if _TryGetOldRADStudioName(AInfo.Version, LOldRADStudioName) then
  begin
    if not TryUnregisterRADStudioBpl(AInfo.Version, ChangeFileExt(AProjectItem.Project.FileName, '.' + LOldRADStudioName + '.bpl')) then
      Log(Format('Skia4Delphi._UnregisterOldBpl: Failed to unregister old bpl of "%s" project...', [AProjectItem.Project.FileName]));
  end;
end;

// end.
#endif
