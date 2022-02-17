#ifndef UnitSkia4Delphi
#define UnitSkia4Delphi

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
// unit Skia4Delphi;

// interface

// implementation

// uses
  #include "Source\IO.Utils.inc"
  #include "Source\RADStudio.inc"
  #include "Source\RADStudio.Project.inc"
  #include "Source\Setup.Main.inc"

const
  LibraryDirVariable = 'SKIADIR';
  LibraryDirDefine = '$(' + LibraryDirVariable + ')';
  LibraryWin32BinaryDir = 'Binary\Win32\Release';
  LibraryiOSDevice64BinaryDir = 'Binary\iOSDevice64\Release';

/// <summary> Try to extract pre-build objects binaries when the command line compilation has failed </summary>
function _OnTryExtractPreBuildObjects(const AInfo: TRADStudioInfo): Boolean; forward;
/// <summary> Make custom changes before the installation </summary>
function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;  
/// <summary> Make custom changes before the uninstallation </summary>
function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;

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
begin
  Log(Format('Skia4Delphi._OnTryPrepareProjectInstallation: Preparing package "%s" before install...', [AProjectItem.Project.FileName]));
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryAddRADStudioEnvVariable(AInfo.Version, LibraryDirVariable, ExpandConstant('{app}')) and
    TryAddRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir));
  if Result then
    TryAddRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, LibraryiOSDevice64BinaryDir))
  else
    Log(Format('Skia4Delphi._OnTryPrepareProjectInstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
begin
  Log(Format('Skia4Delphi._OnTryPrepareProjectUninstallation: Preparing package "%s" to uninstall...', [AProjectItem.Project.FileName]));
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryRemoveRADStudioEnvVariable(AInfo.Version, LibraryDirVariable);
  Result := TryRemoveRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir)) and Result;
  Result := TryRemoveRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, LibraryiOSDevice64BinaryDir)) and Result;
  if not Result then
    Log(Format('Skia4Delphi._OnTryPrepareProjectUninstallation: Failed to prepare the project "%s"', [AProjectItem.Project.FileName]));
end;

<event('InitializeSetup')>
function _Skia4DelphiInitializeSetup: Boolean;
begin
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

// end.
#endif
