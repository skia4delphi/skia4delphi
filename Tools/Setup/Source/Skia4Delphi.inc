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
  #include "Source\RADStudio.inc"
  #include "Source\RADStudio.Project.inc"
  #include "Source\Setup.Main.inc"

const
  LibraryDirVariable = 'SKIADIR';
  LibraryDirDefine = '$(' + LibraryDirVariable + ')';
  LibraryWin32BinaryDir = 'Binary\Win32\Release';
  LibraryiOSDevice64BinaryDir = 'Binary\iOSDevice64\Release';

/// <summary> Make custom changes before the installation </summary>
function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;  
/// <summary> Make custom changes before the uninstallation </summary>
function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean; forward;

function _OnTryPrepareProjectInstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
begin
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryAddRADStudioEnvVariable(AInfo.Version, LibraryDirVariable, ExpandConstant('{app}')) and
    TryAddRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir));
  if Result then
    TryAddRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, LibraryiOSDevice64BinaryDir));
end;

function _OnTryPrepareProjectUninstallation(var AProjectItem: TRADStudioGroupProjectItem; const AInfo: TRADStudioInfo): Boolean;
var
  I: Integer;
  LAppPath: string;
begin
  LAppPath := ExpandConstant('{app}');
  for I := 0 to GetArrayLength(AProjectItem.Project.SourcePaths) - 1 do
    StringChangeEx(AProjectItem.Project.SourcePaths[I], LAppPath, LibraryDirDefine, True);
  StringChangeEx(AProjectItem.Project.DCUOutputPath, LAppPath, LibraryDirDefine, True);
  Result := TryRemoveRADStudioEnvVariable(AInfo.Version, LibraryDirVariable);
  Result := TryRemoveRADStudioPathEnvVariable(AInfo.Version, CombinePath(LAppPath, LibraryWin32BinaryDir)) and Result;
  Result := TryRemoveRADStudioLibrarySearchPath(AInfo.Version, pfiOSDevice64, CombinePath(LibraryDirDefine, LibraryiOSDevice64BinaryDir)) and Result;
end;

<event('InitializeSetup')>
function _Skia4DelphiInitializeSetup: Boolean;
begin
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
