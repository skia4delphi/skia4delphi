

; Custom Parameters
; -----------------------------------------------------------------------------------
;
;   /DefaultRADStudio=
;     Values allowed: 4.0 to 22.0
;     Default: (latest found in computer)
;     Description: Used to set the default RAD Studio version that will install the library. Ex: /RADStudioVersion=22.0 will check only in RAD Studio 11 Alexandria. Without set this parameter, the value will be the newest RAD Studio found on the machine.

;   /DefaultPlatforms=
;     Values allowed: windows|all
;     Default: windows
;     Description: Plataforms checked by default. If "windows", then Win32 and Win64 will be checked by default, but if is "all" then all platforms available for the selected platforms will be checked.

;   /CreateUninstallRegKey=
;     Values allowed: no|yes or false|true
;     Default: yes
;     Description: When true the uninstall shortcut in applications panel will be created and before the setup starts will call the uninstall of others versions
;
;   /CloseRadStudioMessage
;     Values allowed: no|yes or false|true
;     Default: no
;     Description: When true the and the rad studio is running, will show a warning in installation and an error in uninstaller
;
; -----------------------------------------------------------------------------------
;  Example of command line to install silent:
;     start /b /d "C:\skia4delphi" /wait cmd /c "C:\skia4delphi\Setup.exe" /SP- /verysilent /DefaultRADStudio=22.0 /DefaultPlatforms=all /CreateUninstallRegKey=false /CloseRadStudioMessage=false
;  Example of command line to install silent:
;     start /b /d "C:\skia4delphi" /wait cmd /c "C:\skia4delphi\unins000.exe" /SP- /verysilent /DefaultRADStudio=22.0 /CloseRadStudioMessage=false
; -----------------------------------------------------------------------------------

#define LibraryName "Skia4Delphi"
#define LibraryVersion "2.0.1"
#define LibraryPublisher "Skia4Delphi Team"
#define LibraryCopyright "Copyright (c) 2021 Skia4Delphi Project"
#define LibraryURL "https://skia4delphi.org"
#define LibrarySamplesFolder "Samples"
#define LibraryDocumentationURL "https://github.com/viniciusfbb/skia4delphi"
#define LibrarySupportURL "https://github.com/viniciusfbb/skia4delphi/issues/"
#define LibraryUpdatesURL "https://github.com/viniciusfbb/skia4delphi/releases/"
#define Images "..\..\Assets\Setup\image.bmp"
#define FilesEmbedded

[Setup]
; AppId uniquely identifies this application, don't use the same AppId value in installers for other applications.
AppId={{A8524AB1-6BAE-4AEF-9E67-8F1D086BE3C7}
AppName={#LibraryName}
AppVersion={#LibraryVersion}
AppCopyright={#LibraryCopyright}
AppPublisher={#LibraryPublisher}
AppPublisherURL={#LibraryURL}
AppSupportURL={#LibrarySupportURL}
AppUpdatesURL={#LibraryUpdatesURL}
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName={#LibraryName}
DisableProgramGroupPage=yes
#ifdef FilesEmbedded
  OutputBaseFilename={#LibraryName}_{#LibraryVersion}_Setup
  OutputDir=Output\
#else
  OutputBaseFilename=Setup
  OutputDir=..\..\
#endif
WizardImageFile={#Images}
DisableWelcomePage=no
DisableReadyPage=yes
Compression=lzma
SolidCompression=yes
AllowCancelDuringInstall=yes
CloseApplications=no
PrivilegesRequired=lowest
Uninstallable=yes
CreateUninstallRegKey=NeedsUninstallRegKey
DisableDirPage=no

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
#ifdef FilesEmbedded
  Source: "..\..\*"; Excludes: "*.exe,*.dll,*.bpl,*.bpi,*.dcp,*.so,*.apk,*.drc,*.map,*.dres,*.rsm,*.tds,*.dcu,*.lib,*.jdbg,*.plist,*.cfg,*Resource.rc,*.cfg,*Resource.rc,*.local,*.identcache,*.projdata,*.tvsconfig,*.skincfg,*.cbk,*.dsk,__history\*,__recovery\*,*.~*,*.stat,modules\*,*template*\*,*template*,*.a,*.dex,*.o,*.vrc,*.res,*.log,*.deployproj,Externals\*,Binary\*,Logs\*,Bpl\*,*.Logs.txt"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion
  Source: "..\..\Binary\*"; DestDir: "{app}\Binary"; Flags: recursesubdirs ignoreversion
#endif

[Icons]
Name: "{group}\Uninstall"; Filename: "{uninstallexe}"

[Messages]
SetupWindowTitle=%1 Setup
WelcomeLabel1=[name] Setup
WelcomeLabel2=This will install [name/ver] on your computer.
FinishedHeadingLabel=Completing [name] Setup
FinishedLabel=Setup has finished installing [name] on your computer.

[CustomMessages]
UninstallAbortedToCloseDelphiInstance=Uninstall aborted! Before uninstall, close all instances of Delphi / RADStudio.
InstallationSuccesfullyRestartDelphi=Installation succesfully. Please restart your Delphi / RADStudio.
ChooseDelphVersionsTitle=Choose Delphi Versions
ChooseDelphVersionsMessage=Check the Delphi versions do you want to install the {#LibraryName}
ChooseDelphVersionsWarningMissingDelphiVersion=%n%nIt is possible that some version of Delphi has not been detected. In this case, just open and close the Delphi version in question, and reopen the installer.
ChooseDelphiPlatformsTitle=Choose Delphi Platforms
ChooseDelphiPlatformsMessage=Check the Delphi platforms do you want to install the {#LibraryName}
CannotPossibleToRemoveOldFiles=The uninstall cannot be fully done. Make sure no Delphi processes are running and try again.
RemovingOldFiles=Removing old files...
CompilingFor=Compiling for %1...
ErrorCompilingFor=Error compiling for %1 (code %2, message "%3").%n%nPlease, report this issue.
InstallingPackages=Installing packages...
CannotPossibleToUninstallDetectedVersion=Cannot possible to uninstall another versions of {#LibraryName} detected in your system.
UninstallingDetectedVersion=Uninstalling another versions
ErrorCantFoundRsVars=Can't found the rsvars file "%1"

[Run]
Filename: "{app}\{#LibrarySamplesFolder}"; Description: "Open samples folder"; Flags: shellexec runasoriginaluser postinstall;
Filename: "{#LibraryDocumentationURL}"; Description: "View online documentation"; Flags: shellexec runasoriginaluser postinstall unchecked;

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assets\*";
Type: filesandordirs; Name: "{app}\Binary\*";
Type: filesandordirs; Name: "{app}\Documents\*";
Type: filesandordirs; Name: "{app}\Externals\*";
Type: filesandordirs; Name: "{app}\Library\*";
Type: filesandordirs; Name: "{app}\Packages\*";
Type: filesandordirs; Name: "{app}\Samples\*";
Type: filesandordirs; Name: "{app}\Source\*";
Type: filesandordirs; Name: "{app}\Tools\*";
Type: filesandordirs; Name: "{app}\Website\*";
Type: filesandordirs; Name: "{app}\.gitignore";
Type: filesandordirs; Name: "{app}\.gitmodules";
Type: filesandordirs; Name: "{app}\LICENSE";
Type: filesandordirs; Name: "{app}\README.md";
Type: filesandordirs; Name: "{app}\Setup.exe";
Type: filesandordirs; Name: "{app}\SKIA-LICENSE";
Type: dirifempty; Name: "{app}\Assets";
Type: dirifempty; Name: "{app}\Binary";
Type: dirifempty; Name: "{app}\Documents";
Type: dirifempty; Name: "{app}\Externals";
Type: dirifempty; Name: "{app}\Library";
Type: dirifempty; Name: "{app}\Packages";
Type: dirifempty; Name: "{app}\Samples";
Type: dirifempty; Name: "{app}\Source";
Type: dirifempty; Name: "{app}\Tools";
Type: dirifempty; Name: "{app}\Website";
Type: dirifempty; Name: "{app}";

[Code]
{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
type
  TSetupKind = (skInstalling, skUninstalling);
  TDelphiVersion = (D7, D2005, D2007, D2009, D2010, DXE, DXE2, DXE3, DXE4, DXE5, DXE6, DXE7, DXE8, D10, D101, D102, D103, D104, D11);
  TDelphiVersions = array of TDelphiVersion;
  TDelphiPlatform = (pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32, pfOSX64, pfOSXARM64, pfLinux64);
  TDelphiPlatforms = array of TDelphiPlatform;
  TDelphiConfig = (cfRelease, cfDebug);

  TDelphiPackage = record
    Name: string;
    Description: string;
    SourcePaths: string;
    DCUOutputPath: string;
    Platforms: TDelphiPlatforms;
    Installable: Boolean;
    UseFiremonkey: Boolean;
    MinDelphiVersion: TDelphiVersion;
    MaxDelphiVersion: TDelphiVersion;
  end;

const
  LowDelphiVersion = D7;
  HighDelphiVersion = D11;
  LowDelphiPlatform = pfWin32;
  HighDelphiPlatform = pfLinux64;
  LowDelphiConfig = cfRelease;
  HighDelphiConfig = cfDebug;

var
  FSetupKind: TSetupKind;
  FDefaultPlatforms: TDelphiPlatforms;

  // Delphi versions
  FSupportedDelphiVersions: TDelphiVersions;
  FAvailableDelphiVersions: TDelphiVersions;
  FSelectedDelphiVersions: TDelphiVersions;
  FChooseDelphiVersionsPage: TInputOptionWizardPage;

  // Platforms
  FSupportedDelphiPlatforms: TDelphiPlatforms;
  FAvailableDelphiPlatforms: TDelphiPlatforms;
  FSelectedDelphiPlatforms: TDelphiPlatforms;
  FChooseDelphiPlatformsPage: TInputOptionWizardPage;

{************************************************************************}
{                                                                        }
{                               Config                                   }
{                                                                        }
{************************************************************************}
var
  FPackages: array of TDelphiPackage;
  FPlatformsInVersionWithLimitedPlatforms: TDelphiPlatforms;

const
  LibraryDirVariable = 'SKIADIR';
  LatestVersionWithLimitedPlatforms = D103;
  PackagesFolder = 'Packages';
  iOSDevice64ExtraLibraryPath = 'Binary\Static\iOSDevice64';

procedure SetConfigs;
var
  LPackage: TDelphiPackage;
begin
  FPlatformsInVersionWithLimitedPlatforms := [pfWin32, pfWin64];
  FPackages := [];

  LPackage.Name := 'Skia.Package.RTL';
  LPackage.Description := 'Skia4Delphi RTL package';
  LPackage.SourcePaths := 'Source';
  LPackage.DCUOutputPath := 'Library';
  LPackage.Platforms := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfOSX64, pfOSXARM64, pfLinux64];
  LPackage.Installable := False;
  LPackage.UseFiremonkey := False;
  LPackage.MinDelphiVersion := DXE6;
  LPackage.MaxDelphiVersion := D11;
  SetArrayLength(FPackages, GetArrayLength(FPackages) + 1);
  FPackages[GetArrayLength(FPackages) - 1] := LPackage;

  LPackage.Name := 'Skia.Package.FMX';
  LPackage.Description := 'Skia4Delphi FMX package';
  LPackage.SourcePaths := 'Source\FMX';
  LPackage.DCUOutputPath := 'Library';
  LPackage.Platforms := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfOSX64, pfOSXARM64, pfLinux64];
  LPackage.Installable := False;
  LPackage.UseFiremonkey := True;
  LPackage.MinDelphiVersion := DXE6;
  LPackage.MaxDelphiVersion := D11;
  SetArrayLength(FPackages, GetArrayLength(FPackages) + 1);
  FPackages[GetArrayLength(FPackages) - 1] := LPackage;

  LPackage.Name := 'Skia.Package.FMX.Designtime';
  LPackage.Description := 'Skia4Delphi FMX Designtime package';
  LPackage.SourcePaths := 'Source\FMX\Designtime';
  LPackage.DCUOutputPath := 'Library';
  LPackage.Platforms := [pfWin32];
  LPackage.Installable := True;
  LPackage.UseFiremonkey := True;
  LPackage.MinDelphiVersion := DXE6;
  LPackage.MaxDelphiVersion := D11;
  SetArrayLength(FPackages, GetArrayLength(FPackages) + 1);
  FPackages[GetArrayLength(FPackages) - 1] := LPackage;

  LPackage.Name := 'Skia.Package.VCL';
  LPackage.Description := 'Skia4Delphi VCL package';
  LPackage.SourcePaths := 'Source\VCL';
  LPackage.DCUOutputPath := 'Library';
  LPackage.Platforms := [pfWin32, pfWin64];
  LPackage.Installable := False;
  LPackage.UseFiremonkey := False;
  LPackage.MinDelphiVersion := DXE6;
  LPackage.MaxDelphiVersion := D11;
  SetArrayLength(FPackages, GetArrayLength(FPackages) + 1);
  FPackages[GetArrayLength(FPackages) - 1] := LPackage;

  LPackage.Name := 'Skia.Package.VCL.Designtime';
  LPackage.Description := 'Skia4Delphi VCL Designtime package';
  LPackage.SourcePaths := 'Source\VCL\Designtime';
  LPackage.DCUOutputPath := 'Library';
  LPackage.Platforms := [pfWin32];
  LPackage.Installable := True;
  LPackage.UseFiremonkey := False;
  LPackage.MinDelphiVersion := DXE6;
  LPackage.MaxDelphiVersion := D11;
  SetArrayLength(FPackages, GetArrayLength(FPackages) + 1);
  FPackages[GetArrayLength(FPackages) - 1] := LPackage;
end;

{************************************************************************}
{                                                                        }
{                               Consts                                   }
{                                                                        }
{************************************************************************}

function GetDelphiConfigName(const AConfig: TDelphiConfig): string;
begin
  case AConfig of
    cfRelease: Result := 'Release';
    cfDebug  : Result := 'Debug';
  else
    Result := '';
  end;
end;

// Get the Delphi path from the registry
function GetDelphiRegKey(const ADelphiVersion: TDelphiVersion): string;
begin
  case ADelphiVersion of
    D7   : Result := 'Software\Borland\Delphi\7.0';
    D2005: Result := 'Software\Borland\BDS\3.0';
    D2007: Result := 'Software\Borland\BDS\5.0';
    D2009: Result := 'Software\CodeGear\BDS\6.0';
    D2010: Result := 'Software\CodeGear\BDS\7.0';
    DXE  : Result := 'Software\Embarcadero\BDS\8.0';
    DXE2 : Result := 'Software\Embarcadero\BDS\9.0';
    DXE3 : Result := 'Software\Embarcadero\BDS\10.0';
    DXE4 : Result := 'Software\Embarcadero\BDS\11.0';
    DXE5 : Result := 'Software\Embarcadero\BDS\12.0';
    DXE6 : Result := 'Software\Embarcadero\BDS\14.0';
    DXE7 : Result := 'Software\Embarcadero\BDS\15.0';
    DXE8 : Result := 'Software\Embarcadero\BDS\16.0';
    D10  : Result := 'Software\Embarcadero\BDS\17.0';
    D101 : Result := 'Software\Embarcadero\BDS\18.0';
    D102 : Result := 'Software\Embarcadero\BDS\19.0';
    D103 : Result := 'Software\Embarcadero\BDS\20.0';
    D104 : Result := 'Software\Embarcadero\BDS\21.0';
    D11  : Result := 'Software\Embarcadero\BDS\22.0';
  else
    Result := '';
  end;
end;

// All platforms supported by delphi in each version
function GetSupportedPlatformsByDelphiVersion(const AVersion: TDelphiVersion): TDelphiPlatforms;
begin
  case AVersion of
    D7   : Result := [pfWin32];
    D2005: Result := [pfWin32];
    D2007: Result := [pfWin32];
    D2009: Result := [pfWin32];
    D2010: Result := [pfWin32];
    DXE  : Result := [pfWin32];
    DXE2 : Result := [pfWin32, pfWin64, pfOSX32];
    DXE3 : Result := [pfWin32, pfWin64, pfOSX32];
    DXE4 : Result := [pfWin32, pfWin64, pfiOSDevice32, pfiOSSimulator, pfOSX32];
    DXE5 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSSimulator, pfOSX32];
    DXE6 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSSimulator, pfOSX32];
    DXE7 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSSimulator, pfOSX32];
    DXE8 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
    D10  : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
    D101 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32];
    D102 : Result := [pfWin32, pfWin64, pfAndroid, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32, pfLinux64];
    D103 : Result := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice32, pfiOSDevice64, pfiOSSimulator, pfOSX32, pfOSX64, pfLinux64];
    D104 : Result := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimulator, pfOSX64, pfLinux64];
    D11  : Result := [pfWin32, pfWin64, pfAndroid, pfAndroid64, pfiOSDevice64, pfiOSSimulator, pfOSX64, pfOSXARM64, pfLinux64];
  else
    Result := [];
  end;
end;

// Friendly name of Delphi version, displayed to user
function GetDelphiVersionFriendlyName(const AVersion: TDelphiVersion): string;
begin
  case AVersion of
    D7   : Result := 'Delphi 7';
    D2005: Result := 'Delphi 2005';
    D2007: Result := 'Delphi 2007';
    D2009: Result := 'Delphi 2009';
    D2010: Result := 'Delphi 2010';
    DXE  : Result := 'Delphi XE';
    DXE2 : Result := 'Delphi XE2';
    DXE3 : Result := 'Delphi XE3';
    DXE4 : Result := 'Delphi XE4';
    DXE5 : Result := 'Delphi XE5';
    DXE6 : Result := 'Delphi XE6';
    DXE7 : Result := 'Delphi XE7';
    DXE8 : Result := 'Delphi XE8';
    D10  : Result := 'Delphi 10 Seattle';
    D101 : Result := 'Delphi 10.1 Berlin';
    D102 : Result := 'Delphi 10.2 Tokyo';
    D103 : Result := 'Delphi 10.3 Rio';
    D104 : Result := 'Delphi 10.4 Sydney';
    D11  : Result := 'Delphi 11 Alexandria';
  else
    Result := '';
  end;
end;

// Delphi version folder name used inside the library folders
function GetSourceFolderName(const ADelphiVersion: TDelphiVersion): string;
begin
  case ADelphiVersion of
    D7   : Result := 'Delphi7';
    D2005: Result := 'Delphi2005';
    D2007: Result := 'Delphi2007';
    D2009: Result := 'Delphi2009';
    D2010: Result := 'Delphi2010';
    DXE  : Result := 'DelphiXE';
    DXE2 : Result := 'DelphiXE2';
    DXE3 : Result := 'DelphiXE3';
    DXE4 : Result := 'DelphiXE4';
    DXE5 : Result := 'DelphiXE5';
    DXE6 : Result := 'DelphiXE6';
    DXE7 : Result := 'DelphiXE7';
    DXE8 : Result := 'DelphiXE8';
    D10  : Result := 'Delphi10Seattle';
    D101 : Result := 'Delphi10Berlin';
    D102 : Result := 'Delphi10Tokyo';
    D103 : Result := 'Delphi10Rio';
    D104 : Result := 'Delphi10Sydney';
    D11  : Result := 'Delphi11Alexandria';
  else
    Result := '';
  end;
end;

function GetDelphiStudioVersion(const ADelphiVersion: TDelphiVersion): string;
begin
  case ADelphiVersion of
    D7   : Result := '4.0';
    D2005: Result := '5.0';
    D2007: Result := '6.0';
    D2009: Result := '7.0';
    D2010: Result := '8.0';
    DXE  : Result := '9.0';
    DXE2 : Result := '10.0';
    DXE3 : Result := '11.0';
    DXE4 : Result := '12.0';
    DXE5 : Result := '13.0';
    DXE6 : Result := '14.0';
    DXE7 : Result := '15.0';
    DXE8 : Result := '16.0';
    D10  : Result := '17.0';
    D101 : Result := '18.0';
    D102 : Result := '19.0';
    D103 : Result := '20.0';
    D104 : Result := '21.0';
    D11  : Result := '22.0';
  else
    Result := '';
  end;
end;

function TryGetDelphiVersionFromRadStudioVersion(const ARadStudioVersion: string; var ADelphiVersion: TDelphiVersion): Boolean;
var
  LVersionNumber: Integer;
begin
  Result := True;
  if Pos('.', ARadStudioVersion) > 0 then
    LVersionNumber := StrToIntDef(Copy(ARadStudioVersion, 1, Pos('.', ARadStudioVersion) - 1), 0)
  else
    LVersionNumber := StrToIntDef(ARadStudioVersion, 0);
  case LVersionNumber of
    4:  ADelphiVersion := D7;
    5:  ADelphiVersion := D2005;
    6:  ADelphiVersion := D2007;
    7:  ADelphiVersion := D2009;
    8:  ADelphiVersion := D2010;
    9:  ADelphiVersion := DXE;
    10: ADelphiVersion := DXE2;
    11: ADelphiVersion := DXE3;
    12: ADelphiVersion := DXE4;
    13: ADelphiVersion := DXE5;
    14: ADelphiVersion := DXE6;
    15: ADelphiVersion := DXE7;
    16: ADelphiVersion := DXE8;
    17: ADelphiVersion := D10;
    18: ADelphiVersion := D101;
    19: ADelphiVersion := D102;
    20: ADelphiVersion := D103;
    21: ADelphiVersion := D104;
    22: ADelphiVersion := D11;
  else
    Result := False;
  end;
end;

// Get a platform name
function GetPlatformName(const APlatform: TDelphiPlatform): string;
begin
  case APlatform of
    pfWin32       : Result := 'Win32';
    pfWin64       : Result := 'Win64';
    pfAndroid     : Result := 'Android';
    pfAndroid64   : Result := 'Android64';
    pfiOSDevice32 : Result := 'iOSDevice32';
    pfiOSDevice64 : Result := 'iOSDevice64';
    pfiOSSimulator: Result := 'iOSSimulator';
    pfOSX32       : Result := 'OSX32';
    pfOSX64       : Result := 'OSX64';
    pfOSXARM64    : Result := 'OSXARM64';
    pfLinux64     : Result := 'Linux64';
  else
    Result := '';
  end;
end;

function GetPlatformLibraryName(const APlatform: TDelphiPlatform): string;
begin
  if APlatform = pfAndroid then
    Result := 'Android32'
  else if APlatform = pfiOSDevice32 then
    Result := 'iOSDevice'
  else
    Result := GetPlatformName(APlatform);
end;

// Get a platform friendly name
function GetPlatformFriendlyName(const APlatform: TDelphiPlatform): string;
begin
  case APlatform of
    pfWin32       : Result := 'Windows 32-bit';
    pfWin64       : Result := 'Windows 64-bit';
    pfAndroid     : Result := 'Android 32-bit';
    pfAndroid64   : Result := 'Android 64-bit';
    pfiOSDevice32 : Result := 'iOS Device 32-bit';
    pfiOSDevice64 : Result := 'iOS Device 64-bit';
    pfiOSSimulator: Result := 'iOS Simulator';
    pfOSX32       : Result := 'OSX 32-bit';
    pfOSX64       : Result := 'OSX 64-bit';
    pfOSXARM64    : Result := 'OSX ARM 64-bit';
    pfLinux64     : Result := 'Linux 64-bit';
  else
    Result := '';
  end;
end;

{************************************************************************}
{                                                                        }
{                             String Utils                               }
{                                                                        }
{************************************************************************}

// Parses a string into an array of substrings
function SplitString(AText: string; const ADelimiter: string): TArrayOfString;
var
  LTemp: string;
  I, P: Integer;
begin
  LTemp := AText;
  SetArrayLength(Result, StringChangeEx(LTemp, ADelimiter, '', True) + 1);
  for I := 0 to Length(Result) - 1 do
  begin
    P := Pos(ADelimiter, AText);
    if P > 0 then
    begin
      Result[I] := Copy(AText, 1, P - 1);
      Delete(AText, 1, P + Length(ADelimiter) - 1);
    end
    else
      Result[I] := AText;
  end;
end;

// Concatenates strings into one, separating each string by a delimiter
function JoinStrings(const AStrings: TArrayOfString; const ADelimiter: string; const AJoinEmptyStrings: Boolean): string;
var
  LString: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to GetArrayLength(AStrings) - 1 do
  begin
    LString := AStrings[I];
    if (Length(LString) > 0) or AJoinEmptyStrings then
    begin
      if Length(Result) = 0 then
        Result := LString
       else
        Result := Result + ADelimiter + LString;
    end;
  end;
end;

function AddString(const AStrings: TArrayOfString; const ANewString: string): TArrayOfString;
begin
  Result := AStrings;
  SetArrayLength(Result, GetArrayLength(Result) + 1);
  Result[GetArrayLength(Result) - 1] := ANewString;
end;

function InsertStringAtBeginning(const AStrings: TArrayOfString; const ANewString: string): TArrayOfString;
var
  I: Integer;
begin
  Result := AStrings;
  SetArrayLength(Result, GetArrayLength(Result) + 1);
  for I := GetArrayLength(Result) - 1 downto 1 do
    Result[I] := Result[I - 1];
  Result[0] := ANewString;
end;

// Remove all matches of a string in an array
function RemoveString(const AStrings: TArrayOfString; const AValue: string; const ACaseSensitive: Boolean): TArrayOfString;
var
  LString: string;
  I: Integer;
begin
  Result := [];
  if ACaseSensitive then
  begin
    for I := 0 to GetArrayLength(AStrings) - 1 do
    begin
      LString := AStrings[I];
      if LString <> AValue then
        Result := AddString(Result, LString);
    end;
  end
  else
    for I := 0 to GetArrayLength(AStrings) - 1 do
    begin
      LString := AStrings[I];
      if not SameText(LString, AValue) then
        Result := AddString(Result, LString);
    end;
end;

{************************************************************************}
{                                                                        }
{                              Type Utils                                }
{                                                                        }
{************************************************************************}

function IsDelphiVersionIn(const AVersions: TDelphiVersions; const AVersion: TDelphiVersion): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetArrayLength(AVersions) - 1 do
  begin
    if AVersions[I] = AVersion then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IncludeDelphiVersion(const AVersions: TDelphiVersions; const ANewVersion: TDelphiVersion): TDelphiVersions;
begin
  Result := AVersions;
  if not IsDelphiVersionIn(Result, ANewVersion) then
  begin
    SetArrayLength(Result, GetArrayLength(Result) + 1);
    Result[GetArrayLength(Result) - 1] := ANewVersion;
  end;
end;

function IncludeDelphiVersions(const AVersions1, AVersions2: TDelphiVersions): TDelphiVersions;
var
  I: Integer;
begin
  Result := AVersions1;
  for I := 0 to GetArrayLength(AVersions2) - 1 do
    Result := IncludeDelphiVersion(Result, AVersions2[I]);
end;

function IntersectionDelphiVersions(const AVersions1, AVersions2: TDelphiVersions): TDelphiVersions;
var
  I: Integer;
  J: Integer;
begin
  Result := [];
  for I := 0 to GetArrayLength(AVersions1) - 1 do
    for J := 0 to GetArrayLength(AVersions2) - 1 do
      if AVersions1[I] = AVersions2[J] then
        Result := IncludeDelphiVersion(Result, AVersions1[I]);
end;

function IsDelphiPlatformIn(const APlatforms: TDelphiPlatforms; const APlatform: TDelphiPlatform): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetArrayLength(APlatforms) - 1 do
  begin
    if APlatforms[I] = APlatform then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IncludeDelphiPlatform(const APlatforms: TDelphiPlatforms; const ANewPlatform: TDelphiPlatform): TDelphiPlatforms;
begin
  Result := APlatforms;
  if not IsDelphiPlatformIn(Result, ANewPlatform) then
  begin
    SetArrayLength(Result, GetArrayLength(Result) + 1);
    Result[GetArrayLength(Result) - 1] := ANewPlatform;
  end;
end;

function IncludeDelphiPlatforms(const APlatforms1, APlatforms2: TDelphiPlatforms): TDelphiPlatforms;
var
  I: Integer;
begin
  Result := APlatforms1;
  for I := 0 to GetArrayLength(APlatforms2) - 1 do
    Result := IncludeDelphiPlatform(Result, APlatforms2[I]);
end;

function ExcludeDelphiPlatform(const APlatforms: TDelphiPlatforms; const ARemovingPlatform: TDelphiPlatform): TDelphiPlatforms;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to GetArrayLength(APlatforms) - 1 do
  begin
    if APlatforms[I] <> ARemovingPlatform then
    begin
      SetArrayLength(Result, GetArrayLength(Result) + 1);
      Result[GetArrayLength(Result) - 1] := APlatforms[I];
    end;
  end;
end;

function IntersectionDelphiPlatforms(const APlatforms1, APlatforms2: TDelphiPlatforms): TDelphiPlatforms;
var
  I: Integer;
  J: Integer;
begin
  Result := [];
  for I := 0 to GetArrayLength(APlatforms1) - 1 do
    for J := 0 to GetArrayLength(APlatforms2) - 1 do
      if APlatforms1[I] = APlatforms2[J] then
        Result := IncludeDelphiPlatform(Result, APlatforms1[I]);
end;

{************************************************************************}
{                                                                        }
{                             Setup Utils                                }
{                                                                        }
{************************************************************************}

function GetUninstallString: string;
var
  LUninstallRegKey: string;
begin
  LUninstallRegKey := ExpandConstant('Software\Microsoft\Windows\CurrentVersion\Uninstall\{#emit SetupSetting("AppId")}_is1');
  Result := '';
  if not RegQueryStringValue(HKEY_LOCAL_MACHINE, LUninstallRegKey, 'UninstallString', Result) then
    RegQueryStringValue(HKEY_CURRENT_USER, LUninstallRegKey, 'UninstallString', Result);
end;

function IsUpgrade: Boolean;
begin
  Result := GetUninstallString <> '';
end;

function TryUninstallOtherVersions: Boolean;
var
  LResultCode: Integer;
begin
  Result := Exec(RemoveQuotes(GetUninstallString), '/SILENT /NORESTART /SUPPRESSMSGBOXES','', SW_HIDE, ewWaitUntilTerminated, LResultCode);
end;

function GetLatestAvailableVersion: TDelphiVersion;
var
  I: Integer;
  LVersion: TDelphiVersion;
begin
  Result := D7;
  for I := GetArrayLength(FSupportedDelphiVersions) - 1 downto 0 do
  begin
    LVersion := FSupportedDelphiVersions[I];
    if IsDelphiVersionIn(FAvailableDelphiVersions, LVersion) then
    begin
      Result := LVersion;
      Break;
    end;
  end;
end;

// Delphi root path like "C:\Program Files (x86)\Embarcadero\Studio\22.0\"
function GetDelphiPath(const ADelphiVersion: TDelphiVersion): string;
begin
  if not RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(ADelphiVersion), 'RootDir', Result) then
    Result := '';
end;

// Delphi root path like "c:\program files (x86)\embarcadero\studio\22.0\bin\bds.exe"
function GetDelphiApp(const ADelphiVersion: TDelphiVersion): string;
begin
  if not RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(ADelphiVersion), 'App', Result) then
    Result := '';
end;

// Get installed platforms in an delphi version
function GetInstalledPlatforms(const ADelphiVersion: TDelphiVersion): TDelphiPlatforms;
var
  LPlatform: TDelphiPlatform;
  LValue: string;
begin
  Result := [];
  Result := IncludeDelphiPlatform(Result, pfWin32);
  Result := IncludeDelphiPlatform(Result, pfWin64);
  for LPlatform := LowDelphiPlatform to HighDelphiPlatform do
    if RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(ADelphiVersion) + '\PlatformSDKs', 'Default_' + GetPlatformName(LPlatform), LValue) then
      Result := IncludeDelphiPlatform(Result, LPlatform);
end;

// Get installed Delphi versions in machine
function GetInstalledDelphiVersions: TDelphiVersions;
var
  LVersion: TDelphiVersion;
begin
  Result := [];
  for LVersion := LowDelphiVersion to HighDelphiVersion do
    if FileExists(GetDelphiApp(LVersion)) then
      Result := IncludeDelphiVersion(Result, LVersion);
end;

function IsDelphiInstalledWithoutInitialized(const AVersion: TDelphiVersion): Boolean;
var
  LPath: string;
  LPaths: TArrayOfString;
  I: Integer;
begin
  Result := not IsDelphiVersionIn(FAvailableDelphiVersions, AVersion);
  if Result then
  begin
    Result := False;
    if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'Path', LPath) and (LPath <> '') then
    begin
      LPaths := SplitString(LPath, ';');
      for I := 0 to GetArrayLength(LPaths) - 1 do
      begin
        if Pos('\Studio\' + GetDelphiStudioVersion(AVersion) + '\Bpl', LPaths[I]) > 0 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

// Get delphi versions that the installation but have not yet been used (Package installation cannot be done in these cases)
function GetDelphiInstalledWithoutInitialized: TDelphiVersions;
var
  LVersion: TDelphiVersion;
begin
  Result := [];
  for LVersion := LowDelphiVersion to HighDelphiVersion do
    if IsDelphiInstalledWithoutInitialized(LVersion) then
        Result := IncludeDelphiVersion(Result, LVersion);
end;

function HaveFmxLinux(const AVersion: TDelphiVersion): Boolean;
var
  LValue: string;
begin
  Result := RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Experts', 'FmxLinux', LValue) and (LValue <> '');
end;

// Get supported platforms by package
function GetSupportedPlatformsByPackage(const APackage: TDelphiPackage; const AVersion: TDelphiVersion): TDelphiPlatforms;
begin
  Result := IntersectionDelphiPlatforms(GetSupportedPlatformsByDelphiVersion(AVersion), APackage.Platforms);
  if IsDelphiPlatformIn(Result, pfLinux64) and APackage.UseFiremonkey and not HaveFmxLinux(AVersion) then
    Result := ExcludeDelphiPlatform(Result, pfLinux64);
end;

function IsVersionWithLimitedPlatforms(const AVersion: TDelphiVersion): Boolean;
var
  LVersion: TDelphiVersion;
  LFound: Boolean;
begin
  Result := False;
  LFound := False;
  for LVersion := LowDelphiVersion to HighDelphiVersion do
  begin
    if LVersion = AVersion then
      LFound := True;
    if LVersion = LatestVersionWithLimitedPlatforms then
    begin
      if LFound then
        Result := True;
      Exit;
    end;
  end;
end;

// Get all available platforms for a set o Delphi versions
function GetAvailableDelphiPlatforms(const AVersions: TDelphiVersions): TDelphiPlatforms;
var
  I: Integer;
  J: Integer;
  LVersion: TDelphiVersion;
  LPackage: TDelphiPackage;
  LIncludePlatforms: TDelphiPlatforms;
  LInstalledPlatforms: TDelphiPlatforms;
begin
  Result := [];
  for I := 0 to GetArrayLength(AVersions) - 1 do
  begin
    LVersion := AVersions[I];
    LInstalledPlatforms := GetInstalledPlatforms(LVersion);
    for J := 0 to GetArrayLength(FPackages) - 1 do
    begin
      LPackage := FPackages[J];
      LIncludePlatforms := GetSupportedPlatformsByPackage(LPackage, LVersion);
      if (FSetupKind = skInstalling) and IsVersionWithLimitedPlatforms(LVersion) then
        LIncludePlatforms := IntersectionDelphiPlatforms(LIncludePlatforms, FPlatformsInVersionWithLimitedPlatforms);
      Result := IncludeDelphiPlatforms(Result, IntersectionDelphiPlatforms(LIncludePlatforms, LInstalledPlatforms));
    end;
  end;
end;

// Get Delphi versions supported by packages
function GetSupportedDelphiVersions: TDelphiVersions;
var
  I: Integer;
  LVersion: TDelphiVersion;
begin
  Result := [];
  for I := 0 to Length(FPackages) - 1 do
    for LVersion := FPackages[I].MinDelphiVersion to FPackages[I].MaxDelphiVersion do
      Result := IncludeDelphiVersion(Result, LVersion);
end;

// Get Delphi platforms supported by packages
function GetSupportedDelphiPlatforms: TDelphiPlatforms;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to Length(FPackages) - 1 do
    Result := IncludeDelphiPlatforms(Result, FPackages[I].Platforms);
end;

// Check if there is any instance of delphi running
function IsThereAnyDelphiInstanceRunning: Boolean;
begin
  Result := (FindWindowByClassName('TAppBuilder') <> 0) and
    (FindWindowByClassName('TPropertyInspector') <> 0) and
    (FindWindowByWindowName('Object Inspector') <> 0);
end;

// Add an environment variable exclusive of Delphi application
procedure AddDelphiEnvVariable(const AVersion: TDelphiVersion; const AName, AValue: string);
begin
  RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', AName, AValue);
end;

// Remove an environment variable exclusive of Delphi application
procedure RemoveDelphiEnvVariable(const AVersion: TDelphiVersion; const AName: string);
begin
  RegDeleteValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', AName);
end;

procedure AddToDelphiPathEnvVariable(const AVersion: TDelphiVersion; const APath: string);
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  if RegKeyExists(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion)) then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LPaths := InsertStringAtBeginning(LPaths, APath);
    LValue := JoinStrings(LPaths, ';', False);
    RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', 'PATH', LValue);
  end;
end;

procedure RemoveFromDelphiPathEnvVariable(const AVersion: TDelphiVersion; const APath: string);
var
  LValue: string;
  LPaths: TArrayOfString;
begin
  if RegValueExists(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', 'PATH') then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', 'PATH', LValue)) or (LValue = '') then
      LValue := '$(PATH)';
    LPaths := RemoveString(SplitString(LValue, ';'), APath, False);
    LPaths := RemoveString(LPaths, APath + '\', False);
    LValue := JoinStrings(LPaths, ';', False);
    RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Environment Variables', 'PATH', LValue);
  end;
end;

// Add a library path in a platform of Delphi
procedure AddDelphiLibraryPath(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const ALibraryPathName, APath: string);
var
  LNewValue: string;
  LPathsToInsert: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I: Integer;
begin
  if RegKeyExists(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Library\' + GetPlatformLibraryName(APlatform)) then
  begin
    if (not RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Library\' + GetPlatformLibraryName(APlatform), ALibraryPathName, LNewValue)) or (Length(LNewValue) = 0) then
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
        LCurrentPaths := AddString(LCurrentPaths, LNewValue);
      end;
      LNewValue := JoinStrings(LCurrentPaths, ';', False);
    end;
    RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Library\' + GetPlatformLibraryName(APlatform), ALibraryPathName, LNewValue);
  end;
end;

// Remove a library path in a platform of Delphi
procedure RemoveDelphiLibraryPath(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const ALibraryPathName, APath: string);
var
  LNewValue: string;
  LPathsToRemove: TArrayOfString;
  LCurrentPaths: TArrayOfString;
  I: Integer;
begin
  if RegQueryStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Library\' + GetPlatformLibraryName(APlatform), ALibraryPathName, LNewValue) and (Length(LNewValue) <> 0) then
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
    RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Library\' + GetPlatformLibraryName(APlatform), ALibraryPathName, LNewValue);
  end;
end;

// Register the Bpl in Delphi registry
procedure RegisterDelphiBpl(const AVersion: TDelphiVersion; const ABplFileName, ADescription: string);
begin
  RegWriteStringValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Known Packages', ABplFileName, ADescription);
end;

// Unregister the Bpl in Delphi registry
procedure UnregisterDelphiBpl(const AVersion: TDelphiVersion; const ABplFileName: string);
begin
  RegDeleteValue(HKEY_CURRENT_USER, GetDelphiRegKey(AVersion) + '\Known Packages', ABplFileName);
end;

function ExpandPath(const AString: string): string;
begin
  Result := AString;
  StringChangeEx(Result, '$(' + LibraryDirVariable + ')', '{app}', True);
  if (Pos('{app}', Result) <> 1) and (Pos(':', Result) = 0) then
    Result := '{app}\' + Result;
  Result := ExpandConstant(Result);
end;

function AddLibraryDirInPaths(const APaths: string): string;
var
  I: Integer;
  LPaths: TArrayOfString;
begin
  LPaths := SplitString(APaths, ';');
  for I := 0 to GetArrayLength(LPaths) - 1 do
    if LPaths[I] <> '' then
      LPaths[I] := '$(' + LibraryDirVariable + ')\' + LPaths[I];
  Result := JoinStrings(LPaths, ';', False);
end;

{************************************************************************}
{                                                                        }
{                            Installation                                }
{                                                                        }
{************************************************************************}

function GetDcuPath(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const AConfig: TDelphiConfig; const APackage: TDelphiPackage): string;
begin
  Result := Format('$(%s)\%s\%s\%s\%s', [LibraryDirVariable, APackage.DCUOutputPath, GetSourceFolderName(AVersion), GetPlatformName(APlatform), GetDelphiConfigName(AConfig)]);
end;

function GetDcpPath(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const AConfig: TDelphiConfig; const APackage: TDelphiPackage): string;
begin
  Result := GetDcuPath(AVersion, APlatform, AConfig, APackage);
end;

function GetBplPath(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const AConfig: TDelphiConfig; const APackage: TDelphiPackage): string;
begin
  Result := GetDcuPath(AVersion, APlatform, AConfig, APackage) + '\Bpl';
end;

function GetRsvarsBatchFileName(const AVersion: TDelphiVersion): string;
begin
  Result := GetDelphiPath(AVersion) + 'bin\rsvars.bat';
end;

function GetBuildParams(const ADcuOutput, ADcpOutput, ABplOutput, ADefines: string; const AConfig: TDelphiConfig; const APlatform: TDelphiPlatform): string;
begin
  Result := Format('/target:Build /p:config=%s /p:platform=%s /p:DCC_BuildAllUnits=true /p:DCC_DcuOutput="%s" /p:DCC_Define="%s" /p:DCC_BplOutput="%s" /p:DCC_DcpOutput="%s"', [GetDelphiConfigName(AConfig), GetPlatformName(APlatform), ADcuOutput, ADefines, ABplOutput, ADcpOutput]);
end;

function GetBuildCommand(const AConfig: TDelphiConfig; const APlatform: TDelphiPlatform; const ARsVarsBatchFileName, ADprojFileName, ADcuOutput, ADcpOutput, ABplOutput, ADefines, ALogFileName: string): string;
begin
  Result := AddQuotes(ARsVarsBatchFileName) + '& msbuild ' + AddQuotes(ADprojFileName) + ' ' +
    GetBuildParams(ADcuOutput, ADcpOutput, ABplOutput, ADefines, AConfig, APlatform) + '  >' + AddQuotes(ALogFileName);
end;

procedure InstallDelphiPackage(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const APackage: TDelphiPackage);
begin
  AddDelphiLibraryPath(AVersion, APlatform, 'Search Path', GetDcuPath(AVersion, APlatform, cfRelease, APackage));
  AddDelphiLibraryPath(AVersion, APlatform, 'Browsing Path', AddLibraryDirInPaths(APackage.SourcePaths));
  AddDelphiLibraryPath(AVersion, APlatform, 'Debug DCU Path', GetDcuPath(AVersion, APlatform, cfDebug, APackage));
end;

procedure ShowCompileError(const ACode: Integer; const ADescription, ALogFileName: string; const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const AConfig: TDelphiConfig; const APackage: TDelphiPackage);
var
  LResultCode: Integer;
begin
  MsgBox(FmtMessage(CustomMessage('ErrorCompilingFor'), [GetDelphiVersionFriendlyName(AVersion) + ' - ' + APackage.Name + '.dproj - ' + GetPlatformName(APlatform), InttoStr(ACode), ADescription]), mbError, mb_OK);
  ShellExec('open', ExpandConstant('{#LibrarySupportURL}'), '', '', SW_SHOW, ewNoWait, LResultCode);
  if ALogFileName <> '' then
    ShellExec('open', ALogFileName, '', '', SW_SHOW, ewNoWait, LResultCode);
end;

// Register packages in Delphi
function TryCompileDelphiPackage(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const AConfig: TDelphiConfig; const APackage: TDelphiPackage): Boolean;
var
  LDprojFileName: string;
  LDcuOutput: string;
  LDcpOutput: string;
  LBplOutput: string;
  LResultCode: Integer;
  LLogFileName: string;
  LRsVarsBatchFileName: string;
  LCommand: string;
begin
  WizardForm.StatusLabel.Caption:= FmtMessage(CustomMessage('CompilingFor'), [GetDelphiVersionFriendlyName(AVersion)]);
  WizardForm.FilenameLabel.Caption := APackage.Name + '.dproj - ' + GetPlatformName(APlatform);

  LDprojFileName := ExpandPath(PackagesFolder + '\' + GetSourceFolderName(AVersion) + '\' + APackage.Name + '.dproj');
  LDcuOutput := ExpandPath(GetDcuPath(AVersion, APlatform, AConfig, APackage));
  LDcpOutput := ExpandPath(GetDcpPath(AVersion, APlatform, AConfig, APackage));
  LBplOutput := ExpandPath(GetBplPath(AVersion, APlatform, AConfig, APackage));
  LLogFileName := ExpandConstant('{app}\Build.Logs.txt');
  LRsVarsBatchFileName := GetRsVarsBatchFileName(AVersion);


  if FileExists(LRsVarsBatchFileName) then
  begin
    // Build package
    LResultCode := 0;
    LCommand := GetBuildCommand(AConfig, APlatform, LRsVarsBatchFileName, LDprojFileName, LDcuOutput, LDcpOutput, LBplOutput, '', LLogFileName);
    Result := Exec(ExpandConstant('{cmd}'), '/C call ' + LCommand, '', SW_HIDE, ewWaitUntilTerminated, LResultCode) and (LResultCode = 0);

    if Result then
      DeleteFile(LLogFileName)
    else
      ShowCompileError(LResultCode, SysErrorMessage(LResultCode), LLogFileName, AVersion, APlatform, AConfig, APackage);
  end
  else
    ShowCompileError(0, FmtMessage(CustomMessage('ErrorCantFoundRsVars'), [LRsVarsBatchFileName]), '', AVersion, APlatform, AConfig, APackage);
  WizardForm.FilenameLabel.Caption := '';
end;

function GetCompilationsCount: Integer;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LPackage: TDelphiPackage;
  LTargetPlatforms: TDelphiPlatforms;
  LConfig: TDelphiConfig;
begin
  Result := 0;
  for I := 0 to GetArrayLength(FSelectedDelphiVersions) - 1 do
    for J := 0 to GetArrayLength(FPackages) - 1 do
    begin
      LPackage := FPackages[J];
      LTargetPlatforms := IntersectionDelphiPlatforms(FSelectedDelphiPlatforms, GetSupportedPlatformsByPackage(LPackage, FSelectedDelphiVersions[I]));
      if IsVersionWithLimitedPlatforms(FSelectedDelphiVersions[I]) then
        LTargetPlatforms := IntersectionDelphiPlatforms(LTargetPlatforms, FPlatformsInVersionWithLimitedPlatforms);
      for K := 0 to GetArrayLength(LTargetPlatforms) - 1 do
        for LConfig := LowDelphiConfig to HighDelphiConfig do
          Result := Result + 1;
    end;
end;

function TryCompileDelphiPackages: Boolean;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LVersion: TDelphiVersion;
  LPackage: TDelphiPackage;
  LPlatform: TDelphiPlatform;
  LTargetPlatforms: TDelphiPlatforms;
  LConfig: TDelphiConfig;
begin
  Result := True;
  for I := 0 to GetArrayLength(FSelectedDelphiVersions) - 1 do
  begin
    LVersion := FSelectedDelphiVersions[I];
    for J := 0 to GetArrayLength(FPackages) - 1 do
    begin
      LPackage := FPackages[J];
      LTargetPlatforms := IntersectionDelphiPlatforms(FSelectedDelphiPlatforms, GetSupportedPlatformsByPackage(LPackage, LVersion));
      if IsVersionWithLimitedPlatforms(LVersion) then
        LTargetPlatforms := IntersectionDelphiPlatforms(LTargetPlatforms, FPlatformsInVersionWithLimitedPlatforms);
      for K := 0 to GetArrayLength(LTargetPlatforms) - 1 do
      begin
        LPlatform := LTargetPlatforms[K];
        for LConfig := LowDelphiConfig to HighDelphiConfig do
        begin
          if not TryCompileDelphiPackage(LVersion, LPlatform, LConfig, LPackage) then
          begin
            Result := False;
            Exit;
          end;
          WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Position + 1;
        end;
      end;
    end;
  end;
end;

function TryRemoveOldFiles: Boolean;
var
  I: Integer;
  LVersion: TDelphiVersion;
  LPackage: TDelphiPackage;
  LPlatform: TDelphiPlatform;
begin
  if FSetupKind = skInstalling then
    WizardForm.StatusLabel.Caption := CustomMessage('RemovingOldFiles');
  Result := True;
  try
    for I := 0 to GetArrayLength(FPackages) - 1 do
    begin
      LPackage := FPackages[I];
      if DirExists(LPackage.DCUOutputPath) and not RemoveDir(ExpandPath(LPackage.DCUOutputPath)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    if not Result then
      MsgBox(CustomMessage('CannotPossibleToRemoveOldFiles'), mbError, MB_OK);
  end;
  if FSetupKind = skInstalling then
    WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Position + 1;
end;

function InstallPackages: Boolean;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LVersion: TDelphiVersion;
  LPackage: TDelphiPackage;
  LTargetPlatforms: TDelphiPlatforms;
  LInstalledPlatforms: TDelphiPlatforms;
begin
  WizardForm.ProgressGauge.Min := 0;
  WizardForm.ProgressGauge.Max := GetCompilationsCount + 2;
  WizardForm.ProgressGauge.Position := 0;
  Result := TryRemoveOldFiles and TryCompileDelphiPackages;
  if Result then
  begin
    WizardForm.StatusLabel.Caption := CustomMessage('InstallingPackages');
    for I := 0 to GetArrayLength(FSelectedDelphiVersions) - 1 do
    begin
      LVersion := FSelectedDelphiVersions[I];
      LInstalledPlatforms := GetInstalledPlatforms(LVersion);
      AddDelphiEnvVariable(LVersion, LibraryDirVariable, ExpandConstant('{app}'));
      AddToDelphiPathEnvVariable(LVersion, ExpandPath('Binary\Shared\Win32'));
      if iOSDevice64ExtraLibraryPath <> '' then
        AddDelphiLibraryPath(LVersion, pfiOSDevice64, 'Search Path', AddLibraryDirInPaths(iOSDevice64ExtraLibraryPath));
      for J := 0 to GetArrayLength(FPackages) - 1 do
      begin
        LPackage := FPackages[J];
        LTargetPlatforms := IntersectionDelphiPlatforms(FSelectedDelphiPlatforms, GetSupportedPlatformsByPackage(LPackage, LVersion));
        LTargetPlatforms := IntersectionDelphiPlatforms(LTargetPlatforms, LInstalledPlatforms);
        if IsVersionWithLimitedPlatforms(LVersion) then
          LTargetPlatforms := IntersectionDelphiPlatforms(LTargetPlatforms, FPlatformsInVersionWithLimitedPlatforms);
        for K := 0 to GetArrayLength(LTargetPlatforms) - 1 do
          InstallDelphiPackage(LVersion, LTargetPlatforms[K], LPackage);
        if IsDelphiPlatformIn(LTargetPlatforms, pfWin32) and LPackage.Installable then
          RegisterDelphiBpl(LVersion, ExpandPath(GetBplPath(LVersion, pfWin32, cfRelease, LPackage) + '\' + LPackage.Name + '.' + GetSourceFolderName(LVersion) + '.bpl'), LPackage.Description);
        if IsDelphiPlatformIn(LTargetPlatforms, pfWin32) then
          AddToDelphiPathEnvVariable(LVersion, ExpandPath(GetBplPath(LVersion, pfWin32, cfRelease, LPackage)));
        if IsDelphiPlatformIn(LTargetPlatforms, pfWin64) then
          AddToDelphiPathEnvVariable(LVersion, ExpandPath(GetBplPath(LVersion, pfWin64, cfRelease, LPackage)));
      end;
    end;
  end;
  WizardForm.ProgressGauge.Position := WizardForm.ProgressGauge.Max;
end;

procedure UninstallDelphiPackage(const AVersion: TDelphiVersion; const APlatform: TDelphiPlatform; const APackage: TDelphiPackage);
begin
  RemoveDelphiLibraryPath(AVersion, APlatform, 'Search Path', GetDcuPath(AVersion, APlatform, cfRelease, APackage));
  RemoveDelphiLibraryPath(AVersion, APlatform, 'Browsing Path', AddLibraryDirInPaths(APackage.SourcePaths));
  RemoveDelphiLibraryPath(AVersion, APlatform, 'Debug DCU Path', GetDcuPath(AVersion, APlatform, cfDebug, APackage));
end;

// Uninstall a Delphi package
function UninstallDelphiPackages: Boolean;
var
  I: Integer;
  J: Integer;
  K: Integer;
  LVersion: TDelphiVersion;
  LPackage: TDelphiPackage;
  LTargetPlatforms: TDelphiPlatforms;
begin
  for I := 0 to GetArrayLength(FSelectedDelphiVersions) - 1 do
  begin
    LVersion := FSelectedDelphiVersions[I];
    for J := 0 to GetArrayLength(FPackages) - 1 do
    begin
      LPackage := FPackages[J];
      LTargetPlatforms := IntersectionDelphiPlatforms(FSelectedDelphiPlatforms, LPackage.Platforms);
      UnregisterDelphiBpl(LVersion, ExpandPath(GetBplPath(LVersion, pfWin32, cfRelease, LPackage) + '\' + LPackage.Name + '.' + GetSourceFolderName(LVersion) + '.bpl'));
      RemoveFromDelphiPathEnvVariable(LVersion, ExpandPath(GetBplPath(LVersion, pfWin32, cfRelease, LPackage)));
      RemoveFromDelphiPathEnvVariable(LVersion, ExpandPath(GetBplPath(LVersion, pfWin64, cfRelease, LPackage)));
      for K := 0 to GetArrayLength(LTargetPlatforms) - 1 do
        UninstallDelphiPackage(LVersion, LTargetPlatforms[K], LPackage);
    end;
    RemoveFromDelphiPathEnvVariable(LVersion, ExpandPath('Binary\Shared\Win32'));
    if iOSDevice64ExtraLibraryPath <> '' then
      RemoveDelphiLibraryPath(LVersion, pfiOSDevice64, 'Search Path', AddLibraryDirInPaths(iOSDevice64ExtraLibraryPath));
    RemoveDelphiEnvVariable(LVersion, LibraryDirVariable);
  end;
  Result := TryRemoveOldFiles;
end;

{************************************************************************}
{                                                                        }
{                                Pages                                   }
{                                                                        }
{************************************************************************}

// Get all selected platforms in Delphi platforms page
function GetSelectedPlatforms: TDelphiPlatforms;
var
  I: Integer;
begin
  Result := [];
  if FChooseDelphiPlatformsPage <> nil then
    for I := 0 to GetArrayLength(FSupportedDelphiPlatforms) - 1 do
      if FChooseDelphiPlatformsPage.CheckListBox.ItemEnabled[I] and (FChooseDelphiPlatformsPage.CheckListBox.State[I] = cbChecked) then
        Result := IncludeDelphiPlatform(Result, FSupportedDelphiPlatforms[I]);
end;

// Update all platforms enabled in platforms page, and check by default all availables
procedure UpdatePlatformsAvailable;
var
  I: Integer;
begin
  FAvailableDelphiPlatforms := GetAvailableDelphiPlatforms(FSelectedDelphiVersions);
  for I := 0 to GetArrayLength(FSupportedDelphiPlatforms) - 1 do
  begin
    FChooseDelphiPlatformsPage.CheckListBox.ItemEnabled[I] := IsDelphiPlatformIn(FAvailableDelphiPlatforms, FSupportedDelphiPlatforms[I]);
    // The default value of non windows platforms are unchecked
    if IsDelphiPlatformIn(FDefaultPlatforms, FSupportedDelphiPlatforms[I]) then
      FChooseDelphiPlatformsPage.CheckListBox.Checked[I] := FChooseDelphiPlatformsPage.CheckListBox.ItemEnabled[I]
    else
      FChooseDelphiPlatformsPage.CheckListBox.Checked[I] := False;
  end;
end;

// Event OnNextButtonClick of Delphi platforms page
function OnDelphiPlatformsPageNextButtonClick(ASender: TWizardPage): Boolean;
begin
  FSelectedDelphiPlatforms := GetSelectedPlatforms;
  Result := GetArrayLength(FSelectedDelphiPlatforms) <> 0;
end;

// Create the Delphi platforms page
function CreateDelphiPlatformsPage(const AAfterID: Integer): TInputOptionWizardPage;
var
  I: Integer;
  LPlatform: TDelphiPlatform;
begin
  Result := CreateInputOptionPage(AAfterID, CustomMessage('ChooseDelphiPlatformsTitle'), '',
    ExpandConstant(CustomMessage('ChooseDelphiPlatformsMessage')), False, False);
  for I := 0 to GetArrayLength(FSupportedDelphiPlatforms) - 1 do
  begin
    LPlatform := FSupportedDelphiPlatforms[I];
    Result.CheckListBox.AddCheckBox(GetPlatformFriendlyName(LPlatform), '', 0, IsDelphiPlatformIn(FAvailableDelphiPlatforms, LPlatform), IsDelphiPlatformIn(FAvailableDelphiPlatforms, LPlatform), False, False, nil);
  end;
  Result.OnNextButtonClick := @OnDelphiPlatformsPageNextButtonClick;
end;

// Get all selected versions in Delphi versions page
function GetSelectedVersions: TDelphiVersions;
var
  I: Integer;
begin
  Result := [];
  if FChooseDelphiVersionsPage <> nil then
    for I := 0 to GetArrayLength(FSupportedDelphiVersions) - 1 do
      if FChooseDelphiVersionsPage.CheckListBox.ItemEnabled[I] and (FChooseDelphiVersionsPage.CheckListBox.State[I] = cbChecked) then
        Result := IncludeDelphiVersion(Result, FSupportedDelphiVersions[I]);
end;

// Event OnNextButtonClick of Delphi versions page
function OnDelphiVersionsPageNextButtonClick(ASender: TWizardPage): Boolean;
begin
  FSelectedDelphiVersions := GetSelectedVersions;
  UpdatePlatformsAvailable;
  Result := GetArrayLength(FSelectedDelphiVersions) <> 0;
end;

// Create the Delphi versions page
function CreateDelphiVersionsPage(const AAfterID: Integer): TInputOptionWizardPage;
var
  I: Integer;
  LVersion: TDelphiVersion;
  LWarnings: string;
begin
  if GetArrayLength(GetDelphiInstalledWithoutInitialized) = 0 then
    LWarnings := ''
  else
    LWarnings := CustomMessage('ChooseDelphVersionsWarningMissingDelphiVersion');
  Result := CreateInputOptionPage(AAfterID, CustomMessage('ChooseDelphVersionsTitle'), '',
    ExpandConstant(CustomMessage('ChooseDelphVersionsMessage') + LWarnings), False, False);

  for I := 0 to GetArrayLength(FSupportedDelphiVersions) - 1 do
  begin
    LVersion := FSupportedDelphiVersions[I];
    Result.CheckListBox.AddCheckBox(GetDelphiVersionFriendlyName(LVersion), '', 0, IsDelphiVersionIn(FAvailableDelphiVersions, LVersion) and IsDelphiVersionIn(FSelectedDelphiVersions,  LVersion), IsDelphiVersionIn(FAvailableDelphiVersions, LVersion), False, False, nil);
  end;
  Result.OnNextButtonClick := @OnDelphiVersionsPageNextButtonClick;
end;

{************************************************************************}
{                                                                        }
{                       Inno Setup event functions                       }
{                                                                        }
{************************************************************************}

function NeedsUninstallRegKey: Boolean;
begin
  Result := SameText(ExpandConstant('{param:CreateUninstallRegKey|yes}'), 'yes') or SameText(ExpandConstant('{param:CreateUninstallRegKey|true}'), 'true');
end;

function NeedsDirPage: Boolean;
begin
  #ifdef FilesEmbedded
  Result := True
  #else
  Result := False;
  #endif
end;

function GetDefaultDirName(AParam: string): string;
begin
  if NeedsDirPage then
    Result := ExpandConstant('{userdocs}\{#LibraryName}')
  else
    Result := GetCurrentDir;
end;

// Set global variables (called at start)
procedure SetGlobals;
var
  LDefaultRADStudio: TDelphiVersion;
begin
  SetConfigs;
  FSupportedDelphiVersions := GetSupportedDelphiVersions;
  FAvailableDelphiVersions := IntersectionDelphiVersions(GetInstalledDelphiVersions, FSupportedDelphiVersions);
  if (not TryGetDelphiVersionFromRadStudioVersion(ExpandConstant('{param:DefaultRADStudio|}'), LDefaultRADStudio)) or (not IsDelphiVersionIn(FAvailableDelphiVersions, LDefaultRADStudio)) then
  begin
    LDefaultRADStudio := GetLatestAvailableVersion;
    FSelectedDelphiVersions := [LDefaultRADStudio];
  end
  else if FSetupKind = skUninstalling then
    FSelectedDelphiVersions := FAvailableDelphiVersions
  else
    FSelectedDelphiVersions := [LDefaultRADStudio];

  FSupportedDelphiPlatforms := GetSupportedDelphiPlatforms;
  FAvailableDelphiPlatforms := GetAvailableDelphiPlatforms(FSelectedDelphiVersions);
  if SameText(ExpandConstant('{param:DefaultPlatforms|windows}'), 'all') or (FSetupKind = skUninstalling) then
    FDefaultPlatforms := FSupportedDelphiPlatforms
  else
    FDefaultPlatforms := [pfWin32, pfWin64];
  FSelectedDelphiPlatforms := IntersectionDelphiPlatforms(FDefaultPlatforms, FSupportedDelphiPlatforms);
end;

// Install initialization
function InitializeSetup: Boolean;
begin
  FSetupKind := skInstalling;
  SetGlobals;
  Result := True;
end;

// Create wizard pages
procedure InitializeWizard;
begin
  FChooseDelphiVersionsPage := CreateDelphiVersionsPage(wpWelcome);
  FChooseDelphiPlatformsPage := CreateDelphiPlatformsPage(FChooseDelphiVersionsPage.ID);
end;

function CanShowCloseRadStudioMessage: Boolean;
begin
  Result := SameText(ExpandConstant('{param:CloseRadStudioMessage|yes}'), 'yes') or SameText(ExpandConstant('{param:CloseRadStudioMessage|true}'), 'true');;
end;

function ShouldSkipPage(APageID: Integer): Boolean;
begin
  Result := (APageID = wpSelectDir) and not NeedsDirPage;
end;

// Install process
procedure CurStepChanged(ACurStep: TSetupStep);
begin
  case ACurStep of
    ssInstall:
      if IsUpgrade then
      begin
        WizardForm.StatusLabel.Caption := CustomMessage('UninstallingDetectedVersion');
        if NeedsUninstallRegKey and not TryUninstallOtherVersions then
        begin
          MsgBox(ExpandConstant(CustomMessage('CannotPossibleToUninstallDetectedVersion')), mbError, MB_OK);
          Abort;
        end;
      end;
    ssPostInstall:
      begin
        if InstallPackages then
        begin
          if CanShowCloseRadStudioMessage and IsThereAnyDelphiInstanceRunning then
            MsgBox(CustomMessage('InstallationSuccesfullyRestartDelphi'), mbInformation, MB_OK);
        end
        else
          Abort;
      end;
  else
  end;
end;

// Uninstall initialization
function InitializeUninstall: Boolean;
begin
  FSetupKind := skUninstalling;
  SetGlobals;
  if CanShowCloseRadStudioMessage then
  begin
    Result := not IsThereAnyDelphiInstanceRunning;
    if not Result then
      MsgBox(CustomMessage('UninstallAbortedToCloseDelphiInstance'), mbError, MB_OK);
  end
  else
    Result := True;
end;

// Uninstall process
procedure CurUninstallStepChanged(ACurUninstallStep: TUninstallStep);
begin
  if ACurUninstallStep = usPostUninstall then
    if not UninstallDelphiPackages then
      Abort;
end;
