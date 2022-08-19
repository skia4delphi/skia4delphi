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
{                                                                        }
{                          Custom Parameters                             }
{                                                                        }
{ /RADStudioVersions=                                                    }
{   Values allowed: 9.0 to 22.0 separed by comma or all keyword          }
{   Default: (latest version found in computer)                          }
{   Description: The version used is the product version in resgistry,   }
{     i.e, the RAD Studio 10.3 Rio is "20.0", the RAD Studio 10.4 Sydney }
{     is "21.0", etc. This is used to set the RAD Studio versions, in    }
{     silent mode, that will installed the library.                      }
{     Ex: /RADStudioVersions=21.0,22.0 will install only in              }
{     RAD Studio 11 Alexandria and 10.4 Sydney. But if the parameter is  }
{     /RADStudioVersions=all will install in all RAD Studio installed in }
{     the machine. Without set this parameter, the value will be only    }
{     the newest RAD Studio version found on the machine. A cool tip of  }
{     this param is that if the setup is being executed by the GetIt,    }
{     you can replace the version number to the environment variable     }
{     ProductVersion like this: /RADStudioVersions=$(ProductVersion)     }
{                                                                        }
{ /CreateUninstallRegKey=                                                }
{   Values allowed: no|yes or false|true or 0|1                          }
{   Default: yes                                                         }
{   Description: When true the uninstall shortcut in applications panel  }
{     will be created and before the setup starts will call the          }
{     uninstall of others versions                                       }
{                                                                        }
{************************************************************************}
{                                                                        }
{ Example of command line to install in silent mode:                     }
{   cmd /C ""Skia4Delphi_3.4.1_Setup.exe" /DIR="C:\Skia4Delphi" /SILENT  }
{     /RADStudioVersions=all"                                            }
{                                                                        }
{ In GetIt implementation, the installation command could be:            }
{   cmd /C ""$(BDSCatalogRepository)\Skia4Delphi-3.4.1\                  }
{     Skia4Delphi_3.4.1_Setup.exe"                                       }
{     /DIR="$(BDSCatalogRepository)\Skia4Delphi-3.4.1" /VERYSILENT       }
{     /RADStudioVersions=$(ProductVersion) /CreateUninstallRegKey=no"    }
{                                                                        }
{ Example of command line to uninstall in silent mode:                   }
{   cmd /C ""C:\Skia4Delphi\unins000.exe" /VERYSILENT                    }
{     /RADStudioVersions=all"                                            }
{                                                                        }
{ In GetIt implementation, the uninstall command could be:               }
{   cmd /C ""$(BDSCatalogRepository)\Skia4Delphi-3.4.1\unins000.exe"     }
{     /VERYSILENT /RADStudioVersions=$(ProductVersion)"                  }
{                                                                        }
{************************************************************************}

#define LibraryName "Skia4Delphi"
#define LibraryVersion "3.4.1"
#define LibraryPublisher "Skia4Delphi Team"
#define LibraryCopyright "Copyright (c) 2021-2022 Skia4Delphi Project"
#define LibraryURL "https://skia4delphi.org"
#define LibrarySamplesFolder "Samples"
#define LibraryPackagesFolder "Packages"
#define LibraryDCUFolder "Library"
#define LibraryDocumentationURL "https://github.com/skia4delphi/skia4delphi"
#define LibrarySupportURL "https://github.com/skia4delphi/skia4delphi/issues/"
#define LibraryUpdatesURL "https://github.com/skia4delphi/skia4delphi/releases/"
#define LibraryLicenseFileName "Languages\Default.LICENSE"
#define BannerImagesFileName "..\..\Assets\Setup\image.bmp,..\..\Assets\Setup\image2.bmp,..\..\Assets\Setup\image3.bmp,..\..\Assets\Setup\image4.bmp,..\..\Assets\Setup\image5.bmp,..\..\Assets\Setup\image6.bmp,..\..\Assets\Setup\image7.bmp"
#define SmallImagesFileName "..\..\Assets\Setup\image-small.bmp,..\..\Assets\Setup\image-small2.bmp,..\..\Assets\Setup\image-small3.bmp,..\..\Assets\Setup\image-small4.bmp,..\..\Assets\Setup\image-small5.bmp"
#define SetupFolder "Tools\Setup"
#define VclStyle "Windows11.Dark.vsf"
//#define SignSetup
#define FilesEmbedded
#if DirExists("..\..\" + LibraryDCUFolder + "\")
  #define UseLibraryDCUFolder
#endif

[Setup]
AllowCancelDuringInstall=yes
AppCopyright={#LibraryCopyright}
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{A8524AB1-6BAE-4AEF-9E67-8F1D086BE3C7}
AppName={#LibraryName}
AppPublisher={#LibraryPublisher}
AppPublisherURL={#LibraryURL}
AppSupportURL={#LibrarySupportURL}
AppUpdatesURL={#LibraryUpdatesURL}
AppVersion={#LibraryVersion}
CloseApplications=no
Compression=lzma2/ultra64
CreateUninstallRegKey=NeedsUninstallRegKey
DefaultDirName={code:GetDefaultDirName}
DefaultGroupName={#LibraryName}
DirExistsWarning=no
DisableDirPage=no
DisableProgramGroupPage=yes
DisableReadyPage=yes
DisableStartupPrompt=yes
DisableWelcomePage=no
InternalCompressLevel=ultra64
LicenseFile={#LibraryLicenseFileName}
LZMANumBlockThreads=6
LZMAUseSeparateProcess=yes
MissingMessagesWarning=yes
NotRecognizedMessagesWarning=yes
PrivilegesRequired=lowest
SetupLogging=yes
ShowLanguageDialog=no
SolidCompression=yes
UsePreviousAppDir=no
WizardImageFile={#BannerImagesFileName}
WizardSmallImageFile={#SmallImagesFileName}
#ifdef FilesEmbedded
  #ifdef SignSetup
    OutputBaseFilename={#LibraryName}_{#LibraryVersion}_Setup
  #else
    OutputBaseFilename={#LibraryName}_{#LibraryVersion}_Setup_Unsigned
  #endif
  OutputDir=Output\
  Uninstallable=yes
#else
  #ifdef SignSetup
    OutputBaseFilename=Setup
  #else
    OutputBaseFilename=Setup_Unsigned
  #endif
  OutputDir=..\..\
  Uninstallable=no
#endif
#ifdef SignSetup
  SignedUninstaller=yes
  SignTool=signtool_skia4delphi
  SignToolRunMinimized=yes
#endif

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl,Languages\Default.isl"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl,Languages\Default.isl,Languages\BrazilianPortuguese.isl"; LicenseFile: "Languages\BrazilianPortuguese.LICENSE"
Name: "catalan"; MessagesFile: "compiler:Languages\Catalan.isl,Languages\Default.isl"
Name: "corsican"; MessagesFile: "compiler:Languages\Corsican.isl,Languages\Default.isl"
Name: "czech"; MessagesFile: "compiler:Languages\Czech.isl,Languages\Default.isl"
Name: "danish"; MessagesFile: "compiler:Languages\Danish.isl,Languages\Default.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl,Languages\Default.isl"
Name: "finnish"; MessagesFile: "compiler:Languages\Finnish.isl,Languages\Default.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl,Languages\Default.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl,Languages\Default.isl"
Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl,Languages\Default.isl"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl,Languages\Default.isl"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl,Languages\Default.isl"
Name: "norwegian"; MessagesFile: "compiler:Languages\Norwegian.isl,Languages\Default.isl"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl,Languages\Default.isl"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl,Languages\Default.isl,Languages\BrazilianPortuguese.isl"; LicenseFile: "Languages\BrazilianPortuguese.LICENSE"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl,Languages\Default.isl"
Name: "slovenian"; MessagesFile: "compiler:Languages\Slovenian.isl,Languages\Default.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl,Languages\Default.isl"
Name: "turkish"; MessagesFile: "compiler:Languages\Turkish.isl,Languages\Default.isl"
Name: "ukrainian"; MessagesFile: "compiler:Languages\Ukrainian.isl,Languages\Default.isl"

#ifdef FilesEmbedded
  #expr Exec(AddBackslash(SourcePath) + 'Scripts\Setup.Preprocessor.ClearFiles.bat', '', AddBackslash(SourcePath) + 'Scripts\')
  #define CommonRADStudioFilesExcludes "*.exe,*.dll,*.bpl,*.bpi,*.dcp,*.so,*.apk,*.drc,*.map,*.dres,*.rsm,*.tds,*.dcu,*.lib,*.jdbg,*.plist,*.cfg,*Resource.rc,*.cfg,*Resource.rc,*.local,*.identcache,*.projdata,*.tvsconfig,*.skincfg,*.cbk,*.dsk,__history\*,__recovery\*,*.~*,*.stat,modules\*,.github\*,*template*\*,*template*,*.a,*.dex,*.o,*.vrc,*.res,*.log,*.deployproj,*.bak,unins0*.dat,*.nupkg"
  ; Don't change the order of the files. This could affect the performance when extract temp files
  [Files]
    Source: "Style\*"; DestDir: "{app}\{#SetupFolder}\Style"; Flags: ignoreversion
    Source: "..\..\{#LibraryPackagesFolder}\*"; Excludes: "{#CommonRADStudioFilesExcludes}"; DestDir: "{app}\{#LibraryPackagesFolder}"; Flags: recursesubdirs ignoreversion
    Source: "..\..\*"; Excludes: "{#CommonRADStudioFilesExcludes},*.gitattributes,*.gitignore,*.gitmodules,README.md,\.github\*,\Assets\Artwork\*,\Assets\Documents\*,\Assets\Setup\*,Binary\*,\Documents\*,\Externals\*,\{#LibraryDCUFolder}\*,Logs\*,*.Logs.txt,Objects\*,\{#SetupFolder}\*,\Tools\Chocolatey Package\*,\{#LibraryPackagesFolder}\*"; DestDir: "{app}"; Flags: recursesubdirs ignoreversion
    Source: "..\..\Binary\*"; DestDir: "{app}\Binary"; Flags: recursesubdirs ignoreversion
    #ifdef UseLibraryDCUFolder
      Source: "..\..\{#LibraryDCUFolder}\*"; DestDir: "{app}\{#LibraryDCUFolder}"; Flags: recursesubdirs ignoreversion dontcopy
    #endif
#else
  #ifdef VclStyle
    [Files]
      Source: "Style\*.dll"; DestDir: "{app}"; Flags: dontcopy
  #endif
#endif

[Icons]
Name: "{group}\Uninstall"; Filename: "{uninstallexe}"

[Run]
Filename: "{app}\{#LibrarySamplesFolder}"; Description: "{cm:SetupOpenSamplesFolder}"; Flags: shellexec runasoriginaluser postinstall;
Filename: "{#LibraryDocumentationURL}"; Description: "{cm:SetupViewOnlineDocumentation}"; Flags: shellexec runasoriginaluser postinstall unchecked;

[UninstallDelete]
Type: filesandordirs; Name: "{app}\Assets\*";
Type: filesandordirs; Name: "{app}\Benchmark\*";
Type: filesandordirs; Name: "{app}\Binary\*";
Type: filesandordirs; Name: "{app}\Documents\*";
Type: filesandordirs; Name: "{app}\Externals\*";
Type: filesandordirs; Name: "{app}\Library\*";
Type: filesandordirs; Name: "{app}\Packages\*";
Type: filesandordirs; Name: "{app}\Samples\*";
Type: filesandordirs; Name: "{app}\Source\*";
Type: filesandordirs; Name: "{app}\Tests\*";
Type: filesandordirs; Name: "{app}\Tools\*";
Type: filesandordirs; Name: "{app}\.gitattributes";
Type: filesandordirs; Name: "{app}\.gitignore";
Type: filesandordirs; Name: "{app}\.gitmodules";
Type: filesandordirs; Name: "{app}\*.Logs.txt";
Type: filesandordirs; Name: "{app}\LICENSE";
Type: filesandordirs; Name: "{app}\README.md";
Type: filesandordirs; Name: "{app}\Setup.exe";
Type: filesandordirs; Name: "{app}\SKIA-LICENSE";
Type: dirifempty; Name: "{app}\Assets";
Type: dirifempty; Name: "{app}\Benchmark";
Type: dirifempty; Name: "{app}\Binary";
Type: dirifempty; Name: "{app}\Documents";
Type: dirifempty; Name: "{app}\Externals";
Type: dirifempty; Name: "{app}\Library";
Type: dirifempty; Name: "{app}\Packages";
Type: dirifempty; Name: "{app}\Samples";
Type: dirifempty; Name: "{app}\Source";
Type: dirifempty; Name: "{app}\Tests";
Type: dirifempty; Name: "{app}\Tools";
Type: dirifempty; Name: "{app}";

// Include
#include "Source\Setup.Main.inc"
#include "Source\Skia4Delphi.inc"
