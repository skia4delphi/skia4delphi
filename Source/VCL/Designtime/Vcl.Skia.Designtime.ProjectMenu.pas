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
unit Vcl.Skia.Designtime.ProjectMenu;

interface

{$SCOPEDENUMS ON}

procedure Register;

implementation

{$IF CompilerVersion >= 36} // RAD Studio 12 and newer
  {$DEFINE SKIAEMB}
{$ENDIF}

uses
  { Delphi }
  Winapi.Windows,
  Winapi.ShLwApi,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IOUtils,
  System.TypInfo,
  System.Generics.Collections,
  Vcl.ActnList,
  Vcl.Dialogs,
  ToolsAPI,
  DeploymentAPI,
  DesignIntf,
  ILinkStrs,
  BCCStrs,
  DCCStrs;

type
  TSkProjectPlatform = (Unknown, Win32, Win64, Android, Android64, iOSDevice32, iOSDevice64, iOSSimARM64, iOSSimulator, OSX64, OSXARM64, Linux64);
  TSkProjectPlatforms = set of TSkProjectPlatform;

  { TSkDeployFile }

  TSkDeployFile = record
    &Platform: TSkProjectPlatform;
    LocalFileName: string;
    RemotePath: string;
    CopyToOutput: Boolean;
    Required: Boolean;
    Operation: TDeployOperation;
    Condition: string;
    function Equals(const AProjectDeployFile: IProjectDeploymentFile; const APlatformName: string): Boolean;
  end;

  { TSkProjectPlatformHelper }

  TSkProjectPlatformHelper = record helper for TSkProjectPlatform
    function ToString: string;
    class function FromString(const AText: string): TSkProjectPlatform; static;
  end;

  { TSkProjectMenuCreatorNotifier }

  TSkProjectMenuCreatorNotifier = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  strict private class var
    FNotifierIndex: Integer;
  strict private
    class constructor Create;
    class destructor Destroy;
    { IOTAProjectMenuItemCreatorNotifier }
    procedure AddMenu(const AProject: IOTAProject; const AIdentList: TStrings; const AProjectManagerMenuList: IInterfaceList; AIsMultiSelect: Boolean);
  public
    class procedure Register; static;
  end;

  { TSkProjectManagerMenu }

  TSkProjectManagerMenu = class(TNotifierObject, IOTALocalMenu, IOTAProjectManagerMenu)
  strict private
    FCaption: string;
    FExecuteProc: TProc;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FVerb: string;
  strict protected
    { IOTALocalMenu }
    function GetCaption: string;
    function GetChecked: Boolean; virtual;
    function GetEnabled: Boolean; virtual;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const AValue: string);
    procedure SetChecked(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHelpContext(AValue: Integer);
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: string);
    procedure SetPosition(AValue: Integer);
    procedure SetVerb(const AValue: string);
    { IOTAProjectManagerMenu }
    procedure Execute(const AMenuContextList: IInterfaceList); overload;
    function GetIsMultiSelectable: Boolean;
    function PostExecute(const AMenuContextList: IInterfaceList): Boolean;
    function PreExecute(const AMenuContextList: IInterfaceList): Boolean;
    procedure SetIsMultiSelectable(AValue: Boolean);
  public
    constructor Create(const ACaption, AVerb: string; const APosition: Integer; const AExecuteProc: TProc = nil;
      const AName: string = ''; const AParent: string = '');
  end;

  { TSkProjectManagerMenuSeparator }

  TSkProjectManagerMenuSeparator = class(TSkProjectManagerMenu)
  public
    constructor Create(const APosition: Integer); reintroduce;
  end;

  { TSkProjectManagerMenuEnableSkia }

  TSkProjectManagerMenuEnableSkia = class(TSkProjectManagerMenu)
  strict private
    FIsSkiaEnabled: Boolean;
    procedure SetSkiaEnabled(const AProject: IOTAProject; const AEnabled: Boolean);
  strict protected
    function GetEnabled: Boolean; override;
  public
    constructor Create(const AProject: IOTAProject; const APosition: Integer); reintroduce;
  end;

  { TSkCompileNotifier }

  TSkCompileNotifier = class(TInterfacedObject, IOTACompileNotifier)
  strict private class var
    FNotifierIndex: Integer;
  strict private
    class constructor Create;
    class destructor Destroy;
    { IOTACompileNotifier }
    procedure ProjectCompileFinished(const AProject: IOTAProject; AResult: TOTACompileResult);
    procedure ProjectCompileStarted(const AProject: IOTAProject; AMode: TOTACompileMode);
    procedure ProjectGroupCompileFinished(AResult: TOTACompileResult);
    procedure ProjectGroupCompileStarted(AMode: TOTACompileMode);
  public
    class procedure Register; static;
  end;

  { TSkDeployFilesHelper }

  TSkDeployFilesHelper = record
  strict private
    class function IsValidDeployFile(const AFile: IProjectDeploymentFile; const APlatformName: string; const AAllowedFiles: TArray<TSkDeployFile>): Boolean; static;
  public
    class procedure AddDeployFiles(const AProject: IOTAProject); static;
    class function CanDeploy(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string): Boolean; static;
    class procedure CopyToOutput(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string); static;
    class procedure DeleteFromOutput(const AProject: IOTAProject); overload; static;
    class procedure DeleteFromOutput(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string); overload; static;
    class procedure EnsureDeployFiles(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string); static;
    class function LocalFilesExists: Boolean; static;
    class procedure RemoveDeployFiles(const AProject: IOTAProject); static;
  end;

  { TSkProjectHelper }

  TSkProjectHelper = record
  strict private
    class function DefinesName(const AProject: IOTAProject): string; static;
    class function GetIsSkiaDefined(const AProject: IOTAProject): Boolean; static;
    class procedure SetIsSkiaDefined(const AProject: IOTAProject; const AValue: Boolean); static;
  public
    class function IsSkiaDefinedForPlatform(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string): Boolean; static;
    class function IsSupported(const AProject: IOTAProject): Boolean; overload; static;
    class function IsSupported(const AProject: IOTAProject; const APlatform: TSkProjectPlatform): Boolean; overload; static;
    class property IsSkiaDefined[const AProject: IOTAProject]: Boolean read GetIsSkiaDefined write SetIsSkiaDefined;
  end;

  { TSkOTAHelper }

  TSkOTAHelper = record
  strict private const
    DefaultOptionsSeparator = ';'; // do not localize
    FinalOutputDirPropertyName = 'FinalOutputDir'; // do not localize
    OutputDirPropertyName = 'OutputDir'; // do not localize
  strict private
    class function ExpandConfiguration(const ASource: string; const AConfig: IOTABuildConfiguration): string; overload; static;
    class function ExpandConfiguration(const ASource: string; const APlatform: TSkProjectPlatform; const AConfig: string): string; overload; static;
    class function ExpandEnvironmentVar(var AValue: string): Boolean; static;
    class function ExpandOutputPath(const ASource: string; const ABuildConfig: IOTABuildConfiguration): string; static;
    class function ExpandPath(const ABaseDir, ARelativeDir: string): string; static;
    class function ExpandVars(const ASource: string): string; overload; static;
    class function GetEnvironmentVars(const AVars: TStrings; AExpand: Boolean): Boolean; static;
    class function GetProjectOptionsConfigurations(const AProject: IOTAProject): IOTAProjectOptionsConfigurations; static;
    class procedure MultiSzToStrings(const ADest: TStrings; const ASource: PChar); static;
    class procedure StrResetLength(var S: string); static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; ABuildConfig: IOTABuildConfiguration; out AOutputPath: string): Boolean; overload; static;
  public
    class function BuildConfigs(const AProject: IOTAProject): TArray<string>; static;
    class function ContainsOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): Boolean; static;
    class function ExpandVars(const ASource: string; const APlatform: TSkProjectPlatform; const AConfig: string): string; overload; static;
    class function InsertOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;
    class function RemoveOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;
    class function TryCopyFileToOutputPath(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig, AFileName: string): Boolean; static;
    class function TryGetBuildConfig(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string; out ABuildConfig: IOTABuildConfiguration): Boolean; static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string; out AOutputPath: string): Boolean; overload; static;
    class function TryRemoveOutputFile(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: string; AFileName: string): Boolean; static;
  end;

  { TSkiaLibrary }

  TSkiaLibrary = class
  strict private const
    {$IF CompilerVersion < 28} // Below RAD Studio XE7
    CBuilderSupportedPlatforms = [];
    DelphiSupportedPlatforms = [];
    {$ELSEIF CompilerVersion < 33} // RAD Studio XE7 to RAD Studio 10.2 Tokyo
    CBuilderSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64];
    DelphiSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64];
    {$ELSEIF CompilerVersion < 35} // RAD Studio 10.3 Rio and RAD Studio 10.4 Sydney
    CBuilderSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64, TSkProjectPlatform.Android];
    DelphiSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64, TSkProjectPlatform.Android,
      TSkProjectPlatform.Android64];
    {$ELSE} // RAD Studio 11 Alexandria and newer
    CBuilderSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64, TSkProjectPlatform.Android,
      TSkProjectPlatform.iOSDevice64];
    DelphiSupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64, TSkProjectPlatform.Android,
      TSkProjectPlatform.Android64, TSkProjectPlatform.iOSDevice64, TSkProjectPlatform.iOSSimARM64,
      TSkProjectPlatform.OSX64, TSkProjectPlatform.OSXARM64, TSkProjectPlatform.Linux64];
    {$ENDIF}
  strict private class var
    FBinariesChecked: Boolean;
    FBinariesFound: Boolean;
  public
    class function IsPersonalitySupported(const APersonality: string): Boolean; static;
    class function IsSupported: Boolean; static;
    class function SupportedPlatforms(const APersonality: string): TSkProjectPlatforms; static;
  end;

resourcestring
  sDisableSkia = 'Disable Skia';
  sEnableSkia  = 'Enable Skia';
  sUnsupportedPlatformMessage =
    'The Skia does not support the platform %s in this RAD Studio version.' + sLineBreak + sLineBreak +
    'To avoid problems, disable Skia in this project (Project menu > %s) or, if you want to disable it just in ' +
    'a specific platform, set the define directive "%s" in the project settings of this platform. In both cases, ' +
    'be sure you are not using any Skia units, otherwise you will get "runtime error" on startup of your application.';

const
  cbtRelease                = 'Release'; // do not localize
  InvalidNotifier           = -1;
  SkiaDeploymentClass       = 'Skia'; // do not localize
  SkiaDirVariable           = 'SKIADIR'; // do not localize
  SkiaMenuCaption: array[Boolean] of string = (sEnableSkia, sDisableSkia);
  SkiaProjectDefine         = 'SKIA'; // do not localize
  SkiaProjectDisabledDefine = 'SKIA_DISABLED'; // do not localize

  {$IF CompilerVersion >= 32}
  SkiaPlatformsOptions: array[0..1] of
  record
    &Platform: TSkProjectPlatform;
    CppLibraryPath: TArray<string>;
    CppLinkedLibraries: TArray<string>;
  end = (
    (&Platform: TSkProjectPlatform.Android;     CppLibraryPath: [];                             CppLinkedLibraries: ['Skia.Package.RTL', 'Skia.Package.FMX']),
    (&Platform: TSkProjectPlatform.iOSDevice64; CppLibraryPath: ['/usr/lib/clang/lib/darwin/']; CppLinkedLibraries: ['clang_rt.ios', 'sk4d', 'Skia.Package.RTL', 'Skia.Package.FMX'])
  );
  {$ENDIF}

  {$IF CompilerVersion >= 35}
  SkiaDeployFiles: array[0..15] of TSkDeployFile = (
    (&Platform: TSkProjectPlatform.Win32;       LocalFileName: '$(BDS)\bin\sk4d.dll';                                            RemotePath: '.\';                       CopyToOutput: True;  Required: True; Operation: TDeployOperation.doCopyOnly;   Condition: '''$('+SkiaDirVariable+')''=='''''), // Win32
    (&Platform: TSkProjectPlatform.Win64;       LocalFileName: '$(BDS)\bin64\sk4d.dll';                                          RemotePath: '.\';                       CopyToOutput: True;  Required: True; Operation: TDeployOperation.doCopyOnly;   Condition: '''$('+SkiaDirVariable+')''=='''''), // Win64
    (&Platform: TSkProjectPlatform.Android;     LocalFileName: '$(BDS)\binandroid32\libsk4d.so';                                 RemotePath: 'library\lib\armeabi-v7a\'; CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''''), // Android
    (&Platform: TSkProjectPlatform.Android64;   LocalFileName: '$(BDS)\binandroid64\libsk4d.so';                                 RemotePath: 'library\lib\arm64-v8a\';   CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''''), // Android64
    (&Platform: TSkProjectPlatform.Android64;   LocalFileName: '$(BDS)\binandroid32\libsk4d.so';                                 RemotePath: 'library\lib\armeabi-v7a\'; CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''' and ''$(AndroidAppBundle)''==''true'''), // Android64
    (&Platform: TSkProjectPlatform.OSX64;       LocalFileName: '$(BDS)\binosx64\libsk4d.dylib';                                  RemotePath: 'Contents\MacOS\';          CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''''), // OSX64
    (&Platform: TSkProjectPlatform.OSXARM64;    LocalFileName: '$(BDS)\binosxarm64\libsk4d.dylib';                               RemotePath: 'Contents\MacOS\';          CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''''), // OSXARM64
    (&Platform: TSkProjectPlatform.Linux64;     LocalFileName: '$(BDS)\binlinux64\libsk4d.so';                                   RemotePath: '.\';                       CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''=='''''), // Linux64
  {$ELSE}
  SkiaDeployFiles: array[0..7] of TSkDeployFile = (
  {$ENDIF}
    (&Platform: TSkProjectPlatform.Win32;       LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Win32\sk4d.dll';            RemotePath: '.\';                       CopyToOutput: True;  Required: True; Operation: TDeployOperation.doCopyOnly;   Condition: '''$('+SkiaDirVariable+')''!='''''), // Win32
    (&Platform: TSkProjectPlatform.Win64;       LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Win64\sk4d.dll';            RemotePath: '.\';                       CopyToOutput: True;  Required: True; Operation: TDeployOperation.doCopyOnly;   Condition: '''$('+SkiaDirVariable+')''!='''''), // Win64
    (&Platform: TSkProjectPlatform.Android;     LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Android\libsk4d.so';        RemotePath: 'library\lib\armeabi-v7a\'; CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!='''''), // Android
    (&Platform: TSkProjectPlatform.Android64;   LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Android64\libsk4d.so';      RemotePath: 'library\lib\arm64-v8a\';   CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!='''''), // Android64
    (&Platform: TSkProjectPlatform.Android64;   LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Android\libsk4d.so';        RemotePath: 'library\lib\armeabi-v7a\'; CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!='''' and ''$(AndroidAppBundle)''==''true'''), // Android64
    (&Platform: TSkProjectPlatform.OSX64;       LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\OSX64\libsk4d.dylib';       RemotePath: 'Contents\MacOS\';          CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!='''''), // OSX64
    (&Platform: TSkProjectPlatform.OSXARM64;    LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\OSXARM64\libsk4d.dylib';    RemotePath: 'Contents\MacOS\';          CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!='''''), // OSXARM64
    (&Platform: TSkProjectPlatform.Linux64;     LocalFileName: '$('+SkiaDirVariable+')\Binary\Shared\Linux64\libsk4d.so';        RemotePath: '.\';                       CopyToOutput: False; Required: True; Operation: TDeployOperation.doSetExecBit; Condition: '''$('+SkiaDirVariable+')''!=''''')  // Linux64
  );

function ContainsStringInArray(const AString: string;
  const AArray: TArray<string>; const ACaseSensitive: Boolean = True): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ACaseSensitive then
  begin
    for I := Low(AArray) to High(AArray) do
      if AArray[I] = AString then
        Exit(True);
  end
  else
  begin
    for I := Low(AArray) to High(AArray) do
      if SameText(AArray[I], AString) then
        Exit(True);
  end;
end;

function GetSkiaDeployFiles(const APlatform: TSkProjectPlatform): TArray<TSkDeployFile>;
var
  LDeployFile: TSkDeployFile;
begin
  Result := [];
  for LDeployFile in SkiaDeployFiles do
    if LDeployFile.Platform = APlatform then
      Result := Result + [LDeployFile];
end;

{ TSkDeployFile }

function TSkDeployFile.Equals(const AProjectDeployFile: IProjectDeploymentFile;
  const APlatformName: string): Boolean;
begin
  Result := (AProjectDeployFile.LocalName = LocalFileName) and
    (IncludeTrailingPathDelimiter(AProjectDeployFile.RemoteDir[APlatformName]) = IncludeTrailingPathDelimiter(RemotePath)) and
    (AProjectDeployFile.RemoteName[APlatformName] = TPath.GetFileName(LocalFileName)) and
    (AProjectDeployFile.Condition = Condition) and (AProjectDeployFile.Operation[APlatformName] = Operation) and
    (AProjectDeployFile.DeploymentClass = SkiaDeploymentClass) and
    AProjectDeployFile.Enabled[APlatformName] and AProjectDeployFile.Overwrite[APlatformName] and
    (AProjectDeployFile.Required = Required);
end;

{ TSkProjectPlatformHelper }

function TSkProjectPlatformHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TSkProjectPlatform), Ord(Self));
end;

class function TSkProjectPlatformHelper.FromString(const AText: string): TSkProjectPlatform;
var
  LEnumValue: Integer;
begin
  LEnumValue := GetEnumValue(TypeInfo(TSkProjectPlatform), AText);
  if LEnumValue = -1 then
    Result := TSkProjectPlatform.Unknown
  else
    Result := TSkProjectPlatform(GetEnumValue(TypeInfo(TSkProjectPlatform), AText));
end;

{ TSkProjectMenuCreatorNotifier }

procedure TSkProjectMenuCreatorNotifier.AddMenu(const AProject: IOTAProject;
  const AIdentList: TStrings; const AProjectManagerMenuList: IInterfaceList;
  AIsMultiSelect: Boolean);
begin
  if (not AIsMultiSelect) and (AIdentList.IndexOf(sProjectContainer) <> -1) and
    Assigned(AProjectManagerMenuList) and (TSkProjectHelper.IsSkiaDefined[AProject] or
    (TSkiaLibrary.IsSupported and TSkProjectHelper.IsSupported(AProject))) then
  begin
    AProjectManagerMenuList.Add(TSkProjectManagerMenuSeparator.Create(pmmpRunNoDebug + 10));
    AProjectManagerMenuList.Add(TSkProjectManagerMenuEnableSkia.Create(AProject, pmmpRunNoDebug + 20));
  end;
end;

class constructor TSkProjectMenuCreatorNotifier.Create;
begin
  FNotifierIndex := InvalidNotifier;
end;

class destructor TSkProjectMenuCreatorNotifier.Destroy;
var
  LProjectManager: IOTAProjectManager;
begin
  if (FNotifierIndex > InvalidNotifier) and Supports(BorlandIDEServices, IOTAProjectManager, LProjectManager) then
    LProjectManager.RemoveMenuItemCreatorNotifier(FNotifierIndex);
end;

class procedure TSkProjectMenuCreatorNotifier.Register;
var
  LProjectManager: IOTAProjectManager;
begin
  if (FNotifierIndex <= InvalidNotifier) and Supports(BorlandIDEServices, IOTAProjectManager, LProjectManager) then
    FNotifierIndex := LProjectManager.AddMenuItemCreatorNotifier(TSkProjectMenuCreatorNotifier.Create);
end;

{ TSkProjectManagerMenu }

constructor TSkProjectManagerMenu.Create(const ACaption, AVerb: string;
  const APosition: Integer; const AExecuteProc: TProc = nil;
  const AName: string = ''; const AParent: string = '');
begin
  inherited Create;
  FCaption := ACaption;
  FName := AName;
  FParent := AParent;
  FPosition := APosition;
  FVerb := AVerb;
  FExecuteProc := AExecuteProc;
end;

procedure TSkProjectManagerMenu.Execute(const AMenuContextList: IInterfaceList);
begin
  if Assigned(FExecuteProc) then
    FExecuteProc;
end;

function TSkProjectManagerMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TSkProjectManagerMenu.GetChecked: Boolean;
begin
  Result := False;
end;

function TSkProjectManagerMenu.GetEnabled: Boolean;
begin
  Result := True;
end;

function TSkProjectManagerMenu.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TSkProjectManagerMenu.GetIsMultiSelectable: Boolean;
begin
  Result := False;
end;

function TSkProjectManagerMenu.GetName: string;
begin
  Result := FName;
end;

function TSkProjectManagerMenu.GetParent: string;
begin
  Result := FParent;
end;

function TSkProjectManagerMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TSkProjectManagerMenu.GetVerb: string;
begin
  Result := FVerb;
end;

function TSkProjectManagerMenu.PostExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

function TSkProjectManagerMenu.PreExecute(const AMenuContextList: IInterfaceList): Boolean;
begin
  Result := False;
end;

procedure TSkProjectManagerMenu.SetCaption(const AValue: string);
begin
end;

procedure TSkProjectManagerMenu.SetChecked(AValue: Boolean);
begin
end;

procedure TSkProjectManagerMenu.SetEnabled(AValue: Boolean);
begin
end;

procedure TSkProjectManagerMenu.SetHelpContext(AValue: Integer);
begin
end;

procedure TSkProjectManagerMenu.SetIsMultiSelectable(AValue: Boolean);
begin
end;

procedure TSkProjectManagerMenu.SetName(const AValue: string);
begin
end;

procedure TSkProjectManagerMenu.SetParent(const AValue: string);
begin
end;

procedure TSkProjectManagerMenu.SetPosition(AValue: Integer);
begin
end;

procedure TSkProjectManagerMenu.SetVerb(const AValue: string);
begin
end;

{ TSkProjectManagerMenuSeparator }

constructor TSkProjectManagerMenuSeparator.Create(const APosition: Integer);
begin
  inherited Create('-', '', APosition);
end;

{ TSkProjectManagerMenuEnableSkia }

constructor TSkProjectManagerMenuEnableSkia.Create(const AProject: IOTAProject;
  const APosition: Integer);
begin
  FIsSkiaEnabled := TSkProjectHelper.IsSkiaDefined[AProject];
  inherited Create(SkiaMenuCaption[FIsSkiaEnabled], '', APosition,
    procedure()
    begin
      SetSkiaEnabled(AProject, not FIsSkiaEnabled);
    end);
end;

function TSkProjectManagerMenuEnableSkia.GetEnabled: Boolean;
begin
  Result := FIsSkiaEnabled or TSkiaLibrary.IsSupported;
end;

procedure TSkProjectManagerMenuEnableSkia.SetSkiaEnabled(
  const AProject: IOTAProject; const AEnabled: Boolean);

  function ApplyDelphiSourceChange(var ASource: string; const AEnabled: Boolean): Boolean;

    // Add the "FMX.Skia" to uses, after the FMX.Forms, if it isn't inside a ifdef
    function AddSkiaFMXUnit(const ASourceList: TStringList): Boolean;
    var
      LIfDefCount: Integer;
      I: Integer;
    begin
      Result := False;
      for I := 0 to ASourceList.Count - 1 do
        if ASourceList[I].TrimLeft.StartsWith('FMX.Skia,', True) or ASourceList[I].TrimLeft.StartsWith('FMX.Skia ', True) then
          Exit;
      LIfDefCount := 0;
      for I := 0 to ASourceList.Count - 1 do
      begin
        if ASourceList[I].TrimLeft.StartsWith('{$IF', True) then
          Inc(LIfDefCount);
        if ASourceList[I].ToUpper.Contains('{$END') then
          LIfDefCount := Max(LIfDefCount - 1, 0);
        if ASourceList[I].TrimLeft.StartsWith('FMX.Forms,', True) or ASourceList[I].TrimLeft.StartsWith('FMX.Forms ', True) then
        begin
          if LIfDefCount = 0 then
          begin
            ASourceList.Insert(I + 1, '  FMX.Skia,');
            Exit(True);
          end
          else
            Break;
        end;
      end;
    end;

    function AddGlobalUseSkia(const ASourceList: TStringList): Boolean;
    var
      LIfDefCount: Integer;
      I: Integer;
    begin
      Result := False;
      if not ASourceList.Text.ToLower.Contains(string('FMX.Skia,').ToLower) and
        not ASourceList.Text.ToLower.Contains(string('FMX.Skia ').ToLower) then
      begin
        Exit;
      end;
      for I := 0 to ASourceList.Count - 1 do
        if ASourceList[I].Replace(' ', '').StartsWith('GlobalUseSkia:=True', True) then
          Exit;
      LIfDefCount := 0;
      for I := ASourceList.Count - 1 downto 0 do
      begin
        if ASourceList[I].ToUpper.Contains('{$END') then
          Inc(LIfDefCount);
        if ASourceList[I].TrimLeft.StartsWith('{$IF', True) then
          LIfDefCount := Max(LIfDefCount - 1, 0);
        if SameText(ASourceList[I].Replace(' ', ''), 'GlobalUseSkia:=False;') and (LIfDefCount = 0) then
          ASourceList.Delete(I);
      end;
      for I := 0 to ASourceList.Count - 1 do
      begin
        if SameText(ASourceList[I].Trim, 'begin') then
        begin
          ASourceList.Insert(I + 1, '  GlobalUseSkia := True;');
          Exit(True);
        end;
      end;
    end;

    // Remove line starting with specific text, if it isn't inside a ifdef
    function RemoveLineStartingWith(const ASourceList: TStringList; AStartText: string): Boolean;
    var
      LIfDefCount: Integer;
      I: Integer;
    begin
      Result := False;
      AStartText := AStartText.Replace(' ', '');
      LIfDefCount := 0;
      for I := ASourceList.Count - 1 downto 0 do
      begin
        if ASourceList[I].ToUpper.Contains('{$END') then
          Inc(LIfDefCount);
        if ASourceList[I].TrimLeft.StartsWith('{$IF', True) then
          LIfDefCount := Max(LIfDefCount - 1, 0);
        if ASourceList[I].Replace(' ', '').StartsWith(AStartText) then
        begin
          if LIfDefCount = 0 then
          begin
            ASourceList.Delete(I);
            Result := True;
          end
          else
            Break;
        end;
      end;
    end;

  var
    LSourceList: TStringList;
  begin
    LSourceList := TStringList.Create;
    try
      {$IF CompilerVersion >= 31}
      LSourceList.TrailingLineBreak := False;
      {$ENDIF}
      LSourceList.Text := ASource;
      if AEnabled then
      begin
        Result := AddSkiaFMXUnit(LSourceList);
        Result := AddGlobalUseSkia(LSourceList) or Result;
      end
      else
      begin
        Result := RemoveLineStartingWith(LSourceList, '  FMX.Skia,');
        Result := RemoveLineStartingWith(LSourceList, '  GlobalUseSkia :=') or Result;
        Result := RemoveLineStartingWith(LSourceList, '  GlobalUseSkiaRasterWhenAvailable :=') or Result;
      end;
      if Result then
      begin
        ASource := LSourceList.Text;
        {$IF CompilerVersion < 31}
        ASource := ASource.TrimRight;
        {$ENDIF}
      end;
    finally
      LSourceList.Free;
    end;
  end;

  function GetEditorString(const ASourceEditor: IOTASourceEditor70; out AValue: string): Boolean;
  const
    BufferSize: Integer = 1024;
  var
    LReader: IOTAEditReader;
    LReadCount: Integer;
    LPosition: Integer;
    LBuffer: AnsiString;
  begin
    LReader := ASourceEditor.CreateReader;
    Result := Assigned(LReader);
    if Result then
    begin
      AValue := '';
      LPosition := 0;
      repeat
        SetLength(LBuffer, BufferSize);
        LReadCount := LReader.GetText(LPosition, PAnsiChar(LBuffer), BufferSize);
        SetLength(LBuffer, LReadCount);
        AValue := AValue + string(LBuffer);
        Inc(LPosition, LReadCount);
      until LReadCount < BufferSize;
    end;
  end;

  procedure SetEditorString(const ASourceEditor: IOTASourceEditor70; const AValue: string);
  var
    LEditorWriter: IOTAEditWriter;
  begin
    LEditorWriter := ASourceEditor.CreateUndoableWriter;
    if Assigned(LEditorWriter) then
    begin
      LEditorWriter.CopyTo(0);
      LEditorWriter.DeleteTo(MaxInt);
      LEditorWriter.Insert(PAnsiChar(AnsiString(AValue)));
      ASourceEditor.MarkModified;
    end;
  end;

  procedure ChangeSource(const AProject: IOTAProject; const AEnabled: Boolean);
  var
    LSourceEditor: IOTASourceEditor70;
    LSourceText: string;
    I: Integer;
  begin
    if (AProject.ApplicationType = sApplication) and (AProject.FrameworkType = sFrameworkTypeFMX) then
    begin
      for I := 0 to AProject.GetModuleFileCount - 1 do
      begin
        if Supports(AProject.ModuleFileEditors[I], IOTASourceEditor70, LSourceEditor) and
          GetEditorString(LSourceEditor, LSourceText) then
        begin
          if (AProject.Personality = sDelphiPersonality) and ApplyDelphiSourceChange(LSourceText, AEnabled) then
            SetEditorString(LSourceEditor, LSourceText);
        end;
      end;
    end;
  end;

var
  LProjectOptions: IOTAProjectOptions;
begin
  TSkDeployFilesHelper.RemoveDeployFiles(AProject);
  TSkProjectHelper.IsSkiaDefined[AProject] := AEnabled;
  if AEnabled then
    TSkDeployFilesHelper.AddDeployFiles(AProject)
  else
    TSkDeployFilesHelper.DeleteFromOutput(AProject);
  LProjectOptions := AProject.ProjectOptions;
  if Assigned(LProjectOptions) then
    LProjectOptions.ModifiedState := True;
  ChangeSource(AProject, AEnabled);

  {$IF CompilerVersion >= 35}
  var LProjectBuilder := AProject.ProjectBuilder;
  if Assigned(LProjectBuilder) then
    LProjectBuilder.BuildProject(TOTACompileMode.cmOTAClean, False, True);
  {$ENDIF}
end;

{ TSkCompileNotifier }

class constructor TSkCompileNotifier.Create;
begin
  FNotifierIndex := InvalidNotifier;
end;

class destructor TSkCompileNotifier.Destroy;
var
  LCompileServices: IOTACompileServices;
begin
  if (FNotifierIndex > InvalidNotifier) and Supports(BorlandIDEServices, IOTACompileServices, LCompileServices) then
    LCompileServices.RemoveNotifier(FNotifierIndex);
end;

procedure TSkCompileNotifier.ProjectCompileFinished(const AProject: IOTAProject;
  AResult: TOTACompileResult);
begin
end;

procedure TSkCompileNotifier.ProjectCompileStarted(const AProject: IOTAProject;
  AMode: TOTACompileMode);
var
  LConfig: string;
  LPlatform: TSkProjectPlatform;
begin
  if TSkProjectHelper.IsSupported(AProject) and TSkProjectHelper.IsSkiaDefined[AProject] then
  begin
    LPlatform := TSkProjectPlatform.FromString(AProject.CurrentPlatform);
    if LPlatform = TSkProjectPlatform.Unknown then
      Exit;
    LConfig := AProject.CurrentConfiguration;

    case AMode of
      cmOTAMake,
      cmOTABuild:
        begin
          TSkDeployFilesHelper.EnsureDeployFiles(AProject, LPlatform, LConfig);
          if TSkDeployFilesHelper.CanDeploy(AProject, LPlatform, LConfig) then
            TSkDeployFilesHelper.CopyToOutput(AProject, LPlatform, LConfig)
          else
            TSkDeployFilesHelper.DeleteFromOutput(AProject, LPlatform, LConfig);
          if TSkProjectHelper.IsSkiaDefinedForPlatform(AProject, LPlatform, LConfig) and
            not TSkProjectHelper.IsSupported(AProject, LPlatform) then
          begin
            ShowMessage(Format(sUnsupportedPlatformMessage, [AProject.CurrentPlatform, SkiaMenuCaption[True], SkiaProjectDisabledDefine]));
          end;
        end;
      {$IF CompilerVersion >= 35}
      cmOTAClean: TSkDeployFilesHelper.DeleteFromOutput(AProject, LPlatform, LConfig);
      {$ENDIF}
    else
    end;
  end;
end;

procedure TSkCompileNotifier.ProjectGroupCompileFinished(
  AResult: TOTACompileResult);
begin
end;

procedure TSkCompileNotifier.ProjectGroupCompileStarted(AMode: TOTACompileMode);
begin
end;

class procedure TSkCompileNotifier.Register;
var
  LCompileServices: IOTACompileServices;
begin
  if (FNotifierIndex <= InvalidNotifier) and Supports(BorlandIDEServices, IOTACompileServices, LCompileServices) then
    FNotifierIndex := LCompileServices.AddNotifier(TSkCompileNotifier.Create);
end;

{ TSkDeployFilesHelper }

class procedure TSkDeployFilesHelper.AddDeployFiles(
  const AProject: IOTAProject);

  procedure DoAddFile(const AProjectDeployment: IProjectDeployment;
    const APlatformName, AConfigName: string; const ADeployFile: TSkDeployFile);
  var
    LFile: IProjectDeploymentFile;
  begin
    LFile := AProjectDeployment.CreateFile(AConfigName, APlatformName, ADeployFile.LocalFileName);
    if Assigned(LFile) then
    begin
      LFile.Overwrite[APlatformName] := True;
      LFile.Enabled[APlatformName] := True;
      LFile.Required := ADeployFile.Required;
      LFile.Condition := ADeployFile.Condition;
      LFile.Operation[APlatformName] := ADeployFile.Operation;
      LFile.RemoteDir[APlatformName] := ADeployFile.RemotePath;
      LFile.DeploymentClass := SkiaDeploymentClass;
      LFile.RemoteName[APlatformName] := TPath.GetFileName(ADeployFile.LocalFileName);
      AProjectDeployment.AddFile(AConfigName, APlatformName, LFile);
      AProject.MarkModified;
    end;
  end;

  procedure SetDeployFiles(const AProjectDeployment: IProjectDeployment;
    const AConfig: string; const APlatform: TSkProjectPlatform);
  var
    LDeployFile: TSkDeployFile;
  begin
    if (AProject.ApplicationType = sLibrary) and not (APlatform in [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64]) then
      Exit;
    for LDeployFile in GetSkiaDeployFiles(APlatform) do
      DoAddFile(AProjectDeployment, LDeployFile.Platform.ToString, AConfig, LDeployFile);
  end;

var
  LConfig: string;
  LPlatform: TSkProjectPlatform;
  LProjectDeployment: IProjectDeployment;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then
    for LConfig in TSkOTAHelper.BuildConfigs(AProject) do
      for LPlatform := Low(TSkProjectPlatform) to High(TSkProjectPlatform) do
        if TSkProjectHelper.IsSupported(AProject, LPlatform) and TSkProjectHelper.IsSkiaDefinedForPlatform(AProject, LPlatform, LConfig) then
            SetDeployFiles(LProjectDeployment, LConfig, LPlatform);
end;

class function TSkDeployFilesHelper.CanDeploy(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform;
  const AConfig: string): Boolean;
begin
  Result := TSkProjectHelper.IsSupported(AProject, APlatform) and Supports(AProject, IProjectDeployment)
    and TSkProjectHelper.IsSkiaDefined[AProject] and TSkProjectHelper.IsSkiaDefinedForPlatform(AProject, APlatform, AConfig);
end;

class procedure TSkDeployFilesHelper.CopyToOutput(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: string);
var
  LDeployFile: TSkDeployFile;
begin
  for LDeployFile in GetSkiaDeployFiles(APlatform) do
    if LDeployFile.CopyToOutput or (AProject.ApplicationType = sLibrary) then
      TSkOTAHelper.TryCopyFileToOutputPath(AProject, APlatform, AConfig, TSkOTAHelper.ExpandVars(LDeployFile.LocalFileName, APlatform, AConfig));
end;

class procedure TSkDeployFilesHelper.DeleteFromOutput(
  const AProject: IOTAProject);
var
  LConfig: string;
  LPlatform: TSkProjectPlatform;
begin
  for LConfig in TSkOTAHelper.BuildConfigs(AProject) do
    for LPlatform := Low(TSkProjectPlatform) to High(TSkProjectPlatform) do
      if TSkProjectHelper.IsSupported(AProject, LPlatform) then
        DeleteFromOutput(AProject, LPlatform, LConfig);
end;

class procedure TSkDeployFilesHelper.DeleteFromOutput(
  const AProject: IOTAProject; const APlatform: TSkProjectPlatform;
  const AConfig: string);
var
  LDeployFile: TSkDeployFile;
begin
  for LDeployFile in GetSkiaDeployFiles(APlatform) do
    if LDeployFile.CopyToOutput or (AProject.ApplicationType = sLibrary) then
      TSkOTAHelper.TryRemoveOutputFile(AProject, APlatform, AConfig, TPath.GetFileName(LDeployFile.LocalFileName));
end;

class procedure TSkDeployFilesHelper.EnsureDeployFiles(
  const AProject: IOTAProject; const APlatform: TSkProjectPlatform;
  const AConfig: string);

  function HasValidSkiaFiles(const AProjectDeployment: IProjectDeployment): Boolean;
  var
    LFile: IProjectDeploymentFile;
    LFiles: TDeploymentFileArray;
    LFound: Boolean;
    LPlatformName: string;
    LSkiaDeployFiles: TArray<TSkDeployFile>;
    LSkiaFile: TSkDeployFile;
  begin
    LPlatformName := APlatform.ToString;
    if (AProject.ApplicationType = sLibrary) and not (APlatform in [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64]) then
      LSkiaDeployFiles := []
    else
      LSkiaDeployFiles := GetSkiaDeployFiles(APlatform);
    for LFile in AProjectDeployment.FindFiles('', AConfig, LPlatformName, '') do
      if not IsValidDeployFile(LFile, LPlatformName, LSkiaDeployFiles) then
        Exit(False);

    LFiles := AProjectDeployment.FindFiles('', AConfig, LPlatformName, SkiaDeploymentClass);
    for LSkiaFile in LSkiaDeployFiles do
    begin
      LFound := False;
      for LFile in LFiles do
      begin
        if LSkiaFile.Equals(LFile, LPlatformName) then
        begin
          LFound := True;
          Break;
        end;
      end;
      if not LFound then
        Exit(False);
    end;
    Result := True;
  end;

var
  LProjectDeployment: IProjectDeployment;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then
  begin
    if (CanDeploy(AProject, APlatform, AConfig) and not HasValidSkiaFiles(LProjectDeployment)) or
      ((not CanDeploy(AProject, APlatform, AConfig)) and (LProjectDeployment.GetFilesOfClass(SkiaDeploymentClass) <> nil)) then
    begin
      RemoveDeployFiles(AProject);
      if TSkProjectHelper.IsSkiaDefined[AProject] then
        AddDeployFiles(AProject);
    end;
  end;
end;

class function TSkDeployFilesHelper.IsValidDeployFile(
  const AFile: IProjectDeploymentFile; const APlatformName: string;
  const AAllowedFiles: TArray<TSkDeployFile>): Boolean;
var
  LDeployFile: TSkDeployFile;
begin
  if AFile.DeploymentClass = SkiaDeploymentClass then
  begin
    for LDeployFile in AAllowedFiles do
      if LDeployFile.Equals(AFile, APlatformName) then
        Exit(True);
    Result := False;
  end
  else
  begin
    for LDeployFile in AAllowedFiles do
    begin
      if SameText(IncludeTrailingPathDelimiter(AFile.RemoteDir[APlatformName]), IncludeTrailingPathDelimiter(LDeployFile.RemotePath)) and
        SameText(AFile.RemoteName[APlatformName], TPath.GetFileName(LDeployFile.LocalFileName)) then
      begin
        Exit(False);
      end;
    end;
    Result := True;
  end;
end;

class function TSkDeployFilesHelper.LocalFilesExists: Boolean;
var
  LFile: TSkDeployFile;
begin
  Result := False;
  for LFile in GetSkiaDeployFiles(TSkProjectPlatform.Win32) do
    if TFile.Exists(TSkOTAHelper.ExpandVars(LFile.LocalFileName, TSkProjectPlatform.Win32, cbtRelease)) then
      Exit(True);
end;

class procedure TSkDeployFilesHelper.RemoveDeployFiles(
  const AProject: IOTAProject);
var
  LChanged: Boolean;
  LConfig: string;
  LFile: IProjectDeploymentFile;
  LPlatform: TSkProjectPlatform;
  LPlatformDeployFiles: TArray<TSkDeployFile>;
  LPlatformName: string;
  LProjectDeployment: IProjectDeployment;
begin
  if Supports(AProject, IProjectDeployment, LProjectDeployment) then
  begin
    LChanged := LProjectDeployment.GetFilesOfClass(SkiaDeploymentClass) <> nil;
    LProjectDeployment.RemoveFilesOfClass(SkiaDeploymentClass);
    LProjectDeployment.RemoveClass(SkiaDeploymentClass);
    for LConfig in TSkOTAHelper.BuildConfigs(AProject) do
    begin
      for LPlatform := Low(TSkProjectPlatform) to High(TSkProjectPlatform) do
      begin
        if LPlatform = TSkProjectPlatform.Unknown then
          Continue;
        LPlatformDeployFiles := GetSkiaDeployFiles(LPlatform);
        LPlatformName := LPlatform.ToString;

        for LFile in LProjectDeployment.FindFiles('', LConfig, LPlatformName, '') do
        begin
          if LFile.DeploymentClass = SkiaDeploymentClass then
            Continue;
          if not IsValidDeployFile(LFile, LPlatformName, LPlatformDeployFiles) then
          begin
            LProjectDeployment.RemoveFile(LConfig, LPlatformName, LFile.LocalName);
            LChanged := True;
          end;
        end;
      end;
    end;
    if LChanged then
      AProject.MarkModified;
  end;
end;

{ TSkProjectHelper }

class function TSkProjectHelper.DefinesName(
  const AProject: IOTAProject): string;
begin
  if AProject.Personality = sDelphiPersonality then
    Result := DCCStrs.sDefine
  else if AProject.Personality = sCBuilderPersonality then
    Result := BCCStrs.sDefines
  else
    Result := '';
end;

class function TSkProjectHelper.GetIsSkiaDefined(
  const AProject: IOTAProject): Boolean;
var
  LBaseConfiguration: IOTABuildConfiguration;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
begin
  Result := Assigned(AProject) and Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations);
  if Result then
  begin
    LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
    Result := Assigned(LBaseConfiguration) and
      TSkOTAHelper.ContainsOptionValue(LBaseConfiguration.Value[DefinesName(AProject)], SkiaProjectDefine);
  end;
end;

class function TSkProjectHelper.IsSkiaDefinedForPlatform(
  const AProject: IOTAProject; const APlatform: TSkProjectPlatform;
  const AConfig: string): Boolean;
var
  LBuildConfig: IOTABuildConfiguration;
begin
  Result := TSkOTAHelper.TryGetBuildConfig(AProject, APlatform, AConfig, LBuildConfig) and
    not TSkOTAHelper.ContainsOptionValue(LBuildConfig.Value[DefinesName(AProject)], SkiaProjectDisabledDefine);
end;

class function TSkProjectHelper.IsSupported(
  const AProject: IOTAProject): Boolean;
begin
  Result := Assigned(AProject) and TSkiaLibrary.IsPersonalitySupported(AProject.Personality) and
    ((AProject.ApplicationType = sApplication) or (AProject.ApplicationType = sConsole) or (AProject.ApplicationType = sLibrary)) and
    TSkiaLibrary.IsSupported;
end;

class function TSkProjectHelper.IsSupported(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform): Boolean;
begin
  Result := IsSupported(AProject) and (APlatform in TSkiaLibrary.SupportedPlatforms(AProject.Personality)) and
    ContainsStringInArray(APlatform.ToString, AProject.SupportedPlatforms, False);
end;

class procedure TSkProjectHelper.SetIsSkiaDefined(const AProject: IOTAProject;
  const AValue: Boolean);

  {$IF CompilerVersion >= 32}
  procedure SetPlatformsOptions;
  var
    LBuildConfig: IOTABuildConfiguration;
    LConfig: string;
    LLibrary: string;
    LPath: string;
    I: Integer;
  begin
    if AProject.Personality <> sCBuilderPersonality then
      Exit;
    for LConfig in TSkOTAHelper.BuildConfigs(AProject) do
    begin
      for I := 0 to Length(SkiaPlatformsOptions) - 1 do
      begin
        if TSkOTAHelper.TryGetBuildConfig(AProject, SkiaPlatformsOptions[I].Platform, LConfig, LBuildConfig) then
        begin
          for LLibrary in SkiaPlatformsOptions[I].CppLinkedLibraries do
          begin
            if AValue then
              LBuildConfig.Value[ILinkStrs.sAdditionalLinkerFiles] := TSkOTAHelper.InsertOptionValue(LBuildConfig.Value[ILinkStrs.sAdditionalLinkerFiles], LLibrary)
            else
              LBuildConfig.Value[ILinkStrs.sAdditionalLinkerFiles] := TSkOTAHelper.RemoveOptionValue(LBuildConfig.Value[ILinkStrs.sAdditionalLinkerFiles], LLibrary);
          end;
          for LPath in SkiaPlatformsOptions[I].CppLibraryPath do
          begin
            if AValue then
              LBuildConfig.Value[ILinkStrs.sLibraryPath] := TSkOTAHelper.InsertOptionValue(LBuildConfig.Value[ILinkStrs.sLibraryPath], LPath)
            else
              LBuildConfig.Value[ILinkStrs.sLibraryPath] := TSkOTAHelper.RemoveOptionValue(LBuildConfig.Value[ILinkStrs.sLibraryPath], LPath);
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}

var
  LBaseConfiguration: IOTABuildConfiguration;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LProjectOptions: IOTAProjectOptions;
begin
  if Assigned(AProject) then
  begin
    LProjectOptions := AProject.ProjectOptions;
    if Assigned(LProjectOptions) then
    begin
      if Supports(LProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations) then
      begin
        LBaseConfiguration := LOptionsConfigurations.BaseConfiguration;
        if Assigned(LBaseConfiguration) then
        begin
          if AValue then
            LBaseConfiguration.Value[DefinesName(AProject)] := TSkOTAHelper.InsertOptionValue(LBaseConfiguration.Value[DefinesName(AProject)], SkiaProjectDefine)
          else
            LBaseConfiguration.Value[DefinesName(AProject)] := TSkOTAHelper.RemoveOptionValue(LBaseConfiguration.Value[DefinesName(AProject)], SkiaProjectDefine);
        end;
        {$IF CompilerVersion >= 32}
        SetPlatformsOptions;
        {$ENDIF}
      end;
      LProjectOptions.ModifiedState := True;
    end;
  end;
end;

{ TSkOTAHelper }

class function TSkOTAHelper.BuildConfigs(
  const AProject: IOTAProject): TArray<string>;
var
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  I: Integer;
begin
  Result := [];
  if Assigned(AProject) and Supports(AProject.ProjectOptions, IOTAProjectOptionsConfigurations, LOptionsConfigurations) then
  begin
    for I := 0 to LOptionsConfigurations.ConfigurationCount - 1 do
    begin
      if (LOptionsConfigurations.BaseConfiguration = nil) or
        (LOptionsConfigurations.Configurations[I].Name <> LOptionsConfigurations.BaseConfiguration.Name) then
      begin
        Result := Result + [LOptionsConfigurations.Configurations[I].Name];
      end;
    end;
  end;
end;

class function TSkOTAHelper.ContainsOptionValue(const AValues, AValue,
  ASeparator: string): Boolean;
var
  LValues: TArray<string>;
  I: Integer;
begin
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  for I := 0 to Length(LValues) - 1 do
    if SameText(LValues[I], AValue) then
      Exit(True);
  Result := False;
end;

class function TSkOTAHelper.ExpandConfiguration(const ASource: string;
  const AConfig: IOTABuildConfiguration): string;
begin
  Result := StringReplace(ASource, '$(Platform)', AConfig.Platform, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Config)', AConfig.Name, [rfReplaceAll, rfIgnoreCase]);
end;

class function TSkOTAHelper.ExpandConfiguration(const ASource: string;
  const APlatform: TSkProjectPlatform; const AConfig: string): string;
begin
  Result := StringReplace(ASource, '$(Platform)', APlatform.ToString, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Config)', AConfig, [rfReplaceAll, rfIgnoreCase]);
end;

class function TSkOTAHelper.ExpandEnvironmentVar(var AValue: string): Boolean;
var
  R: Integer;
  LExpanded: string;
begin
  SetLength(LExpanded, 1);
  R := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), 0);
  SetLength(LExpanded, R);
  Result := ExpandEnvironmentStrings(PChar(AValue), PChar(LExpanded), R) <> 0;
  if Result then
  begin
    StrResetLength(LExpanded);
    AValue := LExpanded;
  end;
end;

class function TSkOTAHelper.ExpandOutputPath(const ASource: string;
  const ABuildConfig: IOTABuildConfiguration): string;
begin
  if Assigned(ABuildConfig) then
    Result := ExpandConfiguration(ASource, ABuildConfig)
  else
    Result := ASource;
  Result := ExpandVars(Result);
end;

class function TSkOTAHelper.ExpandPath(const ABaseDir,
  ARelativeDir: string): string;
var
  LBuffer: array [0..MAX_PATH - 1] of Char;
begin
  if PathIsRelative(PChar(ARelativeDir)) then
    Result := IncludeTrailingPathDelimiter(ABaseDir) + ARelativeDir
  else
    Result := ARelativeDir;
  if PathCanonicalize(@LBuffer[0], PChar(Result)) then
    Result := LBuffer;
end;

class function TSkOTAHelper.ExpandVars(const ASource: string;
  const APlatform: TSkProjectPlatform; const AConfig: string): string;
begin
  Result := ExpandVars(ExpandConfiguration(ASource, APlatform, AConfig));
end;

class function TSkOTAHelper.ExpandVars(const ASource: string): string;
var
  LVars: TStrings;
  I: Integer;
begin
  Result := ASource;
  if not Result.IsEmpty then
  begin
    LVars := TStringList.Create;
    try
      GetEnvironmentVars(LVars, True);
      for I := 0 to LVars.Count - 1 do
        Result := StringReplace(Result, '$(' + LVars.Names[I] + ')', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
      for I := 0 to LVars.Count - 1 do
        Result := StringReplace(Result, '%' + LVars.Names[I] + '%', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
    finally
      LVars.Free;
    end;
  end;
end;

class function TSkOTAHelper.GetEnvironmentVars(const AVars: TStrings;
  AExpand: Boolean): Boolean;
var
  LRaw: PChar;
  LExpanded: string;
  I: Integer;
begin
  AVars.BeginUpdate;
  try
    AVars.Clear;
    LRaw := GetEnvironmentStrings;
    try
      MultiSzToStrings(AVars, LRaw);
      Result := True;
    finally
      FreeEnvironmentStrings(LRaw);
    end;
    if AExpand then
    begin
      for I := 0 to AVars.Count - 1 do
      begin
        LExpanded := AVars[I];
        if ExpandEnvironmentVar(LExpanded) then
          AVars[I] := LExpanded;
      end;
    end;
  finally
    AVars.EndUpdate;
  end;
end;

class function TSkOTAHelper.GetProjectOptionsConfigurations(
  const AProject: IOTAProject): IOTAProjectOptionsConfigurations;
var
  LProjectOptions: IOTAProjectOptions;
begin
  Result := nil;
  if AProject <> nil then
  begin
    LProjectOptions := AProject.ProjectOptions;
    if LProjectOptions <> nil then
      Supports(LProjectOptions, IOTAProjectOptionsConfigurations, Result);
  end;
end;

class function TSkOTAHelper.InsertOptionValue(const AValues, AValue,
  ASeparator: string): string;
var
  LValues: TArray<string>;
  I: Integer;
begin
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  try
    for I := 0 to Length(LValues) - 1 do
    begin
      if SameText(LValues[I], AValue) then
      begin
        LValues[I] := AValue;
        Exit;
      end;
    end;
    LValues := LValues + [AValue];
  finally
    if LValues = nil then
      Result := ''
    else
      Result := string.Join(ASeparator, LValues);
  end;
end;

class procedure TSkOTAHelper.MultiSzToStrings(const ADest: TStrings;
  const ASource: PChar);
var
  P: PChar;
begin
  ADest.BeginUpdate;
  try
    ADest.Clear;
    if ASource <> nil then
    begin
      P := ASource;
      while P^ <> #0 do
      begin
        ADest.Add(P);
        P := StrEnd(P);
        Inc(P);
      end;
    end;
  finally
    ADest.EndUpdate;
  end;
end;

class function TSkOTAHelper.RemoveOptionValue(const AValues, AValue,
  ASeparator: string): string;
var
  LValues: TArray<string>;
  LNewValues: TArray<string>;
  I: Integer;
begin
  LNewValues := [];
  LValues := AValues.Split([ASeparator], TStringSplitOptions.None);
  for I := 0 to Length(LValues) - 1 do
    if not SameText(LValues[I], AValue) then
      LNewValues := LNewValues + [LValues[I]];
  if LNewValues = nil then
    Result := ''
  else
    Result := string.Join(ASeparator, LNewValues);
end;

class procedure TSkOTAHelper.StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

class function TSkOTAHelper.TryCopyFileToOutputPath(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig, AFileName: string): Boolean;
var
  LProjectOutputPath: string;
begin
  Result := False;
  if (APlatform <> TSkProjectPlatform.Unknown) and TFile.Exists(AFileName) and TryGetProjectOutputPath(AProject, APlatform, AConfig, LProjectOutputPath) then
  begin
    try
      if not TDirectory.Exists(LProjectOutputPath) then
        TDirectory.CreateDirectory(LProjectOutputPath);
      TFile.Copy(AFileName, TPath.Combine(LProjectOutputPath, TPath.GetFileName(AFileName)), True);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

class function TSkOTAHelper.TryGetBuildConfig(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: string;
  out ABuildConfig: IOTABuildConfiguration): Boolean;
var
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  I: Integer;
begin
  Result := False;
  ABuildConfig := nil;
  if APlatform <> TSkProjectPlatform.Unknown then
  begin
    LOptionsConfigurations := GetProjectOptionsConfigurations(AProject);
    if Assigned(LOptionsConfigurations) then
    begin
      for I := LOptionsConfigurations.ConfigurationCount - 1 downto 0 do
      begin
        ABuildConfig := LOptionsConfigurations.Configurations[I];
        if ABuildConfig.Name = AConfig then
        begin
          ABuildConfig := ABuildConfig.PlatformConfiguration[APlatform.ToString];
          Exit(Assigned(ABuildConfig));
        end;
      end;
    end;
  end;
end;

class function TSkOTAHelper.TryGetProjectOutputPath(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: string;
  out AOutputPath: string): Boolean;
var
  LBuildConfig: IOTABuildConfiguration;
begin
  Result := (APlatform <> TSkProjectPlatform.Unknown) and
    TryGetBuildConfig(AProject, APlatform, AConfig, LBuildConfig) and
    TryGetProjectOutputPath(AProject, LBuildConfig, AOutputPath) and
    TPath.HasValidPathChars(AOutputPath, False);
  if not Result then
    AOutputPath := '';
end;

class function TSkOTAHelper.TryGetProjectOutputPath(const AProject: IOTAProject;
  ABuildConfig: IOTABuildConfiguration; out AOutputPath: string): Boolean;
var
  LOptions: IOTAProjectOptions;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LRelativeOutputPath: string;
begin
  Result := False;
  try
    if Assigned(AProject) then
    begin
      AOutputPath := TPath.GetDirectoryName(AProject.FileName);
      LOptions := AProject.ProjectOptions;
      if LOptions <> nil then
      begin
        if not Assigned(ABuildConfig) then
        begin
          LOptionsConfigurations := GetProjectOptionsConfigurations(AProject);
          if Assigned(LOptionsConfigurations) then
            ABuildConfig := LOptionsConfigurations.ActiveConfiguration;
        end;

        if Assigned(ABuildConfig) then
        begin
          if AProject.Personality = sCBuilderPersonality then
            LRelativeOutputPath := LOptions.Values[FinalOutputDirPropertyName]
          else
            LRelativeOutputPath := LOptions.Values[OutputDirPropertyName];
          AOutputPath := ExpandOutputPath(ExpandPath(AOutputPath, LRelativeOutputPath), ABuildConfig);
          Result := True;
        end
        else
          Result := False;
      end
      else
        Result := True;
    end;
  finally
    if not Result then
      AOutputPath := '';
  end;
end;

class function TSkOTAHelper.TryRemoveOutputFile(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: string;
  AFileName: string): Boolean;
var
  LProjectOutputPath: string;
begin
  Result := False;
  if (APlatform <> TSkProjectPlatform.Unknown) and
    TSkOTAHelper.TryGetProjectOutputPath(AProject, APlatform, AConfig, LProjectOutputPath) then
  begin
    AFileName := TPath.Combine(LProjectOutputPath, AFileName);
    if TFile.Exists(AFileName) then
    begin
      try
        TFile.Delete(AFileName);
        Result := True;
      except
        Result := False;
      end;
    end;
  end;
end;

{ TSkiaLibrary }

class function TSkiaLibrary.IsPersonalitySupported(
  const APersonality: string): Boolean;
begin
  Result := (APersonality = sDelphiPersonality)
    {$IFDEF SKIAEMB}or (APersonality = sCBuilderPersonality){$ENDIF};
end;

class function TSkiaLibrary.IsSupported: Boolean;
begin
  if not FBinariesChecked then
  begin
    FBinariesFound := TSkDeployFilesHelper.LocalFilesExists;
    FBinariesChecked := True;
  end;
  Result := FBinariesFound;
end;

class function TSkiaLibrary.SupportedPlatforms(
  const APersonality: string): TSkProjectPlatforms;
begin
  if not IsPersonalitySupported(APersonality) then
    Exit([]);
  if APersonality = sDelphiPersonality then
    Result := DelphiSupportedPlatforms
  else if APersonality = sCBuilderPersonality then
    Result := CBuilderSupportedPlatforms
  else
    Result := [];
end;

{ Register }

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  TSkProjectMenuCreatorNotifier.Register;
  TSkCompileNotifier.Register;
end;

end.
