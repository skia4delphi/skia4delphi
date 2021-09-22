unit Skia.Vcl.Designtime.ProjectMenu;

interface

{$SCOPEDENUMS ON}

procedure Register;

implementation

uses
  { Delphi }
  Winapi.Windows,
  Winapi.ShLwApi,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.TypInfo,
  System.Generics.Collections,
  Vcl.ActnList,
  Vcl.Dialogs,
  ToolsAPI,
  DeploymentAPI,
  DesignIntf,
  DCCStrs,

  { Skia }
  Skia;

type
  TSkProjectConfig = (Release, Debug);
  TSkProjectPlatform = (Unknown, Win32, Win64, Android, Android64, iOSDevice32, iOSDevice64, iOSSimulator, OSX64, OSXARM64, Linux64);

  { TSkProjectConfigHelper }

  TSkProjectConfigHelper = record helper for TSkProjectConfig
    function ToString: string;
    class function FromString(const AText: string): TSkProjectConfig; static;
  end;

  { TSkProjectPlatformHelper }

  TSkProjectPlatformHelper = record helper for TSkProjectPlatform
    function ToString: string;
    class function FromString(const AText: string): TSkProjectPlatform; static;
  end;

  { TSkProjectMenuCreatorNotifier }

  TSkProjectMenuCreatorNotifier = class(TNotifierObject, IOTANotifier, IOTAProjectMenuItemCreatorNotifier)
  strict private
    class var FNotifierIndex: Integer;
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
    function GetIsMultiSelectable: Boolean;
    procedure Execute(const AMenuContextList: IInterfaceList); overload;
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
    procedure SetDeployFiles(const AProject: IOTAProject; const AConfig: TSkProjectConfig; const APlatform: TSkProjectPlatform; const AEnabled: Boolean);
    procedure SetSkiaEnabled(const AProject: IOTAProject; const AEnabled: Boolean);
  strict protected
    function GetEnabled: Boolean; override;
  public
    constructor Create(const AProject: IOTAProject; const APosition: Integer); reintroduce;
  end;

  { TSkCompileNotifier }

  TSkCompileNotifier = class(TInterfacedObject, IOTACompileNotifier)
  strict private
    const
      UnsupportedPlatformMessage =
        'The Skia does not support the platform %s in this RAD Studio version.' + sLineBreak + sLineBreak +
        'To avoid problems, disable Skia in this project (Project menu > %s) or, if you want to disable it just in ' +
        'a specific platform, set the define directive "%s" in the project settings of this platform. In both cases, ' +
        'be sure you are not using any Skia units, otherwise you will get "runtime error" on startup of your application.';
    class var FNotifierIndex: Integer;
    class constructor Create;
    class destructor Destroy;
    { IOTACompileNotifier }
    procedure ProjectCompileStarted(const AProject: IOTAProject; AMode: TOTACompileMode);
    procedure ProjectCompileFinished(const AProject: IOTAProject; AResult: TOTACompileResult);
    procedure ProjectGroupCompileStarted(AMode: TOTACompileMode);
    procedure ProjectGroupCompileFinished(AResult: TOTACompileResult);
  public
    class procedure Register; static;
  end;

  { TSkProjectHelper }

  TSkProjectHelper = record
  strict private
    class function GetIsSkiaDefined(const AProject: IOTAProject): Boolean; static;
    class procedure SetIsSkiaDefined(const AProject: IOTAProject; const AValue: Boolean); static;
  public
    class procedure AddDeployFile(const AProject: IOTAProject; const AConfig: TSkProjectConfig; const APlatform: TSkProjectPlatform; const ALocalFileName, ARemoteDir: string); static;
    class procedure RemoveDeployFile(const AProject: IOTAProject; const AConfig: TSkProjectConfig; const APlatform: TSkProjectPlatform; ALocalFileName: string; const ARemoteDir: string); static;
    class function IsSkiaDefinedForPlatform(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig): Boolean; static;
    class property IsSkiaDefined[const AProject: IOTAProject]: Boolean read GetIsSkiaDefined write SetIsSkiaDefined;
  end;

  { TSkOTAHelper }

  TSkOTAHelper = record
  strict private
    const
      DefaultOptionsSeparator = ';';
      OutputDirPropertyName = 'OutputDir';
    class function ExpandConfiguration(const ASource: string; const AConfig: IOTABuildConfiguration): string; static;
    class function ExpandEnvironmentVar(var AValue: string): Boolean; static;
    class function ExpandOutputPath(const ASource: string; const ABuildConfig: IOTABuildConfiguration): string; static;
    class function ExpandPath(const ABaseDir, ARelativeDir: string): string; static;
    class function ExpandVars(const ASource: string): string; static;
    class function GetEnvironmentVars(const AVars: TStrings; AExpand: Boolean): Boolean; static;
    class function GetProjectOptionsConfigurations(const AProject: IOTAProject): IOTAProjectOptionsConfigurations; static;
    class procedure MultiSzToStrings(const ADest: TStrings; const ASource: PChar); static;
    class procedure StrResetLength(var S: string); static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; ABuildConfig: IOTABuildConfiguration; out AOutputPath: string): Boolean; overload; static;
  public
    class function ContainsOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): Boolean; static;
    class function GetEnvironmentVar(const AName: string; AExpand: Boolean): string; static;
    class function InsertOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;
    class function RemoveOptionValue(const AValues, AValue: string; const ASeparator: string = DefaultOptionsSeparator): string; static;
    class function TryCopyFileToOutputPath(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig; const AFileName: string): Boolean; static;
    class function TryCopyFileToOutputPathOfActiveBuild(const AProject: IOTAProject; const AFileName: string): Boolean; static;
    class function TryGetBuildConfig(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig; out ABuildConfig: IOTABuildConfiguration): Boolean; static;
    class function TryGetProjectOutputPath(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig; out AOutputPath: string): Boolean; overload; static;
    class function TryGetProjectOutputPathOfActiveBuild(const AProject: IOTAProject; out AOutputPath: string): Boolean; static;
    class function TryRemoveOutputFile(const AProject: IOTAProject; const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig; AFileName: string): Boolean; static;
    class function TryRemoveOutputFileOfActiveBuild(const AProject: IOTAProject; const AFileName: string): Boolean; static;
  end;

  { TSkia4DelphiProject }

  TSkia4DelphiProject = class
  strict private
    type
      TDeployFile = record
        LocalFileName: string;
        RemotePath: string;
        CopyToOutput: Boolean;
      end;
    class var
      FAbsolutePath: string;
      FPath: string;
      FPathChecked: Boolean;
    class procedure FindPath(out APath, AAbsolutePath: string); static;
    class function GetAbsolutePath: string; static;
    class function GetFound: Boolean; static;
    class function GetPath: string; static;
    class function IsValidSkiaDir(const APath: string): Boolean; static;
  public
    const
      DeploymentClass       = 'Skia';
      ProjectDefine         = 'SKIA';
      ProjectDisabledDefine = 'SKIA_DISABLED';
      SkiaDirVariable       = 'SKIADIR';
      DeployFile: array[TSkProjectPlatform] of TDeployFile = (
        (LocalFileName: '';                                  RemotePath: '';                         CopyToOutput: False), // Unknown
        (LocalFileName: 'Binary\Shared\Win32\sk4d.dll';      RemotePath: '.\';                       CopyToOutput: True),  // Win32
        (LocalFileName: 'Binary\Shared\Win64\sk4d.dll';      RemotePath: '.\';                       CopyToOutput: True),  // Win64
        (LocalFileName: 'Binary\Shared\Android\sk4d.so';     RemotePath: 'library\lib\armeabi-v7a\'; CopyToOutput: False), // Android
        (LocalFileName: 'Binary\Shared\Android64\sk4d.so';   RemotePath: 'library\lib\arm64-v8a\';   CopyToOutput: False), // Android64
        (LocalFileName: '';                                  RemotePath: '';                         CopyToOutput: False), // iOSDevice32
        (LocalFileName: '';                                  RemotePath: '';                         CopyToOutput: False), // iOSDevice64
        (LocalFileName: '';                                  RemotePath: '';                         CopyToOutput: False), // iOSSimulator
        (LocalFileName: 'Binary\Shared\OSX64\sk4d.dylib';    RemotePath: 'Contents\MacOS\';          CopyToOutput: False), // OSX64
        (LocalFileName: 'Binary\Shared\OSXARM64\sk4d.dylib'; RemotePath: 'Contents\MacOS\';          CopyToOutput: False), // OSXARM64
        (LocalFileName: 'Binary\Shared\Linux64\sk4d.so';     RemotePath: '.\';                       CopyToOutput: False)  // Linux64
      );
      MenuCaption: array[Boolean] of string = ('Enable Skia', 'Disable Skia');
      {$IF CompilerVersion < 34.0}
      SupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64];
      {$ELSE}
      SupportedPlatforms = [TSkProjectPlatform.Win32, TSkProjectPlatform.Win64, TSkProjectPlatform.Android,
        TSkProjectPlatform.Android64, TSkProjectPlatform.iOSDevice64, TSkProjectPlatform.OSX64,
        TSkProjectPlatform.OSXARM64, TSkProjectPlatform.Linux64];
      {$ENDIF}
    class property AbsolutePath: string read GetAbsolutePath;
    class property Found: Boolean read GetFound;
    class property Path: string read GetPath;
  end;

const
  InvalidNotifier = -1;

{ TSkProjectHelper }

class procedure TSkProjectHelper.AddDeployFile(const AProject: IOTAProject;
  const AConfig: TSkProjectConfig; const APlatform: TSkProjectPlatform;
  const ALocalFileName, ARemoteDir: string);
type
  TDeployFileExistence = (DoesNotExist, AlreadyExists, NeedReplaced);

  function GetDeployFileExistence(const AProjectDeployment: IProjectDeployment;
    const ALocalFileName, ARemoteDir, APlatformName, AConfigName: string): TDeployFileExistence;
  var
    LRemoteFileName: string;
    LFile: IProjectDeploymentFile;
    LFiles: TDictionary<string, IProjectDeploymentFile>.TValueCollection;
  begin
    Result := TDeployFileExistence.DoesNotExist;
    LRemoteFileName := TPath.Combine(ARemoteDir, TPath.GetFileName(ALocalFileName));
    LFiles := AProjectDeployment.Files;
    if Assigned(LFiles) then
    begin
      for LFile in LFiles do
      begin
        if (LFile.FilePlatform = APlatformName) and (LFile.Configuration = AConfigName) then
        begin
          if SameText(LRemoteFileName, TPath.Combine(LFile.RemoteDir[APlatformName], LFile.RemoteName[APlatformName])) then
          begin
            if (LFile.LocalName = ALocalFileName) and (LFile.DeploymentClass = TSkia4DelphiProject.DeploymentClass) and
              (Result = TDeployFileExistence.DoesNotExist) then
            begin
              Result := TDeployFileExistence.AlreadyExists
            end
            else
              Exit(TDeployFileExistence.NeedReplaced);
          end;
        end;
      end;
    end;
  end;

  procedure DoAddDeployFile(const AProjectDeployment: IProjectDeployment;
    const ALocalFileName, ARemoteDir, APlatformName, AConfigName: string);
  var
    LFile: IProjectDeploymentFile;
  begin
    {$IF CompilerVersion >= 29}
    LFile := AProjectDeployment.CreateFile(AConfigName, APlatformName, ALocalFileName);
    {$ELSE}
    LFile := AProjectDeployment.CreateFile(APlatformName, ALocalFileName);
    {$ENDIF}
    if Assigned(LFile) then
    begin
      LFile.Overwrite[APlatformName] := True;
      LFile.Enabled[APlatformName] := True;
      LFile.RemoteDir[APlatformName] := ARemoteDir;
      LFile.DeploymentClass := TSkia4DelphiProject.DeploymentClass;
      LFile.RemoteName[APlatformName] := TPath.GetFileName(ALocalFileName);
      {$IF CompilerVersion >= 29}
      AProjectDeployment.AddFile(AConfigName, APlatformName, LFile);
      {$ELSE}
      AProjectDeployment.AddFile(APlatformName, LFile);
      {$ENDIF}
    end;
  end;

var
  LProjectDeployment: IProjectDeployment;
  LConfigName: string;
  LPlatformName: string;
  LLocalFileName: string;
  LDeployFileExistence: TDeployFileExistence;
begin
  if (ALocalFileName <> '') and Supports(AProject, IProjectDeployment, LProjectDeployment)  then
  begin
    LConfigName := AConfig.ToString;
    LPlatformName := APlatform.ToString;
    LLocalFileName := TPath.Combine(TSkia4DelphiProject.Path, ALocalFileName);
    LDeployFileExistence := GetDeployFileExistence(LProjectDeployment, LLocalFileName, ARemoteDir, LPlatformName, LConfigName);
    if LDeployFileExistence = TDeployFileExistence.NeedReplaced then
      RemoveDeployFile(AProject, AConfig, APlatform, ALocalFileName, ARemoteDir);
    if LDeployFileExistence in [TDeployFileExistence.NeedReplaced, TDeployFileExistence.DoesNotExist] then
      DoAddDeployFile(LProjectDeployment, LLocalFileName, ARemoteDir, LPlatformName, LConfigName);
  end;
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
      TSkOTAHelper.ContainsOptionValue(LBaseConfiguration.Value[sDefine], TSkia4DelphiProject.ProjectDefine);
  end;
end;

class function TSkProjectHelper.IsSkiaDefinedForPlatform(
  const AProject: IOTAProject; const APlatform: TSkProjectPlatform;
  const AConfig: TSkProjectConfig): Boolean;
var
  LBuildConfig: IOTABuildConfiguration;
begin
  Assert(IsSkiaDefined[AProject]);
  Result := TSkOTAHelper.TryGetBuildConfig(AProject, APlatform, AConfig, LBuildConfig) and
    not TSkOTAHelper.ContainsOptionValue(LBuildConfig.Value[sDefine], TSkia4DelphiProject.ProjectDisabledDefine);
end;

class procedure TSkProjectHelper.RemoveDeployFile(const AProject: IOTAProject;
  const AConfig: TSkProjectConfig; const APlatform: TSkProjectPlatform;
  ALocalFileName: string; const ARemoteDir: string);
var
  LProjectDeployment: IProjectDeployment;
  LFiles: TDictionary<string, IProjectDeploymentFile>.TValueCollection;
  LFile: IProjectDeploymentFile;
  LRemoteFileName: string;
  LRemoveFiles: TArray<IProjectDeploymentFile>;
begin
  if (ALocalFileName <> '') and Supports(AProject, IProjectDeployment, LProjectDeployment) then
  begin
    ALocalFileName := TPath.Combine(TSkia4DelphiProject.Path, ALocalFileName);
    {$IF CompilerVersion >= 29}
    LProjectDeployment.RemoveFile(AConfig.ToString, APlatform.ToString, ALocalFileName);
    {$ELSE}
    LProjectDeployment.RemoveFile(APlatform.ToString, ALocalFileName);
    {$ENDIF}
    LFiles := LProjectDeployment.Files;
    if Assigned(LFiles) then
    begin
      LRemoteFileName := TPath.Combine(ARemoteDir, TPath.GetFileName(ALocalFileName));
      LRemoveFiles := nil;
      for LFile in LFiles do
      begin
        if SameText(LRemoteFileName, TPath.Combine(LFile.RemoteDir[APlatform.ToString], LFile.RemoteName[APlatform.ToString])) then
        begin
          SetLength(LRemoveFiles, Length(LRemoveFiles) + 1);
          LRemoveFiles[Length(LRemoveFiles) - 1] := LFile;
        end;
      end;
      for LFile in LRemoveFiles do
      begin
        {$IF CompilerVersion >= 29}
        LProjectDeployment.RemoveFile(AConfig.ToString, APlatform.ToString, LFile.LocalName);
        {$ELSE}
        LProjectDeployment.RemoveFile(APlatform.ToString, LFile.LocalName);
        {$ENDIF}
      end;
    end;
  end;
end;

class procedure TSkProjectHelper.SetIsSkiaDefined(const AProject: IOTAProject;
  const AValue: Boolean);
var
  LProjectOptions: IOTAProjectOptions;
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LBaseConfiguration: IOTABuildConfiguration;
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
            LBaseConfiguration.Value[sDefine] := TSkOTAHelper.InsertOptionValue(LBaseConfiguration.Value[sDefine], TSkia4DelphiProject.ProjectDefine)
          else
            LBaseConfiguration.Value[sDefine] := TSkOTAHelper.RemoveOptionValue(LBaseConfiguration.Value[sDefine], TSkia4DelphiProject.ProjectDefine);
        end;
      end;
      LProjectOptions.ModifiedState := True;
    end;
  end;
end;

{ TSkProjectConfigHelper }

function TSkProjectConfigHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TSkProjectConfig), Ord(Self));
end;

class function TSkProjectConfigHelper.FromString(const AText: string): TSkProjectConfig;
begin
  Result := TSkProjectConfig(GetEnumValue(TypeInfo(TSkProjectConfig), AText));
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

  function CanHandle(const AProject: IOTAProject): Boolean;
  begin
    Result := Assigned(AProject) and AProject.FileName.EndsWith('.dproj', True) and
      ((AProject.ApplicationType = sApplication) or (AProject.ApplicationType = sConsole));
  end;

begin
  if (not AIsMultiSelect) and (AIdentList.IndexOf(sProjectContainer) <> -1) and
    Assigned(AProjectManagerMenuList) and CanHandle(AProject) then
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
    FNotifierIndex := LProjectManager.AddMenuItemCreatorNotifier(TSkProjectMenuCreatorNotifier.Create);;
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
  Result := True; // for Show IPA, check platform etc
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
  inherited Create(TSkia4DelphiProject.MenuCaption[FIsSkiaEnabled], '', APosition,
    procedure()
    begin
      SetSkiaEnabled(AProject, not FIsSkiaEnabled);
    end);
end;

function TSkProjectManagerMenuEnableSkia.GetEnabled: Boolean;
begin
  Result := FIsSkiaEnabled or TSkia4DelphiProject.Found;
end;

procedure TSkProjectManagerMenuEnableSkia.SetDeployFiles(
  const AProject: IOTAProject; const AConfig: TSkProjectConfig;
  const APlatform: TSkProjectPlatform; const AEnabled: Boolean);
begin
  if AEnabled and (APlatform in TSkia4DelphiProject.SupportedPlatforms) then
  begin
    TSkProjectHelper.AddDeployFile(AProject, AConfig, APlatform,
      TSkia4DelphiProject.DeployFile[APlatform].LocalFileName, TSkia4DelphiProject.DeployFile[APlatform].RemotePath);
  end
  else
  begin
    TSkProjectHelper.RemoveDeployFile(AProject, AConfig, APlatform,
      TSkia4DelphiProject.DeployFile[APlatform].LocalFileName, TSkia4DelphiProject.DeployFile[APlatform].RemotePath);
    if TSkia4DelphiProject.DeployFile[APlatform].CopyToOutput then
      TSkOTAHelper.TryRemoveOutputFile(AProject, APlatform, AConfig, TPath.GetFileName(TSkia4DelphiProject.DeployFile[APlatform].LocalFileName))
  end;
end;

procedure TSkProjectManagerMenuEnableSkia.SetSkiaEnabled(
  const AProject: IOTAProject; const AEnabled: Boolean);

  function SupportsPlatform(const APlatform: TSkProjectPlatform): Boolean;
  var
    LPlatformName: string;
    LSupportedPlatform: string;
  begin
    if APlatform <> TSkProjectPlatform.Unknown then
    begin
      LPlatformName := APlatform.ToString;
      for LSupportedPlatform in AProject.SupportedPlatforms do
        if SameText(LPlatformName, LSupportedPlatform) then
          Exit(True);
    end;
    Result := False;
  end;

var
  LPlatform: TSkProjectPlatform;
  LConfig: TSkProjectConfig;
  LProjectOptions: IOTAProjectOptions;
begin
  for LPlatform := Low(TSkProjectPlatform) to High(TSkProjectPlatform) do
    if SupportsPlatform(LPlatform) then
      for LConfig := Low(TSkProjectConfig) to High(TSkProjectConfig) do
        SetDeployFiles(AProject, LConfig, LPlatform, AEnabled);
  TSkProjectHelper.IsSkiaDefined[AProject] := AEnabled;
  LProjectOptions := AProject.ProjectOptions;
  if Assigned(LProjectOptions) then
    LProjectOptions.ModifiedState := True;
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
  LPlatform: TSkProjectPlatform;
begin
  if Assigned(AProject) then
    LPlatform := TSkProjectPlatform.FromString(AProject.CurrentPlatform)
  else
    LPlatform := TSkProjectPlatform.Unknown;
  if LPlatform = TSkProjectPlatform.Unknown then
    Exit;

  if (AMode in [TOTACompileMode.cmOTAMake, TOTACompileMode.cmOTABuild]) and
    TSkProjectHelper.IsSkiaDefined[AProject] and TSkia4DelphiProject.Found then
  begin
    if TSkProjectHelper.IsSkiaDefinedForPlatform(AProject, LPlatform, TSkProjectConfig.FromString(AProject.CurrentConfiguration)) then
    begin
      if LPlatform in TSkia4DelphiProject.SupportedPlatforms then
      begin
        if TSkia4DelphiProject.DeployFile[LPlatform].CopyToOutput then
        begin
          Assert(TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName <> '');
          TSkOTAHelper.TryCopyFileToOutputPathOfActiveBuild(AProject, TPath.Combine(TSkia4DelphiProject.AbsolutePath, TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName));
        end;
        TSkProjectHelper.AddDeployFile(AProject, TSkProjectConfig.FromString(AProject.CurrentConfiguration),
          LPlatform, TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName,
          TSkia4DelphiProject.DeployFile[LPlatform].RemotePath);
      end
      else
      begin
        TSkProjectHelper.RemoveDeployFile(AProject, TSkProjectConfig.FromString(AProject.CurrentConfiguration),
          LPlatform, TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName,
          TSkia4DelphiProject.DeployFile[LPlatform].RemotePath);
        Showmessage(Format(UnsupportedPlatformMessage, [AProject.CurrentPlatform, TSkia4DelphiProject.MenuCaption[True],
          TSkia4DelphiProject.ProjectDisabledDefine]));
      end;
    end
    else
    begin
      TSkProjectHelper.RemoveDeployFile(AProject, TSkProjectConfig.FromString(AProject.CurrentConfiguration),
        LPlatform, TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName,
        TSkia4DelphiProject.DeployFile[LPlatform].RemotePath);
    end;
  end
  {$IF CompilerVersion >= 35.0}
  else if (AMode = TOTACompileMode.cmOTAClean) and TSkProjectHelper.IsSkiaDefined[AProject] then
  begin
    if TSkia4DelphiProject.DeployFile[LPlatform].CopyToOutput then
      TSkOTAHelper.TryRemoveOutputFileOfActiveBuild(AProject, TPath.GetFileName(TSkia4DelphiProject.DeployFile[LPlatform].LocalFileName));
  end;
  {$ENDIF}
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

{ TSkOTAHelper }

class function TSkOTAHelper.ContainsOptionValue(const AValues, AValue,
  ASeparator: string): Boolean;
var
  LValues: TArray<string>;
  I: Integer;
begin
  LValues := AValues.Split(TArray<string>.Create(ASeparator), TStringSplitOptions.None);
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
      begin
        Result := StringReplace(Result, '$(' + LVars.Names[I] + ')', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
        Result := StringReplace(Result, '%' + LVars.Names[I] + '%', LVars.Values[LVars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
      end;
    finally
      LVars.Free;
    end;
  end;
end;

class function TSkOTAHelper.GetEnvironmentVar(const AName: string; AExpand: Boolean): string;
const
  BufSize = 1024;
var
  Len: Integer;
  Buffer: array[0..BufSize - 1] of Char;
  LExpanded: string;
begin
  Result := '';
  Len := Winapi.Windows.GetEnvironmentVariable(PChar(AName), @Buffer, BufSize);
  if Len < BufSize then
    SetString(Result, PChar(@Buffer), Len)
  else
  begin
    SetLength(Result, Len - 1);
    Winapi.Windows.GetEnvironmentVariable(PChar(AName), PChar(Result), Len);
  end;
  if AExpand then
  begin
    LExpanded := Result;
    if ExpandEnvironmentVar(LExpanded) then
      Result := LExpanded;
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
  LValues := AValues.Split(TArray<string>.Create(ASeparator), TStringSplitOptions.None);
  try
    for I := 0 to Length(LValues) - 1 do
    begin
      if SameText(LValues[I], AValue) then
      begin
        LValues[I] := AValue;
        Exit;
      end;
    end;
    SetLength(LValues, Length(LValues) + 1);
    LValues[Length(LValues) - 1] := AValue;
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
  LNewValues := nil;
  LValues := AValues.Split(TArray<string>.Create(ASeparator), TStringSplitOptions.None);
  for I := 0 to Length(LValues) - 1 do
  begin
    if not SameText(LValues[I], AValue) then
    begin
      SetLength(LNewValues, Length(LNewValues) + 1);
      LNewValues[Length(LNewValues) - 1] := LValues[I];
    end;
  end;
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
  const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig;
  const AFileName: string): Boolean;
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

class function TSkOTAHelper.TryCopyFileToOutputPathOfActiveBuild(
  const AProject: IOTAProject; const AFileName: string): Boolean;
var
  LPlatform: TSkProjectPlatform;
  LConfig: TSkProjectConfig;
begin
  LPlatform := TSkProjectPlatform.Unknown;
  LConfig := TSkProjectConfig.Release;
  if Assigned(AProject) then
  begin
    LPlatform := TSkProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TSkProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryCopyFileToOutputPath(AProject, LPlatform, LConfig, AFileName);
end;

class function TSkOTAHelper.TryGetBuildConfig(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig;
  out ABuildConfig: IOTABuildConfiguration): Boolean;
var
  LOptionsConfigurations: IOTAProjectOptionsConfigurations;
  LConfigName: string;
  I: Integer;
begin
  Result := False;
  ABuildConfig := nil;
  if APlatform <> TSkProjectPlatform.Unknown then
  begin
    LOptionsConfigurations := GetProjectOptionsConfigurations(AProject);
    if Assigned(LOptionsConfigurations) then
    begin
      LConfigName := AConfig.ToString;
      for I := LOptionsConfigurations.ConfigurationCount - 1 downto 0 do
      begin
        ABuildConfig := LOptionsConfigurations.Configurations[I];
        if ContainsOptionValue(ABuildConfig.Value[sDefine], LConfigName) then
        begin
          ABuildConfig := ABuildConfig.PlatformConfiguration[APlatform.ToString];
          Exit(Assigned(ABuildConfig));
        end;
      end;
    end;
  end;
end;

class function TSkOTAHelper.TryGetProjectOutputPath(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig;
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

class function TSkOTAHelper.TryGetProjectOutputPathOfActiveBuild(
  const AProject: IOTAProject; out AOutputPath: string): Boolean;
var
  LPlatform: TSkProjectPlatform;
  LConfig: TSkProjectConfig;
begin
  LPlatform := TSkProjectPlatform.Unknown;
  LConfig := TSkProjectConfig.Release;
  if Assigned(AProject) then
  begin
    LPlatform := TSkProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TSkProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryGetProjectOutputPath(AProject, LPlatform, LConfig, AOutputPath);
end;

class function TSkOTAHelper.TryRemoveOutputFile(const AProject: IOTAProject;
  const APlatform: TSkProjectPlatform; const AConfig: TSkProjectConfig;
  AFileName: string): Boolean;
var
  LProjectOutputPath: string;
begin
  Result := False;
  if (APlatform <> TSkProjectPlatform.Unknown) and TSkOTAHelper.TryGetProjectOutputPathOfActiveBuild(AProject, LProjectOutputPath) then
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

class function TSkOTAHelper.TryRemoveOutputFileOfActiveBuild(
  const AProject: IOTAProject; const AFileName: string): Boolean;
var
  LPlatform: TSkProjectPlatform;
  LConfig: TSkProjectConfig;
begin
  LPlatform := TSkProjectPlatform.Unknown;
  LConfig := TSkProjectConfig.Release;
  if Assigned(AProject) then
  begin
    LPlatform := TSkProjectPlatform.FromString(AProject.CurrentPlatform);
    LConfig := TSkProjectConfig.FromString(AProject.CurrentConfiguration);
  end;
  Result := TryRemoveOutputFile(AProject, LPlatform, LConfig, AFileName);
end;

{ TSkia4DelphiProject }

class procedure TSkia4DelphiProject.FindPath(out APath, AAbsolutePath: string);
begin
  AAbsolutePath := TSkOTAHelper.GetEnvironmentVar(SkiaDirVariable, True);
  if IsValidSkiaDir(AAbsolutePath) then
    APath := '$(' + SkiaDirVariable + ')'
  else
  begin
    APath := '';
    AAbsolutePath := '';
  end;
end;

class function TSkia4DelphiProject.GetAbsolutePath: string;
begin
  if not FPathChecked then
    GetPath;
  Result := FAbsolutePath;
end;

class function TSkia4DelphiProject.GetFound: Boolean;
begin
  Result := not Path.IsEmpty;
end;

class function TSkia4DelphiProject.GetPath: string;
begin
  if not FPathChecked then
  begin
    FindPath(FPath, FAbsolutePath);
    FPathChecked := True;
  end;
  Result := FPath;
end;

class function TSkia4DelphiProject.IsValidSkiaDir(const APath: string): Boolean;
begin
  Result := TDirectory.Exists(APath);
end;

{ Register }

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  TSkProjectMenuCreatorNotifier.Register;
  TSkCompileNotifier.Register;
end;

end.
