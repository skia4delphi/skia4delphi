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
unit Skia.Tests.Foundation.Runner;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  DUnitX.TestFramework, DUnitX.Extensibility;

type
  { IAsyncTestRunnerLogger }

  IAsyncTestRunnerLogger = interface
    ['{C0544B4A-7928-49CD-BA56-D8F863193DCE}']
    procedure OnEndTest(const AThreadId: TThreadID; const ATest: ITestResult);
    procedure OnEndTestFixture(const AThreadId: TThreadID; const AResults: IFixtureResult);
    procedure OnLog(const ALogType: TLogLevel; const AMessage: string);
    procedure OnTestingEnds(const ARunResults: IRunResults);
    procedure OnTestingStarts(const AThreadId: TThreadID; ATestCount, ATestActiveCount: Cardinal);
  end;

  { IAsyncTestRunner }

  IAsyncTestRunner = interface
    ['{0399B7E2-A927-4312-9440-BCEB4AC6BECD}']
    procedure AddLogger(const ALogger: IAsyncTestRunnerLogger);
    function GetExpectedImageFileName(const ATestResult: ITestResult): string;
    function GetFixtureList: ITestFixtureList;
    function GetGenerateExpectedImages: Boolean;
    function GetWrongImageFileName(const ATestResult: ITestResult): string;
    procedure Execute;
    procedure RemoveLogger(const ALogger: IAsyncTestRunnerLogger);
    procedure SetGenerateExpectedImages(const AValue: Boolean);
    procedure Wait;
    property FixtureList: ITestFixtureList read GetFixtureList;
    property GenerateExpectedImages: Boolean read GetGenerateExpectedImages write SetGenerateExpectedImages;
  end;

var
  FAsyncTestRunner: IAsyncTestRunner;

implementation

uses
  { Delphi }
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  System.SyncObjs,
  System.IOUtils,
  {$IF CompilerVersion >= 29}
  System.Hash,
  {$ELSE}
  IdHashMessageDigest,
  {$ENDIF}

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TAsyncTestRunner }

  TAsyncTestRunner = class(TInterfacedObject, ITestLogger, IAsyncTestRunner)
  private const
    ImagesExtension = '.png';
  private class var
    FWrongImagesPath: string;
    FWrongImagesPathWasCreated: Boolean;
    class destructor Destroy;
    class function ExpectedImagesPath: string; static;
    class function WrongImagesPath: string; static;
  private
    FCanceled: Boolean;
    FEvent: TEvent;
    FExecuting: Boolean;
    FExecuteFinishedEvent: TEvent;
    FGenerateExpectedImages: Boolean;
    FLastImageChecking: ISkImage;
    FLock: TCriticalSection;
    FLoggers: TList<IAsyncTestRunnerLogger>;
    FTask: ITask;
    FTestRunner: ITestRunner;
    function GetExpectedImageFileName(const ATestResult: ITestResult): string;
    function GetFixtureList: ITestFixtureList;
    function GetGenerateExpectedImages: Boolean;
    function GetLoggers: TArray<IAsyncTestRunnerLogger>;
    function GetWrongImageFileName(const ATestResult: ITestResult): string;
    property Loggers: TArray<IAsyncTestRunnerLogger> read GetLoggers;
    procedure SetGenerateExpectedImages(const AValue: Boolean);
  protected
    { ITestLogger }
    procedure OnBeginTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnEndSetupFixture(const AThreadId: TThreadID; const AFixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnEndTearDownFixture(const AThreadId: TThreadID; const AFixture: ITestFixtureInfo);
    procedure OnEndTearDownTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnEndTest(const AThreadId: TThreadID; const ATest: ITestResult);
    procedure OnEndTestFixture(const AThreadId: TThreadID; const AResults: IFixtureResult);
    procedure OnExecuteTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnImageChecking(const AImage: ISkImage);
    procedure OnLog(const ALogType: TLogLevel; const AMessage: string);
    procedure OnSetupFixture(const AThreadId: TThreadID; const AFixture: ITestFixtureInfo);
    procedure OnSetupTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnStartTestFixture(const AThreadId: TThreadID; const AFixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const AThreadId: TThreadID; const AFixture: ITestFixtureInfo);
    procedure OnTearDownTest(const AThreadId: TThreadID; const ATest: ITestInfo);
    procedure OnTestError(const AThreadId: TThreadID; const AError: ITestError);
    procedure OnTestFailure(const AThreadId: TThreadID; const AFailure: ITestError);
    procedure OnTestIgnored(const AThreadId: TThreadID; const AIgnored: ITestResult);
    procedure OnTestingEnds(const ARunResults: IRunResults);
    procedure OnTestingStarts(const AThreadId: TThreadID; ATestCount, ATestActiveCount: Cardinal);
    procedure OnTestMemoryLeak(const AThreadId: TThreadID; const AIgnored: ITestResult);
    procedure OnTestSuccess(const AThreadId: TThreadID; const ATest: ITestResult);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddLogger(const ALogger: IAsyncTestRunnerLogger);
    procedure Execute;
    procedure RemoveLogger(const ALogger: IAsyncTestRunnerLogger);
    procedure Wait;
    property FixtureList: ITestFixtureList read GetFixtureList;
    property GenerateExpectedImages: Boolean read GetGenerateExpectedImages write SetGenerateExpectedImages;
  end;

  {$IF CompilerVersion < 32}
  { TSkThreadHelper }

  TSkThreadHelper = class helper for TThread
  public
    class procedure ForceQueue(AThread: TThread; AThreadProc: TThreadProcedure); static;
  end;
  {$ENDIF}

function TestHash(const ATest: ITestInfo): string;
{$IF CompilerVersion >= 29}
begin
  Result := THashMD5.GetHashString(ATest.FullName);
{$ELSE}
var
  LMD5: TIdHashMessageDigest5;
begin
  LMD5 := TIdHashMessageDigest5.Create;
  try
    Result := LMD5.HashStringAsHex(ATest.FullName);
  finally
    LMD5.Free;
  end;
{$ENDIF}
end;

{ TAsyncTestRunner }

procedure TAsyncTestRunner.AddLogger(const ALogger: IAsyncTestRunnerLogger);
begin
  if ALogger = nil then
    Exit;
  FLock.Enter;
  try
    FLoggers.Add(ALogger);
  finally
    FLock.Leave;
  end;
end;

constructor TAsyncTestRunner.Create;
var
  LSelfPointer: Pointer;
begin
  inherited Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FExecuteFinishedEvent := TEvent.Create(nil, True, True, '');
  FLock := TCriticalSection.Create;
  FLoggers := TList<IAsyncTestRunnerLogger>.Create;
  FTestRunner := TDUnitX.CreateRunner;
  FTestRunner.UseRTTI := True;
  FTestRunner.FailsOnNoAsserts := False;
  FTestRunner.AddLogger(Self);
  _Release;
  LSelfPointer := Self;
  FTask := TTask.Run(
    procedure
    var
      LSelf: TAsyncTestRunner absolute LSelfPointer;
    begin
      try
        repeat
          LSelf.FEvent.WaitFor;
          if LSelf.FExecuting and not LSelf.FCanceled then
          begin
            LSelf.FExecuteFinishedEvent.ResetEvent;
            LSelf.FTestRunner.Execute;
            LSelf.FExecuting := False;
            LSelf.FExecuteFinishedEvent.SetEvent;
          end;
        until LSelf.FCanceled;
      finally
        LSelf.FExecuting := False;
        LSelf.FExecuteFinishedEvent.SetEvent;
      end;
    end);
end;

class destructor TAsyncTestRunner.Destroy;
begin
  if FWrongImagesPathWasCreated and TDirectory.Exists(FWrongImagesPath) then
    TDirectory.Delete(FWrongImagesPath);
end;

destructor TAsyncTestRunner.Destroy;
var
  LFileName: string;
begin
  Wait;
  FCanceled := True;
  FEvent.SetEvent;
  FTask.Wait;
  FLoggers.Free;
  FEvent.Free;
  FExecuteFinishedEvent.Free;
  FLock.Free;
  if TDirectory.Exists(WrongImagesPath) then
    for LFileName in TDirectory.GetFiles(WrongImagesPath, '*' + ImagesExtension, TSearchOption.soTopDirectoryOnly) do
      TFile.Delete(LFileName);
  inherited Destroy;
end;

procedure TAsyncTestRunner.Execute;
var
  LSelfWeak: Pointer;
begin
  if FExecuting then
    Exit;

  if FWrongImagesPathWasCreated and TDirectory.Exists(FWrongImagesPath) then
  begin
    TDirectory.Delete(FWrongImagesPath, True);
    FWrongImagesPathWasCreated := False;
  end;

  LSelfWeak := Self;
  Assert.OnImageChecking :=
    procedure(const AImage: ISkImage)
    var
      LSelf: TAsyncTestRunner;
    begin
      LSelf := TAsyncTestRunner(LSelfWeak);
      LSelf.OnImageChecking(AImage);
    end;

  FExecuting := True;
  FEvent.SetEvent;
end;

class function TAsyncTestRunner.ExpectedImagesPath: string;
begin
  Result := TPath.Combine(TTestBase.RootAssetsPath, 'Expected') + PathDelim;
end;

function TAsyncTestRunner.GetExpectedImageFileName(
  const ATestResult: ITestResult): string;
begin
  Result := ExpectedImagesPath + TestHash(ATestResult.Test).ToLower + ImagesExtension;
end;

function TAsyncTestRunner.GetFixtureList: ITestFixtureList;
begin
  Supports(FTestRunner.BuildFixtures, ITestFixtureList, Result);
end;

function TAsyncTestRunner.GetGenerateExpectedImages: Boolean;
begin
  Result := FGenerateExpectedImages;
end;

function TAsyncTestRunner.GetLoggers: TArray<IAsyncTestRunnerLogger>;
begin
  FLock.Enter;
  try
    Result := FLoggers.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TAsyncTestRunner.GetWrongImageFileName(
  const ATestResult: ITestResult): string;
begin
  Result := WrongImagesPath + TestHash(ATestResult.Test).ToLower + ImagesExtension;
end;

procedure TAsyncTestRunner.OnBeginTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnEndSetupFixture(const AThreadId: TThreadID;
  const AFixture: ITestFixtureInfo);
begin
end;

procedure TAsyncTestRunner.OnEndSetupTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnEndTearDownFixture(const AThreadId: TThreadID;
  const AFixture: ITestFixtureInfo);
begin
end;

procedure TAsyncTestRunner.OnEndTearDownTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnEndTest(const AThreadId: TThreadID;
  const ATest: ITestResult);
begin
  if FGenerateExpectedImages and TFile.Exists(GetExpectedImageFileName(ATest)) then
    TFile.Delete(GetExpectedImageFileName(ATest));
  if FLastImageChecking <> nil then
  begin
    try
      case ATest.ResultType of
        TTestResultType.Pass:
          if FGenerateExpectedImages then
          begin
            if not TDirectory.Exists(ExpectedImagesPath) then
              TDirectory.CreateDirectory(ExpectedImagesPath);
            FLastImageChecking.EncodeToFile(GetExpectedImageFileName(ATest));
          end;
        TTestResultType.Failure,
        TTestResultType.Error,
        TTestResultType.Warning:
          begin
            if not TDirectory.Exists(WrongImagesPath) then
            begin
              TDirectory.CreateDirectory(WrongImagesPath);
              FWrongImagesPathWasCreated := True;
            end;
            FLastImageChecking.EncodeToFile(GetWrongImageFileName(ATest));
          end;
      else
      end;
    except
    end;
    FLastImageChecking := nil;
  end;
  TThread.ForceQueue(nil,
    procedure
    var
      LLogger: IAsyncTestRunnerLogger;
    begin
      if Assigned(FAsyncTestRunner) then
        for LLogger in TAsyncTestRunner(FAsyncTestRunner).Loggers do
          LLogger.OnEndTest(AThreadId, ATest);
    end);
end;

procedure TAsyncTestRunner.OnEndTestFixture(const AThreadId: TThreadID;
  const AResults: IFixtureResult);
begin
  TThread.ForceQueue(nil,
    procedure
    var
      LLogger: IAsyncTestRunnerLogger;
    begin
      if Assigned(FAsyncTestRunner) then
        for LLogger in TAsyncTestRunner(FAsyncTestRunner).Loggers do
          LLogger.OnEndTestFixture(AThreadId, AResults);
    end);
end;

procedure TAsyncTestRunner.OnExecuteTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnImageChecking(const AImage: ISkImage);
begin
  FLastImageChecking := AImage;
end;

procedure TAsyncTestRunner.OnLog(const ALogType: TLogLevel;
  const AMessage: string);
begin
end;

procedure TAsyncTestRunner.OnSetupFixture(const AThreadId: TThreadID;
  const AFixture: ITestFixtureInfo);
begin
end;

procedure TAsyncTestRunner.OnSetupTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnStartTestFixture(const AThreadId: TThreadID;
  const AFixture: ITestFixtureInfo);
begin
end;

procedure TAsyncTestRunner.OnTearDownFixture(const AThreadId: TThreadID;
  const AFixture: ITestFixtureInfo);
begin
end;

procedure TAsyncTestRunner.OnTearDownTest(const AThreadId: TThreadID;
  const ATest: ITestInfo);
begin
end;

procedure TAsyncTestRunner.OnTestError(const AThreadId: TThreadID;
  const AError: ITestError);
begin
end;

procedure TAsyncTestRunner.OnTestFailure(const AThreadId: TThreadID;
  const AFailure: ITestError);
begin
end;

procedure TAsyncTestRunner.OnTestIgnored(const AThreadId: TThreadID;
  const AIgnored: ITestResult);
begin
end;

procedure TAsyncTestRunner.OnTestingEnds(const ARunResults: IRunResults);
begin
  TThread.ForceQueue(nil,
    procedure
    var
      LLogger: IAsyncTestRunnerLogger;
    begin
      if Assigned(FAsyncTestRunner) then
        for LLogger in TAsyncTestRunner(FAsyncTestRunner).Loggers do
          LLogger.OnTestingEnds(ARunResults);
    end);
end;

procedure TAsyncTestRunner.OnTestingStarts(const AThreadId: TThreadID;
  ATestCount, ATestActiveCount: Cardinal);
begin
  TThread.ForceQueue(nil,
    procedure
    var
      LLogger: IAsyncTestRunnerLogger;
    begin
      if Assigned(FAsyncTestRunner) then
        for LLogger in TAsyncTestRunner(FAsyncTestRunner).Loggers do
          LLogger.OnTestingStarts(AThreadId, ATestCount, ATestActiveCount);
    end);
end;

procedure TAsyncTestRunner.OnTestMemoryLeak(const AThreadId: TThreadID;
  const AIgnored: ITestResult);
begin
end;

procedure TAsyncTestRunner.OnTestSuccess(const AThreadId: TThreadID;
  const ATest: ITestResult);
begin
end;

procedure TAsyncTestRunner.RemoveLogger(const ALogger: IAsyncTestRunnerLogger);
var
  I: Integer;
begin
  if ALogger = nil then
    Exit;
  FLock.Enter;
  try
    for I := FLoggers.Count - 1 downto 0 do
      if TObject(ALogger) = TObject(FLoggers[I]) then
        FLoggers.Delete(I);
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncTestRunner.SetGenerateExpectedImages(const AValue: Boolean);
begin
  FGenerateExpectedImages := AValue;
end;

procedure TAsyncTestRunner.Wait;
begin
  if not FExecuting then
    Exit;
  FExecuteFinishedEvent.WaitFor;
  if not FExecuting then
    FExecuteFinishedEvent.SetEvent;
end;

class function TAsyncTestRunner.WrongImagesPath: string;
begin
  if FWrongImagesPath = '' then
  begin
    {$IF defined(IOS) or defined(ANDROID)}
    FWrongImagesPath := TPath.GetTempPath;
    if not FWrongImagesPath.EndsWith(TPath.DirectorySeparatorChar) then
      FWrongImagesPath := FWrongImagesPath + TPath.DirectorySeparatorChar;
    {$ELSE}
    FWrongImagesPath := TPath.Combine(TPath.GetTempPath, TPath.GetGUIDFileName(False) + TPath.DirectorySeparatorChar);
    {$ENDIF}
  end;
  Result := FWrongImagesPath;
end;

{$IF CompilerVersion < 32}
{ TSkThreadHelper }

class procedure TSkThreadHelper.ForceQueue(AThread: TThread; AThreadProc: TThreadProcedure);
begin
  if not Assigned(AThreadProc) then
    Exit;
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(AThread,
        procedure()
        begin
          AThreadProc;
        end);
    end).Start;
end;
{$ENDIF}

initialization
  FAsyncTestRunner := TAsyncTestRunner.Create;
end.
