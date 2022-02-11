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
    function GetFixtureList: ITestFixtureList;
    procedure Execute;
    procedure RemoveLogger(const ALogger: IAsyncTestRunnerLogger);
    procedure Wait;
    property FixtureList: ITestFixtureList read GetFixtureList;
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
  System.SyncObjs;

type
  { TAsyncTestRunner }

  TAsyncTestRunner = class(TInterfacedObject, ITestLogger, IAsyncTestRunner)
  private
    FCanceled: Boolean;
    FEvent: TEvent;
    FExecuting: Boolean;
    FExecuteFinishedEvent: TEvent;
    FLock: TCriticalSection;
    FLoggers: TList<IAsyncTestRunnerLogger>;
    FTask: ITask;
    FTestRunner: ITestRunner;
    function GetFixtureList: ITestFixtureList;
    function GetLoggers: TArray<IAsyncTestRunnerLogger>;
    property Loggers: TArray<IAsyncTestRunnerLogger> read GetLoggers;
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
  end;

  {$IF CompilerVersion < 32}
  { TSkThreadHelper }

  TSkThreadHelper = class helper for TThread
  public
    class procedure ForceQueue(AThread: TThread; AThreadProc: TThreadProcedure); static;
  end;
  {$ENDIF}

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

destructor TAsyncTestRunner.Destroy;
begin
  Wait;
  FCanceled := True;
  FEvent.SetEvent;
  FTask.Wait;
  FLoggers.Free;
  FEvent.Free;
  FExecuteFinishedEvent.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TAsyncTestRunner.Execute;
begin
  if FExecuting then
    Exit;
  FExecuting := True;
  FEvent.SetEvent;
end;

function TAsyncTestRunner.GetFixtureList: ITestFixtureList;
begin
  Supports(FTestRunner.BuildFixtures, ITestFixtureList, Result);
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

procedure TAsyncTestRunner.Wait;
begin
  if not FExecuting then
    Exit;
  FExecuteFinishedEvent.WaitFor;
  if not FExecuting then
    FExecuteFinishedEvent.SetEvent;
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
