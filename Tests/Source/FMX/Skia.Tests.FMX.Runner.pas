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
unit Skia.Tests.FMX.Runner;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Actions, System.Generics.Collections, System.Math, FMX.Types, FMX.Graphics,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.ActnList, FMX.Layouts, FMX.TreeView,
  FMX.Edit, FMX.ListView.Types, FMX.ListView, FMX.ListBox,FMX.Memo, FMX.TabControl,
  FMX.Objects, FMX.Controls.Presentation, DUnitX.TestFramework, DUnitX.Extensibility,
  DUnitX.InternalInterfaces,

  { Tests }
  Skia.Tests.Foundation.Runner;

type
  { TfrmFMXRunner }

  TfrmFMXRunner = class(TForm, IAsyncTestRunnerLogger)
    pnlTestsTreeBackground: TPanel;
    trvTestTree: TTreeView;
    gplDetails: TGridPanelLayout;
    lsvFailList: TListView;
    pnlDetailsSummary: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblSuccessTests: TLabel;
    lblTotalRuns: TLabel;
    Label5: TLabel;
    lblFailTests: TLabel;
    lblMemoryLeakedLabel: TLabel;
    lblMemoryLeaked: TLabel;
    pnlFailTestDetailPanel: TPanel;
    memStackTrace: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    lblFailTestName: TLabel;
    lblFailTestStartTime: TLabel;
    Label7: TLabel;
    lblFailTestFinishTime: TLabel;
    Label6: TLabel;
    memFailTestMessage: TMemo;
    Label8: TLabel;
    tbcContent: TTabControl;
    tbiTests: TTabItem;
    tbiDetails: TTabItem;
    pgbTestRunnerProgress: TProgressBar;
    rctRunAllBackground: TRectangle;
    btnRunAll: TSpeedButton;
    lytTestsFooter: TLayout;
    Label9: TLabel;
    lblErrorsLabel: TLabel;
    lblErrorsTests: TLabel;
    stbStyle: TStyleBook;
    procedure btnRunAllClick(ASender: TObject);
    procedure FormClose(ASender: TObject; var AAction: TCloseAction);
    procedure FormCreate(ASender: TObject);
    procedure FormDestroy(ASender: TObject);
    procedure FormShow(ASender: TObject);
    procedure lsvFailListItemClick(const ASender: TObject; const AItem: TListViewItem);
    procedure lsvFailListChange(Sender: TObject);
  private
    { Private declarations }
    FFailedTests: TDictionary<string, ITestResult>;
    FFixtureList: ITestFixtureList;
    FLastResult: IRunResults;
    FNodes: TList<Pointer>;
    function CreateNode(AOwner: TComponent; ATest: ITest): TTreeViewItem; overload;
    function CreateNode(AOwner: TComponent; AText: string; ATestFullName: string): TTreeViewItem; overload;
    function GetNode(AFullName: string): TTreeViewItem;
    procedure BuildTree(AParentNode: TTreeViewItem; const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
    procedure BuildEnabledTestList;
  protected
    { IAsyncTestRunnerLogger }
    procedure OnEndTest(const AThreadId: TThreadID; const ATest: ITestResult);
    procedure OnEndTestFixture(const AThreadId: TThreadID; const AResults: IFixtureResult);
    procedure OnLog(const ALogType: TLogLevel; const AMessage: string);
    procedure OnTestingEnds(const ARunResults: IRunResults);
    procedure OnTestingStarts(const AThreadId: TThreadID; ATestCount, ATestActiveCount: Cardinal);
  public
    { Public declarations }
  end;

var
  frmFMXRunner: TfrmFMXRunner;

implementation

uses
  { Delphi }
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ELSE}
  FMX.Dialogs,
  {$ENDIF}
  System.IOUtils;

{$R *.fmx}

type
  { TTestNode }

  TTestNode = class(TTreeViewItem)
  strict private
    FFullName: string;
    FPanel: TPanel;
    FTest: ITest;
    FResultType: TTestResultType;
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent; ATest: ITest; AText: string; ATestFullName: string); reintroduce;
    property FullName: string read FFullName;
    property Test: ITest read FTest;
    property ResultType: TTestResultTYpe read FResultType write FResultType;
    procedure SetResultType(AResultType: TTestResultType);
    procedure Reload;
  end;

{ TfrmFMXRunner }

procedure TfrmFMXRunner.btnRunAllClick(ASender: TObject);
begin
  FAsyncTestRunner.Execute;
end;

procedure TfrmFMXRunner.BuildEnabledTestList;

  procedure SetEnabled(AItem: TTreeViewItem);
  var
    J: Integer;
  begin
    TTestNode(AItem).Reload;
    if Assigned(TTestNode(AItem).Test) then
      TTestNode(AItem).Test.Enabled := AItem.IsChecked;
    for J := 0 to AItem.Count - 1 do
      SetEnabled(AItem.Items[J]);
  end;

var
  I: Integer;
begin
  for I := 0 to trvTestTree.Count - 1 do
    SetEnabled(trvTestTree.Items[I]);
end;

procedure TfrmFMXRunner.BuildTree(AParentNode: TTreeViewItem;
  const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
var
  LFixture: ITestFixture;
  LTest: ITest;
  LFixtureNode: TTreeViewItem;
  LTestNode: TTreeViewItem;
begin
  for LFixture in AFixtureList do
  begin
    LFixtureNode := CreateNode(trvTestTree, LFixture.Name, LFixture.FullName);
    if LFixture.HasChildFixtures then
      LFixtureNode.IsExpanded := True;
    if Assigned(AParentNode) then
      LFixtureNode.Parent := AParentNode
    else
      LFixtureNode.Parent := trvTestTree;

    if LFixture.HasChildFixtures then
      BuildTree(LFixtureNode, LFixture.Children, ATotalTests);
    for LTest in LFixture.Tests do
    begin
      Inc(ATotalTests);
      LTestNode := CreateNode(trvTestTree, LTest);
      LTestNode.Parent := LFixtureNode;
      LTestNode.IsChecked := True;
    end;
  end;
end;

function TfrmFMXRunner.CreateNode(AOwner: TComponent; ATest: ITest): TTreeViewItem;
begin
  Result := TTestNode.Create(AOwner, ATest, ATest.Name, ATest.Fixture.FullName + '.' + ATest.Name);
  FNodes.Add(Result);
end;

function TfrmFMXRunner.CreateNode(AOwner: TComponent; AText: string; ATestFullName: string): TTreeViewItem;
begin
  Result := TTestNode.Create(AOwner, nil, AText, ATestFullName);
  FNodes.Add(Result);
end;

procedure TfrmFMXRunner.FormClose(ASender: TObject; var AAction: TCloseAction);
begin
  if AAction <> TCloseAction.caNone then
    FAsyncTestRunner.RemoveLogger(Self);
end;

procedure TfrmFMXRunner.FormCreate(ASender: TObject);
begin
  FFailedTests := TDictionary<string, ITestResult>.Create;
  FNodes := TList<Pointer>.Create;
  lblFailTestName.Text := ' ';
  lblFailTestStartTime.Text := ' ';
  lblFailTestFinishTime.Text := ' ';
  memFailTestMessage.Text := ' ';
  memStackTrace.Text := ' ';
  lblTotalRuns.Text := ' ';
  lblErrorsTests.Text := ' ';
  lblFailTests.Text := ' ';
  lblSuccessTests.Text := ' ';
  lblMemoryLeaked.Text := ' ';
end;

procedure TfrmFMXRunner.FormDestroy(ASender: TObject);
begin
  FAsyncTestRunner.Wait;
  FFailedTests.Free;
  FNodes.Free;
end;

procedure TfrmFMXRunner.FormShow(ASender: TObject);
var
  LTotalTests: Integer;
begin
  FAsyncTestRunner.AddLogger(Self);
  Height := Round(Min(Height, Screen.WorkAreaHeight));
  Top := Round(Max(Top, 0));
  LTotalTests := 0;
  FFixtureList := FAsyncTestRunner.FixtureList;
  BuildTree(nil, FFixtureList, LTotalTests);
  pgbTestRunnerProgress.Max := LTotalTests;
end;

function TfrmFMXRunner.GetNode(AFullName: string): TTreeViewItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count - 1 do
    if TTestNode(FNodes[I]).FullName = AFullName then
      Exit(TTestNode(FNodes[I]));
end;

procedure TfrmFMXRunner.lsvFailListChange(Sender: TObject);
begin
  if lsvFailList.ItemIndex > -1 then
    lsvFailListItemClick(lsvFailList, lsvFailList.Items[lsvFailList.ItemIndex]);
end;

procedure TfrmFMXRunner.lsvFailListItemClick(const ASender: TObject;
  const AItem: TListViewItem);
var
  LTestResult: ITestResult;
begin
  LTestResult := FFailedTests[AItem.Detail];
  lblFailTestName.Text := LTestResult.Test.FullName;
  lblFailTestStartTime.Text := TimeToStr(LTestResult.StartTime);
  lblFailTestFinishTime.Text := TimeToStr(LTestResult.FinishTime);
  memFailTestMessage.Text := LTestResult.Message;
  memStackTrace.Text := LTestResult.StackTrace;
end;

procedure TfrmFMXRunner.OnEndTest(const AThreadId: TThreadID;
  const ATest: ITestResult);
var
  LFailItem: TListViewItem;
  LTestNode: TTestNode;
begin
  pgbTestRunnerProgress.Value := pgbTestRunnerProgress.Value + 1;

  if (ATest.ResultType = TTestResultType.Failure)
    or (ATest.ResultType = TTestResultType.Error)
    or (ATest.ResultType = TTestResultType.MemoryLeak) then
  begin
    FFailedTests.Add(ATest.Test.FullName, ATest);
    LFailItem := lsvFailList.Items.Add;
    LFailItem.Text := ATest.Test.FullName;
    LFailItem.Detail := ATest.Test.FullName;
    FMX.Types.Log.d('[Test failed] %s: %s', [ATest.Test.FullName, ATest.Message]);
  end;

  LTestNode := GetNode(ATest.Test.FullName) as TTestNode;
  if Assigned(LTestNode) then
    LTestNode.SetResultType(ATest.ResultType);
end;

procedure TfrmFMXRunner.OnEndTestFixture(const AThreadId: TThreadID;
  const AResults: IFixtureResult);
var
  LFixtureNode: TTestNode;
begin
  LFixtureNode := GetNode(AResults.Fixture.FullName) as TTestNode;

  // I shouldn't call this here !!!
  (AResults as IFixtureResultBuilder).RollUpResults;

  if Assigned(LFixtureNode) then
  begin
    if AResults.HasFailures then
      LFixtureNode.SetResultType(TTestResultType.Failure)
    else if AResults.Errors.Count > 0 then
      LFixtureNode.SetResultType(TTestResultType.Error)
    else
      LFixtureNode.SetResultType(TTestResultType.Pass);
  end;
end;

procedure TfrmFMXRunner.OnLog(const ALogType: TLogLevel; const AMessage: string);
begin
end;

procedure TfrmFMXRunner.OnTestingEnds(const ARunResults: IRunResults);

  procedure ShowMessage(const AMessage: string);
  begin
    {$IF CompilerVersion >= 31}
    TDialogService.MessageDialog(AMessage, TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    {$ELSE}
    MessageDlg(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
    {$ENDIF}
  end;

begin
  FLastResult := ARunResults;
  lblTotalRuns.Text := IntToStr(FLastResult.TestCount);
  lblErrorsTests.Text := IntToStr(FLastResult.ErrorCount);
  lblFailTests.Text := IntToStr(FLastResult.FailureCount);
  lblSuccessTests.Text := IntToStr(FLastResult.PassCount);
  lblMemoryLeaked.Text := IntToStr(FLastResult.MemoryLeakCount);
  btnRunAll.Enabled := True;
  ShowMessage(Format('Finished with %d problem(s)', [FLastResult.ErrorCount + FLastResult.FailureCount + FLastResult.MemoryLeakCount]));
end;

procedure TfrmFMXRunner.OnTestingStarts(const AThreadId: TThreadID; ATestCount,
  ATestActiveCount: Cardinal);
begin
  btnRunAll.Enabled := False;
  pgbTestRunnerProgress.Value := 0;
  lblFailTestName.Text := ' ';
  lblFailTestStartTime.Text := ' ';
  lblFailTestFinishTime.Text := ' ';
  memFailTestMessage.Text := ' ';
  memStackTrace.Text := ' ';
  lblTotalRuns.Text := ' ';
  lblErrorsTests.Text := ' ';
  lblFailTests.Text := ' ';
  lblSuccessTests.Text := ' ';
  lblMemoryLeaked.Text := ' ';
  FFailedTests.Clear;
  lsvFailList.Selected := nil;
  lsvFailList.Items.Clear;
  tbiDetails.Enabled := True;
  BuildEnabledTestList;
end;

{ TTestNode }

procedure TTestNode.Click;
begin
  inherited;
  IsExpanded := not IsExpanded;
end;

constructor TTestNode.Create(AOwner: TComponent; ATest: ITest; AText, ATestFullName: string);
begin
  inherited Create(AOwner);
  Self.Text := AText;
  FFullName := ATestFullName;
  FTest := Test;
  FPanel := TPanel.Create(AOwner);
  FPanel.HitTest := False;
  FPanel.Width := 48;
  FPanel.Align := TAlignLayout.Right;
  FPanel.Visible := False;
  FPanel.Parent := Self;
end;

procedure TTestNode.Reload;
begin
  FPanel.Visible := False;
end;

procedure TTestNode.SetResultType(AResultType: TTestResultType);
begin
 FResultType := AResultType;
 case AResultType of
   TTestResultType.Pass: FPanel.StyleLookup := 'Panel3Style_success';
   TTestResultType.Failure: FPanel.StyleLookup := 'Panel3Style_failure';
   TTestResultType.Error: FPanel.StyleLookup := 'Panel3Style_error';
   TTestResultType.Ignored: FPanel.StyleLookup := 'Panel3Style_ignored';
   TTestResultType.MemoryLeak: FPanel.StyleLookup := 'Panel3Style_leak';
   TTestResultType.Warning: FPanel.StyleLookup := 'Panel3Style_warning';
 end;
 FPanel.Visible := True;
end;

end.
