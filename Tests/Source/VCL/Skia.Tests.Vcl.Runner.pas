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
unit Skia.Tests.Vcl.Runner;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections, System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Samples.Gauges,
  Vcl.ImgList, Vcl.Imaging.pngimage, DUnitX.TestFramework, DUnitX.Extensibility,
  DUnitX.InternalInterfaces,

  { Tests }
  Skia.Tests.Foundation.Runner;

type
  { TfrmVclRunner }

  TfrmVclRunner = class(TForm, IAsyncTestRunnerLogger)
    btnRunAll: TButton;
    tbcContents: TTabControl;
    pnlTestsFooter: TPanel;
    trvTestTree: TTreeView;
    pnlTests: TPanel;
    pnlDetails: TPanel;
    gplDetails: TGridPanel;
    pnlDetailsSummary: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblTotalRuns: TLabel;
    lblSuccessTests: TLabel;
    lblErrorsTests: TLabel;
    lblFailTests: TLabel;
    lblMemoryLeaked: TLabel;
    pnlFailTestDetailPanel: TPanel;
    Label7: TLabel;
    lblFailTestName: TLabel;
    Label9: TLabel;
    lblFailTestStartTime: TLabel;
    Label11: TLabel;
    lblFailTestFinishTime: TLabel;
    Label8: TLabel;
    memFailTestMessage: TMemo;
    memStackTrace: TMemo;
    Label10: TLabel;
    gagTestRunnerProgress: TGauge;
    imgTestsIcons: TImageList;
    pnlRunAll: TPanel;
    imgRunAllLeftCorners: TImage;
    imgRunAllRightCorners: TImage;
    pnlTestsFooterLine: TPanel;
    lbxFailList: TListBox;
    procedure FormClose(ASender: TObject; var AAction: TCloseAction);
    procedure FormCreate(ASender: TObject);
    procedure FormDestroy(ASender: TObject);
    procedure FormShow(ASender: TObject);
    procedure pnlRunAllClick(ASender: TObject);
    procedure tbcContentsChange(ASender: TObject);
    procedure trvTestTreeCreateNodeClass(ASender: TCustomTreeView; var ANodeClass: TTreeNodeClass);
    procedure lbxFailListClick(Sender: TObject);
  private
    { Private declarations }
    FFailedTests: TList<ITestResult>;
    FFixtureList: ITestFixtureList;
    FLastResult: IRunResults;
    FNodes: TList<TTreeNode>;
    function CreateNode(AOwner: TTreeNode; ATest: ITest): TTreeNode; overload;
    function CreateNode(AOwner: TTreeNode; AText: string; ATestFullName: string): TTreeNode; overload;
    function GetNode(AFullName: string): TTreeNode;
    procedure BuildTree(AParentNode: TTreeNode; const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
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
  frmVclRunner: TfrmVclRunner;

implementation

uses
  { Delphi }
  Winapi.CommCtrl,
  System.UITypes;

{$R *.dfm}

type
  { TTestNode }

  TTestNode = class(TTreeNode)
  strict private
    FFullName: string;
    FTest: ITest;
    FResultType: TTestResultType;
    procedure SetResultType(const AResultType: TTestResultType);
  public
    constructor Create(AOwner: TTreeNodes); override;
    procedure Reload;
    property FullName: string read FFullName write FFullName;
    property Test: ITest read FTest write FTest;
    property ResultType: TTestResultTYpe read FResultType write SetResultType;
  end;

{ TfrmVclRunner }

procedure TfrmVclRunner.BuildEnabledTestList;

  procedure SetEnabled(AItem: TTreeNode);
  var
    J: Integer;
  begin
    TTestNode(AItem).Reload;
    for J := 0 to AItem.Count - 1 do
      SetEnabled(AItem.Item[J]);
  end;

var
  I: Integer;
begin
  for I := 0 to trvTestTree.Items.Count - 1 do
    SetEnabled(trvTestTree.Items[I]);
end;

procedure TfrmVclRunner.BuildTree(AParentNode: TTreeNode;
  const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
var
  LFixture: ITestFixture;
  LTest: ITest;
  LFixtureNode: TTreeNode;
begin
  for LFixture in AFixtureList do
  begin
    LFixtureNode := CreateNode(AParentNode, LFixture.Name, LFixture.FullName);
    LFixtureNode.Selected := True;

    if LFixture.HasChildFixtures then
      BuildTree(LFixtureNode, LFixture.Children, ATotalTests);
    for LTest in LFixture.Tests do
    begin
      Inc(ATotalTests);
      CreateNode(LFixtureNode, LTest);
    end;
  end;
end;

function TfrmVclRunner.CreateNode(AOwner: TTreeNode; ATest: ITest): TTreeNode;
begin
  Result := trvTestTree.Items.AddChild(AOwner, ATest.Name);
  TTestNode(Result).FullName := ATest.Fixture.FullName + '.' + ATest.Name;
  TTestNode(Result).Test := ATest;
  FNodes.Add(Result);
end;

function TfrmVclRunner.CreateNode(AOwner: TTreeNode; AText,
  ATestFullName: string): TTreeNode;
begin
  Result := trvTestTree.Items.AddChild(AOwner, AText);
  TTestNode(Result).FullName := ATestFullName;
  FNodes.Add(Result);
end;

procedure TfrmVclRunner.FormClose(ASender: TObject; var AAction: TCloseAction);
begin
  if AAction <> TCloseAction.caNone then
    FAsyncTestRunner.RemoveLogger(Self);
end;

procedure TfrmVclRunner.FormCreate(ASender: TObject);
begin
  FFailedTests := TList<ITestResult>.Create;
  FNodes := TList<TTreeNode>.Create;
  lblFailTestName.Caption := ' ';
  lblFailTestStartTime.Caption := ' ';
  lblFailTestFinishTime.Caption := ' ';
  memFailTestMessage.Text := ' ';
  memStackTrace.Text := ' ';
  lblTotalRuns.Caption := ' ';
  lblErrorsTests.Caption := ' ';
  lblFailTests.Caption := ' ';
  lblSuccessTests.Caption := ' ';
  lblMemoryLeaked.Caption := ' ';
  trvTestTree.HandleNeeded;
  trvTestTree.Perform(TVM_SETITEMHEIGHT, 38, 0);
end;

procedure TfrmVclRunner.FormDestroy(ASender: TObject);
begin
  FAsyncTestRunner.Wait;
  FFailedTests.Free;
  FNodes.Free;
end;

procedure TfrmVclRunner.FormShow(ASender: TObject);
var
  LTotalTests: Integer;
  I: Integer;
begin
  FAsyncTestRunner.AddLogger(Self);
  Height := Min(Height, Screen.WorkAreaHeight);
  Top := Max(Top, 0);
  LTotalTests := 0;
  FFixtureList := FAsyncTestRunner.FixtureList;
  BuildTree(nil, FFixtureList, LTotalTests);
  gagTestRunnerProgress.MaxValue := LTotalTests;
  if trvTestTree.Items.Count > 0 then
    trvTestTree.Select(trvTestTree.Items[0]);
  for I := trvTestTree.SelectionCount - 1 downto 0 do
    trvTestTree.Deselect(trvTestTree.Items[0]);
  trvTestTree.FullExpand;
  for I := 0 to FNodes.Count - 1 do
    if Assigned(TTestNode(FNodes[I]).Test) and Assigned(FNodes[I].Parent) then
      FNodes[I].Parent.Expanded := False;
  SetScrollPos(trvTestTree.Handle, SB_VERT, 0, True);
end;

function TfrmVclRunner.GetNode(AFullName: string): TTreeNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count - 1 do
    if TTestNode(FNodes[I]).FullName = AFullName then
      Exit(FNodes[I]);
end;

procedure TfrmVclRunner.lbxFailListClick(Sender: TObject);
var
  LTestResult: ITestResult;
begin
  if lbxFailList.ItemIndex = -1 then
  begin
    lblFailTestName.Caption := '';
    lblFailTestStartTime.Caption := '';
    lblFailTestFinishTime.Caption := '';
    memFailTestMessage.Text := '';
    memStackTrace.Text := '';
  end
  else
  begin
    LTestResult := FFailedTests[lbxFailList.ItemIndex];
    lblFailTestName.Caption := LTestResult.Test.FullName;
    lblFailTestStartTime.Caption := TimeToStr(LTestResult.StartTime);
    lblFailTestFinishTime.Caption := TimeToStr(LTestResult.FinishTime);
    memFailTestMessage.Text := LTestResult.Message;
    memStackTrace.Text := LTestResult.StackTrace;
  end;
end;

procedure TfrmVclRunner.OnEndTest(const AThreadId: TThreadID;
  const ATest: ITestResult);
var
  LTestNode: TTestNode;
begin
  gagTestRunnerProgress.Progress := gagTestRunnerProgress.Progress + 1;

  if (ATest.ResultType = TTestResultType.Failure)
    or (ATest.ResultType = TTestResultType.Error)
    or (ATest.ResultType = TTestResultType.MemoryLeak) then
  begin
    FFailedTests.Add(ATest);
    lbxFailList.Items.Add(ATest.Test.FullName);
  end;

  LTestNode := GetNode(ATest.Test.FullName) as TTestNode;
  if Assigned(LTestNode) then
    LTestNode.ResultType := ATest.ResultType;
end;

procedure TfrmVclRunner.OnEndTestFixture(const AThreadId: TThreadID;
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
      LFixtureNode.ResultType := TTestResultType.Failure
    else if AResults.Errors.Count > 0 then
      LFixtureNode.ResultType := TTestResultType.Error
    else
      LFixtureNode.ResultType := TTestResultType.Pass;
  end;
end;

procedure TfrmVclRunner.OnLog(const ALogType: TLogLevel;
  const AMessage: string);
begin
end;

procedure TfrmVclRunner.OnTestingEnds(const ARunResults: IRunResults);
begin
  FLastResult := ARunResults;
  lblTotalRuns.Caption := IntToStr(FLastResult.TestCount);
  lblErrorsTests.Caption := IntToStr(FLastResult.ErrorCount);
  lblFailTests.Caption := IntToStr(FLastResult.FailureCount);
  lblSuccessTests.Caption := IntToStr(FLastResult.PassCount);
  lblMemoryLeaked.Caption := IntToStr(FLastResult.MemoryLeakCount);
  pnlRunAll.Enabled := True;
  MessageDlg(Format('Finished with %d problem(s)',
    [FLastResult.ErrorCount + FLastResult.FailureCount + FLastResult.MemoryLeakCount]),
    TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmVclRunner.OnTestingStarts(const AThreadId: TThreadID; ATestCount,
  ATestActiveCount: Cardinal);
begin
  pnlRunAll.Enabled := False;
  gagTestRunnerProgress.Progress := 0;
  lblFailTestName.Caption := ' ';
  lblFailTestStartTime.Caption := ' ';
  lblFailTestFinishTime.Caption := ' ';
  memFailTestMessage.Text := ' ';
  memStackTrace.Text := ' ';
  lblTotalRuns.Caption := ' ';
  lblErrorsTests.Caption := ' ';
  lblFailTests.Caption := ' ';
  lblSuccessTests.Caption := ' ';
  lblMemoryLeaked.Caption := ' ';
  FFailedTests.Clear;
  lbxFailList.ItemIndex := -1;
  lbxFailList.Items.Clear;
  BuildEnabledTestList;
end;

procedure TfrmVclRunner.pnlRunAllClick(ASender: TObject);
begin
  FAsyncTestRunner.Execute;
end;

procedure TfrmVclRunner.tbcContentsChange(ASender: TObject);
begin
  pnlTests.Visible := tbcContents.TabIndex = 0;
  pnlDetails.Visible := tbcContents.TabIndex = 1;
end;

procedure TfrmVclRunner.trvTestTreeCreateNodeClass(ASender: TCustomTreeView;
  var ANodeClass: TTreeNodeClass);
begin
  ANodeClass := TTestNode;
end;

{ TTestNode }

constructor TTestNode.Create(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);
  ImageIndex := 0;
  SelectedIndex := 1;
end;

procedure TTestNode.Reload;
begin
  ImageIndex := 0;
  SelectedIndex := 1;
end;

procedure TTestNode.SetResultType(const AResultType: TTestResultType);
begin
  FResultType := AResultType;
  case AResultType of
    TTestResultType.Pass: ImageIndex := 2;
    TTestResultType.Failure: ImageIndex := 4;
    TTestResultType.Error: ImageIndex := 6;
    TTestResultType.Ignored: ImageIndex := 8;
    TTestResultType.MemoryLeak: ImageIndex := 10;
    TTestResultType.Warning: ImageIndex := 12;
  end;
  if SelectedIndex <> ImageIndex + 1 then
  begin
    SelectedIndex := ImageIndex + 1;
    if IsVisible then
      Owner.Owner.Repaint;
  end;
end;

end.
