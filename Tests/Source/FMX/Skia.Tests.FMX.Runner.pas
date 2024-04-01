{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2024 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.Tests.FMX.Runner;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Actions,
  System.Generics.Collections, System.Math, FMX.ActnList, FMX.Controls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Forms, FMX.Graphics, FMX.Layouts,
  FMX.ListBox, FMX.ListView, FMX.ListView.Types, FMX.Memo, FMX.Objects,
  FMX.StdCtrls, FMX.TabControl, FMX.TreeView, FMX.Types, FMX.Menus,
  DUnitX.Extensibility, DUnitX.InternalInterfaces, DUnitX.TestFramework,
  {$IFDEF DELPHI_SEATTLE_UP}
  FMX.ListView.Adapters.Base, FMX.ListView.Appearances,
  {$ENDIF}
  {$IFDEF DELPHI_XE8_UP}
  FMX.Memo.Types, FMX.ScrollBox,
  {$ENDIF}

  { Skia }
  System.Skia, FMX.Skia, 
  
  { Tests }
  Skia.Tests.Foundation,
  Skia.Tests.Foundation.Runner;

type
  { TfrmFMXRunner }

  TfrmFMXRunner = class(TForm, ITestRunnerLogger)
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
    Label3: TLabel;
    Label4: TLabel;
    lblFailTestName: TLabel;
    lblFailTestStartTime: TLabel;
    Label7: TLabel;
    lblFailTestFinishTime: TLabel;
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
    hsbFailTestDetail: THorzScrollBox;
    pbxImagePreview: TSkPaintBox;
    tbcExtraDetails: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    memFailTestMessage: TMemo;
    memStackTrace: TMemo;
    TabItem3: TTabItem;
    cbxGenerateExpectedImages: TCheckBox;
    GroupBox1: TGroupBox;
    pmnPreview: TPopupMenu;
    mniCopyPreviewImage: TMenuItem;
    procedure btnRunAllClick(ASender: TObject);
    procedure cbxGenerateExpectedImagesChange(Sender: TObject);
    procedure FormClose(ASender: TObject; var AAction: TCloseAction);
    procedure FormCreate(ASender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(ASender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormShow(ASender: TObject);
    procedure lsvFailListItemClick(const ASender: TObject; const AItem: TListViewItem);
    procedure lsvFailListChange(Sender: TObject);
    procedure pbxImagePreviewDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure pbxImagePreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure pbxImagePreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure mniCopyPreviewImageClick(Sender: TObject);
    procedure pmnPreviewPopup(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    FFailedTests: TList<ITestResult>;
    FFixtureList: ITestFixtureList;
    FImagePreview: ISkImage;
    FImagePreviewExpected: ISkImage;
    FImagePreviewShowExpected: Boolean;
    FVkControlPressed: Boolean;
    FLastResult: IRunResults;
    FNodes: TList<Pointer>;
    procedure BuildEnabledTestList;
    procedure BuildTree(AParentNode: TTreeViewItem; const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
    function CreateNode(AOwner: TComponent; ATest: ITest): TTreeViewItem; overload;
    function CreateNode(AOwner: TComponent; AText: string; ATestFullName: string): TTreeViewItem; overload;
    function GetNode(AFullName: string): TTreeViewItem;
    procedure SetVkControlPressed(const AValue: Boolean);
  protected
    { ITestRunnerLogger }
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
  System.IOUtils,
  System.RegularExpressions,
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ELSE}
  FMX.Dialogs,
  {$ENDIF}
  FMX.Platform;

{$R *.fmx}

type
  { TTestNode }

  TTestNode = class(TTreeViewItem)
  strict private
    FFullName: string;
    FPanel: TPanel;
    FResultType: TTestResultType;
    FTest: ITest;
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent; ATest: ITest; AText: string; ATestFullName: string); reintroduce;
    procedure SetResultType(AResultType: TTestResultType);
    procedure Reload;
    property FullName: string read FFullName;
    property ResultType: TTestResultTYpe read FResultType write FResultType;
    property Test: ITest read FTest;
  end;

{$IF CompilerVersion < 29}
type
  TFormatSettingsHelper = record helper for TFormatSettings
    class function Invariant: TFormatSettings; static;
  end;

class function TFormatSettingshelper.Invariant: TFormatSettings;
begin
  Result.CurrencyString := #$00A4;
  Result.CurrencyFormat := 0;
  Result.CurrencyDecimals := 2;
  Result.DateSeparator := '/';
  Result.TimeSeparator := ':';
  Result.ListSeparator := ',';
  Result.ShortDateFormat := 'MM/dd/yyyy';
  Result.LongDateFormat := 'dddd, dd MMMMM yyyy HH:nn:ss';
  Result.TimeAMString := 'AM';
  Result.TimePMString := 'PM';
  Result.ShortTimeFormat := 'HH:nn';
  Result.LongTimeFormat := 'HH:nn:ss';
  Result.ThousandSeparator := ',';
  Result.DecimalSeparator := '.';
  Result.TwoDigitYearCenturyWindow := 50;
  Result.NegCurrFormat := 0;
end;
{$ENDIF}

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

procedure TfrmFMXRunner.cbxGenerateExpectedImagesChange(Sender: TObject);
begin
  FAsyncTestRunner.GenerateExpectedImages := cbxGenerateExpectedImages.IsChecked;
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
  FFailedTests := TList<ITestResult>.Create;
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
  pbxImagePreview.AutoCapture := True;
  {$IF CompilerVersion >= 30}
  cbxGenerateExpectedImages.Hint := 'It will generate the expected images for successful tests.';
  {$ENDIF}
end;

procedure TfrmFMXRunner.FormDeactivate(Sender: TObject);
begin
  SetVkControlPressed(False);
end;

procedure TfrmFMXRunner.FormDestroy(ASender: TObject);
begin
  FAsyncTestRunner.Wait;
  FFailedTests.Free;
  FNodes.Free;
end;

procedure TfrmFMXRunner.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF MSWINDOWS}
  SetVkControlPressed(ssCtrl in Shift);
  {$ELSE}
  if Key in [vkControl, vkLControl, vkRControl] then
    SetVkControlPressed(True);
  {$ENDIF}
end;

procedure TfrmFMXRunner.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF MSWINDOWS}
  SetVkControlPressed(ssCtrl in Shift);
  {$ELSE}
  if Key in [vkControl, vkLControl, vkRControl] then
    SetVkControlPressed(False);
  {$ENDIF}
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
  LClipboardService: IFMXClipboardService;
  LRect: TRectF;
  LServiceInterface: IInterface;
  LTestResult: ITestResult;
  LText: string;
begin
  LTestResult := FFailedTests[AItem.Tag];
  lblFailTestName.Text := LTestResult.Test.FullName;
  lblFailTestStartTime.Text := TimeToStr(LTestResult.StartTime);
  lblFailTestFinishTime.Text := TimeToStr(LTestResult.FinishTime);
  memFailTestMessage.Text := LTestResult.Message;
  memStackTrace.Text := LTestResult.StackTrace;
  hsbFailTestDetail.ViewportPosition := PointF(0, 0);
  tbcExtraDetails.TabIndex := 0;
  FImagePreview := TSkImage.MakeFromEncodedFile(FAsyncTestRunner.GetWrongImageFileName(LTestResult));
  FImagePreviewExpected := TSkImage.MakeFromEncodedFile(FAsyncTestRunner.GetExpectedImageFileName(LTestResult));
  FImagePreviewShowExpected := False;
  FVkControlPressed := False;

  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    // Copy similarity hash
    if memFailTestMessage.Text.Contains(' (hash: ') then
    begin
      LText := memFailTestMessage.Text.Split([' (hash: '], TStringSplitOptions.None)[1].Split([').'], TStringSplitOptions.None)[0];
      LClipboardService.SetClipboard(LText);
      FMX.Types.Log.d('Similarity hash obtained: "%s"', [LText]);
    end
    // Copy rect
    else if TRegEx.IsMatch(memFailTestMessage.Text, ' but got \([+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+\)') then
    begin
      LRect := StringToRect('(' + memFailTestMessage.Text.Split([' but got ('], TStringSplitOptions.None)[1].Split([')'])[0] + ')');
      LText := Format('%g,%g,%g,%g', [RoundTo(LRect.Left, -2),RoundTo(LRect.Top, -2),RoundTo(LRect.Right, -2),RoundTo(LRect.Bottom, -2)], TFormatSettings.Invariant);
      LClipboardService.SetClipboard(LText);
      FMX.Types.Log.d('Bounds rect obtained: "%s"', [LText]);
    end
    // Copy error message
    else
      LClipboardService.SetClipboard(memFailTestMessage.Text);
  end;
  pbxImagePreview.Redraw;
end;

procedure TfrmFMXRunner.mniCopyPreviewImageClick(Sender: TObject);
var
  LBitmap: TBitmap;
  LClipboardService: IFMXClipboardService;
  LServiceInterface: IInterface;
begin
  if (FImagePreview <> nil) and (lsvFailList.ItemIndex <> -1) and
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    LBitmap := TBitmap.CreateFromSkImage(FImagePreview);
    try
      LClipboardService.SetClipboard(LBitmap);
    finally
      LBitmap.Free;
    end;
  end;
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
    LFailItem := lsvFailList.Items.Add;
    LFailItem.Text := ATest.Test.FullName;
    LFailItem.Detail := ATest.Test.FullName;
    LFailItem.Tag := FFailedTests.Count;
    FFailedTests.Add(ATest);
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

procedure TfrmFMXRunner.pbxImagePreviewDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
const
  DefaultStrokeThickness = 1;
var
  LStrokeThickness: Single;

  procedure DrawChessBackground;
  var
    LChessPaint: ISkPaint;
    LChessSurface: ISkSurface;
  begin
    LChessPaint := TSkPaint.Create;
    LChessPaint.Color := $FFF0F0F0;
    LChessSurface := TSkSurface.MakeRaster(16, 16);
    LChessSurface.Canvas.Clear(TAlphaColors.White);
    LChessSurface.Canvas.DrawRect(RectF(0, 8, 8, 16), LChessPaint);
    LChessSurface.Canvas.DrawRect(RectF(8, 0, 16, 8), LChessPaint);
    LChessPaint := TSkPaint.Create;
    LChessPaint.Shader := LChessSurface.MakeImageSnapshot.MakeShader(TSkTileMode.Repeat, TSkTileMode.Repeat);
    ACanvas.Save;
    try
      ACanvas.ClipRect(ADest);
      ACanvas.DrawPaint(LChessPaint);
    finally
      ACanvas.Restore;
    end;
  end;

  procedure DrawBorder(ABorderRect, AImageRect: TRectF);
  var
    LBorderPaint: ISkPaint;
    LBorderPathBuilder: ISkPathBuilder;
  begin
    ACanvas.Save;
    try
      ACanvas.ClipRect(ABorderRect);
      ACanvas.ClipRect(AImageRect, TSkClipOp.Difference);
      ACanvas.Clear(TAlphaColors.Black);
    finally
      ACanvas.Restore;
    end;

    ABorderRect.Inflate(-LStrokeThickness/2, -LStrokeThickness/2);
    LBorderPathBuilder := TSkPathBuilder.Create;
    LBorderPathBuilder.AddRect(ABorderRect);
    LBorderPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
    LBorderPaint.StrokeWidth := LStrokeThickness;
    LBorderPaint.Color := TAlphaColors.Yellow;
    LBorderPaint.PathEffect := TSkPathEffect.MakeDash([LStrokeThickness * 4, LStrokeThickness * 4], 0);
    ACanvas.DrawPath(LBorderPathBuilder.Detach, LBorderPaint);
  end;

  procedure DrawImage(const AImage: ISkImage; const AImageRect: TRectF);
  begin
    ACanvas.Save;
    try
      ACanvas.Translate(AImageRect.Left, AImageRect.Top);
      ACanvas.Scale(AImageRect.Width / AImage.Width, AImageRect.Height / AImage.Height);
      ACanvas.DrawImage(AImage, 0, 0, TSkSamplingOptions.Low);
    finally
      ACanvas.Restore;
    end;
  end;

  procedure DrawCenterText(const AText: string; const ADest: TRectF;
    const AColor: TAlphaColor; const AFontSize: Single;
    const AFontWeight: TSkFontWeight);
  var
    LFont: ISkFont;
    LPaint: ISkPaint;
    LTextBounds: TRectF;
  begin
    LPaint := TSkPaint.Create;
    LPaint.Color := AColor;
    LFont := TSkFont.Create(TSkTypeFace.MakeFromName('', TSkFontStyle.Create(AFontWeight, TSkFontWidth.Normal, TSkFontSlant.Upright)), AFontSize);
    LFont.MeasureText(AText, LTextBounds, LPaint);
    ACanvas.DrawSimpleText(AText, ADest.Left + ((ADest.Width - LTextBounds.Width) / 2),
      ADest.Top - LTextBounds.Top + ((ADest.Height - LTextBounds.Height) / 2), LFont, LPaint);
  end;

  procedure DrawExpectedText;
  const
    TextRectWidth = 58;
    TextRectHeight = 22;
    TextRectMargin = 8;
    Text = 'Expected';
  var
    LPaint: ISkPaint;
    LTextRect: TRectF;
  begin
    LPaint := TSkPaint.Create;
    LPaint.AntiAlias := True;
    LPaint.AlphaF := 0.5;
    LTextRect := RectF(ADest.Right - TextRectMargin - TextRectWidth, ADest.Top + TextRectMargin,
      ADest.Right - TextRectMargin, ADest.Top + TextRectMargin + TextRectHeight);
    ACanvas.DrawRoundRect(LTextRect, 6, 6, LPaint);
    DrawCenterText(Text, LTextRect, TAlphaColors.White, 11, TSkFontWeight.SemiBold);
  end;

var
  LBorderRect: TRectF;
  LImage: ISkImage;
  LImageRect: TRectF;
  LOffset: TPointF;
  LScale: TPointF;
begin
  DrawChessBackground;
  if lsvFailList.ItemIndex = -1 then
    Exit;

  if FImagePreviewShowExpected or FVkControlPressed then
    LImage := FImagePreviewExpected
  else
    LImage := FImagePreview;
  if LImage = nil then
    DrawCenterText('(Not found)', ADest, $FF3B3B3B, 11, TSkFontWeight.Medium)
  else
  begin
    LScale := ACanvas.GetLocalToDeviceAs3x3.ExtractScale;
    LStrokeThickness := Round(Max(Min(LScale.X, LScale.Y) * DefaultStrokeThickness, 1)) / Min(LScale.X, LScale.Y);

    LBorderRect := ADest;
    LBorderRect.Inflate(-LStrokeThickness, -LStrokeThickness);
    LImageRect := RectF(0, 0, LImage.Width / LScale.X, LImage.Height / LScale.Y);
    {$IF CompilerVersion >= 31}
    LImageRect := LImageRect.PlaceInto(LBorderRect).SnapToPixel(LScale.X, False);
    {$ELSE}
    if (LImageRect.Width < LBorderRect.Width) and (LImageRect.Height < LBorderRect.Height) then
      RectCenter(LImageRect, LBorderRect)
    else
      LImageRect := LImageRect.FitInto(LBorderRect);
    {$ENDIF}
    LBorderRect := LImageRect;
    LBorderRect.Inflate(LStrokeThickness, LStrokeThickness);

    LOffset := PointF(Abs(Min(LBorderRect.Left, 0)), Abs(Min(LBorderRect.Top, 0)));
    LBorderRect.Offset(LOffset);
    LImageRect.Offset(LOffset);

    DrawBorder(LBorderRect, LImageRect);
    DrawImage(LImage, LImageRect);
  end;
  if FImagePreviewShowExpected or FVkControlPressed then
    DrawExpectedText;
end;

procedure TfrmFMXRunner.pbxImagePreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FImagePreviewShowExpected := True;
    pbxImagePreview.Redraw;
  end;
end;

procedure TfrmFMXRunner.pbxImagePreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FImagePreviewShowExpected := False;
    pbxImagePreview.Redraw;
  end;
end;

procedure TfrmFMXRunner.pmnPreviewPopup(Sender: TObject);
begin
  mniCopyPreviewImage.Visible := (FImagePreview <> nil) and (lsvFailList.ItemIndex <> -1);
end;

procedure TfrmFMXRunner.SetVkControlPressed(const AValue: Boolean);
begin
  if FVkControlPressed <> AValue then
  begin
    FVkControlPressed := AValue;
    pbxImagePreview.Redraw;
  end;
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
    {$IF CompilerVersion >= 32}
    TTestResultType.Warning: FPanel.StyleLookup := 'Panel3Style_warning';
    {$ENDIF}
  end;
  FPanel.Visible := True;
end;

end.
