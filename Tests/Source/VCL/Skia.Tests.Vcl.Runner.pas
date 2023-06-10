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
unit Skia.Tests.Vcl.Runner;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Types, System.Classes,
  System.Generics.Collections, System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Samples.Gauges,
  Vcl.ImgList, Vcl.Imaging.pngimage, Vcl.Menus, DUnitX.TestFramework,
  DUnitX.Extensibility, DUnitX.InternalInterfaces,
  {$IFDEF DELPHI_XE8_UP}
  System.ImageList,
  {$ENDIF}

  { Skia }
  System.Skia, Vcl.Skia,

  { Tests }
  Skia.Tests.Foundation,
  Skia.Tests.Foundation.Runner;

type
  { TfrmVclRunner }

  TfrmVclRunner = class(TForm, ITestRunnerLogger)
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
    pbxImagePreview: TSkPaintBox;
    tbcExtraDetails: TTabControl;
    pnlOptions: TPanel;
    GroupBox1: TGroupBox;
    cbxGenerateExpectedImages: TCheckBox;
    pmnPreview: TPopupMenu;
    mniCopyPreviewImage: TMenuItem;
    procedure cbxGenerateExpectedImagesClick(Sender: TObject);
    procedure FormClose(ASender: TObject; var AAction: TCloseAction);
    procedure FormCreate(ASender: TObject);
    procedure FormDestroy(ASender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(ASender: TObject);
    procedure lbxFailListClick(Sender: TObject);
    procedure pbxImagePreviewDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure pbxImagePreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbxImagePreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pmnPreviewPopup(Sender: TObject);
    procedure pnlRunAllClick(ASender: TObject);
    procedure mniCopyPreviewImageClick(Sender: TObject);
    procedure tbcContentsChange(ASender: TObject);
    procedure tbcContentsResize(Sender: TObject);
    procedure tbcExtraDetailsChange(Sender: TObject);
    procedure trvTestTreeCreateNodeClass(ASender: TCustomTreeView; var ANodeClass: TTreeNodeClass);
  private
    { Private declarations }
    FFailedTests: TList<ITestResult>;
    FFixtureList: ITestFixtureList;
    FImagePreview: ISkImage;
    FImagePreviewExpected: ISkImage;
    FImagePreviewShowExpected: Boolean;
    FLastResult: IRunResults;
    FNodes: TList<TTreeNode>;
    FVkCtrlPressed: Boolean;
    procedure BuildEnabledTestList;
    procedure BuildTree(AParentNode: TTreeNode; const AFixtureList: ITestFixtureList; var ATotalTests: Integer);
    function CreateNode(AOwner: TTreeNode; ATest: ITest): TTreeNode; overload;
    function CreateNode(AOwner: TTreeNode; AText: string; ATestFullName: string): TTreeNode; overload;
    function GetNode(AFullName: string): TTreeNode;
    procedure SetVkCtrlPressed(const AValue: Boolean);
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
  frmVclRunner: TfrmVclRunner;

implementation

uses
  { Delphi }
  Winapi.CommCtrl,
  System.UITypes,
  System.IOUtils,
  System.RegularExpressions,
  Vcl.Clipbrd;

{$R *.dfm}

type
  { TTestNode }

  TTestNode = class(TTreeNode)
  strict private
    FFullName: string;
    FResultType: TTestResultType;
    FTest: ITest;
    procedure SetResultType(const AResultType: TTestResultType);
  public
    constructor Create(AOwner: TTreeNodes); override;
    procedure Reload;
    property FullName: string read FFullName write FFullName;
    property ResultType: TTestResultTYpe read FResultType write SetResultType;
    property Test: ITest read FTest write FTest;
  end;

{$IF CompilerVersion <= 30}
  { TRectFHelper }

  TRectFHelper = record helper for TRectF
    function PlaceInto(const ADesignatedArea: TRectF): TRectF;
    function SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean = True): TRectF;
  end;

function TRectFHelper.PlaceInto(const ADesignatedArea: TRectF): TRectF;
var
  LLocation: TPointF;
begin
  Result := Self;
  if (Self.Width > ADesignatedArea.Width) or (Self.Height > ADesignatedArea.Height) then
    Result := Result.FitInto(ADesignatedArea);
  LLocation.X := (ADesignatedArea.Left + ADesignatedArea.Right - Result.Width) / 2;
  LLocation.Y := (ADesignatedArea.Top + ADesignatedArea.Bottom - Result.Height) / 2;
  Result.SetLocation(LLocation);
end;

function TRectFHelper.SnapToPixel(const AScale: Single; const APlaceBetweenPixels: Boolean): TRectF;
var
  LScale, HalfPixel: Single;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := System.Trunc(Self.Left * LScale) / LScale;
  Result.Top := System.Trunc(Self.Top * LScale) / LScale;
  Result.Width := System.Round(Self.Width * LScale) / LScale;
  Result.Height := System.Round(Self.Height * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    Result.Offset(HalfPixel, HalfPixel);
  end;
end;
{$ENDIF}

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

procedure TfrmVclRunner.cbxGenerateExpectedImagesClick(Sender: TObject);
begin
  FAsyncTestRunner.GenerateExpectedImages := cbxGenerateExpectedImages.Checked;
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
  tbcContentsResize(tbcContents);
  tbcContentsResize(tbcExtraDetails);
end;

procedure TfrmVclRunner.FormDestroy(ASender: TObject);
begin
  FAsyncTestRunner.Wait;
  FFailedTests.Free;
  FNodes.Free;
end;

procedure TfrmVclRunner.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SetVkCtrlPressed(ssCtrl in Shift);
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
  LRect: TRectF;
  LTestResult: ITestResult;
begin
  FImagePreviewShowExpected := False;
  if lbxFailList.ItemIndex = -1 then
  begin
    lblFailTestName.Caption := '';
    lblFailTestStartTime.Caption := '';
    lblFailTestFinishTime.Caption := '';
    memFailTestMessage.Text := '';
    memStackTrace.Text := '';
    FImagePreview := nil;
    FImagePreviewExpected := nil;
  end
  else
  begin
    LTestResult := FFailedTests[lbxFailList.ItemIndex];
    lblFailTestName.Caption := LTestResult.Test.FullName;
    lblFailTestStartTime.Caption := TimeToStr(LTestResult.StartTime);
    lblFailTestFinishTime.Caption := TimeToStr(LTestResult.FinishTime);
    memFailTestMessage.Text := LTestResult.Message;
    memStackTrace.Text := LTestResult.StackTrace;
    FImagePreview := TSkImage.MakeFromEncodedFile(FAsyncTestRunner.GetWrongImageFileName(LTestResult));
    FImagePreviewExpected := TSkImage.MakeFromEncodedFile(FAsyncTestRunner.GetExpectedImageFileName(LTestResult));
    FVkCtrlPressed := False;

    if memFailTestMessage.Lines.Text.Contains(' (hash: ') then
      Clipboard.AsText := memFailTestMessage.Lines.Text.Split([' (hash: '], TStringSplitOptions.None)[1].Split(['). '], TStringSplitOptions.None)[0]
    // Copy rect
    else if TRegEx.IsMatch(memFailTestMessage.Lines.Text, ' but got \([+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+,[+-]?([0-9]*[.])?[0-9]+\)') then
    begin
      LRect := StringToRect('(' + memFailTestMessage.Lines.Text.Split([' but got ('], TStringSplitOptions.None)[1].Split([')'])[0] + ')');
      Clipboard.AsText := Format('%g,%g,%g,%g', [RoundTo(LRect.Left, -2),RoundTo(LRect.Top, -2),RoundTo(LRect.Right, -2),RoundTo(LRect.Bottom, -2)], TFormatSettings.Invariant);
    end
    else
      Clipboard.AsText := memFailTestMessage.Lines.Text;
  end;
  pbxImagePreview.Redraw;
end;

procedure TfrmVclRunner.mniCopyPreviewImageClick(Sender: TObject);
var
  LPicture: TPicture;
begin
  if (FImagePreview <> nil) and (lbxFailList.ItemIndex <> -1) then
  begin
    LPicture := TPicture.Create;
    try
      LPicture.LoadFromFile(FAsyncTestRunner.GetWrongImageFileName(FFailedTests[lbxFailList.ItemIndex]));
      Clipboard.Assign(LPicture.Graphic);
    finally
      LPicture.Free;
    end;
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

procedure TfrmVclRunner.pbxImagePreviewDraw(ASender: TObject;
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
    LBorderPaint.PathEffect := TSkPathEffect.MakeDash([LStrokeThickness * 3, LStrokeThickness * 3], 0);
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
  if lbxFailList.ItemIndex = -1 then
    Exit;

  if FImagePreviewShowExpected or FVkCtrlPressed then
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
    LImageRect := LImageRect.PlaceInto(LBorderRect).SnapToPixel(LScale.X, False);
    LBorderRect := LImageRect;
    LBorderRect.Inflate(LStrokeThickness, LStrokeThickness);

    LOffset := PointF(Abs(Min(LBorderRect.Left, 0)), Abs(Min(LBorderRect.Top, 0)));
    LBorderRect.Offset(LOffset);
    LImageRect.Offset(LOffset);

    DrawBorder(LBorderRect, LImageRect);
    DrawImage(LImage, LImageRect);
  end;
  if FImagePreviewShowExpected or FVkCtrlPressed then
    DrawExpectedText;
end;

procedure TfrmVclRunner.pbxImagePreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FImagePreviewShowExpected := True;
    pbxImagePreview.Redraw;
  end;
end;

procedure TfrmVclRunner.pbxImagePreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    FImagePreviewShowExpected := False;
    pbxImagePreview.Redraw;
  end;
end;

procedure TfrmVclRunner.pmnPreviewPopup(Sender: TObject);
begin
  mniCopyPreviewImage.Visible := (FImagePreview <> nil) and (lbxFailList.ItemIndex <> -1);
end;

procedure TfrmVclRunner.pnlRunAllClick(ASender: TObject);
begin
  FAsyncTestRunner.Execute;
end;

procedure TfrmVclRunner.SetVkCtrlPressed(const AValue: Boolean);
begin
  if FVkCtrlPressed <> AValue then
  begin
    FVkCtrlPressed := AValue;
    pbxImagePreview.Redraw;
  end;
end;

procedure TfrmVclRunner.tbcContentsChange(ASender: TObject);
begin
  pnlTests.Visible := tbcContents.TabIndex = 0;
  pnlDetails.Visible := tbcContents.TabIndex = 1;
end;

procedure TfrmVclRunner.tbcContentsResize(Sender: TObject);
var
  LTabControl: TTabControl absolute Sender;
begin
  if Sender is TTabControl then
    LTabControl.TabWidth := (LTabControl.Width div LTabControl.Tabs.Count) - 2;
end;

procedure TfrmVclRunner.tbcExtraDetailsChange(Sender: TObject);
begin
  memFailTestMessage.Visible := tbcExtraDetails.TabIndex = 0;
  memStackTrace.Visible := tbcExtraDetails.TabIndex = 1;
  pnlOptions.Visible := tbcExtraDetails.TabIndex = 2;
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
