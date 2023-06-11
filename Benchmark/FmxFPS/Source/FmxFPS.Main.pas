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
unit FmxFPS.Main;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Diagnostics, System.TypInfo, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Memo, FMX.Dialogs;

type
  { TVertScrollBox }

  TVertScrollBox = class(FMX.Layouts.TVertScrollBox)
  protected
    procedure DoUpdateAniCalculations(const AAniCalculations: TScrollCalculations); override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    vsbContent: TVertScrollBox;
    tmrSimulateScroll: TTimer;
    tmrStart: TTimer;
    procedure tmrSimulateScrollTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrStartTimer(Sender: TObject);
  private
    FNextScrollUp: Boolean;
    FPaintCount: Int64;
    FRunning: Boolean;
    FStopwatch: TStopwatch;
    function CreateControl(const AControlNumber, ATotalOfControls: Integer): TControl;
    class procedure ShowMessage(const AMessage: string; const ACloseDialogProc: TProc = nil); static;
    procedure SimulateScrollDown;
    procedure SimulateScrollUp;
  protected
    procedure DoPaint(const ACanvas: TCanvas; const ARect: TRectF); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  {$IFDEF SKIA}
  { Skia }
  FMX.Skia,
  {$ENDIF}

  { Delphi }
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ENDIF}
  System.IOUtils, FMX.Types3D;

{ TfrmMain }

function TfrmMain.CreateControl(const AControlNumber,
  ATotalOfControls: Integer): TControl;
const
  AllowedCharactersInText = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyZ0123456789,.-+';

  function RandomRangeString(AMinLength, AMaxLength: Integer;
    const ACharactersAllowed: string): string;
  var
    I: Integer;
  begin
    Assert((AMinLength >= 0) and (AMaxLength >= 0) or (AMinLength <= AMaxLength));
    AMinLength := AMinLength + System.Random((AMaxLength - AMinLength) + 1);
    Result := '';
    for I := 0 to AMinLength-1 do
      Result := Result + ACharactersAllowed.Chars[System.Random(Length(ACharactersAllowed))];
  end;

  function GetAssetsPath: string;
  begin
    {$IFDEF MSWINDOWS}
    Result := TPath.GetFullPath('..\..\..\Assets\');
    {$ELSEIF defined(iOS) or defined(ANDROID)}
    Result := TPath.GetDocumentsPath;
    {$ELSEIF defined(MACOS)}
    Result := TPath.GetFullPath('../Resources/');
    {$ELSE}
    Result := ExtractFilePath(ParamStr(0));
    {$ENDIF}
    if (Result <> '') and not Result.EndsWith(PathDelim) then
      Result := Result + PathDelim;
  end;

  function CreateButton: TButton;
  begin
    Result := TButton.Create(Self);
    Result.Text := RandomRangeString(8, 8, AllowedCharactersInText);
  end;

  function CreateCircle(const AHeight: Single): TCircle;
  begin
    Result := TCircle.Create(Self);
    Result.Height := AHeight;
  end;

  function CreateImage: TImage;
  begin
    Result := TImage.Create(Self);
    Result.Bitmap.LoadFromFile(GetAssetsPath + 'bee.jpg');
  end;

  function CreateLabel: TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Text := RandomRangeString(8, 8, AllowedCharactersInText);
  end;

  function CreateRectangle(const ARadius: Single;
    const ACorners: TCorners): TRectangle;
  begin
    Result := TRectangle.Create(Self);
    Result.XRadius := ARadius;
    Result.YRadius := ARadius;
    Result.Corners := ACorners;
  end;

  function CreateSpeedButton: TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    Result.Text := RandomRangeString(8, 8, AllowedCharactersInText);
  end;

begin
  case AControlNumber mod 11 of
    0, 1: Result := CreateRectangle(0, AllCorners);
    2: Result := CreateRectangle(8, AllCorners);
    3: Result := CreateRectangle(8, [TCorner.TopLeft, TCorner.BottomRight]);
    4: Result := CreateCircle(50);
    5: Result := CreateCircle(80);
    6: Result := CreateButton;
    7: Result := CreateSpeedButton;
    8, 9: Result := CreateLabel;
    10: Result := CreateImage;
  else
    Result := nil;
  end;
end;

procedure TfrmMain.DoPaint(const ACanvas: TCanvas; const ARect: TRectF);
begin
  if FRunning then
  begin
    if FPaintCount = 0 then
    begin
      {$IF CompilerVersion < 29}
      vsbContent.ViewportPosition := PointF(0, vsbContent.ContentBounds.Height * 0.3);
      {$ELSE}
      vsbContent.ScrollBy(0, -vsbContent.ContentBounds.Height * 0.3);
      {$ENDIF}
      FStopwatch := TStopwatch.StartNew;
      tmrSimulateScroll.Enabled := True;
      tmrSimulateScrollTimer(nil);
    end;
    Inc(FPaintCount);
  end;
  inherited;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  StartingMessage = 'For 6 seconds, this performance test will simulate a real app, ' +
    'with hundreds of controls, to measure the FPS rate when sliding a vertical scroll.';
  ControlsCount = 500;
  TotalScrollHeight = 5000;
var
  LControl: TControl;
  I: Integer;
begin
  for I := 0 to ControlsCount - 1 do
  begin
    LControl := CreateControl(I, ControlsCount);
    if LControl = nil then
      Continue;
    LControl.HitTest := False;
    LControl.Position.X := ((I mod 50) / 49) * Width;
    LControl.Position.Y := (I / ControlsCount) * TotalScrollHeight;
    if I mod 4 = 3 then
      LControl.Opacity := 0.5;
    LControl.Parent := vsbContent;
  end;
  ShowMessage(StartingMessage,
    procedure
    begin
      frmMain.tmrStart.Enabled := True;
    end);
end;

class procedure TfrmMain.ShowMessage(const AMessage: string;
  const ACloseDialogProc: TProc);
begin
  {$IF CompilerVersion >= 31}
  TDialogService.MessageDialog(AMessage, TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
      if Assigned(ACloseDialogProc) then
        ACloseDialogProc;
    end);
  {$ELSE}
  MessageDlg(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK);
  if Assigned(ACloseDialogProc) then
    ACloseDialogProc;
  {$ENDIF}
end;

procedure TfrmMain.SimulateScrollDown;
begin
  {$IF CompilerVersion >= 32}
  TThread.ForceQueue(nil,
    procedure()
    begin
      vsbContent.AniCalculations.MouseDown(100, 200);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 197);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 186);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 171);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 96);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      vsbContent.AniCalculations.MouseUp(100, 96);
    end);
  {$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(nil,
        procedure()
        begin
          vsbContent.AniCalculations.MouseDown(100, 200);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 197);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 186);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 171);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 96);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          vsbContent.AniCalculations.MouseUp(100, 96);
        end);
    end).Start;
  {$ENDIF}
end;

procedure TfrmMain.SimulateScrollUp;
begin
  {$IF CompilerVersion >= 32}
  TThread.ForceQueue(nil,
    procedure()
    begin
      vsbContent.AniCalculations.MouseDown(100, 96);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 101);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 110);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      Sleep(1);
      vsbContent.AniCalculations.MouseMove(100, 197);
    end);
  TThread.ForceQueue(nil,
    procedure()
    begin
      vsbContent.AniCalculations.MouseUp(100, 197);
    end);
  {$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(nil,
        procedure()
        begin
          vsbContent.AniCalculations.MouseDown(100, 96);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 101);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 110);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          Sleep(1);
          vsbContent.AniCalculations.MouseMove(100, 197);
        end);
      TThread.Queue(nil,
        procedure()
        begin
          vsbContent.AniCalculations.MouseUp(100, 197);
        end);
    end
  ).Start;
  {$ENDIF}
end;

procedure TfrmMain.tmrSimulateScrollTimer(Sender: TObject);

  function GetRenderName: string;
  begin
    Result := Canvas.ClassName;
    // When TCanvasGpu is used, the real render is the 3D forms render
    if SameText(Result, 'TCanvasGpu') then
      Result := Format('%s -> %s', [TContextManager.DefaultContextClass.ClassName, Result]);
  end;

begin
  if FStopwatch.ElapsedMilliseconds > 6000 then
  begin
    FStopwatch.Stop;
    tmrSimulateScroll.Enabled := False;
    vsbContent.HitTest := True;
    {$IFDEF SKIA}
    if GlobalUseSkia then
    begin
      ShowMessage(Format('Skia render (%s): %g fps' + sLineBreak + 'Form.Quality: %s' + sLineBreak + sLineBreak +
        'To compare the results with the firemonkey''s default canvas, just remove the line "GlobalUseSkia := True" ' +
        'from the .dpr file of project.', [GetRenderName, FPaintCount / FStopwatch.Elapsed.TotalSeconds,
        GetEnumName(TypeInfo(TCanvasQuality), Ord(Self.Quality))]))
    end
    else
    {$ENDIF}
      ShowMessage(Format('FMX render (%s): %g fps' + sLineBreak + 'Form.Quality: %s', [GetRenderName,
        FPaintCount / FStopwatch.Elapsed.TotalSeconds, GetEnumName(TypeInfo(TCanvasQuality), Ord(Self.Quality))]));
  end
  else if FNextScrollUp then
    SimulateScrollUp
  else
    SimulateScrollDown;
  FNextScrollUp := not FNextScrollUp;
end;

procedure TfrmMain.tmrStartTimer(Sender: TObject);
begin
  tmrStart.Enabled := False;
  FRunning := True;
  Invalidate;
end;

{ TVertScrollBox }

procedure TVertScrollBox.DoUpdateAniCalculations(
  const AAniCalculations: TScrollCalculations);
begin
  inherited;
  AAniCalculations.Animation := True;
  AAniCalculations.BoundsAnimation := True;
  AAniCalculations.TouchTracking := [ttVertical];
end;

end.
