{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2011-2021 Google LLC.                                    }
{ Copyright (c) 2021 Skia4Delphi Project.                                }
{                                                                        }
{ Use of this source code is governed by a BSD-style license that can be }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Skia.FMX.Designtime.Editor.Lottie;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Memo,
  FMX.TabControl,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ActnList,

  { Skia }
  Skia.FMX;

type
  { TSkLottieEditorForm }

  TSkLottieEditorForm = class(TForm)
    lytRightMenu: TLayout;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    btnOk: TSpeedButton;
    btnCancel: TSpeedButton;
    btnClear: TSpeedButton;
    tbcContent: TTabControl;
    tbiPreview: TTabItem;
    tbiSource: TTabItem;
    memSource: TMemo;
    rctPreviewBackground: TRectangle;
    sdgSaveDialog: TSaveDialog;
    odgOpenDialog: TOpenDialog;
    actActionList: TActionList;
    actCopy: TAction;
    actPaste: TAction;
    tmrRepaint: TTimer;
    lytContent: TLayout;
    procedure actCopyExecute(ASender: TObject);
    procedure actPasteExecute(ASender: TObject);
    procedure btnClearClick(ASender: TObject);
    procedure btnOpenClick(ASender: TObject);
    procedure btnSaveClick(ASender: TObject);
    procedure FormClose(ASender: TObject; var AAction: TCloseAction);
    procedure FormKeyDown(ASender: TObject; var AKey: Word; var AKeyChar: Char; AShift: TShiftState);
    procedure lytContentDragDrop(ASender: TObject; const AData: TDragObject; const APoint: TPointF);
    procedure lytContentDragOver(ASender: TObject; const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
    procedure memSourceChangeTracking(ASender: TObject);
    procedure tmrRepaintTimer(ASender: TObject);
  private
    FLottie: TSkLottieAnimation;
    procedure LoadFromFile(const AFileName: string);
  public
    function ShowModal(var ASource: TSkLottieSource): TModalResult; reintroduce;
  end;

implementation

uses
  { Delphi }
  System.Rtti,
  System.IOUtils,
  FMX.Platform;

{$R *.fmx}

{ TSkLottieEditorForm }

procedure TSkLottieEditorForm.actCopyExecute(ASender: TObject);
var
  LServiceInterface: IInterface;
  LClipboardService: IFMXClipboardService;
begin
  if actCopy.Enabled and TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    LClipboardService.SetClipboard(memSource.Lines.Text);
  end;
end;

procedure TSkLottieEditorForm.actPasteExecute(ASender: TObject);
var
  LServiceInterface: IInterface;
  LClipboardService: IFMXClipboardService;
  LValue: TValue;
  LText: string;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    LValue := LClipboardService.GetClipboard;
    if LValue.IsType<string> then
    begin
      LText := LValue.AsString.Trim;
      if LText.StartsWith('{') and LText.EndsWith('}') then
      begin
        memSource.Lines.Text := LText;
        memSourceChangeTracking(nil);
      end;
    end;
  end;
end;

procedure TSkLottieEditorForm.btnClearClick(ASender: TObject);
begin
  memSource.Lines.Clear;
end;

procedure TSkLottieEditorForm.btnOpenClick(ASender: TObject);
begin
  if odgOpenDialog.Execute then
    LoadFromFile(odgOpenDialog.FileName);
end;

procedure TSkLottieEditorForm.btnSaveClick(ASender: TObject);
begin
  if sdgSaveDialog.Execute and Assigned(FLottie) then
    FLottie.SaveToFile(sdgSaveDialog.FileName);
end;

procedure TSkLottieEditorForm.FormClose(ASender: TObject;
  var AAction: TCloseAction);
begin
  tmrRepaint.Enabled := False;
end;

procedure TSkLottieEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
  var AKeyChar: Char; AShift: TShiftState);
begin
  case AKey of
    vkEscape:
      begin
        AKey := 0;
        ModalResult := mrCancel;
      end;
    vkReturn:
      begin
        AKey := 0;
        ModalResult := mrOk;
      end;
  end;
end;

procedure TSkLottieEditorForm.LoadFromFile(const AFileName: string);
begin
  if Assigned(FLottie) and TFile.Exists(AFileName) then
  begin
    FLottie.LoadFromFile(AFileName);
    memSource.Lines.Text := FLottie.Source;
    memSourceChangeTracking(nil);
  end;
end;

procedure TSkLottieEditorForm.lytContentDragDrop(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF);
begin
  if (Length(AData.Files) = 1) and (AData.Files[0].EndsWith('.json', True) or AData.Files[0].EndsWith('.tgs', True)) then
    LoadFromFile(AData.Files[0]);
end;

procedure TSkLottieEditorForm.lytContentDragOver(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
begin
  if (Length(AData.Files) = 1) and (AData.Files[0].EndsWith('.json', True) or AData.Files[0].EndsWith('.tgs', True)) then
    AOperation := TDragOperation.Move;
end;

procedure TSkLottieEditorForm.memSourceChangeTracking(ASender: TObject);
begin
  btnSave.Enabled := not memSource.Lines.Text.Trim.IsEmpty;
  btnClear.Enabled := btnSave.Enabled;
  actCopy.Enabled := btnSave.Enabled;
  if Assigned(FLottie) and (tbcContent.TabIndex = 0) then
    FLottie.Source := memSource.Lines.Text.Trim;
end;

function TSkLottieEditorForm.ShowModal(var ASource: TSkLottieSource): TModalResult;
begin
  FLottie := TSkLottieAnimation.Create(Self);
  try
    FLottie.Align := TAlignLayout.Client;
    FLottie.FixedProgress := False;
    FLottie.Progress := 0;
    FLottie.Parent := rctPreviewBackground;
    memSource.Lines.Text := string(ASource).Trim;
    tbcContent.TabIndex := 0;
    memSourceChangeTracking(nil);
    tmrRepaint.Enabled := True;
    Result := inherited ShowModal;
    tmrRepaint.Enabled := False;
  finally
    FreeAndNil(FLottie);
  end;
  if Result = mrOk then
    ASource := memSource.Lines.Text.Trim;
end;

procedure TSkLottieEditorForm.tmrRepaintTimer(ASender: TObject);
begin
  tmrRepaint.Enabled := False;
  if (csDesigning in tmrRepaint.ComponentState) and Assigned(FLottie) then
  begin
    FLottie.Repaint;
    Application.ProcessMessages;
    tmrRepaint.Enabled := True;
  end;
end;

end.
