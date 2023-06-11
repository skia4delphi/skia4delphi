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
unit FMX.Skia.Designtime.Editor.SVG;

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
  FMX.Controls.Presentation,

  { Skia }
  FMX.Skia;

type
  { TSkSvgEditorForm }

  TSkSvgEditorForm = class(TForm)
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
    lytContent: TLayout;
    procedure actCopyExecute(ASender: TObject);
    procedure actPasteExecute(ASender: TObject);
    procedure btnClearClick(ASender: TObject);
    procedure btnOpenClick(ASender: TObject);
    procedure btnSaveClick(ASender: TObject);
    procedure FormKeyDown(ASender: TObject; var AKey: Word; var AKeyChar: Char; AShift: TShiftState);
    procedure lytContentDragDrop(ASender: TObject; const AData: TDragObject; const APoint: TPointF);
    procedure lytContentDragOver(ASender: TObject; const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
    procedure memSourceChangeTracking(ASender: TObject);
  private
    FSvg: TSkSvg;
    procedure LoadFromFile(const AFileName: string);
  public
    function ShowModal(var ASource: TSkSvgSource): TModalResult; reintroduce;
  end;

implementation

uses
  { Delphi }
  System.Rtti,
  System.IOUtils,
  FMX.Platform;

{$R *.fmx}

{ TSkSvgEditorForm }

procedure TSkSvgEditorForm.actCopyExecute(ASender: TObject);
var
  LServiceInterface: IInterface;
  LClipboardService: IFMXClipboardService;
begin
  if actCopy.Enabled and (not memSource.IsFocused) and
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    LClipboardService.SetClipboard(memSource.Lines.Text);
  end;
end;

procedure TSkSvgEditorForm.actPasteExecute(ASender: TObject);
var
  LServiceInterface: IInterface;
  LClipboardService: IFMXClipboardService;
  LValue: TValue;
  LText: string;
begin
  if (not memSource.IsFocused) and TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, LServiceInterface) and
    Supports(LServiceInterface, IFMXClipboardService, LClipboardService) then
  begin
    LValue := LClipboardService.GetClipboard;
    if LValue.IsType<string> then
    begin
      LText := LValue.AsString.Trim;
      if LText.Contains('<svg') and LText.EndsWith('</svg>') then
      begin
        memSource.Lines.Text := LText;
        memSourceChangeTracking(nil);
      end;
    end;
  end;
end;

procedure TSkSvgEditorForm.btnClearClick(ASender: TObject);
begin
  memSource.Lines.Clear;
end;

procedure TSkSvgEditorForm.btnOpenClick(ASender: TObject);
begin
  if odgOpenDialog.Execute then
    LoadFromFile(odgOpenDialog.FileName);
end;

procedure TSkSvgEditorForm.btnSaveClick(ASender: TObject);
begin
  if sdgSaveDialog.Execute then
    memSource.Lines.SaveToFile(sdgSaveDialog.FileName, TEncoding.UTF8);
end;

procedure TSkSvgEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
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

procedure TSkSvgEditorForm.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    memSource.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
end;

procedure TSkSvgEditorForm.lytContentDragDrop(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF);
begin
  if (Length(AData.Files) = 1) and (AData.Files[0].EndsWith('.svg')) then
    LoadFromFile(AData.Files[0]);
end;

procedure TSkSvgEditorForm.lytContentDragOver(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
begin
  if (Length(AData.Files) = 1) and (AData.Files[0].EndsWith('.svg')) then
    AOperation := TDragOperation.Move;
end;

procedure TSkSvgEditorForm.memSourceChangeTracking(ASender: TObject);
begin
  btnSave.Enabled := not memSource.Lines.Text.Trim.IsEmpty;
  btnClear.Enabled := btnSave.Enabled;
  actCopy.Enabled := btnSave.Enabled;
  if Assigned(FSvg) and (tbcContent.TabIndex = 0) then
    FSvg.Svg.Source := memSource.Lines.Text.Trim;
end;

function TSkSvgEditorForm.ShowModal(var ASource: TSkSvgSource): TModalResult;
begin
  FSvg := TSkSvg.Create(Self);
  try
    FSvg.Align := TAlignLayout.Client;
    FSvg.Parent := rctPreviewBackground;
    memSource.Lines.Text := string(ASource).Trim;
    tbcContent.TabIndex := 0;
    memSourceChangeTracking(nil);
    Result := inherited ShowModal;
  finally
    FreeAndNil(FSvg);
  end;
  if Result = mrOk then
    ASource := memSource.Lines.Text.Trim;
end;

end.
