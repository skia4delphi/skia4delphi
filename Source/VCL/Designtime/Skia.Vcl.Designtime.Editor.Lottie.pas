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
unit Skia.Vcl.Designtime.Editor.Lottie;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ActnList,
  Vcl.StdActns,
  Vcl.StdCtrls,

  { Skia }
  Skia.Vcl;

type
  { TSkLottieEditorForm }

  TSkLottieEditorForm = class(TForm)
    pnlRightMenu: TPanel;
    btnOpen: TButton;
    btnSave: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    tbcContent: TTabControl;
    odgOpenDialog: TOpenDialog;
    sdgSaveDialog: TSaveDialog;
    imgBackgroundPicture: TImage;
    imgBackground: TImage;
    actActionList: TActionList;
    actCopy: TEditCopy;
    actPaste: TEditPaste;
    memSource: TMemo;
    pnlPreview: TPanel;
    tmrRepaint: TTimer;
    procedure actCopyExecute(ASender: TObject);
    procedure actPasteExecute(ASender: TObject);
    procedure btnCancelClick(ASender: TObject);
    procedure btnClearClick(ASender: TObject);
    procedure btnOkClick(ASender: TObject);
    procedure btnOpenClick(ASender: TObject);
    procedure btnSaveClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure FormKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure FormResize(ASender: TObject);
    procedure memSourceChange(ASender: TObject);
    procedure tbcContentChange(ASender: TObject);
    procedure tmrRepaintTimer(ASender: TObject);
  private
    FLottie: TSkLottieAnimation;
    procedure LoadFromFile(const AFileName: string);
    procedure WMDropFiles(var AMessage: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    function ShowModal(var ASource: TSkLottieSource): TModalResult; reintroduce;
  end;

implementation

uses
  { Delphi }
  System.IOUtils,
  System.Math,
  Vcl.Clipbrd,
  {$IF CompilerVersion >= 34}
  BrandingAPI,
  {$ENDIF}

  { Skia }
  Skia;

{$R *.dfm}

{ TSkLottieEditorForm }

procedure TSkLottieEditorForm.actCopyExecute(ASender: TObject);
begin
  if actCopy.Enabled and (not memSource.Focused) and Assigned(FLottie) then
  begin
    case tbcContent.TabIndex of
      0: Clipboard.AsText := FLottie.Source;
      1: Clipboard.AsText := memSource.Lines.Text;
    else
    end;
  end;
end;

procedure TSkLottieEditorForm.actPasteExecute(ASender: TObject);
var
  LText: string;
begin
  if (not memSource.Focused) and Assigned(FLottie) then
  begin
    LText := Clipboard.AsText.Trim;
    if LText.StartsWith('{') and LText.EndsWith('}') then
    begin
      case tbcContent.TabIndex of
        0: FLottie.Source := LText;
        1:
          begin
            memSource.Lines.Text := LText;
            memSourceChange(nil);
          end;
      else
      end;
    end;
  end;
end;

procedure TSkLottieEditorForm.btnCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSkLottieEditorForm.btnClearClick(ASender: TObject);
begin
  if Assigned(FLottie) then
  begin
    case tbcContent.TabIndex of
      0: FLottie.Source := '';
      1: memSource.Lines.Clear;
    else
    end;
    memSourceChange(nil);
  end;
end;

procedure TSkLottieEditorForm.btnOkClick(ASender: TObject);
begin
  ModalResult := mrOk;
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

procedure TSkLottieEditorForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, True);
end;

procedure TSkLottieEditorForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TSkLottieEditorForm.FormCreate(ASender: TObject);
begin
  {$IF CompilerVersion >= 34}
  if IDEThemeAvailable then
  begin
    IDEThemeManager.RegisterFormClass(TSkLottieEditorForm);
    ThemeProperties.ApplyTheme(Self);
  end;
  {$ENDIF}
end;

procedure TSkLottieEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);
begin
  case AKey of
    VK_ESCAPE:
      begin
        AKey := 0;
        ModalResult := mrCancel;
      end;
    VK_RETURN:
      begin
        AKey := 0;
        ModalResult := mrOk;
      end;
  end;
end;

{$REGION ' - Drawing form background'}
procedure TSkLottieEditorForm.FormResize(ASender: TObject);

  procedure TileImage(const ASource, ATarget: TImage);
  var
    LBitmap: TBitmap;
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.SetSize(ATarget.Width, ATarget.Height);
      LBitmap.SkiaDraw(
        procedure(const ACanvas: ISkCanvas)
        var
          LImage: ISkImage;
          LPaint: ISkPaint;
          LBitmap: TBitmap;
        begin
          LBitmap := TBitmap.Create;
          try
            LBitmap.Assign(imgBackgroundPicture.Picture.Graphic);
            LImage := LBitmap.ToSkImage;
          finally
            LBitmap.Free;
          end;
          LPaint := TSkPaint.Create;
          LPaint.Shader := LImage.MakeShader(TSkTileMode.Replicate, TSkTileMode.Replicate);
          LPaint.Style := TSkPaintStyle.Fill;
          ACanvas.DrawRect(ACanvas.GetLocalClipBounds, LPaint);
        end);
      ATarget.Picture.Bitmap := LBitmap;
    finally
      LBitmap.Free;
    end;
  end;

begin
  if ((imgBackground.Width <> Screen.Width) and (imgBackground.Width < pnlPreview.Width * 1.5)) or
    ((imgBackground.Height <> Screen.Height) and (imgBackground.Height < pnlPreview.Height * 1.5)) then
  begin
    imgBackground.SetBounds(imgBackground.Left, imgBackground.Top, Min(Screen.Width, pnlPreview.Width * 3), Min(Screen.Height, pnlPreview.Height * 3));
    TileImage(imgBackgroundPicture, imgBackground);
  end;
end;
{$ENDREGION}

procedure TSkLottieEditorForm.LoadFromFile(const AFileName: string);
begin
  if Assigned(FLottie) and TFile.Exists(AFileName) then
  begin
    FLottie.LoadFromFile(AFileName);
    if memSource.Visible then
      memSource.Lines.Text := FLottie.Source;
    memSourceChange(nil);
  end;
end;

procedure TSkLottieEditorForm.memSourceChange(ASender: TObject);
begin
  if Assigned(FLottie) and (tbcContent.TabIndex = 0) then
    btnSave.Enabled := not string(FLottie.Source).Trim.IsEmpty
  else
    btnSave.Enabled := not memSource.Lines.Text.Trim.IsEmpty;
  btnClear.Enabled := btnSave.Enabled;
  actCopy.Enabled := btnSave.Enabled;
  if Assigned(ASender) and Assigned(FLottie) and (tbcContent.TabIndex = 0) then
    FLottie.Source := memSource.Lines.Text.Trim;
end;

function TSkLottieEditorForm.ShowModal(var ASource: TSkLottieSource): TModalResult;
begin
  FLottie := TSkLottieAnimation.Create(Self);
  try
    memSource.Lines.Clear;
    FLottie.Align := TAlign.alClient;
    FLottie.FixedProgress := False;
    FLottie.Progress := 0;
    FLottie.Parent := pnlPreview;
    FLottie.BringToFront;
    FLottie.Source := ASource;
    tbcContent.TabIndex := 0;
    tmrRepaint.Enabled := True;
    memSourceChange(nil);
    Result := inherited ShowModal;
    tmrRepaint.Enabled := False;
    if Result = mrOk then
    begin
      if memSource.Visible then
        ASource := memSource.Lines.Text.Trim
      else
        ASource := FLottie.Source;
    end;
  finally
    FreeAndNil(FLottie);
  end;
end;

procedure TSkLottieEditorForm.tbcContentChange(ASender: TObject);
begin
  pnlPreview.Visible := tbcContent.TabIndex = 0;
  memSource.Visible := tbcContent.TabIndex = 1;
  if Assigned(FLottie) then
  begin
    if memSource.Visible then
      memSource.Lines.Text := FLottie.Source
    else
      FLottie.Source := memSource.Lines.Text.Trim;
  end;
end;

procedure TSkLottieEditorForm.tmrRepaintTimer(ASender: TObject);
begin
  if (csDesigning in tmrRepaint.ComponentState) and Assigned(FLottie) then
    FLottie.Repaint;
end;

procedure TSkLottieEditorForm.WMDropFiles(var AMessage: TWMDropFiles);
var
  LCount: Integer;
  LFileName: array[0..MAX_PATH - 1] of Char;
begin
  LCount := DragQueryFile(AMessage.Drop, $FFFFFFFF, LFileName, MAX_PATH);
  if (LCount = 1) and (DragQueryFile(AMessage.Drop, 0, LFileName, MAX_PATH) <> 0) and
    (string(LFileName).EndsWith('.json', True) or string(LFileName).EndsWith('.tgs', True)) then
  begin
    LoadFromFile(LFileName);
  end;
end;

end.

