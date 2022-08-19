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
unit Skia.Vcl.Designtime.Editor.SVG;

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
  { TSkSvgEditorForm }

  TSkSvgEditorForm = class(TForm)
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
  private
    FSvg: TSkSvg;
    procedure LoadFromFile(const AFileName: string);
    procedure WMDropFiles(var AMessage: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    function ShowModal(var ASource: TSkSvgSource): TModalResult; reintroduce;
  end;

implementation

uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.Math,
  Vcl.Clipbrd,
  {$IF CompilerVersion >= 34}
  BrandingAPI,
  {$ENDIF}

  { Skia }
  Skia;

{$R *.dfm}

{ TSkSvgEditorForm }

procedure TSkSvgEditorForm.actCopyExecute(ASender: TObject);
begin
  if actCopy.Enabled and not memSource.Focused then
    Clipboard.AsText := memSource.Lines.Text;
end;

procedure TSkSvgEditorForm.actPasteExecute(ASender: TObject);
var
  LText: string;
begin
  if not memSource.Focused then
  begin
    LText := Clipboard.AsText.Trim;
    if LText.Contains('<svg') and LText.EndsWith('</svg>') then
    begin
      memSource.Lines.Text := LText;
      memSourceChange(nil);
    end;
  end;
end;

procedure TSkSvgEditorForm.btnCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSkSvgEditorForm.btnClearClick(ASender: TObject);
begin
  memSource.Lines.Clear;
  memSourceChange(nil);
end;

procedure TSkSvgEditorForm.btnOkClick(ASender: TObject);
begin
  ModalResult := mrOk;
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

procedure TSkSvgEditorForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, True);
end;

procedure TSkSvgEditorForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TSkSvgEditorForm.FormCreate(ASender: TObject);
begin
  {$IF CompilerVersion >= 34}
  if IDEThemeAvailable then
  begin
    IDEThemeManager.RegisterFormClass(TSkSvgEditorForm);
    ThemeProperties.ApplyTheme(Self);
  end;
  {$ENDIF}
end;

procedure TSkSvgEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
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
procedure TSkSvgEditorForm.FormResize(ASender: TObject);

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
          LPaint.Shader := LImage.MakeShader(TSkTileMode.Repeat, TSkTileMode.Repeat);
          LPaint.Style := TSkPaintStyle.Fill;
          ACanvas.DrawRect(TRectF.Create(0, 0, ATarget.Width, ATarget.Height), LPaint);
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

procedure TSkSvgEditorForm.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    memSource.Lines.LoadFromFile(AFileName, TEncoding.UTF8);
end;

procedure TSkSvgEditorForm.memSourceChange(ASender: TObject);
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
    FSvg.Align := TAlign.alClient;
    FSvg.Parent := pnlPreview;
    FSvg.BringToFront;
    memSource.Lines.Text := string(ASource).Trim;
    tbcContent.TabIndex := 0;
    memSourceChange(nil);
    Result := inherited ShowModal;
  finally
    FreeAndNil(FSvg);
  end;
  if Result = mrOk then
    ASource := memSource.Lines.Text.Trim;
end;

procedure TSkSvgEditorForm.tbcContentChange(ASender: TObject);
begin
  pnlPreview.Visible := tbcContent.TabIndex = 0;
  memSource.Visible := tbcContent.TabIndex = 1;
end;

procedure TSkSvgEditorForm.WMDropFiles(var AMessage: TWMDropFiles);
var
  LCount: Integer;
  LFileName: array[0..MAX_PATH - 1] of Char;
begin
  LCount := DragQueryFile(AMessage.Drop, $FFFFFFFF, LFileName, MAX_PATH);
  if (LCount = 1) and (DragQueryFile(AMessage.Drop, 0, LFileName, MAX_PATH) <> 0) and
    string(LFileName).EndsWith('.svg', True) then
  begin
    LoadFromFile(LFileName);
  end;
end;

end.

