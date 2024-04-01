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
unit Vcl.Skia.Designtime.Editor.AnimatedImage;

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
  Vcl.Skia;

type
  { TSkAnimatedImageEditorForm }

  TSkAnimatedImageEditorForm = class(TForm)
    pnlRightMenu: TPanel;
    btnOpen: TButton;
    btnSave: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    odgOpenDialog: TOpenDialog;
    sdgSaveDialog: TSaveDialog;
    imgBackgroundPicture: TImage;
    imgBackground: TImage;
    pnlPreview: TPanel;
    procedure btnCancelClick(ASender: TObject);
    procedure btnClearClick(ASender: TObject);
    procedure btnOkClick(ASender: TObject);
    procedure btnOpenClick(ASender: TObject);
    procedure btnSaveClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure FormKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure FormResize(ASender: TObject);
  private
    FAnimatedImage: TSkAnimatedImage;
    FSupportedExtensions: TArray<string>;
    function IsSupportedFile(const AFileName: string): Boolean;
    procedure LoadFromFile(const AFileName: string);
    procedure UpdateButtons;
    procedure WMDropFiles(var AMessage: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    function ShowModal(var AData: TBytes): TModalResult; reintroduce;
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
  System.Skia;

{$R *.dfm}

{ TSkAnimatedImageEditorForm }

procedure TSkAnimatedImageEditorForm.btnCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSkAnimatedImageEditorForm.btnClearClick(ASender: TObject);
begin
  FAnimatedImage.Source.Data := nil;
  UpdateButtons;
end;

procedure TSkAnimatedImageEditorForm.btnOkClick(ASender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSkAnimatedImageEditorForm.btnOpenClick(ASender: TObject);
begin
  if odgOpenDialog.Execute then
    LoadFromFile(odgOpenDialog.FileName);
end;

procedure TSkAnimatedImageEditorForm.btnSaveClick(ASender: TObject);
begin
  if sdgSaveDialog.Execute and Assigned(FAnimatedImage) then
    TFile.WriteAllBytes(sdgSaveDialog.FileName, FAnimatedImage.Source.Data);
end;

procedure TSkAnimatedImageEditorForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, True);
end;

procedure TSkAnimatedImageEditorForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TSkAnimatedImageEditorForm.FormCreate(ASender: TObject);

  function GetOpenDialogFilter: string;
  const
    AllImagesDescription = 'All animated images';
  var
    LCodecClass: TSkAnimatedImage.TAnimationCodecClass;
    LFormat: TSkAnimatedImage.TFormatInfo;
    LAllExtensions: string;
    LExtensions: string;
  begin
    Result := '';
    LAllExtensions := '';
    for LCodecClass in TSkAnimatedImage.RegisteredCodecs do
    begin
      for LFormat in LCodecClass.SupportedFormats do
      begin
        LExtensions := string('').Join(';', LFormat.Extensions);
        Result := Result + Format('|%s (%s)|%s', [LFormat.Description, LExtensions, LExtensions]);
        if not LAllExtensions.IsEmpty then
          LAllExtensions := LAllExtensions + ';';
        LAllExtensions := LAllExtensions + LExtensions;
      end;
    end;
    Result := Format('%s (%s)|%s', [AllImagesDescription, LAllExtensions, LAllExtensions]) + Result;
    Result := Result.Replace('.', '*.');
  end;

  function GetSupportedExtensions: TArray<string>;
  var
    LCodecClass: TSkAnimatedImage.TAnimationCodecClass;
    LFormat: TSkAnimatedImage.TFormatInfo;
    LExtension: string;
  begin
    Result := [];
    for LCodecClass in TSkAnimatedImage.RegisteredCodecs do
      for LFormat in LCodecClass.SupportedFormats do
        for LExtension in LFormat.Extensions do
          Result := Result + [LExtension];
  end;

begin
  odgOpenDialog.Filter := GetOpenDialogFilter;
  FSupportedExtensions := GetSupportedExtensions;

  {$IF CompilerVersion >= 34}
  if IDEThemeAvailable then
  begin
    IDEThemeManager.RegisterFormClass(TSkAnimatedImageEditorForm);
    ThemeProperties.ApplyTheme(Self);
  end;
  {$ENDIF}
end;

procedure TSkAnimatedImageEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
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
procedure TSkAnimatedImageEditorForm.FormResize(ASender: TObject);

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

function TSkAnimatedImageEditorForm.IsSupportedFile(
  const AFileName: string): Boolean;
var
  LExtension: string;
begin
  for LExtension in FSupportedExtensions do
    if AFileName.EndsWith(LExtension, True) then
      Exit(True);
  Result := False;
end;

procedure TSkAnimatedImageEditorForm.LoadFromFile(const AFileName: string);
begin
  if Assigned(FAnimatedImage) and TFile.Exists(AFileName) then
  begin
    FAnimatedImage.LoadFromFile(AFileName);
    UpdateButtons;
  end;
end;

function TSkAnimatedImageEditorForm.ShowModal(var AData: TBytes): TModalResult;
begin
  odgOpenDialog.FilterIndex := 0;
  FAnimatedImage := TSkAnimatedImage.Create(Self);
  try
    FAnimatedImage.Align := TAlign.alClient;
    FAnimatedImage.Parent := pnlPreview;
    FAnimatedImage.BringToFront;
    FAnimatedImage.Source.Data := AData;
    UpdateButtons;
    Result := inherited ShowModal;
    if Result = mrOk then
      AData := FAnimatedImage.Source.Data;
  finally
    FreeAndNil(FAnimatedImage);
  end;
end;

procedure TSkAnimatedImageEditorForm.UpdateButtons;

  function TrySetSaveDialogFilter: Boolean;
  const
    AllFilesDescription = 'All files';
  var
    LFilter: string;
    LExtensions: string;
    LCodecClass: TSkAnimatedImage.TAnimationCodecClass;
    LFormat: TSkAnimatedImage.TFormatInfo;
  begin
    LFilter := '';
    for LCodecClass in TSkAnimatedImage.RegisteredCodecs do
    begin
      if LCodecClass.TryDetectFormat(FAnimatedImage.Source.Data, LFormat) then
      begin
        LExtensions := string('').Join(';', LFormat.Extensions);
        LFilter := Format('|%s (%s)|%s', [LFormat.Description, LExtensions, LExtensions]);
        Break;
      end;
    end;
    if LFilter.IsEmpty then
      Exit(False);
    sdgSaveDialog.Filter := Format('%s (*.*)|*.*', [AllFilesDescription]) + LFilter.Replace('.', '*.');
    sdgSaveDialog.FilterIndex := 1;
    Result := True;
  end;

begin
  btnSave.Enabled := Assigned(FAnimatedImage) and (FAnimatedImage.Source.Data <> nil) and TrySetSaveDialogFilter;
  btnClear.Enabled := btnSave.Enabled;
end;

procedure TSkAnimatedImageEditorForm.WMDropFiles(var AMessage: TWMDropFiles);
var
  LCount: Integer;
  LFileName: array[0..MAX_PATH - 1] of Char;
begin
  LCount := DragQueryFile(AMessage.Drop, $FFFFFFFF, LFileName, MAX_PATH);
  if (LCount = 1) and (DragQueryFile(AMessage.Drop, 0, LFileName, MAX_PATH) <> 0) and
    IsSupportedFile(string(LFileName)) then
  begin
    LoadFromFile(LFileName);
  end;
end;

end.
