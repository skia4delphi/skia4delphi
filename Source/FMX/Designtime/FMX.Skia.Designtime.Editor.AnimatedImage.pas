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
unit FMX.Skia.Designtime.Editor.AnimatedImage;

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
  { TSkAnimatedImageEditorForm }

  TSkAnimatedImageEditorForm = class(TForm)
    lytRightMenu: TLayout;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    btnOk: TSpeedButton;
    btnCancel: TSpeedButton;
    btnClear: TSpeedButton;
    rctPreviewBackground: TRectangle;
    sdgSaveDialog: TSaveDialog;
    odgOpenDialog: TOpenDialog;
    lytContent: TLayout;
    procedure btnClearClick(ASender: TObject);
    procedure btnOpenClick(ASender: TObject);
    procedure btnSaveClick(ASender: TObject);
    procedure FormCreate(ASender: TObject);
    procedure FormKeyDown(ASender: TObject; var AKey: Word; var AKeyChar: Char; AShift: TShiftState);
    procedure lytContentDragDrop(ASender: TObject; const AData: TDragObject; const APoint: TPointF);
    procedure lytContentDragOver(ASender: TObject; const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
  private
    FAnimatedImage: TSkAnimatedImage;
    FSupportedExtensions: TArray<string>;
    function IsSupportedFile(const AFileName: string): Boolean;
    procedure LoadFromFile(const AFileName: string);
    procedure UpdateButtons;
  public
    function ShowModal(var AData: TBytes): TModalResult; reintroduce;
  end;

implementation

uses
  { Delphi }
  System.Rtti,
  System.IOUtils,
  FMX.Platform;

{$R *.fmx}

{ TSkAnimatedImageEditorForm }

procedure TSkAnimatedImageEditorForm.btnClearClick(ASender: TObject);
begin
  FAnimatedImage.Source.Data := nil;
  UpdateButtons;
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
end;

procedure TSkAnimatedImageEditorForm.FormKeyDown(ASender: TObject; var AKey: Word;
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

procedure TSkAnimatedImageEditorForm.lytContentDragDrop(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF);
begin
  if (Length(AData.Files) = 1) and IsSupportedFile(AData.Files[0]) then
    LoadFromFile(AData.Files[0]);
end;

procedure TSkAnimatedImageEditorForm.lytContentDragOver(ASender: TObject;
  const AData: TDragObject; const APoint: TPointF; var AOperation: TDragOperation);
begin
  if (Length(AData.Files) = 1) and IsSupportedFile(AData.Files[0]) then
    AOperation := TDragOperation.Move;
end;

function TSkAnimatedImageEditorForm.ShowModal(var AData: TBytes): TModalResult;
begin
  odgOpenDialog.FilterIndex := 0;
  FAnimatedImage := TSkAnimatedImage.Create(Self);
  try
    FAnimatedImage.Align := TAlignLayout.Client;
    FAnimatedImage.Parent := rctPreviewBackground;
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

end.
