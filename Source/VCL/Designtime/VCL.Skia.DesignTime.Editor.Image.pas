unit VCL.Skia.DesignTime.Editor.Image;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdActns, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  System.Skia,VCL.Skia;

type
  TSkImageEditorForm = class(TForm)
    imgBackgroundPicture: TImage;
    pnlRightMenu: TPanel;
    btnOpen: TButton;
    btnSave: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    pnlPreview: TPanel;
    imgBackground: TImage;
    odgOpenDialog: TOpenDialog;
    sdgSaveDialog: TSaveDialog;
    actActionList: TActionList;
    actCopy: TEditCopy;
    actPaste: TEditPaste;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
  private
    { Déclarations privées }
    FImage: ISkImage;
    procedure LoadFromFile(const AFileName: string);
    procedure WMDropFiles(var AMessage: TWMDropFiles); message WM_DROPFILES;
    function Getvalid_image: boolean;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure UpdateImage;
  public
    function ShowModal(var ASource: TBytes): TModalResult; reintroduce;

    property valid_image: boolean read Getvalid_image;
  end;

var
  SkImageEditorForm: TSkImageEditorForm;

implementation
uses
  { Delphi }
  System.Types,
  System.IOUtils,
  System.Math,
  WinApi.shellApi,
  {$IFDEF PACKAGE }
  {$IF (CompilerVersion >= 34) }
  BrandingAPI,
  {$ENDIF}
  {$ENDIF}
  Vcl.Clipbrd;

{$R *.dfm}

procedure TSkImageEditorForm.actCopyExecute(Sender: TObject);
begin
  if assigned(FImage) then
  begin
    var LBitmap := SkImageToBitmap(FImage);
    try
      Clipboard.Assign(LBitmap);
    finally
      LBitmap.Free;
    end;
  end;

end;

procedure TSkImageEditorForm.actPasteExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    var LBitmap := TBitmap.Create;
    try
      LBitmap.Assign(Clipboard);
      Fimage := LBitmap.ToSkImage;
      UpdateImage;
    finally
      LBitmap.Free;
    end;
  end else begin
    var LText := Clipboard.AsText.DeQuotedString('"');
    if FileExists(Ltext) then
        LoadFromFile(LText);
  end;
end;

procedure TSkImageEditorForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSkImageEditorForm.btnClearClick(Sender: TObject);
begin
  FImage := nil;
  UpdateImage;
end;

procedure TSkImageEditorForm.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSkImageEditorForm.btnOpenClick(Sender: TObject);
begin
  if odgOpenDialog.Execute then
    LoadFromFile(odgOpenDialog.FileName);
end;

procedure TSkImageEditorForm.btnSaveClick(Sender: TObject);
begin
  if valid_image and sdgSaveDialog.Execute then
  begin
    TSkImage(Fimage).EncodeToFile(sdgSaveDialog.FileName);
  end;
end;

procedure TSkImageEditorForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, True);
end;

procedure TSkImageEditorForm.DestroyWnd;
begin
  DragAcceptFiles(Handle, False);
  inherited;
end;

procedure TSkImageEditorForm.FormCreate(Sender: TObject);
begin
  {$IFDEF PACKAGE}
  {$IF CompilerVersion >= 34}
  if IDEThemeAvailable then
  begin
    IDEThemeManager.RegisterFormClass(TSkImageEditorForm);
    ThemeProperties.ApplyTheme(Self);
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TSkImageEditorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        ModalResult := mrCancel;
      end;
    VK_RETURN:
      begin
        Key := 0;
        ModalResult := mrOk;
      end;
  end;
end;

{$REGION ' - Drawing form background'}
procedure TSkImageEditorForm.FormResize(Sender: TObject);

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
          var LDestRect := TRectF.Create(0, 0, ATarget.Width, ATarget.Height);
          ACanvas.DrawRect(LDestRect, LPaint);
          if assigned(FImage) then
          begin
            ACanvas.DrawImageRect(FImage,LDestRect);
          end;
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

function TSkImageEditorForm.Getvalid_image: boolean;
begin
  result := Assigned(FImage) and (FImage.ImageInfo.Width > 0);
end;

procedure TSkImageEditorForm.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
  begin
    FImage := TSkImage.MakeFromEncodedFile(AFileName);
    UpdateImage;
  end;
end;

function TSkImageEditorForm.ShowModal(var ASource: TBytes): TModalResult;
begin
  if length(ASource) > 0 then
  begin
    Fimage := TSkImage.MakeFromEncoded(ASource);
  end;
  result := inherited ShowModal;
  if result = mrOk then
  begin
    if valid_image then
      aSource := TSkImage(FImage).Encode
    else
      aSource := [];
  end;
end;

procedure TSkImageEditorForm.UpdateImage;
begin
  btnSave.Enabled := valid_image;
  btnClear.Enabled := btnSave.Enabled;
  actCopy.Enabled := btnSave.Enabled;
  FormResize(Self);
end;

procedure TSkImageEditorForm.WMDropFiles(var AMessage: TWMDropFiles);
var
  LCount: Integer;
  LFileName: array[0..MAX_PATH - 1] of Char;
begin
  LCount := DragQueryFile(AMessage.Drop, $FFFFFFFF, LFileName, MAX_PATH);
  if (LCount = 1) and (DragQueryFile(AMessage.Drop, 0, LFileName, MAX_PATH) <> 0) then
  begin
    LoadFromFile(LFileName);
  end;
end;

end.
