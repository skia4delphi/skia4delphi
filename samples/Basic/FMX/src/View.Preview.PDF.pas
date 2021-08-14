unit View.Preview.PDF;

interface

uses
  { Delphi }
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF}
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Variants,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Objects,
  FMX.Types,
  FMX.WebBrowser,
  FMX.Forms,
  FMX.Layouts,
  FMX.ListBox,
  {$IF CompilerVersion >= 31}
  FMX.DialogService,
  {$ELSE}
  FMX.Dialogs,
  {$ENDIF}
  FMX.Graphics;

type
  { TfrmPDFPreview }

  TfrmPDFPreview = class(TForm)
    wbrBrowser: TWebBrowser;
    rctHeader: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    imgArrow: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmPDFPreview: TfrmPDFPreview;

implementation

{$R *.fmx}

uses
  { Sample }
  View.Main;

{ TfrmPDFPreview }

procedure TfrmPDFPreview.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPDFPreview.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
end;

procedure TfrmPDFPreview.Show(const AFileName: string);
{$IFDEF ANDROID}
begin
  {$IF CompilerVersion >= 31}TDialogService.{$ENDIF}ShowMessage(Format('The file was succesfull writted to "%s"', [AFileName]));
{$ELSE}
var
  LURL: string;
begin
  LURL := AFileName.Replace('\', '/', [rfReplaceAll]);
  LURL := 'file://' + LURL;
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(LURL), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  wbrBrowser.Navigate(LURL);
  inherited Show;
  {$ENDIF}
{$ENDIF}
end;

end.
