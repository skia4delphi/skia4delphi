unit View.Preview.PDF;

interface

uses
  { Delphi }
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ELSEIF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.Helpers,
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
  FMX.Graphics;

type
  { TfrmPDFPreview }

  TfrmPDFPreview = class(TForm)
    wbrBrowser: TWebBrowser;
    rctHeader: TRectangle;
    btnClose: TSpeedButton;
    lblTitle: TLabel;
    imgArrow: TImage;
    procedure btnCloseClick(Sender: TObject);
  public
    procedure Show(const AFileName: string); reintroduce;
  end;

var
  frmPDFPreview: TfrmPDFPreview;

implementation

{$R *.fmx}

{ TfrmPDFPreview }

procedure TfrmPDFPreview.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPDFPreview.Show(const AFileName: string);
{$IFDEF ANDROID}
var
  LIntent: JIntent;
  LFile: JFile;
begin
  LFile := TJFile.JavaClass.init(StringToJString(AFileName));
  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(TAndroidHelper.JFileToJURI(LFile), StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
end;
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
end;
{$ENDIF}

end.
