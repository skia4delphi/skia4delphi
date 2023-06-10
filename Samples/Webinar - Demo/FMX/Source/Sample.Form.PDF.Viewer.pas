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
unit Sample.Form.PDF.Viewer;

interface

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.WebBrowser,

  { Skia }
  System.Skia, FMX.Skia;

type
  { TfrmPDFViewer }

  TfrmPDFViewer = class(TForm)
    rctHeader: TRectangle;
    lblTitle: TSkLabel;
    btnBack: TSpeedButton;
    svgBackArrow: TSkSvg;
    wbrBrowser: TWebBrowser;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Show(AFileName: string); reintroduce;
  end;

var
  frmPDFViewer: TfrmPDFViewer;

implementation

uses
{$IFDEF MSWINDOWS}
  { Delphi }
  Winapi.Windows,
  Winapi.ShellAPI,
{$ELSEIF defined(LINUX)}
  { Delphi }
  Posix.Stdlib,
{$ELSEIF defined(ANDROID)}
  { Delphi }
  {$IF CompilerVersion < 34}
  System.IOUtils,
  {$ENDIF}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.Helpers,
{$ENDIF}

  { Sample }
  Sample.Form.Main;

{$R *.fmx}

procedure TfrmPDFViewer.btnBackClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPDFViewer.FormCreate(Sender: TObject);
begin
  StyleBook := frmMain.StyleBook;
end;

// iOS and macOS: It will open the PDF in a TWebBrowser
// Windows, Android and Linux: It will open in default app
procedure TfrmPDFViewer.Show(AFileName: string);
{$IFDEF ANDROID}
var
  LIntent: JIntent;
  LFile: JFile;
begin
  {$IF CompilerVersion < 34}
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   In RAD Studio Rio, the "Secure File Sharing" project option is not
  // -   correctly applied to the manifest, so files generated in the app's
  // -   internal folders cannot be shared with other apps. Then we will copy it
  // -   to a temporary folder, accessible to all other apps.
  // -
  // - -------------------------------------------------------------------------
  TFile.Copy(AFileName, TPath.Combine(TPath.GetTempPath, TPath.GetFileName(AFileName)), True);
  AFileName := TPath.Combine(TPath.GetTempPath, TPath.GetFileName(AFileName));
  // - -------------------------------------------------------------------------
  {$ENDIF}
  LFile := TJFile.JavaClass.init(StringToJString(AFileName));
  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(TAndroidHelper.JFileToJURI(LFile), StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
end;
{$ELSE}
begin
  {$IFDEF MSWINDOWS}
  AFileName := 'file://' + AFileName.Replace('\', '/');
  ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL);
  {$ELSEIF defined(LINUX)}
  _system(PAnsiChar('xdg-open "' + AnsiString(AFileName) + '"'));
  {$ELSE}
  AFileName := 'file://' + AFileName.Replace('\', '/');
  wbrBrowser.Navigate(AFileName);
  inherited Show;
  {$ENDIF}
end;
{$ENDIF}

end.
