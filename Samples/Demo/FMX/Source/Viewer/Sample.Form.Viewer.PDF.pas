{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2025 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit Sample.Form.Viewer.PDF;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.WebBrowser, FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base.Viewer;

type
  TfrmPDFViewer = class(TfrmBaseViewer)
    wbrBrowser: TWebBrowser;
  public
    procedure Show(const ATitle, ADescription: string; AFileName: string); reintroduce;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  { Delphi }
  Winapi.Windows,
  Winapi.ShellAPI,
  System.IOUtils;
{$ELSEIF defined(LINUX)}
uses
  { Delphi }
  Posix.Stdlib;
{$ELSEIF defined(ANDROID)}
uses
  { Delphi }
  {$IF CompilerVersion < 34}
  System.IOUtils,
  {$ENDIF}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.Helpers;
{$ENDIF}

{$R *.fmx}

{ TfrmPDFViewer }

procedure TfrmPDFViewer.Show(const ATitle, ADescription: string;
  AFileName: string);
{$IFDEF ANDROID}
var
  LIntent: JIntent;
  LFile: JFile;
begin
  {$REGION ' - Workaround RSP-23512'}
  // - -------------------------------------------------------------------------
  // - WORKAROUND
  // - -------------------------------------------------------------------------
  // -
  // - Description:
  // -   In RAD Studio Rio, the "Secure File Sharing" project option is not
  // -   correctly applied to the manifest, so files generated in the app's
  // -   internal folders cannot be shared with other apps. Then we will copy it
  // -   to a temporary folder, accessible to all other apps.
  // -
  // - Bug report:
  // -   https://quality.embarcadero.com/browse/RSP-23512
  // -
  // - -------------------------------------------------------------------------
  {$IF CompilerVersion < 34}
  TFile.Copy(AFileName, TPath.Combine(TPath.GetTempPath, TPath.GetFileName(AFileName)), True);
  AFileName := TPath.Combine(TPath.GetTempPath, TPath.GetFileName(AFileName));
  {$ENDIF}
  // - -------------------------------------------------------------------------
  {$ENDREGION}
  LFile := TJFile.JavaClass.init(StringToJString(AFileName));
  LIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
  LIntent.setDataAndType(TAndroidHelper.JFileToJURI(LFile), StringToJString('application/pdf'));
  LIntent.setFlags(TJIntent.JavaClass.FLAG_GRANT_READ_URI_PERMISSION);
  TAndroidHelper.Activity.startActivity(LIntent);
end;
{$ELSE}
begin
  {$IFDEF MSWINDOWS}
  AFileName := 'file://' + TPath.GetFullPath(AFileName).Replace('\', '/');
  ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL);
  {$ELSEIF defined(LINUX)}
  _system(PAnsiChar('xdg-open "' + AnsiString(AFileName) + '"'));
  {$ELSE}
  AFileName := 'file://' + AFileName.Replace('\', '/');
  BackgroundKind := TBackgroundKind.Solid;
  wbrBrowser.Navigate(AFileName);
  inherited Show(ATitle, ADescription);
  {$ENDIF}
end;
{$ENDIF}

end.
