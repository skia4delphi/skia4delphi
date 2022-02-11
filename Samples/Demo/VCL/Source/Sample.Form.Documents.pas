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
unit Sample.Form.Documents;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  Winapi.Windows, Winapi.ShellAPI, System.SysUtils, System.Classes,
  System.Types, System.UITypes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmDocuments = class(TfrmBase)
    pnlCreatePDFDrawingSVG: TPanel;
    lblCreatePDFDrawingSVGDescription: TSkLabel;
    lblCreatePDFDrawingSVGTitle: TSkLabel;
    svgCreatePDFDrawingSVGArrow: TSkSvg;
    pnlCreatePDFDrawingSVGLine: TPanel;
    pnlCreateXPSDrawingSVG: TPanel;
    lblCreateXPSDrawingSVGDescription: TSkLabel;
    lblCreateXPSDrawingSVGTitle: TSkLabel;
    svgCreateXPSDrawingSVGArrow: TSkSvg;
    pnlCreateXPSDrawingSVGLine: TPanel;
    procedure pnlCreatePDFDrawingSVGClick(Sender: TObject);
    procedure pnlCreateXPSDrawingSVGClick(Sender: TObject);
  private
    procedure ViewPDF(AFileName: string);
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmDocuments.pnlCreatePDFDrawingSVGClick(Sender: TObject);
var
  LCanvas: ISkCanvas;
  LDocument: ISkDocument;
  LDocumentFileName: string;
  LDocumentStream: TStream;
  LSVGDOM: ISkSVGDOM;
  LSize: TSizeF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(AssetsPath + 'lion.svg');
  LSize := TSizeF.Create(600, 600);
  LSVGDOM.SetContainerSize(LSize);

  LDocumentFileName := OutputPath + 'document.pdf';
  LDocumentStream := TFileStream.Create(LDocumentFileName, fmCreate);
  try
    LDocument := TSkDocument.MakePDF(LDocumentStream);
    try
      LCanvas := LDocument.BeginPage(LSize.Width, LSize.Height);
      try
        LSVGDOM.Render(LCanvas);
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LDocumentStream.Free;
  end;
  ViewPDF(LDocumentFileName);
end;

procedure TfrmDocuments.pnlCreateXPSDrawingSVGClick(Sender: TObject);
var
  LCanvas: ISkCanvas;
  LDocument: ISkDocument;
  LDocumentFileName: string;
  LDocumentStream: TStream;
  LSVGDOM: ISkSVGDOM;
  LSize: TSizeF;
begin
  LSVGDOM := TSkSVGDOM.MakeFromFile(AssetsPath + 'lion.svg');
  LSize := TSizeF.Create(600, 600);
  LSVGDOM.SetContainerSize(LSize);

  LDocumentFileName := OutputPath + 'document.xps';
  LDocumentStream := TFileStream.Create(LDocumentFileName, fmCreate);
  try
    LDocument := TSkDocument.MakeXPS(LDocumentStream);
    if LDocument = nil then
    begin
      Showmessage('This OS doesn''t support XPS!');
      Exit;
    end;
    try
      LCanvas := LDocument.BeginPage(LSize.Width, LSize.Height);
      try
        LSVGDOM.Render(LCanvas);
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LDocumentStream.Free;
  end;
  Showmessage('Created XPS file!');
end;

procedure TfrmDocuments.ViewPDF(AFileName: string);
begin
  AFileName := 'file://' + AFileName.Replace('\', '/');
  ShellExecute(0, 'open', PChar(AFileName), nil, nil, SW_SHOWNORMAL);
end;

end.
