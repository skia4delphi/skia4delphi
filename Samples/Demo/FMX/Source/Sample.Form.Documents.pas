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
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmDocuments = class(TfrmBase)
    btnCreateXPSDrawingSVG: TSpeedButton;
    lblCreateXPSDrawingSVGTitle: TSkLabel;
    lblCreateXPSDrawingSVGDescription: TSkLabel;
    btnCreatePDFDrawingSVG: TSpeedButton;
    lblCreatePDFDrawingSVGTitle: TSkLabel;
    lblCreatePDFDrawingSVGDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnCreatePDFDrawingSVGClick(Sender: TObject);
    procedure btnCreateXPSDrawingSVGClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.PDF;

{$R *.fmx}

procedure TfrmDocuments.btnCreatePDFDrawingSVGClick(Sender: TObject);
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
  ChildForm<TfrmPDFViewer>.Show('Creating PDF', '', LDocumentFileName);
end;

procedure TfrmDocuments.btnCreateXPSDrawingSVGClick(Sender: TObject);
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

end.
