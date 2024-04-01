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
unit Skia.Tests.Documents;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  System.Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkDocumentsTests }

  [TestFixture]
  TSkDocumentsTests = class(TTestBase)
  protected
    procedure DocumentDraw(const ADocument: ISkDocument);
  public
    [Test]
    procedure TestPDF;
    {$IFDEF MSWINDOWS}
    [Test]
    procedure TestXPS;
    {$ENDIF}
  end;

implementation

uses
  { Delphi }
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  {$ENDIF}
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkDocumentsTests }

procedure TSkDocumentsTests.DocumentDraw(const ADocument: ISkDocument);
var
  LCanvas: ISkCanvas;
  LOval: ISkRoundRect;
  LPaint: ISkPaint;
  LRect: TRectF;
begin
  try
    LCanvas := ADocument.BeginPage(500, 500);
    try
      LPaint := TSkPaint.Create;
      LPaint.Style := TSkPaintStyle.Fill;
      LPaint.AntiAlias := True;
      LPaint.StrokeWidth := 4;
      LPaint.Color := $FF4285F4;

      LRect := TRectF.Create(TPointF.Create(10, 10), 100, 160);
      LCanvas.DrawRect(LRect, LPaint);

      LOval := TSkRoundRect.Create;
      LOval.SetOval(LRect);
      LOval.Offset(40, 80);
      LPaint.Color := $FFDB4437;
      LCanvas.DrawRoundRect(LOval, LPaint);

      LPaint.Color := $FF0F9D58;
      LCanvas.DrawCircle(180, 50, 25, LPaint);

      LRect.Offset(80, 50);
      LPaint.Color := $FFF4B400;
      LPaint.Style := TSkPaintStyle.Stroke;
      LCanvas.DrawRoundRect(LRect, 10, 10, LPaint);
    finally
      ADocument.EndPage;
    end;
  finally
    ADocument.Close;
  end;
end;

procedure TSkDocumentsTests.TestPDF;
const
  MinDocumentFileSize = 1000;
var
  LDocument: ISkDocument;
  LDocumentStream: TStream;
begin
  LDocumentStream := TMemoryStream.Create;
  try
    LDocument := TSkDocument.MakePDF(LDocumentStream);
    Assert.IsNotNull(LDocument, 'Cannot possible to create the PDF document');
    DocumentDraw(LDocument);
    Assert.IsTrue(LDocumentStream.Size > MinDocumentFileSize, Format('The generated PDF file size does not seem to be correct (%d bytes)', [LDocumentStream.Size]));
  finally
    LDocumentStream.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TSkDocumentsTests.TestXPS;
const
  MinDocumentFileSize = 1000;
var
  LDocument: ISkDocument;
  LDocumentStream: TStream;
begin
  // For some reason we couldn't use COM objects in DUnitX console tests
  if not IsConsole then
  begin
    CoInitialize(nil);
    try
      LDocumentStream := TMemoryStream.Create;
      try
        LDocument := TSkDocument.MakeXPS(LDocumentStream);
        Assert.IsNotNull(LDocument, 'Cannot possible to create the XPS document');
        DocumentDraw(LDocument);
        Assert.IsTrue(LDocumentStream.Size > MinDocumentFileSize, Format('The generated XPS file size does not seem to be correct (%d bytes)', [LDocumentStream.Size]));
      finally
        LDocumentStream.Free;
      end;
    finally
      CoUnInitialize();
    end;
  end;
end;
{$ENDIF}

initialization
  TDUnitX.RegisterTestFixture(TSkDocumentsTests);
end.
