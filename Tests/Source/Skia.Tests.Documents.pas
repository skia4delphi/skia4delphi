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
unit Skia.Tests.Documents;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils,
  DUnitX.TestFramework,

  { Skia }
  Skia,

  { Tests }
  Skia.Tests.Foundation;

type
  { TSkDocumentsTests }

  [TestFixture]
  TSkDocumentsTests = class(TTestBase)
  protected
    function AssetsPath: string; override;
    procedure DocumentDraw(const ADocument: ISkDocument);
  public
    [Test]
    procedure TestPDF;
  end;

implementation

uses
  { Delphi }
  System.Classes,
  System.Types,
  System.UITypes,
  System.IOUtils,
  System.Math,
  System.Math.Vectors;

{ TSkDocumentsTests }

function TSkDocumentsTests.AssetsPath: string;
begin
  Result := CombinePaths(inherited AssetsPath, 'Documents');
end;

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
    Assert.IsNotNull(LDocument, 'Cannot possible to create the pdf document');
    DocumentDraw(LDocument);
    Assert.IsTrue(LDocumentStream.Size > MinDocumentFileSize, Format('The generated pdf file size does not seem to be correct (%d bytes)', [LDocumentStream.Size]));
  finally
    LDocumentStream.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TSkDocumentsTests);
end.
