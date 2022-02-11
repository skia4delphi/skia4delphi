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
unit Sample.Form.Image;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.Types, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmImage = class(TfrmBase)
    pnlEncodeWebpVsJpeg: TPanel;
    lblEncodeWebpVsJpegDescription: TSkLabel;
    lblEncodeWebpVsJpegTitle: TSkLabel;
    svgEncodeWebpVsJpegArrow: TSkSvg;
    pnlEncodeWebpVsJpegLine: TPanel;
    pnlNinePatch: TPanel;
    lblNinePatchDescription: TSkLabel;
    lblNinePatchTitle: TSkLabel;
    svgNinePatchArrow: TSkSvg;
    pnlNinePatchLine: TPanel;
    pnlWebpInImage: TPanel;
    lblWebpInImageDescription: TSkLabel;
    lblWebpInImageTitle: TSkLabel;
    svgWebpInImageArrow: TSkSvg;
    pnlWebpInImageLine: TPanel;
    procedure pnlEncodeWebpVsJpegClick(Sender: TObject);
    procedure pnlNinePatchClick(Sender: TObject);
    procedure pnlWebpInImageClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.PaintBox,
  Sample.Form.Viewer.Comparison.Image,
  Sample.Form.Viewer.TImage;

{$R *.dfm}

function FormatBytes(const ABytesCount: Int64): string;
const
  KBYTES = Int64(1024);
begin
  Result := Format('%s KB', [FormatFloat('0.#', ABytesCount / KBYTES)]);
end;

procedure TfrmImage.pnlEncodeWebpVsJpegClick(Sender: TObject);
const
  Quality = 80;
var
  LImage: ISkImage;
  LBytesWebP: TBytes;
  LBytesJpeg: TBytes;
begin
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'kung-fu-panda.png');
  LBytesWebP := LImage.Encode(TSkEncodedImageFormat.WEBP, Quality);
  LBytesJpeg := LImage.Encode(TSkEncodedImageFormat.JPEG, Quality);

  ChildForm<TfrmComparisonImageViewer>.Show('Encode WebP vs JPEG', '',
    Format('WebP - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesWebP))]),
    Format('Jpeg - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesJpeg))]),
    LBytesWebP, LBytesJpeg);
end;

procedure TfrmImage.pnlNinePatchClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('9-Patch', '',
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LImage: ISkImage;
      LImageDest: TRectF;
    begin
      LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'chat-bubble.png');
      LImageDest := RectF(0, 0, Max(ADest.Width - 160, 50), Max(ADest.Height - 400, 50));
      RectCenter(LImageDest, ADest);
      ACanvas.DrawImageNine(LImage, TRect.Create(39, 36, 40, 37), LImageDest);
    end);
end;

procedure TfrmImage.pnlWebpInImageClick(Sender: TObject);
begin
  ChildForm<TfrmTImageViewer>.Show('Loading .WebP in TImage', 'Skia4Delphi adds support for new formats to TPicture: .svg, .webp, .wbmp and raw images (.arw, .cr2, .dng, .nef, .nrw, .orf, .raf, .rw2, .pef and .srw)',
    procedure(const AImage: TImage)
    begin
      AImage.Picture.LoadFromFile(AssetsPath + 'kung-fu-panda.webp');
    end);
end;

end.
