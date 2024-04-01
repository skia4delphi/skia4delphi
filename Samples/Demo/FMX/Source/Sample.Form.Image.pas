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
unit Sample.Form.Image;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Types, System.Classes, System.Math, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmImage = class(TfrmBase)
    btnEncodeWebpVsJpeg: TSpeedButton;
    lblEncodeWebpVsJpegTitle: TSkLabel;
    lblEncodeWebpVsJpegDescription: TSkLabel;
    btnWebpInImage: TSpeedButton;
    lblWebpInImageTitle: TSkLabel;
    lblWebpInImageDescription: TSkLabel;
    btnNinePatch: TSpeedButton;
    lblNinePatchTitle: TSkLabel;
    lblNinePatchDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnEncodeWebpVsJpegClick(Sender: TObject);
    procedure btnNinePatchClick(Sender: TObject);
    procedure btnWebpInImageClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.PaintBox,
  Sample.Form.Viewer.Comparison.Image,
  Sample.Form.Viewer.TImage;

{$R *.fmx}

function FormatBytes(const ABytesCount: Int64): string;
const
  KiloByte = Int64(1024);
begin
  Result := Format('%s KB', [FormatFloat('0.#', ABytesCount / KiloByte)]);
end;

procedure TfrmImage.btnEncodeWebpVsJpegClick(Sender: TObject);
const
  Quality = 80;
var
  LImage: ISkImage;
  LBytesWebP: TBytes;
  LBytesJpeg: TBytes;
begin
  LImage := TSkImage.MakeFromEncodedFile(AssetsPath + 'golden-pheasant.png');
  LBytesWebP := LImage.Encode(TSkEncodedImageFormat.WEBP, Quality);
  LBytesJpeg := LImage.Encode(TSkEncodedImageFormat.JPEG, Quality);

  ChildForm<TfrmComparisonImageViewer>.Show('Encode WebP vs JPEG', '',
    Format('WebP - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesWebP))]),
    Format('Jpeg - %s quality - %s', [Quality.ToString + '%', FormatBytes(Length(LBytesJpeg))]),
    LBytesWebP, LBytesJpeg);
end;

procedure TfrmImage.btnNinePatchClick(Sender: TObject);
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

procedure TfrmImage.btnWebpInImageClick(Sender: TObject);
begin
  ChildForm<TfrmTImageViewer>.Show('Loading .WebP in TImage', 'Skia4Delphi adds support for new formats to TBitmap: ' +
    '.bmp, .gif, .ico, .webp, .wbmp and raw images (.arw, .cr2, .dng, .nef, .nrw, .orf, .raf, .rw2, .pef and .srw)',
    procedure(const AImage: TImage)
    begin
      AImage.Bitmap.LoadFromFile(AssetsPath + 'golden-pheasant.webp');
    end);
end;

end.
