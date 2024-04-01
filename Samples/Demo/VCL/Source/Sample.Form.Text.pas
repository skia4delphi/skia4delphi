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
unit Sample.Form.Text;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, System.Types, System.UITypes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmText = class(TfrmBase)
    pnlBasicTexts: TPanel;
    lblBasicTextsDescription: TSkLabel;
    lblBasicTextsTitle: TSkLabel;
    svgBasicTextsArrow: TSkSvg;
    pnlBasicTextsLine: TPanel;
    pnlRTL: TPanel;
    lblRTLDescription: TSkLabel;
    lblRTLTitle: TSkLabel;
    svgRTLArrow: TSkSvg;
    pnlRTLLine: TPanel;
    pnlParagraph: TPanel;
    lblParagraphDescription: TSkLabel;
    lblParagraphTitle: TSkLabel;
    svgParagraphArrow: TSkSvg;
    pnlParagraphLine: TPanel;
    pnlCustomFont: TPanel;
    lblCustomFontDescription: TSkLabel;
    lblCustomFontTitle: TSkLabel;
    svgCustomFontArrow: TSkSvg;
    pnlCustomFontLine: TPanel;
    pnlParagraphToPath: TPanel;
    lblParagraphToPathDescription: TSkLabel;
    lblParagraphToPathTitle: TSkLabel;
    svgParagraphToPathArrow: TSkSvg;
    pnlParagraphToPathLine: TPanel;
    pnlTSkLabel: TPanel;
    lblTSkLabelDescription: TSkLabel;
    lblTSkLabelTitle: TSkLabel;
    svgTSkLabelArrow: TSkSvg;
    pnlTSkLabelLine: TPanel;
    procedure pnlBasicTextsClick(Sender: TObject);
    procedure pnlCustomFontClick(Sender: TObject);
    procedure pnlParagraphClick(Sender: TObject);
    procedure pnlParagraphToPathClick(Sender: TObject);
    procedure pnlRTLClick(Sender: TObject);
    procedure pnlTSkLabelClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.PaintBox,
  Sample.Form.Controls.TSkLabel;

{$R *.dfm}

procedure TfrmText.pnlBasicTextsClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Basic Texts', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LBlob1: ISkTextBlob;
      LBlob2: ISkTextBlob;
      LFont1: ISkFont;
      LFont2: ISkFont;
      LPaint1: ISkPaint;
      LPaint2: ISkPaint;
      LPaint3: ISkPaint;
      LTypeface: ISkTypeface;
    begin
      LTypeface := TSkTypeface.MakeFromName('Monospace', TSkFontStyle.Normal);
      LFont1 := TSkFont.Create(LTypeface, 64, 1);
      LFont2 := TSkFont.Create(LTypeface, 64, 1.5);
      LFont1.Edging := TSkFontEdging.AntiAlias;
      LFont2.Edging := TSkFontEdging.AntiAlias;

      LBlob1 := TSkTextBlob.MakeFromText('Skia', LFont1);
      LBlob2 := TSkTextBlob.MakeFromText('Skia', LFont2);

      LPaint1 := TSkPaint.Create;
      LPaint1.AntiAlias := True;
      LPaint1.SetARGB($FF, $42, $85, $F4);

      LPaint2 := TSkPaint.Create(TSkPaintStyle.Stroke);
      LPaint2.AntiAlias := True;
      LPaint2.SetARGB($FF, $DB, $44, $37);
      LPaint2.StrokeWidth := 3;

      LPaint3 := TSkPaint.Create;
      LPaint3.AntiAlias := True;
      LPaint3.SetARGB($FF, $0F, $9D, $58);

      ACanvas.DrawTextBlob(LBlob1, 20, 64, LPaint1);
      ACanvas.DrawSimpleText('Skia', 20, 154, LFont1, LPaint2);
      ACanvas.DrawTextBlob(LBlob2, 20, 244, LPaint3);
    end);
end;

procedure TfrmText.pnlCustomFontClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Custom Font', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LFont: ISkFont;
      LPaint: ISkPaint;
    begin
      LPaint := TSkPaint.Create;
      LPaint.Color := $8FFFFFFF;
      ACanvas.DrawRect(ADest, LPaint);
      LPaint.Reset;

      LFont := TSkFont.Create(TSkTypeface.MakeFromFile(AssetsPath + 'nunito-extrabold.ttf'), 23);
      LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(256, 145), $FFFF5F5F, $FF5B8DFE, TSkTileMode.Clamp);

      ACanvas.DrawSimpleText('"Each dream that you', 2, 25, LFont, LPaint);
      ACanvas.DrawSimpleText('leave behind is a part', 2, 55, LFont, LPaint);
      ACanvas.DrawSimpleText('of your future that will', 2, 85, LFont, LPaint);
      ACanvas.DrawSimpleText('no longer exist."', 2, 115, LFont, LPaint);

      LFont := TSkFont.Create(TSkTypeface.MakeFromFile(AssetsPath + 'bonheur-royale-regular.ttf'), 28);
      LPaint.Shader := nil;
      LPaint.Color  := $FF5B8DFE;
      ACanvas.DrawSimpleText('(Steve Jobs)', 2, 150, LFont, LPaint);
    end);
end;

procedure TfrmText.pnlParagraphClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Paragraph', '', 440, 440,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LParagraph: ISkParagraph;
      LBuilder: ISkParagraphBuilder;
      LTextStyle: ISkTextStyle;
      LParagraphStyle: ISkParagraphStyle;
    begin
      LParagraphStyle := TSkParagraphStyle.Create;
      LParagraphStyle.MaxLines := 3;
      LParagraphStyle.Ellipsis := '...';
      LBuilder := TSkParagraphBuilder.Create(LParagraphStyle);

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Black;
      LTextStyle.FontSize := 28;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.Light, TSkFontWidth.Normal, TSkFontSlant.Upright);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText('English English 字典 字典 😀😅😂😂');

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Crimson;
      LTextStyle.FontSize := 22;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.SemiBold, TSkFontWidth.Normal, TSkFontSlant.Upright);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText(' !سلام دنیا');

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Blueviolet;
      LTextStyle.FontSize := 30;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.ExtraBold, TSkFontWidth.Normal, TSkFontSlant.Italic);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText(' World domination is such an ugly phrase - I prefer to call it world optimisation.');

      LParagraph := LBuilder.Build;
      LParagraph.Layout(ADest.Width);
      LParagraph.Paint(ACanvas, 0, 0);
    end);
end;

procedure TfrmText.pnlParagraphToPathClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Paragraph to Path', '', 440, 440,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LParagraph: ISkParagraph;
      LBuilder: ISkParagraphBuilder;
      LTextStyle: ISkTextStyle;
      LParagraphStyle: ISkParagraphStyle;
      LPaint: ISkPaint;
    begin
      LParagraphStyle := TSkParagraphStyle.Create;
      LParagraphStyle.MaxLines := 3;
      LParagraphStyle.Ellipsis := '...';
      LBuilder := TSkParagraphBuilder.Create(LParagraphStyle);

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Black;
      LTextStyle.FontSize := 28;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.Light, TSkFontWidth.Normal, TSkFontSlant.Upright);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText('English English 字典 字典');

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Crimson;
      LTextStyle.FontSize := 22;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.SemiBold, TSkFontWidth.Normal, TSkFontSlant.Upright);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText(' !سلام دنیا');

      LTextStyle := TSkTextStyle.Create;
      LTextStyle.Color := TAlphaColors.Blueviolet;
      LTextStyle.FontSize := 30;
      LTextStyle.FontStyle := TSkFontStyle.Create(TSkFontWeight.ExtraBold, TSkFontWidth.Normal, TSkFontSlant.Italic);
      LBuilder.PushStyle(LTextStyle);
      LBuilder.AddText(' World domination is such an ugly phrase - I prefer to call it world optimisation.');

      LParagraph := LBuilder.Build;
      LParagraph.Layout(ADest.Width);

      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.Color := TAlphaColors.Black;

      ACanvas.DrawPath(LParagraph.ToPath, LPaint);
    end);
end;

procedure TfrmText.pnlRTLClick(Sender: TObject);
begin
  ChildForm<TfrmPaintBoxViewer>.Show('Right-to-left', '', 256, 256,
    procedure (const ACanvas: ISkCanvas; const ADest: TRectF)
    var
      LBlob: ISkTextBlob;
      LFont: ISkFont;
      LPaint: ISkPaint;
      LShaper: ISkShaper;
    begin
      LFont := TSkFont.Create(TSkTypeface.MakeDefault, 55, 1);
      LShaper := TSkShaper.Create;
      LBlob := LShaper.Shape('سلام دنیا!', LFont, False, MaxSingle);

      LPaint := TSkPaint.Create;
      LPaint.AntiAlias := True;
      LPaint.Color := TAlphaColors.Tomato;

      ACanvas.DrawTextBlob(LBlob, 0, 0, LPaint);
    end);
end;

procedure TfrmText.pnlTSkLabelClick(Sender: TObject);
begin
  ChildForm<TfrmTSkLabel>.Show;
end;

end.
