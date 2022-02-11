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
unit Sample.Form.Controls.TSkLabel;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.UITypes, System.IOUtils, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkLabel = class(TfrmBase)
    pnlMultiStyle: TPanel;
    lblMultiStyleDescription: TSkLabel;
    lblMultiStyleTitle: TSkLabel;
    svgMultiStyleArrow: TSkSvg;
    pnlMultiStyleLine: TPanel;
    pnlRTL: TPanel;
    lblRTLDescription: TSkLabel;
    lblRTLTitle: TSkLabel;
    svgRTLArrow: TSkSvg;
    pnlRTLLine: TPanel;
    pnlFontWeight: TPanel;
    lblFontWeightDescription: TSkLabel;
    lblFontWeightTitle: TSkLabel;
    svgFontWeightArrow: TSkSvg;
    pnlFontWeightLine: TPanel;
    pnlMaxLines: TPanel;
    lblMaxLinesDescription: TSkLabel;
    lblMaxLinesTitle: TSkLabel;
    svgMaxLinesArrow: TSkSvg;
    pnlMaxLinesLine: TPanel;
    pnlWordOnClickEvent: TPanel;
    lblWordOnClickEventDescription: TSkLabel;
    lblWordOnClickEventTitle: TSkLabel;
    svgWordOnClickEventArrow: TSkSvg;
    pnlWordOnClickEventLine: TPanel;
    pnlFamiliesFallbacks: TPanel;
    lblFamiliesFallbacksDescription: TSkLabel;
    lblFamiliesFallbacksTitle: TSkLabel;
    svgFamiliesFallbacksArrow: TSkSvg;
    pnlFamiliesFallbacksLine: TPanel;
    pnlJustifiedText: TPanel;
    lblJustifiedTextDescription: TSkLabel;
    lblJustifiedTextTitle: TSkLabel;
    svgJustifiedTextArrow: TSkSvg;
    pnlJustifiedTextLine: TPanel;
    pnlCustomFontFamily: TPanel;
    lblCustomFontFamilyDescription: TSkLabel;
    lblCustomFontFamilyTitle: TSkLabel;
    svgCustomFontFamilyArrow: TSkSvg;
    pnlCustomFontFamilyLine: TPanel;
    procedure pnlFamiliesFallbacksClick(Sender: TObject);
    procedure pnlFontWeightClick(Sender: TObject);
    procedure pnlJustifiedTextClick(Sender: TObject);
    procedure pnlMaxLinesClick(Sender: TObject);
    procedure pnlMultiStyleClick(Sender: TObject);
    procedure pnlRTLClick(Sender: TObject);
    procedure pnlWordOnClickEventClick(Sender: TObject);
    procedure pnlCustomFontFamilyClick(Sender: TObject);
  private
    procedure OnWordClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.Control;

{$R *.dfm}

procedure TfrmTSkLabel.OnWordClick(Sender: TObject);
var
  LItem: TSkLabel.TWordsItem absolute Sender;
begin
  MessageDlg(Format('Clicked text: "%s"', [LItem.Caption]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmTSkLabel.pnlCustomFontFamilyClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Custom Font', 'The custom font will be available only inside the app, without extra configs like installation in Windows.',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      // It is preferable to register only once at startup.
      TSkTypefaceManager.RegisterTypeface(AssetsPath + 'bonheur-royale-regular.ttf');
      TSkTypefaceManager.RegisterTypeface(AssetsPath + 'nunito-extrabold.ttf');

      LLabel := TSkLabel.Create(nil);
      LLabel.Align := alTop;
      LLabel.Words.Add('"Each dream that you leave behind is a part of your future that will no longer exist."' + sLineBreak,
        $FFFF5F5F, 23, TSkFontComponent.TSkFontWeight.UltraBold).Font.Families := 'Nunito';
      LLabel.Words.Add('(Steve Jobs)', $FF5B8DFE, 28).Font.Families := 'Bonheur Royale';
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.pnlFamiliesFallbacksClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Families Fallbacks', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := alTop;
      LLabel.TextSettings.Font.Families := 'Poppins, Nunito, Tenorite, Roboto, Segoe UI';
      LLabel.TextSettings.Font.Size := 20;
      LLabel.Caption := Format('Text with font families fallbacks, that is, in the font family property there is a list of families separated by comma where the first family available on the device will be considered. ' + sLineBreak + sLineBreak +
        'In this label, the families list is "%s"', [LLabel.TextSettings.Font.Families]);
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.pnlFontWeightClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Font Weight', '',
    function (): TControl
    const
      FontWeightName: array[TSkFontComponent.TSkFontWeight] of string = ('Thin', 'UltraLight', 'Light', 'SemiLight', 'Regular', 'Medium', 'Semibold', 'Bold', 'UltraBold', 'Black', 'UltraBlack');
    var
      LLabel: TSkLabel absolute Result;
      LFontWeight: TSkFontComponent.TSkFontWeight;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.Left := 10;
      LLabel.Align := alTop;
      LLabel.AlignWithMargins := True;
      for LFontWeight := Low(LFontWeight) to High(LFontWeight) do
        LLabel.Words.Add(sLineBreak + FontWeightName[LFontWeight], TAlphaColors.Black, 34, LFontWeight);
    end);
end;

procedure TfrmTSkLabel.pnlJustifiedTextClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Justified Text', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.SetBounds(10, 10, 10, 10);
      LLabel.Align := alTop;
      LLabel.AlignWithMargins := True;
      LLabel.Caption := TFile.ReadAllText(AssetsPath + 'lorem-ipsum.txt');
      LLabel.TextSettings.HorzAlign := TSkTextHorzAlign.Justify;
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.pnlMaxLinesClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Maximum Lines', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := alTop;
      LLabel.TextSettings.MaxLines := 3;
      LLabel.Caption := 'This is a text that is limited to 3 lines in maximum. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
      LLabel.TextSettings.Font.Size := 32;
    end);
end;

procedure TfrmTSkLabel.pnlMultiStyleClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Multiple Styles', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := alTop;
      LLabel.Words.Add('English English 字典 字典 😀😅😂😂', TAlphaColors.Black, 28, TSkFontComponent.TSkFontWeight.Light);
      LLabel.Words.Add(' !سلام دنیا', TAlphaColors.Crimson, 22, TSkFontComponent.TSkFontWeight.Semibold);
      LLabel.Words.Add(' World domination is such an ugly phrase - I prefer to call it world optimisation.', TAlphaColors.Blueviolet, 30,
        TSkFontComponent.TSkFontWeight.UltraBold, TSkFontComponent.TSkFontSlant.Italic);
    end);
end;

procedure TfrmTSkLabel.pnlRTLClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Right-to-left', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.SetBounds(10, 10, 10, 10);
      LLabel.Align := alTop;
      LLabel.AlignWithMargins := True;
      LLabel.TextSettings.Font.Size := 18;
      LLabel.Caption := TFile.ReadAllText(AssetsPath + 'persian-lorem-ipsum.txt');
      ChildForm<TfrmControlViewer>.BiDiMode := bdRightToLeft;
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.pnlWordOnClickEventClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Word OnClick Event', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
      LItem: TSkLabel.TCustomWordsItem;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := alTop;
      LLabel.Words.Add('Click on the following text: ', TAlphaColors.Black, 20);
      LItem := LLabel.Words.Add('this is just an item of a TSkLabel', TAlphaColors.Darkgoldenrod, 20, TSkFontComponent.TSkFontWeight.Semibold);
      LItem.Cursor := crHandPoint;
      LItem.Decorations.Decorations := [TSkTextDecoration.Underline];
      LItem.OnClick := OnWordClick;
    end, TBackgroundKind.Solid);
end;

end.
