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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts,
  FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkLabel = class(TfrmBase)
    btnJustifiedText: TSpeedButton;
    lblJustifiedTextTitle: TSkLabel;
    lblJustifiedTextDescription: TSkLabel;
    btnFamiliesFallbacks: TSpeedButton;
    lblFamiliesFallbacksTitle: TSkLabel;
    lblFamiliesFallbacksDescription: TSkLabel;
    btnWordOnClickEvent: TSpeedButton;
    lblWordOnClickEventTitle: TSkLabel;
    lblWordOnClickEventDescription: TSkLabel;
    btnMaxLines: TSpeedButton;
    lblMaxLinesTitle: TSkLabel;
    lblMaxLinesDescription: TSkLabel;
    btnRTL: TSpeedButton;
    lblRTLTitle: TSkLabel;
    lblRTLDescription: TSkLabel;
    btnFontWeight: TSpeedButton;
    lblFontWeightTitle: TSkLabel;
    lblFontWeightDescription: TSkLabel;
    btnMultiStyle: TSpeedButton;
    lblMultiStyleTitle: TSkLabel;
    lblMultiStyleDescription: TSkLabel;
    btnCustomFontFamily: TSpeedButton;
    lvlCustomFontFamilyTitle: TSkLabel;
    lblCustomFontFamilyDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnCustomFontFamilyClick(Sender: TObject);
    procedure btnFamiliesFallbacksClick(Sender: TObject);
    procedure btnFontWeightClick(Sender: TObject);
    procedure btnJustifiedTextClick(Sender: TObject);
    procedure btnMaxLinesClick(Sender: TObject);
    procedure btnMultiStyleClick(Sender: TObject);
    procedure btnRTLClick(Sender: TObject);
    procedure btnWordOnClickEventClick(Sender: TObject);
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

{$R *.fmx}

{ TfrmTSkLabel }

procedure TfrmTSkLabel.btnCustomFontFamilyClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Custom Font', 'The custom font will be available only inside the app, without extra configs like installation (Windows) or config info.plist (iOS).',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      // It is preferable to register only once at startup.
      TSkTypefaceManager.RegisterTypeface(AssetsPath + 'bonheur-royale-regular.ttf');
      TSkTypefaceManager.RegisterTypeface(AssetsPath + 'nunito-extrabold.ttf');

      LLabel := TSkLabel.Create(nil);
      LLabel.Align := TAlignLayout.Top;
      LLabel.Words.Add('"Each dream that you leave behind is a part of your future that will no longer exist."' + sLineBreak,
        $FFFF5F5F, 23, TFontWeight.UltraBold).Font.Families := 'Nunito';
      LLabel.Words.Add('(Steve Jobs)', $FF5B8DFE, 28).Font.Families := 'Bonheur Royale';
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.btnFamiliesFallbacksClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Families Fallbacks', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := TAlignLayout.Top;
      LLabel.TextSettings.Font.Families := 'Poppins, Nunito, Tenorite, Roboto, Segoe UI';
      LLabel.TextSettings.Font.Size := 20;
      LLabel.Text := Format('Text with font families fallbacks, that is, in the font family property there is a list of families separated by comma where the first family available on the device will be considered. ' + sLineBreak + sLineBreak +
        'In this label, the families list is "%s"', [LLabel.TextSettings.Font.Families]);
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.btnFontWeightClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Font Weight', '',
    function (): TControl
    const
      FontWeightName: array[TFontWeight] of string = ('Thin', 'UltraLight', 'Light', 'SemiLight', 'Regular', 'Medium', 'Semibold', 'Bold', 'UltraBold', 'Black', 'UltraBlack');
    var
      LLabel: TSkLabel absolute Result;
      LFontWeight: TFontWeight;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.Left := 10;
      LLabel.Align := TAlignLayout.Top;
      for LFontWeight := Low(LFontWeight) to High(LFontWeight) do
        LLabel.Words.Add(sLineBreak + FontWeightName[LFontWeight], TAlphaColors.Black, 34, LFontWeight);
    end);
end;

procedure TfrmTSkLabel.btnJustifiedTextClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Justified Text', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.Rect := RectF(10, 10, 10, 10);
      LLabel.Align := TAlignLayout.Top;
      LLabel.Text := TFile.ReadAllText(AssetsPath + 'lorem-ipsum.txt');
      LLabel.TextSettings.HorzAlign := TSkTextHorzAlign.Justify;
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.btnMaxLinesClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Maximum Lines', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := TAlignLayout.Top;
      LLabel.TextSettings.MaxLines := 3;
      LLabel.Text := 'This is a text that is limited to 3 lines in maximum. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
      LLabel.TextSettings.Font.Size := 32;
    end);
end;

procedure TfrmTSkLabel.btnMultiStyleClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Multiple Styles', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := TAlignLayout.Top;
      LLabel.Words.Add('English English 字典 字典 😀😅😂😂', TAlphaColors.Black, 28, TFontWeight.Light);
      LLabel.Words.Add(' !سلام دنیا', TAlphaColors.Crimson, 22, TFontWeight.Semibold);
      LLabel.Words.Add(' World domination is such an ugly phrase - I prefer to call it world optimisation.', TAlphaColors.Blueviolet, 30,
        TFontWeight.UltraBold, TFontSlant.Italic);
    end);
end;

procedure TfrmTSkLabel.btnRTLClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Right-to-left', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Margins.Rect := RectF(10, 10, 10, 10);
      LLabel.Align := TAlignLayout.Top;
      LLabel.TextSettings.Font.Size := 18;
      LLabel.Text := TFile.ReadAllText(AssetsPath + 'persian-lorem-ipsum.txt');
      ChildForm<TfrmControlViewer>.BiDiMode := bdRightToLeft;
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.btnWordOnClickEventClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Word OnClick Event', '',
    function (): TControl
    var
      LLabel: TSkLabel absolute Result;
      LItem: TSkLabel.TCustomWordsItem;
    begin
      LLabel := TSkLabel.Create(nil);
      LLabel.Align := TAlignLayout.Top;
      LLabel.HitTest := True;
      LLabel.Words.Add('Click on the following text: ', TAlphaColors.Black, 20);
      LItem := LLabel.Words.Add('this is just an item of a TSkLabel', TAlphaColors.Darkgoldenrod, 20, TFontWeight.Semibold);
      LItem.Cursor := crHandPoint;
      LItem.Decorations.Decorations := [TSkTextDecoration.Underline];
      LItem.OnClick := OnWordClick;
    end, TBackgroundKind.Solid);
end;

procedure TfrmTSkLabel.OnWordClick(Sender: TObject);
var
  LItem: TSkLabel.TWordsItem absolute Sender;
begin
  Showmessage(Format('Clicked text: "%s"', [LItem.Text]));
end;

end.
