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
unit Sample.Form.Controls.TSkSVG;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.SysUtils, System.Classes, System.UITypes, System.IOUtils, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  System.Skia, Vcl.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkSVG = class(TfrmBase)
    pnlPanda: TPanel;
    lblPandaDescription: TSkLabel;
    lblPandaTitle: TSkLabel;
    svgPandaArrow: TSkSvg;
    pnlPandaLine: TPanel;
    pnlDelphi: TPanel;
    lblDelphiDescription: TSkLabel;
    lblDelphiTitle: TSkLabel;
    svgDelphiArrow: TSkSvg;
    pnlDelphiLine: TPanel;
    pnlEditingElement: TPanel;
    lblEditingElementDescription: TSkLabel;
    lblEditingElementTitle: TSkLabel;
    svgEditingElementArrow: TSkSvg;
    pnlEditingElementLine: TPanel;
    pnlCustomColor: TPanel;
    lblCustomColorDescription: TSkLabel;
    lblCustomColorTitle: TSkLabel;
    svgCustomColorArrow: TSkSvg;
    pnlCustomColorLine: TPanel;
    pnlTileWrapMode: TPanel;
    lblTileWrapModeDescription: TSkLabel;
    lblTileWrapModeTitle: TSkLabel;
    svgTileWrapModeArrow: TSkSvg;
    pnlTileWrapModeLine: TPanel;
    procedure pnlCustomColorClick(Sender: TObject);
    procedure pnlDelphiClick(Sender: TObject);
    procedure pnlEditingElementClick(Sender: TObject);
    procedure pnlPandaClick(Sender: TObject);
    procedure pnlTileWrapModeClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.Control;

{$R *.dfm}

procedure TfrmTSkSVG.pnlCustomColorClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddBoolean('Show original', False);

  ChildForm<TfrmControlViewer>.Show('Custom Color', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := alClient;
      if LOptions['Show original'] = False then
        LSvgControl.Svg.OverrideColor := TAlphaColors.Blueviolet;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'bat.svg');
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmTSkSVG.pnlDelphiClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('delphi.svg', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := alClient;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'delphi.svg');
    end);
end;

procedure TfrmTSkSVG.pnlEditingElementClick(Sender: TObject);
var
  LOptions: IViewerOptions;
begin
  LOptions := TViewerOptions.Create;
  LOptions.AddBoolean('Show original', False);

  ChildForm<TfrmControlViewer>.Show('Editing an SVG Element', 'Editing the eyes of the lamb svg to red color',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := alClient;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'lamb.svg');
      if LOptions['Show original'] = False then
        LSvgControl.Svg.DOM.FindNodeById('eyes').TrySetAttribute('fill', 'red');
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmTSkSVG.pnlPandaClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('panda.svg', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := alClient;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'panda.svg');
    end);
end;

procedure TfrmTSkSVG.pnlTileWrapModeClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Tile Wrap Mode', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := alClient;
      LSvgControl.Svg.WrapMode := TSkSvgWrapMode.Tile;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'carrots.svg');
    end, TBackgroundKind.Solid);
end;

end.
