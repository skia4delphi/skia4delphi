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
  System.SysUtils, System.UITypes, System.Classes, System.IOUtils, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation,

  { Skia }
  System.Skia, FMX.Skia,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkSVG = class(TfrmBase)
    btnTileWrapMode: TSpeedButton;
    lblTileWrapModeTitle: TSkLabel;
    lblTileWrapModeDescription: TSkLabel;
    btnCustomColor: TSpeedButton;
    lblCustomColorTitle: TSkLabel;
    lblCustomColorDescription: TSkLabel;
    btnEditingElement: TSpeedButton;
    lblEditingElementTitle: TSkLabel;
    lblEditingElementDescription: TSkLabel;
    btnDelphi: TSpeedButton;
    lblDelphiTitle: TSkLabel;
    lblDelphiDescription: TSkLabel;
    btnPanda: TSpeedButton;
    lblPandaTitle: TSkLabel;
    lblPandaDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnCustomColorClick(Sender: TObject);
    procedure btnDelphiClick(Sender: TObject);
    procedure btnEditingElementClick(Sender: TObject);
    procedure btnPandaClick(Sender: TObject);
    procedure btnTileWrapModeClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Base.Viewer,
  Sample.Form.Viewer.Control;

{$R *.fmx}

procedure TfrmTSkSVG.btnCustomColorClick(Sender: TObject);
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
      LSvgControl.Align := TAlignLayout.Client;
      if LOptions['Show original'] = False then
        LSvgControl.Svg.OverrideColor := TAlphaColors.Blueviolet;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'bat.svg');
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmTSkSVG.btnDelphiClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('delphi.svg', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := TAlignLayout.Client;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'delphi.svg');
    end);
end;

procedure TfrmTSkSVG.btnEditingElementClick(Sender: TObject);
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
      LSvgControl.Align := TAlignLayout.Client;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'lamb.svg');
      if LOptions['Show original'] = False then
        LSvgControl.Svg.DOM.FindNodeById('eyes').TrySetAttribute('fill', 'red');
    end, TBackgroundKind.Chess, LOptions);
end;

procedure TfrmTSkSVG.btnPandaClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('panda.svg', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := TAlignLayout.Client;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'panda.svg');
    end);
end;

procedure TfrmTSkSVG.btnTileWrapModeClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Tile Wrap Mode', '',
    function (): TControl
    var
      LSvgControl: TSkSvg absolute Result;
    begin
      LSvgControl := TSkSvg.Create(nil);
      LSvgControl.Align := TAlignLayout.Client;
      LSvgControl.Svg.WrapMode := TSkSvgWrapMode.Tile;
      LSvgControl.Svg.Source := TFile.ReadAllText(AssetsPath + 'carrots.svg');
    end, TBackgroundKind.Solid);
end;

end.
