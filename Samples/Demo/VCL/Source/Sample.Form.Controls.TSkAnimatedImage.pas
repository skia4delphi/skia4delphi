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
unit Sample.Form.Controls.TSkAnimatedImage;

interface

{$SCOPEDENUMS ON}

uses
  { Delphi }
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,

  { Skia }
  Skia, Skia.Vcl,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkAnimatedImage = class(TfrmBase)
    pnlLottieLoadings: TPanel;
    lblLottieLoadingsDescription: TSkLabel;
    lblLottieLoadingsTitle: TSkLabel;
    svgLottieLoadingsArrow: TSkSvg;
    pnlLottieLoadingsLine: TPanel;
    pnlAnimatedGIF: TPanel;
    lblAnimatedGIFDescription: TSkLabel;
    lblAnimatedGIFTitle: TSkLabel;
    svgAnimatedGIFArrow: TSkSvg;
    pnlAnimatedGIFLine: TPanel;
    pnlTelegramStickers: TPanel;
    lblTelegramStickersDescription: TSkLabel;
    lblTelegramStickersTitle: TSkLabel;
    svgTelegramStickersArrow: TSkSvg;
    pnlTelegramStickersLine: TPanel;
    pnlAnimatedWebP: TPanel;
    lblAnimatedWebPDescription: TSkLabel;
    lblAnimatedWebPTitle: TSkLabel;
    svgAnimatedWebPArrow: TSkSvg;
    pnlAnimatedWebPLine: TPanel;
    pnlLottieRocket: TPanel;
    lblLottieRocketDescription: TSkLabel;
    lblLottieRocketTitle: TSkLabel;
    svgLottieRocketArrow: TSkSvg;
    pnlLottieRocketLine: TPanel;
    pnlLottieCheck: TPanel;
    lblLottieCheckDescription: TSkLabel;
    lblLottieCheckTitle: TSkLabel;
    svgLottieCheckArrow: TSkSvg;
    pnlLottieCheckLine: TPanel;
    procedure pnlAnimatedGIFClick(Sender: TObject);
    procedure pnlAnimatedWebPClick(Sender: TObject);
    procedure pnlLottieCheckClick(Sender: TObject);
    procedure pnlLottieLoadingsClick(Sender: TObject);
    procedure pnlLottieRocketClick(Sender: TObject);
    procedure pnlTelegramStickersClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Control;

{$R *.dfm}

procedure TfrmTSkAnimatedImage.pnlAnimatedGIFClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Animated GIF', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'animated.gif');
    end);
end;

procedure TfrmTSkAnimatedImage.pnlAnimatedWebPClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Animated WebP', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'rocket.webp');
    end);
end;

procedure TfrmTSkAnimatedImage.pnlLottieCheckClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - check.json', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'check.json');
    end);
end;

procedure TfrmTSkAnimatedImage.pnlLottieLoadingsClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - Loadings', '',
    function (): TControl
    var
      LGrid: TGridPanel absolute Result;
      LAnimatedImage: TSkAnimatedImage;
    begin
      LGrid := TGridPanel.Create(nil);
      LGrid.Align := alCLient;
      LGrid.RowCollection.BeginUpdate;
      try
        LGrid.RowCollection.Items[0].Value := 33.333333333333333333;
        LGrid.RowCollection.Items[1].Value := 33.333333333333333333;
        LGrid.RowCollection.Add.Value := 33.333333333333333333;
      finally
        LGrid.RowCollection.EndUpdate;
      end;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading1.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading2.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading3.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading4.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading5.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.SetBounds(10, 10, 10, 10);
      LAnimatedImage.AlignWithMargins := True;
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading6.json');
      LAnimatedImage.Parent := LGrid;
    end);
end;

procedure TfrmTSkAnimatedImage.pnlLottieRocketClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - rocket.json', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'rocket.json');
    end);
end;

procedure TfrmTSkAnimatedImage.pnlTelegramStickersClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Telegram Sticker', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := alClient;
      LAnimatedImage.LoadFromFile(AssetsPath + 'telegram-sticker.tgs');
    end);
end;

end.
