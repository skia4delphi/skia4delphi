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
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Objects,

  { Skia }
  Skia, Skia.FMX,

  { Sample }
  Sample.Form.Base;

type
  TfrmTSkAnimatedImage = class(TfrmBase)
    btnAnimatedWebP: TSpeedButton;
    lblAnimatedWebPTitle: TSkLabel;
    lblAnimatedWebPDescription: TSkLabel;
    btnAnimatedGIF: TSpeedButton;
    lblAnimatedGIFTitle: TSkLabel;
    lblAnimatedGIFDescription: TSkLabel;
    btnTelegramStickers: TSpeedButton;
    lblTelegramStickersTitle: TSkLabel;
    lblTelegramStickersDescription: TSkLabel;
    btnLottieLoadings: TSpeedButton;
    lblLottieLoadingsTitle: TSkLabel;
    lblLottieLoadingsDescription: TSkLabel;
    btnLottieCheck: TSpeedButton;
    lblLottieCheckTitle: TSkLabel;
    lblLottieCheckDescription: TSkLabel;
    btnLottieRocket: TSpeedButton;
    lblLottieRocketTitle: TSkLabel;
    lblLottieRocketDescription: TSkLabel;
    lytContentTopOffset: TLayout;
    procedure btnAnimatedGIFClick(Sender: TObject);
    procedure btnAnimatedWebPClick(Sender: TObject);
    procedure btnLottieCheckClick(Sender: TObject);
    procedure btnLottieLoadingsClick(Sender: TObject);
    procedure btnLottieRocketClick(Sender: TObject);
    procedure btnTelegramStickersClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
  { Sample }
  Sample.Form.Viewer.Control;

{$R *.fmx}

procedure TfrmTSkAnimatedImage.btnAnimatedGIFClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Animated GIF', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'animated.gif');
    end);
end;

procedure TfrmTSkAnimatedImage.btnAnimatedWebPClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Animated WebP', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'rocket.webp');
    end);
end;

procedure TfrmTSkAnimatedImage.btnLottieCheckClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - check.json', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'check.json');
    end);
end;

procedure TfrmTSkAnimatedImage.btnLottieLoadingsClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - Loadings', '',
    function (): TControl
    var
      LGrid: TGridPanelLayout absolute Result;
      LAnimatedImage: TSkAnimatedImage;
    begin
      LGrid := TGridPanelLayout.Create(nil);
      LGrid.Align := TAlignLayout.Client;
      LGrid.HitTest := False;
      LGrid.RowCollection.BeginUpdate;
      try
        LGrid.RowCollection.Items[0].Value := 33.333333333333333333;
        LGrid.RowCollection.Items[1].Value := 33.333333333333333333;
        LGrid.RowCollection.Add.Value := 33.333333333333333333;
      finally
        LGrid.RowCollection.EndUpdate;
      end;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading1.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading2.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading3.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading4.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading5.json');
      LAnimatedImage.Parent := LGrid;

      LAnimatedImage := TSkAnimatedImage.Create(LGrid);
      LAnimatedImage.Margins.Rect := RectF(10, 10, 10, 10);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'loading6.json');
      LAnimatedImage.Parent := LGrid;
    end);
end;

procedure TfrmTSkAnimatedImage.btnLottieRocketClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Lottie - rocket.json', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'rocket.json');
    end);
end;

procedure TfrmTSkAnimatedImage.btnTelegramStickersClick(Sender: TObject);
begin
  ChildForm<TfrmControlViewer>.Show('Telegram Sticker', '',
    function (): TControl
    var
      LAnimatedImage: TSkAnimatedImage absolute Result;
    begin
      LAnimatedImage := TSkAnimatedImage.Create(nil);
      LAnimatedImage.Align := TAlignLayout.Client;
      LAnimatedImage.LoadFromFile(AssetsPath + 'telegram-sticker.tgs');
    end);
end;

end.
