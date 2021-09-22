object frmSVG: TfrmSVG
  Left = 0
  Top = 0
  ClientHeight = 606
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object imgBackground: TImage
    Left = 0
    Top = 0
    Width = 583
    Height = 606
  end
  object imgBackgroundPicture: TImage
    Left = 0
    Top = 0
    Width = 16
    Height = 16
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
      001008060000001FF3FF61000000294944415478DA63FC0F040C78C0D9B367F1
      4933308E1A302C0C3873E60C5E038C8D8D470D18FE060000015153092B313D82
      0000000049454E44AE426082}
    Visible = False
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 583
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblHeaderTitle: TLabel
      Left = 0
      Top = 0
      Width = 583
      Height = 50
      Align = alClient
      Alignment = taCenter
      Caption = 'SVG'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 27
      ExplicitHeight = 18
    end
  end
end
