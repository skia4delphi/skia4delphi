object frmSkottie: TfrmSkottie
  Left = 0
  Top = 0
  ClientHeight = 606
  ClientWidth = 583
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object imgBackground: TImage
    Left = 0
    Top = 50
    Width = 583
    Height = 556
    Align = alClient
    Visible = False
    ExplicitLeft = 248
    ExplicitTop = 272
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object imgBackgroundPicture: TImage
    Left = 0
    Top = 0
    Width = 16
    Height = 16
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
      001008060000001FF3FF61000000017352474200AECE1CE90000000467414D41
      0000B18F0BFC61050000002A4944415478DA639C3973E67F063CE0ECD9B3F8A4
      1918470D181606A4A5A5E135C0D8D878D480E16F0000DE8B3FE9E4E2B2AF0000
      000049454E44AE426082}
    Visible = False
  end
  object pbxAnimation: TPaintBox
    Left = 0
    Top = 50
    Width = 583
    Height = 556
    Align = alClient
    OnPaint = pbxAnimationPaint
    ExplicitLeft = 248
    ExplicitTop = 272
    ExplicitWidth = 105
    ExplicitHeight = 105
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
      Width = 182
      Height = 18
      Align = alClient
      Alignment = taCenter
      Caption = 'Skottie - Lottie file animation'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
  end
  object tmrRepaint: TTimer
    Enabled = False
    Interval = 15
    OnTimer = tmrRepaintTimer
    Left = 288
    Top = 288
  end
end
