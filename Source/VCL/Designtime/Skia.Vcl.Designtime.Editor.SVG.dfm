object SkSvgEditorForm: TSkSvgEditorForm
  Left = 0
  Top = 0
  Caption = 'SVG Editor'
  ClientHeight = 366
  ClientWidth = 490
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
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
  object pnlRightMenu: TPanel
    Left = 330
    Top = 0
    Width = 160
    Height = 366
    Align = alRight
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    object btnOpen: TButton
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 144
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnSave: TButton
      AlignWithMargins = True
      Left = 8
      Top = 48
      Width = 144
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Save'
      Enabled = False
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 8
      Top = 88
      Width = 144
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Ok'
      TabOrder = 2
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 8
      Top = 128
      Width = 144
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object btnClear: TButton
      AlignWithMargins = True
      Left = 8
      Top = 168
      Width = 144
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Align = alTop
      Caption = 'Clear'
      Enabled = False
      TabOrder = 4
      OnClick = btnClearClick
    end
  end
  object tbcContent: TTabControl
    Left = 0
    Top = 0
    Width = 330
    Height = 366
    Align = alClient
    TabHeight = 36
    TabOrder = 1
    TabPosition = tpBottom
    Tabs.Strings = (
      'Preview'
      'Source')
    TabIndex = 0
    TabWidth = 100
    OnChange = tbcContentChange
    object memSource: TMemo
      Left = 4
      Top = 4
      Width = 322
      Height = 322
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 0
      Visible = False
      OnChange = memSourceChange
    end
    object pnlPreview: TPanel
      Left = 4
      Top = 4
      Width = 322
      Height = 322
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object imgBackground: TImage
        Left = 0
        Top = 0
        Width = 322
        Height = 322
      end
    end
  end
  object odgOpenDialog: TOpenDialog
    DefaultExt = 'svg'
    Filter = 'Vector image SVG (*.svg)|*.svg'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open SVG'
    Left = 32
    Top = 8
  end
  object sdgSaveDialog: TSaveDialog
    DefaultExt = 'svg'
    Filter = 'Vector image SVG (*.svg)|*.svg'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save SVG'
    Left = 32
    Top = 58
  end
  object actActionList: TActionList
    Left = 32
    Top = 112
    object actCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy the svg source'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste the svg source'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
  end
end
