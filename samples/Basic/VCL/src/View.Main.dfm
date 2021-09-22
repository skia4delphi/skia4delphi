object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Skia4Delphi - Basic'
  ClientHeight = 806
  ClientWidth = 384
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object sbxContent: TScrollBox
    Left = 0
    Top = 0
    Width = 384
    Height = 806
    HorzScrollBar.Visible = False
    VertScrollBar.Style = ssHotTrack
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    Padding.Left = 10
    Padding.Top = 10
    Padding.Right = 10
    Padding.Bottom = 10
    ParentBackground = True
    TabOrder = 0
    object gbxShapes: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 20
      Width = 347
      Height = 130
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'Shapes'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 0
      object gplShapesButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 102
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnShapesBasic
            Row = 0
          end
          item
            Column = 1
            Control = btnShapesBezierCurves
            Row = 0
          end
          item
            Column = 0
            Control = btnShapesTranslationsAndRotations
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object btnShapesBasic: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Basic'
          OnClick = btnShapesBasicClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnShapesBezierCurves: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 1
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'B'#233'zier Curves'
          OnClick = btnShapesBezierCurvesClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnShapesTranslationsAndRotations: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 52
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Translations/Rotations'
          OnClick = btnShapesTranslationsAndRotationsClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
      end
    end
    object gbxText: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 180
      Width = 347
      Height = 130
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'Text'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 1
      object gplTextButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 102
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnTextRendering
            Row = 0
          end
          item
            Column = 1
            Control = btnTextRTL
            Row = 0
          end
          item
            Column = 0
            Control = btnTextParagraph
            Row = 1
          end
          item
            Column = 1
            Control = btnTextCustomFont
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object btnTextRendering: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Rendering'
          OnClick = btnTextRenderingClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnTextRTL: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 1
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Text RTL'
          OnClick = btnTextRTLClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnTextParagraph: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 52
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Paragraph'
          OnClick = btnTextParagraphClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnTextCustomFont: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 52
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Custom Font'
          OnClick = btnTextCustomFontClick
          ExplicitLeft = 170
          ExplicitTop = 53
          ExplicitWidth = 165
        end
      end
    end
    object gbxPathsAndEffects: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 340
      Width = 347
      Height = 130
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'Paths and Effects'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 2
      object gplPathsAndEffectsButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 102
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnPathsAndEffectsDiscrete
            Row = 0
          end
          item
            Column = 1
            Control = btnPathsAndEffectsComposed
            Row = 0
          end
          item
            Column = 0
            Control = btnPathsAndEffectsSum
            Row = 1
          end
          item
            Column = 1
            Control = btnPathsAndEffectsShaders
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object btnPathsAndEffectsDiscrete: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Discrete'
          OnClick = btnPathsAndEffectsDiscreteClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnPathsAndEffectsComposed: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 1
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Composed'
          OnClick = btnPathsAndEffectsComposedClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnPathsAndEffectsSum: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 52
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Sum'
          OnClick = btnPathsAndEffectsSumClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnPathsAndEffectsShaders: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 52
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Shaders'
          OnClick = btnPathsAndEffectsShadersClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
      end
    end
    object gbxSVG: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 500
      Width = 347
      Height = 130
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'SVG'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 3
      object gplSVGButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 102
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnSVGGorilla
            Row = 0
          end
          item
            Column = 1
            Control = btnSVGDelphi
            Row = 0
          end
          item
            Column = 0
            Control = btnSVGToPDF
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object btnSVGGorilla: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'gorilla.svg'
          OnClick = btnSVGGorillaClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnSVGDelphi: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 1
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'delphi.svg'
          OnClick = btnSVGDelphiClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnSVGToPDF: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 52
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'SVG to PDF'
          OnClick = btnSVGToPDFClick
          ExplicitLeft = 10
          ExplicitTop = 54
          ExplicitWidth = 173
          ExplicitHeight = 48
        end
      end
    end
    object gbxLottie: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 660
      Width = 347
      Height = 130
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'Lottie animation'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 4
      object gplLottieButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 102
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnLottieRocket
            Row = 0
          end
          item
            Column = 1
            Control = btnLottieCheck
            Row = 0
          end
          item
            Column = 0
            Control = btnLottieTelegramSticker
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 0
        object btnLottieRocket: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'rocket.json'
          OnClick = btnLottieRocketClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
        object btnLottieCheck: TSpeedButton
          AlignWithMargins = True
          Left = 168
          Top = 1
          Width = 166
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'check.json'
          OnClick = btnLottieCheckClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 23
          ExplicitHeight = 22
        end
        object btnLottieTelegramSticker: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 52
          Width = 165
          Height = 49
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'telegram_sticker.tgs'
          OnClick = btnLottieTelegramStickerClick
          ExplicitLeft = 9
          ExplicitTop = 53
          ExplicitWidth = 174
        end
      end
    end
    object gbxImage: TGroupBox
      AlignWithMargins = True
      Left = 10
      Top = 820
      Width = 347
      Height = 80
      Margins.Left = 0
      Margins.Top = 10
      Margins.Right = 0
      Margins.Bottom = 20
      Align = alTop
      Caption = 'Image'
      Padding.Left = 4
      Padding.Top = 3
      Padding.Right = 4
      Padding.Bottom = 8
      TabOrder = 5
      object gplImageButtons: TGridPanel
        Left = 6
        Top = 18
        Width = 335
        Height = 52
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = btnImageWebPvsJpeg
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        object btnImageWebPvsJpeg: TSpeedButton
          AlignWithMargins = True
          Left = 1
          Top = 1
          Width = 165
          Height = 50
          Margins.Left = 1
          Margins.Top = 1
          Margins.Right = 1
          Margins.Bottom = 1
          Align = alClient
          Caption = 'Encode WebP vs Jpeg'
          OnClick = btnImageWebPvsJpegClick
          ExplicitLeft = 0
          ExplicitTop = 26
          ExplicitWidth = 167
          ExplicitHeight = 38
        end
      end
    end
  end
end
