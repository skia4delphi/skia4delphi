inherited frmMain: TfrmMain
  ClientHeight = 620
  ExplicitHeight = 659
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    Height = 620
    ExplicitHeight = 620
    inherited pnlTitle: TPanel
      inherited lblTitle: TSkLabel
        Words = <
          item
            Caption = 'Skia4Delphi Demo'
          end>
      end
    end
    inherited sbxContent: TScrollBox
      Height = 542
      ExplicitHeight = 542
      object pnlBasics: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 71
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 0
        OnClick = pnlBasicsClick
        DesignSize = (
          303
          50)
        object lblBasicsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlBasicsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Learn basic graphics concepts'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblBasicsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlBasicsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Drawing Basics'
            end>
          ExplicitWidth = 303
        end
        object svgBasicsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlBasicsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlBasicsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlControls: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 21
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 1
        OnClick = pnlControlsClick
        DesignSize = (
          303
          50)
        object lblControlsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlControlsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Visual controls like TSkAnimatedImage, TSkLabel and TSkSvg'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblControlsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlControlsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Controls'
            end>
          ExplicitWidth = 303
        end
        object svgControlsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlControlsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlControlsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlDocuments: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 221
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Color = 16249586
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ShowCaption = False
        TabOrder = 2
        OnClick = pnlDocumentsClick
        DesignSize = (
          303
          50)
        object lblDocumentsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlDocumentsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Create documents like PDF or XPS'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblDocumentsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlDocumentsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Documents'
            end>
          ExplicitWidth = 303
        end
        object svgDocumentsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          ParentCustomHint = False
          Anchors = [akTop, akRight]
          OnClick = pnlDocumentsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlDocumentsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlImage: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 321
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 3
        OnClick = pnlImageClick
        DesignSize = (
          303
          50)
        object lblImageDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlImageClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Learn basic image concepts like encode and decode'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblImageTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlImageClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Image'
            end>
          ExplicitWidth = 303
        end
        object svgImageArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlImageClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlImageLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlParticles: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 421
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 4
        OnClick = pnlParticlesClick
        DesignSize = (
          303
          50)
        object lblParticlesDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlParticlesClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Create many kinds of particles animations'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblParticlesTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlParticlesClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Particles'
            end>
          ExplicitWidth = 303
        end
        object svgParticlesArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlParticlesClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlParticlesLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlPathsAndEffects: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 271
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 5
        OnClick = pnlPathsAndEffectsClick
        DesignSize = (
          303
          50)
        object lblPathsAndEffectsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlPathsAndEffectsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Learn to draw paths and use effects'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblPathsAndEffectsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlPathsAndEffectsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Drawing Paths and Effects'
            end>
          ExplicitWidth = 303
        end
        object svgPathsAndEffectsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlPathsAndEffectsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlPathsAndEffectsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlRuntimeEffects: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 471
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 6
        OnClick = pnlRuntimeEffectsClick
        DesignSize = (
          303
          50)
        object lblRuntimeEffectsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlRuntimeEffectsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Create shaders using the skia shader language'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblRuntimeEffectsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlRuntimeEffectsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Runtime Effects'
            end>
          ExplicitWidth = 303
        end
        object svgRuntimeEffectsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlRuntimeEffectsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlRuntimeEffectsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlText: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 121
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 7
        OnClick = pnlTextClick
        DesignSize = (
          303
          50)
        object lblTextDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlTextClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Learn how to draw simple and advanced texts'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblTextTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlTextClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Drawing Text'
            end>
          ExplicitWidth = 303
        end
        object svgTextArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlTextClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlTextLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlUnicode: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 521
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Color = 16249586
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ShowCaption = False
        TabOrder = 8
        OnClick = pnlUnicodeClick
        DesignSize = (
          303
          50)
        object lblUnicodeDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlUnicodeClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Use skia'#39's unicode to iterate through the graphemes'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblUnicodeTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlUnicodeClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Unicode'
            end>
          ExplicitWidth = 303
        end
        object svgUnicodeArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          ParentCustomHint = False
          Anchors = [akTop, akRight]
          OnClick = pnlUnicodeClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlUnicodeLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlTransforms: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 171
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Color = 16249586
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ShowCaption = False
        TabOrder = 9
        OnClick = pnlTransformsClick
        DesignSize = (
          303
          50)
        object lblTransformsDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlTransformsClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Learn about transforms for displaying graphics'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblTransformsTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlTransformsClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Transforms'
            end>
          ExplicitWidth = 303
        end
        object svgTransformsArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          ParentCustomHint = False
          Anchors = [akTop, akRight]
          OnClick = pnlTransformsClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlTransformsLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
      object pnlFilter: TPanel
        AlignWithMargins = True
        Left = 20
        Top = 371
        Width = 303
        Height = 50
        Cursor = crHandPoint
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 20
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15134704
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = True
        ParentFont = False
        ShowCaption = False
        TabOrder = 10
        OnClick = pnlFilterClick
        DesignSize = (
          303
          50)
        object lblFilterDescription: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 25
          Width = 287
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlFilterClick
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFF9FA5A8
          TextSettings.MaxLines = 1
          Words = <
            item
              Caption = 'Use filters in any kind of draw'
            end>
          ExplicitTop = 26
          ExplicitWidth = 303
        end
        object lblFilterTitle: TSkLabel
          AlignWithMargins = True
          Left = 0
          Top = 6
          Width = 287
          Height = 19
          Margins.Left = 0
          Margins.Top = 6
          Margins.Right = 16
          Margins.Bottom = 0
          Align = alTop
          OnClick = pnlFilterClick
          TextSettings.Font.Weight = Medium
          TextSettings.MaxLines = 2
          Words = <
            item
              Caption = 'Skia4Delphi Filter'
            end>
          ExplicitWidth = 303
        end
        object svgFilterArrow: TSkSvg
          Left = 290
          Top = 17
          Width = 16
          Height = 16
          Anchors = [akTop, akRight]
          OnClick = pnlFilterClick
          Svg.Source = 
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M12.314,47.255c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.3' +
            '91-1.023,0-1.414l21.92-21.92l-21.92-21.92'#13#10#9#9'c-0.391-0.391-0.391' +
            '-1.023,0-1.414s1.023-0.391,1.414,0L35.648,22.92c0.391,0.391,0.39' +
            '1,1.023,0,1.414L13.021,46.962'#13#10#9#9'C12.825,47.157,12.57,47.255,12.' +
            '314,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
          ExplicitLeft = 274
        end
        object pnlFilterLine: TPanel
          Left = 0
          Top = 49
          Width = 303
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = 15132908
          ParentBackground = False
          TabOrder = 0
          StyleElements = [seFont, seBorder]
        end
      end
    end
    inherited pnlTip: TPanel
      Top = 586
      ExplicitTop = 586
    end
  end
end
