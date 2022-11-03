object frmBase: TfrmBase
  Left = 0
  Top = 0
  ClientHeight = 604
  ClientWidth = 360
  Color = 16184306
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  StyleElements = [seFont, seBorder]
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 15
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 604
    Align = alClient
    BevelOuter = bvNone
    Color = 16184306
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    OnResize = pnlContentResize
    object pnlTitle: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 360
      Height = 44
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      StyleElements = [seFont, seBorder]
      object lblTitle: TSkLabel
        Left = 0
        Top = 0
        Width = 360
        Height = 43
        Align = alClient
        TextSettings.Font.Size = 16.000000000000000000
        TextSettings.Font.Weight = Semibold
        TextSettings.FontColor = xFF1B1B1B
        TextSettings.HorzAlign = Center
        Words = <
          item
            Caption = 'Title'
          end>
        ExplicitLeft = 168
        ExplicitTop = 8
        ExplicitWidth = 63
        ExplicitHeight = 19
      end
      object pnlTitleLine: TPanel
        Left = 0
        Top = 43
        Width = 360
        Height = 1
        Align = alBottom
        BevelOuter = bvNone
        Color = 15132908
        ParentBackground = False
        TabOrder = 0
        StyleElements = [seFont, seBorder]
      end
      object pnlBack: TPanel
        Left = 0
        Top = 0
        Width = 42
        Height = 42
        Cursor = crHandPoint
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        OnClick = pnlBackClick
        object svgBackArrow: TSkSvg
          Left = 13
          Top = 13
          Width = 16
          Height = 16
          OnClick = pnlBackClick
          Svg.Source =
            '<?xml version="1.0" encoding="iso-8859-1"?>'#13#10'<!-- Generator: Ado' +
            'be Illustrator 19.0.0, SVG Export Plug-In . SVG Version: 6.00 Bu' +
            'ild 0)  -->'#13#10'<svg version="1.1" id="Capa_1" xmlns="http://www.w3' +
            '.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px' +
            '" y="0px"'#13#10#9' viewBox="0 0 47.255 47.255" style="enable-backgroun' +
            'd:new 0 0 47.255 47.255;" xml:space="preserve">'#13#10'<g>'#13#10#9'<path d="' +
            'M34.941,47.255c-0.256,0-0.512-0.098-0.707-0.293L11.607,24.334c-0' +
            '.391-0.391-0.391-1.023,0-1.414L34.234,0.293'#13#10#9#9'c0.391-0.391,1.02' +
            '3-0.391,1.414,0s0.391,1.023,0,1.414l-21.92,21.92l21.92,21.92c0.3' +
            '91,0.391,0.391,1.023,0,1.414'#13#10#9#9'C35.453,47.157,35.197,47.255,34.' +
            '941,47.255z"/>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>' +
            #13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</' +
            'g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'<g>'#13#10'</g>'#13#10'</svg' +
            '>'
        end
      end
    end
    object sbxContent: TScrollBox
      Left = 0
      Top = 44
      Width = 360
      Height = 526
      HorzScrollBar.Tracking = True
      VertScrollBar.Tracking = True
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Padding.Top = 21
      ParentBackground = True
      TabOrder = 1
      StyleElements = [seFont, seBorder]
    end
    object pnlTip: TPanel
      Left = 0
      Top = 570
      Width = 360
      Height = 34
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 2
      Visible = False
      StyleElements = [seFont, seBorder]
      object svgTipIcon: TSkSvg
        AlignWithMargins = True
        Left = 14
        Top = 7
        Width = 23
        Height = 27
        Margins.Left = 14
        Margins.Top = 6
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        Svg.OverrideColor = xFFC6C6C6
        Svg.Source =
          '<svg xmlns="http://www.w3.org/2000/svg" enable-background="new 0' +
          ' 0 24 24" viewBox="0 0 24 24"><path fill="#3b65f5" d="M12,2C6.5,' +
          '2,2,6.5,2,12c0,5.5,4.5,10,10,10s10-4.5,10-10C22,6.5,17.5,2,12,2z' +
          ' M12,20c-4.4,0-8-3.6-8-8s3.6-8,8-8s8,3.6,8,8S16.4,20,12,20z"/><p' +
          'ath fill="#3b65f5" d="M12,9.7c-0.6,0-1,0.4-1,1v5.4c0,0.6,0.4,1,1' +
          ',1s1-0.4,1-1v-5.4C13,10.2,12.6,9.7,12,9.7z"/><circle cx="12" cy=' +
          '"8" r="1" fill="#3b65f5"/></svg>'
        ExplicitTop = 9
        ExplicitHeight = 93
      end
      object pnlTipLine: TPanel
        Left = 0
        Top = 0
        Width = 360
        Height = 1
        Align = alTop
        BevelOuter = bvNone
        Color = 15132908
        ParentBackground = False
        TabOrder = 0
        StyleElements = [seFont, seBorder]
      end
      object pnlTipContent: TPanel
        AlignWithMargins = True
        Left = 51
        Top = 7
        Width = 295
        Height = 27
        Margins.Left = 14
        Margins.Top = 6
        Margins.Right = 14
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        ExplicitHeight = 21
        object lblTipDescription: TSkLabel
          Left = 0
          Top = 0
          Width = 295
          Height = 0
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          OnResize = pnlContentResize
          TextSettings.Font.Size = 11.000000000000000000
          TextSettings.FontColor = xFFA0A0A1
          Words = <
            item
            end>
          ExplicitWidth = 294
        end
      end
    end
  end
end
