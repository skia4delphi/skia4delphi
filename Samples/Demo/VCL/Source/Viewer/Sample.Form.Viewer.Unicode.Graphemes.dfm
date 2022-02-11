inherited frmUnicodeGraphemesViewer: TfrmUnicodeGraphemesViewer
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    inherited pnlTitle: TPanel
      inherited lblTitle: TSkLabel
        Words = <
          item
            Caption = 'Unicode Graphemes'
          end>
      end
    end
    inherited sbxContent: TScrollBox
      object lblResult: TSkLabel
        Left = 10
        Top = 10
        Width = 108
        Height = 126
        TextSettings.Font.Families = 'Consolas'
        TextSettings.Font.Size = 15.000000000000000000
        TextSettings.VertAlign = Leading
        Words = <
          item
            Caption = 'Text:'#13#10'  '
            Font.Weight = Bold
            Name = 'TextTitle'
            StyledSettings = [Family, Size, FontColor]
          end
          item
            Name = 'Text'
          end
          item
            Caption = #13#10#13#10'Length(Text):'#13#10'  '
            Font.Weight = Bold
            Name = 'LengthTitle'
            StyledSettings = [Family, Size, FontColor]
          end
          item
            Name = 'Length'
          end
          item
            Caption = #13#10#13#10'Graphemes:'
            Font.Weight = Bold
            Name = 'GraphemesTitle'
            StyledSettings = [Family, Size, FontColor]
          end
          item
            Name = 'Graphemes'
          end>
      end
    end
  end
end
