inherited frmUnicodeBiDiViewer: TfrmUnicodeBiDiViewer
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    inherited pnlTitle: TPanel
      inherited lblTitle: TSkLabel
        Words = <
          item
            Caption = 'Unicode BiDi'
          end>
      end
    end
    inherited sbxContent: TScrollBox
      object lblResult: TSkLabel
        Left = 10
        Top = 10
        Width = 99
        Height = 72
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
            Caption = #13#10#13#10'BiDi Region:'
            Font.Weight = Bold
            Name = 'BiDiRegionTitle'
            StyledSettings = [Family, Size, FontColor]
          end
          item
            Name = 'BiDiRegion'
          end>
      end
    end
  end
end
