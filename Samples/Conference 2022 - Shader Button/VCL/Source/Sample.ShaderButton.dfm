object frmShaderButton: TfrmShaderButton
  Left = 0
  Top = 0
  Width = 537
  Height = 75
  TabOrder = 0
  object apbBackground: TSkAnimatedPaintBox
    Left = 0
    Top = 0
    Width = 537
    Height = 75
    Align = alClient
    Animation.Duration = 6.000000000000000000
    OnAnimationDraw = apbBackgroundAnimationDraw
    object lblText: TSkLabel
      Left = 0
      Top = 0
      Width = 537
      Height = 75
      Align = alClient
      Visible = False
      TextSettings.Font.Size = 20.000000000000000000
      TextSettings.Font.Weight = Semibold
      TextSettings.FontColor = xFFFEFFFF
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'Assine por R$ 12,90 / m'#234's'
        end>
      ExplicitWidth = 233
      ExplicitHeight = 27
    end
  end
end
