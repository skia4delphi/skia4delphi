object frmShaderButton: TfrmShaderButton
  Left = 0
  Top = 0
  Width = 358
  Height = 50
  TabOrder = 0
  OnMouseDown = FrameMouseDown
  OnMouseUp = FrameMouseUp
  object apbBackground: TSkAnimatedPaintBox
    Left = 0
    Top = 0
    Width = 358
    Height = 50
    Align = alClient
    OnMouseDown = FrameMouseDown
    OnMouseUp = FrameMouseUp
    Animation.Duration = 6.000000000000000000
    OnAnimationDraw = apbBackgroundAnimationDraw
    object lblText: TSkLabel
      Left = 0
      Top = 0
      Width = 358
      Height = 50
      Align = alClient
      Visible = False
      OnMouseDown = FrameMouseDown
      OnMouseUp = FrameMouseUp
      TextSettings.Font.Size = 17.000000000000000000
      TextSettings.Font.Weight = Semibold
      TextSettings.FontColor = xFFFEFFFF
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'Subscribe for $4.99 per month'
        end>
      ExplicitWidth = 233
      ExplicitHeight = 27
    end
  end
end
