object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 380
  ClientWidth = 390
  Color = 2104865
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  StyleElements = [seFont, seBorder]
  DesignSize = (
    390
    380)
  TextHeight = 15
  inline frmShaderButton1: TfrmShaderButton
    Left = 16
    Top = 165
    Width = 358
    Height = 50
    Cursor = crHandPoint
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 165
    ExplicitWidth = 358
    inherited apbBackground: TSkAnimatedPaintBox
      Cursor = crHandPoint
      inherited lblText: TSkLabel
        Cursor = crHandPoint
      end
    end
  end
end
