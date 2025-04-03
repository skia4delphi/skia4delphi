object SkIntfDemoMain: TSkIntfDemoMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Skia4Delphi Proposal'
  ClientHeight = 405
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 25
  object SkPaintBox1: TSkPaintBox
    Left = 10
    Top = 10
    Width = 385
    Height = 277
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    OnDraw = SkPaintBox1Draw
  end
end
