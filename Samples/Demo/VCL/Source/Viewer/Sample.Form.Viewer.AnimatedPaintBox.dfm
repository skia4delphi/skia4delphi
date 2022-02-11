inherited frmAnimatedPaintBoxViewer: TfrmAnimatedPaintBoxViewer
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    inherited pnlTitle: TPanel
      inherited lblTitle: TSkLabel
        Words = <
          item
            Caption = 'PaintBox Viewer'
          end>
      end
    end
    inherited sbxContent: TScrollBox
      OnMouseMove = apbDrawMouseMove
      object apbDraw: TSkAnimatedPaintBox
        Left = 0
        Top = 0
        Width = 50
        Height = 50
        OnMouseMove = apbDrawMouseMove
        Animate = False
        Duration = 100000.000000000000000000
        OnAnimationDraw = apbDrawAnimationDraw
      end
    end
  end
end
