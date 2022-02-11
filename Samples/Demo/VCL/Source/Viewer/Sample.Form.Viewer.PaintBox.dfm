inherited frmPaintBoxViewer: TfrmPaintBoxViewer
  ExplicitWidth = 320
  ExplicitHeight = 240
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
      object pbxDraw: TSkPaintBox
        Left = 0
        Top = 0
        Width = 50
        Height = 50
        OnMouseDown = pbxDrawMouseDown
        OnMouseMove = pbxDrawMouseMove
        OnMouseUp = pbxDrawMouseUp
        OnDraw = pbxDrawDraw
      end
    end
  end
end
