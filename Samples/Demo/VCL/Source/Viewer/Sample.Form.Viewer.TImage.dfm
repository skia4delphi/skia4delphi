inherited frmTImageViewer: TfrmTImageViewer
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    inherited pnlTitle: TPanel
      inherited lblTitle: TSkLabel
        Words = <
          item
            Caption = 'TImage Viewer'
          end>
      end
    end
    inherited sbxContent: TScrollBox
      object imgImage: TImage
        Left = 0
        Top = 0
        Width = 105
        Height = 105
      end
    end
  end
end
