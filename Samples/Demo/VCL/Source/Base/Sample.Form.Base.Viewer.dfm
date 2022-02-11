inherited frmBaseViewer: TfrmBaseViewer
  ClientHeight = 578
  ClientWidth = 351
  OnClose = FormClose
  OnDestroy = FormDestroy
  ExplicitWidth = 367
  ExplicitHeight = 617
  PixelsPerInch = 96
  TextHeight = 15
  inherited pnlContent: TPanel
    Width = 351
    Height = 578
    ExplicitWidth = 351
    ExplicitHeight = 578
    inherited pnlTitle: TPanel
      Width = 351
      ExplicitWidth = 351
      inherited lblTitle: TSkLabel
        Width = 351
        Words = <
          item
            Caption = 'Base Viewer'
          end>
        ExplicitWidth = 351
      end
      inherited pnlTitleLine: TPanel
        Width = 351
        ExplicitWidth = 351
      end
    end
    inherited sbxContent: TScrollBox
      Width = 351
      Height = 500
      DoubleBuffered = True
      Padding.Top = 0
      ParentBackground = False
      ParentDoubleBuffered = False
      ExplicitWidth = 351
      ExplicitHeight = 500
    end
    inherited pnlTip: TPanel
      Top = 544
      Width = 351
      ExplicitTop = 544
      ExplicitWidth = 351
      inherited pnlTipLine: TPanel
        Width = 351
        ExplicitWidth = 351
      end
      inherited pnlTipContent: TPanel
        Width = 286
        ExplicitWidth = 286
        inherited lblTipDescription: TSkLabel
          Width = 286
          ExplicitWidth = 286
        end
      end
    end
  end
end
