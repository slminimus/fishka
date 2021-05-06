inherited vwrList: TvwrList
  Caption = 'vwrList'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgsMain: TcxPageControl [0]
    Left = 0
    Top = 34
    Width = 750
    Height = 416
    Align = alClient
    TabOrder = 4
    TabStop = False
    Properties.ActivePage = tsList
    Properties.CustomButtons.Buttons = <>
    Properties.Options = [pcoAlwaysShowGoDialogButton, pcoRedrawOnResize]
    Properties.Style = 7
    ClientRectBottom = 415
    ClientRectLeft = 1
    ClientRectRight = 749
    ClientRectTop = 21
    object tsList: TcxTabSheet
      Caption = 'tsList'
      object grdMain: TcxGrid
        Left = 0
        Top = 0
        Width = 748
        Height = 394
        Align = alClient
        TabOrder = 0
        object lvlMain: TcxGridLevel
        end
      end
    end
    object tsCard: TcxTabSheet
      Caption = 'tsCard'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  inherited MainActions: TActionList
    Left = 528
    Top = 2
  end
  inherited BarMan: TdxBarManager
    Left = 464
    Top = 2
    PixelsPerInch = 96
    inherited tbrMain: TdxBar
      ItemLinks = <
        item
          Visible = True
          ItemName = 'tbnRefresh'
        end
        item
          Visible = True
          ItemName = 'tbnCard'
        end
        item
          Visible = True
          ItemName = 'tbnInsert'
        end
        item
          Visible = True
          ItemName = 'tbnPost'
        end
        item
          Visible = True
          ItemName = 'tbnDelete'
        end
        item
          Visible = True
          ItemName = 'tbnCancel'
        end>
    end
    inherited tbnCard: TdxBarLargeButton [2]
      AllowAllUp = True
      ImageIndex = 3
    end
    inherited tbnInsert: TdxBarLargeButton [3]
      ImageIndex = 0
    end
    inherited tbnInsCopy: TdxBarLargeButton [4]
    end
    inherited tbnPost: TdxBarLargeButton [5]
    end
    inherited tbnDelete: TdxBarLargeButton [6]
    end
    inherited tbnCancel: TdxBarLargeButton [7]
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 596
    Top = 4
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    Left = 648
    Top = 4
    PixelsPerInch = 96
  end
end
