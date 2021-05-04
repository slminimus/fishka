inherited vwrListDB: TvwrListDB
  Caption = 'vwrListDB'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgsMain: TcxPageControl
    inherited tsList: TcxTabSheet
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 748
      ExplicitHeight = 394
    end
    inherited tsCard: TcxTabSheet
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 748
      ExplicitHeight = 394
    end
  end
  inherited MainActions: TActionList
    Left = 618
  end
  inherited BarMan: TdxBarManager
    Left = 567
    PixelsPerInch = 96
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 468
    Top = 2
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    Left = 519
    Top = 2
    PixelsPerInch = 96
  end
end
