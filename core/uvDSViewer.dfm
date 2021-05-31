inherited vwrDSViewer: TvwrDSViewer
  Caption = 'vwrDSViewer'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BarMan: TdxBarManager
    PixelsPerInch = 96
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  object MemData: TdxMemData
    Indexes = <>
    SortOptions = []
    BeforePost = MemDataBeforePost
    Left = 264
    Top = 56
  end
  object DataSource: TDataSource
    DataSet = MemData
    Left = 328
    Top = 56
  end
end
