inherited vwrEntities: TvwrEntities
  Caption = 'vwrEntities'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgsMain: TcxPageControl
    inherited tsList: TcxTabSheet
      inherited grdMain: TcxGrid
        inherited gvMain: TcxGridTableView
          Styles.ContentEven = nil
          Styles.Footer = nil
          Styles.Group = nil
          Styles.GroupByBox = nil
          Styles.Header = nil
          Styles.Inactive = nil
          Styles.Selection = nil
        end
      end
    end
  end
  inherited BarMan: TdxBarManager
    PixelsPerInch = 96
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    PixelsPerInch = 96
  end
end
