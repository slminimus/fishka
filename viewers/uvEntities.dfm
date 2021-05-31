inherited vwrEntities: TvwrEntities
  Caption = 'vwrEntities'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgsMain: TcxPageControl
    inherited tsList: TcxTabSheet
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 748
      ExplicitHeight = 394
      inherited grdMain: TcxGrid
        inherited gvMain: TcxGridTableView
          OptionsView.GroupByBox = False
          Styles.ContentEven = nil
          Styles.Footer = nil
          Styles.Group = nil
          Styles.GroupByBox = nil
          Styles.Header = nil
          Styles.Inactive = nil
          Styles.Selection = nil
          object gvMain_ID: TcxGridColumn
            Caption = 'ID'
            Visible = False
            HeaderAlignmentHorz = taCenter
            Width = 147
          end
          object gvMain_NAME: TcxGridColumn
            Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
            HeaderAlignmentHorz = taCenter
            Width = 168
          end
          object gvMain_DESCR: TcxGridColumn
            Caption = #1055#1088#1080#1084#1077#1095#1072#1085#1080#1077
            HeaderAlignmentHorz = taCenter
            Width = 419
          end
        end
      end
    end
    inherited tsCard: TcxTabSheet
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 748
      ExplicitHeight = 394
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
