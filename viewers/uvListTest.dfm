inherited vwrListTest: TvwrListTest
  Caption = 'vwrListTest'
  ClientHeight = 452
  ExplicitHeight = 491
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgsMain: TcxPageControl
    Height = 418
    ExplicitHeight = 418
    ClientRectBottom = 417
    inherited tsList: TcxTabSheet
      ExplicitLeft = 1
      ExplicitTop = 21
      ExplicitWidth = 748
      ExplicitHeight = 396
      inherited grdMain: TcxGrid
        Height = 396
        ExplicitHeight = 396
        inherited gvMain: TcxGridTableView
          Styles.ContentEven = nil
          Styles.Footer = nil
          Styles.Group = nil
          Styles.GroupByBox = nil
          Styles.Header = nil
          Styles.Selection = nil
          object gvMain_ID: TcxGridColumn
            Caption = 'ID'
            Visible = False
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_CARDNUM: TcxGridColumn
            Caption = #1053#1086#1084#1077#1088
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_RELEASED: TcxGridColumn
            Caption = #1042#1099#1087#1091#1097#1077#1085#1072
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_FIO: TcxGridColumn
            Caption = #1060#1048#1054
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_PHONES: TcxGridColumn
            Caption = #1058#1077#1083#1077#1092#1086#1085
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_EMAIL: TcxGridColumn
            Caption = #1055#1086#1095#1090#1072
            HeaderAlignmentHorz = taCenter
          end
          object gvMain_DISCONT: TcxGridColumn
            Caption = #1057#1082#1080#1076#1082#1072', %'
            HeaderAlignmentHorz = taCenter
          end
        end
      end
    end
    inherited tsCard: TcxTabSheet
      ExplicitHeight = 396
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
    inherited tbnCardTop: TdxBarButton
      Down = False
    end
    inherited tbnCardBottom: TdxBarButton
      Down = True
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 520
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    Left = 470
    PixelsPerInch = 96
  end
end
