inherited vvrListTblView: TvvrListTblView
  Caption = 'vvrListTblView'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pgsMain: TcxPageControl
    LookAndFeel.NativeStyle = False
    inherited tsList: TcxTabSheet
      inherited grdMain: TcxGrid
        LookAndFeel.NativeStyle = False
        object gvMain: TcxGridTableView [0]
          Navigator.Buttons.CustomButtons = <>
          Navigator.Buttons.First.Visible = True
          Navigator.Buttons.PriorPage.Visible = False
          Navigator.Buttons.Prior.Visible = False
          Navigator.Buttons.Next.Visible = False
          Navigator.Buttons.NextPage.Visible = False
          Navigator.Buttons.Last.Visible = True
          Navigator.Buttons.Insert.Visible = False
          Navigator.Buttons.Append.Visible = False
          Navigator.Buttons.Delete.Visible = False
          Navigator.Buttons.Edit.Visible = False
          Navigator.Buttons.Post.Visible = False
          Navigator.Buttons.Cancel.Visible = False
          Navigator.Buttons.Refresh.Visible = False
          Navigator.Buttons.SaveBookmark.Visible = False
          Navigator.Buttons.GotoBookmark.Visible = False
          Navigator.Buttons.Filter.Visible = True
          Navigator.InfoPanel.DisplayMask = '[RecordIndex] : [RecordCount]'
          Navigator.InfoPanel.Visible = True
          Navigator.Visible = True
          OnCanFocusRecord = ViewCanFocusRecord
          OnCellDblClick = ViewCellDblClick
          OnFocusedRecordChanged = ViewFocusedRecordChanged
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsBehavior.IncSearch = True
          OptionsBehavior.ImmediateEditor = False
          OptionsCustomize.ColumnsQuickCustomization = True
          OptionsData.Deleting = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          Styles.StyleSheet = DmDb.stsGridView
        end
        inherited lvlMain: TcxGridLevel
          GridView = gvMain
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
