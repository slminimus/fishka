inherited vwrCard: TvwrCard
  BorderStyle = bsSizeToolWin
  Caption = 'vwrCard'
  ClientHeight = 344
  ClientWidth = 729
  FormStyle = fsStayOnTop
  Position = poDesigned
  OnCreate = FormCreate
  ExplicitWidth = 745
  ExplicitHeight = 383
  PixelsPerInch = 96
  TextHeight = 13
  object DockPanel: TPanel [0]
    Tag = -1
    Left = 0
    Top = 34
    Width = 729
    Height = 310
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 170
    Constraints.MinWidth = 550
    ShowCaption = False
    TabOrder = 4
    ExplicitWidth = 550
    ExplicitHeight = 170
  end
  inherited MainActions: TActionList
    Left = 600
    Top = 2
  end
  inherited BarMan: TdxBarManager
    Left = 549
    Top = 2
    PixelsPerInch = 96
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 659
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  object srcRecord: TDataSource
    DataSet = dsRecord
    Left = 403
    Top = 2
  end
  object dsRecord: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 504
    Top = 2
  end
end
