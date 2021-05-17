inherited vwrCard: TvwrCard
  BorderStyle = bsSizeToolWin
  Caption = 'vwrCard'
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DockPanel: TPanel [0]
    Tag = -1
    Left = 0
    Top = 34
    Width = 750
    Height = 416
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 4
  end
  inherited MainActions: TActionList
    Left = 600
    Top = 2
  end
  inherited BarMan: TdxBarManager
    Left = 551
    Top = 1
    PixelsPerInch = 96
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 652
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    PixelsPerInch = 96
  end
  object dsRecord: THMemTable
    Left = 507
    Top = 1
  end
  object srcRecord: TDataSource
    DataSet = dsRecord
    Left = 456
    Top = 1
  end
end
