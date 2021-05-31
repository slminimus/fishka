inherited vwrTestCard: TvwrTestCard
  Caption = 'vwrTestCard'
  ClientHeight = 174
  ClientWidth = 373
  ExplicitWidth = 389
  ExplicitHeight = 213
  PixelsPerInch = 96
  TextHeight = 13
  inherited DockPanel: TPanel
    Width = 373
    Height = 140
    Constraints.MinHeight = 140
    Constraints.MinWidth = 373
    ExplicitWidth = 360
    ExplicitHeight = 185
    object vgEdit: TcxDBVerticalGrid
      Left = 0
      Top = 0
      Width = 373
      Height = 140
      Align = alClient
      OptionsView.ScrollBars = ssVertical
      OptionsView.RowHeaderWidth = 110
      OptionsBehavior.RowTracking = False
      OptionsBehavior.AllowChangeRecord = False
      OptionsBehavior.RecordScrollMode = rsmByRecord
      OptionsData.Appending = False
      OptionsData.Deleting = False
      OptionsData.Inserting = False
      Navigator.Buttons.CustomButtons = <>
      Styles.StyleSheet = DmDb.stsVerticalGrid
      TabOrder = 0
      DataController.DataSource = srcRecord
      ExplicitWidth = 360
      ExplicitHeight = 185
      Version = 1
      object vgEditID: TcxDBEditorRow
        Properties.EditPropertiesClassName = 'TcxMaskEditProperties'
        Properties.EditProperties.ReadOnly = True
        Properties.DataBinding.FieldName = 'ID'
        Properties.Options.Editing = False
        Styles.Header = DmDb.stlInfo
        ID = 0
        ParentID = -1
        Index = 0
        Version = 1
      end
      object vgEditCARDNUM: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'CARDNUM'
        ID = 1
        ParentID = -1
        Index = 1
        Version = 1
      end
      object vgEditRELEASED: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'RELEASED'
        ID = 2
        ParentID = -1
        Index = 2
        Version = 1
      end
      object vgEditFIO: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'FIO'
        ID = 3
        ParentID = -1
        Index = 3
        Version = 1
      end
      object vgEditPHONES: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'PHONES'
        ID = 4
        ParentID = -1
        Index = 4
        Version = 1
      end
      object vgEditEMAIL: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'EMAIL'
        ID = 5
        ParentID = -1
        Index = 5
        Version = 1
      end
      object vgEditDISCONT: TcxDBEditorRow
        Properties.EditPropertiesClassName = 'TcxMaskEditProperties'
        Properties.EditProperties.Alignment.Horz = taLeftJustify
        Properties.EditProperties.MaskKind = emkRegExpr
        Properties.EditProperties.EditMask = '\d+'
        Properties.DataBinding.FieldName = 'DISCONT'
        ID = 6
        ParentID = -1
        Index = 6
        Version = 1
      end
    end
  end
  inherited MainActions: TActionList
    Left = 248
    Top = 48
  end
  inherited BarMan: TdxBarManager
    Left = 128
    Top = 49
    PixelsPerInch = 96
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 180
    Top = 50
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    Left = 128
    Top = 98
    PixelsPerInch = 96
  end
  inherited srcRecord: TDataSource
    Left = 244
    Top = 98
  end
  inherited dsRecord: TdxMemData
    Left = 180
    Top = 98
    object dsRecordID: TStringField
      FieldName = 'ID'
      Size = 36
    end
    object dsRecordCARDNUM: TStringField
      DisplayLabel = #1053#1086#1084#1077#1088
      FieldName = 'CARDNUM'
      Size = 36
    end
    object dsRecordRELEASED: TDateTimeField
      DisplayLabel = #1042#1099#1087#1091#1097#1077#1085#1072
      FieldName = 'RELEASED'
    end
    object dsRecordFIO: TStringField
      DisplayLabel = #1060#1048#1054
      FieldName = 'FIO'
      Size = 64
    end
    object dsRecordPHONES: TStringField
      DisplayLabel = #1058#1077#1083#1077#1092#1086#1085#1099
      FieldName = 'PHONES'
      Size = 64
    end
    object dsRecordEMAIL: TStringField
      DisplayLabel = #1055#1086#1095#1090#1072
      FieldName = 'EMAIL'
      Size = 32
    end
    object dsRecordDISCONT: TIntegerField
      DisplayLabel = #1057#1082#1080#1076#1082#1072', %'
      FieldName = 'DISCONT'
    end
  end
end
