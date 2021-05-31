inherited vwcMainTree: TvwcMainTree
  Caption = 'vwcMainTree'
  ClientHeight = 204
  ClientWidth = 440
  ExplicitWidth = 456
  ExplicitHeight = 243
  PixelsPerInch = 96
  TextHeight = 13
  inherited DockPanel: TPanel
    Width = 440
    Height = 170
    Constraints.MinHeight = 90
    Constraints.MinWidth = 440
    object vgEdit: TcxDBVerticalGrid
      Left = 0
      Top = 0
      Width = 440
      Height = 170
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
      ExplicitLeft = 176
      ExplicitTop = -56
      ExplicitWidth = 739
      ExplicitHeight = 422
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
      object vgEditNAME: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'NAME'
        ID = 1
        ParentID = -1
        Index = 1
        Version = 1
      end
      object vgEditENTITY: TcxDBEditorRow
        Properties.DataBinding.FieldName = 'ENTITY'
        ID = 3
        ParentID = -1
        Index = 2
        Version = 1
      end
      object vgEditTAG: TcxDBEditorRow
        Properties.EditPropertiesClassName = 'TcxMaskEditProperties'
        Properties.EditProperties.Alignment.Horz = taLeftJustify
        Properties.EditProperties.MaskKind = emkRegExpr
        Properties.EditProperties.EditMask = '\d+'
        Properties.DataBinding.FieldName = 'TAG'
        ID = 4
        ParentID = -1
        Index = 3
        Version = 1
      end
    end
  end
  inherited MainActions: TActionList
    Left = 216
    Top = 138
  end
  inherited BarMan: TdxBarManager
    Left = 165
    Top = 138
    PixelsPerInch = 96
    inherited tbnInsert: TdxBarLargeButton
      ImageIndex = 0
    end
    inherited tbnCard: TdxBarLargeButton
      ImageIndex = 3
    end
  end
  inherited ddmInsert: TdxBarPopupMenu
    Left = 275
    Top = 138
    PixelsPerInch = 96
  end
  inherited ddmCard: TdxBarPopupMenu
    Left = 72
    Top = 138
    PixelsPerInch = 96
  end
  inherited srcRecord: TDataSource
    Left = 19
    Top = 138
  end
  inherited dsRecord: TdxMemData
    Left = 120
    Top = 138
    object dsRecordID: TStringField
      FieldName = 'ID'
      Size = 36
    end
    object dsRecordNAME: TStringField
      DisplayLabel = #1058#1077#1082#1089#1090
      FieldName = 'NAME'
      Size = 32
    end
    object dsRecordPARENT: TStringField
      FieldName = 'PARENT'
      Size = 36
    end
    object dsRecordTAG: TIntegerField
      DisplayLabel = #1052#1072#1075#1080#1103
      FieldName = 'TAG'
    end
    object dsRecordENTITY: TStringField
      DisplayLabel = #1057#1091#1097#1085#1086#1089#1090#1100
      FieldName = 'ENTITY'
      Size = 36
    end
  end
end
