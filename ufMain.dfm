object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 709
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 215
    Top = 8
    Width = 90
    Height = 25
    Action = acTest1
    TabOrder = 0
  end
  object cxVerticalGrid1: TcxVerticalGrid
    Left = 8
    Top = 8
    Width = 201
    Height = 65
    OptionsView.RowHeaderWidth = 93
    TabOrder = 1
    Version = 1
    object edURL: TcxEditorRow
      Properties.Caption = 'URL'
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = 'localhost:Halt'
      ID = 0
      ParentID = -1
      Index = 0
      Version = 1
    end
    object edLogin: TcxEditorRow
      Properties.Caption = 'Login'
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = 'SYSDBA'
      ID = 1
      ParentID = -1
      Index = 1
      Version = 1
    end
    object edPass: TcxEditorRow
      Properties.Caption = 'Password'
      Properties.DataBinding.ValueType = 'String'
      Properties.Value = 'sa'
      ID = 2
      ParentID = -1
      Index = 2
      Version = 1
    end
  end
  object btnConnect: TcxButton
    Left = 215
    Top = 40
    Width = 90
    Height = 33
    Action = acConnect
    SpeedButtonOptions.AllowAllUp = True
    TabOrder = 2
  end
  object ActionList1: TActionList
    Images = DmDb.imlTools
    Left = 16
    Top = 80
    object acConnect: TAction
      Caption = 'Connect'
      ImageIndex = 27
      OnExecute = acConnectExecute
      OnUpdate = acConnectUpdate
    end
    object acTest1: TAction
      Caption = 'Test1'
      OnExecute = acTest1Execute
      OnUpdate = acTest1Update
    end
  end
end
