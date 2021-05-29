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
  object ActionList1: TActionList
    Images = DmDb.imlTools
    Left = 168
    Top = 32
    object acConnect: TAction
      Caption = 'Connect'
      ImageIndex = 27
    end
    object acTest1: TAction
      Caption = 'Test1'
      OnExecute = acTest1Execute
    end
  end
end
