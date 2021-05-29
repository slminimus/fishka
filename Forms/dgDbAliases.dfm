object dlgDbAliases: TdlgDbAliases
  Left = 0
  Top = 0
  Caption = #1041#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  ClientHeight = 280
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Grid: TcxGrid
    Left = 0
    Top = 0
    Width = 455
    Height = 239
    Align = alClient
    TabOrder = 0
    LookAndFeel.Kind = lfOffice11
    LookAndFeel.NativeStyle = False
    object View: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = False
      Navigator.Buttons.PriorPage.Visible = False
      Navigator.Buttons.Prior.Visible = False
      Navigator.Buttons.Next.Visible = False
      Navigator.Buttons.NextPage.Visible = False
      Navigator.Buttons.Last.Visible = False
      Navigator.Buttons.Edit.Visible = False
      Navigator.Buttons.Refresh.Visible = False
      Navigator.Buttons.SaveBookmark.Visible = False
      Navigator.Buttons.GotoBookmark.Visible = False
      Navigator.Buttons.Filter.Visible = False
      Navigator.InfoPanel.DisplayMask = '[RecordIndex] : [RecordCount]'
      Navigator.InfoPanel.Visible = True
      Navigator.Visible = True
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsData.Appending = True
      OptionsData.DeletingConfirmation = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object View_Alias: TcxGridColumn
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        BestFitMaxWidth = 132
        HeaderAlignmentHorz = taCenter
        MinWidth = 132
        Width = 132
      end
      object View_DbPath: TcxGridColumn
        Caption = #1055#1091#1090#1100' '#1082' '#1041#1044
        HeaderAlignmentHorz = taCenter
        Width = 321
      end
    end
    object Level: TcxGridLevel
      GridView = View
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 239
    Width = 455
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    ShowCaption = False
    TabOrder = 1
    DesignSize = (
      455
      41)
    object btnCancel: TButton
      Left = 86
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 6
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
end
