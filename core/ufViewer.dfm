object Viewer: TViewer
  Left = 0
  Top = 0
  Caption = 'Viewer'
  ClientHeight = 450
  ClientWidth = 750
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainActions: TActionList
    Images = DmDb.imlTools
    Left = 328
    object acRefresh: TAction
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      Hint = #1054#1073#1085#1086#1074#1080#1090#1100
      ImageIndex = 7
      ShortCut = 16466
      OnExecute = acRefreshExecute
      OnUpdate = acRefreshUpdate
    end
    object acCard: TAction
      Caption = #1050#1072#1088#1090#1086#1095#1082#1072
      Hint = #1050#1072#1088#1090#1086#1095#1082#1072
      ImageIndex = 3
      OnExecute = acCardExecute
      OnUpdate = acCardUpdate
    end
    object acEdit: TAction
      Caption = #1048#1089#1087#1088#1072#1074#1080#1090#1100
      Hint = #1048#1089#1087#1088#1072#1074#1080#1090#1100
      ImageIndex = 4
    end
    object acInsert: TAction
      Caption = #1057#1086#1079#1076#1072#1090#1100
      Hint = #1057#1086#1079#1076#1072#1090#1100
      ImageIndex = 0
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      ShortCut = 16462
      OnExecute = acInsertExecute
      OnUpdate = acInsertUpdate
    end
    object acInsCopy: TAction
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1082#1086#1087#1080#1102
      Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1082#1086#1087#1080#1102' '#1090#1077#1082#1091#1097#1077#1081' '#1089#1090#1088#1086#1082#1080
      ImageIndex = 2
      SecondaryShortCuts.Strings = (
        'Shift+Ctrl+Ins')
      ShortCut = 24654
      OnExecute = acInsCopyExecute
      OnUpdate = acInsCopyUpdate
    end
    object acPost: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Enabled = False
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ImageIndex = 5
      ShortCut = 16467
      OnExecute = acPostExecute
      OnUpdate = acPostUpdate
    end
    object acDelete: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Hint = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 1
      ShortCut = 16430
      OnExecute = acDeleteExecute
      OnUpdate = acDeleteUpdate
    end
    object acCancel: TAction
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      Enabled = False
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100' '#1080#1079#1084#1077#1085#1077#1085#1080#1103
      ImageIndex = 6
      ShortCut = 16474
      OnExecute = acCancelExecute
      OnUpdate = acCancelUpdate
    end
  end
  object BarMan: TdxBarManager
    Scaled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Default'
      'CardAlign')
    Categories.ItemsVisibles = (
      2
      2)
    Categories.Visibles = (
      True
      True)
    ImageOptions.Images = DmDb.imlTools
    ImageOptions.LargeImages = DmDb.imlTools
    ImageOptions.StretchGlyphs = False
    ImageOptions.UseLargeImagesForLargeIcons = True
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 264
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      34
      0)
    object tbrMain: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Custom 1'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 778
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'tbnRefresh'
        end
        item
          Visible = True
          ItemName = 'tbnInsert'
        end
        item
          Visible = True
          ItemName = 'tbnPost'
        end
        item
          Visible = True
          ItemName = 'tbnDelete'
        end
        item
          Visible = True
          ItemName = 'tbnCancel'
        end>
      NotDocking = [dsNone, dsLeft, dsTop, dsRight, dsBottom]
      OneOnRow = True
      Row = 0
      ShowMark = False
      SizeGrip = False
      UseOwnFont = False
      UseRecentItems = False
      UseRestSpace = True
      Visible = True
      WholeRow = True
    end
    object tbnRefresh: TdxBarLargeButton
      Action = acRefresh
      Category = 0
      AutoGrayScale = False
      ShowCaption = False
    end
    object tbnInsert: TdxBarLargeButton
      Action = acInsert
      Category = 0
      ButtonStyle = bsDropDown
      DropDownMenu = ddmInsert
      AutoGrayScale = False
      ShowCaption = False
      SyncImageIndex = False
      ImageIndex = 0
    end
    object tbnInsCopy: TdxBarLargeButton
      Action = acInsCopy
      Category = 0
    end
    object tbnPost: TdxBarLargeButton
      Action = acPost
      Category = 0
      AutoGrayScale = False
      ShowCaption = False
    end
    object tbnDelete: TdxBarLargeButton
      Action = acDelete
      Category = 0
      AutoGrayScale = False
      ShowCaption = False
    end
    object tbnCancel: TdxBarLargeButton
      Action = acCancel
      Category = 0
      AutoGrayScale = False
      ShowCaption = False
    end
    object tbnCard: TdxBarLargeButton
      Action = acCard
      Category = 0
      ButtonStyle = bsCheckedDropDown
      DropDownMenu = ddmCard
      AutoGrayScale = False
      ShowCaption = False
      SyncImageIndex = False
      ImageIndex = 3
    end
    object dxBarSeparator2: TdxBarSeparator
      Category = 0
      Visible = ivAlways
    end
    object tbnCardLeft: TdxBarButton
      Tag = 1
      Caption = #1057#1083#1077#1074#1072
      Category = 1
      Hint = #1057#1083#1077#1074#1072
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 23
      OnClick = tbnCardAlignClick
    end
    object tbnCardRight: TdxBarButton
      Tag = 2
      Caption = #1057#1087#1088#1072#1074#1072
      Category = 1
      Hint = #1057#1087#1088#1072#1074#1072
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 24
      OnClick = tbnCardAlignClick
    end
    object tbnCardTop: TdxBarButton
      Tag = 3
      Caption = #1057#1074#1077#1088#1093#1091
      Category = 1
      Hint = #1057#1074#1077#1088#1093#1091
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      Down = True
      ImageIndex = 22
      OnClick = tbnCardAlignClick
    end
    object tbnCardBottom: TdxBarButton
      Tag = 4
      Caption = #1057#1085#1080#1079#1091
      Category = 1
      Hint = #1057#1085#1080#1079#1091
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 21
      OnClick = tbnCardAlignClick
    end
    object tbnCardClient: TdxBarButton
      Tag = 5
      Caption = #1056#1072#1089#1090#1103#1085#1091#1090#1100
      Category = 1
      Hint = #1056#1072#1089#1090#1103#1085#1091#1090#1100
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 25
      OnClick = tbnCardAlignClick
    end
    object tbnCardForm: TdxBarButton
      Tag = 6
      Caption = #1054#1082#1085#1086
      Category = 1
      Hint = #1054#1082#1085#1086
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      ImageIndex = 26
      OnClick = tbnCardAlignClick
    end
  end
  object ddmInsert: TdxBarPopupMenu
    BarManager = BarMan
    ItemLinks = <
      item
        Visible = True
        ItemName = 'tbnInsCopy'
      end>
    UseOwnFont = False
    Left = 396
    Top = 2
    PixelsPerInch = 96
  end
  object ddmCard: TdxBarPopupMenu
    BarManager = BarMan
    ItemLinks = <
      item
        Visible = True
        ItemName = 'tbnCardLeft'
      end
      item
        Visible = True
        ItemName = 'tbnCardRight'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'tbnCardTop'
      end
      item
        Visible = True
        ItemName = 'tbnCardBottom'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'tbnCardClient'
      end
      item
        Visible = True
        ItemName = 'tbnCardForm'
      end>
    UseOwnFont = False
    Left = 456
    Top = 2
    PixelsPerInch = 96
  end
end
