object MainPages: TMainPages
  Left = 0
  Top = 0
  Caption = 'MainPages'
  ClientHeight = 670
  ClientWidth = 905
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Pgs: TcxPageControl
    Left = 0
    Top = 0
    Width = 905
    Height = 670
    Align = alClient
    PopupMenu = pmTabs
    TabOrder = 0
    TabStop = False
    Properties.AllowTabDragDrop = True
    Properties.CloseButtonMode = cbmActiveTab
    Properties.CustomButtons.Buttons = <>
    Properties.HotTrack = True
    Properties.Options = [pcoAlwaysShowGoDialogButton, pcoGoDialog, pcoGradient, pcoGradientClientArea, pcoRedrawOnResize]
    Properties.ShowTabHints = True
    OnCanCloseEx = PgsCanCloseEx
    OnDblClick = PgsDblClick
    OnMouseUp = PgsMouseUp
    OnPageChanging = PgsPageChanging
    OnTabEndDrag = PgsTabEndDrag
    OnTabStartDrag = PgsTabStartDrag
    ClientRectBottom = 666
    ClientRectLeft = 4
    ClientRectRight = 901
    ClientRectTop = 4
  end
  object pmTabs: TPopupMenu
    Left = 368
    Top = 100
    object miClosePage: TMenuItem
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1079#1072#1082#1083#1072#1076#1082#1091
      OnClick = miClosePageClick
    end
    object miCloseOthers: TMenuItem
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1079#1072#1082#1083#1072#1076#1082#1080', '#1082#1088#1086#1084#1077' '#1101#1090#1086#1081
      OnClick = miCloseOthersClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object miSaveDesktop: TMenuItem
      Caption = #1047#1072#1087#1086#1084#1085#1080#1090#1100' '#1079#1072#1082#1083#1072#1076#1082#1080
      OnClick = miSaveDesktopClick
    end
  end
end
