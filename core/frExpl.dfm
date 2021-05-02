object Expl: TExpl
  Left = 0
  Top = 0
  Caption = 'frmExpl'
  ClientHeight = 434
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = pmForm
  Position = poDesigned
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 289
    Top = 0
    Width = 6
    Height = 434
    Beveled = True
    ExplicitTop = 24
    ExplicitHeight = 410
  end
  object PgsMain: TcxPageControl
    Left = 295
    Top = 0
    Width = 517
    Height = 434
    Align = alClient
    ParentBackground = False
    TabOrder = 0
    TabStop = False
    Properties.ActivePage = tsPlace
    Properties.CloseButtonMode = cbmEveryTab
    Properties.CustomButtons.Buttons = <>
    Properties.HotTrack = True
    Properties.Options = [pcoNoArrows]
    Properties.ShowTabHints = True
    Properties.Style = 6
    OnCanCloseEx = PgsMainCanCloseEx
    OnChange = PgsMainChange
    OnGetTabHint = PgsMainGetTabHint
    ClientRectBottom = 434
    ClientRectRight = 517
    ClientRectTop = 25
    object tsPlace: TcxTabSheet
      Caption = #1082#1072#1088#1090#1086#1095#1082#1072
      ImageIndex = 0
    end
  end
  object PgsTree: TcxPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 434
    Align = alLeft
    TabOrder = 1
    TabStop = False
    Properties.ActivePage = tsTree
    Properties.CustomButtons.Buttons = <>
    Properties.HideTabs = True
    Properties.Options = [pcoNoArrows]
    Properties.Style = 6
    ClientRectBottom = 434
    ClientRectRight = 289
    ClientRectTop = 0
    object tsTree: TcxTabSheet
      Caption = 'tsTree'
      ImageIndex = 0
      object TreeView: TcxTreeView
        Left = 0
        Top = 0
        Width = 289
        Height = 434
        Align = alClient
        Style.BorderStyle = cbs3D
        Style.Edges = [bLeft, bTop, bRight, bBottom]
        TabOrder = 0
        HideSelection = False
        Images = dmCommon.NilTree
        ReadOnly = True
        RightClickSelect = True
        OnChange = TreeViewChange
        OnChanging = TreeViewChanging
        OnCollapsing = TreeViewCollapsing
        OnDeletion = TreeViewDeletion
        OnExpanding = TreeViewExpanding
        OnCreateNodeClass = TreeViewCreateNodeClass
      end
    end
  end
  object pmForm: TPopupMenu
    Left = 232
    Top = 184
    object miFocusTree: TMenuItem
      Caption = 'FocusTree'
      ShortCut = 16576
      Visible = False
      OnClick = miFocusTreeClick
    end
    object miTreeVisible: TMenuItem
      Caption = 'ComplexTreeVisible'
      Visible = False
      OnClick = miTreeVisibleClick
    end
    object miClosePage: TMenuItem
      Caption = 'ClosePage'
      ShortCut = 16392
      Visible = False
      OnClick = miClosePageClick
    end
    object miNextPage: TMenuItem
      Caption = 'NextPage'
      ShortCut = 16393
      Visible = False
      OnClick = miNextPageClick
    end
    object miPriorPage: TMenuItem
      Caption = 'PriorPage'
      ShortCut = 24585
      Visible = False
      OnClick = miPriorPageClick
    end
    object miDoneComplex: TMenuItem
      Caption = 'Done Complex'
      ShortCut = 24584
      Visible = False
      OnClick = miDoneComplexClick
    end
  end
end
