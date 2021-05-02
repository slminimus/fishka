object _SplashForm: T_SplashForm
  Left = 1298
  Top = 81
  AutoSize = True
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 200
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 1
    Top = 0
    Width = 440
    Height = 200
    Stretch = True
  end
  object Label1: TLabel
    Left = 0
    Top = 103
    Width = 441
    Height = 93
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Layout = tlCenter
    WordWrap = True
  end
  object lblTrial: TLabel
    Left = 243
    Top = 1
    Width = 195
    Height = 16
    Alignment = taRightJustify
    Caption = #1042#1077#1088#1089#1080#1103' '#1076#1083#1103' '#1086#1079#1085#1072#1082#1086#1084#1083#1077#1085#1080#1103
    Color = clCream
    Constraints.MinWidth = 150
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    Layout = tlCenter
    Visible = False
  end
  object Status: TStaticText
    Left = 0
    Top = 196
    Width = 441
    Height = 4
    Align = alBottom
    Alignment = taCenter
    BevelInner = bvNone
    BevelKind = bkTile
    ShowAccelChar = False
    TabOrder = 0
    Visible = False
    ExplicitWidth = 4
  end
end
