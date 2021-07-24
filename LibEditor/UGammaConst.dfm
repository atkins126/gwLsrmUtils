object frmGammaConst: TfrmGammaConst
  Left = 549
  Top = 285
  Width = 365
  Height = 357
  Caption = 'GammaConst calculation'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = GrmCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 152
    Top = 48
    Width = 75
    Height = 13
    Hint = #1053#1077' '#1091#1095#1080#1090#1099#1074#1072#1090#1100' '#1101#1085#1077#1088#1075#1080#1080' '#1085#1080#1078#1077' '#1087#1086#1088#1086#1075#1086#1074#1086#1081' '#1074#1077#1083#1080#1095#1080#1085#1099
    Caption = 'Threshold, keV:'
    ParentShowHint = False
    ShowHint = True
  end
  object rg1: TRadioGroup
    Left = 8
    Top = 8
    Width = 121
    Height = 81
    Caption = 'Units'
    ItemIndex = 2
    Items.Strings = (
      #1056' '#1089#1084'2/('#1095' '#1084#1050#1080')'
      #1072#1043#1088' '#1084'2/('#1089' '#1041#1082')'
      #1072#1047#1074' '#1084'2/('#1089' '#1041#1082')')
    TabOrder = 0
    OnClick = rg1Click
  end
  object jvedtThresh: TJvValidateEdit
    Left = 232
    Top = 48
    Width = 49
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TabOrder = 2
    OnExit = jvedtThreshExit
  end
  object strngrdGamma: TStringGrid
    Left = 8
    Top = 104
    Width = 161
    Height = 201
    ColCount = 2
    DefaultColWidth = 66
    FixedCols = 0
    RowCount = 20
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
    TabOrder = 1
  end
  object grp1: TGroupBox
    Left = 184
    Top = 96
    Width = 161
    Height = 209
    Caption = 'Dose calculation'
    TabOrder = 3
    object lbl2: TLabel
      Left = 11
      Top = 40
      Width = 62
      Height = 13
      Caption = 'Distance, cm'
    end
    object lblDose: TLabel
      Left = 11
      Top = 144
      Width = 25
      Height = 13
      Caption = 'Dose'
    end
    object lbl3: TLabel
      Left = 11
      Top = 72
      Width = 53
      Height = 13
      Caption = 'Activity, Bq'
    end
    object lblDoseUnits: TLabel
      Left = 115
      Top = 144
      Width = 3
      Height = 13
    end
    object btnCalc: TButton
      Left = 32
      Top = 112
      Width = 65
      Height = 17
      Caption = 'Calc'
      TabOrder = 2
      OnClick = btnCalcClick
    end
    object jvedtDist: TJvValidateEdit
      Left = 80
      Top = 32
      Width = 57
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 2
      TabOrder = 0
    end
    object jvedtDose: TJvValidateEdit
      Left = 48
      Top = 144
      Width = 65
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfScientific
      DecimalPlaces = 3
      TabOrder = 3
    end
    object jvedtAct: TJvValidateEdit
      Left = 80
      Top = 72
      Width = 57
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfCustom
      EditText = '0'
      TabOrder = 1
    end
  end
end
