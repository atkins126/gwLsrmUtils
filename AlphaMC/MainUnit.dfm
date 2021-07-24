object frmMain: TfrmMain
  Left = 663
  Top = 276
  Width = 442
  Height = 540
  Caption = 'Alpha Monte-Carlo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    426
    481)
  PixelsPerInch = 96
  TextHeight = 13
  object grpDet: TGroupBox
    Left = 0
    Top = 0
    Width = 417
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    object lblDetB: TLabel
      Left = 8
      Top = 44
      Width = 3
      Height = 13
    end
    object lblDetA: TLabel
      Left = 8
      Top = 12
      Width = 109
      Height = 13
      Caption = 'Detector diameter, mm:'
    end
    object lbl10: TLabel
      Left = 248
      Top = 12
      Width = 92
      Height = 13
      Caption = 'Detector efficiency:'
    end
    object lblSunkXY: TLabel
      Left = 8
      Top = 72
      Width = 99
      Height = 13
      Caption = 'Detector border, mm:'
    end
    object lblDetSunkZ: TLabel
      Left = 240
      Top = 72
      Width = 104
      Height = 13
      Caption = 'Sunked detector, mm:'
    end
    object jvedtDetB: TJvValidateEdit
      Left = 176
      Top = 42
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 3
      TabOrder = 1
      Visible = False
    end
    object jvedtDetDiam: TJvValidateEdit
      Left = 176
      Top = 12
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 3
      TabOrder = 0
    end
    object jvedtDetEff: TJvValidateEdit
      Left = 352
      Top = 12
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 3
      EditText = '1'
      TabOrder = 2
    end
    object jvedtDetSunkZ: TJvValidateEdit
      Left = 352
      Top = 72
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 4
    end
    object jvedtDetSunkXY: TJvValidateEdit
      Left = 176
      Top = 72
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 3
    end
  end
  object btnStart: TButton
    Left = 112
    Top = 288
    Width = 73
    Height = 17
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnRecalcReal: TButton
    Left = 232
    Top = 288
    Width = 73
    Height = 17
    Caption = 'Recalc real'
    TabOrder = 1
    OnClick = btnRecalcRealClick
  end
  object grpSource: TGroupBox
    Left = 0
    Top = 118
    Width = 417
    Height = 161
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    object lbl2: TLabel
      Left = 5
      Top = 12
      Width = 108
      Height = 13
      Caption = 'Source1 diameter, mm:'
    end
    object lbl3: TLabel
      Left = 5
      Top = 42
      Width = 108
      Height = 13
      Caption = 'Source2 diameter, mm:'
    end
    object lbl4: TLabel
      Left = 5
      Top = 72
      Width = 162
      Height = 13
      Caption = 'Source1 to detector distance, mm:'
    end
    object lbl5: TLabel
      Left = 5
      Top = 102
      Width = 162
      Height = 13
      Caption = 'Source2 to detector distance, mm:'
    end
    object lbl6: TLabel
      Left = 5
      Top = 132
      Width = 142
      Height = 13
      Caption = 'Number of events, thousands:'
    end
    object lbl1: TLabel
      Left = 240
      Top = 72
      Width = 115
      Height = 13
      Caption = 'Source1 radial shift, mm:'
      Visible = False
    end
    object lbl14: TLabel
      Left = 240
      Top = 104
      Width = 115
      Height = 13
      Caption = 'Source2 radial shift, mm:'
      Visible = False
    end
    object jvedtEventsNum: TJvValidateEdit
      Left = 175
      Top = 132
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      TabOrder = 4
    end
    object jvedtSource1Diam: TJvValidateEdit
      Left = 175
      Top = 12
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 0
    end
    object jvedtSource2Diam: TJvValidateEdit
      Left = 175
      Top = 42
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 1
    end
    object jvedtSource1ToDetDist: TJvValidateEdit
      Left = 175
      Top = 72
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 2
    end
    object jvedtSource2ToDetDist: TJvValidateEdit
      Left = 175
      Top = 102
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 3
      TabOrder = 3
    end
    object jvedtSource1RadShift: TJvValidateEdit
      Left = 360
      Top = 72
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 2
      TabOrder = 5
      Visible = False
    end
    object jvedtSource2RadShift: TJvValidateEdit
      Left = 360
      Top = 104
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 2
      TabOrder = 6
      Visible = False
    end
  end
  object grpResults: TGroupBox
    Left = 0
    Top = 312
    Width = 417
    Height = 161
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    object lbl8: TLabel
      Left = 8
      Top = 52
      Width = 55
      Height = 13
      Caption = 'Efficiency1:'
    end
    object lbl9: TLabel
      Left = 8
      Top = 84
      Width = 55
      Height = 13
      Caption = 'Efficiency2:'
    end
    object lbl7: TLabel
      Left = 8
      Top = 124
      Width = 98
      Height = 13
      Caption = 'Efficiency ratio (2/1):'
    end
    object lbl11: TLabel
      Left = 176
      Top = 28
      Width = 21
      Height = 13
      Caption = 'Calc'
    end
    object lbl12: TLabel
      Left = 248
      Top = 15
      Width = 64
      Height = 26
      Caption = 'Calc with det efficiency'
      WordWrap = True
    end
    object lbl13: TLabel
      Left = 344
      Top = 28
      Width = 36
      Height = 13
      Caption = 'Error, %'
    end
    object gaug1: TGauge
      Left = 80
      Top = 56
      Width = 89
      Height = 17
      BackColor = clBtnFace
      Color = clBackground
      ForeColor = clActiveCaption
      ParentColor = False
      Progress = 0
    end
    object jvedtEff1: TJvValidateEdit
      Left = 176
      Top = 52
      Width = 55
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfCustom
      EditText = '0'
      TabOrder = 0
    end
    object pb1: TProgressBar
      Left = 64
      Top = 14
      Width = 89
      Height = 17
      Smooth = True
      TabOrder = 8
      Visible = False
    end
    object pb2: TProgressBar
      Left = 80
      Top = 86
      Width = 89
      Height = 17
      Smooth = True
      TabOrder = 9
    end
    object jvedtEff2: TJvValidateEdit
      Left = 176
      Top = 84
      Width = 55
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfCustom
      EditText = '0'
      TabOrder = 1
    end
    object jvedtEffRatio: TJvValidateEdit
      Left = 176
      Top = 116
      Width = 50
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 4
      TabOrder = 2
    end
    object jvedtRealEff1: TJvValidateEdit
      Left = 248
      Top = 52
      Width = 49
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfCustom
      TabOrder = 3
    end
    object jvedtRealEff2: TJvValidateEdit
      Left = 248
      Top = 84
      Width = 49
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfCustom
      TabOrder = 4
    end
    object jvedtEff1Error: TJvValidateEdit
      Left = 328
      Top = 52
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 2
      TabOrder = 5
    end
    object jvedtEff2Error: TJvValidateEdit
      Left = 328
      Top = 84
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 2
      TabOrder = 6
    end
    object jvedtRatioError: TJvValidateEdit
      Left = 328
      Top = 116
      Width = 49
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloatGeneral
      DecimalPlaces = 2
      TabOrder = 7
    end
  end
  object mm1: TMainMenu
    Left = 384
    Top = 40
    object mniFile: TMenuItem
      Caption = 'File'
      object mniOpenDet: TMenuItem
        Caption = 'Open detector'
        ShortCut = 16463
        OnClick = mniOpenDetClick
      end
      object mniSaveDet: TMenuItem
        Caption = 'Save detector'
        ShortCut = 16467
        OnClick = mniSaveDetClick
      end
    end
    object mniTools: TMenuItem
      Caption = 'Tools'
      object mniCalcDiamFromArea: TMenuItem
        Caption = 'Calc diameter from area'
        OnClick = mniCalcDiamFromAreaClick
      end
      object mniAddExpEff: TMenuItem
        Caption = 'Add experimental efficiency'
        Enabled = False
        OnClick = mniAddExpEffClick
      end
    end
    object mniOptions: TMenuItem
      Caption = 'Options'
      object mniRecDet: TMenuItem
        AutoCheck = True
        Caption = 'Rectangle detector'
        OnClick = mniRecDetClick
      end
      object mniRectangleSource: TMenuItem
        AutoCheck = True
        Caption = 'Rectangle source'
      end
      object mniAddParams: TMenuItem
        AutoCheck = True
        Caption = 'Additional parameters'
        OnClick = mniAddParamsClick
      end
      object mniDebug: TMenuItem
        AutoCheck = True
        Caption = 'Debug mode'
        Enabled = False
      end
    end
    object mniAbout: TMenuItem
      Caption = 'About'
      OnClick = mniAboutClick
    end
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Detector files (*.din)|*.din|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 408
    Top = 8
  end
  object dlgSave1: TSaveDialog
    DefaultExt = 'ini'
    Filter = 'Detector files (*.din)|*.din|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 408
    Top = 40
  end
end
