object frmMain: TfrmMain
  Left = 304
  Top = 221
  Width = 468
  Height = 489
  Caption = 'muDat'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl2: TLabel
    Left = 8
    Top = 144
    Width = 161
    Height = 26
    AutoSize = False
    Caption = 'Input energies in MeV, 1 per line.'#13#10'Blank lines will be ignored'
    WordWrap = True
  end
  object lbl6: TLabel
    Left = 216
    Top = 160
    Width = 124
    Height = 13
    Caption = 'Attenuation (mju) in cm2/g'
  end
  object lbl7: TLabel
    Left = 8
    Top = 368
    Width = 113
    Height = 13
    Caption = 'Enter material thick, cm:'
  end
  object grp1: TGroupBox
    Left = 8
    Top = 16
    Width = 185
    Height = 121
    Caption = 'Input'
    TabOrder = 0
    object lblRho: TLabel
      Left = 8
      Top = 88
      Width = 72
      Height = 13
      Caption = 'Density, g/cm3'
    end
    object rbElNum: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'element number'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbElNumNameClick
    end
    object rbElName: TRadioButton
      Left = 8
      Top = 40
      Width = 121
      Height = 17
      Caption = 'element name'
      TabOrder = 1
      OnClick = rbElNumNameClick
    end
    object edtElName: TEdit
      Left = 112
      Top = 40
      Width = 41
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = 'H'
      OnExit = edtElNameExit
    end
    object jvedtElNum: TJvValidateEdit
      Left = 112
      Top = 16
      Width = 41
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '1'
      HasMaxValue = True
      HasMinValue = True
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      TabOrder = 3
      OnExit = jvedtElNumExit
    end
    object rbMaterial: TRadioButton
      Left = 8
      Top = 64
      Width = 89
      Height = 17
      Caption = 'Material name'
      TabOrder = 2
      OnClick = rbElNumNameClick
    end
    object edtMatName: TEdit
      Left = 112
      Top = 64
      Width = 65
      Height = 21
      Enabled = False
      TabOrder = 5
    end
    object jvedtRho: TJvValidateEdit
      Left = 112
      Top = 88
      Width = 65
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      TrimDecimals = True
      DisplayFormat = dfFloat
      DecimalPlaces = 4
      HasMinValue = True
      TabOrder = 6
    end
  end
  object btnShowMju: TButton
    Left = 16
    Top = 400
    Width = 97
    Height = 17
    Caption = 'Show mju'
    Default = True
    TabOrder = 2
    OnClick = btnShowMjuClick
  end
  object mmoEnergies: TMemo
    Left = 8
    Top = 184
    Width = 177
    Height = 169
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object mmoOutput: TMemo
    Left = 216
    Top = 184
    Width = 225
    Height = 169
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object grp2: TGroupBox
    Left = 216
    Top = 8
    Width = 217
    Height = 105
    Caption = 'Energy scale'
    TabOrder = 4
    object lbl3: TLabel
      Left = 8
      Top = 16
      Width = 23
      Height = 13
      Caption = 'Emin'
    end
    object lbl4: TLabel
      Left = 8
      Top = 40
      Width = 26
      Height = 13
      Caption = 'Emax'
    end
    object lbl5: TLabel
      Left = 96
      Top = 16
      Width = 67
      Height = 13
      Caption = 'Points number'
    end
    object btnSetScale: TButton
      Left = 16
      Top = 72
      Width = 73
      Height = 17
      Caption = 'Set scale'
      TabOrder = 0
      OnClick = btnSetScaleClick
    end
    object chkLogScale: TCheckBox
      Left = 96
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Logariphmic'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object jvedtEmin: TJvValidateEdit
      Left = 40
      Top = 16
      Width = 41
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloat
      EditText = '0'
      HasMinValue = True
      TabOrder = 1
    end
    object jvedtEmax: TJvValidateEdit
      Left = 40
      Top = 40
      Width = 41
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfFloat
      HasMinValue = True
      TabOrder = 2
    end
    object jvedtPointsNum: TJvValidateEdit
      Left = 168
      Top = 16
      Width = 41
      Height = 21
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '2'
      HasMinValue = True
      MaxValue = 2.000000000000000000
      MinValue = 2.000000000000000000
      TabOrder = 3
    end
  end
  object jvedtThick: TJvValidateEdit
    Left = 136
    Top = 368
    Width = 73
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    DisplayFormat = dfFloat
    DecimalPlaces = 3
    TabOrder = 5
  end
  object mm1: TMainMenu
    Left = 416
    Top = 96
    object mniFile: TMenuItem
      Caption = 'File'
      object mniLoadGrid: TMenuItem
        Caption = 'Load grid'
        ShortCut = 16463
        OnClick = mniLoadGridClick
      end
      object mniSaveGrid: TMenuItem
        Caption = 'Save grid'
        OnClick = mniSaveGridClick
      end
      object mniSaveMju: TMenuItem
        Caption = 'Save mju'
        ShortCut = 16467
        OnClick = mniSaveMjuClick
      end
    end
    object mniAction: TMenuItem
      Caption = 'Action'
      object mniShowElInfo: TMenuItem
        Caption = 'Show element info'
        OnClick = mniShowElInfoClick
      end
    end
    object mniOptions: TMenuItem
      Caption = 'Options'
      object mniPreferences: TMenuItem
        Caption = 'Preferences'
        OnClick = mniPreferencesClick
      end
    end
    object mniAbout: TMenuItem
      Caption = 'About'
      OnClick = mniAboutClick
    end
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = 'enx'
    Filter = 'Energy grid (*.enx)|*.enx|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 416
    Top = 128
  end
  object dlgSave1: TSaveDialog
    DefaultExt = 'enx'
    Filter = 'Energy grid (*.enx)|*.enx|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 416
    Top = 160
  end
end
