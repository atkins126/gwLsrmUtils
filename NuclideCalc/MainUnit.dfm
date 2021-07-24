object MainForm: TMainForm
  Left = 441
  Top = 291
  Width = 381
  Height = 258
  Caption = #1055#1088#1086#1075#1088#1072#1084#1084#1072' '#1076#1083#1103' '#1087#1077#1088#1077#1089#1095#1105#1090#1072' '#1072#1082#1090#1080#1074#1085#1086#1089#1090#1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mm1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 9
    Width = 142
    Height = 13
    Caption = #1042#1074#1077#1076#1080#1090#1077' '#1089#1090#1072#1088#1091#1102' '#1072#1082#1090#1080#1074#1085#1086#1089#1090#1100
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 40
    Height = 13
    Caption = #1085#1072' '#1076#1072#1090#1091':'
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 95
    Height = 13
    Caption = #1076#1083#1103' '#1088#1072#1076#1080#1086#1085#1091#1082#1083#1080#1076#1072':'
  end
  object Label4: TLabel
    Left = 8
    Top = 92
    Width = 105
    Height = 13
    Caption = #1055#1077#1088#1077#1089#1095#1080#1090#1072#1090#1100' '#1085#1072' '#1076#1072#1090#1091
  end
  object Resultlbl: TLabel
    Left = 8
    Top = 184
    Width = 3
    Height = 13
  end
  object TdiffLbl: TLabel
    Left = 8
    Top = 120
    Width = 3
    Height = 13
  end
  object ThalfLbl: TLabel
    Left = 248
    Top = 63
    Width = 3
    Height = 13
    ParentShowHint = False
    ShowHint = True
  end
  object OldActEdt: TEdit
    Left = 248
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 6
    Text = '100'
    Visible = False
    OnKeyPress = OldActEdtKeyPress
  end
  object btnCalc: TButton
    Left = 80
    Top = 144
    Width = 81
    Height = 25
    Caption = #1055#1077#1088#1077#1089#1095#1080#1090#1072#1090#1100
    TabOrder = 4
    OnClick = btnCalcClick
  end
  object NuclCmbBx: TComboBox
    Left = 160
    Top = 61
    Width = 65
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'Cs-137'
    OnSelect = NuclCmbBxSelect
  end
  object btnBuffCopy: TButton
    Left = 176
    Top = 144
    Width = 113
    Height = 25
    Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088
    TabOrder = 5
    OnClick = btnBuffCopyClick
  end
  object dtpOldDate: TDateTimePicker
    Left = 160
    Top = 32
    Width = 81
    Height = 21
    Date = 42010.000000000000000000
    Time = 42010.000000000000000000
    TabOrder = 1
  end
  object dtpNewDate: TDateTimePicker
    Left = 160
    Top = 88
    Width = 81
    Height = 21
    Date = 42010.000000000000000000
    Time = 42010.000000000000000000
    TabOrder = 3
  end
  object jvedtOldAct: TJvValidateEdit
    Left = 160
    Top = 8
    Width = 73
    Height = 21
    Alignment = taLeftJustify
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfCustom
    EditText = '100'
    TabOrder = 0
    OnCustomValidate = jvedtOldActCustomValidate
  end
  object dtpOldTime: TDateTimePicker
    Left = 248
    Top = 32
    Width = 73
    Height = 21
    Date = 43070.774205879630000000
    Time = 43070.774205879630000000
    Kind = dtkTime
    TabOrder = 7
  end
  object dtpNewTime: TDateTimePicker
    Left = 248
    Top = 88
    Width = 73
    Height = 21
    Date = 43070.775795532410000000
    Time = 43070.775795532410000000
    Kind = dtkTime
    TabOrder = 8
  end
  object mm1: TMainMenu
    Left = 328
    object mniFile: TMenuItem
      Caption = 'File'
      object mniLoadLibrary: TMenuItem
        Caption = 'Open library'
        ShortCut = 16463
        OnClick = mniLoadLibraryClick
      end
      object mniReloadLibrary: TMenuItem
        Caption = 'Reload library'
        ShortCut = 16466
        OnClick = mniReloadLibraryClick
      end
    end
    object mniEdit: TMenuItem
      Caption = 'Edit'
      object mniCopy: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = mniCopyClick
      end
      object mniPaste: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
        OnClick = mniPasteClick
      end
      object mniEditNuclide: TMenuItem
        Caption = 'Edit nuclide'
        Visible = False
      end
      object mniAddNuclide: TMenuItem
        Caption = 'Add nuclide'
        Visible = False
      end
    end
    object mniTools: TMenuItem
      Caption = 'Tools'
      object mniConvertActivity: TMenuItem
        Caption = 'Convert activity'
        OnClick = mniConvertActivityClick
      end
    end
    object mniAbout: TMenuItem
      Caption = 'About'
      OnClick = mniAboutClick
    end
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = 'lib'
    Filter = 
      'NuclideCalc lib-files(*.lib)|*.lib|NuclideCalc tlb-files (*.tlb)' +
      '|*.tlb|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 328
    Top = 32
  end
end
