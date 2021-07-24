object frmConvert: TfrmConvert
  Left = 529
  Top = 226
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Activity converter'
  ClientHeight = 158
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 40
    Top = 24
    Width = 37
    Height = 13
    Caption = 'Activity:'
  end
  object lbl2: TLabel
    Left = 40
    Top = 112
    Width = 37
    Height = 13
    Caption = 'Activity:'
  end
  object lbl3: TLabel
    Left = 208
    Top = 72
    Width = 39
    Height = 13
    Caption = 'Nuclide:'
  end
  object jvedtAct1: TJvValidateEdit
    Left = 88
    Top = 24
    Width = 153
    Height = 21
    Alignment = taLeftJustify
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    DisplayFormat = dfCustom
    TabOrder = 0
  end
  object jvedtAct2: TJvValidateEdit
    Left = 88
    Top = 104
    Width = 153
    Height = 21
    Alignment = taLeftJustify
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    DisplayFormat = dfCustom
    TabOrder = 1
  end
  object btnConvert: TButton
    Left = 88
    Top = 64
    Width = 81
    Height = 25
    Caption = 'Convert \/'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object cbbUnits1: TComboBox
    Left = 256
    Top = 24
    Width = 57
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 3
    Text = 'Ci'
    Items.Strings = (
      'Bq'
      'kBq'
      'MBq'
      'Ci'
      'mCi'
      'uCi'
      'g')
  end
  object cbbUnits2: TComboBox
    Left = 256
    Top = 104
    Width = 57
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Bq'
    Items.Strings = (
      'Bq'
      'kBq'
      'MBq'
      'Ci'
      'mCi'
      'uCi'
      'g')
  end
  object cbbNuclides: TComboBox
    Left = 256
    Top = 64
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
  end
end
