object frmPreferences: TfrmPreferences
  Left = 649
  Top = 367
  Width = 319
  Height = 189
  Caption = 'Preferences'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 136
    Top = 16
    Width = 41
    Height = 13
    Caption = 'Mu type:'
  end
  object chkOutput: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Clear output'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object rgEnUnits: TRadioGroup
    Left = 8
    Top = 40
    Width = 97
    Height = 65
    Caption = 'Energy units'
    ItemIndex = 0
    Items.Strings = (
      'MeV'
      'keV')
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 56
    Top = 120
    Width = 73
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 176
    Top = 120
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbbMuType: TComboBox
    Left = 136
    Top = 40
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Total without coherent. scat'
    Items.Strings = (
      'Total without coherent. scat'
      'Scattering coherent'
      'Scat. Incoh'
      'Photoelectric absorption'
      'Pair production in nuclear field'
      'Pair production in electron field'
      'Total with coherent scat')
  end
end
