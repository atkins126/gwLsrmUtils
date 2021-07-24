object frmMergedNuclide: TfrmMergedNuclide
  Left = 748
  Top = 438
  BorderStyle = bsDialog
  Caption = 'Merged nuclide name'
  ClientHeight = 152
  ClientWidth = 201
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
    Left = 16
    Top = 8
    Width = 137
    Height = 13
    Caption = 'Select merged nuclide name:'
  end
  object btnOk: TButton
    Left = 16
    Top = 104
    Width = 57
    Height = 17
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 96
    Top = 104
    Width = 65
    Height = 17
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbbNucName: TComboBox
    Left = 16
    Top = 32
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object jvedtFactor: TJvValidateEdit
    Left = 120
    Top = 64
    Width = 41
    Height = 21
    Hint = 
      'All intensities will be multiplied by this factor and added to m' +
      'ain nuclide. Main nuclide intensitires will not change'
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    DisplayFormat = dfFloatGeneral
    DecimalPlaces = 3
    EditText = '1'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object chkFactor: TCheckBox
    Left = 16
    Top = 64
    Width = 89
    Height = 25
    Caption = 'Use multiplier'
    TabOrder = 4
    OnClick = chkFactorClick
  end
end
