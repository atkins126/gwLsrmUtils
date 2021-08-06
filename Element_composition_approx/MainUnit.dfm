object frmMain: TfrmMain
  Left = 430
  Top = 313
  Width = 375
  Height = 212
  Caption = 'Element composition for OISN'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 16
    Width = 101
    Height = 13
    Caption = 'Rho for OISN, g/cm3'
  end
  object jvspnedtRho: TJvSpinEdit
    Left = 128
    Top = 16
    Width = 49
    Height = 21
    Increment = 0.100000000000000000
    MaxValue = 2.500000000000000000
    MinValue = 0.600000000000000000
    ValueType = vtFloat
    Value = 1.000000000000000000
    TabOrder = 0
  end
  object mmoResult: TMemo
    Left = 8
    Top = 88
    Width = 321
    Height = 73
    TabOrder = 1
  end
  object btnCalc: TButton
    Left = 128
    Top = 48
    Width = 81
    Height = 25
    Caption = 'Calc'
    TabOrder = 2
    OnClick = btnCalcClick
  end
  object chkCorrectToNearest: TCheckBox
    Left = 192
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Correct to table values'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
