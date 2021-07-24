object frmAddDetEff: TfrmAddDetEff
  Left = 688
  Top = 516
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Input experimental efficiency'
  ClientHeight = 137
  ClientWidth = 354
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
    Top = 24
    Width = 111
    Height = 13
    Caption = 'Experimental efficiency:'
  end
  object rg1: TRadioGroup
    Left = 24
    Top = 16
    Width = 97
    Height = 65
    Caption = 'Geometry'
    ItemIndex = 0
    Items.Strings = (
      'Geometry 1'
      'Geometry 2')
    TabOrder = 0
  end
  object jvedtExpEff: TJvValidateEdit
    Left = 256
    Top = 24
    Width = 81
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfFloatGeneral
    DecimalPlaces = 3
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 80
    Top = 96
    Width = 65
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 168
    Top = 96
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
