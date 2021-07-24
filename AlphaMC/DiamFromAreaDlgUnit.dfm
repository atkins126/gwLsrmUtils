object frmDiamFromAreaDlg: TfrmDiamFromAreaDlg
  Left = 703
  Top = 540
  BorderStyle = bsDialog
  Caption = 'Calc diameter from area'
  ClientHeight = 214
  ClientWidth = 313
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 161
    Shape = bsFrame
  end
  object lbl1: TLabel
    Left = 16
    Top = 24
    Width = 87
    Height = 13
    Caption = 'Input area in mm2:'
  end
  object OKBtn: TButton
    Left = 79
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 159
    Top = 180
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object jvedtArea: TJvValidateEdit
    Left = 120
    Top = 24
    Width = 73
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    TrimDecimals = True
    DisplayFormat = dfFloatGeneral
    DecimalPlaces = 3
    TabOrder = 2
  end
  object rg1: TRadioGroup
    Left = 16
    Top = 56
    Width = 241
    Height = 97
    Caption = 'Check detector or source'
    ItemIndex = 0
    Items.Strings = (
      'Detector'
      'Source 1'
      'Source 2')
    TabOrder = 3
  end
end
