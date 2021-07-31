object frmMain: TfrmMain
  Left = 558
  Top = 285
  Width = 373
  Height = 288
  Caption = 'High load coefficients calculator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 88
    Top = 184
    Width = 3
    Height = 13
  end
  object strngrd1: TStringGrid
    Left = 8
    Top = 8
    Width = 313
    Height = 153
    ColCount = 4
    DefaultColWidth = 68
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
    ScrollBars = ssVertical
    TabOrder = 0
    OnSelectCell = strngrd1SelectCell
  end
  object btnCalc: TButton
    Left = 16
    Top = 184
    Width = 57
    Height = 17
    Caption = 'Calc'
    TabOrder = 1
    OnClick = btnCalcClick
  end
  object btnShowSLAU: TButton
    Left = 16
    Top = 216
    Width = 89
    Height = 17
    Caption = 'Show equations'
    TabOrder = 2
    OnClick = btnShowSLAUClick
  end
end
