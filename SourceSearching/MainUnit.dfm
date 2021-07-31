object frmMain: TfrmMain
  Left = 361
  Top = 204
  Width = 774
  Height = 751
  Caption = 'Source seach'
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
  object pbSource: TPaintBox
    Left = 8
    Top = 296
    Width = 465
    Height = 393
    OnMouseDown = pbSourceMouseDown
    OnPaint = pbSourcePaint
  end
  object btnTestCalc: TButton
    Left = 608
    Top = 80
    Width = 105
    Height = 17
    Caption = 'TestCalc'
    TabOrder = 0
    OnClick = btnTestCalcClick
  end
  object mmoOutput: TMemo
    Left = 496
    Top = 296
    Width = 241
    Height = 393
    TabOrder = 2
  end
  object stat1: TStatusBar
    Left = 0
    Top = 690
    Width = 758
    Height = 22
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object btnCalc: TButton
    Left = 496
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Calc'
    TabOrder = 1
    OnClick = btnCalcClick
  end
  object strngrdDet: TStringGrid
    Left = 8
    Top = 8
    Width = 345
    Height = 273
    DefaultColWidth = 50
    RowCount = 11
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing]
    TabOrder = 4
  end
end
