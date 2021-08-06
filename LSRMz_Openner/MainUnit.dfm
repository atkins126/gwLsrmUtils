object frmMain: TfrmMain
  Left = 645
  Top = 378
  Width = 426
  Height = 270
  Caption = 'Extracts from LSRMz'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnOpen: TButton
    Left = 16
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object chkUseRelativePath: TCheckBox
    Left = 16
    Top = 48
    Width = 201
    Height = 25
    Caption = 'Use relative path'
    TabOrder = 1
  end
  object stat1: TStatusBar
    Left = 0
    Top = 212
    Width = 410
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object dlgOpen1: TOpenDialog
    DefaultExt = 'LSRMz'
    Filter = 'LSRM zip from mail (*.LSRMz)|*.LSRMz|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 360
    Top = 16
  end
  object jvzlbmltpl1: TJvZlibMultiple
    OnDecompressedFile = jvzlbmltpl1DecompressedFile
    Left = 392
    Top = 16
  end
  object jvslctdrctry1: TJvSelectDirectory
    InitialDir = '.'
    Left = 392
    Top = 48
  end
  object jvbrwflddlg1: TJvBrowseForFolderDialog
    Left = 392
    Top = 88
  end
end
