object frmMain: TfrmMain
  Left = 221
  Top = 151
  Width = 489
  Height = 453
  Caption = 'Scenario generator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    473
    414)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 248
    Width = 63
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Template file:'
  end
  object lbl2: TLabel
    Left = 8
    Top = 288
    Width = 65
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Spe directory:'
  end
  object lbl3: TLabel
    Left = 8
    Top = 328
    Width = 64
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Mask for spe:'
  end
  object mmoTemplate: TMemo
    Left = 8
    Top = 8
    Width = 457
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object jvdiredtSpeDir: TJvDirectoryEdit
    Left = 8
    Top = 304
    Width = 457
    Height = 21
    Hint = 'Spectrum will be added recurcively'
    TextHint = 'Set spectrum directory'
    DialogKind = dkWin32
    Anchors = [akLeft, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = jvdiredtSpeDirChange
  end
  object btnGenerate: TButton
    Left = 8
    Top = 376
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Generate'
    TabOrder = 3
    OnClick = btnGenerateClick
  end
  object btnSave: TButton
    Left = 104
    Top = 376
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Enabled = False
    TabOrder = 4
    OnClick = btnSaveClick
  end
  object jvfnedtTemplate: TJvFilenameEdit
    Left = 8
    Top = 264
    Width = 457
    Height = 21
    Hint = 
      'Set template-scenario|Set template-scenario. Template scenario c' +
      'an contain variables:'#13#10'[SPE_PATH]'
    TextHint = 'Set template scenario'
    Anchors = [akLeft, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = jvfnedtTemplateChange
  end
  object edtMask: TEdit
    Left = 8
    Top = 344
    Width = 113
    Height = 21
    Hint = 'Mask can contains '#39'*'#39' and '#39'!'#39' symbols'
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '*.spe'
  end
  object btnClear: TButton
    Left = 200
    Top = 376
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 6
    OnClick = btnClearClick
  end
  object statInfo: TStatusBar
    Left = 0
    Top = 397
    Width = 473
    Height = 17
    Panels = <
      item
        Width = 50
      end>
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'lsc'
    Filter = 'Scenario files (*.lsc)|*.lsc|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 432
    Top = 208
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'lsc'
    Filter = 'Scenario files (*.lsc)|*.lsc|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 400
    Top = 208
  end
end
