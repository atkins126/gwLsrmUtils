object frmMain: TfrmMain
  Left = 555
  Top = 317
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Ortec HV On'
  ClientHeight = 246
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pm1
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 24
    Width = 37
    Height = 13
    Caption = 'Device:'
  end
  object lbl2: TLabel
    Left = 88
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Target:'
  end
  object lbl3: TLabel
    Left = 88
    Top = 96
    Width = 33
    Height = 13
    Caption = 'Actual:'
  end
  object lbl4: TLabel
    Left = 184
    Top = 64
    Width = 7
    Height = 13
    Caption = 'V'
  end
  object lbl5: TLabel
    Left = 184
    Top = 96
    Width = 7
    Height = 13
    Caption = 'V'
  end
  object lblPolarity: TLabel
    Left = 16
    Top = 128
    Width = 3
    Height = 13
  end
  object lbl6: TLabel
    Left = 8
    Top = 152
    Width = 41
    Height = 13
    Caption = 'HV state'
  end
  object lbl7: TLabel
    Left = 128
    Top = 152
    Width = 69
    Height = 13
    Caption = 'Overload state'
  end
  object cbbDevice: TComboBox
    Left = 56
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbbDeviceChange
  end
  object btnOn: TButton
    Left = 8
    Top = 64
    Width = 49
    Height = 17
    Caption = 'HV On'
    TabOrder = 1
    OnClick = btnOnClick
  end
  object btnOff: TButton
    Left = 8
    Top = 96
    Width = 49
    Height = 17
    Caption = 'HV Off'
    TabOrder = 2
    OnClick = btnOffClick
  end
  object edtTarget: TEdit
    Left = 136
    Top = 64
    Width = 41
    Height = 21
    ReadOnly = True
    TabOrder = 3
  end
  object edtActual: TEdit
    Left = 136
    Top = 96
    Width = 41
    Height = 21
    Color = clInactiveCaptionText
    ReadOnly = True
    TabOrder = 4
  end
  object grp1: TGroupBox
    Left = 216
    Top = 64
    Width = 145
    Height = 57
    Caption = 'Shutdown method'
    TabOrder = 5
    object cbbShutdown: TComboBox
      Left = 8
      Top = 24
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object pnlON: TPanel
    Left = 8
    Top = 168
    Width = 105
    Height = 57
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object pnlOverload: TPanel
    Left = 128
    Top = 168
    Width = 129
    Height = 57
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object btnRenew: TButton
    Left = 224
    Top = 24
    Width = 81
    Height = 17
    Caption = 'Renew'
    TabOrder = 8
    OnClick = btnRenewClick
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmr1Timer
    Left = 328
    Top = 24
  end
  object pm1: TPopupMenu
    Left = 312
    Top = 128
    object mniAbout_pm: TMenuItem
      Caption = 'About'
      OnClick = mniAbout_pmClick
    end
  end
end
