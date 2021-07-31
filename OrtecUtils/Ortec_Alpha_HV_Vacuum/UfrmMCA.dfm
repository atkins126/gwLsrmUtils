object frmMCA: TfrmMCA
  Left = 0
  Top = 0
  Width = 315
  Height = 233
  TabOrder = 0
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 315
    Height = 233
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbl7: TLabel
      Left = 135
      Top = 124
      Width = 34
      Height = 13
      Caption = 'Current'
    end
    object lbl1: TLabel
      Left = 8
      Top = 8
      Width = 37
      Height = 13
      Caption = 'Device:'
    end
    object lbl10: TLabel
      Left = 16
      Top = 32
      Width = 25
      Height = 13
      Caption = 'Alias:'
    end
    object lbl2: TLabel
      Left = 16
      Top = 160
      Width = 41
      Height = 13
      Caption = 'HV state'
    end
    object lbl3: TLabel
      Left = 136
      Top = 64
      Width = 34
      Height = 13
      Caption = 'Target:'
    end
    object lbl4: TLabel
      Left = 136
      Top = 95
      Width = 33
      Height = 13
      Caption = 'Actual:'
    end
    object lbl5: TLabel
      Left = 240
      Top = 64
      Width = 7
      Height = 13
      Caption = 'V'
    end
    object lbl6: TLabel
      Left = 240
      Top = 95
      Width = 7
      Height = 13
      Caption = 'V'
    end
    object lbl8: TLabel
      Left = 240
      Top = 126
      Width = 13
      Height = 13
      Caption = 'nA'
    end
    object lbl9: TLabel
      Left = 136
      Top = 160
      Width = 69
      Height = 13
      Caption = 'Overload state'
    end
    object pnlOn: TPanel
      Left = 16
      Top = 176
      Width = 89
      Height = 57
      Hint = 'High voltage status'
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -32
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object pnlOver: TPanel
      Left = 136
      Top = 176
      Width = 113
      Height = 57
      Hint = 'Overload status'
      BevelOuter = bvLowered
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object btnOff: TButton
      Left = 16
      Top = 96
      Width = 65
      Height = 25
      Hint = 'Disable high voltage for Tract'
      Caption = 'HV Off'
      TabOrder = 2
    end
    object btnOn: TButton
      Left = 16
      Top = 64
      Width = 65
      Height = 25
      Hint = 'Enable high voltage for Tract'
      Caption = 'HV On'
      TabOrder = 3
    end
    object btnRenew: TButton
      Left = 240
      Top = 8
      Width = 65
      Height = 17
      Hint = 'Reconnect to device and read it'#39's parameters'
      Caption = 'Renew'
      TabOrder = 4
    end
    object cbbDevice: TComboBox
      Left = 48
      Top = 8
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
    object edtAlias: TEdit
      Left = 48
      Top = 32
      Width = 177
      Height = 21
      TabOrder = 6
    end
    object edtCurrent: TEdit
      Left = 176
      Top = 122
      Width = 57
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 7
    end
    object edtHV: TEdit
      Left = 176
      Top = 93
      Width = 57
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 8
    end
    object edtHVTarget: TEdit
      Left = 176
      Top = 64
      Width = 57
      Height = 21
      ReadOnly = True
      TabOrder = 9
    end
  end
end
