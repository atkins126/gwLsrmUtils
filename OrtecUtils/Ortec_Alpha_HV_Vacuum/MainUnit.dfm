object frmMain: TfrmMain
  Left = 330
  Top = 197
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'AlphaDUO start manager'
  ClientHeight = 438
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object stat1: TStatusBar
    Left = 0
    Top = 417
    Width = 326
    Height = 21
    Panels = <
      item
        Width = 50
      end>
  end
  object pgc1: TPageControl
    Left = 0
    Top = 0
    Width = 326
    Height = 265
    ActivePage = tsDet1
    Align = alTop
    HotTrack = True
    Style = tsFlatButtons
    TabOrder = 1
    OnChange = pgc1Change
    object tsDet1: TTabSheet
      Caption = 'Tract1'
      inline frmc1: TfrmMCA
        Left = 0
        Top = 0
        Width = 315
        Height = 233
        TabOrder = 0
        inherited pnl1: TPanel
          inherited btnOff: TButton
            OnClick = frmc1btnOffClick
          end
          inherited btnOn: TButton
            OnClick = frmc1btnOnClick
          end
        end
      end
    end
    object tsDet2: TTabSheet
      Caption = 'Tract2'
      ImageIndex = 1
      inline frmc2: TfrmMCA
        Left = 0
        Top = 0
        Width = 315
        Height = 233
        TabOrder = 0
        inherited pnl1: TPanel
          inherited btnOff: TButton
            OnClick = frmc2btnOffClick
          end
          inherited btnOn: TButton
            OnClick = frmc2btnOnClick
          end
        end
      end
    end
  end
  object pnlPump: TPanel
    Left = 0
    Top = 274
    Width = 326
    Height = 143
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lbl9: TLabel
      Left = 24
      Top = 32
      Width = 75
      Height = 13
      Caption = 'Target vacuum:'
    end
    object lbl10: TLabel
      Left = 24
      Top = 56
      Width = 74
      Height = 13
      Caption = 'Actual vacuum:'
    end
    object lbl11: TLabel
      Left = 200
      Top = 32
      Width = 27
      Height = 13
      Caption = 'mTorr'
    end
    object lbl12: TLabel
      Left = 200
      Top = 56
      Width = 27
      Height = 13
      Caption = 'mTorr'
    end
    object lbl13: TLabel
      Left = 24
      Top = 96
      Width = 68
      Height = 13
      Caption = 'Vacuum state:'
    end
    object chkVacMon: TCheckBox
      Left = 24
      Top = 0
      Width = 129
      Height = 25
      Caption = 'Vacuum monitoring'
      TabOrder = 0
      Visible = False
    end
    object edtVacuumTarget: TEdit
      Left = 112
      Top = 32
      Width = 81
      Height = 21
      TabOrder = 1
    end
    object edtVacuum: TEdit
      Left = 112
      Top = 56
      Width = 81
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object cbbPump: TComboBox
      Left = 112
      Top = 96
      Width = 81
      Height = 21
      Hint = 
        'Vent -- open valve to atmosphere'#13#10'Pump -- pump to minimum possib' +
        'le pressure'#13#10'Hold -- pump to target pressure'
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbbPumpChange
    end
    object btnSet: TButton
      Left = 240
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Set'
      TabOrder = 4
      OnClick = btnSetClick
    end
  end
  object tmr1: TTimer
    Enabled = False
    OnTimer = tmr1Timer
    Left = 268
    Top = 176
  end
end
