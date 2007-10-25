object VSTGUI: TVSTGUI
  Left = 337
  Top = 256
  BorderStyle = bsNone
  Caption = 'XSynth'
  ClientHeight = 189
  ClientWidth = 498
  Color = clBtnFace
  TransparentColorValue = 12948623
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object MidiKeys: TGuiMidiKeys
    Left = 0
    Top = 116
    Width = 498
    Height = 73
    BaseOctave = 3
    NumOctaves = 4
    Align = alBottom
    Color = clWhite
    OnMidiKeyDown = MidiKeysMidiKeyDown
    OnMidiKeyUp = MidiKeysMidiKeyUp
  end
  object GBOSC1: TGroupBox
    Left = 8
    Top = 8
    Width = 145
    Height = 102
    Caption = ' Oscilator 1 '
    TabOrder = 0
    object LbOsc1Type: TLabel
      Left = 9
      Top = 19
      Width = 33
      Height = 13
      Caption = 'Type:'
    end
    object Osc1ADSR: TGuiADSRGraph
      Left = 50
      Top = 43
      Width = 89
      Height = 29
      LineWidth = 2
      LineColor = clBlack
      ADSRSettings.Attack = 0.500000000000000000
      ADSRSettings.Decay = 0.500000000000000000
      ADSRSettings.Sustain = 0.500000000000000000
      ADSRSettings.Release = 0.500000000000000000
      OnAttackChange = Osc1ADSRAttackChange
      OnDecayChange = Osc1ADSRDecayChange
      OnSustainChange = Osc1ADSRSustainChange
      OnReleaseChange = Osc1ADSRReleaseChange
      EnvVPadding = 3
      EnvHPadding = 10
    end
    object Label1: TLabel
      Left = 9
      Top = 53
      Width = 38
      Height = 13
      Caption = 'ADSR:'
    end
    object Label5: TLabel
      Left = 9
      Top = 81
      Width = 35
      Height = 13
      Caption = 'Level:'
    end
    object CBOsc1Type: TComboBox
      Left = 50
      Top = 16
      Width = 89
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'sine'
      OnChange = CBOsc1TypeChange
      Items.Strings = (
        'none'
        'sine'
        'halfsine'
        'square'
        'noise')
    end
    object Osc1Level: TScrollBar
      Left = 50
      Top = 81
      Width = 89
      Height = 13
      PageSize = 0
      Position = 100
      TabOrder = 1
      OnChange = Osc1LevelChange
    end
  end
  object GBOsc2: TGroupBox
    Left = 159
    Top = 8
    Width = 145
    Height = 102
    Caption = ' Oscilator 2 '
    TabOrder = 1
    object LbOsc2Type: TLabel
      Left = 9
      Top = 19
      Width = 33
      Height = 13
      Caption = 'Type:'
    end
    object Osc2ADSR: TGuiADSRGraph
      Left = 53
      Top = 43
      Width = 86
      Height = 29
      LineWidth = 2
      LineColor = clBlack
      ADSRSettings.Attack = 0.500000000000000000
      ADSRSettings.Decay = 0.500000000000000000
      ADSRSettings.Sustain = 0.500000000000000000
      ADSRSettings.Release = 0.500000000000000000
      OnAttackChange = Osc2ADSRAttackChange
      OnDecayChange = Osc2ADSRDecayChange
      OnSustainChange = Osc2ADSRSustainChange
      OnReleaseChange = Osc2ADSRReleaseChange
    end
    object Label2: TLabel
      Left = 9
      Top = 53
      Width = 38
      Height = 13
      Caption = 'ADSR:'
    end
    object Label6: TLabel
      Left = 9
      Top = 81
      Width = 35
      Height = 13
      Caption = 'Level:'
    end
    object CBOsc2Type: TComboBox
      Left = 50
      Top = 16
      Width = 89
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'none'
      OnChange = CBOsc2TypeChange
      Items.Strings = (
        'none'
        'sine'
        'halfsine'
        'square'
        'noise')
    end
    object Osc2Level: TScrollBar
      Left = 50
      Top = 81
      Width = 89
      Height = 13
      PageSize = 0
      Position = 100
      TabOrder = 1
      OnChange = Osc2LevelChange
    end
  end
  object GBOutput: TGroupBox
    Left = 310
    Top = 8
    Width = 185
    Height = 102
    Caption = ' Output '
    TabOrder = 2
    object LbLevel: TLabel
      Left = 16
      Top = 81
      Width = 35
      Height = 13
      Caption = 'Level:'
    end
    object LbDrive: TLabel
      Left = 16
      Top = 19
      Width = 36
      Height = 13
      Caption = 'Drive:'
    end
    object Label3: TLabel
      Left = 16
      Top = 38
      Width = 40
      Height = 13
      Caption = 'Cutoff:'
    end
    object Label4: TLabel
      Left = 16
      Top = 57
      Width = 30
      Height = 13
      Caption = 'Res.:'
    end
    object SBLevel: TScrollBar
      Left = 61
      Top = 80
      Width = 108
      Height = 15
      PageSize = 0
      Position = 100
      TabOrder = 0
      OnChange = SBLevelChange
    end
    object SBDrive: TScrollBar
      Left = 61
      Top = 19
      Width = 108
      Height = 13
      Min = 10
      PageSize = 0
      Position = 10
      TabOrder = 1
      OnChange = SBDriveChange
    end
    object SBCutoff: TScrollBar
      Left = 61
      Top = 38
      Width = 108
      Height = 13
      PageSize = 0
      Position = 100
      TabOrder = 2
      OnChange = SBCutoffChange
    end
    object SBResonance: TScrollBar
      Left = 61
      Top = 57
      Width = 108
      Height = 13
      Min = 10
      PageSize = 0
      Position = 10
      TabOrder = 3
      OnChange = SBResonanceChange
    end
  end
end
