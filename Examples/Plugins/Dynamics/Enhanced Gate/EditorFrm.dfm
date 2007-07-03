object EditorForm: TEditorForm
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 279
  ClientWidth = 321
  Color = 11327984
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 152
    Top = 257
    Width = 147
    Height = 16
    Margins.Bottom = 0
    Alignment = taCenter
    Caption = 'ENHANCED AUDIO GATE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 151
    Top = 256
    Width = 147
    Height = 16
    Margins.Bottom = 0
    Alignment = taCenter
    Caption = 'ENHANCED AUDIO GATE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object GBMain: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 131
    Caption = ' MAIN '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object LbThreshold: TLabel
      Left = 15
      Top = 88
      Width = 58
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'THRESHOLD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialThreshold: TDial
      Left = 16
      Top = 32
      Width = 56
      Height = 56
      Position = -0.001000000047497451
      Min = -80.000000000000000000
      Max = -0.001000000047497451
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialThresholdChange
    end
    object LbAttack: TLabel
      Left = 111
      Top = 88
      Width = 58
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'ATTACK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialAttack: TDial
      Left = 112
      Top = 32
      Width = 56
      Height = 56
      Min = -2.000000000000000000
      Max = 3.000000000000000000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialAttackChange
    end
    object LbHold: TLabel
      Left = 174
      Top = 88
      Width = 58
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'HOLD'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialHold: TDial
      Left = 175
      Top = 32
      Width = 56
      Height = 56
      Min = -2.000000000000000000
      Max = 0.397940009832382200
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialHoldChange
    end
    object LbDecay: TLabel
      Left = 237
      Top = 88
      Width = 58
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'DECAY'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialDecay: TDial
      Left = 238
      Top = 32
      Width = 56
      Height = 56
      Position = 3.698970079421997000
      Min = 0.698970019817352300
      Max = 3.698970079421997000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialDecayChange
    end
    object GaugeL: TGauge
      Left = 78
      Top = 32
      Width = 13
      Height = 88
      BackColor = clRed
      ForeColor = clLime
      Kind = gkVerticalBar
      Progress = 80
      ShowText = False
    end
    object Gauge1: TGauge
      Left = 93
      Top = 32
      Width = 13
      Height = 88
      BackColor = clRed
      ForeColor = clLime
      Kind = gkVerticalBar
      Progress = 80
      ShowText = False
    end
    object EdThreshold: TEdit
      Left = 22
      Top = 101
      Width = 44
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 0
      Text = '-42.0 dB'
    end
    object EdAttack: TEdit
      Left = 118
      Top = 101
      Width = 44
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 1
      Text = '2.00 ms'
    end
    object EdHold: TEdit
      Left = 181
      Top = 101
      Width = 44
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 2
      Text = '0.10 ms'
    end
    object EdDecay: TEdit
      Left = 244
      Top = 101
      Width = 44
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 3
      Text = '39.7 ms'
    end
    object CBOnOff: TCheckBox
      Left = 78
      Top = 13
      Width = 34
      Height = 17
      Caption = 'ON'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = CBOnOffClick
    end
    object CBDuck: TCheckBox
      Left = 136
      Top = 13
      Width = 49
      Height = 17
      Caption = 'DUCK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = CBDuckClick
    end
    object CBStereoLink: TCheckBox
      Left = 206
      Top = 13
      Width = 82
      Height = 17
      Caption = 'STEREOLINK'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = CBStereoLinkClick
    end
  end
  object GBSideChain: TGroupBox
    Left = 8
    Top = 145
    Width = 123
    Height = 127
    Caption = ' SIDECHAIN '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object LBLowCut: TLabel
      Left = 8
      Top = 68
      Width = 50
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'LO CUT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialLoCut: TDial
      Left = 8
      Top = 18
      Width = 48
      Height = 48
      Position = 1.301030039787292000
      Min = 1.301030039787292000
      Max = 3.602060079574585000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialLoCutChange
    end
    object LBHighCut: TLabel
      Left = 62
      Top = 68
      Width = 48
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'HI CUT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialHiCut: TDial
      Left = 62
      Top = 18
      Width = 48
      Height = 48
      Position = 4.301030158996582000
      Min = 2.301029920578003000
      Max = 4.301030158996582000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialHiCutChange
    end
    object LbSource: TLabel
      Left = 9
      Top = 106
      Width = 50
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'SOURCE:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EdLoCut: TEdit
      Left = 17
      Top = 81
      Width = 37
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 0
      Text = '200 Hz'
    end
    object EdHiCut: TEdit
      Left = 67
      Top = 81
      Width = 40
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 1
      Text = '20 kHz'
    end
    object CBSideChain: TComboBox
      Left = 65
      Top = 103
      Width = 46
      Height = 19
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 11
      ItemIndex = 0
      ParentFont = False
      TabOrder = 2
      Text = 'INT'
      OnChange = CBSideChainChange
      Items.Strings = (
        'INT'
        'EXT')
    end
  end
  object GBDynamics: TGroupBox
    Left = 137
    Top = 145
    Width = 176
    Height = 108
    Caption = ' DYNAMICS '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object LbRatio: TLabel
      Left = 9
      Top = 68
      Width = 50
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'RATIO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialRatio: TDial
      Left = 9
      Top = 18
      Width = 48
      Height = 48
      Position = 3.970000028610230000
      Min = 1.000000000000000000
      Max = 10.000000000000000000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialRatioChange
    end
    object LbKnee: TLabel
      Left = 63
      Top = 68
      Width = 48
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'KNEE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialKnee: TDial
      Left = 63
      Top = 18
      Width = 48
      Height = 48
      Position = 3.000000000000000000
      Min = 1.000000000000000000
      Max = 6.000000000000000000
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialKneeChange
    end
    object LbRange: TLabel
      Left = 117
      Top = 68
      Width = 48
      Height = 13
      Margins.Bottom = 0
      Alignment = taCenter
      AutoSize = False
      Caption = 'RANGE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialRange: TDial
      Left = 117
      Top = 18
      Width = 48
      Height = 48
      Position = -40.000000000000000000
      Min = -80.000000000000000000
      Max = -0.009999999776482582
      ColorCircle = 6057334
      ColorLine = clBlack
      StitchKind = skHorizontal
      PointerAngles.Start = 210
      PointerAngles.Range = 300
      PointerAngles.Resolution = 300.000000000000000000
      OnChange = DialRangeChange
    end
    object EdRatio: TEdit
      Left = 18
      Top = 81
      Width = 37
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 0
      Text = '3.97'
    end
    object EdKnee: TEdit
      Left = 68
      Top = 81
      Width = 40
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 1
      Text = '3.00 dB'
    end
    object EdRange: TEdit
      Left = 122
      Top = 81
      Width = 40
      Height = 19
      BiDiMode = bdRightToLeft
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 2
      Text = '-40.0 dB'
    end
  end
end
