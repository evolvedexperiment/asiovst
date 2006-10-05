object FmVSTDebug: TFmVSTDebug
  Left = 557
  Top = 406
  Width = 245
  Height = 189
  Caption = 'VST Debug'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bt_ShowEditor: TButton
    Left = 168
    Top = 12
    Width = 66
    Height = 22
    Caption = 'Show Editor'
    TabOrder = 0
    OnClick = Bt_ShowEditorClick
  end
  object Bt_Process: TButton
    Left = 168
    Top = 35
    Width = 66
    Height = 22
    Caption = 'Process'
    TabOrder = 1
    OnClick = Bt_ProcessClick
  end
  object GB_Performance: TGroupBox
    Left = 8
    Top = 88
    Width = 225
    Height = 70
    Caption = ' Performance Test '
    TabOrder = 2
    object Lb_Blocks: TLabel
      Left = 9
      Top = 21
      Width = 87
      Height = 13
      Caption = 'Blocks to process:'
    end
    object SE_Blocks: TSpinEdit
      Left = 104
      Top = 18
      Width = 50
      Height = 22
      MaxValue = 9999
      MinValue = 0
      TabOrder = 0
      Value = 100
      OnChange = SE_BlockSizeChange
    end
    object Bt_Go: TButton
      Left = 162
      Top = 18
      Width = 28
      Height = 21
      Caption = 'GO!'
      TabOrder = 1
      OnClick = Bt_GoClick
    end
    object Ed_Results: TEdit
      Left = 9
      Top = 42
      Width = 207
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object CB_M: TCheckBox
      Left = 197
      Top = 19
      Width = 14
      Height = 17
      TabOrder = 3
      OnClick = CB_MClick
    end
  end
  object GB_Host: TGroupBox
    Left = 8
    Top = 8
    Width = 153
    Height = 73
    Caption = ' Host Options '
    TabOrder = 3
    object Label1: TLabel
      Left = 9
      Top = 19
      Width = 47
      Height = 13
      Caption = 'BlockSize'
    end
    object Label2: TLabel
      Left = 9
      Top = 45
      Width = 64
      Height = 13
      Caption = 'Sample Rate:'
    end
    object SE_BlockSize: TSpinEdit
      Left = 80
      Top = 15
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SE_BlockSizeChange
    end
    object SE_SampleRate: TSpinEdit
      Left = 80
      Top = 41
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SE_SampleRateChange
    end
  end
  object Rb_Random: TRadioButton
    Left = 169
    Top = 57
    Width = 64
    Height = 17
    Caption = 'Random'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object Rb_Impulse: TRadioButton
    Left = 169
    Top = 72
    Width = 64
    Height = 17
    Caption = 'Impulse'
    TabOrder = 5
  end
  object VSTHost: TVSTHost
    VSTPlugIns = <
      item
        DisplayName = 'Test'
        numInputs = 0
        numOutputs = 0
        numPrograms = 0
        numParams = 0
        Version = 0
        InitialDelay = 0
        EffectOptions = []
        PlugCategory = kpcUnknown
        OnAudioMasterIdle = VSTHostVSTPlugIns0AudioMasterIdle
        OnAudioMasterNeedIdle = VSTHostVSTPlugIns0AudioMasterNeedIdle
        OnAudioMasterUpdateDisplay = VSTHostVSTPlugIns0AudioMasterUpdateDisplay
      end>
    Language = kvlEnglish
    ParameterQuantization = 0
    BlockSize = 2048
    CanDos = []
    ManageIdleAutomaticly = False
    Tempo = 120.000000000000000000
    VSTVersion = 2300
    VendorVersion = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    Left = 48
    Top = 32
  end
end
