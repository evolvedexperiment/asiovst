object FmVSTEditor: TFmVSTEditor
  Left = 325
  Top = 594
  BorderStyle = bsToolWindow
  Caption = 'ITA VST Plugin Editor'
  ClientHeight = 228
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 16
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 329
    Height = 27
    ButtonHeight = 24
    Color = clBtnFace
    EdgeInner = esNone
    EdgeOuter = esNone
    ParentColor = False
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 4
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object LbPreset: TLabel
      Left = 4
      Top = 0
      Width = 53
      Height = 24
      AutoSize = False
      Caption = 'Preset: '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object ToolButton2: TToolButton
      Left = 57
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object CBPreset: TComboBox
      Left = 65
      Top = 0
      Width = 144
      Height = 24
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBtnFace
      ItemHeight = 16
      TabOrder = 0
      OnChange = CBPresetChange
    end
    object ToolButton3: TToolButton
      Left = 209
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BtSetup: TButton
      Left = 217
      Top = 0
      Width = 51
      Height = 24
      Caption = '&Setup'
      TabOrder = 2
      TabStop = False
      OnClick = BtSetupClick
    end
    object ToolButton4: TToolButton
      Left = 268
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object BtExit: TButton
      Left = 276
      Top = 0
      Width = 37
      Height = 24
      Caption = 'E&xit'
      TabOrder = 1
      TabStop = False
      OnClick = BtExitClick
    end
  end
  object VSTPanel: TPanel
    Left = 0
    Top = 27
    Width = 329
    Height = 201
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6695441
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object VstHost: TVstHost
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
      end>
    ParameterQuantization = 0
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    Tempo = 120.000000000000000000
    VstVersion = 2300
    VendorVersion = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    Left = 96
    Top = 75
  end
  object ASIOHost: TASIOHost
    CanDos = []
    PreventClipping = pcAnalog
    ConvertOptimizations = [coSSE, co3DNow]
    SampleRate = 44100.000000000000000000
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    OnReset = ASIOHostReset
    OnLatencyChanged = ASIOHostReset
    OnBufferSwitch = ASIOHostBufferSwitch
    Left = 124
    Top = 75
  end
  object XPManifest1: TXPManifest
    Left = 152
    Top = 75
  end
end
