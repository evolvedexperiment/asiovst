object FmPage: TFmPage
  Left = 372
  Top = 223
  BorderStyle = bsNone
  Caption = 'FmPage'
  ClientHeight = 400
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    340
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 324
    Height = 384
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
    ExplicitWidth = 224
    ExplicitHeight = 144
  end
  object VstHost: TVstHost
    VstPlugIns = <
      item
        DisplayName = 'TVstPlugIn'
      end>
    ParameterQuantization = 0
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    Tempo = 120.000000000000000000
    VstVersion = 2400
    VendorVersion = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    Left = 8
    Top = 32
  end
end
