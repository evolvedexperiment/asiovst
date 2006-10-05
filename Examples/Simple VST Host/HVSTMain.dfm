object SimpleVSTHost: TSimpleVSTHost
  Left = 474
  Top = 254
  Width = 320
  Height = 240
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MIOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIProcess: TMenuItem
        Caption = 'ProcessAudio'
        OnClick = MIProcessClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIHelp: TMenuItem
      Caption = '&Help'
      object MIAbout: TMenuItem
        Caption = '&About'
      end
    end
  end
  object ActionList: TActionList
    Left = 40
    Top = 8
    object AcFileExit: TFileExit
      Category = 'File'
      ShortCut = 16472
    end
  end
  object VSTHost: TVstHost
    VstPlugIns = <
      item
        DisplayName = 'TVstPlugIn'
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
    Left = 72
    Top = 8
  end
end
