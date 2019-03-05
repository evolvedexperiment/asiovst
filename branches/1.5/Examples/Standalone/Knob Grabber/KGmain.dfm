object FormKnobGrabber: TFormKnobGrabber
  Left = 286
  Top = 77
  Caption = 'VST Plugin Knob Grabber'
  ClientHeight = 78
  ClientWidth = 142
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelGUI: TPanel
    Left = 0
    Top = 0
    Width = 142
    Height = 78
    Align = alClient
    BevelOuter = bvNone
    Caption = '(no plugin loaded)'
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemOpen: TMenuItem
        Caption = 'Open VST Plugin...'
        OnClick = MenuItemOpenClick
      end
      object MenuItemGrabKnobs: TMenuItem
        Caption = 'Grab Knobs...'
        Enabled = False
        OnClick = MenuItemGrabKnobsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuItemExitClick
      end
    end
    object MenuItemStitch: TMenuItem
      Caption = 'Stitch'
      object MenuItemHorizontalStitch: TMenuItem
        Caption = '&horizontal'
        RadioItem = True
      end
      object MenuItemVerticalStitch: TMenuItem
        Caption = '&vertical'
        RadioItem = True
      end
      object MenuItemAutoStitch: TMenuItem
        Caption = '&auto'
        Checked = True
        RadioItem = True
      end
    end
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST\=Delphi='
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'TVstPlugIn'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 40
    Top = 8
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'DLL'
    Filter = 'VST Plugin (*.dll)|*.dll'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 8
  end
end
