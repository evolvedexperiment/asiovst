object FormVSTEditor: TFormVSTEditor
  Left = 399
  Top = 298
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'VST Plugin Editor'
  ClientHeight = 124
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelPlugin: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 124
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
        OnShowEdit = ShowCustomEdit
        OnCloseEdit = CloseCustomEdit
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2400
    Left = 140
    Top = 67
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcAnalog
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnLatencyChanged = ASIOHostReset
    OnReset = ASIOHostReset
    Left = 140
    Top = 19
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 228
    Top = 67
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'fxp'
    Filter = 'VST Preset (*.fxp)|*.fxp|VST Bank (*.fxb)|*.fxb'
    Left = 228
    Top = 19
  end
  object MainMenu: TMainMenu
    Left = 28
    Top = 19
    object MenuItemStandalone: TMenuItem
      Caption = '&Standalone'
      object MenuItemExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuItemExitClick
      end
    end
    object MenuItemGUI: TMenuItem
      Caption = 'GUI'
      object MenuItemGuiDefault: TMenuItem
        Caption = 'Default'
        Checked = True
        RadioItem = True
        OnClick = MenuItemGuiDefaultClick
      end
      object MenuItemGuiList: TMenuItem
        Caption = 'List'
        RadioItem = True
        OnClick = MenuItemGuiListClick
      end
      object MenuItemGuiSelector: TMenuItem
        Caption = 'Selector'
        RadioItem = True
        OnClick = MenuItemGuiSelectorClick
      end
      object MenuItemGuiCustom: TMenuItem
        Caption = 'Custom'
        RadioItem = True
        OnClick = MenuItemGuiCustomClick
      end
    end
    object MenuItemProgram: TMenuItem
      Caption = '&Presets'
      object MenuItemLoadPreset: TMenuItem
        Caption = '&Load Preset...'
        OnClick = MenuItemLoadPresetClick
      end
      object MenuItemSavePreset: TMenuItem
        Caption = '&Save Preset...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
    object MenuItemAudio: TMenuItem
      Caption = 'Audio'
      object MenuItemSetup: TMenuItem
        Caption = 'Setup'
        OnClick = MenuItemSetupClick
      end
    end
  end
end
