object PlugInPlugModule: TPlugInPlugModule
  OldCreateOrder = False
  OnCreate = VST2ModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Load VST Plugin'
  ProductName = 'Load VST Plugin'
  VendorName = 'Delphi VST example'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vcgUnknown
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = -1
  KeysRequired = False
  UniqueID = 'Plug'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnOpen = VST2ModuleOpen
  OnClose = VST2ModuleClose
  OnEditOpen = VST_EditOpen
  OnEditIdle = VST2ModuleEditIdle
  OnEditTop = VST2ModuleEditTop
  OnEditSleep = VST2ModuleEditSleep
  OnParameterChange = VST2ModuleParameterChange
  OnBlockSizeChange = VST2ModuleBlockSizeChange
  OnSampleRateChange = VST2ModuleSampleRateChange
  OnGetVU = VST2ModuleGetVU
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcessReplacing
  OnStartProcess = VST2ModuleStartProcess
  OnStopProcess = VST2ModuleStopProcess
  OnBeforeProgramChange = VST2ModuleBeforeProgramChange
  OnCanDo = VST2ModuleCanDo
  Left = 254
  Top = 128
  Height = 199
  Width = 283
  object VstHost: TVstHost
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
    Left = 32
    Top = 16
  end
end
