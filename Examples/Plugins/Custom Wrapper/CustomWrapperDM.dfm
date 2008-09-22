object CustomWrapperDataModule: TCustomWrapperDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'CustomWrapper'
  ProductName = 'CustomWrapper'
  VendorName = 'Delphi ASIO & VST Packages'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'Wrap'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcessReplacing
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnStartProcess = VSTModuleStartProcess
  OnStopProcess = VSTModuleStopProcess
  Left = 215
  Top = 113
  Height = 150
  Width = 215
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 8
    Top = 8
  end
end
