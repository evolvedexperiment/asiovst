object OversamplerTemplateDataModule: TOversamplerTemplateDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Oversampler Template'
  ProductName = 'Oversampler Template'
  VendorName = 'Delphi ASIO & VST Packages'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'Spli'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Oversampling'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Oversmp'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOversamplingChange
      OnCustomParameterDisplay = ParamOversamplingDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OS Factor'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Factor'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOSFactorChange
      OnCustomParameterDisplay = ParamOSFactorDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OS Filter Order'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'OS Filt'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OS Transition BW'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'OS Tran'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnEditIdle = VSTModuleEditIdle
  OnEditTop = VSTModuleEditTop
  OnEditSleep = VSTModuleEditSleep
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnGetVU = VSTModuleGetVU
  OnInputProperties = VSTModuleInputProperties
  OnOfflineNotify = VSTModuleOfflineNotify
  OnOfflinePrepare = VSTModuleOfflinePrepare
  OnOfflineRun = VSTModuleOfflineRun
  OnOutputProperties = VSTModuleOutputProperties
  OnProcessEvents = VSTModuleProcessEvents
  OnProcessVarIO = VSTModuleProcessVarIO
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  OnStartProcess = VSTModuleStartProcess
  OnStopProcess = VSTModuleStopProcess
  OnSuspend = VSTModuleSuspend
  OnVendorSpecific = VSTModuleVendorSpecific
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
    VstPlugIns = <
      item
        DisplayName = 'Wrapped Plugin'
        OnAudioMasterAutomate = LowParameterAutomate
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 8
    Top = 8
  end
end
