object OversampleTemplateDataModule: TOversampleTemplateDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Oversample Template'
  ProductName = 'DAV Wrapper Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdReceiveVstTimeInfo, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'Spli'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Oversampling'
      DisplayName = 'Oversampling'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Oversmp'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOversamplingChange
      OnCustomParameterDisplay = ParamOversamplingDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Oversampling'
      DisplayName = 'OS Factor'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Factor'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOSFactorChange
      OnCustomParameterDisplay = ParamOSFactorDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Pre-Filter'
      DisplayName = 'OS Pre-Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      ReportVST2Properties = True
      ShortLabel = 'Pre-Ord'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamPreFilterOrderValue
      OnCustomParameterDisplay = ParamOrderDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Pre-Filter'
      DisplayName = 'OS Pre-Filter Transition Bandw'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Pre-BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamPreTransBWChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Pre-Filter'
      DisplayName = 'OS Pre-Filter Characteristic'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      ReportVST2Properties = True
      ShortLabel = 'Pre-Chr'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamCharChange
      OnCustomParameterDisplay = ParamCharacterDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Post-Filter'
      DisplayName = 'OS Post-Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      ReportVST2Properties = True
      ShortLabel = 'Pst-Ord'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamPostOrderChange
      OnCustomParameterDisplay = ParamOrderDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Post-Filter'
      DisplayName = 'OS Post-Filter Transition'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Pst-BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamPostFilterBWChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Post-Filter'
      DisplayName = 'OS Post-Filter Characteristic'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      ReportVST2Properties = True
      ShortLabel = 'Pst-Chr'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamPostCharChange
      OnCustomParameterDisplay = ParamCharacterDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Oversampling'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pre-Filter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Post-Filter'
      VSTModule = Owner
    end>
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnEditClose = VSTModuleEditClose
  OnEditIdle = VSTModuleEditIdle
  OnEditTop = VSTModuleEditTop
  OnEditSleep = VSTModuleEditSleep
  OnEditorKeyUp = VSTModuleEditorKeyUp
  OnEditorKeyDown = VSTModuleEditorKeyDown
  OnAfterProgramChange = VSTModuleAfterProgramChange
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnGetVU = VSTModuleGetVU
  OnOfflineNotify = VSTModuleOfflineNotify
  OnOfflinePrepare = VSTModuleOfflinePrepare
  OnOfflineRun = VSTModuleOfflineRun
  OnProcess = VSTModuleProcess32OversampleSingle
  OnProcessDoubleReplacing = VSTModuleProcess64OversampleSingle
  OnProcessEvents = VSTModuleProcessEvents
  OnProcessReplacing = VSTModuleProcess32OversampleSingle
  OnProcessVarIO = VSTModuleProcessVarIO
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  OnStartProcess = VSTModuleStartProcess
  OnStopProcess = VSTModuleStopProcess
  OnSuspend = VSTModuleSuspend
  OnVendorSpecific = VSTModuleVendorSpecific
  Left = 261
  Top = 82
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
        VstOfflineTasks = <>
        OnAudioMasterAutomate = ParamAutomate
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 8
    Top = 8
  end
end
