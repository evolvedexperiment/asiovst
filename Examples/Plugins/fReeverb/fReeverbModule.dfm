object fReeverbVST: TfReeverbVST
  OldCreateOrder = False
  OnCreate = VST2ModuleCreate
  OnDestroy = VST2ModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'fReeverb'
  ProductName = 'fReeverb'
  VendorName = 'fReeverb'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vpcEffect
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = -1
  KeysRequired = False
  UniqueID = 'fRee'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Dry'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties0ParameterChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Wet'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties1ParameterChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Width'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties2ParameterChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'RoomSize'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties3ParameterChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'FreeZe'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties4ParameterChange
    end
    item
      Max = 20.000000000000000000
      Curve = ctLinear
      DisplayName = 'Stretch'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties5ParameterChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Damp'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties6ParameterChange
    end
    item
      Min = 1.000000000000000000
      Max = 16.000000000000000000
      Curve = ctLinear
      DisplayName = 'NumAllPasses'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties7ParameterChange
    end
    item
      Min = 1.000000000000000000
      Max = 16.000000000000000000
      Curve = ctLinear
      DisplayName = 'NumCombs'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = fReeverbVSTParameterProperties8ParameterChange
    end>
  OnEditOpen = VST_EditOpen
  OnSampleRateChange = VST2ModuleSampleRateChange
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcessReplacing
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
