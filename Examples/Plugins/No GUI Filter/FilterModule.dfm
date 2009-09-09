object VSTFilter: TVSTFilter
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'Delphi VST Filter'
  ProductName = 'Delphi VST Filter'
  VendorName = 'Delphi VST'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Preset 1'
  IORatio = 1.000000000000000000
  UniqueID = 'Filt'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 3'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Cutoff Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Cutoff'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = VSTFilterParameterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Resonance'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Res'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      VSTModule = Owner
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  Left = 443
  Top = 102
  Height = 150
  Width = 215
end
