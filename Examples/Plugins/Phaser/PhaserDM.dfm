object PhaserModule: TPhaserModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Phaser VST Example'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'phsx'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Depth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      ShortLabel = 'depth'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = PMDepthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Feedback'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 100.000000000000000000
      ShortLabel = 'fdbck'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = PMFeedbackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Minimum'
      LargeStepFloat = 200.000000000000000000
      LargeStepInteger = 200
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'min'
      SmallStepFloat = 20.000000000000000000
      SmoothingFactor = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = PMMinimumChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Maximum'
      LargeStepFloat = 200.000000000000000000
      LargeStepInteger = 200
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'max'
      SmallStepFloat = 20.000000000000000000
      SmoothingFactor = 20.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = PMMaximumChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = 'rate'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = PMRateChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stages'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 5.000000000000000000
      MaxInteger = 5
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'stage'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = PMStagesChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  Left = 272
  Top = 81
  Height = 150
  Width = 215
end
