object PhaserModule: TPhaserModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Phaser VST Example'
  ProductName = 'Phaser VST Example'
  VendorName = 'Delphi ASIO & VST Example'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'phsx'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Depth'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepInteger = 0
      ShortLabel = 'depth'
      VSTModule = Owner
      OnParameterChange = PMDepthChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Feedback'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      ShortLabel = 'fdbck'
      VSTModule = Owner
      OnParameterChange = PMFeedbackChange
    end
    item
      Min = 20.000000000000000000
      Max = 20000.000000000000000000
      Curve = ctLinear
      DisplayName = 'Minimum'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 20.000000000000000000
      StepFloat = 20.000000000000000000
      SmallStepFloat = 20.000000000000000000
      LargeStepFloat = 200.000000000000000000
      MinInteger = 20
      MaxInteger = 20000
      StepInteger = 20
      LargeStepInteger = 200
      ShortLabel = 'min'
      VSTModule = Owner
      OnParameterChange = PMMinimumChange
    end
    item
      Min = 20.000000000000000000
      Max = 20000.000000000000000000
      Curve = ctLinear
      DisplayName = 'Maximum'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 20.000000000000000000
      StepFloat = 20.000000000000000000
      SmallStepFloat = 20.000000000000000000
      LargeStepFloat = 200.000000000000000000
      MinInteger = 20
      MaxInteger = 20000
      StepInteger = 20
      LargeStepInteger = 200
      ShortLabel = 'max'
      VSTModule = Owner
      OnParameterChange = PMMaximumChange
    end
    item
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Rate'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 10
      LargeStepInteger = 2
      ShortLabel = 'rate'
      VSTModule = Owner
      OnParameterChange = PMRateChange
    end
    item
      Min = 1.000000000000000000
      Max = 5.000000000000000000
      Curve = ctLinear
      DisplayName = 'Stages'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MinInteger = 1
      MaxInteger = 5
      LargeStepInteger = 1
      ShortLabel = 'stage'
      VSTModule = Owner
      OnParameterChange = PMStagesChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 272
  Top = 81
  Height = 150
  Width = 215
end
