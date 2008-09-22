object FmPluginMerger: TFmPluginMerger
  Left = 469
  Top = 326
  Caption = 'Plugin Merger'
  ClientHeight = 278
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbMergedVSTPlugins: TLabel
    Left = 8
    Top = 8
    Width = 97
    Height = 13
    Caption = 'Merged VST Plugins:'
  end
  object LbKnob: TLabel
    Left = 8
    Top = 186
    Width = 63
    Height = 13
    Caption = 'Knob Bitmap:'
  end
  object LbBackgroundColor: TLabel
    Left = 8
    Top = 238
    Width = 88
    Height = 13
    Caption = 'Background Color:'
  end
  object ShBackgroundColor: TShape
    Left = 102
    Top = 238
    Width = 13
    Height = 13
    Brush.Color = clSilver
    OnMouseDown = ShBackgroundColorMouseDown
  end
  object LbKnobsPerRow: TLabel
    Left = 8
    Top = 213
    Width = 73
    Height = 13
    Caption = 'Knobs per row:'
  end
  object LBPlugins: TListBox
    Left = 8
    Top = 24
    Width = 241
    Height = 153
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBPluginsClick
  end
  object EdKnob: TEdit
    Left = 72
    Top = 183
    Width = 177
    Height = 21
    TabOrder = 1
    OnChange = EdKnobChange
    OnClick = EdKnobClick
  end
  object SEKnobsPerRow: TSpinEdit
    Left = 87
    Top = 210
    Width = 42
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 2
    Value = 5
  end
  object CBAntialiasedFont: TCheckBox
    Left = 8
    Top = 257
    Width = 121
    Height = 17
    Caption = 'Use Antialiased Font'
    TabOrder = 3
    OnClick = CBAntialiasedFontClick
  end
  object GBPreview: TGuiGroup
    Left = 135
    Top = 210
    Width = 114
    Height = 64
    Caption = 'Preview'
    Color = clSilver
    ParentColor = False
    Radius = 3
    TabOrder = 4
    Transparent = True
    object DialPreview: TGuiDial
      Left = 56
      Top = 8
      Width = 48
      Height = 48
      LineWidth = 2
      LineColor = clBtnShadow
      Max = 100.000000000000000000
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
    end
    object LbTest: TGuiLabel
      Left = 19
      Top = 32
      Width = 25
      Height = 17
      Caption = 'Test'
    end
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 1
    object MIFile: TMenuItem
      Caption = 'File'
      object MISaveasVST: TMenuItem
        Caption = 'Save as VST...'
        Enabled = False
        OnClick = MISaveasVSTClick
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIPlugin: TMenuItem
      Caption = 'Plugin'
      object MIAdd: TMenuItem
        Caption = 'Add...'
        OnClick = MIAddClick
      end
      object MIClear: TMenuItem
        Caption = 'Clear'
        OnClick = MIClearClick
      end
    end
  end
end
