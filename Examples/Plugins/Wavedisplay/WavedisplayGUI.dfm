object WavedisplayGUI: TWavedisplayGUI
  Left = 208
  Top = 110
  BorderStyle = bsNone
  Caption = 'WavedisplayGUI'
  ClientHeight = 230
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    532
    230)
  PixelsPerInch = 96
  TextHeight = 13
  object Display: TGuiDynamicWaveform
    Left = 0
    Top = 33
    Width = 491
    Height = 197
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    RedrawInterval = 50
    InternalBufferSize = 1024
  end
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 57
    Height = 13
    Caption = 'Draw mode:'
  end
  object Label2: TLabel
    Left = 143
    Top = 12
    Width = 53
    Height = 13
    Caption = 'Wave size:'
  end
  object Label3: TLabel
    Left = 271
    Top = 12
    Width = 84
    Height = 13
    Caption = 'Processing mode:'
  end
  object GuiLevelMeter1: TGuiLevelMeter
    Left = 494
    Top = 33
    Width = 34
    Height = 197
    Anchors = [akTop, akRight, akBottom]
    FillColor = clBlack
    MaxLineColor = clBlack
    MaxLineWidth = 0
    ClippingLineWidth = 0
    ClippingFillColor = clBlack
    ShowMaximum = False
    ShowClipping = scNo
    SampleRate = 44100.000000000000000000
    MaximumTimeFactor = 10.000000000000000000
    LevelAttack = 0.500000000000000000
    LevelRelease = 0.500000000000000000
    LevelDirection = ldirHorizontal
    DisplayChannels = 0
    BarWidthPercentage = 0.800000011920928900
  end
  object ddDrawMode: TComboBox
    Left = 68
    Top = 8
    Width = 61
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'Solid'
    OnChange = ddDrawModeChange
    Items.Strings = (
      'Solid'
      'Outline'
      'Points'
      'Simple')
  end
  object ddWaveSize: TComboBox
    Left = 198
    Top = 8
    Width = 61
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 2
    TabOrder = 1
    Text = '1024'
    OnChange = ddWaveSizeChange
    Items.Strings = (
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object ddProcessing: TComboBox
    Left = 358
    Top = 8
    Width = 91
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = 'Scrolling'
    OnChange = ddProcessingChange
    Items.Strings = (
      'Scrolling'
      'Replacing'
      'Stretching')
  end
end
