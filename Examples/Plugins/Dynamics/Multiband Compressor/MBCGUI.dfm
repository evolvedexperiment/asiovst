object FmMBC: TFmMBC
  Left = 248
  Top = 126
  BorderStyle = bsNone
  Caption = 'FmMBC'
  ClientHeight = 307
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    775
    307)
  PixelsPerInch = 96
  TextHeight = 14
  object LbAbout1: TLabel
    Left = 48
    Top = 10
    Width = 168
    Height = 15
    Caption = 'D3 Multi Band Compressor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbAbout2: TLabel
    Left = 591
    Top = 11
    Width = 168
    Height = 14
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'delphi asio vst presents'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ExplicitLeft = 474
  end
  object LbMasterGain: TLabel
    Left = 34
    Top = 71
    Width = 84
    Height = 14
    Caption = 'Master Gain:'
  end
  object LbCrossover: TLabel
    Left = 34
    Top = 114
    Width = 70
    Height = 14
    Caption = 'Crossover:'
  end
  object LbMasterGaindB: TLabel
    Left = 191
    Top = 89
    Width = 14
    Height = 14
    Caption = 'dB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbLowFreqHz: TLabel
    Left = 191
    Top = 131
    Width = 14
    Height = 14
    Caption = 'Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbHighFreqHz: TLabel
    Left = 191
    Top = 157
    Width = 14
    Height = 14
    Caption = 'Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object DlLowThreshold: TDial
    Left = 33
    Top = 236
    Width = 35
    Height = 35
    Min = -48.000000000000000000
    Max = 6.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlLowThresholdChange
  end
  object DlLowRatio: TDial
    Left = 77
    Top = 236
    Width = 35
    Height = 35
    Max = 1.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlLowRatioChange
  end
  object DlLowAttack: TDial
    Left = 124
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlLowAttackChange
  end
  object DlLowRelease: TDial
    Left = 168
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlLowReleaseChange
  end
  object DlLowGain: TDial
    Left = 212
    Top = 236
    Width = 35
    Height = 35
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlLowGainChange
  end
  object DlMidThreshold: TDial
    Left = 286
    Top = 236
    Width = 35
    Height = 35
    Min = -48.000000000000000000
    Max = 6.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlMidThresholdChange
  end
  object DlMidRatio: TDial
    Left = 327
    Top = 236
    Width = 35
    Height = 35
    Max = 1.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlMidRatioChange
  end
  object DlMidAttack: TDial
    Left = 374
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlMidAttackChange
  end
  object DlMidRelease: TDial
    Left = 418
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlMidReleaseChange
  end
  object DlMidGain: TDial
    Left = 462
    Top = 236
    Width = 35
    Height = 35
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlMidGainChange
  end
  object DlHighThreshold: TDial
    Left = 533
    Top = 236
    Width = 35
    Height = 35
    Min = -48.000000000000000000
    Max = 6.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlHighThresholdChange
  end
  object DlHighRatio: TDial
    Left = 577
    Top = 236
    Width = 35
    Height = 35
    Max = 1.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlHighRatioChange
  end
  object DlHighAttack: TDial
    Left = 624
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlHighAttackChange
  end
  object DlHighRelease: TDial
    Left = 668
    Top = 236
    Width = 35
    Height = 35
    Position = 1.000000000000000000
    Min = 1.000000000000000000
    Max = 3.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlHighReleaseChange
  end
  object DlHighGain: TDial
    Left = 712
    Top = 236
    Width = 35
    Height = 35
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    ColorCircle = 3160125
    ColorLine = 15133420
    StitchKind = skHorizontal
    PointerAngles.Start = 222
    PointerAngles.Range = 274
    PointerAngles.Resolution = 274.000000000000000000
    OnChange = DlHighGainChange
  end
  object Label1: TLabel
    Left = 34
    Top = 200
    Width = 56
    Height = 14
    Caption = 'Low Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 283
    Top = 200
    Width = 56
    Height = 14
    Caption = 'Mid Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 533
    Top = 200
    Width = 63
    Height = 14
    Caption = 'High Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowThreshold: TLabel
    Left = 27
    Top = 277
    Width = 45
    Height = 12
    Alignment = taCenter
    Caption = 'Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowRatio: TLabel
    Left = 82
    Top = 277
    Width = 25
    Height = 12
    Alignment = taCenter
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowAttack: TLabel
    Left = 125
    Top = 277
    Width = 30
    Height = 12
    Alignment = taCenter
    Caption = 'Attack'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowRelease: TLabel
    Left = 168
    Top = 277
    Width = 35
    Height = 12
    Alignment = taCenter
    Caption = 'Release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowGain: TLabel
    Left = 218
    Top = 277
    Width = 20
    Height = 12
    Alignment = taCenter
    Caption = 'Gain'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidThreshold: TLabel
    Left = 279
    Top = 277
    Width = 45
    Height = 12
    Alignment = taCenter
    Caption = 'Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidRatio: TLabel
    Left = 334
    Top = 277
    Width = 25
    Height = 12
    Alignment = taCenter
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidAttack: TLabel
    Left = 377
    Top = 277
    Width = 30
    Height = 12
    Alignment = taCenter
    Caption = 'Attack'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidRelease: TLabel
    Left = 421
    Top = 277
    Width = 35
    Height = 12
    Alignment = taCenter
    Caption = 'Release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidGain: TLabel
    Left = 471
    Top = 277
    Width = 20
    Height = 12
    Alignment = taCenter
    Caption = 'Gain'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighThreshold: TLabel
    Left = 528
    Top = 277
    Width = 45
    Height = 12
    Alignment = taCenter
    Caption = 'Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighRatio: TLabel
    Left = 583
    Top = 277
    Width = 25
    Height = 12
    Alignment = taCenter
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighAttack: TLabel
    Left = 626
    Top = 277
    Width = 30
    Height = 12
    Alignment = taCenter
    Caption = 'Attack'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighRelease: TLabel
    Left = 670
    Top = 277
    Width = 35
    Height = 12
    Alignment = taCenter
    Caption = 'Release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighGain: TLabel
    Left = 720
    Top = 277
    Width = 20
    Height = 12
    Alignment = taCenter
    Caption = 'Gain'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowThresholddB: TLabel
    Left = 27
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowRatioValue: TLabel
    Left = 72
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowAttackValue: TLabel
    Left = 120
    Top = 220
    Width = 43
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowReleaseValue: TLabel
    Left = 163
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbLowGaindB: TLabel
    Left = 207
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidThresholddB: TLabel
    Left = 276
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidRatioValue: TLabel
    Left = 321
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidAttackValue: TLabel
    Left = 369
    Top = 220
    Width = 43
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidReleaseValue: TLabel
    Left = 412
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbMidGaindB: TLabel
    Left = 456
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighThresholddB: TLabel
    Left = 526
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighRatioValue: TLabel
    Left = 571
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighAttackValue: TLabel
    Left = 619
    Top = 220
    Width = 43
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighReleaseValue: TLabel
    Left = 662
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object LbHighGaindB: TLabel
    Left = 706
    Top = 220
    Width = 45
    Height = 12
    Alignment = taCenter
    AutoSize = False
    Caption = 'xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
  end
  object MeterIn: TPaintBox
    Left = 268
    Top = 48
    Width = 49
    Height = 98
    OnPaint = MeterInPaint
  end
  object MeterOut: TPaintBox
    Left = 343
    Top = 48
    Width = 49
    Height = 98
    OnPaint = MeterOutPaint
  end
  object PaintBox1: TPaintBox
    Left = 418
    Top = 48
    Width = 94
    Height = 98
    OnPaint = MeterOutPaint
  end
  object PaintBox2: TPaintBox
    Left = 538
    Top = 48
    Width = 95
    Height = 98
    OnPaint = MeterOutPaint
  end
  object PaintBox3: TPaintBox
    Left = 658
    Top = 48
    Width = 95
    Height = 98
    OnPaint = MeterOutPaint
  end
  object LbInputL: TLabel
    Left = 271
    Top = 149
    Width = 7
    Height = 14
    Caption = 'L'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbInputR: TLabel
    Left = 306
    Top = 149
    Width = 7
    Height = 14
    Caption = 'R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label4: TLabel
    Left = 347
    Top = 149
    Width = 7
    Height = 14
    Caption = 'L'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label5: TLabel
    Left = 382
    Top = 149
    Width = 7
    Height = 14
    Caption = 'R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label6: TLabel
    Left = 276
    Top = 163
    Width = 35
    Height = 14
    Caption = 'input'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label7: TLabel
    Left = 347
    Top = 163
    Width = 42
    Height = 14
    Caption = 'output'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label8: TLabel
    Left = 424
    Top = 149
    Width = 35
    Height = 14
    Caption = 'input'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label9: TLabel
    Left = 548
    Top = 149
    Width = 35
    Height = 14
    Caption = 'input'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label10: TLabel
    Left = 665
    Top = 149
    Width = 35
    Height = 14
    Caption = 'input'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label11: TLabel
    Left = 484
    Top = 149
    Width = 28
    Height = 14
    Caption = 'red.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label12: TLabel
    Left = 605
    Top = 149
    Width = 28
    Height = 14
    Caption = 'red.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label13: TLabel
    Left = 725
    Top = 149
    Width = 28
    Height = 14
    Caption = 'red.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label14: TLabel
    Left = 440
    Top = 163
    Width = 56
    Height = 14
    Caption = 'Low Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label15: TLabel
    Left = 561
    Top = 163
    Width = 56
    Height = 14
    Caption = 'Mid Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label16: TLabel
    Left = 677
    Top = 163
    Width = 63
    Height = 14
    Caption = 'High Band'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object CBLimiter: TCheckBox
    Left = 167
    Top = 48
    Width = 71
    Height = 17
    Caption = 'Limiter'
    Enabled = False
    TabOrder = 0
  end
  object RbLPFIR: TRadioButton
    Left = 98
    Top = 48
    Width = 62
    Height = 17
    Caption = 'LP FIR'
    Enabled = False
    TabOrder = 1
  end
  object RBBWIIR: TRadioButton
    Left = 34
    Top = 48
    Width = 64
    Height = 17
    Caption = 'BW IIR'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object SbMasterGain: TScrollBar
    Left = 34
    Top = 87
    Width = 151
    Height = 16
    Max = 60
    Min = -300
    PageSize = 0
    TabOrder = 3
    OnChange = SbMasterGainChange
  end
  object SbLowFreq: TScrollBar
    Left = 34
    Top = 129
    Width = 151
    Height = 16
    Max = 10000
    PageSize = 0
    TabOrder = 4
    OnChange = SbLowFreqChange
  end
  object SbHighFreq: TScrollBar
    Left = 34
    Top = 155
    Width = 151
    Height = 16
    Max = 10000
    PageSize = 0
    TabOrder = 5
    OnChange = SbHighFreqChange
  end
end
