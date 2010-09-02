object FmBarberpoleShifter: TFmBarberpoleShifter
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Bode Frequency Shifter'
  ClientHeight = 129
  ClientWidth = 223
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GpFrequency: TGuiGroup
    Left = 8
    Top = 8
    Width = 105
    Height = 113
    AntiAlias = gaaLinear4x
    Caption = 'Frequency'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    GroupColor = 15659506
    LineColor = 15659506
    OutlineWidth = 3
    PanelColor = 7373965
    ParentFont = False
    Radius = 5
    TabOrder = 0
    DesignSize = (
      105
      113)
    object DialFrequency: TGuiDial
      Left = 32
      Top = 32
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -2.069999933242798000
      DefaultPosition = 10.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 10.000000000000000000
      Min = 0.009999999776482582
      GlyphCount = 65
      OnChange = DialFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object PnDisplay: TGuiPanel
      Left = 8
      Top = 86
      Width = 90
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'PnDisplay'
      LineColor = 5267301
      BorderWidth = 1.000000000000000000
      PanelColor = 3226174
      ParentColor = True
      Radius = 5.000000000000000000
      TabOrder = 0
      UseDockManager = True
      DesignSize = (
        90
        17)
      object LbFrequencyValue: TGuiLabel
        Left = 5
        Top = 3
        Width = 81
        Height = 12
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AntiAlias = gaaLinear4x
        Caption = 'Frequency'
        Color = 3226174
        Font.Charset = ANSI_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Shadow.Color = clBlack
      end
    end
  end
  object GpMix: TGuiGroup
    Left = 119
    Top = 8
    Width = 98
    Height = 113
    AntiAlias = gaaLinear4x
    Caption = 'Mix'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    GroupColor = 15659506
    LineColor = 15659506
    OutlineWidth = 3
    PanelColor = 7373965
    ParentFont = False
    Radius = 5
    TabOrder = 1
    DesignSize = (
      98
      113)
    object DialMix: TGuiDial
      Left = 25
      Top = 32
      Width = 48
      Height = 48
      CircleColor = 3226174
      DefaultPosition = 20.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      GlyphCount = 65
      OnChange = DialMixChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object PnMix: TGuiPanel
      Left = 8
      Top = 86
      Width = 83
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'PnDisplay'
      LineColor = 5267301
      BorderWidth = 1.000000000000000000
      PanelColor = 3226174
      ParentColor = True
      Radius = 5.000000000000000000
      TabOrder = 0
      UseDockManager = True
      DesignSize = (
        83
        17)
      object LbMixValue: TGuiLabel
        Left = 5
        Top = 2
        Width = 74
        Height = 12
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AntiAlias = gaaLinear4x
        Caption = 'Mix'
        Color = 3226174
        Font.Charset = ANSI_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
        Shadow.Color = clBlack
      end
    end
  end
end
