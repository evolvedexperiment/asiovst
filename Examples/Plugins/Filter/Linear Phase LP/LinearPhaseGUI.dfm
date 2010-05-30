object FmLinearPhase: TFmLinearPhase
  Left = 425
  Top = 147
  BorderStyle = bsNone
  Caption = 'Linear Phase Lowpass'
  ClientHeight = 133
  ClientWidth = 111
  Color = 6974058
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clSilver
  Font.Height = -16
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 18
  object DialFrequency: TGuiDial
    Left = 24
    Top = 36
    Width = 65
    Height = 65
    CircleColor = 6974058
    CurveMapping = -2.099999904632568000
    DefaultPosition = 1000.000000000000000000
    DialImageIndex = -1
    LineColor = clSilver
    LineWidth = 2
    Max = 20000.000000000000000000
    Min = 20.000000000000000000
    NumGlyphs = 65
    OnChange = DialFrequencyChange
    OnDblClick = DialFrequencyDblClick
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 20000.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbFrequency: TGuiLabel
    Left = 8
    Top = 14
    Width = 95
    Height = 21
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Frequency'
  end
  object LbFrequencyValue: TGuiLabel
    Left = 10
    Top = 102
    Width = 89
    Height = 22
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '20 kHz'
    OnDblClick = DialFrequencyDblClick
  end
end
