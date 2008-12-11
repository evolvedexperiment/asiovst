object FmLinearPhase: TFmLinearPhase
  Left = 425
  Top = 147
  BorderStyle = bsNone
  Caption = 'Linear Phase Lowpass'
  ClientHeight = 133
  ClientWidth = 111
  Color = 6974058
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DialFrequency: TGuiDial
    Left = 24
    Top = 36
    Width = 65
    Height = 65
    LineColor = clSilver
    CircleColor = 6974058
    CurveMapping = -2.099999904632568000
    DefaultPosition = 1000.000000000000000000
    Max = 20000.000000000000000000
    Min = 20.000000000000000000
    NumGlyphs = 65
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1000.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    OnChange = DialFrequencyChange
  end
  object LbFrequency: TGuiLabel
    Left = 10
    Top = 14
    Width = 89
    Height = 21
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Frequency'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbFrequencyValue: TGuiLabel
    Left = 10
    Top = 102
    Width = 89
    Height = 22
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '1 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
end
