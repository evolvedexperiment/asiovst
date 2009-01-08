object FmDiracPitchShifter: TFmDiracPitchShifter
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Dirac PitchShifter'
  ClientHeight = 128
  ClientWidth = 129
  Color = 987667
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 15133420
  Font.Height = -19
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 23
  object DialSemitones: TGuiDial
    Left = 37
    Top = 37
    Width = 56
    Height = 56
    AntiAlias = gaaLinear2x
    DialImageIndex = -1
    LineColor = 15133420
    LineWidth = 4
    Max = 12.000000000000000000
    Min = -12.000000000000000000
    NumGlyphs = 65
    OnChange = DialSemitonesChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
  end
  object LbSemitones: TGuiLabel
    Left = 8
    Top = 8
    Width = 115
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Semitones'
  end
  object LbSemitoneValue: TGuiLabel
    Left = 8
    Top = 100
    Width = 115
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Semitones'
  end
end
