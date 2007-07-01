object VSTGUI: TVSTGUI
  Left = 300
  Top = 179
  BorderStyle = bsNone
  Caption = 'Delayla'
  ClientHeight = 50
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbSamples: TLabel
    Left = 8
    Top = 32
    Width = 244
    Height = 13
    Margins.Bottom = 0
    Alignment = taCenter
    AutoSize = False
  end
  object SampleBar: TScrollBar
    Left = 8
    Top = 8
    Width = 244
    Height = 17
    Max = 44100
    Min = 1
    PageSize = 0
    Position = 441
    TabOrder = 0
    OnChange = SampleBarChange
  end
end
