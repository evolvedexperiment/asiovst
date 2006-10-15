object VSTGUI: TVSTGUI
  Left = 317
  Top = 185
  BorderStyle = bsNone
  Caption = 'SimpleSampler'
  ClientHeight = 105
  ClientWidth = 365
  Color = clBtnFace
  TransparentColorValue = 12948623
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object MidiKeys: TMidiKeys
    Left = 0
    Top = 32
    Width = 365
    Height = 73
    BaseOctave = 4
    Align = alBottom
    Color = clWhite
    OnMidiKeyDown = MidiKeysMidiKeyDown
    OnMidiKeyUp = MidiKeysMidiKeyUp
    OnKeyColor = MidiKeysKeyColor
    ExplicitLeft = -16
    ExplicitTop = 33
    ExplicitWidth = 409
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Sample:'
  end
  object EditSample: TEdit
    Left = 58
    Top = 5
    Width = 200
    Height = 21
    TabOrder = 0
    OnChange = EditSampleChange
  end
  object BtSampleSelect: TButton
    Left = 264
    Top = 5
    Width = 93
    Height = 21
    Caption = 'Select...'
    TabOrder = 1
    OnClick = BtSampleSelectClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files (*.wav)|*.wav'
    Title = 'Select a Sample...'
    Left = 8
    Top = 40
  end
end
