object VSTGUI: TVSTGUI
  Left = 317
  Top = 185
  BorderStyle = bsNone
  Caption = 'SineSynth'
  ClientHeight = 73
  ClientWidth = 503
  Color = clBtnFace
  TransparentColorValue = 12948623
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 6695441
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
    Top = 0
    Width = 503
    Height = 73
    BaseOctave = 3
    NumOctaves = 4
    Align = alClient
    Color = clWhite
    OnMidiKeyDown = MidiKeysMidiKeyDown
    OnMidiKeyUp = MidiKeysMidiKeyUp
    ExplicitLeft = -8
    ExplicitTop = -8
    ExplicitWidth = 522
    ExplicitHeight = 93
  end
end
