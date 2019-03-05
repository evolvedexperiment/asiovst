object FormSetup: TFormSetup
  Left = 333
  Top = 221
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 109
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    228
    109)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPreset: TLabel
    Left = 4
    Top = 2
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LabelOutput: TLabel
    Left = 4
    Top = 29
    Width = 50
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object LabelPlaybackSampleRate: TLabel
    Left = 4
    Top = 88
    Width = 138
    Height = 13
    Caption = 'Playback Samplerate'
    Layout = tlCenter
  end
  object ComboBoxDrivers: TComboBox
    Left = 88
    Top = 2
    Width = 136
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ComboBoxDriversChange
  end
  object ComboBoxOutput: TComboBox
    Left = 64
    Top = 29
    Width = 160
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = ComboBoxOutputChange
  end
  object ButtonControlPanel: TButton
    Left = 4
    Top = 55
    Width = 220
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Control Panel'
    TabOrder = 2
    OnClick = ButtonControlPanelClick
  end
  object SpinEditSampleRate: TSpinEdit
    Left = 148
    Top = 85
    Width = 76
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 44100
    OnChange = SpinEditSampleRateChange
  end
end
