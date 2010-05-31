object FmFractionalOctaveProcessor: TFmFractionalOctaveProcessor
  Left = 287
  Top = 277
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Fractional Octave Processor'
  ClientHeight = 84
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbInputFile: TLabel
    Left = 8
    Top = 11
    Width = 46
    Height = 13
    Caption = 'Input File:'
  end
  object LbOutputFile: TLabel
    Left = 8
    Top = 38
    Width = 54
    Height = 13
    Caption = 'Output File:'
  end
  object LbBandSeparation: TLabel
    Left = 8
    Top = 62
    Width = 82
    Height = 13
    Caption = 'Band Separation:'
  end
  object BtProcess: TButton
    Left = 294
    Top = 8
    Width = 91
    Height = 48
    Caption = 'Process'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = BtProcessClick
  end
  object EdInput: TEdit
    Left = 68
    Top = 8
    Width = 220
    Height = 21
    TabOrder = 1
  end
  object EdOutput: TEdit
    Left = 68
    Top = 35
    Width = 220
    Height = 21
    TabOrder = 2
  end
  object BtSelectInputFile: TButton
    Left = 269
    Top = 10
    Width = 17
    Height = 17
    Caption = '...'
    TabOrder = 3
    OnClick = BtSelectInputFileClick
  end
  object BtSelectOutputFile: TButton
    Left = 269
    Top = 37
    Width = 17
    Height = 17
    Caption = '...'
    TabOrder = 4
    OnClick = BtSelectOutputFileClick
  end
  object RbFilter: TRadioButton
    Left = 96
    Top = 62
    Width = 49
    Height = 17
    Caption = 'Filter'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object RbFFT: TRadioButton
    Left = 144
    Top = 62
    Width = 49
    Height = 17
    Caption = 'FFT'
    Enabled = False
    TabOrder = 6
  end
  object CbDownsampling: TCheckBox
    Left = 199
    Top = 62
    Width = 97
    Height = 17
    Caption = 'Downsampling'
    TabOrder = 7
  end
end
