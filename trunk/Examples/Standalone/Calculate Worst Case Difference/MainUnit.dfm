object FmCalculateWorstCaseDifference: TFmCalculateWorstCaseDifference
  Left = 0
  Top = 0
  Caption = 'Calculate Worst Case Difference'
  ClientHeight = 337
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    377
    337)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 8
    Top = 39
    Width = 361
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object BtAddFiles: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Add Files...'
    Default = True
    TabOrder = 1
    OnClick = BtAddFilesClick
  end
  object BtStartCalculation: TButton
    Left = 151
    Top = 8
    Width = 107
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Start Calculation'
    Enabled = False
    TabOrder = 2
    OnClick = BtStartCalculationClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 318
    Width = 377
    Height = 19
    Panels = <
      item
        Text = 'Trial:'
        Width = 70
      end
      item
        Text = 'Average:'
        Width = 124
      end
      item
        Text = 'Maximum:'
        Width = 50
      end>
  end
  object BtClear: TButton
    Left = 103
    Top = 8
    Width = 42
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = BtClearClick
  end
  object BtBuildDifference: TButton
    Left = 264
    Top = 8
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Build Difference...'
    Enabled = False
    TabOrder = 5
    OnClick = BtBuildDifferenceClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'WAV (*.wav)|*.wav'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 32
    Top = 56
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.wav'
    Filter = 'WAV (*.wav)|*.wav'
    Left = 104
    Top = 56
  end
end
