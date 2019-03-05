object FormVST: TFormVST
  Left = 343
  Top = 356
  Caption = 'VST Setup'
  ClientHeight = 128
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    299
    128)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxOutputVST: TGroupBox
    Left = 8
    Top = 67
    Width = 284
    Height = 54
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Output VST '
    TabOrder = 0
    object LabelOutputVST: TLabel
      Left = 11
      Top = 25
      Width = 22
      Height = 13
      Caption = 'VST:'
    end
    object EditOutputVST: TEdit
      Left = 39
      Top = 22
      Width = 185
      Height = 21
      TabOrder = 0
      OnChange = EditOutputVSTChange
    end
    object ButtonOutputVST: TButton
      Left = 204
      Top = 24
      Width = 18
      Height = 18
      Caption = '...'
      TabOrder = 1
      OnClick = ButtonOutputVSTClick
    end
    object ButtonOutputEditor: TButton
      Left = 230
      Top = 22
      Width = 43
      Height = 21
      Caption = 'Editor'
      Enabled = False
      TabOrder = 2
      OnClick = ButtonOutputEditorClick
    end
  end
  object GroupBoxRealtimeVST: TGroupBox
    Left = 8
    Top = 4
    Width = 284
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Realtime FX VST '
    TabOrder = 1
    object LabelRealtimeVST: TLabel
      Left = 11
      Top = 25
      Width = 22
      Height = 13
      Caption = 'VST:'
    end
    object EditRealtimeVST: TEdit
      Left = 39
      Top = 22
      Width = 185
      Height = 21
      TabOrder = 0
      OnChange = EditRealtimeVSTChange
    end
    object ButtonRealtimeVST: TButton
      Left = 204
      Top = 24
      Width = 18
      Height = 18
      Caption = '...'
      TabOrder = 1
      OnClick = ButtonRealtimeVSTClick
    end
    object ButtonRealtimeEditor: TButton
      Left = 230
      Top = 22
      Width = 43
      Height = 21
      Caption = 'Editor'
      Enabled = False
      TabOrder = 2
      OnClick = ButtonRealtimeEditorClick
    end
  end
end
