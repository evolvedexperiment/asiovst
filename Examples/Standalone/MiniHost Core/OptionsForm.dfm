object Options: TOptions
  Left = 213
  Top = 173
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Settings'
  ClientHeight = 459
  ClientWidth = 321
  Color = clGray
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 169
    Caption = ' ASIO '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 14
      Caption = 'ASIO Driver:'
    end
    object Label2: TLabel
      Left = 8
      Top = 40
      Width = 41
      Height = 14
      Caption = 'Outputs:'
    end
    object Label3: TLabel
      Left = 8
      Top = 88
      Width = 54
      Height = 14
      Caption = 'Buffersize:'
    end
    object Label4: TLabel
      Left = 8
      Top = 112
      Width = 57
      Height = 14
      Caption = 'Samplerate:'
    end
    object Label8: TLabel
      Left = 8
      Top = 64
      Width = 32
      Height = 14
      Caption = 'Inputs:'
    end
    object Label9: TLabel
      Left = 8
      Top = 136
      Width = 39
      Height = 14
      Caption = 'Format: '
    end
    object info: TMemo
      Left = 188
      Top = 40
      Width = 289
      Height = 121
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 1
      Visible = False
    end
    object Button1: TButton
      Left = 256
      Top = 16
      Width = 41
      Height = 17
      Caption = 'Info'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 184
    Width = 305
    Height = 265
    Caption = ' Global Settings '
    TabOrder = 1
    object Label6: TLabel
      Left = 8
      Top = 216
      Width = 76
      Height = 14
      Caption = 'Overall Volume:'
    end
    object Label5: TLabel
      Left = 8
      Top = 72
      Width = 68
      Height = 14
      Caption = 'WAV Volume:'
    end
    object Label7: TLabel
      Left = 8
      Top = 24
      Width = 35
      Height = 14
      Caption = 'Tempo:'
    end
    object Label11: TLabel
      Left = 8
      Top = 120
      Width = 65
      Height = 14
      Caption = 'Input Volume:'
    end
    object Label12: TLabel
      Left = 8
      Top = 168
      Width = 63
      Height = 14
      Caption = 'VST Volume:'
    end
    object ScrollBar3: TScrollBar
      Left = 8
      Top = 40
      Width = 289
      Height = 16
      Max = 250
      Min = 10
      PageSize = 0
      Position = 10
      TabOrder = 0
      OnChange = ScrollBar3Change
    end
    object ScrollBar1: TScrollBar
      Left = 8
      Top = 88
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 1
      OnChange = ScrollBar3Change
    end
    object ScrollBar5: TScrollBar
      Left = 8
      Top = 136
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 2
      OnChange = ScrollBar3Change
    end
    object ScrollBar6: TScrollBar
      Left = 8
      Top = 184
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 3
      OnChange = ScrollBar3Change
    end
    object ScrollBar2: TScrollBar
      Left = 8
      Top = 232
      Width = 289
      Height = 16
      PageSize = 0
      Position = 100
      TabOrder = 4
      OnChange = ScrollBar3Change
    end
  end
end
