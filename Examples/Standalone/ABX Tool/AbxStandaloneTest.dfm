object FmABXStandaloneTest: TFmABXStandaloneTest
  Left = 218
  Top = 77
  Caption = 'ABX Test'
  ClientHeight = 388
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    396
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object LbB: TLabel
    Left = 268
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'B'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbBClick
  end
  object LbX: TLabel
    Left = 138
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'X'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbXClick
  end
  object LbA: TLabel
    Left = 8
    Top = 8
    Width = 120
    Height = 161
    Alignment = taCenter
    AutoSize = False
    Caption = 'A'
    Color = clGrayText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -133
    Font.Name = 'Verdana'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    OnClick = LbAClick
  end
  object BtXisA: TButton
    Tag = 1
    Left = 103
    Top = 184
    Width = 75
    Height = 34
    Caption = 'X is A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = BtXisAClick
  end
  object BtXisB: TButton
    Tag = 2
    Left = 215
    Top = 184
    Width = 75
    Height = 34
    Caption = 'X is B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = BtXisBClick
  end
  object BtAudioStop: TButton
    Left = 199
    Top = 224
    Width = 51
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 2
    OnClick = BtAudioStopClick
  end
  object BtAudioPlay: TButton
    Left = 143
    Top = 224
    Width = 51
    Height = 25
    Caption = 'Play'
    TabOrder = 3
    OnClick = BtAudioPlayClick
  end
  object NotesBox: TGroupBox
    Left = 8
    Top = 264
    Width = 380
    Height = 99
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Comments '
    TabOrder = 4
    object Notes: TMemo
      Left = 2
      Top = 15
      Width = 376
      Height = 82
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 369
    Width = 396
    Height = 19
    Panels = <>
  end
  object BtAudioSetup: TButton
    Left = 142
    Top = 224
    Width = 108
    Height = 25
    Caption = 'Audio Setup'
    TabOrder = 6
    OnClick = BtAudioSetupClick
  end
  object ASIOHost: TASIOHost
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    ConvertOptimizations = [coSSE, co3DNow]
    SampleRate = 44100.000000000000000000
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    Left = 16
    Top = 16
  end
  object AdcA: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 48
    Top = 16
  end
  object AdcB: TAudioDataCollection32
    Channels = <>
    SampleRate = 44100.000000000000000000
    Left = 80
    Top = 16
  end
  object Database: TkbmMemTable
    Active = True
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <
      item
        Name = 'Date'
        DataType = ftDate
      end
      item
        Name = 'Name/ID'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Rating'
        DataType = ftFloat
      end
      item
        Name = 'Trials'
        DataType = ftInteger
      end>
    IndexDefs = <>
    SortOptions = []
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    LoadedCompletely = False
    SavedCompletely = False
    FilterOptions = []
    Version = '5.52'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    DefaultFormat = kbmCSVStreamFormat
    CommaTextFormat = kbmCSVStreamFormat
    PersistentFormat = kbmCSVStreamFormat
    AllDataFormat = kbmCSVStreamFormat
    FormFormat = kbmCSVStreamFormat
    Left = 112
    Top = 16
    object DatabaseDate: TDateField
      FieldName = 'Date'
    end
    object DatabaseNameID: TStringField
      FieldName = 'Name/ID'
    end
    object DatabaseFieldRating: TFloatField
      FieldName = 'Rating'
    end
    object DatabaseTrials: TIntegerField
      FieldName = 'Trials'
    end
  end
  object kbmCSVStreamFormat: TkbmCSVStreamFormat
    CSVQuote = '"'
    CSVFieldDelimiter = ';'
    CSVRecordDelimiter = ';'
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    sfLocalFormat = []
    sfQuoteOnlyStrings = []
    sfNoHeader = []
    Version = '3.00'
    sfData = [sfSaveData, sfLoadData]
    sfCalculated = []
    sfLookup = []
    sfNonVisible = []
    sfBlobs = []
    sfDef = []
    sfIndexDef = []
    sfPlaceHolders = []
    sfFiltered = []
    sfIgnoreRange = [sfSaveIgnoreRange]
    sfIgnoreMasterDetail = [sfSaveIgnoreMasterDetail]
    sfDeltas = []
    sfDontFilterDeltas = []
    sfAppend = []
    sfFieldKind = []
    sfFromStart = []
    Left = 144
    Top = 16
  end
end
