object FmSetup: TFmSetup
  Left = 454
  Top = 379
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 66
  ClientWidth = 398
  Color = 8620693
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    398
    66)
  PixelsPerInch = 96
  TextHeight = 14
  object LbPreset: TGuiLabel
    Left = 8
    Top = 13
    Width = 67
    Height = 13
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    Caption = 'ASIO Driver:'
    Transparent = True
    Shadow.Color = clBlack
  end
  object SbDrivers: TGuiSelectBox
    Left = 81
    Top = 8
    Width = 217
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    ArrowColor = clBlack
    ButtonColor = 8620693
    ItemIndex = -1
    LineColor = clBlack
    Radius = 5
    SelectBoxColor = 10333885
    OnChange = SbDriversChange
  end
  object LbOutputChannels: TGuiLabel
    Left = 8
    Top = 41
    Width = 92
    Height = 13
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    Caption = 'Output Channels:'
    Transparent = True
    Shadow.Color = clBlack
  end
  object SbChannels: TGuiSelectBox
    Left = 106
    Top = 36
    Width = 284
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    ArrowColor = clBlack
    ButtonColor = 8620693
    ItemIndex = -1
    LineColor = clBlack
    Radius = 5
    SelectBoxColor = 10333885
    OnChange = SbChannelsChange
  end
  object PnControlPanel: TGuiPanel
    Left = 304
    Top = 8
    Width = 86
    Height = 22
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 0
    UseDockManager = True
    OnClick = LbControlPanelClick
    DesignSize = (
      86
      22)
    object LbControlPanel: TGuiLabel
      Left = 6
      Top = 5
      Width = 75
      Height = 13
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'Control Panel'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbControlPanelClick
    end
  end
end
