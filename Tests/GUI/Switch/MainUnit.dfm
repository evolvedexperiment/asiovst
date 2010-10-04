object FmSwitchTest: TFmSwitchTest
  Left = 299
  Top = 55
  Caption = 'Switch Test'
  ClientHeight = 102
  ClientWidth = 104
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GuiStichedSwitch0: TGuiStichedSwitch
    Left = 8
    Top = 8
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageIndex = -1
    StitchedImageList = GuiStitchedImageList
    OnChange = GuiStichedSwitchChange
  end
  object GuiStichedSwitch1: TGuiStichedSwitch
    Left = 54
    Top = 8
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageIndex = -1
    StitchedImageList = GuiStitchedImageList
    OnChange = GuiStichedSwitchChange
  end
  object GuiStichedSwitch2: TGuiStichedSwitch
    Left = 8
    Top = 54
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageIndex = -1
    StitchedImageList = GuiStitchedImageList
    OnChange = GuiStichedSwitchChange
  end
  object GuiStichedSwitch3: TGuiStichedSwitch
    Left = 54
    Top = 54
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageIndex = -1
    StitchedImageList = GuiStitchedImageList
    OnChange = GuiStichedSwitchChange
  end
  object GuiStitchedImageList: TGuiStitchedImageList
    StitchedImages = <
      item
        DisplayName = 'Switch'
        StitchedPixelMap.Width = 64
        StitchedPixelMap.Height = 64
        GlyphCount = 2
        StitchKind = skVertical
        Height = 64
        Width = 64
      end>
    Left = 32
    Top = 16
  end
end
