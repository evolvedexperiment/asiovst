object FmSwitchTest: TFmSwitchTest
  Left = 329
  Top = 93
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
    StitchedImageList = GuiStitchedImageList
    StitchedImageIndex = 0
    OnChange = GuiStichedSwitchChange
  end
  object GuiStichedSwitch1: TGuiStichedSwitch
    Left = 54
    Top = 8
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageList = GuiStitchedPNGList
    StitchedImageIndex = 0
    OnChange = GuiStichedSwitchChange
  end
  object GuiStichedSwitch2: TGuiStichedSwitch
    Left = 8
    Top = 54
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageList = GuiStitchedImageList
    StitchedImageIndex = 0
  end
  object GuiStichedSwitch3: TGuiStichedSwitch
    Left = 54
    Top = 54
    Width = 40
    Height = 40
    DefaultGlyphIndex = 0
    GlyphIndex = 0
    StitchedImageList = GuiStitchedPNGList
    StitchedImageIndex = 0
  end
  object GuiStitchedImageList: TGuiStitchedImageList
    StitchedImages = <
      item
        DisplayName = 'Switch'
        StitchedPixelMap.Width = 40
        StitchedPixelMap.Height = 80
        StitchedPixelMap.Data = {
          2800000050000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3C026969692C
          6A6A6A755C5C5CF04D4D4DFF3F3F3FFF3A3A3AFF3B3B3BFF3B3B3BFF3B3B3BFF
          3B3B3BFF3B3B3BFF3C3C3CFF3C3C3CFF3C3C3CFF3C3C3CFF3D3D3DFF3D3D3DFF
          3E3E3EFF3D3D3DFF3E3E3EFF3D3D3DFF3D3D3DFF3E3E3EFF3D3D3DFF3E3E3EFF
          393939FF2F2F2FFF232323F0151515750C0C0C2C00000002FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF006969692C6B6B6BBC
          595959F9484848FF383838FF2E2E2EFF2A2A2AFF292929FF2A2A2AFF2A2A2AFF
          292929FF2A2A2AFF2A2A2AFF2A2A2AFF2A2A2AFF2B2B2BFF2A2A2AFF2B2B2BFF
          2B2B2BFF2B2B2BFF2A2A2AFF2A2A2AFF2A2A2AFF2A2A2AFF2B2B2BFF2A2A2AFF
          282828FF212121FF181818FF0B0B0BF9010101BC0000002CFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF006A6A6A75595959F9
          444444FF323232FF232323FF1B1B1BFF191919FF191919FF191919FF181818FF
          181818FF181818FF181818FF181818FF181818FF181818FF181818FF181818FF
          181818FF181818FF171717FF171717FF171717FF171717FF171717FF171717FF
          161616FF151515FF0F0F0FFF080808FF030303F900000075FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF005C5C5CF0484848FF
          323232FF1D1D1DFF151515FF131313FF131313FF131313FF121212FF121212FF
          121212FF121212FF121212FF121212FF111111FF121212FF111111FF111111FF
          111111FF101010FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF
          101010FF0F0F0FFF0D0D0DFF090909FF050505FF020202F0FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004D4D4DFF383838FF
          232323FF151515FF121212FF131313FF121212FF121212FF121212FF111111FF
          121212FF111111FF111111FF111111FF111111FF111111FF111111FF101010FF
          101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          0F0F0FFF0F0F0FFF0E0E0EFF0A0A0AFF070707FF030303FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003F3F3FFF2E2E2EFF
          1B1B1BFF131313FF131313FF121212FF121212FF121212FF111111FF121212FF
          111111FF121212FF111111FF111111FF111111FF111111FF101010FF101010FF
          101010FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0F0F0FFF
          0F0F0FFF0E0E0EFF0D0D0DFF0C0C0CFF080808FF050505FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003A3A3AFF2A2A2AFF
          191919FF131313FF121212FF121212FF121212FF121212FF121212FF111111FF
          121212FF111111FF111111FF111111FF111111FF111111FF101010FF101010FF
          101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          0E0E0EFF0E0E0EFF0E0E0EFF0B0B0BFF080808FF040404FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003B3B3BFF292929FF
          191919FF131313FF121212FF121212FF121212FF121212FF111111FF121212FF
          111111FF121212FF111111FF111111FF111111FF101010FF101010FF101010FF
          101010FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0B0B0BFF080808FF050505FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003B3B3BFF2A2A2AFF
          191919FF121212FF121212FF111111FF121212FF111111FF111111FF121212FF
          121212FF121212FF121212FF111111FF111111FF111111FF101010FF111111FF
          101010FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0C0C0CFF0A0A0AFF060606FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003B3B3BFF2A2A2AFF
          181818FF121212FF111111FF121212FF111111FF121212FF121212FF121212FF
          121212FF121212FF111111FF111111FF111111FF111111FF111111FF101010FF
          111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0D0D0DFF090909FF070707FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003B3B3BFF292929FF
          181818FF121212FF121212FF111111FF121212FF111111FF121212FF121212FF
          121212FF121212FF111111FF111111FF111111FF111111FF101010FF111111FF
          101010FF111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0C0C0CFF0A0A0AFF070707FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003B3B3BFF2A2A2AFF
          181818FF121212FF111111FF121212FF111111FF121212FF121212FF121212FF
          121212FF111111FF111111FF111111FF111111FF111111FF111111FF101010FF
          111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0D0D0DFF0C0C0CFF090909FF060606FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3CFF2A2A2AFF
          181818FF121212FF111111FF111111FF111111FF111111FF121212FF111111FF
          111111FF111111FF101010FF111111FF101010FF111111FF101010FF101010FF
          101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0E0E0EFF0C0C0CFF090909FF070707FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3CFF2A2A2AFF
          181818FF121212FF111111FF111111FF111111FF111111FF111111FF111111FF
          111111FF111111FF111111FF101010FF111111FF101010FF101010FF101010FF
          101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0B0B0BFF090909FF070707FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3CFF2A2A2AFF
          181818FF111111FF111111FF111111FF111111FF111111FF111111FF111111FF
          111111FF111111FF101010FF111111FF101010FF111111FF101010FF101010FF
          0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0C0C0CFF090909FF080808FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3CFF2B2B2BFF
          181818FF121212FF111111FF111111FF111111FF101010FF111111FF111111FF
          111111FF111111FF111111FF101010FF111111FF101010FF101010FF0F0F0FFF
          0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0D0D0DFF0D0D0DFF0B0B0BFF0A0A0AFF080808FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2A2A2AFF
          181818FF111111FF111111FF101010FF101010FF101010FF101010FF111111FF
          101010FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0C0C0CFF090909FF080808FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2B2B2BFF
          181818FF111111FF101010FF101010FF101010FF101010FF111111FF101010FF
          111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0B0B0BFF0A0A0AFF080808FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003E3E3EFF2B2B2BFF
          181818FF111111FF101010FF101010FF101010FF101010FF101010FF111111FF
          101010FF111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0B0B0BFF090909FF090909FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2B2B2BFF
          181818FF101010FF101010FF101010FF101010FF101010FF111111FF101010FF
          111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0B0B0BFF0A0A0AFF090909FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003E3E3EFF2A2A2AFF
          171717FF111111FF0F0F0FFF101010FF0F0F0FFF101010FF101010FF101010FF
          101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0D0D0DFF0C0C0CFF0B0B0BFF0A0A0AFF0A0A0AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2A2A2AFF
          171717FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF101010FF
          101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF
          0D0D0DFF0C0C0CFF0C0C0CFF0B0B0BFF0B0B0BFF090909FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2A2A2AFF
          171717FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF101010FF101010FF
          0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0D0D0DFF0C0C0CFF0B0B0BFF0A0A0AFF090909FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003E3E3EFF2A2A2AFF
          171717FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF
          0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF
          0D0D0DFF0C0C0CFF0C0C0CFF0B0B0BFF0A0A0AFF0A0A0AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003D3D3DFF2B2B2BFF
          171717FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF
          0C0C0CFF0C0C0CFF0B0B0BFF0A0A0AFF0A0A0AFF0A0A0AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003E3E3EFF2A2A2AFF
          171717FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF0C0C0CFF
          0C0C0CFF0C0C0CFF0B0B0BFF0A0A0AFF0A0A0AFF0B0B0BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00393939FF282828FF
          161616FF101010FF0F0F0FFF0F0F0FFF0E0E0EFF0E0E0EFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF
          0C0C0CFF0B0B0BFF0B0B0BFF0A0A0AFF0B0B0BFF0B0B0BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF002F2F2FFF212121FF
          151515FF0F0F0FFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF0C0C0CFF
          0B0B0BFF0C0C0CFF0A0A0AFF0B0B0BFF0B0B0BFF0C0C0CFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00232323F0181818FF
          0F0F0FFF0D0D0DFF0E0E0EFF0D0D0DFF0E0E0EFF0D0D0DFF0E0E0EFF0E0E0EFF
          0E0E0EFF0D0D0DFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0B0B0BFF0B0B0BFF
          0B0B0BFF0A0A0AFF0B0B0BFF0B0B0BFF0C0C0CFF0C0C0CEFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00151515750B0B0BF9
          080808FF090909FF0A0A0AFF0C0C0CFF0B0B0BFF0B0B0BFF0C0C0CFF0D0D0DFF
          0C0C0CFF0C0C0CFF0C0C0CFF0B0B0BFF0C0C0CFF0B0B0BFF0C0C0CFF0B0B0BFF
          0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0A0A0AFF0A0A0AFF
          0A0A0AFF0B0B0BFF0B0B0BFF0C0C0CFF0C0C0CF80D0D0D72FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000404042C010101BC
          030303F9050505FF070707FF080808FF080808FF080808FF0A0A0AFF090909FF
          0A0A0AFF090909FF090909FF090909FF090909FF0A0A0AFF090909FF0A0A0AFF
          090909FF0A0A0AFF0A0A0AFF0B0B0BFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF
          0B0B0BFF0B0B0BFF0C0C0CFF0C0C0CF80D0D0DBA0D0D0D27FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000020000002C
          00000075020202F0030303FF050505FF040404FF050505FF060606FF070707FF
          070707FF060606FF070707FF070707FF080808FF080808FF080808FF080808FF
          090909FF090909FF0A0A0AFF090909FF090909FF0A0A0AFF0A0A0AFF0B0B0BFF
          0B0B0BFF0C0C0CFF0C0C0CEF0D0D0D720D0D0D270D0D0D01FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF001818180215151530
          0B0B0B78000000F0000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000F0000000780303033000000002FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0015151530050505BD
          000000F9000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FF000000F9010101BD07070730FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000B0B0B78000000F9
          050505FF121212FF171717FF1A1A1AFF181818FF181818FF181818FF181818FF
          181818FF181818FF181818FF181818FF181818FF181818FF181818FF181818FF
          181818FF181818FF171717FF171717FF171717FF171717FF171717FF171717FF
          161616FF151515FF0E0E0EFF080808FF202020F920202078FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000F0000000FF
          121212FF171717FF151515FF131313FF131313FF131313FF121212FF121212FF
          121212FF121212FF121212FF121212FF111111FF121212FF111111FF111111FF
          111111FF101010FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF
          101010FF0F0F0FFF0D0D0DFF0A0A0AFF313131FF3F3F3FF0FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF151515FF121212FF131313FF121212FF121212FF121212FF111111FF
          121212FF111111FF111111FF111111FF111111FF111111FF111111FF101010FF
          101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          0F0F0FFF0F0F0FFF0E0E0EFF0A0A0AFF383838FF4B4B4BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          1A1A1AFF131313FF131313FF121212FF121212FF121212FF111111FF121212FF
          111111FF121212FF111111FF111111FF111111FF111111FF101010FF101010FF
          101010FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0F0F0FFF
          0F0F0FFF0E0E0EFF0D0D0DFF0C0C0CFF3C3C3CFF535353FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF131313FF121212FF121212FF121212FF121212FF121212FF111111FF
          121212FF111111FF111111FF111111FF111111FF111111FF101010FF101010FF
          101010FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          0E0E0EFF0E0E0EFF0E0E0EFF0B0B0BFF3D3D3DFF545454FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF131313FF121212FF121212FF121212FF121212FF111111FF121212FF
          111111FF121212FF111111FF111111FF111111FF101010FF101010FF101010FF
          101010FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0B0B0BFF3E3E3EFF555555FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF121212FF111111FF121212FF111111FF111111FF111111FF
          111111FF111111FF111111FF101010FF101010FF101010FF0F0F0FFF101010FF
          0F0F0FFF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0D0D0DFF
          0E0E0EFF0D0D0DFF0D0D0DFF0B0B0BFF3F3F3FFF565656FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF111111FF121212FF111111FF121212FF111111FF111111FF
          111111FF111111FF101010FF101010FF101010FF101010FF101010FF0F0F0FFF
          101010FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0D0D0DFF0E0E0EFF
          0D0D0DFF0E0E0EFF0D0D0DFF0C0C0CFF3E3E3EFF575757FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF121212FF111111FF121212FF111111FF111111FF111111FF
          121212FF121212FF111111FF111111FF111111FF111111FF101010FF111111FF
          101010FF111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0C0C0CFF404040FF585858FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF111111FF121212FF111111FF121212FF111111FF111111FF
          121212FF111111FF111111FF111111FF111111FF111111FF111111FF101010FF
          111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0D0D0DFF0C0C0CFF3F3F3FFF575757FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF111111FF111111FF111111FF111111FF111111FF101010FF
          111111FF111111FF101010FF111111FF101010FF111111FF101010FF101010FF
          101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0E0E0EFF0C0C0CFF3F3F3FFF585858FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF111111FF111111FF111111FF111111FF101010FF101010FF
          111111FF111111FF111111FF101010FF111111FF101010FF101010FF101010FF
          101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0B0B0BFF3F3F3FFF585858FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF111111FF111111FF111111FF111111FF111111FF101010FF101010FF
          111111FF111111FF101010FF111111FF101010FF111111FF101010FF101010FF
          0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0C0C0CFF3F3F3FFF595959FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF121212FF111111FF111111FF111111FF101010FF101010FF101010FF
          111111FF111111FF111111FF101010FF111111FF101010FF101010FF0F0F0FFF
          0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0D0D0DFF0D0D0DFF0B0B0BFF404040FF595959FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF111111FF111111FF101010FF101010FF101010FF0F0F0FFF101010FF
          101010FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0C0C0CFF3F3F3FFF595959FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF111111FF101010FF101010FF101010FF101010FF101010FF0F0F0FFF
          111111FF101010FF101010FF101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0B0B0BFF404040FF595959FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF111111FF101010FF101010FF101010FF101010FF0F0F0FFF101010FF
          101010FF111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0B0B0BFF3F3F3FFF5A5A5AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          181818FF101010FF101010FF101010FF101010FF101010FF101010FF0F0F0FFF
          111111FF101010FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0B0B0BFF404040FF5A5A5AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF111111FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          101010FF101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0D0D0DFF0C0C0CFF0B0B0BFF404040FF5B5B5BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0F0F0FFF
          101010FF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF
          0D0D0DFF0C0C0CFF0C0C0CFF0B0B0BFF414141FF5A5A5AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF101010FF0F0F0FFF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF
          0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF
          0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0D0D0DFF0C0C0CFF0B0B0BFF404040FF5A5A5AFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF101010FF101010FF0F0F0FFF101010FF0F0F0FFF0F0F0FFF0E0E0EFF
          0F0F0FFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF
          0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF
          0D0D0DFF0C0C0CFF0C0C0CFF0B0B0BFF404040FF5B5B5BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF101010FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0D0D0DFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF
          0C0C0CFF0C0C0CFF0B0B0BFF0A0A0AFF404040FF5B5B5BFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          171717FF0F0F0FFF0F0F0FFF0F0F0FFF0F0F0FFF0E0E0EFF0D0D0DFF0E0E0EFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF0C0C0CFF
          0C0C0CFF0C0C0CFF0B0B0BFF0A0A0AFF404040FF5C5C5CFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          161616FF101010FF0F0F0FFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF
          0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF
          0C0C0CFF0B0B0BFF0B0B0BFF0A0A0AFF414141FF5C5C5CFFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
          151515FF0F0F0FFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0E0E0EFF
          0E0E0EFF0F0F0FFF0E0E0EFF0E0E0EFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF
          0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF0C0C0CFF0C0C0CFF0C0C0CFF
          0B0B0BFF0C0C0CFF0A0A0AFF171717FF4A4A4AFF636363FFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000F0000000FF
          0E0E0EFF0D0D0DFF0E0E0EFF0D0D0DFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF
          0E0E0EFF0D0D0DFF0E0E0EFF0D0D0DFF0D0D0DFF0D0D0DFF0C0C0CFF0D0D0DFF
          0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0C0C0CFF0B0B0BFF0B0B0BFF
          0B0B0BFF0A0A0AFF111111FF2B2B2BFF595959FF6A6A6AEFFFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000078000000F9
          080808FF0A0A0AFF0A0A0AFF0C0C0CFF0B0B0BFF0B0B0BFF0B0B0BFF0C0C0CFF
          0C0C0CFF0C0C0CFF0C0C0CFF0B0B0BFF0C0C0CFF0B0B0BFF0C0C0CFF0B0B0BFF
          0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0B0B0BFF0A0A0AFF0A0A0AFF
          0A0A0AFF171717FF2B2B2BFF4C4C4CFF6B6B6BF870707072FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000030010101BD
          202020F9313131FF383838FF3C3C3CFF3D3D3DFF3E3E3EFF3F3F3FFF3E3E3EFF
          404040FF3F3F3FFF3F3F3FFF3F3F3FFF3F3F3FFF404040FF3F3F3FFF404040FF
          3F3F3FFF404040FF404040FF414141FF404040FF404040FF404040FF404040FF
          414141FF4A4A4AFF595959FF6B6B6BF8767676BA6A6A6A27FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000207070730
          202020783F3F3FF04B4B4BFF535353FF545454FF555555FF565656FF575757FF
          585858FF575757FF585858FF585858FF595959FF595959FF595959FF595959FF
          5A5A5AFF5A5A5AFF5B5B5BFF5A5A5AFF5A5A5AFF5B5B5BFF5B5B5BFF5C5C5CFF
          5C5C5CFF636363FF6A6A6AEF707070726A6A6A2755555501FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
          FFFFFF00FFFFFF00}
        GlyphCount = 2
        StitchKind = skVertical
        Height = 80
        Width = 40
      end>
    Left = 40
    Top = 8
  end
  object GuiStitchedPNGList: TGuiStitchedPNGList
    StitchedPNGs = <
      item
        PortableNetworkGraphic.Data = {
          89504E470D0A1A0A0000000D49484452000000280000005008060000006B8F37
          C10000000467414D410000B18F0BFC6105000000206348524D00007A26000080
          840000FA00000080E8000075300000EA6000003A98000017709CBA513C000000
          017352474200AECE1CE90000066A494441546843ED9AC74F1C4B10871F0B989C
          451612888349229DC841E280082772B871022EE84920FE36CBFE9BCCD19CFAF5
          B77259E5A67BBA6716104FC252A96797F5ECB7BFAAEA503555C6987F3EF43F00
          3FB27D68B8B2773FB27A5EC0959595D2F5F5F5D4CDCDCDFDE5E5E5D3FEFEBE59
          5F5F374B4B4B667979F985D9CF1B6DABABAB065B5B5BFB6B74DF5B5C5C343333
          33667474F4A9BBBBFBBEB9B979CAE642C915EC8582C0DDDEDE7E3F3B3B7BDEDE
          DE360B0B0B667A7ADA4C4C4C98F1F1F1F2E85ECB7B939393E5BF318AC9DFDCCF
          8C8D8D99919111D3D7D7679A9A9A9EABAAAABE5BC0A92820CA01B7B5B565E6E7
          E7F985667070D0F4F7F7FF316E9A6ABDBDBDC6673D3D3DC62A67DADADA4C7D7D
          BDA9AEAE7EB680F75140DC8A72C00D0D0D956FD2D5D5F5C73A3B3B8DCF3A3A3A
          CAEF338AB5B7B797AF19B501C56BC6969616D3D0D0606A6B6B4DA9547A8A0212
          73B815E580E34B01D4503E18174C80348C40F19E586B6BAB696C6C34757575A8
          68F9FE9EF65EC4200941CCE156512E04E72AE6534BA04270286813A4EC66548C
          0292AD043431E72A27A0590A66B95343A29C984D9232604D4D4D1C90A9846C0D
          016A385FCCB9803E05351C0A0A609282008A8221C5DC24092546284100240605
          141713875FBE7C49531040A61151488F597029D9ABE1045232994489C6A0B858
          0063905A3D51CC1DDD040929980C98AAA00B1752D09D56443999077131736192
          8B59575D400D923519A7C69C4E12AE053049C118A04FB5AC15C31773BE2C46C1
          DC802117A6646DC8ADAE7A325103C85C184D12146427A2B3D88DADACC4C84A08
          3DB5000A9CCC834C334980ECDB240635989E807D59AAD75E374B7DAA091CF127
          31980C280AFA823E0F9CAB98564D00732BC84E380B502784FE0179624EC3A11E
          4B1D3188456310170BA06F5DCD33098762CE07480C2601A22031C82E1818773F
          E7DBADE48D39C95C195110402C4941010C6D3A4309911A73B855E0C4C5E2E6DC
          8021052B89B99082B95D9CB26DCF5A2974ACB971E72A98CBC5240931E8DBA6EB
          85DE9DDFE475084643C9FCA7E32F290675926837EAF8CA82F4A9E6C69CC049FC
          897AC0466390133F876ACEADBE33444C411750C3B90A02240ACA751490720427
          7E3954FB323304991A73A29C6CF5058ED751406A256C14643329CB931E75ACB9
          CB57963BF594E22AF7DBED3FA3801472A895B07093F6C487BEB10F802FD3AA88
          22BE51E28D512B67FFFF2FEB817FA380F66C3A4521875A09C7403691C08AC992
          24A3ACA1F263DC5140345800EE9B05FC1A05A404469589420EB512CA118072A8
          66E4DC0034A6AFF58F10F535AC5C7BE07EA21C70D6AAA2801FADA0F9FFABB07E
          2A98B3ABF0E9E24A43E653C15757D0AEC325BBDCCDDA65E8D14ED64FD668E6BD
          A5F11D8F7641984DEA930067578C1FF6C3B405DE124CDF9B3EC90FBB3ACD4657
          92DFCA95D7612A59ECAC07060692FB226EFF24D6276137C4D2383C3CFC6CED31
          0A286E15B8F7E893B03ECFCDCDD1728BF749C4ADFCF2F7EC93D09BD9DDDD8D6F
          580510B7BE679F84AADAC1C1413A20B1F49E7D124A2E8787879503BE559F84D3
          E4D1D1513EC094CE925BA24BE9CD71B8720B98B4E08E8F8FF303662956B437E7
          1EC438CFA0E0C9C9497E40B74F225557AD6EA5356B00373636CCE9E9697EC090
          82BE4E67257D125C5C48C1ACB6435667294FCD1A05012CACA0EB42290317EDCD
          F9FA24850043EA69C0D7A8598B82F659896231E86B478412A3689F8424C90DE8
          0393792EB5FDE5AB21BAF5C3C20A66CD73298DC3D49A35359AC20A869E417015
          ACA4660D2049727E7E9E2F067D7D1289B194264E9E3E090AE6020CF54944A93C
          F35CAC662D2EBEB8B84857508AE8A1E75EE4FD947E70565958EA8A9B9B9BF915
          74E18A54FD536AD6A2606E17FBFA24BA3EEDD6AA5362CE6DE28882C4602117FB
          5A117C896F0AA9A44F520830042720A9F35C4A9F0440FBE45D7A9284FA24B2D9
          0C29981273BACB24D57E92241760A84F22DB755F0BB6689F8419636767C75C5D
          5D252958AEC7F8FA231AC0FDBBFC2D6F9F84099FDE201B05FBF467BC4F422187
          BA0CE508691DB80D18DD6B73FB23EE67DD5E896E47A01C707B7B7BC63E37FBEB
          EEEE2EDE27B170B31472A895508EE0C4CFA15A9EE265CDE480435073AD4DDED3
          23D7C417A336DEC3AD2807DCC3C3C337AB605A9F842A13851C6A25942338F173
          A8E6DCCAD190B303DB7319B9E68BB431E962CC6DFA9AD7240346CCE1569403CE
          DEFFB34FF2EA25E557BF61A525DF687DF0B5BFA0D2FBFD07E48BB1F4D9DCDFBB
          0000000049454E44AE426082}
        DisplayName = 'PNG Switch'
        GlyphCount = 2
        StitchKind = skVertical
        Height = 80
        Width = 40
      end>
    Left = 40
    Top = 56
  end
end
