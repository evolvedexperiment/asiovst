object FmSplashScreen: TFmSplashScreen
  Left = 527
  Top = 313
  BorderStyle = bsNone
  Caption = 'Splash Screen'
  ClientHeight = 85
  ClientWidth = 355
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Border: TShape
    Left = 0
    Top = 0
    Width = 355
    Height = 85
    Brush.Style = bsClear
    Pen.Color = clGray
  end
  object IVST: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object IDUnit: TImage
    Left = 8
    Top = 46
    Width = 32
    Height = 32
  end
  object LbTitle: TGuiLabel
    Left = 56
    Top = 8
    Width = 220
    Height = 26
    Caption = 'VST Plugin Unit Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    Shadow.Blur = 2.000000000000000000
    Shadow.Opacity = 127
    Shadow.Visible = True
    Transparent = True
  end
  object LbVstAbout: TLabel
    Left = 56
    Top = 36
    Width = 291
    Height = 11
    Caption = 
      'VST is a registered trademark of Steinberg Media Technologies Gm' +
      'bH'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbDUnitAbout: TLabel
    Left = 56
    Top = 50
    Width = 265
    Height = 11
    Caption = 'DUnit was used as an Xtreme testing framework similar to JUnit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbScanning: TLabel
    Left = 56
    Top = 67
    Width = 54
    Height = 13
    Caption = 'Scanning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object LbScannedPlugin: TLabel
    Left = 116
    Top = 67
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
end
