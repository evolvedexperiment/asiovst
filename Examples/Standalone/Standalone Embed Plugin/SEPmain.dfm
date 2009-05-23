object FmStandaloneEmbedPlugin: TFmStandaloneEmbedPlugin
  Left = 286
  Top = 77
  Caption = 'Standalone Embed Plugin Tool'
  ClientHeight = 79
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbEmbedPlugin: TGuiLabel
    Left = 8
    Top = 8
    Width = 329
    Height = 65
    AntiAlias = gaaLinear4x
    Caption = 'Embed Plugin'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 32
    object MIFile: TMenuItem
      Caption = '&File'
      object MISelectVstPlugin: TMenuItem
        Caption = 'Select VST Plugin...'
        OnClick = MISelectVstPluginClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
  end
  object OpenDialogVST: TOpenDialog
    DefaultExt = '.DLL'
    Filter = 'VST Plugin (*.dll)|*.dll'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Left = 40
    Top = 32
  end
  object SaveDialogStandalone: TSaveDialog
    DefaultExt = '.EXE'
    Filter = 'Standalone (*.exe)|*.exe'
    Left = 72
    Top = 32
  end
end
