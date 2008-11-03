object FmSEModuleExplorer: TFmSEModuleExplorer
  Left = 446
  Top = 223
  Caption = 'SE Merger'
  ClientHeight = 126
  ClientWidth = 229
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
  object LBSEMs: TListBox
    Left = 0
    Top = 0
    Width = 229
    Height = 126
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MIFile: TMenuItem
      Caption = '&File'
      object MINew: TMenuItem
        Caption = 'New'
        OnClick = MINewClick
      end
      object MIOpen: TMenuItem
        Caption = '&Add...'
        OnClick = MIOpenClick
      end
      object MISaveAs: TMenuItem
        Caption = 'Save as...'
        Enabled = False
        OnClick = MISaveAsClick
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
end
