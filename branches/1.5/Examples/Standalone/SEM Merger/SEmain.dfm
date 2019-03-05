object FormSEModuleExplorer: TFormSEModuleExplorer
  Left = 446
  Top = 223
  Caption = 'SE Merger'
  ClientHeight = 164
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
  object ListBoxSEMs: TListBox
    Left = 0
    Top = 0
    Width = 229
    Height = 164
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemNew: TMenuItem
        Caption = 'New'
        OnClick = MenuItemNewClick
      end
      object MenuItemOpen: TMenuItem
        Caption = '&Add...'
        OnClick = MenuItemOpenClick
      end
      object MenuItemSaveAs: TMenuItem
        Caption = 'Save as...'
        Enabled = False
        OnClick = MenuItemSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MenuItemExitClick
      end
    end
    object MenuItemSettings: TMenuItem
      Caption = '&Settings'
      object MenuItemAddMerged: TMenuItem
        Caption = 'add '#39'merged'#39' to ID'
        Checked = True
        OnClick = MenuItemAddMergedClick
      end
    end
  end
end
