object FormSEModuleExplorer: TFormSEModuleExplorer
  Left = 188
  Top = 77
  Caption = 'SE Module Explorer'
  ClientHeight = 439
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 469
    Height = 439
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MenuItemOpenClick
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
      object MenuItemEnableWrapper: TMenuItem
        Caption = 'Enable fixing SEM'
        OnClick = MenuItemEnableWrapperClick
      end
    end
    object MenuItemHelp: TMenuItem
      Caption = '&Help'
      object MenuItemAbout: TMenuItem
        Caption = '&About'
        OnClick = MenuItemAboutClick
      end
    end
  end
end
