object FmCantabileLite: TFmCantabileLite
  Left = 299
  Top = 55
  Caption = 'Cantabile Lite'
  ClientHeight = 595
  ClientWidth = 1030
  Color = 3881787
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RibbonControl: TRibbon
    Left = 0
    Top = 0
    Width = 1030
    Height = 143
    ActionManager = ActionManager
    ApplicationMenu.Caption = 'Recent Setups'
    ApplicationMenu.Menu = RibbonApplicationMenuBar
    Caption = 'RibbonControl'
    QuickAccessToolbar.ActionBar = RibbonQuickAccessToolbar
    Tabs = <
      item
        Caption = '&Home'
        Page = RpHome
      end
      item
        Caption = '&Setup'
        Page = RpSetup
      end>
    TabIndex = 1
    ExplicitLeft = 520
    ExplicitTop = 240
    ExplicitWidth = 0
    DesignSize = (
      1030
      143)
    StyleName = 'Ribbon - Obsidian'
    object RibbonApplicationMenuBar: TRibbonApplicationMenuBar
      ActionManager = ActionManager
      OptionItems = <
        item
          Action = AcFileExit
          Caption = 'E&xit'
          Hint = 'Exit|Close the application'
          Tag = 0
        end>
      RecentItems = <>
    end
    object RibbonQuickAccessToolbar: TRibbonQuickAccessToolbar
      Left = 49
      Top = 1
      Width = 48
      Height = 24
      ActionManager = ActionManager
    end
    object RpHome: TRibbonPage
      Left = 0
      Top = 50
      Width = 1029
      Height = 93
      Caption = '&Home'
      Index = 0
      object RgClipboard: TRibbonGroup
        Left = 4
        Top = 3
        Width = 90
        Height = 86
        ActionManager = ActionManager
        Caption = 'Clipboard'
        GroupIndex = 0
      end
      object RgMasterLevels: TRibbonGroup
        Left = 96
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'Master Levels'
        GroupIndex = 1
      end
      object RgTransport: TRibbonGroup
        Left = 198
        Top = 3
        Width = 105
        Height = 86
        ActionManager = ActionManager
        Caption = 'Transport'
        GroupIndex = 2
      end
      object RgMetronome: TRibbonGroup
        Left = 305
        Top = 3
        Width = 48
        Height = 86
        ActionManager = ActionManager
        Caption = 'Metronome'
        GroupIndex = 3
      end
      object RgMisc: TRibbonGroup
        Left = 355
        Top = 3
        Width = 48
        Height = 86
        ActionManager = ActionManager
        Caption = 'Misc.'
        GroupIndex = 4
      end
      object RgView: TRibbonGroup
        Left = 405
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'View'
        GroupIndex = 5
      end
    end
    object RpSetup: TRibbonPage
      Left = 0
      Top = 50
      Width = 1029
      Height = 93
      Caption = '&Setup'
      Index = 1
      object RgAudio: TRibbonGroup
        Left = 4
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'Audio'
        GroupIndex = 0
      end
      object RgMIDI: TRibbonGroup
        Left = 106
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'MIDI'
        GroupIndex = 1
      end
      object RgPlugins: TRibbonGroup
        Left = 208
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'Plugins'
        GroupIndex = 2
      end
      object RgWindows: TRibbonGroup
        Left = 310
        Top = 3
        Width = 100
        Height = 86
        ActionManager = ActionManager
        Caption = 'Windows'
        GroupIndex = 3
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 576
    Width = 1030
    Height = 19
    Panels = <>
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            ChangesAllowed = [caModify]
            Items = <
              item
                Action = AcFileOpen
                ImageIndex = 7
                ShortCut = 16463
              end>
            Caption = '&ActionClientItem0'
            KeyTip = 'F'
          end>
        ActionBar = RibbonApplicationMenuBar
        AutoSize = False
      end
      item
        ActionBar = RibbonQuickAccessToolbar
        AutoSize = False
      end
      item
        ActionBar = RgTransport
      end
      item
        Items = <
          item
            Action = AcEditPaste
            Default = True
            ImageIndex = 2
            ShortCut = 16470
            CommandProperties.ButtonSize = bsLarge
          end
          item
            Action = AcEditCut
            Caption = 'C&ut'
            ImageIndex = 0
            ShortCut = 16472
          end
          item
            Action = AcEditDelete
            ImageIndex = 5
            ShortCut = 46
          end
          item
            Action = AcEditCopy
            ImageIndex = 1
            ShortCut = 16451
          end>
        ActionBar = RgClipboard
      end>
    Left = 688
    Top = 176
    StyleName = 'Ribbon - Obsidian'
    object AcFileOpen: TFileOpen
      Category = 'File'
      Caption = '&Open'
      Hint = 'Open|Open an existing file'
      ImageIndex = 7
      ShortCut = 16463
    end
    object AcEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Paste the content of the clipboard'
      ImageIndex = 2
      ShortCut = 16470
    end
    object AcEditCut: TEditCut
      Category = 'Edit'
      Caption = '&Cut'
      Hint = 'Cut|Cuts the current object to the clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object AcEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the current object to the clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object AcEditDelete: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Deletes the selection'
      ImageIndex = 5
      ShortCut = 46
    end
    object AcFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Close the application'
      ImageIndex = 43
    end
  end
  object XPManifest: TXPManifest
    Left = 608
    Top = 176
  end
  object ShellResources: TShellResources
    Left = 776
    Top = 176
  end
end
