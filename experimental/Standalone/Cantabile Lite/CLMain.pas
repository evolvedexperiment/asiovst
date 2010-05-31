unit CLMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnCtrls, Ribbon, ActnList, StdActns, ToolWin, ActnMan, ActnMenus,
  RibbonActnMenus, RibbonLunaStyleActnCtrls, RibbonObsidianStyleActnCtrls,
  ComCtrls, ShellAnimations, XPMan;

type
  TFmCantabileLite = class(TForm)
    RibbonControl: TRibbon;
    ActionManager: TActionManager;
    RpHome: TRibbonPage;
    RibbonApplicationMenuBar: TRibbonApplicationMenuBar;
    AcFileOpen: TFileOpen;
    RibbonQuickAccessToolbar: TRibbonQuickAccessToolbar;
    RpSetup: TRibbonPage;
    RgClipboard: TRibbonGroup;
    RgTransport: TRibbonGroup;
    RgMetronome: TRibbonGroup;
    RgMisc: TRibbonGroup;
    RgView: TRibbonGroup;
    RgMasterLevels: TRibbonGroup;
    StatusBar: TStatusBar;
    AcEditPaste: TEditPaste;
    AcEditCut: TEditCut;
    AcEditCopy: TEditCopy;
    AcEditDelete: TEditDelete;
    XPManifest: TXPManifest;
    ShellResources: TShellResources;
    AcFileExit: TFileExit;
    RgAudio: TRibbonGroup;
    RgMIDI: TRibbonGroup;
    RgPlugins: TRibbonGroup;
    RgWindows: TRibbonGroup;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmCantabileLite: TFmCantabileLite;

implementation

{$R *.dfm}

end.
