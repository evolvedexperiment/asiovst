unit MaxxBassCloneGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiLevelMeter;

type
  TFmMaxxBassClone = class(TForm)
    GuiColorLevelMeter1: TGuiColorLevelMeter;
    GuiColorLevelMeter2: TGuiColorLevelMeter;
    LbAudio: TGuiLabel;
    LbMaxxBass: TGuiLabel;
    LbOriginalBass: TGuiLabel;
    LbClipIndicator: TGuiLabel;
    LbFrequency: TLabel;
    SBFrequency: TScrollBar;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmMaxxBassClone: TFmMaxxBassClone;

implementation

{$R *.dfm}

end.
