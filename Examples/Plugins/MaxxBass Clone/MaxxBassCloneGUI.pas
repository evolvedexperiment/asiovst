unit MaxxBassCloneGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiLevelMeter;

type
  TFmHarmonicBassClone = class(TForm)
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
  FmHarmonicBassClone: TFmHarmonicBassClone;

implementation

{$R *.dfm}

end.
