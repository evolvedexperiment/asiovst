unit VAPlotIR;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, DGuiWaveform, Menus,
  DGuiBaseControl;

type
  TFmPlotIR = class(TForm)
    Waveform: TGuiStaticWaveform;
    PUDisplay: TPopupMenu;
    MIWaveform: TMenuItem;
  private
  public
  end;

var
  FmPlotIR: TFmPlotIR;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
initialization
  {$i VAPlotIR.lrs}
{$ENDIF}

end.
