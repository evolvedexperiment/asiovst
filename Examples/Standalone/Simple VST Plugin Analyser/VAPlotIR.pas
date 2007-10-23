unit VAPlotIR;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, Buttons, {$ELSE}Windows, Messages,{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, DGuiWaveform, Menus;

type
  TFmPlotIR = class(TForm)
    Waveform: TWaveform;
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
