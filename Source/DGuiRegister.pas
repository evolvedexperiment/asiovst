unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses Classes, DGuiWaveform, DGuiDial, DGuiMidiKeys, DGuiADSRGraph;

procedure Register;
begin
 RegisterComponents('ASIO/VST GUI', [TWaveform,TMidiKeys,TDial,TADSRGraph]);
end;

end.
