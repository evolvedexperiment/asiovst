unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses Classes, DGuiStaticWaveform, DGuiDynamicWaveform, DGuiDial, DGuiMidiKeys, DGuiADSRGraph;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform,
                                      TGuiStaticWaveform,
                                      TGuiDial,
                                      TGuiMidiKeys,
                                      TGuiADSRGraph]);
end;

end.
