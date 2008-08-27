unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses
  Classes, DGuiStaticWaveform, DGuiDynamicWaveform, DGuiDial, DGuiMidiKeys,
  DGuiGroup, DGuiADSRGraph, DGuiLevelMeter, DGuiAudioDataDisplay;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform,
                                      TGuiStaticWaveform,
                                      TGuiDial,
                                      TGuiDialEx,
                                      TGuiGroup,
                                      TGuiMidiKeys,
                                      TGuiADSRGraph,
                                      TGuiLevelMeter,
                                      TGuiAudioDataDisplay]);
end;

end.
