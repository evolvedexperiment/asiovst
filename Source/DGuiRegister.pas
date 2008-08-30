unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses
  Classes, DGuiStaticWaveform, DGuiDynamicWaveform, DGuiDial, DGuiMidiKeys,
  DGuiGroup, DGuiPanel, DGuiADSRGraph, DGuiLevelMeter, DGuiAudioDataDisplay,
  DGuiLabel, DGuiLED;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
                                      TGuiDial, TGuiDialMetal, TGuiDialEx,
                                      TGuiGroup, TGuiPanel, TGuiLabel,
                                      TGuiLED, TGuiMidiKeys, TGuiADSRGraph,
                                      TGuiLevelMeter, TGuiAudioDataDisplay]);
end;

end.
