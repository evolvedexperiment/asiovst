unit DAVGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DAVGUIRegister.res}{$ENDIF}

uses
  Classes, DAVGuiStaticWaveform, DAVGuiDynamicWaveform, DAVGuiDial, DAVGuiLED,
  DAVGuiGroup, DAVGuiMidiKeys, DAVGuiPanel, DAVGuiADSRGraph, DAVGuiLevelMeter,
  DAVGuiAudioDataDisplay, DAVGuiLabel, DAVGuiSelectBox, DAVGuiVUMeter,
  DAVGuiButton;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
                                      TGuiADSRGraph, TGuiAudioDataDisplay,
                                      TGuiLabel, TGuiGroup, TGuiPanel, TGuiLED,
                                      TGuiDial, TGuiDialMetal, TGuiDialEx,
                                      TGuiSelectBox, TGuiMidiKeys, TGuiButton,
                                      TGuiVUMeter, TGuiLevelMeter]);
end;

end.
