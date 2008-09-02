unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses
  Classes, DGuiStaticWaveform, DGuiDynamicWaveform, DGuiDial, DGuiMidiKeys,
  DGuiGroup, DGuiPanel, DGuiADSRGraph, DGuiLevelMeter, DGuiAudioDataDisplay,
  DGuiLabel, DGuiSelectBox, DGuiLED, DGuiButton;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
                                      TGuiADSRGraph, TGuiAudioDataDisplay,
                                      TGuiLabel, TGuiGroup, TGuiPanel, TGuiLED,
                                      TGuiDial, TGuiDialMetal, TGuiDialEx,
                                      TGuiSelectBox, TGuiMidiKeys, TGuiButton,
                                      TGuiLevelMeter]);
end;

end.
