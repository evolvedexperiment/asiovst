unit DGuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DGUIRegister.res}{$ENDIF}

uses
  Classes, DGuiStaticWaveform, DGuiDynamicWaveform, DGuiDial, DGuiMidiKeys,
  DGuiGroup, DGuiPanel, DGuiADSRGraph, DGuiLevelMeter, DGuiAudioDataDisplay,
  DGuiLabel;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform,
                                      TGuiStaticWaveform,
                                      TGuiDial,
                                      TGuiDialEx,
                                      TGuiGroup,
                                      TGuiPanel,
                                      TGuiLabel,
                                      TGuiMidiKeys,
                                      TGuiADSRGraph,
                                      TGuiLevelMeter,
                                      TGuiAudioDataDisplay]);
end;

end.
