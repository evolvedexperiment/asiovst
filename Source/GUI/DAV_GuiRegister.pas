unit DAV_GuiRegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DAV_GuiRegister.res}{$ENDIF}

uses
  Classes, DAV_GuiStaticWaveform, DAV_GuiDynamicWaveform, DAV_GuiDial, DAV_GuiLED,
  DAV_GuiGroup, DAV_GuiMidiKeys, DAV_GuiPanel, DAV_GuiADSRGraph, DAV_GuiLevelMeter,
  DAV_GuiAudioDataDisplay, DAV_GuiLabel, DAV_GuiSelectBox, DAV_GuiVUMeter,
  DAV_GuiButton;

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
