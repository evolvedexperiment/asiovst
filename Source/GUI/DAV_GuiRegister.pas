unit DAV_GuiRegister;

interface

{$I ..\ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_GuiStaticWaveform,
  DAV_GuiDynamicWaveform, DAV_GuiDial, DAV_GuiLED, DAV_GuiGroup, DAV_GuiButton,
  DAV_GuiMidiKeys, DAV_GuiLevelMeter, DAV_GuiAudioDataDisplay, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiADSRGraph, DAV_GuiSelectBox, DAV_CorrelationMeter,
  DAV_GuiVUMeter;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
                                      TGuiADSRGraph, TGuiAudioDataDisplay,
                                      TGuiLabel, TGuiGroup, TGuiPanel, TGuiLED,
                                      TGuiDial, TGuiDialMetal, TGuiDialEx,
                                      TGuiSelectBox, TGuiMidiKeys, TGuiButton,
                                      TGuiVUMeter, TGuiLevelMeter,
                                      TGuiCorrelationMeter]);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
