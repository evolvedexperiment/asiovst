unit DAV_GuiRegister;

interface

{$I ..\ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiGroup, DAV_GuiLevelMeter, {$ENDIF}
  Classes, DAV_GuiStaticWaveform,
  DAV_GuiDynamicWaveform, DAV_GuiDial, DAV_GuiLED, DAV_GuiButton,
  DAV_GuiMidiKeys, DAV_GuiAudioDataDisplay, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiADSRGraph, DAV_GuiSelectBox, DAV_CorrelationMeter,
  DAV_GuiVUMeter;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [TGuiDynamicWaveform, TGuiStaticWaveform,
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiLabel, TGuiPanel, TGuiLED,
    TGuiVUMeter, TGuiDial, TGuiDialMetal, TGuiCorrelationMeter, TGuiSelectBox,
    TGuiMidiKeys, TGuiButton, {$IFNDEF FPC} TGuiGroup, TGuiLevelMeter, {$ENDIF}
    TGuiDialEx]);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
