{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_GUI_Lazarus; 

interface

uses
  DAV_GuiVUMeter, DAV_CorrelationMeter, DAV_GuiADSRGraph, 
  DAV_GuiAudioDataDisplay, DAV_GuiBaseControl, DAV_GuiButton, DAV_GuiDial, 
  DAV_GuiDynamicWaveform, DAV_GuiLabel, DAV_GuiLED, DAV_GuiMidiKeys, 
  DAV_GuiMidiKeyZones, DAV_GuiPanel, DAV_GuiRegister, DAV_GuiSelectBox, 
  DAV_GuiStaticWaveform, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_GuiRegister', @DAV_GuiRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_GUI_Lazarus', @Register); 
end.
