{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DVSTGuiLaz; 

interface

uses
  DGuiMidiKeys, DGuiWaveform, DGuiADSRGraph, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DGuiMidiKeys', @DGuiMidiKeys.Register); 
  RegisterUnit('DGuiWaveform', @DGuiWaveform.Register); 
  RegisterUnit('DGuiADSRGraph', @DGuiADSRGraph.Register); 
end; 

initialization
  RegisterPackage('DVSTGuiLaz', @Register); 
end.
