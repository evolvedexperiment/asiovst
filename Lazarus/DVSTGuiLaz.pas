{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DVSTGuiLaz; 

interface

uses
  DBarChart, DMidiKeys, DWaveform, DADSRGraph, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DBarChart', @DBarChart.Register); 
  RegisterUnit('DMidiKeys', @DMidiKeys.Register); 
  RegisterUnit('DWaveform', @DWaveform.Register); 
  RegisterUnit('DADSRGraph', @DADSRGraph.Register); 
end; 

initialization
  RegisterPackage('DVSTGuiLaz', @Register); 
end.
