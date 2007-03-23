{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DVSTGFX; 

interface

uses
  DBarChart, DMidiKeys, DWaveform, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DBarChart', @DBarChart.Register); 
  RegisterUnit('DMidiKeys', @DMidiKeys.Register); 
  RegisterUnit('DWaveform', @DWaveform.Register); 
end; 

initialization
  RegisterPackage('DVSTGFX', @Register); 
end.
