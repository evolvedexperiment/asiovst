{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem �bersetzen und Installieren des Packages.
 }

unit DVSTGFX; 

interface

uses
  DBarChart, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DBarChart', @DBarChart.Register); 
end; 

initialization
  RegisterPackage('DVSTGFX', @Register); 
end.
