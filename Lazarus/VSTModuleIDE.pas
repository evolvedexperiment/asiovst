{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit VSTModuleIDE; 

interface

uses
  VSTModuleLazIDEIntf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('VSTModuleLazIDEIntf', @VSTModuleLazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('VSTModuleIDE', @Register); 
end.
