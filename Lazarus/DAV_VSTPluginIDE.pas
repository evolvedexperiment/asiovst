{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_VSTPluginIDE; 

interface

uses
DAV_VSTModuleLazIDE, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_VSTModuleLazIDE', @DAV_VSTModuleLazIDE.Register); 
end; 

initialization
  RegisterPackage('DAV_VSTPluginIDE', @Register); 
end.
