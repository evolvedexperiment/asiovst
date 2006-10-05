{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit HostVSTLaz; 

interface

uses
  DVSTHost, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DVSTHost', @DVSTHost.Register); 
end; 

initialization
  RegisterPackage('HostVSTLaz', @Register); 
end.
