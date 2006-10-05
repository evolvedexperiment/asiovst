{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit HostASIOLaz; 

interface

uses
  DASIOHost, OpenASIO, Asio, DASIOConvert, DASIOGenerator, BeRoASIO, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('DASIOHost', @DASIOHost.Register); 
end; 

initialization
  RegisterPackage('HostASIOLaz', @Register); 
end.
