{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem �bersetzen und Installieren des Packages.
 }

unit AVDCommonLaz; 

interface

uses
  DAVDCommon, CPUDetectionTool, DAVDComplex, MidiFile, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('AVDCommonLaz', @Register); 
end.
