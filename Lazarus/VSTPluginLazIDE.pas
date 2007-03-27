{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit VSTPluginLazIDE; 

interface

uses
  VSTModuleLazIDEIntf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('VSTModuleLazIDEIntf', @VSTModuleLazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('VSTPluginLazIDE', @Register); 

end.
