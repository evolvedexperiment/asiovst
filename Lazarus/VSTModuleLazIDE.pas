{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit VSTModuleLazIDE; 

interface

uses
  VSTModuleLazIDEIDEIntf, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('VSTModuleLazIDEIntf', @VSTModuleLazIDEIntf.Register); 
end; 

initialization
  RegisterPackage('VSTModuleLazIDE', @Register); 
end.
