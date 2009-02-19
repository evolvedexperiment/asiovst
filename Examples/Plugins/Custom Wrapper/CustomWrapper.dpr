{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library CustomWrapper;

uses
  FastMM4,  // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  FastMove, // either download the library or comment if there is an error here
  DAV_VSTBasicModule,
  CustomWrapperDM in 'CustomWrapperDM.pas' {CustomWrapperDataModule: TVSTModule};

begin
 BasicVSTModuleClass := TCustomWrapperDataModule;
end.
