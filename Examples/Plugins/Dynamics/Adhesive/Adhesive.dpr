{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Adhesive;

{$R 'Adhesive.res' 'Adhesive.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  AdhesiveDM in 'AdhesiveDM.pas' {AdhesiveDataModule: TVSTModule},
  AdhesiveGUI in 'AdhesiveGUI.pas' {FmAdhesive};

begin
 BasicVSTModuleClass := TAdhesiveDataModule;
end.
