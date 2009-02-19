{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightGate;

{$R 'Gate.res' 'Gate.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  LightweightGateDM in 'LightweightGateDM.pas' {LightweightGateDataModule: TVSTModule},
  LightweightGateGUI in 'LightweightGateGUI.pas' {FmLightweightGate};

begin
 BasicVSTModuleClass := TLightweightGateDataModule;
end.
