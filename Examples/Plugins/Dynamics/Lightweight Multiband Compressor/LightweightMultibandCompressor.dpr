{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightMultibandCompressor;

{$R 'MultibandCompressor.res' 'MultibandCompressor.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  LightweightMultibandCompressorDM in 'LightweightMultibandCompressorDM.pas' {LightweightMultibandCompressorDataModule: TVSTModule},
  LightweightMultibandCompressorGUI in 'LightweightMultibandCompressorGUI.pas' {FmLightweightMultibandCompressor};

begin
 BasicVSTModuleClass := TLightweightMultibandCompressorDataModule;
end.
