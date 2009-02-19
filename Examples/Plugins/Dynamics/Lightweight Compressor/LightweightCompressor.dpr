{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightCompressor;

{$R 'Compressor.res' 'Compressor.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  LightweightCompressorDM in 'LightweightCompressorDM.pas' {LightweightCompressorDataModule: TVSTModule},
  LightweightCompressorGUI in 'LightweightCompressorGUI.pas' {FmLightweightCompressor};

begin
 BasicVSTModuleClass := TLightweightCompressorDataModule;
end.
