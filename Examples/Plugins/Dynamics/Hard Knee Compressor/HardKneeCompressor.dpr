{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library HardKneeCompressor;

{$R 'HardKneeCompressor.res' 'HardKneeCompressor.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_VSTBasicModule,
  HardKneeCompressorDM in 'HardKneeCompressorDM.pas' {SoftKneeCompressorDataModule: TVSTModule},
  HardKneeCompressorGUI in 'HardKneeCompressorGUI.pas' {FmHardKneeCompressor};

begin
 BasicVSTModuleClass := THardKneeCompressorDataModule;
end.
