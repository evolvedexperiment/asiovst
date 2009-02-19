{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightFeedbackCompressor;

{$R 'FeedbackCompressor.res' 'FeedbackCompressor.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  LightweightFeedbackCompressorDM in 'LightweightFeedbackCompressorDM.pas' {LightweightFeedbackCompressorDataModule: TVSTModule},
  LightweightFeedbackCompressorGUI in 'LightweightFeedbackCompressorGUI.pas' {FmLightweightFeedbackCompressor};

begin
 BasicVSTModuleClass := TLightweightFeedbackCompressorDataModule;
end.
