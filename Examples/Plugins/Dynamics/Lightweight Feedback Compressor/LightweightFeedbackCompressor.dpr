{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightFeedbackCompressor;

{$R 'FeedbackCompressor.res' 'FeedbackCompressor.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,// either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LightweightFeedbackCompressorDM in 'LightweightFeedbackCompressorDM.pas' {LightweightFeedbackCompressorDataModule: TVSTModule},
  LightweightFeedbackCompressorGUI in 'LightweightFeedbackCompressorGUI.pas' {FmLightweightFeedbackCompressor};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLightweightFeedbackCompressorDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TLightweightFeedbackCompressorDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
