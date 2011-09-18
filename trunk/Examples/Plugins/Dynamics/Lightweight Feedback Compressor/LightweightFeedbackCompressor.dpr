{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightFeedbackCompressor;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  FastMove,
  madExcept,
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

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
