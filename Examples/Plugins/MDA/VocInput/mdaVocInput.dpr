{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaVocInput;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  VocInputDM in 'VocInputDM.pas' {VocInputDataModule: TVSTModule},
  DAV_DspVoiceInput in '..\..\..\..\Source\DSP\DAV_DspVoiceInput.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVocInputDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TVocInputDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
