{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ModDelay;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ModDelayDM in 'ModDelayDM.pas' {ModDelayModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TModDelayModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TModDelayModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
