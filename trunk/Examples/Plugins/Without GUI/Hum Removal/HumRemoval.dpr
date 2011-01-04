{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library HumRemoval;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  HumRemovalDSP in 'HumRemovalDSP.pas' {HumRemovalModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, THumRemovalModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(THumRemovalModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
