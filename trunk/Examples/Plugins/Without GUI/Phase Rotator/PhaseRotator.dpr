{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library PhaseRotator;

uses
  FastMM4,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PhaseRotatorDSP in 'PhaseRotatorDSP.pas' {PhaseRotatorModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPhaseRotatorModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TPhaseRotatorModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
