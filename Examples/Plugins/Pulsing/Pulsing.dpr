{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Pulsing;

uses
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  PulsingDSP in 'PulsingDSP.pas' {PulsingDataModule: TVSTModule},
  PulsingGUI in 'PulsingGUI.pas' {FmPulsing};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TPulsingDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TPulsingDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.