{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinkwitzRileyLFE;

uses
  FastMM4,
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LinkwitzRileyDsp in 'LinkwitzRileyDsp.pas' {LinkwitzRileyModule: TVSTModule},
  LinkwitzRileyGUI in 'LinkwitzRileyGUI.pas' {FmLinkwitzRiley};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TLinkwitzRileyModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TLinkwitzRileyModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
    WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
