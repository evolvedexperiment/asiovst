{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseLinkwitzRileyLP;

{$R 'LinearPhaseLinkwitzRiley.res' 'LinearPhaseLinkwitzRiley.rc'}

uses
  FastMM4,
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LinearPhaseLinkwitzRileyDM in 'LinearPhaseLinkwitzRileyDM.pas' {LinearPhaseLinkwitzRileyDataModule: TVSTModule},
  LinearPhaseLinkwitzRileyGUI in 'LinearPhaseLinkwitzRileyGUI.pas' {FmLinearPhaseLinkwitzRiley};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLinearPhaseLinkwitzRileyDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TLinearPhaseLinkwitzRileyDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.