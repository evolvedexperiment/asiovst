{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SchaebichefLP;

{$R 'Schaebichef.res' 'Schaebichef.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SchaebichefDM in 'SchaebichefDM.pas' {SchaebichefLPModule: TVSTModule},
  SchaebichefGUI in 'SchaebichefGUI.pas' {FmSchaebichef};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSchaebichefLPModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSchaebichefLPModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
