{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ChebyshevHP;

{$R 'Chebyshev.res' 'Chebyshev.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ChebyshevDM in 'ChebyshevDM.pas' {ChebyshevHPModule: TVSTModule},
  ChebyshevGUI in 'ChebyshevGUI.pas' {FmChebyshev};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TChebyshevHPModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TChebyshevHPModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
