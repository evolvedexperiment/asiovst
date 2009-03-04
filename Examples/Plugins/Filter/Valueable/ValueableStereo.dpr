{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ValueableStereo;

{$R 'Valueable.res' 'Valueable.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  VTModuleStereo in 'VTModuleStereo.pas' {VTVSTModule: TVST2Module},
  VTGUIStereo in 'VTGUIStereo.pas' {FmVT};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TVTVSTModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TVTVSTModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
