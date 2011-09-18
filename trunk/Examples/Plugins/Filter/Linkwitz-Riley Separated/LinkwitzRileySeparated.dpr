{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinkwitzRileySeparated;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,// either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LinkwitzRileySeparatedDM in 'LinkwitzRileySeparatedDM.pas' {LinkwitzRileySeparatedModule: TVSTModule},
  LinkwitzRileySeparatedGui in 'LinkwitzRileySeparatedGui.pas' {FmLinkwitzRiley};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLinkwitzRileySeparatedModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TLinkwitzRileySeparatedModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

end.
