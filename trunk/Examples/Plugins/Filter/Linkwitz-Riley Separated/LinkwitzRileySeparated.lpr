{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinkwitzRileySeparated;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  LinkwitzRileySeparatedDM in 'LinkwitzRileySeparatedDM.pas' {LinkwitzRileySeparatedModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TLinkwitzRileySeparatedModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TLinkwitzRileySeparatedModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
