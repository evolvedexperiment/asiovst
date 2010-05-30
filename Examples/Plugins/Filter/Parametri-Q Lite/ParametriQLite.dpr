{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ParametriQLite;

{$R 'ParametriQLite.res' 'ParametriQLite.rc'}

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
  ParametriQLiteDM in 'ParametriQLiteDM.pas' {ParametriQLiteDataModule: TVSTModule},
  ParametriQLiteGUI in 'ParametriQLiteGUI.pas' {FmParametriQLite};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TParametriQLiteDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TParametriQLiteDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.