{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Tuner;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  TunerDM in 'TunerDM.pas' {TunerDataModule: TVSTModule},
  TunerGUI in 'TunerGUI.pas' {FmTuner};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TTunerDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TTunerDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.