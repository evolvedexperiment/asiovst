{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BarberpoleTuner;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  BarberpoleTunerDM in 'BarberpoleTunerDM.pas' {BarberpoleTunerDataModule: TVSTModule},
  BarberpoleTunerGUI in 'BarberpoleTunerGUI.pas' {FmBarberpoleTuner};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TBarberpoleTunerDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TBarberpoleTunerDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
