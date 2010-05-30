{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library RingModulator;

{$R 'RingModulator.res' 'RingModulator.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  RingModulatorDM in 'RingModulatorDM.pas' {RingModulatorDataModule: TVSTModule},
  RingModulatorGUI in 'RingModulatorGUI.pas' {FmRingModulator};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TRingModulatorDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TRingModulatorDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.