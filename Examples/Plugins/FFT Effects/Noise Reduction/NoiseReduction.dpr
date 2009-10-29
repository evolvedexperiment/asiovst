{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NoiseReduction;

{$R 'Resources.res' 'Resources.rc'}

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  NoiseReductionDM in 'NoiseReductionDM.pas' {NoiseReductionModule: TVSTModule},
  NoiseReductionGui in 'NoiseReductionGui.pas' {FmNoiseReduction};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TNoiseReductionModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TNoiseReductionModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
