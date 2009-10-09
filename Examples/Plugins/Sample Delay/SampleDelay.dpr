{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SampleDelay;

uses
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SampleDelayDM in 'SampleDelayDM.pas' {SampleDelayDataModule: TVSTModule},
  SampleDelayGui in 'SampleDelayGui.pas' {FmSampleDelay};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TSampleDelayDataModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TSampleDelayDataModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.