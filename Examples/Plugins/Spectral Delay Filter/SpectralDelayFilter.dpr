{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SpectralDelayFilter;

uses
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SpectralDelayFilterDM in 'SpectralDelayFilterDM.pas' {SpectralDelayFilterModule: TVSTModule},
  SpectralDelayFilterGUI in 'SpectralDelayFilterGUI.pas' {FmSpectralDelayFilter},
  DAV_DspFilterSpectralDelay in '..\..\..\Source\DSP\DAV_DspFilterSpectralDelay.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TSpectralDelayFilterModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TSpectralDelayFilterModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.