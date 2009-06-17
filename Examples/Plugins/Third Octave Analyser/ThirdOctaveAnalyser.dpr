{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ThirdOctaveAnalyser;

uses
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  ThirdOctaveAnalyserDM in 'ThirdOctaveAnalyserDM.pas' {ThirdOctaveAnalyserModule: TVSTModule},
  ThirdOctaveAnalyserGUI in 'ThirdOctaveAnalyserGUI.pas' {FmThirdOctaveAnalyser};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TThirdOctaveAnalyserModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TThirdOctaveAnalyserModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.