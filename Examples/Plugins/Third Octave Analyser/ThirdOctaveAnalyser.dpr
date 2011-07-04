{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ThirdOctaveAnalyser;

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
  ThirdOctaveAnalyserDM in 'ThirdOctaveAnalyserDM.pas' {ThirdOctaveAnalyserModule: TVSTModule},
  ThirdOctaveAnalyserGUI in 'ThirdOctaveAnalyserGUI.pas' {FmThirdOctaveAnalyser},
  DAV_ThirdOctaveAnalyser in '..\..\..\Source\DAV_ThirdOctaveAnalyser.pas',
  DAV_ThirdOctaveAnalyserFilter in '..\..\..\Source\DAV_ThirdOctaveAnalyserFilter.pas',
  DAV_ThirdOctaveAnalyserFFT in '..\..\..\Source\DAV_ThirdOctaveAnalyserFFT.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TThirdOctaveAnalyserModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
  Result := WinampDSPModuleHeader(TThirdOctaveAnalyserModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
