{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library GranularPitchShifter;

{$R 'GranularPitchShifter.res' 'GranularPitchShifter.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  Forms,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTModule,
  GranularPitchShifterDM in 'GranularPitchShifterDM.pas' {GranularPitchShifterModule: TVSTModule},
  GranularPitchShifterGUI in 'GranularPitchShifterGUI.pas' {FmGranularPitchShifter};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TGranularPitchShifterModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TGranularPitchShifterModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
