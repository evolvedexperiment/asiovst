{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FrequencyDomainPitchShifter;

{$R 'FrequencyDomainKnob.res' 'FrequencyDomainKnob.rc'}

uses
  FastMM4,
  madLinkDisAsm,
  madListProcesses,
  madListModules, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  FrequencyDomainPitchShifterDM in 'FrequencyDomainPitchShifterDM.pas' {FrequencyDomainPitchShifterModule: TVSTModule},
  FrequencyDomainPitchShifterGUI in 'FrequencyDomainPitchShifterGUI.pas' {FmFrequencyDomainPitchShifter},
  DAV_DspFrequencyDomainPitchshifter in '..\..\..\Source\DSP\DAV_DspFrequencyDomainPitchshifter.pas';

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TFrequencyDomainPitchShifterModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TFrequencyDomainPitchShifterModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
