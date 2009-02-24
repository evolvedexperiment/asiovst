{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SoundTouchPitchShifter;

{$R 'SoundTouchKnob.res' 'SoundTouchKnob.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SoundTouchPitchShifterDM in 'SoundTouchPitchShifterDM.pas' {SoundTouchPitchShifterModule: TVSTModule},
  SoundTouchPitchShifterGUI in 'SoundTouchPitchShifterGUI.pas' {FmSoundTouchPitchShifter};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSoundTouchPitchShifterModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSoundTouchPitchShifterModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
