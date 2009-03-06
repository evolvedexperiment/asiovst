{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SoundTouchDetuner;

{$R 'SoundTouchKnob.res' 'SoundTouchKnob.rc'}
{$R '..\..\..\Bin\SoundTouch.res' '..\..\..\Bin\SoundTouch.RC'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SoundTouchDetunerDM in 'SoundTouchDetunerDM.pas' {SoundTouchDetunerModule: TVSTModule},
  SoundTouchDetunerGUI in 'SoundTouchDetunerGUI.pas' {FmSoundTouchDetuner};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSoundTouchDetunerModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSoundTouchDetunerModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';
exports WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
