{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SplitHarmonizer;

// if the file below is missing please execute the batch file in this
// directory first to compile the resource file

{$R 'SoundTouchKnob.res' 'SoundTouchKnob.rc'}
{$R '..\..\..\Bin\SoundTouch.res' '..\..\..\Bin\SoundTouch.RC'}

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  DAV_WinAmp,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  SplitHarmonizerDM in 'SplitHarmonizerDM.pas' {SplitHarmonizerModule: TVSTModule},
  SplitHarmonizerGUI in 'SplitHarmonizerGUI.pas' {FmSplitHarmonizer};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TSplitHarmonizerModule);
end;

function WinampDSPGetHeader: PWinAmpDSPHeader; cdecl; export;
begin
 Result := WinampDSPModuleHeader(TSplitHarmonizerModule);
end;

exports
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain',
  WinampDSPGetHeader name 'winampDSPGetHeader2';

begin
end.
