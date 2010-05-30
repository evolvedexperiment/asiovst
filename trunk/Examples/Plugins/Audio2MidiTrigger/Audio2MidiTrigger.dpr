{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Audio2MidiTrigger;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  Audio2MidiTriggerDM in 'Audio2MidiTriggerDM.pas' {Audio2MidiTriggerModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TAudio2MidiTriggerModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.
