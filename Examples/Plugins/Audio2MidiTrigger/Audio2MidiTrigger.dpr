{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Audio2MidiTrigger;

uses
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  Audio2MidiTriggerDM in 'Audio2MidiTriggerDM.pas' {Audio2MidiTriggerModule: TVSTModule},
  Audio2MidiTriggerGui in 'Audio2MidiTriggerGui.pas' {FmAudio2MidiTrigger};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  Result := VstModuleMain(AudioMasterCallback, TAudio2MidiTriggerModule);
end;

exports VstPluginMain name 'main';
exports VstPluginMain name 'VSTPluginMain';

begin
end.