{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SineSynth;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  SineSynthModule in 'SineSynthModule.pas' {VSTSSModule: TVSTModule},
  SineSynthGUI in 'SineSynthGUI.pas' {VSTGUI},
  SineSynthVoice in 'SineSynthVoice.pas',
  VoiceList in 'VoiceList.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTSSModule : TVSTSSModule;
begin
 try
  VSTSSModule:=TVSTSSModule.Create(Application);
  VSTSSModule.Effect^.user:=VSTSSModule;
  VSTSSModule.AudioMaster:=audioMaster;
  Result := VSTSSModule.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

