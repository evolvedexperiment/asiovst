{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Vocoder;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  VocoderModule in 'VocoderModule.pas' {VSTSSModule: TVSTModule},
  VocoderGUI in 'VocoderGUI.pas' {VSTGUI},
  VocoderVoice in 'VocoderVoice.pas',
  VoiceList in 'VoiceList.pas';

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVSTSSModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

