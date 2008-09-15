{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleSampler;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleSamplerModule in 'SimpleSamplerModule.pas' {VSTSSModule: TVSTModule},
  SimpleSamplerGUI in 'SimpleSamplerGUI.pas' {VSTGUI},
  SimpleSamplerVoice in 'SimpleSamplerVoice.pas',
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
                                                             
