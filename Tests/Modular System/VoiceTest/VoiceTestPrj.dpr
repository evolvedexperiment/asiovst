{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VoiceTestPrj;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  VoiceTestModuleU in 'VoiceTestModuleU.pas' {VoiceTestModule: TVSTModule},
  VoiceTestFormU in 'VoiceTestFormU.pas' {VoiceTestForm},
  VoiceTestVoiceU in 'VoiceTestVoiceU.pas' {VoiceTestVoice: TDataModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VoiceTestModule: TVoiceTestModule;
begin
  try
    VoiceTestModule := TVoiceTestModule.Create(Application);
    VoiceTestModule.Effect^.user := VoiceTestModule;
    VoiceTestModule.AudioMaster := audioMaster;
    Result := VoiceTestModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.