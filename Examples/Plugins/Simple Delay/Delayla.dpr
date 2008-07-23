{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Delayla;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DelaylaModule in 'DelaylaModule.pas' {SimpleDelayVST: TVST2Module},
  DelaylaGUI in 'DelaylaGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var SimpleDelay : TSimpleDelayVST;
begin
 try
  SimpleDelay:=TSimpleDelayVST.Create(Application);
  SimpleDelay.Effect^.user := SimpleDelay;
  SimpleDelay.AudioMaster := audioMaster;
  Result := SimpleDelay.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.