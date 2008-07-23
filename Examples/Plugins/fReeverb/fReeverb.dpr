{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library fReeverb;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  fReeverbModule in 'fReeverbModule.pas' {fReeverbVST: TVSTModule},
  fReeverbGUI in 'fReeverbGUI.pas' {FmReverb};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var fReeverbVST : TfReeverbVST;
begin
 try
  fReeverbVST:=TfReeverbVST.Create(Application);
  fReeverbVST.Effect^.user := fReeverbVST;
  fReeverbVST.AudioMaster := audioMaster;
  Result := fReeverbVST.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VstPluginMain';

begin
end.