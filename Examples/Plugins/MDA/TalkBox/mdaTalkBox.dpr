{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaTalkBox;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  TalkBoxDM in 'TalkBoxDM.pas' {TalkBoxDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TalkBoxDataModule: TTalkBoxDataModule;
begin
  try
    TalkBoxDataModule := TTalkBoxDataModule.Create(Application);
    TalkBoxDataModule.Effect^.user := TalkBoxDataModule;
    TalkBoxDataModule.AudioMaster := audioMaster;
    Result := TalkBoxDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.