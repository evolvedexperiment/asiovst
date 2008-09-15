{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaBeatBox;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  BeatBoxDM in 'BeatBoxDM.pas' {BeatBoxDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  BeatBoxDataModule: TBeatBoxDataModule;
begin
  try
    BeatBoxDataModule := TBeatBoxDataModule.Create(Application);
    BeatBoxDataModule.Effect^.user := BeatBoxDataModule;
    BeatBoxDataModule.AudioMaster := audioMaster;
    Result := BeatBoxDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.