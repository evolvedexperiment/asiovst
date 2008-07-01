{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaLoudness;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  LoudnessDM in 'LoudnessDM.pas' {LoudnessDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LoudnessDataModule: TLoudnessDataModule;
begin
  try
    LoudnessDataModule := TLoudnessDataModule.Create(Application);
    LoudnessDataModule.Effect^.user := LoudnessDataModule;
    LoudnessDataModule.AudioMaster := audioMaster;
    Result := LoudnessDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.