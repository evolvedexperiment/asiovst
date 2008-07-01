{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaPiano;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  PianoDM in 'PianoDM.pas' {PianoDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  PianoDataModule: TPianoDataModule;
begin
  try
    PianoDataModule := TPianoDataModule.Create(Application);
    PianoDataModule.Effect^.user := PianoDataModule;
    PianoDataModule.AudioMaster := audioMaster;
    Result := PianoDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.