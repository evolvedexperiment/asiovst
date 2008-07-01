{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaRePsycho;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  RePsychoDM in 'RePsychoDM.pas' {RePsychoDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  RePsychoDataModule: TRePsychoDataModule;
begin
  try
    RePsychoDataModule := TRePsychoDataModule.Create(Application);
    RePsychoDataModule.Effect^.user := RePsychoDataModule;
    RePsychoDataModule.AudioMaster := audioMaster;
    Result := RePsychoDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.