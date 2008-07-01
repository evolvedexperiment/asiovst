{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaRezFilter;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  RezFilterDM in 'RezFilterDM.pas' {RezFilterDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  RezFilterDataModule: TRezFilterDataModule;
begin
  try
    RezFilterDataModule := TRezFilterDataModule.Create(Application);
    RezFilterDataModule.Effect^.user := RezFilterDataModule;
    RezFilterDataModule.AudioMaster := audioMaster;
    Result := RezFilterDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.