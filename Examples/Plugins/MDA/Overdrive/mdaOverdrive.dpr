{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaOverdrive;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  OverdriveDM in 'OverdriveDM.pas' {OverdriveDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  OverdriveDataModule: TOverdriveDataModule;
begin
  try
    OverdriveDataModule := TOverdriveDataModule.Create(Application);
    OverdriveDataModule.Effect^.user := OverdriveDataModule;
    OverdriveDataModule.AudioMaster := audioMaster;
    Result := OverdriveDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.