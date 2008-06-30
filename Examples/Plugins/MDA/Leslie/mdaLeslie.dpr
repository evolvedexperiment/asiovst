{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaLeslie;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  LeslieDM in 'LeslieDM.pas' {LeslieDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LeslieDataModule: TLeslieDataModule;
begin
  try
    LeslieDataModule := TLeslieDataModule.Create(Application);
    LeslieDataModule.Effect^.user := LeslieDataModule;
    LeslieDataModule.AudioMaster := audioMaster;
    Result := LeslieDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.