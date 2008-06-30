{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDegrade;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DegradeDM in 'DegradeDM.pas' {DegradeDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DegradeDataModule: TDegradeDataModule;
begin
  try
    DegradeDataModule := TDegradeDataModule.Create(Application);
    DegradeDataModule.Effect^.user := DegradeDataModule;
    DegradeDataModule.AudioMaster := audioMaster;
    Result := DegradeDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.