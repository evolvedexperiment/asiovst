{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDetune;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DetuneDM in 'DetuneDM.pas' {DetuneDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DetuneDataModule: TDetuneDataModule;
begin
  try
    DetuneDataModule := TDetuneDataModule.Create(Application);
    DetuneDataModule.Effect^.user := DetuneDataModule;
    DetuneDataModule.AudioMaster := audioMaster;
    Result := DetuneDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.