{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaRingMod;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  RingModDM in 'RingModDM.pas' {RingModDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  RingModDataModule: TRingModDataModule;
begin
  try
    RingModDataModule := TRingModDataModule.Create(Application);
    RingModDataModule.Effect^.user := RingModDataModule;
    RingModDataModule.AudioMaster := audioMaster;
    Result := RingModDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.