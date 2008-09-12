{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SubBoost;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  SubBoostDM in 'SubBoostDM.pas' {SubBoostDataModule: TVSTModule},
  SubBoostGUI in 'SubBoostGUI.pas' {FmSubBoost};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  SubBoostDataModule: TSubBoostDataModule;
begin
  try
    SubBoostDataModule := TSubBoostDataModule.Create(Application);
    SubBoostDataModule.Effect^.user := SubBoostDataModule;
    SubBoostDataModule.AudioMaster := audioMaster;
    Result := SubBoostDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
