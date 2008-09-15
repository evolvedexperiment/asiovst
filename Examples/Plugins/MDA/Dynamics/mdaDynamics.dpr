{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDynamics;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DynamicsDM in 'DynamicsDM.pas' {DynamicsDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DynamicsDataModule: TDynamicsDataModule;
begin
  try
    DynamicsDataModule := TDynamicsDataModule.Create(Application);
    DynamicsDataModule.Effect^.user := DynamicsDataModule;
    DynamicsDataModule.AudioMaster := audioMaster;
    Result := DynamicsDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.