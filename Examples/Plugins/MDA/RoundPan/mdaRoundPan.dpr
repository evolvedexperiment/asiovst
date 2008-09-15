{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaRoundPan;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  RoundPanDM in 'RoundPanDM.pas' {RoundPanDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  RoundPanDataModule: TRoundPanDataModule;
begin
  try
    RoundPanDataModule := TRoundPanDataModule.Create(Application);
    RoundPanDataModule.Effect^.user := RoundPanDataModule;
    RoundPanDataModule.AudioMaster := audioMaster;
    Result := RoundPanDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.