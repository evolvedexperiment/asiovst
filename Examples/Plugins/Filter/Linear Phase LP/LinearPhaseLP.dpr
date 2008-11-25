{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseLP;

{$R 'LinearPhase.res' 'LinearPhase.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  LinearPhaseDM in 'LinearPhaseDM.pas' {LinearPhaseDataModule: TVSTModule},
  LinearPhaseGUI in 'LinearPhaseGUI.pas' {FmLinearPhase};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LinearPhaseDataModule: TLinearPhaseDataModule;
begin
  try
    LinearPhaseDataModule := TLinearPhaseDataModule.Create(Application);
    LinearPhaseDataModule.Effect^.user := LinearPhaseDataModule;
    LinearPhaseDataModule.AudioMaster := audioMaster;
    Result := LinearPhaseDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.