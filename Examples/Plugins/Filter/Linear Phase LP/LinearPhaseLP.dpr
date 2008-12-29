{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseLP;

{$R 'LinearPhase.res' 'LinearPhase.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  LinearPhaseDM in 'LinearPhaseDM.pas' {LinearPhaseDataModule: TVSTModule},
  LinearPhaseGUI in 'LinearPhaseGUI.pas' {FmLinearPhase};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TLinearPhaseDataModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.