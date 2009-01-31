{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LinearPhaseCrossover;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  LinearPhaseCrossoverDM in 'LinearPhaseCrossoverDM.pas' {LinearPhaseCrossoverModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TLinearPhaseCrossoverModule.Create(Application) do
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
