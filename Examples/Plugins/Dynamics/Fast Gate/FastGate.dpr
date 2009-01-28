{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FastGate;

{$R 'Gate.res' 'Gate.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  FastGateDM in 'FastGateDM.pas' {FastGateDataModule: TVSTModule},
  FastGateGUI in 'FastGateGUI.pas' {FmFastGate};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TFastGateDataModule.Create(Application) do
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
