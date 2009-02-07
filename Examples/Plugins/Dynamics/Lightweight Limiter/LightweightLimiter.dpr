{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightLimiter;

{$R 'Limiter.res' 'Limiter.rc'}

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
  LightweightLimiterDM in 'LightweightLimiterDM.pas' {LightweightLimiterDataModule: TVSTModule},
  LightweightLimiterGUI in 'LightweightLimiterGUI.pas' {FmLightweightLimiter};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TLightweightLimiterDataModule.Create(Application) do
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
