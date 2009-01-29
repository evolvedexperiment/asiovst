{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library FastLimiter;

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
  FastLimiterDM in 'FastLimiterDM.pas' {FastLimiterDataModule: TVSTModule},
  FastLimiterGUI in 'FastLimiterGUI.pas' {FmFastLimiter};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TFastLimiterDataModule.Create(Application) do
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
