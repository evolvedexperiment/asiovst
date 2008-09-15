{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library PerformanceTest;

uses
  FastMM4,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PTDM in 'PTDM.pas' {PerformanceTestModule: TVSTModule},
  PTGUI in 'PTGUI.pas' {FmPerformanceTest};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  PerformanceTestModule: TPerformanceTestModule;
begin
  try
    PerformanceTestModule := TPerformanceTestModule.Create(Application);
    PerformanceTestModule.Effect^.user := PerformanceTestModule;
    PerformanceTestModule.AudioMaster := audioMaster;
    Result := PerformanceTestModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.