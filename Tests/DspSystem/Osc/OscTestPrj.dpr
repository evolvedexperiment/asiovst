{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OscTestPrj;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  OscTestModuleU in 'OscTestModuleU.pas' {OscTestModule: TVSTModule},
  OscTestFormU in 'OscTestFormU.pas' {OscTestForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  OscTestModule: TOscTestModule;
begin
  try
    OscTestModule := TOscTestModule.Create(Application);
    OscTestModule.Effect^.user := OscTestModule;
    OscTestModule.AudioMaster := audioMaster;
    Result := OscTestModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.