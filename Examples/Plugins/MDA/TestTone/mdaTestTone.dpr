{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaTestTone;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  TestToneDM in 'TestToneDM.pas' {TestToneDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TestToneDataModule: TTestToneDataModule;
begin
  try
    TestToneDataModule := TTestToneDataModule.Create(Application);
    TestToneDataModule.Effect^.user := TestToneDataModule;
    TestToneDataModule.AudioMaster := audioMaster;
    Result := TestToneDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.