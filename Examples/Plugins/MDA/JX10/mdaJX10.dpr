{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaJX10;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  JX10 in 'JX10.pas' {JX10DataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  JX10DataModule: TJX10DataModule;
begin
  try
    JX10DataModule := TJX10DataModule.Create(Application);
    JX10DataModule.Effect^.user := JX10DataModule;
    JX10DataModule.AudioMaster := audioMaster;
    Result := JX10DataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.