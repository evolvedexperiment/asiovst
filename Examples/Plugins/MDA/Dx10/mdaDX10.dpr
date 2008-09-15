{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDX10;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DX10DM in 'DX10DM.pas' {DX10DataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DX10DataModule: TDX10DataModule;
begin
  try
    DX10DataModule := TDX10DataModule.Create(Application);
    DX10DataModule.Effect^.user := DX10DataModule;
    DX10DataModule.AudioMaster := audioMaster;
    Result := DX10DataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.