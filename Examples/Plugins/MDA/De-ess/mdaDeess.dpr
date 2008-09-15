{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDeess;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DeessDM in 'DeessDM.pas' {DeessDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DeessDataModule: TDeessDataModule;
begin
  try
    DeessDataModule := TDeessDataModule.Create(Application);
    DeessDataModule.Effect^.user := DeessDataModule;
    DeessDataModule.AudioMaster := audioMaster;
    Result := DeessDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.