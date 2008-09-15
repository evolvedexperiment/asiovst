{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaStereo;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  StereoDM in 'StereoDM.pas' {StereoDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  StereoDataModule: TStereoDataModule;
begin
  try
    StereoDataModule := TStereoDataModule.Create(Application);
    StereoDataModule.Effect^.user := StereoDataModule;
    StereoDataModule.AudioMaster := audioMaster;
    Result := StereoDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.