{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaAmbience;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  mdaAmbienceDM in 'mdaAmbienceDM.pas' {mdaAmbienceDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  mdaAmbienceDataModule: TmdaAmbienceDataModule;
begin
  try
    mdaAmbienceDataModule := TmdaAmbienceDataModule.Create(Application);
    mdaAmbienceDataModule.Effect^.user := mdaAmbienceDataModule;
    mdaAmbienceDataModule.AudioMaster := audioMaster;
    Result := mdaAmbienceDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.