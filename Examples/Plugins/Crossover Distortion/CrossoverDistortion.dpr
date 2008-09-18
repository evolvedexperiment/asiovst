{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library CrossoverDistortion;

{$R 'CrossoverDistortion.res' 'CrossoverDistortion.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  CrossoverDistortionDM in 'CrossoverDistortionDM.pas' {CrossoverDistortionDataModule: TVSTModule},
  CrossoverDistortionGUI in 'CrossoverDistortionGUI.pas' {FmCrossoverDistortion};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  CrossoverDistortionDataModule: TCrossoverDistortionDataModule;
begin
  try
    CrossoverDistortionDataModule := TCrossoverDistortionDataModule.Create(Application);
    CrossoverDistortionDataModule.Effect^.user := CrossoverDistortionDataModule;
    CrossoverDistortionDataModule.AudioMaster := audioMaster;
    Result := CrossoverDistortionDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.