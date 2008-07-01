{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaMultiband;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  MultibandDM in 'MultibandDM.pas' {MultibandDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  MultibandDataModule: TMultibandDataModule;
begin
  try
    MultibandDataModule := TMultibandDataModule.Create(Application);
    MultibandDataModule.Effect^.user := MultibandDataModule;
    MultibandDataModule.AudioMaster := audioMaster;
    Result := MultibandDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.