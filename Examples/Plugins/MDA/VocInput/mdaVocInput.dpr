{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaVocInput;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  VocInputDM in 'VocInputDM.pas' {VocInputDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VocInputDataModule: TVocInputDataModule;
begin
  try
    VocInputDataModule := TVocInputDataModule.Create(Application);
    VocInputDataModule.Effect^.user := VocInputDataModule;
    VocInputDataModule.AudioMaster := audioMaster;
    Result := VocInputDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.