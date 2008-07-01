{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaTransient;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  TransientDM in 'TransientDM.pas' {TransientDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TransientDataModule: TTransientDataModule;
begin
  try
    TransientDataModule := TTransientDataModule.Create(Application);
    TransientDataModule.Effect^.user := TransientDataModule;
    TransientDataModule.AudioMaster := audioMaster;
    Result := TransientDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.