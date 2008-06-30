{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDither;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DitherDM in 'DitherDM.pas' {DitherDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DitherDataModule: TDitherDataModule;
begin
  try
    DitherDataModule := TDitherDataModule.Create(Application);
    DitherDataModule.Effect^.user := DitherDataModule;
    DitherDataModule.AudioMaster := audioMaster;
    Result := DitherDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.