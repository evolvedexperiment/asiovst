{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaShepard;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  ShepardDM in 'ShepardDM.pas' {ShepardDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  ShepardDataModule: TShepardDataModule;
begin
  try
    ShepardDataModule := TShepardDataModule.Create(Application);
    ShepardDataModule.Effect^.user := ShepardDataModule;
    ShepardDataModule.AudioMaster := audioMaster;
    Result := ShepardDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.