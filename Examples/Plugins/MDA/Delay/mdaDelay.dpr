{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDelay;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DelayDM in 'DelayDM.pas' {DelayDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DelayDataModule: TDelayDataModule;
begin
  try
    DelayDataModule := TDelayDataModule.Create(Application);
    DelayDataModule.Effect^.user := DelayDataModule;
    DelayDataModule.AudioMaster := audioMaster;
    Result := DelayDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.