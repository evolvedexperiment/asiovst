{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaDubDelay;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  DubDelayDM in 'DubDelayDM.pas' {DubDelayDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  DubDelayDataModule: TDubDelayDataModule;
begin
  try
    DubDelayDataModule := TDubDelayDataModule.Create(Application);
    DubDelayDataModule.Effect^.user := DubDelayDataModule;
    DubDelayDataModule.AudioMaster := audioMaster;
    Result := DubDelayDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.