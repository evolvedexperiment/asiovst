{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaTracker;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  TrackerDM in 'TrackerDM.pas' {TrackerDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TrackerDataModule: TTrackerDataModule;
begin
  try
    TrackerDataModule := TTrackerDataModule.Create(Application);
    TrackerDataModule.Effect^.user := TrackerDataModule;
    TrackerDataModule.AudioMaster := audioMaster;
    Result := TrackerDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.