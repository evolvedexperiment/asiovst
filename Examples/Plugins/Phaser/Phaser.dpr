{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Phaser;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  PhaserDM in 'PhaserDM.pas' {PhaserModule: TVSTModule},
  PhaserFrm in 'PhaserFrm.pas' {PhaserForm};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  PhaserModule: TPhaserModule;
begin
  try
    PhaserModule := TPhaserModule.Create(Application);
    PhaserModule.Effect^.user := PhaserModule;
    PhaserModule.AudioMaster := audioMaster;
    Result := PhaserModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.