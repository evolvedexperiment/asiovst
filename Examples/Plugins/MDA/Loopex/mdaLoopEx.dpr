{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaLoopEx;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  LoopExDM in 'LoopExDM.pas' {LoopExDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  LoopExDataModule: TLoopExDataModule;
begin
  try
    LoopExDataModule := TLoopExDataModule.Create(Application);
    LoopExDataModule.Effect^.user := LoopExDataModule;
    LoopExDataModule.AudioMaster := audioMaster;
    Result := LoopExDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.