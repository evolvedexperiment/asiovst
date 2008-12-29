{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Phaser;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PhaserDM in 'PhaserDM.pas' {PhaserModule: TPhaserModule},
  PhaserFrm in 'PhaserFrm.pas' {PhaserForm};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TPhaserModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.