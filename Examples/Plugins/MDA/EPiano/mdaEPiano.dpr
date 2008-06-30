{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaEPiano;

uses
  Forms,
  DVSTEffect,
  DVSTModule,
  EPianoDM in 'EPianoDM.pas' {EPianoDataModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  EPianoDataModule: TEPianoDataModule;
begin
  try
    EPianoDataModule := TEPianoDataModule.Create(Application);
    EPianoDataModule.Effect^.user := EPianoDataModule;
    EPianoDataModule.AudioMaster := audioMaster;
    Result := EPianoDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.