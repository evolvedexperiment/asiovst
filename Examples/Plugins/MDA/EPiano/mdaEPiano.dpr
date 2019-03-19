{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaEPiano;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  EPianoDM in 'EPianoDM.pas' {EPianoDataModule: TVSTModule},
  EPianoData in 'EPianoData.pas';

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TEPianoDataModule.Create(Application) do
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
