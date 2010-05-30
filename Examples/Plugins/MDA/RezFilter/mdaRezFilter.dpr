{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaRezFilter;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  RezFilterDM in 'RezFilterDM.pas' {RezFilterDataModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TRezFilterDataModule.Create(Application) do
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