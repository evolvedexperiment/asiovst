{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaOverdrive;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OverdriveDM in 'OverdriveDM.pas' {OverdriveDataModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TOverdriveDataModule.Create(Application) do
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