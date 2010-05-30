{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library mdaMultiband;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  MultibandDM in 'MultibandDM.pas' {MultibandDataModule: TVSTModule};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TMultibandDataModule.Create(Application) do
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