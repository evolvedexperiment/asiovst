{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library VSTPascalScript;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  PSDM in 'PSDM.pas' {PascalScriptDataModule: TVSTModule},
  PSGUI in 'PSGUI.pas' {FmPascalScript};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TPascalScriptDataModule.Create(Application) do
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