{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Exciter;

{$R 'Exciter.res' 'Exciter.rc'}

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ExciterDM in 'ExciterDM.pas' {ExciterDataModule: TVSTModule},
  ExciterGUI in 'ExciterGUI.pas' {FmExciter};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TExciterDataModule.Create(Application) do
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