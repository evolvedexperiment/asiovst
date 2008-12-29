{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleChorus;

{$R 'Chorus.res' 'Chorus.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  FastMove,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleChorusDM in 'SimpleChorusDM.pas' {SimpleChorusModule: TVSTModule},
  SimpleChorusGUI in 'SimpleChorusGUI.pas' {FmSimpleChorus};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSimpleChorusModule.Create(Application) do
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