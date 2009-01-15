{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library SimpleVibrato;

{$R 'Vibrato.res' 'Vibrato.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,
  madLinkDisAsm,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  SimpleVibratoDM in 'SimpleVibratoDM.pas' {SimpleVibratoModule: TVSTModule},
  SimpleVibratoGUI in 'SimpleVibratoGUI.pas' {FmSimpleVibrato};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSimpleVibratoModule.Create(Application) do
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
