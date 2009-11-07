{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library StkChorus;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept,// either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  StkChorusDM in 'StkChorusDM.pas' {StkChorusModule: TVSTModule},
  StkChorusGUI in 'StkChorusGUI.pas' {FmStkChorus};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
  try
    with TStkChorusModule.Create(Application) do
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
