{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BugpassLite;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  BugpassLiteDM in 'BugpassLiteDM.pas' {BugpassLiteDataModule: TVSTModule},
  BugpassLiteGUI in 'BugpassLiteGUI.pas' {FmBugpassLite};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TBugpassLiteDataModule.Create(Application) do
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
