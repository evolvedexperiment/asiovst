{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library BugpassLite;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  BugpassLiteDM in 'BugpassLiteDM.pas' {BugpassLiteDataModule: TVSTModule},
  BugpassLiteGUI in 'BugpassLiteGUI.pas' {FmBugpassLite};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  BugpassLiteDataModule: TBugpassLiteDataModule;
begin
  try
    BugpassLiteDataModule := TBugpassLiteDataModule.Create(Application);
    BugpassLiteDataModule.Effect^.user := BugpassLiteDataModule;
    BugpassLiteDataModule.AudioMaster := audioMaster;
    Result := BugpassLiteDataModule.Effect;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
