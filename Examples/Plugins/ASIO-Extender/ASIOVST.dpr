{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ASIOVST;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ASIOVSTModule in 'ASIOVSTModule.pas' {ASIOVSTModule: TVSTModule},
  ASIOVSTGUI in 'ASIOVSTGUI.pas' {FmASIOVST};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTModule : TASIOVSTModule;
begin
 try
  VSTModule := TASIOVSTModule.Create(Application);
  VSTModule.AudioMaster := audiomaster;
  VSTModule.Effect^.user:=VSTModule;
  Result := VSTModule.Effect;
 except
  Result := nil;
 end;
end;

exports
Main name 'main';

begin
end.