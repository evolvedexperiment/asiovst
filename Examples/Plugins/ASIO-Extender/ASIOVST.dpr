{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library ASIOVST;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  ASIOVSTModule in 'ASIOVSTModule.pas' {ASIOVSTModule: TVSTModule},
  ASIOVSTGUI in 'ASIOVSTGUI.pas' {FmASIOVST};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TASIOVSTModule.Create(Application) do
   begin
    AudioMaster := AudioMasterCallback;
    Result := Effect;
   end;
 except
  Result := nil;
 end;
end;

exports
Main name 'main';

begin
end.