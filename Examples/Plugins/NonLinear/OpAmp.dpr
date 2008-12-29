{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OpAmp;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OpAmpModule in 'OpAmpModule.pas' {VSTOpAmp: TVSTModule},
  OpAmpGUI in 'OpAmpGUI.pas' {VSTGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TVSTOpAmp.Create(Application) do
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

