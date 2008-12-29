{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Delayla;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  DelaylaModule in 'DelaylaModule.pas' {SimpleDelayVST: TVST2Module},
  DelaylaGUI in 'DelaylaGUI.pas' {VSTGUI};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 try
  with TSimpleDelayVST.Create(Application) do
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