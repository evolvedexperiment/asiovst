{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversampledTanh;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OversampledTanhModule in 'OversampledTanhModule.pas' {VST2Module1: TVST2Module},
  OversampledTanhGUI in 'OversampledTanhGUI.pas' {FmOversampledTanh};

function main(AudioMasterCallback: TAudioMasterCallbackFunc): PVstEffect; cdecl; export;
begin
 try
  with TOversampledTanhModule.Create(Application) do
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
