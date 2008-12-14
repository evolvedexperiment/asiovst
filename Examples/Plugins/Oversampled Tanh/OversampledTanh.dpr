{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OversampledTanh;

uses
  Forms,
  DAV_VSTEffect,
  DAV_VSTModule,
  OversampledTanhModule in 'OversampledTanhModule.pas' {VST2Module1: TVST2Module},
  OversampledTanhGUI in 'OversampledTanhGUI.pas' {FmOversampledTanh};

function main(audioMaster: TAudioMasterCallbackFunc): PVstEffect; cdecl; export;
var 
  VSTModule : TVSTModule;
begin
 try
  VSTModule := TOversampledTanhModule.Create(Application);
  VSTModule.Effect^.user := VSTModule;
  VSTModule.AudioMaster := audioMaster;
  Result := VSTModule.Effect;
 except
  Result := nil;
 end;
end;

exports
Main name 'main';

begin
end.
