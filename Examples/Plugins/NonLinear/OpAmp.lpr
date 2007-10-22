{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OpAmp;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  DVSTModule,
  DVSTEffect,
  DAVDCommon,
  OpAmpModule in 'OpAmpModule.pas' {VSTOpAmp: TVSTModule},
  OpAmpGUI in 'OpAmpGUI.pas' {VSTGUI};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTOpAmp : TVSTOpAmp;
begin
 try
  VSTOpAmp:=TVSTOpAmp.Create(Application);
  VSTOpAmp.Effect^.user:=VSTOpAmp;
  VSTOpAmp.AudioMaster:=audioMaster;
  Result := VSTOpAmp.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

