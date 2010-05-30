{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library OpAmp;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTModule,
  DAV_VSTEffect,
  DAV_Common,
  OpAmpModule in 'OpAmpModule.pas' {VSTOpAmp: TVSTModule},
  OpAmpGUI in 'OpAmpGUI.pas' {VSTGUI},
  DAV_VSTPlugin_Lazarus;

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTOpAmp : TVSTOpAmp;
begin
 try
  Application.Initialize;
//  AppInitialized := True;

  VSTOpAmp := TVSTOpAmp.Create(Application);
  VSTOpAmp.Effect^.user := VSTOpAmp;
  VSTOpAmp.AudioMaster := audioMaster;
  Result := VSTOpAmp.Effect;
 except
  Result := nil;
 end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.

