{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library DMOPack2;
// This is the main project, you normally should not change
// anything here!
// Press Ctrl-F12 to access the main files!

{%File 'config.inc'}

uses
  uPlugin in 'uPlugin.pas',
  uEditor in 'uEditor.pas' {PluginEditorWindow},
  DAEffect;

var
   Effect : APlugin;
   Oome   : Boolean;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
 // get vst version
 if audioMaster(nil,audioMasterVersion,0,0,nil,0)=0 then
 begin
  Result:=nil;
  Exit; // Old version
 end;
 effect:=APlugin.CreateAPlugin(audioMaster);
 if not Assigned(effect) then
 begin
  Result:=nil;
  Exit;
 end;
 if oome then
 begin
  Effect.Free;
  Result:=nil;
  Exit;
 end;
 Result:=effect.effect;
end;

exports
Main name 'main';

begin
end.
