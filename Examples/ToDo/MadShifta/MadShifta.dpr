// This is the main project file of the MadShifta source code
// Just boring standard stuff
// Press Ctrl-F12 to access the editor or plugin code units

library MadShifta;

uses
  DAEffect in 'DAEffect.pas',
  DAudioEffect in 'DAudioEffect.pas',
  DVSTUtils in 'DVSTUtils.pas',
  uPlugin in 'uPlugin.pas',
  uEditor in 'uEditor.pas' {PluginEditorWindow};

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
 effect := APlugin.CreateAPlugin(audioMaster);
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
