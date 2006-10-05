{
This is open source, but when you use it, please credit
Alex (alex@smartelectronix.com) for the filter code and
me (tobybear@web.de) for the rest :-)

What you can learn from this thing:
- formant filter code
- simple synthesizer architecture
- simple envelope control
- glide
- note-to-frequency conversion
- synth and effect plugin within one project

Press CTRL-F12 now to access
 uPlugin.pas (the algorithm)
 uEditor.pas (the GUI)
}

library VowelSynth;

uses
  DAEffect,
  DVSTUtils in 'dVSTUtils.pas',
  uPlugin in 'uPlugin.pas',
  uEditor in 'uEditor.pas';

var Effect : APlugin;
    Oome   : Boolean;

function main(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl; export;
begin
 if audioMaster(nil,audioMasterVersion,0,0,nil,0)=0 then
 begin
  Result:=nil;
  Exit;
 end;
 effect := APlugin.Create(audioMaster);
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
