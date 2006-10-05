procedure APlugin.MIDI_CC(ch, num, val: integer);
begin
 xevent[0]^.midiData[0]:=$B0+ch;
 xevent[0]^.midiData[1]:=num;
 xevent[0]^.midiData[2]:=val;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_ChannelAftertouch(ch, val: integer);
begin
 xevent[0]^.midiData[0]:=$D0+ch;
 xevent[0]^.midiData[1]:=val;
 xevent[0]^.midiData[2]:=0;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_NoteOff(ch, note, val: integer);
begin
 xevent[0]^.midiData[0]:=$80+ch;
 xevent[0]^.midiData[1]:=note;
 xevent[0]^.midiData[2]:=val;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_NoteOn(ch, note, val: integer);
begin
 xevent[0]^.midiData[0]:=$90+ch;
 xevent[0]^.midiData[1]:=note;
 xevent[0]^.midiData[2]:=val;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_PitchBend(ch, val: integer);
var a,b:integer;
begin
 a:=(val div 128)+64;
 b:=(val div 128);
 b:=val-b*128;
 xevent[0]^.midiData[0]:=$E0+ch;
 xevent[0]^.midiData[1]:=b;
 xevent[0]^.midiData[2]:=a;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_PitchBend2(ch, x1, x2: integer);
begin
 xevent[0]^.midiData[0]:=$E0+ch;
 xevent[0]^.midiData[1]:=x1;
 xevent[0]^.midiData[2]:=x2;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_PolyAftertouch(ch, note, val: integer);
begin
 xevent[0]^.midiData[0]:=$A0+ch;
 xevent[0]^.midiData[1]:=note;
 xevent[0]^.midiData[2]:=val;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;

procedure APlugin.MIDI_ProgramChange(ch, val: integer);
begin
 xevent[0]^.midiData[0]:=$D0+ch;
 xevent[0]^.midiData[1]:=val;
 xevent[0]^.midiData[2]:=0;
 xevent[0]^.deltaframes:=0;
 sendVstEventsToHost(pxevent);
end;
