unit MIDIPlugInModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTEffect, DVSTModule;

type
  TMIDIModule = class(TVSTModule)
    procedure MIDIModuleParameterProperties0ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessMidi(Sender: TObject;
      MidiEvent: TVstMidiEvent);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
  end;

implementation

{$R *.DFM}

uses MIDIPlugInGUI;

procedure TMIDIModule.MIDIModuleParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 with (EditorForm As TVSTGUI) do
  begin
   Label3.Caption := 'transpose: ' + inttostr(round(Value));

   // Update Scrollbar, if necessary
   if par0.Position<>round(Value) then par0.Position:=round(Value); 
  end;
end;

procedure TMIDIModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TMIDIModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var newnote, time, data1, data2, status, channel: integer;
begin
 channel := MidiEvent.midiData[0] and $0F;
 status := MidiEvent.midiData[0] and $F0;
 data1 := MidiEvent.midiData[1] and $7F;
 data2 := MidiEvent.midiData[2] and $7F;
 time := MidiEvent.deltaFrames;
 // example MIDI code:
 if (status = $90) and (data2 > 0) then // "Note On" ?
 begin
  // data1 contains note number
  // data2 contains note velocity
  newnote := round(f_limit(data1 + Parameter[0], 0, 120));
  MIDI_NoteOn(channel, newnote, data2, time);
 end
 else if ((status = $90) and (data2 = 0)) or
  (status = $80) then // "Note Off" ?
 begin
  // data1 contains note number
  // data2 contains note off velocity
  // send "Note Off" back to host (MIDI thru)
  newnote := round(f_limit(data1 + Parameter[0], 0, 120));
  MIDI_NoteOff(channel, newnote, data2, time);
 end
 else if (status = $A0) then // "Polyphonic Aftertouch" ?
 begin
  // data1 contains note number
  // data2 contains aftertouch value
 end
 else if (status = $B0) then // "MIDI Controller" ?
 begin
  // data1 contains CC number
  // data2 contains data value
 end
 else if (status = $C0) then // "Program Change" ?
 begin
  // data1 contains program number
 end
 else if (status = $D0) then // "Channel Aftertouch" ?
 begin
  // data1 contains channel aftertouch value
 end
 else if (status = $E0) then // "Pitchbend" ?
 begin
  // data1 and data2 make up the 12 bit pitchbend value
 end;
end;

end.
