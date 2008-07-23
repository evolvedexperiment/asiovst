unit SineSynthModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAVDCommon, DVSTEffect, DVSTModule,
  SineSynthVoice, VoiceList;

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleProcess(inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
  public
    Voices      : TVoiceList;
  end;

implementation

{$R *.DFM}

uses
  SineSynthGUI, Math;

procedure TVSTSSModule.VSTModuleProcess(inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 FillChar(Outputs[0, 0], SampleFrames * SizeOf(Single), 0);
 FillChar(Outputs[1, 0], SampleFrames * SizeOf(Single), 0);

 for j := 0 to SampleFrames - 1 do
  for i := 0 to Voices.Count - 1
   do Outputs[0,j] := Outputs[0, j] + Voices[i].Process;

 for i := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[i, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 FillChar(Outputs[0, 0], SampleFrames * SizeOf(Double), 0);
 FillChar(Outputs[1, 0], SampleFrames * SizeOf(Double), 0);

 for j := 0 to SampleFrames - 1 do
  for i := 0 to Voices.Count - 1
   do Outputs[0, j] := Outputs[0, j] + Voices[i].Process;

 for i := 1 to numOutputs - 1
  do Move(Outputs[0, 0], Outputs[i, 0], SampleFrames * SizeOf(Double));
end;

procedure TVSTSSModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTSSModule.VSTModuleInitialize(Sender: TObject);
begin
 Voices := TVoiceList.Create(True);
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status  : Byte;
  i       : Integer;
  newNote : TSineSynthVoice;
const
  VeloDiv : Single = 1 / 128;
begin
 Status := MidiEvent.midiData[0] and $F0; // channel information is removed
 if (Status = $90) and (MidiEvent.mididata[2] > 0) then // "note on" ?
  begin
   if Voices.Count > 7 then Voices.Remove(Voices.Items[0]);
   newNote := TSineSynthVoice.Create(self);
   with newNote do
    begin
     MidiKeyNr := MidiEvent.midiData[1];
     Velocity  := MidiEvent.midiData[2];
     NoteOn(Midi2Pitch[MidiKeyNr], Velocity * VeloDiv);
    end;
   Voices.Add(newNote);
  end
 else if ((status = $90) and (MidiEvent.mididata[2] = 0)) or (status = $80) then // "note off" ?
  begin
   for i:=0 to Voices.Count-1 do
    begin
     if (Voices.Items[i].MidiKeyNr = MidiEvent.midiData[1]) then
      begin
       Voices.Delete(i);
       Break;
      end;
    end;
  end
 else if (status = $B0) and (MidiEvent.midiData[1] = $7E) then
  begin
   // all notes off
   Voices.Clear;
  end;
end;

procedure TVSTSSModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(Voices);
end;

end.
