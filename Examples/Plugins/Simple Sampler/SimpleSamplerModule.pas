unit SimpleSamplerModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTEffect, 
  DVSTModule, SimpleSamplerVoice, VoiceList;

type
  TVSTSSModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleProcessMidi(Sender: TObject;
      MidiEvent: TVstMidiEvent);
    procedure VSTModuleDestroy(Sender: TObject);
  private
  public
    Voices      : TVoiceList;
    Sample      : TAVDSingleDynArray;
  end;

implementation

{$R *.DFM}

uses SimpleSamplerGUI, Math;

procedure TVSTSSModule.VST_EditOpen(Sender: TObject; var GUI: TForm);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTSSModule.VSTModuleProcess(const inputs,
  outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i,j : Integer;
begin
 FillChar(outputs[0,0],sampleframes*SizeOf(Single),0);
 FillChar(outputs[1,0],sampleframes*SizeOf(Single),0);

 for j:=0 to sampleframes-1 do
  for i:=0 to Voices.Count-1
   do outputs[0,j]:=outputs[0,j]+Voices[i].Process;

 for i:=1 to numOutputs-1
  do Move(outputs[0,0], outputs[i,0], sampleframes * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleInitialize(Sender: TObject);
begin
 Voices:=TVoiceList.Create(True);
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var Status  : Byte;
    i       : Integer;
    newNote : TSimpleSamplerVoice;
const VeloDiv : Single = 1/128;
begin
 Status:=MidiEvent.midiData[0] and $F0; // channel information is removed
 if (Status=$90) and (MidiEvent.mididata[2]>0) then // "note on" ?
  begin
   if Voices.Count>7 then Voices.Remove(Voices.Items[0]);
   newNote:=TSimpleSamplerVoice.Create(self);
   with newNote do
    begin
     newNote.MidiKeyNr:=MidiEvent.midiData[1];
     newNote.Velocity:=MidiEvent.midiData[2];
     newNote.NoteOn(Midi2Pitch[MidiKeyNr],Velocity*VeloDiv);
    end;
   Voices.Add(newNote);
  end
 else if ((status=$90) and (MidiEvent.mididata[2]=0)) or (status=$80) then // "note off" ?
  begin
   for i:=0 to Voices.Count-1 do
    begin
     if (Voices.Items[i].MidiKeyNr=MidiEvent.midiData[1]) then
      begin
       Voices.Delete(i);
       Break;
      end;
    end;
  end
 else if ((status=$B0) and (MidiEvent.midiData[1]=$7e)) then
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
