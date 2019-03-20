unit SineSynthGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  DAV_Types, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiMidiKeys;

type
  TVSTGUI = class(TForm)
    MidiKeys: TGuiMidiKeys;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNr: Byte; Velocity: Single);
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  DAV_SynthUtils, SineSynthModule, SineSynthVoice;

{ TVSTGUI }

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
var
  newNote: TSineSynthVoice;
begin
  with TVSTSSModule(Owner) do
  begin
    MidiNoteOn(0, KeyNr, Round(Velocity * 128));
    newNote := TSineSynthVoice.Create(TVSTSSModule(Owner));
    newNote.MidiKey := KeyNr;
    newNote.Velocity := Round(Velocity * 127);
    newNote.NoteOn(Midi2Pitch[KeyNr], Velocity);
    Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
var
  i: Integer;
begin
  with TVSTSSModule(Owner) do
  begin
    MidiNoteOff(0, KeyNr, 0);
    for i := Voices.Count - 1 downto 0 do
      if (Voices[i].MidiKey = KeyNr) then
      begin
        Voices.Delete(i);
        Break;
      end;
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  newNote: TSineSynthVoice;
  Note: Byte;
const
  CVeloDiv: Single = 1/128;
begin
  if not KeyToNote(Key, Note) then
    Exit;

  Assert(Owner is TVSTSSModule);

  with TVSTSSModule(Owner) do
  begin
    for i := 0 to Voices.Count - 1 do
      if (Voices[i].MidiKey = Note) then
        Exit;
    MidiNoteOn(0, Note, 100);
    newNote := TSineSynthVoice.Create(TVSTSSModule(Owner));
    newNote.MidiKey := Note;
    newNote.Velocity := 100;
    newNote.NoteOn(Midi2Pitch[Note], newNote.Velocity * CVeloDiv);
    Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  Note: Byte;
begin
  if not KeyToNote(Key, Note) then
    Exit;

  if Owner is TVSTSSModule then
    with TVSTSSModule(Owner) do
    begin
      MidiNoteOff(0, Note, 100);
      for i := 0 to Voices.Count - 1 do
        if (Voices[i].MidiKey=Note) then
        begin
          Voices.Delete(i);
          Break;
        end;
   end;
end;

end.
