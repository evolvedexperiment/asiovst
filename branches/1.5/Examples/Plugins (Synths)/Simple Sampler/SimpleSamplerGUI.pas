unit SimpleSamplerGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  Dialogs, DAV_Types, DAV_VSTModule, DAV_GuiStaticWaveform,
  DAV_GuiBaseControl, DAV_GuiMidiKeys;

type
  TVSTGUI = class(TForm)
    MidiKeys: TGuiMidiKeys;
    EditSample: TEdit;
    LbSample: TLabel;
    BtSampleSelect: TButton;
    OpenDialog: TOpenDialog;
    Waveform: TGuiStaticWaveform;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtSampleSelectClick(Sender: TObject);
    procedure EditSampleChange(Sender: TObject);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNr: Byte; Velocity: Single);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
  end;

implementation

{$R *.DFM}

uses
  DAV_SynthUtils, SimpleSamplerModule, SimpleSamplerVoice;


{ TVSTGUI }

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
var
  newNote: TSimpleSamplerVoice;
begin
  with TVSTSSModule(Owner) do
  begin
    MidiNoteOn(0, KeyNr, Round(Velocity * 128));
    newNote := TSimpleSamplerVoice.Create(TVSTSSModule(Owner));
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

procedure TVSTGUI.BtSampleSelectClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    EditSample.Text := OpenDialog.FileName;
end;

procedure TVSTGUI.EditSampleChange(Sender: TObject);
begin
  if FileExists(EditSample.Text) then
  begin
    TVSTSSModule(Owner).LoadFromFile(EditSample.Text);
    with TVSTSSModule(Owner) do
      Waveform.SetWaveForm(Sample, SampleLength, True);
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  newNote: TSimpleSamplerVoice;
  Note: Byte;
const
  VeloDiv: Single = 1/128;
begin
  Note := KeyToNote(Key);
  if Note = -1 then
    Exit;

  with (Owner as TVSTSSModule) do
  begin
    for i := 0 to Voices.Count - 1 do
      if (Voices[i].MidiKey = Note) then
        Exit;
    MidiNoteOn(0, Note, 100);
  end;

  with newNote do
  begin
    newNote := TSimpleSamplerVoice.Create((Owner as TVSTSSModule));
    MidiKey := Note;
    Velocity := 100;
    NoteOn(Midi2Pitch[Note], Velocity * VeloDiv);
    (Owner as TVSTSSModule).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  Note: Byte;
begin
  Note := KeyToNote(Key);
  if Note = -1 then
    Exit;

  with (Owner as TVSTSSModule) do
  begin
    MidiNoteOff(0, Note, 100);
    for i := 0 to Voices.Count - 1 do
      if (Voices[i].MidiKey = Note) then
      begin
        Voices.Delete(i);
        Break;
      end;
  end;
end;

procedure TVSTGUI.FormCreate(Sender: TObject);
begin
  MidiKeys.SetKeyColor(60, 60, $00DDEEFF, $00EEFFFF, $00BBCCE6);
end;

end.
