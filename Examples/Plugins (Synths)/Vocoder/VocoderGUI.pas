unit VocoderGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, Graphics,
  DAV_Types, DAV_VSTModule, DAV_GuiMidiKeys, DAV_GuiBaseControl;

type
  TVSTGUI = class(TForm)
    MidiKeys: TGuiMidiKeys;
    SBInputLevel: TScrollBar;
    LbInput: TLabel;
    LbSynthLevel: TLabel;
    LbVocoderLevel: TLabel;
    SBSynthLevel: TScrollBar;
    SBVocoderLevel: TScrollBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SBInputLevelChange(Sender: TObject);
    procedure SBSynthLevelChange(Sender: TObject);
    procedure SBVocoderLevelChange(Sender: TObject);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNo: Byte; Velocity: Single);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNo: Byte);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateSynthVolume;
    procedure UpdateInputVolume;
    procedure UpdateVocoderVolume;
  end;

implementation

{$R *.DFM}

uses
  DAV_SynthUtils, VocoderModule, VocoderVoice;

{ TVSTGUI }

procedure TVSTGUI.FormShow(Sender: TObject);
begin
  UpdateSynthVolume;
  UpdateInputVolume;
  UpdateVocoderVolume;
end;

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNo: Byte;
  Velocity: Single);
var
  newNote: TVocoderVoice;
begin 
  TVSTSSModule(Owner).MidiNoteOn(0, KeyNo, round(Velocity * 128));
  newNote := TVocoderVoice.Create(TVSTSSModule(Owner));
  newNote.MidiKey := KeyNo;
  newNote.Velocity := round(Velocity * 127);
  newNote.NoteOn(Midi2Pitch[KeyNo], Velocity);
  TVSTSSModule(Owner).Voices.Add(newNote);
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNo: Byte);
var
  i: Integer;
begin
  TVSTSSModule(Owner).MidiNoteOff(0, KeyNo, 0);
  with TVSTSSModule(Owner) do
    for i := Voices.Count - 1 downto 0 do
     if (Voices[i].MidiKey = KeyNo) then
      begin
        Voices.Delete(i);
        Break;
      end;
end;

procedure TVSTGUI.SBInputLevelChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[0] := SBInputLevel.Position;
end;

procedure TVSTGUI.SBSynthLevelChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[1] := SBSynthLevel.Position;
end;

procedure TVSTGUI.SBVocoderLevelChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[2] := SBVocoderLevel.Position;
end;

procedure TVSTGUI.UpdateInputVolume;
begin
  with TVSTSSModule(Owner) do
  begin
    SBInputLevel.Position := round(Parameter[0]);
  end;
end;

procedure TVSTGUI.UpdateSynthVolume;
begin
  with TVSTSSModule(Owner) do
  begin
    SBSynthLevel.Position := round(Parameter[1]);
  end;
end;

procedure TVSTGUI.UpdateVocoderVolume;
begin
  with TVSTSSModule(Owner) do
  begin
    SBVocoderLevel.Position := round(Parameter[2]);
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  newNote: TVocoderVoice;
  Note: Byte;
const
  CVeloDiv: Single = 1/128;
begin
  Note := KeyToNote(Key);
  if Note = -1 then
    Exit;

  Assert(Owner is TVSTSSModule);

  with TVSTSSModule(Owner) do
  begin
    for i := 0 to Voices.Count - 1 do
      if (Voices[i].MidiKey = Note) then
        Exit;
    MidiNoteOn(0, Note, 100);
  end;

  with newNote do
  begin
    newNote := TVocoderVoice.Create(TVSTSSModule(Owner));
    MidiKey := Note;
    Velocity := 100;
    NoteOn(Midi2Pitch[Note], Velocity * CVeloDiv);
    TVSTSSModule(Owner).Voices.Add(newNote);
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

  TVSTSSModule(Owner).MidiNoteOff(0, Note, 100);
  with TVSTSSModule(Owner) do
    for i := 0 to Voices.Count - 1 do
      if (Voices[i].MidiKey = Note) then
      begin
        Voices.Delete(i);
        Break;
      end;
end;

end.
