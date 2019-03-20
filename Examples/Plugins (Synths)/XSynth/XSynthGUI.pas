unit XSynthGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiADSRGraph, DAV_GuiBaseControl, DAV_GuiMidiKeys,
  DAV_GuiCustomControl;

type
  TVSTGUI = class(TForm)
    CBOsc1Type: TComboBox;
    CBOsc2Type: TComboBox;
    GBOSC1: TGroupBox;
    GBOsc2: TGroupBox;
    GBOutput: TGroupBox;
    LbOsc1ADSR: TLabel;
    LbOsc2ADSR: TLabel;
    LbCutOff: TLabel;
    LbResonance: TLabel;
    LbOsc1Level: TLabel;
    LbOsc2Level: TLabel;
    LbDrive: TLabel;
    LbLevel: TLabel;
    LbOsc1Type: TLabel;
    LbOsc2Type: TLabel;
    MidiKeys: TGuiMidiKeys;
    Osc1ADSR: TGuiADSRGraph;
    Osc1Level: TScrollBar;
    Osc2ADSR: TGuiADSRGraph;
    Osc2Level: TScrollBar;
    SBCutoff: TScrollBar;
    SBDrive: TScrollBar;
    SBLevel: TScrollBar;
    SBResonance: TScrollBar;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SBLevelChange(Sender: TObject);
    procedure CBOsc1TypeChange(Sender: TObject);
    procedure CBOsc2TypeChange(Sender: TObject);
    procedure SBDriveChange(Sender: TObject);
    procedure SBCutoffChange(Sender: TObject);
    procedure SBResonanceChange(Sender: TObject);
    procedure Osc1ADSRAttackChange(Sender: TObject);
    procedure Osc1ADSRDecayChange(Sender: TObject);
    procedure Osc1ADSRReleaseChange(Sender: TObject);
    procedure Osc1ADSRSustainChange(Sender: TObject);
    procedure Osc1LevelChange(Sender: TObject);
    procedure Osc2ADSRAttackChange(Sender: TObject);
    procedure Osc2ADSRDecayChange(Sender: TObject);
    procedure Osc2ADSRReleaseChange(Sender: TObject);
    procedure Osc2ADSRSustainChange(Sender: TObject);
    procedure Osc2LevelChange(Sender: TObject);
    procedure MidiKeysNoteOn(Sender: TObject; KeyNo: Byte;
      Velocity: Single);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNo: Byte);
  end;

implementation

{$R *.DFM}

uses
  DAV_Common, DAV_Convert, DAV_SynthUtils, XSynthModule, XSynthVoice;

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNo: Byte;
  Velocity: Single);
var
  newNote: TXSynthVoice;
begin
  Assert(Owner is TVSTSSModule);
  TVSTSSModule(Owner).MidiNoteOn(0, KeyNo, Round(Velocity * 128));
  newNote := TXSynthVoice.Create(TVSTSSModule(Owner));
  newNote.MidiKey := KeyNo;
  newNote.Velocity := Round(Velocity * 127);
  newNote.NoteOn(Midi2Pitch[KeyNo], Velocity);
  TVSTSSModule(Owner).Voices.Add(newNote);
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNo: Byte);
var
  i: Integer;
begin
  Assert(Owner is TVSTSSModule);
  TVSTSSModule(Owner).MidiNoteOff(0, KeyNo, 0);
  with TVSTSSModule(Owner) do
    for i := Voices.Count - 1 downto 0 do
      if (Voices[i].MidiKey = KeyNo) and not Voices[i].Released then
      begin
        Voices.Items[i].NoteOff;
        Break;
      end;
end;

procedure TVSTGUI.SBDriveChange(Sender: TObject);
begin
  with TVSTSSModule(Owner) do
    Parameter[numParams - 4] := 0.1 * SBDrive.Position;
end;

procedure TVSTGUI.SBCutoffChange(Sender: TObject);
begin
  with TVSTSSModule(Owner) do
    Parameter[numParams - 3] := FreqLinearToLog(0.01 * SBCutoff.Position);
end;

procedure TVSTGUI.SBResonanceChange(Sender: TObject);
begin
  with TVSTSSModule(Owner) do
    Parameter[numParams - 2] := SBResonance.Position;
end;

procedure TVSTGUI.SBLevelChange(Sender: TObject);
begin
  with TVSTSSModule(Owner) do
    Parameter[numParams - 1] := SBLevel.Position;
end;

procedure TVSTGUI.CBOsc1TypeChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[0] := CBOsc1Type.ItemIndex;
end;

procedure TVSTGUI.Osc1ADSRAttackChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[1] := 100 * Osc1ADSR.Attack;
end;

procedure TVSTGUI.Osc1ADSRDecayChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[2] := 100 * Osc1ADSR.Decay;
end;

procedure TVSTGUI.Osc1ADSRReleaseChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[3] := 100 * Osc1ADSR.Release;
end;

procedure TVSTGUI.Osc1ADSRSustainChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[4] := 100 * Osc1ADSR.Sustain;
end;

procedure TVSTGUI.Osc1LevelChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[5] := Osc1Level.Position;
end;

procedure TVSTGUI.CBOsc2TypeChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[6] := CBOsc2Type.ItemIndex;
end;

procedure TVSTGUI.Osc2ADSRAttackChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[7] := 100 * Osc2ADSR.Attack;
end;

procedure TVSTGUI.Osc2ADSRDecayChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[8] := 100 * Osc2ADSR.Decay;
end;

procedure TVSTGUI.Osc2ADSRReleaseChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[9] := 100 * Osc2ADSR.Release;
end;

procedure TVSTGUI.Osc2ADSRSustainChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[10] := 100 * Osc2ADSR.Sustain;
end;

procedure TVSTGUI.Osc2LevelChange(Sender: TObject);
begin
  TVSTSSModule(Owner).Parameter[11] := Osc2Level.Position;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  newNote: TXSynthVoice;
  Note: Byte;
const
  CVeloDiv: Single = 1 / 128;
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
  end;

  with newNote do
  begin
    newNote := TXSynthVoice.Create(TVSTSSModule(Owner));
    MidiKey := Note;
    Velocity := 100;
    NoteOn(Midi2Pitch[Note],Velocity * CVeloDiv);
    TVSTSSModule(Owner).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  Note: Byte;
begin
  if not KeyToNote(Key, Note) then
    Exit;

  Assert(Owner is TVSTSSModule);

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
