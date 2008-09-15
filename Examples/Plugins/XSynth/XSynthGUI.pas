unit XSynthGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule, DAV_GuiADSRGraph, DAV_GuiBaseControl, DAV_GuiMidiKeys;

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
    procedure MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
      Velocity: Single);
    procedure MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
  end;

implementation

{$R *.DFM}

uses XSynthModule, XSynthVoice, VoiceList;  

procedure TVSTGUI.MidiKeysNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
var newNote : TXSynthVoice;
begin
  (Owner as TVSTSSModule).MIDI_NoteOn(0,KeyNr,round(Velocity*128));
  newNote:=TXSynthVoice.Create((Owner as TVSTSSModule));
  newNote.MidiKeyNr:=KeyNr;
  newNote.Velocity:=round(Velocity*127);
  newNote.NoteOn(Midi2Pitch[KeyNr],Velocity);
  (Owner as TVSTSSModule).Voices.Add(newNote);
end;

procedure TVSTGUI.MidiKeysNoteOff(Sender: TObject; KeyNr: Byte);
var i : Integer;
begin
  (Owner as TVSTSSModule).MIDI_NoteOff(0,KeyNr,0);
  with (Owner as TVSTSSModule) do
  for i:=Voices.Count-1 downto 0 do
   if (Voices[i].MidiKeyNr=KeyNr) and not Voices[i].Released then
    begin
     Voices.Items[i].NoteOff;
     Break;
    end;
end;

procedure TVSTGUI.SBDriveChange(Sender: TObject);
begin
 with TVSTSSModule(Owner) do Parameter[numParams-4]:=0.1*SBDrive.Position;
end;

procedure TVSTGUI.SBCutoffChange(Sender: TObject);
begin
 with TVSTSSModule(Owner) do Parameter[numParams-3]:=FreqLinearToLog(0.01*SBCutoff.Position);
end;

procedure TVSTGUI.SBResonanceChange(Sender: TObject);
begin
 with TVSTSSModule(Owner) do Parameter[numParams-2]:=SBResonance.Position;
end;

procedure TVSTGUI.SBLevelChange(Sender: TObject);
begin
 with TVSTSSModule(Owner) do Parameter[numParams-1]:=SBLevel.Position;
end;

procedure TVSTGUI.CBOsc1TypeChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[0]:=CBOsc1Type.ItemIndex;
end;

procedure TVSTGUI.Osc1ADSRAttackChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[1]:=100*Osc1ADSR.Attack;
end;

procedure TVSTGUI.Osc1ADSRDecayChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[2]:=100*Osc1ADSR.Decay;
end;

procedure TVSTGUI.Osc1ADSRReleaseChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[3]:=100*Osc1ADSR.Release;
end;

procedure TVSTGUI.Osc1ADSRSustainChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[4]:=100*Osc1ADSR.Sustain;
end;

procedure TVSTGUI.Osc1LevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[5]:=Osc1Level.Position;
end;

procedure TVSTGUI.CBOsc2TypeChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[6]:=CBOsc2Type.ItemIndex;
end;

procedure TVSTGUI.Osc2ADSRAttackChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[7]:=100*Osc2ADSR.Attack;
end;

procedure TVSTGUI.Osc2ADSRDecayChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[8]:=100*Osc2ADSR.Decay;
end;

procedure TVSTGUI.Osc2ADSRReleaseChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[9]:=100*Osc2ADSR.Release;
end;

procedure TVSTGUI.Osc2ADSRSustainChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[10]:=100*Osc2ADSR.Sustain;
end;

procedure TVSTGUI.Osc2LevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[11]:=Osc2Level.Position;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var i       : Integer;
    newNote : TXSynthVoice;
    Note    : Byte;
const VeloDiv : Single = 1/128;
begin
 case Key of
  89  : Note:=60;
  83  : Note:=61;
  88  : Note:=62;
  68  : Note:=63;
  67  : Note:=64;
  86  : Note:=65;
  71  : Note:=66;
  66  : Note:=67;
  72  : Note:=68;
  78  : Note:=69;
  74  : Note:=70;
  77  : Note:=71;
  188 : Note:=72;
  81  : Note:=72;
  87  : Note:=74;
  69  : Note:=76;
  82  : Note:=77;
  else Exit;
 end;
 with (Owner as TVSTSSModule) do
  begin
   for i:=0 to Voices.Count-1 do
    if (Voices[i].MidiKeyNr=Note) then Exit;
   MIDI_NoteOn(0,Note,100);
  end;
 with newNote do
  begin
   newNote:=TXSynthVoice.Create(TVSTSSModule(Owner));
   MidiKeyNr:=Note;
   Velocity:=100;
   NoteOn(Midi2Pitch[Note],Velocity*VeloDiv);
   (Owner as TVSTSSModule).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var i    : Integer;
    Note : Byte;
begin
 case Key of
  89  : Note:=60;
  83  : Note:=61;
  88  : Note:=62;
  68  : Note:=63;
  67  : Note:=64;
  86  : Note:=65;
  71  : Note:=66;
  66  : Note:=67;
  72  : Note:=68;
  78  : Note:=69;
  74  : Note:=70;
  77  : Note:=71;
  188 : Note:=72;
  81  : Note:=72;
  87  : Note:=74;
  69  : Note:=76;
  82  : Note:=77;
  else Exit;
 end;
 TVSTSSModule(Owner).MIDI_NoteOff(0,Note,100);
 with (Owner as TVSTSSModule) do
  for i:=0 to Voices.Count-1 do
   if (Voices[i].MidiKeyNr=Note) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

end.
