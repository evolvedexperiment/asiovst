unit XSynthGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
  Controls, StdCtrls, MidiKeys, DMidiKeys, DADSRGraph;

type
  TVSTGUI = class(TForm)
    MidiKeys: TMidiKeys;
    GBOSC1: TGroupBox;
    CBOsc1Type: TComboBox;
    LbOsc1Type: TLabel;
    GBOsc2: TGroupBox;
    LbOsc2Type: TLabel;
    CBOsc2Type: TComboBox;
    GBOutput: TGroupBox;
    LbLevel: TLabel;
    SBLevel: TScrollBar;
    Osc1ADSR: TADSRGraph;
    Osc2ADSR: TADSRGraph;
    Label1: TLabel;
    Label2: TLabel;
    LbDrive: TLabel;
    SBDrive: TScrollBar;
    Label3: TLabel;
    SBCutoff: TScrollBar;
    Label4: TLabel;
    SBResonance: TScrollBar;
    Osc1Level: TScrollBar;
    Label5: TLabel;
    Osc2Level: TScrollBar;
    Label6: TLabel;
    procedure MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
    procedure MidiKeysMidiKeyUp(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
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
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

uses XSynthModule, XSynthVoice, VoiceList;

procedure TVSTGUI.MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
var newNote : TXSynthVoice;
const VeloDiv : Single = 1/128;
begin
 if Key<0 then Key:=0 else if Key>119 then Key:=119;
 theModule.MIDI_NoteOn(0,Key,Round(128*Y/Height));
 with newNote do
  begin
   newNote:=TXSynthVoice.Create(theModule);
   MidiKeyNr:=Key;
   Velocity:=Round(128*Y/Height);
   NoteOn(Midi2Pitch[Key],Velocity*VeloDiv);
   (theModule as TVSTSSModule).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.MidiKeysMidiKeyUp(Sender: TObject; Shift: TShiftState; X,
  Y, Key: Integer);
var i : Integer;
begin
 if ssRight in Shift then Exit;
 if Key<0 then Key:=0 else if Key>119 then Key:=119;
 theModule.MIDI_NoteOff(0,Key,128);
 with (theModule as TVSTSSModule) do
  begin
   i:=0;
   while i<Voices.Count do
    if (Voices[i].MidiKeyNr=Key) and not Voices[i].Released then
     begin
      Voices.Items[i].NoteOff;
      Break;
     end else inc(i);
  end;
end;

procedure TVSTGUI.SBDriveChange(Sender: TObject);
begin
 theModule.Parameter[theModule.numParams-4]:=0.1*SBDrive.Position;
end;

procedure TVSTGUI.SBCutoffChange(Sender: TObject);
begin
 theModule.Parameter[theModule.numParams-3]:=FreqLinearToLog(0.01*SBCutoff.Position);
end;

procedure TVSTGUI.SBResonanceChange(Sender: TObject);
begin
 theModule.Parameter[theModule.numParams-2]:=SBResonance.Position;
end;

procedure TVSTGUI.SBLevelChange(Sender: TObject);
begin
 theModule.Parameter[theModule.numParams-1]:=SBLevel.Position;
end;

procedure TVSTGUI.CBOsc1TypeChange(Sender: TObject);
begin
 theModule.Parameter[0]:=CBOsc1Type.ItemIndex;
end;

procedure TVSTGUI.Osc1ADSRAttackChange(Sender: TObject);
begin
 theModule.Parameter[1]:=100*Osc1ADSR.Attack;
end;

procedure TVSTGUI.Osc1ADSRDecayChange(Sender: TObject);
begin
 theModule.Parameter[2]:=100*Osc1ADSR.Decay;
end;

procedure TVSTGUI.Osc1ADSRReleaseChange(Sender: TObject);
begin
 theModule.Parameter[3]:=100*Osc1ADSR.Release;
end;

procedure TVSTGUI.Osc1ADSRSustainChange(Sender: TObject);
begin
 theModule.Parameter[4]:=100*Osc1ADSR.Sustain;
end;

procedure TVSTGUI.Osc1LevelChange(Sender: TObject);
begin
 theModule.Parameter[5]:=Osc1Level.Position;
end;

procedure TVSTGUI.CBOsc2TypeChange(Sender: TObject);
begin
 theModule.Parameter[6]:=CBOsc2Type.ItemIndex;
end;

procedure TVSTGUI.Osc2ADSRAttackChange(Sender: TObject);
begin
 theModule.Parameter[7]:=100*Osc2ADSR.Attack;
end;

procedure TVSTGUI.Osc2ADSRDecayChange(Sender: TObject);
begin
 theModule.Parameter[8]:=100*Osc2ADSR.Decay;
end;

procedure TVSTGUI.Osc2ADSRReleaseChange(Sender: TObject);
begin
 theModule.Parameter[9]:=100*Osc2ADSR.Release;
end;

procedure TVSTGUI.Osc2ADSRSustainChange(Sender: TObject);
begin
 theModule.Parameter[10]:=100*Osc2ADSR.Sustain;
end;

procedure TVSTGUI.Osc2LevelChange(Sender: TObject);
begin
 theModule.Parameter[11]:=Osc2Level.Position;
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
 with (theModule as TVSTSSModule) do
  begin
   for i:=0 to Voices.Count-1 do
    if (Voices[i].MidiKeyNr=Note) then Exit;
   MIDI_NoteOn(0,Note,100);
  end;
 with newNote do
  begin
   newNote:=TXSynthVoice.Create(theModule);
   MidiKeyNr:=Note;
   Velocity:=100;
   NoteOn(Midi2Pitch[Note],Velocity*VeloDiv);
   (theModule as TVSTSSModule).Voices.Add(newNote);
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
 theModule.MIDI_NoteOff(0,Note,100);
 with (theModule as TVSTSSModule) do
  for i:=0 to Voices.Count-1 do
   if (Voices[i].MidiKeyNr=Note) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

end.
