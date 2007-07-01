unit VocoderGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
  Controls, StdCtrls, DMidiKeys, Graphics;

type
  TVSTGUI = class(TForm)
    MidiKeys: TMidiKeys;
    SBInputLevel: TScrollBar;
    LbInput: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    SBSynthLevel: TScrollBar;
    SBVocoderLevel: TScrollBar;
    procedure MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
    procedure MidiKeysMidiKeyUp(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SBInputLevelChange(Sender: TObject);
    procedure SBSynthLevelChange(Sender: TObject);
    procedure SBVocoderLevelChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses VocoderModule, VocoderVoice, VoiceList;

procedure TVSTGUI.MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
var newNote : TVocoderVoice;
const VeloDiv : Single = 1/128;
begin
 if Key<0 then Key:=0 else if Key>119 then Key:=119;
 TVSTSSModule(Owner).MIDI_NoteOn(0,Key,Round(128*Y/Height));
 with newNote do
  begin
   newNote:=TVocoderVoice.Create(TVSTSSModule(Owner));
   MidiKeyNr:=Key;
   Velocity:=Round(128*Y/Height);
   NoteOn(Midi2Pitch[Key],Velocity*VeloDiv);
   (Owner as TVSTSSModule).Voices.Add(newNote);
  end;
end;

procedure TVSTGUI.MidiKeysMidiKeyUp(Sender: TObject; Shift: TShiftState; X,
  Y, Key: Integer);
var i : Integer;
begin
 if ssRight in Shift then Exit;
 if Key<0 then Key:=0 else if Key>119 then Key:=119;
 TVSTSSModule(Owner).MIDI_NoteOff(0,Key,128);
 with (Owner as TVSTSSModule) do
  for i:=0 to Voices.Count-1 do
   if (Voices[i].MidiKeyNr=Key) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

procedure TVSTGUI.SBInputLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[0]:=SBInputLevel.Position;
end;

procedure TVSTGUI.SBSynthLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[1]:=SBSynthLevel.Position;
end;

procedure TVSTGUI.SBVocoderLevelChange(Sender: TObject);
begin
 TVSTSSModule(Owner).Parameter[2]:=SBVocoderLevel.Position;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var i       : Integer;
    newNote : TVocoderVoice;
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
   newNote:=TVocoderVoice.Create(TVSTSSModule(Owner));
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
