unit SimpleSamplerGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
     Controls, StdCtrls, DMidiKeys, Graphics, Dialogs, WaveIOX;

type
  TVSTGUI = class(TForm)
    MidiKeys: TMidiKeys;
    EditSample: TEdit;
    Label1: TLabel;
    BtSampleSelect: TButton;
    OpenDialog: TOpenDialog;
    procedure MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
    procedure MidiKeysMidiKeyUp(Sender: TObject; Shift: TShiftState; X, Y, Key: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtSampleSelectClick(Sender: TObject);
    procedure EditSampleChange(Sender: TObject);
    procedure MidiKeysKeyColor(Sender: TObject; Key: Integer;
      var Color: TColor);
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

uses SimpleSamplerModule, SimpleSamplerVoice, VoiceList;

procedure TVSTGUI.MidiKeysKeyColor(Sender: TObject; Key: Integer;
  var Color: TColor);
begin
 if Key=60
  then Color:=$00DDEEFF
  else Color:=clWhite;
end;

procedure TVSTGUI.MidiKeysMidiKeyDown(Sender: TObject; Shift: TShiftState;
  X, Y, Key: Integer);
var newNote : TSimpleSamplerVoice;
const VeloDiv : Single = 1/128;
begin
 if Key<0 then Key:=0 else if Key>119 then Key:=119;
 theModule.MIDI_NoteOn(0,Key,128);
 with newNote do
  begin
   newNote:=TSimpleSamplerVoice.Create(theModule);
   MidiKeyNr:=Key;
   Velocity:=100;
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
  for i:=0 to Voices.Count-1 do
   if (Voices[i].MidiKeyNr=Key) then
    begin
     Voices.Delete(i);
     Break;
    end;
end;

procedure TVSTGUI.BtSampleSelectClick(Sender: TObject);
begin
 if OpenDialog.Execute then
  begin
   EditSample.Text:=OpenDialog.FileName;
  end;
end;

procedure TVSTGUI.EditSampleChange(Sender: TObject);
var sr,c,sz : Integer;
    pt      : PSingle;
begin
 if FileExists(EditSample.Text) then
  begin
   pt:=LoadWAVFileMono(EditSample.Text,sr, c, sz);
   SetLength(TVSTSSModule(theModule).Sample,sz);
   for c := 0 to sz - 1 do
    begin
     TVSTSSModule(theModule).Sample[c]:=(pt)^;
     Inc(pt);
    end;
  end;
end;

procedure TVSTGUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var i       : Integer;
    newNote : TSimpleSamplerVoice;
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
   newNote:=TSimpleSamplerVoice.Create(theModule);
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
