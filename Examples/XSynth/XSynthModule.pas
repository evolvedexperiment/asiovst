unit XSynthModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase,
     DVSTEffect, DVSTModule, XSynthVoice, VoiceList;

type
  TVSTSSModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleProcess(const inputs, outputs: TArrayOfSingleArray; sampleframes: Integer);
    procedure VSTModuleInitialize(Sender: TObject);
    procedure VSTModuleProcessMidi(Sender: TObject;
      MidiEvent: TVstMidiEvent);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);

    procedure VSTSSModuleLevelParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1TypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2TypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleDriveParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleCutoffParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleResonanceParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1DecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1SustainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc1LevelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2AttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2DecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2ReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2SustainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTSSModuleOsc2LevelChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fLevel  : Single;
    fDrive  : Single;
    fCutoff : Array[0..1] of Single;
    fRes    : Array[0..1] of Single;
    fOld    : Array[0..1] of Single;
    fOscs   : Array[0..1] of TOsc;
    function GetOscilators(index: integer): TOsc;
  public
    Voices      : TVoiceList;
    property Oscilators[index:integer] : TOsc read GetOscilators;
  end;

implementation

{$R *.DFM}

uses XSynthGUI, Math;

procedure TVSTSSModule.VST_EditOpen(Sender: TObject; var GUI: TForm);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(nil);
 with (GUI As TVSTGUI) do
  begin
   theModule:=Self;
  end;
end;

procedure TVSTSSModule.VSTModuleProcess(const inputs,
  outputs: TArrayOfSingleArray; sampleframes: Integer);
var i,j : Integer;
    fb  : single;
const kDenorm : Single = 1E-30;    
begin

 for j:=0 to sampleframes-1 do
  begin
   outputs[0,j]:=0; i:=0;
   while i<Voices.Count do
    begin
     outputs[0,j]:=outputs[0,j]+Voices[i].Process;
     inc(i);
    end;
  end;

 if fDrive>1 then
  for j:=0 to sampleframes-1 do
   begin
    outputs[0,j]:=Tanh2c(fDrive*outputs[0,j]);
   end;

 fCutOff[1] := 0.9 * fCutOff[1] + 0.1 * fCutOff[0];
 fRes[1] := 0.9 * fRes[1] + 0.1 * fRes[0];

 fb := fRes[1] + fRes[1] / (1 - fCutOff[1] * 0.9);
 for j:=0 to sampleframes-1 do
  begin
   fOld[0] := fOld[0] + fCutOff[1] * (outputs[0,j] - fOld[0] + fb * (fOld[0] - fOld[1])) + kDenorm;
   fOld[1] := fOld[1] + fCutOff[1] * (fOld[0] - fOld[1]);
   outputs[0,j] := fLevel*fOld[1];
  end;

 for i:=1 to numOutputs-1
  do Move(outputs[0,0], outputs[i,0], sampleframes * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleInitialize(Sender: TObject);
begin
 Voices:=TVoiceList.Create(True);
end;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
 fLevel:=1; fDrive:=1;
 ParameterProperties[0].Max:=Integer(otNoise);
 ParameterProperties[1].Max:=Integer(otNoise);
 fOscs[0].OType:=otSine;
 fOscs[0].Attack:=0.5;
 fOscs[0].Decay:=0.5;
 fOscs[0].Sustain:=0.5;
 fOscs[0].Release:=0.5;
 fOscs[0].Level:=1;
 fOscs[1].OType:=otNone;
 fOscs[1].Attack:=0.5;
 fOscs[1].Decay:=0.5;
 fOscs[1].Sustain:=0.5;
 fOscs[1].Release:=0.5;
 fOscs[1].Level:=1;
 fCutOff[0]:=0.5;
 fCutOff[1]:=0.5;
 fRes[0]:=0.1;
 fRes[1]:=0.1;
 with Programs[0] do
  begin
   Parameter[0]:=1;
   Parameter[1]:=50;
   Parameter[2]:=50;
   Parameter[3]:=50;
   Parameter[4]:=50;
   Parameter[5]:=100;
   Parameter[6]:=0;
   Parameter[7]:=50;
   Parameter[8]:=50;
   Parameter[9]:=50;
   Parameter[10]:=50;
   Parameter[11]:=100;
   Parameter[12]:=1;
   Parameter[13]:=20000;
   Parameter[14]:=1;
   Parameter[15]:=100;
  end;
 with Programs[1] do
  begin
   Parameter[0]:=4;
   Parameter[1]:=50;
   Parameter[2]:=50;
   Parameter[3]:=50;
   Parameter[4]:=50;
   Parameter[5]:=20;
   Parameter[6]:=2;
   Parameter[7]:=15;
   Parameter[8]:=50;
   Parameter[9]:=50;
   Parameter[10]:=50;
   Parameter[11]:=100;
   Parameter[12]:=8;
   Parameter[13]:=8000;
   Parameter[14]:=8;
   Parameter[15]:=90;
  end;
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var Status  : Byte;
    i       : Integer;
    newNote : TXSynthVoice;
const VeloDiv : Single = 1/128;
begin
 Status:=MidiEvent.midiData[0] and $F0; // channel information is removed
 if (Status=$90) and (MidiEvent.mididata[2]>0) then // "note on" ?
  begin
   if Voices.Count>7 then Voices.Remove(Voices.Items[0]);
   newNote:=TXSynthVoice.Create(self);
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
       Voices.Items[i].NoteOff;
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

procedure TVSTSSModule.VSTSSModuleLevelParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLevel:=Value*0.01;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   begin
    if SBLevel.Position<>Round(fLevel*100)
     then SBLevel.Position:=Round(fLevel*100);
   end;
end;

procedure TVSTSSModule.VSTSSModuleOsc1TypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].OType:=TOscilatorType(Round(Value));
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if CBOsc1Type.ItemIndex<>Round(Value)
    then CBOsc1Type.ItemIndex:=Round(Value);
end;

procedure TVSTSSModule.VSTSSModuleOsc2TypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].OType:=TOscilatorType(Round(Value));
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if CBOsc2Type.ItemIndex<>Round(Value)
    then CBOsc2Type.ItemIndex:=Round(Value);
end;

procedure TVSTSSModule.VSTSSModuleOsc1AttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].Attack:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc1ADSR.Attack<>0.01*Value
    then Osc1ADSR.Attack:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1DecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].Decay:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc1ADSR.Decay<>0.01*Value
    then Osc1ADSR.Decay:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1ReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].Release:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc1ADSR.Release<>0.01*Value
    then Osc1ADSR.Release:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1SustainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].Sustain:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc1ADSR.Sustain<>0.01*Value
    then Osc1ADSR.Sustain:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc1LevelChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[0].Level:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc1Level.Position<>Round(100*fOscs[0].Level)
    then Osc1Level.Position:=Round(100*fOscs[0].Level);
end;

procedure TVSTSSModule.VSTSSModuleOsc2AttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].Attack:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc2ADSR.Attack<>0.01*Value
    then Osc2ADSR.Attack:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2DecayChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].Decay:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc2ADSR.Decay<>0.01*Value
    then Osc2ADSR.Decay:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2ReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].Release:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc2ADSR.Release<>0.01*Value
    then Osc2ADSR.Release:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2SustainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].Sustain:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc2ADSR.Sustain<>0.01*Value
    then Osc2ADSR.Sustain:=0.01*Value;
end;

procedure TVSTSSModule.VSTSSModuleOsc2LevelChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fOscs[1].Level:=0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Osc2Level.Position<>Round(100*fOscs[1].Level)
    then Osc2Level.Position:=Round(100*fOscs[1].Level);
end;

procedure TVSTSSModule.VSTSSModuleCutoffParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fCutOff[0] := 0.01+Value/20000;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Round(100*FreqLogToLinear(((fCutOff[0]-0.01)*20000)))<>SBCutoff.Position
    then SBCutoff.Position:=Round(100*FreqLogToLinear((fCutOff[0]-0.01)*20000));
end;

procedure TVSTSSModule.VSTSSModuleResonanceParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fRes[0] := 0.01*Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   if Round(100*fRes[0])<>SBResonance.Position
    then SBResonance.Position:=Round(100*fRes[0]);
end;

procedure TVSTSSModule.VSTSSModuleDriveParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDrive:=Value;
 if Assigned(EditorForm) then
  with EditorForm As TVSTGUI do
   begin
    if SBDrive.Position<>Round(fDrive*10)
     then SBDrive.Position:=Round(fDrive*10);
   end;
end;

function TVSTSSModule.GetOscilators(index: integer): TOsc;
begin
 Result:=fOscs[index];
end;

procedure TVSTSSModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(Voices);
end;

end.
