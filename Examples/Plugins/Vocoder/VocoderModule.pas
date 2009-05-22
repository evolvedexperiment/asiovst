unit VocoderModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAV_Common, DAV_VSTEffect, DAV_VSTModule,
  VocoderVoice, VoiceList, DAV_DspVocoder;

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VocInputVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VocSynthVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VocVocoderVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVoices  : TVoiceList;
    FVocoder : TVocoder;
  public
    property Voices: TVoiceList read FVoices;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, VocoderGUI;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
begin
  FVoices := TVoiceList.Create(True);
  FVocoder := TVocoder.Create;

  Parameter[0] := -80;
  Parameter[1] := -80;
  Parameter[2] := 0;
  Parameter[3] := 0.5;
  Parameter[4] := 2;
  Parameter[5] := sqrt(0.5);
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
  FreeAndNil(FVocoder);
  FreeAndNil(FVoices);
end;

procedure TVSTSSModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTSSModule.VSTModuleProcess(
  const Inputs, Outputs: TDAVArrayOfSingleDynArray;
  const SampleFrames: Integer);
var
  i, j        : Integer;
  SynthSignal : Double;
begin
  for j := 0 to SampleFrames - 1 do
   begin
    // process synth input
    SynthSignal := 0;
    for i := 0 to Voices.Count - 1
     do SynthSignal := SynthSignal + Voices[i].Process;

    Outputs[0, j] := FastTanhOpt5Term(FVocoder.Process(Inputs[0, j], SynthSignal));
   end;

  for i := 1 to numOutputs - 1
   do Move(Outputs[0, 0], Outputs[i, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status  : Byte;
  i       : Integer;
  newNote : TVocoderVoice;
const
  VeloDiv: Single = 1 / 128;
begin
  Status := MidiEvent.midiData[0] and $F0; // channel information is removed
  if (Status = $90) and (MidiEvent.mididata[2] > 0) then // "note on" ?
   begin
    if Voices.Count > 7 then Voices.Remove(Voices.Items[0]);
    newNote := TVocoderVoice.Create(self);
    with newNote do
     begin
      MidiKeyNr := MidiEvent.midiData[1];
      Velocity := MidiEvent.midiData[2];
      NoteOn(Midi2Pitch[MidiKeyNr], Velocity * VeloDiv);
     end;
    Voices.Add(newNote);
   end
  else if ((status = $90) and (MidiEvent.mididata[2] = 0)) or
    (status = $80) then // "note off" ?
   begin
    for i := 0 to Voices.Count - 1 do
     if (Voices.Items[i].MidiKeyNr = MidiEvent.midiData[1]) then
      begin
       Voices.Delete(i);
       Break;
      end;
   end
  else
 if (status = $B0) and (MidiEvent.midiData[1] = $7E)
  then Voices.Clear; // all notes off
end;

procedure TVSTSSModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FVocoder)
  then FVocoder.SampleRate := SampleRate;
end;

procedure TVSTSSModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder)
  then FVocoder.SynthesisBandwidth := Value;
end;

procedure TVSTSSModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder)
  then FVocoder.Release := Value;
end;

procedure TVSTSSModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder)
  then FVocoder.Attack := Value;
end;

procedure TVSTSSModule.VocInputVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder) then
  if Value <= -80
   then FVocoder.InputLevel := 0
   else FVocoder.InputLevel := dB_to_Amp(Value);
 if FEditorForm is TVSTGUI
  then TVSTGUI(FEditorForm).UpdateInputVolume;
end;

procedure TVSTSSModule.VocSynthVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder) then
  if Value <= -80
   then FVocoder.SynthLevel := 0
   else FVocoder.SynthLevel := dB_to_Amp(Value);
 if FEditorForm is TVSTGUI
  then TVSTGUI(FEditorForm).UpdateSynthVolume;
end;

procedure TVSTSSModule.VocVocoderVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FVocoder) then
  if Value <= -80
   then FVocoder.VocoderLevel := 0
   else FVocoder.VocoderLevel := dB_to_Amp(Value);
 if FEditorForm is TVSTGUI
  then TVSTGUI(FEditorForm).UpdateVocoderVolume;
end;

end.
