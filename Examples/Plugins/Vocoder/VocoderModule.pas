unit VocoderModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAV_Common, DAV_VSTEffect, DAV_VSTModule,
  VocoderVoice, VoiceList, DAV_DspChebyshevFilter, DAV_DspFilter;

const
  cNumFrequencies = 32;
  cThirdOctaveFrequencies : Array [0..cNumFrequencies - 1] of Single =
      (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
       630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
       10000, 12500, 16000, 20000);

type
  TVSTSSModule = class(TVSTModule)
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VocInputVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VocSynthVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VocVocoderVolumeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FAnalysisFiltersLP : Array [0..cNumFrequencies - 1] of TChebyshev1LP;
    FAnalysisFiltersHP : Array [0..cNumFrequencies - 1] of TChebyshev1HP;
    FAnalysisRMS       : Array [0..cNumFrequencies - 1] of Single;
    FSynthesisFilters  : Array [0..cNumFrequencies - 1] of TSimpleBandpass;
    FDownSampler       : Integer;
    FDownSampleMax     : Integer;
    FVolFactors        : Array [0..2] of Double;
    FVoices            : TVoiceList;
  public
    property Voices: TVoiceList read FVoices;
  end;

implementation

{$R *.DFM}

uses
  VocoderGUI, Math;

procedure TVSTSSModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 FVoices := TVoiceList.Create(True);
 FDownSampler := 0;
 for i := 0 to cNumFrequencies-1 do
  begin
   FAnalysisFiltersLP[i] := TChebyshev1LP.Create;
   with FAnalysisFiltersLP[i] do
    begin
     SampleRate := 44100;
     SetFilterValues(0.87 * (cThirdOctaveFrequencies[cNumFrequencies-i-1]*HalfThirdMulFak),0,10);
     if FDownSampler = -1 then DownsampleAmount := 0 else
      while Power(2, DownsampleAmount) * Frequency < 0.2 * SampleRate
       do DownsampleAmount := DownsampleAmount + 1;
     CalculateCoefficients;
    end;

   FAnalysisFiltersHP[i] := TChebyshev1HP.Create;
   with FAnalysisFiltersHP[i] do
    begin
     SampleRate := 44100;
     SetFilterValues(1.149 * (cThirdOctaveFrequencies[cNumFrequencies - i - 1] / HalfThirdMulFak), 0, 10);
     DownsampleAmount := FAnalysisFiltersLP[i].DownsampleAmount;
     CalculateCoefficients;
    end;

   FSynthesisFilters[i] := TSimpleBandpass.Create;
   with FSynthesisFilters[i] do
    begin
     SampleRate := 44100;
     Gain := 0; Bandwidth := 0.5;
     Frequency := cThirdOctaveFrequencies[cNumFrequencies - i - 1];
    end;
  end;
 FDownSampleMax := FAnalysisFiltersLP[cNumFrequencies - 1].DownsampleFaktor;

 Parameter[0] := -80;
 Parameter[1] := -80;
 Parameter[2] := 0;
end;

procedure TVSTSSModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVoices);
end;

procedure TVSTSSModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(Self);
 with (GUI As TVSTGUI) do
  begin
   SBInputLevel.Position   := round(Amp_to_dB(FVolFactors[0]));
   SBSynthLevel.Position   := round(Amp_to_dB(FVolFactors[1]));
   SBVocoderLevel.Position := round(Amp_to_dB(FVolFactors[2]));
  end;
end;

procedure TVSTSSModule.VSTModuleProcess(const inputs,
  outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j    : Integer;
  d, z, s : Double;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   d := inputs[0, i];
   for j := 0 to cNumFrequencies - 1 do
    begin
     if (FDownSampler mod FAnalysisFiltersLP[j].DownsampleFaktor) <> 0
      then Break;

     d := FAnalysisFiltersLP[j].ProcessSample(d + 1E-32);
     z := FAnalysisFiltersHP[j].ProcessSample(d + 1E-32);

     s := IntPower(0.999, 8 * FAnalysisFiltersLP[j].DownsampleAmount + 1);
     FAnalysisRMS[j] := s * FAnalysisRMS[j] + 400 * (1 - s) * abs(z);
     if FAnalysisRMS[j] > 0.5 then FAnalysisRMS[j] := 0.5;
    end;
   inc(FDownSampler);
   if FDownSampler >= FDownSampleMax then FDownSampler := 0;
  end;

 for j := 0 to SampleFrames - 1 do
  begin
   d := 0.1 * (random - 0.5);
   for i := 0 to Voices.Count - 1
    do d := d + Voices[i].Process;
   z := 0;
   for i := 0 to cNumFrequencies - 1 do
    begin
     z := z + FSynthesisFilters[i].ProcessSample(FAnalysisRMS[i] * d);
    end;
   outputs[0,j] := Tanh2b(FVolFactors[2] * z + FVolFactors[1] * d + FVolFactors[0] * inputs[0, j]);
  end;

 for i := 1 to numOutputs - 1
  do Move(outputs[0, 0], outputs[i, 0], SampleFrames * SizeOf(Single));
end;

procedure TVSTSSModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  Status  : Byte;
  i       : Integer;
  newNote : TVocoderVoice;
const
  VeloDiv : Single = 1 / 128;
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
     NoteOn(Midi2Pitch[MidiKeyNr],Velocity*VeloDiv);
    end;
   Voices.Add(newNote);
  end
 else if ((status = $90) and (MidiEvent.mididata[2] = 0)) or
          (status = $80) then // "note off" ?
  begin
   for i := 0 to Voices.Count-1 do
    begin
     if (Voices.Items[i].MidiKeyNr=MidiEvent.midiData[1]) then
      begin
       Voices.Delete(i);
       Break;
      end;
    end;
  end
 else if (status = $B0) and (MidiEvent.midiData[1] = $7E) then
  begin
   // all notes off
   Voices.Clear;
  end;
end;

procedure TVSTSSModule.VocInputVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FVolFactors[0] := dB_to_Amp(Value);
 if Assigned(fEditorForm) then
  with fEditorForm As TVSTGUI do
   begin
    SBInputLevel.Position := Round(Value);
   end;
end;

procedure TVSTSSModule.VocSynthVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FVolFactors[1] := dB_to_Amp(Value);
 if Assigned(fEditorForm) then
  with fEditorForm As TVSTGUI do
   begin
    SBSynthLevel.Position := Round(Value);
   end;
end;

procedure TVSTSSModule.VocVocoderVolumeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FVolFactors[2] := dB_to_Amp(Value);
 if Assigned(fEditorForm) then
  with fEditorForm As TVSTGUI do
   begin
    SBVocoderLevel.Position := Round(Value);
   end;
end;

end.
