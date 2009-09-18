unit Audio2MidiTriggerDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspAudioToMidiTrigger;

type
  TAudio2MidiTriggerModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMidiNoteDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMidiNoteChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIntervalChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterVelocityShiftChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FAudio2MidiTrigger : TAudio2MidiTrigger;
    FMidiNote          : Byte;
    FVelocityShift     : Byte;
    procedure MidiTrigger(Sender: TObject; const Level: Single);
  end;

implementation

{$R *.DFM}

uses
  Audio2MidiTriggerGui, DAV_VSTBasicModule;

procedure TAudio2MidiTriggerModule.VSTModuleOpen(Sender: TObject);
begin
 FAudio2MidiTrigger := TAudio2MidiTrigger.Create;
 with FAudio2MidiTrigger do
  begin
   SampleRate := Self.SampleRate;
   Threshold  := -30;
   Interval   := 0.02;
   Flags      := [amFilterOutput];
   OnTrigger  := MidiTrigger;
  end;
 FMidiNote := 64;

 Parameter[0] := -30;
 Parameter[1] :=   0;
 Parameter[2] :=  20;
 Parameter[3] :=  64;
 Parameter[4] :=   0;
end;

procedure TAudio2MidiTriggerModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FAudio2MidiTrigger);
end;

procedure TAudio2MidiTriggerModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmAudio2MidiTrigger.Create(Self);
end;

procedure TAudio2MidiTriggerModule.ParameterIntervalChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FAudio2MidiTrigger)
  then FAudio2MidiTrigger.Interval := 0.001 * Value;
end;

procedure TAudio2MidiTriggerModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FAudio2MidiTrigger)
  then FAudio2MidiTrigger.Threshold := Value;
end;

procedure TAudio2MidiTriggerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FAudio2MidiTrigger)
  then FAudio2MidiTrigger.SampleRate := SampleRate;
end;

procedure TAudio2MidiTriggerModule.ParameterMidiNoteChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMidiNote := Round(Value);
end;

procedure TAudio2MidiTriggerModule.ParameterMidiNoteDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(FMidiNote);
end;

procedure TAudio2MidiTriggerModule.ParameterVelocityShiftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FVelocityShift := Round(Value);
end;

procedure TAudio2MidiTriggerModule.MidiTrigger(
  Sender: TObject; const Level: Single);
var
  Velocity : Byte;
begin
 if FAudio2MidiTrigger.Threshold = 0
  then Velocity := 100
  else
   with FAudio2MidiTrigger
    do Velocity := Round(Limit((Level - Threshold) / abs(Threshold), 0, 1) * 127);
 Velocity := Limit(Velocity + FVelocityShift, 0, 127);
 MidiNoteOn(0, FMidiNote, Velocity);
end;

procedure TAudio2MidiTriggerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1
  do Outputs[0, Sample] := FAudio2MidiTrigger.ProcessSample32(Inputs[0, Sample]);
 SendVstEventsToHost(FMidiEvent);
end;

end.
