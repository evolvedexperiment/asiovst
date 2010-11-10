unit PhaseRotatorDSP;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_DspFilter, DAV_DspFilterBasics, DAV_VSTModule, 
  DAV_VSTEffect;

type
  TPhaseRotatorModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMidi(Sender: TObject; const MidiEvent: TVstMidiEvent);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FAllpass         : array of array [0..3] of TBasicAllpassFilter;
    FOrder           : Integer;
    FCriticalSection : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, PhaseRotatorGUI;

procedure TPhaseRotatorModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TPhaseRotatorModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPhaseRotatorModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 SetLength(FAllpass, numInputs);

 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   begin
    FAllpass[ChannelIndex, BandIndex] := TBasicAllpassFilter.Create;
    with FAllpass[ChannelIndex, BandIndex] do
     begin
      SampleRate := Self.SampleRate;
      Frequency  := 200;
      Bandwidth  := 1.4;
     end;
   end;

 Parameter[0] := 200;
 Parameter[1] := 2;
 Parameter[2] := 1.4;
end;

procedure TPhaseRotatorModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1
   do FreeAndNil(FAllpass[ChannelIndex, BandIndex]);
end;

procedure TPhaseRotatorModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmPhaseRotator.Create(Self);
end;

procedure TPhaseRotatorModule.ParameterFrequencyDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 3, 3);
end;

procedure TPhaseRotatorModule.ParameterFrequencyLabel(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TPhaseRotatorModule.ParameterOrderDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(2 * Round(Parameter[Index]));
end;

procedure TPhaseRotatorModule.ParameterBandwidthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(Parameter[Index], ffGeneral, 2, 2);
end;

procedure TPhaseRotatorModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   if Assigned(FAllpass[ChannelIndex, BandIndex])
    then FAllpass[ChannelIndex, BandIndex].Bandwidth := Value;

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateBandwidth;
end;

procedure TPhaseRotatorModule.ParameterFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
   if Assigned(FAllpass[ChannelIndex, BandIndex])
    then FAllpass[ChannelIndex, BandIndex].Frequency := Value;

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateFrequency;
end;

procedure TPhaseRotatorModule.ParameterOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FOrder := IntLimit(Round(Value), 0, 4);

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateStages;
end;

procedure TPhaseRotatorModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 if Abs(SampleRate) > 0 then
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    if Assigned(FAllpass[ChannelIndex, BandIndex])
     then FAllpass[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
end;

procedure TPhaseRotatorModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Data := Inputs[ChannelIndex, SampleIndex];
    for BandIndex := 0 to FOrder - 1
     do Data := FAllpass[ChannelIndex, BandIndex].ProcessSample64(Data);
    Outputs[ChannelIndex, SampleIndex] := Data;
   end;
end;

procedure TPhaseRotatorModule.VSTModuleProcessMidi(Sender: TObject;
  const MidiEvent: TVstMidiEvent);
var
  Status : Integer;
  CCData : Single;
begin
 Status := MidiEvent.MidiData[0] and $F0; // channel information is removed

 if (Status = $B0) then // midi CC ?
  begin
   CCData := MidiEvent.MidiData[2] / 127; // CC data
   case MidiEvent.MidiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData);
    71: Parameter[1] := 4 * CCData;
    72: Parameter[2] := 0.1 * Power(10, 2 * CCData);
   end;
  end;
end;

end.
