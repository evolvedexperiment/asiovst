unit HumRemovalDSP;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Types,
  DAV_VSTModule, DAV_DspHumRemoval, DAV_DspFilterButterworth,
  DAV_DspFilterChebyshevType1, DAV_DspFilterChebyshevType2;

type
  THumRemovalModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterHighpassActiveDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHighpassTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterHighpassOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParameterHighpassTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterHighpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFundamentalFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FHumRemoval      : array of TDspHumRemoval;
  public
    function Magnitude_dB(Frequency: Single): Single;  
  end;

implementation

{$R *.DFM}

uses
  HumRemovalGUI;

procedure THumRemovalModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 assert(numInputs = numOutputs);
end;

procedure THumRemovalModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure THumRemovalModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 SetLength(FHumRemoval, numInputs);

 // create hum removal class
 for Channel := 0 to Length(FHumRemoval) - 1
  do FHumRemoval[Channel] := TDspHumRemoval.Create;

 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 75;
 Parameter[3] := 2;
 Parameter[4] := 50;
 Parameter[5] := 0.08;
end;

procedure THumRemovalModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1
  do FreeAndNil(FHumRemoval[Channel]);
end;

procedure THumRemovalModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmHumRemoval.Create(Self);
end;

procedure THumRemovalModule.ParameterHighpassOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure THumRemovalModule.ParameterHighpassTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel]) then
   case Round(Value) of
    0 : FHumRemoval[Channel].HighpassFilterType := TButterworthLowCut;
    1 : FHumRemoval[Channel].HighpassFilterType := TChebyshev1LowCutFilter;
    2 : FHumRemoval[Channel].HighpassFilterType := TChebyshev2LowCutFilter;
   end;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassType;
end;

procedure THumRemovalModule.ParameterHighpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].HighpassFilter.Frequency := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassFrequency;
end;

procedure THumRemovalModule.ParameterHighpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].HighpassFilter.Order := Round(Value);

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassOrder;
end;

procedure THumRemovalModule.ParameterHighpassActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].HighpassFilterActive := Value > 0.5;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassActive;
end;

function THumRemovalModule.Magnitude_dB(Frequency: Single): Single;
begin
 if assigned(FHumRemoval[0])
  then Result := FHumRemoval[0].Magnitude_dB(Frequency)
  else Result := 1;
end;

procedure THumRemovalModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].Bandwidth := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateBandwidth;
end;

procedure THumRemovalModule.ParameterFundamentalFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].FundamentalFrequency := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateFundamentalFrequency;
end;

procedure THumRemovalModule.ParameterHighpassActiveDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  CActiveStr : array [0..1] of string = ('On', 'Off');
begin
 PreDefined := CActiveStr[Integer(Parameter[Index] < 0.5)];
end;

procedure THumRemovalModule.ParameterHighpassTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : Predefined := 'Butterworth';
  1 : Predefined := 'Chebyshev I';
  2 : Predefined := 'Chebyshev II';
 end;
end;

procedure THumRemovalModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FHumRemoval) - 1 do
   if assigned(FHumRemoval[Channel])
    then FHumRemoval[Channel].HighpassFilter.SampleRate := abs(SampleRate);
end;

procedure THumRemovalModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FHumRemoval[Channel].ProcessSample32(Inputs[Channel, Sample]);
end;

end.
