unit SpectralDelayFilterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_DspFilterSpectralDelay, DAV_VSTModule;

type
  TSpectralDelayFilterModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParameterTuneChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
  private
    FCriticalSection      : TCriticalSection;
    FSpectralDelayFilters : array of TSpectralDelayFilter;
  public
  end;

implementation

{$R *.DFM}

uses
  SpectralDelayFilterGUI;

procedure TSpectralDelayFilterModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 Assert(numInputs = numOutputs);
 SetLength(FSpectralDelayFilters, numOutputs);

 Parameter[0] := 0.9;
 Parameter[1] := 16; 

 for Channel := 0 to Length(FSpectralDelayFilters) - 1
  do FSpectralDelayFilters[Channel] := TSpectralDelayFilter.Create;
end;

procedure TSpectralDelayFilterModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if assigned(FSpectralDelayFilters[Channel])
   then FreeAndNil(FSpectralDelayFilters[Channel]);
end;

procedure TSpectralDelayFilterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSpectralDelayFilterModule.ParameterTuneChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if assigned(FSpectralDelayFilters[Channel])
   then FSpectralDelayFilters[Channel].Frequency := sqrt(Value); // * 0.5 * SampleRate;
end;

procedure TSpectralDelayFilterModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
   if assigned(FSpectralDelayFilters[Channel])
    then FSpectralDelayFilters[Channel].FilterCount := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralDelayFilterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FSpectralDelayFilters[Channel].ProcessSample64(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSpectralDelayFilterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSpectralDelayFilters) - 1 do
  if assigned(FSpectralDelayFilters[Channel])
   then FSpectralDelayFilters[Channel].SampleRate := SampleRate;
end;

procedure TSpectralDelayFilterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSpectralDelayFilter.Create(Self);
end;

end.
