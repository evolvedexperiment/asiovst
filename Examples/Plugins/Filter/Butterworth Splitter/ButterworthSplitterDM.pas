unit ButterworthSplitterDM;

{$I DAV_Compiler.inc}

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_VSTCustomModule,
  DAV_DspFilterButterworth;

type
  TButterworthSplitterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FButterworthSplitter : array of TButterworthSplitBandFilter;
  public
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{ TButterworthSplitterModule }

procedure TButterworthSplitterModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 assert(numOutputs = 2 * numInputs);
 SetLength(FButterworthSplitter, numInputs);
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  begin
   FButterworthSplitter[Channel] := TButterworthSplitBandFilter.Create;
   FButterworthSplitter[Channel].SampleRate := SampleRate;
  end;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

 Parameter[0] := 1000;
 Parameter[1] := 2;
end;

procedure TButterworthSplitterModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1
  do FreeAndNil(FButterworthSplitter[Channel]);
end;

procedure TButterworthSplitterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample, Channel: Integer;
begin
 {$IFDEF Debug} AddLogMessage('VSTModuleProcess'); {$ENDIF}
 // CDenorm32 +
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FButterworthSplitter) - 1
   do FButterworthSplitter[Channel].ProcessSample(Inputs[Channel, Sample],
        Outputs[2 * Channel, Sample], Outputs[2 * Channel + 1, Sample])
end;

procedure TButterworthSplitterModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 Predefined := IntToStr(round(Parameter[Index]));
end;

procedure TButterworthSplitterModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TButterworthSplitterModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TButterworthSplitterModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].Frequency := Value;
end;

procedure TButterworthSplitterModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].Order := round(Value);
end;

procedure TButterworthSplitterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].SampleRate := SampleRate;
end;

{$IFDEF FPC}
initialization
  {$i ButterworthSplitterDM.lrs}
{$ENDIF}

end.
