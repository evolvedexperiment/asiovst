unit ButterworthDM;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilterButterworth, DAV_VstWindowSizer;

type
  TButterworthLPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter: array of TCustomButterworthFilter;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  Math, ButterworthGUI;

procedure TButterworthLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 assert(numInputs = numOutputs);
 assert(numInputs > 0);
 SetLength(FFilter, numInputs);
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TButterworthLowPassFilter.Create;
   FFilter[ch].SetFilterValues(1000, 0);
  end;

 Parameter[0] := 1000;
 Parameter[1] := 4;
end;

procedure TButterworthLPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to Length(FFilter) - 1
  do FreeAndNil(FFilter[ch]);
end;

procedure TButterworthLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthLPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FFilter) - 1 do
  begin
   if assigned(FFilter[ch])
    then FFilter[ch].Order := round(Value);
  end;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateOrder;
end;

procedure TButterworthLPModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].Frequency := Value;
 if EditorForm is TFmButterworth then
  with TFmButterworth(EditorForm)
   do UpdateFrequency;
end;

procedure TButterworthLPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FFilter) - 1
  do FFilter[ch].SampleRate := SampleRate;
end;

procedure TButterworthLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample(Inputs[Channel, Sample]);
end;

procedure TButterworthLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample  : Integer;
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FFilter[Channel].ProcessSample(Inputs[Channel, Sample]);
end;

{$IFDEF FPC}
initialization
  {$i ButterworthDM.lrs}
{$ENDIF}

end.
