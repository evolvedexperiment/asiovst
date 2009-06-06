unit ButterworthDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPButterworthFilter, DAV_DSPChebyshevFilter, DAV_VstWindowSizer;

type
  TButterworthHPModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter : array [0..1] of TCustomButterworthFilter;
  end;

implementation

{$R *.DFM}

uses
  Math, ButterworthGUI;

procedure TButterworthHPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TButterworthHighPassFilter.Create;
   FFilter[ch].SetFilterValues(20, 0);
  end;

 // set initial parameters 
 Parameter[0] := 20;
 Parameter[1] := 2;
end;

procedure TButterworthHPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(FFilter[ch]);
end;

procedure TButterworthHPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmButterworth.Create(Self);
end;

procedure TButterworthHPModule.ParamOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Order := round(Value);

 // update GUI if necessary
 if EditorForm is TFmButterworth
  then TFmButterworth(EditorForm).UpdateOrder;
end;

procedure TButterworthHPModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmButterworth
  then TFmButterworth(EditorForm).UpdateFrequency;
end;

procedure TButterworthHPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFilter[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFilter[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TButterworthHPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFilter[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFilter[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TButterworthHPModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to Length(FFilter) - 1
  do FFilter[ch].SampleRate := SampleRate;
end;

end.
