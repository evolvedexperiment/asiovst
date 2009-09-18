unit Chebyshev2DM;
interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule, DAV_DspFilterChebyshev,
  DAV_DSPFilterChebyshevType2, DAV_VstWindowSizer;

type
  TChebyshev2LPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStopbandChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterCorrectFrequencyDisplay(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array [0..1] of TCustomChebyshev2LowpassFilter;
  end;

implementation

{$R *.DFM}

uses
  Math, Chebyshev2GUI;

procedure TChebyshev2LPModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  begin
   FFilter[Channel] := TChebyshev2LowpassFilter.Create(4);
   FFilter[Channel].SetFilterValues(1000, 0, 1);
  end;

 Parameter[0] := 1000;
 Parameter[1] := -24;
 Parameter[2] := 4;

 with Programs[0] do
  begin
   Parameter[0] := 1000;
   Parameter[1] := -24;
   Parameter[2] := 4;
  end;
end;

procedure TChebyshev2LPModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1
  do FreeAndNil(FFilter[Channel]);
end;

procedure TChebyshev2LPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev.Create(Self);
end;

procedure TChebyshev2LPModule.ParamStopbandChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FFilter[Channel]) then FFilter[Channel].Stopband := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateStopband;
end;

procedure TChebyshev2LPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Order := round(Value); // max(2, 2 * round(0.5 * Value));

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateOrder;
end;

procedure TChebyshev2LPModule.ParameterCorrectFrequencyDisplay(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].FixFrequency := Value > 0.5;
end;

procedure TChebyshev2LPModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TChebyshev2LPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev
  then TFmChebyshev(EditorForm).UpdateFrequency;
end;

procedure TChebyshev2LPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFilter[0].ProcessSample64(Inputs[0, Sample]);
   Outputs[1, Sample] := FFilter[1].ProcessSample64(Inputs[1, Sample]);
  end;
end;

procedure TChebyshev2LPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFilter[0].ProcessSample64(Inputs[0, Sample]);
   Outputs[1, Sample] := FFilter[1].ProcessSample64(Inputs[1, Sample]);
  end;
end;

procedure TChebyshev2LPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if assigned(FFilter[Channel])
   then FFilter[Channel].SampleRate := SampleRate;
end;

end.
