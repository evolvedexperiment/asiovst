unit Chebyshev2DM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPFilterChebyshev, DAV_DSPFilterChebyshevType2, DAV_VstWindowSizer;

type
  TChebyshev2HPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamStopbandChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array [0..1] of TCustomChebyshev2HighpassFilter;
    FResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read FResizer;
  end;

implementation

{$R *.DFM}

uses
  Math, Chebyshev2GUI;
  
procedure TChebyshev2HPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TChebyshev2HighpassFilter.Create;
   FFilter[ch].SetFilterValues(1000, 0, 1);
  end;
(*
 FResizer := TVstWindowSizer.Create;
 FResizer.Effect := Self;
*)

 // Initial Parameters
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

procedure TChebyshev2HPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(FFilter[ch]);
// FreeAndNil(FResizer);
end;

procedure TChebyshev2HPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev2.Create(Self);
end;

procedure TChebyshev2HPModule.ParamStopbandChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch]) then FFilter[ch].Stopband := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateStopband;
end;

procedure TChebyshev2HPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].Order := round(Value); //max(2, 2 * round(0.5 * Value));

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateOrder;
end;

procedure TChebyshev2HPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].Frequency := Value;

 // update GUI if necessary
 if EditorForm is TFmChebyshev2
  then TFmChebyshev2(EditorForm).UpdateFrequency;
end;

procedure TChebyshev2HPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TChebyshev2HPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := FFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TChebyshev2HPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].SampleRate := SampleRate;
end;

end.
