unit ChebyshevDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPChebyshevFilter, DAV_VstWindowSizer;

type
  TChebyshevHPModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRippleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFilter  : array [0..1] of TCustomChebyshev1HighpassFilter;
    FResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read FResizer;
  end;

implementation

{$R *.DFM}

uses
  Math, ChebyshevGUI;
  
procedure TChebyshevHPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   FFilter[ch] := TChebyshev1HighpassFilterAutomatable.Create;
   FFilter[ch].SetFilterValues(1000, 0, 1);
  end;
(*
 FResizer := TVstWindowSizer.Create;
 FResizer.Effect := Self;
*)

 // Initial Parameters
 Parameter[0] := 1000;
 Parameter[1] := 1;
 Parameter[2] := 4;

 with Programs[0] do
  begin
   Parameter[0] := 1000;
   Parameter[1] := 1;
   Parameter[2] := 4;
  end;
end;

procedure TChebyshevHPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(FFilter[ch]);
// FreeAndNil(FResizer);
end;

procedure TChebyshevHPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev.Create(Self);
end;

procedure TChebyshevHPModule.ParamRippleChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch]) then FFilter[ch].Ripple := Value;
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateRipple;
   end;
end;

procedure TChebyshevHPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].Order := max(2, 2 * round(0.5 * Value));
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateOrder;
   end;
end;

procedure TChebyshevHPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].Frequency := Value;
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TChebyshevHPModule.VSTModuleProcess(const Inputs,
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

procedure TChebyshevHPModule.VSTModuleProcessDoubleReplacing(const Inputs,
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

procedure TChebyshevHPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(FFilter[ch])
   then FFilter[ch].SampleRate := SampleRate;
end;

end.