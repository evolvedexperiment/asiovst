unit ChebyshevDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DSPChebyshevFilter, DAV_VstWindowSizer;

type
  TChebyshevLPModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamRippleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fFilter  : array [0..1] of TChebyshev1LP;
    fResizer : TVstWindowSizer;
  public
    property Resizer: TVstWindowSizer read fResizer;
  end;

implementation

{$R *.DFM}

uses
  Math, ChebyshevGUI;

procedure TChebyshevLPModule.ParamRippleChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch]) then fFilter[ch].Ripple := Value;
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateRipple;
   end;
end;

procedure TChebyshevLPModule.ParamOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch])
   then fFilter[ch].Order := max(2, 2 * round(Value) div 2);
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateOrder;
   end;
end;

procedure TChebyshevLPModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch])
   then fFilter[ch].Frequency := Value;
 if EditorForm is TFmChebyshev then
  with TFmChebyshev(EditorForm) do
   begin
    UpdateFrequency;
   end;
end;

procedure TChebyshevLPModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  begin
   fFilter[ch] := TChebyshev1LP.Create;
   fFilter[ch].SetFilterValues(1000, 0, 1);
  end;
(*
 fResizer := TVstWindowSizer.Create;
 fResizer.Effect := Self;
*)

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

procedure TChebyshevLPModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmChebyshev.Create(Self);
end;

procedure TChebyshevLPModule.VSTModuleClose(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1
  do FreeAndNil(fFilter[ch]);
// FreeAndNil(fResizer);
end;

procedure TChebyshevLPModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TChebyshevLPModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fFilter[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fFilter[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TChebyshevLPModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to numInputs - 1 do
  if assigned(fFilter[ch])
   then fFilter[ch].SampleRate := SampleRate;
end;

end.