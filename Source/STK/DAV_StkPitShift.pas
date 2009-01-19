unit DAV_StkPitShift;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK simple pitch shifter effect class.

  This class implements a simple pitch shifter using FDelay lines.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkDelayl;

type
  TStkPitchShifter = class(TStk)
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the pitch shift factor (1.0 produces no shift).
    procedure SetShift(shift: Single);

    // Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure SetEffectMix(mix: Single);

    // Return the last output value.
    function LastOut: Single;

    // Compute one output sample.
    function Tick(input: Single): Single; overload;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function Tick(vector: PSingle; vectorSize: Integer): PSingle; overload;

  protected
    FDelayLine: array[0..1] of TDelayL;
    FEffectMix, FRate, FLastOutput: Single;
    FDelay, FEnv: array[0..1] of Single;
  end;

implementation

constructor TStkPitchShifter.Create;
begin
  inherited Create(SampleRate);
  FDelay[0] := 12;
  FDelay[1] := 512;
  FDelayLine[0] := TDelayL.Create(srate, FDelay[0], 1024);
  FDelayLine[1] := TDelayL.Create(srate, FDelay[1], 1024);
  FEffectMix := 0.5;
  FRate := 1.0;
end;

destructor TStkPitchShifter.Destroy;
begin
  inherited Destroy;
  FDelayLine[0].Free;
  FDelayLine[1].Free;
end;

procedure TStkPitchShifter.Clear;
begin
  FDelayLine[0].Clear;
  FDelayLine[1].Clear;
  FLastOutput := 0.0;
end;

procedure TStkPitchShifter.SetEffectMix;
begin
  FEffectMix := mix;
  if (mix < 0.0) then
    FEffectMix := 0.0
  else if (mix > 1.0) then
    FEffectMix := 1.0;
end;

procedure TStkPitchShifter.SetShift;
begin
  if (shift < 1.0) then
    FRate := 1.0 - shift
  else if (shift > 1.0) then
    FRate := 1.0 - shift
  else
   begin
    FRate := 0.0;
    FDelay[0] := 512;
   end;
end;

function TStkPitchShifter.LastOut: Single;
begin
  Result := FLastOutput;
end;

function TStkPitchShifter.Tick(input: Single): Single;
begin
  FDelay[0] := FDelay[0] + FRate;
  while (FDelay[0] > 1012) do
    FDelay[0] := FDelay[0] - 1000;
  while (FDelay[0] < 12) do
    FDelay[0] := FDelay[0] + 1000;
  FDelay[1] := FDelay[0] + 500;
  while (FDelay[1] > 1012) do
    FDelay[1] := FDelay[1] - 1000;
  while (FDelay[1] < 12) do
    FDelay[1] := FDelay[1] + 1000;
  FDelayLine[0].setDelay(round(FDelay[0]));
  FDelayLine[1].setDelay(round(FDelay[1]));
  FEnv[1] := abs(FDelay[0] - 512) * 0.002;
  FEnv[0] := 1.0 - FEnv[1];
  FLastOutput := FEnv[0] * FDelayLine[0].Tick(input);
  FLastOutput := FLastOutput + FEnv[1] * FDelayLine[1].Tick(input);
  FLastOutput := FLastOutput * FEffectMix;
  FLastOutput := FLastOutput + (1.0 - FEffectMix) * input;
  Result := FLastOutput;
end;

function TStkPitchShifter.Tick(vector: PSingle; vectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
