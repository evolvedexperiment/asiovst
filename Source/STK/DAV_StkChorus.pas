unit DAV_StkChorus;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkChorus effect class.

  This class implements a TStkChorus effect.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkDelayl, DAV_StkLfo;

type
  TStkChorus = class(TStk)
  protected
    FDelayLine: array[0..1] of TDelayL;
    FBaseLength, FModDepth, FEffectMix: Single;
    FLastOutput: array[0..1] of Single;
  public
    mods: array[0..1] of TLFO;

    // Class constructor, taking the longest desired delay length.
    constructor Create(SampleRate, BaseDelay: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set modulation depth.
    procedure setModDepth(depth: Single);

    // Set modulation frequency.
    procedure setModFrequency(frequency: Single);

    // Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure setEffectMix(mix: Single);

    // Return the last output value.
    function lastOut: Single;

    // Return the last left output value.
    function lastOutLeft: Single;

    // Return the last right output value.
    function lastOutRight: Single;

    // Compute one output sample.
    function tick(input: Single): Single; overload;

    // Take \e vectorSize inputs, compute the same number of outputs and return them in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;
  end;

implementation

constructor TStkChorus.Create(SampleRate, BaseDelay: Single);
begin
  inherited Create(SampleRate);
  FDelayLine[0] := TDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay * 1.414) + 2);
  FDelayLine[1] := TDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay) + 2);
  FBaseLength := BaseDelay;

  mods[0] := TLFO.Create(srate);
  mods[1] := TLFO.Create(srate);
  mods[0].SetFrequency(0.2);
  mods[1].SetFrequency(0.222222);
  FModDepth := 0.05;
  FEffectMix := 0.5;
  Clear;
end;

destructor TStkChorus.Destroy;
begin
  inherited Destroy;
  FDelayLine[0].Free;
  FDelayLine[1].Free;
  mods[0].Free;
  mods[1].Free;
end;

procedure TStkChorus.Clear;
begin
  FDelayLine[0].Clear;
  FDelayLine[1].Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
end;

procedure TStkChorus.setEffectMix;
begin
  FEffectMix := mix;
  if (mix < 0.0) then
    FEffectMix := 0.0
  else if (mix > 1.0) then
    FEffectMix := 1.0
end;

procedure TStkChorus.setModDepth;
begin
  FModDepth := depth;
end;

procedure TStkChorus.setModFrequency;
begin
  mods[0].setFrequency(frequency);
  mods[1].setFrequency(frequency * 1.1111);
end;

function TStkChorus.lastOut: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkChorus.lastOutLeft: Single;
begin
  Result := FLastOutput[0];
end;

function TStkChorus.lastOutRight: Single;
begin
  Result := FLastOutput[1];
end;

function TStkChorus.tick(input: Single): Single;
begin
  FDelayLine[0].setDelay(FBaseLength * 0.707 * (1.0 + mods[0].tick));
  FDelayLine[1].setDelay(FBaseLength * 0.5 * (1.0 - mods[1].tick));
  FLastOutput[0] := input * (1.0 - FEffectMix);
  FLastOutput[0] := FLastOutput[0] + FEffectMix * FDelayLine[0].tick(input);
  FLastOutput[1] := input * (1.0 - FEffectMix);
  FLastOutput[1] := FLastOutput[1] + FEffectMix * FDelayLine[1].tick(input);
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkChorus.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
