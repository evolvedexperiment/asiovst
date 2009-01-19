unit DAV_StkEcho;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK echo effect class.

  This class implements a echo effect.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkDelay;

type
  TEcho = class(TStk)
  protected
    FDelayLine   : TDelay;
    FLength      : Integer;
    FLastOutput  : Single;
    FEffectMix   : Single;
  public
    // Class constructor, taking the longest desired delay FLength.
    constructor Create(sr, longestDelay: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the delay line FLength in samples.
    procedure setDelay(delay: Single);

    // Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure setEffectMix(mix: Single);

    // Return the last output value.
    function lastOut: Single;

    // Compute one output sample.
    function tick(input: Single): Single; overload;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: PSingle; vectorSize: Integer): PSingle; overload;
  end;

implementation

constructor TEcho.Create(sr, longestDelay: Single);
begin
  inherited Create(sr);
  FLength := round(longestDelay) + 2;
  FDelayLine := TDelay.Create(srate, FLength shr 1, FLength);
  FEffectMix := 0.5;
  Clear;
end;

destructor TEcho.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
end;

procedure TEcho.Clear;
begin
  FDelayLine.Clear;
  FLastOutput := 0.0;
end;

procedure TEcho.setDelay;
var
  size: Single;
begin
  size := delay;
  if (delay < 0.0) then
    size := 0.0
  else if (delay > FLength) then
    size := FLength;
  FDelayLine.setDelay(round(size));
end;

procedure TEcho.setEffectMix;
begin
  FEffectMix := mix;
  if (mix < 0.0) then
    FEffectMix := 0.0
  else if (mix > 1.0) then
    FEffectMix := 1.0;
end;

function TEcho.lastOut: Single;
begin
  Result := FLastOutput;
end;

function TEcho.tick(input: Single): Single;
begin
  FLastOutput := FEffectMix * FDelayLine.tick(input);
  FLastOutput := FLastOutput + input * (1.0 - FEffectMix);
  Result := FLastOutput;
end;

function TEcho.tick(vector: PSingle; vectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
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
