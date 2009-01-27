unit DAV_StkChorus;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkChorus effect class.

  This class implements a TStkChorus effect.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkDelayl, DAV_StkLfo;

type
  TStkChorus = class(TStk)
  private
    function GetLastOutput: Single;
    procedure SetModDepth(const Value: Single);
    procedure SetModFrequency(const Value: Single);
    procedure SetEffectMix(const Value: Single);
    function GetModFrequency: Single;
  protected
    FDelayLine   : array[0..1] of TStkDelayL;
    FBaseLength  : Single;
    FModDepth    : Single;
    FEffectMix   : Single;
    FLastOutput  : array[0..1] of Single;
    FMods        : array[0..1] of TStkLFO;
  public

    // Class constructor, taking the longest desired delay length.
    constructor Create(const SampleRate, BaseDelay: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Compute one output sample.
    function Tick(const Input: Single): Single; overload;

    // Take \e vectorSize inputs, compute the same number of outputs and return them in \e Vector.
    function Tick(Vector: PSingle; vectorSize: Integer): PSingle; overload;
  published
    property LastOutput: Single read GetLastOutput;
    property LastOutputLeft: Single read FLastOutput[0];
    property LastOutputRight: Single read FLastOutput[1];

    property ModDepth: Single read FModDepth write SetModDepth;
    property ModFrequency: Single read GetModFrequency write SetModFrequency;
    property EffectMix: Single read FEffectMix write SetEffectMix;
  end;

implementation

constructor TStkChorus.Create(const SampleRate, BaseDelay: Single);
begin
  inherited Create(SampleRate);
  FDelayLine[0] := TStkDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay * 1.414) + 2);
  FDelayLine[1] := TStkDelayL.Create(SampleRate, round(BaseDelay), round(BaseDelay) + 2);
  FBaseLength := BaseDelay;

  FMods[0] := TStkLFO.Create(SampleRate);
  FMods[1] := TStkLFO.Create(SampleRate);
  FMods[0].Frequency := 0.2;
  FMods[1].Frequency := 0.222222;
  FModDepth := 0.05;
  FEffectMix := 0.5;
  Clear;
end;

destructor TStkChorus.Destroy;
begin
  inherited Destroy;
  FDelayLine[0].Free;
  FDelayLine[1].Free;
  FMods[0].Free;
  FMods[1].Free;
end;

procedure TStkChorus.Clear;
begin
  FDelayLine[0].Clear;
  FDelayLine[1].Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
end;

procedure TStkChorus.SetEffectMix(const Value: Single);
begin
  if (Value < 0.0)
   then FEffectMix := 0.0 else
  if (Value > 1.0)
   then FEffectMix := 1.0
   else FEffectMix := Value;
end;

procedure TStkChorus.SetModDepth(const Value: Single);
begin
 if FModDepth <> Value
  then FModDepth := Value;
end;

procedure TStkChorus.SetModFrequency(const Value: Single);
begin
 if FMods[0].Frequency <> Value then
  begin
   FMods[0].Frequency := Value;
   FMods[1].Frequency := Value * 1.1111;
  end;
end;

function TStkChorus.GetLastOutput: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkChorus.GetModFrequency: Single;
begin
 result := FMods[0].Frequency
end;

function TStkChorus.Tick(const Input: Single): Single;
begin
  FDelayLine[0].Delay := FBaseLength * 0.707 * (1.0 + FMods[0].Tick);
  FDelayLine[1].Delay := FBaseLength * 0.5 * (1.0 - FMods[1].Tick);
  FLastOutput[0] := Input * (1.0 - FEffectMix);
  FLastOutput[0] := FLastOutput[0] + FEffectMix * FDelayLine[0].Tick(Input);
  FLastOutput[1] := Input * (1.0 - FEffectMix);
  FLastOutput[1] := FLastOutput[1] + FEffectMix * FDelayLine[1].Tick(Input);
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkChorus.Tick(Vector: PSingle; vectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := Vector;
end;

end.
