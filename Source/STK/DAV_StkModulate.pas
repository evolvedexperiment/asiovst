unit DAV_StkModulate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK periodic/random modulator.

   This class combines random and periodic modulations to give a nice, natural
   human modulation function.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkSubNoise, DAV_StkOnePole, DAV_StkLfo;

type
  TStkModulate = class(TStk)
  protected
    FVibrato: TLFO;
    FNoise: TSubNoise;
    FFilter: TOnePole;
    FVibratoGain, FRandomGain, FLastOutput: Single;
  public
    constructor Create(SampleRate: Single); override;
    destructor Destroy; override;

    // Reset internal state.
    procedure reset;

    // Set the periodic (FVibrato) rate or frequency in Hz.
    procedure setVibratoRate(ARate: Single);

    // Set the periodic (FVibrato) gain.
    procedure setVibratoGain(AGain: Single);

    // Set the random modulation gain.
    procedure setRandomGain(AGain: Single);

    // Compute one output sample.
    function Tick: Single; overload;

    // Return \e VectorSize outputs in \e Vector.
    function Tick(Vector: PSingle; VectorSize: Integer): PSingle; overload;

    // Return the last computed output value.
    function LastOut: Single;
  end;

implementation

constructor TModulate.Create;
begin
  inherited Create(SampleRate);
  FVibrato := TLFO.Create(SampleRate);
  FVibrato.setFrequency(6.0);
  FVibratoGain := 0.04;

  FNoise := TSubNoise.Create(SampleRate, 330);
  FRandomGain := 0.05;

  FFilter := TOnePole.Create(SampleRate, 0.999);
  FFilter.setGain(FRandomGain);
end;

destructor TModulate.Destroy;
begin
  inherited Destroy;
  FVibrato.Free;
  FNoise.Free;
  FFilter.Free;
end;

procedure TModulate.reset;
begin
  FLastOutput := 0.0;
end;

procedure TModulate.setVibratoRate;
begin
  FVibrato.setFrequency(ARate);
end;

procedure TModulate.setVibratoGain;
begin
  FVibratoGain := AGain;
end;

procedure TModulate.setRandomGain;
begin
  FRandomGain := AGain;
  FFilter.setGain(FRandomGain);
end;

function TModulate.Tick: Single;
begin
  // Compute periodic and random modulations.
  FLastOutput := FVibratoGain * FVibrato.Tick;
  FLastOutput := FLastOutput + FFilter.Tick(FNoise.Tick);
  Result := FLastOutput;
end;

function TModulate.Tick(Vector: PSingle; VectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick;
    Inc(p);
   end;
  Result := Vector;
end;

function TModulate.LastOut: Single;
begin
  Result := FLastOutput;
end;


end.
