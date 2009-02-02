unit DAV_StkModulate;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK periodic/random modulator.

   This class combines random and periodic modulations to give a nice, natural
   human modulation function.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkSubNoise, DAV_StkOnePole, DAV_StkLfo;

type
  TStkModulate = class(TStk)
  private
    procedure SetVibratoRate(const Value: Single); // in Hz
    procedure SetVibratoGain(const Value: Single);
    procedure SetRandomGain(const Value: Single);
  protected
    FVibrato     : TLFO;
    FNoise       : TSubNoise;
    FFilter      : TOnePole;
    FVibratoGain : Single;
    FRandomGain  : Single;
    FLastOutput  : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Reset internal state.
    procedure Reset;

    // Compute one output sample.
    function Tick: Single; overload;

    // Return \e VectorSize outputs in \e Vector.
    function Tick(Vector: PSingle; VectorSize: Integer): PSingle; overload;

  published
    property VibratoRate: Single read GetVibratoRate write SetVibratoRate;
    property VibratoGain: Single read FVibratoRate write SetVibratoGain;
    property RandomGain: Single read FRandomGain write SetRandomGain;
    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TModulate.Create;
begin
  inherited Create(SampleRate);
  FVibrato := TLFO.Create(SampleRate);
  FVibrato.Frequency := 6.0;
  FVibratoGain := 0.04;

  FNoise := TSubNoise.Create(SampleRate, 330);
  FRandomGain := 0.05;

  FFilter := TOnePole.Create(SampleRate, 0.999);
  FFilter.Gain := FRandomGain;
end;

destructor TModulate.Destroy;
begin
  inherited Destroy;
  FVibrato.Free;
  FNoise.Free;
  FFilter.Free;
end;

procedure TModulate.Reset;
begin
  FLastOutput := 0.0;
end;

function TModulate.GetVibratoRate: Single;
begin
 result := FVibrato.Frequency;
end;

procedure TModulate.SetVibratoRate(const Value: Single);
begin
 if VibratoRate <> Value then
  begin
   FVibrato.Frequency := Value;
  end;
end;

procedure TModulate.SetVibratoGain(const Value: Single);
begin
 if FVibratoGain <> Value then
  begin
   FVibratoGain := Value;
  end;
end;

procedure TModulate.SetRandomGain(const Value: Single);
begin
 if FRandomGain <> Value then
  begin
   FRandomGain := Value;
   FFilter.Gain := FRandomGain;
  end;
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
  i: Integer;
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

end.
