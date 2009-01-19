unit DAV_StkFormSwep;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK sweepable formant filter class.

   This public BiQuad filter subclass implements a formant (resonance) which
   can be "swept" over time from one FFrequency setting to another.
   It provides methods for controlling the sweep rate and target FFrequency.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkBiquad;

type
  TStkFormantSweep = class(TStkBiQuad)
  protected
    FDirty             : Boolean;
    FFrequency         : Single;
    FRadius            : Single;
    FStartFrequency    : Single;
    FStartRadius       : Single;
    FStartGain         : Single;
    FTargetFrequency   : Single;
    FTargetRadius      : Single;
    FTargetGain        : Single;
    FDeltaFrequency    : Single;
    FDeltaRadius       : Single;
    FDeltaGain         : Single;
    FSweepState        : Single;
    FSweepRate         : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Sets the filter coefficients for a resonance at \e FFrequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e FFrequency (in Hz)
    and \e FRadius from the z-plane origin.  The filter zeros are
    placed at z := 1, z := -1, and the coefficients are then normalized to
    produce a constant unity gain (independent of the filter \e gain
    parameter).  The resulting filter FFrequency response has a
    resonance at the given \e FFrequency.  The closer the poles are to
    the unit-circle (\e FRadius close to one), the narrower the
    resulting resonance width.
  }
    procedure setResonance(AFrequency, ARadius: Single);

    // Set both the current and target resonance parameters.
    procedure setStates(AFrequency, ARadius: Single; AGain: Single = 1.0);

    // Set target resonance parameters.
    procedure setTargets(AFrequency, ARadius: Single; AGain: Single = 1.0);

    // Set the sweep rate (between 0.0 - 1.0).
  {
    The formant parameters are varied in increments of the
    sweep rate between their current and target values.
    A sweep rate of 1.0 will produce an immediate change in
    resonance parameters from their current values to the
    target values.  A sweep rate of 0.0 will produce no
    change in resonance parameters.  
  }
    procedure SetSweepRate(ARate: Single);

    // Set the sweep rate in terms of a time value in seconds.
  {
    This method adjusts the sweep rate based on a
    given time for the formant parameters to reach
    their target values.
 }
    procedure SetSweepTime(ATime: Single);

    // Input one sample to the filter and return one output.
    function Tick(sample: Single): Single; overload;

    // Input \e VectorSize samples to the filter and return an equal number of outputs in \e vector.
    function Tick(vector: PSingle; VectorSize: Integer): PSingle; overload;
  end;

implementation

constructor TStkFormantSweep.Create;
begin
  inherited Create(SampleRate);
  FFrequency       := 0;
  FRadius          := 0;
  FTargetGain      := 1;
  FTargetFrequency := 0;
  FTargetRadius    := 0;
  FDeltaGain       := 0;
  FDeltaFrequency  := 0;
  FDeltaRadius     := 0;
  FSweepState      := 0;
  FSweepRate       := 0.002;
  FDirty           := False;
  Clear;
end;

destructor TStkFormantSweep.Destroy;
begin
  inherited Destroy;
end;

procedure TStkFormantSweep.setResonance;
begin
  FDirty := False;
  FRadius := ARadius;
  FFrequency := AFrequency;
  inherited setResonance(FFrequency, FRadius, True);
end;

procedure TStkFormantSweep.setStates;
begin
  FDirty := False;

  if ((FFrequency <> AFrequency) or (FRadius <> ARadius)) then
    inherited setResonance(AFrequency, ARadius, True);

  FFrequency := AFrequency;
  FRadius := ARadius;
  FGain := AGain;
  FTargetFrequency := AFrequency;
  FTargetRadius := ARadius;
  FTargetGain := AGain;
end;

procedure TStkFormantSweep.setTargets;
begin
  FDirty := True;
  FStartFrequency := FFrequency;
  FStartRadius := FRadius;
  FStartGain := gain;
  FTargetFrequency := AFrequency;
  FTargetRadius := ARadius;
  FTargetGain := AGain;
  FDeltaFrequency := AFrequency - FFrequency;
  FDeltaRadius := ARadius - FRadius;
  FDeltaGain := AGain - gain;
  FSweepState := 0;
end;

procedure TStkFormantSweep.SetSweepRate;
begin
  FSweepRate := ARate;
  if (FSweepRate > 1.0) then
    FSweepRate := 1.0;
  if (FSweepRate < 0.0) then
    FSweepRate := 0.0;
end;

procedure TStkFormantSweep.SetSweepTime;
begin
  FSweepRate := 1.0 / (ATime * srate);
  if (FSweepRate > 1.0) then
    FSweepRate := 1.0;
  if (FSweepRate < 0.0) then
    FSweepRate := 0.0;
end;

function TStkFormantSweep.Tick(sample: Single): Single;
begin
  if (FDirty) then
   begin
    FSweepState := FSweepState + FSweepRate;
    if (FSweepState >= 1.0) then
     begin
      FSweepState := 1.0;
      FDirty := False;
      FRadius := FTargetRadius;
      FFrequency := FTargetFrequency;
      gain := FTargetGain;
     end
    else
     begin
      FRadius := FStartRadius + (FDeltaRadius * FSweepState);
      FFrequency := FStartFrequency + (FDeltaFrequency * FSweepState);
      gain := FStartGain + (FDeltaGain * FSweepState);
     end;
    inherited setResonance(FFrequency, FRadius, True);
   end;
  Result := inherited Tick(sample);
end;

function TStkFormantSweep.Tick(vector: PSingle; VectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
