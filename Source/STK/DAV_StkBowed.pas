unit DAV_StkBowed;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkBowed string instrument class.

  This class implements a TStkBowed string model, a la Smith (1986), after
  McIntyre, Schumacher, Woodhouse (1983).

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Bow Pressure = 2
    - Bow Position = 4
    - FVibrato Frequency = 11
    - FVibrato Gain = 1
    - Volume = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelayl, DAV_StkBowtabl,
  DAV_StkOnepole, DAV_StkBiquad, DAV_StkLfo, DAV_StkAdsr, Windows;

type
  TStkBowed = class(TInstrmnt)
  protected
    FNeckDelay    : TDelayl;
    FBridgeDelay  : TDelayl;
    FBowTable     : TBowTable;
    FStringFilter : TOnePole;
    FBodyFilter   : TBiquad;
    FVibrato      : TLfo;
    FAdsr         : TAdsr;
    FMaxVelocity  : Single;
    FBaseDelay    : Single;
    FVibratoGain  : Single;
    FBetaRatio    : Single;
  public
    constructor Create(SampleRate, lowestFrequency: Single); override;
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

    // Set FVibrato Gain.
    procedure setVibrato(Gain: Single);

    // Apply breath pressure to instrument with given Amplitude and Rate of increase.
    procedure startBowing(Amplitude, Rate: Single);

    // Decrease breath pressure with given Rate of decrease.
    procedure stopBowing(Rate: Single);

    // Start a note with the given Frequency and Amplitude.
    procedure noteOn(Frequency, Amplitude: Single);

    // Stop a note with the given Amplitude (speed of decay).
    procedure noteOff(Amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: Single);
  end;

implementation

constructor TStkBowed.Create;
var
  Length: longint;
begin
  inherited Create(SampleRate);
  Length := round(SampleRate / lowestFrequency + 1);
  FNeckDelay := TDelayl.Create(SampleRate, 100.0, Length);
  Length := Length shr 1;
  FBridgeDelay := TDelayl.Create(SampleRate, 29.0, Length);
  FBowTable := TBowTable.Create(SampleRate);
  FBowTable.setSlope(3.0);
  FVibrato := TLfo.Create(SampleRate);
  FVibrato.setFrequency(6.12723);
  FVibratoGain := 0.0;

  FStringFilter := TOnePole.Create(SampleRate);
  FStringFilter.setPole((0.6 - (0.1 * 22050.0 / SampleRate)));
  FStringFilter.setGain(0.95);

  FBodyFilter := TBiquad.Create(SampleRate);
  FBodyFilter.setResonance(500.0, 0.85, True);
  FBodyFilter.setGain(0.2);

  FAdsr := TAdsr.Create(SampleRate);
  FAdsr.setAllTimes(0.02, 0.005, 0.9, 0.01);

  FBetaRatio := 0.127236;

 // Necessary to initialize internal variables.
  setFrequency(220.0);
end;

destructor TStkBowed.Destroy;
begin
  inherited Destroy;
  FNeckDelay.Free;
  FBridgeDelay.Free;
  FBowTable.Free;
  FStringFilter.Free;
  FBodyFilter.Free;
  FVibrato.Free;
  FAdsr.Free;
end;

procedure TStkBowed.Clear;
begin
  FNeckDelay.Clear;
  FBridgeDelay.Clear;
end;

procedure TStkBowed.setFrequency;
var
  Freakwency: Single;
begin
  Freakwency := Frequency;
  if (Frequency <= 0.0) then
    Freakwency := 220.0;

  // Delay := length - approximate filter delay.
  FBaseDelay := srate / Freakwency - 4.0;
  if (FBaseDelay <= 0.0) then
    FBaseDelay := 0.3;
  FBridgeDelay.setDelay(FBaseDelay * FBetaRatio);
                  // bow to bridge length
  FNeckDelay.setDelay(FBaseDelay * (1.0 - FBetaRatio));
 // bow to nut (finger) length
end;

procedure TStkBowed.startBowing;
begin
  FAdsr.setRate(Rate);
  FAdsr.keyOn();
  FMaxVelocity := 0.03 + (0.2 * Amplitude);
end;

procedure TStkBowed.stopBowing;
begin
  FAdsr.setRate(Rate);
  FAdsr.keyOff();
end;

procedure TStkBowed.noteOn;
begin
  startBowing(Amplitude, Amplitude * 0.001);
  setFrequency(Frequency);
end;

procedure TStkBowed.noteOff;
begin
  stopBowing((1.0 - Amplitude) * 0.005);
end;

procedure TStkBowed.setVibrato;
begin
  FVibratoGain := Gain;
end;

function TStkBowed.Tick: Single;
var
  bowVelocity, bridgeRefl, nutRefl, newVel, velDiff, stringVel: Single;
begin
  bowVelocity := FMaxVelocity * FAdsr.Tick();
  bridgeRefl := -stringFilter.Tick(FBridgeDelay.lastOut());
  nutRefl := -FNeckDelay.lastOut();
  stringVel := bridgeRefl + nutRefl;               // Sum is String Velocity
  velDiff := bowVelocity - stringVel;              // Differential Velocity
  newVel := velDiff * FBowTable.Tick(velDiff);   // Non-Linear Bow Function
  FNeckDelay.Tick(bridgeRefl + newVel);           // Do string propagations
  FBridgeDelay.Tick(nutRefl + newVel);

  if (FVibratoGain > 0.0) then
    FNeckDelay.setDelay((FBaseDelay * (1.0 - FBetaRatio)) +
      (FBaseDelay * FVibratoGain * FVibrato.Tick()));
  lastOutput := FBodyFilter.Tick(FBridgeDelay.lastOut());
  Result := lastOutput;
end;

procedure TStkBowed.controlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then norm := 0.0
  else if (norm > 1.0) then norm := 1.0;

  if (number = CMIDIBowPressure) then // 2
    FBowTable.setSlope(5.0 - (4.0 * norm))
  else if (number = CMIDIBowPosition) then
   begin // 4
    FBetaRatio := 0.027236 + (0.2 * norm);
    FBridgeDelay.setDelay(FBaseDelay * FBetaRatio);
    FNeckDelay.setDelay(FBaseDelay * (1.0 - FBetaRatio));
   end
  else if (number = CMIDIModFrequency) then // 11
    FVibrato.setFrequency(norm * 12.0)
  else if (number = CMIDIModWheel) then // 1
    FVibratoGain := (norm * 0.4)
  else if (number = CMIDIAfterTouchCont) then // 128
    FAdsr.setTarget(norm);
end;

end.
