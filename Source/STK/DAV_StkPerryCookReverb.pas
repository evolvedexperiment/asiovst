unit DAV_StkPerryCookReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ Perry's simple reverberator class.

  This class is based on some of the famous Stanford/CCRMA reverbs
  (NRev, KipRev), which were based on the Chowning/Moorer/Schroeder
  reverberators using networks of simple allpass and comb delay filters.
  This class implements two series allpass units and two parallel comb filters.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkReverb, DAV_StkDelay, Math;

type
  TStkPerryCookReverb = class(TStkReverb)
  protected
    FAllpassDelays      : array[0..1] of TStkDelay;
    FCcombDelays        : array[0..1] of TStkDelay;
    FAllpassCoefficient : Single;
    FCombCoefficient    : array[0..1] of Single;
  public
    // Class constructor taking a T60 decay time argument.
    constructor Create(const SampleRate, T60: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Compute one output sample.
    function Tick(Input: Single): Single; override;
  end;

implementation

constructor TStkPerryCookReverb.Create(const SampleRate, T60: Single);
const
  lengths: array[0..3] of integer = (353, 1097, 1777, 2137);
var
  scaler: double;
  delay, i: integer;
begin
  inherited Create(constsr);
  // Delay lengths for 44100 Hz sample rate.
  scaler := srate / 44100.0;

  // Scale the delay lengths if necessary.
  if (scaler <> 1.0) then
    for i := 0 to 3 do
     begin
      delay := round(floor(scaler * lengths[i]));
      if ((delay and 1) = 0) then
        delay := delay + 1;
      while (not isPrime(delay)) do
        delay := delay + 2;
      lengths[i] := delay;
     end;

  for i := 0 to 1 do
   begin
    FAllpassDelays[i] := TStkDelay.Create(srate, lengths[i], lengths[i]);
    FCcombDelays[i] := TStkDelay.Create(srate, lengths[i + 2], lengths[i + 2]);
    FCombCoefficient[i] := power(10, (-3 * lengths[i + 2] / (T60 * srate)));
   end;

  FAllpassCoefficient := 0.7;
  effectMix := 0.5;
  Clear;
end;

destructor TStkPerryCookReverb.Destroy;
begin
 FreeAndNil(FAllpassDelays[0]);
 FreeAndNil(FAllpassDelays[1]);
 FreeAndNil(FCcombDelays[0]);
 FreeAndNil(FCcombDelays[1]);
 inherited Destroy;
end;

procedure TStkPerryCookReverb.Clear;
begin
  FAllpassDelays[0].Clear();
  FAllpassDelays[1].Clear();
  FCcombDelays[0].Clear();
  FCcombDelays[1].Clear();
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
end;

function TStkPerryCookReverb.tick(Input: Single): Single;
var
  temp, temp0, temp1, temp2, temp3: Single;
begin
  temp := FAllpassDelays[0].lastOut();
  temp0 := FAllpassCoefficient * temp;
  temp0 := temp0 + Input;
  FAllpassDelays[0].tick(temp0);
  temp0 := -(FAllpassCoefficient * temp0) + temp;

  temp := FAllpassDelays[1].lastOut();
  temp1 := FAllpassCoefficient * temp;
  temp1 := temp1 + temp0;
  FAllpassDelays[1].tick(temp1);
  temp1 := -(FAllpassCoefficient * temp1) + temp;

  temp2 := temp1 + (FCombCoefficient[0] * FCcombDelays[0].lastOut());
  temp3 := temp1 + (FCombCoefficient[1] * FCcombDelays[1].lastOut());

  lastOutput[0] := effectMix * (FCcombDelays[0].tick(temp2));
  lastOutput[1] := effectMix * (FCcombDelays[1].tick(temp3));
  temp := (1.0 - effectMix) * Input;
  lastOutput[0] := lastOutput[0] + temp;
  lastOutput[1] := lastOutput[1] + temp;
  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

end.
