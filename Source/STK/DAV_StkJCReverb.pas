unit DAV_StkJCReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  John Chowning's reverberator class.

   This class is derived from the CLM TStkJCRev function, which is based on the
   use of networks of simple allpass and comb delay filters.
   This class implements three series allpass units, followed by four parallel
   comb filters, and two decorrelation delay lines in parallel at the output.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkReverb, DAV_StkDelay, Math;

type
  TStkJCRev = class(TStkReverb)
  protected
    FAllpassDelays      : array[0..2] of TStkDelay;
    FCombDelays         : array[0..3] of TStkDelay;
    FOutLeftDelay       : TStkDelay;
    FOutRightDelay      : TStkDelay;
    FAllpassCoefficient : Single;
    FCombCoefficient    : array[0..3] of Single;
  public
    constructor Create(SampleRate, T60: Single); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
    function Tick(const Input: Single): Single; override;
  end;

implementation

uses
  SysUtils, DAV_StkFilter;

constructor TStkJCRev.Create;
var
  lengths  : array[0..8] of Integer;
  scaler   : Double;
  delay, i : Integer;
begin
  inherited Create(SampleRate);
  // Delay lengths for 44100 Hz sample rate.
  lengths[0] := 1777;
  lengths[1] := 1847;
  lengths[2] := 1993;
  lengths[3] := 2137;
  lengths[4] := 389;
  lengths[5] := 127;
  lengths[6] := 43;
  lengths[7] := 211;
  lengths[8] := 179;
  scaler := SampleRate / 44100.0;

  if (scaler <> 1.0) then
    for i := 0 to 8 do
     begin
      delay := round(floor(scaler * lengths[i]));
      if ((delay and 1) = 0) then
        delay := delay + 1;
      while (not isPrime(delay)) do
        delay := delay + 2;
      lengths[i] := delay;
     end;

  for i := 0 to 2 do
    FAllpassDelays[i] := TStkDelay.Create(SampleRate, lengths[i + 4], lengths[i + 4]);

  for i := 0 to 3 do
   begin
    FCombDelays[i] := TStkDelay.Create(SampleRate, lengths[i], lengths[i]);
    FCombCoefficient[i] := Power(10, (-3 * lengths[i] / (T60 * SampleRate)));
   end;

  FOutLeftDelay := TStkDelay.Create(SampleRate, lengths[7], lengths[7]);
  FOutRightDelay := TStkDelay.Create(SampleRate, lengths[8], lengths[8]);
  FAllpassCoefficient := 0.7;
  FEffectMix := 0.3;
  Clear;
end;

destructor TStkJCRev.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FAllpassDelays[0]);
  FreeAndNil(FAllpassDelays[1]);
  FreeAndNil(FAllpassDelays[2]);
  FreeAndNil(FCombDelays[0]);
  FreeAndNil(FCombDelays[1]);
  FreeAndNil(FCombDelays[2]);
  FreeAndNil(FCombDelays[3]);
  FreeAndNil(FOutLeftDelay);
  FreeAndNil(FOutRightDelay);
end;

procedure TStkJCRev.Clear;
begin
  FAllpassDelays[0].Clear;
  FAllpassDelays[1].Clear;
  FAllpassDelays[2].Clear;
  FCombDelays[0].Clear;
  FCombDelays[1].Clear;
  FCombDelays[2].Clear;
  FCombDelays[3].Clear;
  FOutRightDelay.Clear;
  FOutLeftDelay.Clear;
  FLastOutput[0] := 0.0;
  FLastOutput[1] := 0.0;
end;

function TStkJCRev.Tick(const Input: Single): Single;
var
  temp    : Single;
  filtout : Single;
  tmp     : Array [0..6] of Single;

begin
  temp := FAllpassDelays[0].LastOutput;
  tmp[0] := FAllpassCoefficient * temp;
  tmp[0] := tmp[0] + input;
  FAllpassDelays[0].Tick(tmp[0]);
  tmp[0] := -(FAllpassCoefficient * tmp[0]) + temp;

  temp := FAllpassDelays[1].LastOutput;
  tmp[1] := FAllpassCoefficient * temp;
  tmp[1] := tmp[1] + tmp[0];
  FAllpassDelays[1].Tick(tmp[1]);
  tmp[1] := -(FAllpassCoefficient * tmp[1]) + temp;

  temp := FAllpassDelays[2].LastOutput;
  tmp[2] := FAllpassCoefficient * temp;
  tmp[2] := tmp[2] + tmp[1];
  FAllpassDelays[2].Tick(tmp[2]);
  tmp[2] := -(FAllpassCoefficient * tmp[2]) + temp;

  tmp[3] := tmp[2] + (FCombCoefficient[0] * FCombDelays[0].LastOutput);
  tmp[4] := tmp[2] + (FCombCoefficient[1] * FCombDelays[1].LastOutput);
  tmp[5] := tmp[2] + (FCombCoefficient[2] * FCombDelays[2].LastOutput);
  tmp[6] := tmp[2] + (FCombCoefficient[3] * FCombDelays[3].LastOutput);

  FCombDelays[0].Tick(tmp[3]);
  FCombDelays[1].Tick(tmp[4]);
  FCombDelays[2].Tick(tmp[5]);
  FCombDelays[3].Tick(tmp[6]);

  filtout := tmp[3] + tmp[4] + tmp[5] + tmp[6];

  FLastOutput[0] := FEffectMix * (FOutLeftDelay.Tick(filtout));
  FLastOutput[1] := FEffectMix * (FOutRightDelay.Tick(filtout));
  temp := (1.0 - FEffectMix) * input;
  FLastOutput[0] := FLastOutput[0] + temp;
  FLastOutput[1] := FLastOutput[1] + temp;

  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

end.
