unit DAV_StkMultiOsc2;

interface

uses mm_osc, Math;

type
  TMultiOsc2 = class(TOsc)
  private
    wave: integer;
    pwm: single;
  public
    constructor Create(sr: integer);
    procedure SetMorph(morph: single);
    function GetMorph: single;
    procedure SetActiveWave(i: integer);
    function GetActiveWave: integer;
    function Process: single; override;
  end;

implementation

function fmod(x: single): single;
begin
  Result := x - floor(x);
end;

function fpulse(x, a: single): single;
begin
  if (fmod(x) < a) then
    Result := -1
  else
    Result := 1;
end;

function fsaw(x, a: single): single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  x := fmod(x);
  if (x < a) then
    Result := x / a * 2 - 1
  else
    Result := (1 - x) / (1 - a) * 2 - 1;
end;

function ftri(x, a: single): single;
begin
  x := fmod(x + 0.25);
  a := 1 - a;
  if (a < 0.00001) then
    a := 0.00001;
  if (x < 0.5) then
    x := x * 4 - 1
  else
    x := (1 - x) * 4 - 1;
  x := x / (-a);
  if (x < -1) then
    x := -1
  else if (x > 1) then
    x := 1;
  Result := x;
end;

function fpower(x, a: single): single;
begin
  x := fmod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := power(x, exp((a - 0.5) * 10)) * 2 - 1;
end;

function fgauss(x, a: single): single;
begin
  x := fmod(x) * 2 - 1;
  if (a < 0.00001) then
    a := 0.00001;
  Result := exp(-x * x * (exp(a * 8) + 5)) * 2 - 1;
end;

function fdiode(x, a: single): single;
begin
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  a := a * 2 - 1;
  x := cos((x + 0.5) * 2 * pi) - a;
  if (x < 0) then
    x := 0;
  Result := x / (1 - a) * 2 - 1;
end;

function fsine(x, a: single): single;
var
  y: single;
begin
  x := fmod(x);
  if (x < 0.5) then
    y := 1
  else
    y := -1;
  Result := sin(x * 2 * pi) * (1 - a) + a * y;
end;

function fabssine(x, a: single): single;
begin
  x := fmod(x);
  if (a < 0.00001) then
    a := 0.00001
  else if (a > 0.99999) then
    a := 0.99999;
  Result := sin(power(x, exp((a - 0.5) * 5)) * pi) * 2 - 1;
end;

function fpulsesine(x, a: single): single;
begin
  if (a < 0.00001) then
    a := 0.00001;
  x := (fmod(x) - 0.5) * exp((a - 0.5) * log10(128));
  if (x < -0.5) then
    x := -0.5
  else if (x > 0.5) then
    x := 0.5;
  x := sin(x * pi * 2);
  Result := x;
end;

function fstretchsine(x, a: single): single;
var
  b: single;
begin
  x := fmod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 4;
  if (a > 0) then
    a := a * 2;
  a := power(3, a);
  b := power(abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -sin(b * pi);
end;

function fchirp(x, a: single): single;
begin
  x := fmod(x) * 2 * pi;
  a := (a - 0.5) * 4;
  if (a < 0) then
    a := a * 2;
  a := power(3, a);
  Result := sin(x / 2) * sin(a * x * x);
end;

function fsinex(x, a: single): single;
begin
  x := fmod(x);
  Result := 0.5 * (sin(x * 2 * pi) + sin(x * 2 * pi * (1 + a * 10)));
end;

function fabsstretchsine(x, a: single): single;
var
  b: single;
begin
  x := fmod(x + 0.5) * 2 - 1;
  a := (a - 0.5) * 9;
  a := power(3, a);
  b := power(abs(x), a);
  if (x < 0) then
    b := -b;
  Result := -2 * power(sin(b * pi), 2) + 1;
end;

constructor TMultiOsc2.Create(sr: integer);
begin
  wave := 0;
  inherited Create(sr);
end;

function TMultiOsc2.GetActiveWave: integer;
begin
  Result := wave;
end;

function TMultiOsc2.GetMorph: single;
begin
  Result := pwm;
end;

function TMultiOsc2.Process: single;
var
  y, j: single;
begin
  j := srate / freq;
  phase := cnt / j;
  case wave of
    0 : y := fsine(phase, pwm);
    1 : y := fsaw(phase, pwm);
    2 : y := fpulse(phase, pwm);
    3 : y := ftri(phase, pwm);
    4 : y := tmp;
    5 : y := random * 2 - 1;
    6 : y := fpower(phase, pwm);
    7 : y := fgauss(phase, pwm);
    8 : y := fdiode(phase, pwm);
    9 : y := fstretchsine(phase, pwm);
    10 : y := fpulsesine(phase, pwm);
    11 : y := fabssine(phase, pwm);
    12 : y := fabsstretchsine(phase, pwm);
    13 : y := fchirp(phase, pwm);

    14 : if ((pwm = 1) or (phase < pwm)) then
        y := sin(pi * phase / pwm)
      else
        y := -sin(pi * (phase - pwm) / (1 - pwm));
    15 : if ((pwm = 1) or (phase < pwm)) then
        y := phase / pwm
      else
        y := ((phase - pwm) / (1 - pwm)) - 1;
    16 : if (pwm = 0) then
       begin
        if (phase < 0.5 * (pwm + 1)) then
          y := -2 * (phase - pwm) / (1 - pwm)
        else
          y := -1 + (2 * phase - (pwm + 1)) / (1 - pwm);
       end else
      if (pwm = 1) then
       begin
        if (phase < 0.5 * pwm) then
          y := 2 * phase / pwm
        else
          y := 1 - (2 * phase - pwm) / pwm;
       end else
      if (phase < pwm / 2) then
        y := 2 * phase / pwm
      else
      if ((phase >= pwm / 2) and (phase < pwm)) then
        y := 1 - (2 * phase - pwm) / pwm
      else if ((phase >= pwm) and (phase < 0.5 * (pwm + 1))) then
        y := -2 * (phase - pwm) / (1 - pwm)
      else
        y := -1 + (2 * phase - (pwm + 1)) / (1 - pwm);
    17 : y := fsine(phase * (pwm), 0) * ((1 - pwm) + 1) - (1 - pwm);
    18 : y := fsinex(phase, pwm);
  else y := 0;
   end;
  cnt := cnt + 1;
  while (cnt > j) do
   begin
    cnt := cnt - j;
    tmp := random * 2 - 1
   end;
  Result := y;
end;

procedure TMultiOsc2.SetActiveWave(i: integer);
begin
  wave := i;
end;

procedure TMultiOsc2.SetMorph(morph: single);
begin
  if morph > 1 then
    morph := 1
  else if morph < 0 then
    morph := 0;
  pwm := morph;
end;

end.
