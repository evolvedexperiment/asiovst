unit DAV_StkLFO;

interface

uses
  Stk;

type
  TLFO = class(TStk)
  protected
    wave: integer;
    cnt: my_float;
    tmp, pofs, phase, freq: my_float;
  public
    constructor Create(sr: my_float);
    destructor Destroy;
    procedure Reset;
    procedure SetPhase(sp: my_float);
    function GetPhase: my_float;
    procedure SetPhaseOffset(sp: my_float);
    procedure AddPhaseOffset(sp: my_float);
    function GetPhaseOffset: my_float;
    procedure SetFrequency(sf: my_float);
    function GetFrequency: my_float;
    function tick: my_float;
    procedure SetActiveWave(i: integer);
    function GetActiveWave: integer;
  end;

implementation


function TLFO.GetActiveWave: Integer;
begin
  Result := wave;
end;

procedure TLFO.SetActiveWave(i: Integer);
begin
  wave := i;
end;

constructor TLFO.Create(sr: my_float);
begin
  inherited Create(sr);
  SetFrequency(1);
  SetPhase(0);
  SetPhaseOffset(0);
  Reset;
  wave := 0;
end;

function TLFO.tick: my_float;
var
  y, j: my_float;
begin
  j := srate / freq;
  phase := cnt / j;
  case wave of
    0 : y := sin(2 * pi * phase);
    1 : if phase < 0.5 then
        y := 2 * phase
      else
        y := 2 * phase - 2;
    2 : if phase < 0.5 then
        y := 1
      else
        y := -1;
    3 : if (phase >= 0.25) and (phase < 0.75) then
        y := -4 * (phase + 0.25) + 3
      else if (phase < 0.25) then
        y := 4 * (phase + 0.25) - 1
      else
        y := 4 * (phase + 0.25) - 5;
    4 : y := tmp;
  else y := random * 2 - 1;
   end;
  cnt := cnt + 1;
  while (cnt >= j) do
   begin
    cnt := cnt - j;
    tmp := random * 2 - 1
   end;
  Result := y;
end;

procedure TLFO.SetFrequency(sf: my_float);
begin
  freq := sf;
end;

function TLFO.GetFrequency: my_float;
begin
  Result := freq;
end;

procedure TLFO.SetPhaseOffset(sp: my_float);
begin
  while sp >= 1 do
    sp := sp - 1;
  while sp < 0 do
    sp := sp + 1;
  pofs := sp;
  SetPhase(pofs + phase);
end;

function TLFO.GetPhaseOffset: my_float;
begin
  Result := pofs;
end;

procedure TLFO.SetPhase(sp: my_float);
begin
  while sp >= 1 do
    sp := sp - 1;
  while sp < 0 do
    sp := sp + 1;
  phase := sp;
  cnt := phase * srate / freq;
end;

function TLFO.GetPhase: my_float;
begin
  Result := phase;
end;

procedure TLFO.reset;
begin
  SetPhase(pofs);
end;

destructor TLFO.Destroy;
begin
  inherited Destroy;
end;

procedure TLFO.AddPhaseOffset(sp: my_float);
begin
  SetPhase(phase + sp);
end;

end.

