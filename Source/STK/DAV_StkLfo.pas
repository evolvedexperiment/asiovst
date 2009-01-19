unit DAV_StkLFO;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk;

type
  TStkLFO = class(TStk)
  protected
    FWave  : Integer;
    Cnt    : Single;
    FTmp   : Single;
    FPofs  : Single;
    FPhase : Single;
    FFreq  : Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;
    procedure Reset;
    procedure SetPhase(Value: Single);
    function GetPhase: Single;
    procedure SetPhaseOffset(Value: Single);
    procedure AddPhaseOffset(Value: Single);
    function GetPhaseOffset: Single;
    procedure SetFrequency(sf: Single);
    function GetFrequency: Single;
    function Tick: Single;
    procedure SetActiveWave(i: Integer);
    function GetActiveWave: Integer;
  end;

implementation


function TStkLFO.GetActiveWave: Integer;
begin
  Result := FWave;
end;

procedure TStkLFO.SetActiveWave(i: Integer);
begin
  FWave := i;
end;

constructor TStkLFO.Create(SampleRate: Single);
begin
  inherited Create(SampleRate);
  SetFrequency(1);
  SetPhase(0);
  SetPhaseOffset(0);
  Reset;
  FWave := 0;
end;

function TStkLFO.Tick: Single;
var
  y, j: Single;
begin
  j := srate / FFreq;
  FPhase := Cnt / j;
  case FWave of
    0 : y := sin(2 * pi * FPhase);
    1 : if FPhase < 0.5 then
        y := 2 * FPhase
      else
        y := 2 * FPhase - 2;
    2 : if FPhase < 0.5 then
        y := 1
      else
        y := -1;
    3 : if (FPhase >= 0.25) and (FPhase < 0.75) then
        y := -4 * (FPhase + 0.25) + 3
      else if (FPhase < 0.25) then
        y := 4 * (FPhase + 0.25) - 1
      else
        y := 4 * (FPhase + 0.25) - 5;
    4 : y := FTmp;
  else y := random * 2 - 1;
   end;
  Cnt := Cnt + 1;
  while (Cnt >= j) do
   begin
    Cnt := Cnt - j;
    FTmp := random * 2 - 1;
   end;
  Result := y;
end;

procedure TStkLFO.SetFrequency(sf: Single);
begin
  FFreq := sf;
end;

function TStkLFO.GetFrequency: Single;
begin
  Result := FFreq;
end;

procedure TStkLFO.SetPhaseOffset(Value: Single);
begin
  while Value >= 1 do
    Value := Value - 1;
  while Value < 0 do
    Value := Value + 1;
  FPofs := Value;
  SetPhase(FPofs + FPhase);
end;

function TStkLFO.GetPhaseOffset: Single;
begin
  Result := FPofs;
end;

procedure TStkLFO.SetPhase(Value: Single);
begin
  while Value >= 1 do
    Value := Value - 1;
  while Value < 0 do
    Value := Value + 1;
  FPhase := Value;
  Cnt := FPhase * srate / FFreq;
end;

function TStkLFO.GetPhase: Single;
begin
  Result := FPhase;
end;

procedure TStkLFO.reset;
begin
  SetPhase(FPofs);
end;

destructor TStkLFO.Destroy;
begin
  inherited Destroy;
end;

procedure TStkLFO.AddPhaseOffset(Value: Single);
begin
  SetPhase(FPhase + Value);
end;

end.

