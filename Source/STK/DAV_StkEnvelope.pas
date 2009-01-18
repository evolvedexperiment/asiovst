unit DAV_StkEnvelope;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, Windows;

type
  TStkEnvelope = class(TStk)
  private
    procedure SetCurrentValue(const Value: Single);
    procedure SetRate(const Value: Single);
    procedure SetTime(const Value: Single);
    procedure SetTarget(const Value: Single);
  protected
    FCurrentValue : Single;
    FTarget       : Single;
    FRate         : Single;
    FState        : Integer;

    procedure CurrentValueChanged; virtual;
    procedure RateChanged; virtual;
    procedure TargetChanged; virtual;
  public
    constructor Create(const SampleRate: Single = 44100); override;
    destructor Destroy; override;

    procedure KeyOn; virtual;
    procedure KeyOff; virtual;

    function Tick: Single; overload; virtual;
    procedure Tick(const Vector: PDAVSingleFixedArray; const VectorSize: Cardinal); overload; virtual;

    property CurrentValue: Single read FCurrentValue write SetCurrentValue;
    property Rate: Single read FRate write SetRate;
    property Targer: Single read FTarget write SetTarget;
    property State: Single read FState;
  end;

implementation

constructor TStkEnvelope.Create;
begin
  inherited Create(sr);
  FTarget := 0;
  FCurrentValue := 0;
  FRate := 0.001;
  FState := 0;
end;

destructor TStkEnvelope.Destroy;
begin
  inherited Destroy;
end;

procedure TStkEnvelope.KeyOn;
begin
  FTarget := 1;
  if (FCurrentValue <> FTarget)
   then FState := 1;
end;

procedure TStkEnvelope.KeyOff;
begin
  FTarget := 0;
  if (FCurrentValue <> FTarget)
   then FState := 1;
end;

procedure TStkEnvelope.SetRate(Value: Single);
begin
 if ARate < 0
  then raise Exception.Create('Rate must be above 0!');
 if FRate <> Value then
  begin
   FRate := Value;
   RateChanged;
  end;
end;

procedure TStkEnvelope.RateChanged;
begin
 // nothing here yet!
end;

procedure TStkEnvelope.SetTime(const Value: Single);
begin
 Rate := 1.0 / (-Value * SampleRate)
end;

procedure TStkEnvelope.SetTarget(const Value: Single);
begin
 if FTarget <> ATarget then
  begin
   FTarget := ATarget;
   TargetChanged;
  end;
end;

procedure TStkEnvelope.TargetChanged;
begin
 if (FCurrentValue <> FTarget)
  then FState := 1;
end;

procedure TStkEnvelope.SetCurrentValue(const Value: Single);
begin
 if FCurrentValue <> Value then
  begin
   FCurrentValue := AValue;
   CurrentValueChanged;
  end;
end;

procedure TStkEnvelope.CurrentValueChanged;
begin
 FTarget := FCurrentValue;
 FState := 0;
end;

function TStkEnvelope.Tick: Single;
begin
 if (FState > 0) then
  if (FTarget > FCurrentValue) then
   begin
    FCurrentValue := FCurrentValue + FRate;
    if (FCurrentValue >= FTarget) then
     begin
      FCurrentValue := FTarget;
      FState := 0;
     end;
   end
  else
   begin
    FCurrentValue := FCurrentValue - FRate;
    if (FCurrentValue <= FTarget) then
     begin
      FCurrentValue := FTarget;
      FState := 0;
     end;
   end;
 Result := FCurrentValue;
end;

procedure TStkEnvelope.Tick(const Vector: PDAVSingleFixedArray; const VectorSize: Cardinal);
var
  i: Integer;
begin
  for i := 0 to VectorSize - 1
   do Vector^[i] := Tick;
end;

end.
