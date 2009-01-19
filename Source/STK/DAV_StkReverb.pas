unit DAV_StkReverb;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK abstract TReverberator parent class.

  This class provides common functionality for STK TReverberator subclasses.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk;

type
  TStkReverb = class(TStk)
  protected
    FLastOutput : array[0..1] of Single;
    FEffectMix  : Single;
    function isPrime(Number: Integer): boolean;
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set the mixture of input and "TReverberated" levels in the output (0.0 := input only, 1.0 := TStkReverb only).
    procedure setEffectMix(Mix: Single);

    // Return the last output value.
    function lastOut: Single;

    // Return the last left output value.
    function lastOutLeft: Single;

    // Return the last right output value.
    function lastOutRight: Single;

    // Abstract Tick function ... must be implemented in subclasses.
    function Tick(input: Single): Single; overload;

    // Take \e VectorSize inputs, compute the same Number of outputs and return them in \e Vector.
    function Tick(Vector: PSingle; VectorSize: longint): PSingle; overload;

  end;

implementation

constructor TStkReverb.Create;
begin
  inherited Create(SampleRate);
end;

destructor TStkReverb.Destroy;
begin
  inherited Destroy;
end;

procedure TStkReverb.setEffectMix;
begin
  FEffectMix := Mix;
end;

function TStkReverb.lastOut: Single;
begin
  Result := (FLastOutput[0] + FLastOutput[1]) * 0.5;
end;

function TStkReverb.lastOutLeft: Single;
begin
  Result := FLastOutput[0];
end;

function TStkReverb.lastOutRight: Single;
begin
  Result := FLastOutput[1];
end;

function TStkReverb.Tick(Vector: PSingle; VectorSize: longint): PSingle;
var
  i: Integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := Vector;
end;

function TStkReverb.isPrime;
var
  i: Integer;
begin
  if (Number = 2) then
   begin
    Result := True;
    exit
   end;
  if (Number and 1 > 0) then
   begin
    i := 3;
    repeat
      if ((Number mod i) = 0) then
       begin
        Result := False;
        exit
       end;
      i := i + 2;
    until (i >= round(sqrt(Number) + 1));
    Result := True;
   end else
    Result := False;
end;

procedure TStkReverb.Clear;
begin
end;

function TStkReverb.Tick(input: Single): Single;
begin
  Result := 0;
end;

end.
