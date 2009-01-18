unit DAV_StkBowTabl;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK bowed string table class.

   This class implements a simple bowed string non-linear function, as
   described by Smith (1986).
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, Math;

type
  TStkBowTable = class(TStk)
  protected
    FOffSet     : Single;
    FSlope      : Single;
    FLastOutput : Single;
  public
  //! Default constructor.
    constructor Create(SampleRate: Single);

  //! Class destructor.
    destructor Destroy;

  //! Set the table FOffSet value.
  {
    The table FOffSet is a bias which controls the
    symmetry of the friction.  If you want the
    friction to vary with direction, use a non-zero
    value for the FOffSet.  The default value is zero.
  }
    procedure setOffset(aValue: Single);

  //! Set the table FSlope value.
  {
   The table FSlope controls the width of the friction
   pulse, which is related to bow force.
  }
    procedure setSlope(aValue: Single);

  //! Return the last output value.
    function lastOut: Single;

  //! Return the function value for \e input.
  {
    The function input represents differential
    string-to-bow velocity.
  }
    function tick(input: Single): Single; overload;

  //! Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;
  end;

implementation

constructor TStkBowTable.Create(SampleRate: Single);
begin
  inherited Create(SampleRate);
  FOffSet := 0.0;
  FSlope := 0.1;
end;

destructor TStkBowTable.Destroy;
begin
  inherited Destroy;
end;

procedure TStkBowTable.setOffset;
begin
  FOffSet := aValue;
end;

procedure TStkBowTable.setSlope;
begin
  FSlope := aValue;
end;

function TStkBowTable.lastOut: Single;
begin
  Result := FLastOutput;
end;

function TStkBowTable.tick(input: Single): Single;
var
  sample: Single;
begin
  // The input represents differential string vs. bow velocity.
  sample := input + FOffSet;  // add bias to input
  sample := sample * FSlope;          // then scale it
  FLastOutput := abs(sample) + 0.75;
  FLastOutput := power(FLastOutput, -4.0);

  // Set minimum friction to 0.0
  //if (FLastOutput < 0.0 ) FLastOutput := 0.0;
  // Set maximum friction to 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;

  Result := FLastOutput;
end;

function TStkBowTable.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
