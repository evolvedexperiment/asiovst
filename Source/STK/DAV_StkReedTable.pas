unit DAV_StkReedTable;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK reed table class.

  This class implements a simple one breakpoint, non-linear reed function, as
  described by Smith (1986).  This function is based on a memoryless non-linear
  spring model of the reed (the reed mass is ignored) which saturates when the
  reed collides with the mouthpiece facing.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkReedTable = class(TStk)
  protected
    FOffSet, FSlope, FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Set the table FOffSet value.
  {
    The table FOffSet roughly corresponds to the size
    of the initial reed tip opening (a greater FOffSet
    represents a smaller opening).
  }
    procedure setOffset(aValue: Single);

    // Set the table FSlope value.
  {
   The table FSlope roughly corresponds to the reed
   stiffness (a greater FSlope represents a harder
   reed).
  }
    procedure setSlope(aValue: Single);

    // Return the last output value.
    function lastOut: Single;

    // Return the function value for \e input.
  {
    The function input represents the differential
    pressure across the reeds.
  }
    function tick(input: Single): Single; overload;

    // Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function tick(vector: PSingle; vectorSize: longint): PSingle; overload;
  end;

implementation

constructor TStkReedTable.Create;
begin
  inherited Create(SampleRate);
  FOffSet := 0.6;  // FOffSet is a bias, related to reed rest position.
  FSlope := -0.8;  // FSlope corresponds loosely to reed stiffness.
end;

destructor TStkReedTable.Destroy;
begin
  inherited Destroy;
end;

procedure TStkReedTable.setOffset;
begin
  FOffSet := aValue;
end;

procedure TStkReedTable.setSlope;
begin
  FSlope := aValue;
end;

function TStkReedTable.lastOut: Single;
begin
  Result := FLastOutput;
end;

function TStkReedTable.tick(input: Single): Single;
begin
  // The input is differential pressure across the reed.
  FLastOutput := FOffSet + (FSlope * input);

  // If output is > 1, the reed has slammed shut and the
  // reflection function value saturates at 1.0.
  if (FLastOutput > 1.0) then
    FLastOutput := 1.0;

  // This is nearly impossible in a physical system, but
  // a reflection function value of -1.0 corresponds to
  // an open end (and no discontinuity in bore profile).
  if (FLastOutput < -1.0) then
    FLastOutput := -1.0;
  Result := FLastOutput;
end;

function TStkReedTable.tick(vector: PSingle; vectorSize: longint): PSingle;
var
  i: integer;
  p: pSingle;
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
