unit DAV_StkDelay;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{  TDelay
   STK non-interpolating TDelay line class.

   This protected Filter subclass implements a non-interpolating digital
   Delay-line. A fixed maximum FLength of 4095 and a delay of zero is set using
   the default constructor.
   Alternatively, the delay and maximum FLength can be set during instantiation
   with an overloaded constructor.

   A non-interpolating delay line is typically used in fixed delay-FLength
   applications, such as for reverberation.
}

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkFilter;

type
  TDelay = class(TFilter)
  protected
    FInPoint, FOutPoint, FLength: Integer;
    Delay: Single;
  public
    // Default constructor creates a delay-line with maximum FLength of 4095 samples and zero TDelay.
    constructor Create(SampleRate: Single); overload;

    // Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(SampleRate, theDelay: Single; maxDelay: Integer); overload;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the delay line.
    procedure Clear;

    // Set the delay-line FLength.
  {
    The valid range for \e theDelay is from 0 to the maximum delay-line FLength.
  }
    procedure SetDelay(theDelay: Integer);

    // Return the current delay-line FLength.
    function getDelay: Integer;

    // Calculate and return the signal Energy in the delay-line.
    function Energy: Single;

    // Return the value at \e tapDelay samples from the delay-line input.
  {
    The valid range for \e tapDelay is 1 to the delay-line FLength.
  }
    function contentsAt(tapDelay: Integer): Single;

    // Return the last computed output value.
    function lastOut: Single;

    // Return the value which will be output by the next call to tick().
  {
    This method is valid only for delay settings greater than zero!
   }
    function nextOut: Single;

    // Input one sample to the delay-line and return one output.
    function tick(sample: Single): Single; overload;

    // Input \e vectorSize samples to the delay-line and return an equal number of Outputs in \e vector.
    function tick(vector: PSingle; vectorSize: Integer): PSingle; overload;
  end;

implementation

{ TDelay }

procedure TDelay.Clear;
var
  i: Integer;
  p: PSingle;
begin
  p := Inputs;
  for i := 0 to FLength - 1 do
   begin
    p^ := 0.0;
    Inc(p);
   end;
  if assigned(Outputs) then
    Outputs^ := 0.0;
end;

function TDelay.contentsAt(tapDelay: Integer): Single;
var
  tap, i: Integer;
begin
  i := tapDelay;
  if (i > Delay) then
    i := round(Delay);
  tap := FInPoint - i;
  if (tap < 0) then // Check for wraparound.
    tap := tap + FLength;
  Result := index(Inputs, tap);
end;

constructor TDelay.Create(SampleRate, theDelay: Single; maxDelay: Integer);
begin
  inherited Create(SampleRate);
   // Writing before reading allows TDelays from 0 to FLength-1.
  // If we want to allow a TDelay of maxTDelay, we need a
  // TDelay-line of FLength := maxTDelay+1.
  FLength := maxDelay + 1;

  // We need to delete the previously allocated Inputs.
  FreeMem(Inputs);
  GetMem(Inputs, FLength * sizeof(Single));
  Clear;

  FInPoint := 0;
  SetDelay(round(theDelay));
end;

constructor TDelay.Create(SampleRate: Single);
begin
  inherited Create(SampleRate);
    // Default max TDelay FLength set to 4095.
  FLength := 4096;

  FreeMem(Inputs);
  GetMem(Inputs, FLength * sizeof(Single));
  Clear;

  FInPoint := 0;
  FOutPoint := 0;
  Delay := 0;
end;

destructor TDelay.Destroy;
begin
  inherited Destroy;
end;

function TDelay.Energy: Single;
var
  i: integer;
  t, e: Single;
begin
  e := 0;
  if (FInPoint >= FOutPoint) then
    for i := FOutPoint to FInPoint - 1 do
     begin
      t := index(Inputs, i);
      e := e + t * t;
     end else
   begin
    for i := FOutPoint to FLength - 1 do
     begin
      t := index(Inputs, i);
      e := e + t * t;
     end;
    for i := 0 to FInPoint - 1 do
     begin
      t := index(Inputs, i);
      e := e + t * t;
     end;
   end;
  Result := e;
end;

function TDelay.getDelay: Integer;
begin
  Result := round(Delay);
end;

function TDelay.lastOut: Single;
begin
  Result := inherited lastOut;
end;

function TDelay.nextOut: Single;
begin
  Result := index(Inputs, FOutPoint);
end;

procedure TDelay.SetDelay(theDelay: Integer);
begin
  if (theDelay > FLength - 1) then
   begin // The value is too big.
    // Force TDelay to maxLength.
    FOutPoint := FInPoint + 1;
    Delay := FLength - 1;
   end
  else if (theDelay < 0) then
   begin
    FOutPoint := FInPoint;
    Delay := 0;
   end
  else
   begin
    FOutPoint := FInPoint - round(theDelay);  // read chases write
    Delay := theDelay;
   end;

  while (FOutPoint < 0) do
    FOutPoint := FOutPoint + FLength;  // modulo maximum FLength
end;

function TDelay.tick(sample: Single): Single;
var
  p: PSingle;
begin
  p := PSingle(Integer(Inputs) + sizeof(Single) * FInPoint);
  p^ := sample;
  FInPoint := FInPoint + 1;

  // Check for end condition
  if (FInPoint = FLength) then
    FInPoint := FInPoint - FLength;

  // Read out next value
  p := PSingle(Integer(Inputs) + sizeof(Single) * FOutPoint);
  Outputs^ := p^;
  FOutPoint := FOutPoint + 1;
  if (FOutPoint >= FLength) then
    FOutPoint := FOutPoint - FLength;
  Result := Outputs^;
end;

function TDelay.tick(vector: PSingle; vectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
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
