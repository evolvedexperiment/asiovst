unit DAV_StkDelay;

interface

{
/***************************************************/
/*! \class TDelay
    \brief STK non-interpolating TDelay line class.

    This protected Filter subclass implements
    a non-interpolating digital Delay-line.
    A fixed maximum length of 4095 and a delay
    of zero is set using the default constructor.
    Alternatively, the delay and maximum length
    can be set during instantiation with an
    overloaded constructor.

    A non-interpolating delay line is typically
    used in fixed delay-length applications, such
    as for reverberation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}

uses
  DAV_StkCommon, DAV_StkFilter;

type
  TDelay = class(TFilter)
  public
  //! Default constructor creates a delay-line with maximum length of 4095 samples and zero TDelay.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(sr, theDelay: my_float; maxDelay: longint); overload;

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal state of the delay line.
    procedure Clear;

  //! Set the delay-line length.
  {
    The valid range for \e theDelay is from 0 to the maximum delay-line length.
  }
    procedure setDelay(theDelay: longint);

  //! Return the current delay-line length.
    function getDelay: longint;

  //! Calculate and return the signal energy in the delay-line.
    function energy: MY_FLOAT;

  //! Return the value at \e tapDelay samples from the delay-line input.
  {
    The valid range for \e tapDelay is 1 to the delay-line length.
  }
    function contentsAt(tapDelay: longint): MY_FLOAT;

  //! Return the last computed output value.
    function lastOut: my_float;

  //! Return the value which will be output by the next call to tick().
  {
    This method is valid only for delay settings greater than zero!
   }
    function nextOut: my_float;

  //! Input one sample to the delay-line and return one output.
    function tick(sample: MY_FLOAT): MY_FLOAT; overload;

  //! Input \e vectorSize samples to the delay-line and return an equal number of outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    inPoint, outPoint, length: longint;
    Delay: MY_FLOAT;
  end;

implementation

{ TDelay }

procedure TDelay.Clear;
var
  i: longint;
  p: pmy_float;
begin
  p := inputs;
  for i := 0 to length - 1 do
   begin
    p^ := 0.0;
    Inc(p);
   end;
  if assigned(outputs) then
    outputs^ := 0.0;
end;

function TDelay.contentsAt(tapDelay: longint): MY_FLOAT;
var
  tap, i: longint;
begin
  i := tapDelay;
  if (i > Delay) then
    i := round(Delay);
  tap := inPoint - i;
  if (tap < 0) then // Check for wraparound.
    tap := tap + length;
  Result := index(inputs, tap);
end;

constructor TDelay.Create(sr, theDelay: my_float; maxDelay: longint);
begin
  inherited Create(sr);
   // Writing before reading allows TDelays from 0 to length-1.
  // If we want to allow a TDelay of maxTDelay, we need a
  // TDelay-line of length := maxTDelay+1.
  length := maxDelay + 1;

  // We need to delete the previously allocated inputs.
  freemem(inputs);
  getmem(inputs, length * sizeof(my_float));
  Clear;

  inPoint := 0;
  setDelay(round(theDelay));
end;

constructor TDelay.Create(sr: my_float);
begin
  inherited Create(sr);
    // Default max TDelay length set to 4095.
  length := 4096;

  freemem(inputs);
  getmem(inputs, length * sizeof(my_float));
  Clear;

  inPoint := 0;
  outPoint := 0;
  Delay := 0;
end;

destructor TDelay.Destroy;
begin
  inherited Destroy;
end;

function TDelay.energy: MY_FLOAT;
var
  i: integer;
  t, e: my_float;
begin
  e := 0;
  if (inPoint >= outPoint) then
    for i := outPoint to inPoint - 1 do
     begin
      t := index(inputs, i);
      e := e + t * t;
     end else
   begin
    for i := outPoint to length - 1 do
     begin
      t := index(inputs, i);
      e := e + t * t;
     end;
    for i := 0 to inPoint - 1 do
     begin
      t := index(inputs, i);
      e := e + t * t;
     end;
   end;
  Result := e;
end;

function TDelay.getDelay: longint;
begin
  Result := round(Delay);
end;

function TDelay.lastOut: my_float;
begin
  Result := inherited lastOut;
end;

function TDelay.nextOut: my_float;
begin
  Result := index(inputs, outPoint);
end;

procedure TDelay.setDelay(theDelay: longint);
begin
  if (theDelay > length - 1) then
   begin // The value is too big.
    // Force TDelay to maxLength.
    outPoint := inPoint + 1;
    Delay := length - 1;
   end
  else if (theDelay < 0) then
   begin
    outPoint := inPoint;
    Delay := 0;
   end
  else
   begin
    outPoint := inPoint - round(theDelay);  // read chases write
    Delay := theDelay;
   end;

  while (outPoint < 0) do
    outPoint := outPoint + length;  // modulo maximum length
end;

function TDelay.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: PMY_FLOAT;
begin
  p := PMY_FLOAT(longint(inputs) + sizeof(MY_FLOAT) * inPoint);
  p^ := sample;
  inPoint := inPoint + 1;

  // Check for end condition
  if (inPoint = length) then
    inPoint := inPoint - length;

  // Read out next value
  p := pmy_float(longint(inputs) + sizeof(MY_FLOAT) * outPoint);
  outputs^ := p^;
  outPoint := outPoint + 1;
  if (outPoint >= length) then
    outPoint := outPoint - length;
  Result := outputs^;
end;

function TDelay.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
