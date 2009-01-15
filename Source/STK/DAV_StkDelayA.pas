unit DAV_StkDelayA;

{
/***************************************************/
/*! \class DelayA
    \brief STK allpass interpolating delay line class.

    This delay subclass implements a fractional-
    length digital delay-line using a first-order
    allpass filter. A fixed maximum length
    of 4095 and a delay of 0.5 is set using the
    default constructor.  Alternatively, the
    delay and maximum length can be set during
    instantiation with an overloaded constructor.

    An allpass filter has unity magnitude gain but
    variable phase delay properties, making it useful
    in achieving fractional delays without affecting
    a signal's frequency magnitude response.  In
    order to achieve a maximally flat phase delay
    response, the minimum delay possible in this
    implementation is limited to a value of 0.5.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkDelay;

type
  TDelayA = class(TDelay)
  public
  //! Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(sr, theDelay: MY_FLOAT; maxDelay: longint); overload;

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal state of the delay line.
    procedure Clear;

  //! Set the delay-line length
  {
    The valid range for \e theDelay is from 0.5 to the maximum delay-line length.
  }
    procedure setDelay(theDelay: MY_FLOAT);

  //! Return the current delay-line length.
    function getDelay: my_float;

  //! Return the value which will be output by the next call to tick().
  {
    This method is valid only for delay settings greater than zero!
   }
    function nextOut: MY_FLOAT;

  //! Input one sample to the delay-line and return one output.
    function tick(sample: MY_FLOAT): MY_FLOAT;

  protected
    alpha, coeff, apInput, nextOutput: MY_FLOAT;
    doNextOut: boolean;
  end;

implementation

{ TDelayA }

procedure TDelayA.Clear;
begin
  inherited Clear;
  apInput := 0.0;
end;

constructor TDelayA.Create(sr: my_float);
begin
  inherited Create(sr);
  setDelay(0.5);
  apInput := 0.0;
  doNextOut := True;
end;

constructor TDelayA.Create(sr, theDelay: MY_FLOAT; maxDelay: Integer);
begin
  inherited Create(sr);
   // Writing before reading allows delays from 0 to length-1.
  length := maxDelay + 1;

  if (length > 4096) then
   begin
    // We need to delete the previously allocated inputs.
    freemem(inputs);
    getmem(inputs, sizeof(MY_FLOAT) * length);
    Clear;
   end;

  inPoint := 0;
  setDelay(theDelay);
  doNextOut := True;
end;

destructor TDelayA.Destroy;
begin
  inherited Destroy;
end;

function TDelayA.getDelay: my_float;
begin
  Result := inherited getdelay;
end;

function TDelayA.nextOut: MY_FLOAT;
begin
  if (doNextOut) then
   begin
    // Do allpass interpolation delay.
    nextOutput := -coeff * outputs^;
    nextOutput := nextOutput + apInput + (coeff * index(inputs, outPoint));
    doNextOut := False;
   end;
  Result := nextOutput;
end;

procedure TDelayA.setDelay(theDelay: MY_FLOAT);
var
  outPointer: MY_FLOAT;
begin
  if (theDelay > length - 1) then
   begin
    // Force delay to maxLength
    outPointer := inPoint + 1.0;
    delay := length - 1;
   end
  else if (theDelay < 0.5) then
   begin
    outPointer := inPoint + 0.4999999999;
    delay := 0.5;
   end
  else
   begin
    outPointer := inPoint - theDelay + 1.0;     // outPoint chases inpoint
    delay := theDelay;
   end;

  if (outPointer < 0) then
    outPointer := outPointer + length;  // modulo maximum length

  outPoint := round(outPointer);        // integer part
  alpha := 1.0 + outPoint - outPointer; // fractional part

  if (alpha < 0.5) then
   begin
    // The optimal range for alpha is about 0.5 - 1.5 in order to
    // achieve the flattest phase delay response.
    outPoint := outPoint + 1;
    if (outPoint >= length) then
      outPoint := outPoint - length;
    alpha := alpha + 1;
   end;
  coeff := (1 - alpha) / (1 + alpha);         // coefficient for all pass
end;

function TDelayA.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: pmy_float;
begin
  p := pindex(inputs, inPoint);
  p^ := sample;
  inPoint := inPoint + 1;

 // Increment input pointer modulo length.
  if (inPoint = length) then
    inPoint := inPoint - length;

  outputs^ := nextOut;
  doNextOut := True;

 // Save the allpass input and increment modulo length.
  apInput := index(inputs, outPoint);
  if (outPoint = length) then
    outPoint := outPoint - length;
  Result := outputs^;
  outPoint := outPoint + 1;
end;

end.
