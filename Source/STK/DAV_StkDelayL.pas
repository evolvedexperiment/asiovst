unit DAV_StkDelayL;

interface

{
/***************************************************/
/*! \class DelayL
    \brief STK linear interpolating delay line class.

    This delay subclass implements a fractional-
    length digital delay-line using first-order
    linear interpolation.  A fixed maximum length
    of 4095 and a delay of zero is set using the
    default constructor.  Alternatively, the
    delay and maximum length can be set during
    instantiation with an overloaded constructor.

    Linear interpolation is an efficient technique
    for achieving fractional delay lengths, though
    it does introduce high-frequency signal
    attenuation to varying degrees depending on the
    fractional delay setting.  The use of higher
    order Lagrange interpolators can typically
    improve (minimize) this attenuation characteristic.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
uses
  DAV_StkCommon, DAV_StkDelay;//dialogs,sysutils;

type
  TDelayL = class(TDelay)
  public
  //! Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which specifies the current and maximum delay-line lengths.

    constructor Create(sr, theDelay: MY_FLOAT; maxDelay: longint); overload;

  //! Class destructor.
    destructor Destroy;

  //! Set the delay-line length.
  {
    The valid range for \e theDelay is from 0 to the maximum delay-line length.
  }
    procedure setDelay(theDelay: MY_FLOAT);

  //! Return the current delay-line length.
    function getDelay: MY_FLOAT;

  //! Return the value which will be output by the next call to tick().
  {
    This method is valid only for delay settings greater than zero!
   }
    function nextOut: MY_FLOAT;

  //! Input one sample to the delay-line and return one output.
    function tick(sample: MY_FLOAT): MY_FLOAT;

  protected
    alpha, omAlpha, nextOutput: my_float;
    doNextOut: boolean;
  end;

implementation

{ TDelayL }

constructor TDelayL.Create(sr: my_float);
begin
  inherited Create(sr);
  doNextOut := True;
end;

constructor TDelayL.Create(sr, theDelay: MY_FLOAT; maxDelay: longint);
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

destructor TDelayL.Destroy;
begin
  inherited Destroy;
end;

function TDelayL.getDelay: MY_FLOAT;
begin
  Result := delay;
end;

function TDelayL.nextOut: MY_FLOAT;
begin
  if (doNextOut) then
   begin
    // First 1/2 of interpolation
    nextOutput := index(inputs, outPoint) * omAlpha;
    // Second 1/2 of interpolation
    if (outPoint + 1 < length) then
      nextOutput := nextOutput + index(inputs, outPoint + 1) * alpha
    else
      nextOutput := nextOutput + inputs^ * alpha;
    doNextOut := False;
   end;
  Result := nextOutput;
end;

procedure TDelayL.setDelay(theDelay: MY_FLOAT);
var
  outPointer: my_float;
begin
  if (theDelay > length - 1) then
   begin
    // Force delay to maxLength
    outPointer := inPoint + 1.0;
    delay := length - 1;
   end
  else if (theDelay < 0) then
   begin
    outPointer := inPoint;
    delay := 0;
   end
  else
   begin
    outPointer := inPoint - theDelay;  // read chases write
    delay := theDelay;
   end;

  while (outPointer < 0) do
    outPointer := outPointer + length; // modulo maximum length

  outPoint := round(outPointer);  // integer part
  alpha := outPointer - outPoint; // fractional part
  omAlpha := 1 - alpha;
end;

function TDelayL.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: pmy_float;
begin
  p := pindex(inputs, inpoint);
  p^ := sample;
  inPoint := inPoint + 1;
  // Increment input pointer modulo length.
  if (inPoint = length) then
    inPoint := inPoint - length;

  outputs^ := nextOut;
  doNextOut := True;

  // Increment output pointer modulo length.
  outPoint := outPoint + 1;
  if (outPoint >= length) then
    outPoint := outPoint - length;
  Result := outputs^;
end;

end.
