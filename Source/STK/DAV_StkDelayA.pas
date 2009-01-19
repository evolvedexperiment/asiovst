unit DAV_StkDelayA;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK allpass interpolating delay line class.

   This delay subclass implements a fractional-length digital delay-line using
   a first-order allpass filter. A fixed maximum length of 4095 and a delay of
   0.5 is set using the default constructor.  Alternatively, the delay and
   maximum length can be set during instantiation with an overloaded
   constructor.

   An allpass filter has unity magnitude gain but variable phase delay
   properties, making it useful in achieving fractional delays without
   affecting a signal's frequency magnitude response.  In order to achieve a
   maximally flat phase delay response, the minimum delay possible in this
   implementation is limited to a value of 0.5.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkDelay;

type
  TDelayA = class(TDelay)
  protected
    FAlpha, FCoeff, FApInput, FNextOutput: Single;
    FDoNextOut: Boolean;
  public
    // Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
    constructor Create(SampleRate: Single); overload;

    // Overloaded constructor which specifies the current and maximum delay-line lengths.
    constructor Create(SampleRate, ADelay: Single; AMaxDelay: longint); overload;

    // Class destructor.
    destructor Destroy;

    // Clears the internal state of the delay line.
    procedure Clear;

    // Set the delay-line length
  {
    The valid range for \e ADelay is from 0.5 to the maximum delay-line length.
  }
    procedure setDelay(ADelay: Single);

    // Return the current delay-line length.
    function getDelay: Single;

    // Return the value which will be output by the next call to tick().
  {
    This method is valid only for delay settings greater than zero!
   }
    function nextOut: Single;

    // Input one sample to the delay-line and return one output.
    function tick(sample: Single): Single;

  end;

implementation

{ TDelayA }

procedure TDelayA.Clear;
begin
  inherited Clear;
  FApInput := 0.0;
end;

constructor TDelayA.Create(SampleRate: Single);
begin
  inherited Create(SampleRate);
  setDelay(0.5);
  FApInput := 0.0;
  FDoNextOut := True;
end;

constructor TDelayA.Create(SampleRate, ADelay: Single; AMaxDelay: Integer);
begin
  inherited Create(SampleRate);
   // Writing before reading allows delays from 0 to length-1.
  length := AMaxDelay + 1;

  if (length > 4096) then
   begin
    // We need to delete the previously allocated inputs.
    freemem(inputs);
    getmem(inputs, sizeof(Single) * length);
    Clear;
   end;

  inPoint := 0;
  setDelay(ADelay);
  FDoNextOut := True;
end;

destructor TDelayA.Destroy;
begin
  inherited Destroy;
end;

function TDelayA.getDelay: Single;
begin
  Result := inherited getdelay;
end;

function TDelayA.nextOut: Single;
begin
  if (FDoNextOut) then
   begin
    // Do allpass interpolation delay.
    FNextOutput := -coeff * outputs^;
    FNextOutput := FNextOutput + FApInput + (FCoeff * index(inputs, outPoint));
    FDoNextOut := False;
   end;
  Result := FNextOutput;
end;

procedure TDelayA.setDelay(ADelay: Single);
var
  outPointer: Single;
begin
  if (ADelay > length - 1) then
   begin
    // Force delay to maxLength
    outPointer := inPoint + 1.0;
    delay := length - 1;
   end
  else if (ADelay < 0.5) then
   begin
    outPointer := inPoint + 0.4999999999;
    delay := 0.5;
   end
  else
   begin
    outPointer := inPoint - ADelay + 1.0;     // outPoint chases inpoint
    delay := ADelay;
   end;

  if (outPointer < 0) then
    outPointer := outPointer + length;  // modulo maximum length

  outPoint := round(outPointer);        // integer part
  FAlpha := 1.0 + outPoint - outPointer; // fractional part

  if (FAlpha < 0.5) then
   begin
    // The optimal range for FAlpha is about 0.5 - 1.5 in order to
    // achieve the flattest phase delay response.
    outPoint := outPoint + 1;
    if (outPoint >= length) then
      outPoint := outPoint - length;
    FAlpha := FAlpha + 1;
   end;
  FCoeff := (1 - FAlpha) / (1 + FAlpha);         // coefficient for all pass
end;

function TDelayA.tick(sample: Single): Single;
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
  FDoNextOut := True;

 // Save the allpass input and increment modulo length.
  FApInput := index(inputs, outPoint);
  if (outPoint = length) then
    outPoint := outPoint - length;
  Result := outputs^;
  outPoint := outPoint + 1;
end;

end.
