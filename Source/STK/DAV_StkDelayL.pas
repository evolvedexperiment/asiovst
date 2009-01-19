unit DAV_StkDelayL;

interface

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK linear interpolating Delay line class.

   This Delay subclass implements a fractional-length digital Delay-line using
   first-order linear interpolation.  A fixed maximum length of 4095 and a
   Delay of zero is set using the default constructor. Alternatively, the Delay
   and maximum length can be set during instantiation with an overloaded
   constructor.

   Linear interpolation is an efficient technique for achieving fractional
   Delay lengths, though it does introduce high-frequency signal attenuation
   to varying degrees depending on the fractional Delay setting.  The use of
   higher order Lagrange interpolators can typically improve (minimize) this
   attenuation characteristic.
}

uses
  DAV_StkCommon, DAV_StkDelay;//dialogs,sysutils;

{$I ..\DAV_Compiler.inc}

type
  TDelayL = class(TDelay)
  protected
    FAlpha, FOmAlpha, FNextOutput: Single;
    FDoNextOut: boolean;
  public
    // Default constructor creates a Delay-line with maximum length of 4095 samples and zero Delay.
    constructor Create(ASampleRate: Single); overload;

    // Overloaded constructor which specifies the current and maximum Delay-line lengths.

    constructor Create(ASampleRate, ADelay: Single; AMaxDelay: longint); overload;

    // Class destructor.
    destructor Destroy;

    // Set the Delay-line length.
  {
    The valid range for \e ADelay is from 0 to the maximum Delay-line length.
  }
    procedure setDelay(ADelay: Single);

    // Return the current Delay-line length.
    function getDelay: Single;

    // Return the value which will be output by the next call to tick().
  {
    This method is valid only for Delay settings greater than zero!
   }
    function nextOut: Single;

    // Input one sample to the Delay-line and return one output.
    function tick(sample: Single): Single;

  end;

implementation

{ TDelayL }

constructor TDelayL.Create(ASampleRate: Single);
begin
  inherited Create(ASampleRate);
  FDoNextOut := True;
end;

constructor TDelayL.Create(ASampleRate, ADelay: Single; AMaxDelay: longint);
begin
  inherited Create(ASampleRate);
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

destructor TDelayL.Destroy;
begin
  inherited Destroy;
end;

function TDelayL.getDelay: Single;
begin
  Result := Delay;
end;

function TDelayL.nextOut: Single;
begin
  if (FDoNextOut) then
   begin
    // First 1/2 of interpolation
    FNextOutput := index(inputs, outPoint) * FOmAlpha;
    // Second 1/2 of interpolation
    if (outPoint + 1 < length) then
      FNextOutput := FNextOutput + index(inputs, outPoint + 1) * FAlpha
    else
      FNextOutput := FNextOutput + inputs^ * FAlpha;
    FDoNextOut := False;
   end;
  Result := FNextOutput;
end;

procedure TDelayL.setDelay(ADelay: Single);
var
  outPointer: Single;
begin
  if (ADelay > length - 1) then
   begin
    // Force Delay to maxLength
    outPointer := inPoint + 1.0;
    Delay := length - 1;
   end
  else if (ADelay < 0) then
   begin
    outPointer := inPoint;
    Delay := 0;
   end
  else
   begin
    outPointer := inPoint - ADelay;  // read chases write
    Delay := ADelay;
   end;

  while (outPointer < 0) do
    outPointer := outPointer + length; // modulo maximum length

  outPoint := round(outPointer);  // integer part
  FAlpha := outPointer - outPoint; // fractional part
  FOmAlpha := 1 - FAlpha;
end;

function TDelayL.tick(sample: Single): Single;
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
  FDoNextOut := True;

  // Increment output pointer modulo length.
  outPoint := outPoint + 1;
  if (outPoint >= length) then
    outPoint := outPoint - length;
  Result := outputs^;
end;

end.
