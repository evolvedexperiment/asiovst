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
   to varying degrees depending on the fractional delay setting. The use of
   higher order Lagrange interpolators can typically improve (minimize) this
   attenuation characteristic.
}

uses
  DAV_StkCommon, DAV_StkDelay;//dialogs,sysutils;

{$I ..\DAV_Compiler.inc}

type
  TStkDelayL = class(TStkDelay)
  private
    procedure SetDelay(const ADelay: Single);
  protected
    FAlpha      : Single;
    FOmAlpha    : Single;
    FNextOutput : Single;
    FDoNextOut  : Boolean;
    FDelay      : Single;
  public
    // Default constructor creates a Delay-line with maximum length of 4095 samples and zero Delay.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which specifies the current and maximum Delay-line lengths.

    constructor Create(const SampleRate, ADelay: Single; const AMaxDelay: Integer); overload; override;

    // Class destructor.
    destructor Destroy;  override;

    // Return the value which will be output by the next call to Tick().
  {
    This method is valid only for Delay settings greater than zero!
   }
    function NextOut: Single;

    // Input one Sample to the Delay-line and return one output.
    function Tick(const Sample: Single): Single; override;
  published
    property Delay: Single read FDelay write SetDelay;
  end;

implementation

{ TStkDelayL }

constructor TStkDelayL.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FDoNextOut := True;
end;

constructor TStkDelayL.Create(const SampleRate, ADelay: Single; const AMaxDelay: Integer);
begin
  inherited Create(SampleRate);
   // Writing before reading allows delays from 0 to length-1.
  FLength := AMaxDelay + 1;
  if (FLength > 4096) then
   begin
    // We need to delete the previously allocated inputs.
    Dispose(FInputs);
    GetMem(FInputs, SizeOf(Single) * FLength);
    Clear;
   end;
  FInPoint := 0;
  Delay := ADelay;
  FDoNextOut := True;
end;

destructor TStkDelayL.Destroy;
begin
  inherited Destroy;
end;

function TStkDelayL.NextOut: Single;
begin
  if (FDoNextOut) then
   begin
    // First 1/2 of interpolation
    FNextOutput := FInputs^[FOutPoint] * FOmAlpha;
    // Second 1/2 of interpolation
    if (FOutPoint + 1 < length) then
      FNextOutput := FNextOutput + FInputs^[FOutPoint + 1] * FAlpha
    else
      FNextOutput := FNextOutput + FInputs^[0] * FAlpha;
    FDoNextOut := False;
   end;
  Result := FNextOutput;
end;

procedure TStkDelayL.SetDelay(const ADelay: Single);
var
  OutPointer: Single;
begin
  if (ADelay > length - 1) then
   begin
    // Force Delay to maxLength
    OutPointer := FInPoint + 1;
    Delay := FLength - 1;
   end
  else if (ADelay < 0) then
   begin
    OutPointer := FInPoint;
    Delay := 0;
   end
  else
   begin
    OutPointer := FInPoint - ADelay;  // read chases write
    Delay := ADelay;
   end;

  while (outPointer < 0) do
    outPointer := outPointer + length; // modulo maximum length

  FOutPoint := round(OutPointer);  // integer part
  FAlpha := OutPointer - FOutPoint; // fractional part
  FOmAlpha := 1 - FAlpha;
end;

function TStkDelayL.Tick(const Sample: Single): Single;
begin
  FInputs^[FInpoint] := Sample;
  Inc(FInPoint);
  // Increment input pointer modulo length.
  if FInPoint = FLength
   then FInPoint := FInPoint - FLength;

  FOutputs^[0] := NextOut;
  FDoNextOut := True;

  // Increment output pointer modulo length.
  Inc(FOutPoint);
  if (FOutPoint >= FLength)
   then FOutPoint := FOutPoint - FLength;
  Result := FOutputs[0];
end;

end.
