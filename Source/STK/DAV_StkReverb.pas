unit DAV_StkReverb;

{
/***************************************************/
/*! \class TReverb
    \brief STK abstract TReverberator parent class.

    This class provides common functionality for
    STK TReverberator subclasses.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk;

type
  TReverb = class(TStk)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set the mixture of input and "TReverberated" levels in the output (0.0 := input only, 1.0 := TReverb only).
    procedure setEffectMix(mix: MY_FLOAT);

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  //! Return the last left output value.
    function lastOutLeft: MY_FLOAT;

  //! Return the last right output value.
    function lastOutRight: MY_FLOAT;

  //! Abstract tick function ... must be implemented in subclasses.
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Take \e vectorSize inputs, compute the same number of outputs and return them in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    lastOutput: array[0..1] of my_float;
    effectMix: my_float;
    function isPrime(number: integer): boolean;
  end;

implementation

constructor TReverb.Create;
begin
  inherited Create(sr);
end;

destructor TReverb.Destroy;
begin
  inherited Destroy;
end;

procedure TReverb.setEffectMix;
begin
  effectMix := mix;
end;

function TReverb.lastOut: my_float;
begin
  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

function TReverb.lastOutLeft: my_float;
begin
  Result := lastOutput[0];
end;

function TReverb.lastOutRight: my_float;
begin
  Result := lastOutput[1];
end;

function TReverb.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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

function TReverb.isPrime;
var
  i: integer;
begin
  if (number = 2) then
   begin
    Result := True;
    exit
   end;
  if (number and 1 > 0) then
   begin
    i := 3;
    repeat
      if ((number mod i) = 0) then
       begin
        Result := False;
        exit
       end;
      i := i + 2;
    until (i >= round(sqrt(number) + 1));
    Result := True;
   end else
    Result := False;
end;

procedure TReverb.Clear;
begin
end;

function TReverb.tick(input: MY_FLOAT): MY_FLOAT;
begin
  Result := 0;
end;

end.
