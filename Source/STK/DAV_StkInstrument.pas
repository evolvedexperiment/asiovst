unit DAV_StkInstrument;

{
/***************************************************/
/*! \class TInstrmnt
    \brief STK instrument abstract base class.

    This class provides a common interface for
    all STK instruments.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk;

type
  TInstrmnt = class(TStk)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: my_float);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: my_float);

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Return the last output value.
    function lastOut: my_float;

  //! Compute one output sample.
    function tick: my_float; overload;

  //! Computer \e vectorSize outputs and return them in \e vector.
    function tick(vector: Pmy_float; vectorSize: longint): Pmy_float; overload;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: my_float);

  protected
    lastOutput: my_float;
  end;

implementation

constructor TInstrmnt.Create;
begin
  inherited Create(sr);
end;

destructor TInstrmnt.Destroy;
begin
  inherited Destroy;
end;

procedure TInstrmnt.setFrequency;
begin
end;

function TInstrmnt.lastOut: MY_FLOAT;
begin
  Result := lastOutput;
end;

function TInstrmnt.tick: my_float;
begin
  Result := 0;
end;

function TInstrmnt.tick(vector: pmy_float; vectorSize: longint): pmy_float;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick;
    Inc(p);
   end;
  Result := vector;
end;

procedure TInstrmnt.controlChange;
begin
end;

procedure TInstrmnt.noteOn;
begin
end;

procedure TInstrmnt.noteOff;
begin
end;

end.
