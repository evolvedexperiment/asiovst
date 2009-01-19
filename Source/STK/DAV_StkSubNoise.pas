unit DAV_StkSubNoise;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sub-sampled noise generator.

  Generates a new random number every "rate" ticks using the C rand() function.
  The quality of the rand() function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkNoise;

type
  TSubNoise = class(TNoise)
  public
    // Default constructor sets sub-sample rate to 16.
    constructor Create(sr: my_float; subRate: integer);

    // Class destructor.
    destructor Destroy;

    // Return the current sub-sampling rate.
    function subRate: integer;

    // Set the sub-sampling rate.
    procedure setRate(subRate: integer);

    // Return a sub-sampled random number between -1.0 and 1.0.
    function tick: my_float;
  protected
    counter, rate: integer;
  end;

implementation

constructor TSubNoise.Create;
begin
  inherited Create(sr);
  rate := subRate;
  counter := rate;
end;

destructor TSubNoise.Destroy;
begin
  inherited Destroy;
end;

function TSubNoise.subRate;
begin
  Result := rate;
end;

procedure TSubNoise.setRate;
begin
  if (subRate > 0) then
    rate := subRate;
end;

function TSubNoise.tick: my_float;
begin
  counter := counter + 1;
  if (counter > rate) then
   begin
    inherited tick;
    counter := 1;
   end;
  Result := lastOutput;
end;

end.
