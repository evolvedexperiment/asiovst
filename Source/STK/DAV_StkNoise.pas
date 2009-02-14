unit DAV_StkNoise;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK noise generator.

   Generic random number generation using the C rand() function.
   The quality of the rand() function varies from one OS to another.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkNoise = class(TStk)
  protected
    FLastOutput: Single;
  public
    constructor Create(const SampleRate: Single); override;
    destructor Destroy; override;

    // Return a random number between -1.0 and 1.0 using rand().
    function Tick: Single; overload;

    // Return VectorSize random numbers between -1.0 and 1.0 in \ Vector.
    function Tick(Vector: PSingle; VectorSize: Integer): PSingle; overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

constructor TStkNoise.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FLastOutput := 0.0;
end;

destructor TStkNoise.Destroy;
begin
  inherited Destroy;
end;

function TStkNoise.Tick: Single;
begin
  FLastOutput := (2.0 * random) - 1;
  Result := FLastOutput;
end;

function TStkNoise.Tick(Vector: PSingle; VectorSize: Integer): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := Vector;
  for i := 0 to VectorSize - 1 do
   begin
    p^ := Tick;
    Inc(p);
   end;
  Result := Vector;
end;

end.
