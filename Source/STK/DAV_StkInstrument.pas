unit DAV_StkInstrument;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK instrument abstract base class.

   This class provides a common interface for all STK instruments.
}
interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk;

type
  TStkInstrument = class(TStk)
  protected
    FLastOutput: Single;
  public
    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(Frequency, Amplitude: Single); virtual; abstract;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(Amplitude: Single); virtual; abstract;

    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(Number: Integer; Value: Single); virtual; abstract;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(Frequency: Single); virtual; abstract;


    // Compute one output sample.
    function Tick: Single; overload; virtual;

    // Computer VectorSize outputs and return them in Vector.
    function Tick(Vector: PSingle; VectorSize: longint): PSingle; overload; virtual;

    property LastOutput: Single read FLastOutput;
  end;

implementation

function TStkInstrument.Tick: Single;
begin
  Result := 0;
end;

function TStkInstrument.Tick(Vector: PSingle; VectorSize: longint): PSingle;
var
  i: Integer;
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
