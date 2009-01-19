unit DAV_StkMandolin;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  DAV_Stk TMandolin instrument model class.

   This class inherits from PluckTwo and uses "commuted synthesis" techniques
   to model a mandolin instrument.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.
   Commuted Synthesis, in particular, is covered by patents, granted, pending,
   and/or applied-for. All are assigned to the Board of Trustees,
   Stanford University. For information, contact the Office of Technology
   Licensing, Stanford University.

   Control Change Numbers:
     - Body Size = 2
     - Pluck APosition = 4
     - String Sustain = 11
     - String Detuning = 1
     - Microphone APosition = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkPluckTwo, DAV_StkWavePlayer;

type
  TMandolin = class(TPluckTwo)
  protected
    FSoundFile  : array[0..11] of TWavePlayer;
    FDirectBody : Single;
    FMic        : Integer;
    FDampTime   : Integer;
    FWaveDone   : Boolean;
  public
    constructor Create(SampleRate, LowestFrequency: Single);
    destructor Destroy; override;

    // Pluck the strings with the given AAmplitude (0.0 - 1.0) using the current AFrequency.
    procedure Pluck(AAmplitude: Single); overload;

    // Pluck the strings with the given AAmplitude (0.0 - 1.0) and APosition (0.0 - 1.0).
    procedure Pluck(AAmplitude, APosition: Single); overload;

    // Start a note with the given AFrequency and AAmplitude (0.0 - 1.0).
    procedure NoteOn(AFrequency, AAmplitude: Single);

    // Set the body Size (a value of 1.0 produces the "default" Size).
    procedure SetBodySize(Size: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by number and value (0.0 - 128.0).
    procedure ControlChange(Number: Integer; Value: Single);
  end;

implementation

constructor TMandolin.Create;
begin
  inherited Create(SampleRate, LowestFrequency);
  FSoundFile[ 0] := TWavePlayer.Create(SampleRate, 'mand1.wav');
  FSoundFile[ 1] := TWavePlayer.Create(SampleRate, 'mand2.wav');
  FSoundFile[ 2] := TWavePlayer.Create(SampleRate, 'mand3.wav');
  FSoundFile[ 3] := TWavePlayer.Create(SampleRate, 'mand4.wav');
  FSoundFile[ 4] := TWavePlayer.Create(SampleRate, 'mand5.wav');
  FSoundFile[ 5] := TWavePlayer.Create(SampleRate, 'mand6.wav');
  FSoundFile[ 6] := TWavePlayer.Create(SampleRate, 'mand7.wav');
  FSoundFile[ 7] := TWavePlayer.Create(SampleRate, 'mand8.wav');
  FSoundFile[ 8] := TWavePlayer.Create(SampleRate, 'mand9.wav');
  FSoundFile[ 9] := TWavePlayer.Create(SampleRate, 'mand10.wav');
  FSoundFile[10] := TWavePlayer.Create(SampleRate, 'mand11.wav');
  FSoundFile[11] := TWavePlayer.Create(SampleRate, 'mand12.wav');
  FDirectBody := 1.0;
  FMic := 0;
  FDampTime := 0;
// FWaveDone := FSoundFile[FMic].isFinished;
end;

destructor TMandolin.Destroy;
var
  Integer: Integer;
begin
  inherited Destroy;
  for Integer := 0 to 11 do
    FSoundFile[Integer].Free;
end;

procedure TMandolin.Pluck(AAmplitude: Single);
begin
 // This function gets interesting, because pluck
 // may be longer than string length, so we just
 // reset the FSoundFile and add in the pluck in
 // the Tick method.
  FSoundFile[FMic].reset;
  FWaveDone := False;
  pluckAmplitude := AAmplitude;
  if (AAmplitude < 0.0) then
    pluckAmplitude := 0.0
  else if (AAmplitude > 1.0) then
    pluckAmplitude := 1.0;
 // Set the pick APosition, which puts zeroes at APosition * length.
  combDelay.setDelay(0.5 * pluckPosition * lastLength);
  FDampTime := round(lastLength);   // See Tick method below.
end;

procedure TMandolin.Pluck(AAmplitude, APosition: Single);
begin
  // Pluck APosition puts zeroes at APosition * length.
  pluckPosition := APosition;
  if (APosition < 0.0)
   then pluckPosition := 0.0 else
  if (APosition > 1.0)
   then pluckPosition := 1.0;
  Pluck(AAmplitude);
end;

procedure TMandolin.noteOn;
begin
  SetFrequency(AFrequency);
  Pluck(AAmplitude);
end;

procedure TMandolin.SetBodySize;
var
  Rate    : Single;
  Integer : Integer;
begin
  // Scale the commuted body response by its sample rate (22050).
  Rate := Size;
  for Integer := 0 to 11
   do FSoundFile[Integer].SetFrequency(Rate);
end;

function TMandolin.Tick: Single;
var
  temp: Single;
begin
// if ( not FWaveDone ) then
   begin
   // Scale the pluck excitation with comb
   // filtering for the duration of the file.
    temp := FSoundFile[FMic].Tick * pluckAmplitude;
    temp := temp - combDelay.Tick(temp);
//    FWaveDone:=FSoundFile[FMic].isFinished;
   end;

  // Damping hack to help aprocedure overflow on re-plucking.
  if (FDampTime >= 0) then
   begin
    FDampTime := FDampTime - 1;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput := delayLine.Tick(
      filter.Tick(temp + (delayLine.lastOut * 0.7)));
    // Calculate 2nd delay just like the 1st.
    lastOutput := lastoutput + delayLine2.Tick(
      filter2.Tick(temp + (delayLine2.lastOut * 0.7)));
   end
  else
   begin // No damping hack after 1 period.
    loopgain := 0.999;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput := delayLine.Tick(
      filter.Tick(temp + (delayLine.lastOut * loopGain)));
    // Calculate 2nd delay just like the 1st.
    lastOutput := lastoutput + delayLine2.Tick(
      filter2.Tick(temp + (delayLine2.lastOut * loopGain)));
   end;

  lastOutput := lastoutput * 0.3;
  Result := lastOutput;
end;

procedure TMandolin.ControlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then norm := 0.0 else
  if (norm > 1.0) then norm := 1.0;

  if (Number = __SK_BodySize_) then // 2
    SetBodySize(norm * 2.0)
  else if (Number = __SK_PickPosition_) then // 4
    setPluckPosition(norm)
  else if (Number = __SK_StringDamping_) then // 11
    setBaseLoopGain(0.97 + (norm * 0.03))
  else if (Number = __SK_StringDetune_) then // 1
    setDetune(1.0 - (norm * 0.1))
  else if (Number = __SK_AfterTouch_Cont_) then // 128
    FMic := round(norm * 11.0);
end;

end.
