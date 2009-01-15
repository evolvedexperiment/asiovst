unit DAV_StkMandolin;

{
/***************************************************/
/*! \class TMandolin
    \brief STK TMandolin instrument model class.

    This class inherits from PluckTwo and uses
    "commuted synthesis" techniques to model a
    mandolin instrument.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.
    Commuted Synthesis, in particular, is covered
    by patents, granted, pending, and/or
    applied-for.  All are assigned to the Board of
    Trustees, Stanford University.  For
    information, contact the Office of Technology
    Licensing, Stanford University.

    Control Change Numbers: 
       - Body Size:=2
       - Pluck Position:=4
       - String Sustain:=11
       - String Detuning:=1
       - Microphone Position:=128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, PluckTwo, wavePlayer;

type
  TMandolin = class(TPluckTwo)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Pluck the strings with the given amplitude (0.0 - 1.0) using the current frequency.
    procedure pluck(amplitude: MY_FLOAT); overload;

  //! Pluck the strings with the given amplitude (0.0 - 1.0) and position (0.0 - 1.0).
    procedure pluck(amplitude, position: MY_FLOAT); overload;

  //! Start a note with the given frequency and amplitude (0.0 - 1.0).
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Set the body size (a value of 1.0 produces the "default" size).
    procedure setBodySize(size: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    soundfile: array[0..11] of twavePlayer;
    directBody: my_float;
    mic: integer;
    dampTime: longint;
    waveDone: boolean;
  end;

implementation

constructor TMandolin.Create;
begin
  inherited Create(sr, lowestfrequency);
  soundfile[0] := TWavePlayer.Create(srate, 'c:\stk\mand1.wav');
  soundfile[1] := TWavePlayer.Create(srate, 'c:\stk\mand2.wav');
  soundfile[2] := TWavePlayer.Create(srate, 'c:\stk\mand3.wav');
  soundfile[3] := TWavePlayer.Create(srate, 'c:\stk\mand4.wav');
  soundfile[4] := TWavePlayer.Create(srate, 'c:\stk\mand5.wav');
  soundfile[5] := TWavePlayer.Create(srate, 'c:\stk\mand6.wav');
  soundfile[6] := TWavePlayer.Create(srate, 'c:\stk\mand7.wav');
  soundfile[7] := TWavePlayer.Create(srate, 'c:\stk\mand8.wav');
  soundfile[8] := TWavePlayer.Create(srate, 'c:\stk\mand9.wav');
  soundfile[9] := TWavePlayer.Create(srate, 'c:\stk\mand10.wav');
  soundfile[10] := TWavePlayer.Create(srate, 'c:\stk\mand11.wav');
  soundfile[11] := TWavePlayer.Create(srate, 'c:\stk\mand12.wav');
  directBody := 1.0;
  mic := 0;
  dampTime := 0;
// waveDone:=soundfile[mic].isFinished;
end;

destructor TMandolin.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to 11 do
    soundfile[i].Free;
end;

procedure TMandolin.pluck(amplitude: MY_FLOAT);
begin
 // This function gets interesting, because pluck
 // may be longer than string length, so we just
 // reset the soundfile and add in the pluck in
 // the tick method.
  soundfile[mic].reset;
  waveDone := False;
  pluckAmplitude := amplitude;
  if (amplitude < 0.0) then
    pluckAmplitude := 0.0
  else if (amplitude > 1.0) then
    pluckAmplitude := 1.0;
 // Set the pick position, which puts zeroes at position * length.
  combDelay.setDelay(0.5 * pluckPosition * lastLength);
  dampTime := round(lastLength);   // See tick method below.
end;

procedure TMandolin.pluck(amplitude, position: MY_FLOAT);
begin
  // Pluck position puts zeroes at position * length.
  pluckPosition := position;
  if (position < 0.0) then
    pluckPosition := 0.0
  else if (position > 1.0) then
    pluckPosition := 1.0;
  pluck(amplitude);
end;

procedure TMandolin.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
end;

procedure TMandolin.setBodySize;
var
  rate: my_float;
  i: integer;
begin
  // Scale the commuted body response by its sample rate (22050).
  rate := size;
  for i := 0 to 11 do
    soundfile[i].setfrequency(rate);
end;

function TMandolin.tick: my_float;
var
  temp: my_float;
begin
// if ( not waveDone ) then
   begin
   // Scale the pluck excitation with comb
   // filtering for the duration of the file.
    temp := soundfile[mic].tick * pluckAmplitude;
    temp := temp - combDelay.tick(temp);
//    waveDone:=soundfile[mic].isFinished;
   end;

  // Damping hack to help aprocedure overflow on re-plucking.
  if (dampTime >= 0) then
   begin
    dampTime := dampTime - 1;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput := delayLine.tick(
      filter.tick(temp + (delayLine.lastOut * 0.7)));
    // Calculate 2nd delay just like the 1st.
    lastOutput := lastoutput + delayLine2.tick(
      filter2.tick(temp + (delayLine2.lastOut * 0.7)));
   end
  else
   begin // No damping hack after 1 period.
    loopgain := 0.999;
    // Calculate 1st delay filtered reflection plus pluck excitation.
    lastOutput := delayLine.tick(
      filter.tick(temp + (delayLine.lastOut * loopGain)));
    // Calculate 2nd delay just like the 1st.
    lastOutput := lastoutput + delayLine2.tick(
      filter2.tick(temp + (delayLine2.lastOut * loopGain)));
   end;

  lastOutput := lastoutput * 0.3;
  Result := lastOutput;
end;

procedure TMandolin.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_BodySize_) then // 2
    setBodySize(norm * 2.0)
  else if (number = __SK_PickPosition_) then // 4
    setPluckPosition(norm)
  else if (number = __SK_StringDamping_) then // 11
    setBaseLoopGain(0.97 + (norm * 0.03))
  else if (number = __SK_StringDetune_) then // 1
    setDetune(1.0 - (norm * 0.1))
  else if (number = __SK_AfterTouch_Cont_) then // 128
    mic := round(norm * 11.0);
end;

end.
