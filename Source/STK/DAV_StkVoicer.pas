unit DAV_StkVoicer;

{
/***************************************************/
/*! \class TVoicer
    \brief STK voice manager class.

    This class can be used to manage a group of
    STK instrument classes.  Individual voices can
    be controlled via unique note tags.
    Instrument groups can be controlled by channel
    number.

    A previously constructed STK instrument class
    is linked with a voice manager using the
    addInstrument function.  An optional channel
    number argument can be specified to the
    addInstrument function as well (default
    channel:=0).  The voice manager does not
    delete any instrument instances ... it is the
    responsibility of the user to allocate and
    deallocate all instruments.

    The tick function result:=s the mix of all
    sounding voices.  Each noteOn result:=s a unique
    tag (credits to the NeXT MusicKit), so you can
    send control changes to specific voices within
    an ensemble.  Alternately, control changes can
    be sent to all voices on a given channel.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, Math;

type
  pinstrmnt = ^tinstrmnt;

  voice = object
    instrument: pInstrmnt;
    tag: integer;
    noteNumber, frequency: my_float;
    sounding, channel: integer;
  end;

  TVoicer = class(TStk)
  public
  //! Class constructor taking the maximum number of instruments to control and an optional note decay time (in seconds).
    constructor Create(sr: my_float; maxInstruments: integer;
      decayTime: MY_FLOAT = 0.2);

  //! Class destructor.
    destructor Destroy;

  //! Add an instrument with an optional channel number to the voice manager.
  {
    A set of instruments can be grouped by channel number and
    controlled via the functions which take a channel number argument.
  }
    procedure addInstrument(instrument: PInstrmnt; channel: integer = 0);

  //! Remove the given instrument pointer from the voice manager's control.
  {
    It is important that any instruments which are to be deleted by
    the user while the voice manager is running be first removed from
    the manager's control via this function!!
   }
    procedure removeInstrument(instrument: pInstrmnt);

  //! Initiate a noteOn event with the given note number and amplitude and result:= a unique note tag.
  {
    Send the noteOn message to the first available unused voice.
    If all voices are sounding, the oldest voice is interrupted and
    sent the noteOn message.  If the optional channel argument is
    non-zero, only voices on that channel are used.  If no voices are
    found for a specified non-zero channel value, the function returns
    -1.  The amplitude value should be in the range 0.0 - 1.0.
  }
    function noteOn(noteNumber, amplitude: MY_FLOAT; channel: integer = 0): integer;

  //! Send a noteOff to all voices having the given noteNumber and optional channel (default channel:=0).
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure noteOff(noteNumber, amplitude: MY_FLOAT; channel: integer = 0); overload;

  //! Send a noteOff to the voice with the given note tag.
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure noteOff(tag: integer; amplitude: MY_FLOAT); overload;

  //! Send a frequency update message to all voices assigned to the optional channel argument (default channel:=0).
  {
    The \e noteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure setFrequency(noteNumber: MY_FLOAT; channel: integer = 0); overload;

  //! Send a frequency update message to the voice with the given note tag.
  {
    The \e noteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure setFrequency(tag: integer; noteNumber: MY_FLOAT); overload;

  //! Send a pitchBend message to all voices assigned to the optional channel argument (default channel:=0).
    procedure pitchBend(Value: MY_FLOAT; channel: integer = 0); overload;

  //! Send a pitchBend message to the voice with the given note tag.
    procedure pitchBend(tag: integer; Value: MY_FLOAT); overload;

  //! Send a controlChange to all instruments assigned to the optional channel argument (default channel:=0).
    procedure controlChange(number: integer; Value: MY_FLOAT;
      channel: integer = 0); overload;

  //! Send a controlChange to the voice with the given note tag.
    procedure controlChange(tag: integer; number: integer; Value: MY_FLOAT); overload;

  //! Send a noteOff message to all existing voices.
    procedure silence;

  //! Mix the output for all sounding voices.
    function tick: MY_FLOAT; overload;

  //! Computer \e vectorSize output mixes and result:= them in \e vector.
    function tick(vector: pMY_FLOAT; vectorSize: integer): PMY_FLOAT; overload;

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  protected
    nVoices, maxVoices: integer;
    voices: array[0..128] of voice;
    mutetime, tags: integer;
    lastOutput: my_float;
  end;

implementation

{ TVoicer }

procedure TVoicer.addInstrument(instrument: PInstrmnt; channel: integer);
begin
  if (nVoices >= maxVoices) then
    exit;
  voices[nVoices].instrument := instrument;
  voices[nVoices].tag := 0;
  voices[nVoices].channel := channel;
  voices[nVoices].noteNumber := -1;
  voices[nVoices].frequency := 0.0;
  voices[nVoices].sounding := 0;
  nVoices := nVoices + 1;
end;

procedure TVoicer.controlChange(number: integer; Value: MY_FLOAT;
  channel: integer);
var
  i: integer;
begin
  for i := 0 to nVoices - 1 do
    if (voices[i].channel = channel) then
      voices[i].instrument.controlChange(number, Value);
end;

procedure TVoicer.controlChange(tag, number: integer; Value: MY_FLOAT);
var
  i: integer;
begin
  for i := 0 to nVoices - 1 do
    if (voices[i].tag = tag) then
     begin
      voices[i].instrument.controlChange(number, Value);
      break;
     end;
end;

constructor TVoicer.Create(sr: my_float; maxInstruments: integer;
  decayTime: MY_FLOAT);
begin
  inherited Create(sr);
  nVoices := 0;
  maxVoices := maxInstruments;
  tags := 23456;
  muteTime := round(decayTime * srate);
end;

destructor TVoicer.Destroy;
begin
  inherited Destroy;
end;

function TVoicer.lastOut: MY_FLOAT;
begin
  Result := lastOutput;
end;

procedure TVoicer.noteOff(tag: integer; amplitude: MY_FLOAT);
var
  i: integer;
begin
  for i := 0 to nVoices - 1 do
    if (voices[i].tag = tag) then
     begin
      voices[i].instrument.noteOff(amplitude);
      voices[i].sounding := -muteTime;
      break;
     end;
end;

procedure TVoicer.noteOff(noteNumber, amplitude: MY_FLOAT; channel: integer);
var
  i: integer;
begin
  for i := 0 to nVoices - 1 do
    if (voices[i].noteNumber = noteNumber) and (voices[i].channel = channel) then
     begin
      voices[i].instrument.noteOff(amplitude);
      voices[i].sounding := -muteTime;
     end;
end;

function TVoicer.noteOn(noteNumber, amplitude: MY_FLOAT;
  channel: integer): integer;
var
  ovoice, i: integer;
  frequency: my_float;
begin
  frequency := 220.0 * power(2.0, (noteNumber - 57.0) / 12.0);
  for i := 0 to nVoices - 1 do
    if (voices[i].noteNumber < 0) and (voices[i].channel = channel) then
     begin
      voices[i].tag := tags;
      tags := tags + 1;
      voices[i].channel := channel;
      voices[i].noteNumber := noteNumber;
      voices[i].frequency := frequency;
      voices[i].instrument.noteOn(frequency, amplitude);
      voices[i].sounding := 1;
      Result := voices[i].tag;
      exit;
     end;
  // All voices are sounding, so interrupt the oldest voice.
  ovoice := -1;
  for i := 0 to nVoices - 1 do
    if (voices[i].channel = channel) then
      if (ovoice = -1) then
        ovoice := i
      else if (voices[i].tag < voices[ovoice].tag) then
        ovoice := i;

  if (ovoice >= 0) then
   begin
    voices[ovoice].tag := tags;
    tags := tags + 1;
    voices[ovoice].channel := channel;
    voices[ovoice].noteNumber := noteNumber;
    voices[ovoice].frequency := frequency;
    voices[ovoice].instrument.noteOn(frequency, amplitude);
    voices[ovoice].sounding := 1;
    Result := voices[ovoice].tag;
    exit;
   end;
  Result := -1;
end;

procedure TVoicer.pitchBend(tag: integer; Value: MY_FLOAT);
var
  pitchScaler: my_float;
  i: integer;
begin
  Value := Value * 128;
  if (Value < 64.0) then
    pitchScaler := power(0.5, (64.0 - Value) / 64.0)
  else
    pitchScaler := power(2.0, (Value - 64.0) / 64.0);
  for i := 0 to nVoices - 1 do
    if (voices[i].tag = tag) then
     begin
      voices[i].instrument.setFrequency((voices[i].frequency * pitchScaler));
      break;
     end;
end;

procedure TVoicer.pitchBend(Value: MY_FLOAT; channel: integer);
var
  pitchScaler: my_float;
  i: integer;
begin
  Value := Value * 128;
  if (Value < 64.0) then
    pitchScaler := power(0.5, (64.0 - Value) / 64.0)
  else
    pitchScaler := power(2.0, (Value - 64.0) / 64.0);
  for i := 0 to nVoices - 1 do
    if (voices[i].channel = channel) then
      voices[i].instrument.setFrequency(
        (voices[i].frequency * pitchScaler));
end;

procedure TVoicer.removeInstrument(instrument: pInstrmnt);
var
  found: boolean;
  i: integer;
begin
  for i := 0 to nVoices - 1 do
   begin
    if (voices[i].instrument = instrument) then
      found := True;
    if (found) and (i + 1 < nVoices) then
     begin
      voices[i].instrument := voices[i + 1].instrument;
      voices[i].tag := voices[i + 1].tag;
      voices[i].noteNumber := voices[i + 1].noteNumber;
      voices[i].frequency := voices[i + 1].frequency;
      voices[i].sounding := voices[i + 1].sounding;
      voices[i].channel := voices[i + 1].channel;
     end;
   end;
  if (found) then
    nVoices := nVoices - 1;
end;

procedure TVoicer.setFrequency(tag: integer; noteNumber: MY_FLOAT);
var
  frequency: my_float;
  i: integer;
begin
  frequency := 220.0 * power(2.0, (noteNumber - 57.0) / 12.0);
  for i := 0 to nVoices - 1 do
    if (voices[i].tag = tag) then
     begin
      voices[i].noteNumber := noteNumber;
      voices[i].frequency := frequency;
      voices[i].instrument.setFrequency(frequency);
      break;
     end;
end;

procedure TVoicer.setFrequency(noteNumber: MY_FLOAT; channel: integer);
var
  frequency: my_float;
  i: integer;
begin
  frequency := 220.0 * power(2.0, (noteNumber - 57.0) / 12.0);
  for i := 0 to nVoices - 1 do
    if (voices[i].channel = channel) then
     begin
      voices[i].noteNumber := noteNumber;
      voices[i].frequency := frequency;
      voices[i].instrument.setFrequency(frequency);
     end;
end;

procedure TVoicer.silence;
var
  i: integer;
begin
  for i := 0 to nVoices - 1 do
    if (voices[i].sounding > 0) then
      voices[i].instrument.noteOff(0.5);
end;

function TVoicer.tick(vector: pMY_FLOAT; vectorSize: integer): PMY_FLOAT;
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

function TVoicer.tick: MY_FLOAT;
var
  i: integer;
begin
  lastOutput := 0.0;
  for i := 0 to nVoices - 1 do
   begin
    if (voices[i].sounding <> 0) then
      lastOutput := lastOutput + voices[i].instrument.tick;
    if (voices[i].sounding < 0) then
     begin
      voices[i].sounding := voices[i].sounding + 1;
      if (voices[i].sounding = 0) then
        voices[i].noteNumber := -1;
     end;
   end;
  Result := lastOutput / nVoices;
end;

end.
