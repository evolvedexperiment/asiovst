unit DAV_StkVoicer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TVoice manager class.

  This class can be used to manage a group of STK Instrument classes.
  Individual FVoices can be controlled via unique note Tags. Instrument groups
  can be controlled by Channel number.

  A previously constructed STK Instrument class is linked with a TVoice manager
  using the addInstrument function. An optional Channel number argument can be
  specified to the addInstrument function as well (default Channel:=0).
  The TVoice manager does not delete any Instrument instances ... it is the
  responsibility of the user to allocate and deallocate all instruments.

  The tick function result:=s the mix of all Sounding FVoices. Each noteOn
  result := s a unique Tag (credits to the NeXT MusicKit), so you can send
  control changes to specific FVoices within an ensemble. Alternately, control
  changes can be sent to all FVoices on a given Channel.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, Math;

type
  PStkInstrument = ^TStkInstrument;

  TVoice = object
    Instrument: PStkInstrument;
    Tag: Integer;
    NoteNumber, Frequency: Single;
    Sounding, Channel: Integer;
  end;

  TVoiceManager = class(TStk)
  protected
    FVoiceCount     : Integer;
    FMaxVoiceCount  : Integer;
    FVoices         : array[0..128] of TVoice;
    FMutetime, Tags : Integer;
    FLastOutput     : Single;
  public
    // Class constructor taking the maximum number of instruments to control and an optional note decay time (in seconds).
    constructor Create(SampleRate: Single; maxInstruments: Integer;
      decayTime: Single = 0.2);

    // Class destructor.
    destructor Destroy;

    // Add an Instrument with an optional Channel number to the TVoice manager.
  {
    A set of instruments can be grouped by Channel number and
    controlled via the functions which take a Channel number argument.
  }
    procedure addInstrument(Instrument: PStkInstrument; Channel: Integer = 0);

    // Remove the given Instrument pointer from the TVoice manager's control.
  {
    It is important that any instruments which are to be deleted by
    the user while the TVoice manager is running be first removed from
    the manager's control via this function!!
   }
    procedure removeInstrument(Instrument: PStkInstrument);

    // Initiate a noteOn event with the given note number and amplitude and result:= a unique note Tag.
  {
    Send the noteOn message to the first available unused TVoice.
    If all FVoices are Sounding, the oldest TVoice is interrupted and
    sent the noteOn message.  If the optional Channel argument is
    non-zero, only FVoices on that Channel are used.  If no FVoices are
    found for a specified non-zero Channel value, the function returns
    -1.  The amplitude value should be in the range 0.0 - 1.0.
  }
    function noteOn(NoteNumber, amplitude: Single; Channel: Integer = 0): Integer;

    // Send a noteOff to all FVoices having the given NoteNumber and optional Channel (default Channel:=0).
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure noteOff(NoteNumber, amplitude: Single; Channel: Integer = 0); overload;

    // Send a noteOff to the TVoice with the given note Tag.
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure noteOff(Tag: Integer; amplitude: Single); overload;

    // Send a Frequency update message to all FVoices assigned to the optional Channel argument (default Channel:=0).
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure setFrequency(NoteNumber: Single; Channel: Integer = 0); overload;

    // Send a Frequency update message to the TVoice with the given note Tag.
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure setFrequency(Tag: Integer; NoteNumber: Single); overload;

    // Send a pitchBend message to all FVoices assigned to the optional Channel argument (default Channel:=0).
    procedure pitchBend(Value: Single; Channel: Integer = 0); overload;

    // Send a pitchBend message to the TVoice with the given note Tag.
    procedure pitchBend(Tag: Integer; Value: Single); overload;

    // Send a controlChange to all instruments assigned to the optional Channel argument (default Channel:=0).
    procedure controlChange(number: Integer; Value: Single;
      Channel: Integer = 0); overload;

    // Send a controlChange to the TVoice with the given note Tag.
    procedure controlChange(Tag: Integer; number: Integer; Value: Single); overload;

    // Send a noteOff message to all existing FVoices.
    procedure silence;

    // Mix the output for all Sounding FVoices.
    function tick: Single; overload;

    // Computer \e vectorSize output mixes and result:= them in \e vector.
    function tick(vector: pMY_FLOAT; vectorSize: Integer): PMY_FLOAT; overload;

    // Return the last output value.
    function lastOut: Single;
  end;

implementation

{ TVoiceManager }

procedure TVoiceManager.addInstrument(Instrument: PStkInstrument; Channel: Integer);
begin
  if (FVoiceCount >= FMaxVoiceCount) then
    exit;
  FVoices[FVoiceCount].Instrument := Instrument;
  FVoices[FVoiceCount].Tag := 0;
  FVoices[FVoiceCount].Channel := Channel;
  FVoices[FVoiceCount].NoteNumber := -1;
  FVoices[FVoiceCount].Frequency := 0.0;
  FVoices[FVoiceCount].Sounding := 0;
  FVoiceCount := FVoiceCount + 1;
end;

procedure TVoiceManager.controlChange(number: Integer; Value: Single;
  Channel: Integer);
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Channel = Channel) then
      FVoices[i].Instrument.controlChange(number, Value);
end;

procedure TVoiceManager.controlChange(Tag, number: Integer; Value: Single);
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Tag = Tag) then
     begin
      FVoices[i].Instrument.controlChange(number, Value);
      break;
     end;
end;

constructor TVoiceManager.Create(SampleRate: Single; maxInstruments: Integer;
  decayTime: Single);
begin
  inherited Create(SampleRate);
  FVoiceCount := 0;
  FMaxVoiceCount := maxInstruments;
  Tags := 23456;
  FMutetime := round(decayTime * srate);
end;

destructor TVoiceManager.Destroy;
begin
  inherited Destroy;
end;

function TVoiceManager.lastOut: Single;
begin
  Result := FLastOutput;
end;

procedure TVoiceManager.noteOff(Tag: Integer; amplitude: Single);
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Tag = Tag) then
     begin
      FVoices[i].Instrument.noteOff(amplitude);
      FVoices[i].Sounding := -FMutetime;
      break;
     end;
end;

procedure TVoiceManager.noteOff(NoteNumber, amplitude: Single; Channel: Integer);
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].NoteNumber = NoteNumber) and (FVoices[i].Channel = Channel) then
     begin
      FVoices[i].Instrument.noteOff(amplitude);
      FVoices[i].Sounding := -FMutetime;
     end;
end;

function TVoiceManager.noteOn(NoteNumber, amplitude: Single;
  Channel: Integer): Integer;
var
  ovoice, i: Integer;
  Frequency: Single;
begin
  Frequency := 220.0 * power(2.0, (NoteNumber - 57.0) / 12.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].NoteNumber < 0) and (FVoices[i].Channel = Channel) then
     begin
      FVoices[i].Tag := Tags;
      Tags := Tags + 1;
      FVoices[i].Channel := Channel;
      FVoices[i].NoteNumber := NoteNumber;
      FVoices[i].Frequency := Frequency;
      FVoices[i].Instrument.noteOn(Frequency, amplitude);
      FVoices[i].Sounding := 1;
      Result := FVoices[i].Tag;
      exit;
     end;
  // All FVoices are Sounding, so interrupt the oldest TVoice.
  ovoice := -1;
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Channel = Channel) then
      if (ovoice = -1) then
        ovoice := i
      else if (FVoices[i].Tag < FVoices[ovoice].Tag) then
        ovoice := i;

  if (ovoice >= 0) then
   begin
    FVoices[ovoice].Tag := Tags;
    Tags := Tags + 1;
    FVoices[ovoice].Channel := Channel;
    FVoices[ovoice].NoteNumber := NoteNumber;
    FVoices[ovoice].Frequency := Frequency;
    FVoices[ovoice].Instrument.noteOn(Frequency, amplitude);
    FVoices[ovoice].Sounding := 1;
    Result := FVoices[ovoice].Tag;
    exit;
   end;
  Result := -1;
end;

procedure TVoiceManager.pitchBend(Tag: Integer; Value: Single);
var
  pitchScaler: Single;
  i: Integer;
begin
  Value := Value * 128;
  if (Value < 64.0) then
    pitchScaler := power(0.5, (64.0 - Value) / 64.0)
  else
    pitchScaler := power(2.0, (Value - 64.0) / 64.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Tag = Tag) then
     begin
      FVoices[i].Instrument.setFrequency((FVoices[i].Frequency * pitchScaler));
      break;
     end;
end;

procedure TVoiceManager.pitchBend(Value: Single; Channel: Integer);
var
  pitchScaler: Single;
  i: Integer;
begin
  Value := Value * 128;
  if (Value < 64.0) then
    pitchScaler := power(0.5, (64.0 - Value) / 64.0)
  else
    pitchScaler := power(2.0, (Value - 64.0) / 64.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Channel = Channel) then
      FVoices[i].Instrument.setFrequency(
        (FVoices[i].Frequency * pitchScaler));
end;

procedure TVoiceManager.removeInstrument(Instrument: PStkInstrument);
var
  found: boolean;
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
   begin
    if (FVoices[i].Instrument = Instrument) then
      found := True;
    if (found) and (i + 1 < FVoiceCount) then
     begin
      FVoices[i].Instrument := FVoices[i + 1].Instrument;
      FVoices[i].Tag := FVoices[i + 1].Tag;
      FVoices[i].NoteNumber := FVoices[i + 1].NoteNumber;
      FVoices[i].Frequency := FVoices[i + 1].Frequency;
      FVoices[i].Sounding := FVoices[i + 1].Sounding;
      FVoices[i].Channel := FVoices[i + 1].Channel;
     end;
   end;
  if (found) then
    FVoiceCount := FVoiceCount - 1;
end;

procedure TVoiceManager.setFrequency(Tag: Integer; NoteNumber: Single);
var
  Frequency: Single;
  i: Integer;
begin
  Frequency := 220.0 * power(2.0, (NoteNumber - 57.0) / 12.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Tag = Tag) then
     begin
      FVoices[i].NoteNumber := NoteNumber;
      FVoices[i].Frequency := Frequency;
      FVoices[i].Instrument.setFrequency(Frequency);
      break;
     end;
end;

procedure TVoiceManager.setFrequency(NoteNumber: Single; Channel: Integer);
var
  Frequency: Single;
  i: Integer;
begin
  Frequency := 220.0 * power(2.0, (NoteNumber - 57.0) / 12.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Channel = Channel) then
     begin
      FVoices[i].NoteNumber := NoteNumber;
      FVoices[i].Frequency := Frequency;
      FVoices[i].Instrument.setFrequency(Frequency);
     end;
end;

procedure TVoiceManager.silence;
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Sounding > 0) then
      FVoices[i].Instrument.noteOff(0.5);
end;

function TVoiceManager.tick(vector: pMY_FLOAT; vectorSize: Integer): PMY_FLOAT;
var
  i: Integer;
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

function TVoiceManager.tick: Single;
var
  i: Integer;
begin
  FLastOutput := 0.0;
  for i := 0 to FVoiceCount - 1 do
   begin
    if (FVoices[i].Sounding <> 0) then
      FLastOutput := FLastOutput + FVoices[i].Instrument.tick;
    if (FVoices[i].Sounding < 0) then
     begin
      FVoices[i].Sounding := FVoices[i].Sounding + 1;
      if (FVoices[i].Sounding = 0) then
        FVoices[i].NoteNumber := -1;
     end;
   end;
  Result := FLastOutput / FVoiceCount;
end;

end.
