unit DAV_StkVoicer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK voice manager class.

  This class can be used to manage a group of STK instrument classes.
  Individual voices can be controlled via unique note tags. instrument groups
  can be controlled by channel number.

  A previously constructed STK instrument class is linked with a voice manager
  using the addInstrument function. An optional channel number argument can be
  specified to the addInstrument function as well (default channel = 0).
  The voice manager does not delete any instrument instances ... it is the
  responsibility of the user to allocate and deallocate all instruments.

  The tick function returns the mix of all sounding voices. Each noteOn
  returns a unique Tag (credits to the NeXT MusicKit), so you can send
  control changes to specific voices within an ensemble. Alternately, control
  changes can be sent to all voices on a given Channel.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, Math;

type
  TVoice = object
    Instrument : TStkControlableInstrument;
    Tag        : Integer;
    NoteNumber : Single;
    Frequency  : Single;
    Sounding   : Integer;
    Channel    : Integer;
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
    constructor Create(const SampleRate: Single; const maxInstruments: Integer;
      const decayTime: Single = 0.2); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Add an Instrument with an optional channel number to the voice manager.
  {
    A set of instruments can be grouped by channel number and
    controlled via the functions which take a channel number argument.
  }
    procedure AddInstrument(const Instrument: TStkControlableInstrument; const Channel: Integer = 0);

    // Remove the given Instrument pointer from the voice manager's control.
  {
    It is important that any instruments which are to be deleted by
    the user while the voice manager is running be first removed from
    the manager's control via this function!!
   }
    procedure RemoveInstrument(const Instrument: TStkControlableInstrument);

    // Initiate a noteOn event with the given note number and amplitude and result:= a unique note Tag.
  {
    Send the noteOn message to the first available unused TVoice.
    If all voices are Sounding, the oldest TVoice is interrupted and
    sent the noteOn message.  If the optional channel argument is
    non-zero, only voices on that channel are used.  If no voices are
    found for a specified non-zero channel value, the function returns
    -1.  The amplitude value should be in the range 0.0 - 1.0.
  }
    function NoteOn(const NoteNumber, Amplitude: Single; const Channel: Integer = 0): Integer;

    // Send a noteOff to all voices having the given NoteNumber and optional channel (default Channel:=0).
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure NoteOff(const NoteNumber, Amplitude: Single; const Channel: Integer = 0); overload;

    // Send a noteOff to the TVoice with the given note Tag.
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure NoteOff(const Tag: Integer; const Amplitude: Single); overload;

    // Send a Frequency update message to all voices assigned to the optional channel argument (default Channel:=0).
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure SetFrequency(const NoteNumber: Single; const Channel: Integer = 0); overload;

    // Send a Frequency update message to the TVoice with the given note Tag.
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure SetFrequency(const Tag: Integer; const NoteNumber: Single); overload;

    // Send a pitchBend message to all voices assigned to the optional channel argument (default channel = 0).
    procedure PitchBend(const Value: Single; const Channel: Integer = 0); overload;

    // Send a pitchBend message to the voice with the given note Tag.
    procedure PitchBend(const Tag: Integer; Value: Single); overload;

    // Send a controlChange to all instruments assigned to the optional channel argument (default channel = 0).
    procedure ControlChange(const Number: Integer; const Value: Single;
      const Channel: Integer = 0); overload;

    // Send a controlChange to the voice with the given note tag.
    procedure ControlChange(const Tag, Number: Integer; const Value: Single); overload;

    // Send a noteOff message to all existing voices.
    procedure Silence;

    // Mix the output for all sounding voices.
    function tick: Single; overload;

    // Computer \e vectorSize output mixes and result:= them in \e vector.
    function tick(vector: pSingle; vectorSize: Integer): PSingle; overload;

    property LastOutput: Single read FLastOutput;
  end;

implementation

uses
  SysUtils;

{ TVoiceManager }

procedure TVoiceManager.AddInstrument(const Instrument: TStkControlableInstrument; const Channel: Integer);
begin
  if (FVoiceCount >= FMaxVoiceCount) then exit;
  FVoices[FVoiceCount].Instrument := Instrument;
  FVoices[FVoiceCount].Tag := 0;
  FVoices[FVoiceCount].Channel := Channel;
  FVoices[FVoiceCount].NoteNumber := -1;
  FVoices[FVoiceCount].Frequency := 0.0;
  FVoices[FVoiceCount].Sounding := 0;
  FVoiceCount := FVoiceCount + 1;
end;

procedure TVoiceManager.ControlChange(const Number: Integer; const Value: Single;
  const Channel: Integer);
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Channel = Channel) then
      FVoices[i].Instrument.ControlChange(number, Value);
end;

procedure TVoiceManager.controlChange(const Tag, Number: Integer; const Value: Single);
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

constructor TVoiceManager.Create(const SampleRate: Single; const MaxInstruments: Integer;
  const DecayTime: Single);
begin
  inherited Create(SampleRate);
  FVoiceCount := 0;
  FMaxVoiceCount := maxInstruments;
  Tags := 23456;
  FMutetime := round(decayTime * SampleRate);
end;

destructor TVoiceManager.Destroy;
begin
  inherited Destroy;
end;

procedure TVoiceManager.noteOff(const Tag: Integer; const Amplitude: Single);
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

procedure TVoiceManager.NoteOff(const NoteNumber, Amplitude: Single; const Channel: Integer);
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

function TVoiceManager.NoteOn(const NoteNumber, Amplitude: Single;
  const Channel: Integer): Integer;
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

procedure TVoiceManager.PitchBend(const Tag: Integer; Value: Single);
var
  pitchScaler: Single;
  i: Integer;
begin
  Value := Value * 128;
  if (Value < 64.0) then
    pitchScaler := Power(0.5, (64.0 - Value) / 64.0)
  else
    pitchScaler := Power(2.0, (Value - 64.0) / 64.0);
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Tag = Tag) then
     begin
      FVoices[i].Instrument.Frequency := FVoices[i].Frequency * pitchScaler;
      break;
     end;
end;

procedure TVoiceManager.PitchBend(const Value: Single; const Channel: Integer);
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
      FVoices[i].Instrument.SetFrequency(
        (FVoices[i].Frequency * pitchScaler));
end;

procedure TVoiceManager.removeInstrument(const Instrument: PStkInstrument);
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

procedure TVoiceManager.SetFrequency(const Tag: Integer; const NoteNumber: Single);
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
      FVoices[i].Instrument.SetFrequency(Frequency);
      break;
     end;
end;

procedure TVoiceManager.SetFrequency(const NoteNumber: Single; const Channel: Integer);
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
      FVoices[i].Instrument.SetFrequency(Frequency);
     end;
end;

procedure TVoiceManager.Silence;
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    if (FVoices[i].Sounding > 0) then
      FVoices[i].Instrument.NoteOff(0.5);
end;

function TVoiceManager.tick(vector: pSingle; vectorSize: Integer): PSingle;
var
  i: Integer;
  p: pSingle;
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
