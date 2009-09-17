unit DAV_DspAudioToMidiTrigger;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspFilterBasics;

type
  TAudio2MidiTriggerFlag = (amFilterBypass, amFilterOutput);
  TAudio2MidiTriggerFlags = set of TAudio2MidiTriggerFlag;

  TTriggerNotifyEvent = procedure(Sender: TObject; const Level: Single) of object;

  TCustomAudio2MidiTrigger = class(TDspObject)
  private
    procedure SetSampleRate(const Value: Double);
    procedure SetFlags(const Value: TAudio2MidiTriggerFlags);
    procedure SetThreshold(const Value: Double);
    procedure SetInterval(const Value: Double);
    procedure CalculateReciprocalSamplerate;
    function GetFilterCount: Integer;
    function GetFilter(Index: Integer): TCustomFilter;
  protected
    FInterval         : Double;
    FFilter           : array of TCustomFilter;
    FFlags            : TAudio2MidiTriggerFlags;
    FOnTrigger        : TTriggerNotifyEvent;
    FSampleCount      : Integer;
    FSampleInterval   : Integer;
    FSampleRate, FSRR : Double;
    FThreshold        : Double;
    FThresholdFactor  : Double;
    procedure IntervalChanged; virtual;
    procedure FlagsChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure ThresholdChanged; virtual;

    property SampleRateReciprocal: Double read FSRR;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ProcessSample(const Input: Double): Double; virtual;
    procedure AddFilter(const Filter: TCustomFilter); virtual;
    procedure DeleteFilter(const Filter: TCustomFilter); overload; virtual;
    procedure DeleteFilter(const Index: Integer); overload; virtual;

    property Flags: TAudio2MidiTriggerFlags read FFlags write SetFlags;
    property FilterCount: Integer read GetFilterCount;
    property Filter[Index: Integer]: TCustomFilter read GetFilter;
    property Interval: Double read FInterval write SetInterval;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Threshold: Double read FThreshold write SetThreshold;
    property OnTrigger: TTriggerNotifyEvent read FOnTrigger write FOnTrigger;
  end;

  TAudio2MidiTrigger = class(TCustomAudio2MidiTrigger)
  published
    property Flags;       
    property Interval;    // [s]
    property SampleRate;  // [Hz]
    property Threshold;   // [dB]
  end;

implementation

uses
  SysUtils;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds [%d]';

{ TCustomAudio2MidiTrigger }

constructor TCustomAudio2MidiTrigger.Create;
begin

end;

destructor TCustomAudio2MidiTrigger.Destroy;
var
  Band : Integer;
begin
 for Band := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Band])
   then FreeAndNil(FFilter[Band]);
  inherited;
end;

procedure TCustomAudio2MidiTrigger.AddFilter(const Filter: TCustomFilter);
begin
 // make sure a filter is passed
 assert(Filter <> nil);

 // increase size of filter array
 SetLength(FFilter, Length(FFilter) + 1);

 // actually add filter
 FFilter[Length(FFilter) - 1] := Filter;
end;

procedure TCustomAudio2MidiTrigger.DeleteFilter(const Filter: TCustomFilter);
var
  Index : Integer;
begin
 // make sure a filter is passed
 assert(Filter <> nil);

 Index := 0;
 while Index < Length(FFilter) do
  begin
   if Index < Length(FFilter) - 1
    then Move(FFilter[Index + 1], FFilter[Index], (Length(FFilter) - Index - 1) * SizeOf(Pointer));

   // decrease size of filter array
   SetLength(FFilter, Length(FFilter) - 1);
  end;
end;

procedure TCustomAudio2MidiTrigger.DeleteFilter(const Index: Integer);
begin
 if (Index >= 0) and (Index < Length(FFilter)) then
  begin
   if Index < Length(FFilter) - 1
    then Move(FFilter[Index + 1], FFilter[Index], (Length(FFilter) - Index - 1) * SizeOf(Pointer));

   // decrease size of filter array
   SetLength(FFilter, Length(FFilter) - 1);
  end else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomAudio2MidiTrigger.CalculateReciprocalSamplerate;
begin
 FSRR := 1 / FSampleRate;
end;

procedure TCustomAudio2MidiTrigger.SetFlags(const Value: TAudio2MidiTriggerFlags);
begin
 if FFlags <> Value then
  begin
   FFlags := Value;
   FlagsChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.SetInterval(const Value: Double);
begin
 if FInterval <> Value then
  begin
   FInterval := Value;
   IntervalChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.FlagsChanged;
begin
 // eventually change function pointer here!
end;

function TCustomAudio2MidiTrigger.GetFilter(Index: Integer): TCustomFilter;
begin
 if (Index >= 0) and (Index < Length(FFilter))
  then Result := Filter[Index] 
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomAudio2MidiTrigger.GetFilterCount: Integer;
begin
 Result := Length(FFilter);
end;

procedure TCustomAudio2MidiTrigger.IntervalChanged;
begin
 FSampleInterval := round(Interval * SampleRate);
end;

procedure TCustomAudio2MidiTrigger.SampleRateChanged;
var
  Band : Integer;
begin
 CalculateReciprocalSamplerate;
 for Band := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Band]) then FFilter[Band].SampleRate := SampleRate;
end;

procedure TCustomAudio2MidiTrigger.ThresholdChanged;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;

function TCustomAudio2MidiTrigger.ProcessSample(const Input: Double): Double;
var
  Band : Integer;
begin
 Result := Input;

 // eventually filter audio data
 if not (amFilterBypass in FFlags) then
  for Band := 0 to Length(FFilter) - 1 do
   if assigned(FFilter[Band])
    then Result := FFilter[Band].ProcessSample(Result);

 // check if interval is over
 if (FSampleCount >= 0) then
  if (abs(Result) > FThresholdFactor) then
   begin
    if assigned(FOnTrigger)
     then FOnTrigger(Self, Amp_to_dB(abs(Result)));

    // reset sample count
    FSampleCount := FSampleInterval;
   end else
  else Dec(FSampleCount);

 // eventually restore original signal 
 if not (amFilterOutput in FFlags)
  then Result := Input;
end;

end.
