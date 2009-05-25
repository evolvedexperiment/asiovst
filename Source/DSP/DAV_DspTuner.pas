unit DAV_DspTuner;

interface

{$I ..\DAV_Compiler.inc}
{$DEFINE OnlineFreqCalc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspButterworthFilter, DAV_DspCorrelation;

type
  TCustomTuner = class(TDspObject)
  private
    FSampleRate : Single;
    procedure SetSampleRate(const Value: Single);
  protected
    procedure SampleRateChanged; virtual;
    function GetCurrentFrequency: Single; virtual; abstract;
  public
    constructor Create; virtual;
    procedure Process(Input: Single); virtual; abstract;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property CurrentFrequency: Single read GetCurrentFrequency;
  end;

  TCustomDownsampledTuner = class(TCustomTuner)
  private
    FMaximumFrequency : Single;
    FMinimumFrequency : Single;
    FDownsampleBW     : Single;
    function GetDSFilterOrder: Cardinal;
    procedure SetDSFilterOrder(const Value: Cardinal);
    procedure SetMaximumFrequency(const Value: Single);
    procedure SetMinimumFrequency(const Value: Single);
    procedure SetupMaximumFrequency;
    procedure SetDownsampleBW(Value: Single);
  protected
    FLowpass           : TButterworthLowPassFilter;
    FHighpass          : TButterworthHighPassFilter;
    FDownSampleFactor  : Integer;
    FDownSampleCounter : Integer;
    procedure ProcessDownsampled(DownSampled: Single); virtual; abstract;
    procedure SampleRateChanged; override;
    procedure CalculateDownsampleFactor; virtual;
    procedure MaximumFrequencyChanged; virtual;
    procedure MinimumFrequencyChanged; virtual;
  public
    constructor Create; override;
    procedure Process(Input: Single); override;

    property DownSampleFilterOrder: Cardinal read GetDSFilterOrder write SetDSFilterOrder;
    property DownSampleBandwidth: Single read FDownsampleBW write SetDownsampleBW;
    property MaximumFrequency: Single read FMaximumFrequency write SetMaximumFrequency;
    property MinimumFrequency: Single read FMinimumFrequency write SetMinimumFrequency;
  end;

  TCustomZeroCrossingTuner = class(TCustomDownsampledTuner)
  private
    FSmoothFactor   : Single;
    procedure SetSmoothFactor(const Value: Single);
    procedure SetOneCrossingOnly(const Value: Boolean);
  protected
    FIsAbove         : Boolean;
    FSamples         : Integer;
    FAverageSamples  : Single;
    FOneCrossingOnly : Boolean;
    FFrequencyFactor : Single;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq     : Single;
    {$ENDIF}
    function GetCurrentFrequency: Single; override;
    procedure SmoothFactorChanged; virtual;
    procedure ProcessDownsampled(Downsampled: Single); override;
    function CalculateCurrentFrequency: Single; virtual;
  public
    constructor Create; override;
    property SmoothFactor: Single read FSmoothFactor write SetSmoothFactor;
    property OneCrossingOnly: Boolean read FOneCrossingOnly write SetOneCrossingOnly;
  end;

  TCustomLinearZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  protected
    FLastSample : Single;
    FLastOffset : Single;
    procedure ProcessDownsampled(Downsampled: Single); override;
  end;

  TTunerNoteString = array [0..1] of AnsiChar;
  TCustomAdvancedTuner = class(TCustomLinearZeroCrossingTuner)
  private
    FCurrentNote             : TTunerNoteString;
    FCurrentDetune           : Single;
    FAttack, FAttackFactor   : Single;
    FRelease, FReleaseFactor : Single;
    FLevel, FThreshold       : Single;

    procedure SetAttack(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetThreshold(const Value: Single);
  protected
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure CalculateDownsampleFactor; override;
    function CalculateCurrentFrequency: Single; override;
    procedure ProcessDownsampled(Downsampled: Single); override;
  public
    constructor Create; override;
    property Attack: Single read FAttack write SetAttack;
    property Release: Single read FRelease write SetRelease;
    property Threshold: Single read FThreshold write SetThreshold;
    property CurrentNote: TTunerNoteString read FCurrentNote;
    property CurrentDetune: Single read FCurrentDetune;
  end;

  TZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property OneCrossingOnly;
    property SmoothFactor;
  end;

  TLinearZeroCrossingTuner = class(TCustomLinearZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property OneCrossingOnly;
    property SmoothFactor;
  end;

  TAdvancedTuner = class(TCustomAdvancedTuner)
  published
    property CurrentDetune;
  end;

  TTuner = class(TLinearZeroCrossingTuner);

implementation

uses
  DAV_Approximations;

{ TCustomTuner }

constructor TCustomTuner.Create;
begin
 FSampleRate := 44100;
 SampleRateChanged;
end;

procedure TCustomTuner.SampleRateChanged;
begin
 // nothing in here yet!
end;

procedure TCustomTuner.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TCustomDownsampledTuner }

constructor TCustomDownsampledTuner.Create;
begin
 FLowpass  := TButterworthLowPassFilter.Create(4);
 FHighpass := TButterworthHighPassFilter.Create(2);
 FMaximumFrequency := 4000;
 FMinimumFrequency := 100;
 FDownsampleBW     := 0.7;

 MinimumFrequencyChanged;
 SetupMaximumFrequency;
 FDownSampleCounter := 1;
 inherited;
end;

procedure TCustomDownsampledTuner.SampleRateChanged;
begin
 inherited;
 CalculateDownsampleFactor;
 FLowpass.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate / FDownSampleFactor;
end;

procedure TCustomDownsampledTuner.CalculateDownsampleFactor;
var
  NewFactor      : Integer;
  CurrentNyquist : Single;
begin
 CurrentNyquist := 0.5 * SampleRate;
 NewFactor := 1;
 while FDownsampleBW * 0.5 * CurrentNyquist > MaximumFrequency do
  begin
   CurrentNyquist := 0.5 * CurrentNyquist;
   NewFactor := NewFactor shl 1;
  end;
 FDownSampleFactor := NewFactor;
end;

function TCustomDownsampledTuner.GetDSFilterOrder: Cardinal;
begin
 result := FLowpass.Order;
end;

procedure TCustomDownsampledTuner.SetupMaximumFrequency;
begin
 FLowpass.Frequency := FMaximumFrequency;
end; 

procedure TCustomDownsampledTuner.MaximumFrequencyChanged;
begin
 SetupMaximumFrequency;
 CalculateDownsampleFactor;
 FHighpass.SampleRate := SampleRate / FDownSampleFactor;
end;

procedure TCustomDownsampledTuner.MinimumFrequencyChanged;
begin
 FHighpass.Frequency := FMinimumFrequency;
end;

procedure TCustomDownsampledTuner.Process(Input: Single);
var
  LowpassedSignal : Double;
begin
 LowpassedSignal := FLowpass.ProcessSample(Input);
 Dec(FDownSampleCounter);
 if FDownSampleCounter = 0 then
  begin
   FDownSampleCounter := FDownSampleFactor;
   ProcessDownsampled(FHighpass.ProcessSample(LowpassedSignal));
  end;
end;

procedure TCustomDownsampledTuner.SetDownsampleBW(Value: Single);
begin
 Value := Limit(Value, 0, 1);
 if Value <> FDownsampleBW then
  begin
   FDownsampleBW := Value;
   CalculateDownsampleFactor;
  end;
end;

procedure TCustomDownsampledTuner.SetDSFilterOrder(const Value: Cardinal);
begin
 if FLowpass.Order <> Value then
  begin
   FLowpass.Order := Value;
   CalculateDownsampleFactor;
  end;
end;

procedure TCustomDownsampledTuner.SetMaximumFrequency(const Value: Single);
begin
 if FMaximumFrequency <> Value then
  begin
   FMaximumFrequency := Value;
   MaximumFrequencyChanged;
  end;
end;

procedure TCustomDownsampledTuner.SetMinimumFrequency(const Value: Single);
begin
 if FMinimumFrequency <> Value then
  begin
   FMinimumFrequency := Value;
   MinimumFrequencyChanged;
  end;
end;

{ TCustomZeroCrossingTuner }

constructor TCustomZeroCrossingTuner.Create;
begin
 inherited;
 FOneCrossingOnly := True;
 FFrequencyFactor := 1;
 FSmoothFactor    := 0.99;
 FAverageSamples  := FFrequencyFactor * SampleRate / (DownSampleFilterOrder * 440);
end;

function TCustomZeroCrossingTuner.GetCurrentFrequency: Single;
begin
 {$IFDEF OnlineFreqCalc}
 result := FCurrentFreq;
 {$ELSE}
 result := CalculateCurrentFrequency;
 {$ENDIF}
end;

procedure TCustomZeroCrossingTuner.ProcessDownsampled(Downsampled: Single);
begin
 if (Downsampled < 0) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * FSamples;
   FSamples := 1;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
end;

function TCustomZeroCrossingTuner.CalculateCurrentFrequency: Single;
begin
 result := FFrequencyFactor * FSampleRate / (FDownSampleFactor * FAverageSamples);
end;

procedure TCustomZeroCrossingTuner.SetOneCrossingOnly(const Value: Boolean);
begin
 if FOneCrossingOnly <> Value then
  begin
   FOneCrossingOnly := Value;
   if FOneCrossingOnly
    then FFrequencyFactor := 1
    else FFrequencyFactor := 0.5;
  end;
end;

procedure TCustomZeroCrossingTuner.SetSmoothFactor(const Value: Single);
begin
 if FSmoothFactor <> Value then
  begin
   FSmoothFactor := Value;
   SmoothFactorChanged;
  end;
end;

procedure TCustomZeroCrossingTuner.SmoothFactorChanged;
begin
// FSmoothFactor := exp( -ln2 / (FSmooth * 0.001 * SampleRate));
end;


{ TCustomLinearZeroCrossingTuner }

procedure TCustomLinearZeroCrossingTuner.ProcessDownsampled(
  Downsampled: Single);
var
  Offset : Single;
begin
 if (Downsampled < 0) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
 FLastSample := Downsampled;
end;

{ TCustomAdvancedTuner }

function TCustomAdvancedTuner.CalculateCurrentFrequency: Single;
var
  CurrentNote : Single;
begin
 result := inherited CalculateCurrentFrequency;

 CurrentNote := 12 * FastLog2ContinousError4(FCurrentFreq / 440);
 while CurrentNote < -6 do CurrentNote := CurrentNote + 12;
 while CurrentNote >  6 do CurrentNote := CurrentNote - 12;

 case round(CurrentNote) of
  -6, 6 : FCurrentNote := 'Eb';
  -5    : FCurrentNote := 'E';
  -4    : FCurrentNote := 'F';
  -3    : FCurrentNote := 'F#';
  -2    : FCurrentNote := 'G';
  -1    : FCurrentNote := 'G#';
   0    : FCurrentNote := 'A';
   1    : FCurrentNote := 'Bb';
   2    : FCurrentNote := 'B';
   3    : FCurrentNote := 'C';
   4    : FCurrentNote := 'C#';
   5    : FCurrentNote := 'D';
 end;

 FCurrentDetune := 100 * CurrentNote - round(CurrentNote);

 FCurrentFreq := 440 * FastPower2ContinousError3(CurrentNote * COneTwelfth32);
end;

procedure TCustomAdvancedTuner.CalculateDownsampleFactor;
begin
 inherited;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomAdvancedTuner.SetAttack(const Value: Single);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomAdvancedTuner.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomAdvancedTuner.SetThreshold(const Value: Single);
begin
 FThreshold := Limit(Value, -1, 1);
end;

procedure TCustomAdvancedTuner.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomAdvancedTuner.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomAdvancedTuner.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate / FDownSampleFactor));
end;

procedure TCustomAdvancedTuner.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate / FDownSampleFactor));
end;

constructor TCustomAdvancedTuner.Create;
begin
 inherited;
 FAttack := 1;
 FRelease := 10;
 FThreshold := 0;
end;

procedure TCustomAdvancedTuner.ProcessDownsampled(Downsampled: Single);
var
  Offset : Single;
begin
 if abs(Downsampled) > FLevel
  then FLevel := FLevel + (abs(Downsampled) - FLevel) * FAttackFactor
  else FLevel := abs(Downsampled) + (FLevel - abs(Downsampled)) * FReleaseFactor;

 if (Downsampled < FThreshold * FLevel) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := CalculateCurrentFrequency;
   {$ENDIF}
  end
 else inc(FSamples);
 FLastSample := Downsampled;
end;

end.
