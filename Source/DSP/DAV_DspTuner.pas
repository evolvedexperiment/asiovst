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
    FMaximumFrequency: Single;
    FMinimumFrequency: Single;
    function GetDSFilterOrder: Cardinal;
    procedure SetDSFilterOrder(const Value: Cardinal);
    procedure CalculateDownsampleFactor;
    procedure SetMaximumFrequency(const Value: Single);
    procedure SetMinimumFrequency(const Value: Single);
    procedure SetupMaximumFrequency;
  protected
    FLowpass           : TButterworthLowPassFilter;
    FHighpass          : TButterworthHighPassFilter;
    FDownSampleFactor  : Integer;
    FDownSampleCounter : Integer;
    procedure ProcessDownsampled(DownSampled: Single); virtual; abstract;
    procedure SampleRateChanged; override;
    procedure MaximumFrequencyChanged; virtual;
    procedure MinimumFrequencyChanged; virtual;
  public
    constructor Create; override;
    procedure Process(Input: Single); override;

    property DownSampleFilterOrder: Cardinal read GetDSFilterOrder write SetDSFilterOrder;
    property MaximumFrequency: Single read FMaximumFrequency write SetMaximumFrequency;
    property MinimumFrequency: Single read FMinimumFrequency write SetMinimumFrequency;
  end;

  TCustomZeroCrossingTuner = class(TCustomDownsampledTuner)
  private
    FSmoothFactor   : Single;
    procedure SetSmoothFactor(const Value: Single);
  protected
    FIsAbove        : Boolean;
    FSamples        : Integer;
    FAverageSamples : Single;
    {$IFDEF OnlineFreqCalc}
    FCurrentFreq    : Single;
    {$ENDIF}
    function GetCurrentFrequency: Single; override;
    procedure SmoothFactorChanged; virtual;
    procedure ProcessDownsampled(Downsampled: Single); override;
  public
    constructor Create; override;
    property SmoothFactor: Single read FSmoothFactor write SetSmoothFactor;
  end;

  TCustomLinearZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  protected
    FLastSample : Single;
    FLastOffset : Single;
    procedure ProcessDownsampled(Downsampled: Single); override;
  end;

  TZeroCrossingTuner = class(TCustomZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
  end;

  TLinearZeroCrossingTuner = class(TCustomLinearZeroCrossingTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
  end;

  TTuner = class(TLinearZeroCrossingTuner);

implementation

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
 while 0.4 * CurrentNyquist > MaximumFrequency do
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
 FSmoothFactor := 0.99;
 FAverageSamples := 0.5 * SampleRate / (DownSampleFilterOrder * 440);
end;

function TCustomZeroCrossingTuner.GetCurrentFrequency: Single;
begin
 {$IFDEF OnlineFreqCalc}
 result := FCurrentFreq;
 {$ELSE}
 result := 0.5 * FSampleRate / (FDownSampleFactor * FAverageSamples);
 {$ENDIF}
end;

procedure TCustomZeroCrossingTuner.ProcessDownsampled(Downsampled: Single);
begin
 if (Downsampled < 0) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * FSamples;
   FSamples := 1;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := 0.5 * FSampleRate / (FDownSampleFactor * FAverageSamples);
   {$ENDIF}
  end
 else inc(FSamples);
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

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := FSmoothFactor * FAverageSamples +
     (1 - FSmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   {$IFDEF OnlineFreqCalc}
   FCurrentFreq := 0.5 * FSampleRate / (FDownSampleFactor * FAverageSamples);
   {$ENDIF}
  end
 else inc(FSamples);
 FLastSample := Downsampled; 
end;

end.
