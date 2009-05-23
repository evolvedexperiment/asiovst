unit DAV_DspTuner;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspButterworthFilter, DAV_DspCorrelation;

type
  TCustomTuner = class(TDspObject)
  private
    procedure SetSampleRate(const Value: Single);
  protected
    FSampleRate: Single;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    procedure Process(Input: Single); virtual; abstract;
    property SampleRate: Single read FSampleRate write SetSampleRate;
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
  protected
    FLowpass           : TButterworthLowPassFilter;
    FHighpass          : TButterworthHighPassFilter;
    FDownsampled       : Single;
    FDownSampleFactor  : Integer;
    FDownSampleCounter : Integer;
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

  TTuner = class(TCustomDownsampledTuner)
  published
    property SampleRate;
    property DownSampleFilterOrder;
  end;

implementation

{ TCustomTuner }

constructor TCustomTuner.Create;
begin
 // nothing in here yet!
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
 inherited;
 FLowpass  := TButterworthLowPassFilter.Create(4);
 FHighpass := TButterworthHighPassFilter.Create(2);
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
   inc(NewFactor);
  end;
 FDownSampleFactor := NewFactor;
end;

function TCustomDownsampledTuner.GetDSFilterOrder: Cardinal;
begin
 result := FLowpass.Order;
end;

procedure TCustomDownsampledTuner.MaximumFrequencyChanged;
begin
 FLowpass.Frequency := FMaximumFrequency;
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
   FDownsampled := FHighpass.ProcessSample(LowpassedSignal);
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

end.
