unit DAV_DspExciter;

interface

{$I ..\DAV_Compiler.inc}

uses 
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DSPFilterButterworth,
  DAV_DspFilterLinkwitzRiley, DAV_DspDynamics, DAV_DspLightweightDynamics,
  DAV_DspPolyphaseUpsampler, DAV_DspPolyphaseDownsampler;

type
  TCustomExciter = class(TDspObject)
  private
    FFrequency   : Single;
    FSampleRate  : Single;
    FGains       : array [0..3] of Single;
    FCrossover   : TButterworthSplitBandFilter;
    FHighpass    : TButterworthHighPassFilter;
    FUpsampler   : TPolyphaseUpsampler32;
    FDownsampler : TPolyphaseDownsampler32;

    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Process(Input: Single): Single; virtual;

    property InputLevel: Single read FGains[0] write FGains[0];
    property LowFrequencyLevel: Single read FGains[1] write FGains[1];
    property HighFrequencyLevel: Single read FGains[2] write FGains[2];
    property HarmonicsLevel: Single read FGains[3] write FGains[3];
    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TExciter = class(TCustomExciter)
  published
    property InputLevel;
    property LowFrequencyLevel;
    property HighFrequencyLevel;
    property HarmonicsLevel;
    property Frequency;
    property SampleRate;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

{ TCustomExciter }

constructor TCustomExciter.Create;
begin
 FSampleRate := 44100;
 FFrequency := 80;

 FGains[0] := 1;
 FGains[1] := 1;
 FGains[2] := 1;
 FGains[3] := 0;

 // create crossover filter
 FCrossover := TButterworthSplitBandFilter.Create(3);
 FCrossover.SampleRate := SampleRate;

 // create highpass filter
 FHighpass := TButterworthHighPassFilter.Create(1);
 FHighpass.SampleRate := SampleRate;

 // upsampler
 FUpsampler := TPolyphaseUpsampler32.Create;
 FUpsampler.NumberOfCoefficients := 6;

 // downsampler
 FDownsampler := TPolyphaseDownsampler32.Create;
 FDownsampler.NumberOfCoefficients := 6;
end;

destructor TCustomExciter.Destroy;
begin
 FreeAndNil(FCrossover);
 inherited;
end;

procedure TCustomExciter.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomExciter.FrequencyChanged;
begin
 FCrossover.Frequency := FFrequency;
 FHighpass.Frequency := 1.5 * FFrequency;
end;

procedure TCustomExciter.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomExciter.SampleRateChanged;
begin
 FCrossover.SampleRate := FSampleRate;
 FHighpass.SampleRate := FSampleRate;
end;

function TCustomExciter.Process(Input: Single): Single;
var
  Low, High, Harmonic : Single;
  Data : TDAV2SingleArray;
begin
 FCrossover.ProcessSample(FGains[0] * Input, Low, High);

 FUpsampler.ProcessSample(Low, Data);
 Data[0] := 0.125 * FastTanhMinError2(8 * Low);
 Data[1] := 0.125 * FastTanhMinError2(8 * Low);
 Harmonic := FHighpass.ProcessSample(FDownsampler.ProcessSample(Data));

 result := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
end;

end.
