unit DAV_DspExciter;

interface

{$I ..\DAV_Compiler.inc}

uses 
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DSPFilterButterworth,
  DAV_DspFilterLinkwitzRiley, DAV_DspDynamics, DAV_DspLightweightDynamics;

type
  TCustomExciter = class(TDspObject)
  private
    FFrequency  : Single;
    FSampleRate : Single;
    FGains      : array [0..3] of Single;
    FCrossover  : TButterworthSplitBandFilter;
    FHighpass   : TButterworthHighPassFilter;

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
 FHighpass := TButterworthHighPassFilter.Create(3);
 FHighpass.SampleRate := SampleRate;
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
begin
 FCrossover.ProcessSample(FGains[0] * Input, Low, High);

 Harmonic := FHighpass.ProcessSample(0.25 * FastTanhMinError2(4 * Low));

 result := FGains[1] * Low + FGains[2] * High - FGains[3] * Harmonic;
end;

end.
