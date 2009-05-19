unit DAV_DspVocoder;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspChebyshevFilter, DAV_DspFilter,
  DAV_DspFilterBasics;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies: array [0..cNumFrequencies - 1] of Single =
    (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
    630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
    10000, 12500, 16000, 20000);

type
  TVocoder = class(TDspSampleRateDependent)
  private
    FSynthesisBW: Double;
    procedure SetInputLevel(const Value: Double);
    procedure SetSynthLevel(const Value: Double);
    procedure SetVocoderLevel(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetSynthesisBW(const Value: Double);
  public
    FAnalysisFiltersLP : array [0..cNumFrequencies - 1] of TChebyshev1LowpassFilter;
    FAnalysisFiltersHP : array [0..cNumFrequencies - 1] of TChebyshev1HighpassFilter;
    FAnalysisRMS       : array [0..cNumFrequencies - 1] of Single;
    FSynthesisFilters  : array [0..cNumFrequencies - 1] of TBasicBandpassFilter;
    FDownSampler       : Integer;
    FDownSampleMax     : Integer;
    FVolFactors        : array [0..2] of Double;
    FAttack            : Double;
    FAttackFactor      : Double;
    FRelease           : Double;
    FReleaseFactor     : Double;
    FSampleRate        : Double;
    procedure SampleRateChanged; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure SynthesisBandwidthChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Process(Input, Carrier: Single): Single;

    property InputLevel: Double read FVolFactors[0] write SetInputLevel;
    property SynthLevel: Double read FVolFactors[1] write SetSynthLevel;
    property VocoderLevel: Double read FVolFactors[2] write SetVocoderLevel;
    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
    property Attack: Double read FAttack write SetAttack;    // in ms
    property Release: Double read FRelease write SetRelease; // in ms
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations;

{ TVocoder }

constructor TVocoder.Create;
var
  i: Integer;
const
  HalfThirdOctaveMulFak64: Double = 1.0905077326652576592070106557607;
begin
  FSampleRate := 44100;

  FDownSampler := 0;
  for i := 0 to CNumFrequencies - 1 do
   begin
    FAnalysisFiltersLP[i] := TChebyshev1LowpassFilter.Create(6);
    with FAnalysisFiltersLP[i] do
     begin
      SampleRate := 44100;
      SetFilterValues(min(0.5 * Samplerate, HalfThirdOctaveMulFak64 * (CThirdOctaveFrequencies[CNumFrequencies - i - 1])), 0, 0.1);
      if FDownSampler = -1
       then DownsampleAmount := 0
       else while IntPower(2, DownsampleAmount) * Frequency < 0.1 * SampleRate
        do DownsampleAmount := DownsampleAmount + 1;
      CalculateCoefficients;
     end;

    FAnalysisFiltersHP[i] := TChebyshev1HighpassFilter.Create(6);
    with FAnalysisFiltersHP[i] do
     begin
      SampleRate := 44100;
      SetFilterValues((CThirdOctaveFrequencies[CNumFrequencies - i - 1]) / HalfThirdOctaveMulFak64, 0, 0.1);
      DownsampleAmount := FAnalysisFiltersLP[i].DownsampleAmount;
      CalculateCoefficients;
     end;

    FSynthesisFilters[i] := TBasicBandpassFilter.Create;
    with FSynthesisFilters[i] do
     begin
      SampleRate := 44100;
      Gain := 0;
      Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;
   end;

  FDownSampleMax := FAnalysisFiltersLP[CNumFrequencies - 1].DownsampleFaktor;

  FAttack := 0.5;
  FRelease := 2;
  CalculateAttackFactor;
  CalculateReleaseFactor;
end;

destructor TVocoder.Destroy;
var
  i: Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FAnalysisFiltersLP[i]);
   FreeAndNil(FAnalysisFiltersHP[i]);
   FreeAndNil(FSynthesisFilters[i]);
  end;
 inherited;
end;

function TVocoder.Process(Input, Carrier: Single): Single;
var
  i, j       : Integer;
  Lowpassed  : Double;
  BandSignal : Double;
  Smooth     : Double;
begin
 Lowpassed := Input;
 for j := 0 to CNumFrequencies - 1 do
  begin
   if (FDownSampler mod FAnalysisFiltersLP[j].DownsampleFaktor) <> 0
    then Break;

   Lowpassed := FAnalysisFiltersLP[j].ProcessSample(Lowpassed + 1E-32);
   BandSignal := FAnalysisFiltersHP[j].ProcessSample(Lowpassed + 1E-32);

   if abs(BandSignal) > FAnalysisRMS[j]
    then FAnalysisRMS[j] := FAnalysisRMS[j] + (abs(BandSignal) - FAnalysisRMS[j]) * FAttackFactor
    else FAnalysisRMS[j] := abs(BandSignal) + (FAnalysisRMS[j] - abs(BandSignal)) * FReleaseFactor;
  end;
 Inc(FDownSampler);
 if FDownSampler >= FDownSampleMax
  then FDownSampler := 0;

 // process vocoded signal
 result := 0;
 for i := 0 to CNumFrequencies - 1
  do result := result + FSynthesisFilters[i].ProcessSample(FAnalysisRMS[i] * Carrier);

 result := FVolFactors[2] * result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TVocoder.SampleRateChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FAnalysisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FAnalysisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFilters) - 1
  do FSynthesisFilters[Band].SampleRate := SampleRate;

 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TVocoder.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TVocoder.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TVocoder.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TVocoder.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TVocoder.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate));
end;

procedure TVocoder.CalculateReleaseFactor;
begin
 if FRelease = 0
  then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TVocoder.SetInputLevel(const Value: Double);
begin
 if FVolFactors[0] <> Value
  then FVolFactors[0] := Value;
end;

procedure TVocoder.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TVocoder.SynthesisBandwidthChanged;
var
  Band: Integer;
begin
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   FSynthesisFilters[Band].Bandwidth := FSynthesisBW;
  end;
end;

procedure TVocoder.SetSynthLevel(const Value: Double);
begin
 if FVolFactors[1] <> Value
  then FVolFactors[1] := Value;
end;

procedure TVocoder.SetVocoderLevel(const Value: Double);
begin
 if FVolFactors[2] <> Value
  then FVolFactors[2] := Value;
end;

end.
