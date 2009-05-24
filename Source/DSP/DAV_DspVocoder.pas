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

  CBarkFrequencyScale: array [0..23] of Single = (100, 200, 300, 400, 510,
    630, 770, 920, 1080, 1270, 1480, 1720, 2000, 2320, 2700, 3150, 3700, 4400,
    5300, 6400, 7700, 9500, 12000, 15500);

type
  TCustomVocoder = class(TDspSampleRateDependent)
  private
    procedure SetInputLevel(const Value: Double);
    procedure SetSynthLevel(const Value: Double);
    procedure SetVocoderLevel(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
  public
    FVolFactors        : array [0..2] of Double;
    FAttack            : Double;
    FAttackFactor      : Double;
    FRelease           : Double;
    FReleaseFactor     : Double;
    FSampleRate        : Double;
    procedure SampleRateChanged; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
  public
    constructor Create; virtual;

    function Process(Input, Carrier: Single): Single; virtual; abstract;

    property InputLevel: Double read FVolFactors[0] write SetInputLevel;
    property SynthLevel: Double read FVolFactors[1] write SetSynthLevel;
    property VocoderLevel: Double read FVolFactors[2] write SetVocoderLevel;
    property Attack: Double read FAttack write SetAttack;    // in ms
    property Release: Double read FRelease write SetRelease; // in ms
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

  TSimpleThirdOctaveVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    procedure SetSynthesisBW(const Value: Double);
  protected
    FAnalysisPeak      : array [0..cNumFrequencies - 1] of Single;
    FAnalysisFilters   : array [0..cNumFrequencies - 1] of TBasicBandpassFilter;
    FSynthesisFilters  : array [0..cNumFrequencies - 1] of TBasicBandpassFilter;
    procedure SampleRateChanged; override;
    procedure SynthesisBandwidthChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

  TBarkScaleVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    FAnalysisOrder: Integer;
    procedure SetSynthesisBW(const Value: Double);
    procedure SetAnalysisOrder(Value: Integer);
  protected
    FAnalysisFiltersLP   : array [0..23] of TChebyshev1LowpassFilter;
    FAnalysisFiltersHP   : array [0..23] of TChebyshev1HighpassFilter;
    FAnalysisPeak        : array [0..23] of Single;
    FSynthesisFiltersLP  : array [0..23] of TChebyshev1LowpassFilter;
    FSynthesisFiltersHP  : array [0..23] of TChebyshev1HighpassFilter;
    FDownSampler         : Integer;
    FDownSampleMax       : Integer;
    FTransitionBandwidth : Single;
    procedure SampleRateChanged; override;
    procedure AnalysisOrderChanged;
    procedure SynthesisBandwidthChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
    property AnalysisOrder: Integer read FAnalysisOrder write SetAnalysisOrder;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

  TVocoder = class(TCustomVocoder)
  private
    FSynthesisBW: Double;
    FAnalysisOrder: Integer;
    procedure SetSynthesisBW(const Value: Double);
    procedure SetAnalysisOrder(Value: Integer);
  protected
    FAnalysisFiltersLP : array [0..cNumFrequencies - 1] of TChebyshev1LowpassFilter;
    FAnalysisFiltersHP : array [0..cNumFrequencies - 1] of TChebyshev1HighpassFilter;
    FAnalysisPeak      : array [0..cNumFrequencies - 1] of Single;
    FSynthesisFilters  : array [0..cNumFrequencies - 1] of TBasicBandpassFilter;
    FDownSampler       : Integer;
    FDownSampleMax     : Integer;
    procedure SampleRateChanged; override;
    procedure AnalysisOrderChanged;
    procedure SynthesisBandwidthChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input, Carrier: Single): Single; override;

    property SynthesisBandwidth: Double read FSynthesisBW write SetSynthesisBW;
    property AnalysisOrder: Integer read FAnalysisOrder write SetAnalysisOrder;
  published
    property InputLevel;
    property SynthLevel;
    property VocoderLevel;
    property Attack;       // in ms
    property Release;      // in ms
    property SampleRate;
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations;

{ TCustomVocoder }

constructor TCustomVocoder.Create;
begin
 inherited;
 FSampleRate := 44100;
 FAttack := 0.5;
 FRelease := 2;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomVocoder.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomVocoder.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomVocoder.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := 1 - FastPower2MinError3(-1 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomVocoder.CalculateReleaseFactor;
begin
 if FRelease = 0
  then FReleaseFactor := 0
  else FReleaseFactor := FastPower2MinError3(-1 / (FRelease * 0.001 * SampleRate));
end;

procedure TCustomVocoder.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomVocoder.SetAttack(const Value: Double);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomVocoder.SetRelease(const Value: Double);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomVocoder.SetInputLevel(const Value: Double);
begin
 if FVolFactors[0] <> Value
  then FVolFactors[0] := Value;
end;

procedure TCustomVocoder.SetSynthLevel(const Value: Double);
begin
 if FVolFactors[1] <> Value
  then FVolFactors[1] := Value;
end;

procedure TCustomVocoder.SetVocoderLevel(const Value: Double);
begin
 if FVolFactors[2] <> Value
  then FVolFactors[2] := Value;
end;

procedure TCustomVocoder.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TSimpleThirdOctaveVocoder }

constructor TSimpleThirdOctaveVocoder.Create;
var
  i: Integer;
const
  HalfThirdOctaveMulFak64: Double = 1.0905077326652576592070106557607;
begin
  inherited;

  for i := 0 to CNumFrequencies - 1 do
   begin
    FAnalysisFilters[i] := TBasicBandpassFilter.Create;
    with FAnalysisFilters[i] do
     begin
      SampleRate := Self.SampleRate;
      Gain := 0; Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;

    FSynthesisFilters[i] := TBasicBandpassFilter.Create;
    with FSynthesisFilters[i] do
     begin
      SampleRate := Self.SampleRate;
      Gain := 0; Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;
   end;
end;

destructor TSimpleThirdOctaveVocoder.Destroy;
var
  i: Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FAnalysisFilters[i]);
   FreeAndNil(FSynthesisFilters[i]);
  end;
 inherited;
end;

function TSimpleThirdOctaveVocoder.Process(Input, Carrier: Single): Single;
var
  Band       : Integer;
  BandSignal : Double;
begin
 for Band := 0 to CNumFrequencies - 1 do
  begin
   BandSignal := FAnalysisFilters[Band].ProcessSample(Input + 1E-32);

   if abs(BandSignal) > FAnalysisPeak[Band]
    then FAnalysisPeak[Band] := FAnalysisPeak[Band] + (abs(BandSignal) - FAnalysisPeak[Band]) * FAttackFactor
    else FAnalysisPeak[Band] := abs(BandSignal) + (FAnalysisPeak[Band] - abs(BandSignal)) * FReleaseFactor;
  end;

 // process vocoded signal
 result := 0;
 for Band := 0 to CNumFrequencies - 1
  do result := result + FSynthesisFilters[Band].ProcessSample(FAnalysisPeak[Band] * Carrier);

 result := FVolFactors[2] * result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TSimpleThirdOctaveVocoder.SampleRateChanged;
var
  Band : Integer;
begin
 inherited;

 for Band := 0 to Length(FAnalysisFilters) - 1
  do FAnalysisFilters[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFilters) - 1
  do FSynthesisFilters[Band].SampleRate := SampleRate;
end;

procedure TSimpleThirdOctaveVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TSimpleThirdOctaveVocoder.SynthesisBandwidthChanged;
var
  Band: Integer;
begin
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   FSynthesisFilters[Band].Bandwidth := FSynthesisBW;
  end;
end;

{ TBarkScaleVocoder }

constructor TBarkScaleVocoder.Create;
var
  Band : Integer;
  DS   : Integer;
begin
  FDownSampler := 0;
  FTransitionBandwidth := 0.1;
  DS := 0;
  for Band := 0 to Length(CBarkFrequencyScale) - 1 do
   begin
    // create filters
    FAnalysisFiltersLP[Band] := TChebyshev1LowpassFilter.Create(4);
    FAnalysisFiltersHP[Band] := TChebyshev1HighpassFilter.Create(4);
    FSynthesisFiltersLP[Band] := TChebyshev1LowpassFilter.Create(4);
    FSynthesisFiltersHP[Band] := TChebyshev1HighpassFilter.Create(4);

    if FDownSampler >= 0 then
     while IntPower(2, DS) * CBarkFrequencyScale[Band] < FTransitionBandwidth * SampleRate
      do DS := DS + 1;

    // setup filters
    with FAnalysisFiltersLP[Band] do
     begin
      SampleRate := Self.SampleRate;
      SetFilterValues(CBarkFrequencyScale[Band] , 0, 0.1);
      DownsampleAmount := DS;
      CalculateCoefficients;
     end;

    with FAnalysisFiltersHP[Band] do
     begin
      SampleRate := Self.SampleRate;
      SetFilterValues(CBarkFrequencyScale[Band], 0, 0.1);
      DownsampleAmount := DS;
      CalculateCoefficients;
     end;

    with FAnalysisFiltersLP[Band] do
     begin
      SampleRate := Self.SampleRate;
      SetFilterValues(CBarkFrequencyScale[Band] , 0, 0.1);
      DownsampleAmount := DS;
      CalculateCoefficients;
     end;

    with FAnalysisFiltersHP[Band] do
     begin
      SampleRate := Self.SampleRate;
      SetFilterValues(CBarkFrequencyScale[Band], 0, 0.1);
      DownsampleAmount := DS;
      CalculateCoefficients;
     end;
   end;

  FDownSampleMax := FAnalysisFiltersLP[Length(CBarkFrequencyScale) - 1].DownsampleFaktor;

  inherited Create;
end;

destructor TBarkScaleVocoder.Destroy;
var
  i: Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FAnalysisFiltersLP[i]);
   FreeAndNil(FAnalysisFiltersHP[i]);
   FreeAndNil(FSynthesisFiltersLP[i]);
   FreeAndNil(FSynthesisFiltersHP[i]);
  end;
 inherited;
end;

function TBarkScaleVocoder.Process(Input, Carrier: Single): Single;
var
  i, j       : Integer;
  Lowpassed  : Double;
  BandSignal : Double;
begin
 Lowpassed := Input;
 for j := 0 to CNumFrequencies - 1 do
  begin
   if (FDownSampler mod FAnalysisFiltersLP[j].DownsampleFaktor) <> 0
    then Break;

   BandSignal := FAnalysisFiltersHP[j].ProcessSample(Lowpassed + 1E-32);

   if abs(BandSignal) > FAnalysisPeak[j]
    then FAnalysisPeak[j] := FAnalysisPeak[j] + (abs(BandSignal) - FAnalysisPeak[j]) * FAttackFactor
    else FAnalysisPeak[j] := abs(BandSignal) + (FAnalysisPeak[j] - abs(BandSignal)) * FReleaseFactor;

   Lowpassed := FAnalysisFiltersLP[j].ProcessSample(Lowpassed + 1E-32);
  end;

 Inc(FDownSampler);
 if FDownSampler >= FDownSampleMax
  then FDownSampler := 0;

(*
 // process vocoded signal
 result := 0;
 for i := 0 to CNumFrequencies - 1
  do result := result + FSynthesisFilters[i].ProcessSample(FAnalysisPeak[i] * Carrier);
*)

 result := FVolFactors[2] * result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TBarkScaleVocoder.SampleRateChanged;
var
  Band : Integer;
begin
 inherited;

 for Band := 0 to Length(FAnalysisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FAnalysisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].SampleRate := SampleRate;
end;

procedure TBarkScaleVocoder.SetAnalysisOrder(Value: Integer);
begin
 if Value < 2
  then Value := 2
  else Value := ((Value shr 1) shl 1);
 if FAnalysisOrder <> Value then
  begin
   FAnalysisOrder := Value;
   AnalysisOrderChanged;
  end;
end;

procedure TBarkScaleVocoder.AnalysisOrderChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FAnalysisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].Order := FAnalysisOrder shr 1;

 for Band := 0 to Length(FAnalysisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].Order := FAnalysisOrder shr 1;
end;

procedure TBarkScaleVocoder.SetSynthesisBW(const Value: Double);
begin
 if FSynthesisBW <> Value then
  begin
   FSynthesisBW := Value;
   SynthesisBandwidthChanged;
  end;
end;

procedure TBarkScaleVocoder.SynthesisBandwidthChanged;
var
  Band: Integer;
begin
(*
 for Band := 0 to Length(FSynthesisFilters) - 1 do
  begin
   // ToDo
  end;
*)
end;

{ TVocoder }

constructor TVocoder.Create;
var
  i: Integer;
const
  HalfThirdOctaveMulFak64: Double = 1.0905077326652576592070106557607;
begin
  FDownSampler := 0;
  for i := 0 to CNumFrequencies - 1 do
   begin
    FAnalysisFiltersLP[i] := TChebyshev1LowpassFilter.Create(6);
    with FAnalysisFiltersLP[i] do
     begin
      SampleRate := Self.SampleRate;
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
      SampleRate := Self.SampleRate;
      SetFilterValues((CThirdOctaveFrequencies[CNumFrequencies - i - 1]) / HalfThirdOctaveMulFak64, 0, 0.1);
      DownsampleAmount := FAnalysisFiltersLP[i].DownsampleAmount;
      CalculateCoefficients;
     end;

    FSynthesisFilters[i] := TBasicBandpassFilter.Create;
    with FSynthesisFilters[i] do
     begin
      SampleRate := Self.SampleRate;
      Gain := 0;
      Bandwidth := 0.707;
      Frequency := CThirdOctaveFrequencies[CNumFrequencies - i - 1];
     end;
   end;

  FDownSampleMax := FAnalysisFiltersLP[CNumFrequencies - 1].DownsampleFaktor;

  inherited Create;
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
begin
 Lowpassed := Input;
 for j := 0 to CNumFrequencies - 1 do
  begin
   if (FDownSampler mod FAnalysisFiltersLP[j].DownsampleFaktor) <> 0
    then Break;

   Lowpassed := FAnalysisFiltersLP[j].ProcessSample(Lowpassed + 1E-32);
   BandSignal := FAnalysisFiltersHP[j].ProcessSample(Lowpassed + 1E-32);

   if abs(BandSignal) > FAnalysisPeak[j]
    then FAnalysisPeak[j] := FAnalysisPeak[j] + (abs(BandSignal) - FAnalysisPeak[j]) * FAttackFactor
    else FAnalysisPeak[j] := abs(BandSignal) + (FAnalysisPeak[j] - abs(BandSignal)) * FReleaseFactor;
  end;
 Inc(FDownSampler);
 if FDownSampler >= FDownSampleMax
  then FDownSampler := 0;

 // process vocoded signal
 result := 0;
 for i := 0 to CNumFrequencies - 1
  do result := result + FSynthesisFilters[i].ProcessSample(FAnalysisPeak[i] * Carrier);

 result := FVolFactors[2] * result +
           FVolFactors[1] * Carrier +
           FVolFactors[0] * Input;
end;

procedure TVocoder.SampleRateChanged;
var
  Band : Integer;
begin
 inherited;

 for Band := 0 to Length(FAnalysisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FAnalysisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].SampleRate := SampleRate;

 for Band := 0 to Length(FSynthesisFilters) - 1
  do FSynthesisFilters[Band].SampleRate := SampleRate;
end;

procedure TVocoder.SetAnalysisOrder(Value: Integer);
begin
 if Value < 2
  then Value := 2
  else Value := ((Value shr 1) shl 1);
 if FAnalysisOrder <> Value then
  begin
   FAnalysisOrder := Value;
   AnalysisOrderChanged;
  end;
end;

procedure TVocoder.AnalysisOrderChanged;
var
  Band : Integer;
begin
 for Band := 0 to Length(FAnalysisFiltersLP) - 1
  do FAnalysisFiltersLP[Band].Order := FAnalysisOrder shr 1;

 for Band := 0 to Length(FAnalysisFiltersHP) - 1
  do FAnalysisFiltersHP[Band].Order := FAnalysisOrder shr 1;
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

end.
