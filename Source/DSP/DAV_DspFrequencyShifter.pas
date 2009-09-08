unit DAV_DspFrequencyShifter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspLfo, DAV_DspPolyphaseHilbert;

type
  TCustomBodeFrequencyShifter = class(TDspObject)
  private
    FSampleRate       : Single;
    FFrequency        : Single;
    FCoefficientCount : Integer;
    FTransitionBandwidth: Single;
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
    procedure SetCoefficientCount(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Single);
  protected
    procedure CoefficientCountChanged; virtual; abstract;
    procedure FrequencyChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
    procedure TransitionBandwidthChanged; virtual; abstract;
  public
    constructor Create; virtual;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
    property CoefficientCount: Integer read FCoefficientCount write SetCoefficientCount;
    property TransitionBandwidth: Single read FTransitionBandwidth write SetTransitionBandwidth;
  end;

  TCustomBodeFrequencyShifter32 = class(TCustomBodeFrequencyShifter)
  private
    FLfo     : TLFOSine32;
    FHilbert : TPhaseHalfPi32;
  protected
    procedure CoefficientCountChanged; override;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
    procedure TransitionBandwidthChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSample(Input: Single; var Upshift, Downshift: Single); virtual;
  end;

  TBodeFrequencyShifter32 = class(TCustomBodeFrequencyShifter32)
  published
    property SampleRate;
    property Frequency;
  end;

implementation

uses
  SysUtils;

{ TCustomBodeFrequencyShifter }

constructor TCustomBodeFrequencyShifter.Create;
begin
 FSampleRate := 44100;
 FFrequency := 1000;
end;

procedure TCustomBodeFrequencyShifter.SetCoefficientCount(const Value: Integer);
begin
 if FCoefficientCount <> Value then
  begin
   FCoefficientCount := Value;
   CoefficientCountChanged;
  end;
end;

procedure TCustomBodeFrequencyShifter.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomBodeFrequencyShifter.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomBodeFrequencyShifter.SetTransitionBandwidth(
  const Value: Single);
begin
 if FTransitionBandwidth <> Value then
  begin
   FTransitionBandwidth := Value;
   TransitionBandwidthChanged;
  end;
end;

{ TCustomBodeFrequencyShifter32 }

constructor TCustomBodeFrequencyShifter32.Create;
begin
 inherited;
 FLfo := TLFOSine32.Create;
 with FLfo do
  begin
   SampleRate := FSampleRate;
   Frequency := FFrequency;
  end;
 FHilbert := TPhaseHalfPi32.Create;
 FHilbert.SetCoefficients(8, 0.1);
end;

destructor TCustomBodeFrequencyShifter32.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FHilbert);
 inherited;
end;

procedure TCustomBodeFrequencyShifter32.CoefficientCountChanged;
begin
 FHilbert.NumberOfCoefficients := FCoefficientCount;
end;

procedure TCustomBodeFrequencyShifter32.FrequencyChanged;
begin
 FLfo.Frequency := Frequency;
end;

procedure TCustomBodeFrequencyShifter32.SampleRateChanged;
begin
 FLfo.SampleRate := SampleRate;
end;

procedure TCustomBodeFrequencyShifter32.TransitionBandwidthChanged;
begin
 FHilbert.Transition := FTransitionBandwidth;
end;

procedure TCustomBodeFrequencyShifter32.ProcessSample(Input: Single; var Upshift, Downshift: Single);
var
  Cmplx : TComplexSingle;
const
  CSqrtHalf32 : Single = 0.70710678118654752440084436210485;
begin
 FHilbert.ProcessHilbertSample(Input, Cmplx.Re, Cmplx.Im);
 Cmplx.Im := FLfo.Sine * Cmplx.Im;
 Cmplx.Re := FLfo.Cosine * Cmplx.Re;
 FLfo.CalculateNextSample;
 Upshift   := (Cmplx.Re - Cmplx.Im) * CSqrtHalf32;
 Downshift := (Cmplx.Re + Cmplx.Im) * CSqrtHalf32;
end;

end.
