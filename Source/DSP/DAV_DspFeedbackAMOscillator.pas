unit DAV_DspFeedbackAMOscillator;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspLFO, DAV_DspFilter, DAV_DspDelayLines;

type
  TCustomFeedbackAMOscillator = class(TDspObject)
  private
    FSampleRate : Single;
    FAmplitude  : Single;
    FFrequency  : Single;
    FFeedback   : Single;
    FState      : Single;
    procedure SetAmplitude(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetSampleRate(const Value: Single);
    procedure SetFeedback(const Value: Single);
    procedure FeedbackChanged;
  protected
    FOscillator : TLFOSine32;
    procedure AmplitudeChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Amplitude: Single read FAmplitude write SetAmplitude;
    property Frequency: Single read FFrequency write SetFrequency;
    property Feedback: Single read FFeedback write SetFeedback;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TFeedbackAMOscillator = class(TCustomFeedbackAMOscillator)
  public
    constructor Create; override;
    function ProcessSample: Single;
  published
    property Amplitude;
    property Frequency;
    property Feedback;
    property SampleRate;
  end;

  TDelayedFeedbackAMOscillator = class(TCustomFeedbackAMOscillator)
  private
    FDelayLine : TCustomDelayLineSamples32;
  public
    constructor Create; override;
    function ProcessSample: Single;
  published
    property Amplitude;
    property Frequency;
    property Feedback;
    property SampleRate;
  end;

implementation

uses
  SysUtils;

{ TCustomFeedbackAMOscillator }

constructor TCustomFeedbackAMOscillator.Create;
begin
 inherited;
 FSampleRate := 44100;
 FOscillator := TLFOSine32.Create;
 with FOscillator do
  begin
   Amplitude := 0.5;
   SampleRate := Self.SampleRate;
  end;
 FFeedback  := 1;
 FAmplitude := 1;
end;

destructor TCustomFeedbackAMOscillator.Destroy;
begin
 FreeAndNil(FOscillator);
 inherited;
end;

procedure TCustomFeedbackAMOscillator.SetAmplitude(const Value: Single);
begin
 if FAmplitude <> Value then
  begin
   FAmplitude := Value;
   AmplitudeChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.SetFeedback(const Value: Single);
begin
 if FFeedback <> Value then
  begin
   FFeedback := Value;
   FeedbackChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomFeedbackAMOscillator.AmplitudeChanged;
begin
// nothing here yet!
end;

procedure TCustomFeedbackAMOscillator.FeedbackChanged;
begin
// nothing here yet!
end;

procedure TCustomFeedbackAMOscillator.FrequencyChanged;
begin
 FOscillator.Frequency := Frequency;
end;

procedure TCustomFeedbackAMOscillator.SampleRateChanged;
begin
 FOscillator.SampleRate := SampleRate;
end;


{ TFeedbackAMOscillator }

constructor TFeedbackAMOscillator.Create;
begin
 inherited;
 FState := 0;
end;

function TFeedbackAMOscillator.ProcessSample: Single;
begin
(*
 Result := FState * FOscillator.Sine;
 FState := CHalf32 + FFeedback * Result;
 FOscillator.CalculateNextSample;
 Result := FScale * Result;
*)
 Result := FOscillator.Sine * (1 + FFeedback * FState);
 FState := Result;
 FOscillator.CalculateNextSample;
 Result := FAmplitude * Result;
end;

{ TDelayedFeedbackAMOscillator }

constructor TDelayedFeedbackAMOscillator.Create;
begin
 inherited;
 FDelayLine := TCustomDelayLineSamples32.Create(128);
end;

function TDelayedFeedbackAMOscillator.ProcessSample: Single;
begin
 Result := FOscillator.Sine * (1 + FFeedback * FState);
 FState := FDelayLine.ProcessSample(Result);
 FOscillator.CalculateNextSample;
 Result := FAmplitude * Result;
end;

end.
