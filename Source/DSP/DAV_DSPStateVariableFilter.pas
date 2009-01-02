unit DAV_DSPStateVariableFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon;

type
  TFrequencyTuningMethod = (ftmSimple, ftmIdeal);

  TSVF = class(TDspObject)
  private
    FQ1, FQ, FF1, FF      : Single;
    FSampleRate           : Single;
    FReciprocalSampleRate : Single;
    FDelay                : Array [0..1] of Double;
    //ftL, ftH              : Double;
    //ftB, ftN              : Double;
    FFTM                  : TFrequencyTuningMethod;
    procedure SetFrequency(Value: Single);
    procedure SetQ(Value: Single);
    procedure CalculateQ;
    procedure SetSampleRate(const Value: Single);
    procedure FrequencyChanged;
  public
    constructor Create;
    procedure Process(const Input: Single; var Low, Band, Notch, High: Single);
    procedure ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
    property Frequency: Single read FF write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Q: Single read FQ write SetQ;
    property FrequencyTuningMethod: TFrequencyTuningMethod read FFTM write FFTM;
  end;

implementation

uses SysUtils;

const
  kDenorm = 1.0e-32;

constructor TSVF.Create;
begin
  inherited;
  FQ1                   := 1;
  FF1                   := 1000;
  FSampleRate           := 44100;
  FReciprocalSampleRate := 1 / FSampleRate;
  FFTM                  := ftmIdeal;
end;

procedure TSVF.SetFrequency(Value: Single);
begin
  if FSampleRate <= 0
   then raise Exception.Create('Sample Rate Error!');
  if Value <> FF then
   begin
    FF := Value;
    FrequencyChanged;
   end;
end;

procedure TSVF.FrequencyChanged;
begin
 case FFTM of
   ftmSimple :
    begin
     // simple frequency tuning with error towards nyquist
     // F is the filter's center frequency, and Fs is the sampling rate
     if FF > 17000
      then FF1 := 2 * pi * 17000 * FReciprocalSampleRate
      else FF1 := 2 * pi * FF * FReciprocalSampleRate;
     CalculateQ;
    end;
   ftmIdeal :
    begin
     // ideal tuning:
     if FF > 17000
      then FF1 := 2 * sin(pi * 17000 * FReciprocalSampleRate)
      else FF1 := 2 * sin(pi * FF * FReciprocalSampleRate);
     CalculateQ;
    end;
  end;
end;

procedure TSVF.CalculateQ;
const
  CSpd: Double = 1 / 1200;
begin
  if FF > 5000
   then FQ1 := 1 / (FQ + ((FF - 5000) * CSpd))
   else FQ1 := 1 / FQ;
end;

procedure TSVF.SetQ(Value: Single);
begin
  if Value < 0.5 then Value := 0.5;
  if Value <> FQ then
   begin
    FQ := Value;
    CalculateQ;
   end;
end;

procedure TSVF.Process(const Input: Single; var Low, Band, Notch, High: Single);
begin
  Low := FDelay[1] + FF1 * FDelay[0];
  High := (Input + kDenorm) - Low - FQ1 * FDelay[0];
  Band := FF1 * High + FDelay[0];
  Notch := High + Low;
 // store delays
  FDelay[0] := Band;
  FDelay[1] := Low - kDenorm;
end;

procedure TSVF.ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
var
  c: Integer;
begin
  for c := 0 to SampleFrames - 1 do
   begin
    Low^[c] := FDelay[1] + FF1 * FDelay[0];
    High^[c] := (Input^[c] + kDenorm) - Low^[c] - FQ1 * FDelay[0] - kDenorm;
    Band^[c] := FF1 * High^[c] + FDelay[0];
    Notch^[c] := High^[c] + Low^[c];

    // store delays
    FDelay[0] := Band^[c];
    FDelay[1] := Low^[c] - kDenorm;
   end;
end;

procedure TSVF.SetSampleRate(const Value: Single);
begin
  if FSampleRate <> Value then
   begin
    FSampleRate := Value;
    FReciprocalSampleRate := 1 / FSampleRate;
   end;
end;

end.
