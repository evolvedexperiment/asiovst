unit DAV_DSPStateVariableFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon;

type
  TFrequencyTuningMethod = (ftmSimple, ftmIdeal);

  TSVF = class(TDspSampleRatePersistent)
  private
    FQ1, FQ, FF1, FF : Single;
    FSampleRateInv   : Single;
    FDelay           : Array [0..1] of Double;
    FFTM             : TFrequencyTuningMethod;
    procedure SetFrequency(Value: Single);
    procedure SetQ(Value: Single);
    procedure CalculateQ;
  protected
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; virtual;
    procedure QChanged; virtual;
  public
    constructor Create; override;
    procedure Process(const Input: Single; var Low, Band, Notch, High: Single);
    procedure ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
    property Frequency: Single read FF write SetFrequency;
    property SampleRate;
    property Q: Single read FQ write SetQ;
    property FrequencyTuningMethod: TFrequencyTuningMethod read FFTM write FFTM;
  end;

implementation

uses
  SysUtils;

constructor TSVF.Create;
begin
  inherited;
  FQ1            := 1;
  FF1            := 1000;
  FSampleRateInv := 1 / SampleRate;
  FFTM           := ftmIdeal;
end;

procedure TSVF.SampleRateChanged;
begin
 inherited;
 FSampleRateInv := 1 / SampleRate;
end;

procedure TSVF.SetFrequency(Value: Single);
begin
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
      then FF1 := 2 * pi * 17000 * FSampleRateInv
      else FF1 := 2 * pi * FF * FSampleRateInv;
     CalculateQ;
    end;
   ftmIdeal :
    begin
     // ideal tuning:
     if FF > 17000
      then FF1 := 2 * sin(pi * 17000 * FSampleRateInv)
      else FF1 := 2 * sin(pi * FF * FSampleRateInv);
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
    QChanged;

   end;
end;

procedure TSVF.QChanged;
begin
 CalculateQ;
end;

procedure TSVF.Process(const Input: Single; var Low, Band, Notch, High: Single);
begin
  Low := FDelay[1] + FF1 * FDelay[0];
  High := (Input + CDenorm32) - Low - FQ1 * FDelay[0];
  Band := FF1 * High + FDelay[0];
  Notch := High + Low;
 // store delays
  FDelay[0] := Band;
  FDelay[1] := Low - CDenorm32;
end;

procedure TSVF.ProcessBlock(Input, Low, Band, Notch, High: PDAVSingleFixedArray; SampleFrames: Integer);
var
  c: Integer;
begin
  for c := 0 to SampleFrames - 1 do
   begin
    Low^[c] := FDelay[1] + FF1 * FDelay[0];
    High^[c] := (Input^[c] + CDenorm32) - Low^[c] - FQ1 * FDelay[0] - CDenorm32;
    Band^[c] := FF1 * High^[c] + FDelay[0];
    Notch^[c] := High^[c] + Low^[c];

    // store delays
    FDelay[0] := Band^[c];
    FDelay[1] := Low^[c] - CDenorm32;
   end;
end;

end.
