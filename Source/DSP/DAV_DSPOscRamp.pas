unit DAV_DSPOscRamp;

interface

uses
  DAV_Common, DAV_Complex, DAV_DspBaseComponent, DAV_DSPBaseOsc;

type
  TDspOscRamp = class(TDspBaseOsc)
  protected
    procedure FrequencyChanged; override; 
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation


procedure TDspOscRamp.FrequencyChanged;
begin
  FAngle.Re:=FFrequency/FSampleRate
end;

procedure TDspOscRamp.Process(var Data: Single; const channel: integer);
begin
  fPosition[channel].Re := fPosition[channel].Re + FAngle.Re;
  if fPosition[channel].Re > 1 then
    fPosition[channel].Re := f_Frac(fPosition[channel].Re);

  Data := (fPosition[channel].Re * 2 - 1) * fAmplitude + FDCOffset;
end;

procedure TDspOscRamp.Process(var Data: Double; const channel: integer);
begin
  fPosition[channel].Re := fPosition[channel].Re + FAngle.Re;
  if fPosition[channel].Re > 1 then
    fPosition[channel].Re := f_Frac(fPosition[channel].Re);

  Data:=(fPosition[channel].Re * 2 - 1) * fAmplitude + FDCOffset;
end;

end.
