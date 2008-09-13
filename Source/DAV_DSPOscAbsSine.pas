unit DAV_DSPOscAbsSine;

interface

uses
  DAV_Common, DAV_Complex, DAV_DspBaseComponent, DAV_DSPBaseOsc;

type
  TDspOscAbsSine = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation


procedure TDspOscAbsSine.Process(var Data: Single; const channel: integer);
begin
  Data:=fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im:=fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re:=Data;
  f_abs(Data);
  Data := (Data*2-1) * fAmplitude + FDCOffset;
end;

procedure TDspOscAbsSine.Process(var Data: Double; const channel: integer);
begin
  Data:=fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im:=fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re:=Data;
  f_abs(Data);
  Data := (Data*2-1) * fAmplitude + FDCOffset;
end;

end.
