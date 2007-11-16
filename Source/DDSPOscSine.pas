unit DDSPOscSine;

interface

uses DAVDCommon, DAVDComplex, DDspBaseComponent, DDSPBaseOsc;

type
  TDspOscSine = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation

procedure TDspOscSine.Process(var Data: Single; const channel: integer);
begin
  Data:=fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im:=fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re:=Data;
  Data:=Data * fAmplitude + FDCOffset;
end;

procedure TDspOscSine.Process(var Data: Double; const channel: integer);
begin
  Data:=fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im:=fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re:=Data;
  Data:=Data * fAmplitude + FDCOffset;
end;

end.
