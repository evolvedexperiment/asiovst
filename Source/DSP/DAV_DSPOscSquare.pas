unit DAV_DSPOscSquare;

interface

uses
  DAV_Common, DAV_Complex, DAV_DspBaseComponent, DAV_DSPBaseOsc;

type
  TDspOscSquare = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  end;

implementation

procedure TDspOscSquare.Process(var Data: Single; const channel: integer);
begin
  Data := fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im := fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re := Data;

  if Data > 0 then Data := 1 else Data := -1;
  Data := Data * fAmplitude + FDCOffset;
end;

procedure TDspOscSquare.Process(var Data: Double; const channel: integer);
begin
  Data := fPosition[channel].Re*fAngle.Re-fPosition[channel].Im*fAngle.Im;
  fPosition[channel].Im := fPosition[channel].Im*fAngle.Re+fPosition[channel].Re*fAngle.Im;
  fPosition[channel].Re := Data;

  if Data>0 then Data := 1 else Data := -1;
  Data := Data * fAmplitude + FDCOffset;
end;

end.
