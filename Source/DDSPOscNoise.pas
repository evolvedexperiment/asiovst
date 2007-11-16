unit DDSPOscNoise;

interface

uses DAVDCommon, DAVDComplex, DDspBaseComponent, DDSPBaseOsc;

type
  TDspOscNoise = class(TDspBaseOsc)
  protected
    procedure Process(var Data: Single; const channel: integer); override;
    procedure Process(var Data: Double; const channel: integer); override;
  public
    procedure Init;  override;
  end;

implementation

procedure TDspOscNoise.Init;
begin
  inherited;
  Randomize;
end;

procedure TDspOscNoise.Process(var Data: Single; const channel: integer);
begin
  Data:=(2*random-1)*fAmplitude+FDCOffset;
end;

procedure TDspOscNoise.Process(var Data: Double; const channel: integer);
begin
  Data:=(2*random-1)*fAmplitude+FDCOffset;
end;

end.
