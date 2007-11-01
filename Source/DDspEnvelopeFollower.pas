unit DDspEnvelopeFollower;

interface

uses DDspBaseComponent, DAVDCommon;

type
  TDspEnvelopeFollower = class(TDspBaseComponent)
  protected
    fLastOutputSingle: TAVDSingleDynArray;
    fLastOutputDouble: TAVDDoubleDynArray;
    fInternalAttack: Single;
    fInternalRelease: Single;
    fAttack: Single;
    fRelease: Single;
    procedure SetAttack(const Value: single);
    procedure SetRelease(const Value: single);
  protected
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; override;
  public
    procedure Init; override;
    procedure Reset; override;
    function Process(channel: integer; input: Single): Single; override;
    function Process(channel: integer; input: Double): Double; override;
  published
    property Attack:  single read fAttack write SetAttack;   // 0..1
    property Release: single read fRelease write SetRelease; // 0..1
  end;

implementation

uses Math;

procedure TDspEnvelopeFollower.Init;
begin
  fAttack:=0.5;
  fRelease:=0.5;
  Reset;
end;

procedure TDspEnvelopeFollower.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;

procedure TDspEnvelopeFollower.SampleRateChanged;
begin
  fInternalAttack:=power(0.01,1/((0.001+fAttack*1.999)*fSampleRate));
  fInternalRelease:=power(0.01,1/((0.001+fRelease*1.999)*fSampleRate));
end;

procedure TDspEnvelopeFollower.ChannelsChanged;
begin
  SetLength(fLastOutputSingle, fChannels);
  SetLength(fLastOutputDouble, fChannels);

  FillChar(fLastOutputSingle[0], fChannels * SizeOf(Single), 0);
  FillChar(fLastOutputDouble[0], fChannels * SizeOf(Double), 0);
end;

procedure TDspEnvelopeFollower.SetAttack(const Value: single);
begin
  if fAttack <> Value then
  begin
    fAttack := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

procedure TDspEnvelopeFollower.SetRelease(const Value: single);
begin
  if fRelease <> Value then
  begin
    fRelease := max(0,min(1,Value));
    SampleRateChanged;
  end;
end;

function TDspEnvelopeFollower.Process(channel: integer; input: Double): Double;
var tmp: Double;
begin
  f_abs(input);

  if input>=fLastOutputDouble[channel] then
    tmp:=fInternalAttack
  else
    tmp:=fInternalRelease;

  fLastOutputDouble[channel] := tmp * (fLastOutputDouble[channel] - input) + input;
  result:=fLastOutputDouble[channel];
end;

function TDspEnvelopeFollower.Process(channel: integer; input: Single): Single;
var tmp: Single;
begin
  f_abs(input);

  if input>=fLastOutputSingle[channel] then
    tmp:=fInternalAttack
  else
    tmp:=fInternalRelease;

  fLastOutputSingle[channel] := tmp * (fLastOutputSingle[channel] - input) + input;
  result:=fLastOutputSingle[channel];
end;

end.
