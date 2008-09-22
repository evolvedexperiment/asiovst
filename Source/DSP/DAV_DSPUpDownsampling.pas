unit DAV_DSPUpDownSampling;

interface

{$I ASIOVST.INC}

uses
  Classes, SysUtils, DAV_Common, DAV_AudioData, DAV_DSPButterworthFilter;

type
  TDAVUpDownsampling = class(TAudioObject)
  private
    fFilter              : array [0..1] of TButterworthLP;
    fFactor              : Integer;
    fOrder               : Integer;
    fTransitionBandwidth : Double;
    fSampleRate          : Double;
    procedure SetFactor(const Value: Integer);
    procedure SetOrder(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Double);
    procedure SetSampleRate(const Value: Double);
  protected
    procedure UpdateFilter; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Upsample32(Input: Single; Output: PDAVSingleFixedArray);
    procedure Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
    function Downsample32(Input: PDAVSingleFixedArray): Single;
    function Downsample64(Input: PDAVDoubleFixedArray): Double;
  published
    property Factor: Integer read fFactor write SetFactor;
    property Order: Integer read fOrder write SetOrder default 2;
    property TransitionBandwidth: Double read fTransitionBandwidth write SetTransitionBandwidth;
    property SampleRate: Double read fSampleRate write SetSampleRate;
  end;

implementation

const
  cDenorm = 1E-32;

{ TDAVUpDownsampling }

constructor TDAVUpDownsampling.Create(AOwner: TComponent);
begin
 inherited;
 fFilter[0]           := TButterworthLP.Create;
 fFilter[1]           := TButterworthLP.Create;
 fTransitionBandwidth := 0.99;
 fSampleRate          := 44100;
 Order                := 2;
end;

destructor TDAVUpDownsampling.Destroy;
begin
 FreeAndNil(fFilter[0]);
 FreeAndNil(fFilter[1]);
 inherited;
end;

procedure TDAVUpDownsampling.Upsample32(Input: Single;
  Output: PDAVSingleFixedArray);
var
  i : Integer;
begin
 Output[0] := fFilter[0].ProcessSample(Factor * Input + cDenorm);
 for i := 1 to Factor - 1
  do Output[i] := fFilter[0].ProcessSample(-cDenorm);
end;

procedure TDAVUpDownsampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  i : Integer;
begin
 Output[0] := fFilter[0].ProcessSample(Factor * Input + cDenorm);
 for i := 1 to Factor - 1
  do Output[i] := fFilter[0].ProcessSample(-cDenorm);
end;

function TDAVUpDownsampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  i : Integer;
begin
 result := fFilter[1].ProcessSample(Input[0] + cDenorm);
 for i := 1 to Factor - 1
  do fFilter[1].ProcessSample(Input[i]);
end;

function TDAVUpDownsampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  i : Integer;
begin
 result := fFilter[1].ProcessSample(Input[0] + cDenorm);
 for i := 1 to Factor - 1
  do fFilter[1].ProcessSample(Input[i] - cDenorm);
end;

procedure TDAVUpDownsampling.SetFactor(const Value: Integer);
begin
 if fFactor <> Value then
  begin
   fFactor := Value;
   UpdateFilter;
  end;
end;

procedure TDAVUpDownsampling.SetTransitionBandwidth(const Value: Double);
begin
 if fTransitionBandwidth <> Value then
  begin
   fTransitionBandwidth := Value;
   UpdateFilter;
  end;
end;

procedure TDAVUpDownsampling.UpdateFilter;
var
  Frequency : Double;
begin
 Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 fFilter[0].Frequency := Frequency;
 fFilter[1].Frequency := Frequency;
end;

procedure TDAVUpDownsampling.SetOrder(const Value: Integer);
begin
 if fOrder <> Value then
  begin
   fOrder := Value;
   fFilter[0].Order := Value;
   fFilter[1].Order := Value;
  end;
end;

procedure TDAVUpDownsampling.SampleRateChanged;
begin
 UpdateFilter;
end;

procedure TDAVUpDownsampling.SetSampleRate(const Value: Double);
begin
 if fSampleRate <> Value then
  begin
   fSampleRate := Value;
   SampleRateChanged;
  end;
end;

end.
