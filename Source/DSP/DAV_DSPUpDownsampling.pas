unit DAV_DSPUpDownSampling;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_AudioData, DAV_DspFilter,
  DAV_DSPFilterButterworth, DAV_DSPChebyshevFilter, DAV_DSPBesselFilter;

type
  TDAVResampling = class(TAudioObject)
  private
    FFilterClass : TOrderFilterClass;
    procedure SetFactor(const Value: Integer);
    procedure SetOrder(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetFilterClass(const Value: TOrderFilterClass);
  protected
    FFactor              : Integer;
    FOrder               : Integer;
    FTransitionBandwidth : Double;
    FSampleRate          : Double;
    procedure FactorChanged; virtual;
    procedure FilterClassChanged; virtual; abstract;
    procedure OrderChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure TransitionBandwidthChanged; virtual;
    procedure UpdateFilter; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property FilterClass: TOrderFilterClass read FFilterClass write SetFilterClass;
  published
    property Factor: Integer read FFactor write SetFactor;
    property Order: Integer read FOrder write SetOrder default 2;
    property TransitionBandwidth: Double read FTransitionBandwidth write SetTransitionBandwidth;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

  TDAVUpSampling = class(TDAVResampling)
  private
    FFilter : TCustomOrderFilter;
  protected
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Upsample32(Input: Single; Output: PDAVSingleFixedArray);
    procedure Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
  end;

  TDAVDownSampling = class(TDAVResampling)
  private
    FFilter : TCustomOrderFilter;
  protected
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Downsample32(Input: PDAVSingleFixedArray): Single;
    function Downsample64(Input: PDAVDoubleFixedArray): Double;
  end;

  TDAVUpDownsampling = class(TDAVResampling)
  private
    FFilter : array [0..1] of TCustomOrderFilter;
  protected
    procedure FilterClassChanged; override;
    procedure OrderChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateFilter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Upsample32(Input: Single; Output: PDAVSingleFixedArray);
    procedure Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
    function Downsample32(Input: PDAVSingleFixedArray): Single;
    function Downsample64(Input: PDAVDoubleFixedArray): Double;
  end;

implementation

{ TDAVResampling }

constructor TDAVResampling.Create(AOwner: TComponent);
begin
 inherited;
 FFactor              := 1;
 FTransitionBandwidth := 0.99;
 FSampleRate          := 44100;
 Order                := 2;
end;

procedure TDAVResampling.SetFactor(const Value: Integer);
begin
 if FFactor <> Value then
  begin
   FFactor := Value;
   FactorChanged;
  end;
end;

procedure TDAVResampling.SetFilterClass(const Value: TOrderFilterClass);
begin
 if FFilterClass <> Value then
  begin
   FFilterClass := Value;
   FilterClassChanged;
  end;
end;

procedure TDAVResampling.FactorChanged;
begin
 UpdateFilter;
end;

procedure TDAVResampling.OrderChanged;
begin
 UpdateFilter;
end;

procedure TDAVResampling.TransitionBandwidthChanged;
begin
 UpdateFilter;
end;

procedure TDAVResampling.SetTransitionBandwidth(const Value: Double);
begin
 if FTransitionBandwidth <> Value then
  begin
   FTransitionBandwidth := Value;
   TransitionBandwidthChanged;
  end;
end;

procedure TDAVResampling.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TDAVResampling.SampleRateChanged;
begin
 UpdateFilter;
end;

procedure TDAVResampling.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TDAVUpDownsampling }

constructor TDAVUpDownsampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVUpDownsampling.Destroy;
begin
 FreeAndNil(FFilter[0]);
 FreeAndNil(FFilter[1]);
 inherited;
end;

procedure TDAVUpDownsampling.UpdateFilter;
var
  Frequency : Double;
begin
 Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 assert(assigned(FFilter[0]));
 assert(assigned(FFilter[1]));
 FFilter[0].Frequency := Frequency;
 FFilter[1].Frequency := Frequency;
 FFilter[0].ResetStates;
 FFilter[1].ResetStates;
end;

procedure TDAVUpDownsampling.Upsample32(Input: Single;
  Output: PDAVSingleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter[0].ProcessSample(Factor * Input + cDenorm32);
 for i := 1 to Factor - 1
  do Output[i] := FFilter[0].ProcessSample(-cDenorm32);
end;

procedure TDAVUpDownsampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter[0].ProcessSample(Factor * Input + cDenorm64);
 for i := 1 to Factor - 1
  do Output[i] := FFilter[0].ProcessSample(-cDenorm64);
end;

function TDAVUpDownsampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  i : Integer;
begin
 result := FFilter[1].ProcessSample(Input[0] + cDenorm32);
 for i := 1 to Factor - 1
  do FFilter[1].ProcessSample(Input[i]);
end;

function TDAVUpDownsampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  i : Integer;
begin
 result := FFilter[1].ProcessSample(Input[0] + cDenorm64);
 for i := 1 to Factor - 1
  do FFilter[1].ProcessSample(Input[i] - cDenorm64);
end;


procedure TDAVUpDownsampling.FilterClassChanged;
var
  i         : Integer;
  oldFilter : TCustomOrderFilter;
begin
 for i := 0 to Length(FFilter) - 1 do
  begin
   oldFilter := FFilter[i];
   FFilter[i] := FFilterClass.Create(FOrder);
   if assigned(oldFilter)
    then FFilter[i].Assign(oldFilter);
   if FFilter[i] is TCustomChebyshev1Filter then
    with TCustomChebyshev1Filter(FFilter[i]) do
     begin
      Ripple := 0.1;
     end;
   FreeAndNil(oldFilter);
  end;
end;

procedure TDAVUpDownsampling.OrderChanged;
begin
 FFilter[0].Order := FOrder;
 FFilter[1].Order := FOrder;
 FFilter[0].ResetStates;
 FFilter[1].ResetStates;
 inherited;
end;

procedure TDAVUpDownsampling.SampleRateChanged;
begin
 FFilter[0].SampleRate := SampleRate;
 FFilter[1].SampleRate := SampleRate;
 inherited;
end;

{ TDAVUpSampling }

constructor TDAVUpSampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVUpSampling.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TDAVUpSampling.FilterClassChanged;
var
  oldFilter : TCustomOrderFilter;
begin
 oldFilter := FFilter;
 FFilter := FFilterClass.Create;
 if assigned(oldFilter)
  then FFilter.Assign(oldFilter);
 if FFilter is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(FFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(oldFilter);
end;

procedure TDAVUpSampling.OrderChanged;
begin
 FFilter.Order := FOrder;
 FFilter.ResetStates;
 inherited;
end;

procedure TDAVUpSampling.SampleRateChanged;
begin
 FFilter.SampleRate := FSampleRate;
 inherited;
end;

procedure TDAVUpSampling.UpdateFilter;
begin
 assert(assigned(FFilter));
 FFilter.Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 FFilter.ResetStates;
end;

procedure TDAVUpSampling.Upsample32(Input: Single; Output: PDAVSingleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter.ProcessSample(Factor * Input + cDenorm32);
 for i := 1 to Factor - 1
  do Output[i] := FFilter.ProcessSample(-cDenorm32);
end;

procedure TDAVUpSampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  i : Integer;
begin
 Output[0] := FFilter.ProcessSample(Factor * Input + cDenorm64);
 for i := 1 to Factor - 1
  do Output[i] := FFilter.ProcessSample(-cDenorm64);
end;

{ TDAVDownSampling }

constructor TDAVDownSampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLowPassFilter;
 inherited;
end;

destructor TDAVDownSampling.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TDAVDownSampling.FilterClassChanged;
var
  oldFilter : TCustomOrderFilter;
begin
 oldFilter := FFilter;
 FFilter := FFilterClass.Create;
 if assigned(oldFilter)
  then FFilter.Assign(oldFilter);
 if FFilter is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(FFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(oldFilter);
end;

function TDAVDownSampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  i : Integer;
begin
 result := FFilter.ProcessSample(Input[0] + cDenorm32);
 for i := 1 to Factor - 1
  do FFilter.ProcessSample(Input[i]);
end;

function TDAVDownSampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  i : Integer;
begin
 result := FFilter.ProcessSample(Input[0] + cDenorm64);
 for i := 1 to Factor - 1
  do FFilter.ProcessSample(Input[i]);
end;

procedure TDAVDownSampling.OrderChanged;
begin
 FFilter.Order := FOrder;
 FFilter.ResetStates;
 inherited;
end;

procedure TDAVDownSampling.SampleRateChanged;
begin
 FFilter.SampleRate := FSampleRate;
 inherited;
end;

procedure TDAVDownSampling.UpdateFilter;
begin
 assert(assigned(FFilter));
 FFilter.Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 FFilter.ResetStates;
end;

end.
