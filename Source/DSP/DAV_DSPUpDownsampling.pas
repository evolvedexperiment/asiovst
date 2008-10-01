unit DAV_DSPUpDownSampling;

interface

{$I ASIOVST.INC}

uses
  Classes, SysUtils, DAV_Common, DAV_AudioData, DAV_DspFilter,
  DAV_DSPButterworthFilter, DAV_DSPChebyshevFilter, DAV_DSPBesselFilter;

type
  TDAVResampling = class(TAudioObject)
  private
    fFilterClass: TIIRFilterClass;
    procedure SetFactor(const Value: Integer);
    procedure SetOrder(const Value: Integer);
    procedure SetTransitionBandwidth(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetFilterClass(const Value: TIIRFilterClass);
  protected
    fFactor              : Integer;
    fOrder               : Integer;
    fTransitionBandwidth : Double;
    fSampleRate          : Double;
    procedure FactorChanged; virtual;
    procedure FilterClassChanged; virtual; abstract;
    procedure OrderChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure TransitionBandwidthChanged; virtual;
    procedure UpdateFilter; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property FilterClass: TIIRFilterClass read fFilterClass write SetFilterClass;
  published
    property Factor: Integer read fFactor write SetFactor;
    property Order: Integer read fOrder write SetOrder default 2;
    property TransitionBandwidth: Double read fTransitionBandwidth write SetTransitionBandwidth;
    property SampleRate: Double read fSampleRate write SetSampleRate;
  end;

  TDAVUpSampling = class(TDAVResampling)
  private
    fFilter : TIIRFilter;
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
    fFilter : TIIRFilter;
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
    fFilter : array [0..1] of TIIRFilter;
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

const
  cDenorm = 1E-32;

{ TDAVResampling }

constructor TDAVResampling.Create(AOwner: TComponent);
begin
 inherited;
 fTransitionBandwidth := 0.99;
 fSampleRate          := 44100;
 Order                := 2;
end;

procedure TDAVResampling.SetFactor(const Value: Integer);
begin
 if fFactor <> Value then
  begin
   fFactor := Value;
   FactorChanged;
  end;
end;

procedure TDAVResampling.SetFilterClass(const Value: TIIRFilterClass);
begin
 if fFilterClass <> Value then
  begin
   fFilterClass := Value;
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
 if fTransitionBandwidth <> Value then
  begin
   fTransitionBandwidth := Value;
   TransitionBandwidthChanged;
  end;
end;

procedure TDAVResampling.SetOrder(const Value: Integer);
begin
 if fOrder <> Value then
  begin
   fOrder := Value;
   OrderChanged;
  end;
end;

procedure TDAVResampling.SampleRateChanged;
begin
 UpdateFilter;
end;

procedure TDAVResampling.SetSampleRate(const Value: Double);
begin
 if fSampleRate <> Value then
  begin
   fSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TDAVUpDownsampling }

constructor TDAVUpDownsampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLP;
 inherited;
end;

destructor TDAVUpDownsampling.Destroy;
begin
 FreeAndNil(fFilter[0]);
 FreeAndNil(fFilter[1]);
 inherited;
end;

procedure TDAVUpDownsampling.UpdateFilter;
var
  Frequency : Double;
begin
 Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
 fFilter[0].Frequency := Frequency;
 fFilter[1].Frequency := Frequency;
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


procedure TDAVUpDownsampling.FilterClassChanged;
var
  i         : Integer;
  oldFilter : TIIRFilter;
begin
 for i := 0 to Length(fFilter) - 1 do
  begin
   oldFilter := fFilter[i];
   fFilter[i] := fFilterClass.Create;
   if assigned(oldFilter)
    then fFilter[i].Assign(oldFilter);
   if fFilter[i] is TChebyshev1Filter then
    with TChebyshev1Filter(fFilter[i]) do
     begin
      Ripple := 0.1;
     end;
   FreeAndNil(oldFilter);
  end;
end;

procedure TDAVUpDownsampling.OrderChanged;
begin
 fFilter[0].Order := fOrder;
 fFilter[1].Order := fOrder;
 inherited;
end;

procedure TDAVUpDownsampling.SampleRateChanged;
begin
 fFilter[0].SampleRate := SampleRate;
 fFilter[1].SampleRate := SampleRate;
 inherited;
end;

{ TDAVUpSampling }

constructor TDAVUpSampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLP;
 inherited;
end;

destructor TDAVUpSampling.Destroy;
begin
 FreeAndNil(fFilter);
 inherited;
end;

procedure TDAVUpSampling.FilterClassChanged;
var
  oldFilter : TIIRFilter;
begin
 oldFilter := fFilter;
 fFilter := fFilterClass.Create;
 if assigned(oldFilter)
  then fFilter.Assign(oldFilter);
 if fFilter is TChebyshev1Filter then
  with TChebyshev1Filter(fFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(oldFilter);
end;

procedure TDAVUpSampling.OrderChanged;
begin
 fFilter.Order := fOrder;
 inherited;
end;

procedure TDAVUpSampling.SampleRateChanged;
begin
 fFilter.SampleRate := fSampleRate;
 inherited;
end;

procedure TDAVUpSampling.UpdateFilter;
begin
 fFilter.Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
end;

procedure TDAVUpSampling.Upsample32(Input: Single; Output: PDAVSingleFixedArray);
var
  i : Integer;
begin
 Output[0] := fFilter.ProcessSample(Factor * Input + cDenorm);
 for i := 1 to Factor - 1
  do Output[i] := fFilter.ProcessSample(-cDenorm);
end;

procedure TDAVUpSampling.Upsample64(Input: Double; Output: PDAVDoubleFixedArray);
var
  i : Integer;
begin
 Output[0] := fFilter.ProcessSample(Factor * Input + cDenorm);
 for i := 1 to Factor - 1
  do Output[i] := fFilter.ProcessSample(-cDenorm);
end;

{ TDAVDownSampling }

constructor TDAVDownSampling.Create(AOwner: TComponent);
begin
 FilterClass := TButterworthLP;
 inherited;
end;

destructor TDAVDownSampling.Destroy;
begin
 FreeAndNil(fFilter);
 inherited;
end;

procedure TDAVDownSampling.FilterClassChanged;
var
  oldFilter : TIIRFilter;
begin
 oldFilter := fFilter;
 fFilter := fFilterClass.Create;
 if assigned(oldFilter)
  then fFilter.Assign(oldFilter);
 if fFilter is TChebyshev1Filter then
  with TChebyshev1Filter(fFilter) do
   begin
    Ripple := 0.1;
   end;
 FreeAndNil(oldFilter);
end;

function TDAVDownSampling.Downsample32(Input: PDAVSingleFixedArray): Single;
var
  i : Integer;
begin
 result := fFilter.ProcessSample(Input[0] + cDenorm);
 for i := 1 to Factor - 1
  do fFilter.ProcessSample(Input[i]);
end;

function TDAVDownSampling.Downsample64(Input: PDAVDoubleFixedArray): Double;
var
  i : Integer;
begin
 result := fFilter.ProcessSample(Input[0] + cDenorm);
 for i := 1 to Factor - 1
  do fFilter.ProcessSample(Input[i]);
end;

procedure TDAVDownSampling.OrderChanged;
begin
 fFilter.Order := fOrder;
 inherited;
end;

procedure TDAVDownSampling.SampleRateChanged;
begin
 fFilter.SampleRate := fSampleRate;
 inherited;
end;

procedure TDAVDownSampling.UpdateFilter;
begin
 fFilter.Frequency := 0.5 * TransitionBandwidth * SampleRate / Factor;
end;

end.
