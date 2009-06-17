unit AnalyserChebyshevFilter;

interface

{$DEFINE x87}

type
  TAnalyseFilter = class(TObject)
  private
    function GetFrequency: Double;
    function GetGain: Double;
    function GetQ: Double;
    function GetSampleRate: Double;
    procedure SetDownsamplePower(Value: Integer);
    procedure SetOrder(const Value: Integer);
  protected
    fFrequency      : Double;
    fGain, fQ       : Double;
    fSampleRate     : Double;
    fSRR            : Double;
    fSinW0,fW0      : Double;
    fGainSpeed      : Double;
    fRipple         : array [0..1] of Double;
    fDownsamplePow  : Integer;
    fDownsampleFak  : Integer;
    fOrder          : Integer;
    fAB             : array [0..127] of Double;
    fD64            : array [0.. 63] of Double;
    procedure SetW0; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    procedure SetGain(const Value: Double); virtual;
    procedure SetQ(const Value: Double); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRipple; virtual;

    property GainSpeed: Double read fGainSpeed;
    property SinW0: Double read fSinW0;
    property W0: Double read fW0;
  public
    constructor Create; virtual;
    function ProcessSample(const s:Double):Double; virtual; abstract;
    function ProcessSampleASM:Double; virtual; abstract;
    procedure CalcCoefficients; virtual; abstract;
    procedure SetFilterValues(const Frequency, Gain, Q : Single); virtual;
    procedure ResetState; virtual; abstract;
    function Magnitude(f:Single):Single; virtual;
    function MagnitudeLog10(f:Single):Single; virtual;
    procedure Reset; virtual;
    property SampleRate : Double read GetSampleRate write SetSampleRate;
    property SampleRateRez : Double read fSRR;
    property Frequency : Double read GetFrequency write SetFrequency;
    property Gain : Double read GetGain write SetGain;
    property Bandwidth : Double read GetQ write SetQ;
    property DownsampleAmount : Integer read fDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read fDownsampleFak;
    property Order : Integer read fOrder write SetOrder;
  end;

  TAnalyseFilterLP = class(TAnalyseFilter)
  private
  protected
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function ProcessSampleASM: Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

  TAnalyseFilterHP = class(TAnalyseFilter)
  private
  protected
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function ProcessSampleASM: Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

implementation

uses
  Math, DAV_Common, SysUtils;

constructor TAnalyseFilter.Create;
begin
 fDownsamplePow := 0;
 fDownsampleFak := 1;
 fFrequency := 0;
 fGain := 0; fQ := 1;
 fOrder := 10;
 SampleRate := 44100;
end;

function TAnalyseFilter.GetFrequency: Double;
begin
 Result := fFrequency;
end;

function TAnalyseFilter.GetGain: Double;
begin
 Result := fGain;
end;

function TAnalyseFilter.GetQ: Double;
begin
 Result := fQ;
end;

procedure TAnalyseFilter.Reset;
begin
 fGain := 0;
 CalcCoefficients;
end;

procedure TAnalyseFilter.SetSampleRate(const Value: Double);
begin
 if Value=0 then Exit;
 if Value <> fSampleRate then
  begin
   fSampleRate := Value;
   fSRR := 1 / fSampleRate;
  end;
end;

function TAnalyseFilter.GetSampleRate: Double;
begin
 result := fSampleRate;
end;

procedure TAnalyseFilter.SetDownsamplePower(Value: Integer);
begin
 if Value < 0 then Value := 0;
 if fDownsamplePow <> Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2, fDownsamplePow));
   SetW0;
  end;
end;

procedure TAnalyseFilter.SetW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency * fDownsampleFak);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TAnalyseFilter.SetGain(const Value: Double);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fGain := Value;
 fGainSpeed := Exp(fGain * ln10_0025);
end;

procedure TAnalyseFilter.SetOrder(const Value: Integer);
begin
 fOrder := Value;
 CalcCoefficients;
end;

procedure TAnalyseFilter.SetFrequency(const Value: Double);
begin
 if fFrequency <> Value then
  begin
   SetW0;
   SetRipple;
  end;
end;

procedure TAnalyseFilter.SetQ(const Value: Double);
begin
 if Value <> fQ then
  begin
   fQ := Value;
   SetRipple;
  end;
end;

procedure TAnalyseFilter.SetRipple;
var
  t : Double;
begin
 t := arcsinh(10 * fQ) * 0.1;
 fRipple[1] := sinh(t);
 fRipple[0] := sqr(cosh(t));
end;

procedure TAnalyseFilter.SetFilterValues(const Frequency, Gain, Q: Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency := Frequency; fGain := Gain; fQ := Q;
 fGainSpeed := Exp((fGain * ln10_0025));
 SetW0;
 SetRipple;
end;

function TAnalyseFilter.Magnitude(f: Single): Single;
begin
 Result := 1;
end;

function TAnalyseFilter.MagnitudeLog10(f: Single): Single;
begin
 result := 20 * Log10(Magnitude(f));
end;

{ TAnalyseFilterLP }

constructor TAnalyseFilterLP.Create;
begin
 inherited Create;
 fGainSpeed := 1;
end;

procedure TAnalyseFilterLP.CalcCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K := tan(fW0 * 0.5); K2 := K * K;
 for i := (fOrder div 2)-1 downto 0 do
  begin
   t  := cos( ((i * 2 + 1) * Pi * 0.05) );
   t1 := 1 / (fRipple[0] - (t * t));
   t2 := K * t1 * fRipple[1] * (2 * t);
   t :=      1 / (t2 + K2 + t1);
   fAB[4 * i]     := K2 * t;
   fAB[4 * i + 1] := 2 * fAB[4 * i];
   fAB[4 * i + 2] := 2 * (t1 - K2) * t;
   fAB[4 * i + 3] := (t2 - K2 - t1) * t;
  end;
 fAB[0] := fAB[0] * fGainSpeed;
 fAB[1] := fAB[1] * fGainSpeed;
end;
{$ELSE}
const
  chalf : Double = 0.5;
asm
 fld1
 fild [self.fOrder]                // fOrder, 1
 fadd st(0),st(0)                  // 2*fOrder, 1
 fdivp                             // 1/2*fOrder

 fld [self.fW0]                    // fW0, 1/2*fOrder
 fmul chalf                        // fW0/2, 1/2*fOrder
 fsincos                           // sin(fW0/2), cos(fW0/2), 1/2*fOrder
 fdivp                             // K = tan(fW0*0.5), 1/2*fOrder
 fld st(0)                         // K, K, 1/2*fOrder
 fmul st(0),st(0)                  // K², K, 1/2*fOrder
 fxch                              // K, K², 1/2*fOrder

 mov ecx,[self.fOrder]             // ecx = order
 shr ecx,1                         // ecx = order div 2
 @OrderLoop:
  mov edx,ecx                      // edx = i
  imul edx,2                       // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  shl edx,1                        // edx = 4*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K², 1/2*fOrder
  fldpi                            // Pi, 2*i+1, K, K², 1/2*fOrder
  fmulp                            // Pi * (2*i+1), K, K², 1/2*fOrder
  fmul st(0),st(3)                 // Pi * (2*i+1)/(2*Order), K, K², 1/2*fOrder
  fcos                             // cos((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*fOrder
  fld st(0)                        // t, t, K, K², 1/2*fOrder
  fmul st(0),st(0)                 // t²,t, K, K², 1/2*fOrder
  fld [self.fRipple].Double        // fRipple[0], t²,t, K, K², 1/2*fOrder
  fsubrp                           // fRipple[0] - t²,t, K, K², 1/2*fOrder
  fld1                             // 1, fRipple[0] - t²,t, K, K², 1/2*fOrder
  fdivrp                           // 1 / (fRipple[0] - t²) = t1, t, K, K², 1/2*fOrder
  fxch                             // t, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*t, t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                 // 2*t * t1, t1, K, K², 1/2*fOrder
  fmul [self.fRipple+8].Double     // fRipple[1]*2*t*t1, t1, K, K², 1/2*fOrder
  fmul st(0), st(2)                // K*fRipple[1]*2*t*t1 = t2, t1, K, K², 1/2*fOrder
  fld st(0)                        // t2, t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(2)                 // t1+t2, t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(4)                 // t1+t2+K², t2, t1, K, K², 1/2*fOrder
  fld1                             // 1, t1+t2+K², t2, t1, K, K², 1/2*fOrder
  fdivrp                           // (1/t1+t2+K²)=t, t2, t1, K, K², 1/2*fOrder
  fld st(0)                        // t, t, t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(5)                 // t*K²=fA[2*i], t, t2, t1, K, K², 1/2*fOrder
  fst [Self.fAB+8*edx].Double      // store to fA[2*i], 1/2*fOrder
  fadd st(0),st(0)                 // 2*fA[2*i], t, t2, t1, K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+8].Double   // store to fA[2*i+1], 1/2*fOrder

  fld st(2)                        // t1, t, t2, t1, K, K², 1/2*fOrder
  fsub st(0),st(5)                 // t1-K², t, t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*(t1-K²), t, t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                 // 2*(t1-K²)*t, t, t2, t1, K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+16].Double  // store to fB[2*i], 1/2*fOrder
  fxch                             // t2, t, t1, K, K², 1/2*fOrder
  fsubrp st(2),st(0)               // t, t2-t1, K, K², 1/2*fOrder
  fxch                             // t2-t1, t, K, K², 1/2*fOrder
  fsub st(0),st(3)                 // t2-t1-K², t, K, K², 1/2*fOrder
  fmulp                            // (t2-t1-K²) * t, K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+24].Double  // store to fB[2*i+1], 1/2*fOrder
 loop @OrderLoop
 fstp st(0)                        // K², 1/2*fOrder
 fstp st(0)                        // 1/2*fOrder
 fstp st(0)                        // stack free!

 fld [self.fAB].Double             // load fA[0]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fAB].Double            // store fA[0]
 fld [self.fAB+8].Double           // load fA[1]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fAB+8].Double          // store fA[1]
end;
{$ENDIF}

function TAnalyseFilterLP.Magnitude(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2 * cos(f * pi * fSRR);
 a := sqr(cw - 2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fAB[4 * i]) * a /
                 (1 + sqr(fAB[4 * i + 2]) + sqr(fAB[4 * i + 3]) + 2 * fAB[4 * i + 3] + cw *
                   ((fAB[4 * i + 2] - cw) * fAB[4 * i + 3] - fAB[4 * i + 2]));
 Result := sqrt(Result);
end;

function TAnalyseFilterLP.MagnitudeLog10(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw := 2 * cos(f * pi * fSRR); a := sqr(cw - 2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fAB[4 * i]) * a /
       (1 + sqr(fAB[4 * i + 2]) + sqr(fAB[4 * i + 3]) + 2 * fAB[4 * i + 3] + cw *
         ((fAB[4 * i + 2] - cw) * fAB[4 * i + 3] - fAB[4 * i + 2]));
 Result := 10 * Log10(Result);
end;

function TAnalyseFilterLP.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.fOrder]
 shl ecx, 1
 fld s.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+16].Double
  fadd [self.fD64+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fD64+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fD64+ecx*4+8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y, x : Double;
  i    : Integer;
begin
 Result := s;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x := Result;
   Result          := fAB[4 * i + 0] * x                            + fD64[2 * i];
   fD64[2 * i    ] := fAB[4 * i + 1] * x + fAB[4 * i + 2] * Result  + fD64[2 * i + 1];
   fD64[2 * i + 1] := fAB[4 * i + 0] * x + fAB[4 * i + 3] * Result;
  end;
end;
{$ENDIF}

function TAnalyseFilterLP.ProcessSampleASM: Double;
asm
 push ecx
 mov ecx, [self.fOrder]
 shl ecx,1
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+8].Double
  fadd [self.fD64+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fD64+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fD64+ecx*4+8].Double
 jnz @FilterLoop
 pop ecx
end;

procedure TAnalyseFilterLP.ResetState;
begin
 FillChar(fD64[0], 10 * SizeOf(Double), 0);
end;

{ TAnalyseFilterHP }

constructor TAnalyseFilterHP.Create;
begin
 inherited Create;
 fGainSpeed := 1;
end;

procedure TAnalyseFilterHP.CalcCoefficients;
{$IFDEF PUREPASCAL}
var
  K, K2     : Double;
  t, t1, t2 : Double;
  i         : Integer;
begin
 K := tan(fW0 * 0.5); K2 := K * K;
 for i := (fOrder div 2) - 1 downto 0 do
  begin
   t  := sqr( sin( ((i * 2 + 1) * Pi / (4 * fOrder)) ) );
   t1 := 1 / (fRipple[0] + 4 * t - 4 * sqr(t)-1);
   t2 := 2 * K * t1 * fRipple[1] * (1 - 2 * t);
   fAB[4 * i    ] := 1 / (t2 + 1 + t1 * K2);
   fAB[4 * i + 1] := -2 * fAB[4 * i];
   fAB[4 * i + 2] := 2 * (   1 - t1 * K2) * fAB[4 * i];
   fAB[4 * i + 3] := (t2 - 1 - t1 * K2) * fAB[4 * i];
  end;
 fAB[0] := fAB[0] * fGainSpeed;
 fAB[1] := fAB[1] * fGainSpeed;
end;
{$ELSE}
const
  chalf : Double = 0.5;
asm
 fld1
 fild [self.fOrder]                // fOrder, 1
 fadd st(0),st(0)                  // 2*fOrder, 1
 fadd st(0),st(0)                  // 4*fOrder, 1
 fdivp                             // 1/2*fOrder

 fld [self.fW0]                    // fW0, 1/2*fOrder
 fmul chalf                        // fW0/2, 1/2*fOrder
 fsincos                           // sin(fW0/2), cos(fW0/2), 1/2*fOrder
 fdivp                             // K = tan(fW0*0.5), 1/2*fOrder
 fld st(0)                         // K, K, 1/2*fOrder
 fmul st(0),st(0)                  // K², K, 1/2*fOrder
 fxch                              // K, K², 1/2*fOrder

 mov ecx,[self.fOrder]             // ecx = order
 @OrderLoop:
  mov edx,ecx                      // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  shl edx,1                        // edx = 4*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K²
  fldpi                            // Pi, 2*i+1, K, K², 1/2*fOrder
  fmulp                            // Pi * (2*i+1), K, K², 1/2*fOrder
  fmul st(0),st(3)                 // Pi * (2*i+1) / (4*Order), K, K², 1/2*fOrder
  fsin                             // sin((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*fOrder
  fmul st(0),st(0)                 // sqr(sin((i*2+1)*Pi*0.025)), K, K², 1/2*fOrder

  fld st(0)                        // t, t, K, K², 1/2*fOrder
  fmul st(0),st(0)                 // t²,t, K, K², 1/2*fOrder
  fld st(1)                        // t, t²,t, K, K², 1/2*fOrder
  fsubrp                           // t-t²,t, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*(t-t²),t, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 4*(t-t²),t, K, K², 1/2*fOrder
  fld [self.fRipple].Double        // fRipple[0], 4*t²,t, K, K², 1/2*fOrder
  faddp                            // fRipple[0] - 4*t²,t, K, K², 1/2*fOrder
  fld1                             // 1, fRipple[0]+4*t-4*t²,t, K, K², 1/2*fOrder
  fsub st(1),st(0)                 // 1, fRipple[0]+4*t-4*t²-1,t, K, K², 1/2*fOrder
  fdivrp                           // 1 / (fRipple[0]+4*t-4*t²-1) = t1, t, K, K², 1/2*fOrder

  fxch                             // t, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*t, t1, K, K², 1/2*fOrder
  fld1                             // 1, 2*t, t1, K, K², 1/2*fOrder
  fsubrp                           // 1 - 2*t, t1, K, K², 1/2*fOrder
  fmul [self.fRipple+8].Double     // fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fmul st(0),st(2)                 // K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                 // t1*K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // t2=2*t1*K*fRipple[1]*(1-2*t), t1, K, K², 1/2*fOrder
  fld st(1)                        // t1, t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(4)                 // t1*K², t2, t1, K, K², 1/2*fOrder
  fld1                             // 1, t1*K², t2, t1, K, K², 1/2*fOrder
  faddp                            // 1+t1*K², t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(1)                 // 1+t1*K²+t2, t2, t1, K, K², 1/2*fOrder
  fld1                             // 1, 1+t1*K²+t2, t2, t1, K, K², 1/2*fOrder
  fdivrp                           // 1/(1+t1*K²+t2)=A[2*i], t2, t1, K, K², 1/2*fOrder
  fst [Self.fAB+8*edx].Double      // store to fA[2*i]

  fld st(0)                        // A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fchs                             // -2*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+8].Double    // store to fA[2*i+1]

  fld st(2)                        // t1, A[2*i], t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(5)                 // t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fld1                             // 1, t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fsubrp                           // 1-t1*K², A[2*i], t2, t1, K, K², 1/2*fOrder
  fadd st(0),st(0)                 // 2*(1-t1*K²), A[2*i], t2, t1, K, K², 1/2*fOrder
  fmul st(0),st(1)                 // 2*(1-t1*K²)*A[2*i], A[2*i], t2, t1, K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+16].Double  // store to fB[2*i]

  fxch st(2)                       // t1, t2, A[2*i], K, K², 1/2*fOrder
  fmul st(0), st(4)                // t1*K², t2, A[2*i], K, K², 1/2*fOrder
  fld1                             // 1, t1*K², t2, A[2*i], K, K², 1/2*fOrder
  faddp                            // 1 + t1*K², t2, A[2*i], K, K², 1/2*fOrder
  fsubp                            // t2 - (1 + t1*K²), A[2*i], K, K², 1/2*fOrder
  fmulp                            // (t2 - (1 + t1*K²)) * A[2*i], K, K², 1/2*fOrder
  fstp [self.fAB+8*edx+24].Double  // store to fB[2*i+1], 1/2*fOrder
  sub ecx,2
 jnz @OrderLoop
 fstp st(0)                        // K², 1/2*fOrder
 fstp st(0)                        // 1/2*fOrder
 fstp st(0)                        // stack free!

 fld [self.fAB].Double             // load fA[0]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fAB].Double            // store fA[0]
 fld [self.fAB+8].Double           // load fA[1]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fAB+8].Double          // store fA[1]
end;
{$ENDIF}

function TAnalyseFilterHP.Magnitude(f: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2*cos(f*pi*fSRR); a := sqr(cw-2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fAB[4*i]) * a /
       (1 + sqr(fAB[4 * i + 2]) + sqr(fAB[4 * i + 3]) + 2 * fAB[4 * i + 3] + cw *
         ((fAB[4 * i + 2] - cw) * fAB[4 * i + 3] - fAB[4 * i + 2]));
 Result := sqrt(Result);
end;

function TAnalyseFilterHP.MagnitudeLog10(f: Single): Single;
var
  i     : Integer;
  a, cw : Double;
begin
 cw := 2*cos(f*pi*fSRR); a := sqr(cw-2);
 Result := 1;

 for i := 0 to (fOrder div 2) - 1
  do Result := Result * sqr(fAB[4 * i]) * a /
       (1 + sqr(fAB[4 * i + 2]) + sqr(fAB[4 * i + 3]) + 2 * fAB[4 * i + 3] + cw *
         ((fAB[4 * i + 2] - cw) * fAB[4 * i + 3] - fAB[4 * i + 2]));
 Result := 10 * log10(Result);
end;

function TAnalyseFilterHP.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.fOrder]
 shl ecx, 1
 fld s.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB  + ecx * 8].Double
  fadd [self.fD64 + ecx * 4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB  + ecx * 8 + 16].Double
  fadd [self.fD64 + ecx * 4 + 8].Double
  fld st(3)
  fmul [self.fAB  + ecx * 8 + 8].Double
  faddp
  fstp [self.fD64 + ecx * 4].Double
  fmul [self.fAB  + ecx * 8 + 24].Double
  fxch
  fxch st(2)
  fmul [self.fAB  + ecx * 8].Double
  faddp
  fstp [self.fD64 + ecx * 4 + 8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y,x : Double;
  i   : Integer;
begin
 Result := s;
 for i := 0 to (fOrder div 2) - 1 do
  begin
   x := Result;
   Result          := fAB[4 * i + 0] * x                            + fD64[2 * i];
   fD64[2 * i    ] := fAB[4 * i + 1] * x + fAB[4 * i + 2] * Result  + fD64[2 * i + 1];
   fD64[2 * i + 1] := fAB[4 * i + 0] * x + fAB[4 * i + 3] * Result;
  end;
end;
{$ENDIF}

function TAnalyseFilterHP.ProcessSampleASM: Double;
asm
 push ecx
 mov ecx, [self.fOrder]
 shl ecx,1
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.fAB+ecx*8].Double
  fadd [self.fD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.fAB+ecx*8+8].Double
  fadd [self.fD64+ecx*4+8].Double
  fld st(3)
  fmul [self.fAB+ecx*8+8].Double
  faddp
  fstp [self.fD64+ecx*4].Double
  fmul [self.fAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.fAB+ecx*8].Double
  faddp
  fstp [self.fD64+ecx*4+8].Double
 jnz @FilterLoop
 pop ecx
end;

procedure TAnalyseFilterHP.ResetState;
begin
 FillChar(fD64[0],10*SizeOf(Double),0);
end;

end.
