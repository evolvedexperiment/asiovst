unit AnalyserChebyshevFilter;

interface

{$IFNDEF FPC}
{$DEFINE x87}
{$ENDIF}

type
  TChebyshevFilter = class(TObject)
  private
    function GetFrequency: Double;
    function GetGain: Double;
    function GetQ: Double;
    function GetSampleRate: Double;
    procedure SetDownsamplePower(Value: Integer);
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
    procedure SetW0; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    procedure SetGain(const Value: Double); virtual;
    procedure SetQ(const Value: Double); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRipple; virtual; abstract;

    property GainSpeed: Double read fGainSpeed;
    property SinW0: Double read fSinW0;
    property W0: Double read fW0;
  public
    constructor Create; virtual;
    function ProcessSample(const s:Double):Double; virtual; abstract;
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
  end;

  TChebyshevILP10Filter = class(TChebyshevFilter)
  private
    fA              : array [0..9] of Double;
    fB              : array [0..9] of Double;
    fD64            : array [0..9] of Double;
  protected
    procedure SetRipple; override;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

  TChebyshevIHP10Filter = class(TChebyshevFilter)
  private
    fA              : array [0..9] of Double;
    fB              : array [0..9] of Double;
    fD64            : array [0..9] of Double;
  protected
    procedure SetRipple; override;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

implementation

uses Math, DDSPBase, SysUtils;

constructor TChebyshevFilter.Create;
begin
 fDownsamplePow:=0;
 fDownsampleFak:=1;
 fFrequency:=0;
 fGain:=0;
 fQ:=1;
 SampleRate:=44100;
end;

function TChebyshevFilter.GetFrequency: Double;
begin
 Result:=fFrequency;
end;

function TChebyshevFilter.GetGain: Double;
begin
 Result:=fGain;
end;

function TChebyshevFilter.GetQ: Double;
begin
 Result:=fQ;
end;

procedure TChebyshevFilter.Reset;
begin
 fGain:=0;
 CalcCoefficients;
end;

procedure TChebyshevFilter.SetSampleRate(const Value: Double);
begin
 if Value=0 then Exit;
 if Value<>fSampleRate then
  begin
   fSampleRate := Value;
   fSRR:=1/fSampleRate;
  end;
end;

function TChebyshevFilter.GetSampleRate: Double;
begin
 result:=fSampleRate;
end;

procedure TChebyshevFilter.SetDownsamplePower(Value: Integer);
begin
 if Value<0 then Value:=0;
 if fDownsamplePow<>Value then
  begin
   fDownsamplePow := Value;
   fDownsampleFak := round(IntPower(2,fDownsamplePow));
   SetW0;
  end;
end;

procedure TChebyshevFilter.SetW0;
begin
 fW0:=2*Pi*fSRR*(fFrequency*fDownsampleFak);
 fSinW0:=sin(fW0);
 if fW0>3.1 then fW0:=3.1;
end;

procedure TChebyshevFilter.SetGain(const Value: Double);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fGain := Value;
 fGainSpeed:=Exp(fGain*ln10_0025);
end;

procedure TChebyshevFilter.SetFrequency(const Value: Double);
begin
 if fFrequency <> Value then
  begin
   SetW0;
   SetRipple;
  end;
end;

procedure TChebyshevFilter.SetQ(const Value: Double);
begin
 if Value<>fQ then
  begin
   fQ := Value;
   SetRipple;
  end;
end;

procedure TChebyshevFilter.SetFilterValues(const Frequency, Gain, Q: Single);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency:=Frequency; fGain:=Gain; fQ:=Q;
 fGainSpeed:=Exp((fGain*ln10_0025));
 SetW0;
 SetRipple;
end;

function TChebyshevFilter.Magnitude(f: Single): Single;
begin
 Result:=1;
end;

function TChebyshevFilter.MagnitudeLog10(f: Single): Single;
begin
 result:=20*Log10(Magnitude(f));
end;

{ TChebyshevILP10Filter }

constructor TChebyshevILP10Filter.Create;
begin
 inherited Create;
 fGainSpeed:=1;
end;

procedure TChebyshevILP10Filter.CalcCoefficients;
{$IFDEF x87}
const chalf : Double = 0.5;
      cOrder: Integer = 5;
      cOrderDiv : Double = 1/(4*5);
asm
 fld [self.fW0]                    // fW0
 fmul chalf                        // fW0/2
 fsincos                           // sin(fW0/2), cos(fW0/2)
 fdivp                             // K = tan(fW0*0.5)
 fld st(0)                         // K, K
 fmul st(0),st(0)                  // K², K
 fxch                              // K, K²

 mov ecx,cOrder
 @OrderLoop:
  mov edx,ecx                      // edx = i
  imul edx,2                       // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K²
  fldpi                            // Pi, 2*i+1, K, K²
  fmulp                            // Pi * (2*i+1), K, K²
  fmul cOrderDiv                   // Pi * (2*i+1) / (2*Order), K, K²
  fcos                             // cos((i*2+1)*Pi/(2*Order)) = t, K, K²
  fld st(0)                        // t, t, K, K²
  fmul st(0),st(0)                 // t²,t, K, K²
  fld [self.fRipple].Double        // fRipple[0], t²,t, K, K²
  fsubrp                           // fRipple[0] - t²,t, K, K²
  fld1                             // 1, fRipple[0] - t²,t, K, K²
  fdivrp                           // 1 / (fRipple[0] - t²) = t1, t, K, K²
  fxch                             // t, t1, K, K²
  fadd st(0),st(0)                 // 2*t, t1, K, K²
  fmul st(0),st(1)                 // 2*t * t1, t1, K, K²
  fmul [self.fRipple+8].Double     // fRipple[1]*2*t*t1, t1, K, K²
  fmul st(0), st(2)                // K*fRipple[1]*2*t*t1 = t2, t1, K, K²
  fld st(0)                        // t2, t2, t1, K, K²
  fadd st(0),st(2)                 // t1+t2, t2, t1, K, K²
  fadd st(0),st(4)                 // t1+t2+K², t2, t1, K, K²
  fld1                             // 1, t1+t2+K², t2, t1, K, K²
  fdivrp                           // (1/t1+t2+K²)=t, t2, t1, K, K²
  fld st(0)                        // t, t, t2, t1, K, K²
  fmul st(0),st(5)                 // t*K²=fA[2*i], t, t2, t1, K, K²
  fst [Self.fA+8*edx].Double       // store to fA[2*i]
  fadd st(0),st(0)                 // 2*fA[2*i], t, t2, t1, K, K²
  fstp [self.fA+8*edx+8].Double    // store to fA[2*i+1]

  fld st(2)                        // t1, t, t2, t1, K, K²
  fsub st(0),st(5)                 // t1-K², t, t2, t1, K, K²
  fadd st(0),st(0)                 // 2*(t1-K²), t, t2, t1, K, K²
  fmul st(0),st(1)                 // 2*(t1-K²)*t, t, t2, t1, K, K²
  fstp [self.fB+8*edx].Double      // store to fB[2*i]
  fxch                             // t2, t, t1, K, K²
  fsubrp st(2),st(0)               // t, t2-t1, K, K²
  fxch                             // t2-t1, t, K, K²
  fsub st(0),st(3)                 // t2-t1-K², t, K, K²
  fmulp                            // (t2-t1-K²) * t, K, K²
  fstp [self.fB+8*edx+8].Double    // store to fB[2*i+1]
 loop @OrderLoop
 fstp st(0)                        // K²
 fstp st(0)                        // stack free!

 fld [self.fA].Double              // load fA[0]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fA].Double             // store fA[0]
 fld [self.fA+8].Double            // load fA[1]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fA+8].Double           // store fA[1]
end;
{$ELSE}
var K,K2    : Double;
    t,t1,t2 : Double;
    i       : Integer;
begin
 K:=tan(fW0*0.5); K2:=K*K;

 for i:=0 to 4 do
  begin
   t :=cos( ((i*2+1)*Pi*0.05) );
   t1:=1/(fRipple[0]-(t*t));
   t2:=K*t1*fRipple[1]*(2*t);

   t:=     1/(t2+K2+t1);
   fB[2*i]   := 2*(-K2+t1)*t;
   fB[2*i+1] :=   (-K2-t1+t2)*t;
   fA[2*i]   :=t*K2;
   fA[2*i+1] :=2*fA[2*i];
  end;
 fA[8]:=fA[8]*fGainSpeed;
 fA[9]:=fA[9]*fGainSpeed;
end;
{$ENDIF}

function TChebyshevILP10Filter.Magnitude(f: Single): Single;
var cw,a : Double;
begin
 cw:=2*cos(2*pi*f*fSRR); a:=(4 + cw*(4+cw));
 Result:=Sqrt(fA[0]*fA[0]*fA[2]*fA[2]*fA[4]*fA[4]*fA[6]*fA[6]*fA[8]*fA[8]*a*a*a*a*a
         /(((1+fB[0]*fB[0]+fB[1]*(fB[1]+2)) + cw*(fB[0]*(fB[1]-1)-cw*fB[1]))*
           ((1+fB[2]*fB[2]+fB[3]*(fB[3]+2)) + cw*(fB[2]*(fB[3]-1)-cw*fB[3]))*
           ((1+fB[4]*fB[4]+fB[5]*(fB[5]+2)) + cw*(fB[4]*(fB[5]-1)-cw*fB[5]))*
           ((1+fB[6]*fB[6]+fB[7]*(fB[7]+2)) + cw*(fB[6]*(fB[7]-1)-cw*fB[7]))*
           ((1+fB[8]*fB[8]+fB[9]*(fB[9]+2)) + cw*(fB[8]*(fB[9]-1)-cw*fB[9]))));
end;

function TChebyshevILP10Filter.MagnitudeLog10(f: Single): Single;
var cw,a : Double;
begin
 cw:=2*cos(2*pi*f/SampleRate); a:=(4 + cw*(4+cw));
 Result:=10*Log10(fA[0]*fA[0]*fA[2]*fA[2]*fA[4]*fA[4]*fA[6]*fA[6]*fA[8]*fA[8]*a*a*a*a*a
         /(((1+fB[0]*fB[0]+fB[1]*(fB[1]+2)) + cw*(fB[0]*(fB[1]-1)-cw*fB[1]))*
           ((1+fB[2]*fB[2]+fB[3]*(fB[3]+2)) + cw*(fB[2]*(fB[3]-1)-cw*fB[3]))*
           ((1+fB[4]*fB[4]+fB[5]*(fB[5]+2)) + cw*(fB[4]*(fB[5]-1)-cw*fB[5]))*
           ((1+fB[6]*fB[6]+fB[7]*(fB[7]+2)) + cw*(fB[6]*(fB[7]-1)-cw*fB[7]))*
           ((1+fB[8]*fB[8]+fB[9]*(fB[9]+2)) + cw*(fB[8]*(fB[9]-1)-cw*fB[9]))));
end;

function TChebyshevILP10Filter.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 fld s.Double

 mov ecx, 10
 @FilterLoop:
  sub ecx,2
  fld st(0)
  fmul [self.fA + ecx*8].Double
  fadd [self.fD64 + ecx*8].Double
  fld st(0)
  fld st(0)
  fmul [self.fB + ecx*8].Double
  fadd [self.fD64 + ecx*8 + 8].Double
  fld st(3)
  fmul [self.fA + ecx*8 + 8].Double
  faddp
  fstp [self.fD64 + ecx*8].Double
  fmul [self.fB + ecx*8 + 8].Double
  fxch
  fxch st(2)
  fmul [self.fA + ecx*8].Double
  faddp
  fstp [self.fD64 + ecx*8 + 8].Double
 jnz @FilterLoop
end;
{$ELSE}
var a : Double;
begin
 a       := fA[0]*s                     + fD64[0];
 fD64[0] := fA[1]*s      + fB[0]*a      + fD64[1];
 fD64[1] := fA[0]*s      + fB[1]*a;
 Result  := fA[2]*a                     + fD64[2];
 fD64[2] := fA[3]*a      + fB[2]*Result + fD64[3];
 fD64[3] := fA[2]*a      + fB[3]*Result;
 a       := fA[4]*Result                + fD64[4];
 fD64[4] := fA[5]*Result + fB[4]*a      + fD64[5];
 fD64[5] := fA[4]*Result + fB[5]*a;
 Result  := fA[6]*a                     + fD64[6];
 fD64[6] := fA[7]*a      + fB[6]*Result + fD64[7];
 fD64[7] := fA[6]*a      + fB[7]*Result;
 a       := fA[8]*Result                + fD64[8];
 fD64[8] := fA[9]*Result + fB[8]*a      + fD64[9];
 fD64[9] := fA[8]*Result + fB[9]*a;
 Result  := a;
end;
{$ENDIF}

procedure TChebyshevILP10Filter.ResetState;
begin
 FillChar(fD64[0],10*SizeOf(Double),0);
end;

procedure TChebyshevILP10Filter.SetRipple;
var t : Double;
begin
 t:=arcsinh(10*fQ)*0.1;
 fRipple[1]:=sinh(t);
 fRipple[0]:=IntPower(cosh(t),2);
end;

{ TChebyshevIHP10Filter }

constructor TChebyshevIHP10Filter.Create;
begin
 inherited Create;
 fGainSpeed:=1;
end;

procedure TChebyshevIHP10Filter.CalcCoefficients;
{$IFDEF _x87}
const chalf : Double = 0.5;
      cOrder: Integer = 5;
      cOrderDiv : Double = 1/(8*5);
asm
 fld [self.fW0]                    // fW0
 fmul chalf                        // fW0/2
 fsincos                           // sin(fW0/2), cos(fW0/2)
 fdivp                             // K = tan(fW0*0.5)
 fld st(0)                         // K, K
 fmul st(0),st(0)                  // K², K
 fxch                              // K, K²

 mov ecx,cOrder
 @OrderLoop:
  mov edx,ecx                      // edx = i
  imul edx,2                       // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K²

  fldpi                            // Pi, 2*i+1, K, K²
  fmulp                            // Pi * (2*i+1), K, K²
  fmul cOrderDiv                   // Pi * (2*i+1) / (4*Order), K, K²
  fsin                             // sin((i*2+1)*Pi/(2*Order)) = t, K, K²
  fmul st(0),st(0)                 // sqr(sin((i*2+1)*Pi*0.025)), K, K²

  fld st(0)                        // t, t, K, K²
  fmul st(0),st(0)                 // t²,t, K, K²
  fadd st(0),st(0)                 // 2*t²,t, K, K²
  fadd st(0),st(0)                 // 4*t²,t, K, K²
  fld [self.fRipple].Double        // fRipple[0], 4*t²,t, K, K²
  fsubrp                           // fRipple[0] - 4*t²,t, K, K²
  fld st(2)                        // t, fRipple[0]-4*t²,t, K, K²
  fadd st(0),st(0)                 // 2*t,fRipple[0]-4*t²,t, K, K²
  fadd st(0),st(0)                 // 4*t,fRipple[0]-4*t²,t, K, K²
  faddp                            // fRipple[0]+4*t-4*t²,t, K, K²
  fld1                             // 1, fRipple[0]+4*t-4*t²,t, K, K²
  fsubp                            // fRipple[0]+4*t-4*t²-1,t, K, K²

  fdivrp                           // 1 / (fRipple[0]+4*t-4*t²-1) = t1, t, K, K²
  fxch                             // t, t1, K, K²
  fadd st(0),st(0)                 // 2*t, t1, K, K²

  fld1                             // 1, 2*t, t1, K, K²
  fsubpr                           // 1 - 2*t, t1, K, K²


 ToDo 
   t2:=K*t1*fRipple[1]*(1-2*t);

   fA[2*i]   := 1/(t2+1+t1*K2);
   fA[2*i+1] :=-2*fA[2*i];
   fB[2*i]   := 2*(   1-t1*K2)*fA[2*i];
   fB[2*i+1] :=   (t2-1-t1*K2)*fA[2*i];

  fmul st(0),st(1)                 // (1-2*t) * t1, t1, K, K²
  fmul [self.fRipple+8].Double     // fRipple[1]*2*t*t1, t1, K, K²
  fmul st(0), st(2)                // K*fRipple[1]*2*t*t1 = t2, t1, K, K²

  ToDo

  fld st(1)                        // t1, t2, t1, K, K²
  fmul st(0),st(4)                 // t1*K², t2, t1, K, K²
  fld1                             // 1, t1*K², t2, t1, K, K²
  faddp                            // 1+t1*K², t2, t1, K, K²
  fadd st(0),st(1)                 // 1+t1*K²+t2, t2, t1, K, K²
  fld1                             // 1, 1+t1*K²+t2, t2, t1, K, K²
  fdivrp                           // 1/(1+t1*K²+t2)=A[2*i], t2, t1, K, K²
  fst [Self.fA+8*edx].Double       // store to fA[2*i]
  fld st(0)                        // A[2*i], A[2*i], t2, t1, K, K²
  fadd st(0),st(0)                 // 2*A[2*i], A[2*i], t2, t1, K, K²
  fchs                             // -2*A[2*i], A[2*i], t2, t1, K, K²
  fstp [self.fA+8*edx+8].Double    // store to fA[2*i+1]

  fld st(2)                        // t1, A[2*i], t2, t1, K, K²
  fmul st(0),st(5)                 // t1*K², A[2*i], t2, t1, K, K²
  fld1                             // 1, t1*K², A[2*i], t2, t1, K, K²
  fsubrp                           // 1-t1*K², A[2*i], t2, t1, K, K²
  fadd st(0),st(0)                 // 2*(1-t1*K²), A[2*i], t2, t1, K, K²
  fmul st(0),st(1)                 // 2*(1-t1*K²)*A[2*i], A[2*i], t2, t1, K, K²
  fstp [self.fB+8*edx].Double      // store to fB[2*i]

  fxch st(2)                       // t1, t2, A[2*i], K, K²
  fmul st(0), st(4)                // t1*K², t2, A[2*i], K, K²
  fld1                             // 1, t1*K², t2, A[2*i], K, K²
  faddp                            // 1 + t1*K², t2, A[2*i], K, K²
  fsubp                            // t2 - (1 + t1*K²), A[2*i], K, K²
  fmulp                            // (t2 - (1 + t1*K²)) * A[2*i], K, K²
  fstp [self.fB+8*edx+8].Double    // store to fB[2*i+1]
 loop @OrderLoop
 fstp st(0)                        // K²
 fstp st(0)                        // stack free!

 fld [self.fA].Single              // load fA[0]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fA].Single             // store fA[0]
 fld [self.fA+4].Single            // load fA[1]
 fmul [self.fGainSpeed].Double     // apply fGainSpeed
 fstp [self.fA+4].Single           // store fA[1]
end;
{$ELSE}
var K,K2    : Double;
    t,t1,t2 : Double;
    i       : Integer;
begin
 K:=tan(fW0*0.5); K2:=K*K;
 for i:=0 to 4 do
  begin
   t :=sqr(sin((i*2+1)*Pi*0.025));

   t1:=1/(fRipple[0]+4*t-4*sqr(t)-1);
   t2:=K*t1*fRipple[1]*(1-2*t);

   fA[2*i]   := 1/(t2+1+t1*K2);
   fA[2*i+1] :=-2*fA[2*i];
   fB[2*i]   := 2*(   1-t1*K2)*fA[2*i];
   fB[2*i+1] :=   (t2-1-t1*K2)*fA[2*i];
  end;
 fA[8]:=fA[8]*fGainSpeed;
 fA[9]:=fA[9]*fGainSpeed;
end;
{$ENDIF}

function TChebyshevIHP10Filter.Magnitude(f: Single): Single;
var cw,a : Double;
begin
 cw:=2*cos(f*pi*fSRR); a:=(4 + cw*(4+cw));
 Result:=Sqrt(fA[0]*fA[0]*fA[2]*fA[2]*fA[4]*fA[4]*fA[6]*fA[6]*fA[8]*fA[8]*a*a*a*a*a
         /(((1+fB[0]*fB[0]+fB[1]*(fB[1]+2)) + cw*(fB[0]*(fB[1]-1)-cw*fB[1]))*
           ((1+fB[2]*fB[2]+fB[3]*(fB[3]+2)) + cw*(fB[2]*(fB[3]-1)-cw*fB[3]))*
           ((1+fB[4]*fB[4]+fB[5]*(fB[5]+2)) + cw*(fB[4]*(fB[5]-1)-cw*fB[5]))*
           ((1+fB[6]*fB[6]+fB[7]*(fB[7]+2)) + cw*(fB[6]*(fB[7]-1)-cw*fB[7]))*
           ((1+fB[8]*fB[8]+fB[9]*(fB[9]+2)) + cw*(fB[8]*(fB[9]-1)-cw*fB[9]))));
end;

function TChebyshevIHP10Filter.MagnitudeLog10(f: Single): Single;
var cw,a : Double;
begin
 cw:=2*cos(2*f*pi*fSRR); a:=(4 + cw*(cw-4));
 Result:=10*Log10(fA[0]*fA[0]*fA[2]*fA[2]*fA[4]*fA[4]*fA[6]*fA[6]*fA[8]*fA[8]*a*a*a*a*a
         /(((1+fB[0]*fB[0]+fB[1]*(fB[1]+2)) + cw*(fB[0]*(fB[1]-1)-cw*fB[1]))*
           ((1+fB[2]*fB[2]+fB[3]*(fB[3]+2)) + cw*(fB[2]*(fB[3]-1)-cw*fB[3]))*
           ((1+fB[4]*fB[4]+fB[5]*(fB[5]+2)) + cw*(fB[4]*(fB[5]-1)-cw*fB[5]))*
           ((1+fB[6]*fB[6]+fB[7]*(fB[7]+2)) + cw*(fB[6]*(fB[7]-1)-cw*fB[7]))*
           ((1+fB[8]*fB[8]+fB[9]*(fB[9]+2)) + cw*(fB[8]*(fB[9]-1)-cw*fB[9]))));
end;

function TChebyshevIHP10Filter.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 fld s.Double
 mov ecx, 10
 @FilterLoop:
  sub ecx,2
  fld st(0)
  fmul [self.fA + ecx*8].Double
  fadd [self.fD64 + ecx*8].Double
  fld st(0)
  fld st(0)
  fmul [self.fB + ecx*8].Double
  fadd [self.fD64 + ecx*8 + 8].Double
  fld st(3)
  fmul [self.fA + ecx*8 + 8].Double
  faddp
  fstp [self.fD64 + ecx*8].Double
  fmul [self.fB + ecx*8 + 8].Double
  fxch
  fxch st(2)
  fmul [self.fA + ecx*8].Double
  faddp
  fstp [self.fD64 + ecx*8 + 8].Double
 jnz @FilterLoop
end;
{$ELSE}
var a : Double;
begin
 a       := fA[0]*s                     + fD64[0];
 fD64[0] := fA[1]*s      + fB[0]*a      + fD64[1];
 fD64[1] := fA[0]*s      + fB[1]*a;
 Result  := fA[2]*a                     + fD64[2];
 fD64[2] := fA[3]*a      + fB[2]*Result + fD64[3];
 fD64[3] := fA[2]*a      + fB[3]*Result;
 a       := fA[4]*Result                + fD64[4];
 fD64[4] := fA[5]*Result + fB[4]*a      + fD64[5];
 fD64[5] := fA[4]*Result + fB[5]*a;
 Result  := fA[6]*a                     + fD64[6];
 fD64[6] := fA[7]*a      + fB[6]*Result + fD64[7];
 fD64[7] := fA[6]*a      + fB[7]*Result;
 a       := fA[8]*Result                + fD64[8];
 fD64[8] := fA[9]*Result + fB[8]*a      + fD64[9];
 fD64[9] := fA[8]*Result + fB[9]*a;
 Result  := a;
end;
{$ENDIF}

procedure TChebyshevIHP10Filter.ResetState;
begin
 FillChar(fD64[0],10*SizeOf(Double),0);
end;

procedure TChebyshevIHP10Filter.SetRipple;
var t : Double;
begin
 t:=arcsinh(10*fQ)*0.1;
 fRipple[1]:=2*sinh(t);
 fRipple[0]:=sqr(cosh(t));
end;

end.
