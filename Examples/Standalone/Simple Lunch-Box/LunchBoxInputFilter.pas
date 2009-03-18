unit LunchBoxInputFilter;

{$I DAV_Compiler.inc}

interface

uses
  DAV_Common;

type
  TInputFilter = class(TObject)
  private
    function GetFrequency: Double;
    function GetGain: Double;
    function GetQ: Double;
    function GetSampleRate: Double;
    procedure SetDownsamplePower(Value: Integer);
    procedure SetOrder(const Value: Integer);
  protected
    FFrequency      : Double;
    FGain, FQ       : Double;
    FSampleRate     : Double;
    FSRR            : Double;
    FSinW0, FW0     : Double;
    FGainSpeed      : Double;
    FRipple         : TDAV2DoubleArray;
    FDownsamplePow  : Integer;
    FDownsampleFak  : Integer;
    FOrder          : Integer;
    FAB             : array [0..127] of Double;
    FD64            : array [0.. 63] of Double;
    procedure SetW0; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    procedure SetGain(const Value: Double); virtual;
    procedure SetQ(const Value: Double); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRipple; virtual;

    property GainSpeed: Double read FGainSpeed;
    property SinW0: Double read FSinW0;
    property W0: Double read FW0;
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
    property SampleRateRez : Double read FSRR;
    property Frequency : Double read GetFrequency write SetFrequency;
    property Gain : Double read GetGain write SetGain;
    property Bandwidth : Double read GetQ write SetQ;
    property DownsampleAmount : Integer read FDownsamplePow write SetDownsamplePower;
    property DownsampleFaktor : Integer read FDownsampleFak;
    property Order : Integer read FOrder write SetOrder;
  end;

  TInputFilterLP = class(TInputFilter)
  private
  protected
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

  TInputFilterHP = class(TInputFilter)
  private
  protected
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure CalcCoefficients; override;
    function ProcessSample(const s:Double):Double; override;
    function Magnitude(f: Single): Single; override;
    function MagnitudeLog10(f: Single): Single; override;
  end;

implementation

uses Math, SysUtils;

constructor TInputFilter.Create;
begin
 FDownsamplePow:=0;
 FDownsampleFak:=1;
 FFrequency:=0;
 FGain:=0; FQ:=1;
 FOrder:=10;
 SampleRate:=44100;
end;

function TInputFilter.GetFrequency: Double;
begin
 Result:=FFrequency;
end;

function TInputFilter.GetGain: Double;
begin
 Result:=FGain;
end;

function TInputFilter.GetQ: Double;
begin
 Result:=FQ;
end;

procedure TInputFilter.Reset;
begin
 FGain:=0;
 CalcCoefficients;
end;

procedure TInputFilter.SetSampleRate(const Value: Double);
begin
 if Value=0 then Exit;
 if Value<>FSampleRate then
  begin
   FSampleRate := Value;
   FSRR:=1/FSampleRate;
  end;
end;

function TInputFilter.GetSampleRate: Double;
begin
 result:=FSampleRate;
end;

procedure TInputFilter.SetDownsamplePower(Value: Integer);
begin
 if Value<0 then Value:=0;
 if FDownsamplePow<>Value then
  begin
   FDownsamplePow := Value;
   FDownsampleFak := round(IntPower(2,FDownsamplePow));
   SetW0;
  end;
end;

procedure TInputFilter.SetW0;
begin
 FW0:=2*Pi*FSRR*(FFrequency*FDownsampleFak);
 FSinW0:=sin(FW0);
 if FW0>3.1 then FW0:=3.1;
end;

procedure TInputFilter.SetGain(const Value: Double);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 FGain := Value;
 FGainSpeed:=Exp(FGain*ln10_0025);
end;

procedure TInputFilter.SetOrder(const Value: Integer);
begin
 FOrder := Value;
 CalcCoefficients;
end;

procedure TInputFilter.SetFrequency(const Value: Double);
begin
 if FFrequency <> Value then
  begin
   SetW0;
   SetRipple;
  end;
end;

procedure TInputFilter.SetQ(const Value: Double);
begin
 if Value<>FQ then
  begin
   FQ := Value;
   SetRipple;
  end;
end;

procedure TInputFilter.SetRipple;
var t : Double;
begin
 t:=arcsinh(10*FQ)*0.1;
 FRipple[1]:=sinh(t);
 FRipple[0]:=sqr(cosh(t));
end;

procedure TInputFilter.SetFilterValues(const Frequency, Gain, Q: Single);
const ln10_0025 : Double = 5.7564627325E-2;
begin
 FFrequency:=Frequency; FGain:=Gain; FQ:=Q;
 FGainSpeed:=Exp((FGain*ln10_0025));
 SetW0;
 SetRipple;
end;

function TInputFilter.Magnitude(f: Single): Single;
begin
 Result:=1;
end;

function TInputFilter.MagnitudeLog10(f: Single): Single;
begin
 result:=20*Log10(Magnitude(f));
end;

{ TInputFilterLP }

constructor TInputFilterLP.Create;
begin
 inherited Create;
 FGainSpeed:=1;
end;

procedure TInputFilterLP.CalcCoefficients;
{$IFDEF x87}
const chalf : Double = 0.5;
asm
 fld1
 fild [self.FOrder]                // FOrder, 1
 fadd st(0),st(0)                  // 2*FOrder, 1
 fdivp                             // 1/2*FOrder

 fld [self.FW0]                    // FW0, 1/2*FOrder
 fmul chalf                        // FW0/2, 1/2*FOrder
 fsincos                           // sin(FW0/2), cos(FW0/2), 1/2*FOrder
 fdivp                             // K = tan(FW0*0.5), 1/2*FOrder
 fld st(0)                         // K, K, 1/2*FOrder
 fmul st(0),st(0)                  // K², K, 1/2*FOrder
 fxch                              // K, K², 1/2*FOrder

 mov ecx,[self.FOrder]             // ecx = order
 shr ecx,1                         // ecx = order div 2
 @OrderLoop:
  mov edx,ecx                      // edx = i
  imul edx,2                       // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  shl edx,1                        // edx = 4*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K², 1/2*FOrder
  fldpi                            // Pi, 2*i+1, K, K², 1/2*FOrder
  fmulp                            // Pi * (2*i+1), K, K², 1/2*FOrder
  fmul st(0),st(3)                 // Pi * (2*i+1)/(2*Order), K, K², 1/2*FOrder
  fcos                             // cos((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*FOrder
  fld st(0)                        // t, t, K, K², 1/2*FOrder
  fmul st(0),st(0)                 // t²,t, K, K², 1/2*FOrder
  fld [self.FRipple].Double        // FRipple[0], t²,t, K, K², 1/2*FOrder
  fsubrp                           // FRipple[0] - t²,t, K, K², 1/2*FOrder
  fld1                             // 1, FRipple[0] - t²,t, K, K², 1/2*FOrder
  fdivrp                           // 1 / (FRipple[0] - t²) = t1, t, K, K², 1/2*FOrder
  fxch                             // t, t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*t, t1, K, K², 1/2*FOrder
  fmul st(0),st(1)                 // 2*t * t1, t1, K, K², 1/2*FOrder
  fmul [self.FRipple+8].Double     // FRipple[1]*2*t*t1, t1, K, K², 1/2*FOrder
  fmul st(0), st(2)                // K*FRipple[1]*2*t*t1 = t2, t1, K, K², 1/2*FOrder
  fld st(0)                        // t2, t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(2)                 // t1+t2, t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(4)                 // t1+t2+K², t2, t1, K, K², 1/2*FOrder
  fld1                             // 1, t1+t2+K², t2, t1, K, K², 1/2*FOrder
  fdivrp                           // (1/t1+t2+K²)=t, t2, t1, K, K², 1/2*FOrder
  fld st(0)                        // t, t, t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(5)                 // t*K²=fA[2*i], t, t2, t1, K, K², 1/2*FOrder
  fst [Self.FAB+8*edx].Double      // store to fA[2*i], 1/2*FOrder
  fadd st(0),st(0)                 // 2*fA[2*i], t, t2, t1, K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+8].Double   // store to fA[2*i+1], 1/2*FOrder

  fld st(2)                        // t1, t, t2, t1, K, K², 1/2*FOrder
  fsub st(0),st(5)                 // t1-K², t, t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*(t1-K²), t, t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(1)                 // 2*(t1-K²)*t, t, t2, t1, K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+16].Double  // store to fB[2*i], 1/2*FOrder
  fxch                             // t2, t, t1, K, K², 1/2*FOrder
  fsubrp st(2),st(0)               // t, t2-t1, K, K², 1/2*FOrder
  fxch                             // t2-t1, t, K, K², 1/2*FOrder
  fsub st(0),st(3)                 // t2-t1-K², t, K, K², 1/2*FOrder
  fmulp                            // (t2-t1-K²) * t, K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+24].Double  // store to fB[2*i+1], 1/2*FOrder
 loop @OrderLoop
 fstp st(0)                        // K², 1/2*FOrder
 fstp st(0)                        // 1/2*FOrder
 fstp st(0)                        // stack free!

 fld [self.FAB].Double             // load fA[0]
 fmul [self.FGainSpeed].Double     // apply FGainSpeed
 fstp [self.FAB].Double            // store fA[0]
 fld [self.FAB+8].Double           // load fA[1]
 fmul [self.FGainSpeed].Double     // apply FGainSpeed
 fstp [self.FAB+8].Double          // store fA[1]
end;
{$ELSE}
var K,K2    : Double;
    t,t1,t2 : Double;
    i       : Integer;
begin
 K:=tan(FW0*0.5); K2:=K*K;
 for i:=(FOrder div 2)-1 downto 0 do
  begin
   t :=cos( ((i*2+1)*Pi*0.05) );
   t1:=1/(FRipple[0]-(t*t));
   t2:=K*t1*FRipple[1]*(2*t);
   t:=     1/(t2+K2+t1);
   FAB[4*i]   := K2*t;
   FAB[4*i+1] := 2*FAB[4*i];
   FAB[4*i+2] := 2*(-K2+t1)*t;
   FAB[4*i+3] :=   (-K2-t1+t2)*t;
  end;
 FAB[0]:=FAB[0]*FGainSpeed;
 FAB[1]:=FAB[1]*FGainSpeed;
end;
{$ENDIF}

function TInputFilterLP.Magnitude(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(f*pi*FSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*FAB[4*i]*FAB[4*i]*a/(1+sqr(FAB[4*i+2])+sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result:=sqrt(Result);
end;

function TInputFilterLP.MagnitudeLog10(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(f*pi*FSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*FAB[4*i]*FAB[4*i]*a/(1+sqr(FAB[4*i+2])+sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result:=10*Log10(Result);
end;

function TInputFilterLP.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.FOrder]
 shl ecx, 1
 fld s.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.FAB+ecx*8].Double
  fadd [self.FD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.FAB+ecx*8+16].Double
  fadd [self.FD64+ecx*4+8].Double
  fld st(3)
  fmul [self.FAB+ecx*8+8].Double
  faddp
  fstp [self.FD64+ecx*4].Double
  fmul [self.FAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.FAB+ecx*8].Double
  faddp
  fstp [self.FD64+ecx*4+8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y,x : Double;
  i   : Integer;
begin
 Result:=s;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x:=Result;
   Result      := FAB[4*i+0]*x                      + FD64[2*i];
   FD64[2*i  ] := FAB[4*i+1]*x + FAB[4*i+2]*Result  + FD64[2*i+1];
   FD64[2*i+1] := FAB[4*i+0]*x + FAB[4*i+3]*Result;
  end;
end;
{$ENDIF}

procedure TInputFilterLP.ResetState;
begin
 FillChar(FD64[0],10*SizeOf(Double),0);
end;

{ TInputFilterHP }

constructor TInputFilterHP.Create;
begin
 inherited Create;
 FGainSpeed:=1;
end;

procedure TInputFilterHP.CalcCoefficients;
{$IFDEF x87}
const chalf : Double = 0.5;
asm
 fld1
 fild [self.FOrder]                // FOrder, 1
 fadd st(0),st(0)                  // 2*FOrder, 1
 fadd st(0),st(0)                  // 4*FOrder, 1
 fdivp                             // 1/2*FOrder

 fld [self.FW0]                    // FW0, 1/2*FOrder
 fmul chalf                        // FW0/2, 1/2*FOrder
 fsincos                           // sin(FW0/2), cos(FW0/2), 1/2*FOrder
 fdivp                             // K = tan(FW0*0.5), 1/2*FOrder
 fld st(0)                         // K, K, 1/2*FOrder
 fmul st(0),st(0)                  // K², K, 1/2*FOrder
 fxch                              // K, K², 1/2*FOrder

 mov ecx,[self.FOrder]             // ecx = order
 @OrderLoop:
  mov edx,ecx                      // edx = 2*i
  dec edx                          // edx = 2*i+1
  mov [esp-4],edx                  // edx to stack
  dec edx                          // edx = 2*i
  shl edx,1                        // edx = 4*i
  fild [esp-4].Integer             // edx in st(0) = 2*i+1, K, K²
  fldpi                            // Pi, 2*i+1, K, K², 1/2*FOrder
  fmulp                            // Pi * (2*i+1), K, K², 1/2*FOrder
  fmul st(0),st(3)                 // Pi * (2*i+1) / (4*Order), K, K², 1/2*FOrder
  fsin                             // sin((i*2+1)*Pi/(2*Order)) = t, K, K², 1/2*FOrder
  fmul st(0),st(0)                 // sqr(sin((i*2+1)*Pi*0.025)), K, K², 1/2*FOrder

  fld st(0)                        // t, t, K, K², 1/2*FOrder
  fmul st(0),st(0)                 // t²,t, K, K², 1/2*FOrder
  fld st(1)                        // t, t²,t, K, K², 1/2*FOrder
  fsubrp                           // t-t²,t, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*(t-t²),t, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 4*(t-t²),t, K, K², 1/2*FOrder
  fld [self.FRipple].Double        // FRipple[0], 4*t²,t, K, K², 1/2*FOrder
  faddp                            // FRipple[0] - 4*t²,t, K, K², 1/2*FOrder
  fld1                             // 1, FRipple[0]+4*t-4*t²,t, K, K², 1/2*FOrder
  fsub st(1),st(0)                 // 1, FRipple[0]+4*t-4*t²-1,t, K, K², 1/2*FOrder
  fdivrp                           // 1 / (FRipple[0]+4*t-4*t²-1) = t1, t, K, K², 1/2*FOrder

  fxch                             // t, t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*t, t1, K, K², 1/2*FOrder
  fld1                             // 1, 2*t, t1, K, K², 1/2*FOrder
  fsubrp                           // 1 - 2*t, t1, K, K², 1/2*FOrder
  fmul [self.FRipple+8].Double     // FRipple[1]*(1-2*t), t1, K, K², 1/2*FOrder
  fmul st(0),st(2)                 // K*FRipple[1]*(1-2*t), t1, K, K², 1/2*FOrder
  fmul st(0),st(1)                 // t1*K*FRipple[1]*(1-2*t), t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // t2=2*t1*K*FRipple[1]*(1-2*t), t1, K, K², 1/2*FOrder
  fld st(1)                        // t1, t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(4)                 // t1*K², t2, t1, K, K², 1/2*FOrder
  fld1                             // 1, t1*K², t2, t1, K, K², 1/2*FOrder
  faddp                            // 1+t1*K², t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(1)                 // 1+t1*K²+t2, t2, t1, K, K², 1/2*FOrder
  fld1                             // 1, 1+t1*K²+t2, t2, t1, K, K², 1/2*FOrder
  fdivrp                           // 1/(1+t1*K²+t2)=A[2*i], t2, t1, K, K², 1/2*FOrder
  fst [Self.FAB+8*edx].Double      // store to fA[2*i]

  fld st(0)                        // A[2*i], A[2*i], t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*A[2*i], A[2*i], t2, t1, K, K², 1/2*FOrder
  fchs                             // -2*A[2*i], A[2*i], t2, t1, K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+8].Double    // store to fA[2*i+1]

  fld st(2)                        // t1, A[2*i], t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(5)                 // t1*K², A[2*i], t2, t1, K, K², 1/2*FOrder
  fld1                             // 1, t1*K², A[2*i], t2, t1, K, K², 1/2*FOrder
  fsubrp                           // 1-t1*K², A[2*i], t2, t1, K, K², 1/2*FOrder
  fadd st(0),st(0)                 // 2*(1-t1*K²), A[2*i], t2, t1, K, K², 1/2*FOrder
  fmul st(0),st(1)                 // 2*(1-t1*K²)*A[2*i], A[2*i], t2, t1, K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+16].Double  // store to fB[2*i]

  fxch st(2)                       // t1, t2, A[2*i], K, K², 1/2*FOrder
  fmul st(0), st(4)                // t1*K², t2, A[2*i], K, K², 1/2*FOrder
  fld1                             // 1, t1*K², t2, A[2*i], K, K², 1/2*FOrder
  faddp                            // 1 + t1*K², t2, A[2*i], K, K², 1/2*FOrder
  fsubp                            // t2 - (1 + t1*K²), A[2*i], K, K², 1/2*FOrder
  fmulp                            // (t2 - (1 + t1*K²)) * A[2*i], K, K², 1/2*FOrder
  fstp [self.FAB+8*edx+24].Double  // store to fB[2*i+1], 1/2*FOrder
  sub ecx,2
 jnz @OrderLoop
 fstp st(0)                        // K², 1/2*FOrder
 fstp st(0)                        // 1/2*FOrder
 fstp st(0)                        // stack free!

 fld [self.FAB].Double             // load fA[0]
 fmul [self.FGainSpeed].Double     // apply FGainSpeed
 fstp [self.FAB].Double            // store fA[0]
 fld [self.FAB+8].Double           // load fA[1]
 fmul [self.FGainSpeed].Double     // apply FGainSpeed
 fstp [self.FAB+8].Double          // store fA[1]
end;
{$ELSE}
var K,K2    : Double;
    t,t1,t2 : Double;
    i       : Integer;
begin
 K:=tan(FW0*0.5); K2:=K*K;
 for i:=(FOrder div 2)-1 downto 0 do
  begin
   t :=sqr( sin( ((i*2+1)*Pi/(4*FOrder)) ) );
   t1:=1/(FRipple[0]+4*t-4*sqr(t)-1);
   t2:=2*K*t1*FRipple[1]*(1-2*t);
   FAB[4*i]   := 1/(t2+1+t1*K2);
   FAB[4*i+1] :=-2*FAB[4*i];
   FAB[4*i+2] := 2*(   1-t1*K2)*FAB[4*i];
   FAB[4*i+3] :=   (t2-1-t1*K2)*FAB[4*i];
  end;
 FAB[0]:=FAB[0]*FGainSpeed;
 FAB[1]:=FAB[1]*FGainSpeed;
end;
{$ENDIF}

function TInputFilterHP.Magnitude(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(f*pi*FSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*FAB[4*i]*FAB[4*i]*a/(1+sqr(FAB[4*i+2])+sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result:=sqrt(Result);
end;

function TInputFilterHP.MagnitudeLog10(f: Single): Single;
var
  i    : Integer;
  a,cw : Double;
begin
 cw:=2*cos(f*pi*FSRR); a:=sqr(cw-2);
 Result:=1;

 for i := 0 to (FOrder div 2) - 1
  do Result:=Result*FAB[4*i]*FAB[4*i]*a/(1+sqr(FAB[4*i+2])+sqr(FAB[4*i+3])+2*FAB[4*i+3]+cw*((FAB[4*i+2]-cw)*FAB[4*i+3]-FAB[4*i+2]));
 Result:=10*log10(Result);
end;

function TInputFilterHP.ProcessSample(const s: Double): Double;
{$IFDEF x87}
asm
 mov ecx, [self.FOrder]
 shl ecx, 1
 fld s.Double;
 @FilterLoop:
  sub ecx,4
  fld st(0)
  fmul [self.FAB+ecx*8].Double
  fadd [self.FD64+ecx*4].Double
  fld st(0)
  fld st(0)
  fmul [self.FAB+ecx*8+16].Double
  fadd [self.FD64+ecx*4+8].Double
  fld st(3)
  fmul [self.FAB+ecx*8+8].Double
  faddp
  fstp [self.FD64+ecx*4].Double
  fmul [self.FAB+ecx*8+24].Double
  fxch
  fxch st(2)
  fmul [self.FAB+ecx*8].Double
  faddp
  fstp [self.FD64+ecx*4+8].Double
 jnz @FilterLoop
end;
{$ELSE}
var
  y,x : Double;
  i   : Integer;
begin
 Result:=s;
 for i := 0 to (FOrder div 2) - 1 do
  begin
   x:=Result;
   Result      := FAB[4*i+0]*x                      + FD64[2*i];
   FD64[2*i  ] := FAB[4*i+1]*x + FAB[4*i+2]*Result  + FD64[2*i+1];
   FD64[2*i+1] := FAB[4*i+0]*x + FAB[4*i+3]*Result;
  end;
end;
{$ENDIF}

procedure TInputFilterHP.ResetState;
begin
 FillChar(FD64[0],10*SizeOf(Double),0);
end;

end.
