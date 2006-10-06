unit DFilter;

interface

type
  TComplex=record
             Re : Single;
             Im : Single;
            end;
  TCalcType = (tpPeak, tpLowShelf, tpHiShelf);
  TPNType=array[0..1] of TComplex;

  TFilter=class(TObject)
  private
    procedure SetSampleRate(const Value: Single);
  protected
    fGain        : Single;
    fGainSpeed   : Single;
    fFrequency   : Single;
    fSinW0, fW0  : Single;
    fSampleRate  : Single;
    fSRR         : Single; // reciprocal of fSampleRate
    procedure SetW0; virtual;
    procedure SetGain(s:Single); virtual;
    procedure SetFrequency(Value:Single); virtual;
    procedure CalculateCoefficients; virtual; abstract;
  public
    constructor Create; virtual;
    function Magnitude(f:Single):Single; virtual; abstract;
    function Phase(f:Single):Single; virtual; abstract;
  published
    property Gain: Single read fGain write SetGain;
    property Frequency: Single read fFrequency write SetFrequency;
    property SampleRate: Single read fSampleRate write SetSampleRate;
  end;

  TIIRFilter=class(TFilter)
  private
  protected
    fBandWidth   : Single;
    procedure SetBW(s:Single); virtual;
  public
  published
    property Bandwidth: Single read fBandWidth write SetBW;
  end;

  TBiquadIIRFilter=class(TIIRFilter)
  private
    fDenominator : array[1..2] of Single;
    fNominator   : array[0..2] of Double;
    fPoles       : TPNType;
    fZeros       : TPNType;
    fCalc        : Procedure of Object;
    fCalcType    : TCalcType;
  protected
//    a         : array[0..2] of Double;
//    b         : array[1..2] of Double;
    fState    : array[0..1] of Double;
    procedure CalcRBJ;
    procedure CalcLowShelf;
    procedure CalcHiShelf;
    procedure CalcPolesZeros;
    procedure Init;
    function GetPoles:TPNType;
    function GetZeros:TPNType;
    procedure SetCalcType(const value :TCalcType);
    procedure CalculateCoefficients; override;
  public
    constructor Create;
    function Process(s:single):single;
    function Magnitude(f:Single):Single; override;
    function Phase(f:Single):Single; override;
    property Poles: TPNType read fPoles; //GetPoles;
    property Zeros: TPNType read fZeros; //GetZeros;
  published
    property CalcType: TCalcType read fCalcType write SetCalcType;
  end;

  TSimplePeakFilter=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleLowShelfFilter=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleHighShelfFilter=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleLowpassFilter=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleHighpassFilter=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses math, DDSPBase;

function cf_phi(f,rate,a0,a1,a2,b1,b2:Double): Double;
const pihalf = pi*0.5; pia    = 1/pi;
var w : Double;
begin
 w:=(2*f*pi)/rate;
 Result:=((ArcTan2(
                  (
                    a0+a1*b1+a2*b2+
                   (a0*b1+a1*(1+b2)+a2*b1)*cos(w)+
                   (a0*b2+a2)*cos(2*w)
                  )
                  /
                  (
                    1+b1*b1+b2*b2+
                    2*
                    (
                     (b1+b1*b2)*cos(w)+ b2*cos(2*w)
                    )
                  )
                  ,
                  (
                   (
                     a1-a0*b1+a2*b1-a1*b2+
                     2*(-a0*b2+a2)*cos(w)
                   )*sin(w)
                  /
                   (
                     1+b1*b1+b2*b2+
                     2*(b1 + b1*b2)*cos(w)+
                     2*b2*cos(2*w)
                   )
                  )
                 )-pihalf)
                 *pia);
end;

{ TFilter }

constructor TFilter.Create;
begin
 fGain:=0; fGainSpeed:=1;
 fFrequency:=1000;
 SetW0;
end;

procedure TFilter.SetFrequency(Value:Single);
begin
 if fFrequency <> Value then
  begin
   if Value>0
    then fFrequency:=Value
    else fFrequency:=1;
   SetW0; 
   CalculateCoefficients;
  end;
end;

procedure TFilter.SetGain(s:Single);
begin
 fGain:=s;
 fGainSpeed:=dB_to_Amp(fGain);
 CalculateCoefficients;
end;

procedure TFilter.SetSampleRate(const Value: Single);
begin
 fSampleRate := Value;
 fSRR:= 1 / fSampleRate;
end;

procedure TFilter.SetW0;
begin
 fW0:=2*Pi*fFrequency*fSRR;
 fSinW0:=sin(fW0);
 if fW0>3.1 then fW0:=3.1;
end;

{ TIIRFilter }

procedure TIIRFilter.SetBW(s:Single);
begin
 if s=0
  then fBandWidth:=0.01
  else fBandWidth:=s;
 CalculateCoefficients;
end;

{ TBiquadIIRFilter }

constructor TBiquadIIRFilter.create;
begin
 inherited create;
 fCalc:=CalcRBJ;
 fGain:=0;
 fFrequency:=1000;
 fBandWidth:=1;
 fSampleRate:=44100;
 Init;
end;

function TBiquadIIRFilter.Magnitude(f:Single):Single;
var cw : Double;
begin
 cw:=2*cos(f*pi*fSRR);
 Result:=10*Log10( (( (fNominator[0]-fNominator[2])*
                      (fNominator[0]-fNominator[2])+
                       fNominator[1]*fNominator[1] +
                      (fNominator[0]*fNominator[1] +
                       fNominator[1]*fNominator[2] +
                       fNominator[0]*fNominator[2] * cw) * cw)
                     /
                     ((fDenominator[2]+1)*(fDenominator[2]+1) +
                       fDenominator[1]*fDenominator[1] +
                      (fDenominator[1]*(fDenominator[2]-1)-
                                     cw*fDenominator[2])*cw )));
end;

function TBiquadIIRFilter.Phase(f:Single):Single;
begin
 Result:=cf_phi(f,fSampleRate,fNominator[0],fNominator[1],fNominator[2],fDenominator[1],fDenominator[2]);
end;

procedure TBiquadIIRFilter.Init;
begin
 // initialize values
 fDenominator[1]:=0;
 fDenominator[2]:=0;
 fNominator[0]:=0;
 fNominator[1]:=0;
 fNominator[2]:=0;
 fcalc;
end;

function dB_to_Amp(g:single):single;
begin
 if (g>-90.0) then result:=math.power(10,g*0.05)
 else result:=0;
end;

procedure TBiquadIIRFilter.CalcRBJ;
var alpha,t,Amp,w : Double;
begin
 Amp:=math.Power(10,fGain/40);
 w:=2*PI*fFrequency*fSRR;
 alpha:=Sinh((Ln2/2)*fBandWidth*(w/Sin(w)))*Sin(w);
 t:=1/(1+alpha/Amp);
 fNominator[0]:=(1+alpha*Amp)*t;
 fNominator[1]:=-2*cos(w)*t;
 fNominator[2]:=(1-alpha*Amp)*t;
 fDenominator[1]:=fNominator[1];
 fDenominator[2]:=(1-alpha/Amp)*t;
 CalcPolesZeros;
end;

procedure TBiquadIIRFilter.CalcLowShelf;
var be,Amp,b0,w0,t,co,si : Double;
begin
 Amp:=math.Power(10,fGain/40);
 w0:=2*PI*fFrequency*fSRR;
 be:=sqrt(Amp)/0.7;
 co:=cos(w0);
 si:=sin(w0);

 b0:=1/((Amp+1.0)+(Amp-1.0)*co+be*si);
 fNominator[0]:=0.5*((Amp+1.0)-(Amp-1.0)*co+be*si);
 fNominator[1]:=(Amp-1.0)-(Amp+1.0)*co;
 fNominator[2]:=0.5*((Amp+1.0)-(Amp-1.0)*co-be*si);
 fDenominator[1]:=(-2.0*((Amp-1.0)+(Amp+1.0)*co))*b0;
 fDenominator[2]:=((Amp+1.0)+(Amp-1.0)*co-be*si)*b0;

 CalcPolesZeros;
end;

procedure TBiquadIIRFilter.CalcHiShelf;
var be,Amp,b0,w0,t,co,si : Double;
begin
 Amp:=math.Power(10,fGain/40);
 w0:=2*PI*fFrequency*fSRR;
 be:=sqrt(Amp)/0.7;
 co:=cos(w0);
 si:=sin(w0);

 b0:=1/((Amp+1.0)-(Amp-1.0)*co+be*si);
 fNominator[0]:=0.5*(Amp*((Amp+1.0)+(Amp-1.0)*co+be*si));
 fNominator[1]:=-Amp*((Amp-1.0)+(Amp+1.0)*co);
 fNominator[2]:=0.5*(Amp*((Amp+1.0)+(Amp-1.0)*co-be*si));
 fDenominator[1]:=(2.0*((Amp-1.0)-(Amp+1.0)*co))*b0;
 fDenominator[2]:=((Amp+1.0)-(Amp-1.0)*co-be*si)*b0;

 CalcPolesZeros;
end;

procedure TBiquadIIRFilter.CalcPolesZeros;
var p,q : Double;
    e   : Double;
begin
 p:=-fNominator[1]/(2*fNominator[0]);
 q:=(fNominator[2]/fNominator[0]);
 fZeros[0].Re:=p;
 fZeros[1].Re:=p;
 e:=q-(p*p);
 if e>0
  then
   begin
    fZeros[0].Im:=sqrt(e);
    fZeros[1].Im:=-sqrt(e);
   end
  else
   begin
    fZeros[0].Re:=fZeros[0].Re+sqrt(-e);
    fZeros[1].Re:=fZeros[0].Re-sqrt(-e);
    fZeros[0].Im:=0;
    fZeros[1].Im:=0;
   end;

 p:=-fDenominator[1]/2;
 q:=fDenominator[2];
 fPoles[0].Re:=p;
 fPoles[1].Re:=p;
 e:=q-(p*p);
 if e>0
  then
   begin
    fPoles[0].Im:=sqrt(e);
    fPoles[1].Im:=-sqrt(e);
   end
  else
   begin
    fPoles[0].Re:=fPoles[0].Re+sqrt(-e);
    fPoles[1].Re:=fPoles[0].Re-sqrt(-e);
    fPoles[0].Im:=0;
    fPoles[1].Im:=0;
   end;
end;

function TBiquadIIRFilter.Process(s:single):single;
begin
 result    := fNominator[0]*s + fState[0];
 fState[0] := fNominator[1]*s - fDenominator[1]*result + fState[1];
 fState[1] := fNominator[2]*s - fDenominator[2]*result;
end;

function TBiquadIIRFilter.GetPoles:TPNType;
var p,q : Single;
begin
 p:=fDenominator[1]/(2*fDenominator[2]);
 q:=(1/fDenominator[2]);
 Result[0].Re:=p;
 Result[1].Re:=p;
 Result[0].Im:=sqrt(q-(p*p));
 Result[1].Im:=-sqrt(q-(p*p));
end;

function TBiquadIIRFilter.GetZeros:TPNType;
var p,q : Single;
begin
 p:=fNominator[1]/(2*fNominator[2]);
 q:=(fNominator[0]/fNominator[2]);
 Result[0].Re:=p;
 Result[1].Re:=p;
 Result[0].Im:=sqrt(q-(p*p));
 Result[1].Im:=-sqrt(q-(p*p));
end;

procedure TBiquadIIRFilter.SetCalcType(const value :TCalcType);
begin
 if fCalcType<>Value then
  begin
   fCalcType:=Value;
   case fCalcType of
     tpPeak       : fCalc:=CalcRBJ;
     tpLowShelf   : fCalc:=CalcLowshelf;
     tpHiShelf    : fCalc:=CalcHishelf;
   end;
   fcalc;
  end;
end;

procedure TBiquadIIRFilter.CalculateCoefficients;
begin
 fCalc;
end;

{ TSimplePeakFilter }

procedure TSimplePeakFilter.CalculateCoefficients;
var aa, alpha,t,Amp,w : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;

 aa:=alpha/fGainSpeed;
 t:=1/(1+aa);
 fDenominator[2]:=-(aa-1)*t;
 fNominator[1]:=-2*cos(fW0)*t;
 fDenominator[1]:=fNominator[1];
 fNominator[0]:=(1+alpha*fGainSpeed)*t;
 fNominator[2]:=(1-alpha*fGainSpeed)*t;
 CalcPolesZeros;
end;

{ TSimpleLowShelfFilter }

procedure TSimpleLowShelfFilter.CalculateCoefficients;
var aa, alpha,t,Amp,w : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;

 aa:=alpha/fGainSpeed;
 t:=1/(1+aa);
 fDenominator[2]:=-(aa-1)*t;
 fNominator[1]:=-2*cos(fW0)*t;
 fDenominator[1]:=fNominator[1];
 fNominator[0]:=(1+alpha*fGainSpeed)*t;
 fNominator[2]:=(1-alpha*fGainSpeed)*t;
 CalcPolesZeros;
end;

{ TSimpleHighShelfFilter }

procedure TSimpleHighShelfFilter.CalculateCoefficients;
var cn,alpha,t   : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;
 t:=1/(1+alpha);
 cn:=cos(fW0);
 fNominator[0]:=fGainSpeed*fGainSpeed*(1+cn)*0.5*t;
 fNominator[1]:=-2*fNominator[0];
 fNominator[2]:=fNominator[0];
 fDenominator[1]:=-2*cn*t;
 fDenominator[2]:=(1-alpha)*t;
 CalcPolesZeros;
end;

{ TSimpleLowpassFilter }

procedure TSimpleLowpassFilter.CalculateCoefficients;
var aa, alpha,t,Amp,w : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;

 aa:=alpha/fGainSpeed;
 t:=1/(1+aa);
 fDenominator[2]:=-(aa-1)*t;
 fNominator[1]:=-2*cos(fW0)*t;
 fDenominator[1]:=fNominator[1];
 fNominator[0]:=(1+alpha*fGainSpeed)*t;
 fNominator[2]:=(1-alpha*fGainSpeed)*t;
 CalcPolesZeros;
end;

{ TSimpleHighpassFilter }

procedure TSimpleHighpassFilter.CalculateCoefficients;
var aa, alpha,t,Amp,w : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;

 aa:=alpha/fGainSpeed;
 t:=1/(1+aa);
 fDenominator[2]:=-(aa-1)*t;
 fNominator[1]:=-2*cos(fW0)*t;
 fDenominator[1]:=fNominator[1];
 fNominator[0]:=(1+alpha*fGainSpeed)*t;
 fNominator[2]:=(1-alpha*fGainSpeed)*t;
 CalcPolesZeros;
end;

end.
