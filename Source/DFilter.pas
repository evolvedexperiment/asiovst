unit DFilter;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

type
  TComplex=record
             Re : Single;
             Im : Single;
            end;
  TPNType=array[0..1] of TComplex;

  TFilter=class(TObject)
  private
  protected
    fGain        : Double;
    fGainSpeed   : Double;
    fFrequency   : Double;
    fSinW0, fW0  : Double;
    fSampleRate  : Double;
    fSRR         : Double; // reciprocal of fSampleRate
    procedure SetW0; virtual;
    procedure SetGain(const Value:Double); virtual;
    procedure SetFrequency(const Value:Double); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure CalculateCoefficients; virtual; abstract;
  public
    constructor Create; virtual;
    function ProcessSample(Input:Single):Single; virtual; abstract;
    function Magnitude(Frequency:Double):Double; virtual; abstract;
    function Phase(Frequency:Double):Double; virtual; abstract;
    procedure ResetStates; virtual; abstract;
    procedure Reset; virtual; abstract;
    property GainSpeed: Double read fGainSpeed;
    property SampleRateReci : Double read fSRR;
    property SinW0: Double read fSinW0;
    property W0: Double read fW0;
  published
    property Gain: Double read fGain write SetGain;
    property Frequency: Double read fFrequency write SetFrequency;
    property SampleRate: Double read fSampleRate write SetSampleRate;
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
  protected
    fState    : array[0..1] of Double;
    procedure CalcPolesZeros; virtual;
    function GetPoles:TPNType;
    function GetZeros:TPNType;
  public
    constructor Create; override;
    procedure ResetStates; override;
    function ProcessSample(Input:Single):Single; override;
    function Magnitude(Frequency:Double):Double; override;
    function Phase(Frequency:Double):Double; override;
    procedure Reset; override;
    property Poles: TPNType read fPoles; //GetPoles;
    property Zeros: TPNType read fZeros; //GetZeros;
  published
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

  TSimpleBandpass=class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

implementation

uses math, DDSPBase;

function cf_phi(f,rate,a0,a1,a2,b1,b2:Double): Double;
const pia = 1/pi;
var w : Double;
begin
 w:=(2*f*pi)/rate;
 Result:=ArcTan2((
                   a0+a1*b1+a2*b2+
                  (a0*b1+a1*(1+b2)+a2*b1)*cos(w)+
                  (a0*b2+a2)*cos(2*w))
                /
                ( 1+b1*b1+b2*b2+
                  2*((b1+b1*b2)*cos(w)+ b2*cos(2*w)))
                ,
               ((a1-a0*b1+a2*b1-a1*b2+
                   2*(-a0*b2+a2)*cos(w))*sin(w)
                /
                 ( 1+b1*b1+b2*b2+
                   2*(b1 + b1*b2)*cos(w)+
                   2*b2*cos(2*w)))
               )*pia-0.5;
end;

{ TFilter }

constructor TFilter.Create;
begin
 fGain:=0; fGainSpeed:=1;
 fFrequency:=1000;
 SetW0;
end;

procedure TFilter.SetFrequency(const Value:Double);
begin
 if fFrequency <> Value then
  begin
   if Value>0
    then fFrequency:=Value
    else fFrequency:=1;
  end;
 SetW0;
 CalculateCoefficients;
end;

procedure TFilter.SetGain(const Value:Double);
begin
 fGain:=Value;
 fGainSpeed:=dB_to_Amp(fGain);
 CalculateCoefficients;
end;

procedure TFilter.SetSampleRate(const Value: Double);
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
 fGain:=0;
 fFrequency:=1000;
 fBandWidth:=1;
 fSampleRate:=44100;
 fSRR:=1/44100;
 ResetStates;
end;

function TBiquadIIRFilter.Magnitude(Frequency:Double):Double;
var cw : Double;
begin
 cw:=2*cos(Frequency*pi*fSRR);
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

function TBiquadIIRFilter.Phase(Frequency:Double):Double;
const pihalf = pi*0.5;
      pia    = 1/pi;
var cw, sw, den : Double;
begin
 GetSinCos(Frequency*pi*fSRR,sw,cw);
 den:=1+sqr(fDenominator[1])+sqr(fDenominator[2])-2*fDenominator[2]+
      2*cw*(fDenominator[1]*(1+fDenominator[2])+2*fDenominator[2]*cw);
 Result:=ArcTan2(
                  (
                    fNominator[0]+fNominator[1]*fDenominator[1]+fNominator[2]*fDenominator[2]+
                   (fNominator[0]*fDenominator[1]+fNominator[1]*(1+fDenominator[2])+fNominator[2]*fDenominator[1])*cw+
                   (fNominator[0]*fDenominator[2]+fNominator[2])*(2*cw*cw-1)
                  )/den,
                  (
                    fNominator[1]-fNominator[0]*fDenominator[1]+
                    fNominator[2]*fDenominator[1]-fNominator[1]*fDenominator[2]+
                     2*(-fNominator[0]*fDenominator[2]+fNominator[2])*cw
                   )*sw
                  /den
                )/Pi-0.5;
end;

procedure TBiquadIIRFilter.Reset;
begin
 Gain:=0;
end;

procedure TBiquadIIRFilter.ResetStates;
begin
 fState[0]:=0;
 fState[1]:=0;
end;

function dB_to_Amp(g:single):single;
begin
 if (g>-90.0) then result:=math.power(10,g*0.05)
 else result:=0;
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

function TBiquadIIRFilter.ProcessSample(Input:Single):Single;
begin
 result    := fNominator[0]*Input + fState[0];
 fState[0] := fNominator[1]*Input - fDenominator[1]*result + fState[1];
 fState[1] := fNominator[2]*Input - fDenominator[2]*result;
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

{ TSimplePeakFilter }

procedure TSimplePeakFilter.CalculateCoefficients;
var aa,alpha,t : Double;
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
var aa, alpha,t : Double;
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
var aa, alpha,t : Double;
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
var aa, alpha,t : Double;
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

{ TSimpleBandpass }

procedure TSimpleBandpass.CalculateCoefficients;
var alpha,t : Double;
begin
 if (fSinW0=0)
  then alpha:=fSinW0/(2*fBandWidth)
  else alpha:=Sinh(ln22*cos(fW0*0.43)*fBandWidth*(fW0/fSinW0))*fSinW0;

 t:=1/(1+alpha);
 fNominator[0]:=fGainSpeed*fGainSpeed*alpha*t;
 fNominator[2]:=-fNominator[0];
 fDenominator[1]:=-2*cos(fW0)*t;
 fDenominator[2]:=(1-alpha)*t;
 fNominator[1]:=0;
end;

end.
