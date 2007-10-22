unit DPhaser;

{$I ASIOVST.INC}

interface

type
  TLFOSine = class(TObject)
  protected
    iSpeed     : Integer;
    fSpeed     : Single;
    fMax, fMin : Single;
    fValue     : Single;
    fPos       : Integer;
    fScale     : Single;
    fPosMul    : Single;
    fHalfScale : Single;
    function GetValue:Single; virtual;
    procedure SetMin(v:Single);
    procedure SetMax(v:Single);
    procedure SetSpeed(v:Single);
  public
    constructor Create;
  published
    property Value:Single read GetValue;
    property Speed:Single read FSpeed Write SetSpeed;
    property Min:Single read FMin write SetMin;
    property Max:Single read FMax Write SetMax;
  end;

  TLFOTriangle = class(TObject)
  protected
    iSpeed     : Integer;
    fSpeed     : Single;
    fMax, fMin : Single;
    fValue     : Single;
    fPos       : Integer;
    fScale     : Single;
    fPosMul    : Single;
    fHalfScale : Single;
    function GetValue:Single; virtual;
    procedure SetMin(v:Single);
    procedure SetMax(v:Single);
    procedure SetSpeed(v:Single);
  public
    constructor Create;
  published
    property Value:Single read GetValue;
    property Speed:Single read FSpeed Write SetSpeed;
    property Min:Single read FMin write SetMin;
    property Max:Single read FMax Write SetMax;
  end;

  TMasterAllPass=class(TObject)
  private
    fA1 : Single;
    fDelay : Single;
    fStages : Integer;
    fY : array [0..31] of Single;
    fSampleRate : Single;
    procedure SetDelay(v:Single);
  public
    constructor Create;
    destructor Destroy; override;
    function Process(const x:single):single;
    property Delay: Single read fDelay write SetDelay;
    property Stages: Integer read fStages write fStages;
    property SampleRate : Single read fSampleRate write fSampleRate;
  end;

  TPhaser=class(TObject)
  private
    fZM1 : Single;
    fDepth : Single;
    fLFO: TLFOSine;
    fLFOPhase : Single;
    fFeedBack : Single;
    fRate : Single;
    fMinimum: Single;
    fMaximum: Single;
    fMin: Single;
    fMax: Single;
    fSampleRate : Single;
    fMasterAllPass: TMasterAllPass;
    procedure SetSampleRate(v:Single);
    procedure SetMinimum(v:Single);
    procedure SetMaximum(v:Single);
    procedure SetRate(v:Single);
    procedure SetStages(v:Integer);
    function GetStages:Integer;
    procedure Calculate;
  public
    constructor Create;
    destructor Destroy; override;
    function Process(const x:single):single;
    property SampleRate : Single read fSampleRate write SetSampleRate;
    property Depth: Single read fDepth write fDepth; //0..1
    property Feedback: Single read fFeedback write fFeedback; // 0..<1
    property Minimum: Single read fMin write SetMinimum;
    property Maximum: Single read fMax write SetMaximum;
    property Stages: Integer read GetStages write SetStages;
    property Rate: Single read fRate write SetRate; // Hz
  end;

implementation

uses DAVDCommon;

const kDenorm=1E-25;

constructor TLFOSine.Create;
begin
 fMax:=1;
 fMin:=0;
 fValue:=1;
 fPos:=0;
 Speed:=100;
 fScale:=fMax-((fMin+fMax)*0.5);
 inherited;
 fPosMul:=(Sqrt(fScale*2))/$80000000;
 fHalfScale:=(Sqrt(fScale*2))*0.5;
end;

procedure TLFOSine.SetMin(v: Single);
begin
 fMin:=v;
 fScale:=fMax-((fMin+fMax)*0.5);
end;

procedure TLFOSine.SetMax(v: Single);
begin
 fMax:=v;
 fScale:=fMax-((fMin+fMax)*0.5);
end;

procedure TLFOSine.SetSpeed(v:Single);
begin
 fSpeed:=v;
 iSpeed:=Round($100000000/fSpeed);
end;

function TLFOSine.GetValue:Single;
begin
 Result:=Abs(fPos*fPosMul)-fHalfScale;
 Result:=Result*(fHalfScale*2-Abs(Result))*2;
 Result:=Result+((fMin+fMax)*0.5);
 fPos:=fPos+iSpeed;
end;

constructor TLFOTriangle.Create;
begin
 fMax:=1;
 fMin:=0;
 fValue:=1;
 fPos:=0;
 Speed:=100;
 fScale:=fMax-((fMin+fMax)*0.5);
 fPosMul:=fScale/$80000000;
 fHalfScale:=(Sqrt(fScale*2))*0.5;
end;

procedure TLFOTriangle.SetMin(v: Single);
begin
 fMin:=v;
 fScale:=fMax-((fMin+fMax)*0.5);
end;

procedure TLFOTriangle.SetMax(v: Single);
begin
 fMax:=v;
 fScale:=fMax-((fMin+fMax)*0.5);
end;

procedure TLFOTriangle.SetSpeed(v:Single);
begin
 fSpeed:=v;
 iSpeed:=Round($100000000/fSpeed);
end;

function TLFOTriangle.GetValue:Single;
begin
 Result:=Abs(fPos*(2*fPosMul))+fMin;
 fPos:=fPos+iSpeed;
end;

constructor TMasterAllpass.Create;
begin
 inherited;
 fA1:=0;
 fY[0]:=0;
 fY[1]:=0;
 fY[2]:=0;
 fY[3]:=0;
 fY[4]:=0;
 fY[5]:=0;
 fStages:=1;
end;

destructor TMasterAllpass.Destroy;
begin
 inherited;
end;

{$IFDEF PUREPASCAL}
function TMasterAllpass.Process(const x:single):single;
var a : array[0..1] of Single;
    b : Single;
begin
 a[0]:=x*fA1+fY[0];
 b:=a[0]*fA1;
 fY[0]:=b-x;
 a[1]:=b-fY[1];
 b:=a[1]*fA1;
 fY[1]:=a[0]-b;
 a[0]:=b-fY[2];
 b:=a[0]*fA1;
 fY[2]:=a[1]-b;
 a[1]:=+b-fY[3];
 b:=a[1]*fA1;
 fY[3]:=a[0]-b;
 a[0]:=b-fY[4];
 b:=a[0]*fA1;
 fY[4]:=a[1]-b;
 a[1]:=b-fY[5];
 b:=a[1]*fA1;
 fY[5]:=a[0]-b;
 Result:=a[1];
end;

{$ELSE}

function TMasterAllpass.Process(const x:single):single;
asm
 fld self.fA1.Single
 mov ecx,self.fStages.Integer
 add eax,fY.Single
 fld x.single
 fmul st(0),st(1)
 fadd [eax].Single
 fld st(0)
 fmul st(0),st(2)
 fld st(0)
 fsub x.Single
 fstp [eax].Single
 add eax,4
 fsub [eax].Single
 fld st(0)
 fmul st(0),st(3)
 fsub st(2),st(0)
 fxch st(2)
 fstp [eax].Single
 add eax,4
 fld [eax].Single
 fsubp st(2),st(0)
 fld st(1)
 fmul st(0),st(3)
 fsub st(1),st(0)
 fxch

@loop:
 fstp [eax].Single
 add eax,4
 fsub [eax].Single
 fld st(0)
 fmul st(0),st(3)
 fsub st(2),st(0)
 fxch st(2)
 fstp [eax].Single
 add eax,4
 fld [eax].Single
 fsubp st(2),st(0)
 fld st(1)
 fmul st(0),st(3)
 fsub st(1),st(0)
 loop @loop


 fxch
 fstp [eax].Single
 add eax,4
 fadd [eax].Single
 fxch
 fld st(1)
 fmulp st(3),st(0)
 fsubp st(2),st(0)
 fxch
 fstp [eax].Single
end;

{$ENDIF}

procedure TMasterAllpass.SetDelay(v:Single);
begin
 fDelay:=v;
 fA1:=(1-v)/(1+v);
end;

constructor TPhaser.Create;
begin
 inherited;
 fLFO:=TLFOSine.Create;
 fSampleRate:=44100;
 fFeedBack:=0.7;
 fLFOPhase:=0;
 fDepth:=1;
 fZM1:=0;
 Minimum:=440;
 Maximum:=1600;
 Rate:=0;
 fMasterAllPass:=TMasterAllPass.Create;
 Stages:=5;
end;

destructor TPhaser.Destroy;
begin
 fLFO.Free;
 fMasterAllPass.Free;
 inherited;
end;

procedure TPhaser.SetRate(v:Single);
begin
 fLFO.Speed:=2*SampleRate/v;
end;

procedure TPhaser.Calculate;
var x : Double;
begin
 x:=1/fSampleRate;
 fMin:= 2*fMinimum*x;
 fMax:= 2*fMaximum*x;
end;

procedure TPhaser.SetMinimum(v:Single);
begin
 fMinimum:=v;
 Calculate;
end;

procedure TPhaser.SetMaximum(v:Single);
begin
 fMaximum:=v;
 Calculate;
end;

function TPhaser.Process(const x:single):single;
var d: Single;
//    i: Integer;
begin
 d:=fMin + (fMax-fMin) * fLFO.Value;
 fMasterAllPass.Delay:=d;
 Result:= fMasterAllPass.Process(kDenorm + x + fZM1 * fFeedBack );
 fZM1:=tanh2c(2*Result);
 Result:=1.1*tanh2c(2*(x + Result * fDepth));
end;

procedure TPhaser.SetSampleRate(v:Single);
begin
 fSampleRate:=v;
end;

procedure TPhaser.SetStages(v:Integer);
begin
 fMasterAllPass.fStages:=v;
end;

function TPhaser.GetStages:Integer;
begin
 Result:=fMasterAllPass.fStages;
end;

end.
