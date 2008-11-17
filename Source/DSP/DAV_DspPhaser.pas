unit DAV_DspPhaser;

{$I ..\ASIOVST.INC}

interface

uses
  DAV_Common, DAV_DspCommon;

type
  TLFOSine = class(TDspObject)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetSpeed(Value: Single);
  public
    constructor Create;
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TLFOTriangle = class(TDspObject)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(Value: Single);
    procedure SetMax(Value: Single);
    procedure SetSpeed(Value: Single);
  public
    constructor Create;
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TMasterAllPass = class(TDspObject)
  private
    FA1         : Single;
    FDelay      : Single;
    FStages     : Integer;
    FY          : array [0..31] of Single;
    FSampleRate : Single;
    procedure SetDelay(Value: Single);
  public
    constructor Create;
    destructor Destroy; override;
    function Process(const x: Single): Single;
    property Delay: Single read FDelay write SetDelay;
    property Stages: Integer read FStages write FStages;
    property SampleRate: Single read FSampleRate write FSampleRate;
  end;

  TPhaser = class(TDspObject)
  private
    FZM1           : Single;
    FDepth         : Single;
    FLFO           : TLFOSine;
    FLFOPhase      : Single;
    FFeedBack      : Single;
    FRate          : Single;
    FMinimum       : Single;
    FMaximum       : Single;
    FMin           : Single;
    FMax           : Single;
    FSampleRate    : Single;
    FMasterAllPass : TMasterAllPass;
    procedure SetSampleRate(Value: Single);
    procedure SetMinimum(Value: Single);
    procedure SetMaximum(Value: Single);
    procedure SetRate(Value: Single);
    procedure SetStages(Value: Integer);
    function GetStages: Integer;
    procedure Calculate;
  protected
    procedure RateChanged; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Process(const Input: Single): Single;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Depth: Single read FDepth write FDepth; //0..1
    property Feedback: Single read FFeedBack write FFeedBack; // 0..<1
    property Minimum: Single read FMin write SetMinimum;
    property Maximum: Single read FMax write SetMaximum;
    property Stages: Integer read GetStages write SetStages;
    property Rate: Single read FRate write SetRate; // Hz
  end;

implementation

uses
  SysUtils;

const
  kDenorm = 1E-25;


{ TLFOSine }

constructor TLFOSine.Create;
begin
  FMax   := 1;
  FMin   := 0;
  FValue := 1;
  FPos   := 0;
  Speed  := 100;
  FScale := FMax - ((FMin + FMax) * 0.5);
  inherited;
  FPosMul := Sqrt(FScale * 2) / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TLFOSine.SetMin(Value: Single);
begin
  FMin := Value;
  FScale := FMax - ((FMin + FMax) * 0.5);
end;

procedure TLFOSine.SetMax(Value: Single);
begin
  FMax := Value;
  FScale := FMax - ((FMin + FMax) * 0.5);
end;

procedure TLFOSine.SetSpeed(Value: Single);
begin
  FSpeed := Value;
  FIntSpeed := Round($100000000 / FSpeed);
end;

function TLFOSine.GetValue: Single;
begin
  Result := Abs(FPos * FPosMul) - FHalfScale;
  Result := Result * (FHalfScale * 2 - Abs(Result)) * 2;
  Result := Result + (FMin + FMax) * 0.5;
  FPos := FPos + FIntSpeed;
end;


{ TLFOTriangle }

constructor TLFOTriangle.Create;
begin
  FMax := 1;
  FMin := 0;
  FValue := 1;
  FPos := 0;
  Speed := 100;
  FScale := FMax - (FMin + FMax) * 0.5;
  FPosMul := FScale / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TLFOTriangle.SetMin(Value: Single);
begin
  FMin := Value;
  FScale := FMax - (FMin + FMax) * 0.5;
end;

procedure TLFOTriangle.SetMax(Value: Single);
begin
  FMax := Value;
  FScale := FMax - (FMin + FMax) * 0.5;
end;

procedure TLFOTriangle.SetSpeed(Value: Single);
begin
  FSpeed := Value;
  FIntSpeed := Round($100000000 / FSpeed);
end;

function TLFOTriangle.GetValue: Single;
begin
  Result := Abs(FPos * (2 * FPosMul)) + FMin;
  FPos := FPos + FIntSpeed;
end;


{ TMasterAllpass }

constructor TMasterAllpass.Create;
begin
  inherited;
  FA1 := 0;
  FY[0] := 0;
  FY[1] := 0;
  FY[2] := 0;
  FY[3] := 0;
  FY[4] := 0;
  FY[5] := 0;
  FStages := 1;
end;

destructor TMasterAllpass.Destroy;
begin
  inherited;
end;

{$IFDEF PUREPASCAL}
function TMasterAllpass.Process(const x: single): single;
var
  a: array[0..1] of Single;
  b: Single;
begin
  a[0] := x * FA1 + FY[0];
  b := a[0] * FA1;
  FY[0] := b - x;
  a[1] := b - FY[1];
  b := a[1] * FA1;
  FY[1] := a[0] - b;
  a[0] := b - FY[2];
  b := a[0] * FA1;
  FY[2] := a[1] - b;
  a[1] := b - FY[3];
  b := a[1] * FA1;
  FY[3] := a[0] - b;
  a[0] := b - FY[4];
  b := a[0] * FA1;
  FY[4] := a[1] - b;
  a[1] := b - FY[5];
  b := a[1] * FA1;
  FY[5] := a[0] - b;
  Result := a[1];
end;

{$ELSE}

function TMasterAllpass.Process(const x:single):single;
asm
 fld self.FA1.Single
 mov ecx,self.FStages.Integer
 add eax,FY.Single
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
 fmulp st(3), st(0)
 fsubp st(2), st(0)
 fxch
 fstp [eax].Single
end;

{$ENDIF}

procedure TMasterAllpass.SetDelay(Value: Single);
begin
  FDelay := Value;
  FA1 := (1 - Value) / (1 + Value);
end;

{ TPhaser }

constructor TPhaser.Create;
begin
  inherited;
  FLFO := TLFOSine.Create;
  FMasterAllPass := TMasterAllPass.Create;
  FSampleRate := 44100;
  FMinimum    := 440;
  FMaximum    := 1600;
  FFeedBack   := 0.7;
  FLFOPhase   := 0;
  FDepth      := 1;
  FZM1        := 0;
  Rate        := 0;
  Stages      := 5;
  Calculate;
end;

destructor TPhaser.Destroy;
begin
  FreeAndNil(FLFO);
  FreeAndNil(FMasterAllPass);
  inherited;
end;

procedure TPhaser.RateChanged;
begin
 FLFO.Speed := 2 * SampleRate / FRate;
end;

procedure TPhaser.SetRate(Value: Single);
begin
 if Value <> FRate then
  begin
   FRate := Value;
   RateChanged;
  end;
end;

procedure TPhaser.Calculate;
var
  x: Double;
begin
  x := 1 / FSampleRate;
  FMin := 2 * FMinimum * x;
  FMax := 2 * FMaximum * x;
end;

procedure TPhaser.SetMinimum(Value: Single);
begin
 if FMinimum <> Value then
  begin
   FMinimum := Value;
   Calculate;
  end;
end;

procedure TPhaser.SetMaximum(Value: Single);
begin
 if FMaximum <> Value then
  begin
   FMaximum := Value;
   Calculate;
  end;
end;

function TPhaser.Process(const Input: single): single;
var
  d: Single;
//  i: Integer;
begin
  d := FMin + (FMax - FMin) * FLFO.Value;
  FMasterAllPass.Delay := d;
  Result := FMasterAllPass.Process(kDenorm + Input + FZM1 * FFeedBack);
  FZM1 := tanh2c(2 * Result);
  Result := 1.1 * tanh2c(2 * (Input + Result * FDepth));
end;

procedure TPhaser.SampleRateChanged;
begin
 FMasterAllPass.SampleRate := FSampleRate;
 Calculate;
// FLFO.SampleRate := FSampleRate;
end;

procedure TPhaser.SetSampleRate(Value: Single);
begin
 if Value = 0
  then exit
  else Value := abs(Value);
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TPhaser.SetStages(Value: Integer);
begin
  FMasterAllPass.FStages := Value;
end;

function TPhaser.GetStages: Integer;
begin
  Result := FMasterAllPass.FStages;
end;

end.
