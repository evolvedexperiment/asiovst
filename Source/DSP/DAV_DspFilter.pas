unit DAV_DspFilter;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_Complex, DAV_DspCommon;

type
  TPNType = array[0..1] of TComplexSingle;

  TCustomFilter = class(TDspObject)
  private
    procedure SetFrequency(Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetGaindB(const Value: Double);
  protected
    FGain_dB           : Double;
    FGainFactor        : Double;
    FGainFactorSquared : Double;
    FSampleRate        : Double;
    FFrequency, FW0    : Double;
    FExpW0             : TComplexDouble;
    FSRR               : Double; // reciprocal of FSampleRate
    FOnChange          : TNotifyEvent;
    procedure CalculateW0; virtual;
    procedure CalculateGainFactor; virtual;
    procedure CalculateCoefficients; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure GainChanged; virtual;

    // Order
    function GetOrder: Cardinal; virtual; abstract;
    procedure SetOrder(const Value: Cardinal); virtual; abstract;

    property GainFactor: Double read FGainFactor;
    property SampleRateReciprocal: Double read FSRR;
    property ExpW0: TComplexDouble read FExpW0;
    property W0: Double read FW0;
  public
    constructor Create; virtual;
    function ProcessSample(const Input: Double): Double; overload; virtual; abstract;
    function ProcessSample(const Input: Int64): Int64; overload; virtual; abstract;
    function ProcessSampleASM: Double; virtual;
    function MagnitudeSquared(const Frequency: Double): Double; virtual; abstract;
    function MagnitudeLog10(const Frequency: Double): Double; virtual; abstract;
    function Phase(const Frequency: Double): Double; virtual;
    function Real(const Frequency: Double): Double; virtual; abstract;
    function Imaginary(const Frequency: Double): Double; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Single); overload; virtual;
    procedure ResetStates; virtual; abstract;
    procedure ResetStatesInt64; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure GetIR(ImpulseResonse : TDAVSingleDynArray); overload;
    procedure GetIR(ImpulseResonse : TDAVDoubleDynArray); overload;
    procedure PushStates; virtual; abstract;
    procedure PopStates; virtual; abstract;

    property Gain: Double read FGain_dB write SetGaindB;
    property Frequency: Double read FFrequency write SetFrequency;
    property Order: Cardinal read GetOrder write SetOrder;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TOrderFilterClass = class of TCustomOrderFilter;
  TCustomOrderFilter = class(TCustomFilter)
  protected
    FOrder: Cardinal;
    class function GetMaxOrder: Cardinal; virtual; abstract;
    procedure OrderChanged; virtual;
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
  public
    constructor Create(const Order: Integer = 0); reintroduce; virtual;
  end;

  TFIRFilterClass = class of TCustomFIRFilter;
  TCustomFIRFilter = class(TCustomFilter)
  private
    procedure SetKernelSize(const Value: Integer);
  protected
    FKernelSize : Integer;
    FIR         : TDAVDoubleDynArray;
    FHistory    : TDAVDoubleDynArray;
    FCircular   : TDAVDoubleDynArray;
    FSpeedTab   : TDAVDoubleDynArray;
    FStateStack : TDAVDoubleDynArray;
    FBufferPos  : Integer;

    // Order
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
  public
    constructor Create; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function ProcessSample(const Input: Double): Double; override;
//    function ProcessSample(const Input: Int64): Int64; override;
//    function ProcessSampleASM: Double; override;
    procedure PushStates; override;
    procedure PopStates; override;
    property KernelSize: Integer Read FKernelSize Write SetKernelSize;
  end;

  TIIRFilterClass = class of TCustomIIRFilter;
  TCustomIIRFilter = class(TCustomFilter)
  end;

  TFirstOrderAllpassFilter = class(TCustomIIRFilter)
  protected
    FState  : Double;
    FStates : TDAVDoubleDynArray;
    procedure FrequencyChanged; override;
    function GetOrder: Cardinal; override;
    procedure CalculateCoefficients; override;
    procedure SetOrder(const Value: Cardinal); override;
  public
    function ProcessSample(const Input: Double): Double; override;
    constructor Create; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override; 
  end;

  TBandwidthIIRFilterClass = class of TCustomBandwidthIIRFilter;
  TCustomBandwidthIIRFilter = class(TCustomIIRFilter)
  private
    procedure SetBW(Value: Double);
  protected
    FBandWidth   : Double;
    FAlpha       : Double;
    procedure CalculateW0; override;
    procedure CalculateAlpha; virtual;
    procedure BandwidthChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;

    property Alpha: Double read FAlpha;
  public
    constructor Create; override;
    property BandWidth: Double read FBandWidth write SetBW;
  end;

  TBiquadIIRFilter = class(TCustomBandwidthIIRFilter)
  protected
    FDenominator  : array[1..2] of Double;
    FNominator    : array[0..2] of Double;
    FPoles        : TPNType;
    FZeros        : TPNType;
    FState        : array[0..1] of Double;
    FStateStack   : array of array[0..1] of Double;
    procedure CalcPolesZeros; virtual;
    function GetPoles: TPNType;
    function GetZeros: TPNType;
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
  public
    constructor Create; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    function ProcessSample(const Input: Double): Double; override;
    function ProcessSample(const Input: Int64): Int64; override;
    function ProcessSampleASM: Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double):Double; override;
    function Phase(const Frequency: Double):Double; override;
    function Real(const Frequency: Double):Double; override;
    function Imaginary(const Frequency: Double):Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Single); overload; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
    property Poles: TPNType read FPoles; //GetPoles;
    property Zeros: TPNType read FZeros; //GetZeros;
  published
    property Gain;
    property Frequency;
    property SampleRate;
    property Bandwidth;
  end;

(*
  TDspLowpassFilter = class(TDspBaseComponent)
  private
    FFrequency: Single;
    fFilter : Array of TSimpleLowpassFilter;
    procedure SetFrequency(const Value: Single);
  protected
    procedure SampleRateChanged; override;
    procedure ChannelsChanged; override;
  published
  public
    procedure Init; override;
    procedure Reset; override;
    procedure Process(var Data: Single; const channel: integer); overload;
    procedure Process(var Data: Double; const channel: integer); overload;
  published
    property Frequency: Single read FFrequency write SetFrequency;  // 20..20000
  end;
*)

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils, DAV_DspDFT;

{ TCustomFilter }

constructor TCustomFilter.Create;
begin
 FGain_dB           := 0;
 FGainFactor        := 1;
 FGainFactorSquared := 1;
 FSampleRate        := 44100;
 FSRR               := 1 / FSampleRate;
 FFrequency         := 1000;
 CalculateW0;
end;

procedure TCustomFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFilter then
  begin
   TCustomFilter(Dest).FGain_dB    := FGain_dB;
   TCustomFilter(Dest).FGainFactor := FGainFactor;
   TCustomFilter(Dest).FSampleRate := FSampleRate;
   TCustomFilter(Dest).FSRR        := FSRR;
   TCustomFilter(Dest).FW0         := FW0;
   TCustomFilter(Dest).FExpW0      := FExpW0;
  end
 else inherited;
end;

procedure TCustomFilter.CalculateGainFactor;
begin
 FGainFactor := dB_to_Amp(CHalf32 * FGain_dB); // do not change this!
 FGainFactorSquared := sqr(FGainFactor);
end;

procedure TCustomFilter.CalculateW0;
begin
 FW0 := 2 * Pi * FFrequency * FSRR;
 GetSinCos(FW0, FExpW0.Im, FExpW0.Re);
 if FW0 > 3.141
  then FW0 := 3.141;
end;

procedure TCustomFilter.Complex(const Frequency: Double; out Real,
  Imaginary: Single);
var
  Complex64 : TComplexDouble;
begin
 inherited;
 Complex(Frequency, Complex64.Re, Complex64.Im);
 Real := Complex64.Re;
 Imaginary := Complex64.Im;
end;

procedure TCustomFilter.FrequencyChanged;
begin
 CalculateW0;
 CalculateCoefficients;
 if assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomFilter.GainChanged;
begin
 CalculateGainFactor;
 CalculateCoefficients;
 if assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomFilter.GetIR(ImpulseResonse: TDAVSingleDynArray);
var
  i : Cardinal;
begin
 if Length(ImpulseResonse) = 0 then Exit;
 PushStates;
 ImpulseResonse[0] := ProcessSample(1.0);
 for i := 1 to Length(ImpulseResonse) - 1
  do ImpulseResonse[i] := ProcessSample(0.0);
 PopStates;
end;

procedure TCustomFilter.GetIR(ImpulseResonse: TDAVDoubleDynArray);
var
  i : Cardinal;
begin
 if Length(ImpulseResonse) = 0 then Exit;
 PushStates;
 ImpulseResonse[0] := ProcessSample(1.0);
 for i := 1 to Length(ImpulseResonse) - 1
  do ImpulseResonse[i] := ProcessSample(0.0);
 PopStates;
end;

function TCustomFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

function TCustomFilter.ProcessSampleASM: Double;
{$IFDEF PUREPASCAL}
begin
end;
{$ELSE}
asm
 push eax
 push ecx
 push edx
 fstp [esp - 4].Single
 push dword ptr [esp - 4]
 mov edx, [eax]
 call dword ptr [edx + $24] // ProcessSample
 pop edx
 pop ecx
 pop eax
end;
{$ENDIF}

procedure TCustomFilter.SampleRateChanged;
begin
 CalculateW0;
 CalculateCoefficients;
 if assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomFilter.SetFrequency(Value: Double);
begin
 if Value > 1E-10
  then Value := Value
  else Value := 1E-10;
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomFilter.SetGaindB(const Value: Double);
begin
 if FGain_dB <> Value then
  begin
   FGain_dB := Value;
   GainChanged;
  end;
end;

procedure TCustomFilter.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   FSRR :=  1 / FSampleRate;
   SampleRateChanged;
  end;
end;


{ TCustomOrderFilter }

constructor TCustomOrderFilter.Create(const Order: Integer);
begin
 FOrder := Order;
 OrderChanged;

 inherited Create;
end;

function TCustomOrderFilter.GetOrder: Cardinal;
begin
 result := FOrder;
end;

procedure TCustomOrderFilter.OrderChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomOrderFilter.SetOrder(const Value: Cardinal);
var
  NewOrder: Cardinal;
begin
 NewOrder := GetMaxOrder;
 if Value < NewOrder
  then NewOrder := Value;
 if NewOrder <> Order then
  begin
   FOrder := NewOrder;
   OrderChanged;
  end;
end;


{ TCustomFIRFilter }

constructor TCustomFIRFilter.Create;
begin
 inherited;
end;

function TCustomFIRFilter.GetOrder: Cardinal;
begin
 result := FKernelSize;
end;

function TCustomFIRFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * log10(MagnitudeSquared(Frequency));
end;

function TCustomFIRFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  Cmplx    : TComplexDouble;
begin
 Cmplx := Goertzel(PDAVDoubleFixedArray(@FIR[0]), FKernelSize, Pi * Frequency / SampleRate);
 Result := FGainFactor * (sqr(Cmplx.Re) + sqr(Cmplx.Im));
end;

procedure TCustomFIRFilter.PopStates;
begin
 Move(FStateStack[0], FHistory[0], Length(FHistory) * SizeOf(Double));
 Move(FStateStack[Length(FHistory)], FCircular[0], Length(FCircular) * SizeOf(Double));
end;

procedure ConvolveIR_X87(InOutBuffer, IRBuffer: PDAVDoubleFixedArray; SampleFrames: Integer;
  Current: Double);
asm
  fld   Current.Double
  @SmallLoop:
  fld   [edx].Double
  fmul  st(0),st(1)
  fld   [eax].Double
  faddp

  fstp [eax].Double
  add   eax, 8
  add   edx, 8
  loop  @SmallLoop

  @EndSmallLoop:
  ffree st(0)
end;

procedure ConvolveIR_X87large(InOutBuffer, IRBuffer: PDAVDoubleFixedArray;
  SampleFrames: Integer; Current: Double);
asm
  fld   Current.Double

  push ecx
  shr ecx,2
  jz @SkipLargeAddLoop
  @LargeLoop:
  fld   [edx].Double
  fmul  st(0),st(1)
  fld   [eax].Double
  faddp
  fstp  [eax].Double
  fld   [edx+8].Double
  fmul  st(0),st(1)
  fld   [eax+8].Double
  faddp
  fstp  [eax+8].Double
  fld   [edx+16].Double
  fmul  st(0),st(1)
  fld   [eax+16].Double
  faddp
  fstp  [eax+16].Double
  fld   [edx+24].Double
  fmul  st(0),st(1)
  fld   [eax+24].Double
  faddp
  fstp  [eax+24].Double

  add   eax, 32
  add   edx, 32
  loop  @LargeLoop

  @SkipLargeAddLoop:
  pop ecx
  and ecx,$00000003
  jz @EndSmallLoop

  @SmallLoop:
  fld   [edx].Double
  fmul  st(0),st(1)
  fld   [eax].Double
  faddp
  fstp [eax].Double

  add   eax, 8
  add   edx, 8
  loop  @SmallLoop

  @EndSmallLoop:
  ffree st(0)
end;

function TCustomFIRFilter.ProcessSample(const Input: Double): Double;
begin
 FHistory[FBufferPos] := Input;
 Result := (FCircular[FBufferPos] + FHistory[FBufferPos] * FIR[0]);
 ConvolveIR_X87large(@FCircular[FBufferPos], @FIR[0], FKernelSize, FHistory[FBufferPos]);
 Inc(FBufferPos);
 if FBufferPos >= FKernelSize then
  begin
   FBufferPos := 0;
   move(FCircular[FKernelSize], FCircular[0], FKernelSize * SizeOf(Double));
   FillChar(FCircular[FKernelSize], FKernelSize * SizeOf(Double), 0);
  end;
end;

procedure TCustomFIRFilter.PushStates;
begin
 Move(FHistory[0], FStateStack[0], Length(FHistory) * SizeOf(Double));
 Move(FCircular[0], FStateStack[Length(FHistory)], Length(FCircular) * SizeOf(Double));
end;

procedure TCustomFIRFilter.SetKernelSize(const Value: Integer);
begin
 if FKernelSize <> Value then
  begin
   FKernelSize := Value;
  end;
end;

procedure TCustomFIRFilter.SetOrder(const Value: Cardinal);
begin
 KernelSize := Value;
end;

{ TFirstOrderAllpassFilter }

procedure TFirstOrderAllpassFilter.CalculateCoefficients;
begin
 // do nothing yet;
end;

constructor TFirstOrderAllpassFilter.Create;
begin
 inherited;
 FState := 0;
end;

procedure TFirstOrderAllpassFilter.FrequencyChanged;
begin
 assert (FFrequency >= -0.5);
 assert (FFrequency <= 1);
end;

function TFirstOrderAllpassFilter.GetOrder: Cardinal;
begin
 result := 1;
end;

function TFirstOrderAllpassFilter.MagnitudeLog10(
  const Frequency: Double): Double;
begin
 result := FGain_dB;
end;

function TFirstOrderAllpassFilter.MagnitudeSquared(
  const Frequency: Double): Double;
begin
 result := FGainFactor;
end;

procedure TFirstOrderAllpassFilter.PopStates;
begin
 FState := FStates[Length(FStates) - 1];
 SetLength(FStates, Length(FStates) - 1);
end;

function TFirstOrderAllpassFilter.ProcessSample(const Input: Double): Double;
begin
 result := FState + FFrequency * Input;
 FState := Input - FFrequency * result;
end;

procedure TFirstOrderAllpassFilter.PushStates;
begin
 SetLength(FStates, Length(FStates) + 1);
 FStates[Length(FStates) - 1] := FState;
end;

procedure TFirstOrderAllpassFilter.Reset;
begin
 FFrequency := 0;
end;

procedure TFirstOrderAllpassFilter.ResetStates;
begin
 FState := 0;
end;

procedure TFirstOrderAllpassFilter.ResetStatesInt64;
begin
 FState := 0;
end;

procedure TFirstOrderAllpassFilter.SetOrder(const Value: Cardinal);
begin
 raise Exception.Create('Order is fixed!');
end;

{ TCustomBandwidthIIRFilter }

constructor TCustomBandwidthIIRFilter.Create;
begin
 FBandWidth := 1;
 inherited;
 CalculateAlpha;
end;

procedure TCustomBandwidthIIRFilter.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBandwidthIIRFilter then
  begin
   TCustomBandwidthIIRFilter(Dest).BandWidth := Bandwidth;
  end;
end;

procedure TCustomBandwidthIIRFilter.BandwidthChanged;
begin
 CalculateAlpha;
 CalculateCoefficients;
 if assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomBandwidthIIRFilter.CalculateW0;
begin
 inherited;
 CalculateAlpha;
end;

procedure TCustomBandwidthIIRFilter.CalculateAlpha;
begin
 if (FExpW0.Im = 0)
  then FAlpha := FExpW0.Im /( 2 * FBandWidth)
  else FAlpha := Sinh(ln22 * cos(FW0 * 0.5) * FBandWidth * (FW0 / FExpW0.Im)) * FExpW0.Im;
end;

procedure TCustomBandwidthIIRFilter.SetBW(Value: Double);
begin
 if Value <= 1E-3 then Value := 1E-3;
 if FBandWidth <> Value then
  begin
   FBandWidth := Value;
   BandwidthChanged;
  end;
end;

{ TBiquadIIRFilter }

constructor TBiquadIIRFilter.Create;
begin
 inherited;
 FBandWidth := 1;
 CalculateCoefficients;
 ResetStates;
end;

function TBiquadIIRFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := (sqr(FNominator[0] - FNominator[2]) + sqr(FNominator[1]) + (FNominator[1] * (FNominator[0] + FNominator[2]) + FNominator[0] * FNominator[2] * cw) * cw)
         / (sqr(1 - FDenominator[2]) + sqr(FDenominator[1]) + (FDenominator[1] * (FDenominator[2] + 1) + cw * FDenominator[2]) * cw );
end;

function TBiquadIIRFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * Log10(MagnitudeSquared(Frequency));
end;

function TBiquadIIRFilter.Phase(const Frequency: Double): Double;
var
  cw, sw : Double;
begin
 GetSinCos(2 * Frequency * Pi * FSRR, sw, cw);
 Result := ArcTan2(-sw * (FNominator[0] * (2 * cw * FDenominator[2] + FDenominator[1]) + FNominator[1] * (FDenominator[2] - 1) - FNominator[2] * (2 * cw + FDenominator[1])),
                  (FNominator[0] * (FDenominator[2] * (2 * sqr(cw) - 1) + 1 + FDenominator[1] * cw) + FNominator[1] * (cw * (FDenominator[2] + 1) + FDenominator[1]) + FNominator[2] * (2 * sqr(cw) + FDenominator[1] * cw + FDenominator[2] - 1)));
end;

function TBiquadIIRFilter.Real(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Real := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
          +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
          + (2 * sqr(cw) - 1) * (FNominator[0] * FDenominator[2] + FNominator[2]))
          / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
          + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
end;

function TBiquadIIRFilter.Imaginary(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - sqr(cw))
              / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
              + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]))
end;

procedure TBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := 1 / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2 * sqr(cw)-1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - sqr(cw)) * Divider;
end;

procedure TBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  Cw, Divider : Double;
begin
 cw := cos(2 * Frequency * pi * FSRR);
 Divider   := 1 / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2 * sqr(cw) - 1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - sqr(cw)) * Divider;
end;

procedure TBiquadIIRFilter.Reset;
begin
 Gain := 0;
end;

procedure TBiquadIIRFilter.ResetStates;
begin
 FState[0] := 0;
 FState[1] := 0;
end;

procedure TBiquadIIRFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TBiquadIIRFilter.SetOrder(const Value: Cardinal);
begin
 raise Exception.Create('Order is fixed!');
end;

procedure TBiquadIIRFilter.CalcPolesZeros;
var
  p, q : Double;
  e    : Double;
begin
 p := -FNominator[1] / (2 * FNominator[0]);
 q := (FNominator[2] / FNominator[0]);
 FZeros[0].Re := p;
 FZeros[1].Re := p;
 e := q-(p*p);
 if e > 0
  then
   begin
    FZeros[0].Im := sqrt(e);
    FZeros[1].Im := -sqrt(e);
   end
  else
   begin
    FZeros[0].Re := FZeros[0].Re + sqrt(-e);
    FZeros[1].Re := FZeros[0].Re - sqrt(-e);
    FZeros[0].Im := 0;
    FZeros[1].Im := 0;
   end;

 p := -FDenominator[1] * 0.5;
 q :=  FDenominator[2];
 FPoles[0].Re := p;
 FPoles[1].Re := p;
 e := q - sqr(p);
 if e > 0
  then
   begin
    FPoles[0].Im := sqrt(e);
    FPoles[1].Im := -sqrt(e);
   end
  else
   begin
    FPoles[0].Re := FPoles[0].Re + sqrt(-e);
    FPoles[1].Re := FPoles[0].Re - sqrt(-e);
    FPoles[0].Im := 0;
    FPoles[1].Im := 0;
   end;
end;

function TBiquadIIRFilter.ProcessSample(const Input:Double):Double;
{$IFDEF PUREPASCAL}
begin
 result    := FNominator[0] * Input + FState[0];
 FState[0] := FNominator[1] * Input - FDenominator[1] * result + FState[1];
 FState[1] := FNominator[2] * Input - FDenominator[2] * result;
end;
{$ELSE}
asm
 fld Input.Double                    // Input
 fmul [self.FNominator].Double       // a0 * Input
 fadd [self.FState].Double           // r = d0 + a0 * Input
 fld st(0)                           // r, r
 fld st(0)                           // r, r, r
 fmul [self.FDenominator].Double     // b0 * r, r, r
 fld Input.Double                    // Input, b0 * r, r, r
 fmul [self.FNominator + 8].Double   // a1 * Input, b0 * r, r, r
 fsubrp                              // a1 * Input + b0 * r, r, r
 fadd [self.FState+8].Double         // d1 + a1 * Input - b0 * r, r, r
 fstp [self.FState].Double           // d0 = a1 * Input + d1 + b1 * r, r, r
 fmul [self.FDenominator+8].Double   // b1*r, r
 fld Input.Double                    // Input, b1*r, r
 fmul [self.FNominator+16].Double    // a2*Input, b1*r, r
 fsubrp st(1), st(0)                 // b1*r + a2*Input, r !!!
 fstp [self.FState+8].Double         // d1 = b1*r + a2*Input, r !!!
end;
{$ENDIF}

function TBiquadIIRFilter.ProcessSample(const Input: Int64): Int64;
begin
 result              := Round(FNominator[0]*Input) + PInt64(@FState[0])^;
 PInt64(@FState[0])^ := Round(FNominator[1]*Input) - Round(FDenominator[1]*result) + PInt64(@FState[1])^;
 PInt64(@FState[1])^ := Round(FNominator[2]*Input) - Round(FDenominator[2]*result);
end;

function TBiquadIIRFilter.ProcessSampleASM: Double;
{$IFDEF PUREPASCAL}
begin
end;
{$ELSE}
asm
 fld st(0)                           // s, s
 fmul [self.FNominator].Double       // a0*s, s
 fadd [self.FState].Double           // r = d0+a0*s, s
 fld st(0)                           // r, r, s
 fld st(0)                           // r, r, r, s
 fmul [self.FDenominator].Double     // b0*r, r, r, s
 fld st(3)                           // s, b0*r, r, r, s
 fmul [self.FNominator+8].Double     // a1*s, b0*r, r, r, s
 fsubrp                              // a1*s + b0*r, r, r, s
 fadd [self.FState+8].Double         // d1+a1*s-b0*r, r, r, s

 fstp [self.FState].Double           // d0 = a1*s + d1+b1*r, r, r, s
 fmul [self.FDenominator+8].Double   // b1*r, r, s
 fxch st(2)                          // s, r, b1*r,
 fmul [self.FNominator+16].Double    // a2*s, r, b1*r,
 fsubrp st(2), st(0)                 // b1*r + a2*s, r, !!!
 fxch
 fstp [self.FState+8].Double         // d1 = b1*r + a2*s, r, !!!
end;
{$ENDIF}

procedure TBiquadIIRFilter.PushStates;
begin
 SetLength(FStateStack,Length(FStateStack)+1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0,0],FStateStack[1,0], (Length(FStateStack)-1)*Length(FStateStack[0])*SizeOf(Double));
 Move(FState[0],FStateStack[0,0],Length(FStateStack[0])*SizeOf(Double));
end;

procedure TBiquadIIRFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0], FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

function TBiquadIIRFilter.GetOrder: Cardinal;
begin
 result := 2;
end;

function TBiquadIIRFilter.GetPoles: TPNType;
var
 p, q : Double;
begin
 p := FDenominator[1] / (2 * FDenominator[2]);
 q := (1 / FDenominator[2]);
 Result[0].Re := p;
 Result[1].Re := p;
 Result[0].Im := sqrt(q-(p*p));
 Result[1].Im := -sqrt(q-(p*p));
end;

function TBiquadIIRFilter.GetZeros:TPNType;
var p, q : Double;
begin
 p := FNominator[1]/(2*FNominator[2]);
 q := (FNominator[0]/FNominator[2]);
 Result[0].Re := p;
 Result[1].Re := p;
 Result[0].Im := sqrt(q-(p*p));
 Result[1].Im := -sqrt(q-(p*p));
end;

(*
{ TDspLowpassFilter }

procedure TDspLowpassFilter.ChannelsChanged;
var
  i : Integer;
begin
 inherited;
 for i := fChannels to Length(fFilter) - 1
  do fFilter[i].Free;
 SetLength(fFilter, fChannels);
 for i := 0 to fChannels - 1
  do if not Assigned(fFilter[i])
   then fFilter[i] := TSimpleLowpassFilter.Create;
end;

procedure TDspLowpassFilter.Init;
begin
  fStdProcessS  := Process;
  fStdProcessD  := Process;
  Reset;
end;

procedure TDspLowpassFilter.Process(var Data: Double; const channel: integer);
begin
  Data := fFilter[channel].ProcessSample(Data);
end;

procedure TDspLowpassFilter.Process(var Data: Single; const channel: integer);
begin
  Data := fFilter[channel].ProcessSample(Data);
end;

procedure TDspLowpassFilter.Reset;
begin
  ChannelsChanged;
  SampleRateChanged;
end;

procedure TDspLowpassFilter.SampleRateChanged;
var i : Integer;
begin
 inherited;
 for i := 0 to Length(fFilter) - 1
  do fFilter[i].SampleRate := FSampleRate;
end;

procedure TDspLowpassFilter.SetFrequency(const Value: Single);
var i : Integer;
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   for i := 0 to Length(fFilter) - 1
    do fFilter[i].Frequency := FFrequency;
  end;
end;
*)

end.
