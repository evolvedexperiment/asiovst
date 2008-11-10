unit DAV_DspFilter;

interface

{$I ..\ASIOVST.INC}

uses
  Classes, DAV_Complex, DAV_Common;

type
  TPNType = array[0..1] of TComplexSingle;

  TCustomFilter = class(TPersistent)
  private
    procedure SetFrequency(Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetGaindB(const Value: Double);
  protected
    FGain_dB     : Double;
    FGainFactor  : Double;
    FFrequency   : Double;
    FSinW0, fW0  : Double;
    FSampleRate  : Double;
    FSRR         : Double; // reciprocal of FSampleRate
    procedure CalculateW0; virtual;
    procedure CalculateCoefficients; virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SampleRateChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure GainChanged; virtual;

    property GainFactor: Double read FGainFactor;
    property SampleRateReciprocal: Double read FSRR;
    property SinW0: Double read FSinW0;
    property W0: Double read fW0;
  public
    constructor Create; virtual;
    function ProcessSample(const Input: Double): Double; overload; virtual; abstract;
    function ProcessSample(const Input: Int64): Int64; overload; virtual; abstract;
    function ProcessSampleASM: Double; virtual;
    function MagnitudeSquared(const Frequency: Double): Double; virtual; abstract;
    function MagnitudeLog10(const Frequency: Double): Double; virtual; abstract;
    function Phase(const Frequency: Double): Double; virtual; abstract;
    function Real(const Frequency: Double): Double; virtual; abstract;
    function Imaginary(const Frequency: Double): Double; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Single); overload; virtual; abstract;
    procedure ResetStates; virtual; abstract;
    procedure ResetStatesInt64; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure GetIR(ImpulseResonse : TDAVSingleDynArray); overload;
    procedure GetIR(ImpulseResonse : TDAVDoubleDynArray); overload;
    procedure PushStates; virtual; abstract;
    procedure PopStates; virtual; abstract;

    property Gain: Double read FGain_dB write SetGaindB;
    property Frequency: Double read FFrequency write SetFrequency;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

  TCustomOrderFilter = class(TCustomFilter)
  private
    procedure SetOrder(Value: Cardinal);
  protected
    FOrder : Cardinal;
    class function GetMaxOrder: Cardinal; virtual; abstract;
    procedure OrderChanged; virtual;
  public
    property Order: Cardinal read FOrder write SetOrder;
  end;

  TCustomBandwidthFilter = class(TCustomFilter)
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

  TIIRFilterClass = class of TCustomIIRFilter;
  TCustomIIRFilter = class(TCustomBandwidthFilter)
  protected
    function GetOrder: Integer; virtual; abstract;
  public
    property Order: Integer read GetOrder;
  end;

  TBiquadIIRFilter = class(TCustomIIRFilter)
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
    function GetOrder: Integer; override;
  public
    constructor Create; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    function ProcessSample(const Input:Double):Double; override;
    function ProcessSample(const Input:Int64):Int64; override;
    function ProcessSampleASM:Double; override;
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

  TSimpleGainFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample(const Input: Double): Double; override;
    function ProcessSampleASM: Double; override;
  end;

  TSimplePeakFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleAllpassFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleLowShelfFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleHighShelfFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleHighcutFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleLowcutFilter = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleLowpassFilter = class(TSimpleHighcutFilter);
  TSimpleHighpassFilter = class(TSimpleLowcutFilter);

  TSimpleBandpass = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
  end;

  TSimpleNotch = class(TBiquadIIRFilter)
  protected
    procedure CalculateCoefficients; override;
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

uses Math;

{ TCustomFilter }

constructor TCustomFilter.Create;
begin
 FGain_dB    := 0;
 FGainFactor := 1;
 FSampleRate := 44100;
 FSRR        := 1 / FSampleRate;
 FFrequency  := 1000;
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
   TCustomFilter(Dest).fW0         := fW0;
   TCustomFilter(Dest).FSinW0      := FSinW0;
  end
 else inherited;
end;

procedure TCustomFilter.CalculateW0;
begin
 fW0 := 2 * Pi * FFrequency * FSRR;
 FSinW0 := sin(fW0);
 if fW0 > 3.141
  then fW0 := 3.141;
end;

procedure TCustomFilter.FrequencyChanged;
begin
 CalculateW0;
 CalculateCoefficients;
end;

procedure TCustomFilter.GainChanged;
begin
 CalculateCoefficients;
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
   FGainFactor := dB_to_Amp(FGain_dB);
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

procedure TCustomOrderFilter.OrderChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomOrderFilter.SetOrder(Value: Cardinal);
begin
 if Value > GetMaxOrder then Value := GetMaxOrder;
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

{ TCustomBandwidthFilter }

constructor TCustomBandwidthFilter.Create;
begin
 FBandWidth := 1;
 inherited;
 CalculateAlpha;
end;

procedure TCustomBandwidthFilter.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBandwidthFilter then
  begin
   TCustomBandwidthFilter(Dest).BandWidth := Bandwidth;
  end;
end;

procedure TCustomBandwidthFilter.BandwidthChanged;
begin
 CalculateAlpha;
 CalculateCoefficients;
end;

procedure TCustomBandwidthFilter.CalculateW0;
begin
 inherited;
 CalculateAlpha;
end;

procedure TCustomBandwidthFilter.CalculateAlpha;
begin
 if (FSinW0 = 0)
  then FAlpha := FSinW0 /( 2 * FBandWidth)
  else FAlpha := Sinh(ln22 * cos(fW0 * 0.5) * FBandWidth * (fW0 / FSinW0)) * FSinW0;
end;

procedure TCustomBandwidthFilter.SetBW(Value: Double);
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
 ResetStates;
end;

function TBiquadIIRFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := sqrt((sqr(FNominator[0] - FNominator[2])+sqr(FNominator[1]) + (FNominator[1]*(FNominator[0]+FNominator[2])+FNominator[0]*FNominator[2]*cw)*cw)
             /(sqr(1-FDenominator[2])+sqr(FDenominator[1])+(FDenominator[1]*(FDenominator[2]+1)+cw*FDenominator[2])*cw ));
end;

function TBiquadIIRFilter.MagnitudeLog10(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := 10*log10((sqr(FNominator[0]-FNominator[2])+sqr(FNominator[1])+(FNominator[1]*(FNominator[0]+FNominator[2])+FNominator[0]*FNominator[2]*cw)*cw)
                 /(sqr(1-FDenominator[2])+sqr(FDenominator[1])+(FDenominator[1]*(FDenominator[2]+1)+cw*FDenominator[2])*cw ));
end;

function TBiquadIIRFilter.Phase(const Frequency: Double): Double;
var
  cw, sw : Double;
begin
 GetSinCos(2 * Frequency * pi * FSRR, sw, cw);
 Result := ArcTan2(-sw*(FNominator[0]*(2*cw*FDenominator[2]+FDenominator[1])+FNominator[1]*(FDenominator[2]-1)-FNominator[2]*(2*cw+FDenominator[1])),
                     (FNominator[0]*(FDenominator[2]*(2*sqr(cw)-1)+1+FDenominator[1]*cw)+FNominator[1]*(cw*FDenominator[2]+cw+FDenominator[1])+FNominator[2]*(2*sqr(cw)+FDenominator[1]*cw+FDenominator[2]-1)));
end;

function TBiquadIIRFilter.Real(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := cos(2 * Frequency * pi * FSRR);
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
 cw := cos(2 * Frequency * pi * FSRR);
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - sqr(cw))
              / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
              + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]))
end;

procedure TBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * pi * FSRR);
 Divider   := 1 / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2*sqr(cw)-1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - sqr(cw)) * Divider;
end;

procedure TBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var cw, Divider : Double;
begin
 cw := cos(2 * Frequency * pi * FSRR);
 Divider   := 1 / ( sqr(FDenominator[2]) - 2 * FDenominator[2] + sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2*sqr(cw)-1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
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

function dB_to_Amp(g:single):single;
begin
 if (g>-90.0) then result := math.power(10,g*0.05)
 else result := 0;
end;

procedure TBiquadIIRFilter.CalcPolesZeros;
var p,q : Double;
    e   : Double;
begin
 p := -FNominator[1]/(2*FNominator[0]);
 q := (FNominator[2]/FNominator[0]);
 FZeros[0].Re := p;
 FZeros[1].Re := p;
 e := q-(p*p);
 if e>0
  then
   begin
    FZeros[0].Im := sqrt(e);
    FZeros[1].Im := -sqrt(e);
   end
  else
   begin
    FZeros[0].Re := FZeros[0].Re+sqrt(-e);
    FZeros[1].Re := FZeros[0].Re-sqrt(-e);
    FZeros[0].Im := 0;
    FZeros[1].Im := 0;
   end;

 p := -FDenominator[1]/2;
 q := FDenominator[2];
 FPoles[0].Re := p;
 FPoles[1].Re := p;
 e := q-(p*p);
 if e>0
  then
   begin
    FPoles[0].Im := sqrt(e);
    FPoles[1].Im := -sqrt(e);
   end
  else
   begin
    FPoles[0].Re := FPoles[0].Re+sqrt(-e);
    FPoles[1].Re := FPoles[0].Re-sqrt(-e);
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

function TBiquadIIRFilter.ProcessSampleASM:Double;
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
 if Length(FStateStack)>1
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

function TBiquadIIRFilter.GetOrder: Integer;
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

{ TSimplePeakFilter }

procedure TSimplePeakFilter.CalculateCoefficients;
var
  t : Double;
begin
 t := FGainFactor / (FGainFactor + FAlpha);
 FDenominator[2] := (FGainFactor - FAlpha) / (FGainFactor + FAlpha);
 FDenominator[1] := -2 * cos(fW0)*t;
 FNominator[1] := FDenominator[1];
 FNominator[0] := (1 + FAlpha * FGainFactor) * t;
 FNominator[2] := (1 - FAlpha * FGainFactor) * t;
 CalcPolesZeros;
end;

{ TSimpleAllpassFilter }

procedure TSimpleAllpassFilter.CalculateCoefficients;
var t, a : Double;
begin
 t               := 1 / (1 + FAlpha);
 a               := FGainFactor * FGainFactor;
 FDenominator[1] := 2 * cos(fW0) * t;
 FDenominator[2] := (FAlpha - 1) * t;
 FNominator[1]   := -FDenominator[1] * a;
 FNominator[0]   := -FDenominator[2] * a;
 FNominator[2]   := a;
end;

{ TSimpleLowShelfFilter }

procedure TSimpleLowShelfFilter.CalculateCoefficients;
var t,A1,A2 : Double;
    cn,sA   : Double;
begin
 sA := 2 * sqrt(FGainFactor) * FAlpha;
 cn := cos(fW0);
 A1 := FGainFactor + 1;
 A2 := FGainFactor - 1;
 t  := 1 / (A1 + A2 * cn + sA);
 FDenominator[1] := -2 * (A2 + A1 * cn) * t;
 FDenominator[2] := (A1 + A2 * cn - sA) * t;
 FNominator[0] := FGainFactor * t * (A1 - A2 * cn + sA);
 FNominator[1] := FGainFactor * t * (A2 - A1 * cn) * 2;
 FNominator[2] := FGainFactor * t * (A1 - A2 * cn - sA);
 CalcPolesZeros;
end;

{ TSimpleHighShelfFilter }

procedure TSimpleHighShelfFilter.CalculateCoefficients;
var t,A1,A2 : Double;
    cn,sA   : Double;
begin
 cn := cos(fW0);
 sA := 2*sqrt(FGainFactor)*FAlpha;
 A1 := FGainFactor + 1;
 A2 := FGainFactor - 1;
 t  := 1 / (A1 - (A2 * cn) + sA);
 FDenominator[1] := 2 * (A2 -A1 * cn) * t;
 FDenominator[2] := (A1 - A2 * cn - sA) * t;
 FNominator[0] := FGainFactor * (A1 + A2 * cn + sA) * t;
 FNominator[1] := FGainFactor * (A2 + A1 * cn) * -2 * t;
 FNominator[2] := FGainFactor * (A1 + A2 * cn - sA) * t;
 CalcPolesZeros;
end;

{ TSimpleHighcut }

procedure TSimpleHighcutFilter.CalculateCoefficients;
var cn, t : Double;
begin
 t := 1/(1+FAlpha);
 cn := cos(fW0);
 FNominator[0]   := sqr(FGainFactor) * (1 - cn) * 0.5 * t;
 FNominator[1]   := 2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

{ TSimpleLowcutFilter }

procedure TSimpleLowcutFilter.CalculateCoefficients;
var cn, t : Double;
begin
 t := 1 / (1 + FAlpha);
 cn := cos(fW0);
 FNominator[0]   := sqr(FGainFactor) * (1 + cn) * 0.5 * t;
 FNominator[1]   := -2 * FNominator[0];
 FNominator[2]   := FNominator[0];
 FDenominator[1] := -2 * cn * t;
 FDenominator[2] := (1 - FAlpha) * t;
 CalcPolesZeros;
end;

{ TSimpleBandpass }

procedure TSimpleBandpass.CalculateCoefficients;
var t : Double;
begin
 t := 1 / (1 + FAlpha);
 FNominator[0]   := FGainFactor*FGainFactor*FAlpha*t;
 FNominator[2]   := -FNominator[0];
 FDenominator[1] := -2*cos(fW0)*t;
 FDenominator[2] := (1-FAlpha)*t;
 FNominator[1]   := 0;
end;

{ TSimpleNotch }

procedure TSimpleNotch.CalculateCoefficients;
var t,a : Double;
begin
 try
  t := 1 / (1 + FAlpha);
  a := sqr(FGainFactor);
  FDenominator[1] := 2 * cos(fW0)*t;
  FNominator[1]   := -FDenominator[1]*a;
  FDenominator[2] := (FAlpha-1)*t;

  FNominator[0] := a * t;
  FNominator[2] := FNominator[0];
 except
  FNominator[0] := 1; FNominator[1] := 0; FNominator[2] := 0; FDenominator[1] := 0; FDenominator[2] := 0;
 end;
end;

{ TSimpleGainFilter }

procedure TSimpleGainFilter.CalculateCoefficients;
begin
 FNominator[0] := sqr(FGainFactor);
 FNominator[1] := 0;
 FNominator[2] := 0;
 FDenominator[1] := 0;
 FDenominator[2] := 0;
end;

function TSimpleGainFilter.ProcessSample(const Input: Double): Double;
begin
 result := Input * sqr(FGainFactor);
end;

function TSimpleGainFilter.ProcessSampleASM: Double;
asm
 fmul [eax.FGainFactor].Double
 fmul [eax.FGainFactor].Double
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
