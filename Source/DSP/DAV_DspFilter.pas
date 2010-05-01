unit DAV_DspFilter;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspAnalogueFilterPrototypes;

type
  TCustomFilter = class(TDspSampleRatePersistent, IDspProcessor32,
    IDspProcessor64)
  protected
    FSRR      : Double; // reciprocal of SampleRate
    procedure SampleRateChanged; override;
    procedure CalculateReciprocalSamplerate; virtual;
    procedure CalculateSamplerateDependentVariables; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    property SampleRateReciprocal: Double read FSRR;
  public
    constructor Create; override;
    {$IFNDEF PUREPASCAL}
    function ProcessSampleASM: Double; virtual;
    {$ENDIF}
    function ProcessSample32(Input: Single): Single; virtual;
    function ProcessSample64(Input: Double): Double; overload; virtual; abstract;
    function ProcessSample64(Input: Int64): Int64; overload; virtual; abstract;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); virtual;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer); virtual;
    function MagnitudeSquared(const Frequency: Double): Double; virtual; abstract;
    function MagnitudeLog10(const Frequency: Double): Double; virtual; abstract;
    function Real(const Frequency: Double): Double; virtual; abstract;
    function Imaginary(const Frequency: Double): Double; virtual; abstract;
    function Phase(const Frequency: Double): Double; virtual;
    procedure PushStates; virtual; abstract;
    procedure PopStates; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; virtual; abstract;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Single); overload; virtual;
    procedure ResetStates; virtual; abstract;
    procedure ResetStatesInt64; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure GetIR(ImpulseResonse : TDAVSingleDynArray); overload;
    procedure GetIR(ImpulseResonse : TDAVDoubleDynArray); overload;
  end;

  TCustomFilterCascade = class(TCustomFilter)
  private
    FOwnFilters: Boolean;
    function GetFilter(Index: Integer): TCustomFilter;
  protected
    FFilterArray : array of TCustomFilter;
    procedure CalculateSamplerateDependentVariables; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; overload; override;
    function ProcessSample64(Input: Int64): Int64; overload; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
    procedure AddFilter(Filter: TCustomFilter); virtual;
    procedure Clear; virtual;
    procedure Delete(Filter: TCustomFilter); overload; virtual;
    procedure Delete(Index: Integer); overload; virtual;
    procedure PushStates; override;
    procedure PopStates; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure Reset; override;

    property OwnFilters: Boolean read FOwnFilters write FOwnFilters;
    property Filter[Index: Integer]: TCustomFilter read GetFilter;
  end;

  TFilterCascade = class(TCustomFilterCascade)
  published
    property OwnFilters;
  end;

  TCustomFilterWithOrder = class(TCustomFilter)
  protected
    function GetOrder: Cardinal; virtual; abstract;
    procedure SetOrder(const Value: Cardinal); virtual; abstract;
    procedure CalculateCoefficients; virtual; abstract;
    procedure CoefficientsChanged; virtual;
  public  
    property Order: Cardinal read GetOrder write SetOrder;
  end;

  TCustomGainFrequencyFilter = class(TCustomFilterWithOrder)
  private
    procedure SetFrequency(Value: Double);
    procedure SetGaindB(const Value: Double);
  protected
    FGain_dB           : Double;
    FGainFactor        : Double;
    FGainFactorSquared : Double;
    FFrequency, FW0    : Double;
    FExpW0             : TComplexDouble;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateW0; virtual;
    procedure CalculateGainFactor; virtual;
    procedure FrequencyChanged; virtual;
    procedure GainChanged; virtual;
    procedure CalculateSamplerateDependentVariables; override;

    property GainFactor: Double read FGainFactor;
    property ExpW0: TComplexDouble read FExpW0;
    property W0: Double read FW0;
  public
    constructor Create; override;
    property Gain: Double read FGain_dB write SetGaindB;
    property Frequency: Double read FFrequency write SetFrequency;
  end;

  TOrderFilterClass = class of TCustomOrderFilter;
  TCustomOrderFilter = class(TCustomGainFrequencyFilter)
  protected
    FOrder: Cardinal;
    class function GetMaxOrder: Cardinal; virtual; abstract;
    function GetOrder: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure OrderChanged; virtual;
    procedure SetOrder(const Value: Cardinal); override;
  public
    constructor Create(const Order: Integer = 0); reintroduce; virtual;
  end;

  TFIRFilterClass = class of TCustomFIRFilter;
  TCustomFIRFilter = class(TCustomGainFrequencyFilter)
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

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
//    function ProcessSample64(Input: Int64): Int64; override;
//    function ProcessSampleASM: Double; override;
    procedure PushStates; override;
    procedure PopStates; override;
    property KernelSize: Integer Read FKernelSize Write SetKernelSize;
  end;

  TIIRFilterClass = class of TCustomIIRFilter;
  TCustomIIRFilter = class(TCustomGainFrequencyFilter)
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

  TCustomBiquadIIRFilter = class(TCustomBandwidthIIRFilter)
  protected
    FDenominator  : array [1..2] of Double;
    FNominator    : array [0..2] of Double;
    FPoles        : array [0..1] of TComplexSingle;
    FZeros        : array [0..1] of TComplexSingle;
    FState        : array [0..1] of Double;
    FStateStack   : array of array[0..1] of Double;
    procedure CalculatePoleZeroes; virtual;
    function GetOrder: Cardinal; override;
    procedure SetOrder(const Value: Cardinal); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CoefficientsChanged; override;
  public
    constructor Create; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    function ProcessSample32(Input: Single): Single; override;
    function ProcessSample64(Input: Double): Double; override;
    function ProcessSample64(Input: Int64): Int64; override;
    {$IFNDEF PUREPASCAL}
    function ProcessSampleASM: Double; override;
    {$ENDIF}
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double) :Double; override;
    function Phase(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Double); overload; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary : Single); overload; override;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
  end;

  TBiquadIIRFilter = class(TCustomBiquadIIRFilter)
  published
    property Gain;
    property Frequency;
    property SampleRate;
    property Bandwidth;
  end;

  TBilinearTransformedBiquadIIRFilter = class(TCustomBiquadIIRFilter)
  private
    function GetAnaloguePrototypeClass: TCustomBiquadAnalogueFilterPrototypeClass;
    procedure SetAnaloguePrototype(const Value: TCustomBiquadAnalogueFilterPrototype);
    procedure SetAnaloguePrototypeClass(const Value: TCustomBiquadAnalogueFilterPrototypeClass);
  protected
    FBandwidthWarpFactor : Double;
    FWarpedFrequency     : Double;
    FAnaloguePrototype   : TCustomBiquadAnalogueFilterPrototype;
    FPrototypeOwned      : Boolean;
    procedure GainChanged; override;
    procedure FrequencyChanged; override;
    procedure BandwidthChanged; override;
    procedure CalculateW0; override;
    procedure CalculateAlpha; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CalculateCoefficients; override;
  published
    property Gain;
    property Frequency;
    property SampleRate;
    property Bandwidth;
    property AnaloguePrototypeClass: TCustomBiquadAnalogueFilterPrototypeClass
      read GetAnaloguePrototypeClass write SetAnaloguePrototypeClass;
    property AnaloguePrototype: TCustomBiquadAnalogueFilterPrototype
      read FAnaloguePrototype write SetAnaloguePrototype;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, SysUtils, DAV_Common, DAV_Math, DAV_DspDFT;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TCustomFilter }

constructor TCustomFilter.Create;
begin
 inherited;
 CalculateReciprocalSamplerate;
end;

procedure TCustomFilter.SampleRateChanged;
begin
 CalculateSampleRateDependentVariables;
 Changed;
end;

procedure TCustomFilter.CalculateSamplerateDependentVariables;
begin
 CalculateReciprocalSamplerate;
end;

procedure TCustomFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFilter then
  with TCustomFilter(Dest) do
   begin
    inherited;
    FSRR := Self.FSRR;
   end
  else inherited;
end;

procedure TCustomFilter.CalculateReciprocalSamplerate;
begin
 FSRR := 1 / SampleRate;
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

procedure TCustomFilter.GetIR(ImpulseResonse: TDAVSingleDynArray);
var
  SampleIndex : Cardinal;
begin
 if Length(ImpulseResonse) = 0 then Exit;
 PushStates;
 ImpulseResonse[0] := ProcessSample64(1.0);
 for SampleIndex := 1 to Length(ImpulseResonse) - 1
  do ImpulseResonse[SampleIndex] := ProcessSample64(0.0);
 PopStates;
end;

procedure TCustomFilter.GetIR(ImpulseResonse: TDAVDoubleDynArray);
var
  SampleIndex : Cardinal;
begin
 if Length(ImpulseResonse) = 0 then Exit;
 PushStates;
 ImpulseResonse[0] := ProcessSample64(1.0);
 for SampleIndex := 1 to Length(ImpulseResonse) - 1
  do ImpulseResonse[SampleIndex] := ProcessSample64(0.0);
 PopStates;
end;

function TCustomFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TCustomFilter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32(Data[SampleIndex]);
end;

procedure TCustomFilter.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample64(Data[SampleIndex]);
end;

function TCustomFilter.ProcessSample32(Input: Single): Single;
begin
 Result := ProcessSample64(Input);
end;

{$IFNDEF PUREPASCAL}
function TCustomFilter.ProcessSampleASM: Double;
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


{ TCustomFilterCascade }

constructor TCustomFilterCascade.Create;
begin
 inherited;
 SetLength(FFilterArray, 0);
 OwnFilters := True;
end;

procedure TCustomFilterCascade.Delete(Filter: TCustomFilter);
var
  i : Integer;
begin
 i := 0;
 while i < Length(FFilterArray) do
  if FFilterArray[i] = Filter then
   begin
    if (Length(FFilterArray) - 1 - i) > 0
     then Move(FFilterArray[i + 1], FFilterArray[i], (Length(FFilterArray) - 1 - i) * SizeOf(Single));
    SetLength(FFilterArray, Length(FFilterArray) - 1);
   end
  else inc(i);
 if OwnFilters
  then FreeAndNil(Filter);
end;

procedure TCustomFilterCascade.Delete(Index: Integer);
begin
 if (Index >= 0) and (Index < Length(FFilterArray))
  then
   begin
    if OwnFilters then FreeAndNil(FFilterArray[Index]);
    if (Length(FFilterArray) - 1 - Index) > 0
     then Move(FFilterArray[Index + 1], FFilterArray[Index], (Length(FFilterArray) - 1 - Index) * SizeOf(Single));
    SetLength(FFilterArray, Length(FFilterArray) - 1);
   end
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

destructor TCustomFilterCascade.Destroy;
begin
 Clear;
 inherited;
end;

procedure TCustomFilterCascade.AddFilter(Filter: TCustomFilter);
begin
 SetLength(FFilterArray, Length(FFilterArray) + 1);
 FFilterArray[Length(FFilterArray) - 1] := Filter;
end;

function TCustomFilterCascade.GetFilter(Index: Integer): TCustomFilter;
begin
 if (Index >= 0) and (Index < Length(FFilterArray))
  then Result := FFilterArray[Index]
  else Result := nil;
end;

procedure TCustomFilterCascade.Complex(const Frequency: Double; out Real,
  Imaginary: Double);
var
  i   : Integer;
  Tmp : TComplexDouble;
begin
 if Length(FFilterArray) = 0 then exit;
 Assert(Assigned(FFilterArray[0]));
 FFilterArray[0].Complex(Frequency, Real, Imaginary);
 for i := 1 to Length(FFilterArray) - 1 do
  begin
   Assert(Assigned(FFilterArray[i]));
   FFilterArray[i].Complex(Frequency, Tmp.Re, Tmp.Im);
   ComplexMultiply(Real, Imaginary, Tmp.Re, Tmp.Im);
  end;
end;

function TCustomFilterCascade.Real(const Frequency: Double): Double;
var
  Imag : Double;
begin
 Complex(Frequency, Result, Imag);
end;

function TCustomFilterCascade.Imaginary(const Frequency: Double): Double;
var
  Real : Double;
begin
 Complex(Frequency, Real, Result);
end;

function TCustomFilterCascade.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * log10(MagnitudeSquared(Frequency));
end;

function TCustomFilterCascade.MagnitudeSquared(const Frequency: Double): Double;
var
  i   : Integer;
begin
 if Length(FFilterArray) = 0 then
  begin
   Result := 1;
   exit;
  end;
 Assert(Assigned(FFilterArray[0]));
 Result := FFilterArray[0].MagnitudeSquared(Frequency);
 for i := 1 to Length(FFilterArray) - 1 do
  begin
   Assert(Assigned(FFilterArray[i]));
   Result := Result * FFilterArray[i].MagnitudeSquared(Frequency);
  end;
end;

procedure TCustomFilterCascade.PopStates;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].PopStates;
end;

procedure TCustomFilterCascade.PushStates;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].PushStates;
end;

procedure TCustomFilterCascade.Reset;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].Reset;
end;

procedure TCustomFilterCascade.ResetStates;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].ResetStates;
end;

procedure TCustomFilterCascade.ResetStatesInt64;
var
  i : Integer;
begin
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].ResetStatesInt64;
end;

procedure TCustomFilterCascade.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFilterCascade then
  with TCustomFilterCascade(Dest) do
   begin
    inherited;
    FOwnFilters  := Self.FOwnFilters;
    FFilterArray := Self.FFilterArray;
   end
  else inherited;
end;

procedure TCustomFilterCascade.Clear;
var
  i : Integer;
begin
 if OwnFilters then
  for i := 0 to Length(FFilterArray) - 1 do
   if Assigned(FFilterArray[i])
    then FreeAndNil(FFilterArray[i]);
 SetLength(FFilterArray, 0);
end;

procedure TCustomFilterCascade.CalculateSamplerateDependentVariables;
var
  i : Integer;
begin
 inherited;
 for i := 0 to Length(FFilterArray) - 1
  do FFilterArray[i].SampleRate := SampleRate;
end;

function TCustomFilterCascade.ProcessSample32(Input: Single): Single;
var
  Band : Integer;
begin
 Result := Input;
 for Band := 0 to Length(FFilterArray) - 1
  do Result := FFilterArray[Band].ProcessSample32(Result);
end;

function TCustomFilterCascade.ProcessSample64(Input: Double): Double;
var
  Band : Integer;
begin
 Result := Input;
 for Band := 0 to Length(FFilterArray) - 1
  do Result := FFilterArray[Band].ProcessSample64(Result);
end;

function TCustomFilterCascade.ProcessSample64(Input: Int64): Int64;
var
  i : Integer;
begin
 Result := Input;
 for i := 0 to Length(FFilterArray) - 1
  do Result := FFilterArray[i].ProcessSample64(Result);
end;


{ TCustomFilterWithOrder }

procedure TCustomFilterWithOrder.CoefficientsChanged;
begin
 CalculateCoefficients;
end;


{ TCustomGainFrequencyFilter }

constructor TCustomGainFrequencyFilter.Create;
begin
 inherited;
 FGain_dB           := 0;
 FGainFactor        := 1;
 FGainFactorSquared := 1;
 FFrequency         := 1000;
 CalculateW0;
end;

procedure TCustomGainFrequencyFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomGainFrequencyFilter then
  with TCustomGainFrequencyFilter(Dest) do
   begin
    inherited;
    FGain_dB    := Self.FGain_dB;
    FGainFactor := Self.FGainFactor;
    FSRR        := Self.FSRR;
    FW0         := Self.FW0;
    FExpW0      := Self.FExpW0;
   end
  else inherited;
end;

procedure TCustomGainFrequencyFilter.CalculateGainFactor;
begin
 FGainFactor := dB_to_Amp(CHalf32 * FGain_dB); // do not change this!
 FGainFactorSquared := Sqr(FGainFactor);
end;

procedure TCustomGainFrequencyFilter.CalculateW0;
begin
 FW0 := 2 * Pi * FFrequency * FSRR;
 GetSinCos(FW0, FExpW0.Im, FExpW0.Re);
 if FW0 > 3.141
  then FW0 := 3.141;
end;

procedure TCustomGainFrequencyFilter.FrequencyChanged;
begin
 CalculateW0;
 CoefficientsChanged;
 Changed;
end;

procedure TCustomGainFrequencyFilter.GainChanged;
begin
 CalculateGainFactor;
 CoefficientsChanged;
 Changed;
end;

procedure TCustomGainFrequencyFilter.CalculateSamplerateDependentVariables;
begin
 inherited;
 CalculateW0;
 CoefficientsChanged;
end;

procedure TCustomGainFrequencyFilter.SetFrequency(Value: Double);
begin
 if Value < 1E-10
  then Value := 1E-10;
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomGainFrequencyFilter.SetGaindB(const Value: Double);
begin
 if FGain_dB <> Value then
  begin
   FGain_dB := Value;
   GainChanged;
  end;
end;

{ TCustomOrderFilter }

procedure TCustomOrderFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomOrderFilter then
  with TCustomOrderFilter(Dest) do
   begin
    inherited;
    FOrder  := Self.FOrder;
   end
  else inherited;
end;

constructor TCustomOrderFilter.Create(const Order: Integer);
begin
 FOrder := Order;
 OrderChanged;

 inherited Create;
end;

function TCustomOrderFilter.GetOrder: Cardinal;
begin
 Result := FOrder;
end;

procedure TCustomOrderFilter.OrderChanged;
begin
 CoefficientsChanged;
 Changed;
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

procedure TCustomFIRFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomFIRFilter then
  with TCustomFIRFilter(Dest) do
   begin
    inherited;
    FKernelSize := Self.FKernelSize;
    FIR         := Self.FIR;
    FHistory    := Self.FHistory;
    FCircular   := Self.FCircular;
    FSpeedTab   := Self.FSpeedTab;
    FStateStack := Self.FStateStack;
    FBufferPos  := Self.FBufferPos;
   end
  else inherited;
end;

constructor TCustomFIRFilter.Create;
begin
 inherited;
end;

function TCustomFIRFilter.GetOrder: Cardinal;
begin
 Result := FKernelSize;
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
 Result := FGainFactor * (Sqr(Cmplx.Re) + Sqr(Cmplx.Im));
end;

procedure TCustomFIRFilter.PopStates;
begin
 Move(FStateStack[0], FHistory[0], Length(FHistory) * SizeOf(Double));
 Move(FStateStack[Length(FHistory)], FCircular[0], Length(FCircular) * SizeOf(Double));
end;

procedure ConvolveIR_X87(InOutBuffer, IRBuffer: PDAVDoubleFixedArray; SampleFrames: Integer;
  Current: Double);
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1
  do InOutBuffer[SampleIndex] := InOutBuffer[SampleIndex] + IRBuffer[SampleIndex] * Current;
{$ELSE}
asm
  fld   Current.Double
  @SmallLoop:
  fld   [edx].Double
  fmul  st(0),st(1)
  fld   [eax].Double
  faddp st(1), st(0)

  fstp  [eax].Double
  add   eax, 8
  add   edx, 8
  loop  @SmallLoop

  @EndSmallLoop:
  ffree st(0)
{$ENDIF}
end;

procedure ConvolveIR_X87large(InOutBuffer, IRBuffer: PDAVDoubleFixedArray;
  SampleFrames: Integer; Current: Double);
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1
  do InOutBuffer[SampleIndex] := InOutBuffer[SampleIndex] + IRBuffer[SampleIndex] * Current;
{$ELSE}
asm
  fld   Current.Double

  push ecx
  shr ecx,2
  jz @SkipLargeAddLoop
  @LargeLoop:
  fld   [edx].Double
  fmul  st(0),st(1)
  fld   [eax].Double
  faddp st(1), st(0)
  fstp  [eax].Double
  fld   [edx+8].Double
  fmul  st(0),st(1)
  fld   [eax+8].Double
  faddp st(1), st(0)
  fstp  [eax+8].Double
  fld   [edx+16].Double
  fmul  st(0),st(1)
  fld   [eax+16].Double
  faddp st(1), st(0)
  fstp  [eax+16].Double
  fld   [edx+24].Double
  fmul  st(0),st(1)
  fld   [eax+24].Double
  faddp st(1), st(0)
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
  faddp st(1), st(0)
  fstp [eax].Double

  add   eax, 8
  add   edx, 8
  loop  @SmallLoop

  @EndSmallLoop:
  ffree st(0)
{$ENDIF}
end;

function TCustomFIRFilter.ProcessSample32(Input: Single): Single;
begin
 FHistory[FBufferPos] := Input;
 Result := (FCircular[FBufferPos] + FHistory[FBufferPos] * FIR[0]);
 ConvolveIR_X87large(@FCircular[FBufferPos], @FIR[0], FKernelSize, FHistory[FBufferPos]);
 Inc(FBufferPos);
 if FBufferPos >= FKernelSize then
  begin
   FBufferPos := 0;
   Move(FCircular[FKernelSize], FCircular[0], FKernelSize * SizeOf(Double));
   FillChar(FCircular[FKernelSize], FKernelSize * SizeOf(Double), 0);
  end;
end;

function TCustomFIRFilter.ProcessSample64(Input: Double): Double;
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
 CoefficientsChanged;
 Changed;
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
  else FAlpha := Sinh(ln22 * Sqrt(0.5 * (1 + FExpW0.Re)) * FBandWidth * (FW0 / FExpW0.Im)) * FExpW0.Im;
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

{ TCustomBiquadIIRFilter }

constructor TCustomBiquadIIRFilter.Create;
begin
 inherited;
 FBandWidth := 1;
 CalculateCoefficients;
 CalculatePoleZeroes;
 ResetStates;
end;

function TCustomBiquadIIRFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * Pi * FSRR);
 Result := (Sqr(FNominator[0] - FNominator[2]) + Sqr(FNominator[1]) + (FNominator[1] * (FNominator[0] + FNominator[2]) + FNominator[0] * FNominator[2] * cw) * cw)
         / (Sqr(1 - FDenominator[2]) + Sqr(FDenominator[1]) + (FDenominator[1] * (FDenominator[2] + 1) + cw * FDenominator[2]) * cw );
end;

function TCustomBiquadIIRFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := 10 * Log10(MagnitudeSquared(Frequency));
end;

function TCustomBiquadIIRFilter.Phase(const Frequency: Double): Double;
var
  cw, sw : Double;
begin
 GetSinCos(2 * Frequency * Pi * FSRR, sw, cw);
 Result := ArcTan2(-sw * (FNominator[0] * (2 * cw * FDenominator[2] + FDenominator[1]) + FNominator[1] * (FDenominator[2] - 1) - FNominator[2] * (2 * cw + FDenominator[1])),
                  (FNominator[0] * (FDenominator[2] * (2 * Sqr(cw) - 1) + 1 + FDenominator[1] * cw) + FNominator[1] * (cw * (FDenominator[2] + 1) + FDenominator[1]) + FNominator[2] * (2 * Sqr(cw) + FDenominator[1] * cw + FDenominator[2] - 1)));
end;

function TCustomBiquadIIRFilter.Real(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Real := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
          +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
          + (2 * Sqr(cw) - 1) * (FNominator[0] * FDenominator[2] + FNominator[2]))
          / ( Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
          + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
end;

function TCustomBiquadIIRFilter.Imaginary(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - Sqr(cw))
              / ( Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
              + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]))
end;

procedure TCustomBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
var
  cw, Divider : Double;
begin
 cw := cos(2 * Frequency * Pi * FSRR);
 Divider   := 1 / ( Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2 * Sqr(cw)-1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - Sqr(cw)) * Divider;
end;

procedure TCustomBiquadIIRFilter.CoefficientsChanged;
begin
 inherited;
 // CalculatePoleZeroes;
end;

procedure TCustomBiquadIIRFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  Cw, Divider : Double;
begin
 cw := cos(2 * Frequency * pi * FSRR);
 Divider   := 1 / ( Sqr(FDenominator[2]) - 2 * FDenominator[2] + Sqr(FDenominator[1]) + 1
                    + 2 * cw * (FDenominator[1] * (FDenominator[2] + 1) + 2 * cw * FDenominator[2]));
 Real      := (FNominator[0] + FNominator[1] * FDenominator[1] + FNominator[2] * FDenominator[2]
              +        cw     * (FNominator[1] * (1 + FDenominator[2]) + FDenominator[1] * (FNominator[2] + FNominator[0]))
              + (2 * Sqr(cw) - 1) * (FNominator[0] * FDenominator[2] + FNominator[2])) * Divider;
 Imaginary := (FDenominator[1] * (FNominator[2] - FNominator[0]) + FNominator[1] * (1 - FDenominator[2])
              + 2 * cw * (FNominator[2] - FNominator[0] * FDenominator[2])) * sqrt(1 - Sqr(cw)) * Divider;
end;

procedure TCustomBiquadIIRFilter.Reset;
begin
 Gain := 0;
end;

procedure TCustomBiquadIIRFilter.ResetStates;
begin
 FState[0] := 0;
 FState[1] := 0;
end;

procedure TCustomBiquadIIRFilter.ResetStatesInt64;
begin
 PInt64(@FState[0])^ := 0;
 PInt64(@FState[1])^ := 0;
end;

procedure TCustomBiquadIIRFilter.SetOrder(const Value: Cardinal);
begin
 raise Exception.Create('Order is fixed!');
end;

procedure TCustomBiquadIIRFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomBiquadIIRFilter then
  with TCustomBiquadIIRFilter(Dest) do
   begin
    inherited;
    FDenominator  := Self.FDenominator;
    FNominator    := Self.FNominator;
    FPoles        := Self.FPoles;
    FZeros        := Self.FZeros;
    FState        := Self.FState;
    FStateStack   := Self.FStateStack;
   end
  else inherited;
end;

procedure TCustomBiquadIIRFilter.CalculatePoleZeroes;
var
  p, q : Double;
  e    : Double;
begin
 p := -FNominator[1] / (2 * FNominator[0]);
 q := (FNominator[2] / FNominator[0]);
 FZeros[0].Re := p;
 FZeros[1].Re := p;
 e := q - Sqr(p);
 if e > 0
  then
   begin
    FZeros[0].Im := sqrt(e);
    FZeros[1].Im := -FZeros[0].Im;
   end
  else
   begin
    FZeros[0].Re := FZeros[0].Re + Sqrt(-e);
    FZeros[1].Re := FZeros[0].Re - Sqrt(-e);
    FZeros[0].Im := 0;
    FZeros[1].Im := 0;
   end;

 p := -FDenominator[1] * 0.5;
 q :=  FDenominator[2];
 FPoles[0].Re := p;
 FPoles[1].Re := p;
 e := q - Sqr(p);
 if e > 0
  then
   begin
    FPoles[0].Im := sqrt(e);
    FPoles[1].Im := -FPoles[0].Im;
   end
  else
   begin
    FPoles[0].Re := FPoles[0].Re + Sqrt(-e);
    FPoles[1].Re := FPoles[0].Re - Sqrt(-e);
    FPoles[0].Im := 0;
    FPoles[1].Im := 0;
   end;
end;

function TCustomBiquadIIRFilter.ProcessSample64(Input: Double): Double;
{$IFDEF PUREPASCAL}
begin
 Result    := FNominator[0] * Input + FState[0];
 FState[0] := FNominator[1] * Input - FDenominator[1] * Result + FState[1];
 FState[1] := FNominator[2] * Input - FDenominator[2] * Result;
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
 fadd [self.FState + 8].Double       // d1 + a1 * Input - b0 * r, r, r
 fstp [self.FState].Double           // d0 = a1 * Input + d1 + b1 * r, r, r
 fmul [self.FDenominator + 8].Double // b1*r, r
 fld Input.Double                    // Input, b1*r, r
 fmul [self.FNominator + 16].Double  // a2*Input, b1*r, r
 fsubrp st(1), st(0)                 // b1*r + a2*Input, r !!!
 fstp [self.FState + 8].Double       // d1 = b1*r + a2*Input, r !!!
end;
{$ENDIF}

function TCustomBiquadIIRFilter.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result    := FNominator[0] * Input + FState[0];
 FState[0] := FNominator[1] * Input - FDenominator[1] * Result + FState[1];
 FState[1] := FNominator[2] * Input - FDenominator[2] * Result;
end;
{$ELSE}
asm
 fld Input.Single                    // Input
 fmul [self.FNominator].Double       // a0 * Input
 fadd [self.FState].Double           // r = d0 + a0 * Input
 fld st(0)                           // r, r
 fld st(0)                           // r, r, r
 fmul [self.FDenominator].Double     // b0 * r, r, r
 fld Input.Single                    // Input, b0 * r, r, r
 fmul [self.FNominator + 8].Double   // a1 * Input, b0 * r, r, r
 fsubrp                              // a1 * Input + b0 * r, r, r
 fadd [self.FState + 8].Double       // d1 + a1 * Input - b0 * r, r, r
 fstp [self.FState].Double           // d0 = a1 * Input + d1 + b1 * r, r, r
 fmul [self.FDenominator + 8].Double // b1 * r, r
 fld Input.Single                    // Input, b1 * r, r
 fmul [self.FNominator + 16].Double  // a2*Input, b1 * r, r
 fsubrp st(1), st(0)                 // b1 * r + a2 * Input, r !!!
 fstp [self.FState + 8].Double       // d1 = b1 * r + a2 * Input, r !!!
end;
{$ENDIF}

function TCustomBiquadIIRFilter.ProcessSample64(Input: Int64): Int64;
begin
 Result              := Round(FNominator[0] * Input) + PInt64(@FState[0])^;
 PInt64(@FState[0])^ := Round(FNominator[1] * Input) - Round(FDenominator[1] * Result) + PInt64(@FState[1])^;
 PInt64(@FState[1])^ := Round(FNominator[2] * Input) - Round(FDenominator[2] * Result);
end;

{$IFNDEF PUREPASCAL}
function TCustomBiquadIIRFilter.ProcessSampleASM: Double;
asm
 fld st(0)                           // s, s
 fmul [self.FNominator].Double       // a0*s, s
 fadd [self.FState].Double           // r = d0+a0*s, s
 fld st(0)                           // r, r, s
 fld st(0)                           // r, r, r, s
 fmul [self.FDenominator].Double     // b0*r, r, r, s
 fld st(3)                           // s, b0*r, r, r, s
 fmul [self.FNominator + 8].Double   // a1*s, b0*r, r, r, s
 fsubrp                              // a1*s + b0*r, r, r, s
 fadd [self.FState + 8].Double       // d1+a1*s-b0*r, r, r, s

 fstp [self.FState].Double           // d0 = a1*s + d1+b1*r, r, r, s
 fmul [self.FDenominator + 8].Double // b1*r, r, s
 fxch st(2)                          // s, r, b1*r,
 fmul [self.FNominator + 16].Double  // a2*s, r, b1*r,
 fsubrp st(2), st(0)                 // b1*r + a2*s, r, !!!
 fxch
 fstp [self.FState + 8].Double       // d1 = b1*r + a2*s, r, !!!
end;
{$ENDIF}

procedure TCustomBiquadIIRFilter.PushStates;
begin
 SetLength(FStateStack, Length(FStateStack) + 1);
 if Length(FStateStack) > 1
  then Move(FStateStack[0, 0],FStateStack[1, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
 Move(FState[0], FStateStack[0, 0], Length(FStateStack[0]) * SizeOf(Double));
end;

procedure TCustomBiquadIIRFilter.PopStates;
begin
 if Length(FStateStack) > 0 then
  begin
   Move(FStateStack[0, 0], FState[0], Length(FStateStack[0]) * SizeOf(Double));
   if Length(FStateStack) > 1
    then Move(FStateStack[1, 0], FStateStack[0, 0], (Length(FStateStack) - 1) * Length(FStateStack[0]) * SizeOf(Double));
   SetLength(FStateStack, Length(FStateStack) - 1);
  end;
end;

function TCustomBiquadIIRFilter.GetOrder: Cardinal;
begin
 Result := 2;
end;

{ TBilinearTransformedBiquadIIRFilter }

constructor TBilinearTransformedBiquadIIRFilter.Create;
begin
 inherited;
end;

destructor TBilinearTransformedBiquadIIRFilter.Destroy;
begin
 if FPrototypeOwned and Assigned(FAnaloguePrototype)
  then FreeAndNil(FAnaloguePrototype);
  
 inherited;
end;

procedure TBilinearTransformedBiquadIIRFilter.CalculateW0;
begin
 inherited;
 FWarpedFrequency := FExpW0.Im / (1 + FExpW0.Re);
end;

procedure TBilinearTransformedBiquadIIRFilter.CalculateAlpha;
begin
 FBandwidthWarpFactor := 1 / Sqrt(0.5 * (1 + FExpW0.Re));

 if Assigned(FAnaloguePrototype)
  then FAnaloguePrototype.Bandwidth := FBandWidth * FBandwidthWarpFactor;
end;

procedure TBilinearTransformedBiquadIIRFilter.CalculateCoefficients;
var
  K, K2   : Double;
  Divisor : Double;
  AnaNom  : array [0..2] of Double;
  AnaDen  : array [0..2] of Double;
begin
 if Assigned(FAnaloguePrototype) then
  begin
   AnaNom[0] := FAnaloguePrototype.Nominator[0];
   AnaNom[1] := FAnaloguePrototype.Nominator[1];
   AnaNom[2] := FAnaloguePrototype.Nominator[2];
   AnaDen[0] := FAnaloguePrototype.Denominator[0];
   AnaDen[1] := FAnaloguePrototype.Denominator[1];
   AnaDen[2] := FAnaloguePrototype.Denominator[2];

   K := FWarpedFrequency;
   K2 := Sqr(K);

   Divisor := 1 / (AnaDen[0] * K2 + AnaDen[1] * K + AnaDen[2]);

   FNominator[0] := (AnaNom[0] * K2 + AnaNom[1] * K + AnaNom[2]) * Divisor;
   FNominator[1] := 2 * (AnaNom[0] * K2 - AnaNom[2]) * Divisor;
   FNominator[2] := (AnaNom[0] * K2 - AnaNom[1] * K + AnaNom[2]) * Divisor;
   FDenominator[1] := 2 * (AnaDen[0] * K2 - AnaDen[2]) * Divisor;
   FDenominator[2] := (AnaDen[0] * K2 - AnaDen[1] * K + AnaDen[2]) * Divisor;
  end
 else
  begin
   FNominator[0] := 1;
   FNominator[1] := 0;
   FNominator[2] := 0;
   FDenominator[1] := 0;
   FDenominator[2] := 0;
  end;
 inherited;
end;

procedure TBilinearTransformedBiquadIIRFilter.BandwidthChanged;
begin
 if Assigned(FAnaloguePrototype)
  then FAnaloguePrototype.Bandwidth := FBandWidth * FBandwidthWarpFactor;
 inherited;
end;

procedure TBilinearTransformedBiquadIIRFilter.FrequencyChanged;
begin
 if Assigned(FAnaloguePrototype) then
  begin
   FAnaloguePrototype.Frequency := FFrequency;
   FAnaloguePrototype.Bandwidth := FBandWidth * FBandwidthWarpFactor;
  end;
 inherited;
end;

procedure TBilinearTransformedBiquadIIRFilter.GainChanged;
begin
 if Assigned(FAnaloguePrototype)
  then FAnaloguePrototype.Gain := FGain_dB;
 inherited;
end;

function TBilinearTransformedBiquadIIRFilter.GetAnaloguePrototypeClass: TCustomBiquadAnalogueFilterPrototypeClass;
begin
 if Assigned(FAnaloguePrototype)
  then Result := TCustomBiquadAnalogueFilterPrototypeClass(FAnaloguePrototype.ClassType)
  else Result := nil;
end;

procedure TBilinearTransformedBiquadIIRFilter.SetAnaloguePrototype(
  const Value: TCustomBiquadAnalogueFilterPrototype);
var
  OldPrototype : TCustomBiquadAnalogueFilterPrototype;
begin
 if FPrototypeOwned then
  begin
   OldPrototype := FAnaloguePrototype;
   FAnaloguePrototype := Value;
   if Assigned(OldPrototype)
    then FreeAndNil(OldPrototype);
   FPrototypeOwned := False;
  end
 else FAnaloguePrototype := Value
end;

procedure TBilinearTransformedBiquadIIRFilter.SetAnaloguePrototypeClass(
  const Value: TCustomBiquadAnalogueFilterPrototypeClass);
var
  OldPrototype : TCustomBiquadAnalogueFilterPrototype;
begin
 if Value <> GetAnaloguePrototypeClass then
  begin
   OldPrototype := FAnaloguePrototype;
   FAnaloguePrototype := Value.Create;
   FPrototypeOwned := True;
   if Assigned(OldPrototype) then
    begin
     FAnaloguePrototype.Assign(OldPrototype);
     FreeAndNil(OldPrototype);
    end
   else
    begin
     FAnaloguePrototype.Frequency := FFrequency;
     FAnaloguePrototype.Bandwidth := FBandWidth * FExpW0.Re;
     FAnaloguePrototype.Gain := FGain_dB;
    end;
  end;
end;

end.
