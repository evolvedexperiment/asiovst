unit DAV_DspFDNReverb;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_VectorMath, DAV_DspFilter, 
  DAV_DspFeedbackDelayNetwork, DAV_DspVibrato;

type
  TDampingFilter = class(TCustomFilter)
  protected
    FCoeffs : array [0..1] of Double;
    FState  : Double;
    procedure CalculateW0; override;
  public
    constructor Create; override;
    function Imaginary(const Frequency: Double): Double; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function MagnitudeSquared(const Frequency: Double): Double; override;
    function Phase(const Frequency: Double): Double; override;
    function ProcessSample(const Input: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    procedure CalculateCoefficients; override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Double); override;
    procedure Complex(const Frequency: Double; out Real, Imaginary: Single); override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure SetFilterValues(const AFrequency, AGain : Single); virtual;
    procedure Reset; override;
    procedure PushStates; override;
    procedure PopStates; override;
  end;

  TReverbGeometry = (rgSphere, rgBox1, rgBox2, rgDense1, rgDense2, rgDrum,
    rgHallway, rgRoom1, rgRoom2, rgSparse1, rgSparse2, rgMinor, rgMajor,
    rgHarmonic, rgOctaves, rgFifth, rgDetuned);

  TCustomDspFDNReverb = class(TDspObject)
  private
    FDamping           : Double;
    FDryMix            : Double;
    FFeedbackRotation  : TDAV2DoubleArray;
    FFeedbackInversion : Double;
    FHalfLife          : Double;
    FInputAngle        : Double;
    FModulationDepth   : Double;
    FModulationSpread  : Double;
    FNonLinearGain     : Double;
    FOutputAngle       : Double;
    FWetMix            : Double;
    FSampleRate        : Single;
    FHold              : Boolean;
    FModulationActive  : Boolean;
    FNonLinearActive   : Boolean;
    FGeometry          : TReverbGeometry;
    FBaseDelay: Double;
    function GetFeedbackRotation(Index: Integer): Double;
    procedure SetDamping(const Value: Double);
    procedure SetDryMix(const Value: Double);
    procedure SetFeedbackInversion(const Value: Double);
    procedure SetFeedbackRotation(Index: Integer; const Value: Double);
    procedure SetGeometry(const Value: TReverbGeometry);
    procedure SetHalfLife(const Value: Double);
    procedure SetHold(const Value: Boolean);
    procedure SetInputAngle(const Value: Double);
    procedure SetModulationActive(const Value: Boolean);
    procedure SetModulationDepth(const Value: Double);
    procedure SetModulationSpread(const Value: Double);
    procedure SetNonLinearActive(const Value: Boolean);
    procedure SetNonLinearGain(const Value: Double);
    procedure SetOutputAngle(const Value: Double);
    procedure SetSampleRate(const Value: Single);
    procedure SetWetMix(const Value: Double);
    procedure SetBaseDelay(const Value: Double);
  protected
    procedure BaseDelayChanged; virtual; abstract;
    procedure DampingChanged; virtual; abstract;
    procedure DryMixChanged; virtual; abstract;
    procedure FeedbackInversionChanged; virtual; abstract;
    procedure GeometryChanged; virtual; abstract;
    procedure HalfLifeChanged; virtual; abstract;
    procedure HoldChanged; virtual; abstract;
    procedure InputAngleChanged; virtual; abstract;
    procedure ModulationActiveChanged; virtual; abstract;
    procedure ModulationDepthChanged; virtual; abstract;
    procedure ModulationSpreadChanged; virtual; abstract;
    procedure NonLinearActiveChanged; virtual; abstract;
    procedure NonLinearGainChanged; virtual; abstract;
    procedure OutputAngleChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
    procedure WetMixChanged; virtual; abstract;
  public
    constructor Create; virtual;
    property Geometry: TReverbGeometry read FGeometry write SetGeometry;
    property BaseDelay: Double read FBaseDelay write SetBaseDelay;
    property HalfLife: Double read FHalfLife write SetHalfLife;
    property Damping: Double read FDamping write SetDamping;

    property ModulationActive: Boolean read FModulationActive write SetModulationActive;
    property ModulationDepth: Double read FModulationDepth write SetModulationDepth;
    property ModulationSpread: Double read FModulationSpread write SetModulationSpread;
    property NonLinearActive: Boolean read FNonLinearActive write SetNonLinearActive;
    property NonLinearGain: Double read FNonLinearGain write SetNonLinearGain;
    property InputAngle: Double read FInputAngle write SetInputAngle;
    property OutputAngle: Double read FOutputAngle write SetOutputAngle;
    property FeedbackRotation[Index : Integer]: Double read GetFeedbackRotation write SetFeedbackRotation;
    property FeedbackInversion: Double read FFeedbackInversion write SetFeedbackInversion;
    property DryMix: Double read FDryMix write SetDryMix;
    property WetMix: Double read FWetMix write SetWetMix;
    property Hold: Boolean read FHold write SetHold;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TDspFDNReverb32 = class(TCustomDspFDNReverb)
  private
    FFeedbackDelayNetwork : TFeedbackZDelayNetwork32;
    FHalflifeVector       : TDAVVector32;
    FDampingFilter        : Array [0..3] of TDampingFilter;
    FVibrato              : Array [0..3] of TDspVibrato32;
    procedure ReverbTimesChanged;
    procedure ProcessFeedbackPath(var FeedbackVector: TDAVVector32);
  protected
    procedure DampingChanged; override;
    procedure HalfLifeChanged; override;
    procedure InputAngleChanged; override;
    procedure ModulationActiveChanged; override;
    procedure NonLinearActiveChanged; override;
    procedure NonLinearGainChanged; override;
    procedure OutputAngleChanged; override;
    procedure SampleRateChanged; override;
    procedure GeometryChanged; override;
    procedure BaseDelayChanged; override;
    procedure FeedbackInversionChanged; override;
    procedure DryMixChanged; override;
    procedure WetMixChanged; override;
    procedure HoldChanged; override;
    procedure ModulationDepthChanged; override;
    procedure ModulationSpreadChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single;
    procedure ProcessStereo(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
  end;

implementation

uses
  Math, SysUtils, DAV_Approximations, DAV_DspInterpolation;

{ TDampingFilter }

constructor TDampingFilter.Create;
begin
 inherited;
 CalculateCoefficients;
end;

procedure TDampingFilter.Reset;
begin
 FState := 0;
end;

procedure TDampingFilter.ResetStates;
begin
 FState := 0;
end;

procedure TDampingFilter.ResetStatesInt64;
begin
 PInt64(@FState)^ := 0;
end;

procedure TDampingFilter.CalculateW0;
begin
 fW0 := 2 * Pi * fSRR * (fFrequency);
 fSinW0 := sin(fW0);
 if fW0 > 3.1 then fW0 := 3.1;
end;

procedure TDampingFilter.SetFilterValues(const AFrequency, AGain : Single);
const
  ln10_0025 : Double = 5.7564627325E-2;
begin
 fFrequency := AFrequency;
 FGain_dB := AGain;
 FGainFactor := Exp(FGain_dB * ln10_0025);
 CalculateW0;
end;

function TDampingFilter.Real(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, result, Temp);
end;

function TDampingFilter.Imaginary(const Frequency: Double): Double;
var
  Temp: Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TDampingFilter.Complex(const Frequency: Double; out Real, Imaginary: Double);
(*
var
  cw, Divider  : Double;
  cmplx        : TComplexDouble;
  i            : Integer;
*)
begin
(*
 if fOrder = 0 then
  begin
   Real := 1;
   Imaginary := 1;
  end
 else
  begin
   cw := cos(2 * Frequency * pi * fSRR);
   Divider   := 1 / ( sqr(FCoeffs[3]) - 2 * FCoeffs[3] + sqr(FCoeffs[2]) + 1
                      + 2 * cw * (FCoeffs[2] * (FCoeffs[3] + 1) + 2 * cw * FCoeffs[3]));
   Real      := (FCoeffs[0] + FCoeffs[1] * FCoeffs[2] + FCoeffs[0] * FCoeffs[3]
                + cw * (FCoeffs[1] * (1 + FCoeffs[3]) + FCoeffs[2] * 2 * FCoeffs[0])
                + (2 * sqr(cw) - 1) * FCoeffs[0] * (FCoeffs[3] + 1)) * Divider;
   Imaginary := (FCoeffs[1] * (1 - FCoeffs[3])
                + 2 * cw * FCoeffs[0] * (1 - FCoeffs[3])) * sqrt(1 - sqr(cw)) * Divider;
   for i := 1 to (fOrder div 2) - 1 do
    begin
     Divider   := 1 / ( sqr(FCoeffs[4*i+3]) - 2 * FCoeffs[4*i+3] + sqr(FCoeffs[4*i+2]) + 1
                + 2 * cw * (FCoeffs[4*i+2] * (FCoeffs[4*i+3] + 1) + 2 * cw * FCoeffs[4*i+3]));
     cmplx.Re  := (FCoeffs[4*i+0] + FCoeffs[4*i+1] * FCoeffs[4*i+2] + FCoeffs[4*i+0] * FCoeffs[4*i+3]
                 + cw * (FCoeffs[4*i+1] * (1 + FCoeffs[4*i+3]) + FCoeffs[4*i+2] * 2 * FCoeffs[4*i+0])
                 + (2*sqr(cw)-1) * FCoeffs[4*i+0] * (FCoeffs[4*i+3] + 1)) * Divider;
     cmplx.Im := (FCoeffs[4*i+1] * (1 - FCoeffs[4*i+3])
                 + 2 * cw * (FCoeffs[4*i+0] - FCoeffs[4*i+0] * FCoeffs[4*i+3])) * sqrt(1 - sqr(cw)) * Divider;
     ComplexMultiplyInplace(Real, Imaginary, cmplx.Re, cmplx.Im);
    end;
  end;
*)
end;

procedure TDampingFilter.Complex(const Frequency: Double; out Real, Imaginary: Single);
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Real := cmplx.Re;
 Imaginary := cmplx.Im;
end;

function TDampingFilter.MagnitudeLog10(const Frequency: Double): Double;
begin
 result := 20 * Log10(MagnitudeSquared(Frequency));
end;

function TDampingFilter.Phase(const Frequency: Double): Double;
var
  cmplx : TComplexDouble;
begin
 Complex(Frequency, cmplx.Re, cmplx.Im);
 Result := ArcTan2(cmplx.Im, cmplx.Re);
end;

procedure TDampingFilter.CalculateCoefficients;
var
  K, t : Double;
begin
 K := tan(fW0 * 0.5);
 t := 1 / (K + 1);
 FCoeffs[0] := K * t;
 FCoeffs[1] := (1 - K) * t;
end;

function TDampingFilter.MagnitudeSquared(const Frequency: Double): Double;
var
  cw : Double;
begin
 cw := 2 * cos(2 * Frequency * pi * fSRR);
 Result := Abs(1E-32 + sqr(FCoeffs[0]) * (cw + 2) / (1 + sqr(FCoeffs[1]) - cw * FCoeffs[1]));
end;

function TDampingFilter.ProcessSample(const Input: Double): Double;
{$IFDEF PUREPASCAL}
var
  x : Double;
  i : Integer;
begin
 x      := FCoeffs[0] * Input;
 Result := x + FState;
 FState := x + FCoeffs[1] * Result;
{$ELSE}
asm
 fld Input.Double;
 fmul [self.FCoeffs].Double     // x
 fld  st(0)                     // x, x
 fadd [self.FState].Double      // x + FState, x
 fld  st(0)                     // x + FState, x + FState, x
 fmul [self.FCoeffs + 8].Double
 faddp st(2), st(0)
 fxch
 fstp [self.FState].Double
{$ENDIF}
end;

procedure TDampingFilter.PopStates;
begin
 raise Exception.Create('Not supported');
end;

procedure TDampingFilter.PushStates;
begin
 raise Exception.Create('Not supported');
end;

{ TCustomDspFDNReverb }

constructor TCustomDspFDNReverb.Create;
begin
 FHalfLife         := 1;
 FDamping          := 0.25;
 FNonLinearActive  := False;
 FModulationActive := False;
 FNonLinearGain    := 1;
 SampleRate        := 44100;
end;

function TCustomDspFDNReverb.GetFeedbackRotation(Index: Integer): Double;
begin
 if (Index < 0) or (Index > 1)
  then raise Exception.CreateFmt('Index out of bounds (%d)', [Index])
  else result := FFeedbackRotation[Index];
end;

procedure TCustomDspFDNReverb.SetBaseDelay(const Value: Double);
begin
 if BaseDelay <> Value then
  begin
   FBaseDelay := Value;
   BaseDelayChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetDamping(const Value: Double);
begin
 if Damping <> Value then
  begin
   FDamping := Value;
   DampingChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetDryMix(const Value: Double);
begin
 if DryMix <> Value then
  begin
   FDryMix := Value;
   DryMixChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetFeedbackInversion(const Value: Double);
begin
 if FeedbackInversion <> Value then
  begin
   FFeedbackInversion := Value;
   FeedbackInversionChanged;
  end
end;

procedure TCustomDspFDNReverb.SetFeedbackRotation(Index: Integer;
  const Value: Double);
begin
 if (Index < 0) or (Index > 1)
  then raise Exception.CreateFmt('Index out of bounds (%d)', [Index])
  else FFeedbackRotation[Index] := Value;
end;

procedure TCustomDspFDNReverb.SetGeometry(const Value: TReverbGeometry);
begin
 if Geometry <> Value then
  begin
   FGeometry := Value;
   GeometryChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetHalfLife(const Value: Double);
begin
 if HalfLife <> Value then
  begin
   FHalfLife := Value;
   HalfLifeChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetHold(const Value: Boolean);
begin
 if Hold <> Value then
  begin
   FHold := Value;
   HoldChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetInputAngle(const Value: Double);
begin
 if InputAngle <> Value then
  begin
   FInputAngle := Value;
   InputAngleChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationActive(
  const Value: Boolean);
begin
 if FModulationActive <> Value then
  begin
   FModulationActive := Value;
   ModulationActiveChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationDepth(const Value: Double);
begin
 if ModulationDepth <> Value then
  begin
   FModulationDepth := Value;
   ModulationDepthChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetModulationSpread(const Value: Double);
begin
 if ModulationSpread <> Value then
  begin
   FModulationSpread := Value;
   ModulationSpreadChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetNonLinearActive(const Value: Boolean);
begin
 if FNonLinearActive <> Value then
  begin
   FNonLinearActive := Value;
   NonLinearActiveChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetNonLinearGain(const Value: Double);
begin
 if FNonLinearGain <> Value then
  begin
   FNonLinearGain := Value;
   NonLinearGainChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetOutputAngle(const Value: Double);
begin
 if FOutputAngle <> Value then
  begin
   FOutputAngle := Value;
   OutputAngleChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDspFDNReverb.SetWetMix(const Value: Double);
begin
 if WetMix <> Value then
  begin
   FWetMix := Value;
   WetMixChanged;
  end;
end;

{ TDspFDNReverb32 }

constructor TDspFDNReverb32.Create;
var
  n : Integer;
begin
 inherited;
 FFeedbackDelayNetwork := TFeedbackZDelayNetwork32.Create;
 FFeedbackDelayNetwork.OnProcessFeedbackPath := ProcessFeedbackPath;
 FDamping := 0.25;
 for n := 0 to 3 do
  begin
   FDampingFilter[n] := TDampingFilter.Create;
   FVibrato[n]       := TDspVibrato32.Create;
//   FVibrato[n].Depth :=
  end;
 DampingChanged;
end;

destructor TDspFDNReverb32.Destroy;
var
  n : Integer;
begin
 for n := 0 to 3 do
  begin
   FreeAndNil(FDampingFilter[n]);
   FreeAndNil(FVibrato[n]);
  end;
 FreeAndNil(FFeedbackDelayNetwork);
 inherited;
end;

procedure TDspFDNReverb32.DryMixChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.FeedbackInversionChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.GeometryChanged;
begin
 ReverbTimesChanged;
end;

procedure TDspFDNReverb32.DampingChanged;
var
  n : Integer;
begin
 for n := 0 to 3
  do FDampingFilter[n].Frequency := (0.01 + 0.99 * FDamping) * 5000 * (4 - n);
end;

procedure TDspFDNReverb32.BaseDelayChanged;
begin
 ReverbTimesChanged;
 HalfLifeChanged;
end;

procedure TDspFDNReverb32.HalfLifeChanged;
var
  HL : Double;
begin
 HL := exp(-ln2 / (FHalfLife * (FBaseDelay * SampleRate)));
 FHalflifeVector[0] := HL;
 FHalflifeVector[1] := HL;
 FHalflifeVector[2] := HL;
 FHalflifeVector[3] := HL;
end;

procedure TDspFDNReverb32.HoldChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.InputAngleChanged;
var
  Cmplx : TComplexSingle;
begin
 GetSinCos(FInputAngle * Pi / 180, Cmplx.Im, Cmplx.Re);
 FFeedbackDelayNetwork.InputVector[0] := Cmplx.Re;
 FFeedbackDelayNetwork.InputVector[1] := Cmplx.Im;
 FFeedbackDelayNetwork.InputVector[2] := -Cmplx.Re;
 FFeedbackDelayNetwork.InputVector[3] := -Cmplx.Im;
end;

procedure TDspFDNReverb32.ModulationActiveChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.ModulationDepthChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.ModulationSpreadChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.NonLinearActiveChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.NonLinearGainChanged;
begin
 // nothing here yet
end;

procedure TDspFDNReverb32.OutputAngleChanged;
var
  Cmplx : TComplexSingle;
begin
 GetSinCos(FOutputAngle * Pi / 180, Cmplx.Im, Cmplx.Re);
 FFeedbackDelayNetwork.OutputVector[0] := Cmplx.Re;
 FFeedbackDelayNetwork.OutputVector[1] := Cmplx.Im;
 FFeedbackDelayNetwork.OutputVector[2] := -Cmplx.Re;
 FFeedbackDelayNetwork.OutputVector[3] := -Cmplx.Im;
end;

procedure TDspFDNReverb32.ProcessFeedbackPath(var FeedbackVector: TDAVVector32);
begin
 if FNonLinearActive then
  begin
   FeedbackVector[0] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[0]);
   FeedbackVector[1] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[1]);
   FeedbackVector[2] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[2]);
   FeedbackVector[3] := FastTanhOpt5TermFPU(FNonLinearGain * FeedbackVector[3]);
  end;

 // Halflife
 ScaleVector(FeedbackVector, FHalflifeVector);

 FeedbackVector[0] := FDampingFilter[0].ProcessSample(FeedbackVector[0]);
 FeedbackVector[1] := FDampingFilter[1].ProcessSample(FeedbackVector[1]);
 FeedbackVector[2] := FDampingFilter[2].ProcessSample(FeedbackVector[2]);
 FeedbackVector[3] := FDampingFilter[3].ProcessSample(FeedbackVector[3]);

 if FModulationActive then
  begin
   FeedbackVector[0] := FVibrato[0].Process(FeedbackVector[0]);
   FeedbackVector[1] := FVibrato[0].Process(FeedbackVector[1]);
   FeedbackVector[2] := FVibrato[0].Process(FeedbackVector[2]);
   FeedbackVector[3] := FVibrato[0].Process(FeedbackVector[3]);
  end;
end;

function TDspFDNReverb32.ProcessSample(const Input: Single): Single;
begin
 result := FDryMix * Input +
           FWetMix * FFeedbackDelayNetwork.ProcessSample(Input);
end;

procedure TDspFDNReverb32.ProcessStereo(const InLeft, InRight: Single;
  out OutLeft, OutRight: Single);
begin
 FFeedbackDelayNetwork.ProcessStereo(InLeft,  InRight, OutLeft, OutRight);
 OutLeft  := InLeft  * FDryMix + OutLeft  * FWetMix;
 OutRight := InRight * FDryMix + OutRight * FWetMix;
end;

procedure TDspFDNReverb32.ReverbTimesChanged;
const
  CGeometryTimeScales: Array [0..16, 0..2] of Single = (
    (1, 1, 1), (1.04, 1.11, 1.16), (1.05, 1.14, 1.3), (1.22, 1.34, 1.72),
    (1.78, 2.44, 2.62), (1.15, 1.63, 2.41), (1.41, 2.22, 3.13),
    (1.67, 2.52, 3.11), (1.44, 2.59, 3.35), (2.24, 3.78, 4.67),
    (2.61, 3.78, 5.1), (4/3, 5/6, 2), (4/3, 8/5, 2), (4/3, 2, 4),
    (2, 4, 8), (3/2, 2, 3), (1.0001, 1.0030, 1.0031));
begin
 if round(FBaseDelay * SampleRate) <= 0 then exit;
 FFeedbackDelayNetwork.DelaySamples[0] := round(FBaseDelay * SampleRate);
 case FGeometry of
  rgSphere : with FFeedbackDelayNetwork do
              begin
               DelaySamples[1] := DelaySamples[0];
               DelaySamples[2] := DelaySamples[0];
               DelaySamples[3] := DelaySamples[0];
              end;
   else with FFeedbackDelayNetwork do
         begin
          DelaySamples[1] := round(CGeometryTimeScales[Integer(FGeometry), 0] * FBaseDelay * SampleRate);
          DelaySamples[2] := round(CGeometryTimeScales[Integer(FGeometry), 1] * FBaseDelay * SampleRate);
          DelaySamples[3] := round(CGeometryTimeScales[Integer(FGeometry), 2] * FBaseDelay * SampleRate);
         end;
 end;
end;

procedure TDspFDNReverb32.SampleRateChanged;
var
  n : Integer;
begin
 for n := 0 to 3 do
  begin
   if assigned(FDampingFilter[n])
    then FDampingFilter[n].SampleRate := SampleRate;
   if assigned(FVibrato[n])
    then FVibrato[n].SampleRate := SampleRate;
  end;
end;

procedure TDspFDNReverb32.WetMixChanged;
begin
 // nothing here yet
end;

end.
