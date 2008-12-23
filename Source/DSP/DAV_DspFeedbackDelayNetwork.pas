unit DAV_DspFeedbackDelayNetwork;

interface

{$I ASIOVST.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_VectorMath, DAV_DspFilter,
  DAV_DspChorus;

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

  TCustomDelayLine = class(TDspObject)
  private
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create; virtual;
    procedure Reset; virtual;
  end;

  TDelayLineSamples32 = class(TCustomDelayLine)
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
  published
    property BufferSize;
  end;

  TDelayLineSamples64 = class(TCustomDelayLine)
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Double): Double;
  published
    property BufferSize;
  end;

  TCustomDelayLineTime = class(TCustomDelayLine)
  private
    procedure SetSampleRate(const Value: Single);
    procedure SetTime(const Value: Single);
  protected
    FSampleRate : Single;
    FTime       : Single;
    procedure SampleRateChanged; virtual; abstract;
    procedure TimeChanged; virtual; abstract;
  public
    constructor Create; override;
    property Samplerate: Single read FSampleRate write SetSampleRate;
    property Time: Single read FTime write SetTime;
  end;

  TDelayLineTime32 = class(TCustomDelayLineTime)
  private
    procedure CalculateBufferSize;
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FFractional     : Single;
    FIntBuffer      : TDAV4SingleArray;
    procedure BufferSizeChanged; override;
    procedure SampleRateChanged; override;
    procedure TimeChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
  published
    property Samplerate;
    property Time;
  end;

  TCustomFeedbackDelayNetwork = class(TDspObject)
  private
    FHalfLife        : Double;
    FDamping         : Double;
    FNonLinearActive : Boolean;
    FNonLinearGain   : Double;
    FSampleRate      : Single;
    procedure SetHalfLife(const Value: Double);
    procedure SetDamping(const Value: Double);
    procedure SetNonLinearActive(const Value: Boolean);
    procedure SetNonLinearGain(const Value: Double);
    procedure SetSampleRate(const Value: Single);
  protected
    procedure HalfLifeChanged; virtual; abstract;
    procedure DampingChanged; virtual; abstract;
    procedure NonLinearActiveChanged; virtual; abstract;
    procedure NonLinearGainChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
    function GetDelaySamples(Index: Integer): Integer; virtual; abstract;
    function GetDelayTimes(Index: Integer): Single; virtual; abstract;
    function GetFeedbackMatrix(InputIndex, OutputIndex: Integer): Double; virtual; abstract;
    function GetInputVector(Index: Integer): Double; virtual; abstract;
    function GetOutputVector(Index: Integer): Double; virtual; abstract;
    procedure SetDelaySamples(Index: Integer; const Value: Integer); virtual; abstract;
    procedure SetDelayTimes(Index: Integer; const Value: Single); virtual; abstract;
    procedure SetFeedbackMatrix(InputIndex, OutputIndex: Integer; const Value: Double); virtual; abstract;
    procedure SetInputVector(Index: Integer; const Value: Double); virtual; abstract;
    procedure SetOutputVector(Index: Integer; const Value: Double); virtual; abstract;
  public
    constructor Create; virtual;
    property InputVector[Index: Integer]: Double read GetInputVector write SetInputVector;
    property OutputVector[Index: Integer]: Double read GetOutputVector write SetOutputVector;
    property DelaySamples[Index: Integer]: Integer read GetDelaySamples write SetDelaySamples;
    property DelayTimes[Index: Integer]: Single read GetDelayTimes write SetDelayTimes;
    property FeedbackMatrix[InputIndex, OutputIndex: Integer]: Double read GetFeedbackMatrix write SetFeedbackMatrix;
    property HalfLife: Double read FHalfLife write SetHalfLife;
    property Damping: Double read FDamping write SetDamping;
    property NonLinearActive: Boolean read FNonLinearActive write SetNonLinearActive;
    property NonLinearGain: Double read FNonLinearGain write SetNonLinearGain;
    property SampleRate: Single read FSampleRate write SetSampleRate; 
  end;

  TFeedbackDelayNetwork32 = class(TCustomFeedbackDelayNetwork)
  private
    FInputVector    : TDAVVector32;
    FOutputVector   : TDAVVector32;
    FHalflifeVector : TDAVVector32;
    FDelaySamples   : Array [0..3] of Integer;
    FDelayPos       : Array [0..3] of Integer;
    FDelayBuffers   : Array [0..3] of PDAVSingleFixedArray;
    FDelayFracs     : Array [0..3] of Single;
    FFeedbackMatrix : TDAVMatrix32;
    FRotationMatrix : TDAVMatrix32;
    FDampingFilter  : Array [0..3] of TDampingFilter;
    procedure DelaySamplesChanged(Index: Integer);
  protected
    procedure HalfLifeChanged; override;
    procedure DampingChanged; override;
    procedure NonLinearActiveChanged; override;
    procedure NonLinearGainChanged; override;
    procedure SampleRateChanged; override;
    function GetDelaySamples(Index: Integer): Integer; override;
    function GetDelayTimes(Index: Integer): Single; override;
    function GetFeedbackMatrix(InputIndex, OutputIndex: Integer): Double; override;
    function GetInputVector(Index: Integer): Double; override;
    function GetOutputVector(Index: Integer): Double; override;
    procedure SetDelaySamples(Index: Integer; const Value: Integer); override;
    procedure SetDelayTimes(Index: Integer; const Value: Single); override;
    procedure SetFeedbackMatrix(InputIndex, OutputIndex: Integer; const Value: Double); override;
    procedure SetInputVector(Index: Integer; const Value: Double); override;
    procedure SetOutputVector(Index: Integer; const Value: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single;
    procedure ProcessStereo(const InLeft, InRight: Single; out OutLeft, OutRight: Single);
  end;

implementation

uses
  Math, SysUtils, DAV_DspInterpolation;

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


{ TCustomDelayLine }

constructor TCustomDelayLine.Create;
begin
 inherited;
 FBufferSize := 0;
 FBufferPos  := 0;
end;

procedure TCustomDelayLine.Reset;
begin
 FBufferPos := 0;
end;

procedure TCustomDelayLine.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TDelayLineSamples32 }

constructor TDelayLineSamples32.Create;
begin
 inherited;
 FBuffer := nil;
end;

destructor TDelayLineSamples32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TDelayLineSamples32.ProcessSample(const Input: Single): Single;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TDelayLineSamples32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
end;

procedure TDelayLineSamples32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TDelayLineSamples64 }

constructor TDelayLineSamples64.Create;
begin
 inherited;
 FBuffer := nil;
end;

destructor TDelayLineSamples64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineSamples64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

function TDelayLineSamples64.ProcessSample(const Input: Double): Double;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TDelayLineSamples64.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TCustomDelayLineTime }

constructor TCustomDelayLineTime.Create;
begin
 inherited;
 FTime := 1;
 FSampleRate := 44100;
end;

procedure TCustomDelayLineTime.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDelayLineTime.SetTime(const Value: Single);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;


{ TDelayLineTime32 }

constructor TDelayLineTime32.Create;
begin
 inherited;
 FBuffer := nil;
 FIntBuffer[3] := 0;
end;

destructor TDelayLineTime32.Destroy;
begin
 assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineTime32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

function TDelayLineTime32.ProcessSample(const Input: Single): Single;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[1], FIntBuffer[0], 2 * SizeOf(Single));
 FIntBuffer[2] := FBuffer^[FBufferPos];
 result := Hermite32_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineTime32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

procedure TDelayLineTime32.CalculateBufferSize;
begin
 BufferSize      := round(FTime * FSampleRate + 0.5) + 1;
 FFractional     := BufferSize - 1 - (FTime * FSampleRate);
 FBuffer[BufferSize - 1] := 0;
 assert(FFractional >= 0);
 assert(FFractional <= 1);
end;

procedure TDelayLineTime32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TDelayLineTime32.TimeChanged;
begin
 inherited;
 CalculateBufferSize;
end;


{ TCustomFeedbackDelayNetwork }

constructor TCustomFeedbackDelayNetwork.Create;
begin
 inherited;
 FHalfLife        := 1;
 FDamping         := 0.25;
 FNonLinearActive := False;
 FNonLinearGain   := 1;
end;

procedure TCustomFeedbackDelayNetwork.SetDamping(const Value: Double);
begin
 if FDamping <> Value then
  begin
   FDamping := Value;
   DampingChanged;
  end;
end;

procedure TCustomFeedbackDelayNetwork.SetHalfLife(const Value: Double);
begin
 if FHalfLife <> Value then
  begin
   FHalfLife := Value;
   HalfLifeChanged;
  end;
end;

procedure TCustomFeedbackDelayNetwork.SetNonLinearActive(const Value: Boolean);
begin
 if FNonLinearActive <> Value then
  begin
   FNonLinearActive := Value;
   NonLinearActiveChanged;
  end;
end;

procedure TCustomFeedbackDelayNetwork.SetNonLinearGain(const Value: Double);
begin
 if FNonLinearGain <> Value then
  begin
   FNonLinearGain := Value;
   NonLinearGainChanged;
  end;
end;

procedure TCustomFeedbackDelayNetwork.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TFeedbackDelayNetwork32 }

constructor TFeedbackDelayNetwork32.Create;
var
  n : Integer;
begin
 inherited;
 FDelayBuffers[0] := nil;
 FDelayBuffers[1] := nil;
 FDelayBuffers[2] := nil;
 FDelayBuffers[3] := nil;

 FInputVector  := CHomogeneousXVector32;
 FOutputVector := CHomogeneousXVector32;

 FDelayPos[0] := 0;
 FDelayPos[1] := 0;
 FDelayPos[2] := 0;
 FDelayPos[3] := 0;

 DelaySamples[0] := 1;
 DelaySamples[1] := 1;
 DelaySamples[2] := 1;
 DelaySamples[3] := 1;

 FRotationMatrix := CIdentityHomogeneousMatrix32;
 FFeedbackMatrix := CIdentityHomogeneousMatrix32;

 FDamping := 0.25; 
 for n := 0 to 3 do
  begin
   FDampingFilter[n] := TDampingFilter.Create;
   FDampingFilter[n].SampleRate := 44100;
  end;
 DampingChanged;
end;

destructor TFeedbackDelayNetwork32.Destroy;
begin
 Dispose(FDelayBuffers[0]);
 Dispose(FDelayBuffers[1]);
 Dispose(FDelayBuffers[2]);
 Dispose(FDelayBuffers[3]);
 inherited;
end;

function TFeedbackDelayNetwork32.GetDelaySamples(Index: Integer): Integer;
begin
 case Index of
  0..3 : result := FDelaySamples[Index];
  else result := 0;
 end;
end;

function TFeedbackDelayNetwork32.GetDelayTimes(Index: Integer): Single;
begin
 case Index of
  0..3 : result := (FDelaySamples[Index] + FDelayFracs[Index]) * FSampleRate;
  else result := 0;
 end;
end;

function TFeedbackDelayNetwork32.GetFeedbackMatrix(InputIndex,
  OutputIndex: Integer): Double;
begin
 case InputIndex of
  0..3 :
   case OutputIndex of
    0..3 : result := FFeedbackMatrix[InputIndex, OutputIndex];
    else result := 0;
   end;
  else result := 0;
 end;
end;

function TFeedbackDelayNetwork32.GetInputVector(Index: Integer): Double;
begin
 case Index of
  0..3 : result := FInputVector[Index];
  else result := 0;
 end;
end;

function TFeedbackDelayNetwork32.GetOutputVector(Index: Integer): Double;
begin
 case Index of
  0..3 : result := FOutputVector[Index];
  else result := 0;
 end;
end;

procedure TFeedbackDelayNetwork32.HalfLifeChanged;
begin
 ScaleVector(FHalflifeVector, FHalfLife);
end;

procedure TFeedbackDelayNetwork32.NonLinearActiveChanged;
begin
 // nothing here yet
end;

procedure TFeedbackDelayNetwork32.NonLinearGainChanged;
begin
 // nothing here yet
end;

procedure TFeedbackDelayNetwork32.SampleRateChanged;
begin
 if assigned(FDampingFilter[0]) then FDampingFilter[0].SampleRate := SampleRate;
 if assigned(FDampingFilter[1]) then FDampingFilter[1].SampleRate := SampleRate;
 if assigned(FDampingFilter[2]) then FDampingFilter[2].SampleRate := SampleRate;
 if assigned(FDampingFilter[3]) then FDampingFilter[3].SampleRate := SampleRate;
end;

procedure TFeedbackDelayNetwork32.SetDelaySamples(Index: Integer;
  const Value: Integer);
begin
 case Index of
  0..3 : if FDelaySamples[Index] <> Value then
          begin
           FDelaySamples[Index] := Value;
           FDelayFracs[Index] := 0;
           DelaySamplesChanged(Index);
          end;
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
 end;
end;

procedure TFeedbackDelayNetwork32.SetDelayTimes(Index: Integer;
  const Value: Single);
begin
 case Index of
  0..3 : begin
          DelaySamples[Index] := round(Value * SampleRate + 0.5);
          FDelayFracs[Index] := DelaySamples[Index] - Value * SampleRate;
          assert(FDelayFracs[Index] >= 0);
          assert(FDelayFracs[Index] <= 1);
         end;
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
 end;
end;

procedure TFeedbackDelayNetwork32.SetFeedbackMatrix(InputIndex,
  OutputIndex: Integer; const Value: Double);
begin
 case InputIndex of
  0..3 :
   case OutputIndex of
    0..3 : FFeedbackMatrix[InputIndex, OutputIndex] := Value;
    else raise Exception.CreateFmt('Output Index out of bounds (%d)', [OutputIndex]);
   end;
  else raise Exception.CreateFmt('Input Index out of bounds (%d)', [InputIndex]);
 end;
end;

procedure TFeedbackDelayNetwork32.DampingChanged;
var
  n : Integer;
begin
 for n := 0 to 3
  do FDampingFilter[n].Frequency := (0.01 + 0.99 * FDamping) * 5000 * (4 - n);
end;

procedure TFeedbackDelayNetwork32.DelaySamplesChanged(Index: Integer);
begin
 assert(Index in [0..3]);
 ReallocMem(FDelayBuffers[Index], FDelaySamples[Index] * SizeOf(Single));
 if FDelayPos[Index] >= FDelaySamples[Index]
  then FDelayPos[Index] := 0;
end;

procedure TFeedbackDelayNetwork32.SetInputVector(Index: Integer;
  const Value: Double);
begin
 case Index of
  0..3 : FInputVector[Index] := Value;
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
 end;
end;

procedure TFeedbackDelayNetwork32.SetOutputVector(Index: Integer;
  const Value: Double);
begin
 case Index of
  0..3 : FOutputVector[Index] := Value;
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
 end;
end;

function TFeedbackDelayNetwork32.ProcessSample(const Input: Single): Single;
var
  DelayedSignal : TDAV4SingleArray;
  FeedbackInput : TDAV4SingleArray;
begin
 // Build Delay Vector
 DelayedSignal[0] := FDelayBuffers[0]^[FDelayPos[0]];
 DelayedSignal[1] := FDelayBuffers[1]^[FDelayPos[1]];
 DelayedSignal[2] := FDelayBuffers[0]^[FDelayPos[0]];
 DelayedSignal[3] := FDelayBuffers[1]^[FDelayPos[1]];

 // Output
 result := VectorDotProduct(FOutputVector, DelayedSignal);

 // Feedback Matrix
 FeedbackInput := VectorTransform(DelayedSignal, FFeedbackMatrix);

 // Halflife
 ScaleVector(FeedbackInput, FHalflifeVector);

 if FNonLinearActive then
  begin
   FeedbackInput[0] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[0].ProcessSample(FeedbackInput[0]));
   FeedbackInput[1] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[1].ProcessSample(FeedbackInput[1]));
   FeedbackInput[2] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[2].ProcessSample(FeedbackInput[2]));
   FeedbackInput[3] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[3].ProcessSample(FeedbackInput[3]));
  end
 else
  begin
   FeedbackInput[0] := FDampingFilter[0].ProcessSample(FeedbackInput[0]);
   FeedbackInput[1] := FDampingFilter[1].ProcessSample(FeedbackInput[1]);
   FeedbackInput[2] := FDampingFilter[2].ProcessSample(FeedbackInput[2]);
   FeedbackInput[3] := FDampingFilter[3].ProcessSample(FeedbackInput[3]);
  end;

 FDelayBuffers[0]^[FDelayPos[0]] := FInputVector[0] * Input + FeedbackInput[0];
 FDelayBuffers[1]^[FDelayPos[1]] := FInputVector[1] * Input + FeedbackInput[1];
 FDelayBuffers[2]^[FDelayPos[2]] := FInputVector[2] * Input + FeedbackInput[2];
 FDelayBuffers[3]^[FDelayPos[3]] := FInputVector[3] * Input + FeedbackInput[3];

 inc(FDelayPos[0]);
 inc(FDelayPos[1]);
 inc(FDelayPos[2]);
 inc(FDelayPos[3]);

 if FDelayPos[0] >= FDelaySamples[0] then FDelayPos[0] := 0;
 if FDelayPos[1] >= FDelaySamples[1] then FDelayPos[1] := 0;
 if FDelayPos[2] >= FDelaySamples[2] then FDelayPos[2] := 0;
 if FDelayPos[3] >= FDelaySamples[3] then FDelayPos[3] := 0;
end;

procedure TFeedbackDelayNetwork32.ProcessStereo(const InLeft, InRight: Single;
  out OutLeft, OutRight: Single);
var
  FeedbackInput : TDAV4SingleArray;
begin
 OutLeft  := FOutputVector[0] * FDelayBuffers[0]^[FDelayPos[0]] +
             FOutputVector[1] * FDelayBuffers[1]^[FDelayPos[1]] +
             FOutputVector[2] * FDelayBuffers[2]^[FDelayPos[2]] +
             FOutputVector[3] * FDelayBuffers[3]^[FDelayPos[3]];

 OutRight := FOutputVector[1] * FDelayBuffers[0]^[FDelayPos[0]] +
             FOutputVector[2] * FDelayBuffers[1]^[FDelayPos[1]] +
             FOutputVector[3] * FDelayBuffers[2]^[FDelayPos[2]] +
             FOutputVector[0] * FDelayBuffers[3]^[FDelayPos[3]];

 FeedbackInput[0] := FFeedbackMatrix[0, 0] * FDelayBuffers[0]^[FDelayPos[0]] +
                     FFeedbackMatrix[1, 0] * FDelayBuffers[1]^[FDelayPos[1]] +
                     FFeedbackMatrix[2, 0] * FDelayBuffers[2]^[FDelayPos[2]] +
                     FFeedbackMatrix[3, 0] * FDelayBuffers[3]^[FDelayPos[3]];
 FeedbackInput[1] := FFeedbackMatrix[0, 1] * FDelayBuffers[0]^[FDelayPos[0]] +
                     FFeedbackMatrix[1, 1] * FDelayBuffers[1]^[FDelayPos[1]] +
                     FFeedbackMatrix[2, 1] * FDelayBuffers[2]^[FDelayPos[2]] +
                     FFeedbackMatrix[3, 1] * FDelayBuffers[3]^[FDelayPos[3]];
 FeedbackInput[2] := FFeedbackMatrix[0, 2] * FDelayBuffers[0]^[FDelayPos[0]] +
                     FFeedbackMatrix[1, 2] * FDelayBuffers[1]^[FDelayPos[1]] +
                     FFeedbackMatrix[2, 2] * FDelayBuffers[2]^[FDelayPos[2]] +
                     FFeedbackMatrix[3, 2] * FDelayBuffers[3]^[FDelayPos[3]];
 FeedbackInput[3] := FFeedbackMatrix[0, 3] * FDelayBuffers[0]^[FDelayPos[0]] +
                     FFeedbackMatrix[1, 3] * FDelayBuffers[1]^[FDelayPos[1]] +
                     FFeedbackMatrix[2, 3] * FDelayBuffers[2]^[FDelayPos[2]] +
                     FFeedbackMatrix[3, 3] * FDelayBuffers[3]^[FDelayPos[3]];

 // halflife
 ScaleVector(FeedbackInput, FHalflifeVector);

 if FNonLinearActive then
  begin
   FeedbackInput[0] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[0].ProcessSample(FeedbackInput[0]));
   FeedbackInput[1] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[1].ProcessSample(FeedbackInput[1]));
   FeedbackInput[2] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[2].ProcessSample(FeedbackInput[2]));
   FeedbackInput[3] := FastTanhOpt5asm(FNonLinearGain * FDampingFilter[3].ProcessSample(FeedbackInput[3]));
  end
 else
  begin
   FeedbackInput[0] := FDampingFilter[0].ProcessSample(FeedbackInput[0]);
   FeedbackInput[1] := FDampingFilter[1].ProcessSample(FeedbackInput[1]);
   FeedbackInput[2] := FDampingFilter[2].ProcessSample(FeedbackInput[2]);
   FeedbackInput[3] := FDampingFilter[3].ProcessSample(FeedbackInput[3]);
  end;

 FDelayBuffers[0]^[FDelayPos[0]] := FInputVector[0] * InLeft + FInputVector[1] * InRight + FeedbackInput[0];
 FDelayBuffers[1]^[FDelayPos[1]] := FInputVector[1] * InLeft + FInputVector[2] * InRight + FeedbackInput[1];
 FDelayBuffers[2]^[FDelayPos[2]] := FInputVector[2] * InLeft + FInputVector[3] * InRight + FeedbackInput[2];
 FDelayBuffers[3]^[FDelayPos[3]] := FInputVector[3] * InLeft + FInputVector[0] * InRight + FeedbackInput[3];

 inc(FDelayPos[0]);
 inc(FDelayPos[1]);
 inc(FDelayPos[2]);
 inc(FDelayPos[3]);

 if FDelayPos[0] >= FDelaySamples[0] then FDelayPos[0] := 0;
 if FDelayPos[1] >= FDelaySamples[1] then FDelayPos[1] := 0;
 if FDelayPos[2] >= FDelaySamples[2] then FDelayPos[2] := 0;
 if FDelayPos[3] >= FDelaySamples[3] then FDelayPos[3] := 0;
end;

end.
