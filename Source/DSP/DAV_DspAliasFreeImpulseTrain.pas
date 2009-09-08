unit DAV_DspAliasFreeImpulseTrain;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspFilter;

type
  TCustomThiranAllpass = class(TCustomFilter)
  private
    FFrequency   : Single;
    FPhaseDelay  : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetPhaseDelay(const Value: Single);
  protected  
    procedure FrequencyChanged; virtual;
    procedure PhaseDelayChanged; virtual;
    procedure CalculateCoefficients; virtual; abstract;
  public
    property Frequency: Single read FFrequency write SetFrequency;
    property PhaseDelay: Single read FPhaseDelay write SetPhaseDelay;
  end;

  TThiranAllpass1stOrder = class(TCustomThiranAllpass)
  private
    FCoefficient : Single;
    FLastOutput  : Single;
    FLastInput   : Single;
  protected  
    procedure CalculateCoefficients; override;
  public
    function ProcessSample(const Input: Double): Double; override;
  published                
    property Frequency;
    property PhaseDelay;
  end;

  TThiranAllpass2ndOrder = class(TCustomThiranAllpass)
  private
    FCoefficient : array [0..1] of Single;
    FState       : array [0..1, 0..1] of Single;
  protected
    procedure CalculateCoefficients; override;
  public
    function ProcessSample(const Input: Double): Double; override;
    procedure Reset; override;
    procedure ResetStates; override;
    procedure ResetStatesInt64; override;
    procedure PushStates; override;
    procedure PopStates; override;
    function MagnitudeLog10(const Frequency: Double): Double; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
  published
    property Frequency;
    property PhaseDelay;
  end;

  TCustomAliasFreeImpulseTrain = class(TDspObject)
  private
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    FSampleRate : Single;
    FFrequency  : Single;
    procedure SampleRateChanged; virtual; abstract;
    procedure FrequencyChanged; virtual; abstract;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create; virtual;
    procedure Reset; virtual;

    property Samplerate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TCustomAliasFreeImpulseTrain32 = class(TCustomAliasFreeImpulseTrain)
  private
    procedure SetFractional(const Value: Single);
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FFractional     : Single;
    FAllpass        : TThiranAllpass2ndOrder;
    procedure BufferSizeChanged; override;
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; override;
    procedure FractionalChanged; virtual;
    procedure CalculateBufferSize; virtual;

    property Fractional: Single read FFractional write SetFractional;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
  end;

  TAliasFreeImpulseTrain32 = class(TCustomAliasFreeImpulseTrain32)
  private
    FLastSample : Single;
  public
    function ProcessSample: Single;
    procedure Reset; override;
  published
    property Samplerate;
    property Frequency;
  end;

  TAliasFreeBipolarImpulseTrain32 = class(TCustomAliasFreeImpulseTrain32)
  private
    FLastSample : Single;
  public
    function ProcessSample: Single;
    procedure Reset; override;
  published
    property Samplerate;
    property Frequency;
  end;

  TSynchronizedImpulseTrain32 = class(TDspObject)
  private
    FSampleRate: Single;
    FFrequency: Single;
    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    FImpulseTrains : array [0..1] of TAliasFreeImpulseTrain32;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ProcessSample: Double; virtual;
    property Samplerate: Single read FSampleRate write SetSampleRate;
    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  Math, SysUtils, DAV_Complex, DAV_DspInterpolation;

{ TCustomThiranAllpass }

procedure TCustomThiranAllpass.FrequencyChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomThiranAllpass.PhaseDelayChanged;
begin
 CalculateCoefficients;
end;

procedure TCustomThiranAllpass.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomThiranAllpass.SetPhaseDelay(const Value: Single);
begin
 if FPhaseDelay <> Value then
  begin
   FPhaseDelay := Value;
   PhaseDelayChanged;
  end;
end;


{ TThiranAllpass1stOrder }

procedure TThiranAllpass1stOrder.CalculateCoefficients;
var
  NormalizedFrequency : Single;
begin
 NormalizedFrequency := 0.5 * FFrequency / FSampleRate;
 FCoefficient := sin((1 - FPhaseDelay) * NormalizedFrequency)/
   sin((1 + FPhaseDelay) * NormalizedFrequency);
end;


function TThiranAllpass1stOrder.ProcessSample(const Input: Double): Double;
begin
 Result := FCoefficient * (Input - FLastOutput) + FLastInput;
 FLastInput := Input;
 FLastOutput := Result;
end;


{ TThiranAllpass2ndOrder }

procedure TThiranAllpass2ndOrder.CalculateCoefficients;
begin
 FCoefficient[0] := (FPhaseDelay - 2) / (FPhaseDelay + 1);
 FCoefficient[0] := (FPhaseDelay - 2) * (FPhaseDelay - 1) /
   ((FPhaseDelay + 1) * (FPhaseDelay + 2));
end;

function TThiranAllpass2ndOrder.ProcessSample(const Input: Double): Double;
begin
 Result := FCoefficient[1] * Input + FCoefficient[0] * FState[0, 0] +
   FState[0, 1] - FCoefficient[0] * FState[1, 0] -
   FCoefficient[1] * FState[1, 1];

 FState[0, 1] := FState[0, 0];
 FState[0, 0] := Input;
 FState[1, 1] := FState[1, 0];
 FState[1, 0] := Result;
end;

function TThiranAllpass2ndOrder.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Result, Temp);
end;

function TThiranAllpass2ndOrder.Imaginary(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Temp, Result);
end;

function TThiranAllpass2ndOrder.MagnitudeLog10(const Frequency: Double): Double;
begin
 Result := Log10(MagnitudeSquared(Frequency));
end;

procedure TThiranAllpass2ndOrder.PopStates;
begin
 inherited;
 raise Exception.Create('not yet implemented');
end;

procedure TThiranAllpass2ndOrder.PushStates;
begin
 inherited;
 raise Exception.Create('not yet implemented');
end;

procedure TThiranAllpass2ndOrder.Reset;
begin
 inherited;
 Frequency := 0.5;
 PhaseDelay := 2;
end;

procedure TThiranAllpass2ndOrder.ResetStates;
begin
 inherited;
 FState[0, 0] := 0;
 FState[0, 1] := 0;
 FState[1, 0] := 0;
 FState[1, 1] := 0;
end;

procedure TThiranAllpass2ndOrder.ResetStatesInt64;
begin
 inherited;
 FState[0, 0] := 0;
 FState[0, 1] := 0;
 FState[1, 0] := 0;
 FState[1, 1] := 0;
end;

{ TCustomAliasFreeImpulseTrain }

constructor TCustomAliasFreeImpulseTrain.Create;
begin
 inherited Create;
 FFrequency := 440;
 FSampleRate := 44100;
end;

procedure TCustomAliasFreeImpulseTrain.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TCustomAliasFreeImpulseTrain.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomAliasFreeImpulseTrain.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomAliasFreeImpulseTrain.Reset;
begin
 FBufferPos := 0;
end;


{ TCustomAliasFreeImpulseTrain32 }

constructor TCustomAliasFreeImpulseTrain32.Create;
begin
 inherited;
 FBuffer := nil;
 FAllpass := TThiranAllpass2ndOrder.Create; 
 CalculateBufferSize;
end;

destructor TCustomAliasFreeImpulseTrain32.Destroy;
begin
 Dispose(FBuffer);
 FreeAndNil(FAllpass);
 inherited;
end;

procedure TCustomAliasFreeImpulseTrain32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 Reset;
end;

procedure TCustomAliasFreeImpulseTrain32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
 FBufferPos := 0;
end;

procedure TCustomAliasFreeImpulseTrain32.CalculateBufferSize;
var
  Samples : Double;
begin
 Samples := FSampleRate / FFrequency;
 BufferSize := Round(Samples - 0.5) - 2;
 Fractional := BufferSize + 3 - Samples;
end;

procedure TCustomAliasFreeImpulseTrain32.FractionalChanged;
begin
 assert(FFractional >= 0);
 assert(FFractional <= 1);
 FAllpass.PhaseDelay := 1 + FFractional;
end;

procedure TCustomAliasFreeImpulseTrain32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TCustomAliasFreeImpulseTrain32.SetFractional(const Value: Single);
begin
 if FFractional <> Value then
  begin
   FFractional := Value;
   FractionalChanged;
  end;
end;

procedure TCustomAliasFreeImpulseTrain32.FrequencyChanged;
begin
 inherited;
 CalculateBufferSize;
end;


{ TAliasFreeImpulseTrain32 }

procedure TAliasFreeImpulseTrain32.Reset;
begin
 inherited;
 FLastSample := 1;
end;

function TAliasFreeImpulseTrain32.ProcessSample: Single;
begin
 FBuffer^[FBufferPos] := FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

{ TAliasFreeBipolarImpulseTrain32 }

function TAliasFreeBipolarImpulseTrain32.ProcessSample: Single;
begin
 FBuffer^[FBufferPos] := -FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

procedure TAliasFreeBipolarImpulseTrain32.Reset;
begin
 inherited;
 FLastSample := 1;
end;

{ TSynchronizedImpulseTrain32 }

constructor TSynchronizedImpulseTrain32.Create;
begin
 inherited;
 FFrequency := 1000;
 FSampleRate := 44100;
 FImpulseTrains[0] := TAliasFreeImpulseTrain32.Create;
 FImpulseTrains[1] := TAliasFreeImpulseTrain32.Create;
end;

destructor TSynchronizedImpulseTrain32.Destroy;
begin
 FreeAndNil(FImpulseTrains[0]);
 FreeAndNil(FImpulseTrains[1]);
 inherited;
end;

function TSynchronizedImpulseTrain32.ProcessSample: Double;
begin
 Result := FImpulseTrains[0].ProcessSample;
// FImpulseTrains[1].ProcessSample;
end;

procedure TSynchronizedImpulseTrain32.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FImpulseTrains[0].Frequency := FFrequency;
   FImpulseTrains[1].Frequency := FFrequency;
  end;
end;

procedure TSynchronizedImpulseTrain32.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   FImpulseTrains[0].SampleRate := FSampleRate;
   FImpulseTrains[1].SampleRate := FSampleRate;
  end;
end;

end.
