unit DAV_DspBandlimitedImpulseTrain;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspFilterAllpasses;

type
  TCustomSimpleBandlimitedImpulseTrain = class(TDspObject)
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

  TCustomSimpleBandlimitedImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain)
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

  TSimpleBandlimitedImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain32)
  private
    FLastSample : Single;
  public
    function ProcessSample: Single;
    procedure Reset; override;
  published
    property Samplerate;
    property Frequency;
  end;

  TSimpleBandlimitedBipolarImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain32)
  private
    FLastSample : Single;
  public
    function ProcessSample: Single; virtual;
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
    FImpulseTrains : array [0..1] of TSimpleBandlimitedImpulseTrain32;
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

{ TCustomSimpleBandlimitedImpulseTrain }

constructor TCustomSimpleBandlimitedImpulseTrain.Create;
begin
 inherited Create;
 FFrequency := 440;
 FSampleRate := 44100;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.Reset;
begin
 FBufferPos := 0;
end;


{ TCustomSimpleBandlimitedImpulseTrain32 }

constructor TCustomSimpleBandlimitedImpulseTrain32.Create;
begin
 inherited;
 FBuffer := nil;
 FAllpass := TThiranAllpass2ndOrder.Create; 
 CalculateBufferSize;
end;

destructor TCustomSimpleBandlimitedImpulseTrain32.Destroy;
begin
 Dispose(FBuffer);
 FreeAndNil(FAllpass);
 inherited;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 Reset;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
 FBufferPos := 0;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.CalculateBufferSize;
var
  Samples : Double;
begin
 Samples := FSampleRate / FFrequency;
 BufferSize := Round(Samples - 0.5) - 2;
 Fractional := BufferSize + 3 - Samples;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.FractionalChanged;
begin
 assert(FFractional >= 0);
 assert(FFractional <= 1);
 FAllpass.PhaseDelay := 1 + FFractional;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.SetFractional(const Value: Single);
begin
 if FFractional <> Value then
  begin
   FFractional := Value;
   FractionalChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.FrequencyChanged;
begin
 inherited;
 CalculateBufferSize;
end;


{ TSimpleBandlimitedImpulseTrain32 }

procedure TSimpleBandlimitedImpulseTrain32.Reset;
begin
 inherited;
 FLastSample := 1;
end;

function TSimpleBandlimitedImpulseTrain32.ProcessSample: Single;
begin
 FBuffer^[FBufferPos] := FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

{ TSimpleBandlimitedBipolarImpulseTrain32 }

function TSimpleBandlimitedBipolarImpulseTrain32.ProcessSample: Single;
begin
 FBuffer^[FBufferPos] := -FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

procedure TSimpleBandlimitedBipolarImpulseTrain32.Reset;
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
 FImpulseTrains[0] := TSimpleBandlimitedImpulseTrain32.Create;
 FImpulseTrains[1] := TSimpleBandlimitedImpulseTrain32.Create;
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
