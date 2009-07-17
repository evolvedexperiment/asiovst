unit DAV_DspModDelay;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspLFO;

type
  TCustomModDelay = class(TDspObject)
  private
    FSampleRate     : Double;
    FDelay          : Double;
    FDepth          : Double;
    FFeedback       : Double;
    FFeedbackFactor : Double;
    FMix            : Double;
    FMixFactors     : array [0..1] of Single;
    FRate           : Double;
    FRealBufSize    : Integer;
    FBufferSize     : Integer;
    FBufferPos      : Integer;
    FLFO            : TLFOSine;
    procedure SetDepth(const Value: Double);
    procedure SetRate(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetDelay(const Value: Double);
    procedure SetFeedback(const Value: Double);
    procedure SetMix(const Value: Double);
  protected
    procedure DelayChanged; virtual;
    procedure DepthChanged; virtual;
    procedure FeedbackChanged; virtual;
    procedure MixChanged; virtual;
    procedure RateChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure UpdateBuffer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
  published
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Mix: Double read FMix write SetMix;
    property Delay: Double read FDelay write SetDelay;
    property Feedback: Double read FFeedback write SetFeedback;
    property Rate: Double read FRate write SetRate;
    property Depth: Double read FDepth write SetDepth;
  end;

  TCustomModDelay32 = class(TCustomModDelay)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single;
    procedure Reset; override;
  end;

  TModDelay32 = class(TCustomModDelay32)
  published
    property Mix;          // [%]
    property Delay;        // [ms]
    property Depth;        // [%]
    property Feedback;     // [%]
    property Rate;         // [Hz]
    property SampleRate;
  end;

  TCustomModDelay64 = class(TCustomModDelay)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Double): Double;
    procedure Reset; override;
  end;

  TModDelay64 = class(TCustomModDelay64)
  published
    property Mix;          // [%]
    property Delay;        // [ms]
    property Depth;        // [%]
    property Feedback;     // [%]
    property Rate;         // [Hz]
    property SampleRate;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TCustomModDelay }

constructor TCustomModDelay.Create;
begin
 FSampleRate   := 44100;
 FRate         := 2;
 FDepth        := 0.5;
 FBufferPos    := 0;
 FLFO          := TLFOSine.Create;
end;

destructor TCustomModDelay.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomModDelay.Reset;
begin
 FLFO.Reset;
end;

procedure TCustomModDelay.UpdateBuffer;
begin
 // calculate buffer size in samples
 FBufferSize  := round(0.001 * FDelay * FSampleRate);
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomModDelay.DelayChanged;
begin
 UpdateBuffer;
end;

procedure TCustomModDelay.DepthChanged;
begin
 FLFO.Amplitude := 0.01 * FDepth;
end;

procedure TCustomModDelay.FeedbackChanged;
begin
 FFeedbackFactor := 0.01 * FFeedback;
end;

procedure TCustomModDelay.MixChanged;
begin
 FMixFactors[1] := 0.01 * FMix;
 FMixFactors[0] := 1 - FMixFactors[1];
end;

procedure TCustomModDelay.RateChanged;
begin
 FLFO.Frequency := Rate;
end;

procedure TCustomModDelay.SampleRateChanged;
begin
 FLFO.SampleRate := FSampleRate;
 UpdateBuffer;
end;

procedure TCustomModDelay.SetDelay(const Value: Double);
begin
 if FDelay <> Value then
  begin
   FDelay := Value;
   DelayChanged;
  end;
end;

procedure TCustomModDelay.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomModDelay.SetFeedback(const Value: Double);
begin
 if FFeedback <> Value then
  begin
   FFeedback := Value;
   FeedbackChanged;
  end;
end;

procedure TCustomModDelay.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomModDelay.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomModDelay.SetRate(const Value: Double);
begin
 if FRate <> Value then
  begin
   FRate := Value;
   RateChanged;
  end;
end;

{ TCustomModDelay32 }

constructor TCustomModDelay32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TCustomModDelay32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomModDelay32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TCustomModDelay32.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
end;

function TCustomModDelay32.ProcessSample(const Input: Single): Single;
var
  p : Integer;
  d : Double;
begin
 inherited;

 // calculate next LFO position
 FLFO.CalculateNextSample;
 d := 4 + FBufferSize * 0.5 * (1 - FLFO.Sine);

 // calculate absolute sample position
 p := round(d - 0.5);
 d := d - p;
 p := FBufferPos + p;
 if p >= FRealBufSize
  then p := p - (FRealBufSize - 4) else
 if p < 4 then p := p + (FRealBufSize - 4);

 // calculate pure result
 result := Hermite32_asm(d, @FBuffer32[p - 4]);

 // store new data
 FBuffer32[FBufferPos] := Input + FFeedbackFactor * result;

 // apply mix
 result := FMixFactors[0] * Input + FMixFactors[1] * result;

 // advance buffer position
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TCustomModDelay64 }

constructor TCustomModDelay64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TCustomModDelay64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TCustomModDelay64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TCustomModDelay64.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
end;

function TCustomModDelay64.ProcessSample(const Input: Double): Double;
var
  p : Integer;
  d : Double;
begin
 inherited;

 // calculate next LFO position
 FLFO.CalculateNextSample;
 d := 4 + FBufferSize * 0.5 * (1 - FLFO.Sine);

 // calculate absolute sample position
 p := round(d - 0.5);
 d := d - p;
 p := FBufferPos + p;
 if p >= FRealBufSize
  then p := p - (FRealBufSize - 4) else
 if p < 4 then p := p + (FRealBufSize - 4);

 // calculate pure result
 result := Hermite64_asm(d, @FBuffer64[p - 4]);

 // store new data
 FBuffer64[FBufferPos] := Input + FFeedbackFactor * result;

 // apply mix
 result := FMixFactors[0] * Input + FMixFactors[1] * result;

 // advance buffer position
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferPos := 4;
  end;
end;

end.
