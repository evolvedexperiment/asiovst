unit DAV_DspVibrato;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspLFO;

type
  TCustomDspVibrato = class(TDspObject)
  private
    FSampleRate   : Double;
    FSpeed        : Double;
    FDepth        : Double;
    FRealBufSize  : Integer;
    FBufferSize   : Integer;
    FBufferPos    : Integer;
    FLFO          : TLFOSine;
    procedure SetDepth(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetSampleRate(const Value: Double);
  protected
    procedure DepthChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure SpeedChanged; virtual;
    procedure UpdateBuffer; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
  published
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Speed: Double read FSpeed write SetSpeed;
    property Depth: Double read FDepth write SetDepth;
  end;

  TCustomDspVibrato32 = class(TCustomDspVibrato)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Process(const Input: Single): Single;
    procedure Reset; override;
  end;

  TDspVibrato32 = class(TCustomDspVibrato32)
  published
    property Depth;
    property SampleRate;
    property Speed;
  end;

  TCustomDspVibrato64 = class(TCustomDspVibrato)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Process(const Input: Double): Double;
    procedure Reset; override;
  end;

  TDspVibrato64 = class(TCustomDspVibrato64)
  published
    property Depth;
    property SampleRate;
    property Speed;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TCustomDspVibrato }

constructor TCustomDspVibrato.Create;
begin
 FSampleRate   := 44100;
 FSpeed        := 2;
 FDepth        := 0.5;
 FBufferPos    := 0;
 FLFO          := TLFOSine.Create;
end;

destructor TCustomDspVibrato.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomDspVibrato.Reset;
begin
 FLFO.Reset;
end;

procedure TCustomDspVibrato.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(sqr(Depth) * 0.25 * FSampleRate); // quarter second
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomDspVibrato.SpeedChanged;
begin
 FLFO.Frequency := Speed;
 UpdateBuffer;
end;

procedure TCustomDspVibrato.DepthChanged;
begin
 UpdateBuffer;
end;

procedure TCustomDspVibrato.SampleRateChanged;
begin
 FLFO.SampleRate := FSampleRate;
 UpdateBuffer;
end;

procedure TCustomDspVibrato.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspVibrato.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDspVibrato.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

{ TCustomDspVibrato32 }

constructor TCustomDspVibrato32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TCustomDspVibrato32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomDspVibrato32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TCustomDspVibrato32.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
end;

function TCustomDspVibrato32.Process(const Input: Single): Single;
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
 result := Hermite32_asm(d, @FBuffer32[p - 4]);

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TCustomDspVibrato64 }

constructor TCustomDspVibrato64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TCustomDspVibrato64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TCustomDspVibrato64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TCustomDspVibrato64.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
end;

function TCustomDspVibrato64.Process(const Input: Double): Double;
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
 result := Hermite64_asm(d, @FBuffer64[p - 4]);

 // store new data
 FBuffer64[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferPos := 4;
  end;
end;

end.
