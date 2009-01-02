unit DAV_DspChorus;

{$I DAV_Compiler.inc}

interface

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspLFO;

type
  TCustomDspChorus = class(TDspObject)
  private
    FSampleRate   : Double;
    FSpeed        : Double;
    FDepth        : Double;
    FMix          : Double;
    FStages       : Byte;
    FStageMix     : Double;
    FRealBufSize  : Integer;
    FBufferSize   : Integer;
    FBufferInPos  : Integer;
    FBufferOutPos : Integer;
    FLFOs         : array of TLFOSine;
    FDrift        : Double;
    procedure SetDepth(const Value: Double);
    procedure SetMix(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetStages(const Value: Byte);
    procedure SetSampleRate(const Value: Double);
    procedure SetDrift(const Value: Double);
    function GetLFO(Index: Integer): TLFOSine;
  protected
    procedure CalculateStageMix; virtual;
    procedure DepthChanged; virtual;
    procedure MixChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure SpeedChanged; virtual;
    procedure StagesChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
    property LFO[Index: Integer]: TLFOSine read GetLFO; 
  published
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Speed: Double read FSpeed write SetSpeed;
    property Depth: Double read FDepth write SetDepth;
    property Drift: Double read FDrift write SetDrift;
    property Stages: Byte read FStages write SetStages default 0;
    property Mix: Double read FMix write SetMix;
  end;

  TDspChorus32 = class(TCustomDspChorus)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Process(const Input: Single): Single;
    procedure Reset; override;
  published
    property Depth;
    property Drift;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TDspChorus64 = class(TCustomDspChorus)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Process(const Input: Double): Double;
    procedure Reset; override;
  published
    property Depth;
    property Drift;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TCustomDspChorus }

constructor TCustomDspChorus.Create;
begin
 FSampleRate   := 44100;
 FSpeed        := 2;
 FDepth        := 0.5;
 FMix          := 0.5;
 FDrift        := 0;
 Stages        := 2;
 FBufferInPos  := 0;
 FBufferOutPos := 0;
end;

destructor TCustomDspChorus.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FLFOs) - 1
  do FreeAndNil(FLFOs[i]);
 inherited;
end;

function TCustomDspChorus.GetLFO(Index: Integer): TLFOSine;
begin
 if (Index >= 0) and (Index < Length(FLFOs))
  then result := FLFOs[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TCustomDspChorus.Reset;
var
  i : Integer;
begin
 for i := 0 to FStages - 1
  do FLFOs[i].Reset;
end;

procedure TCustomDspChorus.AssignTo(Dest: TPersistent);
var
  i : Integer;
begin
 if Dest is TCustomDspChorus then
  with TCustomDspChorus(Dest) do
   begin
    SampleRate   := Self.FSampleRate;
    Speed        := Self.Speed;
    Depth        := Self.Depth;
    Mix          := Self.Drift;
    Stages       := Self.Stages;
    Drift        := Self.Drift;
    for i := 0 to Stages - 1
     do Self.FLFOs[0].Assign(FLFOs[0]);
   end
 else inherited;
end;

procedure TCustomDspChorus.CalculateStageMix;
begin
 FStageMix := 0.5 * (FMix + Power(FMix, Stages));
end;

procedure TCustomDspChorus.StagesChanged;
var
  i, OldStages : Integer;
begin
 OldStages := Length(FLFOs);
 for i := FStages to OldStages - 1 do FreeAndNil(FLFOs[i]);
 SetLength(FLFOs, FStages);
 for i := OldStages to FStages - 1 do
  begin
   FLFOs[i]            := TLFOSine.Create;
   FLFOs[i].SampleRate := FSampleRate;
  end;
 SpeedChanged; 
 CalculateStageMix;
end;

procedure TCustomDspChorus.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(sqr(Depth) * 0.25 * FSampleRate); // quarter second
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferInPos >= FRealBufSize
  then FBufferInPos := 4;
 FBufferOutPos := FBufferInPos + FBufferSize div 2;
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := FBufferOutPos - FBufferSize;
end;

procedure TCustomDspChorus.SpeedChanged;
var
  i         : Integer;
  d         : Double;
  BasePhase : Double;
begin
 // check that at least one LFO is available
 if Length(FLFOs) = 0 then exit;

 // calculate scale factor
 d := 2 * Pi / Length(FLFOs);

 // store current base phase
 BasePhase := FLFOs[0].Phase;
 for i := 0 to Length(FLFOs) - 1 do
  begin
   FLFOs[i].Frequency := (1 - FDrift * random) * Speed;
   FLFOs[i].Phase := BasePhase + (i + FDrift * random) * d;
  end;
 UpdateBuffer;
end;

procedure TCustomDspChorus.DepthChanged;
begin
 UpdateBuffer;
end;

procedure TCustomDspChorus.MixChanged;
begin
 CalculateStageMix;
end;

procedure TCustomDspChorus.SampleRateChanged;
var
  i : Integer;
begin
 for i := 0 to Length(FLFOs) - 1
  do FLFOs[i].SampleRate := FSampleRate;
 UpdateBuffer;
end;

procedure TCustomDspChorus.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspChorus.SetDrift(const Value: Double);
begin
 if FDrift <> Value then
  begin
   FDrift := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspChorus.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomDspChorus.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDspChorus.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspChorus.SetStages(const Value: Byte);
begin
 if FStages <> Value then
  begin
   FStages := Value;
   StagesChanged;
  end;
end;

{ TDspChorus32 }

constructor TDspChorus32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TDspChorus32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspChorus32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TDspChorus32.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer32^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Single), 0);
end;

function TDspChorus32.Process(const Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // get delayed dry output
 result := (1 - FMix) * FBuffer32[FBufferOutPos];

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);
   result := result + m * Hermite32_asm(d, @FBuffer32[p - 4]);
  end;

 // store new data
 FBuffer32[FBufferInPos] := Input;
 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferInPos := 4;
  end;
end;

{ TDspChorus64 }

constructor TDspChorus64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TDspChorus64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspChorus64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TDspChorus64.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer64^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Double), 0);
end;

function TDspChorus64.Process(const Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
begin
 inherited;

 // get delayed dry output
 result := (1 - FMix) * FBuffer64[FBufferOutPos];

 // increase delayed output buffer position
 inc(FBufferOutPos);
 if FBufferOutPos >= FRealBufSize
  then FBufferOutPos := 4;

 m := FStageMix;
 for i := 0 to Stages - 1 do
  begin
   // calculate next LFO position
   FLFOs[i].CalculateNextSample;
   d := 4 + FBufferSize * 0.5 * (1 - FLFOs[i].Sine);

   // calculate absolute sample position
   p := round(d - 0.5);
   d := d - p;
   p := FBufferInPos + p;
   if p >= FRealBufSize
    then p := p - (FRealBufSize - 4) else
   if p < 4 then p := p + (FRealBufSize - 4);
   result := result + m * Hermite64_asm(d, @FBuffer64[p - 4]);
  end;

 // store new data
 FBuffer64[FBufferInPos] := Input;
 inc(FBufferInPos);
 if FBufferInPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferInPos := 4;
  end;
end;

end.
