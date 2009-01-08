unit DAV_DspBarberpole;

{$I DAV_Compiler.inc}

interface

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspLFO;

type
  TBarberpoleDirection = (sdUp, sdDown, sdUpInv, sdDownInv);
  TCustomDspBarberpole = class(TDspObject)
  private
    FSampleRate    : Double;
    FSampleRateInv : Double;
    FSpeed         : Double;
    FDepth         : Double;
    FMix           : Double;
    FOffset        : Double;
    FStageMix      : Double;
    FRealBufSize   : Integer;
    FBufferSize    : Integer;
    FBufferPos     : Integer;
    FDirection     : TBarberpoleDirection;
    FStagePosition : PDAVDoubleFixedArray;
    FStages        : Byte;
    procedure SetDepth(const Value: Double);
    procedure SetMix(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetStages(const Value: Byte);
    procedure SetSampleRate(const Value: Double);
    procedure SetDirection(const Value: TBarberpoleDirection);
  protected
    procedure CalculateStageMix; virtual;
    procedure DirectionChanged; virtual;
    procedure DepthChanged; virtual;
    procedure MixChanged; virtual;
    procedure SampleRateChanged; virtual;
    procedure SpeedChanged; virtual;
    procedure StagesChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Reset; virtual; abstract;
  published
    property Depth: Double read FDepth write SetDepth;
    property Direction: TBarberpoleDirection read FDirection write SetDirection;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Speed: Double read FSpeed write SetSpeed;
    property Stages: Byte read FStages write SetStages default 0;
    property Mix: Double read FMix write SetMix;
  end;

  TDspBarberpole32 = class(TCustomDspBarberpole)
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
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TDspBarberpole64 = class(TCustomDspBarberpole)
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
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TCustomDspBarberpole }

constructor TCustomDspBarberpole.Create;
begin
 FSpeed         := 2;
 FDepth         := 0.5;
 FMix           := 0.5;
 FBufferPos     := 4;
 FStagePosition := nil;
 FDirection     := sdUp;
 Stages         := 2;
 SampleRate     := 44100;
end;

procedure TCustomDspBarberpole.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspBarberpole then
  with TCustomDspBarberpole(Dest) do
   begin
    SampleRate   := Self.FSampleRate;
    Speed        := Self.Speed;
    Depth        := Self.Depth;
    Mix          := Self.Mix;
    Stages       := Self.Stages;
   end
 else inherited;
end;

procedure TCustomDspBarberpole.CalculateStageMix;
begin
 FStageMix := sqrt(2) * 0.5 * (FMix + Power(FMix, Stages));
end;

procedure TCustomDspBarberpole.StagesChanged;
var
  i           : Integer;
  BaseStage   : Double;
  StageOffset : Double;
begin
 BaseStage := FStagePosition^[0];
 StageOffset := 1 / FStages;
 for i := 1 to FStages - 1 do
  begin
   FStagePosition^[i] := BaseStage + i * StageOffset;
   if FStagePosition^[i] >= 1
    then FStagePosition^[i] := FStagePosition^[i] - 1;
  end;
 SpeedChanged;
 CalculateStageMix;
end;

procedure TCustomDspBarberpole.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(sqr(Depth) * 0.25 * FSampleRate); // quarter second
 FRealBufSize := FBufferSize + 4;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomDspBarberpole.SpeedChanged;
begin
 FOffset := FSampleRateInv * Speed;
end;

procedure TCustomDspBarberpole.DepthChanged;
begin
 UpdateBuffer;
end;

procedure TCustomDspBarberpole.MixChanged;
begin
 CalculateStageMix;
end;

procedure TCustomDspBarberpole.SampleRateChanged;
begin
 FSampleRateInv := 1 / SampleRate;
 UpdateBuffer;
end;

procedure TCustomDspBarberpole.DirectionChanged;
begin
 //
end;

procedure TCustomDspBarberpole.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspBarberpole.SetDirection(const Value: TBarberpoleDirection);
begin
 if FDirection <> Value then
  begin
   FDirection := Value;
   DirectionChanged;
  end;
end;

procedure TCustomDspBarberpole.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomDspBarberpole.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDspBarberpole.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspBarberpole.SetStages(const Value: Byte);
begin
 if FStages <> Value then
  begin
   if FStages > Value then
    begin
     FStages := Value;
     ReallocMem(FStagePosition, Value * SizeOf(Double));
    end
   else
    begin
     ReallocMem(FStagePosition, Value * SizeOf(Double));
     FillChar(FStagePosition^[FStages], (Value - FStages) * SizeOf(Double), 0);
     FStages := Value;
    end;
   StagesChanged;
  end;
end;

{ TDspBarberpole32 }

constructor TDspBarberpole32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TDspBarberpole32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspBarberpole32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TDspBarberpole32.UpdateBuffer;
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

function TDspBarberpole32.Process(const Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 result := (1 - FMix) * Input;

 m := FStageMix;
 case FDirection of
  sdUp :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := round(d - CHalf32);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDown :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := round(d + CHalf32);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize else
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdUpInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := round(d - CHalf32);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDownInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := round(d + CHalf32);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
 end;

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TDspBarberpole64 }

constructor TDspBarberpole64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TDspBarberpole64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspBarberpole64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TDspBarberpole64.UpdateBuffer;
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

function TDspBarberpole64.Process(const Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 result := (1 - FMix) * Input;

 m := FStageMix;

 case FDirection of
  sdUp :
    for i := 0 to Stages - 1 do
     begin
      d := FBufferSize * FStagePosition^[i];

      // calculate absolute sample position
      p := round(d + CHalf64);
      d := p - d;

      p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize else
     if p >= FBufferSize then p := p - FBufferSize;

      v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
      result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

      FStagePosition^[i] := FStagePosition^[i] + FOffset;
      if FStagePosition^[i] >= 1
       then FStagePosition^[i] := 0;
     end;
  sdDown :
    for i := 0 to Stages - 1 do
     begin
      d := FBufferSize * FStagePosition^[i];

      // calculate absolute sample position
      p := round(d + CHalf64);
      d := p - d;

      p := FBufferPos - p - 2;

      if p < 0 then p := p + FBufferSize else
      if p >= FBufferSize then p := p - FBufferSize;

      v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
      result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

      FStagePosition^[i] := FStagePosition^[i] + FOffset;
      if FStagePosition^[i] >= 1
       then FStagePosition^[i] := 0;
     end;
 end;

 // store new data
 FBuffer64[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

end.
