unit DAV_DspGranularPitchShifter;

{$I DAV_Compiler.inc}

interface

uses
  Classes, DAV_Common, DAV_DspCommon, DAV_DspFilter;

type
  TFractionalDelayAllpass = class(TDspObject)
  protected
    FState      : Double;
    FFractional : Single;
  public
    function ProcessSample64(const Input: Double): Double; overload; virtual;
    function ProcessSample32(const Input: Single): Single; overload; virtual;
    constructor Create; virtual;
    property Fractional: Single read FFractional write FFractional;
  end;

  TGranularPitchShifterStage = class(TDspObject)
  private
    FAllpass        : TFractionalDelayAllpass;
    FBufferOffset   : Integer;
    FEnvelopePos    : Double;
    procedure SetFractional(const Value: Double);
    function GetFractional: Double;
  public
    constructor Create; virtual;
    property Allpass: TFractionalDelayAllpass read FAllpass;
    property BufferOffset: Integer read FBufferOffset;
    property Fractional: Double read GetFractional write SetFractional;
  end;

  TCustomDspGranularPitchShifter = class(TDspObject)
  private
    FSampleRate        : Double;
    FSampleRateInv     : Double;
    FSampleOffset      : Double;
    FEnvelopeOffset    : Double;
    FBufferSize        : Integer;
    FBufferPos         : Integer;
    FPitchShifterStage : array of TGranularPitchShifterStage;
    FStages            : Byte;
    FStageMix          : Double;
    FSemitones         : Double;
    FGranularity       : Double;
    procedure SetStages(const Value: Byte);
    procedure SetSampleRate(const Value: Double);
    procedure SetSemitones(const Value: Double);
    procedure SetGranularity(const Value: Double);
    procedure CalculateEnvelopeOffset;
  protected
    procedure SampleRateChanged; virtual;
    procedure StagesChanged; virtual;
    procedure SemitonesChanged; virtual;
    procedure GranularityChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Reset; virtual; abstract;
  published
    property Semitones: Double read FSemitones write SetSemitones;
    property Granularity: Double read FGranularity write SetGranularity; // in s
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Stages: Byte read FStages write SetStages default 0;
  end;

  TDspGranularPitchShifter32 = class(TCustomDspGranularPitchShifter)
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
    property SampleRate;
    property Stages;
  end;

  TDspGranularPitchShifter64 = class(TCustomDspGranularPitchShifter)
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
    property SampleRate;
    property Stages;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TFractionalDelayAllpass }

constructor TFractionalDelayAllpass.Create;
begin
 FState := 0;
end;

function TFractionalDelayAllpass.ProcessSample32(const Input: Single): Single;
begin
 result := FState + FFractional * Input;
 FState := Input - FFractional * result;
end;

function TFractionalDelayAllpass.ProcessSample64(const Input: Double): Double;
begin
 result := FState + FFractional * Input;
 FState := Input - FFractional * result;
end;

{ TGranularPitchShifterStage }

constructor TGranularPitchShifterStage.Create;
begin
 inherited;
 FAllpass := TFractionalDelayAllpass.Create;
 FBufferOffset := 0;
end;

function TGranularPitchShifterStage.GetFractional: Double;
begin
 result := FAllpass.Fractional;
end;

procedure TGranularPitchShifterStage.SetFractional(const Value: Double);
begin
 if FAllpass.Fractional <> Value then
  begin
   FAllpass.Fractional := Value;
   while FAllpass.Fractional > 1 do
    begin
     FAllpass.Fractional := FAllpass.Fractional - 1;
     Inc(FBufferOffset);
    end;
   while FAllpass.Fractional < 0 do
    begin
     FAllpass.Fractional := FAllpass.Fractional + 1;
     Dec(FBufferOffset);
    end;
  end;
end;


{ TCustomDspGranularPitchShifter }

constructor TCustomDspGranularPitchShifter.Create;
begin
 FBufferPos     := 0;
 FSemitones     := 0;
 FGranularity   := 0.05;
 Stages         := 2;
 FSampleRate    := 44100;
 SampleRateChanged;
end;

procedure TCustomDspGranularPitchShifter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspGranularPitchShifter then
  with TCustomDspGranularPitchShifter(Dest) do
   begin
    SampleRate   := Self.FSampleRate;
    Semitones    := Self.FSemitones;
    Stages       := Self.Stages;
   end
 else inherited;
end;

procedure TCustomDspGranularPitchShifter.StagesChanged;
var
  i            : Integer;
  BaseStage, d : Double;
  BaseEnv      : Double;
  StageOffset  : Double;
begin
 with FPitchShifterStage[0] do BaseStage := FBufferOffset + Fractional;
 StageOffset := 1 / FStages;
 if FBufferSize > 0 then
  begin
   BaseEnv := BaseStage / FBufferSize;
   FPitchShifterStage[0].FEnvelopePos := BaseEnv;
//   assert(abs(BaseEnv - FPitchShifterStage[0].FEnvelopePos) < 1E-3);
   for i := 1 to FStages - 1 do
    with FPitchShifterStage[i] do
     begin
      FPitchShifterStage[i].FEnvelopePos := i * StageOffset + BaseEnv;
      while FEnvelopePos > 1 do FEnvelopePos := FEnvelopePos - 1;
      d := BaseStage + i * StageOffset * FBufferSize;
      FBufferOffset := round(d + 0.500001) - 1;
      FAllpass.FFractional := d - FBufferOffset;
      while FBufferOffset > FBufferSize
       do FBufferOffset := FBufferOffset - FBufferSize;
     end;
  end;

 FStageMix := StageOffset;
end;

procedure TCustomDspGranularPitchShifter.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(FGranularity * FSampleRate) + 1; // quarter second

 // check and reset buffer position
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;

 StagesChanged;
 CalculateEnvelopeOffset;
end;

procedure TCustomDspGranularPitchShifter.SampleRateChanged;
begin
 FSampleRateInv := 1 / SampleRate;
 UpdateBuffer;
end;

procedure TCustomDspGranularPitchShifter.SemitonesChanged;
begin
 FSampleOffset := Power(2, FSemitones / 12) - 1;
 CalculateEnvelopeOffset;
end;

procedure TCustomDspGranularPitchShifter.SetGranularity(const Value: Double);
begin
 if FGranularity <> Value then
  begin
   FGranularity := Value;
   GranularityChanged;
  end;
end;

procedure TCustomDspGranularPitchShifter.CalculateEnvelopeOffset;
begin
 FEnvelopeOffset := abs(FSampleOffset / FBufferSize);
end;

procedure TCustomDspGranularPitchShifter.GranularityChanged;
begin
 UpdateBuffer;
end;

procedure TCustomDspGranularPitchShifter.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDspGranularPitchShifter.SetSemitones(const Value: Double);
begin
 if FSemitones <> Value then
  begin
   FSemitones := Value;
   SemitonesChanged;
  end;
end;

procedure TCustomDspGranularPitchShifter.SetStages(const Value: Byte);
var
  i : Integer;
begin
 if FStages <> Value then
  begin
   if FStages > Value then
    begin
     FStages := Value;
     for i := Length(FPitchShifterStage) - 1 downto FStages
      do FreeAndNil(FPitchShifterStage[i]);
     SetLength(FPitchShifterStage, FStages);
    end
   else
    begin
     SetLength(FPitchShifterStage, Value);
     for i := FStages to Length(FPitchShifterStage) - 1
      do FPitchShifterStage[i] := TGranularPitchShifterStage.Create;
     FStages := Value;
    end;
   StagesChanged;
  end;
end;

{ TDspGranularPitchShifter32 }

constructor TDspGranularPitchShifter32.Create;
begin
 FBuffer32 := nil;
 inherited;
end;

destructor TDspGranularPitchShifter32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspGranularPitchShifter32.Reset;
begin
 FillChar(FBuffer32^[0], FBufferSize * SizeOf(Single), 0);
end;

procedure TDspGranularPitchShifter32.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FBufferSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FBufferSize * SizeOf(Single));
 if FBufferSize > OldBufferSize
  then FillChar(FBuffer32^[OldBufferSize], (FBufferSize - OldBufferSize) * SizeOf(Single), 0);
end;

function TDspGranularPitchShifter32.Process(const Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 result := 0;

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 0;

 i := 0;
 for i := 0 to Stages - 1 do
  with FPitchShifterStage[i] do
   begin
    p := FBufferPos + FBufferOffset;

    // calculate absolute sample position
    while p >= FBufferSize do p := p - FBufferSize;
    while p < 0 do p := p + FBufferSize;

    v := FStageMix * (1 - abs(2 * FEnvelopePos - 1));
    FEnvelopePos := FEnvelopePos + FEnvelopeOffset;
    if FEnvelopePos >= 1 then FEnvelopePos := FEnvelopePos - 1;
    result := result + v * Allpass.ProcessSample32(FBuffer32[p]);

    FAllpass.FFractional := FAllpass.Fractional + FSampleOffset;
    if ((PInteger(@FAllpass.FFractional)^ and $FF800000) shr 23) > 126 then
     while FAllpass.Fractional > 1 do
      begin
       inc(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional - 1
      end;
     while FPitchShifterStage[i].FAllpass.Fractional < 0 do
      begin
       dec(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional + 1
      end;

    if FBufferOffset >= FBufferSize then FBufferOffset := 0 else
    if FBufferOffset < 0 then FBufferOffset := FBufferOffset + FBufferSize;
   end;
end;

{ TDspGranularPitchShifter64 }

constructor TDspGranularPitchShifter64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TDspGranularPitchShifter64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspGranularPitchShifter64.Reset;
begin
 FillChar(FBuffer64^[0], FBufferSize * SizeOf(Double), 0);
end;

procedure TDspGranularPitchShifter64.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FBufferSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FBufferSize * SizeOf(Double));
 if FBufferSize > OldBufferSize
  then FillChar(FBuffer64^[OldBufferSize], (FBufferSize - OldBufferSize) * SizeOf(Double), 0);
end;

function TDspGranularPitchShifter64.Process(const Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 result := 0;

 // store new data
 FBuffer64[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 0;

 i := 0;
 for i := 0 to Stages - 1 do
  with FPitchShifterStage[i] do
   begin
    d := FBufferPos + FBufferOffset;

    while p >= FBufferSize do p := p - FBufferSize;
    while p < 0 do p := p + FBufferSize;

    result := result + Allpass.ProcessSample64(FBuffer64[p]);

    FAllpass.Fractional := FAllpass.Fractional + FSampleOffset;
    while FAllpass.Fractional > 1 do
     begin
      inc(FBufferOffset);
      FAllpass.Fractional := FAllpass.Fractional - 1
     end;
    while FAllpass.Fractional < 1 do
     begin
      dec(FBufferOffset);
      FAllpass.Fractional := FAllpass.Fractional + 1
     end;

    if FBufferOffset >= FBufferSize then FBufferOffset := 0 else
    if FBufferOffset < 0 then FBufferOffset := FBufferSize - 1;
   end;
end;

end.
