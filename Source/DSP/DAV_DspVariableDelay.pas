unit DAV_DspVariableDelay;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspCommon, DAV_DspInterpolation;

type
  TCustomVariableDelay = class(TDspObject)
  private
    FSampleRate   : Single;
    FDelay        : Single;
    FClearBuffer  : Boolean;
    procedure SetSampleRate(const Value: Single);
    procedure SetDelay(const Value: Single);
  protected
    FFractional    : Single;
    FBufferSize    : Integer;
    FBufferPos     : Integer;
    procedure SampleRateChanged; virtual;
    procedure DelayChanged; virtual;
    procedure ChangeBuffer(const NewSize: Integer); virtual; abstract;
    procedure ResetBufferPosition; virtual;
    class function InterpolatorLength: Integer; virtual; abstract;
  public
    constructor Create; virtual;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property Delay: Single read FDelay write SetDelay;
    property ClearBufferOnChange: Boolean read FClearBuffer write FClearBuffer default true;
  end;

  TCustomVariableDelay32 = class(TCustomVariableDelay)
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure ChangeBuffer(const NewSize: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(const Input: Single): Single; virtual; abstract;
  end;

  TVariableDelay32Linear = class(TCustomVariableDelay32)
  protected
    class function InterpolatorLength: Integer; override;
  public
    function ProcessSample(const Input: Single): Single; override;
  end;

  TVariableDelay32Hermite = class(TCustomVariableDelay32)
  protected
//    FBufferOutPos : Integer;
    class function InterpolatorLength: Integer; override;
    procedure ResetBufferPosition; override;
  public
    constructor Create; override;
    function ProcessSample(const Input: Single): Single; override;
  end;

implementation

uses
  Math;

{ TCustomVariableDelay }

constructor TCustomVariableDelay.Create;
begin
 inherited;
 FClearBuffer := True;
 FSampleRate := 44100;
 FDelay := 0;
 ResetBufferPosition;
end;

procedure TCustomVariableDelay.ResetBufferPosition;
begin
 FBufferPos := InterpolatorLength - 1;
end;

procedure TCustomVariableDelay.SetDelay(const Value: Single);
begin
 if FDelay <> Value then
  begin
   FDelay := Value;
   DelayChanged;
  end;
end;

procedure TCustomVariableDelay.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomVariableDelay.SampleRateChanged;
begin
 DelayChanged;
end;

procedure TCustomVariableDelay.DelayChanged;
var
  NewSize : Integer;
begin
 NewSize := round(FSampleRate * FDelay + 0.5);
 FFractional := NewSize - FSampleRate * FDelay;
 ChangeBuffer(NewSize + InterpolatorLength);
end;

{ TCustomVariableDelay32 }

constructor TCustomVariableDelay32.Create;
begin
 inherited;
 FBuffer := nil;
 ChangeBuffer(InterpolatorLength);
end;

destructor TCustomVariableDelay32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TCustomVariableDelay32.ChangeBuffer(const NewSize: Integer);
begin
 if NewSize > FBufferSize then
  begin
   ReallocMem(FBuffer, NewSize * SizeOf(Single));
   if ClearBufferOnChange
    then FillChar(FBuffer^[FBufferSize], (NewSize - FBufferSize) * SizeOf(Single), 0)
    else
     begin
      Move(FBuffer^[FBufferPos], FBuffer^[(NewSize - FBufferPos)], (FBufferSize - FBufferPos) * SizeOf(Single));
      if (NewSize - 2 * FBufferPos) > 0
       then FillChar(FBuffer^[FBufferPos], (NewSize - 2 * FBufferPos) * SizeOf(Single), 0);
     end;
   FBufferSize := NewSize;
  end else
 if NewSize < FBufferSize then
  begin
   FBufferSize := NewSize;
   ReallocMem(FBuffer, NewSize * SizeOf(Single));
   if not ClearBufferOnChange and (FBufferPos < NewSize)
    then Move(FBuffer^[FBufferSize + FBufferPos - NewSize], FBuffer^[FBufferPos], (NewSize - FBufferPos) * SizeOf(Single));
   if FBufferPos >= FBufferSize
    then ResetBufferPosition;
   ReallocMem(FBuffer, NewSize * SizeOf(Single));
  end;
end;

{ TVariableDelay32Linear }

class function TVariableDelay32Linear.InterpolatorLength: Integer;
begin
 result := 2;
end;

function TVariableDelay32Linear.ProcessSample(const Input: Single): Single;
begin
 FBuffer[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 1;

 if FBufferPos + 1 >= FBufferSize then
  begin
   FBuffer[0] := FBuffer[FBufferPos];
   result := LinearInterpolation(FFractional, @FBuffer[0]);
  end
 else result := LinearInterpolation(FFractional, @FBuffer[FBufferPos]);
end;

{ TVariableDelay32Hermite }

constructor TVariableDelay32Hermite.Create;
begin
 inherited;
end;

class function TVariableDelay32Hermite.InterpolatorLength: Integer;
begin
 result := 4;
end;

function ModifiedHermite32(const Fractional: Single; Pntr: PDAV4SingleArray): Single;
{$IFDEF PUREPASCAL}
var
  c : TDAV4SingleArray;
  b : Single;
begin
  c[0] := (Pntr^[2] - Pntr[0]) * CHalf32;
  c[1] := Pntr[1] - Pntr[2];
  c[2] := c[0] + c[1];
  c[3] := c[2] + c[1] - Pntr[1] * CHalf32;
  b    := c[2] + c[3];
  Result := ((((c[3] * Fractional) - b) * Fractional + c[0]) * Fractional + Pntr[1]);
end;
{$ELSE}
asm
    fld  [Pntr +  8].Single;      // x1
    fsub [Pntr     ].Single       // x1-xm1
    fld  [Pntr +  4].Single       // x0           x1-xm1
    fsub [Pntr +  8].Single       // v            x1-xm1
    fld  [Pntr +  4].Single       // x0           v            x1-xm1
    fxch st(2)                    // x1-m1        v            x0
    fmul CHalf32                  // c            v            x0
    fxch st(2)                    // -x0          v            c
    fmul CHalf32                  // 0.5*x0       v            c
    fxch st(2)                    // c            v            0.5*x0
    fst st(3)                     // c            v            0.5*x0       c
    fadd st(0), st(1)             // w            v            0.5*x0       c
    fxch st(2)                    // 0.5*x0       v            w            c

    // verify!
    fsubp st(1), st(0)            // v-.5*x0      w            c
    fadd st(0), st(1)             // a            w            c
    fadd st(1), st(0)             // a            b_neg        c
    fmul Fractional.Single        // a * frac     b_neg        c
    fsubrp st(1), st(0)           // a * f-b      c
    fmul Fractional.Single        // (a*f-b)*f    c
    faddp st(1), st(0)            // res-x0/f
    fmul Fractional.Single        // res-x0
    fadd [Pntr + 4].Single        // res
end;
{$ENDIF}

function TVariableDelay32Hermite.ProcessSample(const Input: Single): Single;
begin
 FBuffer[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 2;

 if FBufferPos + 2 >= FBufferSize then
  begin
   if FBufferPos + 2 = FBufferSize
    then move(FBuffer[FBufferPos], FBuffer[0], 2 * SizeOf(Single));
   result := ModifiedHermite32(FFractional, @FBuffer[FBufferPos - FBufferSize + 2]);
  end
 else result := ModifiedHermite32(FFractional, @FBuffer[FBufferPos]);
end;

procedure TVariableDelay32Hermite.ResetBufferPosition;
begin
 inherited;
// FBufferOutPos := FBufferPos + 1;
end;

end.
