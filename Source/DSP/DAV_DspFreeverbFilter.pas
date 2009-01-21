unit DAV_DspFreeverbFilter;

interface

uses
  Classes, DAV_Common, DAV_DspCommon;
{$ALIGN 8}

// Reverb model tuning values, taken from original algoritm by Jezar

const
  CMuted           = 0.0;
  CFixedGain       = 0.015;
  CScaleDamp       = 0.4;
  CScaleRoom       = 0.28;
  COffsetRoom      = 0.7;
  CInitialRoom     = 0.5;
  CInitialDamp     = 0.5;
  CInitialWidth    = 1.0;
  CInitialMode     = 0.0;
  CFreezeMode      = 0.5;

  // Allpass filter class declaration
type
  TAllpass = class(TDspObject)
  private
    FFeedback    : Single;
    FBuffer      : PDAVSingleFixedArray;
    FBufferSize  : Integer;
    FBufferPos   : Integer;
    procedure SetBufferSize(const Value: Integer);
  public
    constructor Create(const BS: Integer); virtual;
    destructor Destroy; override;
    function Process(const Input: Single): Single; register;
    procedure Mute;
    property Feedback: Single read FFeedback write FFeedback;
    property BufferSize : Integer read FBufferSize write SetBufferSize;
  end;

  // Comb filter class declaration
  TComb = class(TDspObject)
  private
    FFeedback    : Single;
    FFilterStore : Single;
    FDampA       : Single;
    FDampB       : Single;
    FBuffer      : PDAVSingleFixedArray;
    FBufferSize  : Integer;
    FBufferPos   : Integer;
    FDamp        : Single;
    procedure SetDamp(Value: Single);
    procedure SetBufferSize(const Value: Integer);
  public
    constructor Create(const BS: Integer); virtual;
    destructor Destroy; override;
    function Process(const Input: Single): Single; register;
    procedure Mute;
    property Damp: Single read FDamp write SetDamp;
    property Feedback: Single read FFeedback write FFeedback;
    property BufferSize : Integer read FBufferSize write SetBufferSize;
  end;

implementation

{ TAllpass }

constructor TAllpass.Create(const BS: Integer);
begin
 inherited Create;
 Buffersize := BS;
end;

destructor TAllpass.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TAllpass.SetBufferSize(const Value: Integer);
begin             
 FBufferSize := Value - 1;
 ReallocMem(FBuffer, (FBufferSize + 1) * SizeOf(Single));
 FBufferPos := 0;
end;

procedure TAllpass.Mute;
begin
 Fillchar(FBuffer^[0], (FBufferSize + 1) * SizeOf(Single), 0);
end;

function TAllpass.Process(const Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 FBuffer^[FBufferPos] := ((FBuffer^[FBufferPos] - Input) * FFeedback) + Input;
 if FBufferPos < FBufferSize
  then inc(FBufferPos)
  else FBufferPos := 0;
end;
{$ELSE}
asm
  mov  ecx, [eax].FBuffer                 // FBuffer start in ecx
  mov  edx, [eax].FBufferPos              // FBuffer index in edx
  fld  input

  // This checks for very small values that can cause a Processor
  // to switch in extra precision fMode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // The code is equivalent to the C inline macro by Jezar
  // This is the same spot where the original C macro appears
  test dword ptr [ecx + edx], $7F800000   // test if denormal
  jnz @Normal
  mov dword ptr [ecx + edx], 0            // if so, zero out
@normal:

  fld  [ecx + edx].Single                 // load current sample from FBuffer
  fsub st(0), st(1)                       // subtract input sample

  fxch                                    // this is a zero cycle operant,
                                          // just renames the stack internally
  fmul [eax].FFeedback.Single             // multiply stored sample with FFeedback
  fadd input                              // and add the input
  fstp [ecx + edx].Single;                // store at the current sample pos
  add  edx, 4                             // increment sample position
  cmp  edx, [eax].FBufferSize;            // are we at end of FBuffer?
  jb   @OK
  xor  edx, edx                           // if so, reset FBuffer index
@OK:
  mov  [eax].FBufferPos, edx            // and store new index,
                                          // result already in st(0),
                                          // hence the fxch
end;
{$ENDIF}

{ TComb }

constructor TComb.Create(const BS: Integer);
begin
 inherited Create;
 Buffersize := BS;
 FFilterStore := 0;
end;

destructor TComb.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TComb.SetBufferSize(const Value: Integer);
begin
 FBufferSize := Value;
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FBufferPos := 0;
end;

procedure TComb.SetDamp(Value: Single);
begin
 FDampA := Value;
 FDampB := 1 - Value;
end;

procedure TComb.Mute;
begin
 Fillchar(FBuffer^[0], FBufferSize * SizeOf(Single), 0);
end;

{ I really don't know if this is all as fast as can be,
  but it beats Delphi's compiler generated code hands down,
  Thaddy}

function TComb.Process(const input: Single): Single;
asm
  mov   ecx, [eax].FBuffer                        // FBuffer start in ecx
  mov   edx, [eax].FBufferPos                   // FBuffer index in edx

  // This checks for very small values that can cause a Processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [ecx+edx], $7F800000           // test if denormal
  jnz   @Normal
  mov   dword ptr [ecx+edx], 0                   // if so, zero out
@normal:

  fld   [ecx + edx].Single;                      // load sample from FBuffer
  fld   st(0)                                    // duplicate on the stack
  fmul  [eax].FDampB                             // multiply with FDampB
  fld   [eax].FFilterStore;                      // load stored filtered sample
  fmul  [eax].FDampA                             // multiply with FDampA
  faddp
  fst   [eax].FFilterStore                       // store it back

  // This checks for very small values that can cause a Processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [eax].FFilterStore, $7F800000  // test if denormal
  jnz   @Normal2
  mov   dword ptr [eax].FFilterStore, 0          // if so, zero out
@normal2:

  fmul  [eax].FFeedback                          // multiply with FFeedback
  fadd  input                                    // and add to input sample
  fstp  [ecx+edx].Single                         // store at current FBuffer pos
  add   edx, 4                                   // Update FBuffer index
  cmp   edx, [eax].FBufferSize;                  // end of FBuffer reached?
  jb    @OK
  xor   edx, edx                                 // if so, reset Buffer index
@OK:
  mov  [eax].FBufferPos, edx                   // and store new index.
                                                 // result already in st(0),
                                                 // hence duplicate
end;

end.
