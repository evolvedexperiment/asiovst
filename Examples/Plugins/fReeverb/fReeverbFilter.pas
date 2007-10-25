unit fReeverbFilter;

interface

uses DAVDCommon, Classes;
{$ALIGN 8}

// Reverb fModel tuning values, taken from original algoritm by Jezar

const
  Kdenorm:single =1.0e-23;

  Muted = 0;
  fixedGain = 0.015;
  scaledamp = 0.4;
  scaleroom = 0.28;
  offsetroom = 0.7;
  initialRoom = 0.5;
  initialDamp = 0.5;
  initialWidth = 1;
  initialMode = 0;
  freezeMode = 0.5;

  // Allpass filter class declaration
type
  TAllpass = class(TObject)
  private
    fFeedback: Single;
    fBuffer: TAVDSingleDynArray;
    fBufferSize,
    fBufferIndex: Integer;
    procedure SetBufferSize(const Value: Integer);
  public
    constructor Create(BS: Integer); virtual;
    destructor Destroy; override;
    function Process(const input: Single): Single; register;
    procedure Mute;
    property Feedback: Single read fFeedback write fFeedback;
    property BufferSize : Integer read fBufferSize write SetBufferSize;
  end;

  // Comb filter class declaration
  TComb = class(TObject)
  private
    fFeedback,
    fFilterStore,
    fDampA,
    fDampB: Single;
    fBuffer: TAVDSingleDynArray;
    fBufferSize,
    fBufferIndex: Integer;
    fDamp: Single;
    procedure SetDamp(Value: Single);
    procedure SetBufferSize(const Value: Integer);
  public
    constructor Create(BS: Integer); virtual;
    destructor Destroy; override;
    function Process(const input: Single): Single; register;
    procedure Mute;
    property Damp: Single read fDamp write SetDamp;
    property Feedback: Single read fFeedback write fFeedback;
    property BufferSize : Integer read fBufferSize write SetBufferSize;
  end;

implementation

constructor TAllpass.Create(BS: Integer);
begin
 inherited Create;
 Buffersize:=BS;
end;

destructor TAllpass.Destroy;
begin
 SetLength(fBuffer,0);
 inherited;
end;

procedure TAllpass.SetBufferSize(const Value: Integer);
begin
 fBufferSize := Value;
 SetLength(fBuffer,fBuffersize);
 fBufferIndex := 0;
end;

procedure TAllpass.Mute;
begin
 Fillchar(fBuffer[0],fBufferSize*SizeOf(Single), 0);
end;

function TAllpass.Process(const input: Single): Single;
asm
  mov  ecx, [eax].fBuffer                 // fBuffer start in ecx
  mov  edx, [eax].fBufferIndex            // fBuffer index in edx
  fld  input

  // This checks for very small values that can cause a Processor
  // to switch in extra precision fMode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // The code is equivalent to the C inline macro by Jezar
  // This is the same spot where the original C macro appears
  test dword ptr [ecx+edx], $7F800000     // test if denormal
  jnz @Normal
  mov dword ptr [ecx+edx], 0              // if so, zero out
@normal:

  fld  [ecx+edx].Single                   // load current sample from fBuffer
  fsub st(0), st(1)                       // subtract input sample
  // NOT fsub, because delphi 7 translates that into fsubp!
  fxch                                    // this is a zero cycle operant,
                                          // just renames the stack internally
  fmul [eax].fFeedback                    // multiply stored sample with fFeedback
  fadd input                              // and add the input
  fstp [ecx + edx].Single;                // store at the current sample pos
  add  edx, 4                             // increment sample position
  cmp  edx, [eax].fBufferSize;            // are we at end of fBuffer?
  jb   @OK
  xor  edx, edx                           // if so, reset fBuffer index
@OK:
  mov  [eax].fBufferIndex, edx                  // and store new index,
                                          // result already in st(0),
                                          // hence the fxch
end;

constructor TComb.Create(BS: Integer);
begin
 inherited Create;
 Buffersize:=BS;
 fFilterStore := 0;
end;

destructor TComb.Destroy;
begin
 SetLength(fBuffer,0);
 inherited;
end;

procedure TComb.SetBufferSize(const Value: Integer);
begin
 fBufferSize := Value;
 SetLength(fBuffer,fBuffersize);
 fBufferIndex := 0;
end;

procedure TComb.SetDamp(Value: Single);
begin
 fDampA := Value;
 fDampB := 1 - Value;
end;

procedure TComb.Mute;
begin
 Fillchar(fBuffer[0],fBufferSize*SizeOf(Single), 0);
end;

{ I really don't know if this is all as fast as can be,
  but it beats Delphi's compiler generated code hands down,
  Thaddy}

function TComb.Process(const input: Single): Single;
asm
  mov   ecx, [eax].fBuffer                        // fBuffer start in ecx
  mov   edx, [eax].fBufferIndex                   // fBuffer index in edx

  // This checks for very small values that can cause a Processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [ecx+edx], $7F800000           // test if denormal
  jnz   @Normal
  mov   dword ptr [ecx+edx], 0                   // if so, zero out
@normal:

  fld   [ecx+edx].Single;                        // load sample from fBuffer
  fld   st(0)                                    // duplicate on the stack
  fmul  [eax].fDampB                             // multiply with fDampB
  fld   [eax].fFilterStore;                      // load stored filtered sample
  fmul  [eax].fDampA                             // multiply with fDampA
  faddp
  fst   [eax].fFilterStore                       // store it back

  // This checks for very small values that can cause a Processor
  // to switch in extra precision mode, which is expensive.
  // Since such small values are irrelevant to audio, avoid this.
  // This is the same spot where the original C macro appears
  test  dword ptr [eax].fFilterStore, $7F800000  // test if denormal
  jnz   @Normal2
  mov   dword ptr [eax].fFilterStore, 0          // if so, zero out
@normal2:

  fmul  [eax].fFeedback                          // multiply with fFeedback
  fadd  input                                    // and add to input sample
  fstp  [ecx+edx].Single                         // store at current fBuffer pos
  add   edx, 4                                   // Update fBuffer index
  cmp   edx, [eax].fBufferSize;                  // end of fBuffer reached?
  jb    @OK
  xor   edx, edx                                 // if so, reset Buffer index
@OK:
  mov  [eax].fBufferIndex, edx                   // and store new index.
                                                 // result already in st(0),
                                                 // hence duplicate
end;

end.
