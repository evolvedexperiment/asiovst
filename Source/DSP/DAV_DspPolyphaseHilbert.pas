unit DAV_DspPolyphaseHilbert;

{$I ..\ASIOVST.INC}

interface

uses
  DAV_Common, DAV_DspPolyphaseFilter;

type
  TPhaseMem32 = array [0..1] of
    record
      X : PDAVSingleFixedArray;
      Y : PDAVSingleFixedArray;
    end;
  TPhaseMem64 = array [0..1] of
    record
      X : PDAVDoubleFixedArray;
      Y : PDAVDoubleFixedArray;
    end;
  TProcessHilbertSample32 = procedure(const Input: Single; out OutputA, OutputB: Single) of object;
  TProcessEnvelopeSample32 = function(const Input: Single): Single of object;
  TProcessHilbertSample64 = procedure(const Input: Double; out OutputA, OutputB: Double) of object;
  TProcessEnvelopeSample64 = function(const Input: Double): Double of object;

  TCustomPhaseHalfPi = class(TCustomPolyphaseFilter);

  TPhaseHalfPi32 = class(TCustomPhaseHalfPi)
  private
    FPHilbertSample32 : TProcessHilbertSample32;
    FPEnvSample32     : TProcessEnvelopeSample32;
    FPrev             : Single;
    FPhase            : Integer; 
    FMem              : TPhaseMem32;
  protected
    procedure SetProcedures; override;
    procedure NumberOfCoeffsChanged; override;
    procedure ProcessSampleLarge(const Input: Single;  out OutputA, OutputB: Single); overload;
    procedure ProcessSample1(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample2(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample3(const Input: Single; out OutputA, OutputB: Single); overload;
    procedure ProcessSample4(const Input: Single; out OutputA, OutputB: Single); overload;
    function ProcessSampleLarge(const Input: Single): Single; overload;
    function ProcessSample1(const Input: Single): Single; overload;
    function ProcessSample2(const Input: Single): Single; overload;
    function ProcessSample3(const Input: Single): Single; overload;
    function ProcessSample4(const Input: Single): Single; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, OutputA, OutputB: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;

    property ProcessHilbertSample: TProcessHilbertSample32 read FPHilbertSample32;
    property ProcessEnvelopeSample: TProcessEnvelopeSample32 read FPEnvSample32;
  end;

  TPhaseHalfPi64 = class(TCustomPhaseHalfPi)
  private
    FPHilbertSample64 : TProcessHilbertSample64;
    FPEnvSample64     : TProcessEnvelopeSample64;
    FPrev             : Double;
    FPhase            : Integer;      // 0 or 1
    FMem              : TPhaseMem64;
  protected
    procedure SetProcedures; override;
    procedure NumberOfCoeffsChanged; override;
    procedure ProcessSampleLarge(const Input: Double;  out OutputA, OutputB: Double); overload;
    procedure ProcessSample1(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample2(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample3(const Input: Double; out OutputA, OutputB: Double); overload;
    procedure ProcessSample4(const Input: Double; out OutputA, OutputB: Double); overload;
    function ProcessSampleLarge(const Input: Double): Double; overload;
    function ProcessSample1(const Input: Double): Double; overload;
    function ProcessSample2(const Input: Double): Double; overload;
    function ProcessSample3(const Input: Double): Double; overload;
    function ProcessSample4(const Input: Double): Double; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, OutputA, OutputB: PDAVDoubleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;

    property ProcessHilbertSample: TProcessHilbertSample64 read FPHilbertSample64;
    property ProcessEnvelopeSample: TProcessEnvelopeSample64 read FPEnvSample64;
  end;

implementation

constructor TPhaseHalfPi32.Create;
begin
  inherited;
  FMem[0].X := nil;
  FMem[0].Y := nil;
  FMem[1].X := nil;
  FMem[1].Y := nil;
  NumberOfCoeffsChanged;
end;

destructor TPhaseHalfPi32.Destroy;
begin
  Dispose(FMem[0].X);
  Dispose(FMem[0].Y);
  Dispose(FMem[1].X);
  Dispose(FMem[1].Y);
  inherited;
end;

procedure TPhaseHalfPi32.SetProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FPHilbertSample32 := ProcessSample1;
      FPEnvSample32 := ProcessSample1;
     end;
    2 :
     begin
      FPHilbertSample32 := ProcessSample2;
      FPEnvSample32 := ProcessSample2;
     end;
    3 :
     begin
      FPHilbertSample32 := ProcessSample3;
      FPEnvSample32 := ProcessSample3;
     end;
    4 :
     begin
      FPHilbertSample32 := ProcessSample4;
      FPEnvSample32 := ProcessSample4;
     end;
  else
   begin
    FPHilbertSample32 := ProcessSampleLarge;
    FPEnvSample32 := ProcessSampleLarge;
   end;
   end;
end;

procedure TPhaseHalfPi32.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FMem[0].X, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[0].Y, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[1].X, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FMem[1].Y, FNumberOfCoeffs * SizeOf(Single));
 SetProcedures;
 ClearBuffers;
end;


procedure TPhaseHalfPi32.ProcessBlock(
  const Input, OutputA, OutputB: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  assert(SampleFrames > 0);
  Pos := 0;
  repeat
    ProcessHilbertSample(Input[pos], OutputA[Pos], OutputB[Pos]);
    Inc(Pos);
  until (pos >= SampleFrames);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ClearBuffers                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Clears filter memory, as if it processed silence since an infinite      //
//    amount of time.                                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPhaseHalfPi32.ClearBuffers;
begin
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;


procedure TPhaseHalfPi32.ProcessSample1(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [self + FMem[ebx]]     // edi = X[0]
 mov esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single               // input
 fld  [edi].Single               // X[0],input
 fld  Input.Single               // input,X[0],input
 fadd CDenorm32
 fst  [edi].Single               // FMem[FPhase].X[0] := input;
 fadd [esi].Single               // input + Y[0], X[0], input
 mov ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0]
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0]
 fst [esi].Single                // FMem[FPhase].Y[0] := "
 fstp OutputA.Single             // OutputA := FMem[FPhase].Y[0];
 fld [self.FPrev].Single         // FPrev, input
 fstp OutputB.Single             // OutputB := FPrev;
 fstp [self.FPrev].Single        // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (input + CDenorm32 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := input;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FPrev;
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample2(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [self + FMem[ebx]]     // edi=X[0]
 mov esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx
 fld CDenorm32                   // Pure Speed

 fld  Input.Single               // input
 fld  [edi].Single               // X[0], input
 fld  Input.Single               // input, X[0],input
 fadd st(0),st(3)                // dEnOrMaL
 fst  [edi].Single               // FMem[FPhase].X[0] := input;
 fadd [esi].Single               // input + Y[0], X[0], input
 mov ebx,[self.fCoefficients]    // ebx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0], input
 fst [esi].Single                // FMem[FPhase].Y[0] := "
 fstp OutputA.Single             // OutputA := FMem[FPhase].Y[0];

 fld  [edi + 4].Single           // X[1], input
 fld  [self.FPrev].Single        // FPrev, X[1], input
 fadd st(0), st(3)               // dEnOrMaL
 fst  [edi + 4].Single           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single           // FPrev + Y[1], X[1], input
 mov ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst [esi + 4].Single            // FMem[FPhase].Y[1] := "
 fstp OutputB.Single             // OutputB := FMem[FPhase].Y[1];
 fstp [self.FPrev].Single        // FPrev := Input;

 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm32 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm32 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FMem[FPhase].Y[1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample3(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi = X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx
 fld  CDenorm32                   // Pure Speed

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0], input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fstp OutputA.Single              // OutputB := FMem[FPhase].Y[1];

 fld  [edi +  8].Single           // X[2], input
 fld  [esi     ].Single           // Y[0], X[2], input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fstp OutputB.Single              // OutputB := FMem[FPhase].Y[2];

 fstp [self.FPrev].Single         // FPrev := Input;
 fstp st(0)

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  MemY : PDAV4SingleArray;
  MemX : PDAV4SingleArray;
begin
  MemY  := @FMem[FPhase].Y[0];
  MemX  := @FMem[FPhase].X[0];
  MemY[0] := (Input + CDenorm64 + MemY[0]) * PDAV4DoubleArray(fCoefficients)[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + CDenorm64 + MemY[1]) * PDAV4DoubleArray(fCoefficients)[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(fCoefficients)[2] - MemX[2];           MemX[2] := MemY[0];
  OutputA := MemY[1];
  OutputB := MemY[2];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi32.ProcessSample4(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi = X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx
 fld  CDenorm32                   // Pure Speed

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0], input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 fld  [edi +  8].Single           // X[2], input
 fld  [esi].Single                // Y[0], X[2], input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fstp OutputA.Single              // OutputB := FMem[FPhase].Y[2];

 fld  [edi + 12].Single           // X[3], input
 fld  [esi +  4].Single           // X[1], X[3], input
 fst  [edi + 12].Single           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 12].Single           // FPrev + Y[3], X[3], input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * fCoefficients[3], X[3]
 fsubrp                           // (FPrev + Y[3]) * fCoefficients[3] - X[3]
 fst  [esi + 12].Single           // FMem[FPhase].Y[3] :=  "
 fstp OutputB.Single              // OutputB := FMem[FPhase].Y[3];

 fstp [self.FPrev].Single         // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  MemY : PDAV4SingleArray;
  MemX : PDAV4SingleArray;
begin
  MemY  := @FMem[FPhase].Y[0];
  MemX  := @FMem[FPhase].X[0];
  MemY[0] := (Input + CDenorm64 + MemY[0]) * PDAV4DoubleArray(fCoefficients)^[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + CDenorm64 + MemY[1]) * PDAV4DoubleArray(fCoefficients)^[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(fCoefficients)^[2] - MemX[2]; MemX[2] := MemY[0];
  MemY[3] := (MemY[1] + MemY[3]) * PDAV4DoubleArray(fCoefficients)^[3] - MemX[3]; MemX[3] := MemY[1];
  OutputA := MemY[2];
  OutputB := MemY[3];
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

procedure TPhaseHalfPi32.ProcessSampleLarge(const Input: Single;
  out OutputA, OutputB: Single);
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  CDenorm32                   // Pure Speed!

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // ebx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov ecx, [self.FNumberOfCoeffs]  // ECX=self.FNumberOfCoeffs
 sub ecx, 4                       // "Den Rest mach ich selber"
@Loopy:
 fld  [edi +  8].Single           // X[2], input
 fld  [esi].Single                // Y[0], X[2], input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // FPrev + Y[2], X[2], input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * fCoefficients[2] - X[2]
 fstp [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 add  esi, 4
 add  edi, 4
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop  ecx                         // ecx hat ausgedient!

 fld  [edi + 8].Single            // X[10], input
 fld  [esi].Single                // X[8], X[10], input
 fst  [edi + 8].Single            // FMem[FPhase].X[10] := X[8];
 fadd [esi + 8].Single            // FPrev + Y[10], X[8], input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * fCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * fCoefficients[10] - X[10]
 fst  [esi + 8].Single            // FMem[FPhase].Y[10] :=  "
 fstp [ecx].Single                // OutputB := FMem[FPhase].Y[10];

 fld  [edi + 12].Single           // X[11], input
 fld  [esi +  4].Single           // X[9], X[11], input
 fst  [edi + 12].Single           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 12].Single           // FPrev + Y[11], X[9], input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * fCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * fCoefficients[11] - X[11]
 fst  [esi + 12].Single           // FMem[FPhase].Y[11] :=  "
 fstp [edx].Single                // OutputB := FMem[FPhase].Y[11];

 fstp [self.FPrev].Single         // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * fCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample1(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi = X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd CDenorm32
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0]
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0]
 fst  [esi].Single                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]), Input
 fld  [self.FPrev].Single         // FPrev, sqr(FMem[FPhase].Y[0]), Input
 fmul st(0), st(0)                // sqr(FPrev), sqr(FMem[FPhase].Y[0]), Input
 faddp                            // sqr(FPrev) + sqr(FMem[FPhase].Y[0]), Input
 fsqrt                            // sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0])), Input
 fxch                             // Input, sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0]))
 fstp [self.FPrev].Single         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (input + CDenorm32 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := input;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FPrev));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample2(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi = X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // ebx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fst  [esi].Single                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]);

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);
 faddp                            // sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
 fxch                             // Input, result
 fstp [self.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm32 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm32 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample3(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single                // input
 fld  [edi].Single                // X[0],input
 fld  Input.Single                // input,X[0],input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);

 fld  [edi +  8].Single           // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[2]);
 faddp                            // sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
 fxch                             // Input, result
 fstp [self.FPrev].Single         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) *
    fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) *
    fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    fCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample4(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 mov  ebx, [self.fCoefficients]   // edx=fCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 fld  [edi + 8].Single            // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi + 8].Single            // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 8].Single            // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi + 8].Single            // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[3]);

 fld  [edi + 12].Single           // X[3], input
 fld  [esi + 4].Single            // X[1], X[3], input
 fst  [edi + 12].Single           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 12].Single           // FPrev + Y[3], X[3], input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * fCoefficients[3], X[3]
 fsubrp                           // (FPrev + Y[3]) * fCoefficients[3] - X[3]
 fst  [esi + 12].Single           // FMem[FPhase].Y[3] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[3]);
 faddp                            // sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4])
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4]));
 fxch                             // Input, result
 fstp [self.FPrev].Single         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) *
    fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) *
    fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    fCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) *
    fCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(sqr(FMem[FPhase].Y[2]) + sqr(FMem[FPhase].Y[3]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi32.ProcessSampleLarge(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi = X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Single                // input
 fld  [edi].Single                // X[0], input
 fld  Input.Single                // input, X[0], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi].Single                // FMem[FPhase].X[0] := input;
 fadd [esi].Single                // input + Y[0], X[0], input
 mov  ebx,[self.fCoefficients]    // ebx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], input
 fld  [self.FPrev].Single         // FPrev, X[1], input
 fadd CDenorm32                   // dEnOrMaL
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov  ecx, [self.FNumberOfCoeffs] // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                      // "Den Rest mach ich selber"
@Loopy:
 fld  [edi +  8].Single           // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // FPrev + Y[2], X[2], input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * fCoefficients[2] - X[2]
 fstp [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 add  esi, 4
 add  edi, 4
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop  ecx                         // ecx hat ausgedient!

 fld  [edi + 8].Single            // X[10], input
 fld  [esi].Single                // X[8], X[10], input
 fst  [edi + 8].Single            // FMem[FPhase].X[10] := X[8];
 fadd [esi + 8].Single            // FPrev + Y[10], X[8], input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * fCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * fCoefficients[10] - X[10]
 fst  [esi + 8].Single            // FMem[FPhase].Y[10] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[10]);

 fld  [edi + 12].Single           // X[11], input
 fld  [esi + 4].Single            // X[9], X[11], input
 fst  [edi + 12].Single           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 12].Single           // FPrev + Y[11], X[9], input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * fCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * fCoefficients[11] - X[11]
 fst  [esi + 12].Single           // FMem[FPhase].Y[11] :=  "

 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[11])
 faddp                            // sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]);
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]));
 fxch                             // Input, result
 fstp [self.FPrev].Single         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) *
    fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) *
    fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) *
      fCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}


{ TPhaseHalfPi64 }

constructor TPhaseHalfPi64.Create;
begin
  inherited;
  FMem[0].X := nil;
  FMem[0].Y := nil;
  FMem[1].X := nil;
  FMem[1].Y := nil;
  NumberOfCoeffsChanged;
end;

destructor TPhaseHalfPi64.Destroy;
begin
  Dispose(FMem[0].X);
  Dispose(FMem[0].Y);
  Dispose(FMem[1].X);
  Dispose(FMem[1].Y);
  inherited;
end;

procedure TPhaseHalfPi64.SetProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FPHilbertSample64 := ProcessSample1;
      FPEnvSample64 := ProcessSample1;
     end;
    2 :
     begin
      FPHilbertSample64 := ProcessSample2;
      FPEnvSample64 := ProcessSample2;
     end;
    3 :
     begin
      FPHilbertSample64 := ProcessSample3;
      FPEnvSample64 := ProcessSample3;
     end;
    4 :
     begin
      FPHilbertSample64 := ProcessSample4;
      FPEnvSample64 := ProcessSample4;
     end;
  else
   begin
    FPHilbertSample64 := ProcessSampleLarge;
    FPEnvSample64 := ProcessSampleLarge;
   end;
   end;
end;

procedure TPhaseHalfPi64.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FMem[0].X, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[0].Y, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[1].X, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FMem[1].Y, FNumberOfCoeffs * SizeOf(Double));
 SetProcedures;
 ClearBuffers;
end;


procedure TPhaseHalfPi64.ProcessBlock(
  const Input, OutputA, OutputB: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  assert(SampleFrames > 0);
  Pos := 0;
  repeat
    ProcessHilbertSample(Input[pos], OutputA[Pos], OutputB[Pos]);
    Inc(Pos);
  until (pos >= SampleFrames);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ClearBuffers                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Clears filter memory, as if it processed silence since an infinite      //
//    amount of time.                                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPhaseHalfPi64.ClearBuffers;
begin
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].X[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FMem[0].Y[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;


procedure TPhaseHalfPi64.ProcessSample1(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [self + FMem[ebx]]     // edi = X[0]
 mov esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Double               // input
 fld  [edi].Double               // X[0],input
 fld  Input.Double               // input,X[0],input
 fadd CDenorm64
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0]
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0]
 fst [esi].Double                // FMem[FPhase].Y[0] := "
 fstp OutputA.Double             // OutputA := FMem[FPhase].Y[0];
 fld [self.FPrev].Double         // FPrev, input
 fstp OutputB.Double             // OutputB := FPrev;
 fstp [self.FPrev].Double        // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := input;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FPrev;
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample2(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [self + FMem[ebx]]     // edi=X[0]
 mov esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx
 fld CDenorm64                   // Pure Speed

 fld  Input.Double               // input
 fld  [edi].Double               // X[0], input
 fld  Input.Double               // input, X[0],input
 fadd st(0),st(3)                // dEnOrMaL
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov ebx,[self.fCoefficients]    // ebx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0], input
 fst [esi].Double                // FMem[FPhase].Y[0] := "
 fstp OutputA.Double             // OutputA := FMem[FPhase].Y[0];

 fld  [edi + 8].Double           // X[1], input
 fld  [self.FPrev].Double        // FPrev, X[1], input
 fadd st(0), st(3)               // dEnOrMaL
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], input
 mov ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst [esi + 8].Double            // FMem[FPhase].Y[1] := "
 fstp OutputB.Double             // OutputB := FMem[FPhase].Y[1];
 fstp [self.FPrev].Double        // FPrev := Input;

 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  OutputA := FMem[FPhase].Y[0];
  OutputB := FMem[FPhase].Y[1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample3(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [self + FMem[ebx]]     // edi = X[0]
 mov esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase],ebx           // FPhase = ebx
 fld CDenorm64                   // Pure Speed

 fld  Input.Double               // input
 fld  [edi].Double               // X[0], input
 fld  Input.Double               // input, X[0], input
 fadd st(0), st(3)               // dEnOrMaL
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]  // edx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0], input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], input
 fld  [self.FPrev].Double        // FPrev, X[1], input
 fadd st(0), st(3)               // dEnOrMaL
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst  [esi + 8].Double           // FMem[FPhase].Y[1] :=  "
 fstp OutputA.Double             // OutputB := FMem[FPhase].Y[1];

 fld  [edi + 16].Double          // X[2], input
 fld  [esi].Double               // Y[0], X[2], input
 fst  [edi + 16].Double          // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi + 16].Double          // FMem[FPhase].Y[2] :=  "
 fstp OutputB.Double             // OutputB := FMem[FPhase].Y[2];

 fstp [self.FPrev].Double        // FPrev := Input;
 fstp st(0)

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * fCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  OutputA := FMem[FPhase].Y[1];
  OutputB := FMem[FPhase].Y[2];
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

procedure TPhaseHalfPi64.ProcessSample4(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx
 fld  CDenorm64                   // Pure Speed

 fld  Input.Double                // input
 fld  [edi].Double                // X[0], input
 fld  Input.Double                // input, X[0], input
 fst  [edi].Double                // FMem[FPhase].X[0] := input;
 fadd st(0), st(3)                // dEnOrMaL + input, X[0], input
 fadd [esi].Double                // dEnOrMaL + input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // edx = fCoefficients
 fmul [ebx].Double                // (dEnOrMaL + input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                           // (dEnOrMaL + input + Y[0]) * fCoefficients[0] - X[0], input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], input
 fld  [self.FPrev].Double         // FPrev, X[1], input
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd st(0), st(3)                // dEnOrMaL
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 fld  [edi + 16].Double           // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 fstp OutputA.Double              // OutputB := FMem[FPhase].Y[2];

 fld  [edi + 24].Double           // X[3], input
 fld  [esi +  8].Double           // X[1], X[3], input
 fst  [edi + 24].Double           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 24].Double           // FPrev + Y[3], X[3], input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * fCoefficients[3], X[3]
 fsubp                            // (FPrev + Y[3]) * fCoefficients[3] - X[3]
 fst  [esi + 24].Double           // FMem[FPhase].Y[3] :=  "
 fstp OutputB.Double              // OutputB := FMem[FPhase].Y[3];

 fstp [self.FPrev].Double         // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * fCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * fCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  OutputA := FMem[FPhase].Y[2];
  OutputB := FMem[FPhase].Y[3];
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

procedure TPhaseHalfPi64.ProcessSampleLarge(const Input: Double;
  out OutputA, OutputB: Double);
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  CDenorm64                   // Pure Speed!

 fld  Input.Double                // input
 fld  [edi].Double                // X[0], input
 fld  Input.Double                // input, X[0], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi].Double                // FMem[FPhase].X[0] := input;
 fadd [esi].Double                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // ebx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], input
 fld  [self.FPrev].Double         // FPrev, X[1], input
 fadd st(0), st(3)                // dEnOrMaL
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov ecx, [self.FNumberOfCoeffs]  // ECX=self.FNumberOfCoeffs
 sub ecx, 4                       // "Den Rest mach ich selber"
@Loopy:
 fld  [edi + 16].Double           // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // FPrev + Y[2], X[2], input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * fCoefficients[2] - X[2]
 fstp [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 add esi, 8
 add edi, 8
 add ebx, 8                       // Weiter geht's
 loop @Loopy
 pop ecx                          // ecx hat ausgedient!

 fld  [edi + 16].Double            // X[10], input
 fld  [esi].Double                // X[8], X[10], input
 fst  [edi + 16].Double            // FMem[FPhase].X[10] := X[8];
 fadd [esi + 16].Double            // FPrev + Y[10], X[8], input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * fCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * fCoefficients[10] - X[10]
 fst  [esi + 16].Double            // FMem[FPhase].Y[10] :=  "
 fstp [ecx].Double                // OutputB := FMem[FPhase].Y[10];

 fld  [edi + 24].Double           // X[11], input
 fld  [esi +  8].Double           // X[9], X[11], input
 fst  [edi + 24].Double           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 24].Double           // FPrev + Y[11], X[9], input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * fCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * fCoefficients[11] - X[11]
 fst  [esi + 24].Double           // FMem[FPhase].Y[11] :=  "
 fstp [edx].Double                // OutputB := FMem[FPhase].Y[11];

 fstp [self.FPrev].Double         // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * fCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample1(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [self + FMem[ebx]]     // edi = X[0]
 mov esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Double               // input
 fld  [edi].Double               // X[0], input
 fld  Input.Double               // input, X[0], input
 fadd CDenorm64
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]  // edx = fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0]
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0]
 fst  [esi].Double               // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[0]), Input
 fld  [self.FPrev].Double        // FPrev, sqr(FMem[FPhase].Y[0]), Input
 fmul st(0), st(0)               // sqr(FPrev), sqr(FMem[FPhase].Y[0]), Input
 faddp                           // sqr(FPrev) + sqr(FMem[FPhase].Y[0]), Input
 fsqrt                           // sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0])), Input
 fxch                            // Input, sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0]))
 fstp [self.FPrev].Double        // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := input;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FPrev));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample2(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase],ebx           // FPhase = ebx

 fld  Input.Double                // input
 fld  [edi].Double                // X[0],input
 fld  Input.Double                // input,X[0],input
 fadd CDenorm64                   // dEnOrMaL
 fst  [edi].Double                // FMem[FPhase].X[0] := input;
 fadd [esi].Double                // input + Y[0], X[0], input
 mov  ebx,[self.fCoefficients]    // ebx=fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0],input
 fst  [esi].Double                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]);

 fld  [edi + 8].Double            // X[1], input
 fld  [self.FPrev].Double         // FPrev, X[1], input
 fadd CDenorm64                   // dEnOrMaL
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], input
 mov  ebx,[self.fCoefficients]    // edx = fCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst  [esi + 8].Double            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);
 faddp                            // sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
 fxch                             // Input, result
 fstp [self.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) *
    fCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) *
    fCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample3(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [self + FMem[ebx]]     // edi=X[0]
 mov esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Double               // input
 fld  [edi].Double               // X[0],input
 fld  Input.Double               // input,X[0],input
 fadd CDenorm64                  // dEnOrMaL
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov  ebx,[self.fCoefficients]   // edx=fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], input
 fld  [self.FPrev].Double        // FPrev, X[1], input
 fadd CDenorm64                  // dEnOrMaL
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], input
 mov  ebx, [self.fCoefficients]  // edx=fCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fst [esi + 8].Double            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[1]);

 fld [edi + 16].Double           // X[2], input
 fld [esi].Double                // Y[0], X[2], input
 fst [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[2]);
 faddp                           // sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]
 fsqrt                           // result := sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
 fxch                            // Input, result
 fstp [self.FPrev].Double        // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * fCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample4(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [self.FPhase]          // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [self + FMem[ebx]]     // edi = X[0]
 mov esi, [self + FMem[ebx] + 4] // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Double               // input
 fld  [edi].Double               // X[0],input
 fld  Input.Double               // input, X[0], input
 fadd CDenorm64                  // dEnOrMaL
 fst  [edi].Double               // FMem[FPhase].X[0] := input;
 fadd [esi].Double               // input + Y[0], X[0], input
 mov  ebx,[self.fCoefficients]   // edx=fCoefficients
 fmul [ebx].Double               // (input + Y[0]) * fCoefficients[0], X[0],input
 fsubrp                          // (input + Y[0]) * fCoefficients[0] - X[0],input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], input
 fld  [self.FPrev].Double        // FPrev, X[1], input
 fadd CDenorm64                  // dEnOrMaL
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], input
 mov  ebx, [self.fCoefficients]  // edx=fCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 8].Double           // FMem[FPhase].Y[1] :=  "

 fld  [edi + 16].Double          // X[2], input
 fld  [esi].Double               // Y[0], X[2], input
 fst  [edi + 16].Double          // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * fCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * fCoefficients[2] - X[2]
 fst  [esi + 16].Double          // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[3]);

 fld  [edi + 24].Double          // X[3], input
 fld  [esi + 8].Double           // X[1], X[3], input
 fst  [edi + 24].Double          // FMem[FPhase].X[3] := X[1];
 fadd [esi + 24].Double          // FPrev + Y[3], X[3], input
 fmul [ebx + 24].Double          // (FPrev + Y[3]) * fCoefficients[3], X[3]
 fsubrp                          // (FPrev + Y[3]) * fCoefficients[3] - X[3]
 fst  [esi + 24].Double          // FMem[FPhase].Y[3] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[3]);
 faddp                           // sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4])
 fsqrt                           // result := sqrt(sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4]));
 fxch                            // Input, result
 fstp [self.FPrev].Double        // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * fCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * fCoefficients[3] - FMem[FPhase].X[3]; FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(sqr(FMem[FPhase].Y[2]) + sqr(FMem[FPhase].Y[3]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi64.ProcessSampleLarge(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [self.FPhase]          // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [self + FMem[ebx]]     // edi=X[0]
 mov  esi, [self + FMem[ebx] + 4] // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [self.FPhase], ebx          // FPhase = ebx

 fld  Input.Double                // input
 fld  [edi].Double                // X[0], input
 fld  Input.Double                // input, X[0], input
 fadd CDenorm64                   // dEnOrMaL
 fst  [edi].Double                // FMem[FPhase].X[0] := input;
 fadd [esi].Double                // input + Y[0], X[0], input
 mov  ebx, [self.fCoefficients]   // ebx = fCoefficients
 fmul [ebx].Double                // (input + Y[0]) * fCoefficients[0], X[0], input
 fsubrp                           // (input + Y[0]) * fCoefficients[0] - X[0], input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], input
 fld  [self.FPrev].Double         // FPrev, X[1], input
 fadd CDenorm64                   // dEnOrMaL
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * fCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * fCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov  ecx, [self.FNumberOfCoeffs] // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                      // "Den Rest mach ich selber"
@Loopy:
 fld  [edi + 16].Double           // X[2], input
 fld  [esi].Double                // Y[0], X[2], input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // FPrev + Y[2], X[2], input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * fCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * fCoefficients[2] - X[2]
 fstp [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 add  esi, 8
 add  edi, 8
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop ecx                          // ecx hat ausgedient!

 fld  [edi + 16].Double           // X[10], input
 fld  [esi].Double                // X[8], X[10], input
 fst  [edi + 16].Double           // FMem[FPhase].X[10] := X[8];
 fadd [esi + 16].Double           // FPrev + Y[10], X[8], input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * fCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * fCoefficients[10] - X[10]
 fst  [esi + 16].Double           // FMem[FPhase].Y[10] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[10]);

 fld  [edi + 24].Double           // X[11], input
 fld  [esi + 8].Double            // X[9], X[11], input
 fst  [edi + 24].Double           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 24].Double           // FPrev + Y[11], X[9], input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * fCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * fCoefficients[11] - X[11]
 fst  [esi + 24].Double           // FMem[FPhase].Y[11] :=  "

 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[11])
 faddp                            // sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]);
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]));
 fxch                             // Input, result
 fstp [self.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + CDenorm64 + FMem[FPhase].Y[0]) * fCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + CDenorm64 + FMem[FPhase].Y[1]) * fCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * fCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

end.
