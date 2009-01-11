unit DAV_DspPolyphaseUpsampler;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_DspPolyphaseFilter;

type
  TProcessSample32 = procedure (const Input : Single; out Output : TDAV2SingleArray) of object;
  TProcessSample64 = procedure (const Input : Double; out Output : TDAV2DoubleArray) of object;

  TCustomPolyphaseDownsampler = class(TCustomPolyphaseFilter);

  TPolyphaseUpsampler32 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY           : PDAVSingleFixedArray;
    FStateStack      : PDAVSingleFixedArray;
    FProcessSample32 : TProcessSample32;
    procedure ProcessSample1(const Input : Single; out Output : TDAV2SingleArray);
    procedure ProcessSample2(const Input : Single; out Output : TDAV2SingleArray);
    procedure ProcessSample3(const Input : Single; out Output : TDAV2SingleArray);
    procedure ProcessSample4(const Input : Single; out Output : TDAV2SingleArray);
    procedure ProcessSampleOdd(const Input : Single; out Output : TDAV2SingleArray);
    procedure ProcessSampleEven(const Input : Single; out Output : TDAV2SingleArray);
  protected
    procedure SetProcedures; override;
    procedure NumberOfCoeffsChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; const SampleFrames: Integer);
    procedure ClearBuffers;
    procedure PushStates;
    procedure PopStates;
    procedure ResetStates;

    property ProcessSample: TProcessSample32 read FProcessSample32;
  end;

  TPolyphaseUpsampler64 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY              : PDAVDoubleFixedArray;
    FStateStack         : PDAVDoubleFixedArray;
    FProcessSample64    : TProcessSample64;
    procedure ProcessSample1(const Input : Double; out Output : TDAV2DoubleArray);
    procedure ProcessSample2(const Input : Double; out Output : TDAV2DoubleArray);
    procedure ProcessSample3(const Input : Double; out Output : TDAV2DoubleArray);
    procedure ProcessSample4(const Input : Double; out Output : TDAV2DoubleArray);
    procedure ProcessSampleLarge(const Input : Double; out Output : TDAV2DoubleArray);
  protected
    procedure SetProcedures; override;
    procedure NumberOfCoeffsChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output: PDAVDoubleFixedArray; const SampleFrames: Integer);
    procedure ClearBuffers;
    procedure PushStates;
    procedure PopStates;
    procedure ResetStates;

    property ProcessSample: TProcessSample64 read FProcessSample64;
  end;

implementation

/////////////////////////////// Constructor //////////////////////////////////

const
  CHalf32: Single = 0.5;
  CHalf64: Double = 0.5;

constructor TPolyphaseUpsampler32.Create;
begin
 inherited;
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
end;

destructor TPolyphaseUpsampler32.Destroy;
begin
 Dispose(FX);
 Dispose(FY);
 Dispose(FStateStack);
 inherited;
end;

procedure TPolyphaseUpsampler32.SetProcedures;
begin
 case FNumberOfCoeffs of
    1: FProcessSample32 := ProcessSample1;
    2: FProcessSample32 := ProcessSample2;
    3: FProcessSample32 := ProcessSample3;
    4: FProcessSample32 := ProcessSample4;
  else
  if FNumberOfCoeffs mod 2 <> 0
   then FProcessSample32 := ProcessSampleOdd
   else FProcessSample32 := ProcessSampleEven;
 end;
end;

procedure TPolyphaseUpsampler32.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FX, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FY, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Single));
 SetProcedures;
 ClearBuffers;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: PushStates                                                          //
//  ----------------                                                          //
//                                                                            //
//  Description:                                                              //
//    Pushes the states (X and Y) to the state stack. Currently only one      //
//    combination of push/pop is allowed.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler32.PushStates;
begin
 Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Single));
 Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Single));
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: PopStates                                                           //
//  ---------------                                                           //
//                                                                            //
//  Description:                                                              //
//    Pops the states (X and Y) to the state stack. Currently only one        //
//    combination of push/pop is allowed.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler32.PopStates;
begin
 Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Single));
 Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Single));
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

procedure TPolyphaseUpsampler32.ClearBuffers;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Single), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ResetStates                                                         //
//  -----------------                                                         //
//                                                                            //
//  Description:                                                              //
//    Identical to ClearBuffers (see above).                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler32.ResetStates;
begin
  FillChar(FX[0], FNumberOfCoeffs * SizeOf(Single), 0);
  FillChar(FY[0], FNumberOfCoeffs * SizeOf(Single), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessBlock                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Downsamples (x2) a block of samples.                                    //
//    Input and output blocks may overlap, see assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler32.ProcessBlock(const Input, Output : PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Pos : Integer;
begin
 for Pos := 0 to SampleFrames - 1
  do FProcessSample32(Input[pos], PDAV2SingleArray(@Output[pos * 2])^);
end;


procedure TPolyphaseUpsampler32.ProcessSample1(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0];
 FX[0] := Input;
 Output[0] := FY[0];
 Output[1] := Input;
end;
{$ELSE}
asm
 push edi
 mov  edi, [self.FY]                  // edi = FY
 mov  ecx, [self.FX]                  // esi = FX
 mov  eax, [self.fCoefficients]       // ecx = fCoefficients
 fld  [ecx].Single                    // FX[0]
 fld  Input.Single                    // Input, FX[0]
 fst  [Output + 4].Single             // Output[1] := Input;
 fst  [ecx].Single                    // FX[0] := Input;
 fsub [edi].Single                    // (Input - FY[0])
 fmul [eax].Double                    // (Input - FY[0]) * fCoefficients[0]
 faddp                                // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fst  [edi].Single                    // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [Output].Single                 // Output[1] := FY[3];
 pop edi
end;
{$ENDIF}

procedure TPolyphaseUpsampler32.ProcessSample2(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 Output[0] := FY[0];
 Output[1] := FY[1];
end;
{$ELSE}
asm
 push edi
 mov edi, [self.FY]                  // edi = FY
 mov ecx, [self.FX]                  // esi = FX
 mov eax, [self.fCoefficients]       // ecx = fCoefficients

 fld  [ecx].Single                   // FX[0]
 fld  Input.Single                   // Input, FX[0]
 fst  [ecx].Single                   // FX[0] := Input;
 fsub [edi].Single                   // (Input - FY[0])
 fmul [eax].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fst  [edi].Single                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [Output].Single                // Output[1] := FY[3];

 fld  [ecx + 4].Single               // FX[1]
 fld  Input.Single                   // Input, FX[1]
 fst  [ecx + 4].Single               // FX[1] := Input;
 fsub [edi + 4].Single               // (Input - FY[1])
 fmul [eax + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fst  [edi + 4].Single               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [Output + 4].Single            // Output[1] := FY[1];

 pop edi
end;
{$ENDIF}

procedure TPolyphaseUpsampler32.ProcessSample3(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]; FX[2] := FY[0];
 Output[1] := FY[1];
 Output[0] := FY[2];
end;
{$ELSE}
asm
 push edi
 mov edi, [self.FY]                  // edi = FY
 mov ecx, [self.FX]                  // esi = FX
 mov eax, [self.fCoefficients]       // ecx = fCoefficients

 fld [ecx].Single                    // FX[0]
 fld Input.Single                    // Input, FX[0]
 fst  [ecx].Single                   // FX[0] := Input;
 fsub [edi].Single                   // (Input - FY[0])
 fmul [eax].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Single                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld [ecx + 4].Single                // FX[1]
 fld Input.Single                    // Input, FX[1]
 fst  [ecx + 4].Single               // FX[1] := Input;
 fsub [edi + 4].Single               // (Input - FY[1])
 fmul [eax + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fst  [edi + 4].Single               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [Output + 4].Single            // Output[1] := FY[1];

 fld  [ecx +  8].Single              // FX[2]
 fld  [edi].Single                   // FY[0], FX[2]
 fst  [ecx +  8].Single              // FX[2] := FY[0];
 fsub [edi +  8].Single              // (FY[0] - FY[2])
 fmul [eax + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fst  [edi +  8].Single              // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Single                // Output[0] := FY[2];

 pop edi
end;
{$ENDIF}

procedure TPolyphaseUpsampler32.ProcessSample4(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]; FX[2] := FY[0];
 FY[3] := (FY[1] - FY[3]) * fCoefficients[3] + FX[3]; FX[3] := FY[1];
 Output[0] := FY[2];
 Output[1] := FY[3];
end;
{$ELSE}
asm
 push edi
 mov edi, [self.FY]                  // edi = FY
 mov ecx, [self.FX]                  // esi = FX
 mov eax, [self.fCoefficients]       // ecx = fCoefficients

 fld  [ecx].Single                   // FX[0]
 fld  Input.Single                   // Input, FX[0]
 fst  [ecx].Single                   // FX[0] := Input;
 fsub [edi].Single                   // (Input - FY[0])
 fmul [eax].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Single                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld  [ecx + 4].Single               // FX[1]
 fld  Input.Single                   // Input, FX[1]
 fst  [ecx + 4].Single               // FX[1] := Input;
 fsub [edi + 4].Single               // (Input - FY[1])
 fmul [eax + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [edi + 4].Single               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]

 fld  [ecx +  8].Single              // FX[2]
 fld  [edi].Single                   // FY[0], FX[2]
 fst  [ecx +  8].Single              // FX[2] := FY[0];
 fsub [edi +  8].Single              // (FY[0] - FY[2])
 fmul [eax + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fst  [edi + 8].Single               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Single                // Output[0] := FY[2];

 fld  [ecx + 12].Single              // FX[2], FY[2]
 fld  [edi +  4].Single              // FY[0], FX[2], FY[2]
 fst  [ecx + 12].Single              // FX[2] := FY[0];
 fsub [edi + 12].Single              // (FY[0] - FY[2]), FY[2]
 fmul [eax + 24].Double              // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst  [edi + 12].Single              // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output + 4].Single            // Output[1] := FY[3];

 pop edi
end;
{$ENDIF}

procedure TPolyphaseUpsampler32.ProcessSampleOdd(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 PDav2SingleArray(FY)^[1] := (Input - PDav2SingleArray(FY)^[1]) * PDav2DoubleArray(fCoefficients)^[1] + PDav2SingleArray(FX)^[1];
 PDav2SingleArray(FX)^[1] := Input;

 for i := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[ i] := (FY[i - 2] - FY[i]) * fCoefficients[i] + FX[i];
   FX[ i] :=  FY[i - 2];
  end;
 Output[1] := FY[FNumberOfCoeffs - 2];
 Output[0] := FY[FNumberOfCoeffs - 1];
end;
{$ELSE}
asm
 pushad
 mov  esi, [self.FX]                  // esi = FX
 mov  edi, [self.FY]                  // edi = FY
 mov  ebx, [self.fCoefficients]       // ebx = fCoefficients

 fld  [esi].Single                    // FX[0]
 fld  Input.Single                    // Input, FX[0]
 fst  [esi].Single                    // FX[0] := Input;
 fsub [edi].Single                    // (Input - FY[0])
 fmul [ebx].Double                    // (Input - FY[0]) * fCoefficients[0]
 faddp                                // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Single                    // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld  [esi + 4].Single                // FX[1]
 fld  Input.Single                    // Input, FX[1]
 fst  [esi + 4].Single                // FX[1] := Input;
 fsub [edi + 4].Single                // (Input - FY[1])
 fmul [ebx + 8].Double                // (Input - FY[1]) * fCoefficients[1]
 faddp                                // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [edi + 4].Single                // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]

 push ecx                             // The Saviour of ECX
 mov  ecx,[self.FNumberOfCoeffs]      // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                          // "Den Rest mach ich selber"
@Loopy:
 fld  [esi +  8].Single               // FX[2], FY[2]
 fld  [edi].Single                    // FY[0], FX[2], FY[2]
 fst  [esi +  8].Single               // FX[2] := FY[0];
 fsub [edi +  8].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fstp [edi +  8].Single               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 add  esi, 4
 add  edi, 4
 add  ebx, 8                          // Weiter geht's
 loop @Loopy
 pop ecx                              // ecx hat ausgedient!

 fld  [esi +  8].Single               // FX[2], FY[2]
 fld  [edi].Single                    // FY[0], FX[2], FY[2]
 fst  [esi +  8].Single               // FX[2] := FY[0];
 fsub [edi +  8].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst  [edi +  8].Single               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output + 4].Single             // Output[0] := FY[2];

 fld  [esi + 12].Single               // FX[2], FY[2]
 fld  [edi +  4].Single               // FY[0], FX[2], FY[2]
 fst  [esi + 12].Single               // FX[2] := FY[0];
 fsub [edi + 12].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 24].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst [edi + 12].Single                // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Single                 // Output[1] := FY[3];

 popad
end;
{$ENDIF}

procedure TPolyphaseUpsampler32.ProcessSampleEven(const Input : Single; out Output : TDAV2SingleArray);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 PDav2SingleArray(FY)^[1] := (Input - PDav2SingleArray(FY)^[1]) * PDav2DoubleArray(fCoefficients)^[1] + PDav2SingleArray(FX)^[1];
 PDav2SingleArray(FX)^[1] := Input;

 for i := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[ i] := (FY[i - 2] - FY[i]) * fCoefficients[i] + FX[i];
   FX[ i] :=  FY[i - 2];
  end;
 i := FNumberOfCoeffs and 1;
 Output[0] := FY[FNumberOfCoeffs - 2];
 Output[1] := FY[FNumberOfCoeffs - 1];
end;
{$ELSE}
asm
 pushad
 mov  esi, [self.FX]                  // esi = FX
 mov  edi, [self.FY]                  // edi = FY
 mov  ebx, [self.fCoefficients]       // ebx = fCoefficients

 fld  [esi].Single                    // FX[0]
 fld  Input.Single                    // Input, FX[0]
 fst  [esi].Single                    // FX[0] := Input;
 fsub [edi].Single                    // (Input - FY[0])
 fmul [ebx].Double                    // (Input - FY[0]) * fCoefficients[0]
 faddp                                // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Single                    // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld  [esi + 4].Single                // FX[1]
 fld  Input.Single                    // Input, FX[1]
 fst  [esi + 4].Single                // FX[1] := Input;
 fsub [edi + 4].Single                // (Input - FY[1])
 fmul [ebx + 8].Double                // (Input - FY[1]) * fCoefficients[1]
 faddp                                // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [edi + 4].Single                // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]

 push ecx                             // The Saviour of ECX
 mov  ecx,[self.FNumberOfCoeffs]      // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                          // "Den Rest mach ich selber"
@Loopy:
 fld  [esi +  8].Single               // FX[2], FY[2]
 fld  [edi].Single                    // FY[0], FX[2], FY[2]
 fst  [esi +  8].Single               // FX[2] := FY[0];
 fsub [edi +  8].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fstp [edi +  8].Single               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 add  esi, 4
 add  edi, 4
 add  ebx, 8                          // Weiter geht's
 loop @Loopy
 pop ecx                              // ecx hat ausgedient!

 fld  [esi +  8].Single               // FX[2], FY[2]
 fld  [edi].Single                    // FY[0], FX[2], FY[2]
 fst  [esi +  8].Single               // FX[2] := FY[0];
 fsub [edi +  8].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst  [edi +  8].Single               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Single                 // Output[0] := FY[2];

 fld  [esi + 12].Single               // FX[2], FY[2]
 fld  [edi +  4].Single               // FY[0], FX[2], FY[2]
 fst  [esi + 12].Single               // FX[2] := FY[0];
 fsub [edi + 12].Single               // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 24].Double               // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                                // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst [edi + 12].Single                // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output + 4].Single             // Output[1] := FY[3];

 popad
end;
{$ENDIF}

{ TPolyphaseUpsampler64 }

constructor TPolyphaseUpsampler64.Create;
begin
 inherited;
 FX          := nil;
 FY          := nil;
 FStateStack := nil;
end;

destructor TPolyphaseUpsampler64.Destroy;
begin
 Dispose(FX);
 Dispose(FY);
 Dispose(FStateStack);
 inherited;
end;

procedure TPolyphaseUpsampler64.SetProcedures;
begin
 case FNumberOfCoeffs of
    1: FProcessSample64 := ProcessSample1;
    2: FProcessSample64 := ProcessSample2;
    3: FProcessSample64 := ProcessSample3;
    4: FProcessSample64 := ProcessSample4;
  else FProcessSample64 := ProcessSampleLarge;
 end;
end;

procedure TPolyphaseUpsampler64.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FX, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FY, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Double));
 SetProcedures;
 ClearBuffers;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: PushStates                                                          //
//  ----------------                                                          //
//                                                                            //
//  Description:                                                              //
//    Pushes the states (X and Y) to the state stack. Currently only one      //
//    combination of push/pop is allowed.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler64.PushStates;
begin
 Move(FX[0], FStateStack[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FY[0], FStateStack[FNumberOfCoeffs], FNumberOfCoeffs * SizeOf(Double));
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: PopStates                                                           //
//  ---------------                                                           //
//                                                                            //
//  Description:                                                              //
//    Pops the states (X and Y) to the state stack. Currently only one        //
//    combination of push/pop is allowed.                                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler64.PopStates;
begin
 Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Double));
 Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Double));
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

procedure TPolyphaseUpsampler64.ClearBuffers;
begin
 FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
 FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ResetStates                                                         //
//  -----------------                                                         //
//                                                                            //
//  Description:                                                              //
//    Identical to ClearBuffers (see above).                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler64.ResetStates;
begin
  FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
  FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessBlock                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Downsamples (x2) a block of samples.                                    //
//    Input and output blocks may overlap, see assert() for details.          //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: Input array, containing SampleFrames * 2 samples.              //
//    - SampleFrames: Number of samples to output, > 0                        //
//                                                                            //
//  Output parameters:                                                        //
//    - Output: Array for the output samples, capacity: SampleFrames samples. //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseUpsampler64.ProcessBlock(const Input, Output : PDAVDoubleFixedArray; const SampleFrames: Integer);
var
  Pos : Integer;
begin
 for Pos := 0 to SampleFrames - 1
  do FProcessSample64(Input[pos], PDAV2DoubleArray(@Output[pos * 2])^);
end;

procedure TPolyphaseUpsampler64.ProcessSample1(const Input : Double; out Output : TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 Output[0] := FY[0];
 Output[1] := Input;
end;
{$ELSE}
asm
 pushad
 mov esi, [self.FX]                  // esi = FX
 mov edi, [self.FY]                  // edi = FY
 mov ebx, [self.fCoefficients]       // ecx = fCoefficients
 fld [esi].Double                    // FX[0]
 fld Input.Double                    // Input, FX[0]
 fst [Output + 4].Single             // Output[1] := Input;
 fst  [esi].Double                   // FX[0] := Input;
 fsub [edi].Double                   // (Input - FY[0])
 fmul [ebx].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fst  [edi].Double                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [Output].Double                // Output[1] := FY[3];
 popad
end;
{$ENDIF}

procedure TPolyphaseUpsampler64.ProcessSample2(const Input : Double; out Output : TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 Output[0] := FY[0];
 Output[1] := FY[1];
end;
{$ELSE}
asm
 pushad
 mov esi, [self.FX]                  // esi = FX
 mov edi, [self.FY]                  // edi = FY
 mov ebx, [self.fCoefficients]       // ecx = fCoefficients

 fld [esi].Double                    // FX[0]
 fld Input.Double                    // Input, FX[0]
 fst  [esi].Double                   // FX[0] := Input;
 fsub [edi].Double                   // (Input - FY[0])
 fmul [ebx].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fst  [edi].Double                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [Output].Double                // Output[1] := FY[3];

 fld [esi + 8].Double                // FX[1]
 fld Input.Single                    // Input, FX[1]
 fst  [esi + 8].Double               // FX[1] := Input;
 fsub [edi + 8].Double               // (Input - FY[1])
 fmul [ebx + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fst  [edi + 8].Double               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [Output + 8].Double            // Output[1] := FY[1];

 popad
end;
{$ENDIF}

procedure TPolyphaseUpsampler64.ProcessSample3(const Input : Double; out Output : TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]; FX[2] := FY[0];
 Output[1] := FY[1];
 Output[0] := FY[2];
end;
{$ELSE}
asm
 pushad
 mov esi, [self.FX]                  // esi = FX
 mov edi, [self.FY]                  // edi = FY
 mov ebx, [self.fCoefficients]       // ecx = fCoefficients

 fld [esi].Double                    // FX[0]
 fld Input.Double                    // Input, FX[0]
 fst  [esi].Double                   // FX[0] := Input;
 fsub [edi].Double                   // (Input - FY[0])
 fmul [ebx].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Double                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld [esi + 8].Double                // FX[1]
 fld Input.Single                    // Input, FX[1]
 fst  [esi + 8].Double               // FX[1] := Input;
 fsub [edi + 8].Double               // (Input - FY[1])
 fmul [ebx + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fst  [edi + 8].Double               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [Output + 8].Double            // Output[1] := FY[1];

 fld [esi + 16].Double               // FX[2]
 fld [edi].Double                    // FY[0], FX[2]
 fst  [esi + 16].Double              // FX[2] := FY[0];
 fsub [edi + 16].Double              // (FY[0] - FY[2])
 fmul [ebx + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fst [edi + 16].Double               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Double                // Output[0] := FY[2];

 popad
end;
{$ENDIF}

procedure TPolyphaseUpsampler64.ProcessSample4(const Input : Double; out Output : TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
begin
 FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]; FX[0] := Input;
 FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]; FX[1] := Input;
 FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]; FX[2] := FY[0];
 FY[3] := (FY[1] - FY[3]) * fCoefficients[3] + FX[3]; FX[3] := FY[1];
 Output[0] := FY[2];
 Output[1] := FY[3];
end;
{$ELSE}
asm
 pushad
 mov esi, [self.FX]                  // esi = FX
 mov edi, [self.FY]                  // edi = FY
 mov ebx, [self.fCoefficients]       // ecx = fCoefficients

 fld [esi].Double                    // FX[0]
 fld Input.Double                    // Input, FX[0]
 fst  [esi].Double                   // FX[0] := Input;
 fsub [edi].Double                   // (Input - FY[0])
 fmul [ebx].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Double                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld [esi + 8].Double                // FX[1]
 fld Input.Double                    // Input, FX[1]
 fst  [esi + 8].Double               // FX[1] := Input;
 fsub [edi + 8].Double               // (Input - FY[1])
 fmul [ebx + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [edi + 8].Double               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]

 fld [esi + 16].Double               // FX[2]
 fld [edi].Double                    // FY[0], FX[2]
 fst  [esi + 16].Double              // FX[2] := FY[0];
 fsub [edi + 16].Double              // (FY[0] - FY[2])
 fmul [ebx + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fst [edi + 16].Double               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Double                // Output[0] := FY[2];

 fld [esi + 24].Double               // FX[2], FY[2]
 fld [edi + 8].Double                // FY[0], FX[2], FY[2]
 fst  [esi + 24].Double              // FX[2] := FY[0];
 fsub [edi + 24].Double              // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 24].Double              // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst [edi + 24].Double               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output + 8].Double            // Output[1] := FY[3];

 popad
end;
{$ENDIF}

procedure TPolyphaseUpsampler64.ProcessSampleLarge(const Input : Double; out Output : TDAV2DoubleArray);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 FY[ 0] := (Input  - FY[ 0]) * fCoefficients[ 0] + FX[ 0]; FX[ 0] := Input;
 FY[ 1] := (Input  - FY[ 1]) * fCoefficients[ 1] + FX[ 1]; FX[ 1] := Input;

 for i := 2 to FNumberOfCoeffs-1 do
  begin
   FY[ i] := (FY[i-2] - FY[i]) * fCoefficients[i] + FX[i];
   FX[ i] :=  FY[i-2];
  end;
 Output[0] := FY[FNumberOfCoeffs-2];
 Output[1] := FY[FNumberOfCoeffs-1];
end;
{$ELSE}
asm
 pushad
 mov esi, [self.FX]                  // esi = FX
 mov edi, [self.FY]                  // edi = FY
 mov ebx, [self.fCoefficients]       // ecx = fCoefficients

 fld [esi].Double                    // FX[0]
 fld Input.Double                    // Input, FX[0]
 fst  [esi].Double                   // FX[0] := Input;
 fsub [edi].Double                   // (Input - FY[0])
 fmul [ebx].Double                   // (Input - FY[0]) * fCoefficients[0]
 faddp                               // (Input - FY[0]) * fCoefficients[0] + FX[0]
 fstp [edi].Double                   // FY[0] := (Input - FY[0]) * fCoefficients[0] + FX[0]

 fld [esi + 8].Double                // FX[1]
 fld Input.Double                    // Input, FX[1]
 fst  [esi + 8].Double               // FX[1] := Input;
 fsub [edi + 8].Double               // (Input - FY[1])
 fmul [ebx + 8].Double               // (Input - FY[1]) * fCoefficients[1]
 faddp                               // (Input - FY[1]) * fCoefficients[1] + FX[1]
 fstp [edi + 8].Double               // FY[1] := (Input - FY[1]) * fCoefficients[1] + FX[1]

 push ecx                            // The Saviour of ECX
 mov ecx,[self.FNumberOfCoeffs]      // ECX=self.FNumberOfCoeffs
 sub ecx, 4                          // "Den Rest mach ich selber"
@Loopy:
 fld [esi + 16].Double               // FX[2], FY[2]
 fld [edi].Double                    // FY[0], FX[2], FY[2]
 fst  [esi + 16].Double              // FX[2] := FY[0];
 fsub [edi + 16].Double              // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fstp [edi + 16].Double              // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 add esi,8
 add edi,8
 add ebx,8                           // Weiter geht's
 loop @Loopy
 pop ecx                             // ecx hat ausgedient!

 fld [esi + 16].Double               // FX[2], FY[2]
 fld [edi].Double                    // FY[0], FX[2], FY[2]
 fst  [esi + 16].Double              // FX[2] := FY[0];
 fsub [edi + 16].Double              // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 16].Double              // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst [edi + 16].Double               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output].Double                // Output[0] := FY[2];

 fld [esi + 24].Double               // FX[2], FY[2]
 fld [edi + 8].Double                // FY[0], FX[2], FY[2]
 fst  [esi + 24].Double              // FX[2] := FY[0];
 fsub [edi + 24].Double              // (FY[0] - FY[2]), FY[2]
 fmul [ebx + 24].Double              // (FY[0] - FY[2]) * fCoefficients[2], FY[2]
 faddp                               // (FY[0] - FY[2]) * fCoefficients[2] + FX[2], FY[2]
 fst [edi + 24].Double               // FY[2] := (FY[0] - FY[2]) * fCoefficients[2] + FX[2]
 fstp [Output + 8].Double            // Output[1] := FY[3];

 popad
end;
{$ENDIF}

end.
