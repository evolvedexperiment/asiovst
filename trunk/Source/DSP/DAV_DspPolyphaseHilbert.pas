unit DAV_DspPolyphaseHilbert;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The code is based on the HIIR code by Laurent de Soras, which             //
//  can be found at http://ldesoras.free.fr/prod.html#src_hiir                //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_DspPolyphaseFilter;

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
    procedure ChooseProcedures; override;
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
    procedure ChooseProcedures; override;
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

uses
  DAV_Common;

{$IFDEF HandleDenormals}
var
  CDenorm32 : Single;
  CDenorm64 : Double;
{$ENDIF}

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

procedure TPhaseHalfPi32.ChooseProcedures;
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
 ChooseProcedures;
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
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [eax + FMem[ebx]]      // edi = X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single               // Input
 fld  [edi].Single               // X[0],Input
 fld  Input.Single               // Input,X[0],Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32
 {$ENDIF}
 fst  [edi].Single               // FMem[FPhase].X[0] := Input;
 fadd [esi].Single               // Input + Y[0], X[0], Input
 mov ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0]
 fst [esi].Single                // FMem[FPhase].Y[0] := "
 fstp OutputA.Single             // OutputA := FMem[FPhase].Y[0];
 fld [eax.FPrev].Single          // FPrev, Input
 fstp OutputB.Single             // OutputB := FPrev;
 fstp [eax.FPrev].Single         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
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
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [eax + FMem[ebx]]      // edi=X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld CDenorm32                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Single               // Input
 fld  [edi].Single               // X[0], Input
 fld  Input.Single               // Input, X[0],Input
 {$IFDEF HandleDenormals}
 fadd st(0),st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single               // FMem[FPhase].X[0] := Input;
 fadd [esi].Single               // Input + Y[0], X[0], Input
 mov ebx,[eax.FCoefficients]     // ebx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fst [esi].Single                // FMem[FPhase].Y[0] := "
 fstp OutputA.Single             // OutputA := FMem[FPhase].Y[0];

 fld  [edi + 4].Single           // X[1], Input
 fld  [eax.FPrev].Single         // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)               // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single           // FPrev + Y[1], X[1], Input
 mov ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst [esi + 4].Single            // FMem[FPhase].Y[1] := "
 fstp OutputB.Single             // OutputB := FMem[FPhase].Y[1];
 fstp [eax.FPrev].Single         // FPrev := Input;

 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi = X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld  CDenorm32                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fstp OutputA.Single              // OutputB := FMem[FPhase].Y[1];

 fld  [edi +  8].Single           // X[2], Input
 fld  [esi     ].Single           // Y[0], X[2], Input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fstp OutputB.Single              // OutputB := FMem[FPhase].Y[2];

 fstp [eax.FPrev].Single          // FPrev := Input;
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
  MemY[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[0]) * PDAV4DoubleArray(FCoefficients)[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[1]) * PDAV4DoubleArray(FCoefficients)[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(FCoefficients)[2] - MemX[2];           MemX[2] := MemY[0];
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi = X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld  CDenorm32                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 fld  [edi +  8].Single           // X[2], Input
 fld  [esi].Single                // Y[0], X[2], Input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fstp OutputA.Single              // OutputB := FMem[FPhase].Y[2];

 fld  [edi + 12].Single           // X[3], Input
 fld  [esi +  4].Single           // X[1], X[3], Input
 fst  [edi + 12].Single           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 12].Single           // FPrev + Y[3], X[3], Input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
 fsubrp                           // (FPrev + Y[3]) * FCoefficients[3] - X[3]
 fst  [esi + 12].Single           // FMem[FPhase].Y[3] :=  "
 fstp OutputB.Single              // OutputB := FMem[FPhase].Y[3];

 fstp [eax.FPrev].Single          // FPrev := Input;
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
  MemY[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[0]) * PDAV4DoubleArray(FCoefficients)^[0] - MemX[0]; MemX[0] := Input;
  MemY[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    MemY[1]) * PDAV4DoubleArray(FCoefficients)^[1] - MemX[1]; MemX[1] := FPrev;
  MemY[2] := (MemY[0] + MemY[2]) * PDAV4DoubleArray(FCoefficients)^[2] - MemX[2]; MemX[2] := MemY[0];
  MemY[3] := (MemY[1] + MemY[3]) * PDAV4DoubleArray(FCoefficients)^[3] - MemX[3]; MemX[3] := MemY[1];
  OutputA := MemY[2];
  OutputB := MemY[3];
  FPrev := Input;
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 {$IFDEF HandleDenormals}
 fld  CDenorm32                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // ebx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov ecx, [eax.FNumberOfCoeffs]   // ECX=self.FNumberOfCoeffs
 sub ecx, 4                       // "Den Rest mach ich selber"
@Loopy:
 fld  [edi +  8].Single           // X[2], Input
 fld  [esi].Single                // Y[0], X[2], Input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // FPrev + Y[2], X[2], Input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * FCoefficients[2] - X[2]
 fstp [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 add  esi, 4
 add  edi, 4
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop  ecx                         // ecx hat ausgedient!

 fld  [edi + 8].Single            // X[10], Input
 fld  [esi].Single                // X[8], X[10], Input
 fst  [edi + 8].Single            // FMem[FPhase].X[10] := X[8];
 fadd [esi + 8].Single            // FPrev + Y[10], X[8], Input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * FCoefficients[10] - X[10]
 fst  [esi + 8].Single            // FMem[FPhase].Y[10] :=  "
 fstp [ecx].Single                // OutputB := FMem[FPhase].Y[10];

 fld  [edi + 12].Single           // X[11], Input
 fld  [esi +  4].Single           // X[9], X[11], Input
 fst  [edi + 12].Single           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 12].Single           // FPrev + Y[11], X[9], Input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * FCoefficients[11] - X[11]
 fst  [esi + 12].Single           // FMem[FPhase].Y[11] :=  "
 fstp [edx].Single                // OutputB := FMem[FPhase].Y[11];

 fstp [eax.FPrev].Single          // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample1(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi = X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0]
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0]
 fst  [esi].Single                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]), Input
 fld  [eax.FPrev].Single          // FPrev, sqr(FMem[FPhase].Y[0]), Input
 fmul st(0), st(0)                // sqr(FPrev), sqr(FMem[FPhase].Y[0]), Input
 faddp                            // sqr(FPrev) + sqr(FMem[FPhase].Y[0]), Input
 fsqrt                            // sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0])), Input
 fxch                             // Input, sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0]))
 fstp [eax.FPrev].Single          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi = X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // ebx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fst  [esi].Single                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]);

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);
 faddp                            // sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
 fxch                             // Input, result
 fstp [eax.FPrev].Single          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample3(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0],Input
 fld  Input.Single                // Input,X[0],Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst  [esi + 4].Single            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);

 fld  [edi +  8].Single           // X[2], Input
 fld  [esi].Single                // Y[0], X[2], Input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[2]);
 faddp                            // sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
 fxch                             // Input, result
 fstp [eax.FPrev].Single          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi32.ProcessSample4(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 mov  ebx, [eax.FCoefficients]    // edx=FCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 fld  [edi + 8].Single            // X[2], Input
 fld  [esi].Single                // Y[0], X[2], Input
 fst  [edi + 8].Single            // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 8].Single            // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi + 8].Single            // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[3]);

 fld  [edi + 12].Single           // X[3], Input
 fld  [esi + 4].Single            // X[1], X[3], Input
 fst  [edi + 12].Single           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 12].Single           // FPrev + Y[3], X[3], Input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
 fsubrp                           // (FPrev + Y[3]) * FCoefficients[3] - X[3]
 fst  [esi + 12].Single           // FMem[FPhase].Y[3] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[3]);
 faddp                            // sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4])
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4]));
 fxch                             // Input, result
 fstp [eax.FPrev].Single          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) *
    FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) *
    FCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(sqr(FMem[FPhase].Y[2]) + sqr(FMem[FPhase].Y[3]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi32.ProcessSampleLarge(const Input: Single): Single;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi = X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Single                // Input
 fld  [edi].Single                // X[0], Input
 fld  Input.Single                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Single                // FMem[FPhase].X[0] := Input;
 fadd [esi].Single                // Input + Y[0], X[0], Input
 mov  ebx,[eax.FCoefficients]     // ebx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Single                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 4].Single            // X[1], Input
 fld  [eax.FPrev].Single          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm32                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 4].Single            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 4].Single            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 4].Single            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov  ecx, [eax.FNumberOfCoeffs]  // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                      // "Den Rest mach ich selber"
@Loopy:
 fld  [edi +  8].Single           // X[2], Input
 fld  [esi].Single                // Y[0], X[2], Input
 fst  [edi +  8].Single           // FMem[FPhase].X[2] := Y[0];
 fadd [esi +  8].Single           // FPrev + Y[2], X[2], Input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * FCoefficients[2] - X[2]
 fstp [esi +  8].Single           // FMem[FPhase].Y[2] :=  "
 add  esi, 4
 add  edi, 4
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop  ecx                         // ecx hat ausgedient!

 fld  [edi + 8].Single            // X[10], Input
 fld  [esi].Single                // X[8], X[10], Input
 fst  [edi + 8].Single            // FMem[FPhase].X[10] := X[8];
 fadd [esi + 8].Single            // FPrev + Y[10], X[8], Input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * FCoefficients[10] - X[10]
 fst  [esi + 8].Single            // FMem[FPhase].Y[10] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[10]);

 fld  [edi + 12].Single           // X[11], Input
 fld  [esi + 4].Single            // X[9], X[11], Input
 fst  [edi + 12].Single           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 12].Single           // FPrev + Y[11], X[9], Input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * FCoefficients[11] - X[11]
 fst  [esi + 12].Single           // FMem[FPhase].Y[11] :=  "

 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[11])
 faddp                            // sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]);
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]));
 fxch                             // Input, result
 fstp [eax.FPrev].Single          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm32 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) *
      FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := Input;
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

procedure TPhaseHalfPi64.ChooseProcedures;
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
 ChooseProcedures;
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
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [eax + FMem[ebx]]      // edi = X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0],Input
 fld  Input.Double               // Input,X[0],Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0]
 fst [esi].Double                // FMem[FPhase].Y[0] := "
 fstp OutputA.Double             // OutputA := FMem[FPhase].Y[0];
 fld [eax.FPrev].Double          // FPrev, Input
 fstp OutputB.Double             // OutputB := FPrev;
 fstp [eax.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
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
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [eax + FMem[ebx]]      // edi=X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld CDenorm64                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0], Input
 fld  Input.Double               // Input, X[0],Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)               // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov ebx,[eax.FCoefficients]     // ebx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fst [esi].Double                // FMem[FPhase].Y[0] := "
 fstp OutputA.Double             // OutputA := FMem[FPhase].Y[0];

 fld  [edi + 8].Double           // X[1], Input
 fld  [eax.FPrev].Double         // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)               // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], Input
 mov ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst [esi + 8].Double            // FMem[FPhase].Y[1] := "
 fstp OutputB.Double             // OutputB := FMem[FPhase].Y[1];
 fstp [eax.FPrev].Double         // FPrev := Input;

 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
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
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [eax + FMem[ebx]]      // edi = X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase],ebx            // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld CDenorm64                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0], Input
 fld  Input.Double               // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)               // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]   // edx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], Input
 fld  [eax.FPrev].Double         // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)               // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst  [esi + 8].Double           // FMem[FPhase].Y[1] :=  "
 fstp OutputA.Double             // OutputB := FMem[FPhase].Y[1];

 fld  [edi + 16].Double          // X[2], Input
 fld  [esi].Double               // Y[0], X[2], Input
 fst  [edi + 16].Double          // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi + 16].Double          // FMem[FPhase].Y[2] :=  "
 fstp OutputB.Double             // OutputB := FMem[FPhase].Y[2];

 fstp [eax.FPrev].Double         // FPrev := Input;
 fstp st(0)

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  OutputA := FMem[FPhase].Y[1];
  OutputB := FMem[FPhase].Y[2];
  FPrev := Input;
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx
 {$IFDEF HandleDenormals}
 fld CDenorm64                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Double                // Input
 fld  [edi].Double                // X[0], Input
 fld  Input.Double                // Input, X[0], Input
 fst  [edi].Double                // FMem[FPhase].X[0] := Input;
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL + Input, X[0], Input
 {$ENDIF}
 fadd [esi].Double                // dEnOrMaL + Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // edx = FCoefficients
 fmul [ebx].Double                // (dEnOrMaL + Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                           // (dEnOrMaL + Input + Y[0]) * FCoefficients[0] - X[0], Input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], Input
 fld  [eax.FPrev].Double          // FPrev, X[1], Input
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 fld  [edi + 16].Double           // X[2], Input
 fld  [esi].Double                // Y[0], X[2], Input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double           // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 fstp OutputA.Double              // OutputB := FMem[FPhase].Y[2];

 fld  [edi + 24].Double           // X[3], Input
 fld  [esi +  8].Double           // X[1], X[3], Input
 fst  [edi + 24].Double           // FMem[FPhase].X[3] := X[1];
 fadd [esi + 24].Double           // FPrev + Y[3], X[3], Input
 fmul [ebx + 24].Double           // (FPrev + Y[3]) * FCoefficients[3], X[3]
 fsubp                            // (FPrev + Y[3]) * FCoefficients[3] - X[3]
 fst  [esi + 24].Double           // FMem[FPhase].Y[3] :=  "
 fstp OutputB.Double              // OutputB := FMem[FPhase].Y[3];

 fstp [eax.FPrev].Double          // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2];
  FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * FCoefficients[3] - FMem[FPhase].X[3];
  FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  OutputA := FMem[FPhase].Y[2];
  OutputB := FMem[FPhase].Y[3];
  FPrev := Input;
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8 * FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 {$IFDEF HandleDenormals}
 fld CDenorm64                   // Pure Speed
 {$ELSE}
 fldz
 {$ENDIF}

 fld  Input.Double                // Input
 fld  [edi].Double                // X[0], Input
 fld  Input.Double                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double                // FMem[FPhase].X[0] := Input;
 fadd [esi].Double                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // ebx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], Input
 fld  [eax.FPrev].Double          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd st(0), st(3)                // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov ecx, [eax.FNumberOfCoeffs]   // ECX=self.FNumberOfCoeffs
 sub ecx, 4                       // "Den Rest mach ich selber"
@Loopy:
 fld  [edi + 16].Double           // X[2], Input
 fld  [esi].Double                // Y[0], X[2], Input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // FPrev + Y[2], X[2], Input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * FCoefficients[2] - X[2]
 fstp [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 add esi, 8
 add edi, 8
 add ebx, 8                       // Weiter geht's
 loop @Loopy
 pop ecx                          // ecx hat ausgedient!

 fld  [edi + 16].Double           // X[10], Input
 fld  [esi].Double                // X[8], X[10], Input
 fst  [edi + 16].Double           // FMem[FPhase].X[10] := X[8];
 fadd [esi + 16].Double           // FPrev + Y[10], X[8], Input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * FCoefficients[10] - X[10]
 fst  [esi + 16].Double           // FMem[FPhase].Y[10] :=  "
 fstp [ecx].Double                // OutputB := FMem[FPhase].Y[10];

 fld  [edi + 24].Double           // X[11], Input
 fld  [esi +  8].Double           // X[9], X[11], Input
 fst  [edi + 24].Double           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 24].Double           // FPrev + Y[11], X[9], Input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * FCoefficients[11] - X[11]
 fst  [esi + 24].Double           // FMem[FPhase].Y[11] :=  "
 fstp [edx].Double                // OutputB := FMem[FPhase].Y[11];

 fstp [eax.FPrev].Double          // FPrev := Input;
 fstp st(0)
 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  OutputA := FMem[FPhase].Y[FNumberOfCoeffs - 2];
  OutputB := FMem[FPhase].Y[FNumberOfCoeffs - 1];
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample1(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [eax + FMem[ebx]]      // edi = X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0], Input
 fld  Input.Double               // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]   // edx = FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0]
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0]
 fst  [esi].Double               // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[0]), Input
 fld  [eax.FPrev].Double         // FPrev, sqr(FMem[FPhase].Y[0]), Input
 fmul st(0), st(0)               // sqr(FPrev), sqr(FMem[FPhase].Y[0]), Input
 faddp                           // sqr(FPrev) + sqr(FMem[FPhase].Y[0]), Input
 fsqrt                           // sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0])), Input
 fxch                            // Input, sqrt(sqr(FPrev) + sqr(FMem[FPhase].Y[0]))
 fstp [eax.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
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
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase],ebx            // FPhase = ebx

 fld  Input.Double                // Input
 fld  [edi].Double                // X[0],Input
 fld  Input.Double                // Input,X[0],Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double                // FMem[FPhase].X[0] := Input;
 fadd [esi].Double                // Input + Y[0], X[0], Input
 mov  ebx,[eax.FCoefficients]     // ebx=FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fst  [esi].Double                // FMem[FPhase].Y[0] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[0]);

 fld  [edi + 8].Double            // X[1], Input
 fld  [eax.FPrev].Double          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], Input
 mov  ebx,[eax.FCoefficients]     // edx = FCoefficients
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst  [esi + 8].Double            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[1]);
 faddp                            // sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
 fxch                             // Input, result
 fstp [eax.FPrev].Double          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0];
  FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1];
  FMem[FPhase].X[1] := FPrev;
  Result := Sqrt(sqr(FMem[FPhase].Y[0]) + sqr(FMem[FPhase].Y[1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample3(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8*FPhase
 mov edi, [eax + FMem[ebx]]      // edi=X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0],Input
 fld  Input.Double               // Input,X[0],Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov  ebx,[eax.FCoefficients]    // edx=FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], Input
 fld  [eax.FPrev].Double         // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], Input
 mov  ebx, [eax.FCoefficients]   // edx=FCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fst [esi + 8].Double            // FMem[FPhase].Y[1] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[1]);

 fld [edi + 16].Double           // X[2], Input
 fld [esi].Double                // Y[0], X[2], Input
 fst [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[2]);
 faddp                           // sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]
 fsqrt                           // result := sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
 fxch                            // Input, result
 fstp [eax.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  Result := Sqrt(sqr(FMem[FPhase].Y[1]) + sqr(FMem[FPhase].Y[2]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;

{$ENDIF}

function TPhaseHalfPi64.ProcessSample4(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                        // The Saviours of ebx
 push edi                        // The Saviours of edi
 push esi                        // The Saviours of esi
 mov ebx, [eax.FPhase]           // ebx = FPhase
 shl ebx, 3                      // ebx = 8 * FPhase
 mov edi, [eax + FMem[ebx]]      // edi = X[0]
 mov esi, [eax + FMem[ebx] + 4]  // esi = Y[0]
 shr ebx, 3                      // ebx = FPhase
 xor ebx, $1                     // Toggle FPhase!!
 mov [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Double               // Input
 fld  [edi].Double               // X[0],Input
 fld  Input.Double               // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double               // FMem[FPhase].X[0] := Input;
 fadd [esi].Double               // Input + Y[0], X[0], Input
 mov  ebx,[eax.FCoefficients]    // edx=FCoefficients
 fmul [ebx].Double               // (Input + Y[0]) * FCoefficients[0], X[0],Input
 fsubrp                          // (Input + Y[0]) * FCoefficients[0] - X[0],Input
 fstp [esi].Double               // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double           // X[1], Input
 fld  [eax.FPrev].Double         // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                  // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double           // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double           // FPrev + Y[1], X[1], Input
 mov  ebx, [eax.FCoefficients]   // edx=FCoefficients
 fmul [ebx + 8].Double           // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                          // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 8].Double           // FMem[FPhase].Y[1] :=  "

 fld  [edi + 16].Double          // X[2], Input
 fld  [esi].Double               // Y[0], X[2], Input
 fst  [edi + 16].Double          // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double          // Y[2] + Y[0], X[2], Input
 fmul [ebx + 16].Double          // (Y[0] + Y[2]) * FCoefficients[2], X[2]
 fsubrp                          // (Y[0] + Y[2]) * FCoefficients[2] - X[2]
 fst  [esi + 16].Double          // FMem[FPhase].Y[2] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[3]);

 fld  [edi + 24].Double          // X[3], Input
 fld  [esi + 8].Double           // X[1], X[3], Input
 fst  [edi + 24].Double          // FMem[FPhase].X[3] := X[1];
 fadd [esi + 24].Double          // FPrev + Y[3], X[3], Input
 fmul [ebx + 24].Double          // (FPrev + Y[3]) * FCoefficients[3], X[3]
 fsubrp                          // (FPrev + Y[3]) * FCoefficients[3] - X[3]
 fst  [esi + 24].Double          // FMem[FPhase].Y[3] :=  "
 fmul st(0), st(0)               // sqr(FMem[FPhase].Y[3]);
 faddp                           // sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4])
 fsqrt                           // result := sqrt(sqr(FMem[FPhase].Y[3]) + sqr(FMem[FPhase].Y[4]));
 fxch                            // Input, result
 fstp [eax.FPrev].Double         // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  FMem[FPhase].Y[2] := (FMem[FPhase].Y[0] + FMem[FPhase].Y[2]) * FCoefficients[2] - FMem[FPhase].X[2]; FMem[FPhase].X[2] := FMem[FPhase].Y[0];
  FMem[FPhase].Y[3] := (FMem[FPhase].Y[1] + FMem[FPhase].Y[3]) * FCoefficients[3] - FMem[FPhase].X[3]; FMem[FPhase].X[3] := FMem[FPhase].Y[1];
  Result := Sqrt(sqr(FMem[FPhase].Y[2]) + sqr(FMem[FPhase].Y[3]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

function TPhaseHalfPi64.ProcessSampleLarge(const Input: Double): Double;
{$IFNDEF PUREPASCAL}
asm
 push ebx                         // The Saviours of ebx
 push edi                         // The Saviours of edi
 push esi                         // The Saviours of esi
 mov  ebx, [eax.FPhase]           // ebx = FPhase
 shl  ebx, 3                      // ebx = 8*FPhase
 mov  edi, [eax + FMem[ebx]]      // edi=X[0]
 mov  esi, [eax + FMem[ebx] + 4]  // esi=Y[0]
 shr  ebx, 3                      // ebx = FPhase
 xor  ebx, $1                     // Toggle FPhase!!
 mov  [eax.FPhase], ebx           // FPhase = ebx

 fld  Input.Double                // Input
 fld  [edi].Double                // X[0], Input
 fld  Input.Double                // Input, X[0], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                   // dEnOrMaL
 {$ENDIF}
 fst  [edi].Double                // FMem[FPhase].X[0] := Input;
 fadd [esi].Double                // Input + Y[0], X[0], Input
 mov  ebx, [eax.FCoefficients]    // ebx = FCoefficients
 fmul [ebx].Double                // (Input + Y[0]) * FCoefficients[0], X[0], Input
 fsubrp                           // (Input + Y[0]) * FCoefficients[0] - X[0], Input
 fstp [esi].Double                // FMem[FPhase].Y[0] :=  "

 fld  [edi + 8].Double            // X[1], Input
 fld  [eax.FPrev].Double          // FPrev, X[1], Input
 {$IFDEF HandleDenormals}
 fadd CDenorm64                   // dEnOrMaL
 {$ENDIF}
 fst  [edi + 8].Double            // FMem[FPhase].X[1] := FPrev;
 fadd [esi + 8].Double            // FPrev + Y[1], X[1], Input
 fmul [ebx + 8].Double            // (FPrev + Y[1]) * FCoefficients[1], X[1]
 fsubrp                           // (FPrev + Y[1]) * FCoefficients[1] - X[1]
 fstp [esi + 8].Double            // FMem[FPhase].Y[1] :=  "

 push ecx                         // The Saviour of ECX
 mov  ecx, [eax.FNumberOfCoeffs]  // ECX=self.FNumberOfCoeffs
 sub  ecx, 4                      // "Den Rest mach ich selber"
@Loopy:
 fld  [edi + 16].Double           // X[2], Input
 fld  [esi].Double                // Y[0], X[2], Input
 fst  [edi + 16].Double           // FMem[FPhase].X[2] := Y[0];
 fadd [esi + 16].Double           // FPrev + Y[2], X[2], Input
 fmul [ebx + 16].Double           // (FPrev + Y[2]) * FCoefficients[2], X[2]
 fsubrp                           // (FPrev + Y[2]) * FCoefficients[2] - X[2]
 fstp [esi + 16].Double           // FMem[FPhase].Y[2] :=  "
 add  esi, 8
 add  edi, 8
 add  ebx, 8                      // Weiter geht's
 loop @Loopy
 pop ecx                          // ecx hat ausgedient!

 fld  [edi + 16].Double           // X[10], Input
 fld  [esi].Double                // X[8], X[10], Input
 fst  [edi + 16].Double           // FMem[FPhase].X[10] := X[8];
 fadd [esi + 16].Double           // FPrev + Y[10], X[8], Input
 fmul [ebx + 16].Double           // (FPrev + Y[10]) * FCoefficients[10], X[10]
 fsubrp                           // (FPrev + Y[10]) * FCoefficients[10] - X[10]
 fst  [esi + 16].Double           // FMem[FPhase].Y[10] :=  "
 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[10]);

 fld  [edi + 24].Double           // X[11], Input
 fld  [esi + 8].Double            // X[9], X[11], Input
 fst  [edi + 24].Double           // FMem[FPhase].X[11] := X[9];
 fadd [esi + 24].Double           // FPrev + Y[11], X[9], Input
 fmul [ebx + 24].Double           // (FPrev + Y[11]) * FCoefficients[11], X[11]
 fsubrp                           // (FPrev + Y[11]) * FCoefficients[11] - X[11]
 fst  [esi + 24].Double           // FMem[FPhase].Y[11] :=  "

 fmul st(0), st(0)                // sqr(FMem[FPhase].Y[11])
 faddp                            // sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]);
 fsqrt                            // result := sqrt(sqr(FMem[FPhase].Y[10]) + sqr(FMem[FPhase].Y[11]));
 fxch                             // Input, result
 fstp [eax.FPrev].Double          // FPrev := Input;

 pop esi
 pop edi
 pop ebx
end;
{$ELSE}
var
  i: Integer;
begin
  FMem[FPhase].Y[0] := (Input + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[0]) * FCoefficients[0] - FMem[FPhase].X[0]; FMem[FPhase].X[0] := Input;
  FMem[FPhase].Y[1] := (FPrev + {$IFDEF HandleDenormals}CDenorm64 + {$ENDIF}
    FMem[FPhase].Y[1]) * FCoefficients[1] - FMem[FPhase].X[1]; FMem[FPhase].X[1] := FPrev;
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FMem[FPhase].Y[i] := (FMem[FPhase].Y[i - 2] + FMem[FPhase].Y[i]) * FCoefficients[i] - FMem[FPhase].X[i];
    FMem[FPhase].X[i] := FMem[FPhase].Y[i - 2];
   end;
  Result := Sqrt(sqr(FMem[FPhase].Y[FNumberOfCoeffs - 2]) + sqr(FMem[FPhase].Y[FNumberOfCoeffs - 1]));
  FPrev := Input;
  FPhase := 1 - FPhase;
end;
{$ENDIF}

{$IFDEF HandleDenormals}
initialization
  CDenorm32 := DAV_Common.CDenorm32;
  CDenorm64 := DAV_Common.CDenorm64;
{$ENDIF}

end.
