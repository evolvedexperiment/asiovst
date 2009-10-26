unit DAV_DspPolyphaseDownsampler;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_DspPolyphaseFilter;

type
  TProcessSample32 = function(const Output: TDAV2SingleArray): Single of object;
  TProcessSample64 = function(const Output: TDAV2DoubleArray): Double of object;
  TProcessSampleSplit32 = procedure(out Low, High: Single; Input: TDAV2SingleArray) of object;
  TProcessSampleSplit64 = procedure(out Low, High: Double; Input: TDAV2DoubleArray) of object;

  TCustomPolyphaseDownsampler = class(TCustomPolyphaseFilter);

  TPolyphaseDownsampler32 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY                : PDAVSingleFixedArray;
    FStateStack           : PDAVSingleFixedArray;
    FProcessSample32      : TProcessSample32;
    FProcessSampleSplit32 : TProcessSampleSplit32;

    function ProcessSample1(const Input: TDAV2SingleArray): Single;
    function ProcessSample2(const Input: TDAV2SingleArray): Single;
    function ProcessSample3(const Input: TDAV2SingleArray): Single;
    function ProcessSample4(const Input: TDAV2SingleArray): Single;
    function ProcessSampleLarge(const Input: TDAV2SingleArray): Single;

    procedure ProcessSampleSplit1(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit2(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit3(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplit4(out Low, High: Single; Input: TDAV2SingleArray);
    procedure ProcessSampleSplitLarge(out Low, High: Single; Input: TDAV2SingleArray);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ChooseProcedures; override;
    procedure NumberOfCoeffsChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure ProcessBlockSplit(const OutputL, OutputH, Input: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;
    procedure PushStates;
    procedure PopStates;
    procedure ResetStates;

    function ProcessSample32(const Output: TDAV2SingleArray): Single;

    property ProcessSample: TProcessSample32 read FProcessSample32;
    property ProcessSampleSplit: TProcessSampleSplit32 read FProcessSampleSplit32;
  end;

  TPolyphaseDownsampler64 = class(TCustomPolyphaseDownsampler)
  private
    FX, FY                : PDAVDoubleFixedArray;
    FStateStack           : PDAVDoubleFixedArray;
    FProcessSample64      : TProcessSample64;
    FProcessSampleSplit64 : TProcessSampleSplit64;

    function ProcessSample1(const Input: TDAV2DoubleArray): Double;
    function ProcessSample2(const Input: TDAV2DoubleArray): Double;
    function ProcessSample3(const Input: TDAV2DoubleArray): Double;
    function ProcessSample4(const Input: TDAV2DoubleArray): Double;
    function ProcessSampleLarge(const Input: TDAV2DoubleArray): Double;

    procedure ProcessSampleSplit1(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit2(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit3(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplit4(out Low, High: Double; Input: TDAV2DoubleArray);
    procedure ProcessSampleSplitLarge(out Low, High: Double; Input: TDAV2DoubleArray);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ChooseProcedures; override;
    procedure NumberOfCoeffsChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock(const Input, Output: PDavDoubleFixedArray; SampleFrames: Integer);
    procedure ProcessBlockSplit(const OutputL, OutputH, Input: PDavDoubleFixedArray; SampleFrames: Integer);
    procedure ClearBuffers;
    procedure PushStates;
    procedure PopStates;
    procedure ResetStates;

    function ProcessSample64(const Output: TDAV2DoubleArray): Double;

    property ProcessSample: TProcessSample64 read FProcessSample64;
    property ProcessSampleSplit: TProcessSampleSplit64 read FProcessSampleSplit64;
  end;

implementation

/////////////////////////////// Constructor //////////////////////////////////

const
  CHalf32: Single = 0.5;
  CHalf64: Double = 0.5;

constructor TPolyphaseDownsampler32.Create;
begin
  inherited;
  FX := nil;
  FY := nil;
  FStateStack := nil;
  NumberOfCoeffsChanged;
end;

destructor TPolyphaseDownsampler32.Destroy;
begin
  Dispose(FX);
  Dispose(FY);
  Dispose(FStateStack);
  inherited;
end;

procedure TPolyphaseDownsampler32.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FX, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FY, FNumberOfCoeffs * SizeOf(Single));
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Single));
 ChooseProcedures;
 ClearBuffers;
end;

procedure TPolyphaseDownsampler32.AssignTo(Dest: TPersistent);
begin
 if Dest is TPolyphaseDownsampler32 then
  with TPolyphaseDownsampler32(Dest) do
   begin
    inherited;
    FProcessSample32      := Self.FProcessSample32;
    FProcessSampleSplit32 := Self.FProcessSampleSplit32;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Single));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Single));
   end
 else inherited;
end;

procedure TPolyphaseDownsampler32.ChooseProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FProcessSample32      := ProcessSample1;
      FProcessSampleSplit32 := ProcessSampleSplit1;
     end;
    2 :
     begin
      FProcessSample32      := ProcessSample2;
      FProcessSampleSplit32 := ProcessSampleSplit2;
     end;
    3 :
     begin
      FProcessSample32      := ProcessSample3;
      FProcessSampleSplit32 := ProcessSampleSplit3;
     end;
    4 :
     begin
      FProcessSample32      := ProcessSample4;
      FProcessSampleSplit32 := ProcessSampleSplit4;
     end;
  else
   begin
    FProcessSample32      := ProcessSampleLarge;
    FProcessSampleSplit32 := ProcessSampleSplitLarge;
   end;
 end;
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

procedure TPolyphaseDownsampler32.ClearBuffers;
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

procedure TPolyphaseDownsampler32.ResetStates;
begin
  FillChar(FX[0], FNumberOfCoeffs * SizeOf(Single), 0);
  FillChar(FY[0], FNumberOfCoeffs * SizeOf(Single), 0);
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

procedure TPolyphaseDownsampler32.PushStates;
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

procedure TPolyphaseDownsampler32.PopStates;
begin
  Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Single));
  Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Single));
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ProcessSample                                                       //
//  --------------                                                            //
//                                                                            //
//  Description:                                                              //
//     Downsamples (x2) one pair of samples, to generate one output sample.   //
//                                                                            //
//  Input parameters:                                                         //
//    - Input: pointer on the two samples to decimate                         //
//                                                                            //
//  Returns: Samplerate-reduced sample.                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPolyphaseDownsampler32.ProcessSample1(const Input: TDAV2SingleArray): Single;
{$IFNDEF PUREPASCAL}
asm
  push edi
  mov edi, [eax.FY]              // edi = FY
  mov ecx, [eax.FX]              // esi = FX
  mov eax, [eax.FCoefficients]   // ecx = FCoefficients
  fld [ecx].Single               // FX[0]
  fld [Input + 4].Single         // Input[1], FX[0]
  fst  [ecx].Single              // FX[0] := Input[1];
  fsub [edi].Single              // (Input[1] - FY[0])
  fmul [eax].Double              // (Input[1] - FY[0]) * FCoefficients[0]
  faddp                          // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  fst [edi].Single
  fld [Input].Single             // Input[0], FY[0]
  faddp                          // FY[1] + FY[0]
  fmul CHalf32                   // (FY[1] + FY[0]) * 0.5
  pop edi
end;
{$ELSE}
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0];
 FX[0] := Input[1];
 Result := CHalf * (FY[0] + Input[0]);
end;
{$ENDIF}

function TPolyphaseDownsampler32.ProcessSample2(const Input: TDAV2SingleArray): Single;
{$IFNDEF PUREPASCAL}
asm
  push edi
  mov edi, [eax.FY]              // edi = FY
  mov ecx, [eax.FX]              // esi = FX
  mov eax, [eax.FCoefficients]   // ecx = FCoefficients

  fld  [ecx].Single              // FX[0]
  fld  [Input + 4].Single        // Input[1], FX[0]
  fst  [ecx].Single              // FX[0] := Input[1];
  fsub [edi].Single              // (Input[1] - FY[0])
  fmul [eax].Double              // (Input[1] - FY[0]) * FCoefficients[0]
  faddp                          // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  fst  [edi].Single

  fld  [ecx + 4].Single          // FX[1], FY[0]
  fld  [Input].Single            // Input[0], FX[1], FY[0]
  fst  [ecx + 4].Single          // FX[1] := Input[0];
  fsub [edi + 4].Single          // (Input[0] - FY[1]), FY[0]
  fmul [eax + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1], FY[0]
  faddp                          // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  fst  [edi + 4].Single

  faddp                          // FY[1] + FY[0]
  fmul CHalf32                   // (FY[1] + FY[0]) * 0.5
  pop edi
end;
{$ELSE}
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1] := Input[0];
 Result := CHalf * (FY[0] + FY[1]);
end;
{$ENDIF}

function TPolyphaseDownsampler32.ProcessSample3(const Input: TDAV2SingleArray): Single;
{$IFNDEF PUREPASCAL}
asm
  push edi
  mov edi, [eax.FY]              // edi = FY
  mov ecx, [eax.FX]              // esi = FX
  mov eax, [eax.FCoefficients]   // ecx = FCoefficients

  fld  [ecx].Single              // FX[0]
  fld  [Input + 4].Single        // Input[1], FX[0]
  fst  [ecx].Single              // FX[0] := Input[1];
  fsub [edi].Single              // (Input[1] - FY[0])
  fmul [eax].Double              // (Input[1] - FY[0]) * FCoefficients[0]
  faddp                          // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  fstp [edi].Single

  fld  [ecx + 4].Single          // FX[1]
  fld  [Input].Single            // Input[0], FX[1]
  fst  [ecx + 4].Single          // FX[1] := Input[0];
  fsub [edi + 4].Single          // (Input[0] - FY[1])
  fmul [eax + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1]
  faddp                          // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  fst  [edi + 4].Single

  fld  [ecx +  8].Single         // FX[2], FY[1]
  fld  [edi].Single              // FY[0], FX[2], FY[1]
  fst  [ecx +  8].Single         // FX[2] := FY[0];
  fsub [edi +  8].Single         // (FY[0] - FY[2]), FY[1]
  fmul [eax + 16].Double         // (FY[0] - FY[2]) * FCoefficients[2], FY[1]
  faddp                          // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
  fst  [edi +  8].Single

  faddp                          // FY[2] + FY[1]
  fmul CHalf32                   // (FY[2] + FY[1]) * 0.5
  pop edi
end;
{$ELSE}
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1] := Input[0];
 FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2];    FX[2] := FY[0];
 Result := CHalf * (FY[1] + FY[2]);
end;
{$ENDIF}

function TPolyphaseDownsampler32.ProcessSample4(const Input: TDAV2SingleArray): Single;
{$IFNDEF PUREPASCAL}
asm
  push edi
  mov edi, [eax.FY]             // edi = FY
  mov ecx, [eax.FX]             // esi = FX
  mov eax, [eax.FCoefficients]  // ecx = FCoefficients

  fld  [ecx].Single              // FX[0]
  fld  [Input + 4].Single        // Input[1], FX[0]
  fst  [ecx].Single              // FX[0] := Input[1];
  fsub [edi].Single              // (Input[1] - FY[0])
  fmul [eax].Double              // (Input[1] - FY[0]) * FCoefficients[0]
  faddp                          // (Input[1] - FY[0]) * FCoefficients[0] + FX[0]
  fstp [edi].Single

  fld  [ecx + 4].Single          // FX[1]
  fld  [Input].Single            // Input[0], FX[1]
  fst  [ecx + 4].Single          // FX[1] := Input[0];
  fsub [edi + 4].Single          // (Input[0] - FY[1])
  fmul [eax + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1]
  faddp                          // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  fstp [edi + 4].Single

  fld  [ecx +  8].Single         // FX[2]
  fld  [edi].Single              // FY[0], FX[2]
  fst  [ecx +  8].Single         // FX[2] := FY[0];
  fsub [edi +  8].Single         // (FY[0] - FY[2])
  fmul [eax + 16].Double         // (FY[0] - FY[2]) * FCoefficients[2]
  faddp                          // (FY[0] - FY[2]) * FCoefficients[2] + FX[2]
  fst  [edi + 8].Single

  fld  [ecx + 12].Single         // FX[2], FY[2]
  fld  [edi + 4].Single          // FY[0], FX[2], FY[2]
  fst  [ecx + 12].Single         // FX[2] := FY[0];
  fsub [edi + 12].Single         // (FY[0] - FY[2]), FY[2]
  fmul [eax + 24].Double         // (FY[0] - FY[2]) * FCoefficients[2], FY[2]
  faddp                          // (FY[0] - FY[2]) * FCoefficients[2] + FX[2], FY[2]
  fst  [edi + 12].Single

  faddp                          // FY[3] + FY[2]
  fmul CHalf32                   // (FY[3] + FY[2]) * 0.5
  pop edi
end;
{$ELSE}
begin
 FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
 FY[1] := (Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1] := Input[0];
 FY[2] := (FY[0] - FY[2]) * FCoefficients[2] + FX[2];    FX[2] := FY[0];
 FY[3] := (FY[1] - FY[3]) * FCoefficients[3] + FX[3];    FX[3] := FY[1];
 Result := CHalf * (FY[2] + FY[3]);
end;
{$ENDIF}

function TPolyphaseDownsampler32.ProcessSampleLarge(const Input: TDAV2SingleArray): Single;
{$IFNDEF PUREPASCAL}
asm
  pushad
  mov esi, [eax.FX]             // esi = FX
  mov edi, [eax.FY]             // edi = FY
  mov ebx, [eax.FCoefficients]  // ecx = FCoefficients

  fld [esi].Single               // FX[0]
  fld [Input + 4].Single         // Input[1], FX[0]
  fst  [esi].Single              // FX[0]:=Input[1];
  fsub [edi].Single              // (Input[1]-FY[0])
  fmul [ebx].Double              // (Input[1]-FY[0])*FCoefficients[0]
  faddp                          // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
  fstp [edi].Single

  fld [esi + 4].Single           // FX[1]
  fld [Input].Single             // Input[0], FX[1]
  fst  [esi + 4].Single          // FX[1] := Input[0];
  fsub [edi + 4].Single          // (Input[0] - FY[1])
  fmul [ebx + 8].Double          // (Input[0] - FY[1]) * FCoefficients[1]
  faddp                          // (Input[0] - FY[1]) * FCoefficients[1] + FX[1]
  fstp [edi + 4].Single

  mov ecx,[eax.FNumberOfCoeffs] // ECX=self.FNumberOfCoeffs
  sub ecx, 4                     // "Den Rest mach ich selber"
  @Loopy:
  fld  [esi +  8].Single         // FX[a]
  fld  [edi].Single              // FY[b], FX[a]
  fst  [esi +  8].Single         // FX[a] := FY[b];
  fsub [edi +  8].Single         // (FY[b] - FY[a])
  fmul [ebx + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
  faddp                          // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
  fstp [edi +  8].Single

  add esi, 4
  add edi, 4
  add ebx, 8                     // Weiter geht's
  loop @Loopy

  fld  [esi + 8].Single          // FX[a]
  fld  [edi].Single              // FY[b], FX[a]
  fst  [esi +  8].Single         // FX[a] := FY[b];
  fsub [edi +  8].Single         // (FY[b] - FY[a])
  fmul [ebx + 16].Double         // (FY[b] - FY[a]) * FCoefficients[a]
  faddp                          // (FY[b] - FY[a]) * FCoefficients[a]+FX[a]
  fst  [edi +  8].Single

  fld  [esi + 12].Single         // FX[a], FY[a]
  fld  [edi + 4].Single          // FY[b], FX[a], FY[a]
  fst  [esi + 12].Single         // FX[a] := FY[b];
  fsub [edi + 12].Single         // (FY[b] - FY[a]), FY[a]
  fmul [ebx + 24].Double         // (FY[b] - FY[a]) * FCoefficients[a], FY[a]
  faddp                          // (FY[b] - FY[a]) * FCoefficients[a] + FX[a], FY[a]
  fst [edi + 12].Single

  faddp                          // FY[a] + FY[aalt]
  fmul CHalf32                   // (FY[a] + FY[aalt]) * 0.5
  popad
end;
{$ELSE}
var
  i: Integer;
begin
 FY[0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[ 0]:=Input[1];
 FY[1]:=(Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[ 1]:=Input[0];
 for i := 2 to FNumberOfCoeffs - 1 do
  begin
   FY[i]:= (FY[i-2] - FY[i]) * FCoefficients[i] + FX[i];
   FX[i]:=  FY[i-2];
  end;
 Result := CHalf * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessSampleSplit                                                 //
//   ------------------------                                                 //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler32.ProcessSampleSplit1(out Low, High: Single; Input: TDAV2SingleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  Low := (FY[0] + Input[0]) * 0.5;
  High := FY[0] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit2(out Low, High: Single; Input: TDAV2SingleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
    PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
  PDav2SingleArray(FX)^[1] := Input[0];
  Low := (PDav2SingleArray(FY)^[0] + PDav2SingleArray(FY)^[1]) * 0.5;
  High := PDav2SingleArray(FY)^[1] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit3(out Low, High: Single; Input: TDAV2SingleArray);
begin
  FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(fCoefficients)^[0] + FX[0]; FX[0] := Input[1];

  PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
    PDav4DoubleArray(fCoefficients)^[1] + PDav4SingleArray(FX)^[1];
  PDav4SingleArray(FX)^[1] := Input[0];
  PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
    PDav4DoubleArray(fCoefficients)^[2] + PDav4SingleArray(FX)^[2];
  PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
  Low := (PDav4SingleArray(FY)^[2] + PDav4SingleArray(FY)^[1]) * 0.5;
  High := PDav4SingleArray(FY)^[2] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplit4(out Low, High: Single; Input: TDAV2SingleArray);
begin
  FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(fCoefficients)^[0] + FX[0]; FX[0] := Input[1];
  PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
    PDav4DoubleArray(fCoefficients)^[1] + PDav4SingleArray(FX)^[1];
  PDav4SingleArray(FX)^[1] := Input[0];
  PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
    PDav4DoubleArray(fCoefficients)^[2] + PDav4SingleArray(FX)^[2];
  PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
  PDav4SingleArray(FY)^[3] := (PDav4SingleArray(FY)^[1] - PDav4SingleArray(FY)^[3]) *
    PDav4DoubleArray(fCoefficients)^[3] + PDav4SingleArray(FX)^[3];
  PDav4SingleArray(FX)^[3] := PDav4SingleArray(FY)^[1];
  Low := (PDav4SingleArray(FY)^[3] + PDav4SingleArray(FY)^[2]) * 0.5;
  High := PDav4SingleArray(FY)^[3] - Low;
end;

procedure TPolyphaseDownsampler32.ProcessSampleSplitLarge(
  out Low, High: Single; Input: TDAV2SingleArray);
var
  i: Integer;
begin
  FY[0] := (Input[1] - FY[0]) * PDav4DoubleArray(fCoefficients)^[0] + FX[0]; FX[0] := Input[1];
  PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
    PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
  PDav2SingleArray(FX)^[1] := Input[0];
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FY[i] := (FY[i - 2] - FY[i]) * FCoefficients[i] + FX[i];
    FX[i] := FY[i - 2];
   end;

  Low := 0.5 * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
  High := FY[FNumberOfCoeffs - 1] - Low;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessBlockSplit                                                  //
//   -----------------------                                                  //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler32.ProcessBlockSplit(
  const OutputL, OutputH, Input: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  assert(SampleFrames > 0);
  pos := 0;
  repeat
    ProcessSampleSplit(OutputL[Pos], OutputH[Pos], PDAV2SingleArray(
      @Input[pos * 2])^);
    Inc(Pos);
  until (Pos >= SampleFrames div 2);
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

procedure TPolyphaseDownsampler32.ProcessBlock(
  const Input, Output: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  for Pos := 0 to SampleFrames - 1
   do Output[pos] := FProcessSample32(PDAV2SingleArray(@Input[pos * 2])^);
end;

function TPolyphaseDownsampler32.ProcessSample32(
  const Output: TDAV2SingleArray): Single;
begin
 Result := FProcessSample32(Output);
end;


/////////////////////////////// Constructor //////////////////////////////////

const
  Half: Double = 0.5;

constructor TPolyphaseDownsampler64.Create;
begin
  inherited;
  FX := nil;
  FY := nil;
  FStateStack := nil;
  NumberOfCoeffsChanged;
end;

destructor TPolyphaseDownsampler64.Destroy;
begin
  Dispose(FX);
  Dispose(FY);
  Dispose(FStateStack);
  inherited;
end;

{=============================================================================
Name: SetCoefs
Description:
  Sets filter FCoefficients. Generate them with the PolyphaseIir2Designer class.
  Call this function before doing any processing.
Input parameters:
  - coef_arr: Array of FCoefficients.
Throws: Nothing
=============================================================================}

procedure TPolyphaseDownsampler64.AssignTo(Dest: TPersistent);
begin
 if Dest is TPolyphaseDownsampler64 then
  with TPolyphaseDownsampler64(Dest) do
   begin
    inherited;
    FProcessSample64      := Self.FProcessSample64;
    FProcessSampleSplit64 := Self.FProcessSampleSplit64;

    Assert(FNumberOfCoeffs = Self.FNumberOfCoeffs);
    Move(Self.FX^, FX^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FY^, FY^, FNumberOfCoeffs * SizeOf(Double));
    Move(Self.FStateStack^, FStateStack^, 2 * FNumberOfCoeffs * SizeOf(Double));
   end
 else inherited;
end;

procedure TPolyphaseDownsampler64.ChooseProcedures;
begin
  case FNumberOfCoeffs of
    1 :
     begin
      FProcessSample64      := ProcessSample1;
      FProcessSampleSplit64 := ProcessSampleSplit1;
     end;
    2 :
     begin
      FProcessSample64      := ProcessSample2;
      FProcessSampleSplit64 := ProcessSampleSplit2;
     end;
    3 :
     begin
      FProcessSample64      := ProcessSample3;
      FProcessSampleSplit64 := ProcessSampleSplit3;
     end;
    4 :
     begin
      FProcessSample64      := ProcessSample4;
      FProcessSampleSplit64 := ProcessSampleSplit4;
     end;
  else
   begin
    FProcessSample64      := ProcessSampleLarge;
    FProcessSampleSplit64 := ProcessSampleSplitLarge;
   end;
 end;
end;

procedure TPolyphaseDownsampler64.NumberOfCoeffsChanged;
begin
 inherited;
 ReallocMem(FX, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FY, FNumberOfCoeffs * SizeOf(Double));
 ReallocMem(FStateStack, 2 * FNumberOfCoeffs * SizeOf(Double));
 ChooseProcedures;
 ClearBuffers;
end;

{=============================================================================
Name: ProcessSample
Description:
   Downsamples (x2) one pair of samples, to generate one output sample.
Input parameters:
  - in_ptr: pointer on the two samples to decimate
Returns: Samplerate-reduced sample.
Throws: Nothing
=============================================================================}

function TPolyphaseDownsampler64.ProcessSample1(
  const Input: TDAV2DoubleArray): Double;
{$IFNDEF PUREPASCAL}
const
  half: Double = 0.5;
asm
    pushad
    mov esi, [eax.FX]                  // esi = FX
    mov edi, [eax.FY]                  // edi = FY
    mov ecx, [eax.FCoefficients]       // ecx = FCoefficients

    fld [esi].Double                    // FX[0]
    fld [Input+8].Double                // Input[1], FX[0]
    fst  [esi].Double                   // FX[0]:=Input[1];
    fsub [edi].Double                   // (Input[1]-FY[0])
    fmul [ecx].Double                   // (Input[1]-FY[0])*FCoefficients[0]
    faddp                               // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fst [edi].Double
// FY[0]:=(Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fld [Input].Double                  // Input[0], FY[0]

    faddp                               // FY[1]+FY[0]
    fmul CHalf64                        // (FY[1]+FY[0])*0.5
    popad
end;
{$ELSE}
begin
 FY[0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0]:=Input[1];
 Result:=0.5*(FY[0]+Input[0]);
end;
{$ENDIF}

function TPolyphaseDownsampler64.ProcessSample2(
  const Input: TDAV2DoubleArray): Double;
{$IFNDEF PUREPASCAL}
const
  half: Double = 0.5;
asm
    pushad
    mov esi, [eax.FX]                  // esi = FX
    mov edi, [eax.FY]                  // edi = FY
    mov ecx, [eax.FCoefficients]       // ecx = FCoefficients

    fld [esi].Double                    // FX[0]
    fld [Input+8].Double                // Input[1], FX[0]
    fst  [esi].Double                   // FX[0]:=Input[1];
    fsub [edi].Double                   // (Input[1]-FY[0])
    fmul [ecx].Double                   // (Input[1]-FY[0])*FCoefficients[0]
    faddp                               // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fst [edi].Double
// FY[0]:=(Input[1]-FY[0])*FCoefficients[0]+FX[0]

    fld [esi+8].Double                  // FX[1], FY[0]
    fld [Input].Double                  // Input[0], FX[1], FY[0]
    fst  [esi+8].Double                 // FX[1]:=Input[0];
    fsub [edi+8].Double                 // (Input[0]-FY[1]), FY[0]
    fmul [ecx+8].Double                 // (Input[0]-FY[1])*FCoefficients[1], FY[0]
    faddp                               // (Input[0]-FY[1])*FCoefficients[1]+FX[1]
    fst [edi+8].Double
// FY[1]:=(Input[0]-FY[1])*FCoefficients[1]+FX[1]

    faddp                               // FY[1]+FY[0]
    fmul CHalf64                        // (FY[1]+FY[0])*0.5
    popad
end;
{$ELSE}
begin
 FY[0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0]:=Input[1];
 FY[1]:=(Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1]:=Input[0];
 Result:=0.5*(FY[0]+FY[1]);
end;
{$ENDIF}

function TPolyphaseDownsampler64.ProcessSample3(
  const Input: TDAV2DoubleArray): Double;
{$IFNDEF PUREPASCAL}
const
  half: Double = 0.5;
asm
    pushad
    mov esi, [eax.FX]                  // esi = FX
    mov edi, [eax.FY]                  // edi = FY
    mov ecx, [eax.FCoefficients]       // ecx = FCoefficients

    fld [esi].Double                    // FX[0]
    fld [Input+8].Double                // Input[1], FX[0]
    fst  [esi].Double                   // FX[0]:=Input[1];
    fsub [edi].Double                   // (Input[1]-FY[0])
    fmul [ecx].Double                   // (Input[1]-FY[0])*FCoefficients[0]
    faddp                               // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fstp [edi].Double
// FY[0]:=(Input[1]-FY[0])*FCoefficients[0]+FX[0]

    fld [esi+8].Double                  // FX[1]
    fld [Input].Double                  // Input[0], FX[1]
    fst  [esi+8].Double                 // FX[1]:=Input[0];
    fsub [edi+8].Double                 // (Input[0]-FY[1])
    fmul [ecx+8].Double                 // (Input[0]-FY[1])*FCoefficients[1]
    faddp                               // (Input[0]-FY[1])*FCoefficients[1]+FX[1]
    fst [edi+8].Double
// FY[1]:=(Input[0]-FY[1])*FCoefficients[1]+FX[1]

    fld [esi+16].Double                 // FX[2], FY[1]
    fld [edi].Double                    // FY[0], FX[2], FY[1]
    fst  [esi+16].Double                // FX[2]:=FY[0];
    fsub [edi+16].Double                // (FY[0]-FY[2]), FY[1]
    fmul [ecx+16].Double                // (FY[0]-FY[2])*FCoefficients[2], FY[1]
    faddp                               // (FY[0]-FY[2])*FCoefficients[2]+FX[2]
    fst [edi+16].Double
// FY[2]:=(FY[0]-FY[2])*FCoefficients[2]+FX[2]

    faddp                               // FY[2]+FY[1]
    fmul CHalf64                        // (FY[2]+FY[1])*0.5
    popad
end;
{$ELSE}
begin
 FY[0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0]:=Input[1];
 FY[1]:=(Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1]:=Input[0];
 FY[2]:=(FY[0] - FY[2]) * FCoefficients[2] + FX[2]; FX[2]:=FY[0];
 Result:=0.5*(FY[1]+FY[2]);
end;
{$ENDIF}

function TPolyphaseDownsampler64.ProcessSample4(
  const Input: TDAV2DoubleArray): Double;
{$IFNDEF PUREPASCAL}
const
  half: Double = 0.5;
asm
    push edi
    mov edi, [eax.FY]                  // edi = FY
    mov ecx, [eax.FX]                  // esi = FX
    mov eax, [eax.FCoefficients]       // ecx = FCoefficients

    fld [ecx].Double                    // FX[0]
    fld [Input+8].Double                // Input[1], FX[0]
    fst  [ecx].Double                   // FX[0]:=Input[1];
    fsub [edi].Double                   // (Input[1]-FY[0])
    fmul [eax].Double                   // (Input[1]-FY[0])*FCoefficients[0]
    faddp                               // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fstp [edi].Double
// FY[0]:=(Input[1]-FY[0])*FCoefficients[0]+FX[0]

    fld [ecx+8].Double                  // FX[1]
    fld [Input].Double                  // Input[0], FX[1]
    fst  [ecx+8].Double                 // FX[1]:=Input[0];
    fsub [edi+8].Double                 // (Input[0]-FY[1])
    fmul [eax+8].Double                 // (Input[0]-FY[1])*FCoefficients[1]
    faddp                               // (Input[0]-FY[1])*FCoefficients[1]+FX[1]
    fstp [edi+8].Double
// FY[1]:=(Input[0]-FY[1])*FCoefficients[1]+FX[1]

    fld [ecx+16].Double                 // FX[2]
    fld [edi].Double                    // FY[0], FX[2]
    fst  [ecx+16].Double                // FX[2]:=FY[0];
    fsub [edi+16].Double                // (FY[0]-FY[2])
    fmul [eax+16].Double                // (FY[0]-FY[2])*FCoefficients[2]
    faddp                               // (FY[0]-FY[2])*FCoefficients[2]+FX[2]
    fst [edi+16].Double
// FY[2]:=(FY[0]-FY[2])*FCoefficients[2]+FX[2]

    fld [ecx+24].Double                 // FX[2], FY[2]
    fld [edi+8].Double                  // FY[0], FX[2], FY[2]
    fst  [ecx+24].Double                // FX[2]:=FY[0];
    fsub [edi+24].Double                // (FY[0]-FY[2]), FY[2]
    fmul [eax+24].Double                // (FY[0]-FY[2])*FCoefficients[2], FY[2]
    faddp                               // (FY[0]-FY[2])*FCoefficients[2]+FX[2], FY[2]
    fst [edi+24].Double
// FY[2]:=(FY[0]-FY[2])*FCoefficients[2]+FX[2]
    faddp                               // FY[3]+FY[2]
    fmul CHalf64                        // (FY[3]+FY[2])*0.5
    pop edi
end;
{$ELSE}
begin
 FY[0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0]:=Input[1];
 FY[1]:=(Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[1]:=Input[0];
 FY[2]:=(FY[0] - FY[2]) * FCoefficients[2] + FX[2]; FX[2]:=FY[0];
 FY[3]:=(FY[1] - FY[3]) * FCoefficients[3] + FX[3]; FX[3]:=FY[1];
 Result:=0.5*(FY[2]+FY[3]);
end;
{$ENDIF}

function TPolyphaseDownsampler64.ProcessSampleLarge(
  const Input: TDAV2DoubleArray): Double;
{$IFNDEF PUREPASCAL}
const
  half: Double = 0.5;
asm
    pushad
    mov esi, [eax.FX]                  // esi = FX
    mov edi, [eax.FY]                  // edi = FY
    mov ebx, [eax.FCoefficients]       // ecx = FCoefficients

    fld [esi].Double                    // FX[0]
    fld [Input+8].Double                // Input[1], FX[0]
    fst  [esi].Double                   // FX[0]:=Input[1];
    fsub [edi].Double                   // (Input[1]-FY[0])
    fmul [ebx].Double                   // (Input[1]-FY[0])*FCoefficients[0]
    faddp                               // (Input[1]-FY[0])*FCoefficients[0]+FX[0]
    fstp [edi].Double
// FY[0]:=(Input[1]-FY[0])*FCoefficients[0]+FX[0]

    fld [esi+8].Double                  // FX[1]
    fld [Input].Double                  // Input[0], FX[1]
    fst  [esi+8].Double                 // FX[1]:=Input[0];
    fsub [edi+8].Double                 // (Input[0]-FY[1])
    fmul [ebx+8].Double                 // (Input[0]-FY[1])*FCoefficients[1]
    faddp                               // (Input[0]-FY[1])*FCoefficients[1]+FX[1]
    fstp [edi+8].Double
// FY[1]:=(Input[0]-FY[1])*FCoefficients[1]+FX[1]

    mov ecx,[eax.FNumberOfCoeffs]      // ECX=self.FNumberOfCoeffs
    sub ecx, 4                          // "Den Rest mach ich selber"
    @Loopy:
    fld [esi+16].Double                 // FX[a]
    fld [edi].Double                    // FY[b], FX[a]
    fst  [esi+16].Double                // FX[a]:=FY[b];
    fsub [edi+16].Double                // (FY[b]-FY[a])
    fmul [ebx+16].Double                // (FY[b]-FY[a])*FCoefficients[a]
    faddp                               // (FY[b]-FY[a])*FCoefficients[a]+FX[a]
    fstp [edi+16].Double
// FY[a]:=(FY[b]-FY[a])*FCoefficients[a]+FX[a]
    add esi,8
    add edi,8
    add ebx,8                           // Weiter geht's
    loop @Loopy

    fld [esi+16].Double                 // FX[a]
    fld [edi].Double                    // FY[b], FX[a]
    fst  [esi+16].Double                // FX[a] := FY[b];
    fsub [edi+16].Double                // (FY[b] - FY[a])
    fmul [ebx+16].Double                // (FY[b] - FY[a]) * FCoefficients[a]
    faddp                               // (FY[b] - FY[a]) * FCoefficients[a] + FX[a]
    fst [edi+16].Double
// FY[a]:=(FY[b]-FY[a])*FCoefficients[a]+FX[a]

    fld [esi+24].Double                 // FX[a], FY[a]
    fld [edi+8].Double                  // FY[b], FX[a], FY[a]
    fst  [esi+24].Double                // FX[a] := FY[b];
    fsub [edi+24].Double                // (FY[b] - FY[a]), FY[a]
    fmul [ebx+24].Double                // (FY[b] - FY[a]) * FCoefficients[a], FY[a]
    faddp                               // (FY[b] - FY[a]) * FCoefficients[a] + FX[a], FY[a]
    fst [edi+24].Double
// FY[a]:=(FY[b]-FY[a])*FCoefficients[a]+FX[a]
    faddp                               // FY[a] + FY[aalt]
    fmul CHalf64                        // (FY[a] + FY[aalt]) * 0.5
    popad
end;
{$ELSE}
var i :Integer;
begin
 FY[ 0]:=(Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[ 0]:=Input[1];
 FY[ 1]:=(Input[0] - FY[1]) * FCoefficients[1] + FX[1]; FX[ 1]:=Input[0];
 for i:=2 to FNumberOfCoeffs-1 do
  begin
   FY[i]:=(FY[i-2] - FY[i]) * FCoefficients[i] + FX[i];
   FX[i]:= FY[i-2];
  end;

 Result := 0.5 * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
end;
{$ENDIF}


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

procedure TPolyphaseDownsampler64.PushStates;
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

procedure TPolyphaseDownsampler64.PopStates;
begin
  Move(FStateStack[0], FX[0], FNumberOfCoeffs * SizeOf(Double));
  Move(FStateStack[FNumberOfCoeffs], FY[0], FNumberOfCoeffs * SizeOf(Double));
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

procedure TPolyphaseDownsampler64.ResetStates;
begin
  FillChar(FX[0], FNumberOfCoeffs * SizeOf(Double), 0);
  FillChar(FY[0], FNumberOfCoeffs * SizeOf(Double), 0);
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

procedure TPolyphaseDownsampler64.ClearBuffers;
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

procedure TPolyphaseDownsampler64.ProcessBlock(
  const Input, Output: PDavDoubleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  for Pos := 0 to SampleFrames - 1
   do Output[pos] := FProcessSample64(PDAV2DoubleArray(@Input[pos * 2])^);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessSampleSplit                                                 //
//   ------------------------                                                 //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler64.ProcessSampleSplit1(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  Low := (FY[0] + Input[0]) * 0.5;
  High := FY[0] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit2(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
    PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
  PDav2SingleArray(FX)^[1] := Input[0];
  Low   := (PDav2SingleArray(FY)^[1] + PDav2SingleArray(FY)^[0]) * 0.5;
  High  := PDav2SingleArray(FY)^[1] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit3(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
    PDav4DoubleArray(fCoefficients)^[1] + PDav4SingleArray(FX)^[1];
  PDav4SingleArray(FX)^[1] := Input[0];
  PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
    PDav4DoubleArray(fCoefficients)^[2] + PDav4SingleArray(FX)^[2];
  PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
  Low   := (PDav4SingleArray(FY)^[2] + PDav4SingleArray(FY)^[1]) * 0.5;
  High  := PDav4SingleArray(FY)^[2] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplit4(
  out Low, High: Double; Input: TDAV2DoubleArray);
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  PDav4SingleArray(FY)^[1] := (Input[0] - PDav4SingleArray(FY)^[1]) *
    PDav4DoubleArray(fCoefficients)^[1] + PDav4SingleArray(FX)^[1];
  PDav4SingleArray(FX)^[1] := Input[0];
  PDav4SingleArray(FY)^[2] := (PDav4SingleArray(FY)^[0] - PDav4SingleArray(FY)^[2]) *
    PDav4DoubleArray(fCoefficients)^[2] + PDav4SingleArray(FX)^[2];
  PDav4SingleArray(FX)^[2] := PDav4SingleArray(FY)^[0];
  PDav4SingleArray(FY)^[3] := (PDav4SingleArray(FY)^[1] - PDav4SingleArray(FY)^[3]) *
    PDav4DoubleArray(fCoefficients)^[3] + PDav4SingleArray(FX)^[3];
  PDav4SingleArray(FX)^[3] := PDav4SingleArray(FY)^[1];
  Low   := (PDav4SingleArray(FY)^[3] + PDav4SingleArray(FY)^[2]) * 0.5;
  High  := PDav4SingleArray(FY)^[3] - Low;
end;

procedure TPolyphaseDownsampler64.ProcessSampleSplitLarge(
  out Low, High: Double; Input: TDAV2DoubleArray);
var
  i: Integer;
begin
  FY[0] := (Input[1] - FY[0]) * FCoefficients[0] + FX[0]; FX[0] := Input[1];
  PDav2SingleArray(FY)^[1] := (Input[0] - PDav2SingleArray(FY)^[1]) *
    PDav2DoubleArray(FCoefficients)^[1] + PDav2SingleArray(FX)^[1];
  PDav2SingleArray(FX)^[1] := Input[0];
  for i := 2 to FNumberOfCoeffs - 1 do
   begin
    FY[i] := (FY[i - 2] - FY[i]) * FCoefficients[i] + FX[i];
    FX[i] := FY[i - 2];
   end;

  Low := 0.5 * (FY[FNumberOfCoeffs - 1] + FY[FNumberOfCoeffs - 2]);
  High := FY[FNumberOfCoeffs - 1] - Low;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//   Name: ProcessBlockSplit                                                  //
//   -----------------------                                                  //
//                                                                            //
//   Description:                                                             //
//     Split (spectrum-wise) in half a pair of samples. The lower part of the //
//     spectrum is a classic downsampling, equivalent to the output of        //
//     process_sample().                                                      //
//     The higher part is the complementary signal: original filter response  //
//     is flipped from left to right, becoming a high-pass filter with the    //
//     same cutoff frequency. This signal is then critically sampled          //
//    (decimation by 2), flipping the spectrum: Fs/4...Fs/2 becomes Fs/4...0. //
//                                                                            //
//  Input parameters:                                                         //
//  - Input: pointer on the pair of input samples                             //
//                                                                            //
//  Output parameters:                                                        //
//  - low: output sample, lower part of the spectrum (downsampling)           //
//  - high: output sample, higher part of the spectrum.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPolyphaseDownsampler64.ProcessBlockSplit(
  const OutputL, OutputH, Input: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  Pos: Integer;
begin
  assert(SampleFrames > 0);

  pos := 0;
  repeat
    ProcessSampleSplit(OutputL[Pos], OutputH[Pos], PDAV2DoubleArray(@Input[pos * 2])^);
    Inc(Pos);
  until (Pos >= SampleFrames);
end;

function TPolyphaseDownsampler64.ProcessSample64(
  const Output: TDAV2DoubleArray): Double;
begin
 Result := FProcessSample64(Output);
end;

end.
