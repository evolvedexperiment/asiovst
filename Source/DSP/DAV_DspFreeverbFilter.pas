unit DAV_DspFreeverbFilter;

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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

// Reverb model tuning values, taken from original algoritm by Jezar

const
  {$ALIGN 8}
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
  TFreeverbAllpass = class(TDspPersistent, IDspProcessor32)
  private
    FFeedback    : Single;
    FBuffer      : PDAVSingleFixedArray;
    FBufferSize  : Integer;
    FBufferPos   : Integer;
    procedure SetBufferSize(const Value: Integer);
  protected
    procedure BuffersizeChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const Buffersize: Integer = 1); virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; register;

    procedure Mute;
    property Feedback: Single read FFeedback write FFeedback;
    property BufferSize : Integer read FBufferSize write SetBufferSize;
  end;

  // Comb filter class declaration
  TFreeverbCombFilter = class(TDspPersistent, IDspProcessor32)
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
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure BuffersizeChanged; virtual;
    procedure DampChanged; virtual;
  public
    constructor Create(const Buffersize: Integer); virtual;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; register;

    procedure Mute;
    property Damp: Single read FDamp write SetDamp;
    property Feedback: Single read FFeedback write FFeedback;
    property BufferSize : Integer read FBufferSize write SetBufferSize;
  end;

implementation

{ TFreeverbAllpass }

constructor TFreeverbAllpass.Create(const Buffersize: Integer = 1);
begin
 inherited Create;
 FBuffersize := Buffersize;
 BuffersizeChanged;
end;

destructor TFreeverbAllpass.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TFreeverbAllpass.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value - 1 then
  begin
   FBufferSize := Value - 1;
   BuffersizeChanged;
  end;
end;

procedure TFreeverbAllpass.AssignTo(Dest: TPersistent);
begin
 if Dest is TFreeverbAllpass then
  with TFreeverbAllpass(Dest) do
   begin
    FFeedback    := Self.FFeedback;
    FBufferSize  := Self.FBufferSize;
    BuffersizeChanged;
    FBufferPos   := Self.FBufferPos;

    Move(Self.FBuffer^, FBuffer^, SizeOf(Single));
   end
 else inherited;
end;

procedure TFreeverbAllpass.BuffersizeChanged;
begin
 ReallocMem(FBuffer, (FBufferSize + 1) * SizeOf(Single));
 FBufferPos := 0;
end;

procedure TFreeverbAllpass.Mute;
begin
 FillChar(FBuffer^[0], (FBufferSize + 1) * SizeOf(Single), 0);
end;

procedure TFreeverbAllpass.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TFreeverbAllpass.ProcessSample32(Input: Single): Single;
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
  mov  [eax].FBufferPos, edx              // and store new index,
                                          // result already in st(0),
                                          // hence the fxch
end;
{$ENDIF}

{ TFreeverbCombFilter }

constructor TFreeverbCombFilter.Create(const Buffersize: Integer);
begin
 inherited Create;
 FBuffersize := Buffersize;
 BuffersizeChanged;
 FFilterStore := 0;
end;

destructor TFreeverbCombFilter.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TFreeverbCombFilter.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BuffersizeChanged;
  end;
end;

procedure TFreeverbCombFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TFreeverbCombFilter then
  with TFreeverbCombFilter(Dest) do
   begin
    FFeedback    := Self.FFeedback;
    FFilterStore := Self.FFilterStore;
    FDampA       := Self.FDampA;
    FDampB       := Self.FDampB;
    FBuffer      := Self.FBuffer;
    FDamp        := Self.FDamp;

    FBufferSize  := Self.FBufferSize;
    BuffersizeChanged;
    FBufferPos   := Self.FBufferPos;

    Move(Self.FBuffer^, FBuffer^, SizeOf(Single));
   end
 else inherited;
end;

procedure TFreeverbCombFilter.BuffersizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FBufferPos := 0;
 Changed;
end;

procedure TFreeverbCombFilter.SetDamp(Value: Single);
begin
 if FDampA <> Value then
  begin
   FDampA := Value;
   DampChanged;
  end;
end;

procedure TFreeverbCombFilter.DampChanged;
begin
 FDampB := 1 - FDampA;
 Changed;
end;

procedure TFreeverbCombFilter.Mute;
begin
 Fillchar(FBuffer^[0], FBufferSize * SizeOf(Single), 0);
end;

{ I really don't know if this is all as fast as can be,
  but it beats Delphi's compiler generated code hands down,
  Thaddy}

procedure TFreeverbCombFilter.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TFreeverbCombFilter.ProcessSample32(Input: Single): Single;
asm
  mov   ecx, [eax].FBuffer                        // FBuffer start in ecx
  mov   edx, [eax].FBufferPos                     // FBuffer index in edx

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

initialization
  RegisterDspProcessors32([TFreeverbAllpass, TFreeverbCombFilter]);

end.
