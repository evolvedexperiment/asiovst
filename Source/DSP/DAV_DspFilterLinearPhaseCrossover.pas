unit DAV_DspFilterLinearPhaseCrossover;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspWindowing;

type
  TLinearPhaseCrossover = class(TDspSampleRatePersistent)
  private
    FFilterKernel : PDAVSingleFixedArray;
    FStates       : array [0..1] of PDAVSingleFixedArray;
//    FBuffer       : TLinearPhaseLowpass;
    FFrequency    : Single;
    FFilterLength : Integer;
    procedure AllocateBuffers;
    procedure CalculateFilterKernel;
    procedure SetFrequency(const Value: Single);
    procedure SetFilterLength(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FilterLengthChanged; virtual;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessSample(const Input: Single; out Low, High: Single); overload;
    procedure ProcessSample(const Input: Double; out Low, High: Double); overload;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property FilterLength: Integer read FFilterLength write SetFilterLength;
  end;

implementation

uses
  SysUtils;

{ TLinearPhaseCrossover }

constructor TLinearPhaseCrossover.Create;
begin
 inherited;
 FFrequency    := 1000;
 FFilterKernel := nil;
end;

destructor TLinearPhaseCrossover.Destroy;
begin
 Dispose(FFilterKernel);
 Dispose(FStates[0]);
 Dispose(FStates[1]);
 inherited;
end;

procedure TLinearPhaseCrossover.AssignTo(Dest: TPersistent);
begin
 if Dest is TLinearPhaseCrossover then
  with TLinearPhaseCrossover(Dest) do
   begin
    inherited;
    FFrequency    := Self.FFrequency;
    FFilterLength := Self.FFilterLength;
    AllocateBuffers;

    Move(Self.FFilterKernel^, FFilterKernel^, FFilterLength * SizeOf(Single));
    Move(Self.FStates[0]^, FStates[0]^, FFilterLength * SizeOf(Single));
    Move(Self.FStates[1]^, FStates[1]^, FFilterLength * SizeOf(Single));
   end
 else inherited;
end;

procedure TLinearPhaseCrossover.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TLinearPhaseCrossover.SetFilterLength(const Value: Integer);
begin
 if FFilterLength <> Value then
  begin
   FFilterLength := Value;
   FilterLengthChanged;
  end;
end;

procedure TLinearPhaseCrossover.FilterLengthChanged;
begin
 AllocateBuffers;
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.AllocateBuffers;
begin
 ReallocMem(FFilterKernel, FFilterLength * SizeOf(Single));
 FillChar(FFilterKernel^[0], FFilterLength * SizeOf(Single), 0);
 ReallocMem(FStates[0], FFilterLength * SizeOf(Single));
 FillChar(FStates[0]^[0], FFilterLength * SizeOf(Single), 0);
 ReallocMem(FStates[1], FFilterLength * SizeOf(Single));
 FillChar(FStates[1]^[0], FFilterLength * SizeOf(Single), 0);
end;

procedure TLinearPhaseCrossover.FrequencyChanged;
begin
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.SampleRateChanged;
begin
 CalculateFilterKernel;
end;

procedure TLinearPhaseCrossover.CalculateFilterKernel;
var
  i, h   : Integer;
  n, d   : Double;
  CutOff : Double;
begin
 CutOff := FFrequency / SampleRate;

 if FFilterLength mod 2 = 0 then
  begin
   d := (FFilterLength - 1) * 0.5;
   // Generate sinc delayed by (N-1)/2
   for i := 0 to FFilterLength - 1 do
    begin
     n := PI * (i - d);
     FFilterKernel^[i] := sin(2.0 * Cutoff * n) / n;
    end;
  end
 else
  begin
   h := FFilterLength div 2;
   // Generate sinc delayed by (N-1)/2
   for i := 0 to FFilterLength - 1 do
    if (i = h)
     then FFilterKernel^[i] := 2.0 * CutOff
     else
      begin
       n := PI * (i - h);
       FFilterKernel^[i] := sin(2.0 * Cutoff * n) / n;
      end;
  end;
 ApplyHanningWindow(FFilterKernel, FFilterLength);
end;

procedure TLinearPhaseCrossover.ProcessSample(const Input: Single; out Low,
  High: Single);
{$IFDEF PUREPASCAL}
var
  Tap: Integer;
begin
 Low  := FStates[0]^[0] + Input * FFilterKernel^[0];
 High := FStates[1]^[0] - Input * FFilterKernel^[0];
 for Tap := 1 to FFilterLength - 1 do
  begin
   FStates[0]^[Tap - 1] := FStates[0]^[Tap] + Input * FFilterKernel^[Tap];
   FStates[1]^[Tap - 1] := FStates[1]^[Tap] - Input * FFilterKernel^[Tap];
   if Tap = FFilterLength div 2
    then FStates[1]^[Tap - 1] := FStates[1]^[Tap - 1] + Input
  end;
end;
{$ELSE}
asm
 push ebx
 push edi
 push esi

 mov ebx, [eax.FFilterKernel]

 mov edi, [eax.FStates    ].Integer
 mov esi, [eax.FStates + 4].Integer

 // calculate first sample
 fld  Input               // Input
 fld  st(0)               // Input, Input
 fmul [ebx].Single        // Input * FFilterKernel^[0], Input
 fld  st(0)               // Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fadd [edi].Single        // FStates[0]^[0] + Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fstp [edx].Single        // Input * FFilterKernel^[0], Input
 fld  [esi].Single        // FStates[1]^[0], Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fsubrp                   // FStates[1]^[0] - Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
 fstp [ecx].Single        // Input

 mov ecx, [eax.FFilterLength]
 mov eax, ecx
 shr eax, 1              // eax = FFilterLength div 2
 sub ecx, 1

 @loop:
   add  ebx, 4           // increase FilterKernel Pointer
   fld st(0)             // Input, Input
   fmul [ebx].Single     // Input * FFilterKernel^[i], Input
   fld  st(0)            // Input * FFilterKernel^[0], Input * FFilterKernel^[0], Input
   fadd [edi + 4].Single // FStates[0]^[i] + Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   fstp [edi].Single     // Input * FFilterKernel^[0], Input

   fld  [esi + 4].Single // FStates[1]^[i], Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   fsubrp                // FStates[1]^[i] - Input * FFilterKernel^[i], Input * FFilterKernel^[i], Input
   cmp ecx, eax
   jnz @norm
   fadd st(0), st(1)     // add Input if necessary
   @norm:
   fstp [esi].Single     // Input

   add  edi, 4           // increase State[0] Pointer
   add  esi, 4           // increase State[1] Pointer
 loop @loop

 fstp st(0)              // remove Input

 pop esi
 pop edi
 pop ebx
end;
{$ENDIF}

procedure TLinearPhaseCrossover.ProcessSample(const Input: Double; out Low,
  High: Double);
var
  Tap: Integer;
begin
 Low  := FStates[0]^[0] + Input * FFilterKernel^[0];
 High := FStates[1]^[0] - Input * FFilterKernel^[0];
 for Tap := 1 to FFilterLength - 1 do
  begin
   FStates[0]^[Tap - 1] := FStates[0]^[Tap] + Input * FFilterKernel^[Tap];
   FStates[1]^[Tap - 1] := FStates[1]^[Tap] - Input * FFilterKernel^[Tap];
   if Tap = FFilterLength div 2
    then FStates[1]^[Tap - 1] := FStates[1]^[Tap - 1] + Input
  end;
end;

end.
