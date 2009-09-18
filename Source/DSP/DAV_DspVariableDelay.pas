unit DAV_DspVariableDelay;

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
  DAV_Common, DAV_DspCommon, DAV_DspInterpolation, DAV_DspFilter;

type
  TCustomVariableDelay = class(TDspSampleRatePersistent)
  private
    FDelay        : Single;
    FClearBuffer  : Boolean;
    procedure SetDelay(const Value: Single);
  protected
    FFractional    : Single;
    FBufferSize    : Integer;
    FBufferPos     : Integer;
    procedure SampleRateChanged; override;
    procedure DelayChanged; virtual;
    procedure ChangeBuffer(const NewSize: Integer); virtual; abstract;
    procedure ResetBufferPosition; virtual;
    class function InterpolatorLength: Integer; virtual; abstract;
  public
    constructor Create; override;
    property SampleRate;
    property Delay: Single read FDelay write SetDelay;
    property ClearBufferOnChange: Boolean read FClearBuffer write FClearBuffer default true;
  end;

  TCustomVariableDelay32 = class(TCustomVariableDelay, IDspProcessor32)
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure ChangeBuffer(const NewSize: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample32(Input: Single): Single; virtual; abstract;
  end;

  TVariableDelay32Linear = class(TCustomVariableDelay32)
  protected
    class function InterpolatorLength: Integer; override;
  public
    function ProcessSample32(Input: Single): Single; override;
  end;

  TVariableDelay32Hermite = class(TCustomVariableDelay32)
  protected
//    FBufferOutPos : Integer;
    class function InterpolatorLength: Integer; override;
    procedure ResetBufferPosition; override;
  public
    constructor Create; override;
    function ProcessSample32(Input: Single): Single; override;
  end;

  TVariableDelay32Allpass = class(TCustomVariableDelay32)
  protected
    FAllpassFilter : TFirstOrderAllpassFilter;
    class function InterpolatorLength: Integer; override;
    procedure DelayChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample32(Input: Single): Single; override;
  end;

implementation

uses
  Math, SysUtils;

{ TCustomVariableDelay }

constructor TCustomVariableDelay.Create;
begin
 inherited;
 FClearBuffer := True;
 FDelay := 0;
 ResetBufferPosition;
end;

procedure TCustomVariableDelay.ResetBufferPosition;
begin
 if InterpolatorLength < FBufferSize
  then FBufferPos := InterpolatorLength else
 if FBufferSize > 0
  then FBufferPos := FBufferSize - 1
  else FBufferPos := 0;
end;

procedure TCustomVariableDelay.SetDelay(const Value: Single);
begin
 if FDelay <> Value then
  begin
   FDelay := Value;
   DelayChanged;
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
 NewSize := round(SampleRate * FDelay + 0.50000001);
 FFractional := NewSize - SampleRate * FDelay;
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
 Result := 2;
end;

function TVariableDelay32Linear.ProcessSample32(Input: Single): Single;
begin
 FBuffer[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 1;

 if FBufferPos + 1 >= FBufferSize then
  begin
   FBuffer[0] := FBuffer[FBufferPos];
   Result := LinearInterpolation(FFractional, @FBuffer[0]);
  end
 else Result := LinearInterpolation(FFractional, @FBuffer[FBufferPos]);
end;

{ TVariableDelay32Hermite }

constructor TVariableDelay32Hermite.Create;
begin
 inherited;
end;

class function TVariableDelay32Hermite.InterpolatorLength: Integer;
begin
 Result := 4;
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

function TVariableDelay32Hermite.ProcessSample32(Input: Single): Single;
begin
 FBuffer[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 2;

 if FBufferPos + 2 >= FBufferSize then
  begin
   if FBufferPos + 2 = FBufferSize
    then move(FBuffer[FBufferPos], FBuffer[0], 2 * SizeOf(Single));
   Result := ModifiedHermite32(FFractional, @FBuffer[FBufferPos - FBufferSize + 2]);
  end
 else Result := ModifiedHermite32(FFractional, @FBuffer[FBufferPos]);
end;

procedure TVariableDelay32Hermite.ResetBufferPosition;
begin
 inherited;
// FBufferOutPos := FBufferPos + 1;
end;

{ TVariableDelay32Allpass }

constructor TVariableDelay32Allpass.Create;
begin
 inherited;
 FAllpassFilter := TFirstOrderAllpassFilter.Create;
end;

procedure TVariableDelay32Allpass.DelayChanged;
begin
 inherited;
 FAllpassFilter.Frequency := 0.5 * FFractional;
end;

destructor TVariableDelay32Allpass.Destroy;
begin
 FreeAndNil(FAllpassFilter);
 inherited;
end;

class function TVariableDelay32Allpass.InterpolatorLength: Integer;
begin
 Result := 0;
end;

function TVariableDelay32Allpass.ProcessSample32(Input: Single): Single;
begin
 FBuffer[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 0;
 Result := FAllpassFilter.ProcessSample64(FBuffer[FBufferPos]);
end;

procedure TVariableDelay32Allpass.SampleRateChanged;
begin
 inherited;
 FAllpassFilter.SampleRate := SampleRate;
end;

end.
