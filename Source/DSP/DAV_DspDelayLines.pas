unit DAV_DspDelayLines;

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
  Classes, DAV_Common, DAV_DspCommon;

// TODO: - remove Samplerate from TCustomDelayLineTime and make it
// TCustomFractionalDelayLine, build new class TCustomDelayLineTime in a
// new unit that contains TCustomFractionalDelayLine!!
// - complete assignto

type
  TCustomDelayLine = class(TDspPersistent)
  private
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;
  end;

  TCustomDelayLineSamples32 = class(TCustomDelayLine)
  private
    function GetSample(Index: Integer): Single;
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure BufferSizeChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
    property Sample[Index: Integer]: Single read GetSample;
  end;

  TCustomDelayLineSamples64 = class(TCustomDelayLine)
  private
    function GetSample(Index: Integer): Double;
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure BufferSizeChanged; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Double): Double;
    property Sample[Index: Integer]: Double read GetSample;
  end;

  TDelayLineSamples32 = class(TCustomDelayLineSamples32)
  published
    property BufferSize;
  end;

  TDelayLineSamples64 = class(TCustomDelayLineSamples64)
  published
    property BufferSize;
  end;

  TCustomDelayLineTime = class(TCustomDelayLine)
  private
    procedure SetSampleRate(const Value: Single);
    procedure SetTime(const Value: Single);
  protected
    FSampleRate : Single;
    FTime       : Single;
    procedure SampleRateChanged; virtual; abstract;
    procedure TimeChanged; virtual; abstract;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    property Samplerate: Single read FSampleRate write SetSampleRate;
    property Time: Single read FTime write SetTime;
  end;

  TDelayLineTime32 = class(TCustomDelayLineTime)
  private
    procedure CalculateBufferSize;
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FFractional     : Single;
    FIntBuffer      : TDAV4SingleArray;
    procedure BufferSizeChanged; override;
    procedure SampleRateChanged; override;
    procedure TimeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ProcessSample(const Input: Single): Single;
  published
    property Samplerate;
    property Time;
  end;

implementation

uses
  SysUtils, DAV_DspInterpolation;

{ TCustomDelayLine }

constructor TCustomDelayLine.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 FBufferPos  := 0;
end;

procedure TCustomDelayLine.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDelayLine then
  with TCustomDelayLine(Dest) do
   begin
    FBufferPos  := Self.FBufferPos;
    BufferSize := Self.BufferSize;
   end
  else inherited;
end;

procedure TCustomDelayLine.Reset;
begin
 FBufferPos := 0;
end;

procedure TCustomDelayLine.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TCustomDelayLineSamples32 }

constructor TCustomDelayLineSamples32.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(Buffersize);
end;

destructor TCustomDelayLineSamples32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TCustomDelayLineSamples32.GetSample(Index: Integer): Single;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt('Index out of bounds(%d)', [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);
 result := FBuffer^[Pos];
end;

function TCustomDelayLineSamples32.ProcessSample(const Input: Single): Single;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TCustomDelayLineSamples32.AssignTo(Dest: TPersistent);
var
  SampleNo : Integer;
begin
 if Dest is TCustomDelayLineSamples32 then
  with TCustomDelayLineSamples32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomDelayLineSamples64 then
  with TCustomDelayLineSamples64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for SampleNo := 0 to FBufferSize - 1
     do Self.FBuffer^[SampleNo] := FBuffer^[SampleNo];
   end
  else inherited;
end;

procedure TCustomDelayLineSamples32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomDelayLineSamples32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TCustomDelayLineSamples64 }

constructor TCustomDelayLineSamples64.Create(const BufferSize: Integer = 0);
begin
 inherited Create(BufferSize);
 FBuffer := nil;
end;

destructor TCustomDelayLineSamples64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

function TCustomDelayLineSamples64.GetSample(Index: Integer): Double;
var
  Pos: Integer;
begin
 if (Index < 0) or (Index >= FBufferSize)
  then raise Exception.CreateFmt('Index out of bounds(%d)', [Index]);

 Pos := FBufferPos - Index;
 if Pos < 0
  then Inc(Pos, FBufferSize);
 result := FBuffer^[Pos];
end;

procedure TCustomDelayLineSamples64.AssignTo(Dest: TPersistent);
var
  SampleNo : Integer;
begin
 if Dest is TCustomDelayLineSamples32 then
  with TCustomDelayLineSamples32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for SampleNo := 0 to FBufferSize - 1
     do Self.FBuffer^[SampleNo] := FBuffer^[SampleNo];
   end else
 if Dest is TCustomDelayLineSamples64 then
  with TCustomDelayLineSamples64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomDelayLineSamples64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

function TCustomDelayLineSamples64.ProcessSample(const Input: Double): Double;
begin
 result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TCustomDelayLineSamples64.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TCustomDelayLineTime }

constructor TCustomDelayLineTime.Create;
begin
 inherited;
 FTime := 1;
 FSampleRate := 44100;
end;

procedure TCustomDelayLineTime.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

procedure TCustomDelayLineTime.SetTime(const Value: Single);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;


{ TDelayLineTime32 }

constructor TDelayLineTime32.Create;
begin
 inherited;
 FBuffer := nil;
 FIntBuffer[3] := 0;
end;

destructor TDelayLineTime32.Destroy;
begin
// assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineTime32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
end;

function TDelayLineTime32.ProcessSample(const Input: Single): Single;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[0], FIntBuffer[1], 3 * SizeOf(Single));
 FIntBuffer[0] := FBuffer^[FBufferPos];
 result := Hermite32_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineTime32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

procedure TDelayLineTime32.CalculateBufferSize;
begin
 BufferSize      := round(FTime * FSampleRate + 0.5) + 1;
 FFractional     := BufferSize - 1 - (FTime * FSampleRate);
 FBuffer[BufferSize - 1] := 0;
 assert(FFractional >= 0);
 assert(FFractional <= 1);
end;

procedure TDelayLineTime32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TDelayLineTime32.TimeChanged;
begin
 inherited;
 CalculateBufferSize;
end;

end.
