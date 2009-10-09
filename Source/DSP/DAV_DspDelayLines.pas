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
  Classes, DAV_Common, DAV_Classes;

// ToDo: complete assignto

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
    procedure ClearBuffer; virtual; abstract;
  end;

  TCustomDelayLineSamples32 = class(TCustomDelayLine, IDspProcessor32)
  private
    function GetSample(Index: Integer): Single;
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;
    
    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    property Sample[Index: Integer]: Single read GetSample;
  end;

  TCustomDelayLineSamples64 = class(TCustomDelayLine, IDspProcessor64)
  private
    function GetSample(Index: Integer): Double;
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

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

  TCustomDelayLineFractional = class(TCustomDelayLine)
  private
    procedure SetFractional(const Value: Double);
    function GetFractional: Double;
  protected
    FFractional : Double;
    procedure FractionalChanged; virtual;
  public
    constructor Create(const FractionalBufferSize: Double = 0); reintroduce; virtual;
    property FractionalBuffersize: Double read GetFractional write SetFractional;
  end;

  TDelayLineFractional32 = class(TCustomDelayLineFractional, IDspProcessor32)
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FIntBuffer      : TDAV4SingleArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const FractionalBufferSize: Double = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

  published
    property FractionalBufferSize;
  end;

  TDelayLineFractional64 = class(TCustomDelayLineFractional, IDspProcessor64)
  protected
    FBuffer         : PDAVDoubleFixedArray;
    FRealBufferSize : Integer;
    FIntBuffer      : TDAV4DoubleArray;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const FractionalBufferSize: Double = 0); override;
    destructor Destroy; override;

    procedure ClearBuffer; override;
    procedure Reset; override;

    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;
  published
    property FractionalBufferSize;
  end;


  TDelayLineTime32 = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FFractionalDelay : TDelayLineFractional32;
    FTime: Double;
    procedure SetTime(const Value: Double);
    procedure TimeChanged;
  public
    constructor Create(const BufferSize: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
    function ProcessSample32(Input: Single): Single;
    procedure ProcessBlock32(Data: PDAVSingleFixedArray; SampleCount: Integer);
  published
    property Samplerate;
    property Time: Double read FTime write SetTime;
  end;

  TDelayLineTime64 = class(TDspSampleRatePersistent, IDspProcessor64)
  private
    FFractionalDelay : TDelayLineFractional64;
    FTime            : Double;
    procedure SetTime(const Value: Double);
    procedure TimeChanged;
  public
    constructor Create(const BufferSize: Integer = 0); reintroduce; virtual;
    destructor Destroy; override;

    procedure ProcessBlock64(Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    procedure Reset; virtual;
  published
    property Samplerate;
    property Time: Double read FTime write SetTime;
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
 Result := FBuffer^[Pos];
end;

procedure TCustomDelayLineSamples32.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomDelayLineSamples32.ProcessSample32(Input: Single): Single;
begin
 Result := FBuffer^[FBufferPos];
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
 // ClearBuffer;
end;

procedure TCustomDelayLineSamples32.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TCustomDelayLineSamples32.ClearBuffer;
begin
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
 Result := FBuffer^[Pos];
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
 // ClearBuffer;
end;

procedure TCustomDelayLineSamples64.ProcessBlock64(Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomDelayLineSamples64.ProcessSample64(Input: Double): Double;
begin
 Result := FBuffer^[FBufferPos];
 FBuffer^[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

procedure TCustomDelayLineSamples64.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TCustomDelayLineSamples64.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TCustomDelayLineFractional }

constructor TCustomDelayLineFractional.Create(const FractionalBufferSize: Double = 0);
begin
 assert(FractionalBufferSize >= 0);
 inherited Create(Trunc(FractionalBufferSize));
 FFractional := FractionalBufferSize - FBufferSize;
end;

procedure TCustomDelayLineFractional.FractionalChanged;
begin
 assert(FFractional >= 0);
end;

function TCustomDelayLineFractional.GetFractional: Double;
begin
 Result := FBufferSize + FFractional;
end;

procedure TCustomDelayLineFractional.SetFractional(const Value: Double);
begin
 if FractionalBuffersize <> Value then
  begin
   BufferSize := Trunc(Value);
   FFractional := Value - FBufferSize;
   FractionalChanged;
  end;
end;


{ TDelayLineFractional32 }

constructor TDelayLineFractional32.Create(const FractionalBufferSize: Double = 0);
begin
 inherited Create(Trunc(FractionalBufferSize));
 FBuffer := nil;
 FIntBuffer[3] := 0;
end;

destructor TDelayLineFractional32.Destroy;
begin
// assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineFractional32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
end;

procedure TDelayLineFractional32.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDelayLineFractional32.ProcessSample32(Input: Single): Single;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[0], FIntBuffer[1], 3 * SizeOf(Single));
 FIntBuffer[0] := FBuffer^[FBufferPos];
 Result := Hermite32_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineFractional32.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TDelayLineFractional32.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;


{ TDelayLineFractional64 }

constructor TDelayLineFractional64.Create(const FractionalBufferSize: Double = 0);
begin
 inherited Create(Trunc(FractionalBufferSize));
 FBuffer := nil;
 FIntBuffer[3] := 0;
end;

destructor TDelayLineFractional64.Destroy;
begin
// assert(FBuffer[BufferSize - 1] = 0);
 Dispose(FBuffer);
 inherited;
end;

procedure TDelayLineFractional64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
end;

procedure TDelayLineFractional64.ProcessBlock64(Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDelayLineFractional64.ProcessSample64(Input: Double): Double;
begin
 FBuffer^[FBufferPos] := Input;

 inc(FBufferPos);
 if FBufferPos >= BufferSize - 1
  then FBufferPos := 0;

 Move(FIntBuffer[0], FIntBuffer[1], 3 * SizeOf(Double));
 FIntBuffer[0] := FBuffer^[FBufferPos];
 Result := Hermite64_asm(FFractional, @FIntBuffer);
end;

procedure TDelayLineFractional64.Reset;
begin
 inherited;
 ClearBuffer;
end;

procedure TDelayLineFractional64.ClearBuffer;
begin
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;


{ TDelayLineTime32 }

constructor TDelayLineTime32.Create(const BufferSize: Integer);
begin
 inherited Create;
 FFractionalDelay := TDelayLineFractional32.Create(BufferSize);
 FTime := 0;
end;

destructor TDelayLineTime32.Destroy;
begin
 FreeAndNil(FFractionalDelay);
 inherited;
end;

procedure TDelayLineTime32.ProcessBlock32(Data: PDAVSingleFixedArray;
  SampleCount: Integer);
begin

end;

function TDelayLineTime32.ProcessSample32(Input: Single): Single;
begin
 Result := FFractionalDelay.ProcessSample32(Input);
end;

procedure TDelayLineTime32.Reset;
begin
 inherited;
 FFractionalDelay.Reset;
end;

procedure TDelayLineTime32.SetTime(const Value: Double);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;

procedure TDelayLineTime32.TimeChanged;
begin
 FFractionalDelay.FractionalBufferSize := FTime * SampleRate;
end;


{ TDelayLineTime64 }

constructor TDelayLineTime64.Create(const BufferSize: Integer);
begin
 inherited Create;
 FFractionalDelay := TDelayLineFractional64.Create(BufferSize);
 FTime := 0;
end;

destructor TDelayLineTime64.Destroy;
begin
 FreeAndNil(FFractionalDelay);
 inherited;
end;

procedure TDelayLineTime64.ProcessBlock64(Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDelayLineTime64.ProcessSample64(Input: Double): Double;
begin
 Result := FFractionalDelay.ProcessSample64(Input);
end;

procedure TDelayLineTime64.Reset;
begin
 inherited;
 FFractionalDelay.Reset;
end;

procedure TDelayLineTime64.SetTime(const Value: Double);
begin
 if FTime <> Value then
  begin
   FTime := Value;
   TimeChanged;
  end;
end;

procedure TDelayLineTime64.TimeChanged;
begin
 FFractionalDelay.FractionalBufferSize := FTime * SampleRate;
end;

end.
